//! Background compiler thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::{AudioMessage, AudioThreadSongInterpreter, MusicChannelsMask, SiCad};
use crate::names::NameGetter;
use crate::sample_analyser::{self, FftSettings, SampleAnalysis};
use crate::sfx_export_order::{GuiSfxExportOrder, SfxExportOrderAction};
use crate::GuiMessage;

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Deref;
use std::sync::{mpsc, Arc};
use std::thread;

use compiler::bytecode_interpreter::SongInterpreter;
use compiler::common_audio_data::{
    build_cad_with_sfx_buffer, build_common_audio_data, CommonAudioData,
};
use compiler::data::{self, BrrEvaluator};
use compiler::data::{load_text_file_with_limit, DefaultSfxFlags, LoopSetting, TextFile};
use compiler::driver_constants::{self, COMMON_DATA_BYTES_PER_SOUND_EFFECT};
use compiler::envelope::Envelope;
use compiler::errors::{
    self, BrrError, CommonAudioDataErrors, LoadSongError, MmlPrefixError, ProjectFileErrors,
    SongTooLargeError,
};
use compiler::identifier::ChannelId;
use compiler::mml::{compile_mml_prefix, find_cursor_state};
use compiler::notes::Note;
use compiler::path::{ParentPathBuf, SourcePathBuf};
use compiler::samples::{
    combine_samples, create_test_instrument_data, encode_or_load_brr_file,
    load_sample_for_instrument, load_sample_for_sample, CompiledDataList, InstrumentSampleData,
    SampleAndInstrumentData, SampleFileCache, SampleSampleData, WAV_EXTENSION,
};
use compiler::songs::{test_sample_song, SongAramSize, SongData, BLANK_SONG_ARAM_SIZE};
use compiler::sound_effects::{
    blank_compiled_sound_effects, combine_sound_effects, CompiledSfxMap, CompiledSfxSubroutines,
    SfxExportOrder, SfxSubroutinesMml,
};
use compiler::sound_effects::{compile_sound_effect_input, CompiledSoundEffect, SoundEffectInput};
use compiler::spc_file_export::export_spc_file;
use compiler::Pan;

extern crate fltk;

mod item_id {
    use std::sync::atomic::{AtomicU64, Ordering};

    /// ItemId gives all items a unique id that the compiler and GUI threads can reference without
    /// worrying about the order of the items in the two threads.
    ///
    /// This allows me to:
    ///   1. Not worry about the item index if the compiler thread is slow and the index changed
    ///      before the item has finished compiling.
    ///   2. Ignore any list item moved events in the compiler thread.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ItemId(u64);

    impl ItemId {
        #[allow(clippy::new_without_default)]
        pub fn new() -> Self {
            static NEXT_ID: AtomicU64 = AtomicU64::new(1001);

            let new_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
            Self(new_id)
        }
    }
}
use compiler::time::TickCounter;
pub use item_id::ItemId;

#[derive(Debug)]
pub struct PlaySampleArgs {
    pub note: Note,
    pub note_length: u16,
    pub envelope: Option<Envelope>,
}

#[derive(Debug)]
pub struct ReplaceAllVec<T>(Vec<(ItemId, T)>);

impl<T> ReplaceAllVec<T> {
    pub fn new(v: Vec<(ItemId, T)>) -> Self {
        Self(v)
    }
}

#[derive(Debug)]
pub struct ProjectToCompiler {
    pub default_sfx_flags: DefaultSfxFlags,
    pub sfx_export_order: GuiSfxExportOrder,
    pub pf_songs: ReplaceAllVec<data::Song>,
    pub instruments: ReplaceAllVec<data::Instrument>,
    pub samples: ReplaceAllVec<data::Sample>,
}

#[derive(Debug)]
pub struct SfxToCompiler {
    pub subroutines: SfxSubroutinesMml,
    pub sound_effects: ReplaceAllVec<SoundEffectInput>,
}

#[derive(Debug)]
pub enum ItemChanged<T> {
    AddedOrEdited(ItemId, T),
    MultipleAddedOrEdited(Vec<(ItemId, T)>),
    Removed(ItemId),
}

#[derive(Debug)]
pub enum ToCompiler {
    LoadProject(ProjectToCompiler),
    LoadSoundEffects(SfxToCompiler),

    ClearSampleCacheAndRebuild,

    DefaultSfxFlagChanged(DefaultSfxFlags),
    SfxExportOrder(SfxExportOrderAction),

    ProjectSongs(ItemChanged<data::Song>),

    Instrument(ItemChanged<data::Instrument>),
    Sample(ItemChanged<data::Sample>),

    AnalyseSample(SourcePathBuf, LoopSetting, BrrEvaluator, FftSettings),

    // Updates sfx_data_size and rechecks song sizes.
    // (sent when the user deselects the sound effects tab in the GUI)
    FinishedEditingSoundEffects,

    CompileSoundEffectSubroutines(SfxSubroutinesMml),
    SoundEffects(ItemChanged<SoundEffectInput>),
    PlaySongWithSfxBuffer(ItemId, TickCounter),
    PlaySfxUsingSfxBuffer(ItemId, Pan),

    SongTabClosed(ItemId),
    SongChanged {
        id: ItemId,
        mml: String,
        cursor_index: Option<u32>,
    },
    SongCursorMoved {
        id: ItemId,
        cursor_index: u32,
    },
    CalcSongDriverState(ItemId, TickCounter),

    CompileAndPlaySong(ItemId, String, TickCounter, MusicChannelsMask),
    CompileAndPlaySongSubroutine(ItemId, String, Option<String>, u8, TickCounter),
    PlayInstrument(ItemId, PlaySampleArgs),
    PlaySample(ItemId, PlaySampleArgs),

    ExportSongToSpcFile(ItemId),

    RemoveFileFromSampleCache(SourcePathBuf),
    RecompileInstrumentsUsingSample(SourcePathBuf),
}

#[derive(Debug)]
pub struct InstrumentSize(pub usize);

#[derive(Debug)]
pub struct SampleSize(pub usize);

pub type InstrumentOutput = Result<InstrumentSize, errors::SampleError>;
pub type SampleOutput = Result<SampleSize, errors::SampleError>;
pub type SfxSubroutineOutput = Result<Arc<CompiledSfxSubroutines>, SfxSubroutinesError>;
pub type SoundEffectOutput = Result<Arc<CompiledSoundEffect>, SfxError>;
pub type SongOutput = Result<Arc<SongData>, SongError>;

#[derive(Debug)]
pub enum CursorDriverState {
    None,
    NoSong(ItemId),
    NoCursor(ItemId),
    ManualTicks(ItemId, Box<AudioThreadSongInterpreter>),
    CursorSong(ItemId, Box<AudioThreadSongInterpreter>),
    Subroutine(ItemId, Box<AudioThreadSongInterpreter>),
    BcError(ItemId),
    BcTimeout(ItemId),
}

#[derive(Debug)]
pub enum CompilerOutput {
    Panic(String),

    Instrument(ItemId, InstrumentOutput),
    Sample(ItemId, SampleOutput),

    CommonAudioData(CadOutput),

    SfxSubroutines(SfxSubroutineOutput),
    SoundEffect(ItemId, SoundEffectOutput),

    Song(ItemId, SongOutput),
    SongPrefix(ItemId, Result<(), MmlPrefixError>),

    NumberOfMissingSoundEffects(usize),

    LargestSongSize(SongAramSize),

    // The result of the last `ToCompiler::ExportSongToSpcFile` operation
    SpcFileResult(Result<(String, Box<[u8]>), SpcFileError>),

    SampleAnalysis(Result<SampleAnalysis, BrrError>),

    SongCursorDriverState(CursorDriverState),
}

#[derive(Debug, Default)]
pub struct InstrumentAndSampleNames(Vec<data::Name>);

impl InstrumentAndSampleNames {
    pub fn get(&self, index: usize) -> Option<&data::Name> {
        self.0.get(index)
    }
}

#[derive(Debug)]
pub struct CommonAudioDataNoSfx(pub CommonAudioData, pub Arc<InstrumentAndSampleNames>);

#[derive(Debug)]
pub struct CommonAudioDataWithSfxBuffer(
    pub compiler::common_audio_data::CommonAudioDataWithSfxBuffer,
    pub Arc<InstrumentAndSampleNames>,
);

#[derive(Debug)]
pub struct CommonAudioDataWithSfx {
    pub common_audio_data: CommonAudioData,
    pub sfx_export_order: Arc<GuiSfxExportOrder>,
    pub instrument_and_sample_names: Arc<InstrumentAndSampleNames>,
}

#[derive(Debug)]
pub enum CadOutput {
    None,
    Err(CombineSamplesError),
    NoSfx(Arc<CommonAudioDataNoSfx>),
    SfxBuffer(Arc<CommonAudioDataWithSfxBuffer>),
    WithSfx(Arc<CommonAudioDataWithSfx>),
}

impl CadOutput {
    pub fn is_ok_or_none(&self) -> bool {
        match self {
            Self::Err(..) => false,
            Self::None | Self::NoSfx(..) | Self::SfxBuffer(..) | Self::WithSfx(..) => true,
        }
    }
}

#[derive(Debug)]
pub enum CombineSamplesError {
    IndividualErrors {
        n_instrument_errors: usize,
        n_sample_errors: usize,
    },
    CombineError(errors::SampleAndInstrumentDataError),
    CommonAudioData(errors::CommonAudioDataErrors),
    UniqueNamesError(ProjectFileErrors),
}

impl std::fmt::Display for CombineSamplesError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CombineSamplesError::IndividualErrors {
                n_instrument_errors,
                n_sample_errors,
            } => match (*n_instrument_errors, *n_sample_errors) {
                (1, 0) => writeln!(f, "1 instrument has an error"),
                (0, 1) => writeln!(f, "1 sample has an error"),
                (i, 0) => writeln!(f, "{i} instruments have errors"),
                (0, s) => writeln!(f, "{s} samples have errors"),
                (i, s) => writeln!(f, "{i} instruments and {s} samples have errors"),
            },
            CombineSamplesError::CombineError(e) => {
                writeln!(f, "{}", e.multiline_display())
            }
            CombineSamplesError::CommonAudioData(e) => {
                writeln!(f, "{}", e.multiline_display())
            }
            CombineSamplesError::UniqueNamesError(e) => e.multiline_display().fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum SfxError {
    DependencyInstruments,
    DependencySfxSubroutines,
    Error(errors::SoundEffectError),
}

impl Display for SfxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DependencyInstruments => {
                writeln!(f, "dependency error: error an in instrument or sample")
            }
            Self::DependencySfxSubroutines => {
                writeln!(f, "dependency error: error in a SFX subroutine")
            }
            Self::Error(e) => e.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum SfxSubroutinesError {
    DependencyInstruments,
    Error(errors::SfxSubroutineErrors),
}

impl Display for SfxSubroutinesError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DependencyInstruments => {
                writeln!(f, "dependency error: error an in instrument or sample")
            }
            Self::Error(e) => e.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum SongError {
    Dependency,
    Song(errors::SongError),
    TooLarge(SongTooLargeError),
}

impl SongError {
    pub fn to_short_error(&self) -> ShortSongError {
        match self {
            Self::Dependency => ShortSongError::Dependency,
            Self::Song(_) => ShortSongError::Song,
            Self::TooLarge(_) => ShortSongError::TooLarge,
        }
    }
}

#[derive(Debug)]
pub enum ShortSongError {
    Dependency,
    Song,
    TooLarge,
}

impl Display for ShortSongError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dependency => writeln!(f, "dependency error"),
            Self::Song => writeln!(f, "song error"),
            Self::TooLarge => writeln!(f, "song too large"),
        }
    }
}

#[derive(Debug)]
pub enum SpcFileError {
    NoSong,
    InvalidSong,
    NoSamples,
    CommonAudioDataError(CommonAudioDataErrors),
    Spc(LoadSongError),
}

impl std::fmt::Display for SpcFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoSong => write!(f, "No song to export"),
            Self::InvalidSong => write!(f, "Error compiling song"),
            Self::NoSamples => write!(
                f,
                "cannot create common-audio-data: Sample or Instrument error"
            ),
            Self::CommonAudioDataError(e) => e.multiline_display().fmt(f),
            Self::Spc(e) => e.fmt(f),
        }
    }
}

// Decrement all values greater then index.
// (Assumes index is not in map).
fn decrement_index_from_map<T>(map: &mut HashMap<T, usize>, index: usize) {
    for map_index in map.values_mut() {
        if *map_index > index {
            *map_index -= 1;
        }
    }
}

struct IList<ItemT> {
    items: Vec<ItemT>,
    map: HashMap<ItemId, usize>,
}

impl<ItemT> IList<ItemT> {
    fn new() -> Self {
        Self {
            items: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn get(&self, id: &ItemId) -> Option<&ItemT> {
        self.map.get(id).and_then(|i| self.items.get(*i))
    }

    fn replace_all(&mut self, data: ReplaceAllVec<ItemT>) {
        self.map = data
            .0
            .iter()
            .enumerate()
            .map(|(index, (id, _item))| (*id, index))
            .collect();

        self.items = data.0.into_iter().map(|(_id, item)| item).collect();
    }

    fn add_or_edit(&mut self, id: ItemId, item: ItemT) {
        match self.map.get(&id) {
            Some(index) => {
                self.items[*index] = item;
            }
            None => {
                let index = self.items.len();

                self.items.push(item);
                self.map.insert(id, index);
            }
        }
    }

    fn remove(&mut self, id: ItemId) {
        if let Some(index) = self.map.remove(&id) {
            decrement_index_from_map(&mut self.map, index);

            self.items.remove(index);
        }
    }

    fn process_message(&mut self, m: ItemChanged<ItemT>) {
        match m {
            ItemChanged::AddedOrEdited(id, item) => {
                self.add_or_edit(id, item);
            }
            ItemChanged::MultipleAddedOrEdited(vec) => {
                for (id, item) in vec {
                    self.add_or_edit(id, item);
                }
            }
            ItemChanged::Removed(id) => self.remove(id),
        }

        assert_eq!(self.items.len(), self.map.len());
    }
}

struct CList<ItemT, OutT> {
    items: Vec<ItemT>,
    output: Vec<OutT>,
    map: HashMap<ItemId, usize>,
    name_map: HashMap<data::Name, usize>,
}

impl<ItemT, OutT> CList<ItemT, OutT>
where
    ItemT: NameGetter,
    OutT: Default,
{
    fn new() -> Self {
        Self {
            items: Vec::new(),
            output: Vec::new(),
            map: HashMap::new(),
            name_map: HashMap::new(),
        }
    }

    fn items(&self) -> &[ItemT] {
        &self.items
    }

    fn get_output_for_name(&self, name: &data::Name) -> Option<&OutT> {
        self.name_map.get(name).and_then(|i| self.output.get(*i))
    }

    fn get_output_for_id(&self, id: &ItemId) -> Option<&OutT> {
        self.map.get(id).and_then(|i: &usize| self.output.get(*i))
    }

    fn name_map(&self) -> &HashMap<data::Name, usize> {
        &self.name_map
    }

    // NOTE: Does not compile the items::
    fn replace_all(&mut self, data: ReplaceAllVec<ItemT>) {
        self.map = data
            .0
            .iter()
            .enumerate()
            .map(|(index, (id, _item))| (*id, index))
            .collect();

        self.output.clear();
        self.output.resize_with(data.0.len(), OutT::default);

        self.name_map = data
            .0
            .iter()
            .enumerate()
            .map(|(index, (_id, item))| (item.name().clone(), index))
            .collect();

        self.items = data.0.into_iter().map(|(_id, item)| item).collect();
    }

    fn add_or_edit(
        &mut self,
        id: ItemId,
        item: ItemT,
        compiler_fn: &mut impl FnMut(ItemId, &ItemT) -> OutT,
    ) {
        match self.map.get(&id) {
            Some(index) => {
                let out = compiler_fn(id, &item);

                let old_name = self.items[*index].name();
                if item.name() != old_name {
                    self.name_map.remove(old_name);
                    self.name_map.insert(item.name().clone(), *index);
                }

                self.output[*index] = out;
                self.items[*index] = item;
            }
            None => {
                let index = self.items.len();
                let out = compiler_fn(id, &item);

                self.name_map.insert(item.name().clone(), index);

                self.map.insert(id, index);
                self.output.push(out);
                self.items.push(item);
            }
        }
    }

    fn remove(&mut self, id: ItemId) {
        if let Some(index) = self.map.remove(&id) {
            decrement_index_from_map(&mut self.map, index);

            let name_map_index = self.name_map.remove(self.items[index].name());
            decrement_index_from_map(&mut self.name_map, index);
            assert_eq!(name_map_index, Some(index));

            self.output.remove(index);
            self.items.remove(index);
        }
    }

    fn process_message(
        &mut self,
        m: ItemChanged<ItemT>,
        mut compiler_fn: impl FnMut(ItemId, &ItemT) -> OutT,
    ) {
        match m {
            ItemChanged::AddedOrEdited(id, item) => {
                self.add_or_edit(id, item, &mut compiler_fn);
            }
            ItemChanged::MultipleAddedOrEdited(vec) => {
                for (id, item) in vec {
                    self.add_or_edit(id, item, &mut compiler_fn);
                }
            }
            ItemChanged::Removed(id) => self.remove(id),
        }

        assert_eq!(self.output.len(), self.items.len());
        assert_eq!(self.map.len(), self.items.len());
        assert_eq!(self.name_map.len(), self.items.len());
    }

    fn item_changes_name(&mut self, m: &ItemChanged<ItemT>) -> bool {
        let test_add_or_edit = |id, item: &ItemT| -> bool {
            match self.map.get(id) {
                Some(i) => match self.name_map.get(item.name()) {
                    Some(j) => i != j,
                    None => true,
                },
                None => true,
            }
        };

        match m {
            ItemChanged::AddedOrEdited(id, item) => test_add_or_edit(id, item),
            ItemChanged::MultipleAddedOrEdited(vec) => {
                vec.iter().any(|(id, item)| test_add_or_edit(id, item))
            }
            ItemChanged::Removed(_) => true,
        }
    }

    fn recompile_all(&mut self, mut compiler_fn: impl FnMut(ItemId, &ItemT) -> OutT) {
        for (&id, &index) in &self.map {
            let out = compiler_fn(id, &self.items[index]);
            self.output[index] = out;
        }
    }

    fn recompile_all_if(
        &mut self,
        mut compiler_fn: impl FnMut(ItemId, &ItemT) -> OutT,
        filter_fn: impl Fn(&ItemT) -> bool,
    ) {
        for (&id, &index) in &self.map {
            let item = &self.items[index];
            if filter_fn(item) {
                let out = compiler_fn(id, item);
                self.output[index] = out;
            }
        }
    }
}

impl<ItemT, OutT> CList<ItemT, Option<OutT>> {
    fn count_errors(&self) -> usize {
        self.output.iter().filter(|o| o.is_none()).count()
    }
}

impl<ItemT, OutT> CompiledDataList for CList<ItemT, Option<OutT>> {
    type Item = OutT;

    fn expected_len(&self) -> usize {
        self.output.len()
    }

    fn data_iter(&self) -> impl Iterator<Item = &Self::Item> {
        self.output.iter().flatten()
    }
}

struct Sender {
    sender: fltk::app::Sender<GuiMessage>,
    audio_sender: mpsc::Sender<AudioMessage>,
}

impl Sender {
    fn send(&self, m: CompilerOutput) {
        self.sender.send(GuiMessage::FromCompiler(m))
    }

    fn send_audio(&self, m: AudioMessage) {
        match self.audio_sender.send(m) {
            Ok(()) => (),
            Err(_) => panic!("Cannot send message to audio thread"),
        }
    }
}

fn create_instrument_compiler<'a>(
    sample_file_cache: &'a mut SampleFileCache,
    sender: &'a Sender,
) -> impl (FnMut(ItemId, &data::Instrument) -> Option<InstrumentSampleData>) + 'a {
    |id, inst| match load_sample_for_instrument(inst, sample_file_cache) {
        Ok(s) => {
            sender.send(CompilerOutput::Instrument(
                id,
                Ok(InstrumentSize(s.sample_size())),
            ));
            Some(s)
        }
        Err(e) => {
            sender.send(CompilerOutput::Instrument(id, Err(e)));
            None
        }
    }
}

fn create_sample_compiler<'a>(
    sample_file_cache: &'a mut SampleFileCache,
    sender: &'a Sender,
) -> impl (FnMut(ItemId, &data::Sample) -> Option<SampleSampleData>) + 'a {
    |id, sample| match load_sample_for_sample(sample, sample_file_cache) {
        Ok(s) => {
            sender.send(CompilerOutput::Sample(id, Ok(SampleSize(s.sample_size()))));
            Some(s)
        }
        Err(e) => {
            sender.send(CompilerOutput::Sample(id, Err(e)));
            None
        }
    }
}

fn build_play_instrument_data(
    instruments: &CList<data::Instrument, Option<InstrumentSampleData>>,
    id: ItemId,
    args: PlaySampleArgs,
) -> Option<(Box<CommonAudioData>, SongData)> {
    let sample = match instruments.get_output_for_id(&id) {
        Some(Some(s)) => s,
        _ => return None,
    };

    let (sample_data, max_note) = create_test_instrument_data(sample)?;

    if args.note > max_note {
        return None;
    }

    let blank_sfx = blank_compiled_sound_effects();
    let blank_sfx_subroutines = CompiledSfxSubroutines::blank();
    let common_audio_data =
        Box::new(build_common_audio_data(&sample_data, &blank_sfx_subroutines, &blank_sfx).ok()?);
    let song_data =
        test_sample_song(0, args.note, args.note_length, args.envelope, &sample_data).ok()?;

    Some((common_audio_data, song_data))
}

fn build_play_sample_data(
    samples: &CList<data::Sample, Option<SampleSampleData>>,
    id: ItemId,
    args: PlaySampleArgs,
) -> Option<(Box<CommonAudioData>, SongData)> {
    let sample = match samples.get_output_for_id(&id) {
        Some(Some(s)) => s.clone(),
        _ => return None,
    };

    let sample_data = combine_samples([].as_slice(), [sample].as_slice()).ok()?;

    let blank_sfx = blank_compiled_sound_effects();
    let blank_sfx_subroutines = CompiledSfxSubroutines::blank();
    let common_audio_data =
        Box::new(build_common_audio_data(&sample_data, &blank_sfx_subroutines, &blank_sfx).ok()?);
    let song_data =
        test_sample_song(0, args.note, args.note_length, args.envelope, &sample_data).ok()?;

    Some((common_audio_data, song_data))
}

fn instrument_and_sample_names(
    instruments: &CList<data::Instrument, Option<InstrumentSampleData>>,
    samples: &CList<data::Sample, Option<SampleSampleData>>,
) -> Arc<InstrumentAndSampleNames> {
    let i_names = instruments.items().iter().map(|inst| inst.name.clone());
    let s_names = samples.items().iter().map(|s| s.name.clone());

    Arc::new(InstrumentAndSampleNames(i_names.chain(s_names).collect()))
}

fn compile_sfx_subroutines(
    dependencies: &Option<SongDependencies>,
    mml: &SfxSubroutinesMml,
    sender: &Sender,
) -> Option<Arc<CompiledSfxSubroutines>> {
    let dep = match dependencies.as_ref() {
        Some(d) => d,
        None => {
            sender.send(CompilerOutput::SfxSubroutines(Err(
                SfxSubroutinesError::DependencyInstruments,
            )));
            return None;
        }
    };

    match compiler::sound_effects::compile_sfx_subroutines(
        mml,
        &dep.inst_map,
        dep.combined_samples.pitch_table(),
    ) {
        Ok(s) => {
            let s = Arc::new(s);
            sender.send(CompilerOutput::SfxSubroutines(Ok(s.clone())));
            Some(s)
        }
        Err(e) => {
            sender.send(CompilerOutput::SfxSubroutines(Err(
                SfxSubroutinesError::Error(e),
            )));
            None
        }
    }
}

fn create_sfx_compiler<'a>(
    dependencies: &'a Option<SongDependencies>,
    sfx_subroutines: &'a Option<Arc<CompiledSfxSubroutines>>,
    sender: &'a Sender,
) -> impl (Fn(ItemId, &SoundEffectInput) -> Option<Arc<CompiledSoundEffect>>) + 'a {
    move |id, sfx| {
        let dep = match dependencies.as_ref() {
            Some(d) => d,
            None => {
                sender.send(CompilerOutput::SoundEffect(
                    id,
                    Err(SfxError::DependencyInstruments),
                ));
                return None;
            }
        };
        let sfx_subroutines = match sfx_subroutines.as_ref() {
            Some(s) => s,
            None => {
                sender.send(CompilerOutput::SoundEffect(
                    id,
                    Err(SfxError::DependencySfxSubroutines),
                ));
                return None;
            }
        };
        match compile_sound_effect_input(
            sfx,
            &dep.inst_map,
            dep.combined_samples.pitch_table(),
            sfx_subroutines,
        ) {
            Ok(sfx) => {
                let sfx = Arc::from(sfx);
                sender.send(CompilerOutput::SoundEffect(id, Ok(sfx.clone())));
                Some(sfx)
            }
            Err(e) => {
                sender.send(CompilerOutput::SoundEffect(id, Err(SfxError::Error(e))));
                None
            }
        }
    }
}

fn count_missing_sfx(
    sfx_export_order: &GuiSfxExportOrder,
    sound_effects: &CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>>,
    sender: &Sender,
) {
    if sound_effects.items().is_empty() {
        let n_missing = sfx_export_order.n_sound_effects();
        sender.send(CompilerOutput::NumberOfMissingSoundEffects(n_missing));
        return;
    }

    let n_missing = sfx_export_order
        .export_order()
        .iter()
        .filter(|name| !sound_effects.name_map().contains_key(name))
        .count();

    sender.send(CompilerOutput::NumberOfMissingSoundEffects(n_missing));
}

fn calc_sfx_data_size(
    sfx_export_order: &GuiSfxExportOrder,
    sfx_subroutines: &Option<Arc<CompiledSfxSubroutines>>,
    sound_effects: &CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>>,
) -> usize {
    let sfx_size: usize = sfx_export_order
        .export_order()
        .iter()
        .filter_map(|name| sound_effects.get_output_for_name(name))
        .filter_map(Option::as_ref)
        .map(|o| o.bytecode().len())
        .sum();

    let sfx_table_size = sfx_export_order.n_sound_effects() * COMMON_DATA_BYTES_PER_SOUND_EFFECT;

    let sfx_subroutines = match sfx_subroutines {
        Some(s) => s.cad_data_len(),
        None => 0,
    };

    sfx_table_size + sfx_size + sfx_subroutines
}

struct SongDependencies {
    inst_map: data::UniqueNamesList<data::InstrumentOrSample>,
    combined_samples: SampleAndInstrumentData,
    common_data_no_sfx_size: usize,
    sfx_data_size: usize,
}

impl SongDependencies {
    fn common_data_size(&self) -> usize {
        self.common_data_no_sfx_size + self.sfx_data_size
    }
}

fn build_cad_no_sfx_and_song_dependencies(
    instruments: &CList<data::Instrument, Option<InstrumentSampleData>>,
    samples: &CList<data::Sample, Option<SampleSampleData>>,
    instrument_and_sample_names: &Arc<InstrumentAndSampleNames>,
    sfx_export_order: &GuiSfxExportOrder,
    sfx_subroutines: &Option<Arc<CompiledSfxSubroutines>>,
    sound_effects: &CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>>,
) -> Result<(Arc<CommonAudioDataNoSfx>, SongDependencies), CombineSamplesError> {
    // Test all instruments and samples are compiled
    let n_instrument_errors = instruments.count_errors();
    let n_sample_errors = samples.count_errors();
    if n_instrument_errors + n_sample_errors > 0 {
        return Err(CombineSamplesError::IndividualErrors {
            n_instrument_errors,
            n_sample_errors,
        });
    }

    let combined_samples = match combine_samples(instruments, samples) {
        Ok(s) => s,
        Err(e) => {
            return Err(CombineSamplesError::CombineError(e));
        }
    };

    let cad = match build_common_audio_data(
        &combined_samples,
        &CompiledSfxSubroutines::blank(),
        &blank_compiled_sound_effects(),
    ) {
        Ok(c) => c,
        Err(e) => return Err(CombineSamplesError::CommonAudioData(e)),
    };

    match data::validate_instrument_and_sample_names(
        instruments.items().iter(),
        samples.items.iter(),
    ) {
        Ok(instruments) => {
            let sd = SongDependencies {
                inst_map: instruments,
                combined_samples,
                common_data_no_sfx_size: cad.data().len(),
                sfx_data_size: calc_sfx_data_size(sfx_export_order, sfx_subroutines, sound_effects),
            };
            Ok((
                Arc::new(CommonAudioDataNoSfx(
                    cad,
                    instrument_and_sample_names.clone(),
                )),
                sd,
            ))
        }
        Err(e) => Err(CombineSamplesError::UniqueNamesError(e)),
    }
}

fn build_common_data_with_sfx_buffer(
    dep: &Option<SongDependencies>,
    instrument_and_sample_names: &Arc<InstrumentAndSampleNames>,
    sfx_subroutines: &Option<Arc<CompiledSfxSubroutines>>,
) -> Option<Result<Arc<CommonAudioDataWithSfxBuffer>, CombineSamplesError>> {
    let combined_samples = match dep {
        Some(d) => &d.combined_samples,
        None => return None,
    };
    let sfx_subroutines = match sfx_subroutines {
        Some(sfx) => sfx,
        None => return None,
    };

    match build_cad_with_sfx_buffer(combined_samples, sfx_subroutines) {
        Ok(cad) => Some(Ok(Arc::new(CommonAudioDataWithSfxBuffer(
            cad,
            instrument_and_sample_names.clone(),
        )))),
        Err(e) => Some(Err(CombineSamplesError::CommonAudioData(e))),
    }
}

impl CompiledSfxMap for CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>> {
    fn is_empty(&self) -> bool {
        self.output.is_empty()
    }

    fn get(&self, name: &data::Name) -> Option<&CompiledSoundEffect> {
        self.get_output_for_name(name)?.as_deref()
    }
}

/// Returns `(CommonAudioDataWithSize, sfx_data_size)`
fn build_common_audio_data_with_sfx(
    dep: &Option<SongDependencies>,
    instrument_and_sample_names: &Arc<InstrumentAndSampleNames>,
    sfx_export_order: &Arc<GuiSfxExportOrder>,
    sfx_subroutines: &Option<Arc<CompiledSfxSubroutines>>,
    sound_effects: &CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>>,
    default_sfx_flags: DefaultSfxFlags,
) -> (Option<Arc<CommonAudioDataWithSfx>>, usize) {
    let sfx_data =
        combine_sound_effects(sound_effects, sfx_export_order.deref(), default_sfx_flags);

    // Always try to calculate sfx_data size (to keep song size checks and Project tab up-to-date)
    let sfx_data_size = match &sfx_data {
        Ok(s) => s.sfx_data_size(),
        _ => calc_sfx_data_size(sfx_export_order, sfx_subroutines, sound_effects),
    };

    let cad = match (&dep, sfx_subroutines, &sfx_data) {
        (Some(dep), Some(sfx_sub), Ok(sd)) => {
            match build_common_audio_data(&dep.combined_samples, sfx_sub, sd) {
                Ok(cad) => Some(Arc::new(CommonAudioDataWithSfx {
                    common_audio_data: cad,
                    sfx_export_order: sfx_export_order.clone(),
                    instrument_and_sample_names: instrument_and_sample_names.clone(),
                })),
                Err(_) => None,
            }
        }
        _ => None,
    };

    (cad, sfx_data_size)
}

struct SongState {
    file: TextFile,
    song_data: Option<Arc<SongData>>,
}
struct SongCompiler {
    parent_path: ParentPathBuf,
    songs: HashMap<ItemId, SongState>,
}

impl SongCompiler {
    fn new(parent_path: ParentPathBuf) -> Self {
        Self {
            parent_path,
            songs: HashMap::new(),
        }
    }

    fn get_song_data(&self, id: &ItemId) -> Option<&Arc<SongData>> {
        match self.songs.get(id) {
            Some(s) => s.song_data.as_ref(),
            None => None,
        }
    }

    // Will return a SongData if the song is too large
    // (so the size can be retested when the sound effects are changed)
    fn compile_song(
        id: ItemId,
        name: Option<&data::Name>,
        f: &TextFile,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) -> Option<Arc<SongData>> {
        let dep = match dependencies.as_ref() {
            Some(d) => d,
            None => {
                sender.send(CompilerOutput::Song(id, Err(SongError::Dependency)));
                return None;
            }
        };

        let name = name.cloned();
        let song_data = match compiler::songs::compile_mml_song(
            &f.contents,
            &f.file_name,
            name,
            &dep.inst_map,
            dep.combined_samples.pitch_table(),
        ) {
            Ok(sd) => Arc::from(sd),
            Err(e) => {
                sender.send(CompilerOutput::Song(id, Err(SongError::Song(e))));
                return None;
            }
        };

        match compiler::songs::validate_song_size(&song_data, dep.common_data_size()) {
            Ok(()) => {
                sender.send(CompilerOutput::Song(id, Ok(song_data.clone())));
            }
            Err(e) => {
                sender.send(CompilerOutput::Song(id, Err(SongError::TooLarge(e))));
            }
        }

        Some(song_data)
    }

    fn load_and_compile_song(
        &self,
        id: ItemId,
        source_path: &SourcePathBuf,
        pf_songs: &IList<data::Song>,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) -> SongState {
        let song_name = pf_songs.get(&id).map(|s| &s.name);

        let file = match load_text_file_with_limit(source_path, &self.parent_path) {
            Ok(f) => f,
            Err(_) => TextFile {
                file_name: source_path.file_name().to_owned(),
                path: None,
                contents: String::new(),
            },
        };

        SongState {
            song_data: Self::compile_song(id, song_name, &file, dependencies, sender),
            file,
        }
    }

    fn replace_all_and_load_songs(&mut self, pf_songs: &ReplaceAllVec<data::Song>) {
        self.songs = pf_songs
            .0
            .iter()
            .filter_map(|(id, item)| {
                match load_text_file_with_limit(&item.source, &self.parent_path) {
                    Ok(file) => Some((
                        *id,
                        SongState {
                            song_data: None,
                            file,
                        },
                    )),
                    Err(_) => None,
                }
            })
            .collect();
    }

    fn process_pf_song_message(
        &mut self,
        m: &ItemChanged<data::Song>,
        pf_songs: &IList<data::Song>,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) {
        let mut add_or_edit = |id: &ItemId, item: &data::Song| {
            // Only add songs, do not modify them
            // (source is not editable by the GUI)
            #[allow(clippy::map_entry)] // Cannot use HashMap::entry() due to the borrow checker
            if !self.songs.contains_key(id) {
                self.songs.insert(
                    *id,
                    self.load_and_compile_song(*id, &item.source, pf_songs, dependencies, sender),
                );
            }
        };

        match m {
            ItemChanged::AddedOrEdited(id, item) => {
                add_or_edit(id, item);
            }
            ItemChanged::MultipleAddedOrEdited(vec) => {
                for (id, item) in vec {
                    add_or_edit(id, item)
                }
            }
            ItemChanged::Removed(id) => {
                self.songs.remove(id);
            }
        }

        self.output_largest_song_size(sender);
    }

    fn song_tab_closed(
        &mut self,
        id: ItemId,
        pf_songs: &IList<data::Song>,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) {
        match pf_songs.get(&id) {
            Some(pf_song) => {
                // The song-tab may have been closed without saving it, reload the song file.
                self.songs.insert(
                    id,
                    self.load_and_compile_song(id, &pf_song.source, pf_songs, dependencies, sender),
                );
            }
            None => {
                self.songs.remove(&id);
            }
        }

        self.output_largest_song_size(sender);
    }

    fn compile_all_songs(
        &mut self,
        pf_songs: &IList<data::Song>,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) {
        for (id, s) in self.songs.iter_mut() {
            let song_name = pf_songs.get(id).map(|s| &s.name);

            s.song_data = Self::compile_song(*id, song_name, &s.file, dependencies, sender);
        }

        self.output_largest_song_size(sender);
    }

    fn recheck_song_sizes(&mut self, dependencies: &SongDependencies, sender: &Sender) {
        let common_data_size = dependencies.common_data_size();

        for (id, s) in self.songs.iter() {
            if let Some(song_data) = &s.song_data {
                match compiler::songs::validate_song_size(song_data, common_data_size) {
                    Ok(()) => {}
                    Err(e) => {
                        sender.send(CompilerOutput::Song(*id, Err(SongError::TooLarge(e))));
                    }
                }
            }
        }
    }

    fn edit_and_compile_song(
        &mut self,
        id: ItemId,
        mml: String,
        pf_songs: &IList<data::Song>,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) {
        let song_name = pf_songs.get(&id).map(|s| &s.name);

        match self.songs.entry(id) {
            Entry::Occupied(mut o) => {
                let state = o.get_mut();
                state.file.contents = mml;
                state.song_data =
                    Self::compile_song(id, song_name, &state.file, dependencies, sender)
            }
            Entry::Vacant(v) => {
                let file = TextFile {
                    contents: mml,
                    file_name: "MML".to_owned(),
                    path: None,
                };
                let song_data = Self::compile_song(id, song_name, &file, dependencies, sender);
                v.insert(SongState { file, song_data });
            }
        }

        self.output_largest_song_size(sender);
    }

    fn output_largest_song_size(&self, sender: &Sender) {
        let max_total_size = self
            .songs
            .iter()
            .filter_map(|(_k, v)| v.song_data.as_ref())
            .map(|s| s.song_aram_size())
            .max_by_key(|s| s.total_size())
            .unwrap_or(BLANK_SONG_ARAM_SIZE);

        sender.send(CompilerOutput::LargestSongSize(max_total_size));
    }

    fn export_to_spc_file(
        &self,
        id: ItemId,
        pf_songs: &IList<data::Song>,
        sd: &Option<SongDependencies>,
    ) -> Result<(String, Box<[u8]>), SpcFileError> {
        let common_audio_data = match sd {
            None => return Err(SpcFileError::NoSamples),
            Some(sd) => {
                match build_common_audio_data(
                    &sd.combined_samples,
                    &CompiledSfxSubroutines::blank(),
                    &blank_compiled_sound_effects(),
                ) {
                    Ok(cad) => cad,
                    Err(e) => return Err(SpcFileError::CommonAudioDataError(e)),
                }
            }
        };

        let (title, song_data) = match self.songs.get(&id) {
            None => return Err(SpcFileError::NoSong),
            Some(s) => match &s.song_data {
                None => return Err(SpcFileError::InvalidSong),
                Some(song_data) => {
                    let title = song_data.metadata().title.as_deref();
                    (title, song_data)
                }
            },
        };

        match export_spc_file(&common_audio_data, song_data) {
            Err(e) => Err(SpcFileError::Spc(e)),
            Ok(spc_data) => {
                let name = title
                    .or_else(|| pf_songs.get(&id).map(|s| s.name.as_str()))
                    .unwrap_or("Song");

                Ok((name.to_owned(), spc_data))
            }
        }
    }
}

fn calculate_cursor_song_driver_state(
    cad: &Option<Arc<CommonAudioDataNoSfx>>,
    songs: &SongCompiler,
    id: ItemId,
    cursor_index: u32,
    sender: &Sender,
) {
    sender.send(CompilerOutput::SongCursorDriverState(
        match (cad, songs.get_song_data(&id)) {
            (Some(cad), Some(song)) => match find_cursor_state(song.as_ref(), cursor_index) {
                Some((ChannelId::Channel(_), tc, _)) => {
                    let mut si = Box::new(SongInterpreter::new_zero(
                        SiCad::NoSfx(cad.clone()),
                        song.clone(),
                        cad.0.min_song_data_addr(),
                        driver_constants::AudioMode::Surround,
                    ));

                    match si.process_song_skip_ticks(tc.ticks + TickCounter::new(2)) {
                        true => CursorDriverState::CursorSong(id, si),
                        false => CursorDriverState::BcTimeout(id),
                    }
                }
                Some((ChannelId::Subroutine(subroutine_index), tc, _)) => {
                    match SongInterpreter::new_song_subroutine(
                        SiCad::NoSfx(cad.clone()),
                        song.clone(),
                        cad.0.min_song_data_addr(),
                        None,
                        subroutine_index,
                        driver_constants::AudioMode::Surround,
                    ) {
                        Ok(mut si) => {
                            match si.process_song_skip_ticks(tc.ticks + TickCounter::new(2)) {
                                true => CursorDriverState::Subroutine(id, Box::new(si)),
                                false => CursorDriverState::BcTimeout(id),
                            }
                        }
                        Err(_) => CursorDriverState::BcError(id),
                    }
                }
                _ => CursorDriverState::NoCursor(id),
            },
            _ => CursorDriverState::NoSong(id),
        },
    ));
}

fn calculate_tick_song_driver_state(
    cad: &Option<Arc<CommonAudioDataNoSfx>>,
    songs: &SongCompiler,
    id: ItemId,
    tick: TickCounter,
    sender: &Sender,
) {
    let tick = tick + TickCounter::new(1);

    sender.send(CompilerOutput::SongCursorDriverState(
        match (cad, songs.get_song_data(&id)) {
            (Some(cad), Some(song)) => {
                let mut si = Box::new(SongInterpreter::new_zero(
                    SiCad::NoSfx(cad.clone()),
                    song.clone(),
                    cad.0.min_song_data_addr(),
                    driver_constants::AudioMode::Surround,
                ));

                match si.process_song_skip_ticks(tick) {
                    true => CursorDriverState::ManualTicks(id, si),
                    false => CursorDriverState::BcTimeout(id),
                }
            }
            _ => CursorDriverState::NoSong(id),
        },
    ));
}

fn analyse_sample(
    cache: &mut SampleFileCache,
    source: SourcePathBuf,
    loop_setting: LoopSetting,
    evaluator: BrrEvaluator,
    fft_settings: FftSettings,
) -> Result<SampleAnalysis, BrrError> {
    let brr_sample = Arc::new(encode_or_load_brr_file(
        &source,
        cache,
        &loop_setting,
        evaluator,
    )?);

    let wav_sample = match source.extension() {
        Some(WAV_EXTENSION) => match cache.load_wav_file(&source) {
            Ok(w) => Some(w),
            Err(e) => return Err(e.clone()),
        },
        _ => None,
    };

    Ok(sample_analyser::analyse_sample(
        brr_sample,
        wav_sample,
        fft_settings,
    ))
}

fn compile_all_samples(
    instruments: &mut CList<data::Instrument, Option<InstrumentSampleData>>,
    samples: &mut CList<data::Sample, Option<SampleSampleData>>,
    sample_file_cache: &mut SampleFileCache,
    sender: &Sender,
) {
    let c = create_instrument_compiler(sample_file_cache, sender);
    instruments.recompile_all(c);

    let c = create_sample_compiler(sample_file_cache, sender);
    samples.recompile_all(c);
}

fn bg_thread(
    parent_path: ParentPathBuf,
    receiever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<GuiMessage>,
    audio_sender: mpsc::Sender<AudioMessage>,
) {
    let sender = Sender {
        sender,
        audio_sender,
    };

    let mut default_sfx_flags = DefaultSfxFlags::default();

    // Editable sfx export order
    let mut sfx_eo = GuiSfxExportOrder::default();

    // Sfx export order stored
    // Using an Arc to reduce clones and allow the GUI to detect if the sfx_export_order in CommonAudioDataWithSfx has changed.
    let mut sfx_export_order = Arc::new(sfx_eo.clone());

    let mut pf_songs = IList::new();
    let mut instruments = CList::new();
    let mut samples = CList::new();
    let mut sfx_subroutines_mml = SfxSubroutinesMml(String::new());
    let mut sfx_subroutines = None;
    let mut sound_effects = CList::new();
    let mut songs = SongCompiler::new(parent_path.clone());

    let mut sample_file_cache = SampleFileCache::new(parent_path);

    let mut song_dependencies = None;
    let mut cad_with_sfx_buffer: Option<Arc<CommonAudioDataWithSfxBuffer>> = None;
    let mut cad_no_sfx: Option<Arc<CommonAudioDataNoSfx>> = None;

    let mut inst_sample_names = Arc::default();

    // Used to test if a sound effect was edited in the sfx tab.
    let mut cad_with_sfx_out_of_date = true;

    let mut pending_combine_samples = false;
    let mut pending_compile_all_sfx = false;
    let mut pending_build_cad_with_sfx = false;
    let mut pending_compile_all_songs = false;

    while let Ok(m) = receiever.recv() {
        match m {
            ToCompiler::LoadProject(p) => {
                default_sfx_flags = p.default_sfx_flags;
                sfx_eo = p.sfx_export_order;

                songs.replace_all_and_load_songs(&p.pf_songs);

                pf_songs.replace_all(p.pf_songs);
                instruments.replace_all(p.instruments);
                samples.replace_all(p.samples);

                sfx_export_order = Arc::new(sfx_eo.clone());
                inst_sample_names = instrument_and_sample_names(&instruments, &samples);

                compile_all_samples(
                    &mut instruments,
                    &mut samples,
                    &mut sample_file_cache,
                    &sender,
                );

                pending_combine_samples = true;
                pending_compile_all_songs = true;
            }
            ToCompiler::LoadSoundEffects(sfx) => {
                sfx_subroutines_mml = sfx.subroutines;

                sound_effects.replace_all(sfx.sound_effects);
                count_missing_sfx(&sfx_export_order, &sound_effects, &sender);
                pending_compile_all_sfx = true;
            }

            ToCompiler::ClearSampleCacheAndRebuild => {
                sample_file_cache.clear_cache();
                compile_all_samples(
                    &mut instruments,
                    &mut samples,
                    &mut sample_file_cache,
                    &sender,
                );

                pending_combine_samples = true;
            }

            ToCompiler::DefaultSfxFlagChanged(flags) => {
                default_sfx_flags = flags;
                pending_build_cad_with_sfx = true;
            }
            ToCompiler::SfxExportOrder(a) => {
                sfx_eo.process(&a);
                sfx_export_order = Arc::new(sfx_eo.clone());

                count_missing_sfx(&sfx_export_order, &sound_effects, &sender);

                pending_build_cad_with_sfx = true;
            }
            ToCompiler::ProjectSongs(m) => {
                songs.process_pf_song_message(&m, &pf_songs, &song_dependencies, &sender);
                pf_songs.process_message(m);
            }
            ToCompiler::Instrument(m) => {
                let name_changed = instruments.item_changes_name(&m);

                let c = create_instrument_compiler(&mut sample_file_cache, &sender);
                instruments.process_message(m, c);

                if name_changed {
                    inst_sample_names = instrument_and_sample_names(&instruments, &samples);
                }
                song_dependencies = None;
                pending_combine_samples = true;
            }
            ToCompiler::Sample(m) => {
                let name_changed = samples.item_changes_name(&m);

                let c = create_sample_compiler(&mut sample_file_cache, &sender);
                samples.process_message(m, c);

                if name_changed {
                    inst_sample_names = instrument_and_sample_names(&instruments, &samples);
                }
                song_dependencies = None;
                pending_combine_samples = true;
            }
            ToCompiler::RecompileInstrumentsUsingSample(source_path) => {
                let c = create_instrument_compiler(&mut sample_file_cache, &sender);
                instruments.recompile_all_if(c, |inst| inst.source == source_path);

                let c = create_sample_compiler(&mut sample_file_cache, &sender);
                samples.recompile_all_if(c, |inst| inst.source == source_path);

                song_dependencies = None;
                pending_combine_samples = true;
            }

            ToCompiler::CompileSoundEffectSubroutines(m) => {
                sfx_subroutines_mml = m;
                pending_compile_all_sfx = true;
            }
            ToCompiler::SoundEffects(m) => {
                let name_changed = sound_effects.item_changes_name(&m);

                let c = create_sfx_compiler(&song_dependencies, &sfx_subroutines, &sender);
                sound_effects.process_message(m, c);

                if name_changed {
                    count_missing_sfx(&sfx_export_order, &sound_effects, &sender);
                }

                sender.send_audio(AudioMessage::CommandAudioDataWithSfxChanged(None));

                cad_with_sfx_out_of_date = true;
            }
            ToCompiler::FinishedEditingSoundEffects => {
                if cad_with_sfx_out_of_date {
                    pending_build_cad_with_sfx = true;
                }
            }

            ToCompiler::PlaySongWithSfxBuffer(id, ticks) => {
                if let Some(song) = songs.get_song_data(&id) {
                    let song_ticks = song.max_tick_count();

                    // Ensure at least some of the song is audible when the user presses play
                    let max_ticks = match song.is_looping() {
                        true => song_ticks.value() * 2 / 3,
                        false => song_ticks.value() / 10 * 9,
                    };
                    let ticks = ticks.value().clamp(0, max_ticks);

                    sender.send_audio(AudioMessage::PlaySongWithSfxBuffer(
                        id,
                        song.clone(),
                        TickCounter::new(ticks),
                    ));
                }
            }
            ToCompiler::PlaySfxUsingSfxBuffer(id, pan) => {
                if let Some(Some(sfx_data)) = sound_effects.get_output_for_id(&id) {
                    sender.send_audio(AudioMessage::PlaySfxUsingSfxBuffer(sfx_data.clone(), pan));
                }
            }

            ToCompiler::SongTabClosed(id) => {
                songs.song_tab_closed(id, &pf_songs, &song_dependencies, &sender);
            }
            ToCompiler::SongChanged {
                id,
                mml,
                cursor_index,
            } => {
                songs.edit_and_compile_song(id, mml, &pf_songs, &song_dependencies, &sender);
                if let Some(cursor_index) = cursor_index {
                    calculate_cursor_song_driver_state(
                        &cad_no_sfx,
                        &songs,
                        id,
                        cursor_index,
                        &sender,
                    );
                }
            }
            ToCompiler::SongCursorMoved { id, cursor_index } => {
                calculate_cursor_song_driver_state(&cad_no_sfx, &songs, id, cursor_index, &sender);
            }
            ToCompiler::CalcSongDriverState(id, tick) => {
                calculate_tick_song_driver_state(&cad_no_sfx, &songs, id, tick, &sender);
            }

            ToCompiler::CompileAndPlaySong(id, mml, skip, channels_mask) => {
                sender.send_audio(AudioMessage::Pause);
                songs.edit_and_compile_song(id, mml, &pf_songs, &song_dependencies, &sender);
                if let Some(song) = songs.get_song_data(&id) {
                    sender.send_audio(AudioMessage::PlaySong(
                        id,
                        song.clone(),
                        skip,
                        channels_mask,
                    ));
                }
            }
            ToCompiler::CompileAndPlaySongSubroutine(id, mml, mml_prefix, sid, skip) => {
                sender.send_audio(AudioMessage::Pause);
                songs.edit_and_compile_song(id, mml, &pf_songs, &song_dependencies, &sender);

                if let (Some(song), Some(d)) = (songs.get_song_data(&id), &song_dependencies) {
                    match mml_prefix {
                        Some(prefix) => {
                            match compile_mml_prefix(
                                &prefix,
                                song,
                                d.combined_samples.pitch_table(),
                                &d.inst_map,
                            ) {
                                Ok(prefix) => {
                                    sender.send(CompilerOutput::SongPrefix(id, Ok(())));

                                    sender.send_audio(AudioMessage::PlaySongSubroutine(
                                        id,
                                        song.clone(),
                                        Some(prefix),
                                        sid,
                                        skip,
                                    ));
                                }
                                Err(e) => {
                                    sender.send(CompilerOutput::SongPrefix(id, Err(e)));
                                }
                            }
                        }
                        None => {
                            sender.send_audio(AudioMessage::PlaySongSubroutine(
                                id,
                                song.clone(),
                                None,
                                sid,
                                skip,
                            ));
                        }
                    };
                }
            }
            ToCompiler::PlayInstrument(id, args) => {
                if let Some((c_data, s_data)) = build_play_instrument_data(&instruments, id, args) {
                    sender.send_audio(AudioMessage::PlaySample(c_data, s_data.into()));
                }
            }
            ToCompiler::PlaySample(id, args) => {
                if let Some((c_data, s_data)) = build_play_sample_data(&samples, id, args) {
                    sender.send_audio(AudioMessage::PlaySample(c_data, s_data.into()));
                }
            }

            ToCompiler::ExportSongToSpcFile(id) => {
                let r = songs.export_to_spc_file(id, &pf_songs, &song_dependencies);
                sender.send(CompilerOutput::SpcFileResult(r));
            }

            ToCompiler::RemoveFileFromSampleCache(source_path) => {
                sample_file_cache.remove_path(&source_path);
            }

            ToCompiler::AnalyseSample(source_path, loop_setting, evaluator, fft_settings) => {
                let r = analyse_sample(
                    &mut sample_file_cache,
                    source_path,
                    loop_setting,
                    evaluator,
                    fft_settings,
                );
                sender.send(CompilerOutput::SampleAnalysis(r));
            }
        }

        // This ensures only 1 CadOutput is sent to the GUI thread per event
        let mut pending_cad_output = CadOutput::None;

        if pending_combine_samples {
            pending_combine_samples = false;

            match build_cad_no_sfx_and_song_dependencies(
                &instruments,
                &samples,
                &inst_sample_names,
                &sfx_export_order,
                &sfx_subroutines,
                &sound_effects,
            ) {
                Ok((cad, sd)) => {
                    cad_no_sfx = Some(cad.clone());

                    pending_cad_output = CadOutput::NoSfx(cad);
                    song_dependencies = Some(sd);
                }
                Err(e) => {
                    cad_no_sfx = None;
                    pending_cad_output = CadOutput::Err(e);
                    song_dependencies = None;
                }
            }

            sender.send_audio(AudioMessage::CommonAudioDataChanged(cad_no_sfx.clone()));

            cad_with_sfx_buffer = None;
            pending_compile_all_sfx = true;
            pending_build_cad_with_sfx = true;
            pending_compile_all_songs = true;
        }

        if pending_compile_all_sfx {
            pending_compile_all_sfx = false;

            sfx_subroutines =
                compile_sfx_subroutines(&song_dependencies, &sfx_subroutines_mml, &sender);

            let c = create_sfx_compiler(&song_dependencies, &sfx_subroutines, &sender);
            sound_effects.recompile_all(c);

            match build_common_data_with_sfx_buffer(
                &song_dependencies,
                &inst_sample_names,
                &sfx_subroutines,
            ) {
                Some(Ok(c)) => {
                    pending_cad_output = CadOutput::SfxBuffer(c.clone());

                    cad_with_sfx_buffer = Some(c);
                }
                Some(Err(e)) => {
                    pending_cad_output = CadOutput::Err(e);
                    cad_with_sfx_buffer = None;
                }
                None => {
                    cad_with_sfx_buffer = None;
                }
            }

            sender.send_audio(AudioMessage::CommonAudioDataSfxBufferChanged(
                cad_with_sfx_buffer.clone(),
            ));

            pending_build_cad_with_sfx = true;
        }

        if pending_build_cad_with_sfx {
            pending_build_cad_with_sfx = false;

            let (cad, sfx_data_size) = build_common_audio_data_with_sfx(
                &song_dependencies,
                &inst_sample_names,
                &sfx_export_order,
                &sfx_subroutines,
                &sound_effects,
                default_sfx_flags,
            );

            if let Some(deps) = &mut song_dependencies {
                if deps.sfx_data_size != sfx_data_size {
                    deps.sfx_data_size = sfx_data_size;

                    if !pending_compile_all_songs {
                        songs.recheck_song_sizes(deps, &sender);
                    }
                }
            }

            if let Some(c) = &cad {
                pending_cad_output = CadOutput::WithSfx(c.clone());
            } else if let Some(c) = &cad_with_sfx_buffer {
                // Restores CadOutput to NoSfx if there is an sfx error and the instrument/samples are OK
                pending_cad_output = CadOutput::SfxBuffer(c.clone());
            } else if let Some(c) = &cad_no_sfx {
                pending_cad_output = CadOutput::NoSfx(c.clone());
            }

            sender.send_audio(AudioMessage::CommandAudioDataWithSfxChanged(cad));

            cad_with_sfx_out_of_date = false;
        }

        if !matches!(pending_cad_output, CadOutput::None) {
            sender.send(CompilerOutput::CommonAudioData(pending_cad_output));
        }

        if pending_compile_all_songs {
            pending_compile_all_songs = false;

            songs.compile_all_songs(&pf_songs, &song_dependencies, &sender);
        }
    }
}

fn monitor_thread(
    parent_path: ParentPathBuf,
    reciever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<GuiMessage>,
    audio_sender: mpsc::Sender<AudioMessage>,
) {
    let s = sender;

    let handler = thread::Builder::new()
        .name("compiler_thread".into())
        .spawn(move || bg_thread(parent_path, reciever, sender, audio_sender))
        .unwrap();

    match handler.join() {
        Ok(()) => (),
        Err(e) => {
            // `std::panic::PanicInfo::payload()` mentions panics are commonly `&'static str` or `String`.
            let msg = match e.downcast_ref::<&str>() {
                Some(s) => s,
                None => match e.downcast_ref::<String>() {
                    Some(s) => s.as_str(),
                    None => "Unknown panic type",
                },
            };
            s.send(GuiMessage::FromCompiler(CompilerOutput::Panic(
                msg.to_owned(),
            )));
        }
    }
}

pub fn create_bg_thread(
    parent_path: ParentPathBuf,
    reciever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<GuiMessage>,
    audio_sender: mpsc::Sender<AudioMessage>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || monitor_thread(parent_path, reciever, sender, audio_sender))
}
