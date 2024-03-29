//! Background compiler thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::names::NameGetter;
use crate::sample_analyser::{self, SampleAnalysis};
use crate::GuiMessage;

use crate::audio_thread::{AudioMessage, ChannelsMask, SongSkip};

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;
use std::sync::{mpsc, Arc};
use std::thread;

use compiler::common_audio_data::{build_common_audio_data, CommonAudioData};
use compiler::data::{self, LoopSetting};
use compiler::data::{load_text_file_with_limit, TextFile};
use compiler::driver_constants::COMMON_DATA_BYTES_PER_SOUND_EFFECT;
use compiler::envelope::Envelope;
use compiler::errors::{self, BrrError, ExportSpcFileError, ProjectFileErrors, SongTooLargeError};
use compiler::notes::Note;
use compiler::path::{ParentPathBuf, SourcePathBuf};
use compiler::pitch_table::PitchTable;
use compiler::samples::{
    combine_samples, create_test_instrument_data, encode_or_load_brr_file,
    load_sample_for_instrument, load_sample_for_sample, InstrumentSampleData, SampleFileCache,
    SampleSampleData, WAV_EXTENSION,
};
use compiler::songs::{sound_effect_to_song, test_sample_song, SongData};
use compiler::sound_effects::blank_compiled_sound_effects;
use compiler::sound_effects::{compile_sound_effect_input, CompiledSoundEffect, SoundEffectInput};
use compiler::spc_file_export::export_spc_file;

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
pub use item_id::ItemId;

#[derive(Debug)]
pub struct PlaySampleArgs {
    pub note: Note,
    pub note_length: u32,
    pub envelope: Option<Envelope>,
}

#[derive(Debug)]
pub enum ItemChanged<T> {
    ReplaceAll(Vec<(ItemId, T)>),
    AddedOrEdited(ItemId, T),
    MultipleAddedOrEdited(Vec<(ItemId, T)>),
    Removed(ItemId),
}

#[derive(Debug)]
pub enum ToCompiler {
    SfxExportOrder(ItemChanged<data::Name>),
    ProjectSongs(ItemChanged<data::Song>),

    Instrument(ItemChanged<data::Instrument>),
    Sample(ItemChanged<data::Sample>),

    AnalyseSample(SourcePathBuf, LoopSetting),

    // Merges Instruments into SampleAndInstrumentData
    // (sent when the user deselects the samples tab in the GUI)
    FinishedEditingSamples,

    // Updates sfx_data_size and rechecks song sizes.
    // (sent when the user deselects the sound effects tab in the GUI)
    FinishedEditingSoundEffects,

    SoundEffects(ItemChanged<SoundEffectInput>),
    PlaySoundEffect(ItemId),

    SongTabClosed(ItemId),
    SongChanged(ItemId, String),
    CompileAndPlaySong(ItemId, String, Option<SongSkip>, ChannelsMask),
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
pub type SoundEffectOutput = Result<Arc<CompiledSoundEffect>, SfxError>;
pub type SongOutput = Result<Arc<SongData>, SongError>;

#[derive(Debug)]
pub enum CompilerOutput {
    Panic(String),

    Instrument(ItemId, InstrumentOutput),
    Sample(ItemId, SampleOutput),

    // ::TODO the prevent user from leaving the Samples tab if this error occurs::
    CombineSamples(Result<usize, CombineSamplesError>),

    SoundEffect(ItemId, SoundEffectOutput),

    Song(ItemId, SongOutput),

    NumberOfMissingSoundEffects(usize),

    SoundEffectsDataSize(usize),
    LargestSongSize(usize),

    // The result of the last `ToCompiler::ExportSongToSpcFile` operation
    SpcFileResult(Result<(String, Vec<u8>), SpcFileError>),

    SampleAnalysis(Result<SampleAnalysis, BrrError>),
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
    Dependency,
    Error(errors::SoundEffectError),
}

impl Display for SfxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dependency => writeln!(f, "dependency error"),
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

impl Display for SongError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dependency => writeln!(f, "dependency error"),
            Self::Song(_) => writeln!(f, "song error"),
            Self::TooLarge(_) => writeln!(f, "song too large"),
        }
    }
}

#[derive(Debug)]
pub enum SpcFileError {
    NoSong,
    InvalidSong,
    NoCommonAudioData,
    Spc(ExportSpcFileError),
}

impl std::fmt::Display for SpcFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoSong => writeln!(f, "No song to export"),
            Self::InvalidSong => writeln!(f, "Error compiling song"),
            Self::NoCommonAudioData => writeln!(f, "Error in common audio data"),
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

    fn items(&self) -> &[ItemT] {
        &self.items
    }

    fn get(&self, id: &ItemId) -> Option<&ItemT> {
        self.map.get(id).and_then(|i| self.items.get(*i))
    }

    fn replace(&mut self, data: Vec<(ItemId, ItemT)>) {
        self.map = data
            .iter()
            .enumerate()
            .map(|(index, (id, _item))| (*id, index))
            .collect();

        self.items = data.into_iter().map(|(_id, item)| item).collect();
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
            ItemChanged::ReplaceAll(v) => self.replace(v),
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
    changed: bool,
    name_map_changed: bool,
    items: Vec<ItemT>,
    output: Vec<OutT>,
    map: HashMap<ItemId, usize>,

    // Instrument compiler requires a String map
    name_map: HashMap<data::Name, usize>,
}

impl<ItemT, OutT> CList<ItemT, OutT>
where
    ItemT: NameGetter,
{
    fn new() -> Self {
        Self {
            changed: false,
            name_map_changed: false,
            items: Vec::new(),
            output: Vec::new(),
            map: HashMap::new(),
            name_map: HashMap::new(),
        }
    }

    fn is_changed(&self) -> bool {
        self.changed
    }

    fn clear_changed_flag(&mut self) {
        self.changed = false;
    }

    fn is_name_map_changed(&self) -> bool {
        self.name_map_changed
    }

    fn clear_name_map_changed_flag(&mut self) {
        self.name_map_changed = false;
    }

    fn items(&self) -> &[ItemT] {
        &self.items
    }

    fn output(&self) -> &[OutT] {
        &self.output
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

    fn replace(
        &mut self,
        data: Vec<(ItemId, ItemT)>,
        mut compiler_fn: impl FnMut(ItemId, &ItemT) -> OutT,
    ) {
        self.map = data
            .iter()
            .enumerate()
            .map(|(index, (id, _item))| (*id, index))
            .collect();

        self.output = data
            .iter()
            .map(|(id, item)| compiler_fn(*id, item))
            .collect();

        self.name_map = data
            .iter()
            .enumerate()
            .map(|(index, (_id, item))| (item.name().clone(), index))
            .collect();
        self.name_map_changed = true;

        self.items = data.into_iter().map(|(_id, item)| item).collect();

        self.changed = true;
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
                    self.name_map_changed = true;
                }

                self.output[*index] = out;
                self.items[*index] = item;
            }
            None => {
                let index = self.items.len();
                let out = compiler_fn(id, &item);

                self.name_map.insert(item.name().clone(), index);
                self.name_map_changed = true;

                self.map.insert(id, index);
                self.output.push(out);
                self.items.push(item);
            }
        }

        self.changed = true;
    }

    fn remove(&mut self, id: ItemId) {
        if let Some(index) = self.map.remove(&id) {
            decrement_index_from_map(&mut self.map, index);

            let name_map_index = self.name_map.remove(self.items[index].name());
            decrement_index_from_map(&mut self.name_map, index);
            assert_eq!(name_map_index, Some(index));

            self.name_map_changed = true;

            self.output.remove(index);
            self.items.remove(index);
        }

        self.changed = true;
    }

    fn process_message(
        &mut self,
        m: ItemChanged<ItemT>,
        mut compiler_fn: impl FnMut(ItemId, &ItemT) -> OutT,
    ) {
        match m {
            ItemChanged::ReplaceAll(v) => self.replace(v, compiler_fn),
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

    fn recompile_all(&mut self, compiler_fn: impl Fn(ItemId, &ItemT) -> OutT) {
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

fn combine_sample_data(
    instruments: &CList<data::Instrument, Option<InstrumentSampleData>>,
    samples: &CList<data::Sample, Option<SampleSampleData>>,
) -> Result<(CommonAudioData, PitchTable), CombineSamplesError> {
    let expected_instruments_len = instruments.items().len();
    let expected_samples_len = samples.items().len();

    let instruments: Vec<_> = instruments
        .output()
        .iter()
        .filter_map(|s| s.as_ref().cloned())
        .collect();

    let samples: Vec<_> = samples
        .output()
        .iter()
        .filter_map(|s| s.as_ref().cloned())
        .collect();

    // Test all instruments and samples are compiled
    let n_instrument_errors = expected_instruments_len - instruments.len();
    let n_sample_errors = expected_samples_len - samples.len();
    if n_instrument_errors + n_sample_errors > 0 {
        return Err(CombineSamplesError::IndividualErrors {
            n_instrument_errors,
            n_sample_errors,
        });
    }

    let samples = match combine_samples(&instruments, &samples) {
        Ok(s) => s,
        Err(e) => {
            return Err(CombineSamplesError::CombineError(e));
        }
    };

    let blank_sfx = blank_compiled_sound_effects();

    match build_common_audio_data(&samples, &blank_sfx) {
        Ok(common) => Ok((common, samples.take_pitch_table())),
        Err(e) => Err(CombineSamplesError::CommonAudioData(e)),
    }
}

fn build_play_instrument_data(
    instruments: &CList<data::Instrument, Option<InstrumentSampleData>>,
    id: ItemId,
    args: PlaySampleArgs,
) -> Option<(CommonAudioData, Arc<SongData>)> {
    let sample = match instruments.get_output_for_id(&id) {
        Some(Some(s)) => s,
        _ => return None,
    };

    let (sample_data, max_octave) = create_test_instrument_data(sample)?;

    if args.note > Note::last_note_for_octave(max_octave) {
        return None;
    }

    let blank_sfx = blank_compiled_sound_effects();
    let common_audio_data = build_common_audio_data(&sample_data, &blank_sfx).ok()?;

    let song_data = match test_sample_song(0, args.note, args.note_length, args.envelope) {
        Ok(sd) => Arc::new(sd),
        Err(_) => return None,
    };

    Some((common_audio_data, song_data))
}

fn build_play_sample_data(
    samples: &CList<data::Sample, Option<SampleSampleData>>,
    id: ItemId,
    args: PlaySampleArgs,
) -> Option<(CommonAudioData, Arc<SongData>)> {
    let sample = match samples.get_output_for_id(&id) {
        Some(Some(s)) => s.clone(),
        _ => return None,
    };

    let sample_data = combine_samples(&[], &[sample]).ok()?;

    let blank_sfx = blank_compiled_sound_effects();
    let common_audio_data = build_common_audio_data(&sample_data, &blank_sfx).ok()?;

    let song_data = match test_sample_song(0, args.note, args.note_length, args.envelope) {
        Ok(sd) => Arc::new(sd),
        Err(_) => return None,
    };

    Some((common_audio_data, song_data))
}

fn create_sfx_compiler<'a>(
    dependencies: &'a Option<SongDependencies>,
    sender: &'a Sender,
) -> impl (Fn(ItemId, &SoundEffectInput) -> Option<Arc<CompiledSoundEffect>>) + 'a {
    move |id, sfx| {
        let dep = match dependencies.as_ref() {
            Some(d) => d,
            None => {
                sender.send(CompilerOutput::SoundEffect(id, Err(SfxError::Dependency)));
                return None;
            }
        };
        match compile_sound_effect_input(sfx, &dep.inst_map, &dep.pitch_table) {
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
    sfx_export_order: &IList<data::Name>,
    sound_effects: &CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>>,
    sender: &Sender,
) {
    if sound_effects.items().is_empty() {
        let n_missing = sfx_export_order.items().len();
        sender.send(CompilerOutput::NumberOfMissingSoundEffects(n_missing));
        return;
    }

    let n_missing = sfx_export_order
        .items()
        .iter()
        .filter(|name| !sound_effects.name_map().contains_key(name))
        .count();

    sender.send(CompilerOutput::NumberOfMissingSoundEffects(n_missing));
}

fn calc_sfx_data_size(
    sfx_export_order: &IList<data::Name>,
    sound_effects: &CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>>,
) -> usize {
    let sfx_size: usize = sfx_export_order
        .items()
        .iter()
        .filter_map(|name| sound_effects.get_output_for_name(name))
        .filter_map(Option::as_ref)
        .map(|o| o.bytecode().len())
        .sum();

    let table_size = sfx_export_order.items().len() * COMMON_DATA_BYTES_PER_SOUND_EFFECT;

    table_size + sfx_size
}

struct SongDependencies {
    inst_map: data::UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: PitchTable,
    common_data_no_sfx_size: usize,
    sfx_data_size: usize,
}

impl SongDependencies {
    fn common_data_size(&self) -> usize {
        self.common_data_no_sfx_size + self.sfx_data_size
    }
}

fn build_common_data_no_sfx_and_song_dependencies(
    instruments: &CList<data::Instrument, Option<InstrumentSampleData>>,
    samples: &CList<data::Sample, Option<SampleSampleData>>,
    sfx_export_order: &IList<data::Name>,
    sound_effects: &CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>>,
) -> Result<(CommonAudioData, SongDependencies), CombineSamplesError> {
    let (common_data, pitch_table) = combine_sample_data(instruments, samples)?;

    match data::validate_instrument_and_sample_names(
        instruments.items().iter(),
        samples.items().iter(),
    ) {
        Ok(instruments) => {
            let sd = SongDependencies {
                inst_map: instruments,
                pitch_table,
                common_data_no_sfx_size: common_data.data().len(),
                sfx_data_size: calc_sfx_data_size(sfx_export_order, sound_effects),
            };
            Ok((common_data, sd))
        }
        Err(e) => Err(CombineSamplesError::UniqueNamesError(e)),
    }
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
        let song_data = match compiler::mml::compile_mml(f, name, &dep.inst_map, &dep.pitch_table) {
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

    fn load_song(
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
                    self.load_song(*id, &item.source, pf_songs, dependencies, sender),
                );
            }
        };

        match m {
            ItemChanged::ReplaceAll(v) => {
                self.songs = v
                    .iter()
                    .map(|(id, item)| {
                        (
                            *id,
                            self.load_song(*id, &item.source, pf_songs, dependencies, sender),
                        )
                    })
                    .collect();
            }
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
                    self.load_song(id, &pf_song.source, pf_songs, dependencies, sender),
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
            .map(|s| s.data_and_echo_size())
            .max()
            .unwrap_or(0);

        sender.send(CompilerOutput::LargestSongSize(max_total_size));
    }

    fn export_to_spc_file(
        &self,
        id: ItemId,
        pf_songs: &IList<data::Song>,
        common_audio_data: Option<&CommonAudioData>,
    ) -> Result<(String, Vec<u8>), SpcFileError> {
        let common_audio_data = match common_audio_data {
            None => return Err(SpcFileError::NoCommonAudioData),
            Some(c) => c,
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

        match export_spc_file(common_audio_data, song_data) {
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

fn update_sfx_data_size_and_recheck_all_songs(
    song_dependencies: &mut Option<SongDependencies>,
    songs: &mut SongCompiler,
    sfx_export_order: &IList<data::Name>,
    sound_effects: &CList<SoundEffectInput, Option<Arc<CompiledSoundEffect>>>,
    sender: &Sender,
) {
    let sfx_data_size = calc_sfx_data_size(sfx_export_order, sound_effects);
    sender.send(CompilerOutput::SoundEffectsDataSize(sfx_data_size));

    if let Some(dep) = song_dependencies {
        dep.sfx_data_size = sfx_data_size;
        songs.recheck_song_sizes(dep, sender);
    }
}

fn analyse_sample(
    cache: &mut SampleFileCache,
    source: SourcePathBuf,
    loop_setting: LoopSetting,
) -> Result<SampleAnalysis, BrrError> {
    let brr_sample = Arc::new(encode_or_load_brr_file(&source, cache, &loop_setting)?);

    let wav_sample = match source.extension() {
        Some(WAV_EXTENSION) => match cache.load_wav_file(&source) {
            Ok(w) => Some(w),
            Err(e) => return Err(e.clone()),
        },
        _ => None,
    };

    Ok(sample_analyser::analyse_sample(brr_sample, wav_sample))
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

    let mut sfx_export_order = IList::new();
    let mut pf_songs = IList::new();
    let mut instruments = CList::new();
    let mut samples = CList::new();
    let mut sound_effects = CList::new();
    let mut songs = SongCompiler::new(parent_path.clone());

    let mut sample_file_cache = SampleFileCache::new(parent_path);

    let mut song_dependencies = None;
    let mut common_audio_data_no_sfx = None;

    while let Ok(m) = receiever.recv() {
        match m {
            ToCompiler::SfxExportOrder(m) => {
                sfx_export_order.process_message(m);
                count_missing_sfx(&sfx_export_order, &sound_effects, &sender);
            }
            ToCompiler::ProjectSongs(m) => {
                songs.process_pf_song_message(&m, &pf_songs, &song_dependencies, &sender);
                pf_songs.process_message(m);
            }
            ToCompiler::Instrument(m) => {
                let c = create_instrument_compiler(&mut sample_file_cache, &sender);
                instruments.process_message(m, c);

                song_dependencies = None;
            }
            ToCompiler::Sample(m) => {
                let c = create_sample_compiler(&mut sample_file_cache, &sender);
                samples.process_message(m, c);

                song_dependencies = None;
            }
            ToCompiler::RecompileInstrumentsUsingSample(source_path) => {
                let c = create_instrument_compiler(&mut sample_file_cache, &sender);
                instruments.recompile_all_if(c, |inst| inst.source == source_path);

                let c = create_sample_compiler(&mut sample_file_cache, &sender);
                samples.recompile_all_if(c, |inst| inst.source == source_path);

                song_dependencies = None;
            }

            ToCompiler::FinishedEditingSamples => {
                if instruments.is_changed() || samples.is_changed() {
                    instruments.clear_changed_flag();
                    samples.clear_changed_flag();

                    match build_common_data_no_sfx_and_song_dependencies(
                        &instruments,
                        &samples,
                        &sfx_export_order,
                        &sound_effects,
                    ) {
                        Ok((cd, sd)) => {
                            let data_size = cd.data().len();
                            sender.send(CompilerOutput::CombineSamples(Ok(data_size)));

                            common_audio_data_no_sfx = Some(cd);
                            song_dependencies = Some(sd);
                        }
                        Err(e) => {
                            sender.send(CompilerOutput::CombineSamples(Err(e)));
                            common_audio_data_no_sfx = None;
                            song_dependencies = None;
                        }
                    }

                    sender.send_audio(AudioMessage::CommonAudioDataChanged(
                        common_audio_data_no_sfx.clone(),
                    ));

                    let c = create_sfx_compiler(&song_dependencies, &sender);
                    sound_effects.recompile_all(c);

                    songs.compile_all_songs(&pf_songs, &song_dependencies, &sender);
                }
            }

            ToCompiler::FinishedEditingSoundEffects => {
                update_sfx_data_size_and_recheck_all_songs(
                    &mut song_dependencies,
                    &mut songs,
                    &sfx_export_order,
                    &sound_effects,
                    &sender,
                );
            }

            ToCompiler::SoundEffects(m) => {
                let replace_all_message = matches!(m, ItemChanged::ReplaceAll(_));

                let c = create_sfx_compiler(&song_dependencies, &sender);
                sound_effects.process_message(m, c);

                if sound_effects.is_name_map_changed() {
                    sound_effects.clear_name_map_changed_flag();

                    count_missing_sfx(&sfx_export_order, &sound_effects, &sender);
                }

                if replace_all_message {
                    update_sfx_data_size_and_recheck_all_songs(
                        &mut song_dependencies,
                        &mut songs,
                        &sfx_export_order,
                        &sound_effects,
                        &sender,
                    );
                }
            }
            ToCompiler::PlaySoundEffect(id) => {
                if let Some(Some(sfx_data)) = sound_effects.get_output_for_id(&id) {
                    let song_data = Arc::new(sound_effect_to_song(sfx_data));
                    sender.send_audio(AudioMessage::PlaySong(
                        id,
                        song_data,
                        None,
                        ChannelsMask::ALL,
                    ));
                }
            }

            ToCompiler::SongTabClosed(id) => {
                songs.song_tab_closed(id, &pf_songs, &song_dependencies, &sender);
            }
            ToCompiler::SongChanged(id, mml) => {
                songs.edit_and_compile_song(id, mml, &pf_songs, &song_dependencies, &sender);
            }
            ToCompiler::CompileAndPlaySong(id, mml, song_skip, channels_mask) => {
                sender.send_audio(AudioMessage::Pause);
                songs.edit_and_compile_song(id, mml, &pf_songs, &song_dependencies, &sender);
                if let Some(song) = songs.get_song_data(&id) {
                    sender.send_audio(AudioMessage::PlaySong(
                        id,
                        song.clone(),
                        song_skip,
                        channels_mask,
                    ));
                }
            }
            ToCompiler::PlayInstrument(id, args) => {
                if let Some((c_data, s_data)) = build_play_instrument_data(&instruments, id, args) {
                    sender.send_audio(AudioMessage::PlaySample(id, c_data, s_data));
                }
            }
            ToCompiler::PlaySample(id, args) => {
                if let Some((c_data, s_data)) = build_play_sample_data(&samples, id, args) {
                    sender.send_audio(AudioMessage::PlaySample(id, c_data, s_data));
                }
            }

            ToCompiler::ExportSongToSpcFile(id) => {
                let r = songs.export_to_spc_file(id, &pf_songs, common_audio_data_no_sfx.as_ref());
                sender.send(CompilerOutput::SpcFileResult(r));
            }

            ToCompiler::RemoveFileFromSampleCache(source_path) => {
                sample_file_cache.remove_path(&source_path);
            }

            ToCompiler::AnalyseSample(source_path, loop_setting) => {
                let r = analyse_sample(&mut sample_file_cache, source_path, loop_setting);
                sender.send(CompilerOutput::SampleAnalysis(r));
            }
        }
    }
}

fn monitor_thread(
    parent_path: ParentPathBuf,
    reciever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<GuiMessage>,
    audio_sender: mpsc::Sender<AudioMessage>,
) {
    let s = sender.clone();

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
