//! Background compiler thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::names::NameGetter;
use crate::Message;

use crate::audio_thread::AudioMessage;

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;
use std::sync::mpsc;
use std::thread;

extern crate compiler;
use compiler::common_audio_data::{build_common_audio_data, CommonAudioData};
use compiler::data;
use compiler::data::{load_text_file_with_limit, TextFile};
use compiler::driver_constants::COMMON_DATA_BYTES_PER_SOUND_EFFECT;
use compiler::echo::EchoEdl;
use compiler::envelope::Envelope;
use compiler::errors::{self, ExportSpcFileError, SongTooLargeError};
use compiler::mml_tick_count::{build_tick_count_table, MmlTickCountTable};
use compiler::notes::Note;
use compiler::path::{ParentPathBuf, SourcePathBuf};
use compiler::pitch_table::PitchTable;
use compiler::samples::{combine_samples, load_sample_for_instrument, Sample, SampleFileCache};
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

    // Merges Instruments into SampleAndInstrumentData
    // (sent when the user deselects the samples tab in the GUI)
    FinishedEditingSamples,

    // Updates sfx_data_size and rechecks song sizes.
    // (sent when the user deselects the sound effects tab in the GUI)
    FinishedEditingSoundEffects,

    SoundEffects(ItemChanged<SoundEffectInput>),
    PlaySoundEffect(ItemId),

    SongChanged(ItemId, String),
    CompileAndPlaySong(ItemId, String),
    PlaySample(ItemId, PlaySampleArgs),

    ExportSongToSpcFile(ItemId),

    RemoveFileFromSampleCache(SourcePathBuf),
    RecompileInstrumentsUsingSample(SourcePathBuf),
}

pub type InstrumentOutput = Result<usize, errors::SampleError>;
pub type SoundEffectOutput = Result<usize, SfxError>;
pub type SongOutput = Result<SongOutputData, SongError>;

#[derive(Debug)]
pub enum CompilerOutput {
    Panic(String),

    Instrument(ItemId, Result<usize, errors::SampleError>),

    // ::TODO the prevent user from leaving the Samples tab if this error occurs::
    CombineSamples(Result<usize, CombineSamplesError>),

    SoundEffect(ItemId, SoundEffectOutput),

    Song(ItemId, SongOutput),

    NumberOfMissingSoundEffects(usize),

    SoundEffectsDataSize(usize),
    LargestSongSize(usize),

    // The result of the last `ToCompiler::ExportSongToSpcFile` operation
    SpcFileResult(Result<(String, Vec<u8>), SpcFileError>),
}

#[derive(Debug)]
pub struct SongOutputData {
    pub data_size: usize,
    pub duration: Option<std::time::Duration>,
    pub echo_buffer: EchoEdl,
    pub tick_count_table: MmlTickCountTable,
}

#[derive(Debug)]
pub enum CombineSamplesError {
    InstrumentErrors { n_errors: usize },
    CombineError(errors::SampleAndInstrumentDataError),
    CommonAudioData(errors::CommonAudioDataErrors),
}

impl std::fmt::Display for CombineSamplesError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CombineSamplesError::InstrumentErrors { n_errors } => {
                if *n_errors > 1 {
                    writeln!(f, "{} instruments have errors", n_errors)
                } else {
                    writeln!(f, "One instrument has an error")
                }
            }
            CombineSamplesError::CombineError(e) => {
                writeln!(f, "{}", e.multiline_display())
            }
            CombineSamplesError::CommonAudioData(e) => {
                writeln!(f, "{}", e.multiline_display())
            }
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
    Mml(errors::MmlCompileErrors),
    Song(errors::SongError),
    TooLarge(SongTooLargeError),
}

impl Display for SongError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dependency => writeln!(f, "dependency error"),
            Self::Mml(_) => writeln!(f, "MML error"),
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

    fn get_id(&self, id: ItemId) -> Option<&ItemT> {
        self.map.get(&id).and_then(|i: &usize| self.items.get(*i))
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
            self.name_map.remove(self.items[index].name());
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
    sender: fltk::app::Sender<Message>,
    audio_sender: mpsc::Sender<AudioMessage>,
}

impl Sender {
    fn send(&self, m: CompilerOutput) {
        self.sender.send(Message::FromCompiler(m))
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
) -> impl (FnMut(ItemId, &data::Instrument) -> Option<Sample>) + 'a {
    |id, inst| match load_sample_for_instrument(inst, sample_file_cache) {
        Ok(s) => {
            sender.send(CompilerOutput::Instrument(id, Ok(s.sample_size())));
            Some(s)
        }
        Err(e) => {
            sender.send(CompilerOutput::Instrument(id, Err(e)));
            None
        }
    }
}

fn combine_sample_data(
    instruments: &CList<data::Instrument, Option<Sample>>,
    sender: &Sender,
) -> Option<(CommonAudioData, PitchTable)> {
    let samples: Vec<Sample> = instruments
        .output()
        .iter()
        .filter_map(|s| s.as_ref().cloned())
        .collect();

    // Test all instruments are compiled
    if samples.len() != instruments.items().len() {
        let n_errors = instruments.items().len() - samples.len();
        sender.send(CompilerOutput::CombineSamples(Err(
            CombineSamplesError::InstrumentErrors { n_errors },
        )));
        return None;
    }

    let samples = match combine_samples(&samples) {
        Ok(s) => s,
        Err(e) => {
            sender.send(CompilerOutput::CombineSamples(Err(
                CombineSamplesError::CombineError(e),
            )));
            return None;
        }
    };

    let blank_sfx = blank_compiled_sound_effects();

    match build_common_audio_data(&samples, &blank_sfx) {
        Ok(common) => {
            sender.send(CompilerOutput::CombineSamples(Ok(common.data().len())));

            Some((common, samples.take_pitch_table()))
        }
        Err(e) => {
            sender.send(CompilerOutput::CombineSamples(Err(
                CombineSamplesError::CommonAudioData(e),
            )));
            None
        }
    }
}

fn build_play_sample_data(
    instruments: &CList<data::Instrument, Option<Sample>>,
    id: ItemId,
    args: PlaySampleArgs,
) -> Option<(CommonAudioData, SongData)> {
    match instruments.get_id(id) {
        None => return None,
        Some(inst) => {
            // Test octave is in range.
            if args.note < Note::first_note_for_octave(inst.first_octave)
                || args.note > Note::last_note_for_octave(inst.last_octave)
            {
                return None;
            }
        }
    }

    let sample = match instruments.get_output_for_id(&id) {
        Some(Some(s)) => s,
        _ => return None,
    };

    let sample_data = match combine_samples(&[sample.clone()]) {
        Ok(sd) => sd,
        Err(_) => return None,
    };

    let blank_sfx = blank_compiled_sound_effects();
    let common_audio_data = match build_common_audio_data(&sample_data, &blank_sfx) {
        Ok(common) => common,
        Err(_) => return None,
    };

    let song_data = match test_sample_song(0, args.note, args.note_length, args.envelope) {
        Ok(sd) => sd,
        Err(_) => return None,
    };

    Some((common_audio_data, song_data))
}

fn create_sfx_compiler<'a>(
    dependencies: &'a Option<SongDependencies>,
    sender: &'a Sender,
) -> impl (Fn(ItemId, &SoundEffectInput) -> Option<CompiledSoundEffect>) + 'a {
    move |id, sfx| {
        let dep = match dependencies.as_ref() {
            Some(d) => d,
            None => {
                sender.send(CompilerOutput::SoundEffect(id, Err(SfxError::Dependency)));
                return None;
            }
        };
        match compile_sound_effect_input(sfx, &dep.instruments) {
            Ok(sfx) => {
                sender.send(CompilerOutput::SoundEffect(id, Ok(sfx.data().len())));
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
    sound_effects: &CList<SoundEffectInput, Option<CompiledSoundEffect>>,
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
    sound_effects: &CList<SoundEffectInput, Option<CompiledSoundEffect>>,
) -> usize {
    let sfx_size: usize = sfx_export_order
        .items()
        .iter()
        .filter_map(|name| sound_effects.get_output_for_name(name))
        .filter_map(Option::as_ref)
        .map(|o| o.data().len())
        .sum();

    let table_size = sfx_export_order.items().len() * COMMON_DATA_BYTES_PER_SOUND_EFFECT;

    table_size + sfx_size
}

struct SongDependencies {
    instruments: data::UniqueNamesList<data::Instrument>,
    pitch_table: PitchTable,
    common_data_no_sfx_size: usize,
    sfx_data_size: usize,
}

impl SongDependencies {
    fn common_data_size(&self) -> usize {
        self.common_data_no_sfx_size + self.sfx_data_size
    }
}

fn create_song_dependencies(
    instruments: &CList<data::Instrument, Option<Sample>>,
    pitch_table: PitchTable,
    common_audio_data_no_sfx: &CommonAudioData,
    sfx_export_order: &IList<data::Name>,
    sound_effects: &CList<SoundEffectInput, Option<CompiledSoundEffect>>,
) -> Option<SongDependencies> {
    match data::validate_instrument_names(instruments.items().to_vec()) {
        Ok(instruments) => Some(SongDependencies {
            instruments,
            pitch_table,
            common_data_no_sfx_size: common_audio_data_no_sfx.data().len(),
            sfx_data_size: calc_sfx_data_size(sfx_export_order, sound_effects),
        }),
        Err(_) => None,
    }
}

struct SongState {
    file: TextFile,
    song_data: Option<SongData>,
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

    fn get_song_data(&self, id: &ItemId) -> Option<&SongData> {
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
    ) -> Option<SongData> {
        let dep = match dependencies.as_ref() {
            Some(d) => d,
            None => {
                sender.send(CompilerOutput::Song(id, Err(SongError::Dependency)));
                return None;
            }
        };

        let name = name.cloned();
        let mml = match compiler::mml::compile_mml(f, name, &dep.instruments, &dep.pitch_table) {
            Ok(mml) => mml,
            Err(e) => {
                sender.send(CompilerOutput::Song(id, Err(SongError::Mml(e))));
                return None;
            }
        };
        let tick_count_table = build_tick_count_table(&mml);

        let song_data = match compiler::songs::song_data(mml) {
            Ok(mml) => mml,
            Err(e) => {
                sender.send(CompilerOutput::Song(id, Err(SongError::Song(e))));
                return None;
            }
        };

        match compiler::songs::validate_song_size(&song_data, dep.common_data_size()) {
            Ok(()) => {
                let to_gui = SongOutputData {
                    data_size: song_data.data().len(),
                    duration: song_data.duration(),
                    echo_buffer: song_data.metadata().echo_buffer.edl,
                    tick_count_table,
                };
                sender.send(CompilerOutput::Song(id, Ok(to_gui)));
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
    sound_effects: &CList<SoundEffectInput, Option<CompiledSoundEffect>>,
    sender: &Sender,
) {
    let sfx_data_size = calc_sfx_data_size(sfx_export_order, sound_effects);
    sender.send(CompilerOutput::SoundEffectsDataSize(sfx_data_size));

    if let Some(dep) = song_dependencies {
        dep.sfx_data_size = sfx_data_size;
        songs.recheck_song_sizes(dep, sender);
    }
}

fn bg_thread(
    parent_path: ParentPathBuf,
    receiever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<Message>,
    audio_sender: mpsc::Sender<AudioMessage>,
) {
    let sender = Sender {
        sender,
        audio_sender,
    };

    let mut sfx_export_order = IList::new();
    let mut pf_songs = IList::new();
    let mut instruments = CList::new();
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
            ToCompiler::RecompileInstrumentsUsingSample(source_path) => {
                let c = create_instrument_compiler(&mut sample_file_cache, &sender);
                instruments.recompile_all_if(c, |inst| inst.source == source_path);

                song_dependencies = None;
            }

            ToCompiler::FinishedEditingSamples => {
                if instruments.is_changed() {
                    instruments.clear_changed_flag();

                    match combine_sample_data(&instruments, &sender) {
                        Some((common, pt)) => {
                            song_dependencies = create_song_dependencies(
                                &instruments,
                                pt,
                                &common,
                                &sfx_export_order,
                                &sound_effects,
                            );
                            common_audio_data_no_sfx = Some(common);
                        }
                        None => {
                            song_dependencies = None;
                            common_audio_data_no_sfx = None;
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
                    let song_data = sound_effect_to_song(sfx_data);
                    sender.send_audio(AudioMessage::PlaySong(id, song_data));
                }
            }

            ToCompiler::SongChanged(id, mml) => {
                songs.edit_and_compile_song(id, mml, &pf_songs, &song_dependencies, &sender);
            }
            ToCompiler::CompileAndPlaySong(id, mml) => {
                sender.send_audio(AudioMessage::Stop);
                songs.edit_and_compile_song(id, mml, &pf_songs, &song_dependencies, &sender);
                if let Some(song) = songs.get_song_data(&id) {
                    sender.send_audio(AudioMessage::PlaySong(id, song.clone()));
                }
            }
            ToCompiler::PlaySample(id, args) => {
                if let Some((c_data, s_data)) = build_play_sample_data(&instruments, id, args) {
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
        }
    }
}

fn monitor_thread(
    parent_path: ParentPathBuf,
    reciever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<Message>,
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
            s.send(Message::FromCompiler(CompilerOutput::Panic(msg.to_owned())));
        }
    }
}

pub fn create_bg_thread(
    parent_path: ParentPathBuf,
    reciever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<Message>,
    audio_sender: mpsc::Sender<AudioMessage>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || monitor_thread(parent_path, reciever, sender, audio_sender))
}
