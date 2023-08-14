//! Background compiler thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::names::NameGetter;
use crate::Message;

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::thread;

extern crate compiler;
use compiler::build_common_audio_data;
use compiler::data;
use compiler::data::{load_text_file_with_limit, TextFile};
use compiler::errors;
use compiler::samples::{combine_samples, load_sample_for_instrument, Sample, SampleFileCache};
use compiler::sound_effects::blank_compiled_sound_effects;
use compiler::sound_effects::{compile_sound_effect_input, CompiledSoundEffect, SoundEffectInput};
use compiler::CommonAudioData;
use compiler::PitchTable;
use compiler::SongData;

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
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
pub enum ItemChanged<T> {
    ReplaceAll(Vec<(ItemId, T)>),
    AddedOrEdited(ItemId, T),
    Removed(ItemId),
    // ::TODO add AddOrEditMultiple (for adding missing sfx)::
}

#[derive(Debug)]
pub enum ToCompiler {
    SfxExportOrder(ItemChanged<data::Name>),
    ProjectSongs(ItemChanged<data::Song>),

    Instrument(ItemChanged<data::Instrument>),

    // Merges Instruments into SampleAndInstrumentData
    // (sent when the user deselects the samples tab in the GUI)
    FinishedEditingSamples,

    SoundEffects(ItemChanged<SoundEffectInput>),

    SongChanged(ItemId, String),
}

pub type InstrumentOutput = Result<usize, errors::SampleError>;
pub type SoundEffectOutput = Result<usize, errors::SoundEffectError>;
pub type SongOutput = Result<usize, SongError>;

#[derive(Debug)]
pub enum CompilerOutput {
    Panic(String),

    Instrument(ItemId, Result<usize, errors::SampleError>),

    // ::TODO the prevent user from leaving the Samples tab if this error occurs::
    CombineSamples(Result<usize, CombineSamplesError>),

    SoundEffect(ItemId, Result<usize, errors::SoundEffectError>),

    // ::TODO send song length to the GUI::
    Song(ItemId, SongOutput),

    // May return an empty vec
    MissingSoundEffects(Vec<data::Name>),

    SoundEffectsDataSize(usize),
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
pub enum SongError {
    Dependency,
    Mml(errors::MmlCompileErrors),
    Song(errors::SongError),
}

impl Display for SongError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dependency => writeln!(f, "dependency error"),
            Self::Mml(_) => writeln!(f, "MML error"),
            Self::Song(_) => writeln!(f, "song error"),
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

    fn replace(&mut self, data: Vec<(ItemId, ItemT)>) {
        self.map = data
            .iter()
            .enumerate()
            .map(|(index, (id, _item))| (id.clone(), index))
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
            ItemChanged::AddedOrEdited(id, item) => self.add_or_edit(id, item),
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
    name_map: HashMap<String, u32>,
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

    fn name_map(&self) -> &HashMap<String, u32> {
        &self.name_map
    }

    fn cast_index(index: usize) -> u32 {
        u32::try_from(index).unwrap_or(u32::MAX)
    }

    fn replace(
        &mut self,
        data: Vec<(ItemId, ItemT)>,
        mut compiler_fn: impl FnMut(ItemId, &ItemT) -> OutT,
    ) {
        self.map = data
            .iter()
            .enumerate()
            .map(|(index, (id, _item))| (id.clone(), index))
            .collect();

        self.output = data
            .iter()
            .map(|(id, item)| compiler_fn(id.clone(), item))
            .collect();

        self.name_map = data
            .iter()
            .enumerate()
            .map(|(index, (_id, item))| (item.name().to_string(), Self::cast_index(index)))
            .collect();
        self.name_map_changed = true;

        self.items = data.into_iter().map(|(_id, item)| item).collect();

        self.changed = true;
    }

    fn add_or_edit(
        &mut self,
        id: ItemId,
        item: ItemT,
        mut compiler_fn: impl FnMut(ItemId, &ItemT) -> OutT,
    ) {
        match self.map.get(&id) {
            Some(index) => {
                let out = compiler_fn(id, &item);

                let old_name = self.items[*index].name();
                if item.name() != old_name {
                    self.name_map.remove(old_name.as_str());
                    self.name_map
                        .insert(item.name().to_string(), Self::cast_index(*index));
                    self.name_map_changed = true;
                }

                self.output[*index] = out;
                self.items[*index] = item;
            }
            None => {
                let index = self.items.len();
                let out = compiler_fn(id.clone(), &item);

                self.name_map
                    .insert(item.name().to_string(), Self::cast_index(index));
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
            self.name_map.remove(self.items[index].name().as_str());
            self.name_map_changed = true;

            self.output.remove(index);
            self.items.remove(index);
        }

        self.changed = true;
    }

    fn process_message(
        &mut self,
        m: ItemChanged<ItemT>,
        compiler_fn: impl FnMut(ItemId, &ItemT) -> OutT,
    ) {
        match m {
            ItemChanged::ReplaceAll(v) => self.replace(v, compiler_fn),
            ItemChanged::AddedOrEdited(id, item) => self.add_or_edit(id, item, compiler_fn),
            ItemChanged::Removed(id) => self.remove(id),
        }

        assert_eq!(self.output.len(), self.items.len());
        assert_eq!(self.map.len(), self.items.len());
        assert_eq!(self.name_map.len(), self.items.len());
    }

    fn recompile_all(&mut self, compiler_fn: impl Fn(ItemId, &ItemT) -> OutT) {
        for (id, &index) in &self.map {
            let out = compiler_fn(id.clone(), &self.items[index]);
            self.output[index] = out;
        }
    }
}

struct Sender(fltk::app::Sender<Message>);

impl Sender {
    fn send(&self, m: CompilerOutput) {
        self.0.send(Message::FromCompiler(m))
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

fn create_sfx_compiler<'a>(
    instruments: &'a CList<data::Instrument, Option<Sample>>,
    sender: &'a Sender,
) -> impl (Fn(ItemId, &SoundEffectInput) -> Option<CompiledSoundEffect>) + 'a {
    move |id, sfx| match compile_sound_effect_input(sfx, instruments.name_map()) {
        Ok(sfx) => {
            sender.send(CompilerOutput::SoundEffect(id, Ok(sfx.data().len())));
            Some(sfx)
        }
        Err(e) => {
            sender.send(CompilerOutput::SoundEffect(id, Err(e)));
            None
        }
    }
}

fn find_missing_sfx(
    sfx_export_order: &IList<data::Name>,
    sound_effects: &CList<SoundEffectInput, Option<CompiledSoundEffect>>,
    sender: &Sender,
) {
    let missing = sfx_export_order
        .items()
        .iter()
        .filter(|name| !sound_effects.name_map().contains_key(name.as_str()))
        .cloned()
        .collect();

    sender.send(CompilerOutput::MissingSoundEffects(missing));
}

fn send_sfx_size(
    sound_effects: &CList<SoundEffectInput, Option<CompiledSoundEffect>>,
    sender: &Sender,
) {
    let sfx_size: usize = sound_effects
        .output()
        .iter()
        .filter_map(|o| o.as_ref().map(|d| d.data().len()))
        .sum();
    let table_size = sound_effects.output().len() * 2;
    let data_size = table_size + sfx_size;

    sender.send(CompilerOutput::SoundEffectsDataSize(data_size));
}

struct SongDependencies {
    instruments: data::UniqueNamesList<data::Instrument>,
    pitch_table: PitchTable,
}

fn create_song_dependencies(
    instruments: &CList<data::Instrument, Option<Sample>>,
    pitch_table: PitchTable,
) -> Option<SongDependencies> {
    match data::validate_instrument_names(instruments.items().to_vec()) {
        Ok(instruments) => Some(SongDependencies {
            instruments,
            pitch_table,
        }),
        Err(_) => None,
    }
}

struct SongState {
    file: TextFile,
    song_data: Option<SongData>,
}
struct SongCompiler {
    parent_path: PathBuf,
    songs: HashMap<ItemId, SongState>,
}

fn file_name(p: &Path) -> String {
    p.file_name()
        .unwrap_or(p.as_os_str())
        .to_string_lossy()
        .to_string()
}

impl SongCompiler {
    fn new(parent_path: PathBuf) -> Self {
        Self {
            parent_path,
            songs: HashMap::new(),
        }
    }

    fn compile_song(
        id: ItemId,
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

        let mml = match compiler::compile_mml(f, &dep.instruments, &dep.pitch_table) {
            Ok(mml) => mml,
            Err(e) => {
                sender.send(CompilerOutput::Song(id, Err(SongError::Mml(e))));
                return None;
            }
        };

        let song_data = match compiler::song_data(mml) {
            Ok(mml) => mml,
            Err(e) => {
                sender.send(CompilerOutput::Song(id, Err(SongError::Song(e))));
                return None;
            }
        };

        let data_size = song_data.data().len();
        sender.send(CompilerOutput::Song(id, Ok(data_size)));

        Some(song_data)
    }

    fn load_song(
        &self,
        id: ItemId,
        path: &Path,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) -> SongState {
        let path = self.parent_path.join(path);

        let file = match load_text_file_with_limit(&path) {
            Ok(f) => f,
            Err(_) => TextFile {
                file_name: file_name(&path),
                path: Some(path),
                contents: String::new(),
            },
        };

        SongState {
            song_data: Self::compile_song(id, &file, dependencies, sender),
            file,
        }
    }

    fn process_pf_song_message(
        &mut self,
        m: &ItemChanged<data::Song>,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) {
        match m {
            ItemChanged::ReplaceAll(v) => {
                self.songs = v
                    .iter()
                    .map(|(id, item)| {
                        (
                            id.clone(),
                            self.load_song(id.clone(), &item.source, dependencies, sender),
                        )
                    })
                    .collect();
            }
            ItemChanged::AddedOrEdited(id, item) => {
                // Only add songs, do not modify them
                // (source is not editable by the GUI)

                #[allow(clippy::map_entry)] // Cannot use HashMap::entry() due to the borrow checker
                if !self.songs.contains_key(id) {
                    self.songs.insert(
                        id.clone(),
                        self.load_song(id.clone(), &item.source, dependencies, sender),
                    );
                }
            }
            ItemChanged::Removed(id) => {
                self.songs.remove(id);
            }
        }
    }

    fn compile_all_songs(&mut self, dependencies: &Option<SongDependencies>, sender: &Sender) {
        for (id, s) in self.songs.iter_mut() {
            s.song_data = Self::compile_song(id.clone(), &s.file, dependencies, sender);
        }
    }

    fn edit_and_compile_song(
        &mut self,
        id: ItemId,
        mml: String,
        dependencies: &Option<SongDependencies>,
        sender: &Sender,
    ) {
        match self.songs.entry(id.clone()) {
            Entry::Occupied(mut o) => {
                let state = o.get_mut();
                state.file.contents = mml;
                state.song_data = Self::compile_song(id, &state.file, dependencies, sender)
            }
            Entry::Vacant(v) => {
                let file = TextFile {
                    contents: mml,
                    file_name: "MML".to_owned(),
                    path: None,
                };
                let song_data = Self::compile_song(id, &file, dependencies, sender);
                v.insert(SongState { file, song_data });
            }
        }
    }
}

fn bg_thread(
    parent_path: PathBuf,
    receiever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<Message>,
) {
    let sender = Sender(sender);

    let mut sfx_export_order = IList::new();
    let mut pf_songs = IList::new();
    let mut instruments = CList::new();
    let mut sound_effects = CList::new();
    let mut songs = SongCompiler::new(parent_path.clone());

    let mut sample_file_cache = SampleFileCache::new(parent_path);

    let mut song_dependencies = None;
    let mut common_audio_data_no_sfx = None;

    while let Ok(m) = receiever.recv() {
        // ::TODO remove (silences an unused error message)::
        let _ = &song_dependencies;
        let _ = &common_audio_data_no_sfx;

        match m {
            ToCompiler::SfxExportOrder(m) => {
                sfx_export_order.process_message(m);

                // Only look for missing sfx if the sfx file exists
                if !sound_effects.items().is_empty() {
                    find_missing_sfx(&sfx_export_order, &sound_effects, &sender);
                }
            }
            ToCompiler::ProjectSongs(m) => {
                songs.process_pf_song_message(&m, &song_dependencies, &sender);
                pf_songs.process_message(m);
            }
            ToCompiler::Instrument(m) => {
                let c = create_instrument_compiler(&mut sample_file_cache, &sender);
                instruments.process_message(m, c);

                song_dependencies = None;
            }
            ToCompiler::FinishedEditingSamples => {
                if instruments.is_changed() {
                    instruments.clear_changed_flag();

                    match combine_sample_data(&instruments, &sender) {
                        Some((common, pt)) => {
                            song_dependencies = create_song_dependencies(&instruments, pt);
                            common_audio_data_no_sfx = Some(common);
                        }
                        None => {
                            song_dependencies = None;
                            common_audio_data_no_sfx = None;
                        }
                    }

                    // Sound Effects only require the name map to compile them
                    if instruments.is_name_map_changed() {
                        instruments.clear_name_map_changed_flag();

                        let c = create_sfx_compiler(&instruments, &sender);
                        sound_effects.recompile_all(c);
                    }
                    songs.compile_all_songs(&song_dependencies, &sender);
                }
            }
            ToCompiler::SoundEffects(m) => {
                let c = create_sfx_compiler(&instruments, &sender);
                sound_effects.process_message(m, c);

                if sound_effects.is_name_map_changed() {
                    sound_effects.clear_name_map_changed_flag();

                    find_missing_sfx(&sfx_export_order, &sound_effects, &sender);
                }

                send_sfx_size(&sound_effects, &sender);
            }
            ToCompiler::SongChanged(id, mml) => {
                songs.edit_and_compile_song(id, mml, &song_dependencies, &sender);
            }
        }
    }
}

fn monitor_thread(
    parent_path: PathBuf,
    reciever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<Message>,
) {
    let s = sender.clone();

    let handler = thread::Builder::new()
        .name("compiler_thread".into())
        .spawn(move || bg_thread(parent_path, reciever, sender))
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
    parent_path: PathBuf,
    reciever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<Message>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || monitor_thread(parent_path, reciever, sender))
}
