//! Background compiler thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::names::NameGetter;
use crate::Message;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::mpsc;
use std::thread;

extern crate compiler;
use compiler::build_common_audio_data;
use compiler::data;
use compiler::errors;
use compiler::samples::{combine_samples, load_sample_for_instrument, Sample, SampleFileCache};
use compiler::sound_effects::blank_compiled_sound_effects;
use compiler::sound_effects::{compile_sound_effect_input, CompiledSoundEffect, SoundEffectInput};
use compiler::CommonAudioData;
use compiler::PitchTable;

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
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
}

#[derive(Debug)]
pub enum CompilerOutput {
    Instrument(ItemId, Result<usize, errors::SampleError>),

    // ::TODO the prevent user from leaving the Samples tab if this error occurs::
    CombineSamples(Result<usize, CombineSamplesError>),

    SoundEffect(ItemId, Result<usize, errors::SoundEffectError>),

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
        mut compiler_fn: impl FnMut(ItemId, usize, &ItemT) -> OutT,
    ) {
        self.map = data
            .iter()
            .enumerate()
            .map(|(index, (id, _item))| (id.clone(), index))
            .collect();

        self.output = data
            .iter()
            .enumerate()
            .map(|(index, (id, item))| compiler_fn(id.clone(), index, item))
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
        mut compiler_fn: impl FnMut(ItemId, usize, &ItemT) -> OutT,
    ) {
        match self.map.get(&id) {
            Some(index) => {
                let out = compiler_fn(id, *index, &item);

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
                let out = compiler_fn(id.clone(), index, &item);

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
        compiler_fn: impl FnMut(ItemId, usize, &ItemT) -> OutT,
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

    fn recompile_all(&mut self, compiler_fn: impl Fn(ItemId, usize, &ItemT) -> OutT) {
        for (id, &index) in &self.map {
            let out = compiler_fn(id.clone(), index, &self.items[index]);
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
) -> impl (FnMut(ItemId, usize, &data::Instrument) -> Option<Sample>) + 'a {
    |id, idx, inst| match load_sample_for_instrument(inst, idx, sample_file_cache) {
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
) -> impl (Fn(ItemId, usize, &SoundEffectInput) -> Option<CompiledSoundEffect>) + 'a {
    move |id, _index, sfx| match compile_sound_effect_input(sfx, instruments.name_map()) {
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

    let mut sample_file_cache = SampleFileCache::new(parent_path);

    let mut pitch_table = None;
    let mut common_audio_data_no_sfx = None;

    while let Ok(m) = receiever.recv() {
        // ::TODO remove (silences an unused error message)::
        let _ = &pitch_table;
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
                pf_songs.process_message(m);
            }
            ToCompiler::Instrument(m) => {
                let c = create_instrument_compiler(&mut sample_file_cache, &sender);
                instruments.process_message(m, c);

                pitch_table = None;
            }
            ToCompiler::FinishedEditingSamples => {
                if instruments.is_changed() {
                    instruments.clear_changed_flag();

                    match combine_sample_data(&instruments, &sender) {
                        Some((pt, common)) => {
                            pitch_table = Some(pt);
                            common_audio_data_no_sfx = Some(common);
                        }
                        None => {
                            pitch_table = None;
                            common_audio_data_no_sfx = None;
                        }
                    }

                    // Sound Effects only require the name map to compile them
                    if instruments.is_name_map_changed() {
                        instruments.clear_name_map_changed_flag();

                        let c = create_sfx_compiler(&instruments, &sender);
                        sound_effects.recompile_all(c);
                    }
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
        }
    }
}

pub fn create_bg_thread(
    parent_path: PathBuf,
    reciever: mpsc::Receiver<ToCompiler>,
    sender: fltk::app::Sender<Message>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || bg_thread(parent_path, reciever, sender))
}
