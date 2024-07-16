//! Audio Driver GUI

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]
// Do not open a console window on Windows
#![windows_subsystem = "windows"]

mod audio_thread;
mod compiler_thread;
mod envelope_widget;
mod files;
mod help;
mod helpers;
mod instrument_editor;
mod licenses_dialog;
mod list_editor;
mod menu;
mod mml_editor;
mod monitor_timer;
mod names;
mod sample_analyser;
mod sample_editor;
mod sample_sizes_widget;
mod sample_widgets;
mod sfx_export_order;
mod sfx_window;
mod symbols;
mod tables;
mod tabs;

mod about_tab;
mod project_tab;
mod samples_tab;
mod song_tab;
mod sound_effects_tab;

use crate::about_tab::AboutTab;
use crate::compiler_thread::{
    CompilerOutput, InstrumentOutput, ItemId, SoundEffectOutput, ToCompiler,
};
use crate::files::{
    add_song_to_pf_dialog, load_mml_file, load_pf_sfx_file,
    load_project_file_or_show_error_message, open_mml_file_dialog, open_sfx_file_dialog,
    save_spc_file_dialog,
};
use crate::help::HelpWidget;
use crate::helpers::input_height;
use crate::list_editor::{ListMessage, ListState, ListWithCompilerOutput};
use crate::menu::Menu;
use crate::names::deduplicate_names;
use crate::project_tab::ProjectTab;
use crate::samples_tab::SamplesTab;
use crate::sfx_export_order::SfxId;
use crate::song_tab::{blank_mml_file, SongTab};
use crate::sound_effects_tab::{blank_sfx_file, SoundEffectsTab};
use crate::tabs::{
    close_unsaved_song_tab_dialog, quit_with_unsaved_files_dialog, FileType, SaveResult, SaveType,
    Tab, TabManager,
};

use audio_thread::{AudioMessage, AudioMonitor, MusicChannelsMask, Pan, SongSkip};

use compiler::data;
use compiler::data::{DefaultSfxFlags, ProjectFile};
use compiler::driver_constants;
use compiler::path::{ParentPathBuf, SourcePathBuf};
use compiler::sfx_file::{convert_sfx_inputs_lossy, SoundEffectsFile};
use compiler::songs::SongData;
use compiler::sound_effects::SoundEffectInput;

use compiler::time::TickCounter;
use compiler_thread::{PlaySampleArgs, SampleOutput, ShortSongError};
use files::{
    new_project_dialog, open_instrument_sample_dialog, open_project_dialog,
    open_sample_sample_dialog, song_name_from_path,
};
use fltk::dialog;
use fltk::prelude::*;
use help::HelpSection;
use helpers::ch_units_to_width;
use licenses_dialog::LicensesDialog;
use list_editor::{ListPairWithCompilerOutputs, ListWithCompilerOutputEditor};
use monitor_timer::MonitorTimer;
use names::deduplicate_two_name_vecs;
use sample_analyser::SampleAnalyserDialog;
use sfx_export_order::{GuiSfxExportOrder, SfxExportOrderMessage};
use sfx_window::SfxWindow;

use std::collections::HashMap;
use std::env;
use std::path::PathBuf;
use std::sync::{mpsc, Arc};

const CARGO_PKG_VERSION: &str = env!("CARGO_PKG_VERSION");
const DEFAULT_WINDOW_TITLE: &str = "Terrific Audio Driver";
const WINDOW_TITLE_SUFFIX: &str = " - Terrific Audio Driver";

#[derive(Debug, Clone, Copy)]
pub enum InstrumentOrSampleId {
    Instrument(ItemId),
    Sample(ItemId),
}

#[derive(Debug)]
pub enum GuiMessage {
    SelectedTabChanged,

    RequestCloseSongTab(ItemId),
    SaveAndCloseSongTab(ItemId),
    ForceCloseSongTab(ItemId),

    SaveSelectedTab,
    SaveSelectedTabAs,
    SaveAllUnsaved,
    QuitRequested,
    ForceQuit,
    SaveAllAndQuit(Vec<FileType>),

    DefaultSfxFlagChanged(DefaultSfxFlags),
    EditSfxExportOrder(SfxExportOrderMessage),
    EditProjectSongs(ListMessage<data::Song>),

    SelectProjectSong(usize),

    Instrument(ListMessage<data::Instrument>),
    Sample(ListMessage<data::Sample>),
    EditInstrument(ItemId, data::Instrument),
    UserChangedSelectedInstrument,
    EditSample(ItemId, data::Sample),
    UserChangedSelectedSample,

    NewMmlFile,
    OpenMmlFile,

    ExportCurrentTabToSpcFile,
    ToggleSfxWindow,

    OpenSfxFileDialog,
    NewSfxFile,
    LoadSfxFile,

    RecompileEverything,

    EditSoundEffectList(ListMessage<SoundEffectInput>),
    EditSoundEffect(ItemId, SoundEffectInput),
    UserChangesSelectedSoundEffect,
    SfxFileHeaderChanged,
    AddMissingSoundEffects,

    AddSongToProjectDialog,
    SetProjectSongName(usize, data::Name),

    ShowSampleSizes,

    OpenAnalyseInstrumentDialog(ItemId),
    OpenAnalyseSampleDialog(ItemId),

    CommitSampleAnalyserChanges {
        id: InstrumentOrSampleId,
        freq: f64,
        loop_setting: data::LoopSetting,
    },

    OpenInstrumentSampleDialog(ItemId),
    OpenSampleSampleDialog(ItemId),

    OpenSongTab(usize),

    SongChanged(ItemId, String),
    RecompileSong(ItemId, String),

    PlaySong(ItemId, String, Option<SongSkip>, MusicChannelsMask),
    PlaySongForSfxTab(ItemId, TickCounter),
    PlaySoundEffectCommand(SfxId, Pan),
    PlayEditedSoundEffect(ItemId, Pan),
    PlayInstrument(ItemId, PlaySampleArgs),
    PlaySample(ItemId, PlaySampleArgs),
    PauseAudio,
    PauseResumeAudio(ItemId),
    SetMusicChannels(ItemId, MusicChannelsMask),

    AudioThreadStartedSong(ItemId, Arc<SongData>),
    AudioThreadResumedSong(ItemId),
    SongMonitorTimeout,

    ClearSampleCacheAndRebuild,
    FromCompiler(compiler_thread::CompilerOutput),

    ShowAboutTab,
    ShowOrHideHelpSyntax,
    ShowLicensesDialog,

    NewProject,
    OpenProject,
}

pub type ProjectSongsData =
    ListWithCompilerOutput<data::Song, Result<Arc<SongData>, ShortSongError>>;

pub type InstrumentsAndSamplesData =
    ListPairWithCompilerOutputs<data::Instrument, InstrumentOutput, data::Sample, SampleOutput>;

pub struct ProjectData {
    pf_parent_path: ParentPathBuf,

    // This the value stored in `data::Project`, it is relative to `pf_parent_path`
    sound_effects_file: Option<SourcePathBuf>,

    default_sfx_flags: DefaultSfxFlags,
    sfx_export_order: GuiSfxExportOrder,

    // Using ShortSongError so I do not have to clong a complicated `SongError` struct.
    project_songs: ListWithCompilerOutput<data::Song, Result<Arc<SongData>, ShortSongError>>,

    instruments_and_samples: InstrumentsAndSamplesData,
}

impl ProjectData {
    pub fn instruments(&self) -> &ListWithCompilerOutput<data::Instrument, InstrumentOutput> {
        self.instruments_and_samples.list1()
    }

    pub fn samples(&self) -> &ListWithCompilerOutput<data::Sample, SampleOutput> {
        self.instruments_and_samples.list2()
    }
}

pub struct SoundEffectsData {
    header: String,
    sound_effects: ListWithCompilerOutput<SoundEffectInput, SoundEffectOutput>,
}

struct Project {
    sender: fltk::app::Sender<GuiMessage>,

    data: ProjectData,
    sfx_data: Option<SoundEffectsData>,

    #[allow(dead_code)]
    compiler_thread: std::thread::JoinHandle<()>,
    compiler_sender: mpsc::Sender<ToCompiler>,

    audio_sender: mpsc::Sender<AudioMessage>,
    audio_monitor: AudioMonitor,
    audio_monitor_timer: MonitorTimer,

    sample_analyser_dialog: SampleAnalyserDialog,

    tab_manager: TabManager,
    sfx_tab_selected: bool,

    project_tab: ProjectTab,
    samples_tab: SamplesTab,
    sound_effects_tab: SoundEffectsTab,

    song_tabs: HashMap<ItemId, SongTab>,

    /// Stores closed song tabs so they can be reused when opening a new song tab.
    ///
    /// This minimises the impact of a memory leak when closing a song tab.
    ///
    /// The leak occurs for two reasons:
    ///  1. `DisplayExt::set_buffer()` extends the lifetime of a `TextBuffer` to the lifetime of the program.
    ///  2. There might be an circular reference in the callbacks.
    closed_song_tabs: Vec<SongTab>,

    sfx_window: SfxWindow,
}

impl Project {
    fn new(
        pf: ProjectFile,
        tabs: fltk::group::Tabs,
        menu: Menu,
        sender: fltk::app::Sender<GuiMessage>,
        audio_sender: mpsc::Sender<AudioMessage>,
        audio_monitor: AudioMonitor,
    ) -> Self {
        let c = pf.contents;

        // ::TODO deduplicate paired names (instruments, samples)::
        let (sfx_export_order, sfx_renamed) =
            GuiSfxExportOrder::new_lossy(c.sound_effects, c.low_priority_sound_effects);
        let (songs, songs_renamed) = deduplicate_names(c.songs);
        let (instruments_and_samples, i_and_s_renamed) =
            deduplicate_two_name_vecs(c.instruments, c.samples);

        let total_renamed = sfx_renamed + songs_renamed + i_and_s_renamed;
        if total_renamed > 0 {
            dialog::message_title("Duplicate names found");
            dialog::alert_default(&format!("{} items have been renamed", total_renamed));
        }

        let data = ProjectData {
            pf_parent_path: pf.parent_path,

            sound_effects_file: c.sound_effect_file,

            default_sfx_flags: c.default_sfx_flags,
            sfx_export_order,

            project_songs: ListWithCompilerOutput::new(songs, driver_constants::MAX_N_SONGS),
            instruments_and_samples: ListPairWithCompilerOutputs::new(
                instruments_and_samples,
                driver_constants::MAX_INSTRUMENTS_AND_SAMPLES,
            ),
        };

        sender.send(GuiMessage::RecompileEverything);
        if data.sound_effects_file.is_some() {
            sender.send(GuiMessage::LoadSfxFile);
        }

        let (compiler_sender, compiler_reciever) = mpsc::channel();
        let compiler_thread = compiler_thread::create_bg_thread(
            data.pf_parent_path.clone(),
            compiler_reciever,
            sender.clone(),
            audio_sender.clone(),
        );

        let mut out = Self {
            tab_manager: TabManager::new(tabs, menu),
            sfx_tab_selected: false,

            sample_analyser_dialog: SampleAnalyserDialog::new(
                sender.clone(),
                compiler_sender.clone(),
                audio_sender.clone(),
            ),

            sfx_window: SfxWindow::new(sender.clone()),

            project_tab: ProjectTab::new(&data, sender.clone()),
            samples_tab: SamplesTab::new(&data.instruments_and_samples, sender.clone()),
            sound_effects_tab: SoundEffectsTab::new(sender.clone()),
            closed_song_tabs: Vec::new(),
            song_tabs: HashMap::new(),

            audio_sender,
            audio_monitor,
            audio_monitor_timer: MonitorTimer::new(sender.clone()),

            compiler_thread,
            compiler_sender,

            data,
            sfx_data: None,

            sender,
        };

        out.tab_manager
            .add_or_modify(&out.project_tab, None, Some("Project"));
        out.tab_manager
            .add_or_modify(&out.samples_tab, Some(pf.path), Some("Samples"));
        out.tab_manager
            .add_widget(out.sound_effects_tab.widget_mut(), Some("Sound Effects"));

        out.tab_manager.set_selected_tab(&out.project_tab);

        out
    }

    fn process(&mut self, m: GuiMessage) {
        match m {
            GuiMessage::FromCompiler(m) => {
                self.process_compiler_output(m);
            }

            GuiMessage::DefaultSfxFlagChanged(flags) => {
                self.data.default_sfx_flags = flags;
                let _ = self
                    .compiler_sender
                    .send(ToCompiler::DefaultSfxFlagChanged(flags));

                self.tab_manager.mark_unsaved(FileType::Project);
            }

            GuiMessage::EditSfxExportOrder(m) => {
                let a = self
                    .project_tab
                    .sfx_export_order
                    .process(m, &mut self.data.sfx_export_order);

                if let Some(a) = a {
                    let _ = self.compiler_sender.send(ToCompiler::SfxExportOrder(a));

                    self.tab_manager.mark_unsaved(FileType::Project);
                }
            }

            GuiMessage::EditProjectSongs(m) => {
                let (edited, c) = self.data.project_songs.process(m, &mut self.project_tab);

                if edited {
                    self.tab_manager.mark_unsaved(FileType::Project);
                }
                if let Some(c) = c {
                    self.sound_effects_tab.pf_songs_changed();

                    let _ = self.compiler_sender.send(ToCompiler::ProjectSongs(c));
                }
            }
            GuiMessage::SelectProjectSong(index) => {
                self.project_tab.song_table.set_selected_row(index)
            }

            GuiMessage::Instrument(m) => {
                let (changed, c) = self
                    .data
                    .instruments_and_samples
                    .process1(m, &mut self.samples_tab);

                if changed {
                    self.tab_manager.mark_unsaved(FileType::Project);
                }
                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::Instrument(c));
                }
            }
            GuiMessage::Sample(m) => {
                let (changed, c) = self
                    .data
                    .instruments_and_samples
                    .process2(m, &mut self.samples_tab);

                if changed {
                    self.tab_manager.mark_unsaved(FileType::Project);
                }
                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::Sample(c));
                }
            }
            GuiMessage::UserChangedSelectedInstrument => self
                .samples_tab
                .selected_item_changed(self.data.instruments()),
            GuiMessage::UserChangedSelectedSample => {
                self.samples_tab.selected_item_changed(self.data.samples())
            }
            GuiMessage::EditInstrument(id, inst) => {
                let (changed, c) =
                    self.data
                        .instruments_and_samples
                        .edit_item1(id, inst, &mut self.samples_tab);

                if changed {
                    self.tab_manager.mark_unsaved(FileType::Project);
                }
                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::Instrument(c));
                }
            }
            GuiMessage::EditSample(id, sample) => {
                let (changed, c) =
                    self.data
                        .instruments_and_samples
                        .edit_item2(id, sample, &mut self.samples_tab);

                if changed {
                    self.tab_manager.mark_unsaved(FileType::Project);
                }
                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::Sample(c));
                }
            }

            GuiMessage::EditSoundEffectList(m) => {
                if let Some(sfx_data) = &mut self.sfx_data {
                    let (changed, c) = sfx_data
                        .sound_effects
                        .process(m, &mut self.sound_effects_tab);

                    if changed {
                        self.tab_manager.mark_unsaved(FileType::SoundEffects);
                    }
                    if let Some(c) = c {
                        let _ = self.compiler_sender.send(ToCompiler::SoundEffects(c));
                    }
                }
            }
            GuiMessage::EditSoundEffect(id, sfx) => {
                if let Some(sfx_data) = &mut self.sfx_data {
                    let (changed, c) =
                        sfx_data
                            .sound_effects
                            .edit_item(id, sfx, &mut self.sound_effects_tab);

                    if changed {
                        self.tab_manager.mark_unsaved(FileType::SoundEffects);
                    }
                    if let Some(c) = c {
                        let _ = self.compiler_sender.send(ToCompiler::SoundEffects(c));
                    }
                }
            }
            GuiMessage::UserChangesSelectedSoundEffect => {
                if let Some(sfx_data) = &self.sfx_data {
                    self.sound_effects_tab
                        .selected_item_changed(&sfx_data.sound_effects)
                }
            }
            GuiMessage::SfxFileHeaderChanged => {
                self.tab_manager.mark_unsaved(FileType::SoundEffects);
            }
            GuiMessage::AddMissingSoundEffects => {
                if let Some(sfx_data) = &self.sfx_data {
                    sound_effects_tab::add_missing_sfx(&self.data, sfx_data, &self.sender);
                }
            }

            GuiMessage::SongChanged(id, mml) => {
                self.tab_manager.mark_unsaved(FileType::Song(id));
                let _ = self.compiler_sender.send(ToCompiler::SongChanged(id, mml));
            }
            GuiMessage::RecompileSong(id, mml) => {
                // RecompileSong should not mark the song as unsaved
                let _ = self.compiler_sender.send(ToCompiler::SongChanged(id, mml));
            }
            GuiMessage::PlaySong(id, mml, ticks_to_skip, channels_mask) => {
                // RecompileSong should not mark the song as unsaved
                let _ = self.compiler_sender.send(ToCompiler::CompileAndPlaySong(
                    id,
                    mml,
                    ticks_to_skip,
                    channels_mask,
                ));
            }
            GuiMessage::PlaySongForSfxTab(id, ticks) => {
                let _ = self
                    .compiler_sender
                    .send(ToCompiler::PlaySongWithSfxBuffer(id, ticks));
            }
            GuiMessage::PlaySoundEffectCommand(sfx_id, pan) => {
                let _ = self
                    .audio_sender
                    .send(AudioMessage::PlaySoundEffectCommand(sfx_id, pan));
            }
            GuiMessage::PlayEditedSoundEffect(id, pan) => {
                let _ = self
                    .compiler_sender
                    .send(ToCompiler::PlaySfxUsingSfxBuffer(id, pan));
            }
            GuiMessage::PlayInstrument(id, args) => {
                let _ = self
                    .compiler_sender
                    .send(ToCompiler::PlayInstrument(id, args));
            }
            GuiMessage::PlaySample(id, args) => {
                let _ = self.compiler_sender.send(ToCompiler::PlaySample(id, args));
            }
            GuiMessage::PauseAudio => {
                let _ = self.audio_sender.send(AudioMessage::Pause);
            }
            GuiMessage::PauseResumeAudio(id) => {
                let _ = self.audio_sender.send(AudioMessage::PauseResume(id));
            }
            GuiMessage::SetMusicChannels(id, channel_mask) => {
                let _ = self
                    .audio_sender
                    .send(AudioMessage::SetMusicChannels(id, channel_mask));
            }

            GuiMessage::AudioThreadStartedSong(song_id, song_data) => {
                if let Some(tab) = self.song_tabs.get_mut(&song_id) {
                    tab.audio_thread_started_song(song_data);
                    self.audio_monitor_timer.start();

                    for (&tab_song_id, tab) in self.song_tabs.iter_mut() {
                        if tab_song_id != song_id {
                            tab.clear_note_tracking();
                        }
                    }
                }
            }
            GuiMessage::AudioThreadResumedSong(song_id) => {
                if self.song_tabs.contains_key(&song_id) {
                    self.audio_monitor_timer.start();
                }
            }
            GuiMessage::SongMonitorTimeout => match self.audio_monitor.get() {
                Some(mon) => match mon.song_id {
                    Some(id) => match self.song_tabs.get_mut(&id) {
                        Some(tab) => tab.monitor_timer_elapsed(mon),
                        None => self.audio_monitor_timer.stop(),
                    },
                    None => {
                        // Playing the blank song or testing a sample
                        self.audio_monitor_timer.stop();
                        for tab in self.song_tabs.values_mut() {
                            tab.clear_note_tracking();
                        }
                    }
                },
                None => {
                    // Paused or stopped
                    self.audio_monitor_timer.stop();
                }
            },

            GuiMessage::RequestCloseSongTab(song_id) => {
                let ft = FileType::Song(song_id);
                if self.tab_manager.is_unsaved(&ft) {
                    let file_name = self.tab_manager.get_file_name(&ft);
                    close_unsaved_song_tab_dialog(song_id, file_name, &self.sender);
                } else {
                    self.close_song_tab(song_id)
                }
            }

            GuiMessage::SaveAndCloseSongTab(song_id) => {
                let ft = FileType::Song(song_id);
                let success = self.save_file(ft, SaveType::Save);
                if success {
                    self.close_song_tab(song_id);
                }
            }

            GuiMessage::ForceCloseSongTab(song_id) => {
                self.close_song_tab(song_id);
            }

            GuiMessage::QuitRequested => {
                let unsaved = self.tab_manager.unsaved_tabs();
                if unsaved.is_empty() {
                    fltk::app::quit();
                } else {
                    quit_with_unsaved_files_dialog(unsaved, self.sender.clone());
                }
            }

            GuiMessage::ForceQuit => {
                fltk::app::quit();
            }

            GuiMessage::SaveAllAndQuit(to_save) => {
                let success = self.save_all(to_save);
                if success {
                    // Double check all tabs have been saved
                    self.sender.send(GuiMessage::QuitRequested);
                }
            }

            GuiMessage::SaveSelectedTab => {
                if let Some(ft) = self.tab_manager.selected_file() {
                    self.save_file(ft, SaveType::Save);
                }
            }
            GuiMessage::SaveSelectedTabAs => {
                if let Some(ft) = self.tab_manager.selected_file() {
                    self.save_file(ft, SaveType::SaveAs);
                }
            }
            GuiMessage::SaveAllUnsaved => {
                self.save_all(self.tab_manager.unsaved_tabs());
            }

            GuiMessage::OpenSfxFileDialog => {
                if self.sfx_data.is_none() {
                    if let Some((source_path, sfx_file)) = open_sfx_file_dialog(&self.data) {
                        self.set_sound_effects_file(source_path);
                        self.maybe_set_sfx_file(sfx_file);
                    }
                }
            }
            GuiMessage::NewSfxFile => {
                if self.sfx_data.is_none() {
                    self.maybe_set_sfx_file(blank_sfx_file());
                    self.tab_manager.mark_unsaved(FileType::SoundEffects);
                }
            }
            GuiMessage::LoadSfxFile => {
                if self.sfx_data.is_none() {
                    if let Some(sfx_data) = load_pf_sfx_file(&self.data) {
                        self.maybe_set_sfx_file(sfx_data);
                    }
                }
            }
            GuiMessage::RecompileEverything => {
                self.recompile_everything();
            }

            GuiMessage::AddSongToProjectDialog => {
                add_song_to_pf_dialog(&self.sender, &self.data, &self.tab_manager);
            }

            GuiMessage::ShowSampleSizes => {
                self.samples_tab.show_sample_sizes_widget();
            }
            GuiMessage::OpenInstrumentSampleDialog(id) => {
                open_instrument_sample_dialog(&self.sender, &self.compiler_sender, &self.data, id);
            }
            GuiMessage::OpenSampleSampleDialog(id) => {
                open_sample_sample_dialog(&self.sender, &self.compiler_sender, &self.data, id);
            }

            GuiMessage::OpenAnalyseInstrumentDialog(id) => {
                if let Some((_, inst)) = self.data.instruments().get_id(id) {
                    self.sample_analyser_dialog.show_for_instrument(id, inst);
                }
            }
            GuiMessage::OpenAnalyseSampleDialog(id) => {
                if let Some((_, s)) = self.data.samples().get_id(id) {
                    self.sample_analyser_dialog.show_for_sample(id, s);
                }
            }
            GuiMessage::CommitSampleAnalyserChanges {
                id,
                freq,
                loop_setting,
            } => match id {
                InstrumentOrSampleId::Instrument(id) => {
                    if let Some((index, inst)) = self.data.instruments().get_id(id) {
                        self.process(GuiMessage::Instrument(ListMessage::ItemEdited(
                            index,
                            data::Instrument {
                                freq,
                                loop_setting,
                                ..inst.clone()
                            },
                        )));
                    }
                }
                InstrumentOrSampleId::Sample(id) => {
                    if let Some((index, sample)) = self.data.samples().get_id(id) {
                        self.process(GuiMessage::Sample(ListMessage::ItemEdited(
                            index,
                            data::Sample {
                                loop_setting,
                                ..sample.clone()
                            },
                        )));
                    }
                }
            },

            GuiMessage::SetProjectSongName(index, name) => {
                if let Some(s) = self.data.project_songs.get(index) {
                    self.sender
                        .send(GuiMessage::EditProjectSongs(ListMessage::ItemEdited(
                            index,
                            data::Song { name, ..s.clone() },
                        )))
                }
            }
            GuiMessage::NewMmlFile => self.new_blank_song_tab(),
            GuiMessage::OpenMmlFile => self.open_mml_file_dialog(),
            GuiMessage::OpenSongTab(index) => self.open_pf_song_tab(index),

            GuiMessage::ExportCurrentTabToSpcFile => {
                if let Some(FileType::Song(id)) = self.tab_manager.selected_file() {
                    let _ = self
                        .compiler_sender
                        .send(ToCompiler::ExportSongToSpcFile(id));
                }
            }

            GuiMessage::ClearSampleCacheAndRebuild => {
                let _ = self
                    .compiler_sender
                    .send(ToCompiler::ClearSampleCacheAndRebuild);
            }

            GuiMessage::ToggleSfxWindow => self.sfx_window.show_or_hide(),

            // Ignore these messages, they are handled by MainWindow
            GuiMessage::ShowAboutTab => (),
            GuiMessage::ShowOrHideHelpSyntax => (),
            GuiMessage::ShowLicensesDialog => (),
            GuiMessage::OpenProject => (),
            GuiMessage::NewProject => (),
            GuiMessage::SelectedTabChanged => (),
        }
    }

    fn process_compiler_output(&mut self, m: CompilerOutput) {
        match m {
            CompilerOutput::Panic(message) => {
                dialog::message_title("Compiler thread panicked");
                dialog::alert_default(&format!(
                    "The compiler thread panicked!\n\n{}\n\nThe compiler thread has been stopped and will not be restarted.",
                    message
                ));
            }

            CompilerOutput::Instrument(id, co) => {
                self.data.instruments_and_samples.set_compiler_output1(
                    id,
                    co,
                    &mut self.samples_tab,
                );
            }
            CompilerOutput::Sample(id, co) => {
                self.data.instruments_and_samples.set_compiler_output2(
                    id,
                    co,
                    &mut self.samples_tab,
                );
            }
            CompilerOutput::SoundEffect(id, co) => {
                if let Some(sfx_data) = &mut self.sfx_data {
                    sfx_data
                        .sound_effects
                        .set_compiler_output(id, co, &mut self.sound_effects_tab);

                    self.update_sfx_tab_color();
                }
            }
            CompilerOutput::Song(id, co) => {
                let pf_co = match &co {
                    Ok(sd) => Ok(sd.clone()),
                    Err(e) => Err(e.to_short_error()),
                };
                self.data
                    .project_songs
                    .set_compiler_output(id, pf_co, &mut self.project_tab);

                if let Some(song_tab) = self.song_tabs.get_mut(&id) {
                    self.tab_manager.set_tab_label_color(song_tab, co.is_ok());
                    song_tab.set_compiler_output(Some(co));
                }
            }

            CompilerOutput::CommonAudioData(cad) => {
                self.project_tab.memory_stats.cad_output_changed(&cad);
                self.tab_manager
                    .set_tab_label_color(&mut self.samples_tab, cad.is_ok_or_none());

                self.sfx_window.cad_output_changed(&cad);
                self.samples_tab.set_common_audio_data(cad);
            }

            CompilerOutput::NumberOfMissingSoundEffects(n_missing) => {
                self.project_tab.memory_stats.set_n_missing_sfx(n_missing);
                self.sound_effects_tab.n_missing_sfx_changed(n_missing);

                self.update_sfx_tab_color();
            }
            CompilerOutput::LargestSongSize(size) => {
                self.project_tab.memory_stats.set_largest_song(&size);
                self.samples_tab.set_largest_song(size);
            }

            CompilerOutput::SpcFileResult(r) => match r {
                Ok((name, data)) => save_spc_file_dialog(name, data),
                Err(e) => {
                    dialog::message_title("Error exporting song to SPC file");
                    dialog::alert_default(&e.to_string());
                }
            },

            CompilerOutput::SampleAnalysis(r) => {
                self.sample_analyser_dialog.analysis_from_compiler_thread(r)
            }
        }
    }

    fn recompile_everything(&self) {
        let _ = self.compiler_sender.send(ToCompiler::LoadProject(
            compiler_thread::ProjectToCompiler {
                default_sfx_flags: self.data.default_sfx_flags,
                sfx_export_order: self.data.sfx_export_order.clone(),
                pf_songs: self.data.project_songs.replace_all_vec(),
                instruments: self.data.instruments().replace_all_vec(),
                samples: self.data.samples().replace_all_vec(),
            },
        ));

        // Combine samples after they have been compiled
        if let Some(sfx_data) = &self.sfx_data {
            let _ = self.compiler_sender.send(ToCompiler::LoadSoundEffects(
                sfx_data.sound_effects.replace_all_vec(),
            ));
        }
    }

    fn selected_tab_changed(&mut self, window: &mut fltk::window::Window) {
        if self.sfx_tab_selected {
            let _ = self
                .compiler_sender
                .send(ToCompiler::FinishedEditingSoundEffects);
        }

        let selected_widget = self.tab_manager.selected_widget();

        self.sfx_tab_selected = selected_widget
            .as_ref()
            .is_some_and(|t| t.is_same(self.sound_effects_tab.widget()));

        self.tab_manager.selected_tab_changed();

        match self
            .tab_manager
            .selected_file_name()
            .or_else(|| self.tab_manager.project_file_name())
        {
            Some(file_name) => {
                window.set_label(&[file_name, WINDOW_TITLE_SUFFIX].concat());
            }
            None => window.set_label(DEFAULT_WINDOW_TITLE),
        }

        self.sound_effects_tab
            .selected_tab_changed(self.tab_manager.selected_file(), &self.data.project_songs);
        self.sfx_window
            .tab_changed(self.tab_manager.selected_file());
    }

    fn maybe_set_sfx_file(&mut self, sfx_file: SoundEffectsFile) {
        let sfx = convert_sfx_inputs_lossy(sfx_file.sound_effects);

        let (sfx, sfx_renamed) = deduplicate_names(sfx);
        if sfx_renamed > 0 {
            dialog::message_title("Duplicate names found");
            dialog::alert_default(&format!("{} sound effects have been renamed", sfx_renamed));
        }

        let sound_effects =
            ListWithCompilerOutput::new(sfx, driver_constants::MAX_SOUND_EFFECTS + 20);

        self.sound_effects_tab
            .replace_sfx_file(&sfx_file.header, &sound_effects);
        self.tab_manager.add_or_modify(
            &self.sound_effects_tab,
            sfx_file.path,
            Some("Sound Effects"),
        );

        let _ = self.compiler_sender.send(ToCompiler::LoadSoundEffects(
            sound_effects.replace_all_vec(),
        ));

        self.sfx_data = Some(SoundEffectsData {
            header: sfx_file.header,
            sound_effects,
        });
    }

    fn update_sfx_tab_color(&mut self) {
        let sfx_bc_valid = self
            .sfx_data
            .as_ref()
            .is_some_and(|s| s.sound_effects.all_valid());
        let no_missing_sfx = !self.sound_effects_tab.is_missing_sfx();

        self.tab_manager
            .set_tab_label_color(&mut self.sound_effects_tab, sfx_bc_valid && no_missing_sfx);
    }

    fn new_blank_song_tab(&mut self) {
        let id = ItemId::new();

        self.new_song_tab(id, blank_mml_file());
    }

    fn open_mml_file_dialog(&mut self) {
        if let Some(p) = open_mml_file_dialog(&self.data) {
            let pf_song_index = self
                .data
                .project_songs
                .item_iter()
                .position(|s| s.source == p.source_path);

            if let Some(index) = pf_song_index {
                self.open_pf_song_tab(index)
            } else {
                match self.tab_manager.find_file(&p.full_path) {
                    Some(FileType::Song(id)) => {
                        if let Some(song_tab) = self.song_tabs.get(&id) {
                            self.tab_manager.set_selected_tab(song_tab);
                        }
                    }
                    _ => {
                        self.load_new_song_tab(ItemId::new(), &p.source_path);
                    }
                }
            }
        }
    }

    fn open_pf_song_tab(&mut self, song_index: usize) {
        let (id, song) = match self.data.project_songs.get_with_id(song_index) {
            Some(v) => v,
            None => return,
        };

        if let Some(song_tab) = self.song_tabs.get_mut(&id) {
            self.tab_manager.set_selected_tab(song_tab);
        } else {
            self.load_new_song_tab(id, &song.source.clone());
        }
    }

    // NOTE: No deduplication. Do not create song tabs for a `song_id` or `path` that already exists
    fn load_new_song_tab(&mut self, song_id: ItemId, source: &SourcePathBuf) {
        if let Some(f) = load_mml_file(source, &self.data.pf_parent_path) {
            let song_tab = self.reuse_or_new_song_tab(song_id, &f);

            self.tab_manager.add_or_modify(&song_tab, f.path, None);
            self.tab_manager.set_selected_tab(&song_tab);

            self.song_tabs.insert(song_id, song_tab);

            // Update song in the compiler thread (in case the file changed)
            let _ = self
                .compiler_sender
                .send(ToCompiler::SongChanged(song_id, f.contents));

            self.sender.send(GuiMessage::SelectedTabChanged);
        }
    }

    // NOTE: minimal deduplication. Do not create song tabs for a `song_id` or `path` that already exists
    fn new_song_tab(&mut self, song_id: ItemId, file: data::TextFile) {
        // Cannot use `hash_map::Entry` here because of the reuse_or_new_song_tab call.
        #[allow(clippy::map_entry)]
        if !self.song_tabs.contains_key(&song_id) {
            let song_tab = self.reuse_or_new_song_tab(song_id, &file);

            self.tab_manager.add_or_modify(&song_tab, file.path, None);

            if song_tab.is_new_file() {
                self.tab_manager.mark_unsaved(FileType::Song(song_id));
            }
            self.tab_manager.set_selected_tab(&song_tab);

            self.song_tabs.insert(song_id, song_tab);

            // Update song in the compiler thread (in case the file changed)
            let _ = self
                .compiler_sender
                .send(ToCompiler::SongChanged(song_id, file.contents));

            self.sender.send(GuiMessage::SelectedTabChanged);
        }
    }

    /// Returns a previously closed or new SongTab with the given `song_id` and `mml_text`.
    fn reuse_or_new_song_tab(&mut self, song_id: ItemId, mml_text: &data::TextFile) -> SongTab {
        match self.closed_song_tabs.pop() {
            Some(mut song_tab) => {
                song_tab.reuse_tab(song_id, mml_text);
                song_tab
            }
            None => SongTab::new(song_id, mml_text, self.sender.clone()),
        }
    }

    // NOTE: Does not test if the song is unsaved before closing
    fn close_song_tab(&mut self, song_id: ItemId) {
        if let Some(song_tab) = self.song_tabs.remove(&song_id) {
            self.tab_manager.remove_tab(&song_tab);
            let _ = self
                .compiler_sender
                .send(ToCompiler::SongTabClosed(song_id));

            self.closed_song_tabs.push(song_tab);

            self.process(GuiMessage::SelectedTabChanged);
        }
    }

    fn save_file(&mut self, ft: FileType, save_type: SaveType) -> bool {
        match &ft {
            FileType::Project => {
                // No match required, cannot save_as a project
                self.tab_manager
                    .save_tab(ft, save_type, &self.data, &self.data)
                    .is_saved()
            }
            FileType::SoundEffects => match &mut self.sfx_data {
                Some(sfx_data) => {
                    sfx_data.header = self.sound_effects_tab.header_text();

                    match self
                        .tab_manager
                        .save_tab(ft, save_type, sfx_data, &self.data)
                    {
                        SaveResult::None => false,
                        SaveResult::Saved => true,
                        SaveResult::Renamed(source_path) => {
                            self.set_sound_effects_file(source_path);
                            true
                        }
                    }
                }
                None => false,
            },
            FileType::Song(id) => {
                let id = *id;
                match self.song_tabs.get(&id) {
                    Some(song_tab) => match self
                        .tab_manager
                        .save_tab(ft, save_type, song_tab, &self.data)
                    {
                        SaveResult::None => false,
                        SaveResult::Saved => true,
                        SaveResult::Renamed(source_path) => {
                            self.edit_pf_song_source(id, source_path);
                            true
                        }
                    },
                    None => false,
                }
            }
        }
    }

    fn save_all(&mut self, unsaved: Vec<FileType>) -> bool {
        let mut success = true;
        for f in unsaved {
            success &= self.save_file(f, SaveType::Save);
        }
        success
    }

    fn edit_pf_song_source(&mut self, id: ItemId, source: SourcePathBuf) {
        let pf_songs = &self.data.project_songs;

        if let Some((index, song)) = pf_songs.get_id(id) {
            // Update song source
            self.sender
                .send(GuiMessage::EditProjectSongs(ListMessage::ItemEdited(
                    index,
                    data::Song {
                        source,
                        ..song.clone()
                    },
                )));
        } else if let Some(song_tab) = self.song_tabs.get_mut(&id) {
            // If this the first time a song is saved, ask the user if they want to add the new
            // song to the project.
            if song_tab.is_new_file() && pf_songs.item_iter().all(|s| s.source != source) {
                dialog::message_title("New MML Song");
                let choice =
                    dialog::choice2_default("Add the song to the project?", "No", "Yes", "");
                if choice == Some(1) {
                    self.sender
                        .send(GuiMessage::EditProjectSongs(ListMessage::AddWithItemId(
                            id,
                            data::Song {
                                name: song_name_from_path(&source),
                                source,
                            },
                        )));

                    // Recompile the song to update the song table
                    let _ = self
                        .compiler_sender
                        .send(ToCompiler::SongChanged(id, song_tab.contents()));
                }
            }
        }

        if let Some(t) = self.song_tabs.get_mut(&id) {
            t.clear_new_file_flag();
        }
    }

    fn set_sound_effects_file(&mut self, source: SourcePathBuf) {
        self.project_tab.sfx_file_changed(&source);
        self.data.sound_effects_file = Some(source);

        self.tab_manager.mark_unsaved(FileType::Project);
    }
}

impl ProjectData {
    pub fn to_project(&self) -> compiler::data::Project {
        compiler::data::Project {
            // Always update the About version
            about: data::About {
                version: CARGO_PKG_VERSION.to_owned(),
            },

            instruments: self.instruments().item_iter().cloned().collect(),
            samples: self.samples().item_iter().cloned().collect(),
            songs: self.project_songs.item_iter().cloned().collect(),

            default_sfx_flags: self.default_sfx_flags,

            sound_effects: self.sfx_export_order.normal_priority_sfx().to_owned(),
            low_priority_sound_effects: self.sfx_export_order.low_priority_sfx().to_owned(),

            sound_effect_file: self.sound_effects_file.clone(),
        }
    }
}

impl SoundEffectsData {
    pub fn header(&self) -> &str {
        &self.header
    }

    pub fn sound_effects_iter(&self) -> impl Iterator<Item = &SoundEffectInput> {
        self.sound_effects.item_iter()
    }
}

#[allow(dead_code)]
struct MainWindow {
    app: fltk::app::App,

    sender: fltk::app::Sender<GuiMessage>,

    #[allow(dead_code)]
    audio_thread: std::thread::JoinHandle<()>,
    audio_sender: mpsc::Sender<AudioMessage>,
    audio_monitor: AudioMonitor,

    window: fltk::window::Window,
    menu: Menu,

    row: fltk::group::Flex,
    tabs: fltk::group::Tabs,
    about_tab: AboutTab,

    help_widget: HelpWidget,
    licenses_dialog: LicensesDialog,

    project: Option<Project>,
}

impl MainWindow {
    fn new(sender: fltk::app::Sender<GuiMessage>) -> Self {
        let app = fltk::app::App::default();
        symbols::register_symbols();

        let mut window = fltk::window::Window::default().with_label(DEFAULT_WINDOW_TITLE);

        window.make_resizable(true);

        let window_size = (
            ch_units_to_width(&window, 100),
            ch_units_to_width(&window, 75),
        );
        window.set_size(window_size.0, window_size.1);
        window.size_range(window_size.0, window_size.1, 0, 0);
        let mut window = window.center_screen();

        let mut col = fltk::group::Flex::default_fill().column();

        let (audio_thread, audio_sender, audio_monitor) =
            audio_thread::create_audio_thread(sender.clone());

        let mut menu = Menu::new(sender.clone(), audio_sender.clone());
        menu.deactivate_project_items();
        col.fixed(menu.menu_bar(), input_height(menu.menu_bar()));

        let mut row = fltk::group::Flex::default().row();

        let mut tabs = fltk::group::Tabs::default();
        tabs.set_tab_align(fltk::enums::Align::Right);
        tabs.handle_overflow(fltk::group::TabsOverflow::Compress);
        tabs.end();

        let mut help = HelpWidget::new();
        row.fixed(help.widget(), ch_units_to_width(&row, 60));
        help.hide();

        row.end();
        col.end();

        window.end();

        let mut about_tab = AboutTab::new(tabs.clone(), sender.clone());
        about_tab.show();
        tabs.auto_layout();

        window.show();

        window.set_callback({
            let s = sender.clone();
            move |_| {
                if fltk::app::event() == fltk::enums::Event::Close {
                    s.send(GuiMessage::QuitRequested);
                }
            }
        });

        // Defocus inputs/text/tables when the user clicks outside them
        window.handle(|window, ev| match ev {
            fltk::enums::Event::Push => {
                if let Some(w) = fltk::app::belowmouse::<fltk::widget::Widget>() {
                    if !w.has_focus() && !window.has_focus() {
                        let _ = window.take_focus();
                    }
                }
                false
            }
            _ => false,
        });

        tabs.set_callback({
            let sender = sender.clone();
            move |_| {
                sender.send(GuiMessage::SelectedTabChanged);
            }
        });

        Self {
            app,
            sender,
            audio_thread,
            audio_sender,
            audio_monitor,
            window,
            menu,
            row,
            tabs,
            about_tab,
            help_widget: help,
            licenses_dialog: LicensesDialog::new(),
            project: None,
        }
    }

    fn load_project(&mut self, pf: ProjectFile) {
        if self.project.is_some() {
            return;
        }
        self.menu.project_loaded();
        self.about_tab.project_loaded();
        self.project = Some(Project::new(
            pf,
            self.tabs.clone(),
            self.menu.clone(),
            self.sender.clone(),
            self.audio_sender.clone(),
            self.audio_monitor.clone(),
        ));
        self.selected_tab_changed();
    }

    pub fn selected_tab_changed(&mut self) {
        if let Some(p) = &mut self.project {
            p.selected_tab_changed(&mut self.window);
        }
    }

    fn show_or_hide_help_syntax(&mut self) {
        if self.menu.is_help_syntax_checked() {
            let to_show = match &self.project {
                Some(p) => match p.tab_manager.selected_file() {
                    Some(FileType::SoundEffects) => Some(HelpSection::Bytecode),
                    Some(FileType::Song(_)) => Some(HelpSection::Mml),
                    _ => None,
                },
                None => None,
            };
            self.help_widget.show(to_show);
        } else {
            self.help_widget.hide();
        }
        self.row.layout();
    }

    fn process(&mut self, message: GuiMessage) {
        match message {
            GuiMessage::QuitRequested => match &mut self.project {
                Some(p) => p.process(message),
                None => fltk::app::quit(),
            },
            GuiMessage::ShowAboutTab => {
                self.about_tab.show();
                self.selected_tab_changed();
            }
            GuiMessage::SelectedTabChanged => {
                self.selected_tab_changed();
            }
            GuiMessage::ShowOrHideHelpSyntax => {
                self.show_or_hide_help_syntax();
            }
            GuiMessage::ShowLicensesDialog => {
                self.licenses_dialog.show();
            }
            GuiMessage::OpenProject => {
                if self.project.is_none() {
                    if let Some(pf) = open_project_dialog() {
                        self.load_project(pf);
                    }
                }
            }
            GuiMessage::NewProject => {
                if self.project.is_none() {
                    if let Some(pf) = new_project_dialog() {
                        self.load_project(pf);
                    }
                }
            }
            m => {
                if let Some(p) = &mut self.project {
                    p.process(m);
                }
            }
        }
    }
}

fn get_arg_filename() -> Option<PathBuf> {
    let mut args = env::args_os();

    if args.len() == 2 {
        args.nth(1).map(PathBuf::from)
    } else {
        None
    }
}

fn main() {
    let program_argument = get_arg_filename();

    let (sender, reciever) = fltk::app::channel::<GuiMessage>();

    let mut main_window = MainWindow::new(sender);

    if let Some(path) = program_argument {
        if let Some(pf) = load_project_file_or_show_error_message(&path) {
            main_window.load_project(pf);
        }
    }

    while main_window.app.wait() {
        if let Some(msg) = reciever.recv() {
            main_window.process(msg);
        }
    }
}
