//! Audio Driver GUI

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod compiler_thread;
mod files;
mod helpers;
mod list_editor;
mod names;
mod tables;

mod project_tab;
mod samples_tab;
mod sound_effects_tab;

use crate::compiler_thread::{CompilerOutput, InstrumentOutput, SoundEffectOutput, ToCompiler};
use crate::files::{add_song_to_pf_dialog, load_pf_sfx_file, open_sfx_file_dialog};
use crate::list_editor::{ListMessage, ListState, ListWithCompilerOutput, ListWithSelection};
use crate::names::deduplicate_names;
use crate::project_tab::ProjectTab;
use crate::samples_tab::SamplesTab;
use crate::sound_effects_tab::SoundEffectsTab;

use compiler::sound_effects::{convert_sfx_inputs_lossy, SoundEffectInput, SoundEffectsFile};
use compiler::{data, driver_constants, load_project_file, ProjectFile};

use fltk::dialog;
use fltk::prelude::*;
use list_editor::update_compiler_output;

use std::env;
use std::path::PathBuf;
use std::sync::mpsc;

#[derive(Debug)]
pub enum Message {
    SelectedTabChanged(Option<i32>),

    EditSfxExportOrder(ListMessage<data::Name>),
    EditProjectSongs(ListMessage<data::Song>),
    Instrument(ListMessage<data::Instrument>),

    // ::TODO add menu item for open/load SFX file::
    OpenSfxFileDialog,
    LoadSfxFile,

    RecompileEverything,

    EditSoundEffectList(ListMessage<SoundEffectInput>),

    AddSongToProjectDialog,
    SetProjectSongName(usize, data::Name),

    FromCompiler(compiler_thread::CompilerOutput),
}

trait Tab {
    fn widget(&mut self) -> &mut fltk::group::Flex;
}

// ::TODO remove::
#[allow(dead_code)]
pub struct ProjectData {
    pf_path: PathBuf,
    pf_file_name: String,
    pf_parent_path: PathBuf,

    // `sound_effects_file` is relative to `pf_parent_path`
    sound_effects_file: Option<PathBuf>,

    sfx_export_orders: ListWithSelection<data::Name>,
    project_songs: ListWithSelection<data::Song>,
    instruments: ListWithCompilerOutput<data::Instrument, InstrumentOutput>,

    sound_effects: Option<ListWithCompilerOutput<SoundEffectInput, SoundEffectOutput>>,
}

struct Project {
    sender: fltk::app::Sender<Message>,

    data: ProjectData,

    #[allow(dead_code)]
    compiler_thread: std::thread::JoinHandle<()>,
    compiler_sender: mpsc::Sender<ToCompiler>,

    tabs: fltk::group::Tabs,
    selected_tab: Option<i32>,

    project_tab: ProjectTab,
    samples_tab: SamplesTab,
    sound_effects_tab: SoundEffectsTab,
}

const SAMPLES_TAB_INDEX: i32 = 1;

impl Project {
    fn new(pf: ProjectFile, tabs: fltk::group::Tabs, sender: fltk::app::Sender<Message>) -> Self {
        let c = pf.contents;

        let (sfx_eo, sfx_eo_renamed) = deduplicate_names(c.sound_effects);
        let (songs, songs_renamed) = deduplicate_names(c.songs);
        let (instruments, instruments_renamed) = deduplicate_names(c.instruments);

        let total_renamed = sfx_eo_renamed + songs_renamed + instruments_renamed;
        if total_renamed > 0 {
            dialog::message_title("Duplicate names found");
            dialog::alert_default(&format!("{} items have been renamed", total_renamed));
        }

        let data = ProjectData {
            pf_path: pf.path,
            pf_file_name: pf.file_name,
            pf_parent_path: pf.parent_path,

            sound_effects_file: c.sound_effect_file,

            sfx_export_orders: ListWithSelection::new(sfx_eo, driver_constants::MAX_SOUND_EFFECTS),
            project_songs: ListWithSelection::new(songs, driver_constants::MAX_N_SONGS),
            instruments: ListWithCompilerOutput::new(
                instruments,
                driver_constants::MAX_INSTRUMENTS,
            ),

            sound_effects: None,
        };

        sender.send(Message::RecompileEverything);
        if data.sound_effects_file.is_some() {
            sender.send(Message::LoadSfxFile);
        }

        let (compiler_sender, r) = mpsc::channel();
        let compiler_thread =
            compiler_thread::create_bg_thread(data.pf_parent_path.clone(), r, sender.clone());

        Self {
            tabs,
            selected_tab: Some(0),

            project_tab: ProjectTab::new(
                &data.sfx_export_orders,
                &data.project_songs,
                sender.clone(),
            ),
            samples_tab: SamplesTab::new(&data.instruments, sender.clone()),
            sound_effects_tab: SoundEffectsTab::new(sender.clone()),

            compiler_thread,
            compiler_sender,

            data,
            sender,
        }
    }

    fn process(&mut self, m: Message) {
        match m {
            Message::FromCompiler(m) => {
                self.process_compiler_output(m);
            }

            Message::SelectedTabChanged(tab_index) => {
                if self.selected_tab == Some(SAMPLES_TAB_INDEX) {
                    let _ = self
                        .compiler_sender
                        .send(ToCompiler::FinishedEditingSamples);
                }
                self.selected_tab = tab_index;
            }

            Message::EditSfxExportOrder(m) => {
                let (_a, c) = self
                    .data
                    .sfx_export_orders
                    .process(m, &mut self.project_tab.sfx_export_order_table);

                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::SfxExportOrder(c));
                }
            }
            Message::EditProjectSongs(m) => {
                let (_a, c) = self
                    .data
                    .project_songs
                    .process(m, &mut self.project_tab.song_table);

                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::ProjectSongs(c));
                }
            }
            Message::Instrument(m) => {
                let (_a, c) = self.data.instruments.process(m, &mut self.samples_tab);

                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::Instrument(c));
                }
            }
            Message::EditSoundEffectList(m) => {
                if let Some(sound_effects) = &mut self.data.sound_effects {
                    let (_a, c) = sound_effects.process(m, &mut self.sound_effects_tab);
                    if let Some(c) = c {
                        let _ = self.compiler_sender.send(ToCompiler::SoundEffects(c));
                    }
                }
            }

            Message::OpenSfxFileDialog => {
                if let Some((pf_path, sfx_file)) = open_sfx_file_dialog(&self.data) {
                    // ::TODO mark project file as changed::
                    self.data.sound_effects_file = Some(pf_path);
                    self.maybe_set_sfx_file(sfx_file);
                }
            }
            Message::LoadSfxFile => {
                self.maybe_set_sfx_file(load_pf_sfx_file(&self.data));
            }
            Message::RecompileEverything => {
                self.recompile_everything();
            }

            Message::AddSongToProjectDialog => {
                add_song_to_pf_dialog(&self.sender, &self.data);
            }
            Message::SetProjectSongName(index, name) => {
                if let Some(s) = self.data.project_songs.list().get(index) {
                    self.sender
                        .send(Message::EditProjectSongs(ListMessage::ItemEdited(
                            index,
                            data::Song { name, ..s.clone() },
                        )))
                }
            }
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
                self.data
                    .instruments
                    .set_compiler_output(id, co, &mut self.samples_tab);
            }
            CompilerOutput::SoundEffect(id, co) => {
                if let Some(sound_effects) = &mut self.data.sound_effects {
                    sound_effects.set_compiler_output(id, co, &mut self.sound_effects_tab);
                }
            }
            CompilerOutput::Song(id, co) => {
                let co = Some(co);
                update_compiler_output(
                    id,
                    &co,
                    self.data.project_songs.list(),
                    &mut self.project_tab.song_table,
                );
            }

            CompilerOutput::CombineSamples(o) => {
                // ::TODO do something with `o`::

                self.samples_tab.set_combine_result(&o);

                if let Err(e) = o {
                    dialog::message_title("Error combining samples");
                    dialog::alert_default(&e.to_string());

                    let _ = self.tabs.set_value(self.samples_tab.widget());
                }
            }

            // ::TODO do something with these values::
            CompilerOutput::MissingSoundEffects(_missing) => (),
            CompilerOutput::SoundEffectsDataSize(_size) => (),
        }
    }

    fn recompile_everything(&self) {
        let _ = self.compiler_sender.send(ToCompiler::SfxExportOrder(
            self.data.sfx_export_orders.replace_all_message(),
        ));
        let _ = self.compiler_sender.send(ToCompiler::Instrument(
            self.data.instruments.replace_all_message(),
        ));

        // Combine samples after they have been compiled
        let _ = self
            .compiler_sender
            .send(ToCompiler::FinishedEditingSamples);

        if let Some(sfx) = &self.data.sound_effects {
            let _ = self
                .compiler_sender
                .send(ToCompiler::SoundEffects(sfx.replace_all_message()));
        }

        // Compile songs after samples have been compiled
        let _ = self.compiler_sender.send(ToCompiler::ProjectSongs(
            self.data.project_songs.replace_all_message(),
        ));
    }

    fn maybe_set_sfx_file(&mut self, sfx_file: Option<SoundEffectsFile>) {
        if let Some(sfx_file) = sfx_file {
            let sfx = convert_sfx_inputs_lossy(sfx_file.sound_effects);

            let (sfx, sfx_renamed) = deduplicate_names(sfx);
            if sfx_renamed > 0 {
                dialog::message_title("Duplicate names found");
                dialog::alert_default(&format!("{} sound effects have been renamed", sfx_renamed));
            }

            let state = ListWithCompilerOutput::new(sfx, driver_constants::MAX_SOUND_EFFECTS + 20);

            self.sound_effects_tab.replace_sfx_file(&state);

            let _ = self
                .compiler_sender
                .send(ToCompiler::SoundEffects(state.replace_all_message()));

            self.data.sound_effects = Some(state);
        }
    }
}

#[allow(dead_code)]
struct MainWindow {
    app: fltk::app::App,

    sender: fltk::app::Sender<Message>,

    window: fltk::window::Window,
    tabs: fltk::group::Tabs,

    project: Option<Project>,
}

impl MainWindow {
    fn new(sender: fltk::app::Sender<Message>) -> Self {
        let app = fltk::app::App::default();

        let mut window = fltk::window::Window::default()
            .with_size(800, 600)
            .center_screen()
            .with_label("Audio Driver GUI");

        window.make_resizable(true);

        let row = fltk::group::Flex::default_fill().row();

        let mut tabs = fltk::group::Tabs::default();
        tabs.set_tab_align(fltk::enums::Align::Right);
        tabs.handle_overflow(fltk::group::TabsOverflow::Compress);

        tabs.end();
        tabs.auto_layout();

        row.end();

        window.end();
        window.show();

        window.set_callback(|_| {
            if fltk::app::event() == fltk::enums::Event::Close {
                fltk::app::quit();
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
            move |t| {
                let selected_index = t.value().map(|w| t.find(&w));
                sender.send(Message::SelectedTabChanged(selected_index));
            }
        });

        Self {
            app,
            sender,
            window,
            tabs,
            project: None,
        }
    }

    fn add_tab(&mut self, tab: &mut impl Tab) {
        let w = tab.widget();
        w.set_margins(3, 5, 3, 3);
        self.tabs.add(w);

        self.tabs.auto_layout();
    }

    fn load_project(&mut self, pf: ProjectFile, sender: fltk::app::Sender<Message>) {
        if self.project.is_some() {
            return;
        }
        let mut project = Project::new(pf, self.tabs.clone(), sender);

        self.add_tab(&mut project.project_tab);
        self.add_tab(&mut project.samples_tab);
        self.add_tab(&mut project.sound_effects_tab);

        self.project = Some(project);
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
    // ::TODO handle errors::
    let pf = load_project_file(&get_arg_filename().unwrap()).unwrap();

    let (sender, reciever) = fltk::app::channel::<Message>();

    let mut main_window = MainWindow::new(sender.clone());
    main_window.load_project(pf, sender);

    while main_window.app.wait() {
        if let Some(msg) = reciever.recv() {
            if let Some(p) = &mut main_window.project {
                p.process(msg);
            }
        }
    }
}
