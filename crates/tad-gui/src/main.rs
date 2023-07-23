//! Audio Driver GUI

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod files;
mod helpers;
mod list_editor;
mod tables;

mod project_tab;
mod samples_tab;
mod sound_effects_tab;

use crate::files::{add_song_to_pf_dialog, load_pf_sfx_file, open_sfx_file_dialog};
use crate::list_editor::{ListMessage, ListState};
use crate::project_tab::ProjectTab;
use crate::samples_tab::SamplesTab;
use crate::sound_effects_tab::SoundEffectsTab;

use compiler::sound_effects::{SoundEffectInput, SoundEffectsFile};
use compiler::{data, driver_constants, load_project_file, ProjectFile};

use fltk::prelude::*;

use std::env;
use std::path::PathBuf;

#[derive(Debug)]
pub enum Message {
    EditSfxExportOrder(ListMessage<data::Name>),
    EditProjectSongs(ListMessage<data::Song>),
    Instrument(ListMessage<data::Instrument>),

    // ::TODO add menu item for open/load SFX file::
    OpenSfxFileDialog,
    LoadSfxFile,

    EditSoundEffectList(ListMessage<SoundEffectInput>),

    AddSongToProjectDialog,
    SetProjectSongName(usize, data::Name),
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

    sfx_export_orders: ListState<data::Name>,
    project_songs: ListState<data::Song>,
    instruments: ListState<data::Instrument>,

    sound_effects: Option<ListState<SoundEffectInput>>,
}

struct Project {
    sender: fltk::app::Sender<Message>,

    data: ProjectData,

    project_tab: ProjectTab,
    samples_tab: SamplesTab,
    sound_effects_tab: SoundEffectsTab,
}

impl Project {
    fn new(pf: ProjectFile, sender: fltk::app::Sender<Message>) -> Self {
        let c = pf.contents;

        let data = ProjectData {
            pf_path: pf.path,
            pf_file_name: pf.file_name,
            pf_parent_path: pf.parent_path,

            sound_effects_file: c.sound_effect_file,

            sfx_export_orders: ListState::new(c.sound_effects, driver_constants::MAX_SOUND_EFFECTS),
            project_songs: ListState::new(c.songs, driver_constants::MAX_N_SONGS),
            instruments: ListState::new(c.instruments, driver_constants::MAX_INSTRUMENTS),

            sound_effects: None,
        };

        if data.sound_effects_file.is_some() {
            sender.send(Message::LoadSfxFile);
        }

        Self {
            project_tab: ProjectTab::new(
                &data.sfx_export_orders,
                &data.project_songs,
                sender.clone(),
            ),
            samples_tab: SamplesTab::new(&data.instruments, sender.clone()),
            sound_effects_tab: SoundEffectsTab::new(sender.clone()),

            data,
            sender,
        }
    }

    fn process(&mut self, m: Message) {
        match m {
            Message::EditSfxExportOrder(m) => {
                self.data
                    .sfx_export_orders
                    .process(m, &mut self.project_tab.sfx_export_order_table);
            }
            Message::EditProjectSongs(m) => {
                self.data
                    .project_songs
                    .process(m, &mut self.project_tab.song_table);
            }
            Message::Instrument(m) => {
                self.data.instruments.process(m, &mut self.samples_tab);
            }
            Message::EditSoundEffectList(m) => {
                if let Some(sound_effects) = &mut self.data.sound_effects {
                    sound_effects.process(m, &mut self.sound_effects_tab);
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

    fn maybe_set_sfx_file(&mut self, sfx_file: Option<SoundEffectsFile>) {
        if let Some(sfx_file) = sfx_file {
            let state = ListState::new(
                sfx_file.sound_effects,
                driver_constants::MAX_SOUND_EFFECTS + 20,
            );

            self.sound_effects_tab.replace_sfx_file(&state);
            self.data.sound_effects = Some(state)
        }
    }
}

#[allow(dead_code)]
struct MainWindow {
    app: fltk::app::App,
    window: fltk::window::Window,
    tabs: fltk::group::Tabs,

    project: Option<Project>,
}

impl MainWindow {
    fn new() -> Self {
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

        Self {
            app,
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
        let mut project = Project::new(pf, sender);

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

    let mut main_window = MainWindow::new();
    main_window.load_project(pf, sender);

    while main_window.app.wait() {
        if let Some(msg) = reciever.recv() {
            if let Some(p) = &mut main_window.project {
                p.process(msg);
            }
        }
    }
}
