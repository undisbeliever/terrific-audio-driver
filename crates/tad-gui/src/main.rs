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
mod tabs;

mod project_tab;
mod samples_tab;
mod song_tab;
mod sound_effects_tab;

use crate::compiler_thread::{
    CompilerOutput, InstrumentOutput, ItemId, SoundEffectOutput, ToCompiler,
};
use crate::files::{
    add_song_to_pf_dialog, load_mml_file, load_pf_sfx_file,
    load_project_file_or_show_error_message, open_sfx_file_dialog,
};
use crate::helpers::input_height;
use crate::list_editor::{
    update_compiler_output, ListAction, ListMessage, ListState, ListWithCompilerOutput,
    ListWithSelection,
};
use crate::names::deduplicate_names;
use crate::project_tab::ProjectTab;
use crate::samples_tab::SamplesTab;
use crate::song_tab::SongTab;
use crate::sound_effects_tab::SoundEffectsTab;
use crate::tabs::{quit_with_unsaved_files_dialog, FileType, Tab, TabManager};

use compiler::sound_effects::{convert_sfx_inputs_lossy, SoundEffectInput, SoundEffectsFile};
use compiler::{data, driver_constants, ProjectFile};

use fltk::dialog;
use fltk::enums::Shortcut;
use fltk::menu;
use fltk::prelude::*;

use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use std::sync::mpsc;

#[derive(Debug)]
pub enum Message {
    SelectedTabChanged,

    SaveSelectedTabIfUnsaved,
    SaveAllUnsaved,
    QuitRequested,
    ForceQuit,
    SaveAllAndQuit(Vec<FileType>),

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

    OpenSongTab(usize),

    SongChanged(ItemId, String),

    FromCompiler(compiler_thread::CompilerOutput),
}

// ::TODO remove::
#[allow(dead_code)]
pub struct ProjectData {
    pf_parent_path: PathBuf,

    // This the value stored in `data::Project`, it is relative to `pf_parent_path`
    sound_effects_file: Option<PathBuf>,

    sfx_export_orders: ListWithSelection<data::Name>,
    project_songs: ListWithSelection<data::Song>,
    instruments: ListWithCompilerOutput<data::Instrument, InstrumentOutput>,
}

pub struct SoundEffectsData {
    header: String,
    sound_effects: ListWithCompilerOutput<SoundEffectInput, SoundEffectOutput>,
}

struct Project {
    sender: fltk::app::Sender<Message>,

    data: ProjectData,
    sfx_data: Option<SoundEffectsData>,

    #[allow(dead_code)]
    compiler_thread: std::thread::JoinHandle<()>,
    compiler_sender: mpsc::Sender<ToCompiler>,

    tabs: fltk::group::Tabs,
    tab_manager: TabManager,
    samples_tab_selected: bool,

    // MUST update `all_tabs_iter()` if a tab is added or removed
    project_tab: ProjectTab,
    samples_tab: SamplesTab,
    sound_effects_tab: SoundEffectsTab,
    song_tabs: HashMap<ItemId, SongTab>,
}

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
            pf_parent_path: pf.parent_path,

            sound_effects_file: c.sound_effect_file,

            sfx_export_orders: ListWithSelection::new(sfx_eo, driver_constants::MAX_SOUND_EFFECTS),
            project_songs: ListWithSelection::new(songs, driver_constants::MAX_N_SONGS),
            instruments: ListWithCompilerOutput::new(
                instruments,
                driver_constants::MAX_INSTRUMENTS,
            ),
        };

        sender.send(Message::SelectedTabChanged);
        sender.send(Message::RecompileEverything);
        if data.sound_effects_file.is_some() {
            sender.send(Message::LoadSfxFile);
        }

        let (compiler_sender, r) = mpsc::channel();
        let compiler_thread =
            compiler_thread::create_bg_thread(data.pf_parent_path.clone(), r, sender.clone());

        let mut out = Self {
            tab_manager: TabManager::new(tabs.clone()),
            tabs,
            samples_tab_selected: false,

            project_tab: ProjectTab::new(
                &data.sfx_export_orders,
                &data.project_songs,
                sender.clone(),
            ),

            samples_tab: SamplesTab::new(&data.instruments, sender.clone()),
            sound_effects_tab: SoundEffectsTab::new(sender.clone()),
            song_tabs: HashMap::new(),

            compiler_thread,
            compiler_sender,

            data,
            sfx_data: None,

            sender,
        };

        out.tab_manager.add_or_modify(&out.project_tab, None);
        out.tab_manager
            .add_or_modify(&out.samples_tab, Some(pf.path));
        out.tab_manager
            .add_widget(out.sound_effects_tab.widget_mut());
        out
    }

    fn process(&mut self, m: Message) {
        match m {
            Message::FromCompiler(m) => {
                self.process_compiler_output(m);
            }

            Message::SelectedTabChanged => (),

            Message::EditSfxExportOrder(m) => {
                let (a, c) = self
                    .data
                    .sfx_export_orders
                    .process(m, &mut self.project_tab.sfx_export_order_table);
                self.mark_project_file_unsaved(a);

                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::SfxExportOrder(c));
                }
            }
            Message::EditProjectSongs(m) => {
                let (a, c) = self
                    .data
                    .project_songs
                    .process(m, &mut self.project_tab.song_table);

                self.mark_project_file_unsaved(a);

                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::ProjectSongs(c));
                }
            }
            Message::Instrument(m) => {
                let (a, c) = self.data.instruments.process(m, &mut self.samples_tab);

                self.mark_project_file_unsaved(a);

                if let Some(c) = c {
                    let _ = self.compiler_sender.send(ToCompiler::Instrument(c));
                }
            }
            Message::EditSoundEffectList(m) => {
                if let Some(sfx_data) = &mut self.sfx_data {
                    let (a, c) = sfx_data
                        .sound_effects
                        .process(m, &mut self.sound_effects_tab);
                    if let Some(c) = c {
                        let _ = self.compiler_sender.send(ToCompiler::SoundEffects(c));
                    }
                    if !a.is_none() {
                        self.tab_manager.mark_unsaved(FileType::SoundEffects);
                    }
                }
            }
            Message::SongChanged(id, mml) => {
                self.tab_manager.mark_unsaved(FileType::Song(id.clone()));
                let _ = self.compiler_sender.send(ToCompiler::SongChanged(id, mml));
            }

            Message::QuitRequested => {
                let unsaved = self.tab_manager.unsaved_tabs();
                if unsaved.is_empty() {
                    fltk::app::quit();
                } else {
                    quit_with_unsaved_files_dialog(unsaved, self.sender.clone());
                }
            }

            Message::ForceQuit => {
                fltk::app::quit();
            }

            Message::SaveAllAndQuit(to_save) => {
                let success = self.save_all(to_save);
                if success {
                    // Double check all tabs have been saved
                    self.sender.send(Message::QuitRequested);
                }
            }

            Message::SaveSelectedTabIfUnsaved => {
                if let Some(ft) = self.tab_manager.selected_file() {
                    if self.tab_manager.is_unsaved(&ft) {
                        self.save_file(ft);
                    }
                }
            }
            Message::SaveAllUnsaved => {
                for u in self.tab_manager.unsaved_tabs() {
                    self.save_file(u);
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
            Message::OpenSongTab(index) => self.open_song_tab(index),
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
                if let Some(sfx_data) = &mut self.sfx_data {
                    sfx_data
                        .sound_effects
                        .set_compiler_output(id, co, &mut self.sound_effects_tab);
                }
            }
            CompilerOutput::Song(id, co) => {
                let co = Some(co);
                update_compiler_output(
                    id.clone(),
                    &co,
                    self.data.project_songs.list(),
                    &mut self.project_tab.song_table,
                );

                if let Some(song_tab) = self.song_tabs.get_mut(&id) {
                    song_tab.set_compiler_output(co);
                }
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

        if let Some(sfx_data) = &self.sfx_data {
            let _ = self.compiler_sender.send(ToCompiler::SoundEffects(
                sfx_data.sound_effects.replace_all_message(),
            ));
        }

        // Compile songs after samples have been compiled
        let _ = self.compiler_sender.send(ToCompiler::ProjectSongs(
            self.data.project_songs.replace_all_message(),
        ));
    }

    fn selected_tab_changed(&mut self, menu: &mut Menu) {
        if self.samples_tab_selected {
            let _ = self
                .compiler_sender
                .send(ToCompiler::FinishedEditingSamples);
        }

        self.samples_tab_selected = self
            .tabs
            .value()
            .is_some_and(|t| t.is_same(self.samples_tab.widget()));

        self.tab_manager.selected_tab_changed(menu);
    }

    fn maybe_set_sfx_file(&mut self, sfx_file: Option<SoundEffectsFile>) {
        if let Some(sfx_file) = sfx_file {
            let sfx = convert_sfx_inputs_lossy(sfx_file.sound_effects);

            let (sfx, sfx_renamed) = deduplicate_names(sfx);
            if sfx_renamed > 0 {
                dialog::message_title("Duplicate names found");
                dialog::alert_default(&format!("{} sound effects have been renamed", sfx_renamed));
            }

            let sound_effects =
                ListWithCompilerOutput::new(sfx, driver_constants::MAX_SOUND_EFFECTS + 20);

            self.sound_effects_tab.replace_sfx_file(&sound_effects);
            self.tab_manager
                .add_or_modify(&self.sound_effects_tab, sfx_file.path);

            let _ = self.compiler_sender.send(ToCompiler::SoundEffects(
                sound_effects.replace_all_message(),
            ));

            self.sfx_data = Some(SoundEffectsData {
                header: sfx_file.header,
                sound_effects,
            });
        }
    }

    fn open_song_tab(&mut self, song_index: usize) {
        let (id, song) = match self.data.project_songs.list().get_with_id(song_index) {
            Some(v) => v,
            None => return,
        };

        if let Some(song_tab) = self.song_tabs.get_mut(id) {
            let _ = self.tabs.set_value(song_tab.widget());
            self.sender.send(Message::SelectedTabChanged);
        } else {
            self.create_new_song_tab(id.clone(), &song.source.clone());
        }
    }

    // NOTE: No deduplication. Do not create song tabs for a `song_id` or `path` that already exists
    fn create_new_song_tab(&mut self, song_id: ItemId, path: &Path) {
        if let Some(f) = load_mml_file(&self.data, path) {
            let song_tab = SongTab::new(song_id.clone(), &f, self.sender.clone());

            self.tab_manager.add_or_modify(&song_tab, f.path);
            let _ = self.tabs.set_value(song_tab.widget());

            self.song_tabs.insert(song_id.clone(), song_tab);

            // Update song in the compiler thread (in case the file changed)
            let _ = self
                .compiler_sender
                .send(ToCompiler::SongChanged(song_id, f.contents));
        }
        self.sender.send(Message::SelectedTabChanged);
    }

    fn mark_project_file_unsaved<T>(&mut self, a: ListAction<T>) {
        if !a.is_none() {
            self.tab_manager.mark_unsaved(FileType::Project);
        }
    }

    fn save_file(&mut self, ft: FileType) -> bool {
        match &ft {
            FileType::Project => self.tab_manager.save_tab(ft, &self.data),
            FileType::SoundEffects => match &self.sfx_data {
                Some(sfx_data) => self.tab_manager.save_tab(ft, sfx_data),
                None => false,
            },
            FileType::Song(id) => match self.song_tabs.get(id) {
                Some(song_tab) => self.tab_manager.save_tab(ft, song_tab),
                None => false,
            },
        }
    }

    fn save_all(&mut self, unsaved: Vec<FileType>) -> bool {
        let mut success = true;
        for f in unsaved {
            success &= self.save_file(f);
        }
        success
    }
}

impl ProjectData {
    pub fn to_project(&self) -> compiler::data::Project {
        compiler::data::Project {
            instruments: self.instruments.list().item_iter().cloned().collect(),
            songs: self.project_songs.list().item_iter().cloned().collect(),
            sound_effects: self.sfx_export_orders.list().item_iter().cloned().collect(),

            sound_effect_file: self.sound_effects_file.clone(),
        }
    }
}

impl SoundEffectsData {
    pub fn header(&self) -> &str {
        &self.header
    }

    pub fn sound_effects_iter(&self) -> impl Iterator<Item = &SoundEffectInput> {
        self.sound_effects.list().item_iter()
    }
}

pub struct Menu {
    menu: menu::MenuBar,

    save: menu::MenuItem,
    save_all: menu::MenuItem,
}

impl Menu {
    fn new(sender: fltk::app::Sender<Message>) -> Self {
        let mut menu = fltk::menu::MenuBar::default();
        menu.set_frame(fltk::enums::FrameType::FlatBox);

        let mut add = |label, shortcut, flags, f: fn() -> Message| -> menu::MenuItem {
            let index = menu.add(label, shortcut, flags, {
                let s = sender.clone();
                move |_: &mut fltk::menu::MenuBar| s.send(f())
            });

            menu.at(index).unwrap()
        };

        let save = add(
            "&File/&Save",
            Shortcut::Ctrl | 's',
            fltk::menu::MenuFlag::Normal,
            || Message::SaveSelectedTabIfUnsaved,
        );
        let save_all = add(
            "&File/Save &All",
            Shortcut::Ctrl | Shortcut::Shift | 's',
            fltk::menu::MenuFlag::Normal,
            || Message::SaveAllUnsaved,
        );
        add(
            "&File/&Quit",
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || Message::QuitRequested,
        );

        Menu {
            menu,
            save,
            save_all,
        }
    }

    fn deactivate(&mut self) {
        self.save.deactivate();
        self.save_all.deactivate();
    }

    fn activate(&mut self) {
        self.save_all.activate();
    }
}

#[allow(dead_code)]
struct MainWindow {
    app: fltk::app::App,

    sender: fltk::app::Sender<Message>,

    window: fltk::window::Window,
    menu: Menu,
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

        let mut col = fltk::group::Flex::default_fill().column();

        let mut menu = Menu::new(sender.clone());
        menu.deactivate();
        col.fixed(&menu.menu, input_height(&menu.menu));

        let mut tabs = fltk::group::Tabs::default();
        tabs.set_tab_align(fltk::enums::Align::Right);
        tabs.handle_overflow(fltk::group::TabsOverflow::Compress);

        tabs.end();
        tabs.auto_layout();

        col.end();

        window.end();
        window.show();

        window.set_callback({
            let s = sender.clone();
            move |_| {
                if fltk::app::event() == fltk::enums::Event::Close {
                    s.send(Message::QuitRequested);
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
                sender.send(Message::SelectedTabChanged);
            }
        });

        Self {
            app,
            sender,
            window,
            menu,
            tabs,
            project: None,
        }
    }

    fn load_project(&mut self, pf: ProjectFile, sender: fltk::app::Sender<Message>) {
        if self.project.is_some() {
            return;
        }
        self.menu.activate();
        self.project = Some(Project::new(pf, self.tabs.clone(), sender));
    }

    fn process(&mut self, message: Message) {
        match message {
            Message::SelectedTabChanged => {
                if let Some(p) = &mut self.project {
                    p.selected_tab_changed(&mut self.menu);
                }
            }
            Message::QuitRequested => match &mut self.project {
                Some(p) => p.process(message),
                None => fltk::app::quit(),
            },
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

    let (sender, reciever) = fltk::app::channel::<Message>();

    let mut main_window = MainWindow::new(sender.clone());

    if let Some(path) = program_argument {
        if let Some(pf) = load_project_file_or_show_error_message(&path) {
            main_window.load_project(pf, sender);
        }
    }

    while main_window.app.wait() {
        if let Some(msg) = reciever.recv() {
            main_window.process(msg);
        }
    }
}
