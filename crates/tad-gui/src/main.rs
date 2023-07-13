//! Audio Driver GUI

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod helpers;
mod list_editor;
mod samples_tab;

use crate::list_editor::{ListEditor, ListMessage, ListState};
use crate::samples_tab::SamplesTab;

use compiler::{data, load_project_file, ProjectFile};

use fltk::prelude::*;

use std::env;
use std::path::PathBuf;

#[derive(Debug)]
pub enum Message {
    Instrument(ListMessage<data::Instrument>),
}

trait Tab {
    fn widget(&mut self) -> &mut fltk::group::Flex;
}

struct Project {
    pf: ProjectFile,

    instrument_state: ListState,

    samples_tab: SamplesTab,
}

impl Project {
    fn new(pf: ProjectFile, sender: fltk::app::Sender<Message>) -> Self {
        let mut p = Self {
            pf,
            instrument_state: ListState::default(),
            samples_tab: SamplesTab::new(sender),
        };

        p.samples_tab.list_changed(&p.pf.contents.instruments);

        p
    }

    fn process(&mut self, m: Message) {
        match m {
            Message::Instrument(m) => m.process(
                &mut self.instrument_state,
                &mut self.pf.contents.instruments,
                &mut self.samples_tab,
            ),
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

        self.add_tab(&mut project.samples_tab);

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
