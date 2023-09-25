//! A sidebar widget that shows the help text

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

extern crate fltk;
use fltk::group::{Flex, Tabs};
use fltk::misc::HelpView;
use fltk::prelude::{GroupExt, WidgetExt};

pub enum HelpSection {
    Bytecode,
    Mml,
}

pub struct HelpWidget {
    tabs: Tabs,

    bytecode: Flex,
    mml: Flex,
}

const BYTECODE_SYNTAX_HTML: &str =
    include_str!(concat!(env!("OUT_DIR"), "/bytecode-assembly-syntax.html"));
const MML_SYNTAX_HTML: &str = include_str!(concat!(env!("OUT_DIR"), "/mml-syntax.html"));

fn create_view(label: &str, html: &str) -> Flex {
    let label = [label, " "].concat();

    let flex = Flex::default().with_label(&label);

    let mut view = HelpView::default();
    view.set_text_size(flex.label_size());
    view.set_value(html);

    flex.end();

    flex
}

impl HelpWidget {
    pub fn new() -> Self {
        let mut tabs = Tabs::default();
        tabs.set_tab_align(fltk::enums::Align::Right);
        tabs.handle_overflow(fltk::group::TabsOverflow::Compress);

        let bytecode = create_view("Bytecode Syntax", BYTECODE_SYNTAX_HTML);
        let mml = create_view("MML Syntax", MML_SYNTAX_HTML);

        tabs.end();
        tabs.auto_layout();

        Self {
            tabs,
            bytecode,
            mml,
        }
    }

    pub fn widget(&self) -> &Tabs {
        &self.tabs
    }

    pub fn show(&mut self, section: Option<HelpSection>) {
        if let Some(section) = section {
            let to_show = match section {
                HelpSection::Bytecode => &self.bytecode,
                HelpSection::Mml => &self.mml,
            };
            let _ = self.tabs.set_value(to_show);
        }
        self.tabs.show();
    }

    pub fn hide(&mut self) {
        self.tabs.hide();
    }
}
