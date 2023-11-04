//! A small modal dialogs that displays the program licensing text

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

extern crate fltk;
use fltk::misc::HelpView;
use fltk::prelude::{GroupExt, WidgetBase, WidgetExt, WindowExt};
use fltk::window::Window;

const LICENSES_SHORT_HTML: &str = include_str!(concat!(env!("OUT_DIR"), "/licenses-short.html"));

struct LicensesDialogContents {
    window: Window,
    view: HelpView,
}

pub struct LicensesDialog {
    dialog: Option<LicensesDialogContents>,
}

impl LicensesDialog {
    pub fn new() -> Self {
        Self { dialog: None }
    }

    pub fn show(&mut self) {
        match &mut self.dialog {
            Some(d) => d.show(),
            None => self.dialog = Some(LicensesDialogContents::new()),
        }
    }
}

impl LicensesDialogContents {
    fn new() -> Self {
        let mut window = Window::default().with_size(780, 480).with_label("Licences");
        window.make_resizable(true);

        let mut view = HelpView::default_fill();
        view.set_text_size(16);
        view.set_value(LICENSES_SHORT_HTML);

        window.end();

        window.make_modal(true);
        window.show();

        Self { window, view }
    }

    fn show(&mut self) {
        self.view.set_top_line(0);
        self.window.show();
    }
}
