//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::*;
use crate::list_editor::{
    create_list_button_callbacks, create_list_item_edited_checkbox_handler,
    create_list_item_edited_input_handler, ListButtons, ListEditor, ListMessage,
};
use crate::Message;
use crate::Tab;

use compiler::data::Instrument;
use compiler::STARTING_OCTAVE;

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use fltk::app;
use fltk::button::CheckButton;
use fltk::group::Flex;
use fltk::input::{FloatInput, Input, IntInput};
use fltk::prelude::*;
use fltk::tree::{Tree, TreeItem};

fn set_user_data(ti: &mut TreeItem, index: usize) {
    ti.set_user_data(index);
}

fn get_user_data(ti: &TreeItem) -> Option<usize> {
    // ti contains `usize` from `set_user_data`
    unsafe { ti.user_data::<usize>() }
}

fn blank_instrument() -> Instrument {
    Instrument {
        name: "name".parse().unwrap(),
        source: PathBuf::new(),
        freq: 500.0,
        looping: true,
        loop_point: None,
        dupe_block_hack: None,
        loop_resets_filter: false,
        first_octave: STARTING_OCTAVE,
        last_octave: STARTING_OCTAVE,
        adsr: None,
        gain: None,
        comment: None,
    }
}

pub struct InstrumentEditor {
    group: Flex,

    inst_data: Rc<RefCell<Instrument>>,

    name: Input,
    source: Input,
    freq: FloatInput,
    looping: CheckButton,
    loop_point: IntInput,
    dupe_block_hack: IntInput,
    loop_resets_filter: CheckButton,
    first_octave: IntInput,
    last_octave: IntInput,
    adsr: Input,
    gain: Input,
    comment: Input,
}

impl InstrumentEditor {
    fn new(s: app::Sender<Message>) -> Self {
        let inst_data = Rc::from(RefCell::from(blank_instrument()));

        let mut form = InputForm::new(18);

        let mut name = form.add_input::<Input>("Name:");
        let mut source = form.add_input::<Input>("Source:");
        let mut freq = form.add_input::<FloatInput>("Frequency:");
        let mut looping = form.add_checkbox_left("Looping");
        let mut loop_point = form.add_input::<IntInput>("Loop Point:");
        let mut dupe_block_hack = form.add_input::<IntInput>("Dupe block hack:");
        let mut loop_resets_filter = form.add_checkbox_right("Loop resets filter");
        let mut first_octave = form.add_input::<IntInput>("First octave:");
        let mut last_octave = form.add_input::<IntInput>("Last octave:");
        let mut adsr = form.add_input::<Input>("Adsr:");
        let mut gain = form.add_input::<Input>("Gain:");
        let mut comment = form.add_input::<Input>("Comment:");

        let group = form.take_group_end();

        macro_rules! ih {
            ($field:ident) => {
                create_list_item_edited_input_handler!(
                    $field, Instrument, $field, s, Instrument, inst_data
                );
            };
        }
        macro_rules! ch {
            ($field:ident) => {
                create_list_item_edited_checkbox_handler!(
                    $field, Instrument, $field, s, Instrument, inst_data
                );
            };
        }

        ih!(name);
        ih!(source);
        ih!(freq);
        ch!(looping);
        ih!(loop_point);
        ih!(dupe_block_hack);
        ch!(loop_resets_filter);
        ih!(first_octave);
        ih!(last_octave);
        ih!(adsr);
        ih!(gain);
        ih!(comment);

        let mut out = Self {
            group,
            inst_data,
            name,
            source,
            freq,
            looping,
            loop_point,
            dupe_block_hack,
            loop_resets_filter,
            first_octave,
            last_octave,
            adsr,
            gain,
            comment,
        };
        out.disable_editor();
        out
    }

    fn disable_editor(&mut self) {
        self.group.deactivate();

        self.name.set_value("");
        self.source.set_value("");
        self.freq.set_value("");
        self.looping.clear();
        self.loop_point.set_value("");
        self.dupe_block_hack.set_value("");
        self.loop_resets_filter.clear();
        self.first_octave.set_value("");
        self.last_octave.set_value("");
        self.adsr.set_value("");
        self.gain.set_value("");
    }

    fn set_data(&mut self, data: &Instrument) {
        macro_rules! set_widget {
            ($name:ident) => {
                InputHelper::set_widget_value(&mut self.$name, &data.$name);
            };
        }

        set_widget!(name);
        set_widget!(source);
        set_widget!(freq);
        self.looping.set(data.looping);
        set_widget!(loop_point);
        set_widget!(dupe_block_hack);
        self.loop_resets_filter.set(data.loop_resets_filter);
        set_widget!(first_octave);
        set_widget!(last_octave);
        set_widget!(adsr);
        set_widget!(gain);
        set_widget!(comment);

        self.inst_data.replace(data.clone());

        self.group.activate();
    }
}

pub struct SamplesTab {
    group: Flex,

    inst_list_buttons: ListButtons,

    tree: Tree,
    tree_items: Vec<TreeItem>,

    instrument_editor: InstrumentEditor,
}

impl Tab for SamplesTab {
    fn widget(&mut self) -> &mut Flex {
        &mut self.group
    }
}

impl SamplesTab {
    pub fn new(sender: app::Sender<Message>) -> Self {
        let mut group = Flex::default_fill().with_label("Samples").row();

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let mut inst_list_buttons = ListButtons::new("instrument", true);
        sidebar.fixed(&inst_list_buttons.pack, inst_list_buttons.add.height());

        create_list_button_callbacks!(&mut inst_list_buttons, Instrument, sender, blank_instrument);

        let mut tree = Tree::default();
        tree.set_root_label("Instruments");

        sidebar.end();

        let instrument_editor = InstrumentEditor::new(sender.clone());

        group.end();

        tree.set_callback({
            let s = sender;
            move |t| {
                if let Some(ti) = t.first_selected_item() {
                    if let Some(index) = get_user_data(&ti) {
                        s.send(Message::Instrument(ListMessage::ItemSelected(index)));
                    } else {
                        s.send(Message::Instrument(ListMessage::ClearSelection));
                    }
                }
            }
        });

        Self {
            group,
            inst_list_buttons,
            tree,
            tree_items: Vec::new(),
            instrument_editor,
        }
    }
}

impl ListEditor<Instrument> for SamplesTab {
    fn list_buttons(&mut self) -> &mut ListButtons {
        &mut self.inst_list_buttons
    }

    fn list_changed(&mut self, list: &[Instrument]) {
        for (ti, i) in self.tree_items.iter_mut().zip(list) {
            ti.set_label(i.name.as_str());
        }

        if self.tree_items.len() < list.len() {
            for (index, inst) in list.iter().enumerate().skip(self.tree_items.len()) {
                // Path must be unique for each element in the tree
                if let Some(mut ti) = self.tree.add(&format!("I {index}")) {
                    set_user_data(&mut ti, index);
                    ti.set_label(inst.name.as_str());
                    self.tree_items.push(ti);
                }
            }
        }

        if self.tree_items.len() > list.len() {
            for ti in self.tree_items.drain(list.len()..) {
                let _ = self.tree.remove(&ti);
            }
        }
    }

    fn clear_selected(&mut self) {
        if let Some(r) = self.tree.root() {
            let _ = self.tree.deselect_all(&r, false);
        }
        self.instrument_editor.disable_editor();
    }

    fn set_selected(&mut self, index: usize, inst: &Instrument) {
        match self.tree_items.get(index) {
            Some(ti) => {
                let _ = self.tree.select_only(ti, false);
                self.instrument_editor.set_data(inst);
            }
            None => self.clear_selected(),
        }
    }

    fn item_changed(&mut self, index: usize, inst: &Instrument) {
        if let Some(ti) = self.tree_items.get_mut(index) {
            // ::TODO test if label changed::
            ti.set_label(inst.name.as_str());
            self.tree.redraw();
        }
    }
}
