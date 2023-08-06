//! Sound Effects Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::SoundEffectOutput;
use crate::helpers::*;
use crate::list_editor::{
    CompilerOutputGui, LaVec, ListAction, ListButtons, ListEditor, ListEditorTable, ListMessage,
    ListState, TableCompilerOutput, TableMapping,
};
use crate::tables::{RowWithStatus, SingleColumnRow};
use crate::Message;
use crate::Tab;

use compiler::sound_effects::SoundEffectInput;
use compiler::Name;

use fltk::app;
use fltk::enums::{Color, Event, Font};
use fltk::group::{Flex, Pack, PackType};
use fltk::input::Input;
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, TextEditor, WrapMode};

use std::cell::RefCell;
use std::rc::Rc;

struct SoundEffectMapping;
impl TableMapping for SoundEffectMapping {
    type DataType = SoundEffectInput;
    type RowType = RowWithStatus<SingleColumnRow>;

    const CAN_CLONE: bool = true;

    fn type_name() -> &'static str {
        "sound effect"
    }

    fn headers() -> Vec<String> {
        vec!["Sound effects".to_owned()]
    }

    fn add_clicked() -> Message {
        Message::EditSoundEffectList(ListMessage::Add(SoundEffectInput {
            name: "name".parse().unwrap(),
            sfx: String::new(),
        }))
    }

    fn to_message(lm: ListMessage<SoundEffectInput>) -> Message {
        Message::EditSoundEffectList(lm)
    }

    fn new_row(i: &SoundEffectInput) -> Self::RowType {
        RowWithStatus::new_unchecked(SingleColumnRow(i.name.as_str().to_string()))
    }

    fn edit_row(r: &mut Self::RowType, i: &SoundEffectInput) -> bool {
        if r.columns.0 != i.name.as_str() {
            r.columns.0 = i.name.as_str().to_string();
            true
        } else {
            false
        }
    }
}

impl TableCompilerOutput for SoundEffectMapping {
    type CompilerOutputType = SoundEffectOutput;

    fn set_row_state(r: &mut Self::RowType, co: &Option<Self::CompilerOutputType>) -> bool {
        r.set_status_optional_result(co)
    }
}

pub struct State {
    sender: app::Sender<Message>,
    selected: Option<usize>,
    old_name: Name,

    name: Input,
    editor: TextEditor,
}

pub struct SoundEffectsTab {
    state: Rc<RefCell<State>>,

    // Each sound effect gets it own buffer so they have their own undo/redo stack.
    sfx_buffers: LaVec<Option<TextBuffer>>,

    group: Flex,

    sidebar: Flex,
    sfx_table: ListEditorTable<SoundEffectMapping>,

    main_group: Flex,

    name: Input,
    editor: TextEditor,

    console: TextDisplay,
    console_buffer: TextBuffer,
}

impl Tab for SoundEffectsTab {
    fn widget(&mut self) -> &mut Flex {
        &mut self.group
    }
}

impl SoundEffectsTab {
    pub fn new(sender: app::Sender<Message>) -> Self {
        let mut group = Flex::default_fill().with_label("Sound Effects").row();

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let mut sfx_table = ListEditorTable::new(sender.clone());

        let button_height = sfx_table.button_height();
        sidebar.fixed(&sfx_table.list_buttons().pack, button_height);

        sidebar.end();

        let mut main_group = Flex::default().column();

        let button_size = ch_units_to_width(&main_group, 5);

        let main_toolbar = Pack::default().with_type(PackType::Horizontal);
        main_group.fixed(&main_toolbar, button_size);

        main_toolbar.end();

        let mut name = Input::default();
        name.set_tooltip("Sound effect name");
        main_group.fixed(&name, input_height(&name));

        let mut editor = TextEditor::default();
        editor.set_linenumber_width(ch_units_to_width(&editor, 4));
        editor.set_text_font(Font::Courier);

        let mut console = TextDisplay::default();
        main_group.fixed(&console, button_height * 5);

        main_group.end();
        group.end();

        sidebar.deactivate();
        main_group.deactivate();

        let console_buffer = TextBuffer::default();
        console.set_text_font(Font::Courier);
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        let state = Rc::new(RefCell::from(State {
            sender,
            selected: None,
            old_name: "sfx".parse().unwrap(),
            name: name.clone(),
            editor: editor.clone(),
        }));

        name.handle({
            let s = state.clone();
            move |_widget, ev| {
                if is_input_done_event(ev) {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        s.commit_sfx_if_changed();
                    }
                }
                // Always propagate focus/enter events
                false
            }
        });

        editor.handle({
            let s = state.clone();
            move |_widget, ev| match ev {
                Event::Unfocus => {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        s.commit_sfx_if_changed();
                    }
                    false
                }
                _ => false,
            }
        });

        let mut s = Self {
            state,
            sfx_buffers: LaVec::new(),

            group,

            sidebar,
            sfx_table,

            main_group,
            name,
            editor,

            console,
            console_buffer,
        };
        s.clear_selected();
        s
    }

    pub fn replace_sfx_file(&mut self, state: &impl ListState<Item = SoundEffectInput>) {
        self.clear_selected();
        self.sfx_buffers = LaVec::from_vec(vec![None; state.list().len()]);
        self.sfx_table.replace(state);

        self.sidebar.activate();
    }
}

impl ListEditor<SoundEffectInput> for SoundEffectsTab {
    fn list_buttons(&mut self) -> &mut ListButtons {
        self.sfx_table.list_buttons()
    }

    fn list_edited(&mut self, action: &ListAction<SoundEffectInput>) {
        if let Ok(mut state) = self.state.try_borrow_mut() {
            state.list_edited(action);
        }

        self.sfx_table.list_edited(action);

        self.sfx_buffers.process_map(
            action,
            // new: Don't create a buffer until the sfx is selected
            |_| None,
            // edit: Do not change the buffer, ensures undo/redo stack is unchanged.
            |_, _| {},
        );
    }

    fn clear_selected(&mut self) {
        if let Ok(mut state) = self.state.try_borrow_mut() {
            state.commit_sfx();
            state.selected = None;
        }

        self.sfx_table.clear_selected();

        self.name.set_value("");
        self.editor.set_buffer(None);

        self.main_group.deactivate();
    }

    fn set_selected(&mut self, index: usize, sfx: &SoundEffectInput) {
        if let Some(sfx_buffer) = self.sfx_buffers.get_mut(index) {
            match self.state.try_borrow_mut() {
                Ok(mut state) => {
                    state.commit_sfx();

                    state.selected = Some(index);
                    state.old_name = sfx.name.clone();

                    self.name.set_value(sfx.name.as_str());
                    self.name.clear_changed();

                    if sfx_buffer.is_none() {
                        let mut buf = TextBuffer::default();
                        buf.set_text(&sfx.sfx);
                        buf.can_undo(true);
                        buf.set_tab_distance(4);

                        *sfx_buffer = Some(buf);
                    }
                    self.editor.set_buffer(sfx_buffer.clone());
                    self.editor.clear_changed();

                    self.main_group.activate();

                    self.sfx_table.set_selected(index, sfx);
                }
                // This should not happen
                Err(_) => self.main_group.deactivate(),
            }
        } else {
            self.clear_selected();
        }
    }
}

impl State {
    fn list_edited(&mut self, action: &ListAction<SoundEffectInput>) {
        if let ListAction::Move(from, to) = action {
            if self.selected == Some(*from) {
                self.selected = Some(*to);
            }
        }
    }

    fn commit_sfx_if_changed(&mut self) {
        if self.name.changed() || self.editor.changed() {
            self.commit_sfx();
        }
    }

    fn commit_sfx(&mut self) {
        if let Some(index) = self.selected {
            if let Some(buf) = self.editor.buffer() {
                if let Some(n) = Name::try_new_lossy(self.name.value()) {
                    self.name.set_value(n.as_str());
                    self.old_name = n;
                };

                let sfx = SoundEffectInput {
                    name: self.old_name.clone(),
                    sfx: buf.text(),
                };
                self.sender
                    .send(Message::EditSoundEffectList(ListMessage::ItemEdited(
                        index, sfx,
                    )));

                self.name.clear_changed();
                self.editor.clear_changed();
            }
        }
    }
}

impl CompilerOutputGui<SoundEffectOutput> for SoundEffectsTab {
    fn set_compiler_output(&mut self, index: usize, compiler_output: &Option<SoundEffectOutput>) {
        self.sfx_table.set_compiler_output(index, compiler_output);
    }

    fn set_selected_compiler_output(&mut self, compiler_output: &Option<SoundEffectOutput>) {
        // ::TODO highlight invalid lines::

        match compiler_output {
            None => self.console_buffer.set_text(""),
            Some(Ok(o)) => {
                self.console_buffer
                    .set_text(&format!("Sound effect compiled successfully: {} bytes", o));
                self.console.set_text_color(Color::Foreground);
            }
            Some(Err(e)) => {
                let text = format!("{}", e.multiline_display("line "));

                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Red);
            }
        }
    }
}
