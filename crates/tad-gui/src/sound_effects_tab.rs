//! Sound Effects Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, SfxError, SoundEffectOutput};
use crate::helpers::*;
use crate::list_editor::{
    CompilerOutputGui, LaVec, ListAction, ListButtons, ListEditor, ListEditorTable, ListMessage,
    ListState, TableCompilerOutput, TableMapping,
};
use crate::mml_editor::{CompiledEditorData, EditorBuffer, MmlEditor, TextErrorRef, TextFormat};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::tabs::{FileType, Tab};
use crate::{GuiMessage, ProjectData, SoundEffectsData};

use compiler::data::Name;
use compiler::errors::SfxErrorLines;
use compiler::sound_effects::{SoundEffectInput, SoundEffectText, SoundEffectsFile};

use fltk::app;
use fltk::button::Button;
use fltk::enums::{Color, Event, Font};
use fltk::frame::Frame;
use fltk::group::{Flex, Pack, PackType};
use fltk::input::Input;
use fltk::menu::Choice;
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

// ::TODO read and write sound effects file header in Sound Effects Tab::

#[derive(Clone, Copy)]
enum SoundEffectTypeChoice {
    BytecodeAssembly = 0,
    Mml = 1,
}
impl SoundEffectTypeChoice {
    const CHOICES: &'static str = concat!["&Bytecode assembly", "|&MML",];

    fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::BytecodeAssembly,
            1 => Self::Mml,

            _ => Self::BytecodeAssembly,
        }
    }

    fn to_i32(self) -> i32 {
        self as i32
    }
}

pub fn blank_sfx_file() -> SoundEffectsFile {
    SoundEffectsFile {
        path: None,
        file_name: "new_file.txt".into(),
        header: String::new(),
        sound_effects: Vec::new(),
    }
}

struct SoundEffectMapping;
impl TableMapping for SoundEffectMapping {
    type DataType = SoundEffectInput;
    type RowType = RowWithStatus<SimpleRow<1>>;

    const CAN_CLONE: bool = true;
    const CAN_EDIT: bool = false;

    fn type_name() -> &'static str {
        "sound effect"
    }

    fn headers() -> Vec<String> {
        vec!["Sound effects".to_owned()]
    }

    fn add_clicked() -> GuiMessage {
        GuiMessage::EditSoundEffectList(ListMessage::Add(SoundEffectInput {
            name: "name".parse().unwrap(),
            sfx: SoundEffectText::BytecodeAssembly(String::new()),
        }))
    }

    fn to_message(lm: ListMessage<SoundEffectInput>) -> GuiMessage {
        GuiMessage::EditSoundEffectList(lm)
    }

    fn new_row(i: &SoundEffectInput) -> Self::RowType {
        RowWithStatus::new_unchecked(SimpleRow::new([i.name.as_str().to_string()]))
    }

    fn edit_row(r: &mut Self::RowType, i: &SoundEffectInput) -> bool {
        r.columns.edit_column(0, i.name.as_str())
    }
}

impl TableCompilerOutput for SoundEffectMapping {
    type CompilerOutputType = SoundEffectOutput;

    fn set_row_state(r: &mut Self::RowType, co: &Option<Self::CompilerOutputType>) -> bool {
        r.set_status_optional_result(co)
    }
}

pub struct State {
    sender: app::Sender<GuiMessage>,
    selected: Option<usize>,
    selected_id: Option<ItemId>,
    old_name: Name,

    name: Input,
    sound_effect_type: Choice,
    editor: MmlEditor,

    error_lines: Option<SfxErrorLines>,
}

pub struct SoundEffectsTab {
    state: Rc<RefCell<State>>,

    // An empty text buffer for when no sound effect is selected.
    no_selection_buffer: Rc<RefCell<EditorBuffer>>,

    // Each sound effect gets it own buffer so they have their own undo/redo stack.
    sfx_buffers: LaVec<Option<Rc<RefCell<EditorBuffer>>>>,

    group: Flex,

    no_sfx_file_gui: Flex,

    sidebar: Flex,
    sfx_table: ListEditorTable<SoundEffectMapping>,
    add_missing_sfx_button: Button,

    main_group: Flex,

    name: Input,

    console: TextDisplay,
    console_buffer: TextBuffer,
}

impl Tab for SoundEffectsTab {
    fn widget(&self) -> &Flex {
        &self.group
    }

    fn widget_mut(&mut self) -> &mut Flex {
        &mut self.group
    }

    fn file_type(&self) -> FileType {
        FileType::SoundEffects
    }
}

impl SoundEffectsTab {
    pub fn new(sender: app::Sender<GuiMessage>) -> Self {
        let mut group = Flex::default_fill().row();

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let mut sfx_table = ListEditorTable::new(sender.clone());

        let button_height = sfx_table.button_height();
        sidebar.fixed(&sfx_table.list_buttons().pack, button_height);

        let mut add_missing_sfx_button = Button::default().with_label("Add missing sound effects");
        sidebar.fixed(
            &add_missing_sfx_button,
            input_height(&add_missing_sfx_button),
        );

        sidebar.end();

        let no_sfx_file_group = no_sfx_file_gui(sender.clone());

        let mut main_group = Flex::default().column();
        main_group.hide();

        let button_size = ch_units_to_width(&main_group, 5);

        let main_toolbar = Pack::default().with_type(PackType::Horizontal);
        main_group.fixed(&main_toolbar, button_size);

        let button = |label: &str, tooltip: &str| {
            let mut b = Button::default()
                .with_size(button_size, button_size)
                .with_label(label);
            b.set_tooltip(tooltip);
            b
        };

        let mut play_button = button("@>", "Play sound effect");

        main_toolbar.end();

        let mut name_flex = Flex::default();

        let mut name = Input::default();
        name.set_tooltip("Sound effect name");

        let mut sound_effect_type = Choice::default();
        sound_effect_type.add_choice(SoundEffectTypeChoice::CHOICES);
        name_flex.fixed(&sound_effect_type, ch_units_to_width(&name_flex, 20));

        main_group.fixed(&name_flex, input_height(&name));
        name_flex.end();

        let mut editor = MmlEditor::new("", TextFormat::Bytecode);
        editor.set_text_size(editor.widget().text_size() * 12 / 10);

        let mut console = TextDisplay::default();
        main_group.fixed(&console, button_height * 5);

        main_group.add(editor.status_bar());
        main_group.fixed(editor.status_bar(), input_height(editor.status_bar()));

        main_group.end();
        group.end();

        sidebar.deactivate();
        main_group.deactivate();

        let console_buffer = TextBuffer::default();
        console.set_text_font(Font::Courier);
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        add_missing_sfx_button.set_callback({
            let s = sender.clone();
            move |_| s.send(GuiMessage::AddMissingSoundEffects)
        });

        let state = Rc::new(RefCell::from(State {
            sender,
            selected: None,
            selected_id: None,

            old_name: "sfx".parse().unwrap(),
            name: name.clone(),
            sound_effect_type,
            editor,
            error_lines: None,
        }));

        play_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.play_sound_effect();
                }
            }
        });

        name.handle({
            let s = state.clone();
            move |_widget, ev| {
                if is_input_done_event(ev) {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        s.commit_sfx();
                    }
                }
                // Always propagate focus/enter events
                false
            }
        });

        let no_selection_buffer = state.borrow().editor.buffer();

        {
            let mut s = state.borrow_mut();

            s.sound_effect_type.set_callback({
                let s = state.clone();
                move |_widget| {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        s.sound_effect_type_changed();
                    }
                }
            });

            s.editor.set_changed_callback({
                let s = state.clone();
                move |buffer| {
                    if let Ok(s) = s.try_borrow() {
                        s.text_changed(buffer)
                    }
                }
            });
        }

        console.handle({
            let s = state.clone();
            move |widget, ev| {
                if ev == Event::Released && app::event_clicks() {
                    if let Some(mut buffer) = widget.buffer() {
                        buffer.unselect();

                        if let Ok(mut state) = s.try_borrow_mut() {
                            let line = widget.count_lines(0, widget.insert_position(), false);
                            let line = line.try_into().unwrap_or(0);
                            state.error_line_clicked(line);
                        }
                    }
                }
                false
            }
        });

        let mut s = Self {
            state,

            no_selection_buffer,
            sfx_buffers: LaVec::new(),

            group,
            no_sfx_file_gui: no_sfx_file_group,

            sidebar,
            sfx_table,
            add_missing_sfx_button,

            main_group,
            name,

            console,
            console_buffer,
        };
        s.clear_selected();
        s
    }

    pub fn replace_sfx_file(&mut self, state: &impl ListState<Item = SoundEffectInput>) {
        let v: Vec<_> = (0..state.list().len()).map(|_| None).collect();
        assert!(v.len() == state.list().len());

        self.clear_selected();
        self.sfx_buffers = LaVec::from_vec(v);
        self.sfx_table.replace(state);

        self.group.remove(&self.no_sfx_file_gui);

        self.main_group.show();
        self.group.layout();

        self.sidebar.activate();
    }

    pub fn n_missing_sfx_changed(&mut self, n_missing: usize) {
        self.add_missing_sfx_button.set_active(n_missing != 0);
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

            state.sound_effect_type.set_value(-1);

            state.editor.set_buffer(self.no_selection_buffer.clone());
        }

        self.sfx_table.clear_selected();

        self.name.set_value("");

        self.main_group.deactivate();
    }

    fn set_selected(&mut self, index: usize, id: ItemId, sfx: &SoundEffectInput) {
        if let Some(sfx_buffer) = self.sfx_buffers.get_mut(index) {
            match self.state.try_borrow_mut() {
                Ok(mut state) => {
                    state.commit_sfx();

                    state.selected = Some(index);
                    state.selected_id = Some(id);
                    state.old_name = sfx.name.clone();

                    self.name.set_value(sfx.name.as_str());
                    self.name.clear_changed();

                    let type_choice = match &sfx.sfx {
                        SoundEffectText::BytecodeAssembly(_) => {
                            SoundEffectTypeChoice::BytecodeAssembly
                        }
                        SoundEffectText::Mml(_) => SoundEffectTypeChoice::Mml,
                    };

                    state.sound_effect_type.set_value(type_choice.to_i32());

                    match sfx_buffer {
                        Some(b) => state.editor.set_buffer(b.clone()),
                        None => {
                            let (text, format) = match &sfx.sfx {
                                SoundEffectText::BytecodeAssembly(s) => (s, TextFormat::Bytecode),
                                SoundEffectText::Mml(s) => (s, TextFormat::Mml),
                            };

                            let b = state.editor.new_buffer(text, format);
                            *sfx_buffer = Some(b);
                        }
                    }

                    self.main_group.activate();

                    self.sfx_table.set_selected(index, id, sfx);
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

    fn play_sound_effect(&mut self) {
        self.commit_sfx();
        if let Some(id) = self.selected_id {
            self.sender.send(GuiMessage::PlaySoundEffect(id));
        }
    }

    fn sound_effect_type_changed(&mut self) {
        let f = match SoundEffectTypeChoice::read_widget(&self.sound_effect_type) {
            SoundEffectTypeChoice::BytecodeAssembly => TextFormat::Bytecode,
            SoundEffectTypeChoice::Mml => TextFormat::Mml,
        };
        self.editor.set_format(f);
        self.commit_sfx();
    }

    fn text_changed(&self, buffer: &EditorBuffer) {
        if let Some(index) = self.selected {
            let sfx = SoundEffectInput {
                name: self.old_name.clone(),
                sfx: match SoundEffectTypeChoice::read_widget(&self.sound_effect_type) {
                    SoundEffectTypeChoice::BytecodeAssembly => {
                        SoundEffectText::BytecodeAssembly(buffer.text())
                    }
                    SoundEffectTypeChoice::Mml => SoundEffectText::Mml(buffer.text()),
                },
            };
            self.sender
                .send(GuiMessage::EditSoundEffectList(ListMessage::ItemEdited(
                    index, sfx,
                )));
        }
    }

    fn commit_sfx(&mut self) {
        if let Some(index) = self.selected {
            let text = self.editor.text();

            if let Some(n) = Name::try_new_lossy(self.name.value()) {
                self.name.set_value(n.as_str());
                self.old_name = n;
            };

            let sfx = SoundEffectInput {
                name: self.old_name.clone(),
                sfx: match SoundEffectTypeChoice::read_widget(&self.sound_effect_type) {
                    SoundEffectTypeChoice::BytecodeAssembly => {
                        SoundEffectText::BytecodeAssembly(text)
                    }
                    SoundEffectTypeChoice::Mml => SoundEffectText::Mml(text),
                },
            };
            self.sender
                .send(GuiMessage::EditSoundEffectList(ListMessage::ItemEdited(
                    index, sfx,
                )));
        }
    }

    /// Move the editor cursor and highlight the line that caused the error.
    fn error_line_clicked(&mut self, error_line: u32) {
        if error_line < 1 {
            return;
        }
        if let Some(error_lines) = &self.error_lines {
            let editor_line = error_line
                .checked_sub(error_lines.offset)
                .and_then(|i| usize::try_from(i).ok())
                .and_then(|i| error_lines.lines.get(i))
                .copied()
                .unwrap_or(1);

            self.editor.move_cursor_to_line_end(editor_line);
        }
    }
}

impl CompilerOutputGui<SoundEffectOutput> for SoundEffectsTab {
    fn set_compiler_output(&mut self, index: usize, compiler_output: &Option<SoundEffectOutput>) {
        self.sfx_table.set_compiler_output(index, compiler_output);
    }

    fn set_selected_compiler_output(&mut self, compiler_output: &Option<SoundEffectOutput>) {
        let mut state = self.state.borrow_mut();

        match compiler_output {
            None => self.console_buffer.set_text(""),
            Some(Ok(o)) => {
                let bc_len = o.bytecode().len();
                let tc = o.tick_counter();
                let duration_ms = o.duration().as_millis();

                self.console_buffer.set_text(&format!(
                    "Sound effect compiled successfully: {} bytes\n{} ticks {}.{:03} seconds",
                    bc_len,
                    tc.value(),
                    duration_ms / 1000,
                    duration_ms % 1000,
                ));
                self.console.set_text_color(Color::Foreground);
                state.error_lines = None;

                state
                    .editor
                    .set_compiled_data(CompiledEditorData::SoundEffect(o.clone()));
            }
            Some(Err(e)) => {
                state.editor.clear_compiled_data();

                let (error_lines, text) = match e {
                    SfxError::Error(e) => (
                        Some(e.error_lines()),
                        format!("{}", e.multiline_display("line ")),
                    ),
                    SfxError::Dependency => (None, e.to_string()),
                };
                state.error_lines = error_lines;

                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Red);
            }
        }

        let e = match compiler_output {
            Some(Err(SfxError::Error(e))) => Some(TextErrorRef::SoundEffect(e)),
            _ => None,
        };
        state.editor.highlight_errors(e);
    }
}

fn no_sfx_file_gui(sender: app::Sender<GuiMessage>) -> Flex {
    let mut group = Flex::default().column();

    let button_width = ch_units_to_width(&group, 15);
    let line_height = input_height(&group);

    let label_frame = label("No sound effect file");
    group.fixed(&label_frame, line_height);

    let mut button_pack = Pack::default().with_size(button_width * 3, line_height * 2);
    group.fixed(&button_pack, line_height);
    button_pack.set_spacing(5);

    let button = |label: &str, f: fn() -> GuiMessage| {
        let mut b = Button::default()
            .with_size(button_width, 0)
            .with_label(label);
        b.set_callback({
            let s = sender.clone();
            move |_| {
                s.send(f());
            }
        });
        b
    };

    button("New File", || GuiMessage::NewSfxFile);
    button("Retry File", || GuiMessage::LoadSfxFile);
    button("Open File", || GuiMessage::OpenSfxFileDialog);

    button_pack.end();
    button_pack.set_type(fltk::group::PackType::Horizontal);

    Frame::default();

    group.end();

    group
}

pub fn add_missing_sfx(
    data: &ProjectData,
    sfx_data: &SoundEffectsData,
    sender: &fltk::app::Sender<GuiMessage>,
) {
    let sfx_list = sfx_data.sound_effects.list();
    let sfx_set: HashSet<&Name> = sfx_list.item_iter().map(|s| &s.name).collect();

    let to_add: Vec<SoundEffectInput> = data
        .sfx_export_orders
        .list()
        .item_iter()
        .filter_map(|sfx_name| {
            if !sfx_set.contains(sfx_name) {
                Some(SoundEffectInput {
                    name: sfx_name.clone(),
                    sfx: SoundEffectText::BytecodeAssembly(String::new()),
                })
            } else {
                None
            }
        })
        .collect();

    if !to_add.is_empty() {
        sender.send(GuiMessage::EditSoundEffectList(ListMessage::AddMultiple(
            to_add,
        )));
    }
}
