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
use fltk::text::{StyleTableEntryExt, TextBuffer, TextDisplay, TextEditor, WrapMode};

use std::cell::RefCell;
use std::cmp::{max, min};
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
    editor_widget: TextEditor,

    error_lines: Option<SfxErrorLines>,
}

pub struct SoundEffectsTab {
    state: Rc<RefCell<State>>,

    // Each sound effect gets it own buffer so they have their own undo/redo stack.
    sfx_buffers: LaVec<Option<EditorBuffer>>,

    group: Flex,

    no_sfx_file_gui: Flex,

    sidebar: Flex,
    sfx_table: ListEditorTable<SoundEffectMapping>,
    add_missing_sfx_button: Button,

    main_group: Flex,

    name: Input,
    editor: SfxEditor,

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

        let mut editor = SfxEditor::new();

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
            editor_widget: editor.widget.clone(),
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
                        s.commit_sfx_if_changed();
                    }
                }
                // Always propagate focus/enter events
                false
            }
        });

        state.borrow_mut().sound_effect_type.set_callback({
            let s = state.clone();
            move |_widget| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.commit_sfx();
                }
            }
        });

        editor.widget.handle({
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

        console.handle({
            let s = state.clone();
            let mut editor = editor.widget.clone();
            move |widget, ev| {
                if ev == Event::Released && app::event_clicks() {
                    if let Some(mut buffer) = widget.buffer() {
                        buffer.unselect();

                        if let Ok(state) = s.try_borrow() {
                            let line = widget.count_lines(0, widget.insert_position(), false);
                            let line = line.try_into().unwrap_or(0);
                            Self::error_line_clicked(line, &state, &mut editor);
                        }
                    }
                }
                false
            }
        });

        let mut s = Self {
            state,
            sfx_buffers: LaVec::new(),

            group,
            no_sfx_file_gui: no_sfx_file_group,

            sidebar,
            sfx_table,
            add_missing_sfx_button,

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
        let v: Vec<Option<EditorBuffer>> = (0..state.list().len()).map(|_| None).collect();
        assert!(v.len() == state.list().len());

        self.clear_selected();
        self.sfx_buffers = LaVec::from_vec(v);
        self.sfx_table.replace(state);

        self.group.remove(&self.no_sfx_file_gui);

        self.main_group.show();
        self.group.layout();

        self.sidebar.activate();
    }

    /// Move the editor cursor and highlight the line that caused the error.
    fn error_line_clicked(error_line: u32, state: &State, editor: &mut TextEditor) {
        if error_line < 1 {
            return;
        }
        if let Some(error_lines) = &state.error_lines {
            let editor_line = error_line
                .checked_sub(error_lines.offset)
                .and_then(|i| usize::try_from(i).ok())
                .and_then(|i| error_lines.lines.get(i))
                .and_then(|i| i32::try_from(*i).ok())
                .unwrap_or(1);

            let editor_line = max(1, editor_line);

            if let Some(mut buffer) = editor.buffer() {
                // Fl_Text_Buffer::skip_lines() is not available in fltk-rs, using `TextEditor::skip_lines()` instead.
                // `TextEditor::skip_lines()` skips visible (wrapped) lines.
                // For this function to work as expected word wrapping must be disabled on `editor`.
                let editor_pos = editor.skip_lines(0, editor_line - 1, true);
                let line_end = editor.line_end(editor_pos, true);

                editor.set_insert_position(editor_pos);
                editor.next_word();
                buffer.select(editor.insert_position(), line_end);

                editor.set_insert_position(line_end - 1);
                editor.show_insert_position();
                let _ = editor.take_focus();
            }
        }
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
        }

        self.sfx_table.clear_selected();

        self.name.set_value("");
        self.editor.remove_buffer();

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

                    let (type_choice, text) = match &sfx.sfx {
                        SoundEffectText::BytecodeAssembly(s) => {
                            (SoundEffectTypeChoice::BytecodeAssembly, s)
                        }
                        SoundEffectText::Mml(s) => (SoundEffectTypeChoice::Mml, s),
                    };

                    state.sound_effect_type.set_value(type_choice.to_i32());

                    match sfx_buffer {
                        Some(b) => self.editor.set_buffer(b),
                        None => {
                            let b = self.editor.new_buffer(text, self.state.clone());
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
        self.commit_sfx_if_changed();
        if let Some(id) = self.selected_id {
            self.sender.send(GuiMessage::PlaySoundEffect(id));
        }
    }

    fn commit_sfx_if_changed(&mut self) {
        if self.name.changed() || self.editor_widget.changed() {
            self.commit_sfx();
        }
    }

    fn commit_sfx(&mut self) {
        if let Some(index) = self.selected {
            if let Some(buf) = self.editor_widget.buffer() {
                if let Some(n) = Name::try_new_lossy(self.name.value()) {
                    self.name.set_value(n.as_str());
                    self.old_name = n;
                };

                let sfx = SoundEffectInput {
                    name: self.old_name.clone(),
                    sfx: match SoundEffectTypeChoice::read_widget(&self.sound_effect_type) {
                        SoundEffectTypeChoice::BytecodeAssembly => {
                            SoundEffectText::BytecodeAssembly(buf.text())
                        }
                        SoundEffectTypeChoice::Mml => SoundEffectText::Mml(buf.text()),
                    },
                };
                self.sender
                    .send(GuiMessage::EditSoundEffectList(ListMessage::ItemEdited(
                        index, sfx,
                    )));

                self.name.clear_changed();
                self.editor_widget.clear_changed();
            }
        }
    }
}

impl CompilerOutputGui<SoundEffectOutput> for SoundEffectsTab {
    fn set_compiler_output(&mut self, index: usize, compiler_output: &Option<SoundEffectOutput>) {
        self.sfx_table.set_compiler_output(index, compiler_output);
    }

    fn set_selected_compiler_output(&mut self, compiler_output: &Option<SoundEffectOutput>) {
        // ::MAYDO highlight invalid lines::

        match compiler_output {
            None => self.console_buffer.set_text(""),
            Some(Ok(o)) => {
                self.console_buffer
                    .set_text(&format!("Sound effect compiled successfully: {} bytes", o));
                self.console.set_text_color(Color::Foreground);
                self.state.borrow_mut().error_lines = None;
            }
            Some(Err(e)) => {
                let (error_lines, text) = match e {
                    SfxError::Error(e) => (
                        Some(e.error_lines()),
                        format!("{}", e.multiline_display("line ")),
                    ),
                    SfxError::Dependency => (None, e.to_string()),
                };
                self.state.borrow_mut().error_lines = error_lines;

                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Red);
            }
        }
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

mod style {
    pub const NORMAL: char = 'A';
    pub const COMMENT: char = 'B';

    #[allow(dead_code)]
    pub const UNKNOWN: char = 'C';

    pub const UNKNOWN_STR: &str = "C";
}

fn highlight_data() -> Vec<StyleTableEntryExt> {
    vec![
        // 'A'
        StyleTableEntryExt {
            font: Font::Courier,
            ..StyleTableEntryExt::default()
        },
        // 'B'
        StyleTableEntryExt {
            color: Color::Blue,
            font: Font::Courier,
            ..StyleTableEntryExt::default()
        },
        // 'C' - unknown, used when the inserted text cannot be read
        StyleTableEntryExt {
            color: Color::Green,
            font: Font::Courier,
            ..StyleTableEntryExt::default()
        },
    ]
}

pub struct SfxEditor {
    widget: TextEditor,

    // An empty text buffer for when no sound effect is selected.
    no_selection_buffer: TextBuffer,

    style_buffer: TextBuffer,
}

// I cannot remove the `buffer_modified` callback from TextBuffer (`TextBuffer::remove_modify_callback()` takes a `FnMut` argument).
// Instead I add a new callback to each buffer as it is created.
// Wrapping `TextBuffer` in `EditorBuffer` ensures the `buffer_modified` callback is set correctly.
//
// `EditorBuffer` must only be used in the SfxEditor instance that created it.
struct EditorBuffer(TextBuffer);

impl SfxEditor {
    fn new() -> Self {
        let mut style_buffer = TextBuffer::default();
        style_buffer.can_undo(false);

        let mut no_selection_buffer = TextBuffer::default();
        no_selection_buffer.can_undo(false);

        let mut widget = TextEditor::default();
        widget.set_buffer(no_selection_buffer.clone());

        widget.set_linenumber_width(ch_units_to_width(&widget, 4));
        widget.set_text_font(Font::Courier);
        widget.set_highlight_data_ext(style_buffer.clone(), highlight_data());

        // `error_line_clicked` only works if there is no wrapping.
        widget.wrap_mode(WrapMode::None, 0);

        Self {
            widget,
            no_selection_buffer,
            style_buffer,
        }
    }

    fn new_buffer(&mut self, text: &str, state: Rc<RefCell<State>>) -> EditorBuffer {
        let mut b = TextBuffer::default();
        b.can_undo(true);
        b.set_tab_distance(4);
        b.set_text(text);

        b.add_modify_callback({
            let buffer = b.clone();
            let mut style_buffer = self.style_buffer.clone();
            let state = state;
            move |a, b, c, d, e: &str| {
                Self::buffer_modified(&buffer, &mut style_buffer, &state, a, b, c, d, e);
            }
        });

        let b = EditorBuffer(b);
        self.set_buffer(&b);

        b
    }

    fn set_buffer(&mut self, buf: &EditorBuffer) {
        let buf = &buf.0;

        let (style, _) = Self::style_text(&buf.text(), style::NORMAL);
        self.style_buffer.set_text(&style);

        self.widget.set_buffer(buf.clone());

        self.widget.clear_changed();
        self.widget.activate();
    }

    fn remove_buffer(&mut self) {
        self.style_buffer.set_text("");
        // Using `no_selection_buffer` so the Fl_Text_Editor always has a buffer attached to it.
        self.widget.set_buffer(self.no_selection_buffer.clone());

        self.widget.clear_changed();
        self.widget.deactivate();
    }

    fn style_text(sfx_text: &str, current_style: char) -> (String, char) {
        let mut style_char = current_style;

        let mut style = String::with_capacity(sfx_text.len());

        // Must use bytes here, even tho sfx_text is UTF-8
        for c in sfx_text.bytes() {
            match c {
                b';' => style_char = style::COMMENT,
                b'\n' => style_char = style::NORMAL,
                _ => (),
            }
            style.push(style_char);
        }
        (style, style_char)
    }

    #[allow(clippy::too_many_arguments)]
    fn buffer_modified(
        text_buffer: &TextBuffer,
        style_buffer: &mut TextBuffer,
        state: &Rc<RefCell<State>>,
        pos: i32,
        n_inserted: i32,
        n_deleted: i32,
        _n_restyled: i32,
        deleted_text: &str,
    ) {
        if n_inserted < 0 || n_deleted < 0 {
            return;
        }
        if n_inserted == 0 && n_deleted == 0 {
            return;
        }

        let style_before_pos = style_buffer
            .text_range(pos - 1, pos)
            .and_then(|s| s.chars().next())
            .unwrap_or(style::NORMAL);

        if n_inserted > 0 {
            if let Some(inserted_text) = text_buffer.text_range(pos, pos + n_inserted) {
                let (style, style_char) = Self::style_text(&inserted_text, style_before_pos);

                style_buffer.replace(pos, pos + n_deleted, &style);

                Self::update_style_after(style_buffer, text_buffer, pos + n_inserted, style_char);

                if inserted_text.contains('\n') || deleted_text.contains('\n') {
                    if let Ok(mut state) = state.try_borrow_mut() {
                        state.commit_sfx();
                    }
                }
            } else {
                // Cannot read inserted text, use unknown style instead.
                let n_inserted = n_inserted.try_into().unwrap_or(0);

                style_buffer.replace(pos, pos + n_deleted, &style::UNKNOWN_STR.repeat(n_inserted));
            }
        } else if n_deleted > 0 {
            style_buffer.remove(pos, pos + n_deleted);

            Self::update_style_after(style_buffer, text_buffer, pos, style_before_pos);

            if deleted_text.contains('\n') {
                if let Ok(mut state) = state.try_borrow_mut() {
                    state.commit_sfx();
                }
            }
        }
    }

    // Updates the style after `pos`
    fn update_style_after(
        style_buffer: &mut TextBuffer,
        text_buffer: &TextBuffer,
        pos: i32,
        style_char: char,
    ) {
        // Update style on the remainder of the line the insert ended on
        let style_at_pos = style_buffer
            .text_range(pos, pos + 1)
            .and_then(|s| s.chars().next());

        let style_at_pos = match style_at_pos {
            Some(c) => c,
            None => return,
        };

        if style_char != style_at_pos {
            let line_end = match text_buffer.find_char_forward(pos, '\n') {
                Some(i) => i,
                None => text_buffer.length(),
            };
            let comment_end = text_buffer.find_char_forward(pos, ';').unwrap_or(line_end);
            let end = min(line_end, comment_end);

            let n_new_comment_chars = end.wrapping_sub(pos);
            if n_new_comment_chars > 0 {
                if let Ok(count) = usize::try_from(n_new_comment_chars) {
                    style_buffer.replace(pos, end, &style_char.to_string().repeat(count));
                }
            }
        }
    }
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
