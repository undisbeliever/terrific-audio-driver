//! Sound Effects Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::Pan;
use crate::compiler_thread::{ItemId, SfxError, SoundEffectOutput};
use crate::helpers::*;
use crate::list_editor::{
    CompilerOutputGui, LaVec, ListAction, ListButtons, ListData, ListEditor, ListEditorTable,
    ListMessage, ListState, TableCompilerOutput, TableMapping,
};
use crate::mml_editor::{CompiledEditorData, EditorBuffer, MmlEditor, TextErrorRef, TextFormat};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::tabs::{FileType, Tab};
use crate::{GuiMessage, ProjectData, SoundEffectsData};

use compiler::data;
use compiler::data::Name;
use compiler::driver_constants::{CENTER_PAN, MAX_PAN};
use compiler::errors::SfxErrorLines;
use compiler::sfx_file::SoundEffectsFile;
use compiler::sound_effects::{SoundEffectInput, SoundEffectText};

use compiler::time::TickCounter;
use fltk::app::{self, event_key};
use fltk::button::Button;
use fltk::enums::{Color, Event, Font, Key};
use fltk::frame::Frame;
use fltk::group::{Flex, Pack, PackType};
use fltk::input::{Input, IntInput};
use fltk::menu::Choice;
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};
use fltk::valuator::HorNiceSlider;
use fltk::widget::Widget;

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

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

struct SongChoice {
    choice: Choice,
    song_ids: Vec<ItemId>,

    song_choice_out_of_date: bool,
    pending_song_change: Option<ItemId>,
}

pub struct State {
    sender: app::Sender<GuiMessage>,
    selected: Option<usize>,
    selected_id: Option<ItemId>,
    old_name: Name,

    pan: HorNiceSlider,
    song_choice: SongChoice,
    song_start_ticks: IntInput,

    name: Input,
    sound_effect_type: Choice,
    editor: MmlEditor,

    error_lines: Option<SfxErrorLines>,
}

pub struct SoundEffectsTab {
    state: Rc<RefCell<State>>,

    header_buffer: Rc<RefCell<EditorBuffer>>,

    // Each sound effect gets it own buffer so they have their own undo/redo stack.
    sfx_buffers: LaVec<Option<Rc<RefCell<EditorBuffer>>>>,

    group: Flex,

    no_sfx_file_gui: Flex,

    sidebar: Flex,
    sfx_table: ListEditorTable<SoundEffectMapping>,

    missing_sfx: bool,
    add_missing_sfx_button: Button,

    main_group: Flex,
    main_toolbar: Pack,

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

        // NOTE: toolbar shortcuts are handled by the `group.handle()` callback below
        let mut play_button = button("@play", "Play sound effect (F5)");

        label_packed("  Pan:  ");
        let mut pan = HorNiceSlider::default().with_size(button_size * 5 / 2, button_size);
        pan.set_range(0.0, MAX_PAN as f64);
        pan.set_value(CENTER_PAN as f64);
        pan.set_slider_size(1.0 / MAX_PAN as f32);
        pan.set_tooltip("Pan");

        let mut reset_pan = button("@center_pan", "Center pan");

        let _spacer = Widget::default().with_size(button_size / 4, button_size);

        let song_choice = SongChoice::new(button_size * 4, 0);
        let mut song_start_ticks = IntInput::default().with_size(button_size * 3 / 2, 0);
        let mut play_song_button = button("@play", "Play song (F7)");
        let mut stop_button = button("@stop", "Stop (F8)");

        song_start_ticks.set_value("500");
        song_start_ticks.set_tooltip("Song start position (in ticks)");

        main_toolbar.end();

        let mut name_flex = Flex::default();

        let mut name = Input::default();
        name.set_tooltip("Sound effect name");

        let mut sound_effect_type = Choice::default();
        sound_effect_type.add_choice(SoundEffectTypeChoice::CHOICES);
        name_flex.fixed(&sound_effect_type, ch_units_to_width(&name_flex, 20));

        main_group.fixed(&name_flex, input_height(&name));
        name_flex.end();

        let mut editor = MmlEditor::new("", TextFormat::SoundEffectHeader);
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

        add_missing_sfx_button.deactivate();
        add_missing_sfx_button.set_callback({
            let s = sender.clone();
            move |_| s.send(GuiMessage::AddMissingSoundEffects)
        });

        reset_pan.set_callback({
            let mut p = pan.clone();
            move |_| {
                p.set_value(CENTER_PAN as f64);
            }
        });

        let state = Rc::new(RefCell::from(State {
            sender: sender.clone(),
            selected: None,
            selected_id: None,

            pan,
            song_choice,
            song_start_ticks,

            old_name: "sfx".parse().unwrap(),
            name: name.clone(),
            sound_effect_type,
            editor,
            error_lines: None,
        }));

        // Handle toolbar shortcut keys.
        // This is done here so focus is not stolen from the editor.
        group.handle({
            let s = state.clone();
            let sender = sender.clone();
            move |_widget, ev| match ev {
                Event::KeyDown => match event_key() {
                    Key::F5 => {
                        s.borrow_mut().play_sound_effect();
                        true
                    }
                    Key::F7 => {
                        s.borrow_mut().play_song();
                        true
                    }
                    Key::F8 => {
                        sender.send(GuiMessage::PauseAudio);
                        true
                    }
                    _ => false,
                },
                _ => false,
            }
        });

        play_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.play_sound_effect();
                }
            }
        });

        stop_button.set_callback({
            let s = sender.clone();
            move |_| {
                s.send(GuiMessage::PauseAudio);
            }
        });

        play_song_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.play_song();
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

        let header_buffer = state.borrow().editor.buffer();

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

            header_buffer,
            sfx_buffers: LaVec::new(),

            group,
            no_sfx_file_gui: no_sfx_file_group,

            sidebar,
            sfx_table,
            missing_sfx: false,
            add_missing_sfx_button,

            main_group,
            main_toolbar,
            name,

            console,
            console_buffer,
        };
        s.clear_selected();
        s
    }

    pub fn replace_sfx_file(
        &mut self,
        header: &str,
        state: &impl ListState<Item = SoundEffectInput>,
    ) {
        let v: Vec<_> = (0..state.list().len()).map(|_| None).collect();
        assert!(v.len() == state.list().len());

        self.clear_selected();
        self.state.borrow_mut().editor.set_text(header);

        self.sfx_buffers = LaVec::from_vec(v);
        self.sfx_table.replace(state);

        self.group.remove(&self.no_sfx_file_gui);

        self.main_group.show();
        self.group.layout();

        self.sidebar.activate();
        self.main_group.activate();
    }

    pub fn header_text(&self) -> String {
        self.header_buffer.borrow().text()
    }

    pub fn is_missing_sfx(&mut self) -> bool {
        self.missing_sfx
    }

    pub fn n_missing_sfx_changed(&mut self, n_missing: usize) {
        self.missing_sfx = n_missing != 0;
        self.add_missing_sfx_button.set_active(self.missing_sfx);
    }

    pub fn pf_songs_changed(&mut self) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.song_choice.pf_songs_changed();
        }
    }

    pub fn selected_tab_changed(&mut self, tab: Option<FileType>, pf_songs: &ListData<data::Song>) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.song_choice.selected_tab_changed(tab, pf_songs);
        }
    }
}

impl SongChoice {
    fn new(width: i32, height: i32) -> Self {
        Self {
            choice: Choice::default().with_size(width, height),
            song_ids: Vec::new(),
            song_choice_out_of_date: true,
            pending_song_change: None,
        }
    }

    fn selected_song_id(&self) -> Option<ItemId> {
        let index = usize::try_from(self.choice.value()).ok()?;
        self.song_ids.get(index).cloned()
    }

    fn pf_songs_changed(&mut self) {
        self.song_choice_out_of_date = true;
    }

    fn selected_tab_changed(&mut self, tab: Option<FileType>, pf_songs: &ListData<data::Song>) {
        match tab {
            Some(FileType::SoundEffects) => self.update_song_list(pf_songs),
            Some(FileType::Song(song_id)) => self.pending_song_change = Some(song_id),
            _ => (),
        }
    }

    fn update_song_list(&mut self, pf_songs: &ListData<data::Song>) {
        if self.song_choice_out_of_date {
            let old_song_id = usize::try_from(self.choice.value())
                .ok()
                .and_then(|i| self.song_ids.get(i))
                .cloned();

            self.choice.clear();
            self.song_ids.clear();

            for (id, song) in pf_songs.iter() {
                self.choice.add_choice(song.name.as_str());
                self.song_ids.push(*id);
            }

            if self.pending_song_change.is_none() {
                self.pending_song_change = old_song_id;
                if old_song_id.is_none() {
                    self.choice.set_value(0);
                }
            }

            self.song_choice_out_of_date = false;
        }

        if let Some(song_id) = &self.pending_song_change {
            let new_value = self.song_ids.iter().position(|i| i == song_id).unwrap_or(0);
            self.choice.set_value(new_value.try_into().unwrap_or(0));

            self.pending_song_change = None;
        }
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

            state.name.set_value("Header (not a sound effect)");
            state.name.deactivate();

            state.sound_effect_type.set_value(-1);
            state.sound_effect_type.deactivate();

            state.editor.set_buffer(self.header_buffer.clone());
        }

        self.sfx_table.clear_selected();

        self.main_toolbar.deactivate();
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
                    self.name.activate();

                    let type_choice = match &sfx.sfx {
                        SoundEffectText::BytecodeAssembly(_) => {
                            SoundEffectTypeChoice::BytecodeAssembly
                        }
                        SoundEffectText::Mml(_) => SoundEffectTypeChoice::Mml,
                    };

                    state.sound_effect_type.set_value(type_choice.to_i32());
                    state.sound_effect_type.activate();

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

                    self.sfx_table.set_selected(index, id, sfx);

                    self.main_toolbar.activate();
                    self.main_group.activate();
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
        match action {
            ListAction::Move(from, to) => {
                if self.selected == Some(*from) {
                    self.selected = Some(*to);
                }
            }
            ListAction::Remove(_) => {
                // Prevent `commit_sfx()` from sending a `ListMessage::ItemEdited` message for the old
                // index and overriding the next item in the list with the deleted value.
                self.selected = None;
                self.selected_id = None;
            }
            ListAction::None
            | ListAction::Add(..)
            | ListAction::AddMultiple(..)
            | ListAction::Edit(..) => (),
        }
    }

    fn play_song(&mut self) {
        if let Some(id) = self.song_choice.selected_song_id() {
            let ticks = TickCounter::new(self.song_start_ticks.value().parse().unwrap_or(0));

            self.sender.send(GuiMessage::PlaySongForSfxTab(id, ticks));
        }
    }

    fn play_sound_effect(&mut self) {
        self.commit_sfx();
        if let Some(id) = self.selected_id {
            let pan = Pan::checked_new(self.pan.value() as u8);

            self.sender.send(GuiMessage::PlayEditedSoundEffect(id, pan));
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
        } else {
            // Header
            self.sender.send(GuiMessage::SfxFileHeaderChanged);
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
            let editor_line = usize::try_from(error_line)
                .ok()
                .and_then(|i| i.checked_sub(error_lines.offset))
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

    let sfx_eo = data.sfx_export_orders.list().item_iter();
    let lp_sfx_eo = data.low_priority_sfx_export_orders.list().item_iter();

    let to_add: Vec<SoundEffectInput> = sfx_eo
        .chain(lp_sfx_eo)
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
