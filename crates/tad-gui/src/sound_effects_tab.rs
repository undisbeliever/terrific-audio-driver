//! Sound Effects Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{
    ItemId, SfxError, SfxSubroutineOutput, SfxSubroutinesError, SoundEffectOutput,
};
use crate::list_editor::{
    ListAction, ListEditorTable, ListMessage, ListWithCompilerOutput, ListWithCompilerOutputEditor,
    TableCompilerOutput, TableMapping,
};
use crate::menu::EditAction;
use crate::mml_editor::{CompiledEditorData, EditorBuffer, MmlEditor, TextErrorRef, TextFormat};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::tabs::{FileType, Tab};
use crate::{helpers::*, ProjectSongsData};
use crate::{GuiMessage, ProjectData, SoundEffectsData};

use compiler::data::{DefaultSfxFlags, Name};
use compiler::errors::SfxErrorLines;
use compiler::sfx_file::SoundEffectsFile;
use compiler::sound_effects::{
    SfxExportOrder, SfxFlags, SfxSubroutinesMml, SoundEffectInput, SoundEffectText,
};
use compiler::Pan;

use compiler::time::TickCounter;
use fltk::app::{self, event_key};
use fltk::button::{Button, CheckButton, RadioRoundButton};
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
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

// Sound effect file can hold more sound effects then export-order allows
pub const MAX_SFX_FILE_SOUND_EFFECTS: usize = 1024;

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
        subroutines: SfxSubroutinesMml(String::new()),
        sound_effects: Vec::new(),
    }
}

pub struct SoundEffectMapping;
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
            flags: SfxFlags::default(),
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

    fn user_changes_selection() -> Option<GuiMessage> {
        Some(GuiMessage::UserChangesSelectedSoundEffect)
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

    sfx_file_loaded: bool,

    main_group: Flex,

    show_subroutines: CheckButton,
    subroutines_group: Flex,
    subroutines_editor: MmlEditor,
    subroutine_output: Option<SfxSubroutineOutput>,

    pan: HorNiceSlider,
    song_choice: SongChoice,
    song_start_ticks: IntInput,

    selected_id: Option<ItemId>,
    old_name: Name,
    old_flags: SfxFlags,

    name: Input,
    sound_effect_type: Choice,
    one_channel_flag: SfxFlagRadios,
    interruptible_flag: SfxFlagRadios,
    editor: MmlEditor,

    console: TextDisplay,
    console_buffer: TextBuffer,

    error_lines: Option<SfxErrorLines>,
}

pub struct SoundEffectsTab {
    state: Rc<RefCell<State>>,

    no_sfx_buffer: Rc<RefCell<EditorBuffer>>,

    // Each sound effect gets it own buffer so they have their own undo/redo stack.
    sfx_buffers: HashMap<ItemId, Rc<RefCell<EditorBuffer>>>,

    group: Flex,

    no_sfx_file_gui: Flex,

    sidebar: Flex,
    sfx_table: ListEditorTable<SoundEffectMapping>,

    missing_sfx: bool,
    add_missing_sfx_button: Button,

    main_group: Flex,
    sfx_group: Flex,

    name: Input,
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
    pub fn new(sfx_flags: DefaultSfxFlags, sender: app::Sender<GuiMessage>) -> Self {
        let mut group = Flex::default_fill().row();

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let show_subroutines = CheckButton::default().with_label("Show subroutines");
        sidebar.fixed(&show_subroutines, input_height(&show_subroutines));

        let sfx_table = ListEditorTable::new(&mut sidebar, sender);

        let mut add_missing_sfx_button = Button::default().with_label("Add missing sound effects");
        sidebar.fixed(
            &add_missing_sfx_button,
            input_height(&add_missing_sfx_button),
        );

        sidebar.end();

        let no_sfx_file_group = no_sfx_file_gui(sender);

        let mut main_group = Flex::default().column();
        main_group.hide();

        let mut subroutines_group = Flex::default().column();
        subroutines_group.hide();

        let l = label_packed("SFX Subroutines:");
        subroutines_group.fixed(&l, l.height());

        let mut subroutines_editor = MmlEditor::new("", TextFormat::Mml);
        let text_size = subroutines_editor.widget().text_size() * 12 / 10;
        subroutines_editor.set_text_size(text_size);
        subroutines_group.fixed(
            subroutines_editor.status_bar(),
            input_height(subroutines_editor.status_bar()),
        );

        subroutines_group.end();

        let button_size = ch_units_to_width(&main_group, 5);

        let mut sfx_group = Flex::default().column();
        main_group.fixed(
            &sfx_group,
            3 * (input_height(&sfx_group) + sfx_group.pad()) + button_size,
        );

        let main_toolbar = Pack::default().with_type(PackType::Horizontal);
        sfx_group.fixed(&main_toolbar, button_size);

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
        pan.set_range(Pan::MIN.as_u8().into(), Pan::MAX.as_u8().into());
        pan.set_value(Pan::CENTER.as_u8().into());
        pan.set_slider_size(1.0 / Pan::MAX.as_u8() as f32);
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

        sfx_group.fixed(&name_flex, input_height(&name));
        name_flex.end();

        let one_channel_flag = SfxFlagRadios::new(
            &mut sfx_group,
            "One Channel",
            "The sound effect will play on a maximum of 1 channel\n(The sound effect will reset if it is interruptable)",
            "Both Channels",
            "The sound effect can play on both sound effect channels at the same time",
        );
        let interruptible_flag = SfxFlagRadios::new(
            &mut sfx_group,
            "Interruptible",
            "Can be interrupted by a play_sound_effect command",
            "Uninterruptible",
            "Will not be interrupted by a play_sound_effect command",
        );

        sfx_group.end();

        let mut editor = MmlEditor::new("", TextFormat::Bytecode);
        editor.set_text_size(text_size);

        let mut console = TextDisplay::default();
        main_group.fixed(&console, ch_units_to_width(&console, 10));

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
            let s = sender;
            move |_| s.send(GuiMessage::AddMissingSoundEffects)
        });

        reset_pan.set_callback({
            let mut p = pan.clone();
            move |_| {
                p.set_value(Pan::CENTER.as_u8().into());
            }
        });

        let state = Rc::new(RefCell::from(State {
            sender,

            sfx_file_loaded: false,

            main_group: main_group.clone(),

            show_subroutines,
            subroutines_group,
            subroutines_editor,
            subroutine_output: None,

            pan,
            song_choice,
            song_start_ticks,

            selected_id: None,
            old_name: "sfx".parse().unwrap(),
            old_flags: SfxFlags::default(),

            name: name.clone(),
            sound_effect_type,
            one_channel_flag,
            interruptible_flag,
            editor,
            error_lines: None,

            console,
            console_buffer,
        }));

        // Handle toolbar shortcut keys.
        // This is done here so focus is not stolen from the editor.
        group.handle({
            let s = state.clone();
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
            let s = sender;
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

            s.show_subroutines.set_callback({
                let s = state.clone();
                move |cb| {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        match cb.value() {
                            true => s.show_subroutines(),
                            false => s.hide_subroutines(),
                        }
                    }
                }
            });

            s.subroutines_editor.set_changed_callback({
                let s = state.clone();
                move |buffer| {
                    if let Ok(s) = s.try_borrow() {
                        s.subroutines_changed(buffer);
                    }
                }
            });

            s.default_sfx_flags_changed(sfx_flags);

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
                        s.sfx_text_changed(buffer)
                    }
                }
            });

            s.one_channel_flag.set_callback({
                let s = state.clone();
                move || {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        s.commit_sfx();
                    }
                }
            });
            s.interruptible_flag.set_callback({
                let s = state.clone();
                move || {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        s.commit_sfx();
                    }
                }
            });

            s.console.handle({
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
        }

        let mut s = Self {
            state,

            no_sfx_buffer: header_buffer,
            sfx_buffers: HashMap::new(),

            group,
            no_sfx_file_gui: no_sfx_file_group,

            sidebar,
            sfx_table,
            missing_sfx: false,
            add_missing_sfx_button,

            main_group,
            sfx_group,
            name,
        };
        s.disable_editor();
        s
    }

    pub fn replace_sfx_file(
        &mut self,
        subroutines: &SfxSubroutinesMml,
        sfx_list: &ListWithCompilerOutput<SoundEffectInput, SoundEffectOutput>,
    ) {
        self.state.borrow_mut().sfx_file_loaded = false;

        self.state
            .borrow_mut()
            .subroutines_editor
            .set_text(&subroutines.0);

        self.disable_editor();
        self.sfx_table.replace(sfx_list);

        // ::TODO save old buffers::
        self.sfx_buffers.clear();

        self.group.remove(&self.no_sfx_file_gui);

        self.main_group.show();
        self.group.layout();

        self.sidebar.activate();
        self.main_group.activate();

        self.state.borrow_mut().sfx_file_loaded = true;
    }

    pub fn sfx_subroutines_mml(&self) -> SfxSubroutinesMml {
        SfxSubroutinesMml(self.state.borrow().subroutines_editor.text())
    }

    pub fn is_missing_sfx(&mut self) -> bool {
        self.missing_sfx
    }

    pub fn default_sfx_flags_changed(&mut self, flags: DefaultSfxFlags) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.default_sfx_flags_changed(flags);
        }
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

    pub fn selected_tab_changed(&mut self, tab: Option<FileType>, pf_songs: &ProjectSongsData) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.song_choice.selected_tab_changed(tab, pf_songs);
        }
    }

    fn disable_editor(&mut self) {
        let mut state = self.state.borrow_mut();

        state.commit_sfx();

        state.selected_id = None;
        state.name.set_value("");
        state.sound_effect_type.set_value(-1);

        state.one_channel_flag.clear_value();
        state.interruptible_flag.clear_value();

        state.editor.set_buffer(self.no_sfx_buffer.clone());

        if let Some(Ok(_)) = &state.subroutine_output {
            state.console_buffer.set_text("");
        }

        state.editor.deactivate();
        self.sfx_group.deactivate();
    }

    fn enable_editor(
        &mut self,
        id: ItemId,
        sfx: &SoundEffectInput,
        co: &Option<SoundEffectOutput>,
    ) {
        let mut state = self.state.borrow_mut();

        state.commit_sfx();

        state.selected_id = Some(id);
        state.old_name = sfx.name.clone();
        state.old_flags = sfx.flags.clone();

        self.name.set_value(sfx.name.as_str());
        self.name.clear_changed();

        let type_choice = match &sfx.sfx {
            SoundEffectText::BytecodeAssembly(_) => SoundEffectTypeChoice::BytecodeAssembly,
            SoundEffectText::Mml(_) => SoundEffectTypeChoice::Mml,
        };
        state.sound_effect_type.set_value(type_choice.to_i32());

        state.one_channel_flag.set_value(sfx.flags.one_channel);
        state.interruptible_flag.set_value(sfx.flags.interruptible);

        let b = self.sfx_buffers.entry(id).or_insert_with(|| {
            let (text, format) = match &sfx.sfx {
                SoundEffectText::BytecodeAssembly(s) => (s, TextFormat::Bytecode),
                SoundEffectText::Mml(s) => (s, TextFormat::Mml),
            };
            state.editor.new_buffer(text, format)
        });
        state.editor.set_buffer(b.clone());

        state.selected_compiler_output_changed(co);

        state.editor.activate();
        self.sfx_group.activate();
        self.main_group.activate();
    }

    pub fn set_sfx_subroutine_compiler_output(&mut self, co: SfxSubroutineOutput) {
        self.state.borrow_mut().set_subroutine_compiler_output(co);
    }

    pub fn edit_action(&mut self, action: EditAction) {
        let mut s = self.state.borrow_mut();

        if s.subroutines_editor.has_focus() {
            s.subroutines_editor.edit_action(action)
        } else {
            s.editor.edit_action(action)
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

    fn selected_tab_changed(&mut self, tab: Option<FileType>, pf_songs: &ProjectSongsData) {
        match tab {
            Some(FileType::SoundEffects) => self.update_song_list(pf_songs),
            Some(FileType::Song(song_id)) => self.pending_song_change = Some(song_id),
            _ => (),
        }
    }

    fn update_song_list(&mut self, pf_songs: &ProjectSongsData) {
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

impl ListWithCompilerOutputEditor<SoundEffectInput, SoundEffectOutput> for SoundEffectsTab {
    type TableMapping = SoundEffectMapping;

    fn table_mut(&mut self) -> &mut ListEditorTable<Self::TableMapping> {
        &mut self.sfx_table
    }

    fn list_edited(&mut self, action: &ListAction<SoundEffectInput>) {
        if let Ok(mut state) = self.state.try_borrow_mut() {
            state.list_edited(action);
        }
    }

    fn item_removed(&mut self, id: ItemId) {
        self.sfx_buffers.remove(&id);
    }

    fn item_edited(&mut self, id: ItemId, sfx: &SoundEffectInput) {
        let mut s = self.state.borrow_mut();

        if s.selected_id == Some(id) && s.old_name != sfx.name {
            s.old_name = sfx.name.clone();
            s.name.set_value(sfx.name.as_str());
        }
    }

    fn set_compiler_output(
        &mut self,
        _index: usize,
        id: ItemId,
        compiler_output: &Option<SoundEffectOutput>,
    ) {
        let mut s = self.state.borrow_mut();
        if s.selected_id == Some(id) {
            s.selected_compiler_output_changed(compiler_output);
        }
    }

    fn selected_item_changed(
        &mut self,
        sfx_list: &ListWithCompilerOutput<SoundEffectInput, SoundEffectOutput>,
    ) {
        match sfx_list.get_selected_row(&self.sfx_table) {
            Some((id, sfx, co)) => {
                if self.state.borrow().selected_id != Some(id) {
                    self.enable_editor(id, sfx, co);
                }
            }

            None => {
                if self.state.borrow().selected_id.is_some() {
                    self.disable_editor();
                }
            }
        }
    }
}

impl State {
    fn list_edited(&mut self, _: &ListAction<SoundEffectInput>) {}

    fn play_song(&mut self) {
        if let Some(id) = self.song_choice.selected_song_id() {
            let ticks = TickCounter::new(self.song_start_ticks.value().parse().unwrap_or(0));

            self.sender.send(GuiMessage::PlaySongForSfxTab(id, ticks));
        }
    }

    fn play_sound_effect(&mut self) {
        self.commit_sfx();
        if let Some(id) = self.selected_id {
            let pan = Pan::try_from(self.pan.value() as u8).unwrap_or(Pan::CENTER);

            self.sender.send(GuiMessage::PlayEditedSoundEffect(id, pan));
        }
    }

    fn default_sfx_flags_changed(&mut self, flags: DefaultSfxFlags) {
        self.one_channel_flag
            .update_default_label(flags.one_channel);
        self.interruptible_flag
            .update_default_label(flags.interruptible);
    }

    fn sound_effect_type_changed(&mut self) {
        let f = match SoundEffectTypeChoice::read_widget(&self.sound_effect_type) {
            SoundEffectTypeChoice::BytecodeAssembly => TextFormat::Bytecode,
            SoundEffectTypeChoice::Mml => TextFormat::Mml,
        };
        self.editor.set_format(f);
        self.commit_sfx();
    }

    fn subroutines_changed(&self, buffer: &EditorBuffer) {
        if self.sfx_file_loaded {
            self.sender
                .send(GuiMessage::SfxSubroutinesChanged(SfxSubroutinesMml(
                    buffer.text(),
                )));
        }
    }

    fn sfx_text_changed(&self, buffer: &EditorBuffer) {
        if self.sfx_file_loaded {
            if let Some(id) = self.selected_id {
                let sfx = SoundEffectInput {
                    name: self.old_name.clone(),
                    flags: self.old_flags.clone(),
                    sfx: match SoundEffectTypeChoice::read_widget(&self.sound_effect_type) {
                        SoundEffectTypeChoice::BytecodeAssembly => {
                            SoundEffectText::BytecodeAssembly(buffer.text())
                        }
                        SoundEffectTypeChoice::Mml => SoundEffectText::Mml(buffer.text()),
                    },
                };
                self.sender.send(GuiMessage::EditSoundEffect(id, sfx));
            }
        }
    }

    fn commit_sfx(&mut self) {
        if self.sfx_file_loaded {
            if let Some(id) = self.selected_id {
                let text = self.editor.text();

                if let Some(n) = Name::try_new_lossy(self.name.value()) {
                    self.name.set_value(n.as_str());
                    self.old_name = n;
                };

                self.old_flags = SfxFlags {
                    one_channel: self.one_channel_flag.value(),
                    interruptible: self.interruptible_flag.value(),
                };

                let sfx = SoundEffectInput {
                    name: self.old_name.clone(),
                    flags: self.old_flags.clone(),
                    sfx: match SoundEffectTypeChoice::read_widget(&self.sound_effect_type) {
                        SoundEffectTypeChoice::BytecodeAssembly => {
                            SoundEffectText::BytecodeAssembly(text)
                        }
                        SoundEffectTypeChoice::Mml => SoundEffectText::Mml(text),
                    },
                };
                self.sender.send(GuiMessage::EditSoundEffect(id, sfx));
            }
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

    fn show_subroutines(&mut self) {
        self.show_subroutines.set_checked(true);
        self.subroutines_group.show();
        self.main_group.layout();
    }

    fn hide_subroutines(&mut self) {
        match self.subroutine_output {
            Some(Err(_)) => {
                self.show_subroutines.set_checked(true);
            }
            Some(Ok(_)) | None => {
                self.show_subroutines.set_checked(false);
                self.subroutines_group.hide();

                self.main_group.layout();
            }
        }
    }

    fn set_subroutine_compiler_output(&mut self, co: SfxSubroutineOutput) {
        match &co {
            Ok(o) => {
                if self.selected_id.is_none() {
                    self.console_buffer.set_text("");
                }

                self.subroutines_editor
                    .set_compiled_data(CompiledEditorData::SfxSubroutines(o.clone()));
            }
            Err(e) => {
                self.editor.clear_compiled_data();

                let error_text = match e {
                    SfxSubroutinesError::Error(e) => format!("{}", e.multiline_display("line ")),
                    SfxSubroutinesError::DependencyInstruments => e.to_string(),
                };
                self.console_buffer.set_text(&error_text);
                self.console.set_text_color(Color::Red);

                self.error_lines = None;

                self.show_subroutines();
            }
        }

        self.subroutines_editor.highlight_errors(match &co {
            Err(SfxSubroutinesError::Error(e)) => Some(TextErrorRef::SfxSubroutines(e)),
            _ => None,
        });

        self.subroutine_output = Some(co);
    }

    fn selected_compiler_output_changed(&mut self, compiler_output: &Option<SoundEffectOutput>) {
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
                self.error_lines = None;

                self.editor
                    .set_compiled_data(CompiledEditorData::SoundEffect(o.clone()));
            }
            Some(Err(e)) => {
                self.editor.clear_compiled_data();

                match e {
                    SfxError::Error(e) => {
                        self.error_lines = Some(e.error_lines());
                        self.console_buffer
                            .set_text(&format!("{}", e.multiline_display("line ")));
                        self.console.set_text_color(Color::Red);
                    }
                    SfxError::DependencyInstruments | SfxError::DependencySfxSubroutines => {
                        // Error in MML subroutines
                        self.error_lines = None;
                    }
                }
            }
        }

        let e = match compiler_output {
            Some(Err(SfxError::Error(e))) => Some(TextErrorRef::SoundEffect(e)),
            _ => None,
        };
        self.editor.highlight_errors(e);
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
            let s = sender;
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
    let sfx_set: HashSet<&Name> = sfx_data
        .sound_effects
        .item_iter()
        .map(|s| &s.name)
        .collect();

    let to_add: Vec<SoundEffectInput> = data
        .sfx_export_order
        .export_order()
        .iter()
        .filter_map(|sfx_name| {
            if !sfx_set.contains(sfx_name) {
                Some(SoundEffectInput {
                    name: sfx_name.clone(),
                    flags: SfxFlags::default(),
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

struct SfxFlagRadios {
    set: RadioRoundButton,
    clear: RadioRoundButton,
    default: RadioRoundButton,

    default_set: String,
    default_clear: String,
}

impl SfxFlagRadios {
    const WIDTH_CH: i32 = 20;

    fn new(
        parent: &mut Flex,
        set_label: &str,
        set_tooltip: &str,
        clear_label: &str,
        clear_tooltip: &str,
    ) -> Self {
        let w = ch_units_to_width(parent, Self::WIDTH_CH);
        let h = input_height(parent);

        let mut pack = Pack::default().with_size(w * 3, h);
        parent.fixed(&pack, h);

        let mut set = RadioRoundButton::default()
            .with_size(w, h)
            .with_label(set_label);
        set.set_tooltip(set_tooltip);

        let mut clear = RadioRoundButton::default()
            .with_size(w, h)
            .with_label(clear_label);
        clear.set_tooltip(clear_tooltip);

        let default = RadioRoundButton::default()
            .with_size(w, h)
            .with_label("Default");

        pack.end();
        pack.set_type(fltk::group::PackType::Horizontal);

        Self {
            default,
            set,
            clear,
            default_set: format!("Default ({set_label})"),
            default_clear: format!("Default ({clear_label})"),
        }
    }

    fn update_default_label(&mut self, default: bool) {
        self.default.set_label(match default {
            true => &self.default_set,
            false => &self.default_clear,
        });
    }

    fn value(&self) -> Option<bool> {
        if self.set.value() {
            Some(true)
        } else if self.clear.value() {
            Some(false)
        } else {
            None
        }
    }

    fn set_value(&mut self, value: Option<bool>) {
        self.set.set_value(value == Some(true));
        self.clear.set_value(value == Some(false));
        self.default.set_value(value.is_none());
    }

    fn clear_value(&mut self) {
        self.set.clear();
        self.clear.clear();
        self.default.clear();
    }

    fn set_callback(&mut self, cb: impl Fn() + Clone + 'static) {
        let cb = move |_: &mut RadioRoundButton| {
            cb();
        };

        self.set.set_callback(cb.clone());
        self.clear.set_callback(cb.clone());
        self.default.set_callback(cb);
    }
}
