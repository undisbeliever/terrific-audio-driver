//! Audio driver state subwindow

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::{AudioThreadSongInterpreter, SharedSongInterpreter};
use crate::compiler_thread::CursorDriverState;
use crate::helpers::{ch_units_to_width, input_height};
use crate::GuiMessage;

use compiler::bytecode_interpreter::{ChannelState, GlobalState, PanVolEffectDirection};
use compiler::driver_constants::N_MUSIC_CHANNELS;
use compiler::driver_constants::{ECHO_BUFFER_EDL_MS, FIR_FILTER_SIZE};
use compiler::envelope::{self, Envelope};
use compiler::invert_flags::InvertFlags;
use compiler::time::{timer_register_to_bpm, TickCounter};

use fltk::app;
use fltk::button::CheckButton;
use fltk::enums::{Align, FrameType};
use fltk::frame::Frame;
use fltk::prelude::*;
use fltk::window::Window;

#[derive(Clone, Copy, PartialEq)]
struct TimerBpm(u8);

impl std::fmt::Display for TimerBpm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bpm = timer_register_to_bpm(self.0);
        write!(f, "{:0.1}bpm", bpm)
    }
}

#[derive(Clone, Copy, PartialEq)]
struct Edl(u8);

impl std::fmt::Display for Edl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ms", u32::from(self.0) * ECHO_BUFFER_EDL_MS)
    }
}

#[derive(Clone, Copy, PartialEq)]
struct TempGain(u8);

impl std::fmt::Display for TempGain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0 => Ok(()),
            g => {
                let (mode, value) = envelope::Gain::new(g).to_mode_and_value();
                write!(f, "G{}T{}", mode.to_prefix_str(), value)
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
struct EarlyRelease {
    cmp: u8,
    min_ticks: u8,
    gain: u8,
}

impl std::fmt::Display for EarlyRelease {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.cmp, self.min_ticks, self.gain) {
            (0, _, _) => Ok(()),
            (c, 0, 0) => write!(f, "p{}", c - 1),
            (c, 0, g) => {
                let (g_mode, g_value) = envelope::Gain::new(g).to_mode_and_value();
                write!(f, "p{},{}{}", c - 1, g_mode.to_prefix_str(), g_value)
            }
            (c, m, 0) => write!(f, "p{},{}", c - 1, m),
            (c, m, g) => {
                let (g_mode, g_value) = envelope::Gain::new(g).to_mode_and_value();
                write!(f, "p{},{},{}{}", c - 1, m, g_mode.to_prefix_str(), g_value)
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
struct Detune(i16);

impl std::fmt::Display for Detune {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0 => write!(f, "0"),
            d => write!(f, "{d:+}"),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
struct Vibrato {
    offset_per_tick: u8,
    quarter_wavelength: u8,
}

impl std::fmt::Display for Vibrato {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.offset_per_tick > 0 {
            write!(f, "~{},{}", self.offset_per_tick, self.quarter_wavelength)?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq)]
struct PanVol {
    value: u8,
    direction: PanVolEffectDirection,
}

impl std::fmt::Display for PanVol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.direction {
            PanVolEffectDirection::None => write!(f, "{}", self.value),
            PanVolEffectDirection::SlideUp => write!(f, "^ {}", self.value),
            PanVolEffectDirection::SlideDown => write!(f, "v {}", self.value),
            PanVolEffectDirection::TriangleUp | PanVolEffectDirection::TriangleDown => {
                write!(f, "~{}", self.value)
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
struct DriverInvertFlags(u8);

impl std::fmt::Display for DriverInvertFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 & InvertFlags::MASK != 0 {
            let flags = InvertFlags::from_driver_value(self.0);

            if flags.right {
                write!(f, "R")?;
            }
            if flags.left {
                write!(f, "L")?;
            }
            if flags.mono {
                write!(f, "M")?;
            }
        }
        Ok(())
    }
}

struct Value<T>
where
    T: Copy + PartialEq + std::fmt::Display,
{
    frame: Frame,
    value: Option<T>,
}

impl<T> Value<T>
where
    T: Copy + PartialEq + std::fmt::Display,
{
    fn new(x: i32, y: i32, width: i32, height: i32) -> Self {
        let mut frame = Frame::new(x, y, width, height, None);
        frame.set_align(Align::Inside | Align::Right | Align::Clip);
        frame.set_frame(FrameType::ThinDownBox);

        Self { frame, value: None }
    }

    fn new_smaller(x: i32, y: i32, width: i32, height: i32) -> Self {
        let mut out = Self::new(x, y, width, height);
        out.frame.set_label_size(out.frame.label_size() * 8 / 10);
        out
    }

    fn clear(&mut self) {
        self.frame.set_label("");
        self.value = None;
    }

    fn update(&mut self, new_value: T) {
        if self.value != Some(new_value) {
            self.frame.set_label(&new_value.to_string());
            self.value = Some(new_value);
        }
    }

    fn update_option(&mut self, new_value: Option<T>) {
        if self.value != new_value {
            match new_value {
                Some(v) => self.frame.set_label(&v.to_string()),
                None => self.frame.set_label(""),
            }
            self.value = new_value;
        }
    }
}

struct BoolValue {
    frame: Frame,
    value: bool,
    set: &'static str,
}

impl BoolValue {
    fn new(x: i32, y: i32, width: i32, height: i32, set: &'static str) -> Self {
        let mut frame = Frame::new(x, y, width, height, None);
        frame.set_align(Align::Inside | Align::Right);
        frame.set_frame(FrameType::ThinDownBox);

        Self {
            frame,
            value: false,
            set,
        }
    }

    fn clear(&mut self) {
        self.frame.set_label("");
        self.value = false;
    }

    fn update(&mut self, new_value: bool) {
        if self.value != new_value {
            self.value = new_value;
            match new_value {
                true => self.frame.set_label(self.set),
                false => self.frame.set_label(""),
            }
        }
    }
}

struct GlobalValues {
    is_clear: bool,

    ticks: Value<u32>,
    timer: Value<u8>,
    bpm: Value<TimerBpm>,
    echo_delay: Value<Edl>,
    echo_feedback: Value<i8>,

    echo_volume_l: Value<u8>,
    echo_volume_r: Value<u8>,
    echo_invert: Value<DriverInvertFlags>,
    fir_filter: [Value<i8>; FIR_FILTER_SIZE],
}

impl GlobalValues {
    fn new(x_pos: i32, y_pos: i32, width: i32, height: i32, label_padding: i32) -> Self {
        let row_x = |row| x_pos + row * width;
        let col_y = |col| y_pos + col * height;

        let label = |row, col, text: &'static str| {
            Frame::new(row_x(row), col_y(col), width - label_padding, height, text)
                .with_align(Align::Inside | Align::Right);
        };

        label(0, 0, "Tick");
        let ticks = Value::new(row_x(1), col_y(0), width, height);
        label(2, 0, "Timer");
        let timer = Value::new(row_x(3), col_y(0), width, height);
        label(4, 0, "BPM");
        let bpm = Value::new(row_x(5), col_y(0), width, height);

        label(6, 0, "EDL");
        let echo_delay = Value::new(row_x(7), col_y(0), width, height);

        label(8, 0, "EFB");
        let echo_feedback = Value::new(row_x(9), col_y(0), width, height);

        label(2, 1, "EVOL");
        let echo_volume_l = Value::new(row_x(3), col_y(1), width / 2, height);
        let echo_volume_r = Value::new(row_x(3) + width / 2, col_y(1), width / 2, height);
        let echo_invert = Value::new(row_x(4), col_y(1), width / 2, height);

        label(5, 1, "FIR");
        let fir_filter = {
            let x = row_x(6);
            let width = width / 2;
            std::array::from_fn(|i| {
                let i = i as i32;
                Value::new(x + i * width, col_y(1), width, height)
            })
        };

        Self {
            is_clear: false,

            ticks,
            timer,
            bpm,

            echo_delay,
            echo_feedback,

            echo_volume_l,
            echo_volume_r,
            echo_invert,
            fir_filter,
        }
    }

    fn update(&mut self, tc: TickCounter, state: &GlobalState) {
        self.is_clear = false;

        self.ticks.update(tc.value());

        self.timer.update(state.timer_register);
        self.bpm.update(TimerBpm(state.timer_register));

        self.echo_delay.update(Edl(state.echo.edl));
        self.echo_feedback.update(state.echo.feedback);

        self.echo_volume_l.update(state.echo.volume_l);
        self.echo_volume_r.update(state.echo.volume_r);
        self.echo_invert
            .update(DriverInvertFlags(state.echo.invert_flags));
        for i in 0..FIR_FILTER_SIZE {
            self.fir_filter[i].update(state.echo.fir_filter[i]);
        }
    }

    fn clear(&mut self) {
        if !self.is_clear {
            self.is_clear = true;

            self.ticks.clear();
            self.timer.clear();
            self.bpm.clear();
            self.echo_delay.clear();
            self.echo_feedback.clear();

            self.echo_volume_l.clear();
            self.echo_volume_r.clear();
            self.echo_invert.clear();
            for f in &mut self.fir_filter {
                f.clear();
            }
        }
    }
}

struct ChannelValues {
    is_clear: bool,

    instrument: Value<u8>,
    adsr_or_gain: Value<Envelope>,
    temp_gain: Value<TempGain>,
    prev_temp_gain: Value<TempGain>,
    early_release: Value<EarlyRelease>,
    detune: Value<Detune>,
    vibrato: Value<Vibrato>,

    volume: Value<PanVol>,
    pan: Value<PanVol>,
    invert: Value<DriverInvertFlags>,

    echo: BoolValue,
    pitch_mod: BoolValue,

    stack_pointer: Value<usize>,
}

impl ChannelValues {
    // Channel names must be `'static` strings
    const CHANNEL_NAMES: [&str; N_MUSIC_CHANNELS] = ["A", "B", "C", "D", "E", "F", "G", "H"];

    const LABELS: [&str; 13] = [
        "Instrument",
        "Envelope",
        "Temp-GAIN",
        "Prev temp-GAIN",
        "Early release",
        "Detune",
        "Vibrato",
        "Volume",
        "Pan",
        "Invert",
        "Echo",
        "Pitch mod",
        "Stack pointer",
    ];

    fn new(index: usize, x_pos: i32, y_pos: i32, width: i32, height: i32) -> Self {
        let mut y_pos = y_pos;
        let mut next_y_pos = || {
            let y = y_pos;
            y_pos += height;
            y
        };

        Frame::new(
            x_pos,
            next_y_pos(),
            width,
            height,
            Self::CHANNEL_NAMES[index],
        )
        .with_align(Align::Inside | Align::Center);

        Self {
            is_clear: true,

            instrument: Value::new(x_pos, next_y_pos(), width, height),
            adsr_or_gain: Value::new(x_pos, next_y_pos(), width, height),
            temp_gain: Value::new(x_pos, next_y_pos(), width, height),
            prev_temp_gain: Value::new(x_pos, next_y_pos(), width, height),
            early_release: Value::new_smaller(x_pos, next_y_pos(), width, height),
            detune: Value::new(x_pos, next_y_pos(), width, height),
            vibrato: Value::new(x_pos, next_y_pos(), width, height),

            volume: Value::new(x_pos, next_y_pos(), width, height),
            pan: Value::new(x_pos, next_y_pos(), width, height),
            invert: Value::new(x_pos, next_y_pos(), width, height),

            echo: BoolValue::new(x_pos, next_y_pos(), width, height, "E"),
            pitch_mod: BoolValue::new(x_pos, next_y_pos(), width, height, "PM"),

            stack_pointer: Value::new(x_pos, next_y_pos(), width, height),
        }
    }

    fn add_labels(x: i32, y: i32, width: i32, height: i32) {
        for (i, label) in Self::LABELS.iter().enumerate() {
            let column = i32::try_from(i).unwrap() + 1;

            Frame::new(x, y + column * height, width, height, *label)
                .with_align(Align::Inside | Align::Left);
        }
    }

    fn clear(&mut self) {
        if !self.is_clear {
            self.is_clear = true;

            self.instrument.clear();
            self.adsr_or_gain.clear();
            self.temp_gain.clear();
            self.prev_temp_gain.clear();
            self.early_release.clear();
            self.detune.clear();
            self.vibrato.clear();

            self.volume.clear();
            self.pan.clear();
            self.invert.clear();

            self.echo.clear();
            self.pitch_mod.clear();

            self.stack_pointer.clear();
        }
    }

    fn update(&mut self, c: &ChannelState) {
        self.is_clear = false;

        // ::TODO somehow show instrument name::
        self.instrument.update_option(c.instrument);

        // ::TODO somehow show default envelope::
        self.adsr_or_gain.update_option(
            c.adsr_or_gain_override
                .map(|(a1, a2)| Envelope::from_engine_value(a1, a2)),
        );

        self.temp_gain.update(TempGain(c.temp_gain));
        self.prev_temp_gain.update(TempGain(c.prev_temp_gain));
        self.early_release.update(EarlyRelease {
            cmp: c.early_release_cmp,
            min_ticks: c.early_release_min_ticks,
            gain: c.early_release_gain,
        });
        self.detune.update(Detune(c.detune));
        self.vibrato
            .update_option(c.vibrato_quarter_wavelength_in_ticks.map(|q| Vibrato {
                offset_per_tick: c.vibrato_pitch_offset_per_tick,
                quarter_wavelength: q,
            }));
        self.volume.update(PanVol {
            value: c.volume.value,
            direction: c.volume.direction,
        });
        self.pan.update(PanVol {
            value: c.pan.value,
            direction: c.pan.direction,
        });
        self.invert.update(DriverInvertFlags(c.invert_flags));

        self.echo.update(c.echo);
        self.pitch_mod.update(c.pitch_mod);

        self.stack_pointer.update(c.stack_pointer);
    }
}

struct DriverWidgets {
    status: Status,
    status_label: Frame,

    globals: GlobalValues,
    channels: [ChannelValues; N_MUSIC_CHANNELS],
}

impl DriverWidgets {
    fn set_status(&mut self, status: Status) {
        if self.status != status {
            self.status = status;

            self.status_label.set_label(match self.status {
                Status::None => "",
                Status::Audio => "AUDIO",
                Status::SongCursor => "CURSOR",
                Status::SubroutineCursor => "SUBROUTINE",
                Status::SongError => "ERROR",
            });
        }
    }

    fn update(&mut self, si: &AudioThreadSongInterpreter) {
        self.globals.update(si.tick_counter(), si.global_state());

        for (wc, sc) in self.channels.iter_mut().zip(si.channels().iter()) {
            match sc {
                Some(sc) => wc.update(sc),
                None => wc.clear(),
            }
        }
    }

    fn clear(&mut self) {
        self.globals.clear();
        for c in &mut self.channels {
            c.clear();
        }
    }
}

#[derive(PartialEq)]
enum Status {
    None,
    Audio,
    SongCursor,
    SubroutineCursor,
    SongError,
}

pub struct DriverStateWindow {
    window: Window,

    follow_cursor: bool,
    track_audio_cb: CheckButton,

    cursor_state: CursorDriverState,
    audio_thread_song_interpreter: Option<SharedSongInterpreter>,

    widgets: DriverWidgets,
}

impl DriverStateWindow {
    pub fn new(sender: app::Sender<GuiMessage>) -> Self {
        let mut window = fltk::window::Window::default().with_label("Audio Driver State");
        window.make_modal(false);
        window.make_resizable(false);

        let width = ch_units_to_width(&window, 10);
        let height = input_height(&window);
        let padding = std::cmp::max(ch_units_to_width(&window, 1), 2);

        window.set_size(width * 10 + padding * 2, height * 17 + padding * 3);

        let mut track_audio_cb =
            CheckButton::new(padding, padding, width * 2, height, "Track audio");
        track_audio_cb.set_value(true);
        track_audio_cb.set_callback(move |_| {
            sender.send(GuiMessage::DriverStateTrackingCheckboxChanged);
        });

        let status_label = Frame::new(padding + width * 2, padding, width * 2, height, None)
            .with_align(Align::Inside | Align::Left);

        // ::TODO add song name::

        let globals = GlobalValues::new(
            padding,
            padding + height,
            width,
            height,
            ch_units_to_width(&window, 1),
        );

        let channels_y_pos = padding * 2 + height * 3;
        ChannelValues::add_labels(padding, channels_y_pos, width * 2 - 5, height);
        let channels = std::array::from_fn(|i| {
            let column = i32::try_from(i).unwrap() + 2;
            ChannelValues::new(i, padding + column * width, channels_y_pos, width, height)
        });

        window.end();

        Self {
            window,
            follow_cursor: true,
            track_audio_cb,
            cursor_state: CursorDriverState::None,
            audio_thread_song_interpreter: None,
            widgets: DriverWidgets {
                status: Status::None,
                status_label,
                globals,
                channels,
            },
        }
    }

    fn update_follow_cursor(&mut self) {
        self.follow_cursor =
            !self.track_audio_cb.value() || self.audio_thread_song_interpreter.is_none();
    }

    pub fn on_tracking_checkbox_changed(&mut self) {
        self.update_follow_cursor();
        self.update_all();
    }

    pub fn song_started(&mut self, si: Option<SharedSongInterpreter>) {
        self.audio_thread_song_interpreter = si;

        self.update_follow_cursor();
        self.monitor_timer_elapsed();
    }

    pub fn song_stopped(&mut self) {
        self.follow_cursor = true;
    }

    pub fn show_or_hide(&mut self) {
        if self.window.shown() {
            self.window.hide();
        } else {
            self.update_all();
            self.window.show();
        }
    }

    pub fn non_song_tab_selected(&mut self) {
        self.cursor_state = CursorDriverState::None;

        if self.follow_cursor {
            self.widgets.set_status(Status::None);
        }

        if self.follow_cursor && self.window.visible() {
            self.widgets.clear();
        }
    }

    pub fn song_cursor_state_changed(&mut self, state: CursorDriverState) {
        self.cursor_state = state;

        if self.follow_cursor && self.window.shown() {
            self.update_song_cursor_state();
        }
    }

    pub fn monitor_timer_elapsed(&mut self) {
        if !self.follow_cursor && self.window.shown() {
            self.update_monitor();
        }
    }

    fn update_all(&mut self) {
        if self.follow_cursor {
            self.update_song_cursor_state();
        } else {
            self.update_monitor();
        }
    }

    fn update_song_cursor_state(&mut self) {
        match &self.cursor_state {
            CursorDriverState::Song(si) => {
                self.widgets.set_status(Status::SongCursor);
                self.widgets.update(si);
            }
            CursorDriverState::Subroutine(si) => {
                self.widgets.set_status(Status::SubroutineCursor);
                self.widgets.update(si);
            }
            CursorDriverState::None | CursorDriverState::NoSong => {
                self.widgets.set_status(Status::None);
                self.widgets.clear();
            }
            CursorDriverState::NoCursor => {
                self.widgets.set_status(Status::None);
                self.widgets.clear();
            }
            CursorDriverState::BcError => {
                self.widgets.set_status(Status::SongError);
                self.widgets.clear();
            }
            CursorDriverState::BcTimeout => {
                self.widgets.set_status(Status::SongError);
                self.widgets.clear();
            }
        }
    }

    fn update_monitor(&mut self) {
        self.widgets.set_status(Status::Audio);

        match &self.audio_thread_song_interpreter {
            Some(si) => {
                if let Ok(si) = si.try_borrow() {
                    self.widgets.update(&si);
                }
            }
            None => self.widgets.clear(),
        }
    }
}
