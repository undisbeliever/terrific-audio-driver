//! Sample Analyser widgets

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, ToCompiler};
use crate::helpers::{ch_units_to_width, input_height};
use crate::GuiMessage;

use brr::{BrrSample, MonoPcm16WaveFile};
use compiler::project::{self, BrrSamplePitches};
use fltk::valuator::HorSlider;

use std::cell::RefCell;
use std::cmp::{max, min};
use std::ops::Range;
use std::rc::Rc;
use std::sync::mpsc;

extern crate fltk;
use fltk::app;
use fltk::button::Button;
use fltk::draw;
use fltk::enums::{Align, Color, Event, Font, FrameType};
use fltk::group::{Flex, Group};
use fltk::menu::Choice;
use fltk::output::Output;
use fltk::prelude::{GroupExt, InputExt, MenuExt, ValuatorExt, WidgetBase, WidgetExt};
use fltk::widget::Widget;

extern crate spectrum_analyzer;
use spectrum_analyzer::windows::hann_window;
use spectrum_analyzer::{samples_fft_to_spectrum, scaling, FrequencyLimit, FrequencySpectrum};

const WAVEFORM_ZERO_COLOR: Color = Color::from_rgb(0, 0, 0);
const WAVEFORM_LOOP_POINT_COLOR: Color = Color::from_rgb(128, 128, 128);
const WAVEFORM_COLOR: Color = Color::from_rgb(0, 0, 255);

const SPECTRUM_COLOR: Color = Color::from_rgb(128, 0, 128);
const SPECTRUM_INST_FREQ_COLOR: Color = Color::from_rgb(128, 128, 128);

/// Sample rate to decode BRR samples at (S-DSP sample rate)
const BRR_SAMPLE_RATE: u32 = 32000;
/// Minimum number of samples to render when decoding a looping BRR sample
const MIN_LOOPING_SAMPLES: usize = 8192;

/// spectrum-analyzer crate panics if samples.len() > 32768 samples
const MIN_SPECTRUM_SAMPLES: usize = 256;
const MAX_SPECTRUM_SAMPLES: usize = 32768;

/// Minimum number of waveform samples to show in the waveform widget
const MIN_WAVEFORM_WIDTH: usize = 192;
/// Amount to scroll (in fractions of a screen) when scrolling the scroll wheel in the waveform widget
const WAVEFORM_SCROLL: f64 = 1.0 / 32.0;

/// Number of spectrum frequencies to test to the left and right of the cursor in the spectrum widget
const SPECTRUM_CURSOR_PEAK_LEFT_RIGHT: usize = 12;
/// Maximum frequency to show in the spectrum widget
const MAX_SPECTRUM_FREQ: f32 = 8000.0;

const _: () = assert!((BRR_SAMPLE_RATE / MAX_SPECTRUM_FREQ as u32).is_power_of_two());

const SPECTRUM_RANGE_CHOICES: &str = concat![
    "0kHz - 1kHz",
    "|0kHz - 2kHz",
    "|0kHz - 4kHz",
    "|0kHz - 8kHz",
];

fn read_spectrum_range_choice(c: &Choice) -> f64 {
    match c.value() {
        0 => 1000.0,
        1 => 2000.0,
        2 => 4000.0,
        _ => MAX_SPECTRUM_FREQ as f64,
    }
}

const FFT_SIZE_CHOICES: &str = "256|512|1024|2048|4096|8192|16384|32768";
const _: () = assert!(MIN_SPECTRUM_SAMPLES == 256);
const _: () = assert!(MAX_SPECTRUM_SAMPLES == 32768);

const DEFAULT_FFT_SIZE_VALUE: i32 = 4;

#[derive(Debug)]
struct FftSize {
    choice: u8,
}

fn read_fft_choice(c: &Choice) -> FftSize {
    const MAX_VALUE: i32 =
        (MAX_SPECTRUM_SAMPLES as i32 / MIN_SPECTRUM_SAMPLES as i32).ilog2() as i32;

    FftSize {
        choice: c.value().clamp(0, MAX_VALUE).try_into().unwrap(),
    }
}

impl FftSize {
    const DEFAULT: Self = Self {
        choice: DEFAULT_FFT_SIZE_VALUE as u8,
    };

    fn value(&self) -> usize {
        MIN_SPECTRUM_SAMPLES << self.choice
    }
}

enum WaveformMoveEvent {
    ZoomIn,
    ZoomOut,
    ScrollLeft,
    ScrollRight,
}

pub struct SampleAnalyserWidget {
    state: Rc<RefCell<State>>,
}

struct State {
    sender: app::Sender<GuiMessage>,
    compiler_sender: mpsc::Sender<ToCompiler>,

    item_id: Option<ItemId>,

    freq: f64,

    analysis: Option<SampleAnalysis>,

    spectrum_max_freq: f64,
    spectrum_x_scale: f64,
    spectrum_cursor_clicked: Option<(f64, f64)>,

    waveform_x_offset: usize,
    waveform_x_scale: f64,

    spectrum: Widget,
    waveform: Widget,

    first_group: Group,
    play_button: Button,
    fft_offset: HorSlider,
    fft_size_choice: Choice,

    spectrum_stats_group: Group,
    spectrum_range_choice: Choice,
    peak_freq: Output,
    cursor_freq: Output,
    cursor_peak_freq: Output,

    use_peak: Button,
    use_cursor: Button,
    use_cursor_peak: Button,
}

impl SampleAnalyserWidget {
    pub fn new(
        parent: &mut Flex,
        width: i32,
        sender: app::Sender<GuiMessage>,
        compiler_sender: mpsc::Sender<ToCompiler>,
    ) -> Self {
        let line_height = input_height(parent);

        let output_w = ch_units_to_width(parent, 10);
        let use_w = ch_units_to_width(parent, 4);

        let play_w = use_w;
        let fftsize_w = output_w;
        let srange_w = output_w + use_w;

        let pad = parent.pad();

        let col_width = width / 3;
        let cx = |c| pad + c * col_width;

        let waveform = Widget::default();

        let first_group = Group::new(pad, 0, width, line_height, None);

        let mut play_button = Button::new(pad, 0, play_w, line_height, "@>");
        play_button.set_tooltip("Play BRR sample at 32000Hz");

        let offset_x = pad + play_w + pad;
        let mut fft_offset =
            HorSlider::new(offset_x, 0, col_width - play_w - pad, line_height, None);
        fft_offset.set_tooltip("FFT range");

        let mut fft_size_choice =
            Choice::new(cx(2) - fftsize_w, 0, fftsize_w, line_height, "FFT size: ");
        fft_size_choice.add_choice(FFT_SIZE_CHOICES);
        fft_size_choice.set_value(DEFAULT_FFT_SIZE_VALUE);
        fft_size_choice.set_tooltip("Maximum FFT size");

        let spectrum_range_choice =
            Choice::new(cx(3) - srange_w, 0, srange_w, line_height, "Range: ");

        first_group.end();
        parent.fixed(&first_group, line_height);

        let spectrum = Widget::default();

        let spectrum_stats_group = Group::new(pad, 0, width, line_height, None);

        let freq_stats = |c, label| {
            let bx = cx(c + 1) - use_w;
            let ox = bx - output_w;
            (
                Output::new(ox, 0, output_w, line_height, label),
                Button::new(bx, 0, use_w, line_height, "Use"),
            )
        };
        let (peak_freq, use_peak) = freq_stats(0, "Peak:");
        let (cursor_freq, use_cursor) = freq_stats(1, "Cursor:");
        let (cursor_peak_freq, use_cursor_peak) = freq_stats(2, "C Peak:");

        spectrum_stats_group.end();
        parent.fixed(&spectrum_stats_group, line_height);

        let state = Rc::new(RefCell::from(State {
            sender,
            compiler_sender,

            item_id: None,
            freq: 0.0,
            analysis: None,

            spectrum_max_freq: MAX_SPECTRUM_FREQ.into(),
            spectrum_x_scale: 0.0,
            spectrum_cursor_clicked: None,
            waveform_x_offset: 0,
            waveform_x_scale: 1.0,

            spectrum,
            waveform,

            first_group,
            play_button,
            fft_offset,
            fft_size_choice,

            spectrum_stats_group,
            spectrum_range_choice,
            peak_freq,
            cursor_freq,
            cursor_peak_freq,

            use_peak,
            use_cursor,
            use_cursor_peak,
        }));

        {
            let mut s = state.borrow_mut();

            s.spectrum.draw({
                let state = state.clone();
                move |_| {
                    state.borrow_mut().draw_spectrum();
                }
            });
            s.spectrum.handle({
                let state = state.clone();
                move |_, ev| Self::handle_spectrum_event(ev, &state)
            });

            s.waveform.draw({
                let state = state.clone();
                move |_| {
                    state.borrow().draw_waveform();
                }
            });
            s.waveform.handle({
                let state = state.clone();
                move |_, ev| Self::handle_waveform_event(ev, &state)
            });

            s.play_button.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow().play_button_clicked();
                }
            });

            s.fft_offset.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow().send_analyse_sample_message();
                }
            });
            s.fft_size_choice.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow().send_analyse_sample_message();
                }
            });

            s.spectrum_range_choice.add_choice(SPECTRUM_RANGE_CHOICES);
            s.spectrum_range_choice.set_value(2);
            s.spectrum_range_choice.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow_mut().spectrum_range_changed();
                }
            });

            s.use_peak.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow_mut().use_peak_clicked();
                }
            });
            s.use_cursor.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow_mut().use_cursor_clicked();
                }
            });
            s.use_cursor_peak.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow_mut().use_cursor_peak_clicked();
                }
            });

            s.spectrum_range_changed();
        }

        Self { state }
    }

    pub fn clear_and_deactivate(&mut self) {
        self.state.borrow_mut().clear();
    }

    pub fn selected_sample_edited(&mut self, id: ItemId, data: &project::BrrSample) {
        self.state.borrow_mut().selected_sample_edited(id, data);
    }

    pub fn analysis_from_compiler_thread(
        &mut self,
        sample_id: ItemId,
        analysis: Option<SampleAnalysis>,
    ) {
        self.state
            .borrow_mut()
            .analysis_from_compiler_thread(sample_id, analysis)
    }

    fn handle_spectrum_event(ev: Event, state: &Rc<RefCell<State>>) -> bool {
        match ev {
            Event::Enter => true,
            Event::Leave => {
                state.borrow_mut().spectrum_leave_event();
                true
            }
            Event::Move => {
                state.borrow_mut().spectrum_move_event(app::event_x());
                true
            }
            Event::Push => true,
            Event::Released => {
                state.borrow_mut().spectrum_release_event(app::event_x());
                true
            }
            _ => false,
        }
    }

    fn handle_waveform_event(ev: Event, state: &Rc<RefCell<State>>) -> bool {
        match ev {
            Event::MouseWheel => {
                let m = if app::event_dy() == app::MouseWheel::Up {
                    if app::is_event_ctrl() {
                        WaveformMoveEvent::ZoomOut
                    } else {
                        WaveformMoveEvent::ScrollRight
                    }
                } else if app::event_dy() == app::MouseWheel::Down {
                    if app::is_event_ctrl() {
                        WaveformMoveEvent::ZoomIn
                    } else {
                        WaveformMoveEvent::ScrollLeft
                    }
                } else {
                    // Unknown move wheel movement
                    return true;
                };
                state.borrow_mut().waveform_move(m);
                true
            }
            _ => false,
        }
    }
}

impl State {
    fn clear(&mut self) {
        if self.item_id.is_some() {
            self.item_id = None;
            // Stop analysing samples in the compiler thread
            self.send_analyse_sample_message();
        }

        self.analysis = None;

        self.clear_spectrum_values();
    }

    fn selected_sample_edited(&mut self, id: ItemId, data: &project::BrrSample) {
        if self.item_id != Some(id) {
            self.item_id = Some(id);

            self.analysis = None;

            self.clear_spectrum_values();

            self.send_analyse_sample_message();
        }

        self.freq = match &data.pitches {
            Some(BrrSamplePitches::Notes { tuning, .. })
            | Some(BrrSamplePitches::Octaves { tuning, .. }) => tuning.frequency(),
            Some(BrrSamplePitches::SampleRates { .. }) | None => -1.0,
        };
    }

    fn send_analyse_sample_message(&self) {
        let _ = self
            .compiler_sender
            .send(ToCompiler::SampleAnalyserSettingsChanged(FftSettings {
                sample_id: self.item_id,
                offset: self.fft_offset.value() as u32,
                size: read_fft_choice(&self.fft_size_choice),
            }));
    }

    fn analysis_from_compiler_thread(
        &mut self,
        sample_id: ItemId,
        analysis: Option<SampleAnalysis>,
    ) {
        let analysis = if self.item_id == Some(sample_id) {
            analysis
        } else {
            None
        };

        match &analysis {
            Some(a) => {
                match &a.spectrum {
                    Ok(s) => self.set_spectrum_values(s),
                    Err(_) => self.clear_spectrum_values(),
                }

                {
                    let decoded_size = a.decoded_samples_f32.len();
                    let window_size = a.fft_range.len();
                    let pos = a.fft_range.start;
                    let max = decoded_size - window_size;

                    self.fft_offset.set_range(0.0, max as f64);
                    self.fft_offset
                        .set_slider_size(window_size as f32 / decoded_size as f32);

                    self.fft_offset.set_value(pos as f64);
                }

                self.first_group.activate();
                self.waveform.activate();
                self.spectrum.activate();
            }
            None => {
                self.clear_spectrum_values();

                self.analysis = None;

                self.fft_offset.set_range(0.0, 0.0);

                self.first_group.deactivate();
                self.waveform.deactivate();
                self.spectrum.deactivate();
            }
        }
        self.analysis = analysis;

        self.waveform_x_offset = 0;
        self.waveform_x_scale = 1.0;

        self.spectrum.redraw();
        self.waveform.redraw();
    }

    fn set_spectrum_values(&mut self, s: &FrequencySpectrum) {
        self.peak_freq
            .set_value(&format!("{} Hz", s.max().0.val().round()));
        self.cursor_freq.set_value("");
        self.cursor_peak_freq.set_value("");

        self.spectrum_x_scale = 0.0;
        self.spectrum_cursor_clicked = None;

        self.spectrum_stats_group.activate();
    }

    fn clear_spectrum_values(&mut self) {
        self.peak_freq.set_value("");
        self.cursor_freq.set_value("");
        self.cursor_peak_freq.set_value("");

        self.spectrum_x_scale = 0.0;
        self.spectrum_cursor_clicked = None;

        self.spectrum_stats_group.deactivate();
    }

    fn update_cursor_outputs(&mut self, o: Option<(f64, f64)>) {
        match o {
            Some((cursor, peak)) => {
                self.cursor_freq.set_value(&format!("{} Hz", cursor));
                self.cursor_peak_freq.set_value(&format!("{} Hz", peak));
            }
            None => {
                self.cursor_freq.set_value("");
                self.cursor_peak_freq.set_value("");
            }
        }
    }

    fn use_peak_clicked(&mut self) {
        if let Some(a) = &self.analysis {
            if let Ok(s) = &a.spectrum {
                if let Some(id) = self.item_id {
                    let peak = s.max().0.val().into();
                    self.sender
                        .send(GuiMessage::SetSampleTuningFrequency(id, peak));
                }
            }
        }
    }

    fn use_cursor_clicked(&mut self) {
        if let (Some(id), Some(c)) = (self.item_id, self.spectrum_cursor_clicked) {
            self.sender
                .send(GuiMessage::SetSampleTuningFrequency(id, c.0));
        }
    }

    fn use_cursor_peak_clicked(&mut self) {
        if let (Some(id), Some(c)) = (self.item_id, self.spectrum_cursor_clicked) {
            self.sender
                .send(GuiMessage::SetSampleTuningFrequency(id, c.1));
        }
    }

    fn spectrum_range_changed(&mut self) {
        self.spectrum_max_freq = read_spectrum_range_choice(&self.spectrum_range_choice);

        self.spectrum.redraw();
    }

    fn get_freq_for_curosr_x(&self, event_x: i32) -> Option<(f64, f64)> {
        let spectrum = match &self.analysis {
            Some(a) => match &a.spectrum {
                Ok(s) => s,
                Err(_) => return None,
            },
            None => return None,
        };

        let x_scale = self.spectrum_x_scale;
        let x = event_x - self.spectrum.x() - Self::FRAME_MARGIN;

        if x_scale > 0.0 && x > 0 {
            let cursor_freq = f64::round(x as f64 / x_scale);

            // Find closest peak near the mouse cursor.
            // Assumes `spectrum.data()` is sorted and the frequency is evenly spaced.
            let index =
                cursor_freq / f64::from(spectrum.max_fr().val()) * (spectrum.data().len() as f64);
            let index = (index as usize).clamp(
                SPECTRUM_CURSOR_PEAK_LEFT_RIGHT,
                spectrum.data().len() - SPECTRUM_CURSOR_PEAK_LEFT_RIGHT - 1,
            );
            let range = (index - SPECTRUM_CURSOR_PEAK_LEFT_RIGHT)
                ..=(index + SPECTRUM_CURSOR_PEAK_LEFT_RIGHT);
            let peak = spectrum.data()[range].iter().max_by_key(|s| s.1)?;
            let peak_freq = f64::round(peak.0.val().into());

            Some((cursor_freq, peak_freq))
        } else {
            None
        }
    }

    fn spectrum_move_event(&mut self, event_x: i32) {
        self.update_cursor_outputs(self.get_freq_for_curosr_x(event_x));
    }

    fn spectrum_release_event(&mut self, event_x: i32) {
        self.spectrum_cursor_clicked = self.get_freq_for_curosr_x(event_x);
    }

    fn spectrum_leave_event(&mut self) {
        self.update_cursor_outputs(self.spectrum_cursor_clicked);
    }

    fn draw_frame(w: &Widget) -> (i32, i32, i32, i32) {
        const FM: i32 = State::FRAME_MARGIN;

        let (x, y, w, h) = (w.x(), w.y(), w.w(), w.h());

        draw::draw_box(FrameType::DownBox, x, y, w, h, Color::White);

        (x + FM, y + FM, w - FM * 2, h - FM * 2)
    }
    const FRAME_MARGIN: i32 = 2;

    fn draw_spectrum(&mut self) {
        let w = &self.spectrum;

        let (x, y, w, h) = Self::draw_frame(w);

        draw::push_clip(x, y, w, h);

        draw::set_line_style(draw::LineStyle::Solid, 0);

        if let Some(analysis) = &self.analysis {
            match &analysis.spectrum {
                Ok(s) => {
                    self.spectrum_x_scale = f64::from(w) / self.spectrum_max_freq;

                    self.draw_spectrum_spectrum(s, x, y, w, h)
                }
                Err(e) => {
                    self.spectrum_x_scale = 0.0;

                    draw::set_draw_color(Color::Red);
                    draw::set_font(Font::Helvetica, app::font_size());
                    draw::draw_text2(e, x + 4, y + 4, w - 4, h - 4, Align::TopLeft);
                }
            }
        }

        draw::pop_clip();
    }

    fn draw_spectrum_spectrum(&self, s: &FrequencySpectrum, x: i32, y: i32, w: i32, h: i32) {
        draw::set_draw_color(Color::Black);
        draw::set_font(Font::Helvetica, app::font_size());
        draw::draw_text2("Spectrum", x + 4, y + 4, w - 4, h - 4, Align::TopLeft);

        let x_scale = self.spectrum_x_scale;
        let y_scale = f64::from(-h) / f64::from(s.max().1.val());

        if self.item_id.is_some() {
            let freq_x = (self.freq * x_scale) as i32;

            draw::set_draw_color(SPECTRUM_INST_FREQ_COLOR);
            draw::draw_yxline(x + freq_x, y, y + h);
        }

        draw::push_matrix();
        draw::translate(x.into(), (y + h).into());
        draw::scale_xy(x_scale, y_scale);

        draw::set_draw_color(SPECTRUM_COLOR);
        draw::begin_line();

        for (f, v) in s.data() {
            draw::vertex(f.val().into(), v.val().into());
        }

        draw::end_line();

        draw::pop_matrix();
    }

    fn draw_waveform(&self) {
        let w = &self.waveform;

        let (x, y, w, h) = Self::draw_frame(w);

        draw::push_clip(x, y, w, h);

        draw::set_line_style(draw::LineStyle::Solid, 0);

        if let Some(analysis) = &self.analysis {
            self.draw_waveform_waveform(analysis, x, y, w, h);
        }

        draw::pop_clip();
    }

    fn draw_waveform_waveform(&self, analysis: &SampleAnalysis, x: i32, y: i32, w: i32, h: i32) {
        let samples = &analysis.decoded_samples_f32;
        let fft_range = &analysis.fft_range;

        // Do not show the padding if the sample does not loop
        let n_samples = match analysis.loop_point_samples {
            Some(_) => samples.len(),
            None => analysis.n_input_samples,
        };

        let samples_default_zoom = Self::waveform_samples_default_zoom(analysis);
        let samples_range = Range {
            start: self.waveform_x_offset,
            end: min(
                self.waveform_x_offset
                    + ((samples_default_zoom as f64) / self.waveform_x_scale) as usize,
                n_samples,
            ),
        };

        let center_y = y + h / 2;

        let x_scale = f64::from(w) / (samples_default_zoom as f64) * self.waveform_x_scale;
        let y_scale = f64::from(-h / 2);

        // Darken unanalysed section
        if fft_range.start > samples_range.start {
            let w = (((fft_range.start - samples_range.start) as f64) * x_scale) as i32;

            if w > 0 {
                draw::draw_rect_fill(x, y, w, h, Color::Light1);
            }
        }
        if fft_range.end < samples_range.end {
            let lp_x = (((fft_range.end - samples_range.start) as f64) * x_scale) as i32;
            let w = (((samples_range.end - fft_range.end) as f64) * x_scale) as i32;

            if w > 0 {
                draw::draw_rect_fill(x + lp_x, y, w, h, Color::Light1);
            }
        }

        // Draw loop points
        if let Some(lp) = analysis.loop_point_samples {
            if lp < n_samples {
                let loop_size = analysis.n_brr_samples - lp;

                draw::set_draw_color(WAVEFORM_LOOP_POINT_COLOR);
                for i in (lp..samples.len()).step_by(loop_size) {
                    if samples_range.contains(&i) {
                        let lp_x = (((i - samples_range.start) as f64) * x_scale) as i32;
                        draw::draw_yxline(x + lp_x, y, y + h);
                    }
                }
            }
        }

        draw::set_draw_color(WAVEFORM_ZERO_COLOR);
        draw::draw_xyline(x, center_y, x + w);

        draw::push_matrix();
        draw::translate(x.into(), center_y.into());
        draw::scale_xy(x_scale, y_scale);

        // Draw waveform
        draw::set_draw_color(WAVEFORM_COLOR);
        draw::begin_line();

        for (i, &y) in samples[samples_range].iter().enumerate() {
            let x = i as f64;
            draw::vertex(x, y as f64);
        }

        draw::end_line();
        draw::pop_matrix();
    }

    fn waveform_samples_default_zoom(analysis: &SampleAnalysis) -> usize {
        match analysis.loop_point_samples {
            Some(_) => min(
                analysis.decoded_samples_f32.len(),
                max(analysis.n_input_samples * 2, MIN_WAVEFORM_WIDTH),
            ),
            None => analysis.n_input_samples,
        }
    }

    fn waveform_move(&mut self, m: WaveformMoveEvent) {
        if let Some(a) = &self.analysis {
            match &m {
                WaveformMoveEvent::ZoomIn => {
                    self.waveform_x_scale = f64::min(self.waveform_x_scale * 1.25, 50.0);
                }
                WaveformMoveEvent::ZoomOut => {
                    self.waveform_x_scale = f64::max(self.waveform_x_scale / 1.25, 0.125);
                }
                WaveformMoveEvent::ScrollLeft | WaveformMoveEvent::ScrollRight => (),
            }

            let samples_default_zoom = Self::waveform_samples_default_zoom(a);
            let samples_to_display =
                f64::max(1.0, (samples_default_zoom as f64) / self.waveform_x_scale);
            let move_amount = ((samples_to_display * WAVEFORM_SCROLL) as usize).clamp(1, 64);

            let x_offset = match m {
                WaveformMoveEvent::ScrollLeft => self.waveform_x_offset.saturating_sub(move_amount),
                WaveformMoveEvent::ScrollRight => {
                    self.waveform_x_offset.saturating_add(move_amount)
                }
                WaveformMoveEvent::ZoomIn | WaveformMoveEvent::ZoomOut => self.waveform_x_offset,
            };

            self.waveform_x_offset = min(
                x_offset,
                a.decoded_samples_f32
                    .len()
                    .saturating_sub(samples_to_display as usize),
            );

            self.waveform.redraw();
        }
    }

    fn play_button_clicked(&self) {
        if let Some(id) = self.item_id {
            let _ = self.compiler_sender.send(ToCompiler::PlaySampleAt32Khz(id));
        }
    }
}

#[derive(Debug)]
pub struct FftSettings {
    sample_id: Option<ItemId>,
    size: FftSize,
    offset: u32,
}

impl FftSettings {
    pub fn selected_sample_id(&self) -> Option<ItemId> {
        self.sample_id
    }
}

impl Default for FftSettings {
    fn default() -> Self {
        Self {
            sample_id: None,
            size: FftSize::DEFAULT,
            offset: 0,
        }
    }
}

pub struct SampleAnalysis {
    n_input_samples: usize,
    n_brr_samples: usize,
    loop_point_samples: Option<usize>,

    decoded_samples_f32: Vec<f32>,

    fft_range: Range<usize>,
    spectrum: Result<FrequencySpectrum, String>,
}

impl std::fmt::Debug for SampleAnalysis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt("SampleAnalysis{..}", f)
    }
}

// Called by the compiler thread
pub fn analyse_sample(
    brr_sample: &BrrSample,
    wav_sample: Option<&MonoPcm16WaveFile>,
    fft: &FftSettings,
) -> SampleAnalysis {
    const FLOAT_SCALE: f32 = -(i16::MIN as f32);

    let fft_size = fft.size.value();

    let n_input_samples = match wav_sample {
        Some(w) => w.samples.len(),
        None => brr_sample.n_samples(),
    };

    let samples_to_decode = if brr_sample.is_looping() {
        usize::max(brr_sample.n_samples(), MIN_LOOPING_SAMPLES)
    } else {
        usize::max(brr_sample.n_samples(), MIN_SPECTRUM_SAMPLES)
    };
    let samples_to_decode = if samples_to_decode < fft_size {
        fft_size
    } else {
        samples_to_decode
    };

    let mut decoded_samples = vec![0_i16; samples_to_decode];
    brr_sample.decode_into_buffer(&mut decoded_samples, 0, 0, 0);

    let decoded_samples_f32: Vec<f32> = decoded_samples
        .iter()
        .map(|&s| f32::from(s) / FLOAT_SCALE)
        .collect();

    let window_len = usize::min(decoded_samples_f32.len(), fft_size);
    let window_offset = usize::min(fft.offset as usize, decoded_samples_f32.len() - window_len);
    let fft_range = window_offset..(window_offset + window_len);

    let windowed_samples = hann_window(&decoded_samples_f32[fft_range.clone()]);

    // Analyse spectrum
    let spectrum = samples_fft_to_spectrum(
        &windowed_samples,
        BRR_SAMPLE_RATE,
        FrequencyLimit::Max(MAX_SPECTRUM_FREQ),
        Some(&scaling::divide_by_N_sqrt),
    )
    .map_err(|e| format!("ERROR: {:?}", e));

    SampleAnalysis {
        n_input_samples,
        n_brr_samples: brr_sample.n_samples(),
        loop_point_samples: brr_sample.loop_point_samples(),
        decoded_samples_f32,
        fft_range,
        spectrum,
    }
}
