//! Sample Analyser Dialog

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::AudioMessage;
use crate::compiler_thread::{ItemId, ToCompiler};
use crate::helpers::{ch_units_to_width, input_height, label_packed, InputForm, SetActive};
use crate::sample_widgets::{BrrSettingsWidget, SampleWidgetEditor, SourceFileType};
use crate::{GuiMessage, InstrumentOrSampleId};

use brr::{BrrSample, MonoPcm16WaveFile};
use compiler::data::{self, BrrEvaluator, LoopSetting};

use std::cell::RefCell;
use std::cmp::{max, min};
use std::ops::Range;
use std::rc::Rc;
use std::sync::{mpsc, Arc};

extern crate fltk;
use compiler::errors::BrrError;
use compiler::path::SourcePathBuf;
use fltk::app;
use fltk::button::Button;
use fltk::draw;
use fltk::enums::{Align, Color, Event, Font, FrameType};
use fltk::group::{Flex, Group, Pack, PackType};
use fltk::menu::Choice;
use fltk::misc::Spinner;
use fltk::output::Output;
use fltk::prelude::{GroupExt, InputExt, MenuExt, WidgetBase, WidgetExt, WindowExt};
use fltk::widget::Widget;
use fltk::window::Window;

extern crate spectrum_analyzer;
use spectrum_analyzer::{samples_fft_to_spectrum, scaling, FrequencyLimit, FrequencySpectrum};

const WAVEFORM_ZERO_COLOR: Color = Color::from_rgb(0, 0, 0);
const WAVEFORM_LOOP_POINT_COLOR: Color = Color::from_rgb(128, 128, 128);
const WAVEFORM_COLOR: Color = Color::from_rgb(0, 0, 255);

const SPECTRUM_COLOR: Color = Color::from_rgb(128, 0, 128);
const SPECTRUM_INST_FREQ_COLOR: Color = Color::from_rgb(128, 128, 128);

/// Sample rate to decode BRR samples at (S-DSP sample rate)
const BRR_SAMPLE_RATE: u32 = 32000;
/// Minimum number of samples to render when decoding a looping BRR sample
const MIN_LOOPING_SAMPLES: usize = 2048;

/// spectrum-analyzer crate panics if samples.len() > 16384 samples
const MAX_SPECTRUM_SAMPLES: usize = 16384;

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

enum WaveformMoveEvent {
    ZoomIn,
    ZoomOut,
    ScrollLeft,
    ScrollRight,
}

pub struct SampleAnalyserDialog {
    state: Rc<RefCell<State>>,
}

struct State {
    sender: app::Sender<GuiMessage>,
    compiler_sender: mpsc::Sender<ToCompiler>,
    audio_sender: mpsc::Sender<AudioMessage>,

    item_id: Option<InstrumentOrSampleId>,
    source: SourcePathBuf,
    freq: f64,
    loop_setting: data::LoopSetting,
    evaluator: data::BrrEvaluator,

    analysis: Option<SampleAnalysis>,
    analysis_error: Option<String>,

    spectrum_max_freq: f64,
    spectrum_x_scale: f64,
    spectrum_cursor_clicked: Option<(f64, f64)>,

    waveform_x_offset: usize,
    waveform_x_scale: f64,

    window: Window,

    spectrum: Widget,
    waveform: Widget,

    spectrum_stats_group: Pack,
    spectrum_range_choice: Choice,
    peak_freq: Output,
    cursor_freq: Output,
    cursor_peak_freq: Output,

    use_peak: Button,
    use_cursor: Button,
    use_cursor_peak: Button,

    source_out: Output,
    freq_widget: Spinner,
    brr_settings_widget: BrrSettingsWidget,

    ok_button: Button,
    play_button: Button,
}

impl SampleAnalyserDialog {
    pub fn new(
        sender: app::Sender<GuiMessage>,
        compiler_sender: mpsc::Sender<ToCompiler>,
        audio_sender: mpsc::Sender<AudioMessage>,
    ) -> Self {
        let mut window = Window::default();

        window.set_label("Sample Analyser");
        window.set_size(
            ch_units_to_width(&window, 120),
            ch_units_to_width(&window, 65),
        );
        window.size_range(
            ch_units_to_width(&window, 90),
            ch_units_to_width(&window, 65),
            0,
            0,
        );
        window.make_resizable(true);
        window.make_modal(true);

        let ch = |ch_units| ch_units_to_width(&window, ch_units);

        let mut group = Flex::default_fill().column();
        let margin = ch(1);
        group.set_pad(margin);
        group.set_margin(margin);

        let spectrum = Widget::default();

        let mut spectrum_stats_group = Pack::default();
        spectrum_stats_group.set_type(PackType::Horizontal);
        spectrum_stats_group.set_spacing(margin / 2);

        let line_height = input_height(&spectrum_stats_group);
        let use_width = ch(4);
        let stats_width = ch(10);

        let spectrum_range_choice = Choice::default().with_size(ch(15), line_height);

        label_packed("Peak:");
        let peak_freq = Output::default().with_size(stats_width, line_height);
        let use_peak = Button::default()
            .with_label("Use")
            .with_size(use_width, line_height);

        label_packed(" Cursor:");
        let cursor_freq = Output::default().with_size(stats_width, line_height);
        let use_cursor = Button::default()
            .with_label("Use")
            .with_size(use_width, line_height);

        label_packed(" Cursor (peak):");
        let cursor_peak_freq = Output::default().with_size(stats_width, line_height);
        let use_cursor_peak = Button::default()
            .with_label("Use")
            .with_size(use_width, line_height);

        spectrum_stats_group.end();
        group.fixed(&spectrum_stats_group, line_height);

        let waveform = Widget::default();

        let mut bottom_column = Flex::default().row();

        let mut form = InputForm::new(15);
        let source_out = form.add_input::<Output>("Source:");
        let freq = form.add_input::<Spinner>("Frequency:");
        let brr_settings_widget = BrrSettingsWidget::new(&mut form);

        let form_row_height = form.row_height();
        let form_height = 4 * form.row_height() + margin;
        bottom_column.fixed(&form.take_group_end(), ch(65));

        let _empty_space = Widget::default();

        let (ok_button, mut cancel_button, play_button) = {
            let bw = ch(10);
            let bh = form_row_height;

            let last_x = bw + margin;
            let last_y = form_height - form_row_height;

            let mut b_group = Group::default();
            b_group.set_size(bw * 2 + margin, form_height);
            b_group.make_resizable(false);

            let mut play_button = Button::new(last_x, 0, bw, bh, "@>");
            play_button.set_tooltip("Play BRR sample at 32000Hz");

            let ok_button = Button::new(0, last_y, bw, bh, "Ok");
            let cancel_button = Button::new(last_x, last_y, bw, bh, "Cancel");

            b_group.end();
            bottom_column.fixed(&b_group, b_group.w());

            (ok_button, cancel_button, play_button)
        };

        group.fixed(&bottom_column, form_height);
        bottom_column.end();

        group.end();
        window.end();

        let state = Rc::new(RefCell::from(State {
            sender,
            compiler_sender,
            audio_sender,

            item_id: None,
            source: Default::default(),
            freq: 500.0,
            loop_setting: LoopSetting::None,
            evaluator: BrrEvaluator::Default,

            analysis: None,
            analysis_error: None,

            spectrum_max_freq: MAX_SPECTRUM_FREQ.into(),
            spectrum_x_scale: 0.0,
            spectrum_cursor_clicked: None,
            waveform_x_offset: 0,
            waveform_x_scale: 1.0,

            window,

            spectrum,
            waveform,

            spectrum_stats_group,
            spectrum_range_choice,
            peak_freq,
            cursor_freq,
            cursor_peak_freq,

            use_peak,
            use_cursor,
            use_cursor_peak,

            source_out,
            freq_widget: freq,
            brr_settings_widget,

            ok_button,
            play_button,
        }));

        {
            let mut s = state.borrow_mut();

            s.spectrum.draw({
                let state = state.clone();
                move |_| {
                    state.borrow().draw_waveform();
                }
            });
            s.spectrum.handle({
                let state = state.clone();
                move |_, ev| Self::handle_spectrum_event(ev, &state)
            });

            s.waveform.draw({
                let state = state.clone();
                move |_| {
                    state.borrow_mut().draw_spectrum();
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
            s.ok_button.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow_mut().ok_button_clicked();
                }
            });

            cancel_button.set_callback({
                let mut window = s.window.clone();
                move |_| {
                    window.hide();
                }
            });

            s.brr_settings_widget.set_editor(state.clone());

            s.freq_widget.set_minimum(0.0);
            s.freq_widget.set_maximum(16000.0);

            s.freq_widget.set_callback({
                let state = state.clone();
                move |_| {
                    state.borrow_mut().freq_changed();
                }
            });

            s.spectrum_range_choice.add_choice(SPECTRUM_RANGE_CHOICES);
            s.spectrum_range_choice.set_value(1);
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

    pub fn show_for_instrument(&mut self, id: ItemId, inst: &data::Instrument) {
        self.state.borrow_mut().show(
            InstrumentOrSampleId::Instrument(id),
            inst.source.clone(),
            inst.freq,
            inst.loop_setting.clone(),
            inst.evaluator,
        );
    }

    pub fn show_for_sample(&mut self, id: ItemId, s: &data::Sample) {
        self.state.borrow_mut().show(
            InstrumentOrSampleId::Sample(id),
            s.source.clone(),
            0.0,
            s.loop_setting.clone(),
            s.evaluator,
        );
    }

    pub fn analysis_from_compiler_thread(&mut self, r: Result<SampleAnalysis, BrrError>) {
        self.state.borrow_mut().analysis_from_compiler_thread(r)
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

impl SampleWidgetEditor for State {
    fn on_finished_editing(&mut self) {
        let (loop_setting, evaluator) = self.brr_settings_widget.read_or_reset(&self.loop_setting);

        self.evaluator = evaluator;

        if let Some(ls) = loop_setting {
            self.loop_setting = ls;
            self.send_analyse_sample_message();
        }
    }

    fn loop_settings(&self) -> &LoopSetting {
        &self.loop_setting
    }
}

impl State {
    fn freq_changed(&mut self) {
        self.freq = self.freq_widget.value();

        self.spectrum.redraw();
        self.window.redraw();
    }

    fn show(
        &mut self,
        id: InstrumentOrSampleId,
        source: SourcePathBuf,
        freq: f64,
        loop_setting: LoopSetting,
        evaluator: BrrEvaluator,
    ) {
        self.item_id = Some(id);

        self.source = source;
        self.freq = freq;
        self.loop_setting = loop_setting;
        self.evaluator = evaluator;

        self.source_out.set_value(self.source.as_str());
        self.freq_widget.set_value(freq);
        self.brr_settings_widget
            .set_value(&self.loop_setting, self.evaluator);

        self.brr_settings_widget
            .update_loop_type_choice(SourceFileType::from_source(&self.source));

        self.analysis = None;
        self.analysis_error = None;

        self.clear_spectrum_values();

        self.play_button.deactivate();
        self.ok_button.deactivate();
        let _ = self.ok_button.take_focus();

        let is_instrument = matches!(id, InstrumentOrSampleId::Instrument(_));
        self.freq_widget.set_active(is_instrument);
        self.use_peak.set_active(is_instrument);
        self.use_cursor.set_active(is_instrument);
        self.use_cursor_peak.set_active(is_instrument);

        self.window.show();

        self.send_analyse_sample_message();
    }

    fn send_analyse_sample_message(&self) {
        let _ = self.compiler_sender.send(ToCompiler::AnalyseSample(
            self.source.clone(),
            self.loop_setting.clone(),
            self.evaluator,
        ));
    }

    fn analysis_from_compiler_thread(&mut self, r: Result<SampleAnalysis, BrrError>) {
        match r {
            Ok(a) => {
                match &a.spectrum {
                    Ok(s) => self.set_spectrum_values(s),
                    Err(_) => self.clear_spectrum_values(),
                }
                self.analysis = Some(a);
                self.analysis_error = None;

                self.play_button.activate();
                self.ok_button.activate();
            }
            Err(e) => {
                self.clear_spectrum_values();

                self.analysis = None;
                self.analysis_error = Some(e.to_string());

                self.play_button.deactivate();
                self.ok_button.deactivate();
            }
        }

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

    fn set_freq(&mut self, f: f64) {
        let f = f.round();

        self.freq = f;
        self.freq_widget.set_value(f);

        self.spectrum.redraw();
        self.window.redraw();
    }

    fn use_peak_clicked(&mut self) {
        if let Some(a) = &self.analysis {
            if let Ok(s) = &a.spectrum {
                self.set_freq(s.max().0.val().into());
            }
        }
    }

    fn use_cursor_clicked(&mut self) {
        if let Some(c) = self.spectrum_cursor_clicked {
            self.set_freq(c.0);
        }
    }

    fn use_cursor_peak_clicked(&mut self) {
        if let Some(c) = self.spectrum_cursor_clicked {
            self.set_freq(c.1);
        }
    }

    fn spectrum_range_changed(&mut self) {
        self.spectrum_max_freq = read_spectrum_range_choice(&self.spectrum_range_choice);

        self.spectrum.redraw();
        self.window.redraw();
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

        if let Some(e) = &self.analysis_error {
            draw::set_draw_color(Color::Red);
            draw::set_font(Font::Helvetica, app::font_size());
            draw::draw_text2(e, x + 4, y + 4, w - 4, h - 4, Align::TopLeft);
        }

        if let Some(analysis) = &self.analysis {
            self.draw_waveform_waveform(analysis, x, y, w, h);
        }

        draw::pop_clip();
    }

    fn draw_waveform_waveform(&self, analysis: &SampleAnalysis, x: i32, y: i32, w: i32, h: i32) {
        let samples = &analysis.decoded_samples_f32;
        let n_samples = samples.len();

        let samples_default_zoom = Self::waveform_samples_default_zoom(analysis);
        let samples_range = Range {
            start: self.waveform_x_offset,
            end: min(
                self.waveform_x_offset
                    + ((samples_default_zoom as f64) / self.waveform_x_scale) as usize,
                samples.len(),
            ),
        };

        let center_y = y + h / 2;

        let x_scale = f64::from(w) / (samples_default_zoom as f64) * self.waveform_x_scale;
        let y_scale = f64::from(-h / 2);

        // Draw loop points
        if let Some(lp) = analysis.loop_point_samples {
            if lp < n_samples {
                let loop_size = analysis.brr_sample.n_samples() - lp;

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
        min(
            analysis.decoded_samples_f32.len(),
            max(analysis.n_samples * 2, MIN_WAVEFORM_WIDTH),
        )
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
            self.window.redraw();
        }
    }

    fn play_button_clicked(&self) {
        if let Some(a) = &self.analysis {
            let _ = self
                .audio_sender
                .send(AudioMessage::PlayBrrSampleAt32Khz(a.brr_sample.clone()));
        }
    }

    fn ok_button_clicked(&mut self) {
        if let Some(id) = &mut self.item_id {
            self.sender.send(GuiMessage::CommitSampleAnalyserChanges {
                id: *id,
                freq: self.freq,
                loop_setting: self.loop_setting.clone(),
                evaluator: self.evaluator,
            });
        }
        self.window.hide();
    }
}

pub struct SampleAnalysis {
    brr_sample: Arc<BrrSample>,
    n_samples: usize,
    decoded_samples_f32: Vec<f32>,
    loop_point_samples: Option<usize>,
    spectrum: Result<FrequencySpectrum, String>,
}

impl std::fmt::Debug for SampleAnalysis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt("SampleAnalysis{..}", f)
    }
}

// Called by the compiler thread
pub fn analyse_sample(
    brr_sample: Arc<BrrSample>,
    wav_sample: Option<&MonoPcm16WaveFile>,
) -> SampleAnalysis {
    const FLOAT_SCALE: f32 = -(i16::MIN as f32);

    let n_samples = match wav_sample {
        Some(w) => w.samples.len(),
        None => brr_sample.n_samples(),
    };

    let samples_to_decode = if brr_sample.is_looping() {
        usize::max(brr_sample.n_samples(), MIN_LOOPING_SAMPLES)
    } else {
        brr_sample.n_samples()
    };
    let samples_to_decode = match samples_to_decode {
        ..=MAX_SPECTRUM_SAMPLES => samples_to_decode.next_power_of_two(),
        n => n,
    };

    let mut decoded_samples = vec![0_i16; samples_to_decode];
    brr_sample.decode_into_buffer(&mut decoded_samples, 0, 0, 0);

    let decoded_samples_f32: Vec<f32> = decoded_samples
        .iter()
        .map(|&s| f32::from(s) / FLOAT_SCALE)
        .collect();

    // Analyse spectrum
    let spectrum = samples_fft_to_spectrum(
        match decoded_samples_f32.len() {
            ..=MAX_SPECTRUM_SAMPLES => &decoded_samples_f32,
            _ => &decoded_samples_f32[..MAX_SPECTRUM_SAMPLES],
        },
        BRR_SAMPLE_RATE,
        FrequencyLimit::Max(MAX_SPECTRUM_FREQ),
        Some(&scaling::divide_by_N_sqrt),
    )
    .map_err(|e| format!("ERROR: {:?}", e));

    SampleAnalysis {
        loop_point_samples: brr_sample.loop_point_samples(),
        n_samples,
        brr_sample,
        decoded_samples_f32,
        spectrum,
    }
}
