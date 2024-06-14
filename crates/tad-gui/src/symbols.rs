//! Custom tad-gui symbols

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use fltk::app::add_symbol;
use fltk::draw::{
    begin_complex_polygon, begin_line, begin_loop, begin_polygon, end_complex_polygon, end_line,
    end_loop, end_polygon, scale_xy, set_draw_color, vertex,
};
use fltk::enums::Color;

const ADD_REMOVE_LENGTH: f64 = 1.8;
const ADD_REMOVE_ARROW: f64 = ADD_REMOVE_LENGTH / 2.0;
const ADD_REMOVE_WIDTH: f64 = 0.25;
const ADD_REMOVE_DIAG: f64 = 0.1767766952966369; // sqrt(WIDTH * WIDTH / 2)

const PLAY_WIDTH: f64 = 1.0;
const STOP_SIZE: f64 = 1.8;

const PAUSE_BAR_WIDTH: f64 = 0.6;
const PAUSE_BAR_HEIGHT: f64 = 1.8;

trait DrawType {
    fn begin();
    fn end();

    fn begin_complex();
    fn end_complex();
}

struct Fill;
impl DrawType for Fill {
    fn begin() {
        begin_polygon()
    }
    fn end() {
        end_polygon()
    }
    fn begin_complex() {
        begin_complex_polygon()
    }
    fn end_complex() {
        end_complex_polygon()
    }
}

struct Outline;
impl DrawType for Outline {
    fn begin() {
        begin_loop()
    }
    fn end() {
        end_loop()
    }
    fn begin_complex() {
        begin_loop()
    }
    fn end_complex() {
        end_loop()
    }
}

fn rect<T: DrawType>(x1: f64, y1: f64, x2: f64, y2: f64) {
    T::begin();

    vertex(x1, y1);
    vertex(x2, y1);
    vertex(x2, y2);
    vertex(x1, y2);

    T::end();
}

fn play<T: DrawType>() {
    const X: f64 = PLAY_WIDTH / 2.0;

    T::begin();

    vertex(-X, -1.0);
    vertex(X, 0.0);
    vertex(-X, 1.0);

    T::end();
}

fn left_play<T: DrawType>() {
    T::begin();

    vertex(-1.0, -1.0);
    vertex(PLAY_WIDTH - 1.0, 0.0);
    vertex(-1.0, 1.0);

    T::end();
}

fn right_newline<T: DrawType>() {
    const LW: f64 = 0.25;
    const LY: f64 = LW * 3.0;
    const LX: f64 = LW * 3.0;

    const AY: f64 = 0.25;
    const AX: f64 = AY * 2.0;

    // Top-right corner
    const X: f64 = 1.0;
    const Y: f64 = LW / 2.0;

    T::begin_complex();

    vertex(X, Y);
    vertex(X, Y + LY);
    vertex(X - LX, Y + LY);
    vertex(X - LX, Y + LY + AY);
    vertex(X - LX - AX, Y + LY - LW / 2.0);
    vertex(X - LX, Y + LY - LW - AY);
    vertex(X - LX, Y + LY - LW);
    vertex(X - LW, Y + LY - LW);
    vertex(X - LW, Y);

    T::end_complex();
}

fn right_cursor() {
    const W: f64 = 0.75;
    const Y: f64 = STOP_SIZE / 2.0;
    const CX: f64 = 1.0 - W / 2.0;

    let line = |x1, y1, x2, y2| {
        begin_line();
        vertex(x1, y1);
        vertex(x2, y2);
        end_line();
    };

    line(1.0 - W, -Y, 1.0, -Y);
    line(1.0 - W, Y, 1.0, Y);
    line(CX, -Y, CX, Y);
}

fn draw_add_symbol(c: Color) {
    const L: f64 = ADD_REMOVE_LENGTH / 2.0;
    const W: f64 = ADD_REMOVE_WIDTH / 2.0;

    set_draw_color(c);
    rect::<Fill>(-W, -L, W, L);
    rect::<Fill>(-L, -W, L, W);
}

fn draw_clone_symbol(c: Color) {
    const REAR_SIZE: f64 = 1.0;
    const BOX_SIZE: f64 = 1.5;

    const A: f64 = -1.0 + REAR_SIZE;
    const B: f64 = 1.0 - BOX_SIZE;

    set_draw_color(c);

    begin_line();
    vertex(A, B);
    vertex(A, -1.0);
    vertex(-1.0, -1.0);
    vertex(-1.0, A);
    vertex(B, A);
    end_line();

    rect::<Outline>(B, B, 1.0, 1.0);

    // Cross
    {
        const L: f64 = BOX_SIZE * 0.75 / 2.0;
        const W: f64 = ADD_REMOVE_WIDTH / 2.0;
        const O: f64 = B + BOX_SIZE / 2.0;

        rect::<Fill>(O - W, O - L, O + W, O + L);
        rect::<Fill>(O - L, O - W, O + L, O + W);
    }
}

fn draw_remove_symbol(c: Color) {
    const L: f64 = ADD_REMOVE_LENGTH / 2.0;
    const W: f64 = ADD_REMOVE_WIDTH / 2.0;

    set_draw_color(c);
    rect::<Fill>(-L, -W, L, W);
}

fn up_arrow<T: DrawType>() {
    const X1: f64 = 0.0;
    const X2: f64 = ADD_REMOVE_ARROW;
    const X3: f64 = ADD_REMOVE_ARROW - ADD_REMOVE_DIAG;
    const X4: f64 = ADD_REMOVE_WIDTH / 2.0;

    const Y1: f64 = -ADD_REMOVE_LENGTH / 2.0;
    const Y2: f64 = Y1 + ADD_REMOVE_ARROW;
    const Y3: f64 = Y2 + ADD_REMOVE_DIAG;
    const Y4: f64 = Y3 - X3 + X4;
    const Y5: f64 = ADD_REMOVE_LENGTH / 2.0;

    T::begin_complex();
    vertex(X1, Y1);
    vertex(X2, Y2);
    vertex(X3, Y3);
    vertex(X4, Y4);
    vertex(X4, Y5);
    vertex(-X4, Y5);
    vertex(-X4, Y4);
    vertex(-X3, Y3);
    vertex(-X2, Y2);
    vertex(-X1, Y1);
    T::end_complex();
}

fn draw_move_top_symbol(c: Color) {
    const L: f64 = ADD_REMOVE_LENGTH / 2.0;
    const W: f64 = ADD_REMOVE_WIDTH / 2.0;

    set_draw_color(c);
    rect::<Fill>(-L, -L, L, -L + W * 2.0);
    up_arrow::<Fill>();
}

fn draw_move_up_symbol(c: Color) {
    set_draw_color(c);
    up_arrow::<Fill>();
}

fn draw_move_down_symbol(c: Color) {
    scale_xy(1.0, -1.0);
    draw_move_up_symbol(c);
}

fn draw_move_bottom_symbol(c: Color) {
    scale_xy(1.0, -1.0);
    draw_move_top_symbol(c);
}

fn draw_play_symbol<T: DrawType>(c: Color) {
    set_draw_color(c);
    play::<T>();
}

fn draw_play_line_symbol<T: DrawType>(c: Color) {
    set_draw_color(c);
    left_play::<T>();
    right_newline::<T>();
}

fn draw_play_cursor_symbol<T: DrawType>(c: Color) {
    set_draw_color(c);
    left_play::<T>();
    right_cursor();
}

fn draw_stop_symbol(c: Color) {
    const S: f64 = STOP_SIZE / 2.0;

    set_draw_color(c);
    rect::<Fill>(-S, -S, S, S);
}

fn draw_pause_symbol(c: Color) {
    const X: f64 = STOP_SIZE / 2.0;
    const Y: f64 = PAUSE_BAR_HEIGHT / 2.0;
    const W: f64 = PAUSE_BAR_WIDTH;

    set_draw_color(c);
    rect::<Fill>(-X, -Y, -X + W, Y);
    rect::<Fill>(X - W, -Y, X, Y);
}

fn draw_center_pan_symbol(c: Color) {
    const BOX_W: f64 = 0.375;
    const BOX_H: f64 = SPEAKER_H / 2.0;
    const BOX_OFFSET: f64 = 0.2;
    const SPEAKER_H: f64 = 2.0;

    set_draw_color(c);

    let speaker = |x1, x2, cone_x| {
        const Y: f64 = BOX_H / 2.0;

        rect::<Outline>(x1, Y, x2, -Y);

        begin_line();
        vertex(x1, -Y);
        vertex(cone_x, -SPEAKER_H / 2.0);
        vertex(cone_x, SPEAKER_H / 2.0);
        vertex(x1, Y);
        end_line();
    };
    speaker(-BOX_OFFSET - BOX_W, -BOX_OFFSET, -1.0);
    speaker(BOX_OFFSET + BOX_W, BOX_OFFSET, 1.0);
}

pub fn register_symbols() {
    let _ = add_symbol("add", true, draw_add_symbol);
    let _ = add_symbol("clone", true, draw_clone_symbol);
    let _ = add_symbol("remove", true, draw_remove_symbol);
    let _ = add_symbol("top", true, draw_move_top_symbol);
    let _ = add_symbol("up", true, draw_move_up_symbol);
    let _ = add_symbol("down", true, draw_move_down_symbol);
    let _ = add_symbol("bottom", true, draw_move_bottom_symbol);

    let _ = add_symbol("play", true, draw_play_symbol::<Fill>);
    let _ = add_symbol("play_line", true, draw_play_line_symbol::<Fill>);
    let _ = add_symbol("play_c_line", true, draw_play_line_symbol::<Outline>);
    let _ = add_symbol("play_cursor", true, draw_play_cursor_symbol::<Fill>);
    let _ = add_symbol("play_c_cursor", true, draw_play_cursor_symbol::<Outline>);
    let _ = add_symbol("stop", true, draw_stop_symbol);
    let _ = add_symbol("pause", true, draw_pause_symbol);

    let _ = add_symbol("center_pan", true, draw_center_pan_symbol);
}
