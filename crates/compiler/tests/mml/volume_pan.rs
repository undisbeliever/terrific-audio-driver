// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn set_volume() {
    assert_line_matches_bytecode("v1", &["set_volume 16"]);
    assert_line_matches_bytecode("v8", &["set_volume 128"]);
    assert_line_matches_bytecode("v16", &["set_volume 255"]);

    assert_line_matches_bytecode("V0", &["set_volume 0"]);
    assert_line_matches_bytecode("V42", &["set_volume 42"]);
    assert_line_matches_bytecode("V255", &["set_volume 255"]);
}

#[test]
fn adjust_volume() {
    assert_line_matches_bytecode("v+1", &["adjust_volume +16"]);
    assert_line_matches_bytecode("v+2", &["adjust_volume +32"]);
    assert_line_matches_bytecode("v+7", &["adjust_volume +112"]);
    assert_line_matches_bytecode("v-3", &["adjust_volume -48"]);
    assert_line_matches_bytecode("v-4", &["adjust_volume -64"]);
    assert_line_matches_bytecode("v-8", &["adjust_volume -128"]);

    assert_line_matches_bytecode("V-20", &["adjust_volume -20"]);
    assert_line_matches_bytecode("V-40", &["adjust_volume -40"]);
    assert_line_matches_bytecode("V+60", &["adjust_volume +60"]);
    assert_line_matches_bytecode("V+70", &["adjust_volume +70"]);
}

#[test]
fn set_pan() {
    assert_line_matches_bytecode("p0", &["set_pan 0"]);
    assert_line_matches_bytecode("p64", &["set_pan 64"]);
    assert_line_matches_bytecode("p128", &["set_pan 128"]);
}

#[test]
fn adjust_pan() {
    assert_line_matches_bytecode("p+16", &["adjust_pan +16"]);
    assert_line_matches_bytecode("p+32", &["adjust_pan +32"]);
    assert_line_matches_bytecode("p-48", &["adjust_pan -48"]);
    assert_line_matches_bytecode("p-64", &["adjust_pan -64"]);
}

#[test]
fn merge_pan_commands() {
    merge_mml_commands_test("p1 p2 || p3 p4", &["set_pan 4"]);

    merge_mml_commands_test("p0 || p+5 p+6", &["set_pan 11"]);
    merge_mml_commands_test("p100 || p+100", &["set_pan 128"]);
    merge_mml_commands_test("p0 p+5 || p10", &["set_pan 10"]);
    merge_mml_commands_test("p+1 || p+2 p+3", &["adjust_pan +6"]);

    merge_mml_commands_test("p120 || p-10 p-20", &["set_pan 90"]);
    merge_mml_commands_test("p50 || p-100", &["set_pan 0"]);
    merge_mml_commands_test("p10 p-5 || p10", &["set_pan 10"]);
    merge_mml_commands_test("p-4 || p-5 p-6", &["adjust_pan -15"]);
}

#[test]
fn px_pan() {
    assert_line_matches_bytecode("px0", &["set_pan 64"]);
    assert_line_matches_bytecode("px-64", &["set_pan 0"]);
    assert_line_matches_bytecode("px+64", &["set_pan 128"]);

    assert_one_error_in_mml_line("px-65", 1, ValueError::PxPanOutOfRange(-65).into());
    assert_one_error_in_mml_line("px+65", 1, ValueError::PxPanOutOfRange(65).into());

    assert_line_matches_bytecode("px+16", &["set_pan 80"]);
    assert_line_matches_bytecode("px+32", &["set_pan 96"]);
    assert_line_matches_bytecode("px-48", &["set_pan 16"]);
    assert_line_matches_bytecode("px-15", &["set_pan 49"]);
}

#[test]
fn merge_px_pan() {
    merge_mml_commands_test("px+1 px-2 || px+3 px-4", &["set_pan 60"]);
    merge_mml_commands_test("px-1 px+2 || px-3 px+4", &["set_pan 68"]);

    merge_mml_commands_test("px-20 || p+5 p+6", &["set_pan 55"]);
    merge_mml_commands_test("px+40 || p+100", &["set_pan 128"]);
    merge_mml_commands_test("px-50 p+5 || px+10", &["set_pan 74"]);
    merge_mml_commands_test("p+1 || px+20 p-40", &["set_pan 44"]);
}

#[test]
fn merge_coarse_volume() {
    merge_mml_commands_test("v1 v2 || v3 v4", &["set_volume 64"]);

    merge_mml_commands_test("v0 v+5 || v+6", &["set_volume 176"]);
    merge_mml_commands_test("v12 || v+7", &["set_volume 255"]);
    merge_mml_commands_test("v0 || v+5 v10", &["set_volume 160"]);
    merge_mml_commands_test("v+1 v+2 || v+3", &["adjust_volume +96"]);

    merge_mml_commands_test("v15 || v-5 v-2", &["set_volume 128"]);
    merge_mml_commands_test("v3 || v-5", &["set_volume 0"]);
    merge_mml_commands_test("v0 v-5 || v10", &["set_volume 160"]);
    merge_mml_commands_test("v-1 || v-2 v-3", &["adjust_volume -96"]);
}

#[test]
fn merge_fine_volume() {
    merge_mml_commands_test("V1 V2 || V3 V4", &["set_volume 4"]);

    merge_mml_commands_test("V0 V+5 || V+6", &["set_volume 11"]);
    merge_mml_commands_test("V180 || V+120", &["set_volume 255"]);
    merge_mml_commands_test("V0 V+5 || V10", &["set_volume 10"]);
    merge_mml_commands_test("V+1 || V+2 V+3", &["adjust_volume +6"]);

    merge_mml_commands_test("V120 V-10 || V-20", &["set_volume 90"]);
    merge_mml_commands_test("V50 || V-100", &["set_volume 0"]);
    merge_mml_commands_test("V10 V-5 || V10", &["set_volume 10"]);
    merge_mml_commands_test("V-4 || V-5 V-6", &["adjust_volume -15"]);
}

#[test]
fn merge_pan_and_volume() {
    merge_mml_commands_test("p0 || v5", &["set_pan_and_volume 0 80"]);
    merge_mml_commands_test("v6 || p128", &["set_pan_and_volume 128 96"]);
    merge_mml_commands_test("p30 || V40", &["set_pan_and_volume 30 40"]);
    merge_mml_commands_test("V80 || p90", &["set_pan_and_volume 90 80"]);

    merge_mml_commands_test("p10 || V+5", &["adjust_volume +5", "set_pan 10"]);
    merge_mml_commands_test("V10 || p+5", &["set_volume 10", "adjust_pan +5"]);

    merge_mml_commands_test("p-10 || V5", &["set_volume 5", "adjust_pan -10"]);
    merge_mml_commands_test("V-10 || p5", &["adjust_volume -10", "set_pan 5"]);

    merge_mml_commands_test("p-10 || V+5", &["adjust_volume +5", "adjust_pan -10"]);
    merge_mml_commands_test("V-10 || p+5", &["adjust_volume -10", "adjust_pan +5"]);
}

#[test]
fn large_adjust_volume() {
    assert_line_matches_bytecode("V+127", &["adjust_volume +127"]);
    assert_line_matches_bytecode("V+128", &["adjust_volume +127", "adjust_volume +1"]);
    assert_line_matches_bytecode("V+200", &["adjust_volume +127", "adjust_volume +73"]);
    assert_line_matches_bytecode("V+100 V+100", &["adjust_volume +127", "adjust_volume +73"]);

    assert_line_matches_bytecode("v+12", &["adjust_volume +127", "adjust_volume +65"]);
    assert_line_matches_bytecode("v+6 v+6", &["adjust_volume +127", "adjust_volume +65"]);

    assert_line_matches_bytecode("V-128", &["adjust_volume -128"]);
    assert_line_matches_bytecode("V-129", &["adjust_volume -128", "adjust_volume -1"]);
    assert_line_matches_bytecode("V-200", &["adjust_volume -128", "adjust_volume -72"]);
    assert_line_matches_bytecode("V-100 V-100", &["adjust_volume -128", "adjust_volume -72"]);

    assert_line_matches_bytecode("v-12", &["adjust_volume -128", "adjust_volume -64"]);
    assert_line_matches_bytecode("v-6 v-6", &["adjust_volume -128", "adjust_volume -64"]);

    assert_line_matches_bytecode("V+254", &["adjust_volume +127", "adjust_volume +127"]);
    assert_line_matches_bytecode("V+255", &["set_volume 255"]);
    assert_line_matches_bytecode("V+400", &["set_volume 255"]);
    assert_line_matches_bytecode("V+200 V+200", &["set_volume 255"]);

    assert_line_matches_bytecode("V-254", &["adjust_volume -128", "adjust_volume -126"]);
    assert_line_matches_bytecode("V-255", &["set_volume 0"]);
    assert_line_matches_bytecode("V-400", &["set_volume 0"]);
    assert_line_matches_bytecode("V-200 V-200", &["set_volume 0"]);

    assert_line_matches_bytecode("v+16", &["set_volume 255"]);
    assert_line_matches_bytecode("v-16", &["set_volume 0"]);

    assert_line_matches_bytecode("V+200 V+200 V-10", &["set_volume 245"]);
    assert_line_matches_bytecode("V-200 V-200 V+10", &["set_volume 10"]);
}

// Tests if a large relative pan command turns into an absolute pan command
#[test]
fn large_adjust_pan() {
    assert_line_matches_bytecode("p+127", &["adjust_pan +127"]);
    assert_line_matches_bytecode("p+128", &["set_pan 128"]);
    assert_line_matches_bytecode("p+200", &["set_pan 128"]);
    assert_line_matches_bytecode("p+100 p+100", &["set_pan 128"]);

    assert_line_matches_bytecode("p-127", &["adjust_pan -127"]);
    assert_line_matches_bytecode("p-128", &["set_pan 0"]);
    assert_line_matches_bytecode("p-200", &["set_pan 0"]);
    assert_line_matches_bytecode("p-100 p-100", &["set_pan 0"]);

    assert_line_matches_bytecode("p+100 p+100 p-10", &["set_pan 118"]);
    assert_line_matches_bytecode("p-100 p-100 p+10", &["set_pan 10"]);
}

#[test]
fn volume_slide() {
    assert_line_matches_bytecode("vs+2,8", &["volume_slide +32 8"]);
    assert_line_matches_bytecode("vs-4,16", &["volume_slide -$40 16"]);
    assert_line_matches_bytecode("vs+16,20", &["volume_slide +255 20"]);
    assert_line_matches_bytecode("vs-16,20", &["volume_slide -255 20"]);

    assert_line_matches_bytecode("Vs+20,50", &["volume_slide +20 50"]);
    assert_line_matches_bytecode("Vs-30,80", &["volume_slide -30 80"]);

    // 0x10ff / 16 = 0x10f
    assert_line_matches_bytecode_bytes("vs+1,16", &[opcodes::VOLUME_SLIDE_UP, 16, 0x0f, 0x01]);
    // 0x30ff / 32 = 0x187
    assert_line_matches_bytecode_bytes("vs+3,32", &[opcodes::VOLUME_SLIDE_UP, 32, 0x87, 0x01]);
    // 0x31ff / 14 = 0x392
    assert_line_matches_bytecode_bytes("Vs +49,14", &[opcodes::VOLUME_SLIDE_UP, 14, 0x92, 0x03]);
    // (0x100 * 111 + 0xff) / 256 = 111
    assert_line_matches_bytecode_bytes("Vs+111,256", &[opcodes::VOLUME_SLIDE_UP, 0, 111, 0]);

    assert_line_matches_bytecode_bytes("vs-7,1", &[opcodes::VOLUME_SLIDE_DOWN, 1, 0xff, 0x70]);
    // 0x80ff / 40 = 0x339
    assert_line_matches_bytecode_bytes("vs-8,40", &[opcodes::VOLUME_SLIDE_DOWN, 40, 0x39, 0x03]);
    // (0x100 * 200 + 0xff) / 140 = 0x16f
    assert_line_matches_bytecode_bytes(
        "Vs-200,140",
        &[opcodes::VOLUME_SLIDE_DOWN, 140, 0x6f, 0x01],
    );
    // (0x100 * 222 + 0xff) / 256 = 222
    assert_line_matches_bytecode_bytes("Vs -222,256", &[opcodes::VOLUME_SLIDE_DOWN, 0, 222, 0]);

    assert_one_error_in_mml_line(
        "vs+17,100",
        1,
        ValueError::CoarseVolumeSlideOutOfRange(17).into(),
    );
    assert_one_error_in_mml_line(
        "Vs+256,100",
        1,
        ValueError::VolumeSlideAmountOutOfRange(256).into(),
    );
    assert_one_error_in_mml_line(
        "vs-17,100",
        1,
        ValueError::CoarseVolumeSlideOutOfRange(-17).into(),
    );
    assert_one_error_in_mml_line(
        "Vs-256,100",
        1,
        ValueError::VolumeSlideAmountOutOfRange(-256).into(),
    );

    assert_one_error_in_mml_line(
        "vs+4,0",
        6,
        ValueError::VolumeSlideTicksOutOfRange(0).into(),
    );
    assert_one_error_in_mml_line(
        "Vs+100,0",
        8,
        ValueError::VolumeSlideTicksOutOfRange(0).into(),
    );
    assert_one_error_in_mml_line(
        "vs-4,0",
        6,
        ValueError::VolumeSlideTicksOutOfRange(0).into(),
    );
    assert_one_error_in_mml_line(
        "Vs-100,0",
        8,
        ValueError::VolumeSlideTicksOutOfRange(0).into(),
    );

    assert_one_error_in_mml_line(
        "vs+4,257",
        6,
        ValueError::VolumeSlideTicksOutOfRange(257).into(),
    );
    assert_one_error_in_mml_line(
        "Vs+100,257",
        8,
        ValueError::VolumeSlideTicksOutOfRange(257).into(),
    );
    assert_one_error_in_mml_line(
        "vs-4,257",
        6,
        ValueError::VolumeSlideTicksOutOfRange(257).into(),
    );
    assert_one_error_in_mml_line(
        "Vs-100,257",
        8,
        ValueError::VolumeSlideTicksOutOfRange(257).into(),
    );

    assert_one_error_in_mml_line("vs+10", 1, ValueError::NoCommaVolumeSlideTicks.into());
    assert_one_error_in_mml_line("vs-10", 1, ValueError::NoCommaVolumeSlideTicks.into());
    assert_one_error_in_mml_line("Vs+10", 1, ValueError::NoCommaVolumeSlideTicks.into());
    assert_one_error_in_mml_line("Vs-10", 1, ValueError::NoCommaVolumeSlideTicks.into());
}

#[test]
fn tremolo() {
    assert_line_matches_bytecode("v~2,4", &["tremolo 32 4"]);
    assert_line_matches_bytecode("v~7,8", &["tremolo 112 8"]);
    assert_line_matches_bytecode("v~8,8", &["tremolo 127 8"]);

    assert_line_matches_bytecode("V~20,10", &["tremolo 20 10"]);
    assert_line_matches_bytecode("V~30,20", &["tremolo 30 20"]);

    // 0x107f / 10 = 0x1a6
    assert_line_matches_bytecode_bytes("v~1,10", &[opcodes::TREMOLO, 10, 0xa6, 0x01]);
    // 0x307f / 8 = 0x60f
    assert_line_matches_bytecode_bytes("v~3,8", &[opcodes::TREMOLO, 8, 0x0f, 0x06]);

    // 0x287f / 6 = 0x6bf
    assert_line_matches_bytecode_bytes("V~40,6", &[opcodes::TREMOLO, 6, 0xbf, 0x06]);

    // 0x7f7f / 127 = 0x101 (largest values)
    assert_line_matches_bytecode_bytes("V~127,127", &[opcodes::TREMOLO, 127, 0x01, 0x01]);

    assert_one_error_in_mml_line(
        "v~0,10",
        1,
        ValueError::CoarseTremoloAmplitudeOutOfRange(0).into(),
    );
    assert_one_error_in_mml_line(
        "v~9,10",
        1,
        ValueError::CoarseTremoloAmplitudeOutOfRange(9).into(),
    );
    assert_one_error_in_mml_line(
        "V~0,10",
        1,
        ValueError::TremoloAmplitudeOutOfRange(0).into(),
    );
    assert_one_error_in_mml_line(
        "V~128,10",
        1,
        ValueError::TremoloAmplitudeOutOfRange(128).into(),
    );

    assert_one_error_in_mml_line(
        "v~4,0",
        5,
        ValueError::TremoloQuarterWavelengthTicksOutOfRange(0).into(),
    );
    assert_one_error_in_mml_line(
        "V~100,0",
        7,
        ValueError::TremoloQuarterWavelengthTicksOutOfRange(0).into(),
    );

    assert_one_error_in_mml_line(
        "v~4,128",
        5,
        ValueError::TremoloQuarterWavelengthTicksOutOfRange(128).into(),
    );
    assert_one_error_in_mml_line(
        "V~100,128",
        7,
        ValueError::TremoloQuarterWavelengthTicksOutOfRange(128).into(),
    );

    assert_one_error_in_mml_line("v~3", 1, ValueError::NoCommaQuarterWavelength.into());
    assert_one_error_in_mml_line("V~10", 1, ValueError::NoCommaQuarterWavelength.into());
}

#[test]
fn pan_slide() {
    assert_line_matches_bytecode("ps+15,8", &["pan_slide +15 8"]);
    assert_line_matches_bytecode("ps-30,16", &["pan_slide -30 16"]);

    assert_line_matches_bytecode("ps+128,50", &["pan_slide +128 50"]);
    assert_line_matches_bytecode("ps-128,256", &["pan_slide -128 256"]);

    // 0x14ff / 14 = 0x17f
    assert_line_matches_bytecode_bytes("ps +20,14", &[opcodes::PAN_SLIDE_UP, 14, 0x7f, 0x01]);
    // 0x80ff / 60 = 0x226
    assert_line_matches_bytecode_bytes("ps +128,60", &[opcodes::PAN_SLIDE_UP, 60, 0x26, 0x02]);
    // (0x100 * 64 + 0xff) / 256 = 64
    assert_line_matches_bytecode_bytes("ps+64,256", &[opcodes::PAN_SLIDE_UP, 0, 64, 0]);

    // 0x37ff / 10 = 0x599
    assert_line_matches_bytecode_bytes("ps-55,10", &[opcodes::PAN_SLIDE_DOWN, 10, 0x99, 0x05]);
    // (0x100 * 30 + 0xff) / 256 = 30
    assert_line_matches_bytecode_bytes("ps -30,256", &[opcodes::PAN_SLIDE_DOWN, 0, 30, 0]);

    assert_one_error_in_mml_line(
        "ps+129,100",
        1,
        ValueError::PanSlideAmountOutOfRange(129).into(),
    );
    assert_one_error_in_mml_line(
        "ps-129,100",
        1,
        ValueError::PanSlideAmountOutOfRange(-129).into(),
    );

    assert_one_error_in_mml_line("ps+20,0", 7, ValueError::PanSlideTicksOutOfRange(0).into());
    assert_one_error_in_mml_line("ps-40,0", 7, ValueError::PanSlideTicksOutOfRange(0).into());

    assert_one_error_in_mml_line(
        "ps+50,257",
        7,
        ValueError::PanSlideTicksOutOfRange(257).into(),
    );
    assert_one_error_in_mml_line(
        "ps-60,257",
        7,
        ValueError::PanSlideTicksOutOfRange(257).into(),
    );

    assert_one_error_in_mml_line("ps+10", 1, ValueError::NoCommaPanSlideTicks.into());
    assert_one_error_in_mml_line("ps-10", 1, ValueError::NoCommaPanSlideTicks.into());
}

#[test]
fn panbrello() {
    assert_line_matches_bytecode("p~20,10", &["panbrello 20 10"]);
    assert_line_matches_bytecode("p~30,20", &["panbrello 30 20"]);

    // 0x3c7f / 5 = 0xc19
    assert_line_matches_bytecode_bytes("p~60,5", &[opcodes::PANBRELLO, 5, 0x19, 0x0c]);

    // 0x177f / 17 = 0x161
    assert_line_matches_bytecode_bytes("p~23,17", &[opcodes::PANBRELLO, 17, 0x61, 0x01]);

    // 0x407f / 127 = 0x82 (largest values)
    assert_line_matches_bytecode_bytes("p~64,127", &[opcodes::PANBRELLO, 127, 0x82, 0x00]);

    assert_one_error_in_mml_line(
        "p~0,10",
        1,
        ValueError::PanbrelloAmplitudeOutOfRange(0).into(),
    );
    assert_one_error_in_mml_line(
        "p~65,10",
        1,
        ValueError::PanbrelloAmplitudeOutOfRange(65).into(),
    );

    assert_one_error_in_mml_line(
        "p~10,0",
        6,
        ValueError::PanbrelloQuarterWavelengthTicksOutOfRange(0).into(),
    );

    assert_one_error_in_mml_line(
        "p~20,128",
        6,
        ValueError::PanbrelloQuarterWavelengthTicksOutOfRange(128).into(),
    );

    assert_one_error_in_mml_line("p~10", 1, ValueError::NoCommaQuarterWavelength.into());
}

#[test]
fn l_in_volume_slide() {
    assert_line_matches_line("vs+5,l8", "vs+5,12");
    assert_line_matches_line("vs-6,l2", "vs-6,48");

    assert_line_matches_line("C192 Vs+50,l8", "Vs+50,24");
    assert_line_matches_line("C192 Vs-60,l4", "Vs-60,48");
}

#[test]
fn l_in_tremolo() {
    assert_line_matches_line("C192 v~3,l24", "v~3,8");
    assert_line_matches_line("C192 v~4,l32", "v~4,6");

    assert_line_matches_line("V~50,l8", "V~50,12");
    assert_line_matches_line("V~60,l3", "V~60,32");
}

#[test]
fn l_in_pan_slide() {
    assert_line_matches_line("ps+10,l8", "ps+10,12");
    assert_line_matches_line("ps-20,l2", "ps-20,48");

    assert_line_matches_line("C160 ps+50,l6", "ps+50,26");
    assert_line_matches_line("C160 ps-60,l5", "ps-60,32");
}

#[test]
fn l_in_panbrello() {
    assert_line_matches_line("p~60,l8", "p~60,12");
    assert_line_matches_line("p~50,l3", "p~50,32");

    assert_line_matches_line("C192 p~40,l32", "p~40,6");
    assert_line_matches_line("C192 p~30,l24", "p~30,8");
}
