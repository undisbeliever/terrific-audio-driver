#pragma once

namespace shvc_sound_emu {

struct SampleBuffer {
  constexpr static u32 N_SAMPLES = 256;
  constexpr static u32 MASK = N_SAMPLES * 2 - 2;

  auto reset() -> void {
    pos = 0;
  }

  auto write(i16 left, i16 right) -> void {
    pos &= MASK;

    buffer.at(pos) = left;
    buffer.at(pos + 1) = right;

    pos += 2;
  }

  auto isFull() -> bool {
    return pos > MASK;
  }

  auto samples() const -> const std::array<int16_t, N_SAMPLES * 2>& {
    return buffer;
  }

private:
  std::array<int16_t, N_SAMPLES * 2> buffer;
  u32 pos = MASK;
};

}
