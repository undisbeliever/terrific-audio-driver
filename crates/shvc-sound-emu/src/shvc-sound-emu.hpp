#pragma once

#include <array>
#include <memory>
#include <cstdint>

#include <nall/platform.hpp>
#include <nall/endian.hpp>
#include <nall/literals.hpp>
#include <nall/memory.hpp>
#include <nall/primitives.hpp>

using namespace nall;
using namespace nall::primitives;

#include "types.hpp"

#include "sample-buffer.hpp"

#include "spc700/spc700.hpp"
#include "dsp/dsp.hpp"
#include "smp/smp.hpp"

namespace shvc_sound_emu {

struct ShvcSoundEmu {
  constexpr static uint32_t AUDIO_BUFFER_SAMPLES = 256;
  constexpr static uint32_t AUDIO_BUFFER_SIZE = AUDIO_BUFFER_SAMPLES * 2;

  ShvcSoundEmu(const std::array<uint8_t, 64>& iplrom);
  ~ShvcSoundEmu();

  auto power(bool reset) -> void;

  auto iplrom() const -> const std::array<uint8_t, 64>&;
  auto iplrom_mut () -> std::array<uint8_t, 64>&;

  auto apuram() const -> const std::array<uint8_t, 65536>&;
  auto apuram_mut () -> std::array<uint8_t, 65536>&;

  auto dsp_registers() const -> const std::array<uint8_t, 128>&;

  auto set_echo_buffer_size(uint8_t esa, uint8_t edl) -> void;

  auto write_dsp_register(uint8_t addr, uint8_t value) -> void;
  auto write_smp_register(uint8_t addr, uint8_t value) -> void;

  auto read_io_ports() const -> std::array<uint8_t, 4>;
  auto write_io_ports(std::array<uint8_t, 4> ports) -> void;

  auto set_spc_registers(uint16_t pc, uint8_t a, uint8_t x, uint8_t y, uint8_t psw, uint8_t sp) -> void;

  auto program_counter() const -> uint16_t;

  auto emulate() -> const std::array<int16_t, AUDIO_BUFFER_SIZE>&;

private:
  SMP smp;
};

auto new_emulator(const std::array<uint8_t, 64>& iplrom) -> std::unique_ptr<ShvcSoundEmu>;

}
