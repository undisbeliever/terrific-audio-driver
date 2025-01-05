#include "shvc-sound-emu.hpp"

#include "shvc-sound-emu/src/lib.rs.h"

#include "spc700/spc700.cpp"
#include "smp/smp.cpp"
#include "dsp/dsp.cpp"

namespace shvc_sound_emu {

auto new_emulator(const std::array<uint8_t, 64>& iplrom) -> std::unique_ptr<ShvcSoundEmu>
{
    return std::make_unique<ShvcSoundEmu>(iplrom);
}

ShvcSoundEmu::ShvcSoundEmu(const std::array<uint8_t, 64>& iplrom)
  : smp()
{
  smp.iplrom = iplrom;
  smp.power(false);
}

ShvcSoundEmu::~ShvcSoundEmu() = default;

auto ShvcSoundEmu::reset(ResetRegisters r) -> void {
  constexpr uint8_t ESA_REG = 0x6d;
  constexpr uint8_t EDL_REG = 0x7d;

  smp.power(true);

  smp.r.pc.w = r.pc;
  smp.r.ya.byte.l = r.a;
  smp.r.x = r.x;
  smp.r.ya.byte.h = r.y;
  smp.r.p = r.psw;
  smp.r.s = r.sp;

  smp.dsp.write(ESA_REG, r.esa);
  smp.dsp.write(EDL_REG, r.edl);

  // Echo buffer address/offset/length changes are not instant when the ESA and EDL registers change.
  smp.dsp.resetEchoBuffer();
}

auto ShvcSoundEmu::iplrom() const -> const std::array<uint8_t, 64>& {
  return smp.iplrom;
}

auto ShvcSoundEmu::iplrom_mut () -> std::array<uint8_t, 64>& {
  return smp.iplrom;
}

auto ShvcSoundEmu::apuram() const -> const std::array<uint8_t, 65536>& {
  return smp.dsp.apuram;
}

auto ShvcSoundEmu::apuram_mut () -> std::array<uint8_t, 65536>& {
  return smp.dsp.apuram;
}

auto ShvcSoundEmu::dsp_registers() const -> const std::array<uint8_t, 128>& {
  return smp.dsp.registers;
}

auto ShvcSoundEmu::write_dsp_register(uint8_t addr, uint8_t value) -> void {
  if(addr < smp.dsp.registers.size()) {
    smp.dsp.write(addr, value);
  }
}

auto ShvcSoundEmu::write_smp_register(uint8_t addr, uint8_t value) -> void {
  if(addr > 0xf0 && addr < 0x100) {
    smp.write(addr, value);
  }
}

auto ShvcSoundEmu::read_io_ports() const -> std::array<uint8_t, 4> {
  std::array<uint8_t, 4> out;
  for(auto i : range(4)) {
    out[i] = smp.portRead(i);
  }
  return out;
}

auto ShvcSoundEmu::write_io_ports(std::array<uint8_t, 4> ports) -> void {
  for(auto i : range(4)) {
    smp.portWrite(i, ports[i]);
  }
}

auto ShvcSoundEmu::program_counter() const -> uint16_t {
  return smp.r.pc.w;
}

auto ShvcSoundEmu::emulate() -> const std::array<int16_t, AUDIO_BUFFER_SIZE>& {
  smp.dsp.sampleBuffer.reset();

  while(!smp.dsp.sampleBuffer.isFull()) {
    smp.main();
  }

  return smp.dsp.sampleBuffer.samples();
}

}
