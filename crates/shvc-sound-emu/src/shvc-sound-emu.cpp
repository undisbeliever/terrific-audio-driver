#include "shvc-sound-emu.hpp"

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

auto ShvcSoundEmu::power(bool reset) -> void {
  smp.power(reset);
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

auto ShvcSoundEmu::set_echo_buffer_size(uint8_t esa, uint8_t edl) -> void {
  constexpr uint8_t ESA_REG = 0x6d;
  constexpr uint8_t EDL_REG = 0x7d;

  smp.dsp.write(ESA_REG, esa);
  smp.dsp.write(EDL_REG, edl);

  // Echo buffer address/offset/length changes are not instant when the ESA and EDL registers change.
  smp.dsp.resetEchoBuffer();
}

auto ShvcSoundEmu::dsp_registers() const -> const std::array<uint8_t, 128>& {
  return smp.dsp.registers;
}

auto ShvcSoundEmu::write_io_ports(std::array<uint8_t, 4> ports) -> void {
  for(auto i : range(4)) {
    smp.portWrite(i, ports[i]);
  }
}

auto ShvcSoundEmu::set_spc_registers(uint16_t pc, uint8_t a, uint8_t x, uint8_t y, uint8_t psw, uint8_t sp) -> void {
  smp.r.pc.w = pc;
  smp.r.ya.byte.l = a;
  smp.r.x = x;
  smp.r.ya.byte.h = y;
  smp.r.p = psw;
  smp.r.s = sp;
}

auto ShvcSoundEmu::emulate() -> const std::array<int16_t, AUDIO_BUFFER_SIZE>& {
  smp.dsp.sampleBuffer.reset();

  while(!smp.dsp.sampleBuffer.isFull()) {
    smp.main();
  }

  return smp.dsp.sampleBuffer.samples();
}

}
