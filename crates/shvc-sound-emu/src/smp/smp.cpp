#include <sfc/sfc.hpp>

namespace shvc_sound_emu {

#include "memory.cpp"
#include "io.cpp"
#include "timing.cpp"

auto SMP::main() -> void {
  // ::TODO verify Wait and Stop will advance the DSP::
  if(r.wait) return instructionWait();
  if(r.stop) return instructionStop();

  instruction();
}

auto SMP::power(bool reset) -> void {
  if(auto fp = system.pak->read("ipl.rom")) {
    fp->read({iplrom, 64});
  }

  SPC700::power();

  dsp.power(reset);

  r.pc.byte.l = iplrom[62];
  r.pc.byte.h = iplrom[63];

  io = {};
  timer0 = {};
  timer1 = {};
  timer2 = {};
}

}
