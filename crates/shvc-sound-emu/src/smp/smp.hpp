//Sony CXP1100Q-1

namespace shvc_sound_emu {

struct SMP : SPC700 {
  // ::TODO find out what this does::
  auto synchronizing() const -> bool override { return false; }

  auto main() -> void;
  auto power(bool reset) -> void;

  //memory.cpp
  auto write(n16 address, n8 data) -> void override;

  //io.cpp
  auto portRead(n2 port) const -> n8;
  auto portWrite(n2 port, n8 data) -> void;

  DSP dsp;
  std::array<uint8_t, 64> iplrom;

private:
  struct IO {
    //timing
    u32 clockCounter = 0;
    u32 dspCounter = 0;

    //external
    n8 apu0;
    n8 apu1;
    n8 apu2;
    n8 apu3;

    //$00f0
    n1 timersDisable;
    n1 ramWritable = true;
    n1 ramDisable;
    n1 timersEnable = true;
    n2 externalWaitStates;
    n2 internalWaitStates;

    //$00f1
    n1 iplromEnable = true;

    //$00f2
    n8 dspAddress;

    //$00f4-00f7
    n8 cpu0;
    n8 cpu1;
    n8 cpu2;
    n8 cpu3;

    //$00f8-00f9
    n8 aux4;
    n8 aux5;
  } io;

  //memory.cpp
  auto readRAM(n16 address) -> n8;
  auto writeRAM(n16 address, n8 data) -> void;

  auto idle() -> void override;
  auto read(n16 address) -> n8 override;

  auto readDisassembler(n16 address) -> n8 override;

  //io.cpp
  auto readIO(n16 address) -> n8;
  auto writeIO(n16 address, n8 data) -> void;

  template<u32 Frequency>
  struct Timer {
    n8 stage0;
    n8 stage1;
    n8 stage2;
    n4 stage3;
    b1 line;
    b1 enable;
    n8 target;

    //timing.cpp
    auto step(const SMP& smp, u32 clocks) -> void;
    auto synchronizeStage1(const SMP& smp) -> void;
  };

  Timer<128> timer0;
  Timer<128> timer1;
  Timer< 16> timer2;

  //timing.cpp
  auto wait(bool halve, maybe<n16> address = nothing) -> void;
  auto step(u32 clocks) -> void;
  auto stepTimers(u32 clocks) -> void;
};

}
