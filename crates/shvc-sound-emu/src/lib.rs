//! SHVC-Sound emulator

extern crate cxx;
use cxx::UniquePtr;

#[cxx::bridge(namespace = "shvc_sound_emu")]
mod ffi {
    pub struct ResetRegisters {
        pub pc: u16,
        pub a: u8,
        pub x: u8,
        pub y: u8,
        pub psw: u8,
        pub sp: u8,
        /// S-DSP Echo Start Address register
        pub esa: u8,
        /// S-DSP Echo Delay register
        pub edl: u8,
    }

    unsafe extern "C++" {
        include!("shvc-sound-emu.hpp");

        type ShvcSoundEmu;

        fn new_emulator(iplrom: &[u8; 64]) -> UniquePtr<ShvcSoundEmu>;

        fn reset(self: Pin<&mut ShvcSoundEmu>, registers: ResetRegisters);

        fn iplrom(self: &ShvcSoundEmu) -> &[u8; 64];
        fn iplrom_mut(self: Pin<&mut ShvcSoundEmu>) -> &mut [u8; 64];

        fn apuram(self: &ShvcSoundEmu) -> &[u8; 65536];
        fn apuram_mut(self: Pin<&mut ShvcSoundEmu>) -> &mut [u8; 65536];

        fn dsp_registers(self: &ShvcSoundEmu) -> &[u8; 128];

        fn write_dsp_register(self: Pin<&mut ShvcSoundEmu>, addr: u8, value: u8);
        fn write_smp_register(self: Pin<&mut ShvcSoundEmu>, addr: u8, value: u8);

        fn read_io_ports(self: &ShvcSoundEmu) -> [u8; 4];
        fn write_io_ports(self: Pin<&mut ShvcSoundEmu>, ports: [u8; 4]);

        fn program_counter(self: &ShvcSoundEmu) -> u16;

        fn emulate(self: Pin<&mut ShvcSoundEmu>) -> &[i16; 512];
    }
}

pub use ffi::ResetRegisters;

pub struct ShvcSoundEmu {
    emu: UniquePtr<ffi::ShvcSoundEmu>,
}

impl ShvcSoundEmu {
    pub const AUDIO_BUFFER_SAMPLES: usize = 256;
    pub const AUDIO_BUFFER_SIZE: usize = Self::AUDIO_BUFFER_SAMPLES * 2;

    #[allow(clippy::new_without_default)]
    pub fn new(iplrom: &[u8; 64]) -> Self {
        let emu = ffi::new_emulator(iplrom);
        if emu.is_null() {
            panic!("new_emulator() returned null");
        }
        Self { emu }
    }

    /// CAUTION: also resets S-DSP and S-SMP registers
    pub fn reset(&mut self, registers: ResetRegisters) {
        self.emu.pin_mut().reset(registers);
    }

    pub fn iplrom(&self) -> &[u8; 64] {
        self.emu.iplrom()
    }
    pub fn iplrom_mut(&mut self) -> &mut [u8; 64] {
        self.emu.pin_mut().iplrom_mut()
    }

    pub fn apuram(&self) -> &[u8; 65536] {
        self.emu.apuram()
    }
    pub fn apuram_mut(&mut self) -> &mut [u8; 65536] {
        self.emu.pin_mut().apuram_mut()
    }

    pub fn dsp_registers(&self) -> &[u8; 128] {
        self.emu.dsp_registers()
    }

    /// This method is not reccomended for the `ESA` and `EDL` registers.
    pub fn write_dsp_register(self: &mut ShvcSoundEmu, addr: u8, value: u8) {
        self.emu.pin_mut().write_dsp_register(addr, value)
    }

    pub fn write_smp_register(self: &mut ShvcSoundEmu, addr: u8, value: u8) {
        self.emu.pin_mut().write_smp_register(addr, value)
    }

    pub fn read_io_ports(&self) -> [u8; 4] {
        self.emu.read_io_ports()
    }

    pub fn write_io_ports(&mut self, ports: [u8; 4]) {
        self.emu.pin_mut().write_io_ports(ports)
    }

    pub fn program_counter(self: &ShvcSoundEmu) -> u16 {
        self.emu.program_counter()
    }

    pub fn emulate(&mut self) -> &[i16; Self::AUDIO_BUFFER_SIZE] {
        self.emu.pin_mut().emulate()
    }
}
