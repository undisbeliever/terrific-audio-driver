fn main() {
    cxx_build::bridge("src/lib.rs")
        .file("src/shvc-sound-emu.cpp")
        .include("src")
        .flag_if_supported("-std=c++17")
        .warnings(false)
        .compile("cxx-apu");

    println!("cargo:rerun-if-changed=src/")
}
