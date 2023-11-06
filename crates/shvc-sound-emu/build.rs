fn main() {
    cxx_build::bridge("src/lib.rs")
        .file("src/shvc-sound-emu.cpp")
        .cpp(true)
        .std("c++17")
        .include("src")
        .warnings(false)
        .compile("cxx-apu");

    println!("cargo:rerun-if-changed=src/")
}
