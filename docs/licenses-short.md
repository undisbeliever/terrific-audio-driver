Licenses
========


TLDR
====

The terrific audio driver is copyright (c) 2023, Marcus Rowe.

 * The audio driver (S-SMP and `.spc` code) is licensed under the zlib License.
 * The compiler and GUI are licensed under the MIT License.
 * The audio emulator used by the GUI is licensed under the ISC License.

 * The compiler and GUI make use of multiple third party open source projects.
 * The GUI is based in part on the work of the FLTK project (https://www.fltk.org).


Terrific Audio Driver
=====================

The terrific audio driver is copyright (c) 2023, Marcus Rowe.


Audio Driver
------------
The audio driver (S-SMP and `.spc` file export code in the `audio-driver` directory) is released under the zlib License:

<br/>

```
zlib License

This software is provided 'as-is', without any express or implied warranty.  In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

     1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

     2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

     3. This notice may not be removed or altered from any source distribution.
```


Compiler and GUI
----------------

The compiler and GUI executables are released under the MIT License:

```
The MIT License (MIT)

Copyright (c) 2023, Marcus Rowe <undisbeliever@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```


Audio emulator
--------------

The following crates are released under the ISC license:
 * shvc-sound-emu


```
Copyright (c) 2004-2021 ares team, Near et al
With modifications and rust bindings Copyright (c) 2023, Marcus Rowe <undisbeliever@gmail.com>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
```


Third Party Licenses
====================
This section lists the third-party projects used by the Terrific Audio Driver.
It is generated using [cargo-about](https://embarkstudios.github.io/cargo-about/).

Please see the included [licenses file](licenses.md) for the full third party license text.

  * [codespan-reporting 0.13.1](https://github.com/brendanzab/codespan), Apache License 2.0
  * [cpal 0.16.0](https://github.com/rustaudio/cpal), Apache License 2.0
  * [windows-sys 0.45.0](https://github.com/microsoft/windows-rs), MIT License
  * [windows-sys 0.61.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows-targets 0.42.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows-targets 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_aarch64_gnullvm 0.42.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows_aarch64_gnullvm 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_aarch64_msvc 0.42.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows_aarch64_msvc 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_i686_gnu 0.42.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows_i686_gnu 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_i686_msvc 0.42.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows_i686_msvc 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_gnu 0.42.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_gnu 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_gnullvm 0.42.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_gnullvm 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_msvc 0.42.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_msvc 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [markdown 1.0.0](https://github.com/wooorm/markdown-rs), MIT License
  * [cc 1.2.60](https://github.com/rust-lang/cc-rs), MIT License
  * [cfg-if 1.0.4](https://github.com/rust-lang/cfg-if), MIT License
  * [cmake 0.1.58](https://github.com/rust-lang/cmake-rs), MIT License
  * [find-msvc-tools 0.1.9](https://github.com/rust-lang/cc-rs), MIT License
  * [js-sys 0.3.94](https://github.com/wasm-bindgen/wasm-bindgen/tree/master/crates/js-sys), MIT License
  * [pkg-config 0.3.32](https://github.com/rust-lang/pkg-config-rs), MIT License
  * [wasm-bindgen-futures 0.4.67](https://github.com/wasm-bindgen/wasm-bindgen/tree/master/crates/futures), MIT License
  * [wasm-bindgen-macro-support 0.2.117](https://github.com/wasm-bindgen/wasm-bindgen/tree/master/crates/macro-support), MIT License
  * [wasm-bindgen-macro 0.2.117](https://github.com/wasm-bindgen/wasm-bindgen/tree/master/crates/macro), MIT License
  * [wasm-bindgen-shared 0.2.117](https://github.com/wasm-bindgen/wasm-bindgen/tree/master/crates/shared), MIT License
  * [wasm-bindgen 0.2.117](https://github.com/wasm-bindgen/wasm-bindgen), MIT License
  * [web-sys 0.3.94](https://github.com/wasm-bindgen/wasm-bindgen/tree/master/crates/web-sys), MIT License
  * [bitflags 1.3.2](https://github.com/bitflags/bitflags), MIT License
  * [bitflags 2.11.0](https://github.com/bitflags/bitflags), MIT License
  * [log 0.4.29](https://github.com/rust-lang/log), MIT License
  * [num-complex 0.4.6](https://github.com/rust-num/num-complex), MIT License
  * [num-derive 0.4.2](https://github.com/rust-num/num-derive), MIT License
  * [num-traits 0.2.19](https://github.com/rust-num/num-traits), MIT License
  * [regex-automata 0.4.14](https://github.com/rust-lang/regex), MIT License
  * [regex-syntax 0.8.10](https://github.com/rust-lang/regex), MIT License
  * [regex 1.12.3](https://github.com/rust-lang/regex), MIT License
  * [relative-path 2.0.1](https://github.com/udoprog/relative-path), MIT License
  * [float-cmp 0.10.0](https://github.com/mikedilger/float-cmp), MIT License
  * [coreaudio-rs 0.13.0](https://github.com/RustAudio/coreaudio-rs.git), MIT License
  * [heck 0.5.0](https://github.com/withoutboats/heck), MIT License
  * [unicode-id 0.3.6](https://github.com/Boshen/unicode-id), MIT License
  * [unicode-width 0.2.2](https://github.com/unicode-rs/unicode-width), MIT License
  * [jni-sys 0.3.1](https://github.com/jni-rs/jni-sys), MIT License
  * [jni-sys 0.4.1](https://github.com/jni-rs/jni-sys), MIT License
  * [futures-core 0.3.32](https://github.com/rust-lang/futures-rs), MIT License
  * [futures-task 0.3.32](https://github.com/rust-lang/futures-rs), MIT License
  * [futures-util 0.3.32](https://github.com/rust-lang/futures-rs), MIT License
  * [hashbrown 0.17.0](https://github.com/rust-lang/hashbrown), MIT License
  * [utf8parse 0.2.2](https://github.com/alacritty/vte), MIT License
  * [indexmap 2.14.0](https://github.com/indexmap-rs/indexmap), MIT License
  * [equivalent 1.0.2](https://github.com/indexmap-rs/equivalent), MIT License
  * [bytes 1.11.1](https://github.com/tokio-rs/bytes), MIT License
  * [autocfg 1.5.0](https://github.com/cuviper/autocfg), MIT License
  * [ttf-parser 0.25.1](https://github.com/harfbuzz/ttf-parser), MIT License
  * [ringbuf 0.4.8](https://github.com/agerasev/ringbuf.git), MIT License
  * [slab 0.4.12](https://github.com/tokio-rs/slab), MIT License
  * [bumpalo 3.20.2](https://github.com/fitzgen/bumpalo), MIT License
  * [mach2 0.4.3](https://github.com/JohnTitor/mach2), MIT License
  * [anstream 1.0.0](https://github.com/rust-cli/anstyle.git), MIT License
  * [anstyle-parse 1.0.0](https://github.com/rust-cli/anstyle.git), MIT License
  * [anstyle-query 1.1.5](https://github.com/rust-cli/anstyle.git), MIT License
  * [anstyle-wincon 3.0.11](https://github.com/rust-cli/anstyle.git), MIT License
  * [anstyle 1.0.14](https://github.com/rust-cli/anstyle.git), MIT License
  * [clap 4.6.0](https://github.com/clap-rs/clap), MIT License
  * [clap_builder 4.6.0](https://github.com/clap-rs/clap), MIT License
  * [clap_derive 4.6.0](https://github.com/clap-rs/clap), MIT License
  * [clap_lex 1.1.0](https://github.com/clap-rs/clap), MIT License
  * [colorchoice 1.0.5](https://github.com/rust-cli/anstyle.git), MIT License
  * [is_terminal_polyfill 1.70.2](https://github.com/polyfill-rs/is_terminal_polyfill), MIT License
  * [once_cell_polyfill 1.70.2](https://github.com/polyfill-rs/once_cell_polyfill), MIT License
  * [toml_datetime 1.1.1+spec-1.1.0](https://github.com/toml-rs/toml), MIT License
  * [toml_edit 0.25.11+spec-1.1.0](https://github.com/toml-rs/toml), MIT License
  * [toml_parser 1.1.2+spec-1.1.0](https://github.com/toml-rs/toml), MIT License
  * [libc 0.2.184](https://github.com/rust-lang/libc), MIT License
  * [alsa 0.9.1](https://github.com/diwic/alsa-rs), MIT License
  * [static_assertions 1.1.0](https://github.com/nvzqz/static-assertions-rs), MIT License
  * [alsa-sys 0.3.1](https://github.com/diwic/alsa-sys), MIT License
  * [microfft 0.6.0](https://gitlab.com/teskje/microfft-rs), MIT License
  * [spectrum-analyzer 1.7.0](https://github.com/phip1611/spectrum-analyzer), MIT License
  * [cmk 0.1.2](https://github.com/MoAlyousef/cmk), MIT License
  * [cesu8 1.1.0](https://github.com/emk/cesu8-rs), MIT License
  * [dasp_sample 0.11.0](https://github.com/rustaudio/sample.git), MIT License
  * [jni-sys-macros 0.4.1](https://github.com/jni-rs/jni-sys), MIT License
  * [libm 0.2.16](https://github.com/rust-lang/compiler-builtins), MIT License
  * [ndk-context 0.1.1](https://github.com/rust-windowing/android-ndk-rs), MIT License
  * [ndk-sys 0.6.0+11769913](https://github.com/rust-mobile/ndk), MIT License
  * [ndk 0.9.0](https://github.com/rust-mobile/ndk), MIT License
  * [objc2-audio-toolbox 0.3.2](https://github.com/madsmtm/objc2), MIT License
  * [objc2-core-audio-types 0.3.2](https://github.com/madsmtm/objc2), MIT License
  * [objc2-core-audio 0.3.2](https://github.com/madsmtm/objc2), MIT License
  * [objc2-core-foundation 0.3.2](https://github.com/madsmtm/objc2), MIT License
  * [objc2-encode 4.1.0](https://github.com/madsmtm/objc2), MIT License
  * [objc2-foundation 0.3.2](https://github.com/madsmtm/objc2), MIT License
  * [objc2 0.6.4](https://github.com/madsmtm/objc2), MIT License
  * [windows-core 0.54.0](https://github.com/microsoft/windows-rs), MIT License
  * [windows-link 0.2.1](https://github.com/microsoft/windows-rs), MIT License
  * [windows-result 0.1.2](https://github.com/microsoft/windows-rs), MIT License
  * [windows 0.54.0](https://github.com/microsoft/windows-rs), MIT License
  * [windows_i686_gnullvm 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [fltk-sys 1.5.22](https://github.com/fltk-rs/fltk-rs), MIT License
  * [fltk 1.5.22](https://github.com/fltk-rs/fltk-rs), MIT License
  * [minipaste 0.1.0](https://github.com/MoAlyousef/minipaste), MIT License
  * [cxx-build 1.0.194](https://github.com/dtolnay/cxx), MIT License
  * [cxx 1.0.194](https://github.com/dtolnay/cxx), MIT License
  * [cxxbridge-flags 1.0.194](https://github.com/dtolnay/cxx), MIT License
  * [cxxbridge-macro 1.0.194](https://github.com/dtolnay/cxx), MIT License
  * [itoa 1.0.18](https://github.com/dtolnay/itoa), MIT License
  * [link-cplusplus 1.0.12](https://github.com/dtolnay/link-cplusplus), MIT License
  * [num_enum 0.7.6](https://github.com/illicitonion/num_enum), MIT License
  * [num_enum_derive 0.7.6](https://github.com/illicitonion/num_enum), MIT License
  * [once_cell 1.21.4](https://github.com/matklad/once_cell), MIT License
  * [paste 1.0.15](https://github.com/dtolnay/paste), MIT License
  * [pin-project-lite 0.2.17](https://github.com/taiki-e/pin-project-lite), MIT License
  * [proc-macro-crate 3.5.0](https://github.com/bkchr/proc-macro-crate), MIT License
  * [proc-macro2 1.0.106](https://github.com/dtolnay/proc-macro2), MIT License
  * [quote 1.0.45](https://github.com/dtolnay/quote), MIT License
  * [rustversion 1.0.22](https://github.com/dtolnay/rustversion), MIT License
  * [scratch 1.0.9](https://github.com/dtolnay/scratch), MIT License
  * [serde 1.0.228](https://github.com/serde-rs/serde), MIT License
  * [serde_core 1.0.228](https://github.com/serde-rs/serde), MIT License
  * [serde_derive 1.0.228](https://github.com/serde-rs/serde), MIT License
  * [serde_json 1.0.149](https://github.com/serde-rs/json), MIT License
  * [syn 2.0.117](https://github.com/dtolnay/syn), MIT License
  * [thiserror-impl 1.0.69](https://github.com/dtolnay/thiserror), MIT License
  * [thiserror 1.0.69](https://github.com/dtolnay/thiserror), MIT License
  * [unicode-ident 1.0.24](https://github.com/dtolnay/unicode-ident), MIT License
  * [zmij 1.0.21](https://github.com/dtolnay/zmij), MIT License
  * [winnow 1.0.1](https://github.com/winnow-rs/winnow), MIT License
  * [aho-corasick 1.1.4](https://github.com/BurntSushi/aho-corasick), MIT License
  * [memchr 2.8.0](https://github.com/BurntSushi/memchr), MIT License
  * [termcolor 1.4.1](https://github.com/BurntSushi/termcolor), MIT License
  * [walkdir 2.5.0](https://github.com/BurntSushi/walkdir), MIT License
  * [strsim 0.11.1](https://github.com/rapidfuzz/strsim-rs), MIT License
  * [combine 4.6.7](https://github.com/Marwes/combine), MIT License
  * [shlex 1.3.0](https://github.com/comex/rust-shlex), MIT License
  * [jni 0.21.1](https://github.com/jni-rs/jni-rs), MIT License
  * [same-file 1.0.6](https://github.com/BurntSushi/same-file), MIT License
  * [winapi-util 0.1.11](https://github.com/BurntSushi/winapi-util), MIT License
  * [crossbeam-channel 0.5.15](https://github.com/crossbeam-rs/crossbeam), MIT License
  * [crossbeam-utils 0.8.21](https://github.com/crossbeam-rs/crossbeam), MIT License
  * [unicode-ident 1.0.24](https://github.com/dtolnay/unicode-ident), Unicode License v3
  * [foldhash 0.2.0](https://github.com/orlp/foldhash), zlib License
  * [FLTK](https://www.fltk.org/), [FLTK license](https://www.fltk.org/doc-1.4/license.html)


