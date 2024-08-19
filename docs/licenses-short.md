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

  * [ryu 1.0.18](https://github.com/dtolnay/ryu), Apache License 2.0
  * [codespan-reporting 0.11.1](https://github.com/brendanzab/codespan), Apache License 2.0
  * [windows-sys 0.52.0](https://github.com/microsoft/windows-rs), MIT License
  * [windows-targets 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_aarch64_gnullvm 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_aarch64_msvc 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_i686_gnu 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_i686_gnullvm 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_i686_msvc 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_gnu 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_gnullvm 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [windows_x86_64_msvc 0.52.6](https://github.com/microsoft/windows-rs), MIT License
  * [markdown 1.0.0-alpha.20](https://github.com/wooorm/markdown-rs), MIT License
  * [lazy_static 1.5.0](https://github.com/rust-lang-nursery/lazy-static.rs), MIT License
  * [cc 1.1.13](https://github.com/rust-lang/cc-rs), MIT License
  * [cfg-if 1.0.0](https://github.com/alexcrichton/cfg-if), MIT License
  * [cmake 0.1.51](https://github.com/rust-lang/cmake-rs), MIT License
  * [bitflags 1.3.2](https://github.com/bitflags/bitflags), MIT License
  * [bitflags 2.6.0](https://github.com/bitflags/bitflags), MIT License
  * [num-complex 0.4.6](https://github.com/rust-num/num-complex), MIT License
  * [num-traits 0.2.19](https://github.com/rust-num/num-traits), MIT License
  * [regex-automata 0.4.7](https://github.com/rust-lang/regex/tree/master/regex-automata), MIT License
  * [regex-syntax 0.8.4](https://github.com/rust-lang/regex/tree/master/regex-syntax), MIT License
  * [regex 1.10.6](https://github.com/rust-lang/regex), MIT License
  * [float-cmp 0.9.0](https://github.com/mikedilger/float-cmp), MIT License
  * [libc 0.2.158](https://github.com/rust-lang/libc), MIT License
  * [anstyle-wincon 3.0.4](https://github.com/rust-cli/anstyle.git), MIT License
  * [heck 0.5.0](https://github.com/withoutboats/heck), MIT License
  * [unicode-id 0.3.4](https://github.com/Boshen/unicode-id), MIT License
  * [unicode-width 0.1.13](https://github.com/unicode-rs/unicode-width), MIT License
  * [utf8parse 0.2.2](https://github.com/alacritty/vte), MIT License
  * [anstyle-parse 0.2.5](https://github.com/rust-cli/anstyle.git), MIT License
  * [version-compare 0.1.1](https://gitlab.com/timvisee/version-compare), MIT License
  * [libm 0.2.8](https://github.com/rust-lang/libm), MIT License
  * [autocfg 1.3.0](https://github.com/cuviper/autocfg), MIT License
  * [ttf-parser 0.24.1](https://github.com/RazrFalcon/ttf-parser), MIT License
  * [anstyle 1.0.8](https://github.com/rust-cli/anstyle.git), MIT License
  * [anstream 0.6.15](https://github.com/rust-cli/anstyle.git), MIT License
  * [anstyle-query 1.1.1](https://github.com/rust-cli/anstyle), MIT License
  * [clap 4.5.16](https://github.com/clap-rs/clap), MIT License
  * [colorchoice 1.0.2](https://github.com/rust-cli/anstyle), MIT License
  * [is_terminal_polyfill 1.70.1](https://github.com/polyfill-rs/is_terminal_polyfill), MIT License
  * [unicode-id 0.3.4](https://github.com/Boshen/unicode-id), MIT License
  * [unicode-width 0.1.13](https://github.com/unicode-rs/unicode-width), MIT License
  * [static_assertions 1.1.0](https://github.com/nvzqz/static-assertions-rs), MIT License
  * [fltk-sys 1.4.33](https://github.com/fltk-rs/fltk-rs), MIT License
  * [microfft 0.5.1](https://gitlab.com/teskje/microfft-rs), MIT License
  * [spectrum-analyzer 1.5.0](https://github.com/phip1611/spectrum-analyzer), MIT License
  * [relative-path 1.9.3](https://github.com/udoprog/relative-path), MIT License
  * [sdl2-sys 0.37.0](https://github.com/rust-sdl2/rust-sdl2), MIT License
  * [fltk-sys 1.4.33](https://github.com/fltk-rs/fltk-rs), MIT License
  * [fltk 1.4.33](https://github.com/fltk-rs/fltk-rs), MIT License
  * [fltk-sys 1.4.33](https://github.com/fltk-rs/fltk-rs), MIT License
  * [cxx-build 1.0.125](https://github.com/dtolnay/cxx), MIT License
  * [cxx 1.0.125](https://github.com/dtolnay/cxx), MIT License
  * [cxxbridge-flags 1.0.125](https://github.com/dtolnay/cxx), MIT License
  * [cxxbridge-macro 1.0.125](https://github.com/dtolnay/cxx), MIT License
  * [itoa 1.0.11](https://github.com/dtolnay/itoa), MIT License
  * [link-cplusplus 1.0.9](https://github.com/dtolnay/link-cplusplus), MIT License
  * [once_cell 1.19.0](https://github.com/matklad/once_cell), MIT License
  * [paste 1.0.15](https://github.com/dtolnay/paste), MIT License
  * [proc-macro2 1.0.86](https://github.com/dtolnay/proc-macro2), MIT License
  * [quote 1.0.36](https://github.com/dtolnay/quote), MIT License
  * [scratch 1.0.7](https://github.com/dtolnay/scratch), MIT License
  * [serde 1.0.208](https://github.com/serde-rs/serde), MIT License
  * [serde_derive 1.0.208](https://github.com/serde-rs/serde), MIT License
  * [serde_json 1.0.125](https://github.com/serde-rs/json), MIT License
  * [syn 2.0.75](https://github.com/dtolnay/syn), MIT License
  * [unicode-ident 1.0.12](https://github.com/dtolnay/unicode-ident), MIT License
  * [sdl2 0.37.0](https://github.com/Rust-SDL2/rust-sdl2), MIT License
  * [aho-corasick 1.1.3](https://github.com/BurntSushi/aho-corasick), MIT License
  * [memchr 2.7.4](https://github.com/BurntSushi/memchr), MIT License
  * [termcolor 1.4.1](https://github.com/BurntSushi/termcolor), MIT License
  * [strsim 0.11.1](https://github.com/rapidfuzz/strsim-rs), MIT License
  * [shlex 1.3.0](https://github.com/comex/rust-shlex), MIT License
  * [clap_builder 4.5.15](https://github.com/clap-rs/clap), MIT License
  * [clap_derive 4.5.13](https://github.com/clap-rs/clap), MIT License
  * [clap_lex 0.7.2](https://github.com/clap-rs/clap), MIT License
  * [winapi-util 0.1.9](https://github.com/BurntSushi/winapi-util), MIT License
  * [crossbeam-channel 0.5.13](https://github.com/crossbeam-rs/crossbeam), MIT License
  * [crossbeam-utils 0.8.20](https://github.com/crossbeam-rs/crossbeam), MIT License
  * [aho-corasick 1.1.3](https://github.com/BurntSushi/aho-corasick), MIT License
  * [memchr 2.7.4](https://github.com/BurntSushi/memchr), MIT License
  * [termcolor 1.4.1](https://github.com/BurntSushi/termcolor), MIT License
  * [winapi-util 0.1.9](https://github.com/BurntSushi/winapi-util), MIT License
  * [unicode-ident 1.0.12](https://github.com/dtolnay/unicode-ident), Unicode License Agreement - Data Files and Software (2016)
  * [sdl2-sys 0.37.0](https://github.com/rust-sdl2/rust-sdl2), zlib License
  * [sdl2-sys 0.37.0](https://github.com/rust-sdl2/rust-sdl2), zlib License
  * [FLTK](https://www.fltk.org/), [FLTK license](https://www.fltk.org/doc-1.4/license.html)
  * [SDL2](https://libsdl.org/), [zlib license](https://www.libsdl.org/license.php)


