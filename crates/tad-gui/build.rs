//! Audio Driver GUI uild script
//!
//! Compiles `.md` files into HTML 2.0 (ish) files for syntax help sidebar.

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::env;
use std::fs;
use std::path::PathBuf;

extern crate markdown;

const MARKDOWN_FILES: &[&str] = &[
    "../../docs/bytecode-assembly-syntax.md",
    "../../docs/mml-syntax.md",
];

fn convert_markdown(input: &str) -> String {
    let html = markdown::to_html_with_options(
        input,
        &markdown::Options {
            compile: markdown::CompileOptions {
                allow_dangerous_html: true,
                ..markdown::CompileOptions::default()
            },
            ..markdown::Options::default()
        },
    )
    .unwrap();

    // HACK: Fix the HTML
    //
    // FLTK only supports HTML 2.0

    // Fix bullet point positions.
    let html = html
        .replace("<li>\n<p>", "<li>")
        .replace("</p>\n</li>", "</li>")
        .replace("</p>\n<ul>", "<ul>");

    // Change code to green monospace text
    // (make it easier to see)
    let html = html
        .replace("<code>", "<font face=\"courier\" color=\"#080\">")
        .replace("</code>", "</font>");

    #[allow(clippy::let_and_return)]
    html
}

fn main() {
    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    if !out_dir.is_dir() {
        panic!("invalid OUT_DIR");
    }

    for f in MARKDOWN_FILES {
        let path: PathBuf = f.into();
        let out_path = out_dir
            .join(path.file_stem().unwrap())
            .with_extension("html");

        let input = fs::read_to_string(path).unwrap();
        let html = convert_markdown(&input);

        fs::write(&out_path, html).unwrap();
    }

    println!("cargo:rerun-if-changed=build.rs");
    for f in MARKDOWN_FILES {
        println!("cargo:rerun-if-changed={}", f);
    }
}
