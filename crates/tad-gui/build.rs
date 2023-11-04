//! Audio Driver GUI uild script
//!
//! Compiles `.md` files into HTML 2.0 (ish) files for syntax help sidebar.

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

extern crate markdown;
extern crate regex;

use regex::Captures;
use regex::Regex;

const MARKDOWN_FILES: &[&str] = &[
    "../../docs/bytecode-assembly-syntax.md",
    "../../docs/mml-syntax.md",
    "../../docs/licenses-short.md",
];

fn convert_markdown(input: &str, name: &str) -> String {
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

    // Add padding before and after <pre> tags
    let html = html
        .replace("<pre>", "<p></p>\n<pre>")
        .replace("</pre>", "</pre><p></p>");

    // Extract URLs from <a> links
    let html = Regex::new(r#"<a href="([^"]+)">([^<]+)</a>"#)
        .unwrap()
        .replace_all(&html, |caps: &Captures| {
            const INVALID_CHARS: [char; 3] = ['<', '>', '"'];

            let link = &caps[1];
            let label = &caps[2];

            if link.contains(INVALID_CHARS) || label.contains(INVALID_CHARS) {
                panic!("{name}: has invalid characters in <a> tag {:?}", caps);
            }

            if link != label {
                format!(r##"{} <font color="#c60">{}</font>"##, label, link)
            } else {
                format!(r##"<font color="#c60">{}</font>"##, link)
            }
        })
        .to_string();

    validate_no_links_in_html(&html, name);

    html
}

// Confirms there are no <img> and <a> links in the HTML.
//
// fltk Fl_Help_View will do network requests for <img> or <a> tags and I do not want the GUI to
// access the Internet.
fn validate_no_links_in_html(html: &str, name: &str) {
    let html = html.to_ascii_lowercase();

    if html.contains("<img") {
        panic!("{name} cannot contain <img> tags");
    }
    if html.contains("<a") {
        panic!("{name} cannot contain <a> tags");
    }

    // Double check there are no `src/href` attributes, just to be safe
    // (may cause a false positive)
    if html.contains("src=\"") {
        panic!("{name} cannot contain src attributes");
    }
    if html.contains("href=\"") {
        panic!("{name} cannot contain href attributes");
    }
}

fn main() {
    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    if !out_dir.is_dir() {
        panic!("invalid OUT_DIR");
    }

    for f in MARKDOWN_FILES {
        let path: PathBuf = f.into();
        let html_filename = Path::new(path.file_stem().unwrap()).with_extension("html");
        let html_filename = html_filename.to_str().unwrap();

        let out_path = out_dir.join(html_filename);

        let input = fs::read_to_string(path).unwrap();
        let html = convert_markdown(&input, html_filename);
        fs::write(&out_path, html).unwrap();
    }

    println!("cargo:rerun-if-changed=build.rs");
    for f in MARKDOWN_FILES {
        println!("cargo:rerun-if-changed={}", f);
    }
}
