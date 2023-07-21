//! functions relating to the loading or saving of files

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::list_editor::ListMessage;
use crate::Message;

use compiler::data::{Name, ProjectFile, Song};

extern crate fltk;
use fltk::dialog;

use std::path::PathBuf;

struct PfFileDialogResult {
    pub path: PathBuf,
    pub pf_path: PathBuf,
}

fn pf_file_dialog(
    pf: &ProjectFile,
    title: &str,
    filter: &str,
    default_extension: &str,
) -> Option<PfFileDialogResult> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseSaveFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(dialog::FileDialogOptions::UseFilterExt);
    let _ = dialog.set_directory(&pf.parent_path);
    dialog.show();

    let paths = dialog.filenames();

    if paths.len() != 1 {
        return None;
    }

    let mut path = paths.into_iter().next().unwrap();

    if path.extension().is_none() {
        path.set_extension(default_extension);
    }

    match path.strip_prefix(&pf.parent_path) {
        Ok(p) => Some(PfFileDialogResult {
            pf_path: p.to_owned(),
            path,
        }),
        Err(_) => {
            dialog::message_title("Warning");
            let choice = dialog::choice2_default(
                    &format!("{} is outside of the project file.\nDo you still want to add it to the project?", path.display()),
                    "No", "Yes", "");
            match choice {
                // ::TODO make pf_path a relative path (if possible)::
                Some(1) => Some(PfFileDialogResult {
                    pf_path: path.clone(),
                    path,
                }),
                _ => None,
            }
        }
    }
}

pub fn add_song_to_pf_dialog(sender: &fltk::app::Sender<Message>, pf: &ProjectFile) {
    if let Some(p) = pf_file_dialog(pf, "Add song", "MML Files\t*.mml", "mml") {
        match pf.contents.songs.iter().position(|s| s.source == p.pf_path) {
            Some(i) => sender.send(Message::EditProjectSongs(ListMessage::ItemSelected(i))),
            None => {
                // ::TODO Create a blank file if it does not exist::
                // ::TODO Open a new Song tab::

                let name = match p.pf_path.file_stem() {
                    Some(s) => Name::new_lossy(s.to_string_lossy().to_string()),
                    None => Name::try_new("song".to_owned()).unwrap(),
                };
                sender.send(Message::EditProjectSongs(ListMessage::Add(Song {
                    name,
                    source: p.pf_path,
                })))
            }
        }
    }
}
