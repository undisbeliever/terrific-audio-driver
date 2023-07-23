//! functions relating to the loading or saving of files

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::list_editor::ListMessage;
use crate::{Message, ProjectData};

use compiler::data::{Name, Song};
use compiler::sound_effects::{load_sound_effects_file, SoundEffectsFile};

extern crate fltk;
use fltk::dialog;

use std::io::Write;
use std::path::{Path, PathBuf};

struct PfFileDialogResult {
    pub path: PathBuf,
    pub pf_path: PathBuf,
}

fn pf_file_dialog(
    pd: &ProjectData,
    title: &str,
    filter: &str,
    default_extension: &str,
) -> Option<PfFileDialogResult> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseSaveFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(dialog::FileDialogOptions::UseFilterExt);
    let _ = dialog.set_directory(&pd.pf_parent_path);
    dialog.show();

    let paths = dialog.filenames();

    if paths.len() != 1 {
        return None;
    }

    let mut path = paths.into_iter().next().unwrap();

    if path.extension().is_none() {
        path.set_extension(default_extension);
    }

    match path.strip_prefix(&pd.pf_parent_path) {
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

pub fn open_sfx_file_dialog(pd: &ProjectData) -> Option<(PathBuf, Option<SoundEffectsFile>)> {
    let p = pf_file_dialog(pd, "Load sound effects file", "TXT Files\t*.txt", "txt");

    match p {
        Some(p) => match p.path.try_exists() {
            Ok(true) => Some((p.pf_path, load_sfx_file(&p.path))),
            Ok(false) => match write_to_new_file(&p.path, &[]) {
                Ok(()) => Some((p.pf_path, load_sfx_file(&p.path))),
                Err(e) => {
                    dialog::message_title("Error writing sound effects file");
                    dialog::alert_default(&format!("{}", e));
                    None
                }
            },
            Err(e) => {
                dialog::message_title("Error loading sound effects file");
                dialog::alert_default(&format!("{}", e));
                None
            }
        },
        None => None,
    }
}

pub fn load_pf_sfx_file(pd: &ProjectData) -> Option<SoundEffectsFile> {
    match &pd.sound_effects_file {
        Some(path) => load_sfx_file(&pd.pf_parent_path.join(path)),
        None => None,
    }
}

fn load_sfx_file(path: &Path) -> Option<SoundEffectsFile> {
    match load_sound_effects_file(path) {
        Ok(sfx_file) => Some(sfx_file),
        Err(e) => {
            dialog::message_title("Error loading sound effects file");
            dialog::alert_default(&format!("{}", e));
            None
        }
    }
}

pub fn add_song_to_pf_dialog(sender: &fltk::app::Sender<Message>, pd: &ProjectData) {
    if let Some(p) = pf_file_dialog(pd, "Add song", "MML Files\t*.mml", "mml") {
        match pd
            .project_songs
            .list()
            .iter()
            .position(|s| s.source == p.pf_path)
        {
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

/// Writes contents to a new file and errors if the file already exists::
fn write_to_new_file(path: &Path, contents: &[u8]) -> std::io::Result<()> {
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)?;
    file.write_all(contents)?;

    Ok(())
}
