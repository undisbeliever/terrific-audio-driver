//! functions relating to the loading or saving of files

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::list_editor::{ListMessage, ListState};
use crate::song_tab::SongTab;
use crate::{Message, ProjectData, SoundEffectsData};

use compiler::data;
use compiler::data::{load_text_file_with_limit, Name, ProjectFile, Song, TextFile};
use compiler::sound_effects::{
    build_sound_effects_file, load_sound_effects_file, SoundEffectsFile,
};

extern crate fltk;
use fltk::dialog;

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

const SOUND_EFFECTS_FILTER: &str = "TXT Files\t*.txt";
const MML_SONG_FILTER: &str = "MML Files\t*.mml";

pub struct PfFileDialogResult {
    pub path: PathBuf,
    pub pf_path: PathBuf,
}

fn validate_pf_file_dialog_output(
    dialog: &dialog::NativeFileChooser,
    pd: &ProjectData,
    add_missing_extension: Option<&str>,
    choice_question: &str,
) -> Option<PfFileDialogResult> {
    let paths = dialog.filenames();

    if paths.len() != 1 {
        return None;
    }

    let mut path = paths.into_iter().next().unwrap();

    if let Some(e) = add_missing_extension {
        if path.extension().is_none() {
            path.set_extension(e);
        }
    }

    if !path.is_absolute() {
        dialog::message_title("Error");
        dialog::alert_default("path is not absolute");
        return None;
    }

    match path.strip_prefix(&pd.pf_parent_path) {
        Ok(p) => Some(PfFileDialogResult {
            pf_path: p.to_owned(),
            path,
        }),
        Err(_) => {
            dialog::message_title("Warning");
            let choice = dialog::choice2_default(
                &format!(
                    "{} is outside of the project file.\n{}",
                    path.display(),
                    choice_question
                ),
                "No",
                "Yes",
                "",
            );
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

fn pf_open_file_dialog(pd: &ProjectData, title: &str, filter: &str) -> Option<PfFileDialogResult> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(dialog::FileDialogOptions::UseFilterExt);
    let _ = dialog.set_directory(&pd.pf_parent_path);
    dialog.show();

    validate_pf_file_dialog_output(&dialog, pd, None, "Do you still want to open it?")
}

fn pf_save_file_dialog(
    pd: &ProjectData,
    path: Option<&Path>,
    title: &str,
    filter: &str,
    default_extension: &str,
) -> Option<PfFileDialogResult> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseSaveFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(
        dialog::FileDialogOptions::SaveAsConfirm | dialog::FileDialogOptions::UseFilterExt,
    );

    let _ = dialog.set_directory(&path.unwrap_or(&pd.pf_parent_path));

    dialog.show();

    validate_pf_file_dialog_output(
        &dialog,
        pd,
        Some(default_extension),
        "Do you still want to save to to this location?",
    )
}

pub fn load_project_file_or_show_error_message(path: &Path) -> Option<ProjectFile> {
    let path = match fs::canonicalize(path) {
        Ok(p) => p,
        Err(e) => {
            dialog::message_title("Error loading project file");
            dialog::alert_default(&e.to_string());
            return None;
        }
    };

    if !path.is_absolute() {
        dialog::message_title("Error loading project file");
        dialog::alert_default("path is not absolute");
        return None;
    }

    match data::load_project_file(&path) {
        Ok(pf) => Some(pf),
        Err(e) => {
            dialog::message_title("Error loading project file");
            dialog::alert_default(&e.to_string());
            None
        }
    }
}

pub fn open_sfx_file_dialog(pd: &ProjectData) -> Option<(PathBuf, SoundEffectsFile)> {
    let p = pf_open_file_dialog(pd, "Load sound effects file", SOUND_EFFECTS_FILTER);

    match p {
        Some(p) => match load_sfx_file(&p.path) {
            Some(sfx) => Some((p.pf_path, sfx)),
            None => None,
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

pub fn load_mml_file(full_path: &Path) -> Option<TextFile> {
    match load_text_file_with_limit(full_path) {
        Ok(f) => Some(f),
        Err(e) => {
            dialog::message_title("Error loading MML file");
            dialog::alert_default(&format!("{}", e));
            None
        }
    }
}

pub fn open_mml_file_dialog(pd: &ProjectData) -> Option<PfFileDialogResult> {
    pf_open_file_dialog(pd, "Add song to project", MML_SONG_FILTER)
}

pub fn add_song_to_pf_dialog(sender: &fltk::app::Sender<Message>, pd: &ProjectData) {
    if let Some(p) = open_mml_file_dialog(pd) {
        match pd
            .project_songs
            .list()
            .item_iter()
            .position(|s| s.source == p.pf_path)
        {
            Some(i) => sender.send(Message::EditProjectSongs(ListMessage::ItemSelected(i))),
            None => {
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

pub trait Serializer {
    const FILE_TYPE: &'static str;
    const FILE_EXTENSION: &'static str;
    const DIALOG_FILTER: Option<&'static str>;

    fn serialize(data: &Self) -> Result<Vec<u8>, String>;
}

impl Serializer for ProjectData {
    const FILE_TYPE: &'static str = "project";
    const FILE_EXTENSION: &'static str = "json";
    const DIALOG_FILTER: Option<&'static str> = None;

    fn serialize(pd: &ProjectData) -> Result<Vec<u8>, String> {
        let project = pd.to_project();

        compiler::data::serialize_project(&project).map_err(|e| e.to_string())
    }
}

impl Serializer for SoundEffectsData {
    const FILE_TYPE: &'static str = "sound effects";
    const FILE_EXTENSION: &'static str = "txt";
    const DIALOG_FILTER: Option<&'static str> = Some(SOUND_EFFECTS_FILTER);

    fn serialize(data: &SoundEffectsData) -> Result<Vec<u8>, String> {
        Ok(build_sound_effects_file(data.header(), data.sound_effects_iter()).into())
    }
}

impl Serializer for SongTab {
    const FILE_TYPE: &'static str = "MML song";
    const FILE_EXTENSION: &'static str = "mml";
    const DIALOG_FILTER: Option<&'static str> = Some(MML_SONG_FILTER);

    fn serialize(song_tab: &SongTab) -> Result<Vec<u8>, String> {
        Ok(song_tab.contents().into())
    }
}

pub fn save_data<S>(data: &S, path: &Path) -> bool
where
    S: Serializer,
{
    match Serializer::serialize(data) {
        Ok(contents) => write_file_show_dialog_on_error(path, S::FILE_TYPE, &contents),
        Err(e) => {
            dialog::message_title(&format!("Error saving {}", S::FILE_TYPE));
            dialog::alert_default(&format!("Error serializing {}\n\n{}", path.display(), e));
            false
        }
    }
}

pub fn save_data_with_save_as_dialog<S>(
    data: &S,
    path: Option<&Path>,
    pd: &ProjectData,
) -> Option<PfFileDialogResult>
where
    S: Serializer,
{
    let filter = match S::DIALOG_FILTER {
        Some(s) => s,
        None => return None,
    };
    let dialog_title = format!("Save {} as", S::FILE_TYPE);

    let p = match pf_save_file_dialog(pd, path, &dialog_title, filter, S::FILE_EXTENSION) {
        Some(p) => p,
        None => return None,
    };

    if save_data(data, &p.path) {
        Some(p)
    } else {
        None
    }
}

/// Writes contents to a new file and errors if the file already exists::
#[allow(dead_code)]
fn write_to_new_file(path: &Path, contents: &[u8]) -> std::io::Result<()> {
    assert!(path.is_absolute());

    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)?;
    file.write_all(contents)?;

    Ok(())
}

/// Writes contents to an existing file
fn write_file_show_dialog_on_error(path: &Path, file_type: &str, contents: &[u8]) -> bool {
    assert!(path.is_absolute());

    match fs::write(path, contents) {
        Ok(()) => true,
        Err(e) => {
            dialog::message_title(&format!("Error saving {}", file_type));
            dialog::alert_default(&format!("Error writing to {}\n\n{}", path.display(), e));
            false
        }
    }
}
