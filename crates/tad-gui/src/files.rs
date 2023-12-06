//! functions relating to the loading or saving of files

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::ToCompiler;
use crate::list_editor::{ListMessage, ListState};
use crate::song_tab::SongTab;
use crate::tabs::{FileType, TabManager};
use crate::{GuiMessage, ProjectData, SoundEffectsData};

use compiler::data;
use compiler::data::{load_text_file_with_limit, Name, ProjectFile, Song, TextFile};
use compiler::path::{ParentPathBuf, SourcePathBuf, SourcePathResult};
use compiler::sound_effects::{
    build_sound_effects_file, load_sound_effects_file, SoundEffectsFile,
};

extern crate fltk;
use fltk::dialog;

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::mpsc;

const PROJECT_FILTER: &str = "Project Files\t*.terrificaudio";
const SOUND_EFFECTS_FILTER: &str = "TXT Files\t*.txt";
const MML_SONG_FILTER: &str = "MML Files\t*.mml";
const SPC_FILTER: &str = "SPC Files\t*.spc";
const SAMPLE_FILTERS: &str = concat![
    "WAV and BRR samples\t*.{wav,brr}\n",
    "WAV samples\t*.wav\n",
    "BRR samples\t*.brr",
];

fn save_file_dialog(title: &str, filter: &str, default_extension: &str) -> Option<PathBuf> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseSaveFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(
        dialog::FileDialogOptions::SaveAsConfirm | dialog::FileDialogOptions::UseFilterExt,
    );

    dialog.show();

    let paths = dialog.filenames();
    if paths.len() != 1 {
        return None;
    }

    let mut path = paths.into_iter().next().unwrap();

    if path.extension().is_none() {
        path.set_extension(default_extension);
    }

    if path.is_absolute() {
        Some(path)
    } else {
        dialog::message_title("Error");
        dialog::alert_default("path is not absolute");
        None
    }
}

pub struct PfFileDialogResult {
    pub full_path: PathBuf,
    pub source_path: SourcePathBuf,
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

    #[cfg(windows)]
    {
        // Must canonicalize path (to match `pd.pf_parent_path`) on Windows to prevent a
        // "paths contain different absolute prefixes" SourcePathResult::Err.
        //
        // This error is caused by the canonicalized `pf_parent_path` using the `\\?\C:\` prefix,
        // while the path returned by the fltk dialog uses the `C:\` prefix.
        //
        // Not canonicalizing on non-windows systems.

        // I cannot canonicalize the path as it may not exist.
        // Canonicalizing the parent directory instead.
        let (parent, file_name) = match (path.parent(), path.file_name()) {
            (Some(p), Some(f)) => (p, f),
            _ => {
                dialog::message_title("Error");
                dialog::alert_default(
                    "Cannot canonicalize path: cannot extract filename from path",
                );
                return None;
            }
        };
        path = match parent.canonicalize() {
            Ok(p) => p.join(file_name),
            Err(e) => {
                dialog::message_title("Error");
                dialog::alert_default(&format!("Cannot canonicalize path: {}", e));
                return None;
            }
        };
    }

    match pd.pf_parent_path.create_source_path(&path) {
        SourcePathResult::InsideProject(source_path) => Some(PfFileDialogResult {
            source_path,
            full_path: path,
        }),
        SourcePathResult::OutsideProject(source_path) => {
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
                Some(1) => Some(PfFileDialogResult {
                    source_path,
                    full_path: path,
                }),
                _ => None,
            }
        }
        SourcePathResult::Err(e) => {
            dialog::message_title("Error loading file");
            dialog::alert_default(&e.to_string());
            None
        }
    }
}

fn pf_open_file_dialog(
    pd: &ProjectData,
    title: &str,
    filter: &str,
    old_source_path: Option<&SourcePathBuf>,
) -> Option<PfFileDialogResult> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(dialog::FileDialogOptions::UseFilterExt);

    match old_source_path {
        Some(p) => {
            let p = p.to_path(&pd.pf_parent_path);
            let _ = dialog.set_directory(&p);
        }
        None => {
            let _ = dialog.set_directory(pd.pf_parent_path.as_path());
        }
    };

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

    let _ = dialog.set_directory(&path.unwrap_or(pd.pf_parent_path.as_path()));

    dialog.show();

    validate_pf_file_dialog_output(
        &dialog,
        pd,
        Some(default_extension),
        "Do you still want to save to to this location?",
    )
}

fn open_file_dialog(title: &str, filter: &str) -> Option<PathBuf> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(dialog::FileDialogOptions::UseFilterExt);

    dialog.show();

    let paths = dialog.filenames();

    if paths.len() != 1 {
        return None;
    }

    let path = paths.into_iter().next().unwrap();

    if !path.is_absolute() {
        dialog::message_title("Error");
        dialog::alert_default("path is not absolute");
        return None;
    }

    Some(path)
}

pub fn open_project_dialog() -> Option<ProjectFile> {
    match open_file_dialog("Load Project", PROJECT_FILTER) {
        Some(path) => load_project_file_or_show_error_message(&path),
        None => None,
    }
}

pub fn new_project_dialog() -> Option<ProjectFile> {
    let path = match save_file_dialog("New Project", PROJECT_FILTER, data::PROJECT_FILE_EXTENSION) {
        Some(p) => p,
        None => return None,
    };

    if path.try_exists().ok() == Some(true) {
        dialog::message_title("Cannot create a new project");
        dialog::alert_default("The project file already exists");
        return load_project_file_or_show_error_message(&path);
    }

    let project = data::Project::default();
    let contents = match data::serialize_project(&project) {
        Ok(c) => c,
        Err(e) => {
            dialog::message_title("Cannot serialize new project");
            dialog::alert_default(&e.to_string());
            return None;
        }
    };

    match write_to_new_file(&path, &contents.to_vec()) {
        Ok(()) => load_project_file_or_show_error_message(&path),
        Err(e) => {
            dialog::message_title("Cannot save new project");
            dialog::alert_default(&e.to_string());
            None
        }
    }
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

pub fn open_sfx_file_dialog(pd: &ProjectData) -> Option<(SourcePathBuf, SoundEffectsFile)> {
    let p = pf_open_file_dialog(pd, "Load sound effects file", SOUND_EFFECTS_FILTER, None);

    match p {
        Some(p) => match load_sfx_file(&p.source_path, &pd.pf_parent_path) {
            Some(sfx) => Some((p.source_path, sfx)),
            None => None,
        },
        None => None,
    }
}

pub fn load_pf_sfx_file(pd: &ProjectData) -> Option<SoundEffectsFile> {
    match &pd.sound_effects_file {
        Some(source) => load_sfx_file(source, &pd.pf_parent_path),
        None => None,
    }
}

fn load_sfx_file(
    source_path: &SourcePathBuf,
    parent_path: &ParentPathBuf,
) -> Option<SoundEffectsFile> {
    match load_sound_effects_file(source_path, parent_path) {
        Ok(sfx_file) => Some(sfx_file),
        Err(e) => {
            dialog::message_title("Error loading sound effects file");
            dialog::alert_default(&format!("{}", e));
            None
        }
    }
}

pub fn load_mml_file(source: &SourcePathBuf, parent_path: &ParentPathBuf) -> Option<TextFile> {
    match load_text_file_with_limit(source, parent_path) {
        Ok(f) => Some(f),
        Err(e) => {
            dialog::message_title("Error loading MML file");
            dialog::alert_default(&format!("{}", e));
            None
        }
    }
}

pub fn open_mml_file_dialog(pd: &ProjectData) -> Option<PfFileDialogResult> {
    pf_open_file_dialog(pd, "Add song to project", MML_SONG_FILTER, None)
}

pub fn song_name_from_path(source_path: &SourcePathBuf) -> Name {
    match source_path.file_stem_string() {
        Some(s) => Name::new_lossy(s.to_owned()),
        None => Name::try_new("song".to_owned()).unwrap(),
    }
}

pub fn add_song_to_pf_dialog(
    sender: &fltk::app::Sender<GuiMessage>,
    pd: &ProjectData,
    tab_manager: &TabManager,
) {
    if let Some(p) = open_mml_file_dialog(pd) {
        match pd
            .project_songs
            .list()
            .item_iter()
            .position(|s| s.source == p.source_path)
        {
            Some(i) => sender.send(GuiMessage::EditProjectSongs(ListMessage::ItemSelected(i))),
            None => match tab_manager.find_file(&p.full_path) {
                Some(FileType::Song(id)) => {
                    // The MML file is already open
                    sender.send(GuiMessage::EditProjectSongs(ListMessage::AddWithItemId(
                        id,
                        data::Song {
                            name: song_name_from_path(&p.source_path),
                            source: p.source_path,
                        },
                    )));
                }
                _ => sender.send(GuiMessage::EditProjectSongs(ListMessage::Add(Song {
                    name: song_name_from_path(&p.source_path),
                    source: p.source_path,
                }))),
            },
        }
    }
}

pub fn open_instrument_sample_dialog(
    sender: &fltk::app::Sender<GuiMessage>,
    compiler_sender: &mpsc::Sender<ToCompiler>,
    pd: &ProjectData,
    index: usize,
) {
    let inst = match pd.instruments.list().get(index) {
        Some(inst) => inst,
        None => return,
    };

    if let Some(p) = pf_open_file_dialog(pd, "Select sample", SAMPLE_FILTERS, Some(&inst.source)) {
        let new_source = p.source_path;

        // Remove the previous and new files from sample cache to ensure any file changes are loaded
        let _ = compiler_sender.send(ToCompiler::RemoveFileFromSampleCache(inst.source.clone()));
        let _ = compiler_sender.send(ToCompiler::RemoveFileFromSampleCache(new_source.clone()));

        if inst.source != new_source {
            let new_inst = data::Instrument {
                source: new_source.clone(),
                ..inst.clone()
            };
            sender.send(GuiMessage::Instrument(ListMessage::ItemEdited(
                index, new_inst,
            )));
        }

        // The sample might be used by more than one instrument.
        // Recompile all instruments that use the sample file.
        // Will also recompile this instrument if `source` was unchanged.
        let _ = compiler_sender.send(ToCompiler::RecompileInstrumentsUsingSample(new_source));
    }
}

pub fn save_spc_file_dialog(name: String, data: Vec<u8>) {
    let path = save_file_dialog(
        &format!("Export {} to an .spc file", name),
        SPC_FILTER,
        "spc",
    );

    if let Some(path) = path {
        write_file_show_dialog_on_error(&path, "SPC file", &data);
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
    const FILE_EXTENSION: &'static str = data::PROJECT_FILE_EXTENSION;
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

    if save_data(data, &p.full_path) {
        Some(p)
    } else {
        None
    }
}

/// Writes contents to a new file and errors if the file already exists::
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
