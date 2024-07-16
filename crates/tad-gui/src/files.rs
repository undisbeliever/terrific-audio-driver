//! functions relating to the loading or saving of files

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, ToCompiler};
use crate::list_editor::ListMessage;
use crate::song_tab::SongTab;
use crate::tabs::{FileType, TabManager};
use crate::{GuiMessage, ProjectData, SoundEffectsData};

use compiler::data;
use compiler::data::{load_text_file_with_limit, Name, ProjectFile, Song, TextFile, MAX_FILE_SIZE};
use compiler::path::{ParentPathBuf, SourcePathBuf, SourcePathResult};
use compiler::sfx_file::{build_sound_effects_file, load_sound_effects_file, SoundEffectsFile};

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

fn try_show_native_dialog(mut dialog: dialog::NativeFileChooser) -> Option<PathBuf> {
    match dialog.try_show() {
        Ok(a) => match a {
            dialog::NativeFileChooserAction::Success => {
                let paths = dialog.filenames();
                if paths.len() == 1 {
                    paths.into_iter().next()
                } else {
                    None
                }
            }
            dialog::FileDialogAction::Cancelled => None,
        },
        Err(e) => {
            dialog::message_title("Error");
            dialog::alert_default(&format!("Unable to create a file dialog.\n{e}"));
            None
        }
    }
}

fn save_file_dialog(title: &str, filter: &str, default_extension: &str) -> Option<PathBuf> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseSaveFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(
        dialog::FileDialogOptions::SaveAsConfirm | dialog::FileDialogOptions::UseFilterExt,
    );

    let mut path = try_show_native_dialog(dialog)?;

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

enum PfFileDialogState {
    None,
    Error(String),
    InsideProject(PathBuf, SourcePathBuf),
    OutsideProject(PathBuf, SourcePathBuf),
    RelativePathErr(PathBuf, String),
}

fn show_and_validate_pf_file_dialog_output(
    dialog: dialog::NativeFileChooser,
    pd: &ProjectData,
    add_missing_extension: Option<&str>,
) -> PfFileDialogState {
    let mut path = match try_show_native_dialog(dialog) {
        Some(p) => p,
        None => return PfFileDialogState::None,
    };

    if let Some(e) = add_missing_extension {
        if path.extension().is_none() {
            path.set_extension(e);
        }
    }

    if !path.is_absolute() {
        return PfFileDialogState::Error("path is not absolute".to_owned());
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
                return PfFileDialogState::Error(
                    "Cannot canonicalize path: cannot extract filename from path".to_owned(),
                );
            }
        };
        path = match parent.canonicalize() {
            Ok(p) => p.join(file_name),
            Err(e) => {
                return PfFileDialogState::Error(format!("Cannot canonicalize path: {}", e));
            }
        };
    }

    match pd.pf_parent_path.create_source_path(&path) {
        SourcePathResult::InsideProject(source_path) => {
            PfFileDialogState::InsideProject(path, source_path)
        }
        SourcePathResult::OutsideProject(source_path) => {
            PfFileDialogState::OutsideProject(path, source_path)
        }
        SourcePathResult::Err(e) => PfFileDialogState::RelativePathErr(path, e.to_string()),
    }
}

pub struct PfFileDialogResult {
    pub full_path: PathBuf,
    pub source_path: SourcePathBuf,
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

    match show_and_validate_pf_file_dialog_output(dialog, pd, None) {
        PfFileDialogState::None => None,
        PfFileDialogState::Error(msg) => {
            dialog::message_title("Error");
            dialog::alert_default(&msg);
            None
        }
        PfFileDialogState::InsideProject(full_path, source_path) => Some(PfFileDialogResult {
            full_path,
            source_path,
        }),
        PfFileDialogState::OutsideProject(full_path, source_path) => {
            let file_contents = load_binary_file_with_limit_show_dialog_on_error(&full_path)?;

            dialog::message_title("Warning");
            let choice = dialog::choice2_default(
                &format!(
                    concat![
                        "{} is outside of the project directory.\n",
                        "Do you still want to open it?",
                    ],
                    full_path.display(),
                ),
                "Copy file to project",
                "No", // default
                "Yes",
            );
            match choice {
                Some(0) => copy_file_to_project_dialog(pd, file_contents, &full_path, filter),
                Some(1) => None,
                Some(2) => Some(PfFileDialogResult {
                    source_path,
                    full_path,
                }),
                _ => None,
            }
        }
        PfFileDialogState::RelativePathErr(full_path, error_message) => {
            // `full_path` might be on a different drive to the project file.
            // This will still allow the file to be copied to the project directory.

            let file_contents = load_binary_file_with_limit_show_dialog_on_error(&full_path)?;

            dialog::message_title("Warning");
            let choice = dialog::choice2_default(
                &format!(
                    concat![
                        "{} is outside of the project directory.\n",
                        "The file cannot be converted to a relative path: {}\n",
                        "\n",
                        "Do you want to copy the file into the project directory?"
                    ],
                    full_path.display(),
                    error_message,
                ),
                // Same order as `PfFileDialogState::OutsideProject` dialog
                "Copy file to project",
                "No", // default
                "",
            );
            match choice {
                Some(0) => copy_file_to_project_dialog(pd, file_contents, &full_path, filter),
                _ => None,
            }
        }
    }
}

/// Opens a Save-As dialog that allows the user to copy the file to the project directory.
///
/// To prevent accidental overrides, this dialog will forbid overriding existing files.
///
/// NOTE: This function takes a `file_contents` parameter to ensure the "Cannot load file"
/// error message occurs before the "file is outsode of the project directory" dialog choice.
fn copy_file_to_project_dialog(
    pd: &ProjectData,
    file_contents: Vec<u8>,
    copied_file_path: &Path,
    filter: &str,
) -> Option<PfFileDialogResult> {
    debug_assert!(file_contents.len() <= MAX_FILE_SIZE as usize);

    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseSaveFile);
    dialog.set_title("Copy file to project");
    dialog.set_filter(filter);
    dialog.set_option(
        dialog::FileDialogOptions::SaveAsConfirm | dialog::FileDialogOptions::UseFilterExt,
    );

    let extension: Option<String> = copied_file_path
        .extension()
        .and_then(|e| e.to_str())
        .map(|s| s.to_owned());

    let _ = dialog.set_directory(pd.pf_parent_path.as_path());
    if let Some(f) = copied_file_path.file_name() {
        if let Some(f) = f.to_str() {
            dialog.set_preset_file(f);
        }
    }

    match show_and_validate_pf_file_dialog_output(dialog, pd, extension.as_deref()) {
        PfFileDialogState::None => None,
        PfFileDialogState::Error(msg) | PfFileDialogState::RelativePathErr(_, msg) => {
            dialog::message_title("Error");
            dialog::alert_default(&msg);
            None
        }
        PfFileDialogState::OutsideProject(_, _) => {
            dialog::message_title("Error");
            dialog::alert_default("Path is outside of the project");
            None
        }
        PfFileDialogState::InsideProject(full_path, source_path) => {
            // I am delibratly forbiding writing to existing files.
            match write_to_new_file(&full_path, &file_contents) {
                Ok(()) => Some(PfFileDialogResult {
                    full_path,
                    source_path,
                }),
                Err(e) => {
                    dialog::message_title("Error copying file");
                    dialog::alert_default(&format!(
                        "Error writing to {}\n\n{}",
                        full_path.display(),
                        e
                    ));
                    None
                }
            }
        }
    }
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

    match show_and_validate_pf_file_dialog_output(dialog, pd, Some(default_extension)) {
        PfFileDialogState::None => None,
        PfFileDialogState::Error(msg) => {
            dialog::message_title("Error");
            dialog::alert_default(&msg);
            None
        }
        PfFileDialogState::RelativePathErr(_full_path, error_message) => {
            dialog::message_title("Error");
            dialog::alert_default(&error_message);
            None
        }
        PfFileDialogState::InsideProject(full_path, source_path) => Some(PfFileDialogResult {
            full_path,
            source_path,
        }),
        PfFileDialogState::OutsideProject(full_path, source_path) => {
            dialog::message_title("Warning");
            let choice = dialog::choice2_default(
                &format!(
                    concat![
                        "{} is outside of the project directory.\n",
                        "Do you still want to save to to this location?"
                    ],
                    full_path.display(),
                ),
                "Yes",
                "No", // default
                "",
            );
            match choice {
                Some(0) => Some(PfFileDialogResult {
                    source_path,
                    full_path,
                }),
                _ => None,
            }
        }
    }
}

fn open_file_dialog(title: &str, filter: &str) -> Option<PathBuf> {
    let mut dialog = dialog::NativeFileChooser::new(dialog::FileDialogType::BrowseFile);
    dialog.set_title(title);
    dialog.set_filter(filter);
    dialog.set_option(dialog::FileDialogOptions::UseFilterExt);

    let path = try_show_native_dialog(dialog)?;

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
            .item_iter()
            .position(|s| s.source == p.source_path)
        {
            Some(i) => {
                sender.send(GuiMessage::SelectProjectSong(i));
            }
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

fn open_sample_dialog(
    compiler_sender: &mpsc::Sender<ToCompiler>,
    pd: &ProjectData,
    source: &SourcePathBuf,
) -> Option<SourcePathBuf> {
    if let Some(p) = pf_open_file_dialog(pd, "Select sample", SAMPLE_FILTERS, Some(source)) {
        let new_source = p.source_path;

        // Remove the previous and new files from sample cache to ensure any file changes are loaded
        let _ = compiler_sender.send(ToCompiler::RemoveFileFromSampleCache(source.clone()));
        let _ = compiler_sender.send(ToCompiler::RemoveFileFromSampleCache(new_source.clone()));

        // The sample might be used by more than one instrument.
        // Recompile all instruments that use the sample file.
        // Will also recompile this instrument/sample if `source` was unchanged.
        let _ = compiler_sender.send(ToCompiler::RecompileInstrumentsUsingSample(
            new_source.clone(),
        ));

        if source != &new_source {
            Some(new_source)
        } else {
            None
        }
    } else {
        None
    }
}

pub fn open_instrument_sample_dialog(
    sender: &fltk::app::Sender<GuiMessage>,
    compiler_sender: &mpsc::Sender<ToCompiler>,
    pd: &ProjectData,
    id: ItemId,
) {
    let inst = match pd.instruments().get_id(id) {
        Some((_, inst)) => inst,
        None => return,
    };

    if let Some(new_source) = open_sample_dialog(compiler_sender, pd, &inst.source) {
        let new_inst = data::Instrument {
            source: new_source,
            ..inst.clone()
        };
        sender.send(GuiMessage::EditInstrument(id, new_inst));
    }
}

pub fn open_sample_sample_dialog(
    sender: &fltk::app::Sender<GuiMessage>,
    compiler_sender: &mpsc::Sender<ToCompiler>,
    pd: &ProjectData,
    id: ItemId,
) {
    let sample = match pd.samples().get_id(id) {
        Some((_, s)) => s,
        None => return,
    };

    if let Some(new_source) = open_sample_dialog(compiler_sender, pd, &sample.source) {
        let new_sample = data::Sample {
            source: new_source,
            ..sample.clone()
        };
        sender.send(GuiMessage::EditSample(id, new_sample));
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

fn load_binary_file_with_limit_show_dialog_on_error(path: &Path) -> Option<Vec<u8>> {
    use std::io::Read;

    assert!(path.is_absolute());

    let file = match std::fs::File::open(path) {
        Ok(file) => file,
        Err(e) => {
            dialog::message_title("Error opening file");
            dialog::alert_default(&format!("Error opening {}\n\n{}", path.display(), e));
            return None;
        }
    };

    let mut buffer = Vec::new();

    match file.take(MAX_FILE_SIZE.into()).read_to_end(&mut buffer) {
        Ok(n_bytes_read) => {
            if n_bytes_read < MAX_FILE_SIZE as usize {
                Some(buffer)
            } else {
                dialog::message_title("Error opening file");
                dialog::alert_default("File is too large");
                None
            }
        }
        Err(e) => {
            dialog::message_title("Error opening file");
            dialog::alert_default(&format!("Error opening {}\n\n{}", path.display(), e));
            None
        }
    }
}
