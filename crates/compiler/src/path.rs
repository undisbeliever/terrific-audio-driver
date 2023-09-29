//! Path handling functions

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use std::ffi::OsStr;
use std::fmt::Display;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct PathString(String);

impl Display for PathString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

// The directory that contains the project JSON file.
#[derive(Clone)]
pub struct ParentPathBuf(PathBuf);

impl ParentPathBuf {
    pub(crate) fn new(p: PathBuf) -> Self {
        Self(p)
    }

    pub fn as_path(&self) -> &PathBuf {
        &self.0
    }

    pub fn create_source_path(&self, path: &Path) -> SourcePathResult {
        match path.strip_prefix(&self.0) {
            Ok(p) => SourcePathResult::InsideProject(SourcePathBuf::new(p)),
            Err(_) => SourcePathResult::OutsideProject(SourcePathBuf::new(path)),
        }
    }
}

pub enum SourcePathResult {
    InsideProject(SourcePathBuf),
    OutsideProject(SourcePathBuf),
}

// The source path within the project JSON file, relative to ParentPathBuf.
#[derive(Default, Clone, PartialEq, Eq, Hash, Debug)]
pub struct SourcePathBuf {
    path: PathBuf,
}

impl SourcePathBuf {
    fn new(p: &Path) -> Self {
        Self { path: p.to_owned() }
    }

    pub fn as_str(&self) -> &str {
        self.path.to_str().unwrap_or_default()
    }

    pub fn file_stem_string(&self) -> Option<String> {
        self.path
            .file_stem()
            .map(|s| s.to_string_lossy().to_string())
    }

    pub fn extension(&self) -> Option<&OsStr> {
        self.path.extension()
    }

    pub fn to_path(&self, parent_path: &ParentPathBuf) -> PathBuf {
        parent_path.0.join(&self.path)
    }

    pub fn file_name_string(&self) -> String {
        self.path
            .file_name()
            .unwrap_or(self.path.as_os_str())
            .to_string_lossy()
            .to_string()
    }

    pub fn to_path_string(&self) -> PathString {
        PathString(self.path.to_string_lossy().to_string())
    }
}

impl Display for SourcePathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.path.display().fmt(f)
    }
}

impl Serialize for SourcePathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.path.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SourcePathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let p: &Path = Deserialize::deserialize(deserializer)?;

        Ok(Self::new(p))
    }
}
