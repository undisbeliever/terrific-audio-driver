//! Path handling functions

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

extern crate relative_path;
use relative_path::{PathExt, RelativePathBuf, RelativeToError};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use std::fmt::Display;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct PathString(String);

impl Display for PathString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

// The directory that contains the project file.
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
        match path.relative_to(&self.0) {
            Ok(p) => {
                if !p.starts_with("..") {
                    SourcePathResult::InsideProject(SourcePathBuf::new(p))
                } else {
                    SourcePathResult::OutsideProject(SourcePathBuf::new(p))
                }
            }
            Err(e) => SourcePathResult::Err(e),
        }
    }
}

pub enum SourcePathResult {
    InsideProject(SourcePathBuf),
    OutsideProject(SourcePathBuf),
    Err(RelativeToError),
}

// The source path within the project file, relative to ParentPathBuf.
#[derive(Default, Clone, PartialEq, Eq, Hash, Debug)]
pub struct SourcePathBuf {
    relative_path: RelativePathBuf,
}

impl SourcePathBuf {
    fn new(path: RelativePathBuf) -> Self {
        Self {
            relative_path: path,
        }
    }

    pub fn as_str(&self) -> &str {
        self.relative_path.as_str()
    }

    pub fn file_stem_string(&self) -> Option<&str> {
        self.relative_path.file_stem()
    }

    pub fn extension(&self) -> Option<&str> {
        self.relative_path.extension()
    }

    pub fn to_path(&self, parent_path: &ParentPathBuf) -> PathBuf {
        self.relative_path.to_logical_path(&parent_path.0)
    }

    pub fn file_name(&self) -> &str {
        self.relative_path.file_name().unwrap_or("")
    }

    pub fn to_path_string(&self) -> PathString {
        PathString(self.as_str().to_owned())
    }
}

impl Display for SourcePathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.relative_path.fmt(f)
    }
}

impl Serialize for SourcePathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.relative_path.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SourcePathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let p = Deserialize::deserialize(deserializer)?;

        Ok(Self::new(p))
    }
}
