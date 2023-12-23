//! Validators

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::ItemId;
use crate::list_editor::LaVec;

extern crate compiler;
use compiler::data;
use compiler::data::Name;
use compiler::sound_effects::SoundEffectInput;

pub trait NameGetter {
    fn name(&self) -> &Name;
}

impl<T> NameGetter for (ItemId, T)
where
    T: NameGetter,
{
    fn name(&self) -> &Name {
        self.1.name()
    }
}

impl NameGetter for Name {
    fn name(&self) -> &Name {
        self
    }
}

impl NameGetter for data::Song {
    fn name(&self) -> &Name {
        &self.name
    }
}

impl NameGetter for data::Instrument {
    fn name(&self) -> &Name {
        &self.name
    }
}

impl NameGetter for data::Sample {
    fn name(&self) -> &Name {
        &self.name
    }
}

impl NameGetter for SoundEffectInput {
    fn name(&self) -> &Name {
        &self.name
    }
}

pub trait NameSetter {
    fn set_name(&mut self, name: Name);
}

impl NameSetter for Name {
    fn set_name(&mut self, name: Name) {
        *self = name;
    }
}

impl NameSetter for data::Song {
    fn set_name(&mut self, name: Name) {
        self.name = name;
    }
}

impl NameSetter for data::Instrument {
    fn set_name(&mut self, name: Name) {
        self.name = name;
    }
}

impl NameSetter for data::Sample {
    fn set_name(&mut self, name: Name) {
        self.name = name;
    }
}

impl NameSetter for SoundEffectInput {
    fn set_name(&mut self, name: Name) {
        self.name = name;
    }
}

pub trait NameDeduplicator
where
    Self: Sized,
{
    /// Returns true if a's name != b's name
    fn test_name_changed(a: &Self, b: &Self) -> bool;

    /// Renames item if the name exists in the list.
    fn dedupe_name(item: &mut Self, list: &LaVec<(ItemId, Self)>, index: Option<usize>);
}

impl<T> NameDeduplicator for T
where
    T: NameGetter + NameSetter,
{
    fn test_name_changed(a: &Self, b: &Self) -> bool {
        a.name() != b.name()
    }

    fn dedupe_name(item: &mut Self, list: &LaVec<(ItemId, Self)>, index: Option<usize>) {
        if let Some(new_name) = deduplicate_item_name(item.name(), list, index) {
            item.set_name(new_name);
        }
    }
}

pub struct DeduplicatedNameVec<T>(Vec<T>);

impl<T> DeduplicatedNameVec<T> {
    pub fn into_vec(self) -> Vec<T> {
        self.0
    }
}

pub fn deduplicate_names<T>(list: Vec<T>) -> (DeduplicatedNameVec<T>, usize)
where
    T: NameGetter + NameSetter,
{
    let mut n_fixed = 0;
    let mut out = Vec::with_capacity(list.len());

    for mut e in list.into_iter() {
        if let Some(new_name) = deduplicate_item_name(e.name(), &out, None) {
            e.set_name(new_name);
            n_fixed += 1;
        }
        out.push(e);
    }

    (DeduplicatedNameVec(out), n_fixed)
}

fn deduplicate_item_name<T>(name: &Name, list: &[T], index: Option<usize>) -> Option<Name>
where
    T: NameGetter,
{
    let dupe_prefix = [name.as_str(), "__"].concat();

    let mut duplicate_found = false;
    let mut max_found = 1;

    for (i, v) in list.iter().enumerate() {
        if Some(i) != index {
            let v_name = v.name();
            if v_name == name {
                duplicate_found = true;
            }
            if let Some(a) = v_name.as_str().strip_prefix(&dupe_prefix) {
                if let Ok(n) = a.parse() {
                    if n > max_found {
                        max_found = n;
                    }
                }
            }
        }
    }

    if !duplicate_found {
        None
    } else {
        Some(Name::new_lossy(format!("{}{}", dupe_prefix, max_found + 1)))
    }
}
