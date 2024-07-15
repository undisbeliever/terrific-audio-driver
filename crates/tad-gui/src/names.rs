//! Validators

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::ItemId;

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

/// A two different lists that share names
pub struct TwoDeduplicatedNameVecs<T, U>(DeduplicatedNameVec<T>, DeduplicatedNameVec<U>);

impl<T, U> TwoDeduplicatedNameVecs<T, U> {
    pub fn into_tuple(self) -> (DeduplicatedNameVec<T>, DeduplicatedNameVec<U>) {
        (self.0, self.1)
    }
}

pub fn deduplicate_two_name_vecs<T, U>(
    list1: Vec<T>,
    list2: Vec<U>,
) -> (TwoDeduplicatedNameVecs<T, U>, usize)
where
    T: NameGetter + NameSetter,
    U: NameGetter + NameSetter,
{
    let (out1, n_fixed) = deduplicate_names(list1);
    let iter1 = out1.0.iter().map(NameGetter::name);

    let mut n_fixed = n_fixed;
    let mut out2 = Vec::with_capacity(list2.len());

    for mut e in list2.into_iter() {
        let iter = iter1.clone().chain(out2.iter().map(NameGetter::name));

        if let Some(new_name) = deduplicate_name_iter(e.name(), iter, None) {
            e.set_name(new_name);
            n_fixed += 1;
        }
        out2.push(e);
    }

    (
        TwoDeduplicatedNameVecs(out1, DeduplicatedNameVec(out2)),
        n_fixed,
    )
}

pub fn deduplicate_item_name<T>(name: &Name, list: &[T], index: Option<usize>) -> Option<Name>
where
    T: NameGetter,
{
    deduplicate_name_iter(name, &mut list.iter().map(NameGetter::name), index)
}

pub fn deduplicate_name_iter<'a>(
    name: &Name,
    iter: impl Iterator<Item = &'a Name>,
    index: Option<usize>,
) -> Option<Name> {
    let dupe_prefix = [name.as_str(), "__"].concat();

    let mut duplicate_found = false;
    let mut max_found = 1;

    for (i, v) in iter.enumerate() {
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
