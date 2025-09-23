// Copyright 2022 The Goscript Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Map<K, V> {
    inner: HashMap<K, V>,
}

impl<K, V> Map<K, V>
where
    K: std::hash::Hash + Eq,
{
    pub fn new() -> Map<K, V> {
        Map {
            inner: HashMap::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Map<K, V> {
        Map {
            inner: HashMap::with_capacity(capacity),
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.inner.insert(key, value)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.inner.get_mut(key)
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.inner.remove(key)
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.inner.contains_key(key)
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn clear(&mut self) {
        self.inner.clear();
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<K, V> {
        self.inner.iter()
    }

    pub fn iter_mut(&mut self) -> std::collections::hash_map::IterMut<K, V> {
        self.inner.iter_mut()
    }
}

impl<K, V> Default for Map<K, V>
where
    K: std::hash::Hash + Eq,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> IntoIterator for Map<K, V> {
    type Item = (K, V);
    type IntoIter = std::collections::hash_map::IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<K, V> std::ops::Index<&K> for Map<K, V>
where
    K: std::hash::Hash + Eq,
{
    type Output = V;

    fn index(&self, index: &K) -> &Self::Output {
        &self.inner[index]
    }
}
