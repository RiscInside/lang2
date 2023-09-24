use std::collections::hash_map::Entry;
use std::collections::{hash_map::RandomState, HashMap};
use std::hash::{BuildHasher, Hash};

#[derive(Debug, PartialEq, Eq)]
/// Singly linked list of values corresponding to a single key/identifier.
/// This implementation is based on the assumption that shadowing is rare - Vec<V> would be more expensive.
pub struct Mapping<V> {
    value: V,
    shadowed: Option<Box<Mapping<V>>>,
}

impl<V> Mapping<V> {
    /// Shadow current mapping
    fn shadow(&mut self, value: V) {
        let mapping = std::mem::replace(
            self,
            Mapping {
                value,
                shadowed: None,
            },
        );
        self.shadowed = Some(Box::new(mapping));
    }

    /// Make shadowed mapping available. Returns true if there was no shadowed binding
    fn unshadow(&mut self) -> bool {
        let Some(shadowed) = std::mem::take(&mut self.shadowed) else { return true };
        *self = *shadowed;
        false
    }
}

#[derive(Debug)]
pub struct Table<K, V, S = RandomState> {
    mappings: HashMap<K, Mapping<V>, S>,
}

impl<K, V> Default for Table<K, V, RandomState> {
    fn default() -> Self {
        Self {
            mappings: HashMap::new(),
        }
    }
}

impl<K, V> Table<K, V, RandomState> {
    /// Create a new symbol table
    pub fn new() -> Self {
        Default::default()
    }
}

impl<K: Eq + Hash, V, S: BuildHasher> Table<K, V, S> {
    pub fn new_with_hasher(s: S) -> Self {
        Self {
            mappings: HashMap::with_hasher(s),
        }
    }

    /// Introduce binding into scope
    pub fn push(&mut self, key: K, value: V) {
        match self.mappings.entry(key) {
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().shadow(value);
            }
            Entry::Vacant(vacant) => {
                vacant.insert(Mapping {
                    value,
                    shadowed: None,
                });
            }
        }
    }

    /// Remove binding from scope
    ///
    /// # Panics
    ///
    /// Panics if key isn't present
    pub fn pop(&mut self, key: K) {
        match self.mappings.entry(key) {
            Entry::Occupied(mut occupied) => {
                if occupied.get_mut().unshadow() {
                    occupied.remove();
                }
            }
            Entry::Vacant(_) => unreachable!(),
        }
    }

    /// Lookup value for the binding
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        self.mappings.get(key).map(|mapping| &mapping.value)
    }

    /// Check if a given key is present
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        self.mappings.contains_key(key)
    }
}
