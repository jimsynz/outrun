//! Type and atom interning system
//!
//! Provides fast equality checks and memory efficiency through string interning.
//! All types and atoms are converted to numeric IDs for fast comparison.

use std::fmt;
use string_interner::{DefaultBackend, DefaultSymbol, StringInterner, Symbol};

/// Interned type identifier for fast equality checks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(DefaultSymbol);

/// Interned atom identifier for fast equality checks  
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AtomId(DefaultSymbol);

/// Interned trait identifier for fast equality checks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TraitId(DefaultSymbol);

/// Type interner manages string-to-ID mapping for types, atoms, and traits
#[derive(Debug, Clone)]
pub struct TypeInterner {
    type_names: StringInterner<DefaultBackend>,
    atom_names: StringInterner<DefaultBackend>,
    trait_names: StringInterner<DefaultBackend>,
}

impl TypeInterner {
    /// Create a new type interner with empty tables
    pub fn new() -> Self {
        Self {
            type_names: StringInterner::new(),
            atom_names: StringInterner::new(),
            trait_names: StringInterner::new(),
        }
    }

    /// Intern a type name and return its TypeId
    pub fn intern_type(&mut self, name: &str) -> TypeId {
        TypeId(self.type_names.get_or_intern(name))
    }

    /// Intern an atom name and return its AtomId
    pub fn intern_atom(&mut self, name: &str) -> AtomId {
        AtomId(self.atom_names.get_or_intern(name))
    }

    /// Intern a trait name and return its TraitId
    pub fn intern_trait(&mut self, name: &str) -> TraitId {
        TraitId(self.trait_names.get_or_intern(name))
    }

    /// Get the string representation of a TypeId
    pub fn resolve_type(&self, id: TypeId) -> Option<&str> {
        self.type_names.resolve(id.0)
    }

    /// Get the string representation of an AtomId
    pub fn resolve_atom(&self, id: AtomId) -> Option<&str> {
        self.atom_names.resolve(id.0)
    }

    /// Get the string representation of a TraitId
    pub fn resolve_trait(&self, id: TraitId) -> Option<&str> {
        self.trait_names.resolve(id.0)
    }

    /// Get TypeId for an existing type name (returns None if not interned)
    pub fn get_type(&self, name: &str) -> Option<TypeId> {
        self.type_names.get(name).map(TypeId)
    }

    /// Get AtomId for an existing atom name (returns None if not interned)
    pub fn get_atom(&self, name: &str) -> Option<AtomId> {
        self.atom_names.get(name).map(AtomId)
    }

    /// Get TraitId for an existing trait name (returns None if not interned)
    pub fn get_trait(&self, name: &str) -> Option<TraitId> {
        self.trait_names.get(name).map(TraitId)
    }

    /// Get the name of a type by its ID
    pub fn type_name(&self, type_id: TypeId) -> Option<String> {
        self.type_names.resolve(type_id.0).map(|s| s.to_string())
    }

    /// Get the name of an atom by its ID
    pub fn atom_name(&self, atom_id: AtomId) -> Option<String> {
        self.atom_names.resolve(atom_id.0).map(|s| s.to_string())
    }

    /// Get the name of a trait by its ID
    pub fn trait_name(&self, trait_id: TraitId) -> Option<String> {
        self.trait_names.resolve(trait_id.0).map(|s| s.to_string())
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

// Display implementations for debugging
impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeId({})", self.0.to_usize())
    }
}

impl fmt::Display for AtomId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AtomId({})", self.0.to_usize())
    }
}

impl fmt::Display for TraitId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TraitId({})", self.0.to_usize())
    }
}
