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

/// Type interner manages string-to-ID mapping for types and atoms
/// Uses a single interner for efficiency, with different wrapper types for semantic distinction
#[derive(Debug, Clone)]
pub struct TypeInterner {
    interner: StringInterner<DefaultBackend>,
}

impl TypeInterner {
    /// Create a new type interner with empty table
    pub fn new() -> Self {
        Self {
            interner: StringInterner::new(),
        }
    }

    /// Intern a type name and return its TypeId
    pub fn intern_type(&mut self, name: &str) -> TypeId {
        TypeId(self.interner.get_or_intern(name))
    }

    /// Intern an atom name and return its AtomId
    pub fn intern_atom(&mut self, name: &str) -> AtomId {
        AtomId(self.interner.get_or_intern(name))
    }

    /// Get the string representation of a TypeId
    pub fn resolve_type(&self, id: TypeId) -> Option<&str> {
        self.interner.resolve(id.0)
    }

    /// Get the string representation of an AtomId
    pub fn resolve_atom(&self, id: AtomId) -> Option<&str> {
        self.interner.resolve(id.0)
    }

    /// Get TypeId for an existing type name (returns None if not interned)
    pub fn get_type(&self, name: &str) -> Option<TypeId> {
        self.interner.get(name).map(TypeId)
    }

    /// Get AtomId for an existing atom name (returns None if not interned)
    pub fn get_atom(&self, name: &str) -> Option<AtomId> {
        self.interner.get(name).map(AtomId)
    }

    /// Get the name of a type by its ID
    pub fn type_name(&self, type_id: TypeId) -> Option<String> {
        self.interner.resolve(type_id.0).map(|s| s.to_string())
    }

    /// Get the name of an atom by its ID
    pub fn atom_name(&self, atom_id: AtomId) -> Option<String> {
        self.interner.resolve(atom_id.0).map(|s| s.to_string())
    }

    /// Get the number of interned types and atoms
    pub fn len(&self) -> usize {
        self.interner.len()
    }

    /// Check if the interner is empty
    pub fn is_empty(&self) -> bool {
        self.interner.is_empty()
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
