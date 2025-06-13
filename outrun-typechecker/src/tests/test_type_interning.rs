//! Unit tests for the type interning system
//!
//! Tests TypeInterner functionality for types, atoms, and traits

use crate::types::interning::TypeInterner;

#[test]
fn test_type_interning() {
    let mut interner = TypeInterner::new();

    let id1 = interner.intern_type("Integer");
    let id2 = interner.intern_type("String");
    let id3 = interner.intern_type("Integer"); // Same as id1

    assert_eq!(id1, id3);
    assert_ne!(id1, id2);

    assert_eq!(interner.resolve_type(id1), Some("Integer"));
    assert_eq!(interner.resolve_type(id2), Some("String"));
}

#[test]
fn test_atom_interning() {
    let mut interner = TypeInterner::new();

    let id1 = interner.intern_atom("status");
    let id2 = interner.intern_atom("error");
    let id3 = interner.intern_atom("status"); // Same as id1

    assert_eq!(id1, id3);
    assert_ne!(id1, id2);

    assert_eq!(interner.resolve_atom(id1), Some("status"));
    assert_eq!(interner.resolve_atom(id2), Some("error"));
}

#[test]
fn test_trait_interning() {
    let mut interner = TypeInterner::new();

    let id1 = interner.intern_trait("Display");
    let id2 = interner.intern_trait("Debug");
    let id3 = interner.intern_trait("Display"); // Same as id1

    assert_eq!(id1, id3);
    assert_ne!(id1, id2);

    assert_eq!(interner.resolve_trait(id1), Some("Display"));
    assert_eq!(interner.resolve_trait(id2), Some("Debug"));
}

#[test]
fn test_get_existing() {
    let mut interner = TypeInterner::new();

    // Initially, nothing is interned
    assert_eq!(interner.get_type("Integer"), None);

    // After interning, we can retrieve
    let id = interner.intern_type("Integer");
    assert_eq!(interner.get_type("Integer"), Some(id));

    // Non-existent types still return None
    assert_eq!(interner.get_type("NonExistent"), None);
}
