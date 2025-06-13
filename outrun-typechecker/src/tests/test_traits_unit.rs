//! Unit tests for the trait system
//!
//! Tests TraitDefinition, TraitImplementation, TraitRegistry, and TraitConstraint

use crate::types::traits::*;
use crate::types::{AtomId, TraitId, TypeId, TypeInterner};
use outrun_parser::Span;
use std::collections::HashMap;

fn setup_test_registry() -> (TraitRegistry, TypeInterner, TraitId, TypeId, AtomId) {
    let registry = TraitRegistry::new();
    let mut interner = TypeInterner::new();

    let trait_id = interner.intern_trait("Display");
    let type_id = interner.intern_type("Integer");
    let func_name = interner.intern_atom("to_string");

    (registry, interner, trait_id, type_id, func_name)
}

#[test]
fn test_trait_definition() {
    let (_, mut interner, trait_id, _, func_name) = setup_test_registry();
    let return_type = interner.intern_type("String");

    let func = TraitFunction::new(func_name, vec![], return_type, false, Span::new(0, 10));

    let trait_def = TraitDefinition::new(
        trait_id,
        "Display".to_string(),
        vec![func],
        Span::new(0, 50),
    );

    assert_eq!(trait_def.id, trait_id);
    assert_eq!(trait_def.name, "Display");
    assert_eq!(trait_def.functions.len(), 1);
    assert!(!trait_def.is_generic());
    assert!(trait_def.find_function(func_name).is_some());
}

#[test]
fn test_trait_implementation() {
    let (_, _, trait_id, type_id, func_name) = setup_test_registry();

    let mut functions = HashMap::new();
    let func_id = FunctionId(1);
    functions.insert(func_name, func_id);

    let impl_ = TraitImplementation::new(trait_id, type_id, functions, Span::new(0, 30));

    assert_eq!(impl_.trait_id, trait_id);
    assert_eq!(impl_.type_id, type_id);
    assert!(impl_.implements_function(func_name));
    assert_eq!(impl_.get_function_id(func_name), Some(func_id));
}

#[test]
fn test_trait_registry() {
    let (mut registry, mut interner, trait_id, type_id, func_name) = setup_test_registry();
    let return_type = interner.intern_type("String");

    // Register trait definition
    let func = TraitFunction::new(func_name, vec![], return_type, false, Span::new(0, 10));

    let trait_def = TraitDefinition::new(
        trait_id,
        "Display".to_string(),
        vec![func],
        Span::new(0, 50),
    );

    registry.register_trait(trait_def);

    // Register implementation
    let mut functions = HashMap::new();
    let func_id = registry.next_function_id();
    functions.insert(func_name, func_id);

    let impl_ = TraitImplementation::new(trait_id, type_id, functions, Span::new(0, 30));

    registry.register_implementation(impl_);

    // Test lookups
    assert!(registry.get_trait(trait_id).is_some());
    assert!(registry.get_implementation(trait_id, type_id).is_some());
    assert!(registry.implements_trait(type_id, trait_id));

    // Test collections
    assert_eq!(registry.get_trait_implementations(trait_id).len(), 1);
    assert_eq!(registry.get_type_implementations(type_id).len(), 1);
}

#[test]
fn test_trait_constraint() {
    let (_, mut interner, trait_id, type_id, _) = setup_test_registry();
    let debug_trait = interner.intern_trait("Debug");

    let constraint = TraitConstraint::new(type_id, vec![trait_id, debug_trait], Span::new(0, 20));

    assert!(constraint.requires_trait(trait_id));
    assert!(constraint.requires_trait(debug_trait));
    assert!(!constraint.requires_trait(interner.intern_trait("Unknown")));
}
