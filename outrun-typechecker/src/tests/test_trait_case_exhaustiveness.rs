//! Tests for trait-based case expression exhaustiveness checking
//!
//! These tests verify that trait case statements properly check for exhaustiveness
//! using orphan rules to determine which types implement a trait.

use crate::checker::TypeContext;
use crate::types::traits::{
    ExhaustivenessResult, TraitDefinition, TraitFunction, TraitImplementation,
};
use outrun_parser::Span;
use std::collections::HashMap;

fn setup_test_context() -> (
    TypeContext,
    crate::types::TraitId,
    crate::types::TypeId,
    crate::types::TypeId,
) {
    let mut context = TypeContext::new();

    // Register basic types
    let integer_type = context.interner.intern_type("Integer");
    let string_type = context.interner.intern_type("String");
    let _float_type = context.interner.intern_type("Float");

    // Register Display trait
    let display_trait = context.interner.intern_trait("Display");
    let to_string_func = context.interner.intern_atom("to_string");
    let string_return = context.interner.intern_type("String");

    let display_func = TraitFunction::new(
        to_string_func,
        vec![],
        string_return,
        false,
        Span::new(0, 10),
    );

    let display_def = TraitDefinition::new(
        display_trait,
        "Display".to_string(),
        vec![display_func],
        Span::new(0, 30),
    );

    context.trait_registry.register_trait(display_def);

    // Register implementations: Integer and String implement Display, Float does not
    let mut integer_funcs = HashMap::new();
    integer_funcs.insert(to_string_func, context.trait_registry.next_function_id());
    let integer_impl =
        TraitImplementation::new(display_trait, integer_type, integer_funcs, Span::new(0, 20));
    context.trait_registry.register_implementation(integer_impl);

    let mut string_funcs = HashMap::new();
    string_funcs.insert(to_string_func, context.trait_registry.next_function_id());
    let string_impl =
        TraitImplementation::new(display_trait, string_type, string_funcs, Span::new(0, 20));
    context.trait_registry.register_implementation(string_impl);

    (context, display_trait, integer_type, string_type)
}

#[test]
fn test_exhaustive_trait_case_registry() {
    let (context, display_trait, integer_type, string_type) = setup_test_context();

    // Test that covering all implementations is exhaustive
    let covered_types = vec![integer_type, string_type];
    let result = context
        .trait_registry
        .check_trait_case_exhaustiveness(display_trait, &covered_types);

    assert_eq!(result, ExhaustivenessResult::Exhaustive);
}

#[test]
fn test_non_exhaustive_trait_case_missing_string() {
    let (context, display_trait, integer_type, _string_type) = setup_test_context();

    // Test that missing String implementation is detected
    let covered_types = vec![integer_type];
    let result = context
        .trait_registry
        .check_trait_case_exhaustiveness(display_trait, &covered_types);

    match result {
        ExhaustivenessResult::Missing(missing_types) => {
            assert_eq!(missing_types.len(), 1);
            // We can't easily test the exact type since we'd need to compare TypeId values
            // The important thing is that one type is missing
        }
        ExhaustivenessResult::Exhaustive => {
            assert!(
                false,
                "Expected missing implementations, but got exhaustive"
            );
        }
    }
}

#[test]
fn test_non_exhaustive_trait_case_missing_integer() {
    let (context, display_trait, _integer_type, string_type) = setup_test_context();

    // Test that missing Integer implementation is detected
    let covered_types = vec![string_type];
    let result = context
        .trait_registry
        .check_trait_case_exhaustiveness(display_trait, &covered_types);

    match result {
        ExhaustivenessResult::Missing(missing_types) => {
            assert_eq!(missing_types.len(), 1);
            // We can't easily test the exact type since we'd need to compare TypeId values
            // The important thing is that one type is missing
        }
        ExhaustivenessResult::Exhaustive => {
            assert!(
                false,
                "Expected missing implementations, but got exhaustive"
            );
        }
    }
}

#[test]
fn test_empty_trait_case_registry() {
    let (context, display_trait, _integer_type, _string_type) = setup_test_context();

    // Test that covering no implementations is not exhaustive
    let covered_types = vec![];
    let result = context
        .trait_registry
        .check_trait_case_exhaustiveness(display_trait, &covered_types);

    match result {
        ExhaustivenessResult::Missing(missing_types) => {
            assert_eq!(missing_types.len(), 2); // Both Integer and String are missing
        }
        ExhaustivenessResult::Exhaustive => {
            assert!(
                false,
                "Expected missing implementations, but got exhaustive"
            );
        }
    }
}

#[test]
fn test_trait_implementors() {
    let (context, display_trait, integer_type, string_type) = setup_test_context();

    // Test that we can get all implementors of a trait
    let implementors = context.trait_registry.get_trait_implementors(display_trait);
    assert_eq!(implementors.len(), 2);
    assert!(implementors.contains(&integer_type));
    assert!(implementors.contains(&string_type));
}

#[test]
fn test_trait_implementation_lookup() {
    let (mut context, display_trait, integer_type, string_type) = setup_test_context();

    // Test that we can check if specific types implement the trait
    assert!(context
        .trait_registry
        .implements_trait(integer_type, display_trait));
    assert!(context
        .trait_registry
        .implements_trait(string_type, display_trait));

    // Test with a type that doesn't implement the trait
    let float_type = context.interner.intern_type("Float");
    assert!(!context
        .trait_registry
        .implements_trait(float_type, display_trait));
}
