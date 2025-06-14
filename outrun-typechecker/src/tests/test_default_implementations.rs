//! Tests for default trait implementation override behavior
//!
//! This module tests the TYPE CHECKER's logic for handling default implementations.
//! Note: The parser doesn't yet support trait function definitions with bodies,
//! but the type checker is ready for when that feature is added.
//!
//! Current test approach: Create TraitFunction objects directly to test the logic.

use crate::checker::TypeContext;
use crate::types::traits::TraitFunction;

#[test]
fn test_trait_function_classification() {
    let mut context = TypeContext::new();

    // Create function signature (no default implementation)
    let signature_func = TraitFunction::new(
        context.interner.intern_atom("required_method"),
        vec![(
            context.interner.intern_atom("x"),
            context.interner.intern_type("Integer"),
        )],
        context.interner.intern_type("String"),
        false, // not a guard
        outrun_parser::Span::new(0, 1),
    );

    // Create function with default implementation
    let default_func = TraitFunction::new_with_default(
        context.interner.intern_atom("default_method"),
        vec![(
            context.interner.intern_atom("y"),
            context.interner.intern_type("String"),
        )],
        context.interner.intern_type("Integer"),
        false, // not a guard
        outrun_parser::Span::new(0, 1),
    );

    // Create static function
    let static_func = TraitFunction::new_static(
        context.interner.intern_atom("static_method"),
        vec![],
        context.interner.intern_type("String"),
        outrun_parser::Span::new(0, 1),
    );

    // Test classification
    assert!(!signature_func.is_static);
    assert!(!signature_func.has_default_impl);

    assert!(!default_func.is_static);
    assert!(default_func.has_default_impl);

    assert!(static_func.is_static);
    assert!(static_func.has_default_impl);
}

#[test]
fn test_implementation_requirement_logic() {
    let mut context = TypeContext::new();

    // Test the logic for determining if implementation is required

    // Case 1: Signature only - MUST be implemented
    let signature_func = TraitFunction::new(
        context.interner.intern_atom("required"),
        vec![],
        context.interner.intern_type("String"),
        false,
        outrun_parser::Span::new(0, 1),
    );

    // Case 2: Default implementation - CAN be implemented (optional override)
    let default_func = TraitFunction::new_with_default(
        context.interner.intern_atom("optional"),
        vec![],
        context.interner.intern_type("String"),
        false,
        outrun_parser::Span::new(0, 1),
    );

    // Case 3: Static function - CANNOT be implemented
    let static_func = TraitFunction::new_static(
        context.interner.intern_atom("static"),
        vec![],
        context.interner.intern_type("String"),
        outrun_parser::Span::new(0, 1),
    );

    // Test the validation logic used in check_impl_block
    let signature_required = !signature_func.is_static && !signature_func.has_default_impl;
    let default_required = !default_func.is_static && !default_func.has_default_impl;
    let static_required = !static_func.is_static && !static_func.has_default_impl;

    assert!(
        signature_required,
        "Signature functions must be implemented"
    );
    assert!(!default_required, "Default implementations are optional");
    assert!(!static_required, "Static functions cannot be implemented");
}

#[test]
fn test_trait_function_constructors() {
    let mut context = TypeContext::new();
    let span = outrun_parser::Span::new(0, 1);

    // Test that constructors set the right flags
    let signature = TraitFunction::new(
        context.interner.intern_atom("sig"),
        vec![],
        context.interner.intern_type("String"),
        false,
        span,
    );

    let default = TraitFunction::new_with_default(
        context.interner.intern_atom("def"),
        vec![],
        context.interner.intern_type("String"),
        false,
        span,
    );

    let static_fn = TraitFunction::new_static(
        context.interner.intern_atom("static"),
        vec![],
        context.interner.intern_type("String"),
        span,
    );

    // Verify signatures
    assert!(!signature.is_static);
    assert!(!signature.has_default_impl);

    // Verify default implementations
    assert!(!default.is_static);
    assert!(default.has_default_impl);

    // Verify static functions
    assert!(static_fn.is_static);
    assert!(static_fn.has_default_impl);
}
