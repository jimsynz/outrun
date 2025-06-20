//! Tests for list literal type checking
//!
//! This module tests the four-case list literal type inference:
//! 1. Empty list without type hint → ERROR
//! 2. Empty list with type hint → Use hint
//! 3. Type hint provided → Validate elements match hint
//! 4. No type hint → Require homogeneous elements

use crate::error::TypeError;
use crate::multi_program_compiler::{MultiProgramCompiler, ProgramCollection};
use outrun_parser::{parse_program, Program};

fn create_program_from_source(source: &str) -> Program {
    parse_program(source).unwrap_or_else(|e| {
        panic!("Failed to parse test program: {:?}\nSource: {}", e, source);
    })
}

/// Helper function to compile a single program and return errors
fn compile_program(source: &str) -> Result<(), Vec<TypeError>> {
    let program = create_program_from_source(source);
    let mut collection = ProgramCollection::new();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

    match result {
        Ok(_) => Ok(()),
        Err(errors) => Err(errors),
    }
}

/// Helper function to check if errors contain a specific error type
fn has_error_type<F>(errors: &[TypeError], predicate: F) -> bool
where
    F: Fn(&TypeError) -> bool,
{
    errors.iter().any(predicate)
}

#[test]
fn test_empty_list_without_type_hint_error() {
    let source = r#"
def test(): Integer {
    let empty = []
    42
}
"#;

    let result = compile_program(source);
    assert!(result.is_err(), "Empty list without type hint should error");

    let errors = result.unwrap_err();
    assert!(
        has_error_type(&errors, |e| matches!(
            e,
            TypeError::CannotInferListType { .. }
        )),
        "Should have CannotInferListType error, but got: {:?}",
        errors
    );
}

#[test]
fn test_empty_list_with_type_hint_success() {
    // Use simpler test without core library to debug the duplication issue
    let source = r#"
trait List<T> {
    # Empty trait for testing
}

struct String {
    # Empty struct for testing
}

def test(): String {
    let empty: List<String> = []
    String {}
}
"#;

    let result = compile_program(source);
    if let Err(errors) = &result {
        // Debug: print list-specific errors
        let list_errors: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    TypeError::CannotInferListType { .. } | TypeError::MixedListElements { .. }
                )
            })
            .collect();

        println!("List-specific errors found: {:?}", list_errors);

        assert!(
            list_errors.is_empty(),
            "Empty list with type hint should not have list-specific errors, but got: {:?}",
            list_errors
        );
    }
}

#[test]
fn test_homogeneous_list_success() {
    let source = r#"
def test(): Integer {
    let numbers = [1, 2, 3]
    42
}
"#;

    let result = compile_program(source);
    if let Err(errors) = &result {
        // Filter out unrelated errors
        let list_errors: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    TypeError::CannotInferListType { .. } | TypeError::MixedListElements { .. }
                )
            })
            .collect();

        assert!(
            list_errors.is_empty(),
            "Homogeneous list should not have list-specific errors, but got: {:?}",
            list_errors
        );
    }
}

#[test]
fn test_mixed_elements_without_hint_error() {
    let source = r#"
def test(): Integer {
    let mixed = [1, "hello", true]
    42
}
"#;

    let result = compile_program(source);
    assert!(
        result.is_err(),
        "Mixed elements without type hint should error"
    );

    let errors = result.unwrap_err();
    assert!(
        has_error_type(&errors, |e| matches!(
            e,
            TypeError::MixedListElements { .. }
        )),
        "Should have MixedListElements error, but got: {:?}",
        errors
    );
}

#[test]
fn test_type_hint_validation_success() {
    // This test assumes we have some trait that both Integer and Float implement
    // For now, we'll test with same types which should always work
    let source = r#"
def test(): Integer {
    let numbers: List<Integer> = [1, 2, 3]
    42
}
"#;

    let result = compile_program(source);
    if let Err(errors) = &result {
        // Filter out unrelated errors
        let list_errors: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    TypeError::CannotInferListType { .. }
                        | TypeError::MixedListElements { .. }
                        | TypeError::TypeMismatch { .. }
                )
            })
            .collect();

        assert!(
            list_errors.is_empty(),
            "Type hint validation with compatible types should not error, but got: {:?}",
            list_errors
        );
    }
}

#[test]
fn test_type_hint_validation_error() {
    let source = r#"
trait List<T> {
    # Empty trait for testing
}

struct Integer {
    # Empty struct for testing
}

struct String {
    # Empty struct for testing
}

def test(): Integer {
    let wrong: List<String> = [1, 2, 3]
    Integer {}
}
"#;

    let result = compile_program(source);
    assert!(result.is_err(), "Type hint mismatch should error");

    let errors = result.unwrap_err();
    assert!(
        has_error_type(&errors, |e| matches!(e, TypeError::TypeMismatch { .. })),
        "Should have TypeMismatch error for hint validation, but got: {:?}",
        errors
    );
}

#[test]
fn test_nested_list_literals() {
    let source = r#"
def test(): Integer {
    let nested: List<List<Integer>> = [[1, 2], [3, 4]]
    42
}
"#;

    let result = compile_program(source);
    if let Err(errors) = &result {
        // Filter out unrelated errors
        let list_errors: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    TypeError::CannotInferListType { .. } | TypeError::MixedListElements { .. }
                )
            })
            .collect();

        assert!(
            list_errors.is_empty(),
            "Nested list literals should work with proper type hints, but got: {:?}",
            list_errors
        );
    }
}

#[test]
fn test_empty_nested_list_error() {
    let source = r#"
def test(): Integer {
    let nested = [[]]
    42
}
"#;

    let result = compile_program(source);
    assert!(result.is_err(), "Empty nested list should error");

    let errors = result.unwrap_err();
    assert!(
        has_error_type(&errors, |e| matches!(
            e,
            TypeError::CannotInferListType { .. }
        )),
        "Should have CannotInferListType error for nested empty list, but got: {:?}",
        errors
    );
}

#[test]
fn test_list_in_let_binding_with_complex_type_annotation() {
    let source = r#"
trait Display {
    def show(value: Self): String
}

def test(): Integer {
    let items: List<Display> = []
    42
}
"#;

    let result = compile_program(source);
    if let Err(errors) = &result {
        // Filter out unrelated errors (trait not found, etc.)
        let list_errors: Vec<_> = errors
            .iter()
            .filter(|e| matches!(e, TypeError::CannotInferListType { .. }))
            .collect();

        assert!(
            list_errors.is_empty(),
            "Empty list with trait type hint should not have CannotInferListType error, but got: {:?}",
            list_errors
        );
    }
}

#[test]
fn test_list_literal_error_messages_quality() {
    // Test that error messages are helpful
    let source = r#"
def test(): Integer {
    let empty = []
    42
}
"#;

    let result = compile_program(source);
    assert!(result.is_err());

    let errors = result.unwrap_err();
    let list_error = errors
        .iter()
        .find(|e| matches!(e, TypeError::CannotInferListType { .. }));

    assert!(
        list_error.is_some(),
        "Should have CannotInferListType error"
    );

    // Check that the error message contains helpful guidance
    let error_msg = format!("{}", list_error.unwrap());
    assert!(
        error_msg.contains("Cannot infer type"),
        "Error message should mention type inference: {}",
        error_msg
    );
}

#[test]
fn test_mixed_list_error_messages_quality() {
    let source = r#"
def test(): Integer {
    let mixed = [42, "hello"]
    42
}
"#;

    let result = compile_program(source);
    assert!(result.is_err());

    let errors = result.unwrap_err();
    let list_error = errors
        .iter()
        .find(|e| matches!(e, TypeError::MixedListElements { .. }));

    assert!(list_error.is_some(), "Should have MixedListElements error");

    // Check that the error message contains helpful guidance
    let error_msg = format!("{}", list_error.unwrap());
    assert!(
        error_msg.contains("incompatible types"),
        "Error message should mention incompatible types: {}",
        error_msg
    );
}
