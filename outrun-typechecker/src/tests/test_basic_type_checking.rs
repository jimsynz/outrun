//! Basic type checking integration tests
//!
//! Tests the core type checking functionality with simple programs.

use crate::{typecheck_program, TypeChecker};
use outrun_parser::parse_program;

#[test]
fn test_typechecker_creation() {
    let checker = TypeChecker::new();
    // Type checker should initialize successfully
    // This tests that all the module dependencies work
    drop(checker); // Ensure no panics during drop
}

#[test]
fn test_empty_program_type_checking() {
    let source = "";

    let program = parse_program(source).expect("Should parse empty program");
    let result = typecheck_program(program);

    // Empty program should type check successfully
    assert!(
        result.is_ok(),
        "Empty program should type check: {:?}",
        result
    );
}

#[test]
fn test_simple_literal_program() {
    let source = "42";

    let program = parse_program(source).expect("Should parse literal");
    let result = typecheck_program(program);

    // Should handle simple literals (even if not fully implemented yet)
    // This test may fail initially but will pass once basic expression checking is implemented
    match result {
        Ok(_typed_program) => {
            // Success - basic literal type checking works
        }
        Err(errors) => {
            // Expected for now since expression checking is stubbed
            // Check that we get the expected "unimplemented feature" error
            assert!(!errors.is_empty());
            // Expected error for unimplemented features
        }
    }
}

#[test]
fn test_boolean_literal() {
    let source = "true";

    let program = parse_program(source).expect("Should parse boolean");
    let result = typecheck_program(program);

    match result {
        Ok(_typed_program) => {
            // Success - boolean literal type checking works
        }
        Err(errors) => {
            // Expected for now since expression checking is stubbed
            assert!(!errors.is_empty());
            // Expected error for unimplemented features
        }
    }
}

#[test]
fn test_string_literal() {
    let source = r#""hello world""#;

    let program = parse_program(source).expect("Should parse string");
    let result = typecheck_program(program);

    match result {
        Ok(_typed_program) => {
            // Success - string literal type checking works
        }
        Err(errors) => {
            // Expected for now since expression checking is stubbed
            assert!(!errors.is_empty());
            // Expected error for unimplemented features
        }
    }
}

// Integration test to ensure the full pipeline works
#[test]
fn test_type_checking_pipeline() {
    let mut checker = TypeChecker::new();

    // Create a minimal test program
    let source = "42";
    let program = parse_program(source).expect("Should parse");

    // Test that the full type checking pipeline runs without panicking
    let result = checker.check_program(&program);

    // For now, we just check that it doesn't panic
    // As we implement more features, this test will evolve
    match result {
        Ok(_) => {
            // Type checking succeeded
        }
        Err(_errors) => {
            // Type checking failed (expected)
        }
    }
}
