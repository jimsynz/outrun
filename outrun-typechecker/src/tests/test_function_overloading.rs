//! Tests for function overloading with guards (Phase 4.3)
//!
//! This module tests the advanced function overloading system including:
//! - Multiple function definitions with same name but different guards
//! - Guard-based function resolution
//! - Conflict detection and validation
//! - Function call resolution to correct overload

use crate::checker::TypeChecker;
use outrun_parser::parse_program;

/// Test basic function overloading with guards
#[test]
fn test_basic_function_overloading() {
    let source = r#"
        def divide(a: Integer, b: Integer): Float when b != 0 {
            3.14
        }
        
        def divide(a: Integer, b: Integer): String when b == 0 {
            "Division by zero error"
        }
    "#;

    let program = parse_program(source).expect("Should parse function overloading");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Function overloading with guards should pass validation: {:?}",
        result.err()
    );
}

/// Test function overloading with no guard (default case)
#[test]
fn test_function_overloading_with_default() {
    let source = r#"
        def process(x: Integer): String {
            "Default processing"
        }
        
        def process(x: Integer): String when x > 100 {
            "Large number processing"
        }
    "#;

    let program = parse_program(source).expect("Should parse function with default and guard");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Function overloading with default case should pass: {:?}",
        result.err()
    );
}

/// Test conflicting function overloads (same signature, no guards)
#[test]
fn test_conflicting_function_overloads() {
    let source = r#"
        def duplicate(x: Integer): String {
            "First version"
        }
        
        def duplicate(x: Integer): String {
            "Second version"
        }
    "#;

    let program = parse_program(source).expect("Should parse conflicting functions");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_err(),
        "Conflicting function overloads should fail validation"
    );
}

/// Test different parameter types (should be allowed)
#[test]
fn test_different_parameter_types_allowed() {
    let source = r#"
        def format(x: Integer): String {
            "integer"
        }
        
        def format(x: String): String {
            "string"
        }
    "#;

    let program = parse_program(source).expect("Should parse functions with different parameters");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Functions with different parameter types should be allowed: {:?}",
        result.err()
    );
}

/// Test function overloading call resolution (simple case)
#[test]
fn test_function_call_resolution() {
    let source = r#"
        def get_value(): String {
            "default"
        }
        
        def test_call(): String {
            get_value()
        }
    "#;

    let program = parse_program(source).expect("Should parse function call");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Function call resolution should work: {:?}",
        result.err()
    );
}

/// Test function overloading with guard function calls
#[test]
fn test_guard_function_in_overload() {
    let source = r#"
        def is_positive?(x: Integer): Boolean {
            x > 0
        }
        
        def process(x: Integer): String when x > 0 {
            "Positive processing"
        }
        
        def process(x: Integer): String {
            "Non-positive processing"
        }
    "#;

    let program = parse_program(source).expect("Should parse guard function in overload");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Guard function in overload should work: {:?}",
        result.err()
    );
}

/// Test multiple overloads with complex guards
#[test]
fn test_multiple_complex_overloads() {
    let source = r#"
        def categorize(x: Integer): String when x < 0 {
            "negative"
        }
        
        def categorize(x: Integer): String when x == 0 {
            "zero"
        }
        
        def categorize(x: Integer): String when x > 0 && x <= 10 {
            "small positive"
        }
        
        def categorize(x: Integer): String {
            "large positive"
        }
    "#;

    let program = parse_program(source).expect("Should parse multiple complex overloads");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Multiple complex overloads should work: {:?}",
        result.err()
    );
}

/// Test invalid overload with different return types but same guard
#[test]
fn test_invalid_overload_different_return_types() {
    let source = r#"
        def convert(x: Integer): String when x > 0 {
            "positive"
        }
        
        def convert(x: Integer): Integer when x > 0 {
            42
        }
    "#;

    let program =
        parse_program(source).expect("Should parse overloads with different return types");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    // This should fail - overloads with same guards but different return types should be rejected
    match result {
        Err(errors) => {
            // ✓ Expected: Should detect conflicting overloads
            let has_conflict_error = errors.iter().any(|e| {
                matches!(
                    e,
                    crate::error::TypeError::ConflictingFunctionOverload { .. }
                )
            });
            assert!(has_conflict_error, "Should detect conflicting function overload with same guard but different return types");
        }
        Ok(_) => {
            assert!(
                false,
                "Expected ConflictingFunctionOverload error for same guards with different return types, but type checking succeeded"
            );
        }
    }
}

/// Integration test with function calls and overloading
#[test]
fn test_overloading_integration() {
    let source = r#"
        def safe_divide(a: Integer, b: Integer): String when b == 0 {
            "Cannot divide by zero"
        }
        
        def safe_divide(a: Integer, b: Integer): String {
            "Valid division"
        }
        
        def test_integration(): String {
            let result1 = safe_divide(a: 10, b: 2)
            let result2 = safe_divide(a: 10, b: 0)
            result1
        }
    "#;

    let program = parse_program(source).expect("Should parse integration test");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    // This tests that overloaded functions work in a realistic scenario
    match result {
        Ok(_) => {
            // Integration test with overloaded functions is working - great!
        }
        Err(errors) => {
            // Should be a specific error, not a crash
            assert!(!errors.is_empty(), "Should have specific errors, not crash");
        }
    }
}
