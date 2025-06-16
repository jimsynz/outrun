//! Tests for function definition validation
//!
//! This module tests the comprehensive function definition type checking including:
//! - Parameter type validation and uniqueness
//! - Return type validation (mandatory explicit types)
//! - Guard function validation (Boolean return type)
//! - Function body type checking

use crate::checker::TypeChecker;
use outrun_parser::parse_program;

/// Test valid function definition with explicit return type
#[test]
fn test_valid_function_with_return_type() {
    let source = r#"
        def calculate(x: Integer, y: Integer): Integer {
            x + y
        }
    "#;

    let program = parse_program(source).expect("Should parse valid function");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Valid function should pass validation: {:?}",
        result.err()
    );
}

/// Test function definition with missing return type (should fail)
#[test]
fn test_function_missing_return_type() {
    let source = r#"
        def invalid_function(x: Integer) {
            x * 2
        }
    "#;

    // This should fail to parse because functions require explicit return types
    let program_result = parse_program(source);

    // Either the parser rejects it or the type checker does
    if let Ok(program) = program_result {
        let mut type_checker = TypeChecker::new();
        let result = type_checker.check_program(&program);
        assert!(
            result.is_err(),
            "Function without return type should fail validation"
        );
    }
    // If parser rejects it, that's also acceptable
}

/// Test function with duplicate parameter names (should fail)
#[test]
fn test_function_duplicate_parameter_names() {
    let source = r#"
        def invalid_function(x: Integer, x: String): Integer {
            x
        }
    "#;

    // Either parser or type checker should reject duplicate parameters
    let program_result = parse_program(source);

    if let Ok(program) = program_result {
        let mut type_checker = TypeChecker::new();
        let result = type_checker.check_program(&program);
        assert!(
            result.is_err(),
            "Function with duplicate parameters should fail"
        );
    }
    // If parser rejects it, that's also acceptable
}

/// Test function with invalid parameter type (should fail)
#[test]
fn test_function_invalid_parameter_type() {
    let source = r#"
        def invalid_function(x: NonExistentType): Integer {
            42
        }
    "#;

    let program = parse_program(source).expect("Should parse function with invalid type");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_err(),
        "Function with invalid parameter type should fail"
    );
}

/// Test function with invalid return type (should fail)
#[test]
fn test_function_invalid_return_type() {
    let source = r#"
        def invalid_function(x: Integer): NonExistentType {
            x
        }
    "#;

    let program = parse_program(source).expect("Should parse function with invalid return type");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_err(),
        "Function with invalid return type should fail"
    );
}

/// Test valid guard function (ends with '?' and returns Boolean)
#[test]
fn test_valid_guard_function() {
    let source = r#"
        def is_positive?(x: Integer): Boolean {
            x > 0
        }
    "#;

    let program = parse_program(source).expect("Should parse valid guard function");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Valid guard function should pass validation: {:?}",
        result.err()
    );
}

/// Test invalid guard function (ends with '?' but doesn't return Boolean)
#[test]
fn test_invalid_guard_function_return_type() {
    let source = r#"
        def bad_guard?(x: Integer): Integer {
            x
        }
    "#;

    let program =
        parse_program(source).expect("Should parse guard function with wrong return type");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_err(),
        "Guard function with non-Boolean return should fail"
    );
}

/// Test function with complex parameter validation
#[test]
fn test_function_parameter_validation_comprehensive() {
    let source = r#"
        def complex_function(
            name: String,
            age: Integer,
            is_active: Boolean
        ): String {
            "result"
        }
    "#;

    let program = parse_program(source).expect("Should parse function with multiple parameters");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Function with multiple valid parameters should pass: {:?}",
        result.err()
    );
}

/// Test empty parameter function
#[test]
fn test_function_no_parameters() {
    let source = r#"
        def get_constant(): Integer {
            42
        }
    "#;

    let program = parse_program(source).expect("Should parse function with no parameters");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Function with no parameters should pass validation: {:?}",
        result.err()
    );
}

/// Integration test for multiple function definitions
#[test]
fn test_multiple_function_definitions() {
    let source = r#"
        def add(x: Integer, y: Integer): Integer {
            x + y
        }
        
        def is_zero?(x: Integer): Boolean {
            x == 0
        }
        
        def get_name(user: String): String {
            user
        }
    "#;

    let program = parse_program(source).expect("Should parse multiple functions");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Multiple function definitions should pass validation: {:?}",
        result.err()
    );
}
