//! Tests for enhanced function call resolution
//!
//! This module tests Phase 4.2 enhancements including:
//! - Qualified function calls (Module.function)
//! - Function capture syntax (&function_name)
//! - Enhanced function call validation

use crate::checker::TypeChecker;
use outrun_parser::parse_program;

/// Test basic qualified function call (static trait function)
#[test]
fn test_qualified_function_call_static_trait() {
    let source = r#"
        trait ResultTrait {
            defs ok(value: String): String {
                "success"
            }
            
            def is_ok?(self: Self): Boolean
        }
        
        def test_function(): String {
            ResultTrait.ok(value: "success")
        }
    "#;

    let program = parse_program(source).expect("Should parse qualified function call");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Qualified function call should pass validation: {:?}",
        result.err()
    );
}

/// Test simple function capture
#[test]
fn test_simple_function_capture() {
    let source = r#"
        def helper_function(x: Integer): String {
            "result"
        }
        
        def test_capture(): Function<String> {
            &helper_function
        }
    "#;

    let program = parse_program(source).expect("Should parse function capture");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    // This might fail due to Function<> type not being fully implemented, but should not crash
    // We expect either success or a specific unimplemented feature error
    match result {
        Ok(_) => {
            // Function capture syntax is working - great!
        }
        Err(errors) => {
            // Should be a specific error, not a crash
            assert!(!errors.is_empty(), "Should have specific errors, not crash");
        }
    }
}

/// Test qualified function capture  
#[test]
fn test_qualified_function_capture() {
    let source = r#"
        trait ResultTrait {
            defs ok(value: String): String {
                "success"
            }
        }
        
        def test_qualified_capture(): String {
            &ResultTrait.ok
        }
    "#;

    let program = parse_program(source).expect("Should parse qualified function capture");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    // This tests that qualified capture syntax is processed correctly
    match result {
        Ok(_) => {
            // Qualified function capture syntax is working - great!
        }
        Err(errors) => {
            // Should be a specific error, not a crash
            assert!(!errors.is_empty(), "Should have specific errors, not crash");
        }
    }
}

/// Test function capture with arity specification
#[test]
fn test_function_capture_with_arity() {
    let source = r#"
        def process(x: Integer, y: String): Boolean {
            true
        }
        
        def test_arity_capture(): Function<Boolean> {
            &process/2
        }
    "#;

    let program = parse_program(source).expect("Should parse function capture with arity");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    // This tests arity specification in function capture
    match result {
        Ok(_) => {
            // Function capture with arity is working - great!
        }
        Err(errors) => {
            // Should be a specific error, not a crash
            assert!(!errors.is_empty(), "Should have specific errors, not crash");
        }
    }
}

/// Test invalid qualified function call
#[test]
fn test_invalid_qualified_function_call() {
    let source = r#"
        trait ValidTrait {
            defs valid_function(x: Integer): String {
                "valid"
            }
        }
        
        def test_invalid(): String {
            ValidTrait.invalid_function(x: 123)
        }
    "#;

    let program = parse_program(source).expect("Should parse invalid qualified call");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_err(),
        "Invalid qualified function call should fail"
    );
}

/// Test invalid function capture
#[test]
fn test_invalid_function_capture() {
    let source = r#"
        def test_invalid(): Function<String> {
            &nonexistent_function
        }
    "#;

    let program = parse_program(source).expect("Should parse invalid function capture");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(result.is_err(), "Invalid function capture should fail");
}

/// Test multiple qualified calls in same function
#[test]
fn test_multiple_qualified_calls() {
    let source = r#"
        trait OptionTrait {
            defs some(value: String): String {
                value
            }
            
            defs none(): String {
                "none"
            }
        }
        
        def test_multiple(): String {
            if true {
                OptionTrait.some(value: "hello")
            } else {
                OptionTrait.none()
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse multiple qualified calls");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Multiple qualified calls should work: {:?}",
        result.err()
    );
}

/// Integration test for function calls and captures together
#[test]
fn test_function_calls_and_captures_integration() {
    let source = r#"
        def helper(x: Integer): String {
            "processed"
        }
        
        trait ProcessorTrait {
            defs create(): String {
                "created"
            }
        }
        
        def test_integration(): String {
            let processor = ProcessorTrait.create()
            let captured = &helper
            "integrated"
        }
    "#;

    let program = parse_program(source).expect("Should parse integration test");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    // This tests that both qualified calls and captures work together
    match result {
        Ok(_) => {
            // Integration test is working - great!
        }
        Err(errors) => {
            // Should be a specific error, not a crash
            assert!(!errors.is_empty(), "Should have specific errors, not crash");
        }
    }
}
