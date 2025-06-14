//! Tests for trait implementation validation
//!
//! This module tests the comprehensive trait implementation system including:
//! - Trait implementation registration
//! - Required function validation
//! - Function signature compatibility checking
//! - Error handling for missing/duplicate implementations

use crate::checker::TypeChecker;
use crate::error::TypeError;
use outrun_parser::parse_program;

#[test]
fn test_basic_trait_implementation() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: Integer, y: Integer): String {
                "Drawing rectangle"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    // This should succeed - valid implementation
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Expected successful trait implementation validation"
    );
}

#[test]
fn test_missing_trait_implementation() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
            def color(self: Self): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: Integer, y: Integer): String {
                "Drawing rectangle"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(result.is_err(), "Expected error for missing trait function");

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::MissingImplementation { .. })));
}

#[test]
fn test_extra_implementation_function() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: Integer, y: Integer): String {
                "Drawing rectangle"
            }
            
            def extra_function(self: Self): String {
                "This function is not in the trait"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_err(),
        "Expected error for extra implementation function"
    );

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::ExtraImplementation { .. })));
}

#[test]
fn test_signature_mismatch_parameter_count() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: Integer): String {
                "Wrong parameter count"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_err(),
        "Expected error for parameter count mismatch"
    );

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::SignatureMismatch { .. })));
}

#[test]
fn test_signature_mismatch_parameter_type() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: String, y: Integer): String {
                "Wrong parameter type"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_err(),
        "Expected error for parameter type mismatch"
    );

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::SignatureMismatch { .. })));
}

#[test]
fn test_signature_mismatch_parameter_name() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, width: Integer, height: Integer): String {
                "Wrong parameter names"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_err(),
        "Expected error for parameter name mismatch"
    );

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::SignatureMismatch { .. })));
}

#[test]
fn test_signature_mismatch_return_type() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: Integer, y: Integer): Integer {
                42
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(result.is_err(), "Expected error for return type mismatch");

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::SignatureMismatch { .. })));
}

#[test]
fn test_duplicate_trait_implementation() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: Integer, y: Integer): String {
                "First implementation"
            }
        }
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: Integer, y: Integer): String {
                "Second implementation"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_err(),
        "Expected error for duplicate trait implementation"
    );

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::DuplicateImplementation { .. })));
}

#[test]
fn test_undefined_trait_implementation() {
    let source = r#"
        struct Rectangle(width: Integer, height: Integer) {}
        
        impl UndefinedTrait for Rectangle {
            def some_function(self: Self): String {
                "Implementation for undefined trait"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(result.is_err(), "Expected error for undefined trait");

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::UndefinedTrait { .. })));
}

#[test]
fn test_undefined_type_implementation() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        impl Drawable for UndefinedType {
            def draw(self: Self, x: Integer, y: Integer): String {
                "Implementation for undefined type"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(result.is_err(), "Expected error for undefined type");

    let errors = result.unwrap_err();
    assert!(errors
        .iter()
        .any(|e| matches!(e, TypeError::UndefinedType { .. })));
}

#[test]
fn test_static_trait_functions_not_required() {
    let source = r#"
        trait Factory {
            defs create(): String {
                "default object"
            }
            def process(self: Self, input: String): String
        }
        
        struct TextProcessor(name: String) {}
        
        impl Factory for TextProcessor {
            def process(self: Self, input: String): String {
                input
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    // This should succeed - static functions don't require implementation
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Expected successful trait implementation with static functions"
    );
}

#[test]
fn test_multiple_trait_implementations_different_types() {
    let source = r#"
        trait Drawable {
            def draw(self: Self, x: Integer, y: Integer): String
        }
        
        struct Rectangle(width: Integer, height: Integer) {}
        
        struct Circle(radius: Integer) {}
        
        impl Drawable for Rectangle {
            def draw(self: Self, x: Integer, y: Integer): String {
                "Drawing rectangle"
            }
        }
        
        impl Drawable for Circle {
            def draw(self: Self, x: Integer, y: Integer): String {
                "Drawing circle"
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    // This should succeed - different types can implement the same trait
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Expected successful multiple trait implementations for different types"
    );
}
