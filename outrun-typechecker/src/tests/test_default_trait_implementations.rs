//! Integration tests for default trait implementation behavior
//!
//! Tests the complete flow from parsing trait definitions with default implementations
//! through type checking and validation of overriding behavior.

use crate::checker::TypeChecker;
use outrun_parser::parse_program;

#[test]
fn test_trait_with_default_implementation() {
    let source = r#"
        trait Drawable {
            def draw(self: Self): String {
                "default drawing"
            }
            def color(self: Self): String
        }
        
        struct Circle(radius: Float) {}
        
        impl Drawable for Circle {
            def color(self: Self): String {
                "red"
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse trait with default implementation");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Trait with default implementation should type check: {:?}",
        result.err()
    );

    let mut typed_program = result.unwrap();

    // Verify the trait was registered with both required and default functions
    let drawable_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Drawable")
        .unwrap();
    let trait_def = typed_program
        .type_context
        .trait_registry
        .get_trait(drawable_trait_id)
        .unwrap();

    assert_eq!(trait_def.functions.len(), 2);

    // Find the draw function (should have default implementation)
    let draw_atom = typed_program.type_context.interner.intern_atom("draw");
    let draw_func = trait_def.find_function(draw_atom).unwrap();
    assert!(
        draw_func.has_default_impl,
        "draw function should have default implementation"
    );
    assert!(
        !draw_func.is_static,
        "draw function should be instance function"
    );

    // Find the color function (should not have default implementation)
    let color_atom = typed_program.type_context.interner.intern_atom("color");
    let color_func = trait_def.find_function(color_atom).unwrap();
    assert!(
        !color_func.has_default_impl,
        "color function should not have default implementation"
    );
    assert!(
        !color_func.is_static,
        "color function should be instance function"
    );
}

#[test]
fn test_overriding_default_implementation() {
    let source = r#"
        trait Printable {
            def to_string(self: Self): String {
                "default string"
            }
            def debug(self: Self): String {
                "default debug"
            }
        }
        
        struct User(name: String) {}
        
        impl Printable for User {
            def to_string(self: Self): String {
                "custom user string"
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse trait with overriding implementation");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Overriding default implementation should succeed: {:?}",
        result.err()
    );

    // Verify that we can override some defaults while leaving others
    let typed_program = result.unwrap();

    // Check that the trait was registered
    let printable_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Printable")
        .unwrap();
    let trait_def = typed_program
        .type_context
        .trait_registry
        .get_trait(printable_trait_id)
        .unwrap();

    assert_eq!(trait_def.functions.len(), 2);

    // Both functions should have default implementations
    for func in &trait_def.functions {
        assert!(
            func.has_default_impl,
            "All functions should have default implementations"
        );
    }
}

#[test]
fn test_missing_required_function_with_defaults() {
    let source = r#"
        trait Worker {
            def work(self: Self): String {
                "working"
            }
            def name(self: Self): String
        }
        
        struct Robot(id: Integer) {}
        
        impl Worker for Robot {
            def work(self: Self): String {
                "robot working"
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse incomplete implementation");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_err(),
        "Missing required function should fail type checking"
    );

    let errors = result.unwrap_err();
    assert!(!errors.is_empty(), "Should have at least one error");

    // Should have MissingImplementation error
    let has_missing_impl = errors.iter().any(|e| {
        matches!(
            e,
            crate::error::TypeError::MissingImplementation { function_name, .. }
            if function_name == "name"
        )
    });
    assert!(
        has_missing_impl,
        "Should have missing implementation error for 'name' function"
    );
}

#[test]
fn test_multiple_default_implementations() {
    let source = r#"
        trait Vehicle {
            def start(self: Self): String {
                "starting vehicle"
            }
            def stop(self: Self): String {
                "stopping vehicle"
            }
            def honk(self: Self): String {
                "honk!"
            }
        }
        
        struct Car(make: String) {}
        
        impl Vehicle for Car {
            def start(self: Self): String {
                "starting car engine"
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse trait with multiple defaults");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Multiple default implementations should work: {:?}",
        result.err()
    );

    let typed_program = result.unwrap();

    // Verify all functions have default implementations
    let vehicle_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Vehicle")
        .unwrap();
    let trait_def = typed_program
        .type_context
        .trait_registry
        .get_trait(vehicle_trait_id)
        .unwrap();

    assert_eq!(trait_def.functions.len(), 3);

    for func in &trait_def.functions {
        assert!(
            func.has_default_impl,
            "All Vehicle functions should have default implementations"
        );
        assert!(
            !func.is_static,
            "All Vehicle functions should be instance functions"
        );
    }
}

#[test]
fn test_signature_compatibility_with_defaults() {
    let source = r#"
        trait Calculator {
            def add(self: Self, a: Integer, b: Integer): Integer {
                a + b
            }
        }
        
        struct SimpleCalc(dummy: Integer) {}
        
        impl Calculator for SimpleCalc {
            def add(self: Self, a: Integer, b: Integer): Integer {
                a + b
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse compatible override");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Compatible signature override should succeed: {:?}",
        result.err()
    );
}

#[test]
fn test_signature_mismatch_with_defaults() {
    let source = r#"
        trait Calculator {
            def multiply(self: Self, a: Integer, b: Integer): Integer {
                a * b
            }
        }
        
        struct BadCalc(dummy: Integer) {}
        
        impl Calculator for BadCalc {
            def multiply(self: Self, x: String): Integer {
                42
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse incompatible override");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_err(),
        "Incompatible signature override should fail"
    );

    let errors = result.unwrap_err();
    assert!(!errors.is_empty(), "Should have signature mismatch error");

    let has_signature_error = errors.iter().any(|e| {
        matches!(
            e,
            crate::error::TypeError::SignatureMismatch { function_name, .. }
            if function_name == "multiply"
        )
    });
    assert!(
        has_signature_error,
        "Should have signature mismatch error for 'multiply'"
    );
}

#[test]
fn test_mixed_defaults_and_static_functions() {
    let source = r#"
        trait Factory {
            defs create(name: String): String {
                "created: " + name
            }
            
            def process(self: Self): String {
                "processing"
            }
            
            def validate(self: Self): Boolean
        }
        
        struct TextFactory(dummy: Integer) {}
        
        impl Factory for TextFactory {
            def validate(self: Self): Boolean {
                true
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse mixed defaults and static");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Mixed defaults and static functions should work: {:?}",
        result.err()
    );

    let mut typed_program = result.unwrap();

    // Verify the trait structure
    let factory_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Factory")
        .unwrap();
    let trait_def = typed_program
        .type_context
        .trait_registry
        .get_trait(factory_trait_id)
        .unwrap();

    assert_eq!(trait_def.functions.len(), 3);

    // Check static vs instance and default vs required
    let create_atom = typed_program.type_context.interner.intern_atom("create");
    let process_atom = typed_program.type_context.interner.intern_atom("process");
    let validate_atom = typed_program.type_context.interner.intern_atom("validate");

    let create_func = trait_def.find_function(create_atom).unwrap();
    assert!(create_func.is_static, "create should be static");
    assert!(
        create_func.has_default_impl,
        "create should have default implementation"
    );

    let process_func = trait_def.find_function(process_atom).unwrap();
    assert!(!process_func.is_static, "process should be instance");
    assert!(
        process_func.has_default_impl,
        "process should have default implementation"
    );

    let validate_func = trait_def.find_function(validate_atom).unwrap();
    assert!(!validate_func.is_static, "validate should be instance");
    assert!(
        !validate_func.has_default_impl,
        "validate should not have default implementation"
    );
}
