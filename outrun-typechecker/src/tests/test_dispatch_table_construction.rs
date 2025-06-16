//! Integration tests for dispatch table construction
//!
//! Tests the complete flow from trait implementations through dispatch table
//! construction to function resolution.

use crate::checker::TypeChecker;
use crate::dispatch::resolution::{resolve_static_function, resolve_trait_function};
use outrun_parser::parse_program;

#[test]
fn test_dispatch_table_construction_with_trait_implementations() {
    let source = r#"
        trait Display {
            def to_string(self: Self): String
        }
        
        struct User(name: String) {}
        
        impl Display for User {
            def to_string(self: Self): String {
                "User implementation"
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse program with trait implementation");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Program with trait implementation should type check and build dispatch table: {:?}",
        result.err()
    );

    let typed_program = result.unwrap();

    // Verify dispatch table was built
    let stats = typed_program.dispatch_table.stats();
    assert!(
        stats.trait_implementations > 0,
        "Dispatch table should contain trait implementations"
    );

    // Verify we can resolve the trait implementation
    let display_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Display")
        .expect("Display trait should exist");
    let user_type_id = typed_program
        .type_context
        .interner
        .get_type("User")
        .expect("User type should exist");

    let module_id = typed_program
        .dispatch_table
        .lookup_trait_impl(display_trait_id, user_type_id);
    assert!(
        module_id.is_some(),
        "Should find trait implementation in dispatch table"
    );
}

#[test]
fn test_dispatch_table_construction_with_static_functions() {
    let source = r#"
        trait Result {
            defs ok(value: String): String {
                "ok: " + value
            }
            
            defs error(message: String): String {
                "error: " + message
            }
            
            def is_ok?(self: Self): Boolean
        }
        
        struct MyResult(value: String) {}
        
        impl Result for MyResult {
            def is_ok?(self: Self): Boolean {
                true
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse program with static functions");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Program with static functions should type check and build dispatch table: {:?}",
        result.err()
    );

    let typed_program = result.unwrap();

    // Verify dispatch table was built with static functions
    let stats = typed_program.dispatch_table.stats();
    assert!(
        stats.static_functions > 0,
        "Dispatch table should contain static functions"
    );

    // Verify we can look up static functions
    let result_type_id = typed_program
        .type_context
        .interner
        .get_type("Result")
        .expect("Result type should exist for static function lookup");

    let ok_function = typed_program
        .dispatch_table
        .lookup_static_function(result_type_id, "ok");
    assert!(ok_function.is_some(), "Should find 'ok' static function");

    let error_function = typed_program
        .dispatch_table
        .lookup_static_function(result_type_id, "error");
    assert!(
        error_function.is_some(),
        "Should find 'error' static function"
    );
}

#[test]
fn test_trait_function_resolution() {
    let source = r#"
        trait Printable {
            def print(self: Self): String
        }
        
        struct Document(content: String) {}
        
        impl Printable for Document {
            def print(self: Self): String {
                "Printing document"
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse program");
    let mut checker = TypeChecker::new();
    let typed_program = checker.check_program(&program).expect("Should type check");

    // Get IDs for resolution
    let printable_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Printable")
        .expect("Printable trait should exist");
    let document_type_id = typed_program
        .type_context
        .interner
        .get_type("Document")
        .expect("Document type should exist");
    let print_atom_id = typed_program
        .type_context
        .interner
        .get_atom("print")
        .expect("print atom should exist");

    // Test trait function resolution
    let result = resolve_trait_function(
        &typed_program.dispatch_table,
        printable_trait_id,
        document_type_id,
        print_atom_id,
        outrun_parser::Span::new(0, 0),
    );

    assert!(
        result.is_ok(),
        "Should successfully resolve trait function: {:?}",
        result.err()
    );
}

#[test]
fn test_static_function_resolution() {
    let source = r#"
        trait Factory {
            defs create(name: String): String {
                "created: " + name
            }
            
            def process(self: Self): String
        }
        
        struct Widget(name: String) {}
        
        impl Factory for Widget {
            def process(self: Self): String {
                "processing widget"
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse program");
    let mut checker = TypeChecker::new();
    let typed_program = checker.check_program(&program).expect("Should type check");

    // Get type ID for static function lookup
    let factory_type_id = typed_program
        .type_context
        .interner
        .get_type("Factory")
        .expect("Factory type should exist for static functions");

    // Test static function resolution
    let result = resolve_static_function(
        &typed_program.dispatch_table,
        factory_type_id,
        "create".to_string(),
        outrun_parser::Span::new(0, 0),
    );

    assert!(
        result.is_ok(),
        "Should successfully resolve static function: {:?}",
        result.err()
    );
}

#[test]
fn test_trait_function_resolution_missing_implementation() {
    let source = r#"
        trait Display {
            def to_string(self: Self): String
        }
        
        struct User(name: String) {}
        
    "#;

    let program = parse_program(source).expect("Should parse program");
    let mut checker = TypeChecker::new();
    let typed_program = checker.check_program(&program).expect("Should type check");

    // Get IDs for resolution
    let display_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Display")
        .expect("Display trait should exist");
    let user_type_id = typed_program
        .type_context
        .interner
        .get_type("User")
        .expect("User type should exist");
    let to_string_atom_id = typed_program
        .type_context
        .interner
        .get_atom("to_string")
        .expect("to_string atom should exist");

    // Test trait function resolution with missing implementation
    let result = resolve_trait_function(
        &typed_program.dispatch_table,
        display_trait_id,
        user_type_id,
        to_string_atom_id,
        outrun_parser::Span::new(0, 0),
    );

    assert!(
        result.is_err(),
        "Should fail to resolve trait function for missing implementation"
    );

    // Verify it's the correct error type
    match result {
        Err(crate::error::TypeError::TraitNotImplemented { .. }) => {
            // Expected error type
        }
        other => assert!(
            matches!(
                other,
                Err(crate::error::TypeError::TraitNotImplemented { .. })
            ),
            "Expected TraitNotImplemented error, got: {:?}",
            other
        ),
    }
}

#[test]
fn test_static_function_resolution_missing_function() {
    let source = r#"
        trait SimpleFactory {
            defs basic(): String {
                "basic"
            }
            
            def process(self: Self): String
        }
        
        struct Item(id: Integer) {}
        
        impl SimpleFactory for Item {
            def process(self: Self): String {
                "processing item"
            }
        }
    "#;

    let program = parse_program(source).expect("Should parse program");
    let mut checker = TypeChecker::new();
    let typed_program = checker.check_program(&program).expect("Should type check");

    // Get type ID for static function lookup
    let factory_type_id = typed_program
        .type_context
        .interner
        .get_type("SimpleFactory")
        .expect("SimpleFactory type should exist");

    // Test static function resolution with missing function
    let result = resolve_static_function(
        &typed_program.dispatch_table,
        factory_type_id,
        "nonexistent".to_string(),
        outrun_parser::Span::new(0, 0),
    );

    assert!(
        result.is_err(),
        "Should fail to resolve nonexistent static function"
    );

    // Verify it's the correct error type
    match result {
        Err(crate::error::TypeError::UndefinedFunction { name, .. }) => {
            assert_eq!(name, "nonexistent");
        }
        other => assert!(
            matches!(
                other,
                Err(crate::error::TypeError::UndefinedFunction { .. })
            ),
            "Expected UndefinedFunction error, got: {:?}",
            other
        ),
    }
}
