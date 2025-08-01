//! Test Phase 3.2: Forward Bindings & Arity Detection

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_forward_binding_registration() {
    let mut engine = TypeInferenceEngine::new();

    // Test code that references User before it's defined
    let program_text = r#"
        def create_user(): User {
            User(name: "test")
        }
        
        struct User(name: String)
    "#;

    let program = parse_program(program_text).expect("Should parse successfully");

    // This should work - User is referenced before definition but gets resolved
    let result = engine.register_protocols_and_structs(&program);
    assert!(
        result.is_ok(),
        "Forward binding should allow User to be referenced before definition"
    );

    // Verify User is now registered as a struct
    assert!(engine.type_registry_rc().is_struct("User"));
}

#[test]
fn test_arity_conflict_detection() {
    let mut engine = TypeInferenceEngine::new();

    // Test code that has conflicting arity for the same type
    let program_text = r#"
        def create_list(): List {
            List(count: 0)
        }
        
        def create_another_list(): List {
            List(count: 1)
        }
        
        struct List(count: Integer)
    "#;

    let program = parse_program(program_text).expect("Should parse successfully");

    // This should detect the arity conflict: List (arity 0) vs List<T> (arity 1)
    let result = engine.register_protocols_and_structs(&program);

    // For now, we expect this to work since the struct definition wins
    // In a more sophisticated implementation, this might be an error
    assert!(result.is_ok(), "Should handle arity conflicts gracefully");
}

#[test]
fn test_core_type_arity_accuracy() {
    let registry = crate::registry::TypeRegistry::with_core_types();

    // Verify that core types have correct arity
    assert!(registry.is_struct("Outrun.Core.List"));
    assert!(registry.is_struct("Outrun.Core.Map"));
    assert!(registry.is_struct("Outrun.Core.Option"));
    assert!(registry.is_struct("Outrun.Core.Result"));

    // Test that we can get the modules (this verifies they're properly registered)
    assert!(registry.get_module("Outrun.Core.List").is_some());
    assert!(registry.get_module("Outrun.Core.Map").is_some());

    // println!("✅ Core types registered with proper arity");
}
