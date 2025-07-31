//! Test for Phase 2: Unified Type Registry System
//!
//! Tests that the unified TypeRegistry correctly distinguishes between
//! protocols and concrete types, eliminating hardcoded type knowledge

use crate::inference::TypeInferenceEngine;
use crate::registry::TypeRegistry;

#[test]
fn test_core_type_registration() {
    let type_registry = TypeRegistry::with_core_types();

    // Test that core concrete types are registered (using is_struct)
    assert!(type_registry.is_struct("Outrun.Core.Integer64"));
    assert!(type_registry.is_struct("Outrun.Core.String"));
    assert!(type_registry.is_struct("Outrun.Core.Boolean"));
    assert!(type_registry.is_struct("Outrun.Core.Float64"));
    assert!(type_registry.is_struct("Outrun.Core.Atom"));

    // Test that generic core types are registered
    assert!(type_registry.is_struct("Outrun.Core.List"));
    assert!(type_registry.is_struct("Outrun.Core.Map"));
    assert!(type_registry.is_struct("Outrun.Core.Tuple"));
    assert!(type_registry.is_struct("Outrun.Core.Option"));
    assert!(type_registry.is_struct("Outrun.Core.Result"));

    // Test that unknown types return None (using get_module)
    assert!(type_registry.get_module("UnknownType").is_none());
    assert!(type_registry.get_module("SomeRandomProtocol").is_none());
}

#[test]
fn test_type_kind_resolution() {
    let type_registry = TypeRegistry::with_core_types();

    // Test that core protocols are recognized
    assert!(type_registry.is_protocol("Display"));
    assert!(type_registry.is_protocol("BinaryAddition"));
    
    // Test that core structs are recognized
    assert!(type_registry.is_struct("Outrun.Core.Integer64"));
    assert!(type_registry.is_struct("Outrun.Core.List"));
    
    // Test that unknown types are not found
    assert!(!type_registry.is_protocol("UnknownProtocol"));
    assert!(!type_registry.is_struct("UnknownStruct"));
    
    println!("✅ Type kind resolution working correctly");
}

#[test]
fn test_boolean_no_longer_hardcoded() {
    let mut engine = TypeInferenceEngine::new();

    // Create a simple type annotation for Boolean
    let boolean_annotation = outrun_parser::TypeAnnotation::Simple {
        path: vec![outrun_parser::TypeIdentifier {
            name: "Boolean".to_string(),
            span: outrun_parser::Span::new(0, 0),
        }],
        generic_args: None,
        span: outrun_parser::Span::new(0, 0),
    };

    // Convert the type annotation
    let result = engine.convert_type_annotation(&boolean_annotation);

    match result {
        Ok(crate::types::Type::Concrete { id, .. }) => {
            // This should now resolve to the registered Boolean concrete type
            println!("✅ Boolean resolved to concrete type: {}", id.name());
            // Without protocol registration, it should default to concrete
            assert_eq!(id.name(), "Boolean");
        }
        Ok(other_type) => {
            println!("Boolean resolved to: {:?}", other_type);
        }
        Err(e) => {
            panic!("Failed to convert Boolean type annotation: {:?}", e);
        }
    }
}

#[test]
fn test_protocol_vs_concrete_distinction() {
    let type_registry = TypeRegistry::with_core_types();

    // Test that we can distinguish protocols from structs
    assert!(type_registry.is_protocol("Display"));
    assert!(!type_registry.is_struct("Display"));
    
    assert!(type_registry.is_struct("Outrun.Core.Boolean"));
    assert!(!type_registry.is_protocol("Outrun.Core.Boolean"));

    println!("✅ Protocol vs concrete distinction working correctly");
}
