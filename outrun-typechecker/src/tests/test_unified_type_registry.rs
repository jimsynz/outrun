//! Test for Phase 2: Unified Type Registry System
//!
//! Tests that the unified TypeRegistry correctly distinguishes between
//! protocols and concrete types, eliminating hardcoded type knowledge

use crate::inference::TypeInferenceEngine;
use crate::registry::{TypeKind, TypeRegistry};

#[test]
fn test_core_type_registration() {
    let type_registry = TypeRegistry::with_core_types();
    
    // Test that core concrete types are registered
    assert!(type_registry.is_concrete_type("Outrun.Core.Integer64"));
    assert!(type_registry.is_concrete_type("Outrun.Core.String"));
    assert!(type_registry.is_concrete_type("Outrun.Core.Boolean"));
    assert!(type_registry.is_concrete_type("Outrun.Core.Float64"));
    assert!(type_registry.is_concrete_type("Outrun.Core.Atom"));
    
    // Test that generic core types are registered
    assert!(type_registry.is_concrete_type("Outrun.Core.List"));
    assert!(type_registry.is_concrete_type("Outrun.Core.Map"));
    assert!(type_registry.is_concrete_type("Outrun.Core.Tuple"));
    assert!(type_registry.is_concrete_type("Outrun.Core.Option"));
    assert!(type_registry.is_concrete_type("Outrun.Core.Result"));
    
    // Test that unknown types return None
    assert!(type_registry.get_type_kind("UnknownType").is_none());
    assert!(type_registry.get_type_kind("SomeRandomProtocol").is_none());
}

#[test]
fn test_type_kind_resolution() {
    let mut type_registry = TypeRegistry::with_core_types();
    
    // Register a test protocol
    type_registry.protocol_registry_mut()
        .register_protocol_definition(
            crate::types::ProtocolId::new("TestProtocol"),
            std::collections::HashSet::new(),
            crate::types::ModuleId::new("TestModule"),
            std::collections::HashSet::new(),
            std::collections::HashSet::new(),
            None,
        );
    
    // Test protocol resolution
    match type_registry.get_type_kind("TestProtocol") {
        Some(TypeKind::Protocol(_)) => {
            println!("✅ TestProtocol correctly identified as protocol");
        }
        other => panic!("Expected protocol, got: {:?}", other),
    }
    
    // Test concrete type resolution
    match type_registry.get_type_kind("Outrun.Core.Integer64") {
        Some(TypeKind::ConcreteType(def)) => {
            assert_eq!(def.type_id.name(), "Outrun.Core.Integer64");
            assert!(!def.is_generic);
            println!("✅ Outrun.Core.Integer64 correctly identified as concrete type");
        }
        other => panic!("Expected concrete type, got: {:?}", other),
    }
    
    // Test generic concrete type resolution
    match type_registry.get_type_kind("Outrun.Core.List") {
        Some(TypeKind::ConcreteType(def)) => {
            assert_eq!(def.type_id.name(), "Outrun.Core.List");
            assert!(def.is_generic);
            println!("✅ Outrun.Core.List correctly identified as generic concrete type");
        }
        other => panic!("Expected generic concrete type, got: {:?}", other),
    }
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
    let mut engine = TypeInferenceEngine::new();
    
    // Add Boolean as a protocol to the registry
    engine.type_registry_mut().protocol_registry_mut()
        .register_protocol_definition(
            crate::types::ProtocolId::new("Boolean"),
            std::collections::HashSet::new(),
            crate::types::ModuleId::new("Boolean"),
            std::collections::HashSet::new(),
            std::collections::HashSet::new(),
            None,
        );
    
    let boolean_annotation = outrun_parser::TypeAnnotation::Simple {
        path: vec![outrun_parser::TypeIdentifier {
            name: "Boolean".to_string(),
            span: outrun_parser::Span::new(0, 0),
        }],
        generic_args: None,
        span: outrun_parser::Span::new(0, 0),
    };
    
    let result = engine.convert_type_annotation(&boolean_annotation);
    
    match result {
        Ok(crate::types::Type::Protocol { id, .. }) => {
            println!("✅ Boolean correctly resolved as protocol: {}", id.0);
            assert_eq!(id.0, "Boolean");
        }
        Ok(other_type) => {
            panic!("Expected protocol type, got: {:?}", other_type);
        }
        Err(e) => {
            panic!("Failed to convert Boolean type annotation: {:?}", e);
        }
    }
}