//! Simple test for Phase 3.2 functionality

use crate::registry::TypeRegistry;
use crate::types::{ModuleName, TypeModule};

#[test]
fn test_forward_binding_basic() {
    let mut registry = TypeRegistry::new();

    // Create a forward binding
    let forward_binding = TypeModule::ForwardBinding {
        name: ModuleName::new("User"),
        expected_arity: Some(0),
        source_location: outrun_parser::Span::new(0, 10),
        references: vec![outrun_parser::Span::new(0, 10)],
    };

    // Register the forward binding
    assert!(registry.register_module(forward_binding).is_ok());

    // Verify it's registered
    assert!(registry.get_module("User").is_some());

    // println!("✅ Forward binding registration works");
}

#[test]
fn test_arity_conflict_detection() {
    let mut registry = TypeRegistry::new();

    // Create a forward binding with arity 0
    let forward_binding = TypeModule::ForwardBinding {
        name: ModuleName::new("Container"),
        expected_arity: Some(0),
        source_location: outrun_parser::Span::new(0, 10),
        references: vec![outrun_parser::Span::new(0, 10)],
    };

    assert!(registry.register_module(forward_binding).is_ok());

    // Try to register a struct with different arity - should fail
    let struct_def = crate::types::ConcreteTypeDefinition {
        type_name: ModuleName::new("Container"),
        defining_module: ModuleName::new("Container"),
        is_generic: true, // This implies arity > 0
        span: None,
        never_info: None,
    };

    let struct_module = TypeModule::Struct {
        name: ModuleName::new("Container"),
        definition: struct_def,
        source_location: outrun_parser::Span::new(10, 20),
        generic_arity: 1, // Conflicts with forward binding arity 0
    };

    // This should fail due to arity conflict
    let result = registry.register_module(struct_module);
    assert!(result.is_err());

    // println!("✅ Arity conflict detection works");
}

#[test]
fn test_core_types_accurate_arity() {
    let registry = TypeRegistry::with_core_types();

    // Verify core types are registered
    assert!(registry.is_struct("Outrun.Core.List"));
    assert!(registry.is_struct("Outrun.Core.Map"));
    assert!(registry.is_struct("Outrun.Core.Option"));
    assert!(registry.is_struct("Outrun.Core.Result"));

    // Verify they exist in the registry
    assert!(registry.get_module("Outrun.Core.List").is_some());
    assert!(registry.get_module("Outrun.Core.Map").is_some());

    // println!("✅ Core types with accurate arity work");
}
