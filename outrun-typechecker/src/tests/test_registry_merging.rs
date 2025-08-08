//! Tests for registry merging functionality
//!
//! Verifies that TypeRegistry, FunctionRegistry, and UniversalDispatchRegistry
//! can be properly merged with conflict detection and resolution.

use crate::dispatch::{FunctionInfo, FunctionRegistry, FunctionVisibility};
use crate::registry::TypeRegistry;
use crate::types::{ModuleName, ProtocolDefinition, TypeModule};
use crate::universal_dispatch::{
    ClauseId, ClauseInfo, FunctionBody, FunctionSignature, Guard, UniversalDispatchRegistry,
};
use outrun_parser::Span;
use std::collections::HashSet;

#[test]
fn test_type_registry_merging_no_conflicts() {
    let mut local_registry = TypeRegistry::new();
    let mut dependency_registry = TypeRegistry::new();

    // Add different modules to each registry
    let local_protocol = create_test_protocol("LocalProtocol");
    let dependency_protocol = create_test_protocol("DependencyProtocol");

    local_registry.register_module(local_protocol).unwrap();
    dependency_registry
        .register_module(dependency_protocol)
        .unwrap();

    // Merge should succeed
    let result = local_registry.merge_with_dependency(&dependency_registry);
    assert!(result.is_ok());

    // Both modules should be present
    assert!(local_registry.is_protocol("LocalProtocol"));
    assert!(local_registry.is_protocol("DependencyProtocol"));

    // Check statistics
    let stats = local_registry.get_merge_stats();
    assert_eq!(stats.total_modules, 2);
    assert_eq!(stats.protocol_modules, 2);
}

#[test]
fn test_type_registry_merging_identical_modules() {
    let mut local_registry = TypeRegistry::new();
    let mut dependency_registry = TypeRegistry::new();

    // Add identical modules to both registries
    let protocol1 = create_test_protocol("SharedProtocol");
    let protocol2 = create_test_protocol("SharedProtocol");

    local_registry.register_module(protocol1).unwrap();
    dependency_registry.register_module(protocol2).unwrap();

    // Merge should succeed (identical definitions allowed)
    let result = local_registry.merge_with_dependency(&dependency_registry);
    assert!(result.is_ok());

    // Only one copy should exist
    assert!(local_registry.is_protocol("SharedProtocol"));
    let stats = local_registry.get_merge_stats();
    assert_eq!(stats.total_modules, 1);
}

#[test]
fn test_type_registry_merging_conflicts() {
    let mut local_registry = TypeRegistry::new();
    let mut dependency_registry = TypeRegistry::new();

    // Add conflicting modules (same name, different content)
    let local_protocol = create_test_protocol_with_arity("ConflictProtocol", 0);
    let dependency_protocol = create_test_protocol_with_arity("ConflictProtocol", 1);

    local_registry.register_module(local_protocol).unwrap();
    dependency_registry
        .register_module(dependency_protocol)
        .unwrap();

    // Merge should fail due to conflict
    let result = local_registry.merge_with_dependency(&dependency_registry);
    assert!(result.is_err());
}

#[test]
fn test_function_registry_merging_precedence() {
    let mut local_registry = FunctionRegistry::new();
    let mut dependency_registry = FunctionRegistry::new();

    // Add same function to both registries with different implementations
    let local_function = FunctionInfo {
        defining_scope: "TestModule".to_string(),
        function_name: "test_func".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![],
        return_type: crate::types::Type::concrete("String"),
        body: None,
        span: None,
        generic_parameters: vec![],
        is_generic: false,
    };

    let dependency_function = FunctionInfo {
        defining_scope: "TestModule".to_string(),
        function_name: "test_func".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![],
        return_type: crate::types::Type::concrete("Integer"),
        body: None,
        span: None,
        generic_parameters: vec![],
        is_generic: false,
    };

    local_registry.register_function(
        "TestModule".to_string(),
        "test_func".to_string(),
        local_function.clone(),
    );
    dependency_registry.register_function(
        "TestModule".to_string(),
        "test_func".to_string(),
        dependency_function,
    );

    // Merge with conflict detection
    let merge_result = local_registry.merge_with_conflict_detection(&dependency_registry);

    // Should have one conflict (local takes precedence)
    assert_eq!(merge_result.conflicts.len(), 1);
    assert_eq!(merge_result.added_functions, 0);

    // Local function should still be present
    let retrieved = local_registry
        .get_function("TestModule", "test_func")
        .unwrap();
    assert_eq!(
        retrieved.return_type,
        crate::types::Type::concrete("String")
    );
}

#[test]
fn test_function_registry_merging_new_functions() {
    let mut local_registry = FunctionRegistry::new();
    let mut dependency_registry = FunctionRegistry::new();

    // Add different functions to each registry
    let local_function = create_test_function_info();
    let dependency_function = create_test_function_info();

    local_registry.register_function(
        "LocalModule".to_string(),
        "local_func".to_string(),
        local_function,
    );
    dependency_registry.register_function(
        "DepModule".to_string(),
        "dep_func".to_string(),
        dependency_function,
    );

    // Merge should add new function
    let merge_result = local_registry.merge_with_conflict_detection(&dependency_registry);

    assert_eq!(merge_result.added_functions, 1);
    assert_eq!(merge_result.conflicts.len(), 0);

    // Both functions should be present
    assert!(local_registry
        .get_function("LocalModule", "local_func")
        .is_some());
    assert!(local_registry
        .get_function("DepModule", "dep_func")
        .is_some());
}

#[test]
fn test_universal_dispatch_registry_merging() {
    let mut local_registry = UniversalDispatchRegistry::new();
    let mut dependency_registry = UniversalDispatchRegistry::new();

    // Create test clauses
    let local_clause = create_test_clause("TestFunction", 1);
    let dependency_clause = create_test_clause("DepFunction", 2);

    local_registry.register_clause(local_clause);
    dependency_registry.register_clause(dependency_clause);

    // Merge registries
    let merge_result = local_registry.merge_with_dependency(&dependency_registry);

    assert_eq!(merge_result.added_clauses, 1);
    assert_eq!(merge_result.conflicts.len(), 0);

    // Check statistics
    let stats = local_registry.get_dispatch_stats();
    assert_eq!(stats.total_clauses, 2);
    assert_eq!(stats.dispatch_functions, 2);
}

// Helper functions for creating test data

fn create_test_protocol(name: &str) -> TypeModule {
    let protocol_def = ProtocolDefinition {
        protocol_name: ModuleName::new(name),
        required_protocols: HashSet::new(),
        defining_module: ModuleName::new(name),
        default_implementations: HashSet::new(),
        required_functions: HashSet::new(),
        span: None,
    };

    TypeModule::Protocol {
        name: ModuleName::new(name),
        definition: protocol_def,
        source_location: Span::new(0, 0),
        generic_arity: 0,
    }
}

fn create_test_protocol_with_arity(name: &str, arity: usize) -> TypeModule {
    let protocol_def = ProtocolDefinition {
        protocol_name: ModuleName::new(name),
        required_protocols: HashSet::new(),
        defining_module: ModuleName::new(name),
        default_implementations: HashSet::new(),
        required_functions: HashSet::new(),
        span: None,
    };

    TypeModule::Protocol {
        name: ModuleName::new(name),
        definition: protocol_def,
        source_location: Span::new(0, 0),
        generic_arity: arity,
    }
}

fn create_test_function_info() -> FunctionInfo {
    FunctionInfo {
        defining_scope: "TestModule".to_string(),
        function_name: "test_func".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![("param1".to_string(), crate::types::Type::concrete("String"))],
        return_type: crate::types::Type::concrete("Boolean"),
        body: None,
        span: None,
        generic_parameters: vec![],
        is_generic: false,
    }
}

fn create_test_clause(function_name: &str, priority: u32) -> ClauseInfo {
    let function_signature = FunctionSignature {
        module_path: vec!["TestModule".to_string()],
        function_name: function_name.to_string(),
    };

    let guards = vec![Guard::AlwaysTrue];

    ClauseInfo {
        clause_id: ClauseId::deterministic(&function_signature, &guards),
        function_signature,
        guards,
        body: FunctionBody::IntrinsicFunction("test_intrinsic".to_string()),
        estimated_cost: 1,
        priority,
        span: None,
    }
}
