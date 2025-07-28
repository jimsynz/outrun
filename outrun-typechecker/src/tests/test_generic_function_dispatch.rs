//! Test for Phase 3.4: Generic Function Dispatch
//!
//! Tests the enhanced FunctionDispatcher system for resolving generic function calls
//! and integrating with the MonomorphisationTable.

use crate::dispatch::{
    FunctionDispatcher, FunctionInfo, FunctionRegistry, FunctionVisibility, 
    MonomorphisationEntry, MonomorphisationTable, DispatchResult
};
use crate::registry::ProtocolRegistry;
use crate::types::{Type, TypeId};
use std::collections::HashMap;

fn create_test_registries() -> (ProtocolRegistry, FunctionRegistry) {
    let protocol_registry = ProtocolRegistry::new();
    let mut function_registry = FunctionRegistry::new();
    
    // Register a generic function: Wrapper<T> { def wrap(value: T): Wrapper<T> }
    function_registry.register_function(
        "Wrapper".to_string(),
        "wrap".to_string(),
        FunctionInfo {
            defining_scope: "Wrapper".to_string(),
            function_name: "wrap".to_string(),
            visibility: FunctionVisibility::Public,
            parameters: vec![
                ("value".to_string(), Type::Concrete {
                    id: TypeId::new("T"),
                    args: vec![],
                    span: None,
                })
            ],
            return_type: Type::Concrete {
                id: TypeId::new("Wrapper"),
                args: vec![Type::Concrete {
                    id: TypeId::new("T"),
                    args: vec![],
                    span: None,
                }],
                span: None,
            },
            body: None,
            span: None,
            generic_parameters: vec!["T".to_string()],
            is_generic: true,
        },
    );
    
    // Register a non-generic function for comparison
    function_registry.register_function(
        "Utils".to_string(),
        "identity".to_string(),
        FunctionInfo {
            defining_scope: "Utils".to_string(),
            function_name: "identity".to_string(),
            visibility: FunctionVisibility::Public,
            parameters: vec![
                ("value".to_string(), Type::Concrete {
                    id: TypeId::new("Integer64"),
                    args: vec![],
                    span: None,
                })
            ],
            return_type: Type::Concrete {
                id: TypeId::new("Integer64"),
                args: vec![],
                span: None,
            },
            body: None,
            span: None,
            generic_parameters: Vec::new(),
            is_generic: false,
        },
    );
    
    (protocol_registry, function_registry)
}

fn create_test_monomorphisation_table() -> MonomorphisationTable {
    let mut table = MonomorphisationTable::new();
    
    // Create a generic function for monomorphisation
    let generic_function = FunctionInfo {
        defining_scope: "Wrapper".to_string(),
        function_name: "wrap".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![
            ("value".to_string(), Type::Concrete {
                id: TypeId::new("T"),
                args: vec![],
                span: None,
            })
        ],
        return_type: Type::Concrete {
            id: TypeId::new("Wrapper"),
            args: vec![Type::Concrete {
                id: TypeId::new("T"),
                args: vec![],
                span: None,
            }],
            span: None,
        },
        body: None,
        span: None,
        generic_parameters: vec!["T".to_string()],
        is_generic: true,
    };
    
    // Add monomorphisation for Wrapper<Integer64>
    let mut int_substitutions = HashMap::new();
    let int_type = Type::Concrete {
        id: TypeId::new("Integer64"),
        args: vec![],
        span: None,
    };
    int_substitutions.insert("T".to_string(), int_type.clone());
    
    let int_entry = MonomorphisationEntry {
        generic_function: generic_function.clone(),
        type_substitutions: int_substitutions,
        monomorphised_key: "Wrapper.wrap:Integer64".to_string(),
        monomorphised_function: MonomorphisationTable::create_monomorphised_function(
            &generic_function,
            &HashMap::from([("T".to_string(), int_type)]),
        ).expect("Monomorphization should succeed"),
    };
    
    table.add_instantiation(int_entry);
    
    // Add monomorphisation for Wrapper<String>
    let mut string_substitutions = HashMap::new();
    let string_type = Type::Concrete {
        id: TypeId::new("String"),
        args: vec![],
        span: None,
    };
    string_substitutions.insert("T".to_string(), string_type.clone());
    
    let string_entry = MonomorphisationEntry {
        generic_function: generic_function.clone(),
        type_substitutions: string_substitutions,
        monomorphised_key: "Wrapper.wrap:String".to_string(),
        monomorphised_function: MonomorphisationTable::create_monomorphised_function(
            &generic_function,
            &HashMap::from([("T".to_string(), string_type)]),
        ).expect("Monomorphization should succeed"),
    };
    
    table.add_instantiation(string_entry);
    
    table
}

#[test]
fn test_dispatcher_creation_with_monomorphisation() {
    let (protocol_registry, function_registry) = create_test_registries();
    let mono_table = create_test_monomorphisation_table();
    
    // Test creating dispatcher with monomorphisation table
    let _dispatcher = FunctionDispatcher::new(
        &protocol_registry,
        &function_registry,
        Some(&mono_table),
        None,
    );
    
    // Verify the dispatcher was created (this is mainly a compilation test)
    // If we get here without panicking, the test passes
}

#[test]
fn test_resolve_generic_call_with_monomorphisation() {
    let (protocol_registry, function_registry) = create_test_registries();
    let mono_table = create_test_monomorphisation_table();
    
    let dispatcher = FunctionDispatcher::new(
        &protocol_registry,
        &function_registry,
        Some(&mono_table),
        None,
    );
    
    // Test resolving a monomorphised generic function call
    let int_type = Type::Concrete {
        id: TypeId::new("Integer64"),
        args: vec![],
        span: None,
    };
    
    let result = dispatcher.resolve_generic_call(
        "Wrapper",
        "wrap",
        &[&int_type],
        None,
    ).unwrap();
    
    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "Wrapper.wrap");
            assert!(!resolved_func.function_info.is_generic);
            assert!(resolved_func.function_info.generic_parameters.is_empty());
            
            // Verify the parameter type was monomorphised
            assert_eq!(resolved_func.function_info.parameters.len(), 1);
            match &resolved_func.function_info.parameters[0].1 {
                Type::Concrete { id, .. } => assert_eq!(id.name(), "Integer64"),
                _ => panic!("Expected concrete Integer64 type for parameter"),
            }
        }
        _ => panic!("Expected resolved function"),
    }
}

#[test]
fn test_resolve_generic_call_different_types() {
    let (protocol_registry, function_registry) = create_test_registries();
    let mono_table = create_test_monomorphisation_table();
    
    let dispatcher = FunctionDispatcher::new(
        &protocol_registry,
        &function_registry,
        Some(&mono_table),
        None,
    );
    
    // Test with String type
    let string_type = Type::Concrete {
        id: TypeId::new("String"),
        args: vec![],
        span: None,
    };
    
    let result = dispatcher.resolve_generic_call(
        "Wrapper",
        "wrap",
        &[&string_type],
        None,
    ).unwrap();
    
    match result {
        DispatchResult::Resolved(resolved_func) => {
            // Verify the parameter type was monomorphised to String
            match &resolved_func.function_info.parameters[0].1 {
                Type::Concrete { id, .. } => assert_eq!(id.name(), "String"),
                _ => panic!("Expected concrete String type for parameter"),
            }
            
            // Verify the return type was monomorphised to Wrapper<String>
            match &resolved_func.function_info.return_type {
                Type::Concrete { id, args, .. } => {
                    assert_eq!(id.name(), "Wrapper");
                    assert_eq!(args.len(), 1);
                    match &args[0] {
                        Type::Concrete { id, .. } => assert_eq!(id.name(), "String"),
                        _ => panic!("Expected String type for Wrapper argument"),
                    }
                }
                _ => panic!("Expected concrete Wrapper type for return type"),
            }
        }
        _ => panic!("Expected resolved function"),
    }
}

#[test]
fn test_resolve_generic_call_without_monomorphisation() {
    let (protocol_registry, function_registry) = create_test_registries();
    
    // Create dispatcher without monomorphisation table
    let dispatcher = FunctionDispatcher::new(&protocol_registry, &function_registry, None, None);
    
    let int_type = Type::Concrete {
        id: TypeId::new("Integer64"),
        args: vec![],
        span: None,
    };
    
    let result = dispatcher.resolve_generic_call(
        "Wrapper",
        "wrap",
        &[&int_type],
        None,
    ).unwrap();
    
    // Should return the generic function since no monomorphisation is available
    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert!(resolved_func.function_info.is_generic);
            assert_eq!(resolved_func.function_info.generic_parameters.len(), 1);
            assert_eq!(resolved_func.function_info.generic_parameters[0], "T");
        }
        _ => panic!("Expected resolved generic function"),
    }
}

#[test]
fn test_resolve_qualified_call_with_types() {
    let (protocol_registry, function_registry) = create_test_registries();
    let mono_table = create_test_monomorphisation_table();
    
    let dispatcher = FunctionDispatcher::new(
        &protocol_registry,
        &function_registry,
        Some(&mono_table),
        None,
    );
    
    let int_type = Type::Concrete {
        id: TypeId::new("Integer64"),
        args: vec![],
        span: None,
    };
    
    let result = dispatcher.resolve_qualified_call_with_types(
        "Wrapper.wrap",
        &[&int_type],
        None,
        None,
    ).unwrap();
    
    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "Wrapper.wrap");
            assert!(!resolved_func.function_info.is_generic);
        }
        _ => panic!("Expected resolved function"),
    }
}

#[test]
fn test_resolve_qualified_call_without_types_fallback() {
    let (protocol_registry, function_registry) = create_test_registries();
    let mono_table = create_test_monomorphisation_table();
    
    let dispatcher = FunctionDispatcher::new(
        &protocol_registry,
        &function_registry,
        Some(&mono_table),
        None,
    );
    
    // Test calling a non-generic function (should fall back to normal resolution)
    let result = dispatcher.resolve_qualified_call_with_types(
        "Utils.identity",
        &[], // No type arguments
        None,
        None,
    ).unwrap();
    
    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "Utils.identity");
            assert!(!resolved_func.function_info.is_generic);
        }
        _ => panic!("Expected resolved function"),
    }
}

#[test]
fn test_resolve_generic_call_nonexistent_monomorphisation() {
    let (protocol_registry, function_registry) = create_test_registries();
    let mono_table = create_test_monomorphisation_table();
    
    let dispatcher = FunctionDispatcher::new(
        &protocol_registry,
        &function_registry,
        Some(&mono_table),
        None,
    );
    
    // Try to resolve with a type that doesn't have a monomorphisation
    let boolean_type = Type::Concrete {
        id: TypeId::new("Boolean"),
        args: vec![],
        span: None,
    };
    
    let result = dispatcher.resolve_generic_call(
        "Wrapper",
        "wrap",
        &[&boolean_type],
        None,
    ).unwrap();
    
    // Should return the generic function since no monomorphisation is available for Boolean
    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert!(resolved_func.function_info.is_generic);
            assert_eq!(resolved_func.function_info.generic_parameters[0], "T");
        }
        _ => panic!("Expected resolved generic function"),
    }
}

#[test]
fn test_resolve_generic_call_nonexistent_function() {
    let (protocol_registry, function_registry) = create_test_registries();
    let mono_table = create_test_monomorphisation_table();
    
    let dispatcher = FunctionDispatcher::new(
        &protocol_registry,
        &function_registry,
        Some(&mono_table),
        None,
    );
    
    let int_type = Type::Concrete {
        id: TypeId::new("Integer64"),
        args: vec![],
        span: None,
    };
    
    // Try to resolve a function that doesn't exist
    let result = dispatcher.resolve_generic_call(
        "NonExistent",
        "missing",
        &[&int_type],
        None,
    );
    
    // Should return an error since the function doesn't exist
    assert!(result.is_err());
}