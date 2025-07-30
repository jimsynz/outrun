//! Test for Phase 3.4: Dispatch Table Integration with Monomorphisation
//!
//! Tests the build_dispatch_table_with_monomorphisation function and integration
//! of monomorphised functions into the dispatch table.

use crate::dispatch::{
    build_dispatch_table, FunctionInfo, FunctionRegistry, FunctionVisibility,
    MonomorphisationEntry, MonomorphisationTable,
};
use crate::registry::ProtocolRegistry;
use crate::types::{Type, TypeId};
use std::collections::HashMap;

fn create_test_setup() -> (ProtocolRegistry, FunctionRegistry, MonomorphisationTable) {
    let protocol_registry = ProtocolRegistry::new();
    let mut function_registry = FunctionRegistry::new();
    let mut mono_table = MonomorphisationTable::new();

    // Register a generic function
    let generic_function = FunctionInfo {
        defining_scope: "Container".to_string(),
        function_name: "store".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![(
            "item".to_string(),
            Type::Concrete {
                id: TypeId::new("T"),
                args: vec![],
                span: None,
            },
        )],
        return_type: Type::Concrete {
            id: TypeId::new("Container"),
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

    function_registry.register_function(
        "Container".to_string(),
        "store".to_string(),
        generic_function.clone(),
    );

    // Add monomorphisations for Integer64 and String
    for type_name in &["Integer64", "String"] {
        let concrete_type = Type::Concrete {
            id: TypeId::new(*type_name),
            args: vec![],
            span: None,
        };

        let mut substitutions = HashMap::new();
        substitutions.insert("T".to_string(), concrete_type.clone());

        let mono_entry = MonomorphisationEntry {
            generic_function: generic_function.clone(),
            type_substitutions: substitutions,
            monomorphised_key: format!("Container.store:{}", type_name),
            monomorphised_function: MonomorphisationTable::create_monomorphised_function(
                &generic_function,
                &HashMap::from([("T".to_string(), concrete_type)]),
            )
            .expect("Monomorphization should succeed"),
        };

        mono_table.add_instantiation(mono_entry);
    }

    (protocol_registry, function_registry, mono_table)
}

#[test]
fn test_build_dispatch_table_without_monomorphisation() {
    let (protocol_registry, function_registry, _) = create_test_setup();

    // Build dispatch table without monomorphisation
    let dispatch_table = build_dispatch_table(&protocol_registry, &function_registry, None);

    // Should only contain protocol implementation entries, no monomorphised functions
    // Since we don't have any protocol implementations in this test, it should be empty
    assert!(dispatch_table.is_empty());
}

#[test]
fn test_build_dispatch_table_with_monomorphisation() {
    let (protocol_registry, function_registry, mono_table) = create_test_setup();

    // Build dispatch table with monomorphisation
    let dispatch_table =
        build_dispatch_table(&protocol_registry, &function_registry, Some(&mono_table));

    // Should contain monomorphised entries
    assert!(!dispatch_table.is_empty());
    assert_eq!(dispatch_table.len(), 2); // Integer64 and String instantiations

    // Verify specific entries exist
    assert!(dispatch_table.lookup("Container.store:Integer64").is_some());
    assert!(dispatch_table.lookup("Container.store:String").is_some());
}

#[test]
fn test_monomorphised_entries_have_correct_function_info() {
    let (protocol_registry, function_registry, mono_table) = create_test_setup();

    let dispatch_table =
        build_dispatch_table(&protocol_registry, &function_registry, Some(&mono_table));

    // Check the Integer64 instantiation
    let int_entry = dispatch_table.lookup("Container.store:Integer64").unwrap();
    assert_eq!(int_entry.qualified_name, "Container.store");
    assert!(int_entry.implementing_type.is_none()); // Struct functions don't have implementing types
    assert!(!int_entry.function_info.is_generic);
    assert!(int_entry.function_info.generic_parameters.is_empty());

    // Verify parameter types were substituted
    assert_eq!(int_entry.function_info.parameters.len(), 1);
    match &int_entry.function_info.parameters[0].1 {
        Type::Concrete { id, .. } => assert_eq!(id.name(), "Integer64"),
        _ => panic!("Expected concrete Integer64 type"),
    }

    // Verify return type was substituted
    match &int_entry.function_info.return_type {
        Type::Concrete { id, args, .. } => {
            assert_eq!(id.name(), "Container");
            assert_eq!(args.len(), 1);
            match &args[0] {
                Type::Concrete { id, .. } => assert_eq!(id.name(), "Integer64"),
                _ => panic!("Expected Integer64 type for Container argument"),
            }
        }
        _ => panic!("Expected concrete Container type"),
    }
}

#[test]
fn test_monomorphised_entries_different_types() {
    let (protocol_registry, function_registry, mono_table) = create_test_setup();

    let dispatch_table =
        build_dispatch_table(&protocol_registry, &function_registry, Some(&mono_table));

    // Check both instantiations exist and are different
    let int_entry = dispatch_table.lookup("Container.store:Integer64").unwrap();
    let string_entry = dispatch_table.lookup("Container.store:String").unwrap();

    // Both should have the same function name but different parameter types
    assert_eq!(int_entry.qualified_name, string_entry.qualified_name);

    // Parameter types should be different
    match (
        &int_entry.function_info.parameters[0].1,
        &string_entry.function_info.parameters[0].1,
    ) {
        (Type::Concrete { id: id1, .. }, Type::Concrete { id: id2, .. }) => {
            assert_eq!(id1.name(), "Integer64");
            assert_eq!(id2.name(), "String");
        }
        _ => panic!("Expected concrete types for both parameters"),
    }
}

#[test]
fn test_dispatch_table_entries_are_iterable() {
    let (protocol_registry, function_registry, mono_table) = create_test_setup();

    let dispatch_table =
        build_dispatch_table(&protocol_registry, &function_registry, Some(&mono_table));

    // Collect all entries
    let entries: Vec<_> = dispatch_table.entries().iter().collect();
    assert_eq!(entries.len(), 2);

    // Verify keys exist
    let keys: Vec<&String> = entries.iter().map(|(key, _)| *key).collect();
    assert!(keys.contains(&&"Container.store:Integer64".to_string()));
    assert!(keys.contains(&&"Container.store:String".to_string()));
}

#[test]
fn test_empty_monomorphisation_table() {
    let (protocol_registry, function_registry, _) = create_test_setup();
    let empty_mono_table = MonomorphisationTable::new();

    let dispatch_table = build_dispatch_table(
        &protocol_registry,
        &function_registry,
        Some(&empty_mono_table),
    );

    // Should be empty since the monomorphisation table is empty
    assert!(dispatch_table.is_empty());
}

#[test]
fn test_complex_generic_types_in_dispatch_table() {
    let protocol_registry = ProtocolRegistry::new();
    let mut function_registry = FunctionRegistry::new();
    let mut mono_table = MonomorphisationTable::new();

    // Create a function with complex generic types: Map<K, List<V>>
    let generic_function = FunctionInfo {
        defining_scope: "Database".to_string(),
        function_name: "store_many".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![
            (
                "key".to_string(),
                Type::Concrete {
                    id: TypeId::new("K"),
                    args: vec![],
                    span: None,
                },
            ),
            (
                "values".to_string(),
                Type::Concrete {
                    id: TypeId::new("List"),
                    args: vec![Type::Concrete {
                        id: TypeId::new("V"),
                        args: vec![],
                        span: None,
                    }],
                    span: None,
                },
            ),
        ],
        return_type: Type::Concrete {
            id: TypeId::new("Map"),
            args: vec![
                Type::Concrete {
                    id: TypeId::new("K"),
                    args: vec![],
                    span: None,
                },
                Type::Concrete {
                    id: TypeId::new("List"),
                    args: vec![Type::Concrete {
                        id: TypeId::new("V"),
                        args: vec![],
                        span: None,
                    }],
                    span: None,
                },
            ],
            span: None,
        },
        body: None,
        span: None,
        generic_parameters: vec!["K".to_string(), "V".to_string()],
        is_generic: true,
    };

    function_registry.register_function(
        "Database".to_string(),
        "store_many".to_string(),
        generic_function.clone(),
    );

    // Create substitutions: K = String, V = Integer64
    let mut substitutions = HashMap::new();
    substitutions.insert(
        "K".to_string(),
        Type::Concrete {
            id: TypeId::new("String"),
            args: vec![],
            span: None,
        },
    );
    substitutions.insert(
        "V".to_string(),
        Type::Concrete {
            id: TypeId::new("Integer64"),
            args: vec![],
            span: None,
        },
    );

    let mono_entry = MonomorphisationEntry {
        generic_function: generic_function.clone(),
        type_substitutions: substitutions.clone(),
        monomorphised_key: "Database.store_many:String:List_Integer64".to_string(),
        monomorphised_function: MonomorphisationTable::create_monomorphised_function(
            &generic_function,
            &substitutions,
        )
        .expect("Monomorphization should succeed"),
    };

    mono_table.add_instantiation(mono_entry);

    let dispatch_table =
        build_dispatch_table(&protocol_registry, &function_registry, Some(&mono_table));

    // Verify the complex type entry exists
    let entry = dispatch_table.lookup("Database.store_many:String:List_Integer64");
    assert!(entry.is_some());

    let resolved_func = entry.unwrap();
    assert_eq!(resolved_func.qualified_name, "Database.store_many");
    assert!(!resolved_func.function_info.is_generic);
}
