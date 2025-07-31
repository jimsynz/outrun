//! Test for Phase 3.3: Monomorphisation Table
//!
//! Tests the MonomorphisationTable system for tracking generic function instantiations
//! and generating monomorphised function entries for dispatch.

use crate::dispatch::{
    FunctionInfo, FunctionVisibility, MonomorphisationEntry, MonomorphisationTable,
};
use crate::types::{Type};
use std::collections::HashMap;

#[test]
fn test_monomorphisation_table_creation() {
    let table = MonomorphisationTable::new();

    assert!(table.is_empty());
    assert_eq!(table.instantiation_count(), 0);
}

#[test]
fn test_monomorphised_key_generation() {
    // Test simple function without type arguments
    let key = MonomorphisationTable::generate_key("Wrapper", "wrap", &[]);
    assert_eq!(key, "Wrapper.wrap");

    // Test function with single type argument
    let integer_type = Type::Concrete {
        id: ModuleName::new("Integer64"),
        args: vec![],
        span: None,
    };
    let key = MonomorphisationTable::generate_key("Wrapper", "wrap", &[&integer_type]);
    assert_eq!(key, "Wrapper.wrap:Integer64");

    // Test function with multiple type arguments
    let string_type = Type::Concrete {
        id: ModuleName::new("String"),
        args: vec![],
        span: None,
    };
    let key = MonomorphisationTable::generate_key("Map", "put", &[&string_type, &integer_type]);
    assert_eq!(key, "Map.put:String:Integer64");
}

#[test]
fn test_monomorphised_key_with_generic_types() {
    // Test List<Integer64>
    let integer_type = Type::Concrete {
        id: ModuleName::new("Integer64"),
        args: vec![],
        span: None,
    };
    let list_integer_type = Type::Concrete {
        id: ModuleName::new("List"),
        args: vec![integer_type.clone()],
        span: None,
    };
    let key = MonomorphisationTable::generate_key("Collection", "process", &[&list_integer_type]);
    assert_eq!(key, "Collection.process:List_Integer64");

    // Test Map<String, Integer64>
    let string_type = Type::Concrete {
        id: ModuleName::new("String"),
        args: vec![],
        span: None,
    };
    let map_type = Type::Concrete {
        id: ModuleName::new("Map"),
        args: vec![string_type.clone(), integer_type.clone()],
        span: None,
    };
    let key = MonomorphisationTable::generate_key("Database", "store", &[&map_type]);
    assert_eq!(key, "Database.store:Map_String_Integer64");
}

#[test]
fn test_add_and_lookup_instantiation() {
    let mut table = MonomorphisationTable::new();

    // Create a generic function: Wrapper<T> { def wrap(value: T): Wrapper<T> }
    let generic_function = FunctionInfo {
        defining_scope: "Wrapper".to_string(),
        function_name: "wrap".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![(
            "value".to_string(),
            Type::Concrete {
                id: ModuleName::new("T"),
                args: vec![],
                span: None,
            },
        )],
        return_type: Type::Concrete {
            id: ModuleName::new("Wrapper"),
            args: vec![Type::Concrete {
                id: ModuleName::new("T"),
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

    // Create type substitutions for T = Integer64
    let mut type_substitutions = HashMap::new();
    let integer_type = Type::Concrete {
        id: ModuleName::new("Integer64"),
        args: vec![],
        span: None,
    };
    type_substitutions.insert("T".to_string(), integer_type.clone());

    // Create monomorphised function
    let monomorphised_function = MonomorphisationTable::create_monomorphised_function(
        &generic_function,
        &type_substitutions,
    )
    .expect("Monomorphization should succeed");

    // Verify monomorphised function has concrete types
    assert!(!monomorphised_function.is_generic);
    assert!(monomorphised_function.generic_parameters.is_empty());

    // Check parameter types were substituted
    assert_eq!(monomorphised_function.parameters.len(), 1);
    match &monomorphised_function.parameters[0].1 {
        Type::Concrete { id, .. } => assert_eq!(id.name(), "Integer64"),
        _ => panic!("Expected concrete Integer64 type for parameter"),
    }

    // Check return type was substituted
    match &monomorphised_function.return_type {
        Type::Concrete { id, args, .. } => {
            assert_eq!(id.name(), "Wrapper");
            assert_eq!(args.len(), 1);
            match &args[0] {
                Type::Concrete { id, .. } => assert_eq!(id.name(), "Integer64"),
                _ => panic!("Expected concrete Integer64 type for return type argument"),
            }
        }
        _ => panic!("Expected concrete Wrapper type for return type"),
    }

    // Create and add monomorphisation entry
    let key = "Wrapper.wrap:Integer64".to_string();
    let entry = MonomorphisationEntry {
        generic_function: generic_function.clone(),
        type_substitutions,
        monomorphised_key: key.clone(),
        monomorphised_function,
    };

    table.add_instantiation(entry);

    // Test table state
    assert!(!table.is_empty());
    assert_eq!(table.instantiation_count(), 1);
    assert!(table.has_instantiation(&key));

    // Test lookup
    let retrieved = table.get_instantiation(&key);
    assert!(retrieved.is_some());

    let retrieved_entry = retrieved.unwrap();
    assert_eq!(retrieved_entry.monomorphised_key, key);
    assert_eq!(retrieved_entry.generic_function.function_name, "wrap");
    assert!(!retrieved_entry.monomorphised_function.is_generic);
}

#[test]
fn test_multiple_instantiations_same_function() {
    let mut table = MonomorphisationTable::new();

    // Create generic function: Container<T> { def store(value: T): Container<T> }
    let generic_function = FunctionInfo {
        defining_scope: "Container".to_string(),
        function_name: "store".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![(
            "value".to_string(),
            Type::Concrete {
                id: ModuleName::new("T"),
                args: vec![],
                span: None,
            },
        )],
        return_type: Type::Concrete {
            id: ModuleName::new("Container"),
            args: vec![Type::Concrete {
                id: ModuleName::new("T"),
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

    // Add instantiation for T = Integer64
    let mut int_substitutions = HashMap::new();
    int_substitutions.insert(
        "T".to_string(),
        Type::Concrete {
            id: ModuleName::new("Integer64"),
            args: vec![],
            span: None,
        },
    );

    let int_entry = MonomorphisationEntry {
        generic_function: generic_function.clone(),
        type_substitutions: int_substitutions,
        monomorphised_key: "Container.store:Integer64".to_string(),
        monomorphised_function: MonomorphisationTable::create_monomorphised_function(
            &generic_function,
            &HashMap::from([(
                "T".to_string(),
                Type::Concrete {
                    id: ModuleName::new("Integer64"),
                    args: vec![],
                    span: None,
                },
            )]),
        )
        .expect("Monomorphization should succeed"),
    };

    table.add_instantiation(int_entry);

    // Add instantiation for T = String
    let mut string_substitutions = HashMap::new();
    string_substitutions.insert(
        "T".to_string(),
        Type::Concrete {
            id: ModuleName::new("String"),
            args: vec![],
            span: None,
        },
    );

    let string_entry = MonomorphisationEntry {
        generic_function: generic_function.clone(),
        type_substitutions: string_substitutions,
        monomorphised_key: "Container.store:String".to_string(),
        monomorphised_function: MonomorphisationTable::create_monomorphised_function(
            &generic_function,
            &HashMap::from([(
                "T".to_string(),
                Type::Concrete {
                    id: ModuleName::new("String"),
                    args: vec![],
                    span: None,
                },
            )]),
        )
        .expect("Monomorphization should succeed"),
    };

    table.add_instantiation(string_entry);

    // Test table state
    assert_eq!(table.instantiation_count(), 2);
    assert!(table.has_instantiation("Container.store:Integer64"));
    assert!(table.has_instantiation("Container.store:String"));

    // Test getting all instantiations for the function
    let instantiations = table.get_function_instantiations("Container", "store");
    assert_eq!(instantiations.len(), 2);

    let keys: Vec<String> = instantiations
        .iter()
        .map(|entry| entry.monomorphised_key.clone())
        .collect();
    assert!(keys.contains(&"Container.store:Integer64".to_string()));
    assert!(keys.contains(&"Container.store:String".to_string()));
}

#[test]
fn test_type_substitution_in_complex_types() {
    // Test substitution in nested generic types like Map<K, List<V>>
    let generic_function = FunctionInfo {
        defining_scope: "Database".to_string(),
        function_name: "store_values".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![
            (
                "key".to_string(),
                Type::Concrete {
                    id: ModuleName::new("K"),
                    args: vec![],
                    span: None,
                },
            ),
            (
                "values".to_string(),
                Type::Concrete {
                    id: ModuleName::new("List"),
                    args: vec![Type::Concrete {
                        id: ModuleName::new("V"),
                        args: vec![],
                        span: None,
                    }],
                    span: None,
                },
            ),
        ],
        return_type: Type::Concrete {
            id: ModuleName::new("Map"),
            args: vec![
                Type::Concrete {
                    id: ModuleName::new("K"),
                    args: vec![],
                    span: None,
                },
                Type::Concrete {
                    id: ModuleName::new("List"),
                    args: vec![Type::Concrete {
                        id: ModuleName::new("V"),
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

    // Create substitutions: K = String, V = Integer64
    let mut substitutions = HashMap::new();
    substitutions.insert(
        "K".to_string(),
        Type::Concrete {
            id: ModuleName::new("String"),
            args: vec![],
            span: None,
        },
    );
    substitutions.insert(
        "V".to_string(),
        Type::Concrete {
            id: ModuleName::new("Integer64"),
            args: vec![],
            span: None,
        },
    );

    let monomorphised =
        MonomorphisationTable::create_monomorphised_function(&generic_function, &substitutions)
            .expect("Monomorphization should succeed");

    // Verify parameter substitutions
    assert_eq!(monomorphised.parameters.len(), 2);

    // Check key parameter: K -> String
    match &monomorphised.parameters[0].1 {
        Type::Concrete { id, .. } => assert_eq!(id.name(), "String"),
        _ => panic!("Expected String type for key parameter"),
    }

    // Check values parameter: List<V> -> List<Integer64>
    match &monomorphised.parameters[1].1 {
        Type::Concrete { id, args, .. } => {
            assert_eq!(id.name(), "List");
            assert_eq!(args.len(), 1);
            match &args[0] {
                Type::Concrete { id, .. } => assert_eq!(id.name(), "Integer64"),
                _ => panic!("Expected Integer64 type for List element"),
            }
        }
        _ => panic!("Expected List type for values parameter"),
    }

    // Check return type: Map<K, List<V>> -> Map<String, List<Integer64>>
    match &monomorphised.return_type {
        Type::Concrete { id, args, .. } => {
            assert_eq!(id.name(), "Map");
            assert_eq!(args.len(), 2);

            // Check key type
            match &args[0] {
                Type::Concrete { id, .. } => assert_eq!(id.name(), "String"),
                _ => panic!("Expected String type for Map key"),
            }

            // Check value type List<Integer64>
            match &args[1] {
                Type::Concrete { id, args, .. } => {
                    assert_eq!(id.name(), "List");
                    assert_eq!(args.len(), 1);
                    match &args[0] {
                        Type::Concrete { id, .. } => assert_eq!(id.name(), "Integer64"),
                        _ => panic!("Expected Integer64 type for List element"),
                    }
                }
                _ => panic!("Expected List type for Map value"),
            }
        }
        _ => panic!("Expected Map type for return type"),
    }
}

#[test]
fn test_all_entries_iteration() {
    let mut table = MonomorphisationTable::new();

    // Add a few entries
    let generic_function = FunctionInfo {
        defining_scope: "Test".to_string(),
        function_name: "process".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![],
        return_type: Type::Concrete {
            id: ModuleName::new("T"),
            args: vec![],
            span: None,
        },
        body: None,
        span: None,
        generic_parameters: vec!["T".to_string()],
        is_generic: true,
    };

    for type_name in &["Integer64", "String", "Boolean"] {
        let mut substitutions = HashMap::new();
        substitutions.insert(
            "T".to_string(),
            Type::Concrete {
                id: ModuleName::new(*type_name),
                args: vec![],
                span: None,
            },
        );

        let entry = MonomorphisationEntry {
            generic_function: generic_function.clone(),
            type_substitutions: substitutions,
            monomorphised_key: format!("Test.process:{}", type_name),
            monomorphised_function: MonomorphisationTable::create_monomorphised_function(
                &generic_function,
                &HashMap::from([(
                    "T".to_string(),
                    Type::Concrete {
                        id: ModuleName::new(*type_name),
                        args: vec![],
                        span: None,
                    },
                )]),
            )
            .expect("Monomorphization should succeed"),
        };

        table.add_instantiation(entry);
    }

    // Test iteration
    let all_entries: Vec<_> = table.all_entries().collect();
    assert_eq!(all_entries.len(), 3);

    let keys: Vec<String> = all_entries.iter().map(|(key, _)| (*key).clone()).collect();

    assert!(keys.contains(&"Test.process:Integer64".to_string()));
    assert!(keys.contains(&"Test.process:String".to_string()));
    assert!(keys.contains(&"Test.process:Boolean".to_string()));
}
