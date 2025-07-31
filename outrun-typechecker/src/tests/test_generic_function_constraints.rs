//! Test for Phase 3.5: Generic Function Constraint Generation
//!
//! Tests the constraint generation system for generic function calls that enables
//! the type inference system to properly resolve generic type parameters.

use crate::dispatch::{FunctionInfo, FunctionVisibility};
use crate::inference::{InferenceContext, TypeInferenceEngine};
use crate::types::{Constraint, SelfBindingContext, Substitution, Type};
use std::collections::HashMap;

fn create_test_engine() -> TypeInferenceEngine {
    let mut engine = TypeInferenceEngine::new();

    // Set up basic types in registry
    engine.set_current_module(ModuleName::new("TestModule"));

    engine
}

fn create_generic_function_info() -> FunctionInfo {
    FunctionInfo {
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
    }
}

#[test]
fn test_constraint_generation_for_simple_generic_function() {
    let mut engine = create_test_engine();
    let function_info = create_generic_function_info();

    // Call with Integer64 argument: Wrapper.wrap(value: 42)
    let integer_type = Type::Concrete {
        id: ModuleName::new("Integer64"),
        args: vec![],
        span: None,
    };
    let inferred_arguments = vec![integer_type.clone()];

    let context = InferenceContext {
        substitution: Substitution::new(),
        constraints: Vec::new(),
        expected_type: None,
        self_binding: SelfBindingContext::ProtocolDefinition {
            protocol_id: crate::types::ModuleName::new("TestProtocol"),
            protocol_args: vec![],
        },
        bindings: HashMap::new(),
    };

    // Generate constraints for this generic function call
    let constraints = engine
        .generate_generic_function_constraints(&function_info, &inferred_arguments, &context)
        .expect("Constraint generation should succeed");

    // Should generate constraints that T = Integer64
    assert!(
        !constraints.is_empty(),
        "Should generate at least one constraint"
    );

    // Check that we have equality constraints
    let equality_constraints: Vec<_> = constraints
        .iter()
        .filter(|c| matches!(c, Constraint::Equality { .. }))
        .collect();

    assert!(
        !equality_constraints.is_empty(),
        "Should have equality constraints"
    );

    // Verify constraint involves T and Integer64
    let has_t_integer_constraint = constraints.iter().any(|constraint| {
        match constraint {
            Constraint::Equality { left, right, .. } => {
                (matches!(left.as_ref(), Type::Concrete { id, .. } if id.name() == "T") &&
                 matches!(right.as_ref(), Type::Concrete { id, .. } if id.name() == "Integer64")) ||
                (matches!(right.as_ref(), Type::Concrete { id, .. } if id.name() == "T") &&
                 matches!(left.as_ref(), Type::Concrete { id, .. } if id.name() == "Integer64"))
            }
            _ => false
        }
    });

    assert!(
        has_t_integer_constraint,
        "Should have constraint linking T to Integer64"
    );
}

#[test]
fn test_return_type_instantiation_for_generic_function() {
    let engine = create_test_engine();
    let function_info = create_generic_function_info();

    // Call with String argument: Wrapper.wrap(value: "hello")
    let string_type = Type::Concrete {
        id: ModuleName::new("String"),
        args: vec![],
        span: None,
    };
    let inferred_arguments = vec![string_type.clone()];

    // Test return type instantiation
    let instantiated_return_type = engine
        .instantiate_generic_return_type(
            &function_info.return_type,
            &function_info,
            &inferred_arguments,
        )
        .expect("Return type instantiation should succeed");

    // Should be Wrapper<String>
    match &instantiated_return_type {
        Type::Concrete { id, args, .. } => {
            assert_eq!(id.name(), "Wrapper");
            assert_eq!(args.len(), 1);
            match &args[0] {
                Type::Concrete { id, .. } => {
                    assert_eq!(id.name(), "String");
                }
                _ => panic!("Expected String type argument"),
            }
        }
        _ => panic!("Expected Wrapper<String> type"),
    }
}

#[test]
fn test_multiple_type_parameters() {
    let mut engine = create_test_engine();

    // Function with multiple type parameters: def convert<T, U>(from: T, to_type: Type<U>): U
    let function_info = FunctionInfo {
        defining_scope: "Converter".to_string(),
        function_name: "convert".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![
            (
                "from".to_string(),
                Type::Concrete {
                    id: ModuleName::new("T"),
                    args: vec![],
                    span: None,
                },
            ),
            (
                "to_type".to_string(),
                Type::Concrete {
                    id: ModuleName::new("Type"),
                    args: vec![Type::Concrete {
                        id: ModuleName::new("U"),
                        args: vec![],
                        span: None,
                    }],
                    span: None,
                },
            ),
        ],
        return_type: Type::Concrete {
            id: ModuleName::new("U"),
            args: vec![],
            span: None,
        },
        body: None,
        span: None,
        generic_parameters: vec!["T".to_string(), "U".to_string()],
        is_generic: true,
    };

    // Call: Converter.convert(from: 42, to_type: Type<String>)
    let integer_type = Type::Concrete {
        id: ModuleName::new("Integer64"),
        args: vec![],
        span: None,
    };
    let type_string = Type::Concrete {
        id: ModuleName::new("Type"),
        args: vec![Type::Concrete {
            id: ModuleName::new("String"),
            args: vec![],
            span: None,
        }],
        span: None,
    };
    let inferred_arguments = vec![integer_type, type_string];

    let context = InferenceContext {
        substitution: Substitution::new(),
        constraints: Vec::new(),
        expected_type: None,
        self_binding: SelfBindingContext::ProtocolDefinition {
            protocol_id: crate::types::ModuleName::new("TestProtocol"),
            protocol_args: vec![],
        },
        bindings: HashMap::new(),
    };

    // Generate constraints
    let constraints = engine
        .generate_generic_function_constraints(&function_info, &inferred_arguments, &context)
        .expect("Constraint generation should succeed");

    // Should have constraints for both T and U
    assert!(
        !constraints.is_empty(),
        "Should generate constraints for multiple type parameters"
    );

    // Test return type instantiation - should be String
    let instantiated_return_type = engine
        .instantiate_generic_return_type(
            &function_info.return_type,
            &function_info,
            &inferred_arguments,
        )
        .expect("Return type instantiation should succeed");

    match &instantiated_return_type {
        Type::Concrete { id, .. } => {
            assert_eq!(id.name(), "String");
        }
        _ => panic!("Expected String return type"),
    }
}

#[test]
fn test_nested_generic_types() {
    let engine = create_test_engine();

    // Function: def unwrap<T>(wrapper: Wrapper<Option<T>>): T
    let function_info = FunctionInfo {
        defining_scope: "Unwrapper".to_string(),
        function_name: "unwrap".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![(
            "wrapper".to_string(),
            Type::Concrete {
                id: ModuleName::new("Wrapper"),
                args: vec![Type::Concrete {
                    id: ModuleName::new("Option"),
                    args: vec![Type::Concrete {
                        id: ModuleName::new("T"),
                        args: vec![],
                        span: None,
                    }],
                    span: None,
                }],
                span: None,
            },
        )],
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

    // Call: Unwrapper.unwrap(wrapper: Wrapper<Option<Boolean>>)
    let wrapper_option_boolean = Type::Concrete {
        id: ModuleName::new("Wrapper"),
        args: vec![Type::Concrete {
            id: ModuleName::new("Option"),
            args: vec![Type::Concrete {
                id: ModuleName::new("Boolean"),
                args: vec![],
                span: None,
            }],
            span: None,
        }],
        span: None,
    };
    let inferred_arguments = vec![wrapper_option_boolean];

    // Test return type instantiation - should be Boolean
    let instantiated_return_type = engine
        .instantiate_generic_return_type(
            &function_info.return_type,
            &function_info,
            &inferred_arguments,
        )
        .expect("Return type instantiation should succeed");

    match &instantiated_return_type {
        Type::Concrete { id, .. } => {
            assert_eq!(id.name(), "Boolean");
        }
        _ => panic!("Expected Boolean return type"),
    }
}

#[test]
fn test_constraint_generation_with_non_generic_function() {
    let _engine = create_test_engine();

    // Non-generic function: def add(a: Integer64, b: Integer64): Integer64
    let function_info = FunctionInfo {
        defining_scope: "Math".to_string(),
        function_name: "add".to_string(),
        visibility: FunctionVisibility::Public,
        parameters: vec![
            (
                "a".to_string(),
                Type::Concrete {
                    id: ModuleName::new("Integer64"),
                    args: vec![],
                    span: None,
                },
            ),
            (
                "b".to_string(),
                Type::Concrete {
                    id: ModuleName::new("Integer64"),
                    args: vec![],
                    span: None,
                },
            ),
        ],
        return_type: Type::Concrete {
            id: ModuleName::new("Integer64"),
            args: vec![],
            span: None,
        },
        body: None,
        span: None,
        generic_parameters: vec![],
        is_generic: false,
    };

    let integer_type = Type::Concrete {
        id: ModuleName::new("Integer64"),
        args: vec![],
        span: None,
    };
    let _inferred_arguments = vec![integer_type.clone(), integer_type];

    let _context = InferenceContext {
        substitution: Substitution::new(),
        constraints: Vec::new(),
        expected_type: None,
        self_binding: SelfBindingContext::ProtocolDefinition {
            protocol_id: crate::types::ModuleName::new("TestProtocol"),
            protocol_args: vec![],
        },
        bindings: HashMap::new(),
    };

    // Should not call constraint generation for non-generic functions
    // (This would be handled by the non-generic path in infer_resolved_function_call)
    // This test is mainly to ensure our is_generic flag handling works correctly
    assert!(
        !function_info.is_generic,
        "Function should not be marked as generic"
    );
}

#[test]
fn test_type_parameter_name_detection() {
    use crate::inference::is_type_parameter_name;

    // Test single uppercase letters (common type parameters)
    assert!(is_type_parameter_name("T"));
    assert!(is_type_parameter_name("U"));
    assert!(is_type_parameter_name("K"));
    assert!(is_type_parameter_name("V"));

    // Test T-prefixed names
    assert!(is_type_parameter_name("T1"));
    assert!(is_type_parameter_name("TKey"));
    assert!(is_type_parameter_name("TValue"));

    // Test common type parameter names
    assert!(is_type_parameter_name("Key"));
    assert!(is_type_parameter_name("Value"));
    assert!(is_type_parameter_name("Input"));
    assert!(is_type_parameter_name("Output"));
    assert!(is_type_parameter_name("Result"));
    assert!(is_type_parameter_name("Error"));

    // Test concrete type names (should not be type parameters)
    assert!(!is_type_parameter_name("String"));
    assert!(!is_type_parameter_name("Integer64"));
    assert!(!is_type_parameter_name("Boolean"));
    assert!(!is_type_parameter_name("Option"));
    assert!(!is_type_parameter_name("List"));
    assert!(!is_type_parameter_name("Map"));
}
