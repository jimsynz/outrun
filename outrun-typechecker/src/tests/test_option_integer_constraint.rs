//! Test for Phase 2: Protocol Constraint Resolution - Option<Integer> case
//!
//! Tests the specific issue mentioned: Option<Integer> means "all types that implement
//! the Option protocol are generic over all types that implement the Integer protocol"

use crate::inference::TypeInferenceEngine;
use crate::types::{ModuleName, Type};

#[test]
fn test_option_integer_constraint_compatibility() {
    let mut engine = TypeInferenceEngine::new();

    // Create Option<Integer> - a protocol constraint
    let option_integer_constraint =
        Type::protocol_with_args("Option", vec![Type::protocol("Integer")]);

    // Create Option<Outrun.Core.Integer64> - a concrete instantiation
    let option_concrete_integer =
        Type::protocol_with_args("Option", vec![Type::concrete("Outrun.Core.Integer64")]);

    println!(
        "Option<Integer> constraint: {:?}",
        option_integer_constraint
    );
    println!(
        "Option<Outrun.Core.Integer64> concrete: {:?}",
        option_concrete_integer
    );

    // Test: Option<Outrun.Core.Integer64> should satisfy Option<Integer>
    // This requires that Outrun.Core.Integer64 implements Integer

    // First, register that Outrun.Core.Integer64 implements Integer
    let integer64_type = ModuleName::new("Outrun.Core.Integer64");
    let integer_protocol = ModuleName::new("Integer");

    // Add modules as local to avoid orphan rule violations
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Outrun.Core.Integer64"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Integer"));

    engine
        .type_registry_mut()
        .register_implementation(
            integer64_type.clone(),
            vec![], // no generic args
            integer_protocol.clone(),
            vec![], // no protocol args
            ModuleName::new("Outrun.Core.Integer64"),
            None,
        )
        .expect("Should register Integer implementation");

    // Now test compatibility
    let is_compatible =
        engine.test_types_are_compatible(&option_concrete_integer, &option_integer_constraint);

    println!("Compatibility result: {}", is_compatible);

    // This should be true: Option<Outrun.Core.Integer64> satisfies Option<Integer>
    // because Outrun.Core.Integer64 implements Integer
    assert!(
        is_compatible,
        "Option<Outrun.Core.Integer64> should satisfy Option<Integer> constraint"
    );
}

#[test]
fn test_option_integer_constraint_negative_case() {
    let engine = TypeInferenceEngine::new();

    // Create Option<Integer> - a protocol constraint
    let option_integer_constraint =
        Type::protocol_with_args("Option", vec![Type::protocol("Integer")]);

    // Create Option<String> - should NOT satisfy Option<Integer>
    let option_string =
        Type::protocol_with_args("Option", vec![Type::concrete("Outrun.Core.String")]);

    // Test: Option<String> should NOT satisfy Option<Integer>
    let is_compatible =
        engine.test_types_are_compatible(&option_string, &option_integer_constraint);

    println!("Negative case compatibility: {}", is_compatible);

    // This should be false: Option<String> does not satisfy Option<Integer>
    // because String does not implement Integer
    assert!(
        !is_compatible,
        "Option<String> should NOT satisfy Option<Integer> constraint"
    );
}

#[test]
fn test_nested_constraint_resolution() {
    let mut engine = TypeInferenceEngine::new();

    // Create Result<Option<Integer>, String> - nested protocol constraints
    let result_option_integer_string = Type::protocol_with_args(
        "Result",
        vec![
            Type::protocol_with_args("Option", vec![Type::protocol("Integer")]),
            Type::protocol("String"),
        ],
    );

    // Create concrete Result<Option<Outrun.Core.Integer64>, Outrun.Core.String>
    let result_concrete = Type::protocol_with_args(
        "Result",
        vec![
            Type::protocol_with_args("Option", vec![Type::concrete("Outrun.Core.Integer64")]),
            Type::concrete("Outrun.Core.String"),
        ],
    );

    // Register implementations
    let integer64_type = ModuleName::new("Outrun.Core.Integer64");
    let string_type = ModuleName::new("Outrun.Core.String");
    let integer_protocol = ModuleName::new("Integer");
    let string_protocol = ModuleName::new("String");

    // Add modules as local to avoid orphan rule violations
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Outrun.Core.Integer64"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Outrun.Core.String"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Integer"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("String"));

    engine
        .type_registry_mut()
        .register_implementation(
            integer64_type,
            vec![],
            integer_protocol,
            vec![],
            ModuleName::new("Outrun.Core.Integer64"),
            None,
        )
        .expect("Should register Integer implementation");

    engine
        .type_registry_mut()
        .register_implementation(
            string_type,
            vec![],
            string_protocol,
            vec![],
            ModuleName::new("Outrun.Core.String"),
            None,
        )
        .expect("Should register String implementation");

    // Test nested constraint satisfaction
    let is_compatible =
        engine.test_types_are_compatible(&result_concrete, &result_option_integer_string);

    println!("Nested constraint compatibility: {}", is_compatible);

    // This should be true: nested constraints should be satisfied
    assert!(
        is_compatible,
        "Nested protocol constraints should be properly resolved"
    );
}
