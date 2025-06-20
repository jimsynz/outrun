//! Tests for the type unification algorithm

use crate::unification::{
    unify_structured_types, FunctionParam, StructuredType, UnificationContext, UnificationError,
};
fn create_test_context() -> UnificationContext {
    let mut context = UnificationContext::new();

    // Set up some basic types
    let string_trait_id = context.type_interner.intern_type("String");
    let boolean_trait_id = context.type_interner.intern_type("Boolean");
    let integer_trait_id = context.type_interner.intern_type("Integer");
    let _option_id = context.type_interner.intern_type("Option");
    let _map_id = context.type_interner.intern_type("Map");
    let core_string_id = context.type_interner.intern_type("Outrun.Core.String");
    let core_boolean_id = context.type_interner.intern_type("Outrun.Core.Boolean");
    let core_integer_id = context.type_interner.intern_type("Outrun.Core.Integer64");

    // Register traits
    context.trait_registry.register_trait(string_trait_id);
    context.trait_registry.register_trait(boolean_trait_id);
    context.trait_registry.register_trait(integer_trait_id);

    // Register implementations
    context
        .trait_registry
        .register_implementation(core_string_id, string_trait_id);
    context
        .trait_registry
        .register_implementation(core_boolean_id, boolean_trait_id);
    context
        .trait_registry
        .register_implementation(core_integer_id, integer_trait_id);

    context
}

#[test]
fn test_exact_match_unification() {
    let context = create_test_context();
    let string_id = context.type_interner.get_type("String").unwrap();

    let type1 = StructuredType::simple(string_id);
    let type2 = StructuredType::simple(string_id);

    assert!(unify_structured_types(&type1, &type2, &context).unwrap());
}

#[test]
fn test_trait_concrete_unification() {
    let context = create_test_context();
    let string_trait_id = context.type_interner.get_type("String").unwrap();
    let core_string_id = context
        .type_interner
        .get_type("Outrun.Core.String")
        .unwrap();

    let trait_type = StructuredType::simple(string_trait_id);
    let concrete_type = StructuredType::simple(core_string_id);

    // Trait should unify with implementing concrete type
    assert!(unify_structured_types(&trait_type, &concrete_type, &context).unwrap());
    assert!(unify_structured_types(&concrete_type, &trait_type, &context).unwrap());
}

#[test]
fn test_non_implementing_trait_unification() {
    let context = create_test_context();
    let string_trait_id = context.type_interner.get_type("String").unwrap();
    let core_boolean_id = context
        .type_interner
        .get_type("Outrun.Core.Boolean")
        .unwrap();

    let trait_type = StructuredType::simple(string_trait_id);
    let concrete_type = StructuredType::simple(core_boolean_id);

    // String trait should not unify with Boolean concrete type
    let result = unify_structured_types(&trait_type, &concrete_type, &context);
    assert!(result.is_err());
    assert!(matches!(
        result.unwrap_err(),
        UnificationError::TraitNotImplemented { .. }
    ));
}

#[test]
fn test_generic_type_unification() {
    let mut context = create_test_context();
    let option_id = context.type_interner.intern_type("Option");
    let string_trait_id = context.type_interner.get_type("String").unwrap();
    let core_string_id = context
        .type_interner
        .get_type("Outrun.Core.String")
        .unwrap();

    let option_trait =
        StructuredType::generic(option_id, vec![StructuredType::simple(string_trait_id)]);
    let option_concrete =
        StructuredType::generic(option_id, vec![StructuredType::simple(core_string_id)]);

    // Option<String> should unify with Option<Outrun.Core.String>
    assert!(unify_structured_types(&option_trait, &option_concrete, &context).unwrap());
}

#[test]
fn test_generic_arity_mismatch() {
    let mut context = create_test_context();
    let option_id = context.type_interner.intern_type("Option");
    let map_id = context.type_interner.intern_type("Map");
    let string_id = context.type_interner.get_type("String").unwrap();

    let option_type = StructuredType::generic(option_id, vec![StructuredType::simple(string_id)]);
    let map_type = StructuredType::generic(
        map_id,
        vec![
            StructuredType::simple(string_id),
            StructuredType::simple(string_id),
        ],
    );

    // Option<String> (arity 1) should not unify with Map<String, String> (arity 2)
    let result = unify_structured_types(&option_type, &map_type, &context);
    assert!(!result.unwrap()); // Different base types, so should return false
}

#[test]
fn test_tuple_unification() {
    let context = create_test_context();
    let string_id = context.type_interner.get_type("String").unwrap();
    let boolean_id = context.type_interner.get_type("Boolean").unwrap();

    let tuple1 = StructuredType::tuple(vec![
        StructuredType::simple(string_id),
        StructuredType::simple(boolean_id),
    ]);
    let tuple2 = StructuredType::tuple(vec![
        StructuredType::simple(string_id),
        StructuredType::simple(boolean_id),
    ]);

    assert!(unify_structured_types(&tuple1, &tuple2, &context).unwrap());
}

#[test]
fn test_tuple_arity_mismatch() {
    let context = create_test_context();
    let string_id = context.type_interner.get_type("String").unwrap();

    let tuple1 = StructuredType::tuple(vec![StructuredType::simple(string_id)]);
    let tuple2 = StructuredType::tuple(vec![
        StructuredType::simple(string_id),
        StructuredType::simple(string_id),
    ]);

    // Different tuple arities should not unify
    assert!(!unify_structured_types(&tuple1, &tuple2, &context).unwrap());
}

#[test]
fn test_function_type_unification() {
    let mut context = create_test_context();
    let string_id = context.type_interner.get_type("String").unwrap();
    let boolean_id = context.type_interner.get_type("Boolean").unwrap();

    let name_atom = context.type_interner.intern_atom("name");

    let func1 = StructuredType::function(
        vec![FunctionParam {
            name: name_atom,
            param_type: StructuredType::simple(string_id),
        }],
        StructuredType::simple(boolean_id),
    );

    let func2 = StructuredType::function(
        vec![FunctionParam {
            name: name_atom,
            param_type: StructuredType::simple(string_id),
        }],
        StructuredType::simple(boolean_id),
    );

    assert!(unify_structured_types(&func1, &func2, &context).unwrap());
}

#[test]
fn test_function_parameter_name_mismatch() {
    let mut context = create_test_context();
    let string_id = context.type_interner.get_type("String").unwrap();
    let boolean_id = context.type_interner.get_type("Boolean").unwrap();

    let name1_atom = context.type_interner.intern_atom("name1");
    let name2_atom = context.type_interner.intern_atom("name2");

    let func1 = StructuredType::function(
        vec![FunctionParam {
            name: name1_atom,
            param_type: StructuredType::simple(string_id),
        }],
        StructuredType::simple(boolean_id),
    );

    let func2 = StructuredType::function(
        vec![FunctionParam {
            name: name2_atom,
            param_type: StructuredType::simple(string_id),
        }],
        StructuredType::simple(boolean_id),
    );

    // Different parameter names should cause unification error
    let result = unify_structured_types(&func1, &func2, &context);
    assert!(result.is_err());
    assert!(matches!(
        result.unwrap_err(),
        UnificationError::ParameterNameMismatch { .. }
    ));
}

#[test]
fn test_cross_type_unification_failure() {
    let context = create_test_context();
    let string_id = context.type_interner.get_type("String").unwrap();

    let simple_type = StructuredType::simple(string_id);
    let tuple_type = StructuredType::tuple(vec![StructuredType::simple(string_id)]);

    // Simple type should not unify with tuple type
    assert!(!unify_structured_types(&simple_type, &tuple_type, &context).unwrap());
}

#[test]
fn test_nested_generic_unification() {
    let mut context = create_test_context();
    let option_id = context.type_interner.intern_type("Option");
    let list_id = context.type_interner.intern_type("List");
    let string_id = context.type_interner.get_type("String").unwrap();

    // Option<List<String>>
    let nested1 = StructuredType::generic(
        option_id,
        vec![StructuredType::generic(
            list_id,
            vec![StructuredType::simple(string_id)],
        )],
    );

    // Option<List<String>>
    let nested2 = StructuredType::generic(
        option_id,
        vec![StructuredType::generic(
            list_id,
            vec![StructuredType::simple(string_id)],
        )],
    );

    assert!(unify_structured_types(&nested1, &nested2, &context).unwrap());
}

// Self type resolution is now handled by the type parameter system in multi_program_compiler

// Self type in generic resolution is now handled by the type parameter system in multi_program_compiler
#[test]
fn test_generic_type_resolution() {
    let mut context = create_test_context();
    let option_id = context.type_interner.intern_type("Option");
    let string_id = context.type_interner.get_type("String").unwrap();

    // Test Option<String> resolution (no Self needed)
    let option_string = StructuredType::generic(option_id, vec![StructuredType::simple(string_id)]);
    let resolved = context.resolve_type(&option_string).unwrap();
    assert_eq!(resolved, option_string);
}

#[test]
fn test_string_representation() {
    let context = create_test_context();
    let string_id = context.type_interner.get_type("String").unwrap();
    let option_id = context.type_interner.get_type("Option").unwrap();

    let simple_type = StructuredType::simple(string_id);
    assert_eq!(
        simple_type.to_string_representation(&context.type_interner),
        "String"
    );

    let generic_type = StructuredType::generic(option_id, vec![StructuredType::simple(string_id)]);
    assert_eq!(
        generic_type.to_string_representation(&context.type_interner),
        "Option<String>"
    );

    let tuple_type = StructuredType::tuple(vec![
        StructuredType::simple(string_id),
        StructuredType::simple(string_id),
    ]);
    assert_eq!(
        tuple_type.to_string_representation(&context.type_interner),
        "(String, String)"
    );
}
