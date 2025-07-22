//! Tests for the type unification algorithm

use crate::unification::{
    unify_structured_types, FunctionParam, StructuredType, UnificationContext, UnificationError,
};
fn create_test_context() -> (
    UnificationContext,
    crate::compilation::compiler_environment::CompilerEnvironment,
) {
    let context = UnificationContext::new();
    let compiler_env = crate::compilation::compiler_environment::CompilerEnvironment::new();

    // Set up some basic types
    let string_protocol_id = compiler_env.intern_type_name("String");
    let boolean_protocol_id = compiler_env.intern_type_name("Boolean");
    let integer_protocol_id = compiler_env.intern_type_name("Integer");
    let _option_id = compiler_env.intern_type_name("Option");
    let _map_id = compiler_env.intern_type_name("Map");
    let core_string_id = compiler_env.intern_type_name("Outrun.Core.String");
    let core_boolean_id = compiler_env.intern_type_name("Outrun.Core.Boolean");
    let core_integer_id = compiler_env.intern_type_name("Outrun.Core.Integer64");

    // Register protocol implementations using CompilerEnvironment
    compiler_env.register_protocol_implementation(
        StructuredType::Simple(core_string_id.clone()),
        StructuredType::Simple(string_protocol_id.clone()),
    );
    compiler_env.register_protocol_implementation(
        StructuredType::Simple(core_boolean_id.clone()),
        StructuredType::Simple(boolean_protocol_id.clone()),
    );
    compiler_env.register_protocol_implementation(
        StructuredType::Simple(core_integer_id.clone()),
        StructuredType::Simple(integer_protocol_id.clone()),
    );

    (context, compiler_env)
}

#[test]
fn test_exact_match_unification() {
    let (context, compiler_env) = create_test_context();
    let string_id = compiler_env.intern_type_name("String");

    let type1 = StructuredType::Simple(string_id.clone());
    let type2 = StructuredType::Simple(string_id);

    assert!(
        unify_structured_types(&type1, &type2, &context, &compiler_env)
            .unwrap()
            .is_some()
    );
}

#[test]
fn test_protocol_concrete_unification() {
    let (context, compiler_env) = create_test_context();
    let string_protocol_id = compiler_env.intern_type_name("String");
    let core_string_id = compiler_env.intern_type_name("Outrun.Core.String");

    let protocol_type = StructuredType::Simple(string_protocol_id);
    let concrete_type = StructuredType::Simple(core_string_id);

    // Protocol should unify with implementing concrete type
    assert!(
        unify_structured_types(&protocol_type, &concrete_type, &context, &compiler_env)
            .unwrap()
            .is_some()
    );
    assert!(
        unify_structured_types(&concrete_type, &protocol_type, &context, &compiler_env)
            .unwrap()
            .is_some()
    );
}

#[test]
fn test_non_implementing_protocol_unification() {
    let (context, compiler_env) = create_test_context();
    let string_protocol_id = compiler_env.intern_type_name("String");
    let core_boolean_id = compiler_env.intern_type_name("Outrun.Core.Boolean");

    let protocol_type = StructuredType::Simple(string_protocol_id);
    let concrete_type = StructuredType::Simple(core_boolean_id);

    // String protocol should not unify with Boolean concrete type
    let result = unify_structured_types(&protocol_type, &concrete_type, &context, &compiler_env);
    assert!(result.is_err());
    assert!(matches!(
        result.unwrap_err(),
        UnificationError::ProtocolNotImplemented { .. }
    ));
}

#[test]
fn test_generic_type_unification() {
    let (context, compiler_env) = create_test_context();
    let option_id = compiler_env.intern_type_name("Option");
    let string_protocol_id = compiler_env.intern_type_name("String");
    let core_string_id = compiler_env.intern_type_name("Outrun.Core.String");

    let option_protocol = StructuredType::generic(
        option_id.clone(),
        vec![StructuredType::Simple(string_protocol_id)],
    );
    let option_concrete =
        StructuredType::generic(option_id, vec![StructuredType::Simple(core_string_id)]);

    // Option<String> should unify with Option<Outrun.Core.String>
    assert!(
        unify_structured_types(&option_protocol, &option_concrete, &context, &compiler_env)
            .unwrap()
            .is_some()
    );
}

#[test]
fn test_generic_arity_mismatch() {
    let (context, compiler_env) = create_test_context();
    let option_id = compiler_env.intern_type_name("Option");
    let map_id = compiler_env.intern_type_name("Map");
    let string_id = compiler_env.intern_type_name("String");

    let option_type = StructuredType::generic(
        option_id.clone(),
        vec![StructuredType::Simple(string_id.clone())],
    );
    let map_type = StructuredType::generic(
        map_id,
        vec![
            StructuredType::Simple(string_id.clone()),
            StructuredType::Simple(string_id),
        ],
    );

    // Option<String> (arity 1) should not unify with Map<String, String> (arity 2)
    let result = unify_structured_types(&option_type, &map_type, &context, &compiler_env);
    assert!(result.unwrap().is_none()); // Different base types, so should return None
}

#[test]
fn test_tuple_unification() {
    let (context, compiler_env) = create_test_context();
    let string_id = compiler_env.intern_type_name("String");
    let boolean_id = compiler_env.intern_type_name("Boolean");

    let tuple1 = StructuredType::tuple(vec![
        StructuredType::Simple(string_id.clone()),
        StructuredType::Simple(boolean_id.clone()),
    ]);
    let tuple2 = StructuredType::tuple(vec![
        StructuredType::Simple(string_id),
        StructuredType::Simple(boolean_id),
    ]);

    assert!(
        unify_structured_types(&tuple1, &tuple2, &context, &compiler_env)
            .unwrap()
            .is_some()
    );
}

#[test]
fn test_tuple_arity_mismatch() {
    let (context, compiler_env) = create_test_context();
    let string_id = compiler_env.intern_type_name("String");

    let tuple1 = StructuredType::tuple(vec![StructuredType::Simple(string_id.clone())]);
    let tuple2 = StructuredType::tuple(vec![
        StructuredType::Simple(string_id.clone()),
        StructuredType::Simple(string_id),
    ]);

    // Different tuple arities should not unify
    assert!(
        unify_structured_types(&tuple1, &tuple2, &context, &compiler_env)
            .unwrap()
            .is_none()
    );
}

#[test]
fn test_function_type_unification() {
    let (context, compiler_env) = create_test_context();
    let string_id = compiler_env.intern_type_name("String");
    let boolean_id = compiler_env.intern_type_name("Boolean");

    let name_atom = compiler_env.intern_atom_name("name");

    let func1 = StructuredType::function(
        vec![FunctionParam {
            name: name_atom.clone(),
            param_type: StructuredType::Simple(string_id.clone()),
        }],
        StructuredType::Simple(boolean_id.clone()),
    );

    let func2 = StructuredType::function(
        vec![FunctionParam {
            name: name_atom,
            param_type: StructuredType::Simple(string_id),
        }],
        StructuredType::Simple(boolean_id),
    );

    assert!(
        unify_structured_types(&func1, &func2, &context, &compiler_env)
            .unwrap()
            .is_some()
    );
}

#[test]
fn test_function_parameter_name_mismatch() {
    let (context, compiler_env) = create_test_context();
    let string_id = compiler_env.intern_type_name("String");
    let boolean_id = compiler_env.intern_type_name("Boolean");

    let name1_atom = compiler_env.intern_atom_name("name1");
    let name2_atom = compiler_env.intern_atom_name("name2");

    let func1 = StructuredType::function(
        vec![FunctionParam {
            name: name1_atom,
            param_type: StructuredType::Simple(string_id.clone()),
        }],
        StructuredType::Simple(boolean_id.clone()),
    );

    let func2 = StructuredType::function(
        vec![FunctionParam {
            name: name2_atom,
            param_type: StructuredType::Simple(string_id),
        }],
        StructuredType::Simple(boolean_id),
    );

    // Different parameter names should cause unification error
    let result = unify_structured_types(&func1, &func2, &context, &compiler_env);
    assert!(result.is_err());
    assert!(matches!(
        result.unwrap_err(),
        UnificationError::ParameterNameMismatch { .. }
    ));
}

#[test]
fn test_cross_type_unification_failure() {
    let (context, compiler_env) = create_test_context();
    let string_id = compiler_env.intern_type_name("String");

    let simple_type = StructuredType::Simple(string_id.clone());
    let tuple_type = StructuredType::tuple(vec![StructuredType::Simple(string_id)]);

    // Simple type should not unify with tuple type
    assert!(
        unify_structured_types(&simple_type, &tuple_type, &context, &compiler_env)
            .unwrap()
            .is_none()
    );
}

#[test]
fn test_nested_generic_unification() {
    let (context, compiler_env) = create_test_context();
    let option_id = compiler_env.intern_type_name("Option");
    let list_id = compiler_env.intern_type_name("List");
    let string_id = compiler_env.intern_type_name("String");

    // Option<List<String>>
    let nested1 = StructuredType::generic(
        option_id.clone(),
        vec![StructuredType::generic(
            list_id.clone(),
            vec![StructuredType::Simple(string_id.clone())],
        )],
    );

    // Option<List<String>>
    let nested2 = StructuredType::generic(
        option_id,
        vec![StructuredType::generic(
            list_id,
            vec![StructuredType::Simple(string_id)],
        )],
    );

    assert!(
        unify_structured_types(&nested1, &nested2, &context, &compiler_env)
            .unwrap()
            .is_some()
    );
}

// Self type resolution is now handled by the type parameter system in multi_program_compiler

// Self type in generic resolution is now handled by the type parameter system in multi_program_compiler
#[test]
fn test_generic_type_resolution() {
    let (context, compiler_env) = create_test_context();
    let option_id = compiler_env.intern_type_name("Option");
    let string_id = compiler_env.intern_type_name("String");

    // Test Option<String> resolution (no Self needed)
    let option_string = StructuredType::generic(option_id, vec![StructuredType::Simple(string_id)]);
    let resolved = context.resolve_type(&option_string, &compiler_env);
    assert_eq!(resolved, Ok(option_string.clone()));
}

#[test]
fn test_string_representation() {
    let (_context, compiler_env) = create_test_context();
    let string_id = compiler_env.intern_type_name("String");
    let option_id = compiler_env.intern_type_name("Option");

    let simple_type = StructuredType::Simple(string_id.clone());
    assert_eq!(simple_type.to_string_representation(), "String");

    let generic_type =
        StructuredType::generic(option_id, vec![StructuredType::Simple(string_id.clone())]);
    assert_eq!(generic_type.to_string_representation(), "Option<String>");

    let tuple_type = StructuredType::tuple(vec![
        StructuredType::Simple(string_id.clone()),
        StructuredType::Simple(string_id),
    ]);
    assert_eq!(tuple_type.to_string_representation(), "(String, String)");
}
