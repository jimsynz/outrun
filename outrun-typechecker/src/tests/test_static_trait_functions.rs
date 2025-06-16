//! Tests for static trait function validation
//!
//! These tests verify that static trait functions are properly validated by the type checker
//! and that they are correctly distinguished from instance functions.

use crate::checker::TypeChecker;
use outrun_parser::parse_program;

#[test]
fn test_static_function_validation() {
    let input = r#"
trait Result {
    defs ok(value: String): String {
        "ok"
    }
    
    def is_ok?(self: Self): Boolean
}
"#;

    let program = parse_program(input).unwrap();
    let mut type_checker = TypeChecker::new();

    let result = type_checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Static function validation should succeed: {:?}",
        result.err()
    );

    let typed_program = result.unwrap();

    // Check that the trait was registered with correct function types
    let result_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Result")
        .unwrap();
    let trait_def = typed_program
        .type_context
        .trait_registry
        .get_trait(result_trait_id)
        .unwrap();

    assert_eq!(trait_def.functions.len(), 2);

    // First function should be static
    assert!(
        trait_def.functions[0].is_static,
        "ok function should be static"
    );
    assert!(
        !trait_def.functions[0].is_guard,
        "ok function should not be a guard"
    );

    // Second function should be instance and guard
    assert!(
        !trait_def.functions[1].is_static,
        "is_ok? function should be instance"
    );
    assert!(
        trait_def.functions[1].is_guard,
        "is_ok? function should be a guard"
    );
}

#[test]
fn test_static_function_cannot_be_guard() {
    let input = r#"
trait Test {
    defs check?(value: String): Boolean {
        true
    }
}
"#;

    let program = parse_program(input).unwrap();
    let mut type_checker = TypeChecker::new();

    let result = type_checker.check_program(&program);
    assert!(
        result.is_err(),
        "Static function with guard name should fail"
    );

    // Should get InvalidGuardFunction error in the error list
    let errors = result.unwrap_err();
    assert!(!errors.is_empty(), "Should have at least one error");

    match &errors[0] {
        crate::error::TypeError::InvalidGuardFunction { function_name, .. } => {
            assert_eq!(function_name, "check?");
        }
        other => assert!(
            matches!(other, crate::error::TypeError::InvalidGuardFunction { .. }),
            "Expected InvalidGuardFunction error, got: {:?}",
            other
        ),
    }
}

#[test]
fn test_multiple_static_functions() {
    let input = r#"
trait Option {
    defs some(value: String): String {
        "some"
    }
    
    defs none(): String {
        "none"
    }
    
    def is_some?(self: Self): Boolean
    def unwrap(self: Self): String
}
"#;

    let program = parse_program(input).unwrap();
    let mut type_checker = TypeChecker::new();

    let result = type_checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Multiple static functions should validate: {:?}",
        result.err()
    );

    let typed_program = result.unwrap();

    // Check that both static functions are marked correctly
    let option_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Option")
        .unwrap();
    let trait_def = typed_program
        .type_context
        .trait_registry
        .get_trait(option_trait_id)
        .unwrap();

    assert_eq!(trait_def.functions.len(), 4);

    // First two should be static
    assert!(
        trait_def.functions[0].is_static,
        "some function should be static"
    );
    assert!(
        trait_def.functions[1].is_static,
        "none function should be static"
    );

    // Last two should be instance
    assert!(
        !trait_def.functions[2].is_static,
        "is_some? function should be instance"
    );
    assert!(
        !trait_def.functions[3].is_static,
        "unwrap function should be instance"
    );
}

#[test]
fn test_static_function_parameter_validation() {
    let input = r#"
trait Test {
    defs create(value: Integer, count: String): String {
        "created"
    }
}
"#;

    let program = parse_program(input).unwrap();
    let mut type_checker = TypeChecker::new();

    let result = type_checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Static function with parameters should validate: {:?}",
        result.err()
    );

    let typed_program = result.unwrap();

    // Check parameter types
    let test_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Test")
        .unwrap();
    let trait_def = typed_program
        .type_context
        .trait_registry
        .get_trait(test_trait_id)
        .unwrap();

    assert_eq!(trait_def.functions.len(), 1);
    let static_func = &trait_def.functions[0];

    assert!(static_func.is_static);
    assert_eq!(static_func.params.len(), 2);

    // Check parameter types are resolved correctly
    // Note: Types might be resolved to fully qualified names
    let integer_type = typed_program
        .type_context
        .interner
        .get_type("Integer")
        .or_else(|| {
            typed_program
                .type_context
                .interner
                .get_type("Outrun.Core.Integer64")
        })
        .expect("Integer type should be resolved");
    let string_type = typed_program
        .type_context
        .interner
        .get_type("String")
        .or_else(|| {
            typed_program
                .type_context
                .interner
                .get_type("Outrun.Core.String")
        })
        .expect("String type should be resolved");

    assert_eq!(static_func.params[0].1, integer_type);
    assert_eq!(static_func.params[1].1, string_type);
}

#[test]
fn test_static_function_return_type_validation() {
    let input = r#"
trait Calculator {
    defs add(a: Integer, b: Integer): Integer {
        a + b
    }
    
    defs create() {
        Calculator {}
    }
}
"#;

    let program = parse_program(input).unwrap();
    let mut type_checker = TypeChecker::new();

    let result = type_checker.check_program(&program);
    assert!(
        result.is_ok(),
        "Static functions with different return types should validate: {:?}",
        result.err()
    );

    let typed_program = result.unwrap();

    let calc_trait_id = typed_program
        .type_context
        .interner
        .get_trait("Calculator")
        .unwrap();
    let trait_def = typed_program
        .type_context
        .trait_registry
        .get_trait(calc_trait_id)
        .unwrap();

    assert_eq!(trait_def.functions.len(), 2);

    // First function has explicit Integer return type
    let add_func = &trait_def.functions[0];
    let integer_type = typed_program
        .type_context
        .interner
        .get_type("Integer")
        .or_else(|| {
            typed_program
                .type_context
                .interner
                .get_type("Outrun.Core.Integer64")
        })
        .expect("Integer type should be resolved");
    assert_eq!(add_func.return_type, integer_type);

    // Second function has implicit Unit return type
    let create_func = &trait_def.functions[1];
    let unit_type = typed_program
        .type_context
        .interner
        .get_type("Outrun.Core.Unit")
        .unwrap();
    assert_eq!(create_func.return_type, unit_type);
}
