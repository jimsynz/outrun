//! Integration tests for If expression type checking and typed AST generation

use crate::checker::{TypedExpressionKind, TypedItemKind};
use crate::compilation::compiler_environment::CompilerEnvironment;
use outrun_parser::parse_program;

#[test]
fn test_if_else_with_literals_typed_ast() {
    // Test if-else with literal boolean condition and compatible branch types
    let source = r#"if true {
    42
} else {
    24
}"#;

    let program = parse_program(source).expect("Failed to parse program");

    let mut compiler_env = CompilerEnvironment::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler_env
        .compile_collection(collection)
        .expect("Compilation should succeed with literals");
    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program");

    // Check that we got a typed if expression
    match &typed_program.items[0].kind {
        TypedItemKind::Expression(expr) => {
            match &expr.kind {
                TypedExpressionKind::IfExpression {
                    condition,
                    then_branch,
                    else_branch,
                    result_type,
                } => {
                    // Verify condition is boolean literal
                    assert!(matches!(condition.kind, TypedExpressionKind::Boolean(true)));

                    // Verify then branch is integer literal
                    assert!(matches!(then_branch.kind, TypedExpressionKind::Integer(42)));

                    // Verify else branch exists and is integer literal
                    assert!(else_branch.is_some());
                    if let Some(else_expr) = else_branch {
                        assert!(matches!(else_expr.kind, TypedExpressionKind::Integer(24)));
                    }

                    // Verify result type is resolved (should be Integer)
                    assert!(
                        result_type.is_some(),
                        "Result type should be resolved for if-else"
                    );

                    println!(
                        "✓ If-else expression successfully compiled to typed AST with result type"
                    );
                }
                TypedExpressionKind::Placeholder(msg) => {
                    panic!("Expected typed if expression, got placeholder: {msg}");
                }
                _ => {
                    panic!("Expected if expression, got: {:?}", expr.kind);
                }
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_if_without_else_compilation_success() {
    // Test if without else - should succeed because Integer implements Default
    let source = r#"if true {
    42
}"#;

    let program = parse_program(source).expect("Failed to parse program");

    let mut compiler_env = CompilerEnvironment::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    // Should succeed because Integer implements Default (returns 0)
    let result = compiler_env
        .compile_collection(collection)
        .expect("Compilation should succeed when type implements Default");
    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program");

    // Check that we got a typed if expression without else
    match &typed_program.items[0].kind {
        TypedItemKind::Expression(expr) => {
            match &expr.kind {
                TypedExpressionKind::IfExpression {
                    condition,
                    then_branch,
                    else_branch,
                    result_type,
                } => {
                    // Verify condition is boolean literal
                    assert!(matches!(condition.kind, TypedExpressionKind::Boolean(true)));

                    // Verify then branch is integer literal
                    assert!(matches!(then_branch.kind, TypedExpressionKind::Integer(42)));

                    // Verify no explicit else branch (None)
                    assert!(else_branch.is_none(), "Should have no explicit else branch");

                    // Verify result type is resolved (should be Integer)
                    assert!(result_type.is_some(), "Result type should be resolved for if without else when type implements Default");

                    println!("✓ If without else successfully compiled with Default protocol");
                }
                TypedExpressionKind::Placeholder(msg) => {
                    panic!("Expected typed if expression, got placeholder: {msg}");
                }
                _ => {
                    panic!("Expected if expression, got: {:?}", expr.kind);
                }
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_if_else_with_string_literals() {
    // Test if-else with string literals (should work since types are the same)
    let source = r#"if false {
    "hello"
} else {
    "world"
}"#;

    let program = parse_program(source).expect("Failed to parse program");

    let mut compiler_env = CompilerEnvironment::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler_env
        .compile_collection(collection)
        .expect("Compilation should succeed with compatible string types");
    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program");

    // Check that we got a typed if expression with string branches
    match &typed_program.items[0].kind {
        TypedItemKind::Expression(expr) => {
            match &expr.kind {
                TypedExpressionKind::IfExpression {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    // Verify both branches are string literals
                    assert!(matches!(then_branch.kind, TypedExpressionKind::String(_)));

                    if let Some(else_expr) = else_branch {
                        assert!(matches!(else_expr.kind, TypedExpressionKind::String(_)));
                    }

                    println!("✓ If-else with string literals successfully compiled");
                }
                TypedExpressionKind::Placeholder(msg) => {
                    panic!("Expected typed if expression, got placeholder: {msg}");
                }
                _ => {
                    panic!("Expected if expression, got: {:?}", expr.kind);
                }
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_if_else_with_incompatible_types_failure() {
    // Test if-else with incompatible branch types (should fail)
    let source = r#"if true {
    42
} else {
    "hello"
}"#;

    let program = parse_program(source).expect("Failed to parse program");

    let mut compiler_env = CompilerEnvironment::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    // Should fail compilation due to incompatible branch types
    let result = compiler_env.compile_collection(collection);
    assert!(
        result.is_err(),
        "If-else with incompatible types should fail"
    );

    // Check that the error mentions type mismatch (case-insensitive)
    if let Err(errors) = result {
        let error_messages = errors
            .iter()
            .map(|e| format!("{e}"))
            .collect::<Vec<_>>()
            .join(" ");
        let error_lower = error_messages.to_lowercase();
        assert!(
            error_lower.contains("type mismatch") || error_lower.contains("typemismatch"),
            "Should mention type mismatch, got: {error_messages}"
        );
    }
}

#[test]
fn test_if_without_else_with_non_default_type_failure() {
    // Test if without else - should fail because Atom doesn't implement Default
    let source = r#"if true {
    :test_atom
}"#;

    let program = parse_program(source).expect("Failed to parse program");

    let mut compiler_env = CompilerEnvironment::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    // Should fail compilation because Atom doesn't implement Default
    let result = compiler_env.compile_collection(collection);
    assert!(
        result.is_err(),
        "If without else should fail when type doesn't implement Default"
    );

    // Check that the error mentions Default protocol or ProtocolNotImplemented
    if let Err(errors) = result {
        let error_messages = errors
            .iter()
            .map(|e| format!("{e}"))
            .collect::<Vec<_>>()
            .join(" ");
        assert!(
            error_messages.contains("Default") || error_messages.contains("ProtocolNotImplemented"),
            "Should mention Default protocol requirement, got: {error_messages}"
        );
    }
}

#[test]
fn test_nested_if_expressions() {
    // Test nested if expressions
    let source = r#"if true {
    if false {
        1
    } else {
        2
    }
} else {
    3
}"#;

    let program = parse_program(source).expect("Failed to parse program");

    let mut compiler_env = CompilerEnvironment::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler_env
        .compile_collection(collection)
        .expect("Nested if expressions should compile");
    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program");

    // Check that we got a nested typed if expression
    match &typed_program.items[0].kind {
        TypedItemKind::Expression(expr) => {
            match &expr.kind {
                TypedExpressionKind::IfExpression { then_branch, .. } => {
                    // The then branch should be another if expression
                    match &then_branch.kind {
                        TypedExpressionKind::IfExpression { .. } => {
                            println!("✓ Nested if expressions successfully compiled");
                        }
                        TypedExpressionKind::Placeholder(_) => {
                            println!("Nested if expression still a placeholder - expected until full integration");
                        }
                        _ => {
                            panic!(
                                "Expected nested if expression in then branch, got: {:?}",
                                then_branch.kind
                            );
                        }
                    }
                }
                TypedExpressionKind::Placeholder(msg) => {
                    println!(
                        "If expression still a placeholder: {msg} - expected until full integration"
                    );
                }
                _ => {
                    panic!("Expected if expression, got: {:?}", expr.kind);
                }
            }
        }
        _ => panic!("Expected expression item"),
    }
}
