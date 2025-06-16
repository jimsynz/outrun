use crate::checker::{TypeChecker, TypedExpressionKind};
use crate::error::TypeError;
use outrun_parser::parse_program;

fn create_test_type_checker() -> TypeChecker {
    TypeChecker::new()
}

fn parse_and_typecheck(
    source: &str,
) -> Result<crate::checker::TypedProgram, Vec<crate::error::TypeError>> {
    let program = parse_program(source).expect("Failed to parse test program");
    let mut checker = create_test_type_checker();
    checker.check_program(&program)
}

fn typecheck_expression_success(source: &str) -> crate::checker::TypedExpression {
    let full_program = format!("let test_expr = {}", source);
    let typed_program = parse_and_typecheck(&full_program).expect("Type checking should succeed");

    if let Some(item) = typed_program.items.first() {
        if let crate::checker::TypedItemKind::LetBinding(let_binding) = &item.kind {
            return let_binding.expression.clone();
        }
    }
    panic!("Expected let binding with expression")
}

fn typecheck_expression_error(source: &str) -> TypeError {
    let full_program = format!("let test_expr = {}", source);
    let errors = parse_and_typecheck(&full_program).expect_err("Type checking should fail");
    errors
        .into_iter()
        .next()
        .expect("Expected at least one error")
}

#[test]
fn test_single_clause_anonymous_function() {
    let expr = typecheck_expression_success("fn { x: Integer -> x + 1 }");

    match expr.kind {
        TypedExpressionKind::AnonymousFunction {
            clauses,
            function_type: _,
        } => {
            assert_eq!(clauses.len(), 1);

            let clause = &clauses[0];
            assert_eq!(clause.params.len(), 1);
            assert!(clause.guard.is_none());
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::AnonymousFunction { .. }),
            "Expected AnonymousFunction, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_single_clause_anonymous_function_with_guard() {
    let expr = typecheck_expression_success("fn { x: Integer when x > 0 -> x * 2 }");

    match expr.kind {
        TypedExpressionKind::AnonymousFunction { clauses, .. } => {
            assert_eq!(clauses.len(), 1);

            let clause = &clauses[0];
            assert_eq!(clause.params.len(), 1);
            assert!(clause.guard.is_some());

            if let Some(guard) = &clause.guard {
                assert!(
                    matches!(guard.kind, TypedExpressionKind::BinaryOp { .. }),
                    "Expected binary operation for guard, got {:?}",
                    guard.kind
                );
            }
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::AnonymousFunction { .. }),
            "Expected AnonymousFunction, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_multi_clause_anonymous_function_consistent_signature() {
    let expr = typecheck_expression_success(
        "fn { 
            x: Integer when x > 0 -> x * 2
            x: Integer when x < 0 -> x * -1
            x: Integer -> 0
        }",
    );

    match expr.kind {
        TypedExpressionKind::AnonymousFunction { clauses, .. } => {
            assert_eq!(clauses.len(), 3);

            for clause in &clauses {
                assert_eq!(clause.params.len(), 1);
            }

            assert!(clauses[0].guard.is_some());
            assert!(clauses[1].guard.is_some());
            assert!(clauses[2].guard.is_none());
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::AnonymousFunction { .. }),
            "Expected AnonymousFunction, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_multi_clause_with_different_parameter_names_same_types() {
    let error = typecheck_expression_error(
        "fn { 
            value: Integer when value > 0 -> value + 1
            num: Integer -> num - 1
        }",
    );

    match error {
        TypeError::ParameterSignatureMismatch { clause_index, .. } => {
            assert_eq!(clause_index, 1);
        }
        _ => assert!(
            matches!(error, TypeError::ParameterSignatureMismatch { .. }),
            "Expected ParameterSignatureMismatch, got {:?}",
            error
        ),
    }
}

#[test]
fn test_parameter_signature_mismatch_different_types() {
    let error = typecheck_expression_error(
        "fn { 
            x: Integer -> x + 1
            x: String -> String.length(x)
        }",
    );

    match error {
        TypeError::ParameterSignatureMismatch { clause_index, .. } => {
            assert_eq!(clause_index, 1);
        }
        _ => assert!(
            matches!(error, TypeError::ParameterSignatureMismatch { .. }),
            "Expected ParameterSignatureMismatch, got {:?}",
            error
        ),
    }
}

#[test]
fn test_parameter_signature_mismatch_different_arity() {
    let error = typecheck_expression_error(
        "fn { 
            x: Integer -> x + 1
            (x: Integer, y: Integer) -> x + y
        }",
    );

    match error {
        TypeError::ParameterSignatureMismatch {
            clause_index,
            expected_signature,
            found_signature,
            ..
        } => {
            assert_eq!(clause_index, 1);
            assert!(expected_signature.contains("1 parameter"));
            assert!(found_signature.contains("2 parameters"));
        }
        _ => assert!(
            matches!(error, TypeError::ParameterSignatureMismatch { .. }),
            "Expected ParameterSignatureMismatch, got {:?}",
            error
        ),
    }
}

#[test]
fn test_return_type_mismatch() {
    let error = typecheck_expression_error(
        "fn { 
            x: Integer when x > 0 -> \"positive\"
            x: Integer -> 0
        }",
    );

    match error {
        TypeError::ReturnTypeMismatch {
            clause_index,
            expected_type,
            found_type,
            ..
        } => {
            assert_eq!(clause_index, 1);
            assert!(
                expected_type.contains("String") || expected_type.contains("Outrun.Core.String")
            );
            assert!(found_type.contains("Integer") || found_type.contains("Outrun.Core.Integer64"));
        }
        _ => assert!(
            matches!(error, TypeError::ReturnTypeMismatch { .. }),
            "Expected ReturnTypeMismatch, got {:?}",
            error
        ),
    }
}

#[test]
fn test_invalid_guard_not_boolean() {
    let error = typecheck_expression_error("fn { x: Integer when x -> \"processed\" }");

    match error {
        TypeError::InvalidAnonymousGuard {
            clause_index,
            found_type,
            ..
        } => {
            assert_eq!(clause_index, 0);
            assert!(found_type.contains("Integer") || found_type.contains("Outrun.Core.Integer64"));
        }
        _ => assert!(
            matches!(error, TypeError::InvalidAnonymousGuard { .. }),
            "Expected InvalidAnonymousGuard, got {:?}",
            error
        ),
    }
}

#[test]
fn test_anonymous_function_with_block_body() {
    let expr = typecheck_expression_success(
        "fn { x: Integer -> { 
            let doubled = x * 2
            doubled + 1
        }}",
    );

    match expr.kind {
        TypedExpressionKind::AnonymousFunction { clauses, .. } => {
            assert_eq!(clauses.len(), 1);

            let clause = &clauses[0];
            match &clause.body {
                crate::checker::TypedAnonymousBody::Block(block) => {
                    assert!(!block.statements.is_empty());
                }
                _ => assert!(
                    matches!(clause.body, crate::checker::TypedAnonymousBody::Block(_)),
                    "Expected block body"
                ),
            }
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::AnonymousFunction { .. }),
            "Expected AnonymousFunction, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_anonymous_function_with_mixed_body_types() {
    let expr = typecheck_expression_success(
        "fn { 
            x: Integer when x > 0 -> { 
                let result = x * 2
                result
            }
            x: Integer -> x - 1
        }",
    );

    match expr.kind {
        TypedExpressionKind::AnonymousFunction { clauses, .. } => {
            assert_eq!(clauses.len(), 2);

            assert!(
                matches!(
                    clauses[0].body,
                    crate::checker::TypedAnonymousBody::Block(_)
                ),
                "Expected block body for first clause"
            );
            assert!(
                matches!(
                    clauses[1].body,
                    crate::checker::TypedAnonymousBody::Expression(_)
                ),
                "Expected expression body for second clause"
            );
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::AnonymousFunction { .. }),
            "Expected AnonymousFunction, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_nested_anonymous_functions() {
    let expr = typecheck_expression_success("fn { x: Integer -> fn { y: Integer -> x + y } }");

    match expr.kind {
        TypedExpressionKind::AnonymousFunction { clauses, .. } => {
            assert_eq!(clauses.len(), 1);

            let clause = &clauses[0];
            match &clause.body {
                crate::checker::TypedAnonymousBody::Expression(inner_expr) => {
                    assert!(
                        matches!(
                            inner_expr.kind,
                            TypedExpressionKind::AnonymousFunction { .. }
                        ),
                        "Expected nested AnonymousFunction"
                    );
                }
                _ => assert!(
                    matches!(
                        clause.body,
                        crate::checker::TypedAnonymousBody::Expression(_)
                    ),
                    "Expected expression body with nested function"
                ),
            }
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::AnonymousFunction { .. }),
            "Expected AnonymousFunction, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_anonymous_function_as_argument() {
    let expr = typecheck_expression_success("(fn { x: Integer -> x * 2 }, [1, 2, 3])");

    match expr.kind {
        TypedExpressionKind::Tuple { elements, .. } => {
            assert_eq!(elements.len(), 2);

            let func_element = &elements[0];
            assert!(
                matches!(
                    func_element.kind,
                    TypedExpressionKind::AnonymousFunction { .. }
                ),
                "Expected AnonymousFunction as tuple element"
            );
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::Tuple { .. }),
            "Expected Tuple, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_anonymous_function_type_inference() {
    let typed_program = parse_and_typecheck("let processor = fn { x: Integer -> x + 1 }")
        .expect("Type checking should succeed");

    if let Some(item) = typed_program.items.first() {
        if let crate::checker::TypedItemKind::LetBinding(let_binding) = &item.kind {
            assert!(
                matches!(
                    let_binding.expression.kind,
                    TypedExpressionKind::AnonymousFunction { .. }
                ),
                "Expected AnonymousFunction"
            );
        }
    }
}

#[test]
fn test_complex_guard_expressions() {
    let expr = typecheck_expression_success(
        "fn { 
            x: Integer when x > 0 && x % 2 == 0 -> x / 2
            x: Integer when x > 0 -> x - 1
            x: Integer -> 0
        }",
    );

    match expr.kind {
        TypedExpressionKind::AnonymousFunction { clauses, .. } => {
            assert_eq!(clauses.len(), 3);

            if let Some(guard) = &clauses[0].guard {
                assert!(
                    matches!(guard.kind, TypedExpressionKind::BinaryOp { .. }),
                    "Expected binary operation for complex guard"
                );
            }
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::AnonymousFunction { .. }),
            "Expected AnonymousFunction, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_anonymous_function_with_multiple_parameters() {
    let expr = typecheck_expression_success("fn { (x: Integer, y: Integer) -> x + y }");

    match expr.kind {
        TypedExpressionKind::AnonymousFunction { clauses, .. } => {
            assert_eq!(clauses.len(), 1);

            let clause = &clauses[0];
            assert_eq!(clause.params.len(), 2);
        }
        _ => assert!(
            matches!(expr.kind, TypedExpressionKind::AnonymousFunction { .. }),
            "Expected AnonymousFunction, got {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_anonymous_function_error_context() {
    let error = typecheck_expression_error(
        "fn { 
            x: Integer -> \"hello\"
            x: Integer -> 42
        }",
    );

    match error {
        TypeError::ReturnTypeMismatch {
            first_clause_span,
            span,
            ..
        } => {
            assert!(first_clause_span.offset() < span.offset());
        }
        _ => assert!(
            matches!(error, TypeError::ReturnTypeMismatch { .. }),
            "Expected ReturnTypeMismatch, got {:?}",
            error
        ),
    }
}
