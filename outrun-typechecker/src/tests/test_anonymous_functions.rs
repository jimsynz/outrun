//! Integration tests for anonymous function type checking
//!
//! This module tests the comprehensive anonymous function type checking system including:
//! - Single-clause anonymous functions
//! - Multi-clause anonymous functions with guards
//! - Parameter signature consistency validation
//! - Return type consistency validation
//! - Guard expression validation (must return Boolean)
//! - Error cases with clear error messages

use crate::checker::{TypeChecker, TypedExpressionKind};
use crate::error::TypeError;
use outrun_parser::parse_program;

/// Helper function to create a TypeChecker for testing
fn create_test_type_checker() -> TypeChecker {
    TypeChecker::new()
}

/// Helper function to parse and type check a program
fn parse_and_typecheck(
    source: &str,
) -> Result<crate::checker::TypedProgram, Vec<crate::error::TypeError>> {
    let program = parse_program(source).expect("Failed to parse test program");
    let mut checker = create_test_type_checker();
    checker.check_program(&program)
}

/// Helper function to parse and type check an expression, expecting success
fn typecheck_expression_success(source: &str) -> crate::checker::TypedExpression {
    let full_program = format!("let test_expr = {}", source);
    let typed_program = parse_and_typecheck(&full_program).expect("Type checking should succeed");

    // Extract the expression from the let binding
    if let Some(item) = typed_program.items.first() {
        if let crate::checker::TypedItemKind::LetBinding(let_binding) = &item.kind {
            return let_binding.expression.clone();
        }
    }
    panic!("Expected let binding with expression")
}

/// Helper function to parse and type check an expression, expecting failure
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
            // Note: params are (AtomId, TypeId), so we'd need to look up the AtomId
            // For now, just check length
            assert!(clause.guard.is_none());

            // Function type is inferred correctly
            // Note: Actual function type validation depends on ConcreteType implementation
        }
        _ => panic!("Expected AnonymousFunction, got {:?}", expr.kind),
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

            // Guard should be typed as Boolean
            if let Some(guard) = &clause.guard {
                // Guard expression should be typed (x > 0 is a binary operation)
                match &guard.kind {
                    TypedExpressionKind::BinaryOp { .. } => {
                        // Success - guard is properly typed
                    }
                    _ => panic!("Expected binary operation for guard, got {:?}", guard.kind),
                }
            }
        }
        _ => panic!("Expected AnonymousFunction, got {:?}", expr.kind),
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

            // All clauses should have identical parameter signatures
            for clause in &clauses {
                assert_eq!(clause.params.len(), 1);
                // Note: params are (AtomId, TypeId), parameter names/types should be consistent
            }

            // First two clauses have guards, third is default
            assert!(clauses[0].guard.is_some());
            assert!(clauses[1].guard.is_some());
            assert!(clauses[2].guard.is_none());
        }
        _ => panic!("Expected AnonymousFunction, got {:?}", expr.kind),
    }
}

#[test]
fn test_multi_clause_with_different_parameter_names_same_types() {
    // Test that parameter names must be consistent (current implementation requirement)
    let error = typecheck_expression_error(
        "fn { 
            value: Integer when value > 0 -> value + 1
            num: Integer -> num - 1
        }",
    );

    match error {
        TypeError::ParameterSignatureMismatch { clause_index, .. } => {
            assert_eq!(clause_index, 1); // Second clause (0-indexed)
                                         // Parameter names are currently required to be consistent
        }
        _ => panic!("Expected ParameterSignatureMismatch, got {:?}", error),
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
            assert_eq!(clause_index, 1); // Second clause (0-indexed)
                                         // Different parameter types should be caught
        }
        _ => panic!("Expected ParameterSignatureMismatch, got {:?}", error),
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
            assert_eq!(clause_index, 1); // Second clause (0-indexed)
            assert!(expected_signature.contains("1 parameter"));
            assert!(found_signature.contains("2 parameters"));
        }
        _ => panic!("Expected ParameterSignatureMismatch, got {:?}", error),
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
            assert_eq!(clause_index, 1); // Second clause (0-indexed)
            assert!(
                expected_type.contains("String") || expected_type.contains("Outrun.Core.String")
            );
            assert!(found_type.contains("Integer") || found_type.contains("Outrun.Core.Integer64"));
        }
        _ => panic!("Expected ReturnTypeMismatch, got {:?}", error),
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
            assert_eq!(clause_index, 0); // First clause (0-indexed)
            assert!(found_type.contains("Integer") || found_type.contains("Outrun.Core.Integer64"));
        }
        _ => panic!("Expected InvalidAnonymousGuard, got {:?}", error),
    }
}

// TODO: Test pattern parameter mismatch once parser supports struct patterns in anonymous functions
// #[test]
// fn test_pattern_parameter_mismatch() {
//     // Pattern parameters not yet supported in anonymous functions
// }

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
                    // Block should be properly typed
                    assert!(!block.statements.is_empty());
                }
                _ => panic!("Expected block body"),
            }
        }
        _ => panic!("Expected AnonymousFunction, got {:?}", expr.kind),
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

            // First clause has block body, second has expression body
            match &clauses[0].body {
                crate::checker::TypedAnonymousBody::Block(_) => {}
                _ => panic!("Expected block body for first clause"),
            }

            match &clauses[1].body {
                crate::checker::TypedAnonymousBody::Expression(_) => {}
                _ => panic!("Expected expression body for second clause"),
            }
        }
        _ => panic!("Expected AnonymousFunction, got {:?}", expr.kind),
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
                    // Inner expression should also be an anonymous function
                    match &inner_expr.kind {
                        TypedExpressionKind::AnonymousFunction { .. } => {
                            // Success - nested anonymous function properly typed
                        }
                        _ => panic!("Expected nested AnonymousFunction"),
                    }
                }
                _ => panic!("Expected expression body with nested function"),
            }
        }
        _ => panic!("Expected AnonymousFunction, got {:?}", expr.kind),
    }
}

#[test]
fn test_anonymous_function_as_argument() {
    let expr = typecheck_expression_success("(fn { x: Integer -> x * 2 }, [1, 2, 3])");

    match expr.kind {
        TypedExpressionKind::Tuple { elements, .. } => {
            // Should have two elements: function and list
            assert_eq!(elements.len(), 2);

            // First element should be an anonymous function
            let func_element = &elements[0];
            match &func_element.kind {
                TypedExpressionKind::AnonymousFunction { .. } => {
                    // Success - anonymous function in tuple
                }
                _ => panic!("Expected AnonymousFunction as tuple element"),
            }
        }
        _ => panic!("Expected Tuple, got {:?}", expr.kind),
    }
}

#[test]
fn test_anonymous_function_type_inference() {
    // Test that function type is properly inferred for assignment
    let typed_program = parse_and_typecheck("let processor = fn { x: Integer -> x + 1 }")
        .expect("Type checking should succeed");

    // Extract the let binding
    if let Some(item) = typed_program.items.first() {
        if let crate::checker::TypedItemKind::LetBinding(let_binding) = &item.kind {
            match &let_binding.expression.kind {
                TypedExpressionKind::AnonymousFunction {
                    function_type: _, ..
                } => {
                    // Function type should match the annotation
                    // Specific validation depends on Function type implementation
                }
                _ => panic!("Expected AnonymousFunction"),
            }
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

            // First clause should have complex Boolean guard
            if let Some(guard) = &clauses[0].guard {
                // Complex guard expression should be properly typed
                match &guard.kind {
                    TypedExpressionKind::BinaryOp { .. } => {
                        // Should be && operator with Boolean operands
                    }
                    _ => panic!("Expected binary operation for complex guard"),
                }
            }
        }
        _ => panic!("Expected AnonymousFunction, got {:?}", expr.kind),
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
            // Note: params are (AtomId, TypeId), names are interned as AtomIds
        }
        _ => panic!("Expected AnonymousFunction, got {:?}", expr.kind),
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
            // Should have proper source spans for both clauses
            assert!(first_clause_span.offset() < span.offset());
        }
        _ => panic!("Expected ReturnTypeMismatch, got {:?}", error),
    }
}
