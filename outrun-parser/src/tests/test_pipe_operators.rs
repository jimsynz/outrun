// Pipe operator parsing tests
// Tests for |> and |? operators

use crate::{ast::*, parse_program};

// Helper function to extract expression from program
fn extract_expression_from_program(program: &Program) -> &Expression {
    match &program.items[0].kind {
        ItemKind::Expression(expr) => expr,
        _ => panic!("Expected expression in program"),
    }
}

// Helper function to assert binary operation
fn assert_binary_operation(expr: &Expression, expected_op: BinaryOperator) {
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, expected_op);
        }
        _ => panic!("Expected binary operation, got: {:?}", expr.kind),
    }
}

#[test]
fn test_parse_pipe() {
    let input = "value |> transform";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    assert_binary_operation(expr, BinaryOperator::Pipe);
}

#[test]
fn test_parse_pipe_maybe() {
    let input = "value |? transform";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    assert_binary_operation(expr, BinaryOperator::PipeMaybe);
}

#[test]
fn test_pipe_precedence_lowest() {
    // value + 1 |> transform should parse as (value + 1) |> transform
    let input = "value + 1 |> transform";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);
            // Left side should be addition
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Add);
                }
                _ => panic!("Expected addition on left side"),
            }
        }
        _ => panic!("Expected pipe operation"),
    }
}

#[test]
fn test_pipe_precedence_before_logical() {
    // value |> transform && result should parse as value |> (transform && result)
    let input = "value |> transform && result";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);
            // Right side should be logical AND
            match &op.right.kind {
                ExpressionKind::BinaryOp(right_op) => {
                    assert_eq!(right_op.operator, BinaryOperator::LogicalAnd);
                }
                _ => panic!("Expected logical AND on right side"),
            }
        }
        _ => panic!("Expected pipe operation"),
    }
}

#[test]
fn test_left_associativity_pipe() {
    // value |> f1 |> f2 should parse as (value |> f1) |> f2
    let input = "value |> f1 |> f2";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);
            // Left side should also be pipe
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Pipe);
                }
                _ => panic!("Expected pipe on left side for left associativity"),
            }
        }
        _ => panic!("Expected pipe operation"),
    }
}

#[test]
fn test_left_associativity_pipe_maybe() {
    // value |? f1 |? f2 should parse as (value |? f1) |? f2
    let input = "value |? f1 |? f2";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::PipeMaybe);
            // Left side should also be pipe maybe
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::PipeMaybe);
                }
                _ => panic!("Expected pipe maybe on left side for left associativity"),
            }
        }
        _ => panic!("Expected pipe maybe operation"),
    }
}

#[test]
fn test_mixed_pipe_operators() {
    // value |> transform |? handle should parse as (value |> transform) |? handle
    let input = "value |> transform |? handle";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::PipeMaybe);
            // Left side should be pipe
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Pipe);
                }
                _ => panic!("Expected pipe on left side"),
            }
        }
        _ => panic!("Expected pipe maybe operation"),
    }
}

#[test]
fn test_complex_pipe_expression() {
    // Test: data |> filter(predicate: valid?) |> map(func: transform) |? unwrap
    let input = "data |> filter |> map |? unwrap";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);

    // Should parse as: ((data |> filter) |> map) |? unwrap
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::PipeMaybe);

            // Left side should be the chain of pipe operations
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Pipe);

                    // Left side of that should be another pipe
                    match &left_op.left.kind {
                        ExpressionKind::BinaryOp(inner_left_op) => {
                            assert_eq!(inner_left_op.operator, BinaryOperator::Pipe);
                        }
                        _ => panic!("Expected innermost pipe operation"),
                    }
                }
                _ => panic!("Expected pipe operation in chain"),
            }
        }
        _ => panic!("Expected pipe maybe at top level"),
    }
}

#[test]
fn test_pipe_with_parentheses() {
    // (value + 1) |> (transform * 2) should parse correctly
    let input = "(value + 1) |> (transform * 2)";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);

            // Left side should be parenthesized
            match &op.left.kind {
                ExpressionKind::Parenthesized(paren_expr) => match &paren_expr.kind {
                    ExpressionKind::BinaryOp(add_op) => {
                        assert_eq!(add_op.operator, BinaryOperator::Add);
                    }
                    _ => panic!("Expected addition inside parentheses"),
                },
                _ => panic!("Expected parenthesized expression"),
            }

            // Right side should also be parenthesized
            match &op.right.kind {
                ExpressionKind::Parenthesized(paren_expr) => match &paren_expr.kind {
                    ExpressionKind::BinaryOp(mult_op) => {
                        assert_eq!(mult_op.operator, BinaryOperator::Multiply);
                    }
                    _ => panic!("Expected multiplication inside parentheses"),
                },
                _ => panic!("Expected parenthesized expression"),
            }
        }
        _ => panic!("Expected pipe operation"),
    }
}

#[test]
fn test_pipe_display_preserves_format() {
    let inputs = [
        "value |> transform",
        "value |? handle",
        "a |> b |> c",
        "x |? y |? z",
        "data |> process |? validate",
    ];

    for input in inputs.iter() {
        let result = parse_program(input).unwrap();
        let formatted = format!("{}", result);
        assert_eq!(formatted, *input);
    }
}

#[test]
fn test_pipe_precedence_comprehensive() {
    // Test that pipe has lower precedence than all other operators
    // 1 + 2 * 3 |> result should parse as (1 + (2 * 3)) |> result
    let input = "1 + 2 * 3 |> result";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);
            // Left side should be addition
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Add);
                    // Right side of addition should be multiplication
                    match &left_op.right.kind {
                        ExpressionKind::BinaryOp(mult_op) => {
                            assert_eq!(mult_op.operator, BinaryOperator::Multiply);
                        }
                        _ => panic!("Expected multiplication"),
                    }
                }
                _ => panic!("Expected addition on left side"),
            }
        }
        _ => panic!("Expected pipe operation"),
    }
}

#[test]
fn test_pipe_in_string_interpolation() {
    // Test that pipes work correctly in string interpolations
    let input = "\"Result: #{value |> transform}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::String(string_lit) => {
                    assert_eq!(string_lit.parts.len(), 2);

                    // Second part should be interpolation
                    match &string_lit.parts[1] {
                        StringPart::Interpolation { expression, .. } => match &expression.kind {
                            ExpressionKind::BinaryOp(pipe_op) => {
                                assert_eq!(pipe_op.operator, BinaryOperator::Pipe);
                            }
                            _ => panic!("Expected pipe operation in interpolation"),
                        },
                        _ => panic!("Expected interpolation part"),
                    }
                }
                _ => panic!("Expected string literal"),
            }
        }
        _ => panic!("Expected expression"),
    }
}
