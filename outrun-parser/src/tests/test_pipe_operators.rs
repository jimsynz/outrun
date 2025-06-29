use crate::{ast::*, parse_program};

fn extract_expression_from_program(program: &Program) -> &Expression {
    match &program.items[0].kind {
        ItemKind::Expression(expr) => expr,
        _ => panic!("Expected expression in program"),
    }
}

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
    let input = "value + 1 |> transform";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);
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
    let input = "value |> transform && result";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);
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
    let input = "value |> f1 |> f2";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);
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
    let input = "value |? f1 |? f2";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::PipeMaybe);
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
    let input = "value |> transform |? handle";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::PipeMaybe);
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
    let input = "data |> filter |> map |? unwrap";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::PipeMaybe);

            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Pipe);

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
    let input = "(value + 1) |> (transform * 2)";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);

            match &op.left.kind {
                ExpressionKind::Parenthesized(paren_expr) => match &paren_expr.kind {
                    ExpressionKind::BinaryOp(add_op) => {
                        assert_eq!(add_op.operator, BinaryOperator::Add);
                    }
                    _ => panic!("Expected addition inside parentheses"),
                },
                _ => panic!("Expected parenthesized expression"),
            }

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
        let formatted = format!("{result}");
        assert_eq!(formatted, *input);
    }
}

#[test]
fn test_pipe_precedence_comprehensive() {
    let input = "1 + 2 * 3 |> result";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Pipe);
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Add);
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
    let input = "\"Result: #{value |> transform}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::String(string_lit) => {
                assert_eq!(string_lit.parts.len(), 2);

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
        },
        _ => panic!("Expected expression"),
    }
}
