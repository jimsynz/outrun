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

fn assert_unary_operation(expr: &Expression, expected_op: UnaryOperator) {
    match &expr.kind {
        ExpressionKind::UnaryOp(op) => {
            assert_eq!(op.operator, expected_op);
        }
        _ => panic!("Expected unary operation, got: {:?}", expr.kind),
    }
}

#[test]
fn test_parse_bitwise_and() {
    let input = "5 & 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    assert_binary_operation(expr, BinaryOperator::BitwiseAnd);
}

#[test]
fn test_parse_bitwise_or() {
    let input = "5 | 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    assert_binary_operation(expr, BinaryOperator::BitwiseOr);
}

#[test]
fn test_parse_bitwise_xor() {
    let input = "5 ^ 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    assert_binary_operation(expr, BinaryOperator::BitwiseXor);
}

#[test]
fn test_parse_shift_left() {
    let input = "8 << 2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    assert_binary_operation(expr, BinaryOperator::ShiftLeft);
}

#[test]
fn test_parse_shift_right() {
    let input = "8 >> 2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    assert_binary_operation(expr, BinaryOperator::ShiftRight);
}

#[test]
fn test_parse_bitwise_not() {
    let input = "~5";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    assert_unary_operation(expr, UnaryOperator::BitwiseNot);
}

#[test]
fn test_bitwise_precedence_or_before_xor() {
    let input = "1 | 2 ^ 3";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::BitwiseOr);
            match &op.right.kind {
                ExpressionKind::BinaryOp(right_op) => {
                    assert_eq!(right_op.operator, BinaryOperator::BitwiseXor);
                }
                _ => panic!("Expected XOR on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_bitwise_precedence_xor_before_and() {
    let input = "1 ^ 2 & 3";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::BitwiseXor);
            match &op.right.kind {
                ExpressionKind::BinaryOp(right_op) => {
                    assert_eq!(right_op.operator, BinaryOperator::BitwiseAnd);
                }
                _ => panic!("Expected AND on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_bitwise_precedence_and_before_comparison() {
    let input = "1 & 2 == 3";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Equal);
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::BitwiseAnd);
                }
                _ => panic!("Expected AND on left side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_shift_precedence_before_addition() {
    let input = "1 << 2 + 3";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::ShiftLeft);
            match &op.right.kind {
                ExpressionKind::BinaryOp(right_op) => {
                    assert_eq!(right_op.operator, BinaryOperator::Add);
                }
                _ => panic!("Expected addition on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_bitwise_not_high_precedence() {
    let input = "~1 + 2";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Add);
            match &op.left.kind {
                ExpressionKind::UnaryOp(left_op) => {
                    assert_eq!(left_op.operator, UnaryOperator::BitwiseNot);
                }
                _ => panic!("Expected bitwise NOT on left side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_left_associativity_bitwise_or() {
    let input = "1 | 2 | 3";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::BitwiseOr);
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::BitwiseOr);
                }
                _ => panic!("Expected OR on left side for left associativity"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_left_associativity_shift() {
    let input = "16 >> 2 >> 1";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::ShiftRight);
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::ShiftRight);
                }
                _ => panic!("Expected shift right on left side for left associativity"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_complex_bitwise_expression() {
    let input = "~(1 | 2) & (3 ^ 4) << 1";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::BitwiseAnd);

            match &op.left.kind {
                ExpressionKind::UnaryOp(not_op) => {
                    assert_eq!(not_op.operator, UnaryOperator::BitwiseNot);
                }
                _ => panic!("Expected bitwise NOT"),
            }

            match &op.right.kind {
                ExpressionKind::BinaryOp(shift_op) => {
                    assert_eq!(shift_op.operator, BinaryOperator::ShiftLeft);

                    match &shift_op.left.kind {
                        ExpressionKind::Parenthesized(paren_expr) => match &paren_expr.kind {
                            ExpressionKind::BinaryOp(xor_op) => {
                                assert_eq!(xor_op.operator, BinaryOperator::BitwiseXor);
                            }
                            _ => panic!("Expected XOR inside parentheses"),
                        },
                        _ => panic!("Expected parenthesized expression"),
                    }
                }
                _ => panic!("Expected shift operation"),
            }
        }
        _ => panic!("Expected binary operation at top level"),
    }
}

#[test]
fn test_chained_bitwise_not() {
    let input = "~~5";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::UnaryOp(op) => {
            assert_eq!(op.operator, UnaryOperator::BitwiseNot);
            match &op.operand.kind {
                ExpressionKind::UnaryOp(inner_op) => {
                    assert_eq!(inner_op.operator, UnaryOperator::BitwiseNot);
                }
                _ => panic!("Expected inner bitwise NOT"),
            }
        }
        _ => panic!("Expected unary operation"),
    }
}

#[test]
fn test_bitwise_display_preserves_format() {
    let inputs = ["5 & 3", "5 | 3", "5 ^ 3", "8 << 2", "8 >> 2", "~5"];

    for input in inputs.iter() {
        let result = parse_program(input).unwrap();
        let formatted = format!("{result}");
        assert_eq!(formatted, *input);
    }
}

#[test]
fn test_parentheses_override_bitwise_precedence() {
    let input = "(1 | 2) & 3";
    let result = parse_program(input).unwrap();

    let expr = extract_expression_from_program(&result);
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::BitwiseAnd);
            match &op.left.kind {
                ExpressionKind::Parenthesized(paren_expr) => match &paren_expr.kind {
                    ExpressionKind::BinaryOp(or_op) => {
                        assert_eq!(or_op.operator, BinaryOperator::BitwiseOr);
                    }
                    _ => panic!("Expected OR inside parentheses"),
                },
                _ => panic!("Expected parenthesized expression"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}
