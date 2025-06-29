use crate::{ast::*, parse_program};

#[test]
fn test_parse_addition() {
    let input = "2 + 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Add);

                match &op.left.kind {
                    ExpressionKind::Integer(int) => assert_eq!(int.value, 2),
                    _ => panic!("Expected integer on left"),
                }

                match &op.right.kind {
                    ExpressionKind::Integer(int) => assert_eq!(int.value, 3),
                    _ => panic!("Expected integer on right"),
                }
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_subtraction() {
    let input = "10 - 4";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Subtract);
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_multiplication() {
    let input = "4 * 6";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Multiply);
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_division() {
    let input = "8 / 2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Divide);
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_modulo() {
    let input = "10 % 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Modulo);
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_exponentiation() {
    let input = "2 ** 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Exponent);
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_unary_minus() {
    let input = "-5";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::UnaryOp(op) => {
                assert_eq!(op.operator, UnaryOperator::Minus);

                match &op.operand.kind {
                    ExpressionKind::Integer(int) => assert_eq!(int.value, 5),
                    _ => panic!("Expected integer operand"),
                }
            }
            _ => panic!("Expected unary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_unary_plus() {
    let input = "+7";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::UnaryOp(op) => {
                assert_eq!(op.operator, UnaryOperator::Plus);
            }
            _ => panic!("Expected unary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_multiplication_before_addition() {
    let input = "2 + 3 * 4";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Add);

                match &op.left.kind {
                    ExpressionKind::Integer(int) => assert_eq!(int.value, 2),
                    _ => panic!("Expected integer on left"),
                }

                match &op.right.kind {
                    ExpressionKind::BinaryOp(multiply_op) => {
                        assert_eq!(multiply_op.operator, BinaryOperator::Multiply);

                        match &multiply_op.left.kind {
                            ExpressionKind::Integer(int) => assert_eq!(int.value, 3),
                            _ => panic!("Expected integer in multiplication left"),
                        }

                        match &multiply_op.right.kind {
                            ExpressionKind::Integer(int) => assert_eq!(int.value, 4),
                            _ => panic!("Expected integer in multiplication right"),
                        }
                    }
                    _ => panic!("Expected multiplication on right side"),
                }
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_division_before_subtraction() {
    let input = "10 - 8 / 2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Subtract);

                match &op.right.kind {
                    ExpressionKind::BinaryOp(divide_op) => {
                        assert_eq!(divide_op.operator, BinaryOperator::Divide);
                    }
                    _ => panic!("Expected division on right side"),
                }
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_exponentiation_before_multiplication() {
    let input = "2 * 3 ** 2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Multiply);

                match &op.right.kind {
                    ExpressionKind::BinaryOp(exponent_op) => {
                        assert_eq!(exponent_op.operator, BinaryOperator::Exponent);
                    }
                    _ => panic!("Expected exponentiation on right side"),
                }
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_left_associativity_addition() {
    let input = "1 + 2 + 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Add);

                match &op.left.kind {
                    ExpressionKind::BinaryOp(left_op) => {
                        assert_eq!(left_op.operator, BinaryOperator::Add);
                    }
                    _ => panic!("Expected addition on left side"),
                }

                match &op.right.kind {
                    ExpressionKind::Integer(int) => assert_eq!(int.value, 3),
                    _ => panic!("Expected integer on right"),
                }
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_right_associativity_exponentiation() {
    let input = "2 ** 3 ** 2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Exponent);

                match &op.left.kind {
                    ExpressionKind::Integer(int) => assert_eq!(int.value, 2),
                    _ => panic!("Expected integer on left"),
                }

                match &op.right.kind {
                    ExpressionKind::BinaryOp(right_op) => {
                        assert_eq!(right_op.operator, BinaryOperator::Exponent);
                    }
                    _ => panic!("Expected exponentiation on right side"),
                }
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parentheses_override_precedence() {
    let input = "(2 + 3) * 4";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Multiply);

                match &op.left.kind {
                    ExpressionKind::Parenthesized(paren_expr) => match &paren_expr.kind {
                        ExpressionKind::BinaryOp(add_op) => {
                            assert_eq!(add_op.operator, BinaryOperator::Add);
                        }
                        _ => panic!("Expected addition inside parentheses"),
                    },
                    _ => panic!("Expected parenthesized expression on left"),
                }
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_complex_arithmetic_expression() {
    let input = "2 + 3 * 4 - 5 / 2 ** 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, BinaryOperator::Subtract);
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_chained_unary_operators() {
    let input = "--5";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::UnaryOp(op) => {
                assert_eq!(op.operator, UnaryOperator::Minus);

                match &op.operand.kind {
                    ExpressionKind::UnaryOp(inner_op) => {
                        assert_eq!(inner_op.operator, UnaryOperator::Minus);
                    }
                    _ => panic!("Expected nested unary operation"),
                }
            }
            _ => panic!("Expected unary operation"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_arithmetic_display_preserves_format() {
    let test_cases = vec![
        "2 + 3",
        "10 - 4",
        "4 * 6",
        "8 / 2",
        "10 % 3",
        "2 ** 3",
        "-5",
        "+7",
        "2 + 3 * 4",
        "(2 + 3) * 4",
        "2 ** 3 ** 2",
    ];

    for input in test_cases {
        let result = parse_program(input).unwrap();
        let formatted = format!("{result}");
        assert_eq!(formatted, input, "Display format mismatch for: {input}");
    }
}
