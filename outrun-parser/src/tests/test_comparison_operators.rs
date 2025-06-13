// Comparison operators parsing tests
// Tests for ==, !=, <, <=, >, >= with proper precedence

use crate::{ast::*, parse_program};

// Helper function to extract expressions
fn extract_expression_from_item(item: &Item) -> &Expression {
    match &item.kind {
        ItemKind::Expression(expr) => expr,
        _ => panic!("Expected expression, got: {:?}", item.kind),
    }
}

// === BASIC COMPARISON TESTS ===

#[test]
fn test_parse_equality() {
    let input = "5 == 5";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Equal);

            // Check left operand
            match &op.left.kind {
                ExpressionKind::Integer(int) => assert_eq!(int.value, 5),
                _ => panic!("Expected integer on left"),
            }

            // Check right operand
            match &op.right.kind {
                ExpressionKind::Integer(int) => assert_eq!(int.value, 5),
                _ => panic!("Expected integer on right"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_parse_not_equal() {
    let input = "a != b";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::NotEqual);
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_parse_less_than() {
    let input = "3 < 5";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Less);
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_parse_less_equal() {
    let input = "3 <= 5";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::LessEqual);
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_parse_greater_than() {
    let input = "5 > 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Greater);
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_parse_greater_equal() {
    let input = "5 >= 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::GreaterEqual);
        }
        _ => panic!("Expected binary operation"),
    }
}

// === PRECEDENCE TESTS ===

#[test]
fn test_arithmetic_before_comparison() {
    let input = "2 + 3 > 4";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            // Should be: (2 + 3) > 4
            assert_eq!(op.operator, BinaryOperator::Greater);

            // Left should be (2 + 3)
            match &op.left.kind {
                ExpressionKind::BinaryOp(add_op) => {
                    assert_eq!(add_op.operator, BinaryOperator::Add);

                    match &add_op.left.kind {
                        ExpressionKind::Integer(int) => assert_eq!(int.value, 2),
                        _ => panic!("Expected integer in addition left"),
                    }

                    match &add_op.right.kind {
                        ExpressionKind::Integer(int) => assert_eq!(int.value, 3),
                        _ => panic!("Expected integer in addition right"),
                    }
                }
                _ => panic!("Expected addition on left side"),
            }

            // Right should be 4
            match &op.right.kind {
                ExpressionKind::Integer(int) => assert_eq!(int.value, 4),
                _ => panic!("Expected integer on right"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_multiplication_before_comparison() {
    let input = "2 * 3 == 6";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            // Should be: (2 * 3) == 6
            assert_eq!(op.operator, BinaryOperator::Equal);

            // Left should be (2 * 3)
            match &op.left.kind {
                ExpressionKind::BinaryOp(mult_op) => {
                    assert_eq!(mult_op.operator, BinaryOperator::Multiply);
                }
                _ => panic!("Expected multiplication on left side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_exponentiation_before_comparison() {
    let input = "2 ** 3 > 7";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            // Should be: (2 ** 3) > 7
            assert_eq!(op.operator, BinaryOperator::Greater);

            // Left should be (2 ** 3)
            match &op.left.kind {
                ExpressionKind::BinaryOp(exp_op) => {
                    assert_eq!(exp_op.operator, BinaryOperator::Exponent);
                }
                _ => panic!("Expected exponentiation on left side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

// === ASSOCIATIVITY TESTS ===

#[test]
fn test_left_associativity_comparison() {
    let input = "1 < 2 < 3";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            // Should be: (1 < 2) < 3
            assert_eq!(op.operator, BinaryOperator::Less);

            // Left should be (1 < 2)
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Less);
                }
                _ => panic!("Expected comparison on left side"),
            }

            // Right should be 3
            match &op.right.kind {
                ExpressionKind::Integer(int) => assert_eq!(int.value, 3),
                _ => panic!("Expected integer on right"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

// === MIXED COMPARISON TESTS ===

#[test]
fn test_mixed_comparison_operators() {
    let input = "a == b != c";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            // Should be: (a == b) != c (left associative)
            assert_eq!(op.operator, BinaryOperator::NotEqual);

            // Left should be (a == b)
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Equal);
                }
                _ => panic!("Expected equality on left side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

// === PARENTHESES TESTS ===

#[test]
fn test_parentheses_override_precedence() {
    let input = "2 < (3 + 4)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            // Should be: 2 < (3 + 4)
            assert_eq!(op.operator, BinaryOperator::Less);

            // Right should be parenthesized (3 + 4)
            match &op.right.kind {
                ExpressionKind::Parenthesized(paren_expr) => match &paren_expr.kind {
                    ExpressionKind::BinaryOp(add_op) => {
                        assert_eq!(add_op.operator, BinaryOperator::Add);
                    }
                    _ => panic!("Expected addition inside parentheses"),
                },
                _ => panic!("Expected parenthesized expression on right"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

// === COMPLEX EXPRESSION TESTS ===

#[test]
fn test_complex_comparison_expression() {
    let input = "2 + 3 * 4 >= 5 - 1 ** 2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_item(&result.items[0]);

    match &expr.kind {
        ExpressionKind::BinaryOp(op) => {
            // Should parse with correct precedence
            assert_eq!(op.operator, BinaryOperator::GreaterEqual);

            // Both sides should be complex arithmetic expressions
            match &op.left.kind {
                ExpressionKind::BinaryOp(left_op) => {
                    assert_eq!(left_op.operator, BinaryOperator::Add);
                }
                _ => panic!("Expected addition on left side"),
            }

            match &op.right.kind {
                ExpressionKind::BinaryOp(right_op) => {
                    assert_eq!(right_op.operator, BinaryOperator::Subtract);
                }
                _ => panic!("Expected subtraction on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

// === TYPE COMPATIBILITY TESTS ===

#[test]
fn test_compare_different_types() {
    let test_cases = vec![
        ("5 == 5.0", BinaryOperator::Equal),
        ("true != false", BinaryOperator::NotEqual),
        (":atom < :zebra", BinaryOperator::Less),
        ("\"a\" <= \"b\"", BinaryOperator::LessEqual),
    ];

    for (input, expected_op) in test_cases {
        let result = parse_program(input).unwrap();
        assert_eq!(result.items.len(), 1);

        let expr = extract_expression_from_item(&result.items[0]);
        match &expr.kind {
            ExpressionKind::BinaryOp(op) => {
                assert_eq!(op.operator, expected_op, "Failed for input: {}", input);
            }
            _ => panic!("Expected binary operation for input: {}", input),
        }
    }
}

// === DISPLAY TESTS ===

#[test]
fn test_comparison_display_preserves_format() {
    let test_cases = vec![
        "5 == 5",
        "a != b",
        "3 < 5",
        "3 <= 5",
        "5 > 3",
        "5 >= 3",
        "2 + 3 > 4",
        "(2 + 3) > 4",
        "1 < 2 < 3",
        "2 + 3 * 4 >= 5",
    ];

    for input in test_cases {
        let result = parse_program(input).unwrap();
        let formatted = format!("{}", result);
        assert_eq!(formatted, input, "Display format mismatch for: {}", input);
    }
}
