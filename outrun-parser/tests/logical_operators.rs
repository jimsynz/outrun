// Logical operator parsing tests
// Tests for &&, ||, and ! operators with proper precedence

use outrun_parser::{ast::*, parse_program};

// Helper function to extract expression from program
fn extract_expression_from_program(program: &Program) -> &Expression {
    match &program.items[0].kind {
        ItemKind::Expression(expr) => expr,
        _ => panic!("Expected expression in program"),
    }
}

// Helper function to extract binary operation from expression
fn extract_binary_op_from_expression(expr: &Expression) -> &BinaryOperation {
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => op,
        _ => panic!(
            "Expected binary operation in expression, got: {:?}",
            expr.kind
        ),
    }
}

// Helper function to extract unary operation from expression
fn extract_unary_op_from_expression(expr: &Expression) -> &UnaryOperation {
    match &expr.kind {
        ExpressionKind::UnaryOp(op) => op,
        _ => panic!(
            "Expected unary operation in expression, got: {:?}",
            expr.kind
        ),
    }
}

#[test]
fn test_parse_logical_and() {
    let input = "true && false";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let binary_op = extract_binary_op_from_expression(expr);

    assert_eq!(binary_op.operator, BinaryOperator::LogicalAnd);

    // Left operand should be true
    match &binary_op.left.kind {
        ExpressionKind::Boolean(lit) => assert!(lit.value),
        _ => panic!("Expected boolean literal"),
    }

    // Right operand should be false
    match &binary_op.right.kind {
        ExpressionKind::Boolean(lit) => assert!(!lit.value),
        _ => panic!("Expected boolean literal"),
    }
}

#[test]
fn test_parse_logical_or() {
    let input = "true || false";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let binary_op = extract_binary_op_from_expression(expr);

    assert_eq!(binary_op.operator, BinaryOperator::LogicalOr);

    // Left operand should be true
    match &binary_op.left.kind {
        ExpressionKind::Boolean(lit) => assert!(lit.value),
        _ => panic!("Expected boolean literal"),
    }

    // Right operand should be false
    match &binary_op.right.kind {
        ExpressionKind::Boolean(lit) => assert!(!lit.value),
        _ => panic!("Expected boolean literal"),
    }
}

#[test]
fn test_parse_logical_not() {
    let input = "!true";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let unary_op = extract_unary_op_from_expression(expr);

    assert_eq!(unary_op.operator, UnaryOperator::LogicalNot);

    // Operand should be true
    match &unary_op.operand.kind {
        ExpressionKind::Boolean(lit) => assert!(lit.value),
        _ => panic!("Expected boolean literal"),
    }
}

#[test]
fn test_logical_precedence_and_before_or() {
    // Should parse as: true || (false && true)
    let input = "true || false && true";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let or_op = extract_binary_op_from_expression(expr);

    // Top level should be OR
    assert_eq!(or_op.operator, BinaryOperator::LogicalOr);

    // Left should be true
    match &or_op.left.kind {
        ExpressionKind::Boolean(lit) => assert!(lit.value),
        _ => panic!("Expected boolean literal"),
    }

    // Right should be (false && true)
    match &or_op.right.kind {
        ExpressionKind::BinaryOp(and_op) => {
            assert_eq!(and_op.operator, BinaryOperator::LogicalAnd);

            // Left of AND should be false
            match &and_op.left.kind {
                ExpressionKind::Boolean(lit) => assert!(!lit.value),
                _ => panic!("Expected boolean literal"),
            }

            // Right of AND should be true
            match &and_op.right.kind {
                ExpressionKind::Boolean(lit) => assert!(lit.value),
                _ => panic!("Expected boolean literal"),
            }
        }
        _ => panic!("Expected AND operation"),
    }
}

#[test]
fn test_logical_precedence_comparison_before_and() {
    // Should parse as: (5 > 3) && (2 < 4)
    let input = "5 > 3 && 2 < 4";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let and_op = extract_binary_op_from_expression(expr);

    // Top level should be AND
    assert_eq!(and_op.operator, BinaryOperator::LogicalAnd);

    // Left should be (5 > 3)
    match &and_op.left.kind {
        ExpressionKind::BinaryOp(cmp_op) => {
            assert_eq!(cmp_op.operator, BinaryOperator::Greater);
        }
        _ => panic!("Expected comparison operation"),
    }

    // Right should be (2 < 4)
    match &and_op.right.kind {
        ExpressionKind::BinaryOp(cmp_op) => {
            assert_eq!(cmp_op.operator, BinaryOperator::Less);
        }
        _ => panic!("Expected comparison operation"),
    }
}

#[test]
fn test_logical_not_high_precedence() {
    // Should parse as: (!true) && false
    let input = "!true && false";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let and_op = extract_binary_op_from_expression(expr);

    // Top level should be AND
    assert_eq!(and_op.operator, BinaryOperator::LogicalAnd);

    // Left should be (!true)
    match &and_op.left.kind {
        ExpressionKind::UnaryOp(not_op) => {
            assert_eq!(not_op.operator, UnaryOperator::LogicalNot);
            match &not_op.operand.kind {
                ExpressionKind::Boolean(lit) => assert!(lit.value),
                _ => panic!("Expected boolean literal"),
            }
        }
        _ => panic!("Expected NOT operation"),
    }

    // Right should be false
    match &and_op.right.kind {
        ExpressionKind::Boolean(lit) => assert!(!lit.value),
        _ => panic!("Expected boolean literal"),
    }
}

#[test]
fn test_chained_logical_not() {
    // Should parse as: !!true
    let input = "!!true";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let outer_not = extract_unary_op_from_expression(expr);

    // Outer should be NOT
    assert_eq!(outer_not.operator, UnaryOperator::LogicalNot);

    // Inner should be NOT
    match &outer_not.operand.kind {
        ExpressionKind::UnaryOp(inner_not) => {
            assert_eq!(inner_not.operator, UnaryOperator::LogicalNot);

            // Innermost should be true
            match &inner_not.operand.kind {
                ExpressionKind::Boolean(lit) => assert!(lit.value),
                _ => panic!("Expected boolean literal"),
            }
        }
        _ => panic!("Expected NOT operation"),
    }
}

#[test]
fn test_parentheses_override_logical_precedence() {
    // Should parse as: true && (false || true)
    let input = "true && (false || true)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let and_op = extract_binary_op_from_expression(expr);

    // Top level should be AND
    assert_eq!(and_op.operator, BinaryOperator::LogicalAnd);

    // Left should be true
    match &and_op.left.kind {
        ExpressionKind::Boolean(lit) => assert!(lit.value),
        _ => panic!("Expected boolean literal"),
    }

    // Right should be parenthesized (false || true)
    match &and_op.right.kind {
        ExpressionKind::Parenthesized(inner) => match &inner.kind {
            ExpressionKind::BinaryOp(or_op) => {
                assert_eq!(or_op.operator, BinaryOperator::LogicalOr);
            }
            _ => panic!("Expected OR operation inside parentheses"),
        },
        _ => panic!("Expected parenthesized expression"),
    }
}

#[test]
fn test_mixed_logical_and_arithmetic() {
    // Should parse as: (5 + 3) > 7 && (2 * 4) == 8
    let input = "5 + 3 > 7 && 2 * 4 == 8";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let and_op = extract_binary_op_from_expression(expr);

    // Top level should be AND
    assert_eq!(and_op.operator, BinaryOperator::LogicalAnd);

    // Left should be comparison (5 + 3) > 7
    match &and_op.left.kind {
        ExpressionKind::BinaryOp(cmp_op) => {
            assert_eq!(cmp_op.operator, BinaryOperator::Greater);
            // Left of comparison should be addition
            match &cmp_op.left.kind {
                ExpressionKind::BinaryOp(add_op) => {
                    assert_eq!(add_op.operator, BinaryOperator::Add);
                }
                _ => panic!("Expected addition operation"),
            }
        }
        _ => panic!("Expected comparison operation"),
    }

    // Right should be comparison (2 * 4) == 8
    match &and_op.right.kind {
        ExpressionKind::BinaryOp(cmp_op) => {
            assert_eq!(cmp_op.operator, BinaryOperator::Equal);
            // Left of comparison should be multiplication
            match &cmp_op.left.kind {
                ExpressionKind::BinaryOp(mul_op) => {
                    assert_eq!(mul_op.operator, BinaryOperator::Multiply);
                }
                _ => panic!("Expected multiplication operation"),
            }
        }
        _ => panic!("Expected comparison operation"),
    }
}

#[test]
fn test_logical_operators_display_preserves_format() {
    let test_cases = [
        ("true && false", "true && false"),
        ("true || false", "true || false"),
        ("!true", "!true"),
        ("!!false", "!!false"),
        ("true && false || true", "true && false || true"),
        ("!true || false && !false", "!true || false && !false"),
    ];

    for (input, expected) in test_cases.iter() {
        let result = parse_program(input).unwrap();
        let formatted = format!("{}", result);
        assert_eq!(
            &formatted, expected,
            "Display format preservation failed for: {}",
            input
        );
    }
}

#[test]
fn test_left_associativity_logical_and() {
    // Should parse as: (true && false) && true
    let input = "true && false && true";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let outer_and = extract_binary_op_from_expression(expr);

    // Top level should be AND
    assert_eq!(outer_and.operator, BinaryOperator::LogicalAnd);

    // Left should be (true && false)
    match &outer_and.left.kind {
        ExpressionKind::BinaryOp(inner_and) => {
            assert_eq!(inner_and.operator, BinaryOperator::LogicalAnd);
        }
        _ => panic!("Expected AND operation"),
    }

    // Right should be true
    match &outer_and.right.kind {
        ExpressionKind::Boolean(lit) => assert!(lit.value),
        _ => panic!("Expected boolean literal"),
    }
}

#[test]
fn test_left_associativity_logical_or() {
    // Should parse as: (true || false) || true
    let input = "true || false || true";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    let expr = extract_expression_from_program(&result);
    let outer_or = extract_binary_op_from_expression(expr);

    // Top level should be OR
    assert_eq!(outer_or.operator, BinaryOperator::LogicalOr);

    // Left should be (true || false)
    match &outer_or.left.kind {
        ExpressionKind::BinaryOp(inner_or) => {
            assert_eq!(inner_or.operator, BinaryOperator::LogicalOr);
        }
        _ => panic!("Expected OR operation"),
    }

    // Right should be true
    match &outer_or.right.kind {
        ExpressionKind::Boolean(lit) => assert!(lit.value),
        _ => panic!("Expected boolean literal"),
    }
}
