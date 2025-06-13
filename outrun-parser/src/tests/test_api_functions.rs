// Test public API functions
// Ensures public parse functions work correctly

use crate::*;

#[test]
fn test_parse_expression_api() {
    let input = "1 + 2 * 3";
    let result = parse_expression(input).unwrap();

    // Should parse as: 1 + (2 * 3) due to precedence
    match result.kind {
        ExpressionKind::BinaryOp(op) => {
            assert_eq!(op.operator, BinaryOperator::Add);
            // Left should be integer 1
            match &op.left.kind {
                ExpressionKind::Integer(int_lit) => {
                    assert_eq!(int_lit.value, 1);
                }
                _ => panic!("Expected integer literal on left"),
            }
            // Right should be 2 * 3
            match &op.right.kind {
                ExpressionKind::BinaryOp(right_op) => {
                    assert_eq!(right_op.operator, BinaryOperator::Multiply);
                }
                _ => panic!("Expected multiplication on right"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_parse_program_api() {
    let input = "let x = 42\nlet y = true";
    let program = parse_program(input).unwrap();

    assert_eq!(program.items.len(), 2); // Two let bindings (newline is whitespace)
}

#[test]
fn test_parse_simple_expression() {
    let inputs = vec![
        "42",
        "true",
        "\"hello\"",
        ":atom",
        "[1, 2, 3]",
        "{key: value}",
        "(1, 2)",
    ];

    for input in inputs {
        let result = parse_expression(input);
        assert!(
            result.is_ok(),
            "Failed to parse expression: {} - Error: {:?}",
            input,
            result.err()
        );
    }
}
