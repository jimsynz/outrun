// Test case/when expressions parsing with Pest
// Comprehensive tests for case/when pattern matching

use outrun_parser::ast::*;
use outrun_parser::parser::OutrunParser;

#[test]
fn test_case_with_single_when_clause() {
    let input = r#"case x { when x > 0 -> "pos" else -> "neg" }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::CaseExpression(case_expr) => {
                    // Check case expression value
                    match &case_expr.expression.kind {
                        ExpressionKind::Identifier(id) => {
                            assert_eq!(id.name, "x");
                        }
                        _ => panic!("Expected identifier for case expression"),
                    }

                    // Check when clauses
                    assert_eq!(case_expr.when_clauses.len(), 1);
                    let when_clause = &case_expr.when_clauses[0];

                    // Check guard is a comparison
                    match &when_clause.guard.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::Greater);
                        }
                        _ => panic!("Expected binary operation guard"),
                    }

                    // Check when result
                    match &when_clause.result {
                        CaseResult::Expression(expr) => match &expr.kind {
                            ExpressionKind::String(string_lit) => {
                                assert_eq!(string_lit.format, StringFormat::Basic);
                            }
                            _ => panic!("Expected string literal"),
                        },
                        _ => panic!("Expected expression result"),
                    }

                    // Check else clause
                    match &case_expr.else_clause.result {
                        CaseResult::Expression(expr) => match &expr.kind {
                            ExpressionKind::String(string_lit) => {
                                assert_eq!(string_lit.format, StringFormat::Basic);
                            }
                            _ => panic!("Expected string literal"),
                        },
                        _ => panic!("Expected expression result"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_case_with_multiple_when_clauses() {
    let input = r#"case value {
        when value > 100 -> "large"
        when value > 0 -> "positive"
        when value == 0 -> "zero"
        else -> "negative"
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::CaseExpression(case_expr) => {
                    // Check we have 3 when clauses
                    assert_eq!(case_expr.when_clauses.len(), 3);

                    // Check first when clause (value > 100)
                    match &case_expr.when_clauses[0].guard.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::Greater);
                        }
                        _ => panic!("Expected binary operation guard"),
                    }

                    // Check second when clause (value > 0)
                    match &case_expr.when_clauses[1].guard.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::Greater);
                        }
                        _ => panic!("Expected binary operation guard"),
                    }

                    // Check third when clause (value == 0)
                    match &case_expr.when_clauses[2].guard.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::Equal);
                        }
                        _ => panic!("Expected binary operation guard"),
                    }

                    // Check else clause exists
                    match &case_expr.else_clause.result {
                        CaseResult::Expression(_) => {}
                        _ => panic!("Expected expression result in else"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_case_with_block_results() {
    let input = r#"case value {
        when positive?(value: value) -> {
            warn(message: "positive value")
            "positive"
        }
        else -> "other"
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.when_clauses.len(), 1);

                    // Check when clause has block result
                    match &case_expr.when_clauses[0].result {
                        CaseResult::Block(block) => {
                            // Should have 2 statements: function call and string
                            assert_eq!(block.statements.len(), 2);
                        }
                        _ => panic!("Expected block result"),
                    }

                    // Check else clause has expression result
                    match &case_expr.else_clause.result {
                        CaseResult::Expression(_) => {}
                        _ => panic!("Expected expression result in else"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_case_with_function_call_guard() {
    let input = r#"case data {
        when String.not_empty?(data) && String.valid_format?(data) -> "valid"
        when String.empty?(data) -> "empty"
        else -> "invalid"
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.when_clauses.len(), 2);

                    // Check first guard is logical AND of two function calls
                    match &case_expr.when_clauses[0].guard.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::LogicalAnd);
                        }
                        _ => panic!("Expected logical AND guard"),
                    }

                    // Check second guard is a function call
                    match &case_expr.when_clauses[1].guard.kind {
                        ExpressionKind::FunctionCall(call) => match &call.path {
                            FunctionPath::Qualified { module, name } => {
                                assert_eq!(module.name, "String");
                                assert_eq!(name.name, "empty?");
                            }
                            _ => panic!("Expected qualified function call"),
                        },
                        _ => panic!("Expected function call guard"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_nested_case_expressions() {
    let input = r#"case value {
        when Integer.type?(value) -> {
            case value {
                when value > 0 -> "positive integer"
                else -> "negative integer"
            }
        }
        else -> "not integer"
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::CaseExpression(outer_case) => {
                    assert_eq!(outer_case.when_clauses.len(), 1);

                    // Check when clause has block result with nested case
                    match &outer_case.when_clauses[0].result {
                        CaseResult::Block(block) => {
                            assert_eq!(block.statements.len(), 1);

                            // Check the statement is a case expression
                            match &block.statements[0].kind {
                                StatementKind::Expression(expr) => match &expr.kind {
                                    ExpressionKind::CaseExpression(inner_case) => {
                                        assert_eq!(inner_case.when_clauses.len(), 1);
                                    }
                                    _ => panic!("Expected nested case expression"),
                                },
                                _ => panic!("Expected expression statement"),
                            }
                        }
                        _ => panic!("Expected block result"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_case_in_function_body() {
    let input = r#"def classify(value: Integer): String {
        case value {
            when positive?(value: value) -> "positive"
            when negative?(value: value) -> "negative"
            else -> "unknown"
        }
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name.name, "classify");
            assert_eq!(func.body.statements.len(), 1);

            // Check function body contains case expression
            match &func.body.statements[0].kind {
                StatementKind::Expression(expr) => match &expr.kind {
                    ExpressionKind::CaseExpression(case_expr) => {
                        assert_eq!(case_expr.when_clauses.len(), 2);
                    }
                    _ => panic!("Expected case expression in function body"),
                },
                _ => panic!("Expected expression statement"),
            }
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_case_with_integer_literal_guards() {
    let input = r#"case status_code {
        when status_code == 200 -> "OK"
        when status_code == 404 -> "Not Found"
        when status_code >= 500 -> "Server Error"
        else -> "Unknown"
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.when_clauses.len(), 3);

                    // Check first guard (== 200)
                    match &case_expr.when_clauses[0].guard.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::Equal);
                        }
                        _ => panic!("Expected equality guard"),
                    }

                    // Check second guard (== 404)
                    match &case_expr.when_clauses[1].guard.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::Equal);
                        }
                        _ => panic!("Expected equality guard"),
                    }

                    // Check third guard (>= 500)
                    match &case_expr.when_clauses[2].guard.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::GreaterEqual);
                        }
                        _ => panic!("Expected greater equal guard"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_case_with_string_result() {
    let input = r#"case String.length(input) {
        when length > 20 -> "long string"
        when length > 0 -> "short string"
        else -> "empty string"
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::CaseExpression(case_expr) => {
                    // Check case expression is a function call
                    match &case_expr.expression.kind {
                        ExpressionKind::FunctionCall(call) => match &call.path {
                            FunctionPath::Qualified { module, name } => {
                                assert_eq!(module.name, "String");
                                assert_eq!(name.name, "length");
                            }
                            _ => panic!("Expected qualified function call"),
                        },
                        _ => panic!("Expected function call expression"),
                    }

                    assert_eq!(case_expr.when_clauses.len(), 2);
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_case_expression_display_formatting() {
    let inputs_and_patterns = [
        (
            r#"case x { when x > 0 -> "positive" else -> "negative" }"#,
            vec![
                "case x",
                "when x > 0 -> \"positive\"",
                "else -> \"negative\"",
            ],
        ),
        (
            r#"case value { when positive?(value: value) -> { "pos" } else -> "other" }"#,
            vec!["case value", "when positive?", "else -> \"other\""],
        ),
    ];

    for (input, expected_patterns) in inputs_and_patterns.iter() {
        let program = OutrunParser::parse_program(input).unwrap();
        let formatted = format!("{}", program);

        for pattern in expected_patterns {
            assert!(
                formatted.contains(pattern),
                "Display format failed for: {}. Expected pattern '{}' not found in: {}",
                input,
                pattern,
                formatted
            );
        }
    }
}
