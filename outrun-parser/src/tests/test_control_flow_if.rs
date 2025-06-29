use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_if_with_else_basic() {
    let input = r#"if condition { "true" } else { "false" }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::IfExpression(if_expr) => {
                match &if_expr.condition.kind {
                    ExpressionKind::Identifier(id) => {
                        assert_eq!(id.name, "condition");
                    }
                    _ => panic!("Expected identifier condition"),
                }

                assert_eq!(if_expr.then_block.statements.len(), 1);
                match &if_expr.then_block.statements[0].kind {
                    StatementKind::Expression(expr) => match &expr.kind {
                        ExpressionKind::String(string_lit) => {
                            assert_eq!(string_lit.format, StringFormat::Basic);
                        }
                        _ => panic!("Expected string literal"),
                    },
                    _ => panic!("Expected expression statement"),
                }

                assert!(if_expr.else_block.is_some());
                let else_block = if_expr.else_block.as_ref().unwrap();
                assert_eq!(else_block.statements.len(), 1);
                match &else_block.statements[0].kind {
                    StatementKind::Expression(expr) => match &expr.kind {
                        ExpressionKind::String(string_lit) => {
                            assert_eq!(string_lit.format, StringFormat::Basic);
                        }
                        _ => panic!("Expected string literal"),
                    },
                    _ => panic!("Expected expression statement"),
                }
            }
            _ => panic!("Expected if expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_if_without_else() {
    let input = r#"if condition { "execute" }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::IfExpression(if_expr) => {
                match &if_expr.condition.kind {
                    ExpressionKind::Identifier(id) => {
                        assert_eq!(id.name, "condition");
                    }
                    _ => panic!("Expected identifier condition"),
                }

                assert_eq!(if_expr.then_block.statements.len(), 1);

                assert!(if_expr.else_block.is_none());
            }
            _ => panic!("Expected if expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_if_with_integer_comparison() {
    let input = r#"if x > 10 { 1 } else { 0 }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::IfExpression(if_expr) => {
                match &if_expr.condition.kind {
                    ExpressionKind::BinaryOp(op) => {
                        assert_eq!(op.operator, BinaryOperator::Greater);
                    }
                    _ => panic!("Expected binary operation condition"),
                }

                match &if_expr.then_block.statements[0].kind {
                    StatementKind::Expression(expr) => match &expr.kind {
                        ExpressionKind::Integer(int_lit) => {
                            assert_eq!(int_lit.value, 1);
                        }
                        _ => panic!("Expected integer literal"),
                    },
                    _ => panic!("Expected expression statement"),
                }

                let else_block = if_expr.else_block.as_ref().unwrap();
                match &else_block.statements[0].kind {
                    StatementKind::Expression(expr) => match &expr.kind {
                        ExpressionKind::Integer(int_lit) => {
                            assert_eq!(int_lit.value, 0);
                        }
                        _ => panic!("Expected integer literal"),
                    },
                    _ => panic!("Expected expression statement"),
                }
            }
            _ => panic!("Expected if expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_nested_if_expressions() {
    let input = r#"if x > 0 { if x > 10 { "large" } else { "small" } } else { "negative" }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::IfExpression(outer_if) => {
                match &outer_if.condition.kind {
                    ExpressionKind::BinaryOp(op) => {
                        assert_eq!(op.operator, BinaryOperator::Greater);
                    }
                    _ => panic!("Expected binary operation condition"),
                }

                match &outer_if.then_block.statements[0].kind {
                    StatementKind::Expression(expr) => match &expr.kind {
                        ExpressionKind::IfExpression(inner_if) => {
                            match &inner_if.condition.kind {
                                ExpressionKind::BinaryOp(op) => {
                                    assert_eq!(op.operator, BinaryOperator::Greater);
                                }
                                _ => panic!("Expected binary operation condition"),
                            }

                            assert!(inner_if.else_block.is_some());
                        }
                        _ => panic!("Expected nested if expression"),
                    },
                    _ => panic!("Expected expression statement"),
                }

                assert!(outer_if.else_block.is_some());
            }
            _ => panic!("Expected if expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_if_with_function_call_condition() {
    let input = r#"if Integer.positive?(x) { "positive" } else { "not positive" }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::IfExpression(if_expr) => {
                match &if_expr.condition.kind {
                    ExpressionKind::FunctionCall(call) => match &call.path {
                        FunctionPath::Qualified { module, name } => {
                            assert_eq!(module.name, "Integer");
                            assert_eq!(name.name, "positive?");
                        }
                        _ => panic!("Expected qualified function call"),
                    },
                    _ => panic!("Expected function call condition"),
                }

                assert_eq!(if_expr.then_block.statements.len(), 1);
                assert!(if_expr.else_block.is_some());
            }
            _ => panic!("Expected if expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_if_in_function_body() {
    let input = r#"def check(): String {
        if condition {
            "true"
        } else {
            "false"
        }
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name.name, "check");
            assert_eq!(func.body.statements.len(), 1);

            match &func.body.statements[0].kind {
                StatementKind::Expression(expr) => match &expr.kind {
                    ExpressionKind::IfExpression(if_expr) => {
                        assert!(if_expr.else_block.is_some());
                    }
                    _ => panic!("Expected if expression in function body"),
                },
                _ => panic!("Expected expression statement"),
            }
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_if_with_logical_and_condition() {
    let input = r#"if x > 0 && x < 100 { "valid" } else { "invalid" }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::IfExpression(if_expr) => {
                match &if_expr.condition.kind {
                    ExpressionKind::BinaryOp(op) => {
                        assert_eq!(op.operator, BinaryOperator::LogicalAnd);
                    }
                    _ => panic!("Expected logical AND condition"),
                }

                assert!(if_expr.else_block.is_some());
            }
            _ => panic!("Expected if expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_multiple_if_expressions() {
    let input = r#"if a { 1 }
if b { 2 }
if c { 3 } else { 4 }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    let if_expressions: Vec<_> = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::Expression(expr) => match &expr.kind {
                ExpressionKind::IfExpression(if_expr) => Some(if_expr),
                _ => None,
            },
            _ => None,
        })
        .collect();

    assert_eq!(if_expressions.len(), 3);

    assert!(if_expressions[0].else_block.is_none());
    assert!(if_expressions[1].else_block.is_none());
    assert!(if_expressions[2].else_block.is_some());
}

#[test]
fn test_if_expression_display_formatting() {
    let inputs_and_expected = [
        (
            r#"if x { 1 } else { 2 }"#,
            "if x {\n    1\n} else {\n    2\n}",
        ),
        (r#"if y { "hello" }"#, "if y {\n    \"hello\"\n}"),
        (
            r#"if z > 10 { true } else { false }"#,
            "if z > 10 {\n    true\n} else {\n    false\n}",
        ),
    ];

    for (input, expected_pattern) in inputs_and_expected.iter() {
        let program = OutrunParser::parse_program(input).unwrap();
        let formatted = format!("{program}");
        assert!(formatted.contains("if"));
        if expected_pattern.contains("else") {
            assert!(formatted.contains("else"));
        }
    }
}
