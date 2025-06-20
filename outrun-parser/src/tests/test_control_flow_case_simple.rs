//! Simple tests for case expressions without else clauses
//! Tests the new exhaustiveness-checked case expressions

use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_concrete_case_with_multiple_when_clauses() {
    let input = r#"case x { 
        when x > 0 -> "positive"
        when x < 0 -> "negative" 
        when x == 0 -> "zero"
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::CaseExpression(case_expr) => match case_expr {
                CaseExpression::Concrete(concrete) => {
                    assert_eq!(concrete.when_clauses.len(), 3);

                    match &concrete.expression.kind {
                        ExpressionKind::Identifier(id) => assert_eq!(id.name, "x"),
                        _ => panic!("Expected identifier"),
                    }
                }
                CaseExpression::Trait(_) => panic!("Expected concrete case"),
            },
            _ => panic!("Expected case expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_trait_case_expression() {
    let input = r#"case value as Display {
        String {} -> "string value"
        Integer {} -> "integer value"
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::CaseExpression(case_expr) => match case_expr {
                CaseExpression::Trait(trait_case) => {
                    assert_eq!(trait_case.type_clauses.len(), 2);

                    assert_eq!(trait_case.trait_name.name, "Display");

                    match &trait_case.expression.kind {
                        ExpressionKind::Identifier(id) => assert_eq!(id.name, "value"),
                        _ => panic!("Expected identifier"),
                    }
                }
                CaseExpression::Concrete(_) => panic!("Expected trait case"),
            },
            _ => panic!("Expected case expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_single_when_clause() {
    let input = r#"case flag { when true -> "enabled" }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::CaseExpression(case_expr) => match case_expr {
                CaseExpression::Concrete(concrete) => {
                    assert_eq!(concrete.when_clauses.len(), 1);

                    let when_clause = &concrete.when_clauses[0];
                    match &when_clause.result {
                        CaseResult::Expression(expr) => match &expr.kind {
                            ExpressionKind::String(string_lit) => {
                                assert_eq!(string_lit.format, StringFormat::Basic);
                            }
                            _ => panic!("Expected string literal"),
                        },
                        _ => panic!("Expected expression result"),
                    }
                }
                CaseExpression::Trait(_) => panic!("Expected concrete case"),
            },
            _ => panic!("Expected case expression"),
        },
        _ => panic!("Expected expression"),
    }
}
