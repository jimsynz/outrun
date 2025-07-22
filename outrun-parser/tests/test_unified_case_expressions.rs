// Tests for unified case expressions and as Type expressions
// These test the new unified syntax that combines concrete and protocol cases

use outrun_parser::parse_program;

#[test]
fn test_simple_case_expression_with_pattern() {
    let input = r#"
        case value {
            42 -> "found the answer"
            x when x > 0 -> "positive"
            _ -> "something else"
        }
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse simple case expression: {:?}",
        result.err()
    );

    let program = result.unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.clauses.len(), 3);

                    // Check that all clauses are parsed correctly
                    assert!(case_expr.clauses[1].guard.is_some()); // Second clause has guard
                    assert!(case_expr.clauses[0].guard.is_none()); // First clause no guard
                    assert!(case_expr.clauses[2].guard.is_none()); // Third clause no guard
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_case_expression_with_struct_patterns() {
    let input = r#"
        case user {
            User { name, age } when age >= 18 -> "adult"
            User { name } -> "minor"
            Guest {} -> "visitor"
        }
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse case with struct patterns: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.clauses.len(), 3);

                    // Verify patterns are parsed as struct patterns
                    for clause in &case_expr.clauses {
                        match &clause.pattern {
                            outrun_parser::Pattern::Struct(_) => {} // Expected
                            _ => panic!("Expected struct pattern"),
                        }
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_case_expression_with_block_results() {
    let input = r#"
        case result {
            Ok { value } -> {
                let processed = process(value: value)
                processed
            }
            Err { error } -> {
                log_error(error: error)
                default_value()
            }
        }
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse case with blocks: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.clauses.len(), 2);

                    // Both results should be blocks
                    for clause in &case_expr.clauses {
                        match &clause.result {
                            outrun_parser::CaseResult::Block(_) => {} // Expected
                            _ => panic!("Expected block result"),
                        }
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_as_type_expression_basic() {
    let input = r#"
        value as String
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse as expression: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::BinaryOp(binary_op) => {
                    assert_eq!(binary_op.operator, outrun_parser::BinaryOperator::As);

                    // LHS should be an identifier
                    match &binary_op.left.kind {
                        outrun_parser::ExpressionKind::Identifier(_) => {}
                        _ => panic!("Expected identifier on LHS"),
                    }

                    // RHS should be a type identifier
                    match &binary_op.right.kind {
                        outrun_parser::ExpressionKind::TypeIdentifier(_) => {}
                        _ => panic!("Expected type identifier on RHS"),
                    }
                }
                _ => panic!("Expected binary operation"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_as_type_expression_with_complex_types() {
    let input = r#"
        parse_result as Option<Integer>
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse as with generic type: {:?}",
        result.err()
    );

    // Should parse successfully - type resolution happens in type checker
    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => match &expr.kind {
            outrun_parser::ExpressionKind::BinaryOp(binary_op) => {
                assert_eq!(binary_op.operator, outrun_parser::BinaryOperator::As);
            }
            _ => panic!("Expected binary operation"),
        },
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_as_type_precedence() {
    let input = r#"
        a + b as Integer
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse as with arithmetic: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::BinaryOp(binary_op) => {
                    // Should parse as: a + (b as Integer) due to precedence
                    assert_eq!(binary_op.operator, outrun_parser::BinaryOperator::Add);

                    // Right side should be the "as" expression
                    match &binary_op.right.kind {
                        outrun_parser::ExpressionKind::BinaryOp(as_op) => {
                            assert_eq!(as_op.operator, outrun_parser::BinaryOperator::As);
                        }
                        _ => panic!("Expected as expression on right side"),
                    }
                }
                _ => panic!("Expected binary operation"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_chained_as_expressions() {
    let input = r#"
        value as Integer as Numeric
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse chained as expressions: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::BinaryOp(binary_op) => {
                    // Should parse as: (value as Integer) as Numeric due to right associativity
                    assert_eq!(binary_op.operator, outrun_parser::BinaryOperator::As);

                    // Left side should be another "as" expression
                    match &binary_op.left.kind {
                        outrun_parser::ExpressionKind::BinaryOp(left_as_op) => {
                            assert_eq!(left_as_op.operator, outrun_parser::BinaryOperator::As);
                        }
                        _ => panic!("Expected as expression on left side"),
                    }
                }
                _ => panic!("Expected binary operation"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_let_binding_with_as_type() {
    let input = r#"
        let x: String = value as String
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse let with as: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::LetBinding(let_binding) => {
            // Expression should be an "as" expression
            match &let_binding.expression.kind {
                outrun_parser::ExpressionKind::BinaryOp(binary_op) => {
                    assert_eq!(binary_op.operator, outrun_parser::BinaryOperator::As);
                }
                _ => panic!("Expected as expression in let binding"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_sigil_with_as_type() {
    let input = r#"
        ~HTML"<div>content</div>" as SafeHtml
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse sigil with as: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::BinaryOp(binary_op) => {
                    assert_eq!(binary_op.operator, outrun_parser::BinaryOperator::As);

                    // LHS should be a sigil
                    match &binary_op.left.kind {
                        outrun_parser::ExpressionKind::Sigil(_) => {}
                        _ => panic!("Expected sigil on LHS"),
                    }
                }
                _ => panic!("Expected binary operation"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_complex_unified_case_example() {
    let input = r#"
        case value as Option<String> {
            Some { value } when String.length(value: value) > 0 -> value
            Some { value } -> "empty string"
            None {} -> "no value"
        }
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse complex unified case: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::CaseExpression(case_expr) => {
                    // The expression being matched should be an "as" expression
                    match &case_expr.expression.kind {
                        outrun_parser::ExpressionKind::BinaryOp(binary_op) => {
                            assert_eq!(binary_op.operator, outrun_parser::BinaryOperator::As);
                        }
                        _ => panic!("Expected as expression in case target"),
                    }

                    assert_eq!(case_expr.clauses.len(), 3);

                    // First clause should have a guard
                    assert!(case_expr.clauses[0].guard.is_some());
                    // Other clauses should not have guards
                    assert!(case_expr.clauses[1].guard.is_none());
                    assert!(case_expr.clauses[2].guard.is_none());
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}
