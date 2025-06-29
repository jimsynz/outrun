use crate::ast::{ListElement, Pattern};
use crate::{parse_program, ExpressionKind, ItemKind};

#[test]
fn test_anonymous_function_single_param() {
    let input = r#"fn { x: Integer -> x + 1 }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::AnonymousFunction(anon_fn) = &expr.kind {
            assert_eq!(anon_fn.clauses.len(), 1);

            let clause = &anon_fn.clauses[0];
            match &clause.parameters {
                crate::ast::AnonymousParameters::Single { parameter, .. } => {
                    assert_eq!(parameter.name.name, "x");
                    match &parameter.type_annotation {
                        crate::ast::TypeAnnotation::Simple { path, .. } => {
                            assert_eq!(path.len(), 1);
                            assert_eq!(path[0].name, "Integer");
                        }
                        _ => panic!("Expected simple type annotation"),
                    }
                }
                _ => panic!("Expected single parameter"),
            }

            assert!(clause.guard.is_none());

            match &clause.body {
                crate::ast::AnonymousBody::Expression(expr) => {
                    if let ExpressionKind::BinaryOp(_) = &expr.kind {
                    } else {
                        panic!("Expected binary operation in anonymous function body");
                    }
                }
                _ => panic!("Expected expression body"),
            }
        } else {
            panic!("Expected anonymous function expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_anonymous_function_no_params() {
    let input = r#"fn { () -> 42 }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::AnonymousFunction(anon_fn) = &expr.kind {
            assert_eq!(anon_fn.clauses.len(), 1);

            let clause = &anon_fn.clauses[0];
            match &clause.parameters {
                crate::ast::AnonymousParameters::None { .. } => {}
                _ => panic!("Expected no parameters"),
            }

            match &clause.body {
                crate::ast::AnonymousBody::Expression(expr) => {
                    if let ExpressionKind::Integer(int_lit) = &expr.kind {
                        assert_eq!(int_lit.value, 42);
                    } else {
                        panic!("Expected integer literal");
                    }
                }
                _ => panic!("Expected expression body"),
            }
        } else {
            panic!("Expected anonymous function expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_anonymous_function_multiple_params() {
    let input = r#"fn { (x: Integer, y: String) -> x + y.length() }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::AnonymousFunction(anon_fn) = &expr.kind {
            assert_eq!(anon_fn.clauses.len(), 1);

            let clause = &anon_fn.clauses[0];
            match &clause.parameters {
                crate::ast::AnonymousParameters::Multiple { parameters, .. } => {
                    assert_eq!(parameters.len(), 2);

                    assert_eq!(parameters[0].name.name, "x");
                    match &parameters[0].type_annotation {
                        crate::ast::TypeAnnotation::Simple { path, .. } => {
                            assert_eq!(path[0].name, "Integer");
                        }
                        _ => panic!("Expected simple type annotation"),
                    }

                    assert_eq!(parameters[1].name.name, "y");
                    match &parameters[1].type_annotation {
                        crate::ast::TypeAnnotation::Simple { path, .. } => {
                            assert_eq!(path[0].name, "String");
                        }
                        _ => panic!("Expected simple type annotation"),
                    }
                }
                _ => panic!("Expected multiple parameters"),
            }
        } else {
            panic!("Expected anonymous function expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_anonymous_function_with_guard() {
    let input = r#"fn { x: Integer when Integer.positive?(x) -> x * 2 }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::AnonymousFunction(anon_fn) = &expr.kind {
            assert_eq!(anon_fn.clauses.len(), 1);

            let clause = &anon_fn.clauses[0];

            assert!(clause.guard.is_some());
            if let Some(guard_expr) = &clause.guard {
                if let ExpressionKind::FunctionCall(_) = &guard_expr.kind {
                } else {
                    panic!("Expected function call in guard");
                }
            }
        } else {
            panic!("Expected anonymous function expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_anonymous_function_block_body() {
    let input = r#"fn { x: String -> {
        let processed = String.upcase(x)
        processed
    } }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::AnonymousFunction(anon_fn) = &expr.kind {
            assert_eq!(anon_fn.clauses.len(), 1);

            let clause = &anon_fn.clauses[0];

            match &clause.body {
                crate::ast::AnonymousBody::Block(block) => {
                    assert!(!block.statements.is_empty());
                }
                _ => panic!("Expected block body"),
            }
        } else {
            panic!("Expected anonymous function expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_anonymous_function_multiple_clauses() {
    let input = r#"fn { 
        x: Integer when Integer.positive?(x) -> x * 2
        x: Integer when Integer.negative?(x) -> x * -1
        x: Integer -> 0
    }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::AnonymousFunction(anon_fn) = &expr.kind {
            assert_eq!(anon_fn.clauses.len(), 3);

            assert!(anon_fn.clauses[0].guard.is_some());

            assert!(anon_fn.clauses[1].guard.is_some());

            assert!(anon_fn.clauses[2].guard.is_none());
        } else {
            panic!("Expected anonymous function expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_anonymous_function_complex_expression() {
    let input = r#"fn { (a: Integer, b: Integer) -> a + b * 2 == 10 }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::AnonymousFunction(anon_fn) = &expr.kind {
            assert_eq!(anon_fn.clauses.len(), 1);

            let clause = &anon_fn.clauses[0];

            match &clause.body {
                crate::ast::AnonymousBody::Expression(expr) => {
                    if let ExpressionKind::BinaryOp(_) = &expr.kind {
                    } else {
                        panic!("Expected binary operation");
                    }
                }
                _ => panic!("Expected expression body"),
            }
        } else {
            panic!("Expected anonymous function expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_anonymous_function_source_reconstruction() {
    let input = r#"fn { x: Integer -> x + 1 }"#;

    let result = parse_program(input).unwrap();
    let reconstructed = format!("{result}");

    assert!(reconstructed.contains("fn { x: Integer -> x + 1 }"));
}

#[test]
fn test_anonymous_function_in_variable_binding() {
    let input = r#"let processor = fn { x: Integer -> x * 2 }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::LetBinding(let_binding) = &result.items[0].kind {
        match &let_binding.pattern {
            Pattern::Identifier(identifier) => assert_eq!(identifier.name, "processor"),
            _ => panic!("Expected identifier pattern"),
        }

        if let ExpressionKind::AnonymousFunction(_) = &let_binding.expression.kind {
        } else {
            panic!("Expected anonymous function in let binding");
        }
    } else {
        panic!("Expected let binding");
    }
}

#[test]
fn test_anonymous_function_nested_in_collection() {
    let input = r#"[fn { x: Integer -> x + 1 }, fn { y: String -> y.length() }]"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::List(list) = &expr.kind {
            assert_eq!(list.elements.len(), 2);

            for element in &list.elements {
                match element {
                    ListElement::Expression(expr) => {
                        if let ExpressionKind::AnonymousFunction(_) = &expr.kind {
                        } else {
                            panic!("Expected anonymous function in list");
                        }
                    }
                    ListElement::Spread(_) => panic!("Expected expression, not spread"),
                }
            }
        } else {
            panic!("Expected list expression");
        }
    } else {
        panic!("Expected expression item");
    }
}
