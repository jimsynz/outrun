use outrun_parser::ast::{ListElement, Pattern};
use outrun_parser::{parse_program, ExpressionKind, ItemKind};

#[test]
fn test_anonymous_function_single_param() {
    let input = r#"fn { x: Integer -> x + 1 }"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::AnonymousFunction(anon_fn) = &expr.kind {
            assert_eq!(anon_fn.clauses.len(), 1);

            let clause = &anon_fn.clauses[0];
            // Check parameters - should be single parameter
            match &clause.parameters {
                outrun_parser::ast::AnonymousParameters::Single { parameter, .. } => {
                    assert_eq!(parameter.name.name, "x");
                    // Type annotation should be Integer
                    match &parameter.type_annotation {
                        outrun_parser::ast::TypeAnnotation::Simple { path, .. } => {
                            assert_eq!(path.len(), 1);
                            assert_eq!(path[0].name, "Integer");
                        }
                        _ => panic!("Expected simple type annotation"),
                    }
                }
                _ => panic!("Expected single parameter"),
            }

            // Should have no guard
            assert!(clause.guard.is_none());

            // Body should be an expression (x + 1)
            match &clause.body {
                outrun_parser::ast::AnonymousBody::Expression(expr) => {
                    if let ExpressionKind::BinaryOp(_) = &expr.kind {
                        // Detailed validation would require deeper inspection
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
            // Check parameters - should be no parameters
            match &clause.parameters {
                outrun_parser::ast::AnonymousParameters::None { .. } => {
                    // Expected no parameters
                }
                _ => panic!("Expected no parameters"),
            }

            // Body should be integer literal 42
            match &clause.body {
                outrun_parser::ast::AnonymousBody::Expression(expr) => {
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
            // Check parameters - should be multiple parameters
            match &clause.parameters {
                outrun_parser::ast::AnonymousParameters::Multiple { parameters, .. } => {
                    assert_eq!(parameters.len(), 2);

                    assert_eq!(parameters[0].name.name, "x");
                    match &parameters[0].type_annotation {
                        outrun_parser::ast::TypeAnnotation::Simple { path, .. } => {
                            assert_eq!(path[0].name, "Integer");
                        }
                        _ => panic!("Expected simple type annotation"),
                    }

                    assert_eq!(parameters[1].name.name, "y");
                    match &parameters[1].type_annotation {
                        outrun_parser::ast::TypeAnnotation::Simple { path, .. } => {
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

            // Should have a guard
            assert!(clause.guard.is_some());
            if let Some(guard_expr) = &clause.guard {
                // Guard should be a function call to Integer.positive?(x)
                if let ExpressionKind::FunctionCall(_) = &guard_expr.kind {
                    // Expected function call
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

            // Body should be a block
            match &clause.body {
                outrun_parser::ast::AnonymousBody::Block(block) => {
                    // Block should have statements
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

            // First clause should have a guard
            assert!(anon_fn.clauses[0].guard.is_some());

            // Second clause should have a guard
            assert!(anon_fn.clauses[1].guard.is_some());

            // Third clause should have no guard (fallback)
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

            // Body should be a complex expression (comparison)
            match &clause.body {
                outrun_parser::ast::AnonymousBody::Expression(expr) => {
                    if let ExpressionKind::BinaryOp(_) = &expr.kind {
                        // Should be a comparison operation
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
    let reconstructed = format!("{}", result);

    // Should preserve the basic structure
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

        // Expression should be anonymous function
        if let ExpressionKind::AnonymousFunction(_) = &let_binding.expression.kind {
            // Expected anonymous function
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

            // Both elements should be anonymous functions
            for element in &list.elements {
                match element {
                    ListElement::Expression(expr) => {
                        if let ExpressionKind::AnonymousFunction(_) = &expr.kind {
                            // Expected anonymous function
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
