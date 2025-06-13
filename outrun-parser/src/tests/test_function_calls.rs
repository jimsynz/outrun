// Function call parsing tests with shorthand argument expansion

use crate::*;

// Helper function to extract named argument fields for tests
fn extract_named_arg(arg: &Argument) -> (&Identifier, &Expression, &ArgumentFormat) {
    match arg {
        Argument::Named {
            name,
            expression,
            format,
            ..
        } => (name, expression, format),
        _ => panic!("Expected named argument"),
    }
}

#[test]
fn test_function_call_no_arguments() {
    let input = "greet()";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                // Check function path
                match &call.path {
                    FunctionPath::Simple { name } => {
                        assert_eq!(name.name, "greet");
                    }
                    _ => panic!("Expected simple function path"),
                }

                // Check no arguments
                assert_eq!(call.arguments.len(), 0);
            }
            _ => panic!("Expected function call in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_function_call_explicit_arguments() {
    let input = "add(left: 5, right: 3)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                // Check function path
                match &call.path {
                    FunctionPath::Simple { name } => {
                        assert_eq!(name.name, "add");
                    }
                    _ => panic!("Expected simple function path"),
                }

                // Check arguments
                assert_eq!(call.arguments.len(), 2);

                // First argument: left: 5
                let (name, expression, format) = extract_named_arg(&call.arguments[0]);
                assert_eq!(name.name, "left");
                assert_eq!(*format, ArgumentFormat::Explicit);
                match &expression.kind {
                    ExpressionKind::Integer(int) => {
                        assert_eq!(int.value, 5);
                    }
                    _ => panic!("Expected integer expression in first argument"),
                }

                // Second argument: right: 3
                let (name, expression, format) = extract_named_arg(&call.arguments[1]);
                assert_eq!(name.name, "right");
                assert_eq!(*format, ArgumentFormat::Explicit);
                match &expression.kind {
                    ExpressionKind::Integer(int) => {
                        assert_eq!(int.value, 3);
                    }
                    _ => panic!("Expected integer expression in second argument"),
                }
            }
            _ => panic!("Expected function call in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_function_call_shorthand_arguments() {
    let input = "add(left, right)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                // Check function path
                match &call.path {
                    FunctionPath::Simple { name } => {
                        assert_eq!(name.name, "add");
                    }
                    _ => panic!("Expected simple function path"),
                }

                // Check arguments - should be expanded to named form
                assert_eq!(call.arguments.len(), 2);

                // First argument: left (expanded to left: left)
                let (name, expression, format) = extract_named_arg(&call.arguments[0]);
                assert_eq!(name.name, "left");
                assert_eq!(*format, ArgumentFormat::Shorthand);
                match &expression.kind {
                    ExpressionKind::Identifier(id) => {
                        assert_eq!(id.name, "left");
                    }
                    _ => panic!("Expected identifier expression in first shorthand argument"),
                }

                // Second argument: right (expanded to right: right)
                let (name, expression, format) = extract_named_arg(&call.arguments[1]);
                assert_eq!(name.name, "right");
                assert_eq!(*format, ArgumentFormat::Shorthand);
                match &expression.kind {
                    ExpressionKind::Identifier(id) => {
                        assert_eq!(id.name, "right");
                    }
                    _ => panic!("Expected identifier expression in second shorthand argument"),
                }
            }
            _ => panic!("Expected function call in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_function_call_mixed_arguments() {
    let input = "process(data, timeout: 30)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                // Check function path
                match &call.path {
                    FunctionPath::Simple { name } => {
                        assert_eq!(name.name, "process");
                    }
                    _ => panic!("Expected simple function path"),
                }

                // Check arguments
                assert_eq!(call.arguments.len(), 2);

                // First argument: data (shorthand)
                let (name, expression, format) = extract_named_arg(&call.arguments[0]);
                assert_eq!(name.name, "data");
                assert_eq!(*format, ArgumentFormat::Shorthand);
                match &expression.kind {
                    ExpressionKind::Identifier(id) => {
                        assert_eq!(id.name, "data");
                    }
                    _ => panic!("Expected identifier expression in shorthand argument"),
                }

                // Second argument: timeout: 30 (explicit)
                let (name, expression, format) = extract_named_arg(&call.arguments[1]);
                assert_eq!(name.name, "timeout");
                assert_eq!(*format, ArgumentFormat::Explicit);
                match &expression.kind {
                    ExpressionKind::Integer(int) => {
                        assert_eq!(int.value, 30);
                    }
                    _ => panic!("Expected integer expression in explicit argument"),
                }
            }
            _ => panic!("Expected function call in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_qualified_function_call() {
    let input = "Database.find(table: \"users\")";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                // Check qualified function path
                match &call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "Database");
                        assert_eq!(name.name, "find");
                    }
                    _ => panic!("Expected qualified function path"),
                }

                // Check argument
                assert_eq!(call.arguments.len(), 1);
                let (name, expression, format) = extract_named_arg(&call.arguments[0]);
                assert_eq!(name.name, "table");
                assert_eq!(*format, ArgumentFormat::Explicit);
                match &expression.kind {
                    ExpressionKind::String(string) => {
                        // Verify string content - should be "users"
                        assert_eq!(string.format, StringFormat::Basic);
                        assert_eq!(string.parts.len(), 1);
                        match &string.parts[0] {
                            StringPart::Text { content, .. } => {
                                assert_eq!(content, "users");
                            }
                            _ => panic!("Expected text part in string"),
                        }
                    }
                    _ => panic!("Expected string expression in argument"),
                }
            }
            _ => panic!("Expected function call in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_nested_function_calls() {
    let input = "format(message: capitalize(text))";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                // Check outer function
                match &call.path {
                    FunctionPath::Simple { name } => {
                        assert_eq!(name.name, "format");
                    }
                    _ => panic!("Expected simple function path"),
                }

                // Check argument - should contain nested function call
                assert_eq!(call.arguments.len(), 1);
                let (name, expression, format) = extract_named_arg(&call.arguments[0]);
                assert_eq!(name.name, "message");
                assert_eq!(*format, ArgumentFormat::Explicit);

                // Check nested function call
                match &expression.kind {
                    ExpressionKind::FunctionCall(nested_call) => {
                        match &nested_call.path {
                            FunctionPath::Simple { name } => {
                                assert_eq!(name.name, "capitalize");
                            }
                            _ => panic!("Expected simple function path for nested call"),
                        }

                        // Check nested argument
                        assert_eq!(nested_call.arguments.len(), 1);
                        let (name, expression, format) =
                            extract_named_arg(&nested_call.arguments[0]);
                        assert_eq!(name.name, "text");
                        assert_eq!(*format, ArgumentFormat::Shorthand);
                        match &expression.kind {
                            ExpressionKind::Identifier(id) => {
                                assert_eq!(id.name, "text");
                            }
                            _ => panic!("Expected identifier in nested argument"),
                        }
                    }
                    _ => panic!("Expected nested function call in argument"),
                }
            }
            _ => panic!("Expected function call in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_function_call_with_complex_expressions() {
    let input = "calculate(x: a + b, y: list)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                // Check function path
                match &call.path {
                    FunctionPath::Simple { name } => {
                        assert_eq!(name.name, "calculate");
                    }
                    _ => panic!("Expected simple function path"),
                }

                // Check arguments
                assert_eq!(call.arguments.len(), 2);

                // First argument: x: a + b
                let (name, expression, format) = extract_named_arg(&call.arguments[0]);
                assert_eq!(name.name, "x");
                assert_eq!(*format, ArgumentFormat::Explicit);
                match &expression.kind {
                    ExpressionKind::BinaryOp(binop) => {
                        assert_eq!(binop.operator, BinaryOperator::Add);
                        // Check left operand
                        match &binop.left.kind {
                            ExpressionKind::Identifier(id) => {
                                assert_eq!(id.name, "a");
                            }
                            _ => panic!("Expected identifier on left side of addition"),
                        }
                        // Check right operand
                        match &binop.right.kind {
                            ExpressionKind::Identifier(id) => {
                                assert_eq!(id.name, "b");
                            }
                            _ => panic!("Expected identifier on right side of addition"),
                        }
                    }
                    _ => panic!("Expected binary operation in first argument"),
                }

                // Second argument: y: list
                let (name, expression, format) = extract_named_arg(&call.arguments[1]);
                assert_eq!(name.name, "y");
                assert_eq!(*format, ArgumentFormat::Explicit);
                match &expression.kind {
                    ExpressionKind::Identifier(id) => {
                        assert_eq!(id.name, "list");
                    }
                    _ => panic!("Expected identifier in second argument"),
                }
            }
            _ => panic!("Expected function call in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_function_call_single_shorthand_argument() {
    let input = "process(data)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                // Check function path
                match &call.path {
                    FunctionPath::Simple { name } => {
                        assert_eq!(name.name, "process");
                    }
                    _ => panic!("Expected simple function path"),
                }

                // Check single shorthand argument
                assert_eq!(call.arguments.len(), 1);
                let (name, expression, format) = extract_named_arg(&call.arguments[0]);
                assert_eq!(name.name, "data");
                assert_eq!(*format, ArgumentFormat::Shorthand);
                match &expression.kind {
                    ExpressionKind::Identifier(id) => {
                        assert_eq!(id.name, "data");
                    }
                    _ => panic!("Expected identifier expression in shorthand argument"),
                }
            }
            _ => panic!("Expected function call in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_multiple_function_calls() {
    let input = "init() process(data) cleanup()";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 3);

    // Check each function call
    let expected_names = ["init", "process", "cleanup"];
    let expected_arg_counts = [0, 1, 0];

    for (i, item) in result.items.iter().enumerate() {
        match &item.kind {
            ItemKind::Expression(expr) => match &expr.kind {
                ExpressionKind::FunctionCall(call) => {
                    match &call.path {
                        FunctionPath::Simple { name } => {
                            assert_eq!(name.name, expected_names[i]);
                        }
                        _ => panic!("Expected simple function path at position {}", i),
                    }
                    assert_eq!(call.arguments.len(), expected_arg_counts[i]);
                }
                _ => panic!("Expected function call in expression at position {}", i),
            },
            _ => panic!("Expected expression at position {}", i),
        }
    }
}

#[test]
fn test_function_call_display_formatting() {
    // Test that Display impl correctly reconstructs original syntax
    let input = "add(left, right: 10)";
    let result = parse_program(input).unwrap();

    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let formatted = format!("{}", expr);
            // Should preserve shorthand vs explicit argument formatting
            assert!(formatted.contains("add(left, right: 10)"));
        }
        _ => panic!("Expected expression"),
    }
}
