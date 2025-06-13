// Integration test for spread operator in list construction and function arguments
use crate::{ast::*, parse_program};

#[test]
fn test_spread_operator_integration() {
    // Test the basic example from PEST_PLAN.md
    let input = "let new_list = [first, ..existing_list]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    // Should parse as a let binding with a list expression containing spread
    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            // Check the identifier pattern
            match &let_binding.pattern {
                Pattern::Identifier(id) => assert_eq!(id.name, "new_list"),
                _ => panic!("Expected identifier pattern"),
            }

            // Check the list expression
            match &let_binding.expression.kind {
                ExpressionKind::List(list) => {
                    assert_eq!(list.elements.len(), 2);

                    // First element should be 'first' identifier
                    match &list.elements[0] {
                        ListElement::Expression(expr) => match &expr.kind {
                            ExpressionKind::Identifier(id) => assert_eq!(id.name, "first"),
                            _ => panic!("Expected identifier"),
                        },
                        ListElement::Spread(_) => panic!("Expected expression, not spread"),
                    }

                    // Second element should be spread 'existing_list'
                    match &list.elements[1] {
                        ListElement::Spread(id) => assert_eq!(id.name, "existing_list"),
                        ListElement::Expression(_) => panic!("Expected spread, not expression"),
                    }
                }
                _ => panic!("Expected list expression"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_spread_operator_display_integration() {
    let input = "let result = [1, 2, ..middle, 3, 4]";
    let result = parse_program(input).unwrap();

    // Test that display format is preserved
    let formatted = format!("{}", result);
    assert!(formatted.contains("[1, 2, ..middle, 3, 4]"));
}

// Function argument spread tests

#[test]
fn test_strict_spread_argument() {
    let input = "process_user(..login_result)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                assert_eq!(call.arguments.len(), 1);

                match &call.arguments[0] {
                    Argument::Spread {
                        expression, kind, ..
                    } => {
                        assert_eq!(*kind, SpreadKind::Strict);
                        match &expression.kind {
                            ExpressionKind::Identifier(id) => assert_eq!(id.name, "login_result"),
                            _ => panic!("Expected identifier in spread argument"),
                        }
                    }
                    _ => panic!("Expected spread argument"),
                }
            }
            _ => panic!("Expected function call"),
        },
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_lenient_spread_argument() {
    let input = "process_user(..?user_data)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                assert_eq!(call.arguments.len(), 1);

                match &call.arguments[0] {
                    Argument::Spread {
                        expression, kind, ..
                    } => {
                        assert_eq!(*kind, SpreadKind::Lenient);
                        match &expression.kind {
                            ExpressionKind::Identifier(id) => assert_eq!(id.name, "user_data"),
                            _ => panic!("Expected identifier in spread argument"),
                        }
                    }
                    _ => panic!("Expected spread argument"),
                }
            }
            _ => panic!("Expected function call"),
        },
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_mixed_spread_and_explicit_arguments() {
    let input = "process_user(..login_result, timestamp: custom_time)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                assert_eq!(call.arguments.len(), 2);

                // First argument should be spread
                match &call.arguments[0] {
                    Argument::Spread {
                        expression, kind, ..
                    } => {
                        assert_eq!(*kind, SpreadKind::Strict);
                        match &expression.kind {
                            ExpressionKind::Identifier(id) => assert_eq!(id.name, "login_result"),
                            _ => panic!("Expected identifier in spread argument"),
                        }
                    }
                    _ => panic!("Expected spread argument"),
                }

                // Second argument should be explicit named
                match &call.arguments[1] {
                    Argument::Named {
                        name,
                        expression,
                        format,
                        ..
                    } => {
                        assert_eq!(name.name, "timestamp");
                        assert_eq!(*format, ArgumentFormat::Explicit);
                        match &expression.kind {
                            ExpressionKind::Identifier(id) => assert_eq!(id.name, "custom_time"),
                            _ => panic!("Expected identifier in named argument"),
                        }
                    }
                    _ => panic!("Expected named argument"),
                }
            }
            _ => panic!("Expected function call"),
        },
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_multiple_spread_arguments() {
    let input = "create_account(..signup_data, ..preferences, ..metadata)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                assert_eq!(call.arguments.len(), 3);

                let expected_names = ["signup_data", "preferences", "metadata"];

                for (i, arg) in call.arguments.iter().enumerate() {
                    match arg {
                        Argument::Spread {
                            expression, kind, ..
                        } => {
                            assert_eq!(*kind, SpreadKind::Strict);
                            match &expression.kind {
                                ExpressionKind::Identifier(id) => {
                                    assert_eq!(id.name, expected_names[i]);
                                }
                                _ => panic!("Expected identifier in spread argument"),
                            }
                        }
                        _ => panic!("Expected spread argument"),
                    }
                }
            }
            _ => panic!("Expected function call"),
        },
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_spread_with_complex_expression() {
    let input = "process_user(..User.authenticate(credentials))";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::FunctionCall(call) => {
                assert_eq!(call.arguments.len(), 1);

                match &call.arguments[0] {
                    Argument::Spread {
                        expression, kind, ..
                    } => {
                        assert_eq!(*kind, SpreadKind::Strict);
                        // The expression should be a function call to User.authenticate
                        match &expression.kind {
                            ExpressionKind::FunctionCall(inner_call) => match &inner_call.path {
                                FunctionPath::Qualified { module, name } => {
                                    assert_eq!(module.name, "User");
                                    assert_eq!(name.name, "authenticate");
                                }
                                _ => panic!("Expected qualified function path"),
                            },
                            _ => panic!("Expected function call in spread argument"),
                        }
                    }
                    _ => panic!("Expected spread argument"),
                }
            }
            _ => panic!("Expected function call"),
        },
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_spread_argument_display() {
    let input = "func(..data, ..?optional, explicit: value)";
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);
    assert!(formatted.contains("..data"));
    assert!(formatted.contains("..?optional"));
    assert!(formatted.contains("explicit: value"));
}
