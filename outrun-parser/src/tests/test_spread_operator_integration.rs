use crate::{ast::*, parse_program};

#[test]
fn test_spread_operator_integration() {
    let input = "let new_list = [first, ..existing_list]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(id) => assert_eq!(id.name, "new_list"),
                _ => panic!("Expected identifier pattern"),
            }

            match &let_binding.expression.kind {
                ExpressionKind::List(list) => {
                    assert_eq!(list.elements.len(), 2);

                    match &list.elements[0] {
                        ListElement::Expression(expr) => match &expr.kind {
                            ExpressionKind::Identifier(id) => assert_eq!(id.name, "first"),
                            _ => panic!("Expected identifier"),
                        },
                        ListElement::Spread(_) => panic!("Expected expression, not spread"),
                    }

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

    let formatted = format!("{}", result);
    assert!(formatted.contains("[1, 2, ..middle, 3, 4]"));
}

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
