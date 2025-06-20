use crate::ast::{Argument, ListElement, Pattern};
use crate::{parse_program, ExpressionKind, ItemKind};

#[test]
fn test_function_capture_simple() {
    let input = r#"&upcase"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCapture(capture) = &expr.kind {
            assert!(capture.module_path.is_none());

            assert_eq!(capture.function_name.name, "upcase");

            assert!(capture.arity.is_none());
        } else {
            panic!("Expected function capture expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_function_capture_with_module() {
    let input = r#"&String.upcase"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCapture(capture) = &expr.kind {
            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path.len(), 1);
                assert_eq!(module_path[0].name, "String");
            }

            assert_eq!(capture.function_name.name, "upcase");

            assert!(capture.arity.is_none());
        } else {
            panic!("Expected function capture expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_function_capture_with_nested_module() {
    let input = r#"&Http.Client.connect"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCapture(capture) = &expr.kind {
            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path.len(), 2);
                assert_eq!(module_path[0].name, "Http");
                assert_eq!(module_path[1].name, "Client");
            }

            assert_eq!(capture.function_name.name, "connect");

            assert!(capture.arity.is_none());
        } else {
            panic!("Expected function capture expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_function_capture_with_arity() {
    let input = r#"&map/2"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCapture(capture) = &expr.kind {
            assert!(capture.module_path.is_none());

            assert_eq!(capture.function_name.name, "map");

            assert_eq!(capture.arity, Some(2));
        } else {
            panic!("Expected function capture expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_function_capture_module_with_arity() {
    let input = r#"&List.filter/2"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCapture(capture) = &expr.kind {
            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path.len(), 1);
                assert_eq!(module_path[0].name, "List");
            }

            assert_eq!(capture.function_name.name, "filter");

            assert_eq!(capture.arity, Some(2));
        } else {
            panic!("Expected function capture expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_function_capture_nested_module_with_arity() {
    let input = r#"&Database.Connection.query/3"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCapture(capture) = &expr.kind {
            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path.len(), 2);
                assert_eq!(module_path[0].name, "Database");
                assert_eq!(module_path[1].name, "Connection");
            }

            assert_eq!(capture.function_name.name, "query");

            assert_eq!(capture.arity, Some(3));
        } else {
            panic!("Expected function capture expression");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_function_capture_in_pipeline() {
    let input = r#"data |> String.upcase |> String.trim"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::BinaryOp(_pipe_op) = &expr.kind {
        } else {
            panic!("Expected binary operation (pipe)");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_function_capture_in_function_call() {
    let input = r#"List.map(list: users, mapper: &User.name)"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCall(call) = &expr.kind {
            let mut found_capture = false;
            for arg in &call.arguments {
                match arg {
                    Argument::Named { expression, .. } => {
                        if let ExpressionKind::FunctionCapture(_) = &expression.kind {
                            found_capture = true;
                            break;
                        }
                    }
                    Argument::Spread { expression, .. } => {
                        if let ExpressionKind::FunctionCapture(_) = &expression.kind {
                            found_capture = true;
                            break;
                        }
                    }
                }
            }
            assert!(found_capture, "Should find function capture in arguments");
        } else {
            panic!("Expected function call");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_function_capture_in_collection() {
    let input = r#"[&String.upcase, &String.downcase, &String.reverse]"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::List(list) = &expr.kind {
            assert_eq!(list.elements.len(), 3);

            for element in &list.elements {
                match element {
                    ListElement::Expression(expr) => {
                        if let ExpressionKind::FunctionCapture(_) = &expr.kind {
                        } else {
                            panic!("Expected function capture in list");
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

#[test]
fn test_function_capture_source_reconstruction() {
    let input = r#"&String.upcase"#;

    let result = parse_program(input).unwrap();
    let reconstructed = format!("{}", result);

    assert!(reconstructed.contains("&String.upcase"));
}

#[test]
fn test_function_capture_with_arity_source_reconstruction() {
    let input = r#"&List.map/2"#;

    let result = parse_program(input).unwrap();
    let reconstructed = format!("{}", result);

    assert!(reconstructed.contains("&List.map/2"));
}

#[test]
fn test_function_capture_in_variable_binding() {
    let input = r#"let mapper = &String.upcase"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::LetBinding(let_binding) = &result.items[0].kind {
        match &let_binding.pattern {
            Pattern::Identifier(identifier) => assert_eq!(identifier.name, "mapper"),
            _ => panic!("Expected identifier pattern"),
        }

        if let ExpressionKind::FunctionCapture(capture) = &let_binding.expression.kind {
            assert_eq!(capture.function_name.name, "upcase");
        } else {
            panic!("Expected function capture in let binding");
        }
    } else {
        panic!("Expected let binding");
    }
}

#[test]
fn test_function_capture_guard_function() {
    let input = r#"&User.verified?"#;

    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCapture(capture) = &expr.kind {
            assert_eq!(capture.function_name.name, "verified?");

            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path[0].name, "User");
            }
        } else {
            panic!("Expected function capture expression");
        }
    } else {
        panic!("Expected expression item");
    }
}
