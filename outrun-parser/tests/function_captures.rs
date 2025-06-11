use outrun_parser::{parse_program, ItemKind, ExpressionKind};
use outrun_parser::ast::{Pattern, ListElement};

#[test]
fn test_function_capture_simple() {
    let input = r#"&upcase"#;
    
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::FunctionCapture(capture) = &expr.kind {
            // Should have no module path
            assert!(capture.module_path.is_none());
            
            // Function name should be "upcase"
            assert_eq!(capture.function_name.name, "upcase");
            
            // Should have no arity specification
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
            // Should have module path
            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path.len(), 1);
                assert_eq!(module_path[0].name, "String");
            }
            
            // Function name should be "upcase"
            assert_eq!(capture.function_name.name, "upcase");
            
            // Should have no arity specification
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
            // Should have nested module path
            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path.len(), 2);
                assert_eq!(module_path[0].name, "Http");
                assert_eq!(module_path[1].name, "Client");
            }
            
            // Function name should be "connect"
            assert_eq!(capture.function_name.name, "connect");
            
            // Should have no arity specification
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
            // Should have no module path
            assert!(capture.module_path.is_none());
            
            // Function name should be "map"
            assert_eq!(capture.function_name.name, "map");
            
            // Should have arity 2
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
            // Should have module path
            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path.len(), 1);
                assert_eq!(module_path[0].name, "List");
            }
            
            // Function name should be "filter"
            assert_eq!(capture.function_name.name, "filter");
            
            // Should have arity 2
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
            // Should have nested module path
            assert!(capture.module_path.is_some());
            if let Some(module_path) = &capture.module_path {
                assert_eq!(module_path.len(), 2);
                assert_eq!(module_path[0].name, "Database");
                assert_eq!(module_path[1].name, "Connection");
            }
            
            // Function name should be "query"
            assert_eq!(capture.function_name.name, "query");
            
            // Should have arity 3
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
    // Test simple pipeline with function reference (not function call with args)
    let input = r#"data |> String.upcase |> String.trim"#;
    
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    if let ItemKind::Expression(expr) = &result.items[0].kind {
        if let ExpressionKind::BinaryOp(_pipe_op) = &expr.kind {
            // This should be a pipe operation
            // The exact structure depends on precedence handling
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
            // Check that one of the arguments is a function capture
            let mut found_capture = false;
            for arg in &call.arguments {
                if let ExpressionKind::FunctionCapture(_) = &arg.expression.kind {
                    found_capture = true;
                    break;
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
            
            // All elements should be function captures
            for element in &list.elements {
                match element {
                    ListElement::Expression(expr) => {
                        if let ExpressionKind::FunctionCapture(_) = &expr.kind {
                            // Expected function capture
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
    
    // Should preserve the exact syntax
    assert!(reconstructed.contains("&String.upcase"));
}

#[test]
fn test_function_capture_with_arity_source_reconstruction() {
    let input = r#"&List.map/2"#;
    
    let result = parse_program(input).unwrap();
    let reconstructed = format!("{}", result);
    
    // Should preserve the arity specification
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
        
        // Expression should be function capture
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
            // Function name should be "verified?" (guard function)
            assert_eq!(capture.function_name.name, "verified?");
            
            // Should have module path
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