use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_let_binding_with_explicit_type() {
    let input = "let name: String = \"James\"";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => assert_eq!(identifier.name, "name"),
                _ => panic!("Expected identifier pattern"),
            }

            assert!(let_binding.type_annotation.is_some());
            let type_ann = let_binding.type_annotation.as_ref().unwrap();
            match type_ann {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 1);
                    assert_eq!(path[0].name, "String");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &let_binding.expression.kind {
                ExpressionKind::String(string_lit) => {
                    assert_eq!(string_lit.format, StringFormat::Basic);
                }
                _ => panic!("Expected string literal"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_with_type_inference() {
    let input = "let timestamp = DateTime.now()";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => assert_eq!(identifier.name, "timestamp"),
                _ => panic!("Expected identifier pattern"),
            }

            assert!(let_binding.type_annotation.is_none());

            match &let_binding.expression.kind {
                ExpressionKind::FunctionCall(call) => match &call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "DateTime");
                        assert_eq!(name.name, "now");
                    }
                    _ => panic!("Expected qualified function call"),
                },
                _ => panic!("Expected function call"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_with_integer() {
    let input = "let age: Integer = 35";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => assert_eq!(identifier.name, "age"),
                _ => panic!("Expected identifier pattern"),
            }

            assert!(let_binding.type_annotation.is_some());
            let type_ann = let_binding.type_annotation.as_ref().unwrap();
            match type_ann {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path[0].name, "Integer");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &let_binding.expression.kind {
                ExpressionKind::Integer(int_lit) => {
                    assert_eq!(int_lit.value, 35);
                    assert_eq!(int_lit.format, IntegerFormat::Decimal);
                }
                _ => panic!("Expected integer literal"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_inferred_integer() {
    let input = "let count = 42";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => assert_eq!(identifier.name, "count"),
                _ => panic!("Expected identifier pattern"),
            }

            assert!(let_binding.type_annotation.is_none());

            match &let_binding.expression.kind {
                ExpressionKind::Integer(int_lit) => {
                    assert_eq!(int_lit.value, 42);
                }
                _ => panic!("Expected integer literal"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_with_module_type() {
    let input = "let response: Http.Response = Http.get(url: \"example.com\")";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => assert_eq!(identifier.name, "response"),
                _ => panic!("Expected identifier pattern"),
            }

            assert!(let_binding.type_annotation.is_some());
            let type_ann = let_binding.type_annotation.as_ref().unwrap();
            match type_ann {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 2);
                    assert_eq!(path[0].name, "Http");
                    assert_eq!(path[1].name, "Response");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &let_binding.expression.kind {
                ExpressionKind::FunctionCall(call) => match &call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "Http");
                        assert_eq!(name.name, "get");
                    }
                    _ => panic!("Expected qualified function call"),
                },
                _ => panic!("Expected function call"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_with_complex_expression() {
    let input = "let result = a + b * 2";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => assert_eq!(identifier.name, "result"),
                _ => panic!("Expected identifier pattern"),
            }

            assert!(let_binding.type_annotation.is_none());

            match &let_binding.expression.kind {
                ExpressionKind::BinaryOp(op) => {
                    assert_eq!(op.operator, BinaryOperator::Add);
                }
                _ => panic!("Expected binary operation"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_with_list_literal() {
    let input = "let numbers: List = [1, 2, 3]";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => assert_eq!(identifier.name, "numbers"),
                _ => panic!("Expected identifier pattern"),
            }

            assert!(let_binding.type_annotation.is_some());
            let type_ann = let_binding.type_annotation.as_ref().unwrap();
            match type_ann {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 1);
                    assert_eq!(path[0].name, "List");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &let_binding.expression.kind {
                ExpressionKind::List(list_lit) => {
                    assert_eq!(list_lit.elements.len(), 3);
                }
                _ => panic!("Expected list literal"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_inferred_from_list() {
    let input = "let items = [\"a\", \"b\", \"c\"]";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => assert_eq!(identifier.name, "items"),
                _ => panic!("Expected identifier pattern"),
            }

            assert!(let_binding.type_annotation.is_none());

            match &let_binding.expression.kind {
                ExpressionKind::List(list_lit) => {
                    assert_eq!(list_lit.elements.len(), 3);
                }
                _ => panic!("Expected list literal"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_in_function_body() {
    let input = r#"def calculate(): Integer {
        let x = 10
        let y: Integer = 20
        x + y
    }"#;
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name.name, "calculate");
            assert_eq!(func.body.statements.len(), 3);

            match &func.body.statements[0].kind {
                StatementKind::LetBinding(let_binding) => {
                    match &let_binding.pattern {
                        Pattern::Identifier(identifier) => assert_eq!(identifier.name, "x"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    assert!(let_binding.type_annotation.is_none()); // Inferred
                }
                _ => panic!("Expected let binding"),
            }

            match &func.body.statements[1].kind {
                StatementKind::LetBinding(let_binding) => {
                    match &let_binding.pattern {
                        Pattern::Identifier(identifier) => assert_eq!(identifier.name, "y"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    assert!(let_binding.type_annotation.is_some()); // Explicit
                }
                _ => panic!("Expected let binding"),
            }

            match &func.body.statements[2].kind {
                StatementKind::Expression(expr) => match &expr.kind {
                    ExpressionKind::BinaryOp(op) => {
                        assert_eq!(op.operator, BinaryOperator::Add);
                    }
                    _ => panic!("Expected binary operation"),
                },
                _ => panic!("Expected expression"),
            }
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_multiple_let_bindings() {
    let input = r#"let name: String = "James"
let age = 35
let active: Boolean = true"#;
    let program = OutrunParser::parse_program(input).unwrap();

    let let_bindings: Vec<_> = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::LetBinding(let_binding) => Some(let_binding),
            _ => None,
        })
        .collect();

    assert_eq!(let_bindings.len(), 3);

    match &let_bindings[0].pattern {
        Pattern::Identifier(identifier) => assert_eq!(identifier.name, "name"),
        _ => panic!("Expected identifier pattern"),
    }
    assert!(let_bindings[0].type_annotation.is_some());

    match &let_bindings[1].pattern {
        Pattern::Identifier(identifier) => assert_eq!(identifier.name, "age"),
        _ => panic!("Expected identifier pattern"),
    }
    assert!(let_bindings[1].type_annotation.is_none());

    match &let_bindings[2].pattern {
        Pattern::Identifier(identifier) => assert_eq!(identifier.name, "active"),
        _ => panic!("Expected identifier pattern"),
    }
    assert!(let_bindings[2].type_annotation.is_some());
}

#[test]
fn test_let_binding_display_formatting() {
    let inputs_and_expected = [
        (
            "let name: String = \"James\"",
            "let name: String = \"James\"",
        ),
        ("let age = 35", "let age = 35"),
        ("let count: Integer = 42", "let count: Integer = 42"),
    ];

    for (input, expected) in inputs_and_expected.iter() {
        let program = OutrunParser::parse_program(input).unwrap();
        let formatted = format!("{}", program);
        assert!(
            formatted.contains(expected),
            "Display format failed for: {}. Got: {}",
            input,
            formatted
        );
    }
}
