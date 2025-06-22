use crate::desugaring::DesugaringVisitor;
use outrun_parser::{
    Argument, ArgumentFormat, Expression, ExpressionKind, FunctionPath, Identifier, ListElement,
    SigilLiteral, StringFormat, StringLiteral, StringPart, StructLiteralField, TypeIdentifier,
};

#[test]
fn test_basic_sigil_desugaring() {
    let sigil = SigilLiteral {
        sigil_type: TypeIdentifier {
            name: "JSON".to_string(),
            span: outrun_parser::Span::new(0, 4),
        },
        string: StringLiteral {
            parts: vec![StringPart::Text {
                content: r#"{"name": "test"}"#.to_string(),
                raw_content: r#"{"name": "test"}"#.to_string(),
            }],
            format: StringFormat::Basic,
            span: outrun_parser::Span::new(5, 20),
        },
        span: outrun_parser::Span::new(0, 21),
    };

    let desugared =
        DesugaringVisitor::desugar_sigil_literal(sigil, outrun_parser::Span::new(0, 21));

    match desugared.kind {
        ExpressionKind::FunctionCall(call) => {
            match call.path {
                FunctionPath::Qualified { module, name } => {
                    assert_eq!(module.name, "JSON");
                    assert_eq!(name.name, "parse");
                }
                _ => panic!("Expected qualified function path"),
            }

            assert_eq!(call.arguments.len(), 1);
            match &call.arguments[0] {
                Argument::Named {
                    name,
                    expression,
                    format,
                    ..
                } => {
                    assert_eq!(name.name, "input");
                    assert_eq!(*format, ArgumentFormat::Explicit);

                    // The input should now be a list containing Sigil.Input structures
                    match &expression.kind {
                        ExpressionKind::List(list_lit) => {
                            assert_eq!(list_lit.elements.len(), 1);

                            // First element should be Sigil.Input.String { content: "..." }
                            match &list_lit.elements[0] {
                                ListElement::Expression(elem_expr) => {
                                    match &elem_expr.kind {
                                        ExpressionKind::Struct(struct_lit) => {
                                            // Check type path: Sigil.Input.String
                                            assert_eq!(struct_lit.type_path.len(), 3);
                                            assert_eq!(struct_lit.type_path[0].name, "Sigil");
                                            assert_eq!(struct_lit.type_path[1].name, "Input");
                                            assert_eq!(struct_lit.type_path[2].name, "String");

                                            // Check field: content: "..."
                                            assert_eq!(struct_lit.fields.len(), 1);
                                            match &struct_lit.fields[0] {
                                                StructLiteralField::Assignment {
                                                    name,
                                                    value,
                                                    ..
                                                } => {
                                                    assert_eq!(name.name, "content");
                                                    match &value.kind {
                                                        ExpressionKind::String(string_lit) => {
                                                            assert_eq!(string_lit.parts.len(), 1);
                                                            match &string_lit.parts[0] {
                                                                StringPart::Text { content, .. } => {
                                                                    assert_eq!(content, r#"{"name": "test"}"#);
                                                                }
                                                                _ => panic!("Expected text part"),
                                                            }
                                                        }
                                                        _ => panic!("Expected string expression for content field"),
                                                    }
                                                }
                                                _ => panic!("Expected assignment field"),
                                            }
                                        }
                                        _ => {
                                            panic!("Expected struct literal for Sigil.Input.String")
                                        }
                                    }
                                }
                                _ => panic!("Expected expression element"),
                            }
                        }
                        _ => panic!("Expected list expression for structured sigil input"),
                    }
                }
                _ => panic!("Expected named argument"),
            }
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_sigil_with_interpolation_desugaring() {
    let sigil = SigilLiteral {
        sigil_type: TypeIdentifier {
            name: "SQL".to_string(),
            span: outrun_parser::Span::new(0, 3),
        },
        string: StringLiteral {
            parts: vec![
                StringPart::Text {
                    content: "SELECT * FROM ".to_string(),
                    raw_content: "SELECT * FROM ".to_string(),
                },
                StringPart::Interpolation {
                    expression: Box::new(Expression {
                        kind: ExpressionKind::Identifier(Identifier {
                            name: "table_name".to_string(),
                            span: outrun_parser::Span::new(17, 27),
                        }),
                        span: outrun_parser::Span::new(17, 27),
                    }),
                    span: outrun_parser::Span::new(15, 29),
                },
            ],
            format: StringFormat::Basic,
            span: outrun_parser::Span::new(4, 29),
        },
        span: outrun_parser::Span::new(0, 30),
    };

    let desugared =
        DesugaringVisitor::desugar_sigil_literal(sigil, outrun_parser::Span::new(0, 30));

    match desugared.kind {
        ExpressionKind::FunctionCall(call) => {
            match call.path {
                FunctionPath::Qualified { module, name } => {
                    assert_eq!(module.name, "SQL");
                    assert_eq!(name.name, "parse");
                }
                _ => panic!("Expected qualified function path"),
            }

            assert_eq!(call.arguments.len(), 1);
            match &call.arguments[0] {
                Argument::Named {
                    name, expression, ..
                } => {
                    assert_eq!(name.name, "input");

                    // The input should now be a list containing both String and Expression input parts
                    match &expression.kind {
                        ExpressionKind::List(list_lit) => {
                            assert_eq!(list_lit.elements.len(), 2);

                            // First element should be Sigil.Input.String { content: "SELECT * FROM " }
                            match &list_lit.elements[0] {
                                ListElement::Expression(elem_expr) => {
                                    match &elem_expr.kind {
                                        ExpressionKind::Struct(struct_lit) => {
                                            // Check type path: Sigil.Input.String
                                            assert_eq!(struct_lit.type_path.len(), 3);
                                            assert_eq!(struct_lit.type_path[0].name, "Sigil");
                                            assert_eq!(struct_lit.type_path[1].name, "Input");
                                            assert_eq!(struct_lit.type_path[2].name, "String");

                                            // Check content field
                                            match &struct_lit.fields[0] {
                                                StructLiteralField::Assignment {
                                                    name,
                                                    value,
                                                    ..
                                                } => {
                                                    assert_eq!(name.name, "content");
                                                    match &value.kind {
                                                        ExpressionKind::String(string_lit) => {
                                                            match &string_lit.parts[0] {
                                                                StringPart::Text {
                                                                    content,
                                                                    ..
                                                                } => {
                                                                    assert_eq!(
                                                                        content,
                                                                        "SELECT * FROM "
                                                                    );
                                                                }
                                                                _ => panic!("Expected text part"),
                                                            }
                                                        }
                                                        _ => panic!("Expected string expression"),
                                                    }
                                                }
                                                _ => panic!("Expected assignment field"),
                                            }
                                        }
                                        _ => panic!("Expected struct literal for first part"),
                                    }
                                }
                                _ => panic!("Expected expression element"),
                            }

                            // Second element should be Sigil.Input.Expression { value: table_name }
                            match &list_lit.elements[1] {
                                ListElement::Expression(elem_expr) => {
                                    match &elem_expr.kind {
                                        ExpressionKind::Struct(struct_lit) => {
                                            // Check type path: Sigil.Input.Expression
                                            assert_eq!(struct_lit.type_path.len(), 3);
                                            assert_eq!(struct_lit.type_path[0].name, "Sigil");
                                            assert_eq!(struct_lit.type_path[1].name, "Input");
                                            assert_eq!(struct_lit.type_path[2].name, "Expression");

                                            // Check value field contains the identifier
                                            match &struct_lit.fields[0] {
                                                StructLiteralField::Assignment {
                                                    name,
                                                    value,
                                                    ..
                                                } => {
                                                    assert_eq!(name.name, "value");
                                                    match &value.kind {
                                                        ExpressionKind::Identifier(ident) => {
                                                            assert_eq!(ident.name, "table_name");
                                                        }
                                                        _ => panic!("Expected identifier expression for interpolated value"),
                                                    }
                                                }
                                                _ => panic!("Expected assignment field"),
                                            }
                                        }
                                        _ => panic!("Expected struct literal for second part"),
                                    }
                                }
                                _ => panic!("Expected expression element"),
                            }
                        }
                        _ => panic!("Expected list expression for structured sigil input"),
                    }
                }
                _ => panic!("Expected named argument"),
            }
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_sigil_with_multiple_interpolations_creates_structured_input() {
    // Test the key advantage: multiple interpolations become separate input parts
    // instead of being concatenated into a single string
    let sigil = SigilLiteral {
        sigil_type: TypeIdentifier {
            name: "SQL".to_string(),
            span: outrun_parser::Span::new(0, 3),
        },
        string: StringLiteral {
            parts: vec![
                StringPart::Text {
                    content: "SELECT * FROM ".to_string(),
                    raw_content: "SELECT * FROM ".to_string(),
                },
                StringPart::Interpolation {
                    expression: Box::new(Expression {
                        kind: ExpressionKind::Identifier(Identifier {
                            name: "table".to_string(),
                            span: outrun_parser::Span::new(17, 22),
                        }),
                        span: outrun_parser::Span::new(17, 22),
                    }),
                    span: outrun_parser::Span::new(15, 24),
                },
                StringPart::Text {
                    content: " WHERE id = ".to_string(),
                    raw_content: " WHERE id = ".to_string(),
                },
                StringPart::Interpolation {
                    expression: Box::new(Expression {
                        kind: ExpressionKind::Identifier(Identifier {
                            name: "user_id".to_string(),
                            span: outrun_parser::Span::new(38, 45),
                        }),
                        span: outrun_parser::Span::new(38, 45),
                    }),
                    span: outrun_parser::Span::new(36, 47),
                },
            ],
            format: StringFormat::Basic,
            span: outrun_parser::Span::new(4, 47),
        },
        span: outrun_parser::Span::new(0, 48),
    };

    let desugared =
        DesugaringVisitor::desugar_sigil_literal(sigil, outrun_parser::Span::new(0, 48));

    match desugared.kind {
        ExpressionKind::FunctionCall(call) => {
            // Should call SQL.parse
            match call.path {
                FunctionPath::Qualified { module, name } => {
                    assert_eq!(module.name, "SQL");
                    assert_eq!(name.name, "parse");
                }
                _ => panic!("Expected qualified function path"),
            }

            // Should have input argument with list of 4 parts: [String, Expression, String, Expression]
            assert_eq!(call.arguments.len(), 1);
            match &call.arguments[0] {
                Argument::Named {
                    name, expression, ..
                } => {
                    assert_eq!(name.name, "input");

                    match &expression.kind {
                        ExpressionKind::List(list_lit) => {
                            // Should have 4 elements: text, identifier, text, identifier
                            assert_eq!(list_lit.elements.len(), 4);

                            // This demonstrates that the new structured approach keeps
                            // string parts and expression parts separate, enabling proper
                            // escaping in sigil implementations (e.g., SQL injection prevention)
                        }
                        _ => panic!("Expected list expression"),
                    }
                }
                _ => panic!("Expected named argument"),
            }
        }
        _ => panic!("Expected function call"),
    }
}
#[test]
fn test_list_desugaring_with_nested_expressions() {
    use outrun_parser::{IntegerFormat, IntegerLiteral, ListElement, ListLiteral};

    // Test list with binary operation inside: [1 + 2, 3]
    let list_with_binary_op = Expression {
        kind: ExpressionKind::List(ListLiteral {
            elements: vec![
                ListElement::Expression(Box::new(Expression {
                    kind: ExpressionKind::BinaryOp(outrun_parser::BinaryOperation {
                        left: Box::new(Expression {
                            kind: ExpressionKind::Integer(IntegerLiteral {
                                value: 1,
                                format: IntegerFormat::Decimal,
                                raw_text: "1".to_string(),
                                span: outrun_parser::Span::new(1, 2),
                            }),
                            span: outrun_parser::Span::new(1, 2),
                        }),
                        operator: outrun_parser::BinaryOperator::Add,
                        right: Box::new(Expression {
                            kind: ExpressionKind::Integer(IntegerLiteral {
                                value: 2,
                                format: IntegerFormat::Decimal,
                                raw_text: "2".to_string(),
                                span: outrun_parser::Span::new(5, 6),
                            }),
                            span: outrun_parser::Span::new(5, 6),
                        }),
                        span: outrun_parser::Span::new(1, 6),
                    }),
                    span: outrun_parser::Span::new(1, 6),
                })),
                ListElement::Expression(Box::new(Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 3,
                        format: IntegerFormat::Decimal,
                        raw_text: "3".to_string(),
                        span: outrun_parser::Span::new(8, 9),
                    }),
                    span: outrun_parser::Span::new(8, 9),
                })),
            ],
            span: outrun_parser::Span::new(0, 10),
        }),
        span: outrun_parser::Span::new(0, 10),
    };

    let desugared = DesugaringVisitor::desugar_expression(list_with_binary_op);

    match desugared.kind {
        ExpressionKind::List(list_lit) => {
            assert_eq!(list_lit.elements.len(), 2);

            // First element should be a desugared binary operation (function call)
            match &list_lit.elements[0] {
                ListElement::Expression(elem) => {
                    match &elem.kind {
                        ExpressionKind::FunctionCall(_) => {
                            // Binary operations get desugared to function calls - this is expected
                        }
                        _ => panic!("Expected function call for desugared binary operation"),
                    }
                }
                _ => panic!("Expected expression element"),
            }

            // Second element should remain as integer literal
            match &list_lit.elements[1] {
                ListElement::Expression(elem) => match &elem.kind {
                    ExpressionKind::Integer(int_lit) => {
                        assert_eq!(int_lit.value, 3);
                    }
                    _ => panic!("Expected integer literal"),
                },
                _ => panic!("Expected expression element"),
            }
        }
        _ => panic!("Expected list expression"),
    }
}
