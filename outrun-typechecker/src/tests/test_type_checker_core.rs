use crate::checker::TypeContext;
use crate::checker::{TypeChecker, TypedExpressionKind, TypedItemKind, TypedProgram};
use crate::dispatch::DispatchTable;
use crate::error::TypeError;
use outrun_parser::{Identifier, Pattern, Span, TypeAnnotation, TypeIdentifier};

#[test]
fn test_type_checker_creation() {
    let checker = TypeChecker::new();
    assert!(checker.context().interner.get_type("Integer").is_none());
}

#[test]
fn test_typed_program_structure() {
    let program = TypedProgram {
        items: Vec::new(),
        dispatch_table: DispatchTable::new(),
        type_context: TypeContext::new(),
    };

    assert_eq!(program.items.len(), 0);
}

#[test]
fn test_let_binding_type_inference() {
    let mut checker = TypeChecker::new();

    let let_binding = outrun_parser::LetBinding {
        pattern: Pattern::Identifier(Identifier {
            name: "x".to_string(),
            span: Span::new(0, 1),
        }),
        type_annotation: None,
        expression: outrun_parser::Expression {
            kind: outrun_parser::ExpressionKind::Integer(outrun_parser::IntegerLiteral {
                value: 42,
                format: outrun_parser::IntegerFormat::Decimal,
                span: Span::new(6, 8),
            }),
            span: Span::new(6, 8),
        },
        span: Span::new(0, 8),
    };

    let result = checker.check_let_binding(&let_binding);
    assert!(result.is_ok());

    let typed_item = result.unwrap();
    assert!(
        matches!(typed_item.kind, TypedItemKind::LetBinding(_)),
        "Expected let binding, got {:?}",
        typed_item.kind
    );

    if let TypedItemKind::LetBinding(let_binding) = typed_item.kind {
        let type_name = checker.context().get_type_name(let_binding.type_id);
        assert_eq!(type_name, Some("Outrun.Core.Integer64"));
    }

    let variable = checker.context().lookup_variable("x");
    assert!(variable.is_some());
    assert_eq!(variable.unwrap().name, "x");
}

#[test]
fn test_let_binding_type_annotation() {
    let mut checker = TypeChecker::new();

    let let_binding = outrun_parser::LetBinding {
        pattern: Pattern::Identifier(Identifier {
            name: "x".to_string(),
            span: Span::new(0, 1),
        }),
        type_annotation: Some(TypeAnnotation::Simple {
            path: vec![TypeIdentifier {
                name: "Outrun.Core.Integer64".to_string(),
                span: Span::new(3, 25),
            }],
            generic_args: None,
            span: Span::new(3, 25),
        }),
        expression: outrun_parser::Expression {
            kind: outrun_parser::ExpressionKind::Integer(outrun_parser::IntegerLiteral {
                value: 42,
                format: outrun_parser::IntegerFormat::Decimal,
                span: Span::new(28, 30),
            }),
            span: Span::new(28, 30),
        },
        span: Span::new(0, 30),
    };

    let result = checker.check_let_binding(&let_binding);
    assert!(result.is_ok());

    let typed_item = result.unwrap();
    assert!(
        matches!(typed_item.kind, TypedItemKind::LetBinding(_)),
        "Expected let binding, got {:?}",
        typed_item.kind
    );

    if let TypedItemKind::LetBinding(let_binding) = typed_item.kind {
        let type_name = checker.context().get_type_name(let_binding.type_id);
        assert_eq!(type_name, Some("Outrun.Core.Integer64"));
    }
}

#[test]
fn test_let_binding_type_mismatch() {
    let mut checker = TypeChecker::new();

    let let_binding = outrun_parser::LetBinding {
        pattern: Pattern::Identifier(Identifier {
            name: "x".to_string(),
            span: Span::new(0, 1),
        }),
        type_annotation: Some(TypeAnnotation::Simple {
            path: vec![TypeIdentifier {
                name: "Outrun.Core.String".to_string(),
                span: Span::new(3, 22),
            }],
            generic_args: None,
            span: Span::new(3, 22),
        }),
        expression: outrun_parser::Expression {
            kind: outrun_parser::ExpressionKind::Integer(outrun_parser::IntegerLiteral {
                value: 42,
                format: outrun_parser::IntegerFormat::Decimal,
                span: Span::new(25, 27),
            }),
            span: Span::new(25, 27),
        },
        span: Span::new(0, 27),
    };

    let result = checker.check_let_binding(&let_binding);
    assert!(result.is_err(), "Expected type mismatch error");

    if let Err(TypeError::TypeMismatch {
        expected, found, ..
    }) = result
    {
        assert_eq!(expected, "Outrun.Core.String");
        assert_eq!(found, "Outrun.Core.Integer64");
    } else {
        panic!("Expected TypeMismatch error, got {:?}", result);
    }
}

#[test]
fn test_function_typed_ast_generation() {
    use outrun_parser::parse_program;

    let source = r#"
        def add(x: Integer, y: Integer): Integer {
            x + y
        }
    "#;

    let program = parse_program(source).expect("Should parse function");
    let mut type_checker = TypeChecker::new();
    let typed_program = type_checker
        .check_program(&program)
        .expect("Should type check");

    assert_eq!(typed_program.items.len(), 1);

    assert!(
        matches!(
            &typed_program.items[0].kind,
            TypedItemKind::FunctionDefinition(_)
        ),
        "Expected function definition, got {:?}",
        &typed_program.items[0].kind
    );

    if let TypedItemKind::FunctionDefinition(func_def) = &typed_program.items[0].kind {
        assert_eq!(func_def.name, "add");
        assert_eq!(func_def.params.len(), 2);
        assert_eq!(func_def.params[0].0, "x");
        assert_eq!(func_def.params[1].0, "y");

        match &func_def.body.kind {
            TypedExpressionKind::Block(block) => {
                assert_eq!(block.statements.len(), 1);
            }
            TypedExpressionKind::Integer(0) => {
                panic!("Function body is still stubbed! Typed AST generation not working.");
            }
            other => {
                panic!("Unexpected function body type: {:?}", other);
            }
        }
    }
}

#[test]
fn test_missing_expression_types() {
    use outrun_parser::parse_program;

    let sources = vec![
        (r#"def test(): Boolean { !true }"#, "UnaryOp"),
        (
            r#"def test(user: User): String { user.name }"#,
            "FieldAccess",
        ),
        (
            r#"def test(): String { String.to_string(42) }"#,
            "QualifiedIdentifier",
        ),
        (
            r#"def test(): String { (42).to_string() }"#,
            "Parenthesized",
        ),
        (
            r#"def test(): String { @test_attribute() }"#,
            "MacroInjection",
        ),
    ];

    for (source, _expr_type) in sources {
        let parse_result = parse_program(source);
        if let Ok(program) = parse_result {
            let mut type_checker = TypeChecker::new();
            let _result = type_checker.check_program(&program);
        }
    }
}

#[test]
fn test_implemented_expression_types() {
    use outrun_parser::parse_program;

    let test_cases = vec![
        (r#"def test(): Boolean { !true }"#, "UnaryOp - LogicalNot"),
        (r#"def test(): Integer { -42 }"#, "UnaryOp - Minus"),
        (r#"def test(): Integer { +42 }"#, "UnaryOp - Plus"),
        (r#"def test(): Integer { ~42 }"#, "UnaryOp - BitwiseNot"),
        (r#"def test(): Integer { (42) }"#, "Parenthesized"),
    ];

    for (source, test_name) in test_cases {
        let program = parse_program(source).expect("Should parse");
        let mut type_checker = TypeChecker::new();
        let result = type_checker.check_program(&program);

        assert!(
            result.is_ok(),
            "{} should work but got errors: {:?}",
            test_name,
            result
        );

        if let Ok(typed_program) = result {
            if let TypedItemKind::FunctionDefinition(func_def) = &typed_program.items[0].kind {
                match &func_def.body.kind {
                    TypedExpressionKind::Block(block) => {
                        assert_eq!(
                            block.statements.len(),
                            1,
                            "{} should have one statement",
                            test_name
                        );
                    }
                    TypedExpressionKind::Integer(0) => {
                        panic!("{} body is still stubbed!", test_name);
                    }
                    _ => {}
                }
            }
        }
    }
}

#[test]
fn test_unsupported_expression_types_error() {
    use outrun_parser::parse_program;

    let test_cases = vec![
        (r#"def test(): Boolean { !true }"#, "UnaryOp", true),
        (r#"def test(): Integer { (42) }"#, "Parenthesized", true),
    ];

    for (source, expr_type, should_work) in test_cases {
        if let Ok(program) = parse_program(source) {
            let mut type_checker = TypeChecker::new();
            let result = type_checker.check_program(&program);

            if should_work {
                assert!(
                    result.is_ok(),
                    "{} should work but failed: {:?}",
                    expr_type,
                    result
                );
            } else {
                assert!(result.is_err(), "{} should fail but succeeded", expr_type);

                if let Err(errors) = result {
                    let has_unimplemented = errors.iter().any(|e| {
                        e.to_string().contains("UnimplementedFeature")
                            || e.to_string().contains("not yet implemented")
                    });
                    assert!(
                        has_unimplemented,
                        "{} should produce UnimplementedFeature error, got: {:?}",
                        expr_type, errors
                    );
                }
            }
        }
    }
}
