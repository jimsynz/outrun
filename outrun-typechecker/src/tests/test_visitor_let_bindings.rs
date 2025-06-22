use crate::visitor::{ExpressionCounter, IdentifierCollector, Visitor};
use outrun_parser::{
    BooleanLiteral, Expression, ExpressionKind, Identifier, IntegerFormat, IntegerLiteral, Item,
    ItemKind, LetBinding, Literal, Pattern, Program, Span,
};

fn create_test_span() -> Span {
    Span::new(0, 10)
}

fn create_test_identifier(name: &str) -> Identifier {
    Identifier {
        name: name.to_string(),
        span: create_test_span(),
    }
}

fn create_test_integer_expression(value: i64) -> Expression {
    Expression {
        kind: ExpressionKind::Integer(IntegerLiteral {
            value,
            format: IntegerFormat::Decimal,
            raw_text: value.to_string(),
            span: create_test_span(),
        }),
        span: create_test_span(),
    }
}

#[test]
fn test_let_binding_with_identifier_pattern() {
    let let_binding = LetBinding {
        pattern: Pattern::Identifier(create_test_identifier("x")),
        type_annotation: None,
        expression: create_test_integer_expression(42),
        span: create_test_span(),
    };

    let program = Program {
        items: vec![Item {
            kind: ItemKind::LetBinding(let_binding),
            span: create_test_span(),
        }],
        debug_info: Default::default(),
        span: create_test_span(),
    };

    // Test IdentifierCollector
    let mut collector = IdentifierCollector::default();
    <IdentifierCollector as Visitor<()>>::visit_program(&mut collector, &program).unwrap();

    assert_eq!(collector.identifiers.len(), 1);
    assert!(collector.identifiers.contains(&"x".to_string()));

    // Test ExpressionCounter
    let mut counter = ExpressionCounter::default();
    <ExpressionCounter as Visitor<()>>::visit_program(&mut counter, &program).unwrap();

    assert_eq!(counter.let_bindings, 1);
    assert_eq!(counter.literals, 1); // The integer literal
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_let_binding_with_literal_pattern() {
    let let_binding = LetBinding {
        pattern: Pattern::Literal(outrun_parser::LiteralPattern {
            literal: Literal::Boolean(BooleanLiteral {
                value: true,
                span: create_test_span(),
            }),
            span: create_test_span(),
        }),
        type_annotation: None,
        expression: create_test_integer_expression(42),
        span: create_test_span(),
    };

    let program = Program {
        items: vec![Item {
            kind: ItemKind::LetBinding(let_binding),
            span: create_test_span(),
        }],
        debug_info: Default::default(),
        span: create_test_span(),
    };

    // Test ExpressionCounter
    let mut counter = ExpressionCounter::default();
    <ExpressionCounter as Visitor<()>>::visit_program(&mut counter, &program).unwrap();

    assert_eq!(counter.let_bindings, 1);
    assert_eq!(counter.literals, 2); // Boolean literal in pattern + integer literal in expression
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_let_binding_with_tuple_pattern() {
    let let_binding = LetBinding {
        pattern: Pattern::Tuple(outrun_parser::TuplePattern {
            elements: vec![
                Pattern::Identifier(create_test_identifier("a")),
                Pattern::Identifier(create_test_identifier("b")),
                Pattern::Identifier(create_test_identifier("c")),
            ],
            span: create_test_span(),
        }),
        type_annotation: None,
        expression: create_test_integer_expression(42),
        span: create_test_span(),
    };

    let program = Program {
        items: vec![Item {
            kind: ItemKind::LetBinding(let_binding),
            span: create_test_span(),
        }],
        debug_info: Default::default(),
        span: create_test_span(),
    };

    // Test IdentifierCollector
    let mut collector = IdentifierCollector::default();
    <IdentifierCollector as Visitor<()>>::visit_program(&mut collector, &program).unwrap();

    assert_eq!(collector.identifiers.len(), 3);
    assert!(collector.identifiers.contains(&"a".to_string()));
    assert!(collector.identifiers.contains(&"b".to_string()));
    assert!(collector.identifiers.contains(&"c".to_string()));

    // Test ExpressionCounter
    let mut counter = ExpressionCounter::default();
    <ExpressionCounter as Visitor<()>>::visit_program(&mut counter, &program).unwrap();

    assert_eq!(counter.let_bindings, 1);
    assert_eq!(counter.literals, 1); // The integer literal in expression
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_let_binding_with_struct_pattern() {
    let let_binding = LetBinding {
        pattern: Pattern::Struct(outrun_parser::StructPattern {
            type_path: vec![outrun_parser::TypeIdentifier {
                name: "User".to_string(),
                span: create_test_span(),
            }],
            fields: vec![
                outrun_parser::StructFieldPattern {
                    name: create_test_identifier("name"),
                    pattern: Some(Pattern::Identifier(create_test_identifier("user_name"))),
                    span: create_test_span(),
                },
                outrun_parser::StructFieldPattern {
                    name: create_test_identifier("age"),
                    pattern: None, // Shorthand syntax
                    span: create_test_span(),
                },
            ],
            span: create_test_span(),
        }),
        type_annotation: None,
        expression: create_test_integer_expression(42),
        span: create_test_span(),
    };

    let program = Program {
        items: vec![Item {
            kind: ItemKind::LetBinding(let_binding),
            span: create_test_span(),
        }],
        debug_info: Default::default(),
        span: create_test_span(),
    };

    // Test IdentifierCollector
    let mut collector = IdentifierCollector::default();
    <IdentifierCollector as Visitor<()>>::visit_program(&mut collector, &program).unwrap();

    // Should collect: User (type identifier), name (field), user_name (pattern), age (field)
    assert_eq!(collector.identifiers.len(), 4);
    assert!(collector.identifiers.contains(&"User".to_string()));
    assert!(collector.identifiers.contains(&"name".to_string()));
    assert!(collector.identifiers.contains(&"user_name".to_string()));
    assert!(collector.identifiers.contains(&"age".to_string()));

    // Test ExpressionCounter
    let mut counter = ExpressionCounter::default();
    <ExpressionCounter as Visitor<()>>::visit_program(&mut counter, &program).unwrap();

    assert_eq!(counter.let_bindings, 1);
    assert_eq!(counter.literals, 1); // The integer literal in expression
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_let_binding_with_list_pattern() {
    let let_binding = LetBinding {
        pattern: Pattern::List(outrun_parser::ListPattern {
            elements: vec![
                Pattern::Identifier(create_test_identifier("first")),
                Pattern::Identifier(create_test_identifier("second")),
            ],
            rest: Some(create_test_identifier("rest")),
            span: create_test_span(),
        }),
        type_annotation: None,
        expression: create_test_integer_expression(42),
        span: create_test_span(),
    };

    let program = Program {
        items: vec![Item {
            kind: ItemKind::LetBinding(let_binding),
            span: create_test_span(),
        }],
        debug_info: Default::default(),
        span: create_test_span(),
    };

    // Test IdentifierCollector
    let mut collector = IdentifierCollector::default();
    <IdentifierCollector as Visitor<()>>::visit_program(&mut collector, &program).unwrap();

    // Should collect: first, second, rest
    assert_eq!(collector.identifiers.len(), 3);
    assert!(collector.identifiers.contains(&"first".to_string()));
    assert!(collector.identifiers.contains(&"second".to_string()));
    assert!(collector.identifiers.contains(&"rest".to_string()));

    // Test ExpressionCounter
    let mut counter = ExpressionCounter::default();
    <ExpressionCounter as Visitor<()>>::visit_program(&mut counter, &program).unwrap();

    assert_eq!(counter.let_bindings, 1);
    assert_eq!(counter.literals, 1); // The integer literal in expression
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_nested_let_bindings() {
    // Create a nested structure: let (a, (b, c)) = expression
    let nested_tuple = Pattern::Tuple(outrun_parser::TuplePattern {
        elements: vec![
            Pattern::Identifier(create_test_identifier("b")),
            Pattern::Identifier(create_test_identifier("c")),
        ],
        span: create_test_span(),
    });

    let let_binding = LetBinding {
        pattern: Pattern::Tuple(outrun_parser::TuplePattern {
            elements: vec![
                Pattern::Identifier(create_test_identifier("a")),
                nested_tuple,
            ],
            span: create_test_span(),
        }),
        type_annotation: None,
        expression: create_test_integer_expression(42),
        span: create_test_span(),
    };

    let program = Program {
        items: vec![Item {
            kind: ItemKind::LetBinding(let_binding),
            span: create_test_span(),
        }],
        debug_info: Default::default(),
        span: create_test_span(),
    };

    // Test IdentifierCollector
    let mut collector = IdentifierCollector::default();
    <IdentifierCollector as Visitor<()>>::visit_program(&mut collector, &program).unwrap();

    // Should collect: a, b, c
    assert_eq!(collector.identifiers.len(), 3);
    assert!(collector.identifiers.contains(&"a".to_string()));
    assert!(collector.identifiers.contains(&"b".to_string()));
    assert!(collector.identifiers.contains(&"c".to_string()));

    // Test ExpressionCounter
    let mut counter = ExpressionCounter::default();
    <ExpressionCounter as Visitor<()>>::visit_program(&mut counter, &program).unwrap();

    assert_eq!(counter.let_bindings, 1);
    assert_eq!(counter.literals, 1); // The integer literal in expression
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_multiple_let_bindings() {
    let let_binding1 = LetBinding {
        pattern: Pattern::Identifier(create_test_identifier("x")),
        type_annotation: None,
        expression: create_test_integer_expression(42),
        span: create_test_span(),
    };

    let let_binding2 = LetBinding {
        pattern: Pattern::Identifier(create_test_identifier("y")),
        type_annotation: None,
        expression: create_test_integer_expression(24),
        span: create_test_span(),
    };

    let program = Program {
        items: vec![
            Item {
                kind: ItemKind::LetBinding(let_binding1),
                span: create_test_span(),
            },
            Item {
                kind: ItemKind::LetBinding(let_binding2),
                span: create_test_span(),
            },
        ],
        debug_info: Default::default(),
        span: create_test_span(),
    };

    // Test IdentifierCollector
    let mut collector = IdentifierCollector::default();
    <IdentifierCollector as Visitor<()>>::visit_program(&mut collector, &program).unwrap();

    assert_eq!(collector.identifiers.len(), 2);
    assert!(collector.identifiers.contains(&"x".to_string()));
    assert!(collector.identifiers.contains(&"y".to_string()));

    // Test ExpressionCounter
    let mut counter = ExpressionCounter::default();
    <ExpressionCounter as Visitor<()>>::visit_program(&mut counter, &program).unwrap();

    assert_eq!(counter.let_bindings, 2);
    assert_eq!(counter.literals, 2); // Two integer literals
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}
