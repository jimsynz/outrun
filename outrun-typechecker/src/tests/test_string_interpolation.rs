//! String interpolation type checking tests
//!
//! Tests for validating that interpolated expressions implement the Display trait

use crate::checker::{ExpressionChecker, TypeContext, TypedExpressionKind};
use crate::error::TypeError;
use crate::types::{ConcreteType, TraitDefinition, TraitFunction, TraitImplementation};
use outrun_parser::{
    Expression, ExpressionKind, IntegerFormat, IntegerLiteral, StringFormat, StringLiteral,
    StringPart,
};

/// Helper function to create a minimal type context with Display trait
fn create_context_with_display_trait() -> TypeContext {
    let mut context = TypeContext::new();

    // Define Display trait
    let display_trait_id = context.interner.intern_trait("Display");
    let display_function_atom = context.interner.intern_atom("display");
    let string_type = context.interner.intern_type("Outrun.Core.String");
    let self_type = context.interner.intern_type("Self");

    let display_function = TraitFunction::new(
        display_function_atom,
        vec![(context.interner.intern_atom("self"), self_type)],
        string_type,
        false, // not a guard function
        outrun_parser::Span::new(0, 10),
    );

    let display_trait = TraitDefinition::new(
        display_trait_id,
        "Display".to_string(),
        vec![display_function],
        outrun_parser::Span::new(0, 20),
    );

    context.trait_registry.register_trait(display_trait);

    // Register basic types that implement Display
    let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
    let float_type = context.interner.intern_type("Outrun.Core.Float64");
    let boolean_type = context.interner.intern_type("Outrun.Core.Boolean");
    let atom_type = context.interner.intern_type("Outrun.Core.Atom");

    // Register concrete types
    context.register_concrete_type(string_type, ConcreteType::String);
    context.register_concrete_type(integer_type, ConcreteType::Integer64);
    context.register_concrete_type(float_type, ConcreteType::Float64);
    context.register_concrete_type(boolean_type, ConcreteType::Boolean);
    context.register_concrete_type(atom_type, ConcreteType::Atom);

    // Register Display implementations for basic types
    let display_impl_string = TraitImplementation {
        trait_id: display_trait_id,
        type_id: string_type,
        functions: std::collections::HashMap::new(),
        generic_params: Vec::new(),
        constraints: Vec::new(),
        span: outrun_parser::Span::new(0, 10),
    };

    let display_impl_integer = TraitImplementation {
        trait_id: display_trait_id,
        type_id: integer_type,
        functions: std::collections::HashMap::new(),
        generic_params: Vec::new(),
        constraints: Vec::new(),
        span: outrun_parser::Span::new(0, 10),
    };

    let display_impl_float = TraitImplementation {
        trait_id: display_trait_id,
        type_id: float_type,
        functions: std::collections::HashMap::new(),
        generic_params: Vec::new(),
        constraints: Vec::new(),
        span: outrun_parser::Span::new(0, 10),
    };

    let display_impl_boolean = TraitImplementation {
        trait_id: display_trait_id,
        type_id: boolean_type,
        functions: std::collections::HashMap::new(),
        generic_params: Vec::new(),
        constraints: Vec::new(),
        span: outrun_parser::Span::new(0, 10),
    };

    let display_impl_atom = TraitImplementation {
        trait_id: display_trait_id,
        type_id: atom_type,
        functions: std::collections::HashMap::new(),
        generic_params: Vec::new(),
        constraints: Vec::new(),
        span: outrun_parser::Span::new(0, 10),
    };

    context
        .trait_registry
        .register_implementation(display_impl_string);
    context
        .trait_registry
        .register_implementation(display_impl_integer);
    context
        .trait_registry
        .register_implementation(display_impl_float);
    context
        .trait_registry
        .register_implementation(display_impl_boolean);
    context
        .trait_registry
        .register_implementation(display_impl_atom);

    context
}

#[test]
fn test_string_interpolation_with_integer() {
    let mut context = create_context_with_display_trait();

    // Create "Hello #{42}!"
    let string_literal = StringLiteral {
        parts: vec![
            StringPart::Text {
                content: "Hello ".to_string(),
                raw_content: "Hello ".to_string(),
            },
            StringPart::Interpolation {
                expression: Box::new(Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 42,
                        format: IntegerFormat::Decimal,
                        span: outrun_parser::Span::new(8, 10),
                    }),
                    span: outrun_parser::Span::new(8, 10),
                }),
                span: outrun_parser::Span::new(6, 11),
            },
            StringPart::Text {
                content: "!".to_string(),
                raw_content: "!".to_string(),
            },
        ],
        format: StringFormat::Basic,
        span: outrun_parser::Span::new(0, 13),
    };

    // Create an expression with the string literal
    let expr = Expression {
        kind: ExpressionKind::String(string_literal),
        span: outrun_parser::Span::new(0, 13),
    };

    let result = ExpressionChecker::check_expression(&mut context, &expr);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    // Should be String type
    let string_type = context.interner.intern_type("Outrun.Core.String");
    assert_eq!(typed_expr.type_id, string_type);

    match typed_expr.kind {
        TypedExpressionKind::String(content) => {
            assert_eq!(content, "Hello #{...}!");
        }
        _ => assert!(
            false,
            "Expected string expression, got {:?}",
            typed_expr.kind
        ),
    }
}

#[test]
fn test_string_interpolation_with_multiple_types() {
    let mut context = create_context_with_display_trait();

    // Create "Value: #{42}, Flag: #{true}, Name: #{:atom}"
    let string_literal = StringLiteral {
        parts: vec![
            StringPart::Text {
                content: "Value: ".to_string(),
                raw_content: "Value: ".to_string(),
            },
            StringPart::Interpolation {
                expression: Box::new(Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 42,
                        format: IntegerFormat::Decimal,
                        span: outrun_parser::Span::new(9, 11),
                    }),
                    span: outrun_parser::Span::new(9, 11),
                }),
                span: outrun_parser::Span::new(7, 12),
            },
            StringPart::Text {
                content: ", Flag: ".to_string(),
                raw_content: ", Flag: ".to_string(),
            },
            StringPart::Interpolation {
                expression: Box::new(Expression {
                    kind: ExpressionKind::Boolean(outrun_parser::BooleanLiteral {
                        value: true,
                        span: outrun_parser::Span::new(22, 26),
                    }),
                    span: outrun_parser::Span::new(22, 26),
                }),
                span: outrun_parser::Span::new(20, 27),
            },
            StringPart::Text {
                content: ", Name: ".to_string(),
                raw_content: ", Name: ".to_string(),
            },
            StringPart::Interpolation {
                expression: Box::new(Expression {
                    kind: ExpressionKind::Atom(outrun_parser::AtomLiteral {
                        name: "atom".to_string(),
                        content: "atom".to_string(),
                        raw_content: "atom".to_string(),
                        format: outrun_parser::AtomFormat::Simple,
                        span: outrun_parser::Span::new(37, 42),
                    }),
                    span: outrun_parser::Span::new(37, 42),
                }),
                span: outrun_parser::Span::new(35, 43),
            },
        ],
        format: StringFormat::Basic,
        span: outrun_parser::Span::new(0, 43),
    };

    // Create an expression with the string literal
    let expr = Expression {
        kind: ExpressionKind::String(string_literal),
        span: outrun_parser::Span::new(0, 43),
    };

    let result = ExpressionChecker::check_expression(&mut context, &expr);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    // Should be String type
    let string_type = context.interner.intern_type("Outrun.Core.String");
    assert_eq!(typed_expr.type_id, string_type);
}

#[test]
fn test_string_interpolation_with_non_display_type() {
    let mut context = create_context_with_display_trait();

    // Define a custom type that doesn't implement Display
    let custom_type = context.interner.intern_type("CustomType");
    // Register as a struct type since it's a custom type
    context.register_concrete_type(
        custom_type,
        ConcreteType::Struct {
            name: custom_type,
            fields: Vec::new(),
        },
    );

    // Register a variable of this type
    context.push_scope(false);
    let variable = crate::checker::context::Variable {
        name: "custom_value".to_string(),
        type_id: custom_type,
        is_mutable: false,
        span: outrun_parser::Span::new(0, 12),
    };
    context.register_variable(variable).unwrap();

    // Create "Result: #{custom_value}"
    let string_literal = StringLiteral {
        parts: vec![
            StringPart::Text {
                content: "Result: ".to_string(),
                raw_content: "Result: ".to_string(),
            },
            StringPart::Interpolation {
                expression: Box::new(Expression {
                    kind: ExpressionKind::Identifier(outrun_parser::Identifier {
                        name: "custom_value".to_string(),
                        span: outrun_parser::Span::new(10, 22),
                    }),
                    span: outrun_parser::Span::new(10, 22),
                }),
                span: outrun_parser::Span::new(8, 23),
            },
        ],
        format: StringFormat::Basic,
        span: outrun_parser::Span::new(0, 23),
    };

    // Create an expression with the string literal
    let expr = Expression {
        kind: ExpressionKind::String(string_literal),
        span: outrun_parser::Span::new(0, 23),
    };

    let result = ExpressionChecker::check_expression(&mut context, &expr);

    assert!(result.is_err());

    if let Err(TypeError::StringInterpolationDisplayError { type_name, .. }) = result {
        assert_eq!(type_name, "CustomType");
    } else {
        assert!(
            false,
            "Expected StringInterpolationDisplayError, got {:?}",
            result
        );
    }
}

#[test]
fn test_string_interpolation_no_display_trait_defined() {
    let mut context = TypeContext::new();

    // Don't define Display trait - just register basic types
    let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
    context.register_concrete_type(integer_type, ConcreteType::Integer64);

    // Create "Value: #{42}"
    let string_literal = StringLiteral {
        parts: vec![
            StringPart::Text {
                content: "Value: ".to_string(),
                raw_content: "Value: ".to_string(),
            },
            StringPart::Interpolation {
                expression: Box::new(Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 42,
                        format: IntegerFormat::Decimal,
                        span: outrun_parser::Span::new(9, 11),
                    }),
                    span: outrun_parser::Span::new(9, 11),
                }),
                span: outrun_parser::Span::new(7, 12),
            },
        ],
        format: StringFormat::Basic,
        span: outrun_parser::Span::new(0, 12),
    };

    // Create an expression with the string literal
    let expr = Expression {
        kind: ExpressionKind::String(string_literal),
        span: outrun_parser::Span::new(0, 12),
    };

    let result = ExpressionChecker::check_expression(&mut context, &expr);

    assert!(result.is_err());

    if let Err(TypeError::StringInterpolationDisplayError { type_name, .. }) = result {
        assert_eq!(type_name, "Outrun.Core.Integer64");
    } else {
        assert!(
            false,
            "Expected StringInterpolationDisplayError, got {:?}",
            result
        );
    }
}

#[test]
fn test_plain_string_without_interpolation() {
    let mut context = create_context_with_display_trait();

    // Create "Hello World!" (no interpolation)
    let string_literal = StringLiteral {
        parts: vec![StringPart::Text {
            content: "Hello World!".to_string(),
            raw_content: "Hello World!".to_string(),
        }],
        format: StringFormat::Basic,
        span: outrun_parser::Span::new(0, 14),
    };

    // Create an expression with the string literal
    let expr = Expression {
        kind: ExpressionKind::String(string_literal),
        span: outrun_parser::Span::new(0, 14),
    };

    let result = ExpressionChecker::check_expression(&mut context, &expr);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    // Should be String type
    let string_type = context.interner.intern_type("Outrun.Core.String");
    assert_eq!(typed_expr.type_id, string_type);

    match typed_expr.kind {
        TypedExpressionKind::String(content) => {
            assert_eq!(content, "Hello World!");
        }
        _ => assert!(
            false,
            "Expected string expression, got {:?}",
            typed_expr.kind
        ),
    }
}

#[test]
fn test_empty_string_interpolation() {
    let mut context = create_context_with_display_trait();

    // Create "" (empty string)
    let string_literal = StringLiteral {
        parts: vec![],
        format: StringFormat::Basic,
        span: outrun_parser::Span::new(0, 2),
    };

    // Create an expression with the string literal
    let expr = Expression {
        kind: ExpressionKind::String(string_literal),
        span: outrun_parser::Span::new(0, 2),
    };

    let result = ExpressionChecker::check_expression(&mut context, &expr);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    // Should be String type
    let string_type = context.interner.intern_type("Outrun.Core.String");
    assert_eq!(typed_expr.type_id, string_type);

    match typed_expr.kind {
        TypedExpressionKind::String(content) => {
            assert_eq!(content, "");
        }
        _ => assert!(
            false,
            "Expected string expression, got {:?}",
            typed_expr.kind
        ),
    }
}

#[test]
fn test_string_interpolation_with_complex_expression() {
    let mut context = create_context_with_display_trait();

    // For this test, we just need the context with Display trait
    // The binary operation type checking will handle the arithmetic

    // Create "Result: #{40 + 2}"
    let addition_expr = Expression {
        kind: ExpressionKind::BinaryOp(outrun_parser::BinaryOperation {
            left: Box::new(Expression {
                kind: ExpressionKind::Integer(IntegerLiteral {
                    value: 40,
                    format: IntegerFormat::Decimal,
                    span: outrun_parser::Span::new(10, 12),
                }),
                span: outrun_parser::Span::new(10, 12),
            }),
            operator: outrun_parser::BinaryOperator::Add,
            right: Box::new(Expression {
                kind: ExpressionKind::Integer(IntegerLiteral {
                    value: 2,
                    format: IntegerFormat::Decimal,
                    span: outrun_parser::Span::new(15, 16),
                }),
                span: outrun_parser::Span::new(15, 16),
            }),
            span: outrun_parser::Span::new(10, 16),
        }),
        span: outrun_parser::Span::new(10, 16),
    };

    let string_literal = StringLiteral {
        parts: vec![
            StringPart::Text {
                content: "Result: ".to_string(),
                raw_content: "Result: ".to_string(),
            },
            StringPart::Interpolation {
                expression: Box::new(addition_expr),
                span: outrun_parser::Span::new(8, 17),
            },
        ],
        format: StringFormat::Basic,
        span: outrun_parser::Span::new(0, 17),
    };

    // Create an expression with the string literal
    let expr = Expression {
        kind: ExpressionKind::String(string_literal),
        span: outrun_parser::Span::new(0, 17),
    };

    let result = ExpressionChecker::check_expression(&mut context, &expr);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    // Should be String type
    let string_type = context.interner.intern_type("Outrun.Core.String");
    assert_eq!(typed_expr.type_id, string_type);
}
