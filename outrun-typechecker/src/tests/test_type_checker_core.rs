//! Core TypeChecker unit tests
//!
//! Tests for the main TypeChecker struct and basic functionality

use crate::checker::TypeContext;
use crate::checker::{TypeChecker, TypedItemKind, TypedProgram};
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

    // Create a simple let binding: let x = 42
    let let_binding = outrun_parser::LetBinding {
        pattern: Pattern::Identifier(Identifier {
            name: "x".to_string(),
            span: Span::new(0, 1),
        }),
        type_annotation: None, // Type inference
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
    match typed_item.kind {
        TypedItemKind::LetBinding(let_binding) => {
            // Should have inferred Integer64 type
            let type_name = checker.context().get_type_name(let_binding.type_id);
            assert_eq!(type_name, Some("Outrun.Core.Integer64"));
        }
        _ => panic!("Expected let binding"),
    }

    // Variable should be registered in scope
    let variable = checker.context().lookup_variable("x");
    assert!(variable.is_some());
    assert_eq!(variable.unwrap().name, "x");
}

#[test]
fn test_let_binding_type_annotation() {
    let mut checker = TypeChecker::new();

    // Create a let binding with explicit type: let x: Outrun.Core.Integer64 = 42
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
    match typed_item.kind {
        TypedItemKind::LetBinding(let_binding) => {
            // Should use the explicit type annotation
            let type_name = checker.context().get_type_name(let_binding.type_id);
            assert_eq!(type_name, Some("Outrun.Core.Integer64"));
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_let_binding_type_mismatch() {
    let mut checker = TypeChecker::new();

    // Create a let binding with mismatched types: let x: Outrun.Core.String = 42
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
    assert!(result.is_err());

    // Should be a type mismatch error
    match result.unwrap_err() {
        TypeError::TypeMismatch {
            expected, found, ..
        } => {
            assert_eq!(expected, "Outrun.Core.String");
            assert_eq!(found, "Outrun.Core.Integer64");
        }
        _ => panic!("Expected TypeMismatch error"),
    }
}
