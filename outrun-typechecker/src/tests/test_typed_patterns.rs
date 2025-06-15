//! Typed pattern system tests
//!
//! Tests for converting untyped patterns to typed patterns with complete type information

use crate::checker::{TypeChecker, TypedPattern};
use outrun_parser::parse_program;

#[test]
fn test_typed_identifier_pattern() {
    let mut checker = TypeChecker::new();

    // Simple identifier pattern in let binding
    let program = parse_program(r#"let x = 42"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    // Extract the let binding
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                TypedPattern::Identifier { name, type_id } => {
                    assert_eq!(name, "x");

                    // Verify the type is Integer64
                    let type_name = checker.context().interner.type_name(*type_id).unwrap();
                    assert_eq!(type_name, "Outrun.Core.Integer64");
                }
                _ => panic!("Expected identifier pattern"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_typed_pattern_with_string_literal() {
    let mut checker = TypeChecker::new();

    // Test with string literal to ensure different types work
    let program = parse_program(r#"let message = "hello world""#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    // Extract the let binding
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                TypedPattern::Identifier { name, type_id } => {
                    assert_eq!(name, "message");

                    // Verify the type is String
                    let type_name = checker.context().interner.type_name(*type_id).unwrap();
                    assert_eq!(type_name, "Outrun.Core.String");
                }
                _ => panic!("Expected identifier pattern"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_typed_pattern_with_boolean_literal() {
    let mut checker = TypeChecker::new();

    // Test with boolean literal
    let program = parse_program(r#"let flag = true"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    // Extract the let binding
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                TypedPattern::Identifier { name, type_id } => {
                    assert_eq!(name, "flag");

                    // Verify the type is Boolean
                    let type_name = checker.context().interner.type_name(*type_id).unwrap();
                    assert_eq!(type_name, "Outrun.Core.Boolean");
                }
                _ => panic!("Expected identifier pattern"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_typed_pattern_with_float_literal() {
    let mut checker = TypeChecker::new();

    // Test with float literal
    let program = parse_program(r#"let pi = 3.14"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    // Extract the let binding
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                TypedPattern::Identifier { name, type_id } => {
                    assert_eq!(name, "pi");

                    // Verify the type is Float64
                    let type_name = checker.context().interner.type_name(*type_id).unwrap();
                    assert_eq!(type_name, "Outrun.Core.Float64");
                }
                _ => panic!("Expected identifier pattern"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_typed_pattern_preserves_span_information() {
    let mut checker = TypeChecker::new();

    // Test that span information is preserved in typed patterns
    let program = parse_program(r#"let variable_name = 100"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    // Extract the let binding
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            // Verify the span is preserved
            assert!(let_binding.span.start < let_binding.span.end);

            match &let_binding.pattern {
                TypedPattern::Identifier { name, type_id } => {
                    assert_eq!(name, "variable_name");

                    // Verify the type ID is valid
                    assert!(checker.context().interner.type_name(*type_id).is_some());
                }
                _ => panic!("Expected identifier pattern"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_typed_pattern_type_consistency() {
    let mut checker = TypeChecker::new();

    // Test that pattern type matches expression type
    let program = parse_program(r#"let result = 42 + 8"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    // Extract the let binding
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            // Pattern type should match expression type
            assert_eq!(
                let_binding.pattern.type_id(),
                let_binding.expression.type_id
            );
            assert_eq!(let_binding.type_id, let_binding.expression.type_id);

            match &let_binding.pattern {
                TypedPattern::Identifier { name, type_id } => {
                    assert_eq!(name, "result");

                    // Both pattern and expression should have Integer64 type
                    let type_name = checker.context().interner.type_name(*type_id).unwrap();
                    assert_eq!(type_name, "Outrun.Core.Integer64");
                }
                _ => panic!("Expected identifier pattern"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

// Helper trait to get type_id from TypedPattern
impl TypedPattern {
    fn type_id(&self) -> crate::types::TypeId {
        match self {
            TypedPattern::Identifier { type_id, .. } => *type_id,
            TypedPattern::Literal { type_id, .. } => *type_id,
            TypedPattern::Tuple { tuple_type, .. } => *tuple_type,
            TypedPattern::Struct { type_id, .. } => *type_id,
            TypedPattern::List { list_type, .. } => *list_type,
        }
    }
}
