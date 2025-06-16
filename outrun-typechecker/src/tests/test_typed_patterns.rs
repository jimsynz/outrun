//! Typed pattern system tests
//!
//! Tests for converting untyped patterns to typed patterns with complete type information

use crate::checker::{TypeChecker, TypedPattern};
use outrun_parser::parse_program;

#[test]
fn test_typed_identifier_pattern() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"let x = 42"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            assert!(
                matches!(&let_binding.pattern, TypedPattern::Identifier { .. }),
                "Expected identifier pattern, got {:?}",
                let_binding.pattern
            );

            if let TypedPattern::Identifier { name, type_id } = &let_binding.pattern {
                assert_eq!(name, "x");
                let type_name = checker.context().interner.type_name(*type_id).unwrap();
                assert_eq!(type_name, "Outrun.Core.Integer64");
            }
        }
        _ => assert!(
            false,
            "Expected let binding, got {:?}",
            typed_program.items[0].kind
        ),
    }
}

#[test]
fn test_typed_pattern_with_string_literal() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"let message = "hello world""#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            assert!(
                matches!(&let_binding.pattern, TypedPattern::Identifier { .. }),
                "Expected identifier pattern, got {:?}",
                let_binding.pattern
            );

            if let TypedPattern::Identifier { name, type_id } = &let_binding.pattern {
                assert_eq!(name, "message");
                let type_name = checker.context().interner.type_name(*type_id).unwrap();
                assert_eq!(type_name, "Outrun.Core.String");
            }
        }
        _ => assert!(
            false,
            "Expected let binding, got {:?}",
            typed_program.items[0].kind
        ),
    }
}

#[test]
fn test_typed_pattern_with_boolean_literal() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"let flag = true"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            assert!(
                matches!(&let_binding.pattern, TypedPattern::Identifier { .. }),
                "Expected identifier pattern, got {:?}",
                let_binding.pattern
            );

            if let TypedPattern::Identifier { name, type_id } = &let_binding.pattern {
                assert_eq!(name, "flag");
                let type_name = checker.context().interner.type_name(*type_id).unwrap();
                assert_eq!(type_name, "Outrun.Core.Boolean");
            }
        }
        _ => assert!(
            false,
            "Expected let binding, got {:?}",
            typed_program.items[0].kind
        ),
    }
}

#[test]
fn test_typed_pattern_with_float_literal() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"let pi = 3.14"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            assert!(
                matches!(&let_binding.pattern, TypedPattern::Identifier { .. }),
                "Expected identifier pattern, got {:?}",
                let_binding.pattern
            );

            if let TypedPattern::Identifier { name, type_id } = &let_binding.pattern {
                assert_eq!(name, "pi");
                let type_name = checker.context().interner.type_name(*type_id).unwrap();
                assert_eq!(type_name, "Outrun.Core.Float64");
            }
        }
        _ => assert!(
            false,
            "Expected let binding, got {:?}",
            typed_program.items[0].kind
        ),
    }
}

#[test]
fn test_typed_pattern_preserves_span_information() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"let variable_name = 100"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            assert!(let_binding.span.start < let_binding.span.end);

            assert!(
                matches!(&let_binding.pattern, TypedPattern::Identifier { .. }),
                "Expected identifier pattern, got {:?}",
                let_binding.pattern
            );

            if let TypedPattern::Identifier { name, type_id } = &let_binding.pattern {
                assert_eq!(name, "variable_name");
                assert!(checker.context().interner.type_name(*type_id).is_some());
            }
        }
        _ => assert!(
            false,
            "Expected let binding, got {:?}",
            typed_program.items[0].kind
        ),
    }
}

#[test]
fn test_typed_pattern_type_consistency() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"let result = 42 + 8"#).unwrap();
    let typed_program = checker.convert_program(&program).unwrap();

    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::LetBinding(let_binding) => {
            assert_eq!(
                let_binding.pattern.type_id(),
                let_binding.expression.type_id
            );
            assert_eq!(let_binding.type_id, let_binding.expression.type_id);

            assert!(
                matches!(&let_binding.pattern, TypedPattern::Identifier { .. }),
                "Expected identifier pattern, got {:?}",
                let_binding.pattern
            );

            if let TypedPattern::Identifier { name, type_id } = &let_binding.pattern {
                assert_eq!(name, "result");
                let type_name = checker.context().interner.type_name(*type_id).unwrap();
                assert_eq!(type_name, "Outrun.Core.Integer64");
            }
        }
        _ => assert!(
            false,
            "Expected let binding, got {:?}",
            typed_program.items[0].kind
        ),
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
