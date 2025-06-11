// Test trait definitions parsing
// Comprehensive tests for trait definition syntax

use outrun_parser::ast::*;
use outrun_parser::parser::OutrunParser;

#[test]
fn test_basic_trait_definition() {
    let input = r#"trait Drawable {
        def draw(self: Self): String
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Drawable");
            assert!(trait_def.generic_params.is_none());
            assert!(trait_def.constraints.is_none());
            assert_eq!(trait_def.functions.len(), 1);

            // Check function signature
            match &trait_def.functions[0] {
                TraitFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "draw");
                    assert_eq!(sig.parameters.len(), 1);
                    assert!(sig.return_type.is_some());
                }
                _ => panic!("Expected function signature"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_trait_with_generics() {
    let input = r#"trait Serializable<T> {
        def serialize(self: Self): T
        def deserialize(data: T): Self
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Serializable");

            // Check generics
            assert!(trait_def.generic_params.is_some());
            let generics = trait_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");

            // Check functions
            assert_eq!(trait_def.functions.len(), 2);
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_trait_with_constraints() {
    let input = r#"trait Comparable<T> when T: Orderable {
        def compare(self: Self, other: T): Integer
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Comparable");

            // Check constraints
            assert!(trait_def.constraints.is_some());
            match trait_def.constraints.as_ref().unwrap() {
                ConstraintExpression::Constraint {
                    type_param,
                    trait_bound,
                    ..
                } => {
                    assert_eq!(type_param.name, "T");
                    assert_eq!(trait_bound.len(), 1);
                    assert_eq!(trait_bound[0].name, "Orderable");
                }
                _ => panic!("Expected simple constraint"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_trait_with_default_implementation() {
    let input = r#"trait Logger {
        def log(message: String): Unit
        def log_info(message: String): Unit
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Logger");
            assert_eq!(trait_def.functions.len(), 2);

            // Check first function is signature
            match &trait_def.functions[0] {
                TraitFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "log");
                }
                _ => panic!("Expected function signature"),
            }

            // Check second function is also a signature
            match &trait_def.functions[1] {
                TraitFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "log_info");
                }
                _ => panic!("Expected function signature"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_empty_trait() {
    let input = r#"trait Marker {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Marker");
            assert!(trait_def.generic_params.is_none());
            assert!(trait_def.constraints.is_none());
            assert_eq!(trait_def.functions.len(), 0);
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_trait_display_formatting() {
    let inputs_and_patterns = [
        (
            r#"trait Drawable { def draw(self: Self): String }"#,
            vec!["trait Drawable", "def draw"],
        ),
        (r#"trait Serializable<T> {}"#, vec!["trait Serializable<T>"]),
        (
            r#"trait Comparable<T> when T: Orderable {}"#,
            vec!["trait Comparable<T>", "when T: Orderable"],
        ),
    ];

    for (input, expected_patterns) in inputs_and_patterns.iter() {
        let program = OutrunParser::parse_program(input).unwrap();
        let formatted = format!("{}", program);

        for pattern in expected_patterns {
            assert!(
                formatted.contains(pattern),
                "Display format failed for: {}. Expected pattern '{}' not found in: {}",
                input,
                pattern,
                formatted
            );
        }
    }
}
