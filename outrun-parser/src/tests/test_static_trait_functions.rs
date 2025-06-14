//! Tests for static trait function definitions using `defs` keyword
//!
//! These tests verify that trait static functions can be parsed correctly
//! and that the AST represents them properly.

use crate::ast::*;
use crate::parse_program;

#[test]
fn test_parse_simple_static_function() {
    let input = r#"
trait Result<T, E> {
    defs ok(value: T): Result<T, E> {
        Result.Ok { value: value }
    }
}
"#;

    let program = parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Result");
            assert_eq!(trait_def.functions.len(), 1);

            match &trait_def.functions[0] {
                TraitFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "ok");
                    assert_eq!(static_def.parameters.len(), 1);
                    assert_eq!(static_def.parameters[0].name.name, "value");
                    assert!(static_def.return_type.is_some());
                    // Body should be parsed
                    assert!(!static_def.body.statements.is_empty());
                    // Just verify that the body parsed successfully
                    // The exact structure depends on how field access is parsed
                }
                _ => panic!(
                    "Expected StaticDefinition, got {:?}",
                    trait_def.functions[0]
                ),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_parse_static_function_no_parameters() {
    let input = r#"
trait Option<T> {
    defs none(): Option<T> {
        Option.None {}
    }
}
"#;

    let program = parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Option");
            assert_eq!(trait_def.functions.len(), 1);

            match &trait_def.functions[0] {
                TraitFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "none");
                    assert_eq!(static_def.parameters.len(), 0);
                    assert!(static_def.return_type.is_some());
                }
                _ => panic!("Expected StaticDefinition"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_parse_static_function_multiple_parameters() {
    let input = r#"
trait Range<T> {
    defs from_to(start: T, end: T): Range<T> {
        Range.Inclusive { start: start, end: end }
    }
}
"#;

    let program = parse_program(input).unwrap();

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => match &trait_def.functions[0] {
            TraitFunction::StaticDefinition(static_def) => {
                assert_eq!(static_def.name.name, "from_to");
                assert_eq!(static_def.parameters.len(), 2);
                assert_eq!(static_def.parameters[0].name.name, "start");
                assert_eq!(static_def.parameters[1].name.name, "end");
            }
            _ => panic!("Expected StaticDefinition"),
        },
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_parse_mixed_static_and_instance_functions() {
    let input = r#"
trait Result<T, E> {
    defs ok(value: T): Result<T, E> {
        Result.Ok { value: value }
    }
    
    def is_ok?(self: Self): Boolean
    
    defs error(error: E): Result<T, E> {
        Result.Error { error: error }
    }
}
"#;

    let program = parse_program(input).unwrap();

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.functions.len(), 3);

            // First function should be static
            match &trait_def.functions[0] {
                TraitFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "ok");
                }
                _ => panic!("Expected first function to be static"),
            }

            // Second function should be signature
            match &trait_def.functions[1] {
                TraitFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "is_ok?");
                }
                _ => panic!("Expected second function to be signature"),
            }

            // Third function should be static
            match &trait_def.functions[2] {
                TraitFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "error");
                }
                _ => panic!("Expected third function to be static"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_static_function_display_formatting() {
    let input = r#"
trait Test {
    defs create(value: Integer): Test {
        Test { value: value }
    }
}
"#;

    let program = parse_program(input).unwrap();
    let output = format!("{}", program);

    // Should contain the defs keyword and preserve formatting
    assert!(output.contains("defs create"));
    assert!(output.contains("value: Integer"));
    assert!(output.contains("Test {"));
}

#[test]
fn test_static_function_without_return_type() {
    let input = r#"
trait Logger {
    defs create() {
        Logger.new()
    }
}
"#;

    let program = parse_program(input).unwrap();

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => match &trait_def.functions[0] {
            TraitFunction::StaticDefinition(static_def) => {
                assert_eq!(static_def.name.name, "create");
                assert!(static_def.return_type.is_none());
            }
            _ => panic!("Expected StaticDefinition"),
        },
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_complex_static_function_body() {
    let input = r#"
trait Calculator {
    defs compute(a: Integer, b: Integer): Integer {
        let result = a + b
        if result > 100 {
            100
        } else {
            result
        }
    }
}
"#;

    let program = parse_program(input).unwrap();

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            match &trait_def.functions[0] {
                TraitFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "compute");
                    assert_eq!(static_def.parameters.len(), 2);

                    // Should have parsed a complex block with let binding and if expression
                    assert_eq!(static_def.body.statements.len(), 2);
                }
                _ => panic!("Expected StaticDefinition"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_static_function_source_reconstruction() {
    let input = r#"trait Result<T, E> {
    defs ok(value: T): Result<T, E> {
        Result.Ok { value: value }
    }
}"#;

    let program = parse_program(input).unwrap();
    let reconstructed = format!("{}", program);

    // Should be able to reconstruct source with defs keyword
    assert!(reconstructed.contains("defs ok"));
    assert!(reconstructed.contains("value: T"));
    assert!(reconstructed.contains("Result<T, E>"));
}
