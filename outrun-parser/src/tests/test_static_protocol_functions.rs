//! Tests for static protocol function definitions using `defs` keyword
//!
//! These tests verify that protocol static functions can be parsed correctly
//! and that the AST represents them properly.

use crate::ast::*;
use crate::parse_program;

#[test]
fn test_parse_simple_static_function() {
    let input = r#"
protocol Result<T, E> {
    defs ok(value: T): Result<T, E> {
        Result.Ok { value: value }
    }
}
"#;

    let program = parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Result");
            assert_eq!(protocol_def.functions.len(), 1);

            match &protocol_def.functions[0] {
                ProtocolFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "ok");
                    assert_eq!(static_def.parameters.len(), 1);
                    assert_eq!(static_def.parameters[0].name.name, "value");
                    assert!(!static_def.body.statements.is_empty());
                }
                _ => panic!(
                    "Expected StaticDefinition, got {:?}",
                    protocol_def.functions[0]
                ),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_parse_static_function_no_parameters() {
    let input = r#"
protocol Option<T> {
    defs none(): Option<T> {
        Option.None {}
    }
}
"#;

    let program = parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Option");
            assert_eq!(protocol_def.functions.len(), 1);

            match &protocol_def.functions[0] {
                ProtocolFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "none");
                    assert_eq!(static_def.parameters.len(), 0);
                }
                _ => panic!("Expected StaticDefinition"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_parse_static_function_multiple_parameters() {
    let input = r#"
protocol Range<T> {
    defs from_to(start: T, end: T): Range<T> {
        Range.Inclusive { start: start, end: end }
    }
}
"#;

    let program = parse_program(input).unwrap();

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => match &protocol_def.functions[0] {
            ProtocolFunction::StaticDefinition(static_def) => {
                assert_eq!(static_def.name.name, "from_to");
                assert_eq!(static_def.parameters.len(), 2);
                assert_eq!(static_def.parameters[0].name.name, "start");
                assert_eq!(static_def.parameters[1].name.name, "end");
            }
            _ => panic!("Expected StaticDefinition"),
        },
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_parse_mixed_static_and_instance_functions() {
    let input = r#"
protocol Result<T, E> {
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
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.functions.len(), 3);

            match &protocol_def.functions[0] {
                ProtocolFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "ok");
                }
                _ => panic!("Expected first function to be static"),
            }

            match &protocol_def.functions[1] {
                ProtocolFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "is_ok?");
                }
                _ => panic!("Expected second function to be signature"),
            }

            match &protocol_def.functions[2] {
                ProtocolFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "error");
                }
                _ => panic!("Expected third function to be static"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_static_function_display_formatting() {
    let input = r#"
protocol Test {
    defs create(value: Integer): Test {
        Test { value: value }
    }
}
"#;

    let program = parse_program(input).unwrap();
    let output = format!("{program}");

    assert!(output.contains("defs create"));
    assert!(output.contains("value: Integer"));
    assert!(output.contains("Test {"));
}

#[test]
fn test_complex_static_function_body() {
    let input = r#"
protocol Calculator {
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
        ItemKind::ProtocolDefinition(protocol_def) => match &protocol_def.functions[0] {
            ProtocolFunction::StaticDefinition(static_def) => {
                assert_eq!(static_def.name.name, "compute");
                assert_eq!(static_def.parameters.len(), 2);

                assert_eq!(static_def.body.statements.len(), 2);
            }
            _ => panic!("Expected StaticDefinition"),
        },
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_static_function_source_reconstruction() {
    let input = r#"protocol Result<T, E> {
    defs ok(value: T): Result<T, E> {
        Result.Ok { value: value }
    }
}"#;

    let program = parse_program(input).unwrap();
    let reconstructed = format!("{program}");

    assert!(reconstructed.contains("defs ok"));
    assert!(reconstructed.contains("value: T"));
    assert!(reconstructed.contains("Result<T, E>"));
}

#[test]
fn test_static_function_with_attributes() {
    let input = r#"
protocol Option<T> {
    @Doc(content: "Create a Some variant")
    defs some(value: T): Option<T> {
        Option.Some { value: value }
    }
}
"#;

    let program = parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Option");
            assert_eq!(protocol_def.functions.len(), 1);

            match &protocol_def.functions[0] {
                ProtocolFunction::StaticDefinition(static_def) => {
                    assert_eq!(static_def.name.name, "some");
                    assert_eq!(static_def.attributes.len(), 1);

                    let attr = &static_def.attributes[0];
                    assert_eq!(attr.name.name, "Doc");
                    assert!(attr.args.is_some());
                    assert_eq!(attr.args.as_ref().unwrap().arguments.len(), 1);
                }
                _ => panic!("Expected StaticDefinition"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}
