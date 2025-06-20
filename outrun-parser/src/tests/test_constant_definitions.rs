use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_const_definition_with_integer() {
    let input = "const MAX_USERS: Integer = 1000";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "MAX_USERS");

            match &const_def.type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 1);
                    assert_eq!(path[0].name, "Integer");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &const_def.expression.kind {
                ExpressionKind::Integer(int_lit) => {
                    assert_eq!(int_lit.value, 1000);
                    assert_eq!(int_lit.format, IntegerFormat::Decimal);
                }
                _ => panic!("Expected integer literal"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_string() {
    let input = "const DEFAULT_MESSAGE: String = \"Hello, World!\"";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "DEFAULT_MESSAGE");

            match &const_def.type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path[0].name, "String");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &const_def.expression.kind {
                ExpressionKind::String(string_lit) => {
                    assert_eq!(string_lit.format, StringFormat::Basic);
                }
                _ => panic!("Expected string literal"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_boolean() {
    let input = "const DEBUG_MODE: Boolean = true";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "DEBUG_MODE");

            match &const_def.type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path[0].name, "Boolean");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &const_def.expression.kind {
                ExpressionKind::Boolean(bool_lit) => {
                    assert!(bool_lit.value);
                }
                _ => panic!("Expected boolean literal"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_float() {
    let input = "const PI: Float = 3.14159";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "PI");

            match &const_def.type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path[0].name, "Float");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &const_def.expression.kind {
                ExpressionKind::Float(float_lit) => {
                    assert_eq!(float_lit.format, FloatFormat::Standard);
                    assert!((float_lit.value - std::f64::consts::PI).abs() < 0.00001);
                }
                _ => panic!("Expected float literal"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_module_type() {
    let input = "const DEFAULT_TIMEOUT: Duration = Duration.seconds(value: 30)";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "DEFAULT_TIMEOUT");

            match &const_def.type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 1);
                    assert_eq!(path[0].name, "Duration");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &const_def.expression.kind {
                ExpressionKind::FunctionCall(call) => match &call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "Duration");
                        assert_eq!(name.name, "seconds");
                    }
                    _ => panic!("Expected qualified function call"),
                },
                _ => panic!("Expected function call"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_hex_integer() {
    let input = "const COLOR_RED: Integer = 0xFF0000";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "COLOR_RED");

            match &const_def.expression.kind {
                ExpressionKind::Integer(int_lit) => {
                    assert_eq!(int_lit.value, 0xFF0000);
                    assert_eq!(int_lit.format, IntegerFormat::Hexadecimal);
                }
                _ => panic!("Expected integer literal"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_scientific_notation() {
    let input = "const PLANCK_CONSTANT: Float = 6.626e-34";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "PLANCK_CONSTANT");

            match &const_def.expression.kind {
                ExpressionKind::Float(float_lit) => {
                    assert_eq!(
                        float_lit.format,
                        FloatFormat::Scientific {
                            exponent_case: ExponentCase::Lowercase
                        }
                    );
                    assert!((float_lit.value - 6.626e-34).abs() < f64::EPSILON);
                }
                _ => panic!("Expected float literal"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_complex_module_type() {
    let input = "const HTTP_CLIENT: Http.Client = Http.new()";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "HTTP_CLIENT");

            match &const_def.type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 2);
                    assert_eq!(path[0].name, "Http");
                    assert_eq!(path[1].name, "Client");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &const_def.expression.kind {
                ExpressionKind::FunctionCall(call) => match &call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "Http");
                        assert_eq!(name.name, "new");
                    }
                    _ => panic!("Expected qualified function call"),
                },
                _ => panic!("Expected function call"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_arithmetic_expression() {
    let input = "const BUFFER_SIZE: Integer = 1024 * 8";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "BUFFER_SIZE");

            match &const_def.expression.kind {
                ExpressionKind::BinaryOp(op) => {
                    assert_eq!(op.operator, BinaryOperator::Multiply);

                    match &op.left.kind {
                        ExpressionKind::Integer(int_lit) => {
                            assert_eq!(int_lit.value, 1024);
                        }
                        _ => panic!("Expected integer literal on left"),
                    }

                    match &op.right.kind {
                        ExpressionKind::Integer(int_lit) => {
                            assert_eq!(int_lit.value, 8);
                        }
                        _ => panic!("Expected integer literal on right"),
                    }
                }
                _ => panic!("Expected binary operation"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_const_definition_with_atom() {
    let input = "const DEFAULT_STATUS: Atom = :active";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "DEFAULT_STATUS");

            match &const_def.type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path[0].name, "Atom");
                }
                _ => panic!("Expected simple type annotation"),
            }

            match &const_def.expression.kind {
                ExpressionKind::Atom(atom_lit) => {
                    assert_eq!(atom_lit.name, "active");
                    assert_eq!(atom_lit.format, AtomFormat::Simple);
                }
                _ => panic!("Expected atom literal"),
            }
        }
        _ => panic!("Expected const definition"),
    }
}

#[test]
fn test_multiple_const_definitions() {
    let input = r#"const MAX_CONNECTIONS: Integer = 100
const DEFAULT_PORT: Integer = 8080
const ENABLED: Boolean = true"#;
    let program = OutrunParser::parse_program(input).unwrap();

    let const_defs: Vec<_> = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::ConstDefinition(const_def) => Some(const_def),
            _ => None,
        })
        .collect();

    assert_eq!(const_defs.len(), 3);

    assert_eq!(const_defs[0].name.name, "MAX_CONNECTIONS");
    match &const_defs[0].expression.kind {
        ExpressionKind::Integer(int_lit) => {
            assert_eq!(int_lit.value, 100);
        }
        _ => panic!("Expected integer literal"),
    }

    assert_eq!(const_defs[1].name.name, "DEFAULT_PORT");
    match &const_defs[1].expression.kind {
        ExpressionKind::Integer(int_lit) => {
            assert_eq!(int_lit.value, 8080);
        }
        _ => panic!("Expected integer literal"),
    }

    assert_eq!(const_defs[2].name.name, "ENABLED");
    match &const_defs[2].expression.kind {
        ExpressionKind::Boolean(bool_lit) => {
            assert!(bool_lit.value);
        }
        _ => panic!("Expected boolean literal"),
    }
}

#[test]
fn test_const_definition_display_formatting() {
    let inputs_and_expected = [
        (
            "const MAX_USERS: Integer = 1000",
            "const MAX_USERS: Integer = 1000",
        ),
        ("const PI: Float = 3.14", "const PI: Float = 3.14"),
        (
            "const DEBUG: Boolean = false",
            "const DEBUG: Boolean = false",
        ),
        (
            "const MESSAGE: String = \"Hello\"",
            "const MESSAGE: String = \"Hello\"",
        ),
    ];

    for (input, expected) in inputs_and_expected.iter() {
        let program = OutrunParser::parse_program(input).unwrap();
        let formatted = format!("{}", program);
        assert!(
            formatted.contains(expected),
            "Display format failed for: {}. Got: {}",
            input,
            formatted
        );
    }
}

#[test]
fn test_const_vs_let_binding_difference() {
    let const_input = "const VALUE: Integer = 42";
    let let_input = "let value: Integer = 42";

    let const_program = OutrunParser::parse_program(const_input).unwrap();
    let let_program = OutrunParser::parse_program(let_input).unwrap();

    match &const_program.items[0].kind {
        ItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name.name, "VALUE");
        }
        _ => panic!("Expected const definition"),
    }

    match &let_program.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::Identifier(identifier) => assert_eq!(identifier.name, "value"),
            _ => panic!("Expected identifier pattern"),
        },
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_const_definition_in_program_context() {
    let input = r#"
# Application constants
const VERSION: String = "1.0.0"
const MAX_RETRIES: Integer = 3

# Main function
def main(): Integer {
    let config = load_config()
    process_data(version: VERSION, retries: MAX_RETRIES)
}
"#;
    let program = OutrunParser::parse_program(input).unwrap();

    let const_defs: Vec<_> = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::ConstDefinition(const_def) => Some(const_def),
            _ => None,
        })
        .collect();

    let func_defs: Vec<_> = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::FunctionDefinition(func_def) => Some(func_def),
            _ => None,
        })
        .collect();

    assert_eq!(const_defs.len(), 2);
    assert_eq!(func_defs.len(), 1);

    assert_eq!(const_defs[0].name.name, "VERSION");
    assert_eq!(const_defs[1].name.name, "MAX_RETRIES");

    assert_eq!(func_defs[0].name.name, "main");
}
