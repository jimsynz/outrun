use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_basic_protocol_definition() {
    let input = r#"protocol Drawable {
        def draw(self: Self): String
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Drawable");
            assert!(protocol_def.generic_params.is_none());
            assert!(protocol_def.constraints.is_none());
            assert_eq!(protocol_def.functions.len(), 1);

            match &protocol_def.functions[0] {
                ProtocolFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "draw");
                    assert_eq!(sig.parameters.len(), 1);
                }
                _ => panic!("Expected function signature"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_protocol_with_generics() {
    let input = r#"protocol Serializable<T> {
        def serialize(self: Self): T
        def deserialize(data: T): Self
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Serializable");

            assert!(protocol_def.generic_params.is_some());
            let generics = protocol_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");

            assert_eq!(protocol_def.functions.len(), 2);
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_protocol_with_constraints() {
    let input = r#"protocol Comparable<T> when T: Orderable {
        def compare(self: Self, other: T): Integer
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Comparable");

            assert!(protocol_def.constraints.is_some());
            match protocol_def.constraints.as_ref().unwrap() {
                ConstraintExpression::Constraint {
                    type_param,
                    protocol_bound,
                    ..
                } => {
                    assert_eq!(type_param.name, "T");
                    assert_eq!(protocol_bound.len(), 1);
                    assert_eq!(protocol_bound[0].name, "Orderable");
                }
                _ => panic!("Expected simple constraint"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_protocol_with_default_implementation() {
    let input = r#"protocol Logger {
        def log(message: String): Unit
        def log_info(message: String): Unit
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Logger");
            assert_eq!(protocol_def.functions.len(), 2);

            match &protocol_def.functions[0] {
                ProtocolFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "log");
                }
                _ => panic!("Expected function signature"),
            }

            match &protocol_def.functions[1] {
                ProtocolFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "log_info");
                }
                _ => panic!("Expected function signature"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_empty_protocol() {
    let input = r#"protocol Marker {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Marker");
            assert!(protocol_def.generic_params.is_none());
            assert!(protocol_def.constraints.is_none());
            assert_eq!(protocol_def.functions.len(), 0);
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_protocol_with_self_constraint() {
    let input = r#"protocol Addable when Self: BinaryAddition {
        def add_to_self(value: Self, other: Self): Self
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Addable");
            assert!(protocol_def.constraints.is_some());

            match protocol_def.constraints.as_ref().unwrap() {
                ConstraintExpression::Constraint {
                    type_param,
                    protocol_bound,
                    ..
                } => {
                    assert_eq!(type_param.name, "Self");
                    assert_eq!(protocol_bound.len(), 1);
                    assert_eq!(protocol_bound[0].name, "BinaryAddition");
                }
                _ => panic!("Expected simple Self constraint"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_protocol_with_multiple_self_constraints() {
    let input = r#"protocol Numeric when Self: BinaryAddition && Self: BinaryMultiplication && Self: Comparison {
        def compute(a: Self, b: Self): Self
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Numeric");
            assert!(protocol_def.constraints.is_some());

            match protocol_def.constraints.as_ref().unwrap() {
                ConstraintExpression::And { left, right, .. } => {
                    match left.as_ref() {
                        ConstraintExpression::And {
                            left: nested_left,
                            right: nested_right,
                            ..
                        } => {
                            match nested_left.as_ref() {
                                ConstraintExpression::Constraint {
                                    type_param,
                                    protocol_bound,
                                    ..
                                } => {
                                    assert_eq!(type_param.name, "Self");
                                    assert_eq!(protocol_bound[0].name, "BinaryAddition");
                                }
                                _ => panic!("Expected first Self constraint"),
                            }
                            match nested_right.as_ref() {
                                ConstraintExpression::Constraint {
                                    type_param,
                                    protocol_bound,
                                    ..
                                } => {
                                    assert_eq!(type_param.name, "Self");
                                    assert_eq!(protocol_bound[0].name, "BinaryMultiplication");
                                }
                                _ => panic!("Expected second Self constraint"),
                            }
                        }
                        _ => panic!("Expected nested And constraint"),
                    }
                    match right.as_ref() {
                        ConstraintExpression::Constraint {
                            type_param,
                            protocol_bound,
                            ..
                        } => {
                            assert_eq!(type_param.name, "Self");
                            assert_eq!(protocol_bound[0].name, "Comparison");
                        }
                        _ => panic!("Expected third Self constraint"),
                    }
                }
                _ => panic!("Expected And constraint"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_protocol_with_mixed_constraints() {
    let input = r#"protocol Complex<T> when Self: Addable && T: Comparable {
        def process(self_val: Self, other_val: T): Self
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Complex");
            assert!(protocol_def.constraints.is_some());

            match protocol_def.constraints.as_ref().unwrap() {
                ConstraintExpression::And { left, right, .. } => {
                    match left.as_ref() {
                        ConstraintExpression::Constraint {
                            type_param,
                            protocol_bound,
                            ..
                        } => {
                            assert_eq!(type_param.name, "Self");
                            assert_eq!(protocol_bound[0].name, "Addable");
                        }
                        _ => panic!("Expected Self constraint"),
                    }
                    match right.as_ref() {
                        ConstraintExpression::Constraint {
                            type_param,
                            protocol_bound,
                            ..
                        } => {
                            assert_eq!(type_param.name, "T");
                            assert_eq!(protocol_bound[0].name, "Comparable");
                        }
                        _ => panic!("Expected T constraint"),
                    }
                }
                _ => panic!("Expected And constraint"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_protocol_display_formatting() {
    let inputs_and_patterns = [
        (
            r#"protocol Drawable { def draw(self: Self): String }"#,
            vec!["protocol Drawable", "def draw"],
        ),
        (
            r#"protocol Serializable<T> {}"#,
            vec!["protocol Serializable<T>"],
        ),
        (
            r#"protocol Comparable<T> when T: Orderable {}"#,
            vec!["protocol Comparable<T>", "when T: Orderable"],
        ),
        (
            r#"protocol Addable when Self: BinaryAddition {}"#,
            vec!["protocol Addable", "when Self: BinaryAddition"],
        ),
    ];

    for (input, expected_patterns) in inputs_and_patterns.iter() {
        let program = OutrunParser::parse_program(input).unwrap();
        let formatted = format!("{program}");

        for pattern in expected_patterns {
            assert!(
                formatted.contains(pattern),
                "Display format failed for: {input}. Expected pattern '{pattern}' not found in: {formatted}"
            );
        }
    }
}
