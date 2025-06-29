use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_basic_trait_definition() {
    let input = r#"trait Drawable {
        def draw(self: Self): String
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name_as_string(), "Drawable");
            assert!(trait_def.generic_params.is_none());
            assert!(trait_def.constraints.is_none());
            assert_eq!(trait_def.functions.len(), 1);

            match &trait_def.functions[0] {
                TraitFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "draw");
                    assert_eq!(sig.parameters.len(), 1);
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
            assert_eq!(trait_def.name_as_string(), "Serializable");

            assert!(trait_def.generic_params.is_some());
            let generics = trait_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");

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
            assert_eq!(trait_def.name_as_string(), "Comparable");

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
            assert_eq!(trait_def.name_as_string(), "Logger");
            assert_eq!(trait_def.functions.len(), 2);

            match &trait_def.functions[0] {
                TraitFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "log");
                }
                _ => panic!("Expected function signature"),
            }

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
            assert_eq!(trait_def.name_as_string(), "Marker");
            assert!(trait_def.generic_params.is_none());
            assert!(trait_def.constraints.is_none());
            assert_eq!(trait_def.functions.len(), 0);
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_trait_with_self_constraint() {
    let input = r#"trait Addable when Self: BinaryAddition {
        def add_to_self(value: Self, other: Self): Self
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name_as_string(), "Addable");
            assert!(trait_def.constraints.is_some());

            match trait_def.constraints.as_ref().unwrap() {
                ConstraintExpression::Constraint {
                    type_param,
                    trait_bound,
                    ..
                } => {
                    assert_eq!(type_param.name, "Self");
                    assert_eq!(trait_bound.len(), 1);
                    assert_eq!(trait_bound[0].name, "BinaryAddition");
                }
                _ => panic!("Expected simple Self constraint"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_trait_with_multiple_self_constraints() {
    let input = r#"trait Numeric when Self: BinaryAddition && Self: BinaryMultiplication && Self: Comparison {
        def compute(a: Self, b: Self): Self
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name_as_string(), "Numeric");
            assert!(trait_def.constraints.is_some());

            match trait_def.constraints.as_ref().unwrap() {
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
                                    trait_bound,
                                    ..
                                } => {
                                    assert_eq!(type_param.name, "Self");
                                    assert_eq!(trait_bound[0].name, "BinaryAddition");
                                }
                                _ => panic!("Expected first Self constraint"),
                            }
                            match nested_right.as_ref() {
                                ConstraintExpression::Constraint {
                                    type_param,
                                    trait_bound,
                                    ..
                                } => {
                                    assert_eq!(type_param.name, "Self");
                                    assert_eq!(trait_bound[0].name, "BinaryMultiplication");
                                }
                                _ => panic!("Expected second Self constraint"),
                            }
                        }
                        _ => panic!("Expected nested And constraint"),
                    }
                    match right.as_ref() {
                        ConstraintExpression::Constraint {
                            type_param,
                            trait_bound,
                            ..
                        } => {
                            assert_eq!(type_param.name, "Self");
                            assert_eq!(trait_bound[0].name, "Comparison");
                        }
                        _ => panic!("Expected third Self constraint"),
                    }
                }
                _ => panic!("Expected And constraint"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_trait_with_mixed_constraints() {
    let input = r#"trait Complex<T> when Self: Addable && T: Comparable {
        def process(self_val: Self, other_val: T): Self
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name_as_string(), "Complex");
            assert!(trait_def.constraints.is_some());

            match trait_def.constraints.as_ref().unwrap() {
                ConstraintExpression::And { left, right, .. } => {
                    match left.as_ref() {
                        ConstraintExpression::Constraint {
                            type_param,
                            trait_bound,
                            ..
                        } => {
                            assert_eq!(type_param.name, "Self");
                            assert_eq!(trait_bound[0].name, "Addable");
                        }
                        _ => panic!("Expected Self constraint"),
                    }
                    match right.as_ref() {
                        ConstraintExpression::Constraint {
                            type_param,
                            trait_bound,
                            ..
                        } => {
                            assert_eq!(type_param.name, "T");
                            assert_eq!(trait_bound[0].name, "Comparable");
                        }
                        _ => panic!("Expected T constraint"),
                    }
                }
                _ => panic!("Expected And constraint"),
            }
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
        (
            r#"trait Addable when Self: BinaryAddition {}"#,
            vec!["trait Addable", "when Self: BinaryAddition"],
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
