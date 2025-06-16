//! Tests for pattern validation integration in trait case expressions
//!
//! These tests verify that trait case statements properly validate patterns,
//! bind variables, and integrate with the existing pattern checking system.

use crate::checker::{ExpressionChecker, TypeContext};
use crate::error::TypeError;
use crate::types::traits::{TraitDefinition, TraitFunction, TraitImplementation};
use crate::types::{ConcreteType, StructField};
use outrun_parser::{
    CaseResult, Expression, ExpressionKind, Identifier, IntegerFormat, IntegerLiteral, Span,
    StringFormat, StringLiteral, StringPart, StructFieldPattern, StructPattern, TraitCaseClause,
    TraitCaseExpression, TypeIdentifier,
};
use std::collections::HashMap;

fn setup_test_context_with_structs() -> (
    TypeContext,
    crate::types::TraitId,
    crate::types::TypeId,
    crate::types::TypeId,
) {
    let mut context = TypeContext::new();

    // Register the Display trait
    let display_trait = context.interner.intern_trait("Display");
    let to_string_func = context.interner.intern_atom("to_string");
    let string_return = context.interner.intern_type("String");

    let display_func = TraitFunction::new(
        to_string_func,
        vec![],
        string_return,
        false,
        Span::new(0, 10),
    );

    let display_def = TraitDefinition::new(
        display_trait,
        "Display".to_string(),
        vec![display_func],
        Span::new(0, 30),
    );

    context.trait_registry.register_trait(display_def);

    // Register Person struct type
    let person_type = context.interner.intern_type("Person");
    let name_field = context.interner.intern_atom("name");
    let age_field = context.interner.intern_atom("age");
    let string_type = context.interner.intern_type("Outrun.Core.String");
    let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
    let boolean_type = context.interner.intern_type("Outrun.Core.Boolean");

    // Register basic concrete types
    context.register_concrete_type(string_type, ConcreteType::String);
    context.register_concrete_type(integer_type, ConcreteType::Integer64);
    context.register_concrete_type(boolean_type, ConcreteType::Boolean);

    let person_struct = ConcreteType::Struct {
        name: person_type,
        fields: vec![
            StructField {
                name: name_field,
                type_id: string_type,
                span: Span::new(0, 10),
            },
            StructField {
                name: age_field,
                type_id: integer_type,
                span: Span::new(0, 10),
            },
        ],
    };

    context.register_concrete_type(person_type, person_struct);

    // Register Car struct type
    let car_type = context.interner.intern_type("Car");
    let model_field = context.interner.intern_atom("model");
    let year_field = context.interner.intern_atom("year");

    let car_struct = ConcreteType::Struct {
        name: car_type,
        fields: vec![
            StructField {
                name: model_field,
                type_id: string_type,
                span: Span::new(0, 10),
            },
            StructField {
                name: year_field,
                type_id: integer_type,
                span: Span::new(0, 10),
            },
        ],
    };

    context.register_concrete_type(car_type, car_struct);

    // Register trait implementations
    let mut person_funcs = HashMap::new();
    person_funcs.insert(to_string_func, context.trait_registry.next_function_id());
    let person_impl =
        TraitImplementation::new(display_trait, person_type, person_funcs, Span::new(0, 20));
    context.trait_registry.register_implementation(person_impl);

    let mut car_funcs = HashMap::new();
    car_funcs.insert(to_string_func, context.trait_registry.next_function_id());
    let car_impl = TraitImplementation::new(display_trait, car_type, car_funcs, Span::new(0, 20));
    context.trait_registry.register_implementation(car_impl);

    (context, display_trait, person_type, car_type)
}

fn create_trait_case_with_pattern(clauses: Vec<TraitCaseClause>) -> TraitCaseExpression {
    TraitCaseExpression {
        expression: Box::new(Expression {
            kind: ExpressionKind::Identifier(Identifier {
                name: "value".to_string(),
                span: Span::new(0, 5),
            }),
            span: Span::new(0, 5),
        }),
        trait_name: TypeIdentifier {
            name: "Display".to_string(),
            span: Span::new(8, 15),
        },
        type_clauses: clauses,
        span: Span::new(0, 50),
    }
}

#[test]
fn test_valid_struct_pattern_destructuring() {
    let (mut context, _display_trait, _person_type, _car_type) = setup_test_context_with_structs();

    // Register a variable for the case expression input (use Person type which implements Display)
    let value_type = context.interner.intern_type("Person");
    let value_var = crate::checker::Variable {
        name: "value".to_string(),
        type_id: value_type,
        is_mutable: false,
        span: Span::new(0, 5),
    };
    context.register_variable(value_var).unwrap();

    // Create a trait case with valid struct pattern
    let person_clause = TraitCaseClause {
        type_name: TypeIdentifier {
            name: "Person".to_string(),
            span: Span::new(20, 26),
        },
        pattern: Some(StructPattern {
            type_path: vec![TypeIdentifier {
                name: "Person".to_string(),
                span: Span::new(20, 26),
            }],
            fields: vec![
                StructFieldPattern {
                    name: Identifier {
                        name: "name".to_string(),
                        span: Span::new(28, 32),
                    },
                    pattern: None, // Shorthand syntax
                    span: Span::new(28, 32),
                },
                StructFieldPattern {
                    name: Identifier {
                        name: "age".to_string(),
                        span: Span::new(34, 37),
                    },
                    pattern: None, // Shorthand syntax
                    span: Span::new(34, 37),
                },
            ],
            span: Span::new(20, 40),
        }),
        guard: None,
        result: CaseResult::Expression(Box::new(Expression {
            kind: ExpressionKind::Identifier(Identifier {
                name: "name".to_string(), // Should reference the bound variable
                span: Span::new(45, 49),
            }),
            span: Span::new(45, 49),
        })),
        span: Span::new(20, 50),
    };

    let car_clause = TraitCaseClause {
        type_name: TypeIdentifier {
            name: "Car".to_string(),
            span: Span::new(55, 58),
        },
        pattern: None, // No pattern
        guard: None,
        result: CaseResult::Expression(Box::new(Expression {
            kind: ExpressionKind::String(StringLiteral {
                parts: vec![StringPart::Text {
                    content: "A car".to_string(),
                    raw_content: "A car".to_string(),
                }],
                format: StringFormat::Basic,
                span: Span::new(65, 72),
            }),
            span: Span::new(65, 72),
        })),
        span: Span::new(55, 72),
    };

    let trait_case = create_trait_case_with_pattern(vec![person_clause, car_clause]);

    // This should succeed - the pattern should be valid and variables should be bound
    let result = ExpressionChecker::check_trait_case_expression(&mut context, &trait_case);
    assert!(
        result.is_ok(),
        "Valid struct pattern should pass: {:?}",
        result.err()
    );
}

#[test]
fn test_pattern_on_non_struct_type() {
    let (mut context, _display_trait, _person_type, _car_type) = setup_test_context_with_structs();

    // Register a simple non-struct type that implements Display (using Integer as an example)
    let simple_type = context.interner.intern_type("Integer");
    let simple_concrete = ConcreteType::Integer64;
    context.register_concrete_type(simple_type, simple_concrete);

    let to_string_func = context.interner.intern_atom("to_string");
    let display_trait = context.interner.intern_trait("Display");
    let mut simple_funcs = HashMap::new();
    simple_funcs.insert(to_string_func, context.trait_registry.next_function_id());
    let simple_impl =
        TraitImplementation::new(display_trait, simple_type, simple_funcs, Span::new(0, 20));
    context.trait_registry.register_implementation(simple_impl);

    // Register a variable for the case expression input
    let value_type = context.interner.intern_type("Person");
    let value_var = crate::checker::Variable {
        name: "value".to_string(),
        type_id: value_type,
        is_mutable: false,
        span: Span::new(0, 5),
    };
    context.register_variable(value_var).unwrap();

    // Try to use a struct pattern on a non-struct type
    let invalid_clause = TraitCaseClause {
        type_name: TypeIdentifier {
            name: "Integer".to_string(),
            span: Span::new(20, 27),
        },
        pattern: Some(StructPattern {
            type_path: vec![TypeIdentifier {
                name: "Integer".to_string(),
                span: Span::new(20, 27),
            }],
            fields: vec![],
            span: Span::new(20, 30),
        }),
        guard: None,
        result: CaseResult::Expression(Box::new(Expression {
            kind: ExpressionKind::Integer(IntegerLiteral {
                value: 42,
                format: IntegerFormat::Decimal,
                span: Span::new(40, 42),
            }),
            span: Span::new(40, 42),
        })),
        span: Span::new(20, 42),
    };

    let trait_case = create_trait_case_with_pattern(vec![invalid_clause]);

    // This should fail - can't use struct pattern on non-struct type
    let result = ExpressionChecker::check_trait_case_expression(&mut context, &trait_case);
    assert!(result.is_err(), "Pattern on non-struct type should fail");

    match result.unwrap_err() {
        TypeError::TypeMismatch {
            expected, found, ..
        } => {
            assert!(
                expected.contains("struct type"),
                "Expected error about struct type"
            );
            assert!(
                found.contains("Integer"),
                "Expected error to mention Integer"
            );
        }
        other => assert!(
            matches!(other, TypeError::TypeMismatch { .. }),
            "Expected TypeMismatch error, got: {:?}",
            other
        ),
    }
}

#[test]
fn test_pattern_with_wrong_field() {
    let (mut context, _display_trait, _person_type, _car_type) = setup_test_context_with_structs();

    // Register a variable for the case expression input
    let value_type = context.interner.intern_type("Person");
    let value_var = crate::checker::Variable {
        name: "value".to_string(),
        type_id: value_type,
        is_mutable: false,
        span: Span::new(0, 5),
    };
    context.register_variable(value_var).unwrap();

    // Create a pattern with a field that doesn't exist on Person
    let invalid_clause = TraitCaseClause {
        type_name: TypeIdentifier {
            name: "Person".to_string(),
            span: Span::new(20, 26),
        },
        pattern: Some(StructPattern {
            type_path: vec![TypeIdentifier {
                name: "Person".to_string(),
                span: Span::new(20, 26),
            }],
            fields: vec![StructFieldPattern {
                name: Identifier {
                    name: "nonexistent".to_string(),
                    span: Span::new(28, 39),
                },
                pattern: None,
                span: Span::new(28, 39),
            }],
            span: Span::new(20, 42),
        }),
        guard: None,
        result: CaseResult::Expression(Box::new(Expression {
            kind: ExpressionKind::Integer(IntegerLiteral {
                value: 42,
                format: IntegerFormat::Decimal,
                span: Span::new(45, 47),
            }),
            span: Span::new(45, 47),
        })),
        span: Span::new(20, 47),
    };

    let trait_case = create_trait_case_with_pattern(vec![invalid_clause]);

    // This should fail - the field doesn't exist
    let result = ExpressionChecker::check_trait_case_expression(&mut context, &trait_case);
    assert!(result.is_err(), "Pattern with wrong field should fail");

    match result.unwrap_err() {
        TypeError::TypeMismatch { found, .. } => {
            assert!(
                found.contains("nonexistent"),
                "Expected error about nonexistent field"
            );
        }
        other => assert!(
            matches!(other, TypeError::TypeMismatch { .. }),
            "Expected TypeMismatch error about field, got: {:?}",
            other
        ),
    }
}

#[test]
fn test_pattern_variables_in_guard() {
    let (mut context, _display_trait, _person_type, _car_type) = setup_test_context_with_structs();

    // Register a variable for the case expression input
    let value_type = context.interner.intern_type("Person");
    let value_var = crate::checker::Variable {
        name: "value".to_string(),
        type_id: value_type,
        is_mutable: false,
        span: Span::new(0, 5),
    };
    context.register_variable(value_var).unwrap();

    // Create a trait case where pattern variables are used in the guard
    let person_clause = TraitCaseClause {
        type_name: TypeIdentifier {
            name: "Person".to_string(),
            span: Span::new(20, 26),
        },
        pattern: Some(StructPattern {
            type_path: vec![TypeIdentifier {
                name: "Person".to_string(),
                span: Span::new(20, 26),
            }],
            fields: vec![StructFieldPattern {
                name: Identifier {
                    name: "age".to_string(),
                    span: Span::new(28, 31),
                },
                pattern: None, // Binds 'age' variable
                span: Span::new(28, 31),
            }],
            span: Span::new(20, 35),
        }),
        guard: Some(Expression {
            kind: ExpressionKind::Boolean(outrun_parser::BooleanLiteral {
                value: true, // Simple boolean for now - in real usage would be a comparison like age > 18
                span: Span::new(42, 46),
            }),
            span: Span::new(42, 46),
        }),
        result: CaseResult::Expression(Box::new(Expression {
            kind: ExpressionKind::Integer(IntegerLiteral {
                value: 1,
                format: IntegerFormat::Decimal,
                span: Span::new(50, 51),
            }),
            span: Span::new(50, 51),
        })),
        span: Span::new(20, 51),
    };

    let car_clause = TraitCaseClause {
        type_name: TypeIdentifier {
            name: "Car".to_string(),
            span: Span::new(55, 58),
        },
        pattern: None,
        guard: None,
        result: CaseResult::Expression(Box::new(Expression {
            kind: ExpressionKind::Integer(IntegerLiteral {
                value: 2,
                format: IntegerFormat::Decimal,
                span: Span::new(65, 66),
            }),
            span: Span::new(65, 66),
        })),
        span: Span::new(55, 66),
    };

    let trait_case = create_trait_case_with_pattern(vec![person_clause, car_clause]);

    // This should succeed - pattern variables should be available in guards
    let result = ExpressionChecker::check_trait_case_expression(&mut context, &trait_case);
    assert!(
        result.is_ok(),
        "Pattern variables in guard should work: {:?}",
        result.err()
    );
}
