//! Pattern type checking for destructuring
//!
//! This module handles type checking for all pattern types:
//! - Identifier patterns (variable binding)
//! - Literal patterns (exact value matching)
//! - Struct patterns (field destructuring)
//! - Tuple patterns (element destructuring)
//! - List patterns (head/tail destructuring)

use crate::checker::{TypeContext, Variable};
use crate::error::{TypeError, TypeResult};
use crate::types::{ConcreteType, TypeId};
use outrun_parser::{Literal, Pattern};

/// Pattern type checker
pub struct PatternChecker;

impl PatternChecker {
    /// Type check a pattern against a target type
    /// Returns variables bound by the pattern that should be added to scope
    pub fn check_pattern(
        context: &mut TypeContext,
        pattern: &Pattern,
        target_type: TypeId,
    ) -> TypeResult<Vec<Variable>> {
        let mut bound_variables = Vec::new();
        Self::check_pattern_recursive(context, pattern, target_type, &mut bound_variables)?;
        Ok(bound_variables)
    }

    /// Recursively check pattern and collect bound variables
    fn check_pattern_recursive(
        context: &mut TypeContext,
        pattern: &Pattern,
        target_type: TypeId,
        bound_variables: &mut Vec<Variable>,
    ) -> TypeResult<()> {
        match pattern {
            Pattern::Identifier(ident) => {
                // Identifier patterns always match and bind the target type
                bound_variables.push(Variable {
                    name: ident.name.clone(),
                    type_id: target_type,
                    is_mutable: false, // Patterns always bind immutable variables
                    span: ident.span,
                });
                Ok(())
            }

            Pattern::Literal(literal_pattern) => {
                Self::check_literal_pattern(context, literal_pattern, target_type)
            }

            Pattern::Tuple(tuple_pattern) => {
                Self::check_tuple_pattern(context, tuple_pattern, target_type, bound_variables)
            }

            Pattern::List(list_pattern) => {
                Self::check_list_pattern(context, list_pattern, target_type, bound_variables)
            }

            Pattern::Struct(struct_pattern) => {
                Self::check_struct_pattern(context, struct_pattern, target_type, bound_variables)
            }
        }
    }

    /// Check literal pattern matches target type
    fn check_literal_pattern(
        context: &mut TypeContext,
        literal_pattern: &outrun_parser::LiteralPattern,
        target_type: TypeId,
    ) -> TypeResult<()> {
        let literal_type = match &literal_pattern.literal {
            Literal::Integer(_) => context.interner.intern_type("Outrun.Core.Integer64"),
            Literal::Float(_) => context.interner.intern_type("Outrun.Core.Float64"),
            Literal::Boolean(_) => context.interner.intern_type("Outrun.Core.Boolean"),
            Literal::String(_) => context.interner.intern_type("Outrun.Core.String"),
            Literal::Atom(_) => context.interner.intern_type("Outrun.Core.Atom"),
        };

        // Check if literal type matches target type
        if literal_type != target_type {
            let literal_type_name = context.get_type_name(literal_type).unwrap_or("Unknown");
            let target_type_name = context.get_type_name(target_type).unwrap_or("Unknown");

            return Err(TypeError::type_mismatch(
                target_type_name.to_string(),
                literal_type_name.to_string(),
                crate::error::span_to_source_span(literal_pattern.span),
            ));
        }

        Ok(())
    }

    /// Check tuple pattern matches tuple target type
    fn check_tuple_pattern(
        context: &mut TypeContext,
        tuple_pattern: &outrun_parser::TuplePattern,
        target_type: TypeId,
        bound_variables: &mut Vec<Variable>,
    ) -> TypeResult<()> {
        let target_concrete = context
            .get_concrete_type(target_type)
            .ok_or_else(|| {
                TypeError::internal(format!(
                    "Target type {:?} not found in type registry",
                    target_type
                ))
            })?
            .clone();

        match target_concrete {
            ConcreteType::Tuple { element_types } => {
                if tuple_pattern.elements.len() != element_types.len() {
                    return Err(TypeError::type_mismatch(
                        format!("tuple with {} elements", element_types.len()),
                        format!("pattern with {} elements", tuple_pattern.elements.len()),
                        crate::error::span_to_source_span(tuple_pattern.span),
                    ));
                }

                // Check each element pattern recursively
                for (pattern, element_type) in
                    tuple_pattern.elements.iter().zip(element_types.iter())
                {
                    Self::check_pattern_recursive(
                        context,
                        pattern,
                        *element_type,
                        bound_variables,
                    )?;
                }
                Ok(())
            }
            _ => Err(TypeError::type_mismatch(
                "tuple type".to_string(),
                format!("{:?}", target_concrete),
                crate::error::span_to_source_span(tuple_pattern.span),
            )),
        }
    }

    /// Check list pattern matches list target type
    fn check_list_pattern(
        context: &mut TypeContext,
        list_pattern: &outrun_parser::ListPattern,
        target_type: TypeId,
        bound_variables: &mut Vec<Variable>,
    ) -> TypeResult<()> {
        let target_concrete = context
            .get_concrete_type(target_type)
            .ok_or_else(|| {
                TypeError::internal(format!(
                    "Target type {:?} not found in type registry",
                    target_type
                ))
            })?
            .clone();

        match target_concrete {
            ConcreteType::List { element_type } => {
                // Check all element patterns against the list element type
                for pattern in &list_pattern.elements {
                    Self::check_pattern_recursive(context, pattern, element_type, bound_variables)?;
                }

                // Handle rest pattern if present
                if let Some(rest_ident) = &list_pattern.rest {
                    // Rest pattern binds to the original list type
                    bound_variables.push(Variable {
                        name: rest_ident.name.clone(),
                        type_id: target_type, // Bind to the full list type
                        is_mutable: false,
                        span: rest_ident.span,
                    });
                }
                Ok(())
            }
            _ => Err(TypeError::type_mismatch(
                "list type".to_string(),
                format!("{:?}", target_concrete),
                crate::error::span_to_source_span(list_pattern.span),
            )),
        }
    }

    /// Check struct pattern matches struct target type  
    fn check_struct_pattern(
        context: &mut TypeContext,
        struct_pattern: &outrun_parser::StructPattern,
        target_type: TypeId,
        bound_variables: &mut Vec<Variable>,
    ) -> TypeResult<()> {
        let target_concrete = context
            .get_concrete_type(target_type)
            .ok_or_else(|| {
                TypeError::internal(format!(
                    "Target type {:?} not found in type registry",
                    target_type
                ))
            })?
            .clone();

        match target_concrete {
            ConcreteType::Struct {
                name: struct_name,
                fields,
            } => {
                // Check that pattern struct name matches target struct name
                let pattern_type_name = struct_pattern
                    .type_path
                    .iter()
                    .map(|t| t.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                let pattern_type_id = context.interner.intern_type(&pattern_type_name);
                if pattern_type_id != struct_name {
                    return Err(TypeError::type_mismatch(
                        context
                            .interner
                            .resolve_type(struct_name)
                            .unwrap_or("Unknown")
                            .to_string(),
                        pattern_type_name.clone(),
                        crate::error::span_to_source_span(struct_pattern.span),
                    ));
                }

                // Check each field pattern
                for field_pattern in &struct_pattern.fields {
                    // Find matching field in struct definition
                    let field_atom_id = context.interner.intern_atom(&field_pattern.name.name);
                    let field_info =
                        fields
                            .iter()
                            .find(|f| f.name == field_atom_id)
                            .ok_or_else(|| {
                                TypeError::type_mismatch(
                                    "valid field name".to_string(),
                                    format!(
                                        "field '{}' not found in struct",
                                        field_pattern.name.name
                                    ),
                                    crate::error::span_to_source_span(field_pattern.span),
                                )
                            })?;

                    match &field_pattern.pattern {
                        Some(pattern) => {
                            // Explicit field pattern: field: pattern
                            Self::check_pattern_recursive(
                                context,
                                pattern,
                                field_info.type_id,
                                bound_variables,
                            )?;
                        }
                        None => {
                            // Shorthand pattern: just field name
                            bound_variables.push(Variable {
                                name: field_pattern.name.name.clone(),
                                type_id: field_info.type_id,
                                is_mutable: false,
                                span: field_pattern.name.span,
                            });
                        }
                    }
                }
                Ok(())
            }
            _ => Err(TypeError::type_mismatch(
                "struct type".to_string(),
                format!("{:?}", target_concrete),
                crate::error::span_to_source_span(struct_pattern.span),
            )),
        }
    }

    /// Check if patterns are exhaustive (for case expressions)
    pub fn _check_exhaustiveness(
        _context: &mut TypeContext,
        _patterns: &[Pattern],
        _target_type: TypeId,
    ) -> TypeResult<()> {
        // TODO: Implement exhaustiveness checking
        // - Ensure all possible values are covered
        // - Handle wildcard patterns
        // - Check for unreachable patterns
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::TypeContext;
    use crate::types::{ConcreteType, StructField};
    use outrun_parser::{
        Identifier, IntegerLiteral, ListPattern, Literal, LiteralPattern, Pattern, Span,
        StructFieldPattern, StructPattern, TuplePattern, TypeIdentifier,
    };

    fn create_test_context() -> TypeContext {
        TypeContext::new()
    }

    #[test]
    fn test_identifier_pattern() {
        let mut context = create_test_context();
        let integer_type = context.interner.intern_type("Outrun.Core.Integer64");

        let pattern = Pattern::Identifier(Identifier {
            name: "x".to_string(),
            span: Span::new(0, 1),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, integer_type);
        assert!(result.is_ok());

        let bound_vars = result.unwrap();
        assert_eq!(bound_vars.len(), 1);
        assert_eq!(bound_vars[0].name, "x");
        assert_eq!(bound_vars[0].type_id, integer_type);
        assert!(!bound_vars[0].is_mutable);
    }

    #[test]
    fn test_literal_pattern_matching_type() {
        let mut context = create_test_context();
        let integer_type = context.interner.intern_type("Outrun.Core.Integer64");

        let pattern = Pattern::Literal(LiteralPattern {
            literal: Literal::Integer(IntegerLiteral {
                value: 42,
                format: outrun_parser::IntegerFormat::Decimal,
                span: Span::new(0, 2),
            }),
            span: Span::new(0, 2),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, integer_type);
        assert!(result.is_ok());

        let bound_vars = result.unwrap();
        assert_eq!(bound_vars.len(), 0); // Literal patterns don't bind variables
    }

    #[test]
    fn test_literal_pattern_type_mismatch() {
        let mut context = create_test_context();
        let boolean_type = context.interner.intern_type("Outrun.Core.Boolean");

        let pattern = Pattern::Literal(LiteralPattern {
            literal: Literal::Integer(IntegerLiteral {
                value: 42,
                format: outrun_parser::IntegerFormat::Decimal,
                span: Span::new(0, 2),
            }),
            span: Span::new(0, 2),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, boolean_type);
        assert!(result.is_err());

        match result.unwrap_err() {
            TypeError::TypeMismatch {
                expected, found, ..
            } => {
                assert_eq!(expected, "Outrun.Core.Boolean");
                assert_eq!(found, "Outrun.Core.Integer64");
            }
            _ => panic!("Expected TypeMismatch error"),
        }
    }

    #[test]
    fn test_tuple_pattern_matching_tuple() {
        let mut context = create_test_context();
        let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
        let string_type = context.interner.intern_type("Outrun.Core.String");

        // Create a tuple type (Integer64, String)
        let tuple_concrete_type = ConcreteType::Tuple {
            element_types: vec![integer_type, string_type],
        };
        let tuple_type_id = context.interner.intern_type("TestTuple");
        context.register_concrete_type(tuple_type_id, tuple_concrete_type);

        let pattern = Pattern::Tuple(TuplePattern {
            elements: vec![
                Pattern::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                }),
                Pattern::Identifier(Identifier {
                    name: "y".to_string(),
                    span: Span::new(3, 4),
                }),
            ],
            span: Span::new(0, 5),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, tuple_type_id);
        assert!(result.is_ok());

        let bound_vars = result.unwrap();
        assert_eq!(bound_vars.len(), 2);
        assert_eq!(bound_vars[0].name, "x");
        assert_eq!(bound_vars[0].type_id, integer_type);
        assert_eq!(bound_vars[1].name, "y");
        assert_eq!(bound_vars[1].type_id, string_type);
    }

    #[test]
    fn test_tuple_pattern_arity_mismatch() {
        let mut context = create_test_context();
        let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
        let string_type = context.interner.intern_type("Outrun.Core.String");

        // Create a tuple type (Integer64, String) - 2 elements
        let tuple_concrete_type = ConcreteType::Tuple {
            element_types: vec![integer_type, string_type],
        };
        let tuple_type_id = context.interner.intern_type("TestTuple");
        context.register_concrete_type(tuple_type_id, tuple_concrete_type);

        // Pattern with 3 elements - should fail
        let pattern = Pattern::Tuple(TuplePattern {
            elements: vec![
                Pattern::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                }),
                Pattern::Identifier(Identifier {
                    name: "y".to_string(),
                    span: Span::new(3, 4),
                }),
                Pattern::Identifier(Identifier {
                    name: "z".to_string(),
                    span: Span::new(6, 7),
                }),
            ],
            span: Span::new(0, 8),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, tuple_type_id);
        assert!(result.is_err());

        match result.unwrap_err() {
            TypeError::TypeMismatch {
                expected, found, ..
            } => {
                assert!(expected.contains("2 elements"));
                assert!(found.contains("3 elements"));
            }
            _ => panic!("Expected TypeMismatch error"),
        }
    }

    #[test]
    fn test_list_pattern_matching_list() {
        let mut context = create_test_context();
        let integer_type = context.interner.intern_type("Outrun.Core.Integer64");

        // Create a list type List<Integer64>
        let list_concrete_type = ConcreteType::List {
            element_type: integer_type,
        };
        let list_type_id = context.interner.intern_type("TestList");
        context.register_concrete_type(list_type_id, list_concrete_type);

        let pattern = Pattern::List(ListPattern {
            elements: vec![
                Pattern::Identifier(Identifier {
                    name: "first".to_string(),
                    span: Span::new(0, 5),
                }),
                Pattern::Identifier(Identifier {
                    name: "second".to_string(),
                    span: Span::new(7, 13),
                }),
            ],
            rest: Some(Identifier {
                name: "rest".to_string(),
                span: Span::new(16, 20),
            }),
            span: Span::new(0, 21),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, list_type_id);
        assert!(result.is_ok());

        let bound_vars = result.unwrap();
        assert_eq!(bound_vars.len(), 3);

        // Element patterns
        assert_eq!(bound_vars[0].name, "first");
        assert_eq!(bound_vars[0].type_id, integer_type);
        assert_eq!(bound_vars[1].name, "second");
        assert_eq!(bound_vars[1].type_id, integer_type);

        // Rest pattern binds to the full list type
        assert_eq!(bound_vars[2].name, "rest");
        assert_eq!(bound_vars[2].type_id, list_type_id);
    }

    #[test]
    fn test_struct_pattern_matching_struct() {
        let mut context = create_test_context();
        let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
        let string_type = context.interner.intern_type("Outrun.Core.String");

        // Create a User struct type
        let user_type_id = context.interner.intern_type("User");
        let name_atom = context.interner.intern_atom("name");
        let age_atom = context.interner.intern_atom("age");

        let user_concrete_type = ConcreteType::Struct {
            name: user_type_id,
            fields: vec![
                StructField {
                    name: name_atom,
                    type_id: string_type,
                    span: Span::new(0, 4),
                },
                StructField {
                    name: age_atom,
                    type_id: integer_type,
                    span: Span::new(5, 8),
                },
            ],
        };
        context.register_concrete_type(user_type_id, user_concrete_type);

        let pattern = Pattern::Struct(StructPattern {
            type_path: vec![TypeIdentifier {
                name: "User".to_string(),
                span: Span::new(0, 4),
            }],
            fields: vec![
                StructFieldPattern {
                    name: Identifier {
                        name: "name".to_string(),
                        span: Span::new(6, 10),
                    },
                    pattern: None, // Shorthand
                    span: Span::new(6, 10),
                },
                StructFieldPattern {
                    name: Identifier {
                        name: "age".to_string(),
                        span: Span::new(12, 15),
                    },
                    pattern: Some(Pattern::Identifier(Identifier {
                        name: "user_age".to_string(),
                        span: Span::new(17, 25),
                    })),
                    span: Span::new(12, 25),
                },
            ],
            span: Span::new(0, 26),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, user_type_id);
        assert!(result.is_ok());

        let bound_vars = result.unwrap();
        assert_eq!(bound_vars.len(), 2);

        // Shorthand field
        assert_eq!(bound_vars[0].name, "name");
        assert_eq!(bound_vars[0].type_id, string_type);

        // Explicit field pattern
        assert_eq!(bound_vars[1].name, "user_age");
        assert_eq!(bound_vars[1].type_id, integer_type);
    }

    #[test]
    fn test_struct_pattern_unknown_field() {
        let mut context = create_test_context();
        let string_type = context.interner.intern_type("Outrun.Core.String");

        // Create a User struct type with only name field
        let user_type_id = context.interner.intern_type("User");
        let name_atom = context.interner.intern_atom("name");

        let user_concrete_type = ConcreteType::Struct {
            name: user_type_id,
            fields: vec![StructField {
                name: name_atom,
                type_id: string_type,
                span: Span::new(0, 4),
            }],
        };
        context.register_concrete_type(user_type_id, user_concrete_type);

        let pattern = Pattern::Struct(StructPattern {
            type_path: vec![TypeIdentifier {
                name: "User".to_string(),
                span: Span::new(0, 4),
            }],
            fields: vec![StructFieldPattern {
                name: Identifier {
                    name: "unknown_field".to_string(),
                    span: Span::new(6, 19),
                },
                pattern: None,
                span: Span::new(6, 19),
            }],
            span: Span::new(0, 20),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, user_type_id);
        assert!(result.is_err());

        match result.unwrap_err() {
            TypeError::TypeMismatch {
                expected, found, ..
            } => {
                assert_eq!(expected, "valid field name");
                assert!(found.contains("unknown_field"));
            }
            _ => panic!("Expected TypeMismatch error"),
        }
    }

    #[test]
    fn test_nested_pattern_checking() {
        let mut context = create_test_context();
        let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
        let string_type = context.interner.intern_type("Outrun.Core.String");

        // Create a tuple type (Integer64, (String, Integer64))
        let inner_tuple_type = ConcreteType::Tuple {
            element_types: vec![string_type, integer_type],
        };
        let inner_tuple_type_id = context.interner.intern_type("InnerTuple");
        context.register_concrete_type(inner_tuple_type_id, inner_tuple_type);

        let outer_tuple_type = ConcreteType::Tuple {
            element_types: vec![integer_type, inner_tuple_type_id],
        };
        let outer_tuple_type_id = context.interner.intern_type("OuterTuple");
        context.register_concrete_type(outer_tuple_type_id, outer_tuple_type);

        let pattern = Pattern::Tuple(TuplePattern {
            elements: vec![
                Pattern::Identifier(Identifier {
                    name: "first".to_string(),
                    span: Span::new(0, 5),
                }),
                Pattern::Tuple(TuplePattern {
                    elements: vec![
                        Pattern::Identifier(Identifier {
                            name: "nested_str".to_string(),
                            span: Span::new(8, 18),
                        }),
                        Pattern::Identifier(Identifier {
                            name: "nested_int".to_string(),
                            span: Span::new(20, 30),
                        }),
                    ],
                    span: Span::new(7, 31),
                }),
            ],
            span: Span::new(0, 32),
        });

        let result = PatternChecker::check_pattern(&mut context, &pattern, outer_tuple_type_id);
        assert!(result.is_ok());

        let bound_vars = result.unwrap();
        assert_eq!(bound_vars.len(), 3);
        assert_eq!(bound_vars[0].name, "first");
        assert_eq!(bound_vars[0].type_id, integer_type);
        assert_eq!(bound_vars[1].name, "nested_str");
        assert_eq!(bound_vars[1].type_id, string_type);
        assert_eq!(bound_vars[2].name, "nested_int");
        assert_eq!(bound_vars[2].type_id, integer_type);
    }
}
