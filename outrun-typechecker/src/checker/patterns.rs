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
