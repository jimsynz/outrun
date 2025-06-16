//! Function type checking and signature validation
//!
//! This module handles:
//! - Function definition validation
//! - Parameter type checking and uniqueness
//! - Return type validation
//! - Guard clause validation (must return Boolean)

use crate::checker::{ExpressionChecker, TypeChecker, TypeContext, Variable};
use crate::error::{TypeError, TypeResult};
use crate::types::TypeId;
use outrun_parser::{FunctionDefinition, GuardClause, Parameter, TypeAnnotation};
use std::collections::HashSet;

/// Function type checker
pub struct FunctionChecker;

impl FunctionChecker {
    /// Type check a function definition
    pub fn check_function_definition(
        context: &mut TypeContext,
        function: &FunctionDefinition,
    ) -> TypeResult<()> {
        Self::check_function_definition_with_context(context, function, None)
    }

    /// Type check a function definition with optional implementing type context
    /// When implementing_type is Some, Self will resolve to that type
    pub fn check_function_definition_with_context(
        context: &mut TypeContext,
        function: &FunctionDefinition,
        implementing_type: Option<TypeId>,
    ) -> TypeResult<()> {
        Self::check_function_definition_with_generic_context(
            context,
            function,
            implementing_type,
            &[], // No generic parameters for backwards compatibility
        )
    }

    /// Type check a function definition with full generic context
    /// When implementing_type is Some, Self will resolve to that type
    /// generic_params: Available generic type parameters from containing context (trait/impl/struct)
    pub fn check_function_definition_with_generic_context(
        context: &mut TypeContext,
        function: &FunctionDefinition,
        implementing_type: Option<TypeId>,
        generic_params: &[(String, TypeId)],
    ) -> TypeResult<()> {
        // Check if function name ends with '?' (guard function)
        let is_guard_function = function.name.name.ends_with('?');

        // 1. Validate parameters
        let parameters = Self::validate_parameters_with_generics(
            context,
            &function.parameters,
            implementing_type,
            generic_params,
        )?;

        // 2. Validate return type
        let return_type = Self::validate_return_type_with_generics(
            context,
            function.return_type.as_ref(),
            is_guard_function,
            function.span,
            implementing_type,
            generic_params,
        )?;

        // 3. Create function scope and register parameters
        context.push_scope(true); // Function scope

        // Register parameters as variables in function scope
        for (param_name, param_type, param_span) in &parameters {
            let variable = Variable {
                name: param_name.clone(),
                type_id: *param_type,
                is_mutable: false, // Outrun is immutable
                span: *param_span,
            };
            context.register_variable(variable)?;
        }

        // 4. Validate guard clause if present and create typed guard clause
        let typed_guard_clause = if let Some(guard) = &function.guard {
            let typed_guard = Self::check_guard_clause(context, guard)?;
            Some(crate::checker::context::TypedGuardClause {
                condition: typed_guard,
                span: guard.span,
            })
        } else {
            None
        };

        // 5. Type check function body
        let body_type = Self::check_function_body(context, &function.body, return_type)?;

        // 6. Ensure body type matches return type
        if body_type != return_type {
            let expected_type_name = context
                .interner
                .resolve_type(return_type)
                .unwrap_or("<unknown>")
                .to_string();
            let found_type_name = context
                .interner
                .resolve_type(body_type)
                .unwrap_or("<unknown>")
                .to_string();

            context.pop_scope(); // Clean up before error
            return Err(TypeError::type_mismatch(
                expected_type_name,
                found_type_name,
                crate::error::span_to_source_span(function.body.span),
            ));
        }

        // 7. Clean up function scope
        context.pop_scope();

        // 8. Create and register function signature with guard clause in global scope
        let function_signature = crate::checker::context::FunctionSignature {
            name: function.name.name.clone(),
            params: parameters
                .iter()
                .map(|(name, type_id, _span)| {
                    let atom_id = context.interner.intern_atom(name);
                    (atom_id, *type_id)
                })
                .collect(),
            return_type,
            is_guard: is_guard_function,
            guard_clause: typed_guard_clause,
            span: function.span,
        };

        // Register the function signature (supports overloading) in global scope
        context.register_function(function_signature)?;

        Ok(())
    }

    /// Validate function parameters with generic context support
    /// Returns a vector of (name, type_id, span) tuples for the validated parameters
    fn validate_parameters_with_generics(
        context: &mut TypeContext,
        parameters: &[Parameter],
        implementing_type: Option<TypeId>,
        generic_params: &[(String, TypeId)],
    ) -> TypeResult<Vec<(String, TypeId, outrun_parser::Span)>> {
        let mut validated_params = Vec::new();
        let mut seen_names = HashSet::new();

        for param in parameters {
            // Check parameter name uniqueness
            if seen_names.contains(&param.name.name) {
                return Err(TypeError::VariableAlreadyDefined {
                    span: crate::error::span_to_source_span(param.span),
                    previous_span: crate::error::span_to_source_span(param.span), // TODO: Track actual previous span
                    name: param.name.name.clone(),
                });
            }
            seen_names.insert(param.name.name.clone());

            // Validate parameter type exists
            let param_type = if let Some(implementing_type_id) = implementing_type {
                // Use Self-aware type resolution for impl block functions
                TypeChecker::resolve_type_annotation_with_self(
                    context,
                    &param.type_annotation,
                    generic_params, // Pass generic parameters from containing context
                    implementing_type_id,
                )?
            } else {
                // Use regular type resolution for standalone functions
                TypeChecker::resolve_type_annotation(
                    context,
                    &param.type_annotation,
                    generic_params, // Pass generic parameters from containing context
                )?
            };

            validated_params.push((param.name.name.clone(), param_type, param.span));
        }

        Ok(validated_params)
    }

    /// Validate return type for a function with generic context support
    fn validate_return_type_with_generics(
        context: &mut TypeContext,
        return_type: Option<&TypeAnnotation>,
        is_guard_function: bool,
        function_span: outrun_parser::Span,
        implementing_type: Option<TypeId>,
        generic_params: &[(String, TypeId)],
    ) -> TypeResult<TypeId> {
        let return_type_id = if let Some(ret_type) = return_type {
            // Explicit return type - validate it exists
            if let Some(implementing_type_id) = implementing_type {
                // Use Self-aware type resolution for impl block functions
                TypeChecker::resolve_type_annotation_with_self(
                    context,
                    ret_type,
                    generic_params, // Pass generic parameters from containing context
                    implementing_type_id,
                )?
            } else {
                // Use regular type resolution for standalone functions
                TypeChecker::resolve_type_annotation(
                    context,
                    ret_type,
                    generic_params, // Pass generic parameters from containing context
                )?
            }
        } else {
            // No explicit return type - error (all functions must have explicit return types)
            return Err(TypeError::UnimplementedFeature {
                feature: "Functions must have explicit return type annotations".to_string(),
                span: crate::error::span_to_source_span(function_span),
            });
        };

        // Guard functions must return Boolean
        if is_guard_function {
            let boolean_type = context.interner.intern_type("Outrun.Core.Boolean");
            if return_type_id != boolean_type {
                let found_type_name = context
                    .interner
                    .resolve_type(return_type_id)
                    .unwrap_or("<unknown>")
                    .to_string();
                return Err(TypeError::InvalidGuardFunction {
                    span: crate::error::span_to_source_span(function_span),
                    function_name: "<function>".to_string(), // TODO: Pass function name
                    actual_return_type: found_type_name,
                });
            }
        }

        Ok(return_type_id)
    }

    /// Type check a guard clause and return the typed expression
    fn check_guard_clause(
        context: &mut TypeContext,
        guard: &GuardClause,
    ) -> TypeResult<crate::checker::TypedExpression> {
        // Type check the guard expression
        let guard_expr_type = ExpressionChecker::check_expression(context, &guard.condition)?;

        // Ensure guard expression returns Boolean
        let boolean_type = context.interner.intern_type("Outrun.Core.Boolean");
        if guard_expr_type.type_id != boolean_type {
            let found_type_name = context
                .interner
                .resolve_type(guard_expr_type.type_id)
                .unwrap_or("<unknown>")
                .to_string();
            return Err(TypeError::InvalidConditionType {
                span: crate::error::span_to_source_span(guard.span),
                found_type: found_type_name,
            });
        }

        // TODO: Validate guard functions are side-effect free (complex analysis)
        // For now, we just ensure the type is correct

        Ok(guard_expr_type)
    }

    /// Type check function body and ensure it produces the expected return type
    fn check_function_body(
        context: &mut TypeContext,
        body: &outrun_parser::Block,
        _expected_return_type: TypeId,
    ) -> TypeResult<TypeId> {
        // Type check the function body using the existing block checker
        let typed_body = ExpressionChecker::check_block(context, body)?;

        Ok(typed_body.result_type)
    }

    /// Type check function body and return typed block for AST generation
    pub fn check_function_body_typed(
        context: &mut TypeContext,
        body: &outrun_parser::Block,
        _expected_return_type: TypeId,
    ) -> TypeResult<crate::checker::TypedBlock> {
        // Type check the function body using the existing block checker
        ExpressionChecker::check_block(context, body)
    }
}
