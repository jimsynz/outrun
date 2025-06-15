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
        // Check if function name ends with '?' (guard function)
        let is_guard_function = function.name.name.ends_with('?');

        // 1. Validate parameters
        let parameters =
            Self::validate_parameters(context, &function.parameters, implementing_type)?;

        // 2. Validate return type
        let return_type = Self::validate_return_type(
            context,
            function.return_type.as_ref(),
            is_guard_function,
            function.span,
            implementing_type,
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

        // 4. Validate guard clause if present
        if let Some(guard) = &function.guard {
            Self::check_guard_clause(context, guard)?;
        }

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

        // 7. Register function signature (will be done by caller typically)
        // This is mainly for validation - actual registration happens in trait checking

        // 8. Clean up function scope
        context.pop_scope();

        Ok(())
    }

    /// Validate function parameters
    /// Returns a vector of (name, type_id, span) tuples for the validated parameters
    fn validate_parameters(
        context: &mut TypeContext,
        parameters: &[Parameter],
        implementing_type: Option<TypeId>,
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
                    &[], // No generic parameters in functions
                    implementing_type_id,
                )?
            } else {
                // Use regular type resolution for standalone functions
                TypeChecker::resolve_type_annotation(
                    context,
                    &param.type_annotation,
                    &[], // No generic parameters in functions
                )?
            };

            validated_params.push((param.name.name.clone(), param_type, param.span));
        }

        Ok(validated_params)
    }

    /// Validate return type for a function
    fn validate_return_type(
        context: &mut TypeContext,
        return_type: Option<&TypeAnnotation>,
        is_guard_function: bool,
        function_span: outrun_parser::Span,
        implementing_type: Option<TypeId>,
    ) -> TypeResult<TypeId> {
        let return_type_id = if let Some(ret_type) = return_type {
            // Explicit return type - validate it exists
            if let Some(implementing_type_id) = implementing_type {
                // Use Self-aware type resolution for impl block functions
                TypeChecker::resolve_type_annotation_with_self(
                    context,
                    ret_type,
                    &[], // No generic parameters in functions
                    implementing_type_id,
                )?
            } else {
                // Use regular type resolution for standalone functions
                TypeChecker::resolve_type_annotation(
                    context,
                    ret_type,
                    &[], // No generic parameters in functions
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

    /// Type check a guard clause
    fn check_guard_clause(context: &mut TypeContext, guard: &GuardClause) -> TypeResult<()> {
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

        Ok(())
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
}

#[cfg(test)]
mod tests {
    // use super::*; // TODO: Add when needed

    #[test]
    fn test_function_checker_stub() {
        // Placeholder test for function checker
        // TODO: Add real tests when implementation is complete
    }
}
