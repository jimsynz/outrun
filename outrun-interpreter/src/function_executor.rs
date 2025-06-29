//! Function execution system for the Outrun interpreter
//!
//! This module provides execution of user-defined functions including:
//! - Parameter binding and scope management
//! - Statement execution within function bodies
//! - Proper return value handling
//! - Integration with the evaluator for nested expressions

use crate::context::InterpreterContext;
use crate::evaluator::ExpressionEvaluator;
use crate::patterns::PatternMatchingUtils;
use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::checker::{
    TypedBlock, TypedFunctionDefinition, TypedParameter, TypedStatement,
};
use outrun_typechecker::compilation::compiler_environment::CompilerEnvironment;
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur during function execution
#[derive(Debug, Error)]
pub enum FunctionExecutionError {
    #[error("Evaluation error: {message}")]
    Evaluation { message: String, span: Span },

    #[error("Missing required parameter '{name}'")]
    MissingParameter { name: String, span: Span },

    #[error("Unknown parameter '{name}' provided")]
    UnknownParameter { name: String, span: Span },

    #[error("Function body must contain at least one statement")]
    EmptyFunctionBody { span: Span },

    #[error("Internal error: {message}")]
    Internal { message: String, span: Span },
}

/// Function executor for user-defined functions
pub struct FunctionExecutor;

impl FunctionExecutor {
    /// Create a new function executor
    pub fn new(_compiler_environment: CompilerEnvironment) -> Self {
        Self
    }

    /// Execute a typed function with the given parameters and arguments
    pub fn execute_typed_function(
        &self,
        context: &mut InterpreterContext,
        evaluator: &mut ExpressionEvaluator,
        typed_function: &TypedFunctionDefinition,
        arguments: HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, FunctionExecutionError> {
        // Check that the function body is not empty
        if typed_function.body.statements.is_empty() {
            // Check if this is a trait signature being incorrectly dispatched
            if typed_function.function_id.starts_with("trait_signature::") {
                return Err(FunctionExecutionError::Internal {
                    message: format!(
                        "Attempted to execute trait signature '{}'. This indicates a dispatch error - trait signatures are not executable, only concrete implementations should be dispatched.",
                        typed_function.name
                    ),
                    span,
                });
            }
            return Err(FunctionExecutionError::EmptyFunctionBody { span });
        }

        // Push a new function scope
        context.push_scope(crate::context::ScopeType::Function {
            name: typed_function.name.clone(),
        });

        // Bind arguments to parameters
        self.bind_typed_parameters(context, &typed_function.parameters, arguments, span)?;

        // Execute the function body (typed AST version)
        let result = self.execute_typed_function_body(context, evaluator, &typed_function.body);

        // Pop the function scope
        context
            .pop_scope()
            .map_err(|e| FunctionExecutionError::Internal {
                message: format!("Failed to pop function scope: {e:?}"),
                span,
            })?;

        result
    }

    /// Bind function arguments to typed parameters in the current scope
    fn bind_typed_parameters(
        &self,
        context: &mut InterpreterContext,
        parameters: &[TypedParameter],
        arguments: HashMap<String, Value>,
        span: Span,
    ) -> Result<(), FunctionExecutionError> {
        // Check that all required parameters are provided
        for param in parameters {
            let param_name = &param.name;
            if let Some(value) = arguments.get(param_name) {
                // Bind the parameter to the value
                context
                    .define_variable(param_name.clone(), value.clone())
                    .map_err(|e| FunctionExecutionError::Internal {
                        message: format!("Failed to bind parameter '{param_name}': {e:?}"),
                        span,
                    })?;
            } else {
                return Err(FunctionExecutionError::MissingParameter {
                    name: param_name.clone(),
                    span,
                });
            }
        }

        // Check for unknown parameters
        for arg_name in arguments.keys() {
            if !parameters.iter().any(|p| &p.name == arg_name) {
                return Err(FunctionExecutionError::UnknownParameter {
                    name: arg_name.clone(),
                    span,
                });
            }
        }

        Ok(())
    }

    /// Execute the typed function body and return the result of the last expression
    fn execute_typed_function_body(
        &self,
        context: &mut InterpreterContext,
        evaluator: &mut ExpressionEvaluator,
        body: &TypedBlock,
    ) -> Result<Value, FunctionExecutionError> {
        let mut last_value = None;

        // Execute each typed statement in order
        for statement in &body.statements {
            match statement {
                TypedStatement::Expression(typed_expr) => {
                    // Execute the typed expression directly - no conversion needed!
                    last_value = Some(evaluator.evaluate(context, typed_expr).map_err(|e| {
                        FunctionExecutionError::Evaluation {
                            message: format!("{e:?}"),
                            span: typed_expr.span,
                        }
                    })?);
                }
                TypedStatement::LetBinding(let_binding) => {
                    // Execute the typed expression for the let binding
                    let value = evaluator
                        .evaluate(context, &let_binding.expression)
                        .map_err(|e| FunctionExecutionError::Evaluation {
                            message: format!("{e:?}"),
                            span: let_binding.expression.span,
                        })?;

                    // TODO: Handle complex patterns when pattern matching is implemented
                    // For now, we'll use a simplified approach for let bindings
                    // The pattern should be analyzed during type checking, so we just bind the first variable
                    if let Some(first_var) =
                        self.extract_first_variable_from_pattern(&let_binding.pattern)
                    {
                        context
                            .define_variable(first_var, value.clone())
                            .map_err(|e| FunctionExecutionError::Internal {
                                message: format!("Failed to bind let variable: {e:?}"),
                                span: let_binding.span,
                            })?;
                        last_value = Some(value);
                    } else {
                        return Err(FunctionExecutionError::Internal {
                            message: "Complex let binding patterns not yet fully supported in function bodies".to_string(),
                            span: let_binding.span,
                        });
                    }
                }
            }
        }

        // Return the result of the last statement
        last_value.ok_or(FunctionExecutionError::EmptyFunctionBody { span: body.span })
    }

    /// Extract the first variable name from a pattern
    fn extract_first_variable_from_pattern(
        &self,
        pattern: &outrun_typechecker::patterns::TypedPattern,
    ) -> Option<String> {
        let variables = PatternMatchingUtils::extract_bound_variables(pattern);
        variables.into_iter().next()
    }
}
