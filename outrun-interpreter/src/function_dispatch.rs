//! Function dispatch system for routing function calls to the correct implementation
//!
//! This module handles routing of function calls based on the dispatch strategy
//! determined by the typechecker. It supports:
//! - Static trait functions (e.g., Option.some, List.empty)
//! - Intrinsic functions (e.g., Outrun.Intrinsic.add_integer64)
//! - User-defined functions (TODO: Phase 4)
//! - Trait method dispatch (e.g., list.head where list: List<T>)

use crate::context::InterpreterContext;
use crate::function_call_context::FunctionCallContext;
use crate::function_executor::FunctionExecutor;
use crate::intrinsics::{IntrinsicError, IntrinsicsHandler};
use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::checker::{DispatchMethod, TypedExpression, TypedFunctionPath};
use outrun_typechecker::compilation::compiler_environment::CompilerEnvironment;
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur during function dispatch
#[derive(Debug, Error)]
pub enum DispatchError {
    #[error("Function '{name}' not found")]
    FunctionNotFound { name: String, span: Span },

    #[error("Trait '{trait_name}' not found")]
    TraitNotFound { trait_name: String, span: Span },

    #[error("No implementation of trait '{trait_name}' for type '{type_name}'")]
    NoTraitImplementation {
        trait_name: String,
        type_name: String,
        _span: Span,
    },

    #[error("Invalid function signature: expected {expected_args} arguments, got {actual_args}")]
    InvalidSignature {
        expected_args: usize,
        actual_args: usize,
        _span: Span,
    },

    #[error("Intrinsic error: {source}")]
    Intrinsic {
        #[from]
        source: IntrinsicError,
    },

    #[error("Function execution error: {message}")]
    FunctionExecution { message: String, span: Span },

    #[error("Internal dispatch error: {message}")]
    Internal { message: String, span: Span },
}

/// Function dispatcher that routes function calls to their implementations
pub struct FunctionDispatcher {
    /// Intrinsics handler for built-in functions
    intrinsics: IntrinsicsHandler,

    /// Compiler environment for type operations and function lookup
    compiler_environment: CompilerEnvironment,
}

impl FunctionDispatcher {
    /// Create a new function dispatcher with the given compiler environment
    pub fn new(compiler_environment: CompilerEnvironment) -> Self {
        Self {
            intrinsics: IntrinsicsHandler::new(),
            compiler_environment,
        }
    }

    /// Dispatch a function call using consolidated context object
    pub fn dispatch_function_call_with_context(
        &mut self,
        interpreter_context: &mut InterpreterContext,
        evaluator: &mut crate::evaluator::ExpressionEvaluator,
        call_context: FunctionCallContext,
    ) -> Result<Value, DispatchError> {
        match call_context.dispatch_strategy {
            DispatchMethod::Static { function_id } => self.dispatch_static_function_with_context(
                interpreter_context,
                evaluator,
                function_id,
                &call_context,
            ),
            DispatchMethod::Trait {
                trait_name,
                function_name,
                impl_type,
            } => self.dispatch_trait_function_with_context(
                interpreter_context,
                evaluator,
                trait_name,
                function_name,
                impl_type.as_ref(),
                &call_context,
            ),
        }
    }

    /// Dispatch a function call based on the strategy from the typechecker
    ///
    /// Note: This method is maintained for backward compatibility but internally
    /// uses the context-based approach for cleaner parameter management.
    pub fn dispatch_function_call(
        &mut self,
        context: &mut InterpreterContext,
        evaluator: &mut crate::evaluator::ExpressionEvaluator,
        function_path: &TypedFunctionPath,
        arguments: HashMap<String, Value>,
        dispatch_strategy: &DispatchMethod,
        typed_expr: &TypedExpression,
    ) -> Result<Value, DispatchError> {
        // Create the call context to consolidate parameters
        let call_context =
            FunctionCallContext::new(function_path, arguments, dispatch_strategy, typed_expr);

        // Delegate to the context-based method
        self.dispatch_function_call_with_context(context, evaluator, call_context)
    }

    /// Dispatch a static function call using context object
    fn dispatch_static_function_with_context(
        &mut self,
        interpreter_context: &mut InterpreterContext,
        evaluator: &mut crate::evaluator::ExpressionEvaluator,
        function_id: &str,
        call_context: &FunctionCallContext,
    ) -> Result<Value, DispatchError> {
        // Check if this is an intrinsic function
        if function_id.starts_with("Outrun.Intrinsic.") {
            return Ok(self.intrinsics.execute_intrinsic_with_types(
                function_id,
                call_context.arguments.clone(),
                Some(call_context.typed_expr),
                call_context.span,
            )?);
        }

        // Extract function name for lookup (trait functions have "trait::" prefix)
        let function_name = if function_id.starts_with("trait::") {
            function_id.split("::").last().unwrap_or(function_id)
        } else {
            function_id
        };
        let function_name_atom = self.compiler_environment.intern_atom_name(function_name);

        if let Some(function_entry) = self
            .compiler_environment
            .lookup_local_function(function_name_atom)
        {
            if let Some(typed_function) = function_entry.typed_definition() {
                let function_executor = FunctionExecutor::new(self.compiler_environment.clone());
                return function_executor
                    .execute_typed_function(
                        interpreter_context,
                        evaluator,
                        typed_function,
                        call_context.arguments.clone(),
                        call_context.span,
                    )
                    .map_err(|e| DispatchError::FunctionExecution {
                        message: format!("{:?}", e),
                        span: call_context.span,
                    });
            } else {
                return Err(DispatchError::Internal {
                    message: format!(
                        "Function '{}' found in registry but missing typed definition",
                        function_id
                    ),
                    span: call_context.span,
                });
            }
        }

        Err(DispatchError::Internal {
            message: format!("Static function '{}' not found in registry", function_id),
            span: call_context.span,
        })
    }

    /// Dispatch a trait function call using context object
    fn dispatch_trait_function_with_context(
        &mut self,
        interpreter_context: &mut InterpreterContext,
        evaluator: &mut crate::evaluator::ExpressionEvaluator,
        trait_name: &str,
        function_name: &str,
        impl_type: &outrun_typechecker::unification::StructuredType,
        call_context: &FunctionCallContext,
    ) -> Result<Value, DispatchError> {
        // Extract the base type ID for trait dispatch
        let _impl_type_id = match impl_type {
            outrun_typechecker::unification::StructuredType::Simple(type_id) => type_id.clone(),
            outrun_typechecker::unification::StructuredType::Generic { base, .. } => base.clone(),
            _ => {
                return Err(DispatchError::Internal {
                    message: format!("Unsupported impl_type for trait dispatch: {:?}", impl_type),
                    span: call_context.span,
                });
            }
        };

        // Get trait TypeNameId from trait name
        let trait_type_id = self.compiler_environment.intern_type_name(trait_name);

        // IMPORTANT: Always use Simple trait type for lookups, even for generic traits
        // Trait implementations are registered against the simple trait type (e.g., "List")
        // not the generic type (e.g., "List<T>")
        let trait_type = outrun_typechecker::unification::StructuredType::Simple(trait_type_id);
        let function_name_atom = self.compiler_environment.intern_atom_name(function_name);

        if let Some(function_entry) = self.compiler_environment.lookup_impl_function(
            &trait_type,
            impl_type,
            function_name_atom,
        ) {
            // Check if this is a trait signature (which should not be executed)
            match function_entry.function_type() {
                outrun_typechecker::compilation::FunctionType::TraitSignature => {
                    // Trait signatures should never be executed - this indicates a dispatch error
                    let impl_type_name = match impl_type {
                        outrun_typechecker::unification::StructuredType::Simple(type_id) => self
                            .compiler_environment
                            .resolve_type(type_id.clone())
                            .unwrap_or_else(|| format!("TypeNameId {:?} not found", type_id)),
                        outrun_typechecker::unification::StructuredType::Generic { base, args } => {
                            let base_name = self
                                .compiler_environment
                                .resolve_type(base.clone())
                                .unwrap_or_else(|| format!("TypeNameId {:?} not found", base));
                            format!("{}<{} args>", base_name, args.len())
                        }
                        _ => format!("{:?}", impl_type),
                    };
                    return Err(DispatchError::NoTraitImplementation {
                        trait_name: trait_name.to_string(),
                        type_name: impl_type_name,
                        _span: call_context.span,
                    });
                }
                _ => {
                    // Check if we have a typed function definition
                    if let Some(typed_function) = function_entry.typed_definition() {
                        // Execute the typed function (no conversion needed!)
                        let function_executor =
                            FunctionExecutor::new(self.compiler_environment.clone());
                        return function_executor
                            .execute_typed_function(
                                interpreter_context,
                                evaluator,
                                typed_function,
                                call_context.arguments.clone(),
                                call_context.span,
                            )
                            .map_err(|e| DispatchError::FunctionExecution {
                                message: format!("{:?}", e),
                                span: call_context.span,
                            });
                    } else {
                        // Handle the case where we found a generic implementation without typed definition
                        // For inspect functions on structs, fall back to default struct intrinsic
                        if function_name == "inspect" {
                            // Check if this is a struct type
                            if let outrun_typechecker::unification::StructuredType::Generic {
                                base,
                                ..
                            } = impl_type
                            {
                                let base_name = self
                                    .compiler_environment
                                    .resolve_type(base.clone())
                                    .unwrap_or_else(|| "Unknown".to_string());

                                // For struct types like Option.Some, use struct_inspect intrinsic
                                if base_name.contains('.')
                                    && (base_name.contains("Some")
                                        || base_name.contains("None")
                                        || base_name.contains("Ok")
                                        || base_name.contains("Error"))
                                {
                                    return Ok(self.intrinsics.execute_intrinsic_with_types(
                                        "Outrun.Intrinsic.struct_inspect",
                                        call_context.arguments.clone(),
                                        Some(call_context.typed_expr),
                                        call_context.span,
                                    )?);
                                }
                            }

                            // For simple struct types
                            if let outrun_typechecker::unification::StructuredType::Simple(
                                type_id,
                            ) = impl_type
                            {
                                let type_name = self
                                    .compiler_environment
                                    .resolve_type(type_id.clone())
                                    .unwrap_or_else(|| "Unknown".to_string());

                                // For struct types, use struct_inspect intrinsic
                                if type_name.contains('.') {
                                    return Ok(self.intrinsics.execute_intrinsic_with_types(
                                        "Outrun.Intrinsic.struct_inspect",
                                        call_context.arguments.clone(),
                                        Some(call_context.typed_expr),
                                        call_context.span,
                                    )?);
                                }
                            }
                        }

                        // For other cases, return the original error
                        return Err(DispatchError::Internal {
                            message: format!(
                                "Impl function '{}' found in registry but missing typed definition. This indicates a TypeInterner synchronization issue.",
                                function_name
                            ),
                            span: call_context.span,
                        });
                    }
                }
            }
        }

        // Get proper type name for error message
        let type_name = match impl_type {
            outrun_typechecker::unification::StructuredType::Simple(type_id) => self
                .compiler_environment
                .resolve_type(type_id.clone())
                .unwrap_or_else(|| format!("TypeNameId {:?} not found", type_id)),
            outrun_typechecker::unification::StructuredType::Generic { base, args } => {
                let base_name = self
                    .compiler_environment
                    .resolve_type(base.clone())
                    .unwrap_or_else(|| format!("TypeNameId {:?} not found", base));
                format!("{}<{} args>", base_name, args.len())
            }
            _ => format!("{:?}", impl_type),
        };

        Err(DispatchError::Internal {
            message: format!(
                "INTERPRETER BUG: Trait implementation for '{}' on type '{}' was not loaded into trait registry. \
                 Since typechecking passed, this implementation must exist but was not properly loaded by the interpreter.",
                trait_name, type_name
            ),
            span: call_context.span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::Span;
    use outrun_typechecker::checker::{TypedExpression, TypedExpressionKind, TypedFunctionPath};
    use outrun_typechecker::unification::StructuredType;

    fn create_test_typed_expression() -> TypedExpression {
        let compiler_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        let type_id = compiler_env.intern_type_name("TestType");

        TypedExpression {
            kind: TypedExpressionKind::Integer(42),
            structured_type: Some(StructuredType::Simple(type_id)),
            span: Span::new(0, 0),
            debug_info: None,
        }
    }

    #[test]
    fn test_function_dispatcher_creation() {
        let compiler_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        let _dispatcher = FunctionDispatcher::new(compiler_env);
        // Test passes if dispatcher can be created without panicking
    }

    #[test]
    fn test_trait_function_id_parsing() {
        // Test the function name extraction logic for trait function IDs
        let trait_function_id = "trait::Option::some";
        let extracted_name = if trait_function_id.starts_with("trait::") {
            trait_function_id
                .split("::")
                .last()
                .unwrap_or(trait_function_id)
        } else {
            trait_function_id
        };
        assert_eq!(extracted_name, "some");

        let regular_function_id = "regular_function";
        let extracted_name = if regular_function_id.starts_with("trait::") {
            regular_function_id
                .split("::")
                .last()
                .unwrap_or(regular_function_id)
        } else {
            regular_function_id
        };
        assert_eq!(extracted_name, "regular_function");
    }

    #[test]
    fn test_intrinsic_detection() {
        use outrun_typechecker::unification::UnificationContext;

        let compiler_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        let mut dispatcher = FunctionDispatcher::new(compiler_env);
        let test_compiler_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        let mut context =
            InterpreterContext::new(UnificationContext::new(), test_compiler_env, Some(100));
        let typed_expr = create_test_typed_expression();
        let function_path = TypedFunctionPath::Simple {
            name: "integer_to_string".to_string(),
        };
        let arguments = HashMap::new();

        let dispatch_strategy = DispatchMethod::Static {
            function_id: "Outrun.Intrinsic.integer_to_string".to_string(),
        };

        // This should route to intrinsics, even if the specific intrinsic doesn't exist
        let mut evaluator = crate::evaluator::ExpressionEvaluator::new(
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new(),
        );
        let result = dispatcher.dispatch_function_call(
            &mut context,
            &mut evaluator,
            &function_path,
            arguments,
            &dispatch_strategy,
            &typed_expr,
        );

        // Should return an intrinsic error, not a function not found error
        assert!(matches!(result, Err(DispatchError::Intrinsic { .. })));
    }
}
