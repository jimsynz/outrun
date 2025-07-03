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

        // First, check if this function has multiple clauses (guards) that require SMT-based selection
        if let Some(clause_set) = self.lookup_function_clauses(&function_name_atom) {
            // SMT-based clause selection for functions with guards
            let selected_clause = self.select_function_clause_with_smt(
                &clause_set,
                &call_context.arguments,
                call_context.span,
            )?;
            
            let function_executor = FunctionExecutor::new(self.compiler_environment.clone());
            return function_executor
                .execute_typed_function(
                    interpreter_context,
                    evaluator,
                    selected_clause,
                    call_context.arguments.clone(),
                    call_context.span,
                )
                .map_err(|e| DispatchError::FunctionExecution {
                    message: format!("{e:?}"),
                    span: call_context.span,
                });
        }
        
        // Fall back to single function lookup
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
                        message: format!("{e:?}"),
                        span: call_context.span,
                    });
            } else {
                return Err(DispatchError::Internal {
                    message: format!(
                        "Function '{function_id}' found in registry but missing typed definition"
                    ),
                    span: call_context.span,
                });
            }
        }

        Err(DispatchError::Internal {
            message: format!("Static function '{function_id}' not found in registry"),
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
                    message: format!("Unsupported impl_type for trait dispatch: {impl_type:?}"),
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

        // CRITICAL FIX: Check for function clauses (guard clauses) before doing normal trait dispatch
        // This is the same logic as in dispatch_static_function_with_context (lines 151-173)
        if let Some(clause_set) = self.lookup_function_clauses(&function_name_atom) {
            // SMT-based clause selection for functions with guards
            let selected_clause = self.select_function_clause_with_smt(
                &clause_set,
                &call_context.arguments,
                call_context.span,
            )?;
            
            let function_executor = FunctionExecutor::new(self.compiler_environment.clone());
            return function_executor
                .execute_typed_function(
                    interpreter_context,
                    evaluator,
                    selected_clause,
                    call_context.arguments.clone(),
                    call_context.span,
                )
                .map_err(|e| DispatchError::FunctionExecution {
                    message: format!("{e:?}"),
                    span: call_context.span,
                });
        }

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
                            .unwrap_or_else(|| format!("TypeNameId {type_id:?} not found")),
                        outrun_typechecker::unification::StructuredType::Generic { base, args } => {
                            let base_name = self
                                .compiler_environment
                                .resolve_type(base.clone())
                                .unwrap_or_else(|| format!("TypeNameId {base:?} not found"));
                            format!("{}<{} args>", base_name, args.len())
                        }
                        _ => format!("{impl_type:?}"),
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

                        // Check if this is a trait default implementation that needs Self type context
                        // We need to check both the function type and whether this is actually a default implementation
                        let self_type_for_execution = match function_entry.function_type() {
                            outrun_typechecker::compilation::FunctionType::TraitDefault => {
                                // This is a trait default implementation - pass the impl_type as Self context
                                Some(impl_type)
                            }
                            _ => {
                                // Check if this is a default implementation being called through trait dispatch
                                // If we're calling not_equal? on a type that doesn't explicitly implement it,
                                // it must be using the default implementation
                                if function_name == "not_equal?" && trait_name == "Equality" {
                                    Some(impl_type)
                                } else {
                                    None
                                }
                            }
                        };

                        return function_executor
                            .execute_typed_function_with_self_type(
                                interpreter_context,
                                evaluator,
                                typed_function,
                                call_context.arguments.clone(),
                                call_context.span,
                                self_type_for_execution,
                            )
                            .map_err(|e| DispatchError::FunctionExecution {
                                message: format!("{e:?}"),
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
                                "Impl function '{function_name}' found in registry but missing typed definition. This indicates a TypeInterner synchronization issue."
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
                .unwrap_or_else(|| format!("TypeNameId {type_id:?} not found")),
            outrun_typechecker::unification::StructuredType::Generic { base, args } => {
                let base_name = self
                    .compiler_environment
                    .resolve_type(base.clone())
                    .unwrap_or_else(|| format!("TypeNameId {base:?} not found"));
                format!("{}<{} args>", base_name, args.len())
            }
            _ => format!("{impl_type:?}"),
        };

        Err(DispatchError::Internal {
            message: format!(
                "INTERPRETER BUG: Trait implementation for '{trait_name}' on type '{type_name}' was not loaded into trait registry. \
                 Since typechecking passed, this implementation must exist but was not properly loaded by the interpreter."
            ),
            span: call_context.span,
        })
    }

    // === Function Clause Dispatch with SMT Analysis ===

    /// Look up function clauses for a given function name
    fn lookup_function_clauses(
        &self,
        function_name: &outrun_typechecker::compilation::compiler_environment::AtomId,
    ) -> Option<outrun_typechecker::checker::FunctionClauseSet> {
        // Use the new public method on CompilerEnvironment
        self.compiler_environment.lookup_function_clauses_by_name(function_name)
    }

    /// Select the appropriate function clause using SMT analysis
    /// This is where the SMT-analyzed constraints are applied for runtime dispatch
    fn select_function_clause_with_smt<'a>(
        &self,
        clause_set: &'a outrun_typechecker::checker::FunctionClauseSet,
        arguments: &std::collections::HashMap<String, crate::value::Value>,
        span: outrun_parser::Span,
    ) -> Result<&'a outrun_typechecker::checker::TypedFunctionDefinition, DispatchError> {
        // Get clauses sorted by priority (SMT pre-computed)
        let candidates = clause_set.get_clauses_by_priority();
        
        // Iterate through clauses in priority order
        for clause in candidates {
            // Check if this clause is applicable
            if self.is_clause_applicable(clause, arguments)? {
                // Check guard condition if present
                if let Some(guard_expr) = &clause.base_function.guard {
                    if self.evaluate_guard_condition(guard_expr, arguments, span)? {
                        return Ok(&clause.base_function);
                    }
                } else {
                    // No guard, clause is applicable
                    return Ok(&clause.base_function);
                }
            }
        }
        
        // No applicable clause found
        Err(DispatchError::Internal {
            message: format!("No applicable clause found for function {}", clause_set.function_name),
            span,
        })
    }

    /// Check if a function clause is applicable based on argument types
    fn is_clause_applicable(
        &self,
        clause: &outrun_typechecker::checker::FunctionClause,
        arguments: &std::collections::HashMap<String, crate::value::Value>,
    ) -> Result<bool, DispatchError> {
        // Check argument type compatibility
        for parameter in &clause.base_function.parameters {
            if let Some(argument_value) = arguments.get(&parameter.name) {
                // Check if argument type matches parameter type
                // This would use the SMT ArgumentTypeMatch constraints in a full implementation
                if !self.value_matches_type(argument_value, &parameter.param_type) {
                    return Ok(false);
                }
            } else {
                // Missing required argument
                return Ok(false);
            }
        }
        
        Ok(true)
    }

    /// Evaluate a guard condition at runtime
    fn evaluate_guard_condition(
        &self,
        guard_expr: &outrun_typechecker::checker::TypedExpression,
        arguments: &std::collections::HashMap<String, crate::value::Value>,
        span: outrun_parser::Span,
    ) -> Result<bool, DispatchError> {
        // Guard expressions must be side-effect-free and return Boolean
        // They can reference function parameters and perform pure computations
        
        // Create a temporary evaluation context with function parameters
        let mut guard_context = self.create_guard_evaluation_context(arguments)?;
        
        // Create an expression evaluator for guard evaluation
        let mut evaluator = crate::evaluator::ExpressionEvaluator::new(self.compiler_environment.clone());
        
        // Evaluate the guard expression
        match evaluator.evaluate(&mut guard_context, guard_expr) {
            Ok(guard_result) => {
                // Guard expressions must return Boolean values
                match guard_result {
                    crate::value::Value::Boolean(result) => Ok(result),
                    other => {
                        // Log the issue but don't crash - guard evaluation failed so clause doesn't match
                        eprintln!("Warning: Guard expression returned non-Boolean value: {other:?}");
                        Ok(false) // Guard failed, so clause doesn't match
                    }
                }
            }
            Err(e) => {
                // Guard evaluation failed - this could be due to type mismatches or other issues
                // Instead of crashing, treat this as "guard condition not met"
                eprintln!("Warning: Guard evaluation failed: {e:?}");
                Ok(false) // Guard failed, so clause doesn't match - try next clause
            }
        }
    }
    
    /// Create a temporary evaluation context for guard expression evaluation
    fn create_guard_evaluation_context(
        &self,
        arguments: &std::collections::HashMap<String, crate::value::Value>,
    ) -> Result<crate::context::InterpreterContext, DispatchError> {
        // Create a new interpreter context for guard evaluation
        let unification_context = outrun_typechecker::unification::UnificationContext::new();
        let compiler_env = self.compiler_environment.clone();
        
        let mut context = crate::context::InterpreterContext::new(
            unification_context, 
            compiler_env, 
            Some(1000) // Small recursion limit for guards
        );
        
        // Add function arguments as local variables in the guard context
        for (param_name, param_value) in arguments {
            context.define_variable(param_name.clone(), param_value.clone())
                .map_err(|e| DispatchError::Internal {
                    message: format!("Failed to add guard parameter {param_name}: {e:?}"),
                    span: outrun_parser::Span::new(0, 0),
                })?;
        }
        
        Ok(context)
    }

    /// Check if a runtime value matches an expected parameter type
    fn value_matches_type(
        &self,
        _value: &crate::value::Value,
        _expected_type: &Option<outrun_typechecker::unification::StructuredType>,
    ) -> bool {
        // This would perform runtime type checking against the expected type
        // For now, we'll assume all values match (no type errors at runtime)
        // In a full implementation, this would check:
        // - Value::Integer64 matches StructuredType::Integer64
        // - Value::String matches StructuredType::String
        // - etc.
        true
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
