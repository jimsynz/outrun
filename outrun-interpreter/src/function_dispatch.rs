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
            DispatchMethod::PreResolvedClause {
                trait_name,
                function_name,
                impl_type,
                selected_clause_id,
                guard_pre_evaluated,
                clause_source_order: _,
            } => self.dispatch_pre_resolved_clause(
                interpreter_context,
                evaluator,
                trait_name,
                function_name,
                impl_type.as_ref(),
                selected_clause_id,
                *guard_pre_evaluated,
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
        // Extract the base type ID for trait dispatch, resolving TypeVariables if needed
        let resolved_impl_type = match impl_type {
            outrun_typechecker::unification::StructuredType::TypeVariable(type_id) => {
                // Try to resolve Self or other type variables from context
                let type_name = self.compiler_environment
                    .resolve_type_name(type_id)
                    .unwrap_or_default();
                
                if type_name == "Self" {
                    // For Self type variables in trait functions, we need to get the actual implementing type
                    // This should be available from the call context or can be inferred from arguments
                    if let Some(first_arg) = call_context.arguments.values().next() {
                        // Infer the implementing type from the first argument
                        match self.infer_type_from_value(first_arg) {
                            Some(inferred_type) => inferred_type,
                            None => {
                                return Err(DispatchError::Internal {
                                    message: format!("Could not infer implementing type for Self from arguments"),
                                    span: call_context.span,
                                });
                            }
                        }
                    } else {
                        return Err(DispatchError::Internal {
                            message: format!("Self type variable found but no arguments to infer from"),
                            span: call_context.span,
                        });
                    }
                } else {
                    // Other type variables should have been resolved by now
                    return Err(DispatchError::Internal {
                        message: format!("Unresolved type variable in trait dispatch: {type_name}"),
                        span: call_context.span,
                    });
                }
            },
            _ => impl_type.clone(),
        };
        
        let _impl_type_id = match &resolved_impl_type {
            outrun_typechecker::unification::StructuredType::Simple(type_id) => type_id.clone(),
            outrun_typechecker::unification::StructuredType::Generic { base, .. } => base.clone(),
            _ => {
                return Err(DispatchError::Internal {
                    message: format!("Unsupported impl_type for trait dispatch: {resolved_impl_type:?}"),
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

        // CRITICAL FIX: Check for function clauses (guard clauses) within the specific trait implementation
        // We must look up clauses in the correct trait implementation module, not globally
        if let Some(clause_set) = self.lookup_function_clauses_in_trait_impl(&trait_type, &resolved_impl_type, &function_name_atom) {
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

        // If we reach here, the SMT-first clause lookup failed
        // This indicates a bug in the typechecker - all functions should have clause sets
        panic!(
            "TYPECHECKER BUG: No function clause set found for {}.{} on type {:?}. \
            All functions should be converted to clause sets during compilation. \
            This is a critical failure in the SMT-first architecture.",
            trait_name, function_name, resolved_impl_type
        );
    }

    /// 🚀 SMT-FIRST DISPATCH: Execute pre-resolved function clause
    /// This replaces all runtime SMT solving and guard evaluation with direct execution
    fn dispatch_pre_resolved_clause(
        &mut self,
        interpreter_context: &mut InterpreterContext,
        evaluator: &mut crate::evaluator::ExpressionEvaluator,
        trait_name: &str,
        _function_name: &str,
        impl_type: &outrun_typechecker::unification::StructuredType,
        selected_clause_id: &str,
        guard_pre_evaluated: Option<bool>,
        call_context: &FunctionCallContext,
    ) -> Result<Value, DispatchError> {
        // 🎯 CRITICAL INSIGHT: SMT has already solved everything at compile time!
        // - Which clause to use: selected_clause_id
        // - Whether guard passes: guard_pre_evaluated
        // - Argument compatibility: already verified by SMT constraints
        
        // Step 1: Look up the specific pre-resolved clause by ID
        let function_clause = self.lookup_clause_by_id(selected_clause_id, trait_name, impl_type)?;
        
        // Step 2: Verify guard pre-evaluation result (if applicable)
        if let Some(guard_result) = guard_pre_evaluated {
            if !guard_result {
                // SMT determined guard would fail - this shouldn't happen if SMT is correct
                return Err(DispatchError::Internal {
                    message: format!(
                        "SMT pre-evaluated guard as false for clause {}, but clause was selected. \
                        This indicates an SMT constraint solving bug.", 
                        selected_clause_id
                    ),
                    span: call_context.span,
                });
            }
            // Guard passes - continue execution
        }
        
        // Step 3: Execute the function clause directly (no runtime evaluation needed!)
        let function_executor = FunctionExecutor::new(self.compiler_environment.clone());
        function_executor
            .execute_typed_function(
                interpreter_context,
                evaluator,
                &function_clause,
                call_context.arguments.clone(),
                call_context.span,
            )
            .map_err(|e| DispatchError::FunctionExecution {
                message: format!("{e:?}"),
                span: call_context.span,
            })
    }

    /// Look up a specific function clause by its unique ID within a trait implementation
    fn lookup_clause_by_id(
        &self,
        clause_id: &str,
        trait_name: &str,
        impl_type: &outrun_typechecker::unification::StructuredType,
    ) -> Result<outrun_typechecker::checker::TypedFunctionDefinition, DispatchError> {
        // Create the trait type for module lookup
        let trait_type_id = self.compiler_environment.intern_type_name(trait_name);
        let trait_type = outrun_typechecker::unification::StructuredType::Simple(trait_type_id);
        
        // Look up the trait implementation module
        let modules = self.compiler_environment.modules().read().unwrap();
        let module_key = outrun_typechecker::compilation::compiler_environment::ModuleKey::TraitImpl(
            Box::new(trait_type),
            Box::new(impl_type.clone()),
        );
        
        if let Some(trait_impl_module) = modules.get(&module_key) {
            // Search through all function clauses in this module
            for clause_set in trait_impl_module.function_clauses.values() {
                for clause in clause_set.get_clauses_by_priority() {
                    if clause.clause_id == clause_id {
                        return Ok(clause.base_function.clone());
                    }
                }
            }
        }
        
        Err(DispatchError::Internal {
            message: format!(
                "Pre-resolved clause '{}' not found in trait implementation '{}' for type '{:?}'. \
                This indicates a mismatch between SMT analysis and interpreter state.",
                clause_id, trait_name, impl_type
            ),
            span: outrun_parser::Span::new(0, 0),
        })
    }

    // === Function Clause Dispatch with SMT Analysis ===

    /// Look up function clauses for a given function name (global search - used for static functions)
    fn lookup_function_clauses(
        &self,
        function_name: &outrun_typechecker::compilation::compiler_environment::AtomId,
    ) -> Option<outrun_typechecker::checker::FunctionClauseSet> {
        // Use the new public method on CompilerEnvironment
        self.compiler_environment.lookup_function_clauses_by_name(function_name)
    }

    /// Look up function clauses within a specific trait implementation module
    /// This is the correct approach for trait dispatch - we must respect module boundaries
    fn lookup_function_clauses_in_trait_impl(
        &self,
        trait_type: &outrun_typechecker::unification::StructuredType,
        impl_type: &outrun_typechecker::unification::StructuredType,
        function_name: &outrun_typechecker::compilation::compiler_environment::AtomId,
    ) -> Option<outrun_typechecker::checker::FunctionClauseSet> {
        // Look up function clauses in the specific trait implementation module
        self.compiler_environment.lookup_function_clauses_in_trait_impl(trait_type, impl_type, function_name)
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
        _span: outrun_parser::Span,
    ) -> Result<bool, DispatchError> {
        // Guard expressions must be side-effect-free and return Boolean
        // They can reference function parameters and perform pure computations
        
        // Create a temporary evaluation context with function parameters
        let mut guard_context = self.create_guard_evaluation_context(arguments)?;
        
        // CRITICAL FIX: Create evaluator with proper dispatch context to ensure correct trait resolution
        let dispatch_context = outrun_typechecker::context::FunctionDispatchContext::new(Some(self.compiler_environment.clone()));
        let mut evaluator = crate::evaluator::ExpressionEvaluator::from_dispatch_context(dispatch_context);
        
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

    /// Infer the structured type from a runtime value
    fn infer_type_from_value(&self, value: &crate::value::Value) -> Option<outrun_typechecker::unification::StructuredType> {
        use outrun_typechecker::unification::StructuredType;
        
        match value {
            crate::value::Value::Integer64(_) => {
                let type_id = self.compiler_environment.intern_type_name("Outrun.Core.Integer64");
                Some(StructuredType::Simple(type_id))
            },
            crate::value::Value::Float64(_) => {
                let type_id = self.compiler_environment.intern_type_name("Outrun.Core.Float64");
                Some(StructuredType::Simple(type_id))
            },
            crate::value::Value::String(_) => {
                let type_id = self.compiler_environment.intern_type_name("Outrun.Core.String");
                Some(StructuredType::Simple(type_id))
            },
            crate::value::Value::Boolean(_) => {
                let type_id = self.compiler_environment.intern_type_name("Outrun.Core.Boolean");
                Some(StructuredType::Simple(type_id))
            },
            crate::value::Value::Atom(_) => {
                let type_id = self.compiler_environment.intern_type_name("Outrun.Core.Atom");
                Some(StructuredType::Simple(type_id))
            },
            crate::value::Value::List { list: _, element_type: _ } => {
                // For lists, we'd need to infer the element type, but for now use generic List
                let type_id = self.compiler_environment.intern_type_name("Outrun.Core.List");
                Some(StructuredType::Simple(type_id))
            },
            // Add other value types as needed
            _ => None,
        }
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
