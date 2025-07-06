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
            // For non-trait functions, create a placeholder impl type
            let placeholder_impl_type = outrun_typechecker::unification::StructuredType::Simple(
                self.compiler_environment.intern_type_name("Placeholder")
            );
            let selected_clause = self.select_function_clause_with_smt(
                &clause_set,
                &call_context.arguments,
                &placeholder_impl_type,
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
        
        // CRITICAL FIX: Check trait module function clauses for trait static functions
        // If function_id is "trait::TraitName::function", we need to search the trait module
        if function_id.starts_with("trait::") {
            if let Some(clause_set) = self.lookup_trait_static_function_clauses(function_id, &function_name_atom) {
                // For trait static functions, create a placeholder impl type
                let placeholder_impl_type = outrun_typechecker::unification::StructuredType::Simple(
                    self.compiler_environment.intern_type_name("Placeholder")
                );
                let selected_clause = self.select_function_clause_with_smt(
                    &clause_set,
                    &call_context.arguments,
                    &placeholder_impl_type,
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
        
        println!("🔍 DEBUG: Trait dispatch for {trait_name}.{function_name}");
        println!("🔍 DEBUG: Trait type: {trait_type:?}");
        println!("🔍 DEBUG: Resolved impl type: {resolved_impl_type:?}");
        println!("🔍 DEBUG: First argument value: {:?}", call_context.arguments.values().next());
        
        // Use cross-module clause lookup to get clauses from ALL implementations, not just one
        if let Some(clause_set) = self.compiler_environment.lookup_all_function_clauses_for_trait(&trait_type, &function_name_atom) {
            // SMT-based clause selection for functions with guards
            let selected_clause = self.select_function_clause_with_smt(
                &clause_set,
                &call_context.arguments,
                &resolved_impl_type,
                call_context.span,
            )?;
            
            println!("🔍 DEBUG: Found clause set, executing selected clause: {}", selected_clause.name);
            println!("🔍 DEBUG: Function parameters: {:?}", selected_clause.parameters);
            
            let function_executor = FunctionExecutor::new(self.compiler_environment.clone());
            let result = function_executor
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
            
            if let Ok(ref value) = result {
                println!("🔍 DEBUG: Function returned: {}", value);
            }
            
            return result;
        }

        // If we reach here, the SMT-first clause lookup failed
        // This indicates a bug in the typechecker - all functions should have clause sets
        
        // Gather debugging information before panic
        let modules = self.compiler_environment.modules().read().unwrap();
        let available_modules: Vec<String> = modules.keys().map(|k| format!("{:?}", k)).collect();
        let total_function_clauses: usize = modules.values()
            .map(|m| m.function_clauses.len())
            .sum();
        
        // Try to find what function clauses DO exist for this trait/impl combination
        let module_key = outrun_typechecker::compilation::compiler_environment::ModuleKey::TraitImpl(
            Box::new(outrun_typechecker::unification::StructuredType::Simple(
                self.compiler_environment.intern_type_name(trait_name)
            )),
            Box::new(resolved_impl_type.clone()),
        );
        
        let existing_clauses: Vec<String> = if let Some(trait_impl_module) = modules.get(&module_key) {
            trait_impl_module.function_clauses.keys()
                .map(|atom_id| self.compiler_environment.resolve_atom_name(atom_id).unwrap_or_default())
                .collect()
        } else {
            vec!["MODULE NOT FOUND".to_string()]
        };
        
        panic!(
            "TYPECHECKER BUG: No function clause set found for {trait_name}.{function_name} on type {resolved_impl_type:?}\n\
            DEBUGGING INFO:\n\
            - Total modules in compiler environment: {}\n\
            - Total function clause sets across all modules: {total_function_clauses}\n\
            - Available modules: {available_modules:?}\n\
            - Existing function clauses in target module: {existing_clauses:?}\n\
            - Target module key: {module_key:?}\n\
            This indicates the typechecker failed to create function clause sets during compilation.\n\
            Check Phase 6.5 SMT clause analysis and function clause registration phases.",
            available_modules.len()
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
            // Search for clause by exact ID matching (now using SMT-format IDs)
            for (_function_atom, clause_set) in &trait_impl_module.function_clauses {
                for clause in clause_set.get_clauses_by_priority().iter() {
                    if clause.clause_id == clause_id {
                        return Ok(clause.base_function.clone());
                    }
                }
            }
            
            // If not found, show debug info for Option specifically
            if trait_name == "Option" {
                println!("🔍 DEBUG: Option trait impl module found, but clause '{}' not found", clause_id);
                println!("🔍 DEBUG: Available Option clauses:");
                for (_function_atom, clause_set) in &trait_impl_module.function_clauses {
                    for clause in clause_set.get_clauses_by_priority().iter() {
                        if clause.base_function.name.contains("some") {
                            println!("  - Available Option clause: '{}'", clause.clause_id);
                        }
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
        println!("🔍 DEBUG: lookup_function_clauses called for {:?}", function_name);
        
        // Use the new public method on CompilerEnvironment
        let result = self.compiler_environment.lookup_function_clauses_by_name(function_name);
        
        match &result {
            Some(clause_set) => {
                println!("✅ DEBUG: Found global function clause set for {:?} with {} clauses", 
                    self.compiler_environment.resolve_atom_name(function_name).unwrap_or_default(),
                    clause_set.get_clauses_by_priority().len());
            }
            None => {
                println!("❌ DEBUG: No global function clause set found for {:?}", 
                    self.compiler_environment.resolve_atom_name(function_name).unwrap_or_default());
            }
        }
        
        result
    }

    /// Look up function clauses for trait static functions (e.g., "trait::Option::some")
    /// These are static functions defined with `defs` in trait definitions
    fn lookup_trait_static_function_clauses(
        &self,
        function_id: &str, // Full ID like "trait::Option::some"
        function_name: &outrun_typechecker::compilation::compiler_environment::AtomId,
    ) -> Option<outrun_typechecker::checker::FunctionClauseSet> {
        // Parse trait name from "trait::TraitName::function"
        let parts: Vec<&str> = function_id.split("::").collect();
        if parts.len() != 3 || parts[0] != "trait" {
            return None;
        }
        
        let trait_name = parts[1];
        
        // Create trait type for module lookup
        let trait_type_id = self.compiler_environment.intern_type_name(trait_name);
        let _trait_type = outrun_typechecker::unification::StructuredType::Simple(trait_type_id.clone());
        
        // Look up function clauses in the trait definition module
        let modules = self.compiler_environment.modules().read().unwrap();
        let trait_module_key = outrun_typechecker::compilation::compiler_environment::ModuleKey::Module(trait_type_id.hash);
        
        if let Some(trait_module) = modules.get(&trait_module_key) {
            trait_module.function_clauses.get(function_name).cloned()
        } else {
            None
        }
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
        impl_type: &outrun_typechecker::unification::StructuredType,
        span: outrun_parser::Span,
    ) -> Result<&'a outrun_typechecker::checker::TypedFunctionDefinition, DispatchError> {
        // Get clauses sorted by priority (SMT pre-computed)
        let candidates = clause_set.get_clauses_by_priority();
        
        println!("🔍 CLAUSE SET: Function '{}' has {} clauses:", clause_set.function_name, candidates.len());
        for (i, clause) in candidates.iter().enumerate() {
            println!("🔍   Clause {}: {}", i, clause.clause_id);
        }
        
        // Strategy 1: Try to find an exact type match first
        for clause in &candidates {
            if self.is_clause_exact_match(clause, arguments, impl_type)? {
                // Check guard condition if present
                if let Some(guard_expr) = &clause.base_function.guard {
                    if self.evaluate_guard_condition(guard_expr, arguments, span)? {
                        println!("🔍 CLAUSE SELECTION: Selected exact match clause with guard: {}", clause.base_function.name);
                        return Ok(&clause.base_function);
                    }
                } else {
                    // No guard, exact match clause is best choice
                    println!("🔍 CLAUSE SELECTION: Selected exact match clause: {}", clause.base_function.name);
                    return Ok(&clause.base_function);
                }
            }
        }
        
        // Strategy 2: Fall back to general applicability check
        for clause in &candidates {
            // Check if this clause is applicable (but not exact match)
            if self.is_clause_applicable(clause, arguments, impl_type)? {
                // Check guard condition if present
                if let Some(guard_expr) = &clause.base_function.guard {
                    if self.evaluate_guard_condition(guard_expr, arguments, span)? {
                        println!("🔍 CLAUSE SELECTION: Selected fallback clause with guard: {}", clause.base_function.name);
                        return Ok(&clause.base_function);
                    }
                } else {
                    // No guard, clause is applicable
                    println!("🔍 CLAUSE SELECTION: Selected fallback clause: {}", clause.base_function.name);
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

    /// Check if a function clause is an exact match for the argument types
    /// This is used for precise dispatch when multiple clauses could be applicable
    fn is_clause_exact_match(
        &self,
        clause: &outrun_typechecker::checker::FunctionClause,
        arguments: &std::collections::HashMap<String, crate::value::Value>,
        impl_type: &outrun_typechecker::unification::StructuredType,
    ) -> Result<bool, DispatchError> {
        // For exact matching, we need to check if the clause's source implementation type
        // exactly matches the concrete type of the ACTUAL argument values (not typechecker impl_type)
        
        println!("🔍 EXACT MATCH CHECK: Checking clause ID: {}", clause.clause_id);
        
        // Extract the implementing type from the clause's source module
        if let Some(clause_impl_type) = self.extract_clause_implementing_type(clause) {
            println!("🔍 EXACT MATCH: Clause impl type: {:?}", clause_impl_type);
            
            // Infer the actual implementing type from the first Self argument
            if let Some(actual_impl_type) = self.infer_impl_type_from_arguments(clause, arguments) {
                println!("🔍 EXACT MATCH: Actual argument impl type: {:?}", actual_impl_type);
                
                // Check if clause implementing type matches actual argument implementing type
                if self.types_match_exactly(&clause_impl_type, &actual_impl_type) {
                    // Now check that all arguments are compatible
                    return self.is_clause_applicable(clause, arguments, impl_type);
                }
            } else {
                // Fallback to typechecker-provided impl_type
                println!("🔍 EXACT MATCH: Using typechecker impl type: {:?}", impl_type);
                if self.types_match_exactly(&clause_impl_type, impl_type) {
                    return self.is_clause_applicable(clause, arguments, impl_type);
                }
            }
        }
        
        Ok(false)
    }

    /// Extract the implementing type from a function clause's source information
    fn extract_clause_implementing_type(
        &self,
        clause: &outrun_typechecker::checker::FunctionClause,
    ) -> Option<outrun_typechecker::unification::StructuredType> {
        // The clause should contain information about which implementation it came from
        // For now, we can try to parse it from the clause_id or function source
        
        // Parse clause ID format: "TraitType:ImplType:function_name:index"
        let parts: Vec<&str> = clause.clause_id.split(':').collect();
        println!("🔍 CLAUSE PARSING: Clause ID parts: {:?}", parts);
        if parts.len() >= 2 {
            let impl_type_name = parts[1];
            
            // Handle generic types like "Outrun.Option.Some<T>" -> "Outrun.Option.Some"
            let base_type_name = if let Some(generic_start) = impl_type_name.find('<') {
                &impl_type_name[..generic_start]
            } else {
                impl_type_name
            };
            
            let impl_type_id = self.compiler_environment.intern_type_name(base_type_name);
            return Some(outrun_typechecker::unification::StructuredType::Simple(impl_type_id));
        }
        
        None
    }

    /// Infer the implementing type from the actual runtime argument values
    /// This looks for the first Self parameter and extracts its concrete type
    fn infer_impl_type_from_arguments(
        &self,
        clause: &outrun_typechecker::checker::FunctionClause,
        arguments: &std::collections::HashMap<String, crate::value::Value>,
    ) -> Option<outrun_typechecker::unification::StructuredType> {
        use outrun_typechecker::unification::StructuredType;
        
        // Find the first Self parameter
        for parameter in &clause.base_function.parameters {
            if let Some(param_type) = &parameter.param_type {
                if let StructuredType::Simple(type_id) = param_type {
                    let type_name = self.compiler_environment.resolve_type_name(type_id).unwrap_or_default();
                    if type_name == "Self" {
                        // Found a Self parameter, extract its actual type from the argument
                        if let Some(arg_value) = arguments.get(&parameter.name) {
                            return self.infer_type_from_argument_value(arg_value);
                        }
                    }
                }
            }
        }
        
        None
    }

    /// Infer the implementing type from a runtime value
    fn infer_type_from_argument_value(
        &self,
        value: &crate::value::Value,
    ) -> Option<outrun_typechecker::unification::StructuredType> {
        use outrun_typechecker::unification::StructuredType;
        
        match value {
            crate::value::Value::Struct { type_id, .. } => {
                // Return the concrete struct type
                Some(StructuredType::Simple(type_id.clone()))
            },
            crate::value::Value::String(_) => {
                let string_type_id = self.compiler_environment.intern_type_name("Outrun.Core.String");
                Some(StructuredType::Simple(string_type_id))
            },
            crate::value::Value::Integer64(_) => {
                let int_type_id = self.compiler_environment.intern_type_name("Outrun.Core.Integer64");
                Some(StructuredType::Simple(int_type_id))
            },
            crate::value::Value::Boolean(_) => {
                let bool_type_id = self.compiler_environment.intern_type_name("Outrun.Core.Boolean");
                Some(StructuredType::Simple(bool_type_id))
            },
            // Add more cases as needed
            _ => None,
        }
    }


    /// Check if two types match exactly (not just compatible)
    fn types_match_exactly(
        &self,
        type1: &outrun_typechecker::unification::StructuredType,
        type2: &outrun_typechecker::unification::StructuredType,
    ) -> bool {
        use outrun_typechecker::unification::StructuredType;
        
        match (type1, type2) {
            (StructuredType::Simple(id1), StructuredType::Simple(id2)) => {
                let name1 = self.compiler_environment.resolve_type_name(id1).unwrap_or_default();
                let name2 = self.compiler_environment.resolve_type_name(id2).unwrap_or_default();
                
                println!("🔍 EXACT TYPE MATCH: '{}' vs '{}'", name1, name2);
                
                // Handle common type name variations
                self.normalize_type_name(&name1) == self.normalize_type_name(&name2)
            },
            (StructuredType::Generic { base: base1, .. }, StructuredType::Generic { base: base2, .. }) => {
                let name1 = self.compiler_environment.resolve_type_name(base1).unwrap_or_default();
                let name2 = self.compiler_environment.resolve_type_name(base2).unwrap_or_default();
                
                // For generic types, compare base types
                self.normalize_type_name(&name1) == self.normalize_type_name(&name2)
            },
            // Handle mixed cases: Simple vs Generic (compare simple type to generic base)
            (StructuredType::Simple(simple_id), StructuredType::Generic { base: generic_base, .. }) => {
                let simple_name = self.compiler_environment.resolve_type_name(simple_id).unwrap_or_default();
                let generic_name = self.compiler_environment.resolve_type_name(generic_base).unwrap_or_default();
                
                println!("🔍 MIXED TYPE MATCH: Simple '{}' vs Generic base '{}'", simple_name, generic_name);
                
                self.normalize_type_name(&simple_name) == self.normalize_type_name(&generic_name)
            },
            (StructuredType::Generic { base: generic_base, .. }, StructuredType::Simple(simple_id)) => {
                let generic_name = self.compiler_environment.resolve_type_name(generic_base).unwrap_or_default();
                let simple_name = self.compiler_environment.resolve_type_name(simple_id).unwrap_or_default();
                
                println!("🔍 MIXED TYPE MATCH: Generic base '{}' vs Simple '{}'", generic_name, simple_name);
                
                self.normalize_type_name(&generic_name) == self.normalize_type_name(&simple_name)
            },
            _ => false,
        }
    }

    /// Normalize type names to handle variations like "Option" vs "Outrun.Option"
    fn normalize_type_name(&self, name: &str) -> String {
        // Remove common prefixes for comparison
        if let Some(short_name) = name.strip_prefix("Outrun.Option.") {
            format!("Option.{}", short_name)
        } else if let Some(short_name) = name.strip_prefix("Outrun.Core.") {
            short_name.to_string()
        } else if let Some(short_name) = name.strip_prefix("Outrun.") {
            short_name.to_string()
        } else {
            name.to_string()
        }
    }

    /// Check if a function clause is applicable based on argument types
    fn is_clause_applicable(
        &self,
        clause: &outrun_typechecker::checker::FunctionClause,
        arguments: &std::collections::HashMap<String, crate::value::Value>,
        impl_type: &outrun_typechecker::unification::StructuredType,
    ) -> Result<bool, DispatchError> {
        println!("🔍 CLAUSE CHECK: Checking clause for function '{}'", clause.base_function.name);
        
        // Check argument type compatibility
        for parameter in &clause.base_function.parameters {
            if let Some(argument_value) = arguments.get(&parameter.name) {
                println!("🔍 CLAUSE CHECK: Checking parameter '{}' with type {:?}", parameter.name, parameter.param_type);
                println!("🔍 CLAUSE CHECK: Argument value: {:?}", argument_value);
                
                // Check if argument type matches parameter type
                if !self.value_matches_type(argument_value, &parameter.param_type, impl_type) {
                    println!("🔍 CLAUSE CHECK: ❌ Parameter '{}' type mismatch - clause rejected", parameter.name);
                    return Ok(false);
                }
                println!("🔍 CLAUSE CHECK: ✅ Parameter '{}' type matches", parameter.name);
            } else {
                println!("🔍 CLAUSE CHECK: ❌ Missing required argument '{}' - clause rejected", parameter.name);
                return Ok(false);
            }
        }
        
        println!("🔍 CLAUSE CHECK: ✅ All parameters match - clause accepted");
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
    /// 
    /// CRITICAL: This function enforces EXACT type matching only. 
    /// NO WORKAROUNDS, NO FALLBACKS, NO PERMISSIVE MATCHING.
    /// If types don't match exactly, the clause is rejected.
    /// Any type compatibility issues must be solved by proper monomorphization
    /// in the typechecker, not by runtime workarounds.
    fn value_matches_type(
        &self,
        value: &crate::value::Value,
        expected_type: &Option<outrun_typechecker::unification::StructuredType>,
        impl_type: &outrun_typechecker::unification::StructuredType,
    ) -> bool {
        use outrun_typechecker::unification::StructuredType;
        
        let Some(expected) = expected_type else {
            // No type constraint means the parameter is untyped - this should not happen
            // in a properly monomorphized system, but we'll allow it for now during development
            println!("🔍 TYPE CHECK: No type constraint - allowing match during development");
            return true;
        };
        
        match (value, expected) {
            // EXACT TYPE MATCHING ONLY - NO WORKAROUNDS
            (crate::value::Value::Integer64(_), StructuredType::Simple(type_id)) => {
                let expected_name = self.compiler_environment.resolve_type_name(type_id).unwrap_or_default();
                println!("🔍 TYPE CHECK: Integer64 value vs Expected type '{}'", expected_name);
                
                // STRICT: Only exact type matches allowed
                expected_name == "Outrun.Core.Integer64"
            },
            
            (crate::value::Value::Boolean(_), StructuredType::Simple(type_id)) => {
                let expected_name = self.compiler_environment.resolve_type_name(type_id).unwrap_or_default();
                println!("🔍 TYPE CHECK: Boolean value vs Expected type '{}'", expected_name);
                
                // STRICT: Only exact type matches allowed
                expected_name == "Outrun.Core.Boolean"
            },
            
            (crate::value::Value::String(_), StructuredType::Simple(expected_type_id)) => {
                let expected_type_name = self.compiler_environment.resolve_type_name(expected_type_id).unwrap_or_default();
                
                println!("🔍 TYPE CHECK: String value vs Expected type '{}'", expected_type_name);
                
                // Handle Self type parameters by checking against the implementing type
                if expected_type_name == "Self" {
                    match impl_type {
                        StructuredType::Simple(impl_type_id) => {
                            let impl_type_name = self.compiler_environment.resolve_type_name(impl_type_id).unwrap_or_default();
                            println!("🔍 SELF TYPE CHECK: Checking String value against implementing type '{}'", impl_type_name);
                            
                            // STRICT: Self must resolve to exact concrete type
                            impl_type_name == "Outrun.Core.String"
                        },
                        _ => {
                            println!("🔍 SELF TYPE CHECK: Complex implementing type not supported");
                            false // NO FALLBACKS - reject complex Self types
                        }
                    }
                } else {
                    // STRICT: Only exact type matches allowed
                    expected_type_name == "Outrun.Core.String"
                }
            },
            
            (crate::value::Value::Struct { type_id: value_type_id, .. }, StructuredType::Simple(expected_type_id)) => {
                let value_type_name = self.compiler_environment.resolve_type_name(value_type_id).unwrap_or_default();
                let expected_type_name = self.compiler_environment.resolve_type_name(expected_type_id).unwrap_or_default();
                
                println!("🔍 TYPE CHECK: Value type '{}' vs Expected type '{}'", value_type_name, expected_type_name);
                
                // Handle Self type parameters
                if expected_type_name == "Self" {
                    match impl_type {
                        StructuredType::Simple(impl_type_id) => {
                            let impl_type_name = self.compiler_environment.resolve_type_name(impl_type_id).unwrap_or_default();
                            println!("🔍 SELF TYPE CHECK: Checking value type '{}' against implementing type '{}'", value_type_name, impl_type_name);
                            
                            // STRICT: Self must resolve to exact type
                            value_type_name == impl_type_name
                        },
                        _ => {
                            println!("🔍 SELF TYPE CHECK: Complex implementing type not supported");
                            false // NO FALLBACKS
                        }
                    }
                } else {
                    // STRICT: Only exact type matches allowed
                    value_type_name == expected_type_name
                }
            },
            
            // NO FALLBACK CASES - reject all unhandled combinations
            _ => {
                println!("🔍 TYPE CHECK: Unhandled value/type combination - REJECTING");
                println!("🔍 TYPE CHECK: Value: {:?}", value);
                println!("🔍 TYPE CHECK: Expected: {:?}", expected);
                false // NO FALLBACKS - strict type checking only
            }
        }
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
            crate::value::Value::Struct { type_id, .. } => {
                // For structs, use the exact type stored in the value
                Some(StructuredType::Simple(type_id.clone()))
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
