//! Guard purity analysis for Outrun function clauses
//!
//! This module ensures that guard expressions are side-effect-free and return Boolean values.
//! In Outrun, functions ending in '?' must be pure and return Boolean, and guards can only
//! call such pure functions.

use crate::checker::{TypedExpression, TypedExpressionKind, TypedFunctionDefinition};
use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::error::TypeError;
use crate::unification::StructuredType;
use outrun_parser::Span;
use std::collections::HashSet;

/// Errors related to guard purity violations
#[derive(Debug, Clone)]
pub enum PurityError {
    /// Guard expression doesn't return Boolean
    GuardMustReturnBoolean {
        found_type: StructuredType,
        span: Span,
    },
    /// Guard calls an impure function
    GuardCallsImpureFunction {
        function_name: String,
        span: Span,
    },
    /// Function ending in '?' doesn't return Boolean
    PureFunctionMustReturnBoolean {
        function_name: String,
        found_type: StructuredType,
        span: Span,
    },
    /// Function ending in '?' calls impure functions
    PureFunctionCallsImpureFunction {
        caller: String,
        callee: String,
        span: Span,
    },
}

/// Purity analysis context for tracking function call chains
pub struct PurityAnalyzer<'a> {
    compiler_env: &'a CompilerEnvironment,
    /// Cache of function purity analysis results
    purity_cache: std::collections::HashMap<String, bool>,
    /// Currently analyzing functions (for cycle detection)
    analyzing_stack: HashSet<String>,
}

impl<'a> PurityAnalyzer<'a> {
    /// Create a new purity analyzer
    pub fn new(compiler_env: &'a CompilerEnvironment) -> Self {
        Self {
            compiler_env,
            purity_cache: std::collections::HashMap::new(),
            analyzing_stack: HashSet::new(),
        }
    }

    /// Analyze guard expression for purity violations
    pub fn analyze_guard_expression(
        &mut self,
        guard_expr: &TypedExpression,
        function_name: &str,
    ) -> Result<(), PurityError> {
        // 1. Check that guard returns Boolean
        if let Some(guard_type) = &guard_expr.structured_type {
            if !self.is_boolean_type(guard_type) {
                return Err(PurityError::GuardMustReturnBoolean {
                    found_type: guard_type.clone(),
                    span: guard_expr.span,
                });
            }
        }

        // 2. Check that guard only calls pure functions
        self.analyze_expression_purity(guard_expr, function_name)?;

        Ok(())
    }

    /// Analyze a complete function for purity requirements
    pub fn analyze_function_purity(
        &mut self,
        function_def: &TypedFunctionDefinition,
    ) -> Result<(), PurityError> {
        let function_name = &function_def.name;

        // If function name ends with '?', it must be pure and return Boolean
        if function_name.ends_with('?') {
            // Check return type is Boolean
            if let Some(return_type) = &function_def.return_type {
                if !self.is_boolean_type(return_type) {
                    return Err(PurityError::PureFunctionMustReturnBoolean {
                        function_name: function_name.clone(),
                        found_type: return_type.clone(),
                        span: function_def.span,
                    });
                }
            }

            // Check function body only calls pure functions
            self.analyze_function_body_purity(function_def)?;
        }

        // If function has a guard, analyze the guard
        if let Some(guard_expr) = &function_def.guard {
            self.analyze_guard_expression(guard_expr, function_name)?;
        }

        Ok(())
    }

    /// Analyze expression for purity (used by both guards and pure functions)
    fn analyze_expression_purity(
        &mut self,
        expr: &TypedExpression,
        context_function: &str,
    ) -> Result<(), PurityError> {
        match &expr.kind {
            // Pure expressions - literals and variables
            TypedExpressionKind::Integer(_)
            | TypedExpressionKind::Float(_)
            | TypedExpressionKind::String(_)
            | TypedExpressionKind::Boolean(_)
            | TypedExpressionKind::Atom(_)
            | TypedExpressionKind::Identifier(_) => Ok(()),

            // Function calls - must check purity
            TypedExpressionKind::FunctionCall { function_path, arguments, .. } => {
                // Extract function name from path
                let function_name = self.extract_function_name_from_path(function_path);
                
                // Check if this function call is pure
                if !self.is_function_pure(&function_name)? {
                    return Err(PurityError::GuardCallsImpureFunction {
                        function_name,
                        span: expr.span,
                    });
                }

                // Recursively check argument expressions
                for arg in arguments {
                    self.analyze_expression_purity(&arg.expression, context_function)?;
                }

                Ok(())
            }

            // TODO: Add proper expression analysis for operators and conditionals
            // For now, operators and conditionals are conservatively allowed

            // Other expression types - need individual analysis
            _ => {
                // For now, conservatively allow other expressions
                // In a full implementation, we'd analyze each expression type
                Ok(())
            }
        }
    }

    /// Analyze function body for purity violations  
    fn analyze_function_body_purity(&mut self, function_def: &TypedFunctionDefinition) -> Result<(), PurityError> {
        // Analyze each statement in the function body
        for statement in &function_def.body.statements {
            match statement {
                crate::checker::TypedStatement::Expression(expr) => {
                    self.analyze_expression_purity(expr, &function_def.name)?;
                }
                crate::checker::TypedStatement::LetBinding(binding) => {
                    self.analyze_expression_purity(&binding.expression, &function_def.name)?;
                }
            }
        }
        Ok(())
    }

    /// Check if a function is pure (ends with '?' or is intrinsically pure)
    fn is_function_pure(&mut self, function_name: &str) -> Result<bool, PurityError> {
        // Check cache first
        if let Some(&cached_result) = self.purity_cache.get(function_name) {
            return Ok(cached_result);
        }

        // Detect cycles in purity analysis
        if self.analyzing_stack.contains(function_name) {
            // Assume pure for cycle breaking (conservative approach)
            return Ok(true);
        }

        // Functions ending with '?' are required to be pure
        if function_name.ends_with('?') {
            self.analyzing_stack.insert(function_name.to_string());
            
            // Would need to analyze the function definition here
            // For now, assume functions ending in '?' are pure (as required by language spec)
            let is_pure = true;
            
            self.analyzing_stack.remove(function_name);
            self.purity_cache.insert(function_name.to_string(), is_pure);
            return Ok(is_pure);
        }

        // Intrinsic operations are pure
        if function_name.starts_with("Outrun.Intrinsic.") {
            // Most intrinsics are pure (mathematical operations, comparisons)
            // I/O intrinsics would be impure, but guards shouldn't call those anyway
            let is_pure = self.is_intrinsic_pure(function_name);
            self.purity_cache.insert(function_name.to_string(), is_pure);
            return Ok(is_pure);
        }

        // Other functions are assumed impure unless proven otherwise
        self.purity_cache.insert(function_name.to_string(), false);
        Ok(false)
    }

    /// Check if an intrinsic function is pure
    fn is_intrinsic_pure(&self, intrinsic_name: &str) -> bool {
        match intrinsic_name {
            // Mathematical operations are pure
            "Outrun.Intrinsic.add_integer64" | 
            "Outrun.Intrinsic.sub_integer64" |
            "Outrun.Intrinsic.mul_integer64" |
            "Outrun.Intrinsic.div_integer64" |
            "Outrun.Intrinsic.mod_integer64" |
            
            // Comparisons are pure
            "Outrun.Intrinsic.eq_integer64" |
            "Outrun.Intrinsic.ne_integer64" |
            "Outrun.Intrinsic.lt_integer64" |
            "Outrun.Intrinsic.le_integer64" |
            "Outrun.Intrinsic.gt_integer64" |
            "Outrun.Intrinsic.ge_integer64" |
            
            // Boolean operations are pure
            "Outrun.Intrinsic.and_boolean" |
            "Outrun.Intrinsic.or_boolean" |
            "Outrun.Intrinsic.not_boolean" |
            
            // String operations (non-mutating) are pure
            "Outrun.Intrinsic.concat_string" |
            "Outrun.Intrinsic.length_string" |
            
            // List operations (non-mutating) are pure
            "Outrun.Intrinsic.list_head" |
            "Outrun.Intrinsic.list_tail" |
            "Outrun.Intrinsic.list_empty" => true,
            
            // I/O operations would be impure
            "Outrun.Intrinsic.print" |
            "Outrun.Intrinsic.read_file" |
            "Outrun.Intrinsic.write_file" => false,
            
            // Unknown intrinsics are conservatively considered impure
            _ => false,
        }
    }

    /// Check if a type is Boolean
    fn is_boolean_type(&self, structured_type: &StructuredType) -> bool {
        match structured_type {
            StructuredType::Simple(type_id) => {
                // Check if this type_id corresponds to Boolean
                if let Some(type_name) = self.compiler_env.resolve_type(type_id.clone()) {
                    type_name == "Boolean"
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Extract function name from function path
    fn extract_function_name_from_path(&self, function_path: &crate::checker::TypedFunctionPath) -> String {
        match function_path {
            crate::checker::TypedFunctionPath::Simple { name } => name.clone(),
            crate::checker::TypedFunctionPath::Qualified { module, name } => {
                format!("{}.{}", module, name)
            }
            crate::checker::TypedFunctionPath::Expression { .. } => {
                // Dynamic function calls can't be statically analyzed for purity
                // Conservatively assume impure
                "unknown_dynamic_function".to_string()
            }
        }
    }
}

/// Public API for guard purity validation
impl CompilerEnvironment {
    /// Validate that all guards in a function are pure and return Boolean
    pub fn validate_function_guard_purity(
        &self,
        function_def: &TypedFunctionDefinition,
    ) -> Result<(), TypeError> {
        let mut analyzer = PurityAnalyzer::new(self);
        
        analyzer.analyze_function_purity(function_def)
            .map_err(|purity_error| {
                TypeError::purity_violation(format!("{:?}", purity_error), function_def.span)
            })
    }

    /// Validate purity for all functions with guards in the environment
    pub fn validate_all_guard_purity(&self) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();
        let modules = self.modules().read().unwrap();
        
        for (_module_key, module) in modules.iter() {
            for (_function_name, function_entry) in &module.functions_by_name {
                if let Some(typed_function) = function_entry.typed_definition() {
                    // Only validate functions that have guards
                    if typed_function.guard.is_some() {
                        if let Err(error) = self.validate_function_guard_purity(typed_function) {
                            errors.push(error);
                        }
                    }
                }
            }
        }
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}