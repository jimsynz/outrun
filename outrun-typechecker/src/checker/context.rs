//! Type checking context and scope management
//!
//! This module manages the type checking context including:
//! - Variable and function scope tracking
//! - Type and trait registration
//! - Symbol resolution with proper scoping rules

use crate::dispatch::DispatchTable;
use crate::error::TypeError;
use crate::types::traits::{FunctionId, TraitRegistry};
use crate::types::{AtomId, ConcreteType, TraitId, TypeId, TypeInterner};
use outrun_parser::Span;
use std::collections::HashMap;

/// Main type checking context that tracks all type system state
#[derive(Debug, Clone)]
pub struct TypeContext {
    // Core type system components
    pub interner: TypeInterner,
    pub concrete_types: HashMap<TypeId, ConcreteType>,
    pub trait_registry: TraitRegistry,

    // Scope management
    pub scopes: Vec<Scope>,
    pub current_function: Option<FunctionId>,

    // Dispatch system
    pub dispatch_table: DispatchTable,

    // Error accumulation
    pub errors: Vec<TypeError>,
}

/// A single scope containing variables and local function definitions
#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: HashMap<String, Variable>,
    pub functions: HashMap<String, Vec<FunctionSignature>>, // Support multiple overloaded functions
    pub is_function_scope: bool,                            // True for function parameter scopes
}

/// Variable information in a scope
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub type_id: TypeId,
    pub is_mutable: bool, // Always false in Outrun (immutable language)
    pub span: Span,
}

/// Function signature for scope resolution
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub params: Vec<(AtomId, TypeId)>,
    pub return_type: TypeId,
    pub is_guard: bool,
    pub guard_clause: Option<TypedGuardClause>, // For function overloading
    pub span: Span,
}

/// Typed guard clause for function overloading
#[derive(Debug, Clone, PartialEq)]
pub struct TypedGuardClause {
    pub condition: crate::checker::TypedExpression,
    pub span: Span,
}

impl TypeContext {
    /// Create a new type context with built-in types
    pub fn new() -> Self {
        let mut context = Self {
            interner: TypeInterner::new(),
            concrete_types: HashMap::new(),
            trait_registry: TraitRegistry::new(),
            scopes: Vec::new(),
            current_function: None,
            dispatch_table: DispatchTable::new(),
            errors: Vec::new(),
        };

        context.register_builtin_types();
        context.push_scope(false); // Global scope
        context
    }

    /// Register built-in types (Outrun.Core.Integer64, etc.)
    fn register_builtin_types(&mut self) {
        let builtin_types = [
            ("Outrun.Core.Integer64", ConcreteType::Integer64),
            ("Outrun.Core.Float64", ConcreteType::Float64),
            ("Outrun.Core.Boolean", ConcreteType::Boolean),
            ("Outrun.Core.String", ConcreteType::String),
            ("Outrun.Core.Atom", ConcreteType::Atom),
        ];

        for (name, concrete_type) in builtin_types {
            let type_id = self.interner.intern_type(name);
            self.concrete_types.insert(type_id, concrete_type);
        }
    }

    /// Push a new scope onto the scope stack
    pub fn push_scope(&mut self, is_function_scope: bool) {
        self.scopes.push(Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            is_function_scope,
        });
    }

    /// Pop the current scope from the scope stack
    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    /// Get the current scope (top of stack)
    pub fn current_scope(&self) -> Option<&Scope> {
        self.scopes.last()
    }

    /// Get the current scope mutably
    pub fn current_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }

    /// Register a variable in the current scope
    pub fn register_variable(&mut self, variable: Variable) -> Result<(), TypeError> {
        if let Some(scope) = self.current_scope_mut() {
            if scope.variables.contains_key(&variable.name) {
                return Err(TypeError::VariableAlreadyDefined {
                    name: variable.name.clone(),
                    span: crate::error::span_to_source_span(variable.span),
                    previous_span: crate::error::span_to_source_span(
                        scope.variables[&variable.name].span,
                    ),
                });
            }
            scope.variables.insert(variable.name.clone(), variable);
            Ok(())
        } else {
            Err(TypeError::NoCurrentScope {
                span: crate::error::span_to_source_span(variable.span),
            })
        }
    }

    /// Look up a variable by name, searching through scope stack
    pub fn lookup_variable(&self, name: &str) -> Option<&Variable> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.variables.get(name) {
                return Some(var);
            }
        }
        None
    }

    /// Register a function in the current scope (supports overloading)
    pub fn register_function(&mut self, function: FunctionSignature) -> Result<(), TypeError> {
        // First, check for conflicting overloads without holding mutable reference
        let existing_functions = self
            .scopes
            .last()
            .and_then(|scope| scope.functions.get(&function.name))
            .cloned();

        if let Some(existing_functions) = existing_functions {
            for existing in &existing_functions {
                // Check if this is a valid overload
                if self.is_conflicting_overload(&function, existing)? {
                    return Err(TypeError::ConflictingFunctionOverload {
                        name: function.name.clone(),
                        span: crate::error::span_to_source_span(function.span),
                        previous_span: crate::error::span_to_source_span(existing.span),
                    });
                }
            }
        }

        // Now add the function to the overload list
        if let Some(scope) = self.current_scope_mut() {
            scope
                .functions
                .entry(function.name.clone())
                .or_insert_with(Vec::new)
                .push(function);
            Ok(())
        } else {
            Err(TypeError::NoCurrentScope {
                span: crate::error::span_to_source_span(function.span),
            })
        }
    }

    /// Look up a function by name, searching through scope stack
    /// Returns the first overload if multiple exist (for backward compatibility)
    pub fn lookup_function(&self, name: &str) -> Option<&FunctionSignature> {
        for scope in self.scopes.iter().rev() {
            if let Some(funcs) = scope.functions.get(name) {
                if let Some(first_func) = funcs.first() {
                    return Some(first_func);
                }
            }
        }
        None
    }

    /// Look up all function overloads by name, searching through scope stack
    pub fn lookup_function_overloads(&self, name: &str) -> Vec<&FunctionSignature> {
        for scope in self.scopes.iter().rev() {
            if let Some(funcs) = scope.functions.get(name) {
                return funcs.iter().collect();
            }
        }
        Vec::new()
    }

    /// Check if two function signatures represent conflicting overloads
    fn is_conflicting_overload(
        &self,
        new_func: &FunctionSignature,
        existing_func: &FunctionSignature,
    ) -> Result<bool, TypeError> {
        // Functions with different parameter signatures can coexist
        if new_func.params != existing_func.params {
            return Ok(false);
        }

        // Functions with same parameter signature must have distinguishable guard conditions
        // to avoid conflicts at call time
        match (&new_func.guard_clause, &existing_func.guard_clause) {
            (None, None) => {
                // Two functions with identical signatures and no guards - this is always conflicting
                Ok(true)
            }
            (Some(_), None) | (None, Some(_)) => {
                // One has guard, one doesn't - this is allowed
                // The one without guard acts as the default case
                Ok(false)
            }
            (Some(new_guard), Some(existing_guard)) => {
                // Both have guards - check if they're equivalent (conflicting)
                // For now, assume different guard expressions are non-conflicting
                // TODO: Implement sophisticated guard analysis to detect overlapping conditions
                Ok(self.are_guards_equivalent(&new_guard.condition, &existing_guard.condition))
            }
        }
    }

    /// Check if two guard expressions are equivalent (simplified implementation)
    fn are_guards_equivalent(
        &self,
        _guard1: &crate::checker::TypedExpression,
        _guard2: &crate::checker::TypedExpression,
    ) -> bool {
        // TODO: Implement sophisticated guard expression comparison
        // For now, assume all different guard expressions are non-conflicting
        false
    }

    /// Resolve function call to the appropriate overload based on arguments
    /// This is the core function overloading resolution algorithm
    pub fn resolve_function_overload(
        &self,
        function_name: &str,
        _args: &[(String, crate::checker::TypedExpression)], // For future sophisticated resolution
    ) -> Option<&FunctionSignature> {
        let overloads = self.lookup_function_overloads(function_name);

        if overloads.is_empty() {
            return None;
        }

        // For now, use simple resolution strategy:
        // 1. If there's only one overload, use it
        // 2. If multiple overloads, prefer the one without a guard clause
        // 3. TODO: Implement sophisticated guard evaluation and argument matching

        if overloads.len() == 1 {
            return overloads.first().copied();
        }

        // Find overload without guard clause first (default case)
        for overload in &overloads {
            if overload.guard_clause.is_none() {
                return Some(overload);
            }
        }

        // If all have guard clauses, return the first one
        // TODO: Implement guard evaluation based on arguments
        overloads.first().copied()
    }

    /// Get a concrete type by TypeId
    pub fn get_concrete_type(&self, type_id: TypeId) -> Option<&ConcreteType> {
        self.concrete_types.get(&type_id)
    }

    /// Register a new concrete type
    pub fn register_concrete_type(&mut self, type_id: TypeId, concrete_type: ConcreteType) {
        self.concrete_types.insert(type_id, concrete_type);
    }

    /// Get type name by TypeId
    pub fn get_type_name(&self, type_id: TypeId) -> Option<&str> {
        self.interner.resolve_type(type_id)
    }

    /// Check if a type implements a trait
    pub fn implements_trait(&self, type_id: TypeId, trait_id: TraitId) -> bool {
        self.trait_registry.implements_trait(type_id, trait_id)
    }

    /// Add a type error to the accumulator
    pub fn add_error(&mut self, error: TypeError) {
        self.errors.push(error);
    }

    /// Check if there are any accumulated errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get all accumulated errors
    pub fn take_errors(&mut self) -> Vec<TypeError> {
        std::mem::take(&mut self.errors)
    }

    /// Get the number of currently open scopes
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }

    /// Check if we're currently in a function scope
    pub fn in_function_scope(&self) -> bool {
        self.scopes.iter().any(|scope| scope.is_function_scope)
    }
}

impl Scope {
    /// Create a new empty scope
    pub fn new(is_function_scope: bool) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            is_function_scope,
        }
    }

    /// Check if this scope contains a variable
    pub fn has_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Check if this scope contains a function
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Get the number of variables in this scope
    pub fn variable_count(&self) -> usize {
        self.variables.len()
    }

    /// Get the number of functions in this scope
    pub fn function_count(&self) -> usize {
        self.functions.len()
    }
}

impl Variable {
    /// Create a new variable
    pub fn new(name: String, type_id: TypeId, span: Span) -> Self {
        Self {
            name,
            type_id,
            is_mutable: false, // Outrun is immutable
            span,
        }
    }
}

impl FunctionSignature {
    /// Create a new function signature
    pub fn new(
        name: String,
        params: Vec<(AtomId, TypeId)>,
        return_type: TypeId,
        is_guard: bool,
        span: Span,
    ) -> Self {
        Self {
            name,
            params,
            return_type,
            is_guard,
            guard_clause: None, // No guard clause for this constructor
            span,
        }
    }

    /// Get the arity (number of parameters) of this function
    pub fn arity(&self) -> usize {
        self.params.len()
    }

    /// Check if this function has unique parameter names
    pub fn has_unique_params(&self) -> bool {
        let mut seen = std::collections::HashSet::new();
        self.params.iter().all(|(name, _)| seen.insert(*name))
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_creation() {
        let context = TypeContext::new();
        assert_eq!(context.scope_depth(), 1); // Global scope
        assert!(!context.has_errors());
        assert!(!context.in_function_scope());

        // Built-in types should be registered
        assert!(context.interner.get_type("Outrun.Core.Integer64").is_some());
        assert!(context.interner.get_type("Outrun.Core.String").is_some());
        assert!(context.interner.get_type("Outrun.Core.Boolean").is_some());
    }

    #[test]
    fn test_scope_management() {
        let mut context = TypeContext::new();

        assert_eq!(context.scope_depth(), 1);

        context.push_scope(true); // Function scope
        assert_eq!(context.scope_depth(), 2);
        assert!(context.in_function_scope());

        let popped = context.pop_scope();
        assert!(popped.is_some());
        assert!(popped.unwrap().is_function_scope);
        assert_eq!(context.scope_depth(), 1);
        assert!(!context.in_function_scope());
    }

    #[test]
    fn test_variable_registration() {
        let mut context = TypeContext::new();
        let type_id = context.interner.intern_type("Integer");

        let var = Variable::new("x".to_string(), type_id, Span::new(0, 1));
        assert!(context.register_variable(var).is_ok());

        // Variable should be findable
        let found = context.lookup_variable("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, "x");
        assert_eq!(found.unwrap().type_id, type_id);
    }

    #[test]
    fn test_variable_shadowing() {
        let mut context = TypeContext::new();
        let int_type = context.interner.intern_type("Integer");
        let str_type = context.interner.intern_type("String");

        // Register variable in outer scope
        let var1 = Variable::new("x".to_string(), int_type, Span::new(0, 1));
        assert!(context.register_variable(var1).is_ok());

        // Push new scope and shadow the variable
        context.push_scope(false);
        let var2 = Variable::new("x".to_string(), str_type, Span::new(10, 11));
        assert!(context.register_variable(var2).is_ok());

        // Should find the shadowed variable
        let found = context.lookup_variable("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().type_id, str_type);

        // Pop scope and should find original variable
        context.pop_scope();
        let found = context.lookup_variable("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().type_id, int_type);
    }

    #[test]
    fn test_duplicate_variable_error() {
        let mut context = TypeContext::new();
        let type_id = context.interner.intern_type("Integer");

        let var1 = Variable::new("x".to_string(), type_id, Span::new(0, 1));
        assert!(context.register_variable(var1).is_ok());

        let var2 = Variable::new("x".to_string(), type_id, Span::new(10, 11));
        let result = context.register_variable(var2);

        assert!(result.is_err());
        match result.unwrap_err() {
            TypeError::VariableAlreadyDefined { name, .. } => {
                assert_eq!(name, "x");
            }
            _ => panic!("Expected VariableAlreadyDefined error"),
        }
    }

    #[test]
    fn test_function_registration() {
        let mut context = TypeContext::new();
        let param_name = context.interner.intern_atom("x");
        let int_type = context.interner.intern_type("Integer");

        let func = FunctionSignature::new(
            "add".to_string(),
            vec![(param_name, int_type)],
            int_type,
            false,
            Span::new(0, 10),
        );

        assert!(context.register_function(func).is_ok());

        let found = context.lookup_function("add");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, "add");
        assert_eq!(found.unwrap().arity(), 1);
    }

    #[test]
    fn test_concrete_type_operations() {
        let mut context = TypeContext::new();
        let type_id = context.interner.intern_type("CustomType");
        let concrete = ConcreteType::String;

        context.register_concrete_type(type_id, concrete.clone());

        let retrieved = context.get_concrete_type(type_id);
        assert_eq!(retrieved, Some(&concrete));

        let type_name = context.get_type_name(type_id);
        assert_eq!(type_name, Some("CustomType"));
    }
}
