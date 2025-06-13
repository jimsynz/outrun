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
    pub functions: HashMap<String, FunctionSignature>,
    pub is_function_scope: bool, // True for function parameter scopes
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

    /// Register a function in the current scope
    pub fn register_function(&mut self, function: FunctionSignature) -> Result<(), TypeError> {
        if let Some(scope) = self.current_scope_mut() {
            if scope.functions.contains_key(&function.name) {
                return Err(TypeError::FunctionAlreadyDefined {
                    name: function.name.clone(),
                    span: crate::error::span_to_source_span(function.span),
                    previous_span: crate::error::span_to_source_span(
                        scope.functions[&function.name].span,
                    ),
                });
            }
            scope.functions.insert(function.name.clone(), function);
            Ok(())
        } else {
            Err(TypeError::NoCurrentScope {
                span: crate::error::span_to_source_span(function.span),
            })
        }
    }

    /// Look up a function by name, searching through scope stack
    pub fn lookup_function(&self, name: &str) -> Option<&FunctionSignature> {
        for scope in self.scopes.iter().rev() {
            if let Some(func) = scope.functions.get(name) {
                return Some(func);
            }
        }
        None
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
