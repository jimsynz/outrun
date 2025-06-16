//! Type checking context and scope management
//!
//! This module manages the type checking context including:
//! - Variable and function scope tracking
//! - Type and trait registration
//! - Symbol resolution with proper scoping rules

use crate::dispatch::DispatchTable;
use crate::error::{SpanExt, TypeError};
use crate::types::traits::{FunctionId, TraitRegistry};
use crate::types::{AtomId, ConcreteType, IntrospectionRegistry, TraitId, TypeId, TypeInterner};
use outrun_parser::Span;
use std::collections::HashMap;

/// Built-in type name constants
pub mod type_names {
    pub const BOOLEAN: &str = "Outrun.Core.Boolean";
    pub const STRING: &str = "Outrun.Core.String";
    pub const INTEGER64: &str = "Outrun.Core.Integer64";
    pub const FLOAT64: &str = "Outrun.Core.Float64";
    pub const ATOM: &str = "Outrun.Core.Atom";
    pub const LIST: &str = "Outrun.Core.List";
    pub const MAP: &str = "Outrun.Core.Map";
    pub const TUPLE: &str = "Outrun.Core.Tuple";
    pub const OPTION: &str = "Outrun.Core.Option";
    pub const RESULT: &str = "Outrun.Core.Result";
    pub const UNKNOWN: &str = "Unknown";
}

/// Struct definition registry entry
#[derive(Debug, Clone)]
pub struct StructDefinitionInfo {
    pub name: String,
    pub generic_param_count: usize,
    pub span: Span,
}

/// Main type checking context that tracks all type system state
#[derive(Debug, Clone)]
pub struct TypeContext {
    // Core type system components
    pub interner: TypeInterner,
    pub concrete_types: HashMap<TypeId, ConcreteType>,
    pub trait_registry: TraitRegistry,
    pub introspection_registry: IntrospectionRegistry,
    pub struct_registry: HashMap<String, StructDefinitionInfo>,

    // Scope management
    pub scopes: Vec<Scope>,
    pub current_function: Option<FunctionId>,

    // Dispatch system
    pub dispatch_table: DispatchTable,

    // Error accumulation
    pub errors: Vec<TypeError>,
}

/// A single scope containing variables, functions, and generic type parameters
#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: HashMap<String, Variable>,
    pub functions: HashMap<String, Vec<FunctionSignature>>, // Support multiple overloaded functions
    pub generic_params: HashMap<String, TypeId>, // Generic type parameters in scope (T, U, etc.)
    pub is_function_scope: bool,                 // True for function parameter scopes
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
            introspection_registry: IntrospectionRegistry::new(),
            struct_registry: HashMap::new(),
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
            (type_names::INTEGER64, ConcreteType::Integer64),
            (type_names::FLOAT64, ConcreteType::Float64),
            (type_names::BOOLEAN, ConcreteType::Boolean),
            (type_names::STRING, ConcreteType::String),
            (type_names::ATOM, ConcreteType::Atom),
        ];

        for (name, concrete_type) in builtin_types {
            let type_id = self.interner.intern_type(name);
            self.concrete_types.insert(type_id, concrete_type);
        }

        // Register generic collection base types
        // These are registered without specific generic arguments so they can be used
        // as base types for generic type resolution
        let collection_base_types = [
            type_names::LIST,
            type_names::MAP,
            type_names::TUPLE,
            type_names::OPTION,
            type_names::RESULT,
        ];

        for name in collection_base_types {
            let type_id = self.interner.intern_type(name);
            // For collection types, we register them as generic placeholders
            // The actual concrete type with specific generic arguments will be
            // resolved during type checking
            self.concrete_types.insert(type_id, ConcreteType::String); // Placeholder
        }
    }

    /// Push a new scope onto the scope stack
    pub fn push_scope(&mut self, is_function_scope: bool) {
        self.scopes.push(Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            generic_params: HashMap::new(),
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

    /// Check if two guard expressions are equivalent (basic implementation)
    fn are_guards_equivalent(
        &self,
        guard1: &crate::checker::TypedExpression,
        guard2: &crate::checker::TypedExpression,
    ) -> bool {
        // Basic implementation: check if the expressions are structurally equivalent
        // This handles simple cases like identical expressions
        Self::expressions_are_equivalent(guard1, guard2)
    }

    /// Helper to check if two typed expressions are structurally equivalent
    fn expressions_are_equivalent(
        expr1: &crate::checker::TypedExpression,
        expr2: &crate::checker::TypedExpression,
    ) -> bool {
        // Check type compatibility first
        if expr1.type_id != expr2.type_id {
            return false;
        }

        // Check structural equivalence of expression kinds
        match (&expr1.kind, &expr2.kind) {
            (
                crate::checker::TypedExpressionKind::Integer(a),
                crate::checker::TypedExpressionKind::Integer(b),
            ) => a == b,
            (
                crate::checker::TypedExpressionKind::Float(a),
                crate::checker::TypedExpressionKind::Float(b),
            ) => a == b,
            (
                crate::checker::TypedExpressionKind::String(a),
                crate::checker::TypedExpressionKind::String(b),
            ) => a == b,
            (
                crate::checker::TypedExpressionKind::Boolean(a),
                crate::checker::TypedExpressionKind::Boolean(b),
            ) => a == b,
            (
                crate::checker::TypedExpressionKind::Atom(a),
                crate::checker::TypedExpressionKind::Atom(b),
            ) => a == b,
            (
                crate::checker::TypedExpressionKind::Identifier(a),
                crate::checker::TypedExpressionKind::Identifier(b),
            ) => a == b,
            (
                crate::checker::TypedExpressionKind::BinaryOp {
                    left: l1,
                    operator: op1,
                    right: r1,
                },
                crate::checker::TypedExpressionKind::BinaryOp {
                    left: l2,
                    operator: op2,
                    right: r2,
                },
            ) => {
                op1 == op2
                    && Self::expressions_are_equivalent(l1, l2)
                    && Self::expressions_are_equivalent(r1, r2)
            }
            // For now, other expression types are considered non-equivalent
            // TODO: Implement equivalence for more complex expressions as needed
            _ => false,
        }
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

    /// Get type name by TypeId, returning "Unknown" if not found
    pub fn type_name_or_unknown(&self, type_id: TypeId) -> String {
        self.get_type_name(type_id)
            .unwrap_or(type_names::UNKNOWN)
            .to_string()
    }

    /// Create a type mismatch error using TypeIds
    pub fn type_mismatch_error(
        &self,
        expected: TypeId,
        found: TypeId,
        span: outrun_parser::Span,
    ) -> TypeError {
        TypeError::type_mismatch(
            self.type_name_or_unknown(expected),
            self.type_name_or_unknown(found),
            span.to_source_span(),
        )
    }

    /// Create a trait not implemented error using TypeId and trait name
    pub fn trait_not_implemented_error(
        &self,
        trait_name: String,
        type_id: TypeId,
        span: outrun_parser::Span,
    ) -> TypeError {
        TypeError::trait_not_implemented(
            trait_name,
            self.type_name_or_unknown(type_id),
            span.to_source_span(),
        )
    }

    /// Get the TypeId for boolean type
    pub fn boolean_type(&mut self) -> TypeId {
        self.interner.intern_type(type_names::BOOLEAN)
    }

    /// Get the TypeId for string type
    pub fn string_type(&mut self) -> TypeId {
        self.interner.intern_type(type_names::STRING)
    }

    /// Get the TypeId for integer type
    pub fn integer_type(&mut self) -> TypeId {
        self.interner.intern_type(type_names::INTEGER64)
    }

    /// Get the TypeId for float type
    pub fn float_type(&mut self) -> TypeId {
        self.interner.intern_type(type_names::FLOAT64)
    }

    /// Get the TypeId for atom type
    pub fn atom_type(&mut self) -> TypeId {
        self.interner.intern_type(type_names::ATOM)
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

    /// Register a generic type parameter in the current scope
    pub fn register_generic_param(
        &mut self,
        name: String,
        type_id: TypeId,
    ) -> Result<(), TypeError> {
        if let Some(scope) = self.current_scope_mut() {
            if scope.generic_params.contains_key(&name) {
                return Err(TypeError::GenericParameterAlreadyDefined {
                    name: name.clone(),
                    span: crate::error::span_to_source_span(outrun_parser::Span::new(0, 0)), // TODO: Add proper span tracking
                });
            }
            scope.generic_params.insert(name, type_id);
            Ok(())
        } else {
            Err(TypeError::NoCurrentScope {
                span: crate::error::span_to_source_span(outrun_parser::Span::new(0, 0)),
            })
        }
    }

    /// Look up a generic type parameter by name, searching through scope stack
    pub fn lookup_generic_param(&self, name: &str) -> Option<TypeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&type_id) = scope.generic_params.get(name) {
                return Some(type_id);
            }
        }
        None
    }

    /// Check if a name refers to a generic type parameter in the current context
    pub fn is_generic_param(&self, name: &str) -> bool {
        self.lookup_generic_param(name).is_some()
    }

    /// Get all generic parameters currently in scope
    pub fn current_generic_params(&self) -> Vec<(String, TypeId)> {
        let mut params = Vec::new();
        for scope in &self.scopes {
            for (name, type_id) in &scope.generic_params {
                params.push((name.clone(), *type_id));
            }
        }
        params
    }

    /// Register a struct definition with its generic parameter count
    pub fn register_struct_definition(
        &mut self,
        name: String,
        generic_param_count: usize,
        span: Span,
    ) {
        let info = StructDefinitionInfo {
            name: name.clone(),
            generic_param_count,
            span,
        };
        self.struct_registry.insert(name, info);
    }

    /// Get struct definition info by name
    pub fn get_struct_definition(&self, name: &str) -> Option<&StructDefinitionInfo> {
        self.struct_registry.get(name)
    }
}

impl Scope {
    /// Create a new empty scope
    pub fn new(is_function_scope: bool) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            generic_params: HashMap::new(),
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
