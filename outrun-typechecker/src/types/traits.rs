//! Trait system definitions and implementation tracking
//!
//! This module handles trait definitions, implementations, and the complex
//! trait constraint system that powers Outrun's "everything is traits" philosophy.

use super::{AtomId, TypeId};
use outrun_parser::Span;
use std::collections::HashMap;

/// A trait definition with functions and constraints
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub id: TypeId,
    pub name: String,
    pub functions: Vec<TraitFunction>,
    pub generic_params: Vec<TypeId>,
    pub constraints: Vec<TraitConstraint>,
    pub span: Span,
}

/// Function definition within a trait
#[derive(Debug, Clone, PartialEq)]
pub struct TraitFunction {
    pub name: AtomId,
    pub params: Vec<(AtomId, TypeId)>,
    pub return_type: TypeId,
    pub is_guard: bool,
    pub is_static: bool, // true for `defs` functions, false for instance functions
    pub has_default_impl: bool, // true for function definitions with bodies, false for signatures
    pub span: Span,
}

/// Trait constraint (e.g., T: Display && T: Debug)
#[derive(Debug, Clone, PartialEq)]
pub struct TraitConstraint {
    pub type_param: TypeId,
    pub required_traits: Vec<TypeId>,
    pub span: Span,
}

/// Implementation of a trait for a specific type
#[derive(Debug, Clone, PartialEq)]
pub struct TraitImplementation {
    pub trait_id: TypeId,
    pub type_id: TypeId,
    pub functions: HashMap<AtomId, FunctionId>,
    pub generic_params: Vec<TypeId>,
    pub constraints: Vec<TraitConstraint>,
    pub span: Span,
}

/// Unique identifier for function implementations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub u32);

/// Result of exhaustiveness checking for trait case statements and guard analysis
#[derive(Debug, Clone, PartialEq)]
pub enum ExhaustivenessResult {
    /// All possible cases are covered
    Exhaustive,
    /// Some trait implementations are missing from the case statement
    MissingTraitImplementations(Vec<TypeId>),
    /// Some guard patterns are missing - contains counter-examples from SAT solving
    MissingGuardPatterns(Vec<GuardCounterExample>),
    /// Open type domain (infinite) - requires explicit default case for exhaustiveness
    OpenType,
}

/// Counter-example from SAT solving showing missing guard coverage
#[derive(Debug, Clone, PartialEq)]
pub struct GuardCounterExample {
    /// Variable assignments that satisfy the negated guard condition
    pub variable_assignments: std::collections::HashMap<String, bool>,
    /// Human-readable description of the missing pattern
    pub description: String,
    /// Suggested guard condition to add for coverage
    pub suggested_guard: Option<String>,
}

impl TraitDefinition {
    /// Create a new trait definition
    pub fn new(id: TypeId, name: String, functions: Vec<TraitFunction>, span: Span) -> Self {
        Self {
            id,
            name,
            functions,
            generic_params: Vec::new(),
            constraints: Vec::new(),
            span,
        }
    }

    /// Add a generic parameter to this trait
    pub fn add_generic_param(&mut self, param: TypeId) {
        self.generic_params.push(param);
    }

    /// Add a constraint to this trait
    pub fn add_constraint(&mut self, constraint: TraitConstraint) {
        self.constraints.push(constraint);
    }

    /// Find a function by name in this trait
    pub fn find_function(&self, name: AtomId) -> Option<&TraitFunction> {
        self.functions.iter().find(|f| f.name == name)
    }

    /// Check if this trait has any generic parameters
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }
}

impl TraitFunction {
    /// Create a new trait function signature (no default implementation)
    pub fn new(
        name: AtomId,
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
            is_static: false,        // Default to instance function
            has_default_impl: false, // Signature only - must be implemented
            span,
        }
    }

    /// Create a new trait function with default implementation
    pub fn new_with_default(
        name: AtomId,
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
            is_static: false,
            has_default_impl: true, // Has default implementation - can be overridden
            span,
        }
    }

    /// Create a new static trait function
    pub fn new_static(
        name: AtomId,
        params: Vec<(AtomId, TypeId)>,
        return_type: TypeId,
        span: Span,
    ) -> Self {
        Self {
            name,
            params,
            return_type,
            is_guard: false, // Static functions can't be guards
            is_static: true,
            has_default_impl: true, // Static functions always have implementations
            span,
        }
    }

    /// Check if this function is a guard function (name ends with '?')
    pub fn is_guard_function(&self) -> bool {
        self.is_guard
    }

    /// Get the number of parameters
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

impl TraitImplementation {
    /// Create a new trait implementation
    pub fn new(
        trait_id: TypeId,
        type_id: TypeId,
        functions: HashMap<AtomId, FunctionId>,
        span: Span,
    ) -> Self {
        Self {
            trait_id,
            type_id,
            functions,
            generic_params: Vec::new(),
            constraints: Vec::new(),
            span,
        }
    }

    /// Check if this implementation provides a specific function
    pub fn implements_function(&self, name: AtomId) -> bool {
        self.functions.contains_key(&name)
    }

    /// Get the function ID for a specific function name
    pub fn get_function_id(&self, name: AtomId) -> Option<FunctionId> {
        self.functions.get(&name).copied()
    }

    /// Add a function implementation
    pub fn add_function(&mut self, name: AtomId, function_id: FunctionId) {
        self.functions.insert(name, function_id);
    }
}

impl TraitConstraint {
    /// Create a new trait constraint
    pub fn new(type_param: TypeId, required_traits: Vec<TypeId>, span: Span) -> Self {
        Self {
            type_param,
            required_traits,
            span,
        }
    }

    /// Check if this constraint requires a specific trait
    pub fn requires_trait(&self, trait_id: TypeId) -> bool {
        self.required_traits.contains(&trait_id)
    }
}

/// Trait registry for managing all trait definitions and implementations
#[derive(Debug, Default, Clone)]
pub struct TraitRegistry {
    definitions: HashMap<TypeId, TraitDefinition>,
    implementations: HashMap<(TypeId, TypeId), TraitImplementation>,
    next_function_id: u32,
}

impl TraitRegistry {
    /// Create a new empty trait registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a trait definition
    pub fn register_trait(&mut self, definition: TraitDefinition) {
        self.definitions.insert(definition.id, definition);
    }

    /// Register a trait implementation
    pub fn register_implementation(&mut self, implementation: TraitImplementation) {
        let key = (implementation.trait_id, implementation.type_id);
        self.implementations.insert(key, implementation);
    }

    /// Get a trait definition by ID
    pub fn get_trait(&self, trait_id: TypeId) -> Option<&TraitDefinition> {
        self.definitions.get(&trait_id)
    }

    /// Check if a trait definition exists with the given TraitId
    pub fn has_trait(&self, trait_id: TypeId) -> bool {
        self.definitions.contains_key(&trait_id)
    }

    /// Get a trait implementation
    pub fn get_implementation(
        &self,
        trait_id: TypeId,
        type_id: TypeId,
    ) -> Option<&TraitImplementation> {
        self.implementations.get(&(trait_id, type_id))
    }

    /// Check if a type implements a trait
    pub fn implements_trait(&self, type_id: TypeId, trait_id: TypeId) -> bool {
        self.implementations.contains_key(&(trait_id, type_id))
    }

    /// Generate a new unique function ID
    pub fn next_function_id(&mut self) -> FunctionId {
        let id = FunctionId(self.next_function_id);
        self.next_function_id += 1;
        id
    }

    /// Get all implementations of a trait
    pub fn get_trait_implementations(&self, trait_id: TypeId) -> Vec<&TraitImplementation> {
        self.implementations
            .iter()
            .filter(|((t_id, _), _)| *t_id == trait_id)
            .map(|(_, impl_)| impl_)
            .collect()
    }

    /// Get all traits implemented by a type
    pub fn get_type_implementations(&self, type_id: TypeId) -> Vec<&TraitImplementation> {
        self.implementations
            .iter()
            .filter(|((_, t_id), _)| *t_id == type_id)
            .map(|(_, impl_)| impl_)
            .collect()
    }

    /// Get all concrete types that implement a trait (for exhaustiveness checking)
    pub fn get_trait_implementors(&self, trait_id: TypeId) -> Vec<TypeId> {
        self.implementations
            .iter()
            .filter(|((t_id, _), _)| *t_id == trait_id)
            .map(|((_, type_id), _)| *type_id)
            .collect()
    }

    /// Check if a trait case statement is exhaustive by verifying all implementors are covered
    pub fn check_trait_case_exhaustiveness(
        &self,
        trait_id: TypeId,
        covered_types: &[TypeId],
    ) -> ExhaustivenessResult {
        let all_implementors = self.get_trait_implementors(trait_id);

        // Find missing implementations
        let missing_types: Vec<TypeId> = all_implementors
            .iter()
            .filter(|&type_id| !covered_types.contains(type_id))
            .copied()
            .collect();

        if missing_types.is_empty() {
            ExhaustivenessResult::Exhaustive
        } else {
            ExhaustivenessResult::MissingTraitImplementations(missing_types)
        }
    }

    /// Get all trait implementations for dispatch table construction
    pub fn all_implementations(&self) -> impl Iterator<Item = &TraitImplementation> {
        self.implementations.values()
    }

    /// Get all trait definitions for dispatch table construction
    pub fn all_traits(&self) -> impl Iterator<Item = &TraitDefinition> {
        self.definitions.values()
    }
}
