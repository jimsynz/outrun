//! Protocol system definitions and implementation tracking
//!
//! This module handles protocol definitions, implementations, and the complex
//! protocol constraint system that powers Outrun's "everything is protocols" philosophy.

use crate::compilation::compiler_environment::{AtomId, TypeNameId};
use outrun_parser::Span;
use std::collections::HashMap;

/// A protocol definition with functions and constraints
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolDefinition {
    pub id: TypeNameId,
    pub name: String,
    pub functions: Vec<ProtocolFunction>,
    pub generic_params: Vec<TypeNameId>,
    pub constraints: Vec<ProtocolConstraint>,
    pub span: Span,
}

/// Function definition within a protocol
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolFunction {
    pub name: AtomId,
    pub params: Vec<(AtomId, TypeNameId)>,
    pub return_type: TypeNameId,
    pub is_guard: bool,
    pub is_static: bool, // true for `defs` functions, false for instance functions
    pub has_default_impl: bool, // true for function definitions with bodies, false for signatures
    pub span: Span,
}

/// Protocol constraint (e.g., T: Display && T: Debug)
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolConstraint {
    pub type_param: TypeNameId,
    pub required_protocols: Vec<TypeNameId>,
    pub span: Span,
}

/// Implementation of a protocol for a specific type
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolImplementation {
    pub protocol_id: TypeNameId,
    pub type_id: TypeNameId,
    pub functions: HashMap<AtomId, FunctionId>,
    pub generic_params: Vec<TypeNameId>,
    pub constraints: Vec<ProtocolConstraint>,
    pub span: Span,
}

/// Unique identifier for function implementations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub u32);

/// Result of exhaustiveness checking for protocol case statements and guard analysis
#[derive(Debug, Clone, PartialEq)]
pub enum ExhaustivenessResult {
    /// All possible cases are covered
    Exhaustive,
    /// Some protocol implementations are missing from the case statement
    MissingProtocolImplementations(Vec<TypeNameId>),
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

impl ProtocolDefinition {
    /// Create a new protocol definition
    pub fn new(id: TypeNameId, name: String, functions: Vec<ProtocolFunction>, span: Span) -> Self {
        Self {
            id,
            name,
            functions,
            generic_params: Vec::new(),
            constraints: Vec::new(),
            span,
        }
    }

    /// Add a generic parameter to this protocol
    pub fn add_generic_param(&mut self, param: TypeNameId) {
        self.generic_params.push(param);
    }

    /// Add a constraint to this protocol
    pub fn add_constraint(&mut self, constraint: ProtocolConstraint) {
        self.constraints.push(constraint);
    }

    /// Find a function by name in this protocol
    pub fn find_function(&self, name: AtomId) -> Option<&ProtocolFunction> {
        self.functions.iter().find(|f| f.name == name)
    }

    /// Check if this protocol has any generic parameters
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }
}

impl ProtocolFunction {
    /// Create a new protocol function signature (no default implementation)
    pub fn new(
        name: AtomId,
        params: Vec<(AtomId, TypeNameId)>,
        return_type: TypeNameId,
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

    /// Create a new protocol function with default implementation
    pub fn new_with_default(
        name: AtomId,
        params: Vec<(AtomId, TypeNameId)>,
        return_type: TypeNameId,
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

    /// Create a new static protocol function
    pub fn new_static(
        name: AtomId,
        params: Vec<(AtomId, TypeNameId)>,
        return_type: TypeNameId,
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

impl ProtocolImplementation {
    /// Create a new protocol implementation
    pub fn new(
        protocol_id: TypeNameId,
        type_id: TypeNameId,
        functions: HashMap<AtomId, FunctionId>,
        span: Span,
    ) -> Self {
        Self {
            protocol_id,
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

impl ProtocolConstraint {
    /// Create a new protocol constraint
    pub fn new(type_param: TypeNameId, required_protocols: Vec<TypeNameId>, span: Span) -> Self {
        Self {
            type_param,
            required_protocols,
            span,
        }
    }

    /// Check if this constraint requires a specific protocol
    pub fn requires_protocol(&self, protocol_id: TypeNameId) -> bool {
        self.required_protocols.contains(&protocol_id)
    }
}

/// Protocol registry for managing all protocol definitions and implementations
#[derive(Debug, Default, Clone)]
pub struct ProtocolRegistry {
    definitions: HashMap<TypeNameId, ProtocolDefinition>,
    implementations: HashMap<(TypeNameId, TypeNameId), ProtocolImplementation>,
    next_function_id: u32,
}

impl ProtocolRegistry {
    /// Create a new empty protocol registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a protocol definition
    pub fn register_protocol(&mut self, definition: ProtocolDefinition) {
        self.definitions.insert(definition.id.clone(), definition);
    }

    /// Register a protocol implementation
    pub fn register_implementation(&mut self, implementation: ProtocolImplementation) {
        let key = (
            implementation.protocol_id.clone(),
            implementation.type_id.clone(),
        );
        self.implementations.insert(key, implementation);
    }

    /// Get a protocol definition by ID
    pub fn get_protocol(&self, protocol_id: TypeNameId) -> Option<&ProtocolDefinition> {
        self.definitions.get(&protocol_id)
    }

    /// Check if a protocol definition exists with the given ProtocolId
    pub fn has_protocol(&self, protocol_id: TypeNameId) -> bool {
        self.definitions.contains_key(&protocol_id)
    }

    /// Get a protocol implementation
    pub fn get_implementation(
        &self,
        protocol_id: TypeNameId,
        type_id: TypeNameId,
    ) -> Option<&ProtocolImplementation> {
        self.implementations.get(&(protocol_id, type_id))
    }

    /// Check if a type implements a protocol
    pub fn implements_protocol(&self, type_id: TypeNameId, protocol_id: TypeNameId) -> bool {
        self.implementations.contains_key(&(protocol_id, type_id))
    }

    /// Generate a new unique function ID
    pub fn next_function_id(&mut self) -> FunctionId {
        let id = FunctionId(self.next_function_id);
        self.next_function_id += 1;
        id
    }

    /// Get all implementations of a protocol
    pub fn get_protocol_implementations(
        &self,
        protocol_id: TypeNameId,
    ) -> Vec<&ProtocolImplementation> {
        self.implementations
            .iter()
            .filter(|((t_id, _), _)| t_id.clone() == protocol_id)
            .map(|(_, impl_)| impl_)
            .collect()
    }

    /// Get all protocols implemented by a type
    pub fn get_type_implementations(&self, type_id: TypeNameId) -> Vec<&ProtocolImplementation> {
        self.implementations
            .iter()
            .filter(|((_, t_id), _)| t_id.clone() == type_id)
            .map(|(_, impl_)| impl_)
            .collect()
    }

    /// Get all concrete types that implement a protocol (for exhaustiveness checking)
    pub fn get_protocol_implementors(&self, protocol_id: TypeNameId) -> Vec<TypeNameId> {
        self.implementations
            .iter()
            .filter(|((t_id, _), _)| t_id.clone() == protocol_id)
            .map(|((_, type_id), _)| type_id.clone())
            .collect()
    }

    /// Check if a protocol case statement is exhaustive by verifying all implementors are covered
    pub fn check_protocol_case_exhaustiveness(
        &self,
        protocol_id: TypeNameId,
        covered_types: &[TypeNameId],
    ) -> ExhaustivenessResult {
        let all_implementors = self.get_protocol_implementors(protocol_id);

        // Find missing implementations
        let missing_types: Vec<TypeNameId> = all_implementors
            .iter()
            .filter(|&type_id| !covered_types.contains(type_id))
            .cloned()
            .collect();

        if missing_types.is_empty() {
            ExhaustivenessResult::Exhaustive
        } else {
            ExhaustivenessResult::MissingProtocolImplementations(missing_types)
        }
    }

    /// Get all protocol implementations for dispatch table construction
    pub fn all_implementations(&self) -> impl Iterator<Item = &ProtocolImplementation> {
        self.implementations.values()
    }

    /// Get all protocol definitions for dispatch table construction
    pub fn all_protocols(&self) -> impl Iterator<Item = &ProtocolDefinition> {
        self.definitions.values()
    }
}
