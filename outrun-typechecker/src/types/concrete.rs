//! Concrete type definitions for Outrun
//!
//! This module defines all concrete types that exist at runtime in Outrun,
//! including primitives, collections, and user-defined types.

use crate::compilation::compiler_environment::{AtomId, TypeNameId};
use outrun_parser::Span;

/// All concrete types that can exist at runtime in Outrun
#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteType {
    // Core primitive types
    Integer64,
    Float64,
    Boolean,
    String,
    Atom,

    // Collection types with element type information
    List {
        element_type: TypeNameId,
    },
    Tuple {
        element_types: Vec<TypeNameId>,
    },
    Map {
        key_type: TypeNameId,
        value_type: TypeNameId,
    },

    // Option and Result types for error handling
    Option {
        inner_type: TypeNameId,
    },
    Result {
        ok_type: TypeNameId,
        err_type: TypeNameId,
    },

    // User-defined types
    Struct {
        name: TypeNameId,
        fields: Vec<StructField>,
    },
    Trait {
        name: TypeNameId,
        functions: Vec<TraitFunction>,
    },

    // Function types
    Function {
        params: Vec<(AtomId, TypeNameId)>, // (param_name, param_type)
        return_type: TypeNameId,
    },
}

/// Field definition in a struct
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: AtomId,
    pub type_id: TypeNameId,
    pub span: Span,
}

/// Function in a trait definition
#[derive(Debug, Clone, PartialEq)]
pub struct TraitFunction {
    pub name: AtomId,
    pub signature: FunctionSignature,
    pub span: Span,
}

/// Function signature with parameter and return type information
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub params: Vec<(AtomId, TypeNameId)>, // (param_name, param_type)
    pub return_type: TypeNameId,
    pub is_guard: bool, // True if function name ends with '?'
}

impl ConcreteType {
    /// Check if this type is a primitive type
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            ConcreteType::Integer64
                | ConcreteType::Float64
                | ConcreteType::Boolean
                | ConcreteType::String
                | ConcreteType::Atom
        )
    }

    /// Check if this type is a collection type
    pub fn is_collection(&self) -> bool {
        matches!(
            self,
            ConcreteType::List { .. } | ConcreteType::Tuple { .. } | ConcreteType::Map { .. }
        )
    }

    /// Check if this type is a generic container (Option, Result, List, etc.)
    pub fn is_generic(&self) -> bool {
        matches!(
            self,
            ConcreteType::List { .. }
                | ConcreteType::Tuple { .. }
                | ConcreteType::Map { .. }
                | ConcreteType::Option { .. }
                | ConcreteType::Result { .. }
        )
    }

    /// Get the inner type for Option types
    pub fn option_inner_type(&self) -> Option<TypeNameId> {
        match self {
            ConcreteType::Option { inner_type } => Some(inner_type.clone()),
            _ => None,
        }
    }

    /// Get the ok and error types for Result types
    pub fn result_types(&self) -> Option<(TypeNameId, TypeNameId)> {
        match self {
            ConcreteType::Result { ok_type, err_type } => Some((ok_type.clone(), err_type.clone())),
            _ => None,
        }
    }

    /// Get element type for List
    pub fn list_element_type(&self) -> Option<TypeNameId> {
        match self {
            ConcreteType::List { element_type } => Some(element_type.clone()),
            _ => None,
        }
    }

    /// Get element types for Tuple
    pub fn tuple_element_types(&self) -> Option<&[TypeNameId]> {
        match self {
            ConcreteType::Tuple { element_types } => Some(element_types),
            _ => None,
        }
    }

    /// Get key and value types for Map
    pub fn map_types(&self) -> Option<(TypeNameId, TypeNameId)> {
        match self {
            ConcreteType::Map {
                key_type,
                value_type,
            } => Some((key_type.clone(), value_type.clone())),
            _ => None,
        }
    }
}

impl FunctionSignature {
    /// Create a new function signature
    pub fn new(params: Vec<(AtomId, TypeNameId)>, return_type: TypeNameId, is_guard: bool) -> Self {
        Self {
            params,
            return_type,
            is_guard,
        }
    }

    /// Check if parameter names are unique (required by Outrun)
    pub fn has_unique_params(&self) -> bool {
        let mut seen = std::collections::HashSet::new();
        self.params
            .iter()
            .all(|(name, _)| seen.insert(name.clone()))
    }

    /// Get parameter type by name
    pub fn get_param_type(&self, name: AtomId) -> Option<TypeNameId> {
        self.params
            .iter()
            .find(|(param_name, _)| *param_name == name)
            .map(|(_, type_id)| type_id.clone())
    }

    /// Get all parameter names
    pub fn param_names(&self) -> Vec<AtomId> {
        self.params.iter().map(|(name, _)| name.clone()).collect()
    }
}

/// Type compatibility checking utilities
pub struct TypeCompatibility;

impl TypeCompatibility {
    /// Check if two types are exactly equal
    pub fn are_equal(type1: &ConcreteType, type2: &ConcreteType) -> bool {
        type1 == type2
    }

    /// Check if one type can be assigned to another (for future subtyping)
    pub fn is_assignable_to(from: &ConcreteType, to: &ConcreteType) -> bool {
        // For now, only exact equality. Future: implement subtyping rules
        Self::are_equal(from, to)
    }

    /// Check if a type implements the Boolean trait (for conditionals)
    pub fn can_be_condition(concrete_type: &ConcreteType) -> bool {
        // Only Boolean type can be used in conditionals (no truthiness)
        matches!(concrete_type, ConcreteType::Boolean)
    }
}
