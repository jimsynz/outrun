//! Concrete type definitions for Outrun
//!
//! This module defines all concrete types that exist at runtime in Outrun,
//! including primitives, collections, and user-defined types.

use super::{AtomId, TypeId};
use outrun_parser::Span;
// use std::collections::HashMap; // TODO: Use when needed

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
        element_type: TypeId,
    },
    Tuple {
        element_types: Vec<TypeId>,
    },
    Map {
        key_type: TypeId,
        value_type: TypeId,
    },

    // Option and Result types for error handling
    Option {
        inner_type: TypeId,
    },
    Result {
        ok_type: TypeId,
        err_type: TypeId,
    },

    // User-defined types
    Struct {
        name: TypeId,
        fields: Vec<StructField>,
    },
    Trait {
        name: TypeId,
        functions: Vec<TraitFunction>,
    },

    // Function types
    Function {
        params: Vec<(AtomId, TypeId)>, // (param_name, param_type)
        return_type: TypeId,
    },
}

/// Field definition in a struct
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: AtomId,
    pub type_id: TypeId,
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
    pub params: Vec<(AtomId, TypeId)>, // (param_name, param_type)
    pub return_type: TypeId,
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
    pub fn option_inner_type(&self) -> Option<TypeId> {
        match self {
            ConcreteType::Option { inner_type } => Some(*inner_type),
            _ => None,
        }
    }

    /// Get the ok and error types for Result types
    pub fn result_types(&self) -> Option<(TypeId, TypeId)> {
        match self {
            ConcreteType::Result { ok_type, err_type } => Some((*ok_type, *err_type)),
            _ => None,
        }
    }

    /// Get element type for List
    pub fn list_element_type(&self) -> Option<TypeId> {
        match self {
            ConcreteType::List { element_type } => Some(*element_type),
            _ => None,
        }
    }

    /// Get element types for Tuple
    pub fn tuple_element_types(&self) -> Option<&[TypeId]> {
        match self {
            ConcreteType::Tuple { element_types } => Some(element_types),
            _ => None,
        }
    }

    /// Get key and value types for Map
    pub fn map_types(&self) -> Option<(TypeId, TypeId)> {
        match self {
            ConcreteType::Map {
                key_type,
                value_type,
            } => Some((*key_type, *value_type)),
            _ => None,
        }
    }
}

impl FunctionSignature {
    /// Create a new function signature
    pub fn new(params: Vec<(AtomId, TypeId)>, return_type: TypeId, is_guard: bool) -> Self {
        Self {
            params,
            return_type,
            is_guard,
        }
    }

    /// Check if parameter names are unique (required by Outrun)
    pub fn has_unique_params(&self) -> bool {
        let mut seen = std::collections::HashSet::new();
        self.params.iter().all(|(name, _)| seen.insert(*name))
    }

    /// Get parameter type by name
    pub fn get_param_type(&self, name: AtomId) -> Option<TypeId> {
        self.params
            .iter()
            .find(|(param_name, _)| *param_name == name)
            .map(|(_, type_id)| *type_id)
    }

    /// Get all parameter names
    pub fn param_names(&self) -> Vec<AtomId> {
        self.params.iter().map(|(name, _)| *name).collect()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeInterner;

    fn setup_test_types() -> (TypeInterner, TypeId, TypeId, AtomId) {
        let mut interner = TypeInterner::new();
        let int_type = interner.intern_type("Integer");
        let str_type = interner.intern_type("String");
        let name_atom = interner.intern_atom("name");
        (interner, int_type, str_type, name_atom)
    }

    #[test]
    fn test_primitive_types() {
        let int_type = ConcreteType::Integer64;
        let bool_type = ConcreteType::Boolean;

        assert!(int_type.is_primitive());
        assert!(bool_type.is_primitive());
        assert!(!int_type.is_collection());
        assert!(!int_type.is_generic());
    }

    #[test]
    fn test_collection_types() {
        let (_, int_type, str_type, _) = setup_test_types();

        let list_type = ConcreteType::List {
            element_type: int_type,
        };
        let tuple_type = ConcreteType::Tuple {
            element_types: vec![int_type, str_type],
        };

        assert!(list_type.is_collection());
        assert!(tuple_type.is_collection());
        assert!(list_type.is_generic());
        assert_eq!(list_type.list_element_type(), Some(int_type));
        assert_eq!(
            tuple_type.tuple_element_types(),
            Some(&[int_type, str_type][..])
        );
    }

    #[test]
    fn test_option_result_types() {
        let (_, int_type, str_type, _) = setup_test_types();

        let option_type = ConcreteType::Option {
            inner_type: int_type,
        };
        let result_type = ConcreteType::Result {
            ok_type: int_type,
            err_type: str_type,
        };

        assert!(option_type.is_generic());
        assert!(result_type.is_generic());
        assert_eq!(option_type.option_inner_type(), Some(int_type));
        assert_eq!(result_type.result_types(), Some((int_type, str_type)));
    }

    #[test]
    fn test_function_signature() {
        let (_, int_type, str_type, name_atom) = setup_test_types();

        let sig = FunctionSignature::new(vec![(name_atom, str_type)], int_type, false);

        assert!(sig.has_unique_params());
        assert_eq!(sig.get_param_type(name_atom), Some(str_type));
        assert_eq!(sig.param_names(), vec![name_atom]);
        assert!(!sig.is_guard);
    }

    #[test]
    fn test_function_signature_duplicate_params() {
        let (_, int_type, _, name_atom) = setup_test_types();

        let sig = FunctionSignature::new(
            vec![(name_atom, int_type), (name_atom, int_type)], // Duplicate names
            int_type,
            false,
        );

        assert!(!sig.has_unique_params());
    }

    #[test]
    fn test_type_compatibility() {
        let int1 = ConcreteType::Integer64;
        let int2 = ConcreteType::Integer64;
        let bool_type = ConcreteType::Boolean;

        assert!(TypeCompatibility::are_equal(&int1, &int2));
        assert!(!TypeCompatibility::are_equal(&int1, &bool_type));
        assert!(TypeCompatibility::is_assignable_to(&int1, &int2));
        assert!(!TypeCompatibility::is_assignable_to(&int1, &bool_type));
    }

    #[test]
    fn test_boolean_condition() {
        let bool_type = ConcreteType::Boolean;
        let int_type = ConcreteType::Integer64;

        assert!(TypeCompatibility::can_be_condition(&bool_type));
        assert!(!TypeCompatibility::can_be_condition(&int_type));
    }
}
