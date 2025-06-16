//! Collection type utilities and validation
//!
//! This module provides utilities for working with collection types (List, Tuple, Map)
//! and validating their type constraints.

use super::{ConcreteType, TypeId};

/// Specialized collection type for cleaner handling
#[derive(Debug, Clone, PartialEq)]
pub enum CollectionType {
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
}

impl CollectionType {
    /// Convert to a concrete type
    pub fn to_concrete_type(&self) -> ConcreteType {
        match self {
            CollectionType::List { element_type } => ConcreteType::List {
                element_type: *element_type,
            },
            CollectionType::Tuple { element_types } => ConcreteType::Tuple {
                element_types: element_types.clone(),
            },
            CollectionType::Map {
                key_type,
                value_type,
            } => ConcreteType::Map {
                key_type: *key_type,
                value_type: *value_type,
            },
        }
    }

    /// Create from a concrete type (returns None if not a collection)
    pub fn from_concrete_type(concrete: &ConcreteType) -> Option<Self> {
        match concrete {
            ConcreteType::List { element_type } => Some(CollectionType::List {
                element_type: *element_type,
            }),
            ConcreteType::Tuple { element_types } => Some(CollectionType::Tuple {
                element_types: element_types.clone(),
            }),
            ConcreteType::Map {
                key_type,
                value_type,
            } => Some(CollectionType::Map {
                key_type: *key_type,
                value_type: *value_type,
            }),
            _ => None,
        }
    }

    /// Get all type dependencies for this collection
    pub fn type_dependencies(&self) -> Vec<TypeId> {
        match self {
            CollectionType::List { element_type } => vec![*element_type],
            CollectionType::Tuple { element_types } => element_types.clone(),
            CollectionType::Map {
                key_type,
                value_type,
            } => vec![*key_type, *value_type],
        }
    }

    /// Check if this collection is homogeneous (all elements same type)
    pub fn is_homogeneous(&self) -> bool {
        match self {
            CollectionType::List { .. } => true, // Lists are always homogeneous
            CollectionType::Map { .. } => true,  // Maps are homogeneous per key/value
            CollectionType::Tuple { element_types } => {
                // Tuples are homogeneous if all elements are the same type
                element_types.windows(2).all(|w| w[0] == w[1])
            }
        }
    }
}

/// Validate collection type constraints and compatibility
pub fn validate_collection_type(collection: &CollectionType) -> Result<(), CollectionError> {
    match collection {
        CollectionType::List { element_type: _ } => {
            // Lists are always valid - any type can be a list element
            Ok(())
        }
        CollectionType::Tuple { element_types } => {
            if element_types.is_empty() {
                return Err(CollectionError::EmptyTuple);
            }
            // Tuples are valid as long as they have at least one element
            Ok(())
        }
        CollectionType::Map {
            key_type: _,
            value_type: _,
        } => {
            // TODO: In future, validate that key_type implements Hash + Eq traits
            // For now, all types are valid as map keys/values
            Ok(())
        }
    }
}

/// Errors that can occur during collection type validation
#[derive(Debug, Clone, PartialEq)]
pub enum CollectionError {
    EmptyTuple,
    InvalidKeyType { type_id: TypeId },
    InvalidValueType { type_id: TypeId },
    IncompatibleElementTypes { expected: TypeId, found: TypeId },
}

impl std::fmt::Display for CollectionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CollectionError::EmptyTuple => write!(f, "Tuples cannot be empty"),
            CollectionError::InvalidKeyType { type_id } => {
                write!(f, "Type {:?} cannot be used as map key", type_id)
            }
            CollectionError::InvalidValueType { type_id } => {
                write!(f, "Type {:?} cannot be used as map value", type_id)
            }
            CollectionError::IncompatibleElementTypes { expected, found } => {
                write!(f, "Expected element type {:?}, found {:?}", expected, found)
            }
        }
    }
}

impl std::error::Error for CollectionError {}

/// Collection type inference utilities
pub struct CollectionInference;

impl CollectionInference {
    /// Infer the most specific collection type from element types
    pub fn infer_list_type(element_types: &[TypeId]) -> Option<CollectionType> {
        if element_types.is_empty() {
            return None;
        }

        // For now, just use the first element type as the list element type
        // TODO: Implement proper type unification for mixed element types
        Some(CollectionType::List {
            element_type: element_types[0],
        })
    }

    /// Create a tuple type from element types
    pub fn create_tuple_type(element_types: Vec<TypeId>) -> Option<CollectionType> {
        if element_types.is_empty() {
            return None;
        }
        Some(CollectionType::Tuple { element_types })
    }

    /// Create a map type from key and value types
    pub fn create_map_type(key_type: TypeId, value_type: TypeId) -> CollectionType {
        CollectionType::Map {
            key_type,
            value_type,
        }
    }
}
