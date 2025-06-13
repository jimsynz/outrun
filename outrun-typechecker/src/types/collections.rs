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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeInterner;

    fn setup_test_types() -> (TypeInterner, TypeId, TypeId, TypeId) {
        let mut interner = TypeInterner::new();
        let int_type = interner.intern_type("Integer");
        let str_type = interner.intern_type("String");
        let bool_type = interner.intern_type("Boolean");
        (interner, int_type, str_type, bool_type)
    }

    #[test]
    fn test_collection_type_conversion() {
        let (_, int_type, str_type, _) = setup_test_types();

        let list_collection = CollectionType::List {
            element_type: int_type,
        };
        let list_concrete = list_collection.to_concrete_type();

        assert_eq!(
            list_concrete,
            ConcreteType::List {
                element_type: int_type
            }
        );

        let recovered = CollectionType::from_concrete_type(&list_concrete);
        assert_eq!(recovered, Some(list_collection));

        // Test tuple
        let tuple_collection = CollectionType::Tuple {
            element_types: vec![int_type, str_type],
        };
        let tuple_concrete = tuple_collection.to_concrete_type();

        assert_eq!(
            tuple_concrete,
            ConcreteType::Tuple {
                element_types: vec![int_type, str_type]
            }
        );
    }

    #[test]
    fn test_type_dependencies() {
        let (_, int_type, str_type, bool_type) = setup_test_types();

        let list = CollectionType::List {
            element_type: int_type,
        };
        assert_eq!(list.type_dependencies(), vec![int_type]);

        let tuple = CollectionType::Tuple {
            element_types: vec![int_type, str_type, bool_type],
        };
        assert_eq!(
            tuple.type_dependencies(),
            vec![int_type, str_type, bool_type]
        );

        let map = CollectionType::Map {
            key_type: str_type,
            value_type: int_type,
        };
        assert_eq!(map.type_dependencies(), vec![str_type, int_type]);
    }

    #[test]
    fn test_homogeneous_collections() {
        let (_, int_type, str_type, _) = setup_test_types();

        let list = CollectionType::List {
            element_type: int_type,
        };
        assert!(list.is_homogeneous());

        let map = CollectionType::Map {
            key_type: str_type,
            value_type: int_type,
        };
        assert!(map.is_homogeneous());

        // Homogeneous tuple
        let homogeneous_tuple = CollectionType::Tuple {
            element_types: vec![int_type, int_type, int_type],
        };
        assert!(homogeneous_tuple.is_homogeneous());

        // Heterogeneous tuple
        let heterogeneous_tuple = CollectionType::Tuple {
            element_types: vec![int_type, str_type],
        };
        assert!(!heterogeneous_tuple.is_homogeneous());
    }

    #[test]
    fn test_collection_validation() {
        let (_, int_type, str_type, _) = setup_test_types();

        let list = CollectionType::List {
            element_type: int_type,
        };
        assert!(validate_collection_type(&list).is_ok());

        let valid_tuple = CollectionType::Tuple {
            element_types: vec![int_type, str_type],
        };
        assert!(validate_collection_type(&valid_tuple).is_ok());

        let empty_tuple = CollectionType::Tuple {
            element_types: vec![],
        };
        assert_eq!(
            validate_collection_type(&empty_tuple),
            Err(CollectionError::EmptyTuple)
        );

        let map = CollectionType::Map {
            key_type: str_type,
            value_type: int_type,
        };
        assert!(validate_collection_type(&map).is_ok());
    }

    #[test]
    fn test_collection_inference() {
        let (_, int_type, str_type, bool_type) = setup_test_types();

        // Test list inference
        let list_type = CollectionInference::infer_list_type(&[int_type, int_type, int_type]);
        assert_eq!(
            list_type,
            Some(CollectionType::List {
                element_type: int_type
            })
        );

        let empty_list = CollectionInference::infer_list_type(&[]);
        assert_eq!(empty_list, None);

        // Test tuple creation
        let tuple_type =
            CollectionInference::create_tuple_type(vec![int_type, str_type, bool_type]);
        assert_eq!(
            tuple_type,
            Some(CollectionType::Tuple {
                element_types: vec![int_type, str_type, bool_type]
            })
        );

        let empty_tuple = CollectionInference::create_tuple_type(vec![]);
        assert_eq!(empty_tuple, None);

        // Test map creation
        let map_type = CollectionInference::create_map_type(str_type, int_type);
        assert_eq!(
            map_type,
            CollectionType::Map {
                key_type: str_type,
                value_type: int_type
            }
        );
    }

    #[test]
    fn test_non_collection_concrete_type() {
        let int_concrete = ConcreteType::Integer64;
        let result = CollectionType::from_concrete_type(&int_concrete);
        assert_eq!(result, None);
    }
}
