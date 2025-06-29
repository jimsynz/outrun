//! Type extraction utilities for getting real types from TypedExpression
//!
//! This module provides utilities to extract proper type information from
//! TypedExpression instances, avoiding the need for mock types in most cases.

use outrun_typechecker::{
    compilation::compiler_environment::CompilerEnvironment, unification::StructuredType,
};

/// Utilities for extracting type information from TypedExpression and StructuredType
pub struct TypeExtractor;

impl TypeExtractor {
    /// Extract the element type from a List<T> type
    ///
    /// Returns Some(element_type) if the type is a generic List, None otherwise
    pub fn extract_list_element_type(structured_type: &StructuredType) -> Option<StructuredType> {
        match structured_type {
            StructuredType::Generic { base: _, args } => {
                // Check if this is a List type by examining the base TypeId
                // For now, we'll use a simple approach - in a full implementation
                // we'd check the actual type name through the TypeInterner
                if !args.is_empty() {
                    Some(args[0].clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract key and value types from a Map<K, V> type
    ///
    /// Returns Some((key_type, value_type)) if the type is a generic Map, None otherwise
    pub fn extract_map_types(
        structured_type: &StructuredType,
    ) -> Option<(StructuredType, StructuredType)> {
        match structured_type {
            StructuredType::Generic { base: _, args } => {
                if args.len() >= 2 {
                    Some((args[0].clone(), args[1].clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract element types from a Tuple type
    ///
    /// Returns Some(element_types) if the type is a Tuple, None otherwise
    pub fn extract_tuple_types(structured_type: &StructuredType) -> Option<Vec<StructuredType>> {
        match structured_type {
            StructuredType::Tuple(element_types) => Some(element_types.clone()),
            _ => None,
        }
    }

    /// Extract the value type from an Option<T> type
    ///
    /// Returns Some(value_type) if the type is a generic Option, None otherwise
    pub fn extract_option_value_type(structured_type: &StructuredType) -> Option<StructuredType> {
        match structured_type {
            StructuredType::Generic { base: _, args } => {
                // Similar approach - check if this looks like Option<T>
                if !args.is_empty() {
                    Some(args[0].clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract value and error types from a Result<T, E> type
    ///
    /// Returns Some((value_type, error_type)) if the type is a generic Result, None otherwise
    pub fn extract_result_types(
        structured_type: &StructuredType,
    ) -> Option<(StructuredType, StructuredType)> {
        match structured_type {
            StructuredType::Generic { base: _, args } => {
                if args.len() >= 2 {
                    Some((args[0].clone(), args[1].clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Check if a StructuredType represents a specific named type
    ///
    /// This is a helper for type checking - in a full implementation this would
    /// use the CompilerEnvironment to resolve type names properly
    pub fn is_type_named(
        structured_type: &StructuredType,
        expected_name: &str,
        compiler_environment: &CompilerEnvironment,
    ) -> bool {
        match structured_type {
            StructuredType::Simple(type_id) => {
                // Get the type name from the compiler environment and compare
                // For now we'll use a simplified approach
                compiler_environment
                    .resolve_type(type_id.clone())
                    .map(|name| name.contains(expected_name))
                    .unwrap_or(false)
            }
            StructuredType::Generic { base, .. } => {
                // Check the base type name
                compiler_environment
                    .resolve_type(base.clone())
                    .map(|name| name.contains(expected_name))
                    .unwrap_or(false)
            }
            _ => false,
        }
    }

    /// Create a mock StructuredType when real type extraction fails
    ///
    /// This is a fallback for cases where we can't extract proper type information
    pub fn create_mock_type(type_name: &str) -> StructuredType {
        // Create a temporary interner for mock types
        let env = CompilerEnvironment::new();
        let type_id = env.intern_type_name(type_name);
        StructuredType::Simple(type_id)
    }

    /// Extract type information from function return type for intrinsics
    ///
    /// This looks at what type the function is expected to return and extracts
    /// useful information for creating the right runtime values
    pub fn extract_function_return_info(return_type: &StructuredType) -> FunctionReturnInfo {
        // Check what kind of type we're expected to return
        if let Some(element_type) = Self::extract_list_element_type(return_type) {
            return FunctionReturnInfo::List { element_type };
        }

        if let Some((key_type, value_type)) = Self::extract_map_types(return_type) {
            return FunctionReturnInfo::Map {
                key_type,
                value_type,
            };
        }

        if let Some(element_types) = Self::extract_tuple_types(return_type) {
            return FunctionReturnInfo::Tuple { element_types };
        }

        if let Some(value_type) = Self::extract_option_value_type(return_type) {
            return FunctionReturnInfo::Option { value_type };
        }

        if let Some((value_type, error_type)) = Self::extract_result_types(return_type) {
            return FunctionReturnInfo::Result {
                value_type,
                error_type,
            };
        }

        // For primitive types or unknown types
        FunctionReturnInfo::Other {
            return_type: return_type.clone(),
        }
    }
}

/// Information about what type a function should return
///
/// This helps intrinsics create the right runtime values with proper type information
#[derive(Debug, Clone)]
pub enum FunctionReturnInfo {
    /// Function returns a List<T>
    List { element_type: StructuredType },

    /// Function returns a Map<K, V>
    Map {
        key_type: StructuredType,
        value_type: StructuredType,
    },

    /// Function returns a Tuple<T1, T2, ...>
    Tuple { element_types: Vec<StructuredType> },

    /// Function returns an Option<T>
    Option { value_type: StructuredType },

    /// Function returns a Result<T, E>
    Result {
        value_type: StructuredType,
        error_type: StructuredType,
    },

    /// Function returns some other type (primitive, struct, etc.)
    Other { return_type: StructuredType },
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_typechecker::compilation::compiler_environment::{CompilerEnvironment, TypeNameId};

    fn create_test_types() -> (CompilerEnvironment, TypeNameId, TypeNameId, TypeNameId) {
        let env = CompilerEnvironment::new();
        let list_id = env.intern_type_name("List");
        let map_id = env.intern_type_name("Map");
        let integer_id = env.intern_type_name("Integer");
        (env, list_id, map_id, integer_id)
    }

    #[test]
    fn test_extract_list_element_type() {
        let (_, list_id, _, integer_id) = create_test_types();

        let list_type = StructuredType::Generic {
            base: list_id,
            args: vec![StructuredType::Simple(integer_id.clone())],
        };

        let element_type = TypeExtractor::extract_list_element_type(&list_type);
        assert!(element_type.is_some());
        assert_eq!(element_type.unwrap(), StructuredType::Simple(integer_id));
    }

    #[test]
    fn test_extract_map_types() {
        let (_, _, map_id, integer_id) = create_test_types();
        let env = CompilerEnvironment::new();
        let string_id = env.intern_type_name("String");

        let map_type = StructuredType::Generic {
            base: map_id,
            args: vec![
                StructuredType::Simple(string_id.clone()),
                StructuredType::Simple(integer_id.clone()),
            ],
        };

        let types = TypeExtractor::extract_map_types(&map_type);
        assert!(types.is_some());
        let (key_type, value_type) = types.unwrap();
        assert_eq!(key_type, StructuredType::Simple(string_id));
        assert_eq!(value_type, StructuredType::Simple(integer_id));
    }

    #[test]
    fn test_extract_tuple_types() {
        let (_, _, _, integer_id) = create_test_types();
        let env = CompilerEnvironment::new();
        let string_id = env.intern_type_name("String");

        let tuple_type = StructuredType::Tuple(vec![
            StructuredType::Simple(string_id.clone()),
            StructuredType::Simple(integer_id.clone()),
        ]);

        let element_types = TypeExtractor::extract_tuple_types(&tuple_type);
        assert!(element_types.is_some());
        let types = element_types.unwrap();
        assert_eq!(types.len(), 2);
        assert_eq!(types[0], StructuredType::Simple(string_id));
        assert_eq!(types[1], StructuredType::Simple(integer_id));
    }

    #[test]
    fn test_function_return_info_extraction() {
        let (_, list_id, _, integer_id) = create_test_types();

        let list_type = StructuredType::Generic {
            base: list_id,
            args: vec![StructuredType::Simple(integer_id.clone())],
        };

        let return_info = TypeExtractor::extract_function_return_info(&list_type);
        match return_info {
            FunctionReturnInfo::List { element_type } => {
                assert_eq!(element_type, StructuredType::Simple(integer_id));
            }
            _ => panic!("Expected List return info"),
        }
    }

    #[test]
    fn test_fallback_to_other_type() {
        let (_, _, _, integer_id) = create_test_types();

        let simple_type = StructuredType::Simple(integer_id.clone());
        let return_info = TypeExtractor::extract_function_return_info(&simple_type);

        match return_info {
            FunctionReturnInfo::Other { return_type } => {
                assert_eq!(return_type, StructuredType::Simple(integer_id));
            }
            _ => panic!("Expected Other return info"),
        }
    }
}
