//! Type integration utilities for the Outrun interpreter
//!
//! This module provides bidirectional conversion between the interpreter's `Value`
//! runtime representation and the typechecker's `StructuredType` system.

use crate::value::Value;
use outrun_typechecker::{
    compilation::compiler_environment::{CompilerEnvironment, TypeNameId},
    unification::StructuredType,
};
use thiserror::Error;

/// Errors that can occur during type integration
#[derive(Debug, Error)]
pub enum TypeIntegrationError {
    #[error("Unknown type name: {type_name}")]
    UnknownTypeName { type_name: String },
}

/// Type integration utilities for converting between Value and StructuredType
pub struct TypeIntegration {
    compiler_environment: CompilerEnvironment,

    // Cache for common primitive types to avoid repeated lookups
    primitive_types: PrimitiveTypeCache,
}

/// Cache for frequently used primitive StructuredTypes
#[derive(Debug, Clone)]
struct PrimitiveTypeCache {
    boolean_type: Option<StructuredType>,
    integer64_type: Option<StructuredType>,
    float64_type: Option<StructuredType>,
    string_type: Option<StructuredType>,
    atom_type: Option<StructuredType>,
    list_base_id: Option<TypeNameId>,
    map_base_id: Option<TypeNameId>,
}

impl PrimitiveTypeCache {
    fn new() -> Self {
        Self {
            boolean_type: None,
            integer64_type: None,
            float64_type: None,
            string_type: None,
            atom_type: None,
            list_base_id: None,
            map_base_id: None,
        }
    }
}

impl TypeIntegration {
    /// Create a new type integration instance
    pub fn new(compiler_environment: CompilerEnvironment) -> Self {
        Self {
            compiler_environment,
            primitive_types: PrimitiveTypeCache::new(),
        }
    }

    /// Get the StructuredType for a runtime Value
    ///
    /// This extracts type information from the Value, using cached StructuredType
    /// for complex types and generating it for primitives.
    pub fn value_to_structured_type(&mut self, value: &Value) -> StructuredType {
        match value {
            Value::Boolean(_) => {
                if self.primitive_types.boolean_type.is_none() {
                    let type_id = self
                        .compiler_environment
                        .intern_type_name("Outrun.Core.Boolean");
                    self.primitive_types.boolean_type = Some(StructuredType::Simple(type_id));
                }
                self.primitive_types.boolean_type.clone().unwrap()
            }
            Value::Integer64(_) => {
                if self.primitive_types.integer64_type.is_none() {
                    let type_id = self
                        .compiler_environment
                        .intern_type_name("Outrun.Core.Integer64");
                    self.primitive_types.integer64_type = Some(StructuredType::Simple(type_id));
                }
                self.primitive_types.integer64_type.clone().unwrap()
            }
            Value::Float64(_) => {
                if self.primitive_types.float64_type.is_none() {
                    let type_id = self
                        .compiler_environment
                        .intern_type_name("Outrun.Core.Float64");
                    self.primitive_types.float64_type = Some(StructuredType::Simple(type_id));
                }
                self.primitive_types.float64_type.clone().unwrap()
            }
            Value::String(_) => {
                if self.primitive_types.string_type.is_none() {
                    let type_id = self
                        .compiler_environment
                        .intern_type_name("Outrun.Core.String");
                    self.primitive_types.string_type = Some(StructuredType::Simple(type_id));
                }
                self.primitive_types.string_type.clone().unwrap()
            }
            Value::Atom(_) => {
                if self.primitive_types.atom_type.is_none() {
                    let type_id = self
                        .compiler_environment
                        .intern_type_name("Outrun.Core.Atom");
                    self.primitive_types.atom_type = Some(StructuredType::Simple(type_id));
                }
                self.primitive_types.atom_type.clone().unwrap()
            }
            Value::List { element_type, .. } => {
                if self.primitive_types.list_base_id.is_none() {
                    self.primitive_types.list_base_id =
                        Some(self.compiler_environment.intern_type_name("List"));
                }
                StructuredType::Generic {
                    base: self.primitive_types.list_base_id.clone().unwrap(),
                    args: vec![element_type.clone()],
                }
            }
            Value::Map {
                key_type,
                value_type,
                ..
            } => {
                if self.primitive_types.map_base_id.is_none() {
                    self.primitive_types.map_base_id =
                        Some(self.compiler_environment.intern_type_name("Map"));
                }
                StructuredType::Generic {
                    base: self.primitive_types.map_base_id.clone().unwrap(),
                    args: vec![key_type.clone(), value_type.clone()],
                }
            }
            Value::Tuple { tuple_type, .. } => tuple_type.clone(),
            Value::Struct { struct_type, .. } => struct_type.clone(),
        }
    }

    /// Note: Default value creation is handled by Outrun's `Default` trait
    /// through the normal function dispatch system, not by the interpreter directly.
    ///
    /// Validate that a Value matches the expected StructuredType
    ///
    /// This is useful for runtime type checking and ensuring type safety.
    pub fn validate_value_type(&mut self, value: &Value, expected_type: &StructuredType) -> bool {
        let actual_type = self.value_to_structured_type(value);
        self.types_are_compatible(&actual_type, expected_type)
    }

    /// Check if two StructuredTypes are compatible
    ///
    /// This is a simplified compatibility check. In a full implementation,
    /// this would delegate to the typechecker's unification logic.
    #[allow(clippy::only_used_in_recursion)]
    pub fn types_are_compatible(&self, actual: &StructuredType, expected: &StructuredType) -> bool {
        match (actual, expected) {
            // Exact match
            (StructuredType::Simple(a), StructuredType::Simple(b)) => a == b,

            // Generic types must have same base and compatible args
            (
                StructuredType::Generic {
                    base: a_base,
                    args: a_args,
                },
                StructuredType::Generic {
                    base: e_base,
                    args: e_args,
                },
            ) => {
                a_base == e_base
                    && a_args.len() == e_args.len()
                    && a_args
                        .iter()
                        .zip(e_args.iter())
                        .all(|(a, e)| self.types_are_compatible(a, e))
            }

            // Tuple types must have same arity and compatible elements
            (StructuredType::Tuple(a_elements), StructuredType::Tuple(e_elements)) => {
                a_elements.len() == e_elements.len()
                    && a_elements
                        .iter()
                        .zip(e_elements.iter())
                        .all(|(a, e)| self.types_are_compatible(a, e))
            }

            // Function types compatibility (simplified)
            (
                StructuredType::Function {
                    params: a_params,
                    return_type: a_ret,
                },
                StructuredType::Function {
                    params: e_params,
                    return_type: e_ret,
                },
            ) => {
                a_params.len() == e_params.len()
                    && self.types_are_compatible(a_ret, e_ret)
                    && a_params
                        .iter()
                        .zip(e_params.iter())
                        .all(|(a, e)| self.types_are_compatible(&a.param_type, &e.param_type))
            }

            // Type errors never match
            (StructuredType::TypeError { .. }, _) | (_, StructuredType::TypeError { .. }) => false,

            // Different type kinds don't match
            _ => false,
        }
    }

    /// Create a StructuredType for a collection with the given element types
    pub fn create_list_type(&mut self, element_type: StructuredType) -> StructuredType {
        if self.primitive_types.list_base_id.is_none() {
            self.primitive_types.list_base_id =
                Some(self.compiler_environment.intern_type_name("List"));
        }
        StructuredType::Generic {
            base: self.primitive_types.list_base_id.clone().unwrap(),
            args: vec![element_type],
        }
    }

    /// Create a StructuredType for a map with the given key and value types
    pub fn create_map_type(
        &mut self,
        key_type: StructuredType,
        value_type: StructuredType,
    ) -> StructuredType {
        if self.primitive_types.map_base_id.is_none() {
            self.primitive_types.map_base_id =
                Some(self.compiler_environment.intern_type_name("Map"));
        }
        StructuredType::Generic {
            base: self.primitive_types.map_base_id.clone().unwrap(),
            args: vec![key_type, value_type],
        }
    }

    /// Create a StructuredType for a tuple with the given element types
    pub fn create_tuple_type(&mut self, element_types: Vec<StructuredType>) -> StructuredType {
        StructuredType::Tuple(element_types)
    }

    /// Get a reference to the compiler environment for advanced operations
    pub fn compiler_environment(&mut self) -> &mut CompilerEnvironment {
        &mut self.compiler_environment
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_value_to_structured_type() {
        let env = CompilerEnvironment::new();
        let mut integration = TypeIntegration::new(env);

        // Test primitive types
        let bool_val = Value::boolean(true);
        let bool_type = integration.value_to_structured_type(&bool_val);
        assert!(matches!(bool_type, StructuredType::Simple(_)));

        let int_val = Value::integer(42);
        let int_type = integration.value_to_structured_type(&int_val);
        assert!(matches!(int_type, StructuredType::Simple(_)));

        let float_val = Value::float(2.5);
        let float_type = integration.value_to_structured_type(&float_val);
        assert!(matches!(float_type, StructuredType::Simple(_)));

        let string_val = Value::string("hello".to_string());
        let string_type = integration.value_to_structured_type(&string_val);
        assert!(matches!(string_type, StructuredType::Simple(_)));
    }

    #[test]
    fn test_collection_value_to_structured_type() {
        let env = CompilerEnvironment::new();
        let mut integration = TypeIntegration::new(env.clone());

        // Create element type for list
        let element_type = integration.value_to_structured_type(&Value::integer(0));
        let list_val = Value::empty_list(element_type.clone());
        let list_type = integration.value_to_structured_type(&list_val);

        assert!(matches!(list_type, StructuredType::Generic { .. }));
        if let StructuredType::Generic { args, .. } = list_type {
            assert_eq!(args.len(), 1);
        }

        // Create key and value types for map
        let key_type = integration.value_to_structured_type(&Value::string(String::new()));
        let value_type = integration.value_to_structured_type(&Value::integer(0));
        let map_val = Value::empty_map(key_type.clone(), value_type.clone());
        let map_type = integration.value_to_structured_type(&map_val);

        assert!(matches!(map_type, StructuredType::Generic { .. }));
        if let StructuredType::Generic { args, .. } = map_type {
            assert_eq!(args.len(), 2);
        }
    }

    #[test]
    fn test_list_type_creation() {
        let env = CompilerEnvironment::new();

        // Create Integer type first
        let int_id = env.intern_type_name("Outrun.Core.Integer64");
        let int_type = StructuredType::Simple(int_id);

        let mut integration = TypeIntegration::new(env.clone());
        let list_type = integration.create_list_type(int_type.clone());

        // Verify the list type structure
        assert!(matches!(list_type, StructuredType::Generic { .. }));
        if let StructuredType::Generic { args, .. } = list_type {
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], int_type);
        }
    }

    #[test]
    fn test_map_type_creation() {
        let env = CompilerEnvironment::new();

        // Create types first
        let string_id = env.intern_type_name("Outrun.Core.String");
        let int_id = env.intern_type_name("Outrun.Core.Integer64");
        let string_type = StructuredType::Simple(string_id);
        let int_type = StructuredType::Simple(int_id);

        let mut integration = TypeIntegration::new(env.clone());
        let map_type = integration.create_map_type(string_type.clone(), int_type.clone());

        // Verify the map type structure
        assert!(matches!(map_type, StructuredType::Generic { .. }));
        if let StructuredType::Generic { args, .. } = map_type {
            assert_eq!(args.len(), 2);
            assert_eq!(args[0], string_type);
            assert_eq!(args[1], int_type);
        }
    }

    #[test]
    fn test_tuple_type_creation() {
        let env = CompilerEnvironment::new();

        // Create types first
        let string_id = env.intern_type_name("Outrun.Core.String");
        let int_id = env.intern_type_name("Outrun.Core.Integer64");
        let bool_id = env.intern_type_name("Outrun.Core.Boolean");

        let element_types = vec![
            StructuredType::Simple(string_id),
            StructuredType::Simple(int_id),
            StructuredType::Simple(bool_id),
        ];

        let mut integration = TypeIntegration::new(env.clone());
        let tuple_type = integration.create_tuple_type(element_types.clone());

        // Verify the tuple type structure
        assert!(matches!(tuple_type, StructuredType::Tuple(_)));
        if let StructuredType::Tuple(types) = tuple_type {
            assert_eq!(types.len(), 3);
            assert_eq!(types, element_types);
        }
    }

    #[test]
    fn test_type_compatibility() {
        let env = CompilerEnvironment::new();

        // Create types first
        let int_id = env.intern_type_name("Outrun.Core.Integer64");
        let string_id = env.intern_type_name("Outrun.Core.String");
        let list_id = env.intern_type_name("List");

        let type1 = StructuredType::Simple(int_id.clone());
        let type2 = StructuredType::Simple(int_id.clone());
        let type3 = StructuredType::Simple(string_id.clone());

        let integration = TypeIntegration::new(env);

        // Same simple types are compatible
        assert!(integration.types_are_compatible(&type1, &type2));

        // Different simple types are not compatible
        assert!(!integration.types_are_compatible(&type1, &type3));

        // Generic types with same base and args are compatible
        let list_type1 = StructuredType::Generic {
            base: list_id.clone(),
            args: vec![StructuredType::Simple(int_id.clone())],
        };
        let list_type2 = StructuredType::Generic {
            base: list_id.clone(),
            args: vec![StructuredType::Simple(int_id.clone())],
        };
        assert!(integration.types_are_compatible(&list_type1, &list_type2));

        // Generic types with different args are not compatible
        let list_type3 = StructuredType::Generic {
            base: list_id,
            args: vec![StructuredType::Simple(string_id)],
        };
        assert!(!integration.types_are_compatible(&list_type1, &list_type3));
    }

    #[test]
    fn test_value_type_validation() {
        let env = CompilerEnvironment::new();

        // Create types first
        let int_id = env.intern_type_name("Outrun.Core.Integer64");
        let string_id = env.intern_type_name("Outrun.Core.String");
        let int_type = StructuredType::Simple(int_id.clone());
        let string_type = StructuredType::Simple(string_id);

        let mut integration = TypeIntegration::new(env.clone());

        // Validate that an integer value matches Integer type
        let int_val = Value::integer(42);
        assert!(integration.validate_value_type(&int_val, &int_type));

        // Validate that an integer value doesn't match String type
        assert!(!integration.validate_value_type(&int_val, &string_type));

        // Validate list types
        let element_type = StructuredType::Simple(int_id);
        let list_val = Value::empty_list(element_type.clone());
        let list_type = integration.create_list_type(element_type);
        assert!(integration.validate_value_type(&list_val, &list_type));
    }

    #[test]
    fn test_type_integration_basic_functionality() {
        let env = CompilerEnvironment::new();
        let mut integration = TypeIntegration::new(env.clone());

        // Test that we can create type structures and convert values
        let int_val = Value::integer(42);
        let int_type = integration.value_to_structured_type(&int_val);

        // Test validation works
        assert!(integration.validate_value_type(&int_val, &int_type));

        // Test that type creation functions work
        let list_type = integration.create_list_type(int_type);
        assert!(matches!(list_type, StructuredType::Generic { .. }));
    }
}
