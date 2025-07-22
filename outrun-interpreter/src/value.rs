//! Runtime value representation for the Outrun interpreter
//!
//! This module defines the `Value` enum which represents all possible runtime values
//! in Outrun programs. Values are designed to be efficient for functional programming
//! with reference counting for memory sharing.

use crate::list::List;
use outrun_typechecker::{
    compilation::compiler_environment::{AtomId, TypeNameId},
    unification::StructuredType,
};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// Runtime value representation for all Outrun values
///
/// This enum represents every possible value that can exist at runtime in Outrun.
/// It uses reference counting (Rc) for efficient memory sharing in functional programming.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // Primitive types
    /// 64-bit signed integer (Outrun.Core.Integer64)
    Integer64(i64),
    /// 64-bit floating point (Outrun.Core.Float64)
    Float64(f64),
    /// Boolean value (Outrun.Core.Boolean)
    Boolean(bool),
    /// String value (Outrun.Core.String)
    String(String),
    /// Atom value (Outrun.Core.Atom)
    Atom(AtomId),

    // Collection types with reference counting for efficient sharing
    /// Linked list (Outrun.Core.List<T>)
    List {
        /// The actual linked list data (using Rc for efficient sharing)
        list: Rc<List<Value>>,
        /// Element type for type checking
        element_type: StructuredType,
    },
    /// Map/dictionary (Outrun.Core.Map<K, V>)
    Map {
        /// The map data using reference counting for efficient sharing
        entries: Rc<HashMap<Value, Value>>,
        /// Key type for type checking
        key_type: StructuredType,
        /// Value type for type checking
        value_type: StructuredType,
    },
    /// Tuple (Outrun.Core.Tuple<T1, T2, ...>)
    Tuple {
        /// Tuple elements using reference counting
        elements: Rc<Vec<Value>>,
        /// Complete tuple type for type checking
        tuple_type: StructuredType,
    },

    // Struct instances (created by static protocol functions like Option.some, Result.ok, etc.)
    /// Generic struct instance
    Struct {
        /// Type identifier for this struct
        type_id: TypeNameId,
        /// Field name -> value mapping using reference counting
        fields: Rc<HashMap<AtomId, Value>>,
        /// Complete struct type with generics
        struct_type: StructuredType,
    },
}

impl Value {
    // Constructors for primitive types

    /// Create an integer value
    pub fn integer(value: i64) -> Self {
        Value::Integer64(value)
    }

    /// Create a float value
    pub fn float(value: f64) -> Self {
        Value::Float64(value)
    }

    /// Create a boolean value
    pub fn boolean(value: bool) -> Self {
        Value::Boolean(value)
    }

    /// Create a string value
    pub fn string(value: String) -> Self {
        Value::String(value)
    }

    /// Create an atom value
    pub fn atom(atom_id: AtomId) -> Self {
        Value::Atom(atom_id)
    }

    // Constructors for collection types

    /// Create an empty list with the given element type
    pub fn empty_list(element_type: StructuredType) -> Self {
        Value::List {
            list: Rc::new(List::new()),
            element_type,
        }
    }

    /// Create a list from a vector of values
    pub fn list_from_vec(values: Vec<Value>, element_type: StructuredType) -> Self {
        Value::List {
            list: Rc::new(List::from_vec(values)),
            element_type,
        }
    }

    /// Create a Value from an existing List instance
    pub fn from_list(list: List<Value>, element_type: StructuredType) -> Self {
        Value::List {
            list: Rc::new(list),
            element_type,
        }
    }

    /// Create an empty map with the given key and value types
    pub fn empty_map(key_type: StructuredType, value_type: StructuredType) -> Self {
        Value::Map {
            entries: Rc::new(HashMap::new()),
            key_type,
            value_type,
        }
    }

    /// Create a map from a HashMap
    pub fn map_from_hashmap(
        map: HashMap<Value, Value>,
        key_type: StructuredType,
        value_type: StructuredType,
    ) -> Self {
        Value::Map {
            entries: Rc::new(map),
            key_type,
            value_type,
        }
    }

    /// Create a tuple from a vector of values
    pub fn tuple_from_vec(values: Vec<Value>, tuple_type: StructuredType) -> Self {
        Value::Tuple {
            elements: Rc::new(values),
            tuple_type,
        }
    }

    /// Create a tuple with a mock type (for testing)
    pub fn tuple(values: Vec<Value>) -> Self {
        use outrun_typechecker::{
            compilation::compiler_environment::CompilerEnvironment, unification::StructuredType,
        };
        let env = CompilerEnvironment::new();
        let mock_type = StructuredType::Simple(env.intern_type_name("MockTuple"));
        Self::tuple_from_vec(values, mock_type)
    }

    /// Create a struct instance
    pub fn struct_instance(
        type_id: TypeNameId,
        fields: HashMap<AtomId, Value>,
        struct_type: StructuredType,
    ) -> Self {
        Value::Struct {
            type_id,
            fields: Rc::new(fields),
            struct_type,
        }
    }

    // Type checking and introspection

    /// Get the StructuredType for this value (for complex types only)
    ///
    /// Note: This method can only return StructuredType for complex types (List, Map, Tuple, Struct)
    /// that store their type information. For primitive types, use `TypeIntegration::value_to_structured_type()`.
    ///
    /// Returns None for primitive types that require TypeInterner integration.
    pub fn get_structured_type_cached(&self) -> Option<&StructuredType> {
        match self {
            // Primitive types need TypeInterner access - use TypeIntegration::value_to_structured_type()
            Value::Integer64(_)
            | Value::Float64(_)
            | Value::Boolean(_)
            | Value::String(_)
            | Value::Atom(_) => None,

            // Complex types store their StructuredType directly
            Value::List { element_type, .. } => Some(element_type),
            Value::Map { key_type, .. } => Some(key_type), // Note: Returns key type, use TypeIntegration for full Map type
            Value::Tuple { tuple_type, .. } => Some(tuple_type),
            Value::Struct { struct_type, .. } => Some(struct_type),
        }
    }

    /// Check if this value is a specific primitive type
    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Integer64(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Value::Float64(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Boolean(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_atom(&self) -> bool {
        matches!(self, Value::Atom(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Value::List { .. })
    }

    pub fn is_map(&self) -> bool {
        matches!(self, Value::Map { .. })
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self, Value::Tuple { .. })
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Value::Struct { .. })
    }

    /// Get a human-readable type name for this value
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer64(_) => "Integer64",
            Value::Float64(_) => "Float64",
            Value::Boolean(_) => "Boolean",
            Value::String(_) => "String",
            Value::Atom(_) => "Atom",
            Value::List { .. } => "List",
            Value::Map { .. } => "Map",
            Value::Tuple { .. } => "Tuple",
            Value::Struct { .. } => "Struct",
        }
    }

    // Value extraction (with error handling)

    /// Extract integer value
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Value::Integer64(n) => Some(*n),
            _ => None,
        }
    }

    /// Extract float value
    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float64(f) => Some(*f),
            _ => None,
        }
    }

    /// Extract boolean value
    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            Value::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    /// Extract string value
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Extract atom value
    pub fn as_atom(&self) -> Option<AtomId> {
        match self {
            Value::Atom(atom_id) => Some(atom_id.clone()),
            _ => None,
        }
    }

    /// Extract list value
    pub fn as_list(&self) -> Option<(&List<Value>, &StructuredType)> {
        match self {
            Value::List { list, element_type } => Some((list.as_ref(), element_type)),
            _ => None,
        }
    }

    /// Extract map value
    pub fn as_map(&self) -> Option<(&HashMap<Value, Value>, &StructuredType, &StructuredType)> {
        match self {
            Value::Map {
                entries,
                key_type,
                value_type,
            } => Some((entries, key_type, value_type)),
            _ => None,
        }
    }

    /// Extract tuple value
    pub fn as_tuple(&self) -> Option<(&Vec<Value>, &StructuredType)> {
        match self {
            Value::Tuple {
                elements,
                tuple_type,
            } => Some((elements, tuple_type)),
            _ => None,
        }
    }

    /// Extract struct value
    pub fn as_struct(&self) -> Option<(TypeNameId, &HashMap<AtomId, Value>, &StructuredType)> {
        match self {
            Value::Struct {
                type_id,
                fields,
                struct_type,
            } => Some((type_id.clone(), fields, struct_type)),
            _ => None,
        }
    }

    // Collection operations for list values (simplified for intrinsics)

    /// Get the head of a list value
    pub fn list_head(&self) -> Option<Option<&Value>> {
        match self {
            Value::List { list, .. } => Some(list.head()),
            _ => None,
        }
    }

    /// Get the tail of a list value as a new List value
    pub fn list_tail(&self) -> Option<Value> {
        match self {
            Value::List { list, element_type } => {
                if let Some(tail) = list.tail() {
                    Some(Value::List {
                        list: Rc::new(tail.clone()),
                        element_type: element_type.clone(),
                    })
                } else {
                    Some(Value::empty_list(element_type.clone()))
                }
            }
            _ => None,
        }
    }

    /// Get the length of a list value
    pub fn list_length(&self) -> Option<usize> {
        match self {
            Value::List { list, .. } => Some(list.len()),
            _ => None,
        }
    }

    /// Check if a list is empty
    pub fn list_is_empty(&self) -> Option<bool> {
        match self {
            Value::List { list, .. } => Some(list.is_empty()),
            _ => None,
        }
    }

    /// Prepend a value to a list, returning a new list value
    pub fn list_prepend(&self, value: Value) -> Option<Value> {
        match self {
            Value::List { list, element_type } => {
                let new_list = List::cons(value, list.as_ref().clone());
                Some(Value::List {
                    list: Rc::new(new_list),
                    element_type: element_type.clone(),
                })
            }
            _ => None,
        }
    }

    // Map operations

    /// Get a value from a map
    ///
    /// Note: If the key is a Float64 with NaN value, this will always return Some(None)
    /// even if a NaN key was previously inserted, due to IEEE 754 NaN != NaN property.
    /// This matches Python dictionary behavior.
    pub fn map_get(&self, key: &Value) -> Option<Option<&Value>> {
        match self {
            Value::Map { entries, .. } => Some(entries.get(key)),
            _ => None,
        }
    }

    /// Insert a key-value pair into a map, returning a new map value
    ///
    /// Note: NaN keys can be inserted but will never be retrievable via map_get()
    /// due to IEEE 754 NaN != NaN property. This matches Python dictionary behavior.
    pub fn map_insert(&self, key: Value, value: Value) -> Option<Value> {
        match self {
            Value::Map {
                entries,
                key_type,
                value_type,
            } => {
                let mut new_entries = (**entries).clone();
                new_entries.insert(key, value);
                Some(Value::Map {
                    entries: Rc::new(new_entries),
                    key_type: key_type.clone(),
                    value_type: value_type.clone(),
                })
            }
            _ => None,
        }
    }

    /// Get the size of a map
    pub fn map_size(&self) -> Option<usize> {
        match self {
            Value::Map { entries, .. } => Some(entries.len()),
            _ => None,
        }
    }

    // Tuple operations

    /// Get an element from a tuple by index
    pub fn tuple_get(&self, index: usize) -> Option<Option<&Value>> {
        match self {
            Value::Tuple { elements, .. } => Some(elements.get(index)),
            _ => None,
        }
    }

    /// Get the length of a tuple
    pub fn tuple_length(&self) -> Option<usize> {
        match self {
            Value::Tuple { elements, .. } => Some(elements.len()),
            _ => None,
        }
    }

    // Struct operations

    /// Get a field from a struct
    pub fn struct_get_field(&self, field_name: AtomId) -> Option<Option<&Value>> {
        match self {
            Value::Struct { fields, .. } => Some(fields.get(&field_name)),
            _ => None,
        }
    }

    /// Get the type ID of a struct
    pub fn struct_type_id(&self) -> Option<TypeNameId> {
        match self {
            Value::Struct { type_id, .. } => Some(type_id.clone()),
            _ => None,
        }
    }

    /// Update a field in a struct, returning a new struct value
    pub fn struct_update_field(&self, field_name: AtomId, new_value: Value) -> Option<Value> {
        match self {
            Value::Struct {
                type_id,
                fields,
                struct_type,
            } => {
                let mut new_fields = (**fields).clone();
                new_fields.insert(field_name, new_value);
                Some(Value::Struct {
                    type_id: type_id.clone(),
                    fields: Rc::new(new_fields),
                    struct_type: struct_type.clone(),
                })
            }
            _ => None,
        }
    }
}

// Manual Eq implementation to handle f64 (which doesn't implement Eq due to NaN)
impl Eq for Value {}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer64(n) => write!(f, "{n}"),
            Value::Float64(fl) => write!(f, "{fl}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Atom(_) => write!(f, ":atom"), // TODO: Get atom name from interner
            Value::List { list, .. } => write!(f, "{list}"),
            Value::Map { entries, .. } => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in entries.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key} => {value}")?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Tuple { elements, .. } => {
                write!(f, "(")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{element}")?;
                }
                if elements.len() == 1 {
                    write!(f, ",")?; // Trailing comma for single-element tuples
                }
                write!(f, ")")
            }
            Value::Struct { fields, .. } => {
                // For struct display, we'll show it like a map for now
                // TODO: Better struct display with type names
                write!(f, "Struct{{")?;
                let mut first = true;
                for (field_name, value) in fields.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "field_{field_name:?}: {value}")?; // TODO: Get field name from interner
                    first = false;
                }
                write!(f, "}}")
            }
        }
    }
}

// Implement Hash for Value to enable using it as map keys
// Note: This is a simplified implementation - in a real system we'd need to be more careful
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Integer64(n) => {
                "integer".hash(state);
                n.hash(state);
            }
            Value::Float64(f) => {
                "float".hash(state);
                // Normalize +0.0 and -0.0 to have the same hash since they're equal
                // This ensures 0.0 == -0.0 implies hash(0.0) == hash(-0.0)
                let normalized = if *f == 0.0 { 0.0 } else { *f };
                normalized.to_bits().hash(state);
            }
            Value::Boolean(b) => {
                "boolean".hash(state);
                b.hash(state);
            }
            Value::String(s) => {
                "string".hash(state);
                s.hash(state);
            }
            Value::Atom(atom_id) => {
                "atom".hash(state);
                atom_id.hash(state);
            }
            // Collections are more complex to hash - we'll implement simple versions
            Value::List { list, .. } => {
                "list".hash(state);
                // Hash each element in the list
                for value in list.iter() {
                    value.hash(state);
                }
            }
            Value::Map { entries, .. } => {
                "map".hash(state);
                // Hash all key-value pairs (order independent)
                let mut pairs: Vec<_> = entries.iter().collect();
                pairs.sort_by_key(|(k, _)| format!("{k:?}")); // Simple ordering
                for (key, value) in pairs {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Tuple { elements, .. } => {
                "tuple".hash(state);
                for element in elements.iter() {
                    element.hash(state);
                }
            }
            Value::Struct {
                type_id, fields, ..
            } => {
                "struct".hash(state);
                type_id.hash(state);
                // Hash all field-value pairs (order independent)
                let mut pairs: Vec<_> = fields.iter().collect();
                pairs.sort_by_key(|(k, _)| format!("{k:?}")); // Sort by AtomId debug representation
                for (field, value) in pairs {
                    field.hash(state);
                    value.hash(state);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_typechecker::compilation::compiler_environment::CompilerEnvironment;
    use outrun_typechecker::unification::StructuredType;

    // Helper function to create a simple structured type for testing
    fn simple_type(name: &str) -> StructuredType {
        let env = CompilerEnvironment::new();
        let type_id = env.intern_type_name(name);
        StructuredType::Simple(type_id)
    }

    #[test]
    fn test_primitive_values() {
        let int_val = Value::integer(42);
        assert!(int_val.is_integer());
        assert_eq!(int_val.as_integer(), Some(42));
        assert!(!int_val.is_string());

        let float_val = Value::float(2.5);
        assert!(float_val.is_float());
        assert_eq!(float_val.as_float(), Some(2.5));

        let bool_val = Value::boolean(true);
        assert!(bool_val.is_boolean());
        assert_eq!(bool_val.as_boolean(), Some(true));

        let string_val = Value::string("hello".to_string());
        assert!(string_val.is_string());
        assert_eq!(string_val.as_string(), Some("hello"));
    }

    #[test]
    fn test_list_values() {
        let element_type = simple_type("Integer");
        let empty_list = Value::empty_list(element_type.clone());

        assert!(empty_list.is_list());
        assert_eq!(empty_list.list_is_empty(), Some(true));
        assert_eq!(empty_list.list_length(), Some(0));
        assert_eq!(empty_list.list_head(), Some(None));

        let values = vec![Value::integer(1), Value::integer(2), Value::integer(3)];
        let list_val = Value::list_from_vec(values, element_type);

        assert!(list_val.is_list());
        assert_eq!(list_val.list_is_empty(), Some(false));
        assert_eq!(list_val.list_length(), Some(3));
        assert_eq!(list_val.list_head(), Some(Some(&Value::integer(1))));
    }

    #[test]
    fn test_list_prepend() {
        let element_type = simple_type("Integer");
        let empty_list = Value::empty_list(element_type);

        let list_with_one = empty_list.list_prepend(Value::integer(42)).unwrap();
        assert_eq!(list_with_one.list_length(), Some(1));
        assert_eq!(list_with_one.list_head(), Some(Some(&Value::integer(42))));

        let list_with_two = list_with_one.list_prepend(Value::integer(1)).unwrap();
        assert_eq!(list_with_two.list_length(), Some(2));
        assert_eq!(list_with_two.list_head(), Some(Some(&Value::integer(1))));
    }

    #[test]
    fn test_map_values() {
        let key_type = simple_type("String");
        let value_type = simple_type("Integer");
        let empty_map = Value::empty_map(key_type, value_type);

        assert!(empty_map.is_map());
        assert_eq!(empty_map.map_size(), Some(0));

        let key = Value::string("test".to_string());
        let value = Value::integer(42);
        let map_with_one = empty_map.map_insert(key.clone(), value.clone()).unwrap();

        assert_eq!(map_with_one.map_size(), Some(1));
        assert_eq!(map_with_one.map_get(&key), Some(Some(&value)));

        let missing_key = Value::string("missing".to_string());
        assert_eq!(map_with_one.map_get(&missing_key), Some(None));
    }

    #[test]
    fn test_tuple_values() {
        let tuple_type = simple_type("Tuple");
        let values = vec![
            Value::integer(1),
            Value::string("hello".to_string()),
            Value::boolean(true),
        ];
        let tuple_val = Value::tuple_from_vec(values, tuple_type);

        assert!(tuple_val.is_tuple());
        assert_eq!(tuple_val.tuple_length(), Some(3));
        assert_eq!(tuple_val.tuple_get(0), Some(Some(&Value::integer(1))));
        assert_eq!(
            tuple_val.tuple_get(1),
            Some(Some(&Value::string("hello".to_string())))
        );
        assert_eq!(tuple_val.tuple_get(2), Some(Some(&Value::boolean(true))));
        assert_eq!(tuple_val.tuple_get(3), Some(None));
    }

    #[test]
    fn test_struct_values() {
        let env = CompilerEnvironment::new();
        let type_id = env.intern_type_name("TestStruct");
        let field_id = env.intern_atom_name("field1");

        let mut fields = HashMap::new();
        fields.insert(field_id.clone(), Value::integer(42));

        let struct_type = simple_type("TestStruct");
        let struct_val = Value::struct_instance(type_id.clone(), fields, struct_type);

        assert!(struct_val.is_struct());
        assert_eq!(struct_val.struct_type_id(), Some(type_id));
        assert_eq!(
            struct_val.struct_get_field(field_id.clone()),
            Some(Some(&Value::integer(42)))
        );

        let missing_field = env.intern_atom_name("missing");
        assert_eq!(struct_val.struct_get_field(missing_field), Some(None));
    }

    #[test]
    fn test_struct_field_update() {
        let env = CompilerEnvironment::new();
        let type_id = env.intern_type_name("TestStruct");
        let field_id = env.intern_atom_name("field1");

        let mut fields = HashMap::new();
        fields.insert(field_id.clone(), Value::integer(42));

        let struct_type = simple_type("TestStruct");
        let struct_val = Value::struct_instance(type_id.clone(), fields, struct_type);

        let updated_struct = struct_val
            .struct_update_field(field_id.clone(), Value::integer(100))
            .unwrap();

        // Original struct should be unchanged
        assert_eq!(
            struct_val.struct_get_field(field_id.clone()),
            Some(Some(&Value::integer(42)))
        );

        // Updated struct should have new value
        assert_eq!(
            updated_struct.struct_get_field(field_id),
            Some(Some(&Value::integer(100)))
        );
    }

    #[test]
    fn test_value_display() {
        assert_eq!(format!("{}", Value::integer(42)), "42");
        assert_eq!(format!("{}", Value::float(2.5)), "2.5");
        assert_eq!(format!("{}", Value::boolean(true)), "true");
        assert_eq!(
            format!("{}", Value::string("hello".to_string())),
            "\"hello\""
        );

        let values = vec![Value::integer(1), Value::integer(2)];
        let list_val = Value::list_from_vec(values, simple_type("Integer"));
        assert_eq!(format!("{list_val}"), "[1, 2]");

        let tuple_val = Value::tuple_from_vec(
            vec![Value::integer(1), Value::string("test".to_string())],
            simple_type("Tuple"),
        );
        assert_eq!(format!("{tuple_val}"), "(1, \"test\")");
    }

    #[test]
    fn test_value_equality() {
        let val1 = Value::integer(42);
        let val2 = Value::integer(42);
        let val3 = Value::integer(43);

        assert_eq!(val1, val2);
        assert_ne!(val1, val3);

        let list1 = Value::list_from_vec(
            vec![Value::integer(1), Value::integer(2)],
            simple_type("Integer"),
        );
        let list2 = Value::list_from_vec(
            vec![Value::integer(1), Value::integer(2)],
            simple_type("Integer"),
        );
        let list3 = Value::list_from_vec(
            vec![Value::integer(1), Value::integer(3)],
            simple_type("Integer"),
        );

        assert_eq!(list1, list2);
        assert_ne!(list1, list3);
    }

    #[test]
    fn test_immutability() {
        // Test that operations return new values without modifying originals
        let original_list = Value::list_from_vec(vec![Value::integer(1)], simple_type("Integer"));
        let new_list = original_list.list_prepend(Value::integer(2)).unwrap();

        assert_eq!(original_list.list_length(), Some(1));
        assert_eq!(new_list.list_length(), Some(2));

        let original_map = Value::empty_map(simple_type("String"), simple_type("Integer"));
        let new_map = original_map
            .map_insert(Value::string("key".to_string()), Value::integer(42))
            .unwrap();

        assert_eq!(original_map.map_size(), Some(0));
        assert_eq!(new_map.map_size(), Some(1));
    }

    #[test]
    fn test_nan_map_behavior() {
        // Test that NaN keys exhibit Python-like behavior: can insert but never retrieve
        let key_type = simple_type("Float");
        let value_type = simple_type("String");
        let empty_map = Value::empty_map(key_type, value_type);

        let nan_key = Value::float(f64::NAN);
        let test_value = Value::string("unreachable".to_string());

        // NaN can be inserted as a key
        let map_with_nan = empty_map
            .map_insert(nan_key.clone(), test_value.clone())
            .unwrap();
        assert_eq!(map_with_nan.map_size(), Some(1));

        // But NaN key lookups always return None due to NaN != NaN
        assert_eq!(map_with_nan.map_get(&nan_key), Some(None));

        // Even a fresh NaN with the same bit pattern fails to match
        let another_nan = Value::float(f64::NAN);
        assert_eq!(map_with_nan.map_get(&another_nan), Some(None));

        // Regular floats work normally
        let normal_key = Value::float(1.5);
        let map_with_normal = empty_map
            .map_insert(normal_key.clone(), test_value.clone())
            .unwrap();
        assert_eq!(
            map_with_normal.map_get(&normal_key),
            Some(Some(&test_value))
        );
    }

    #[test]
    fn test_float_edge_cases_in_maps() {
        let key_type = simple_type("Float");
        let value_type = simple_type("Integer");
        let empty_map = Value::empty_map(key_type, value_type);

        // Test positive and negative zero
        let pos_zero = Value::float(0.0);
        let neg_zero = Value::float(-0.0);
        let test_value = Value::integer(42);

        let map_with_pos_zero = empty_map
            .map_insert(pos_zero.clone(), test_value.clone())
            .unwrap();

        // In Rust, 0.0 == -0.0 is true, so both should retrieve the same value
        assert_eq!(
            map_with_pos_zero.map_get(&pos_zero),
            Some(Some(&test_value))
        );
        assert_eq!(
            map_with_pos_zero.map_get(&neg_zero),
            Some(Some(&test_value))
        );

        // Test infinity values
        let pos_inf = Value::float(f64::INFINITY);
        let neg_inf = Value::float(f64::NEG_INFINITY);

        let map_with_inf = empty_map
            .map_insert(pos_inf.clone(), test_value.clone())
            .unwrap();
        assert_eq!(map_with_inf.map_get(&pos_inf), Some(Some(&test_value)));
        assert_eq!(map_with_inf.map_get(&neg_inf), Some(None)); // Different infinities
    }
}
