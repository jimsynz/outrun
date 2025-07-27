//! Runtime value representation for the new Outrun interpreter
//!
//! This module provides the `Value` enum which represents all possible runtime values
//! in Outrun programs. It's designed to integrate with typechecker v3's type system
//! while maintaining the functional programming patterns from the original interpreter.

use std::collections::HashMap;
use std::rc::Rc;

/// Runtime value representation compatible with typechecker v3
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
    /// Atom value (Outrun.Core.Atom) - represented as string for now
    Atom(String),

    // Collection types with reference counting for efficient sharing
    /// Linked list (Outrun.Core.List<T>)
    List {
        /// The actual linked list data (using Rc for efficient sharing)
        list: Rc<List>,
        /// Element type information for integration with typechecker v3
        element_type_info: Option<outrun_parser::ParsedTypeInfo>,
    },

    /// Map/dictionary (Outrun.Core.Map<K, V>)
    /// Note: For now we'll store as Vec of key-value pairs to avoid Hash issues
    Map {
        /// The map data as vector of key-value pairs
        entries: Rc<Vec<(Value, Value)>>,
        /// Key type information
        key_type_info: Option<outrun_parser::ParsedTypeInfo>,
        /// Value type information
        value_type_info: Option<outrun_parser::ParsedTypeInfo>,
    },

    /// Tuple (Outrun.Core.Tuple<T1, T2, ...>)
    Tuple {
        /// The tuple elements in order
        elements: Vec<Value>,
        /// Type information for each element
        element_type_info: Vec<Option<outrun_parser::ParsedTypeInfo>>,
    },

    /// Struct values created by protocol functions
    /// This represents concrete types like Outrun.Option.Some<T>
    Struct {
        /// Type name for dispatch and pattern matching
        type_name: String,
        /// Field values by name
        fields: HashMap<String, Value>,
        /// Type information from typechecker v3
        type_info: Option<outrun_parser::ParsedTypeInfo>,
    },

    /// Function values (for higher-order functions and captures)
    Function {
        /// Function name or identifier
        name: String,
        /// Captured environment (for closures)
        captures: HashMap<String, Value>,
        /// Function type information
        type_info: Option<outrun_parser::ParsedTypeInfo>,
    },
}

/// Functional linked list implementation for Outrun lists
///
/// This maintains the O(1) head/tail access pattern that's essential
/// for functional programming in Outrun.
#[derive(Debug, Clone, PartialEq)]
pub enum List {
    /// Empty list
    Empty,
    /// Cons cell with head value and tail reference
    Cons { head: Value, tail: Rc<List> },
}

impl Value {
    /// Create a new integer value
    pub fn integer(value: i64) -> Self {
        Value::Integer64(value)
    }

    /// Create a new float value
    pub fn float(value: f64) -> Self {
        Value::Float64(value)
    }

    /// Create a new boolean value
    pub fn boolean(value: bool) -> Self {
        Value::Boolean(value)
    }

    /// Create a new string value
    pub fn string(value: String) -> Self {
        Value::String(value)
    }

    /// Create a new atom value
    pub fn atom(value: String) -> Self {
        Value::Atom(value)
    }

    /// Create an empty list
    pub fn empty_list() -> Self {
        Value::List {
            list: Rc::new(List::Empty),
            element_type_info: None,
        }
    }

    /// Create a list with a single element
    pub fn single_list(element: Value) -> Self {
        Value::List {
            list: Rc::new(List::Cons {
                head: element,
                tail: Rc::new(List::Empty),
            }),
            element_type_info: None,
        }
    }

    /// Create an empty map
    pub fn empty_map() -> Self {
        Value::Map {
            entries: Rc::new(Vec::new()),
            key_type_info: None,
            value_type_info: None,
        }
    }

    /// Create a tuple with the given elements
    pub fn tuple(elements: Vec<Value>) -> Self {
        let element_type_info = vec![None; elements.len()];
        Value::Tuple {
            elements,
            element_type_info,
        }
    }

    /// Get the type name of this value for dispatch
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
            Value::Function { .. } => "Function",
        }
    }

    /// Check if this value represents a truthy value
    /// In Outrun, only Boolean true is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            _ => false, // Only boolean true is truthy in Outrun
        }
    }

    /// Get a display representation for REPL output
    pub fn display(&self) -> String {
        match self {
            Value::Integer64(n) => n.to_string(),
            Value::Float64(f) => f.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Atom(a) => format!(":{}", a),
            Value::List { list, .. } => format_list(list),
            Value::Map { entries, .. } => format_map(entries),
            Value::Tuple { elements, .. } => format_tuple(elements),
            Value::Struct {
                type_name, fields, ..
            } => {
                format!("{}{{ {} }}", type_name, format_fields(fields))
            }
            Value::Function { name, .. } => format!("<function {}>", name),
        }
    }
}

/// Format a list for display
fn format_list(list: &List) -> String {
    let mut elements = Vec::new();
    let mut current = list;

    loop {
        match current {
            List::Empty => break,
            List::Cons { head, tail } => {
                elements.push(head.display());
                current = tail;
            }
        }
    }

    format!("[{}]", elements.join(", "))
}

/// Format a map for display
fn format_map(entries: &[(Value, Value)]) -> String {
    let formatted_entries: Vec<String> = entries
        .iter()
        .map(|(k, v)| format!("{}: {}", k.display(), v.display()))
        .collect();
    format!("{{{}}}", formatted_entries.join(", "))
}

/// Format struct fields for display
fn format_fields(fields: &HashMap<String, Value>) -> String {
    let formatted_fields: Vec<String> = fields
        .iter()
        .map(|(k, v)| format!("{}: {}", k, v.display()))
        .collect();
    formatted_fields.join(", ")
}

/// Format a tuple for display
fn format_tuple(elements: &[Value]) -> String {
    let formatted_elements: Vec<String> = elements
        .iter()
        .map(|v| v.display())
        .collect();
    format!("({})", formatted_elements.join(", "))
}

impl List {
    /// Get the head of the list (first element)
    pub fn head(&self) -> Option<&Value> {
        match self {
            List::Empty => None,
            List::Cons { head, .. } => Some(head),
        }
    }

    /// Get the tail of the list (all elements except first)
    pub fn tail(&self) -> Option<&List> {
        match self {
            List::Empty => None,
            List::Cons { tail, .. } => Some(tail),
        }
    }

    /// Check if the list is empty
    pub fn is_empty(&self) -> bool {
        matches!(self, List::Empty)
    }

    /// Get the length of the list (O(n) operation)
    pub fn len(&self) -> usize {
        let mut count = 0;
        let mut current = self;

        loop {
            match current {
                List::Empty => break count,
                List::Cons { tail, .. } => {
                    count += 1;
                    current = tail;
                }
            }
        }
    }

    /// Prepend an element to the front of the list (O(1) operation)
    pub fn prepend(self: Rc<Self>, element: Value) -> List {
        List::Cons {
            head: element,
            tail: self,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_value_creation() {
        let int_val = Value::integer(42);
        assert_eq!(int_val.type_name(), "Integer64");
        assert_eq!(int_val.display(), "42");

        let bool_val = Value::boolean(true);
        assert!(bool_val.is_truthy());

        let false_val = Value::boolean(false);
        assert!(!false_val.is_truthy());
    }

    #[test]
    fn test_list_operations() {
        let empty = Value::empty_list();
        if let Value::List { list, .. } = &empty {
            assert!(list.is_empty());
            assert_eq!(list.len(), 0);
        } else {
            panic!("Expected list value");
        }

        let single = Value::single_list(Value::integer(42));
        if let Value::List { list, .. } = &single {
            assert!(!list.is_empty());
            assert_eq!(list.len(), 1);
            assert_eq!(list.head(), Some(&Value::integer(42)));
        } else {
            panic!("Expected list value");
        }
    }

    #[test]
    fn test_value_display() {
        assert_eq!(Value::integer(42).display(), "42");
        assert_eq!(Value::string("hello".to_string()).display(), "\"hello\"");
        assert_eq!(Value::atom("test".to_string()).display(), ":test");
        assert_eq!(Value::boolean(true).display(), "true");
    }

    #[test]
    fn test_tuple_creation_and_display() {
        // Test empty tuple
        let empty_tuple = Value::tuple(vec![]);
        assert_eq!(empty_tuple.display(), "()");
        assert_eq!(empty_tuple.type_name(), "Tuple");

        // Test single element tuple
        let single_tuple = Value::tuple(vec![Value::integer(42)]);
        assert_eq!(single_tuple.display(), "(42)");

        // Test two element tuple
        let pair_tuple = Value::tuple(vec![Value::integer(1), Value::integer(2)]);
        assert_eq!(pair_tuple.display(), "(1, 2)");

        // Test mixed type tuple
        let mixed_tuple = Value::tuple(vec![
            Value::integer(42),
            Value::string("hello".to_string()),
            Value::boolean(true),
        ]);
        assert_eq!(mixed_tuple.display(), "(42, \"hello\", true)");
    }
}
