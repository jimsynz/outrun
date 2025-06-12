//! Runtime value representation for the Outrun interpreter.
//!
//! This module defines the Value enum that represents all possible
//! runtime values in Outrun, along with operations for type checking,
//! arithmetic, comparisons, and conversions.

use crate::error::{Result, RuntimeError};
use indexmap::IndexMap;
use std::cmp::Ordering;

/// Runtime values in the Outrun interpreter
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// 64-bit signed integer
    Integer(i64),
    /// 64-bit floating point number
    Float(f64),
    /// Boolean value
    Boolean(bool),
    /// UTF-8 string
    String(String),
    /// Atom (symbol) - interned string starting with ':'
    Atom(String),

    /// Ordered list of values
    List(Vec<Value>),
    /// Fixed-size tuple of values  
    Tuple(Vec<Value>),
    /// Ordered map with Value keys and values
    Map(IndexMap<Value, Value>),

    /// Built-in function
    BuiltinFunction {
        name: String,
        arity: usize,
        function: fn(&[Value]) -> Result<Value>,
    },

    /// Unit value (no meaningful result)
    Unit,

    /// Represents absence of value (for Maybe-like operations)
    None,
}

impl Value {
    /// Get the type name of this value
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "Integer",
            Value::Float(_) => "Float",
            Value::Boolean(_) => "Boolean",
            Value::String(_) => "String",
            Value::Atom(_) => "Atom",
            Value::List(_) => "List",
            Value::Tuple(_) => "Tuple",
            Value::Map(_) => "Map",
            Value::BuiltinFunction { .. } => "Function",
            Value::Unit => "Unit",
            Value::None => "None",
        }
    }

    /// Check if this value is truthy (for if expressions, logical operators)
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::None => false,
            Value::Unit => false,
            Value::Integer(n) => *n != 0,
            Value::Float(f) => *f != 0.0 && !f.is_nan(),
            Value::String(s) => !s.is_empty(),
            Value::List(list) => !list.is_empty(),
            Value::Tuple(tuple) => !tuple.is_empty(),
            Value::Map(map) => !map.is_empty(),
            Value::Atom(_) => true,
            Value::BuiltinFunction { .. } => true,
        }
    }

    /// Convert to string representation (for string interpolation)
    pub fn to_string_repr(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                if f.fract() == 0.0 {
                    format!("{:.1}", f) // Show 1.0 instead of 1
                } else {
                    f.to_string()
                }
            }
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::Atom(name) => format!(":{}", name),
            Value::List(items) => {
                let item_strings: Vec<String> = items.iter().map(|v| v.to_string_repr()).collect();
                format!("[{}]", item_strings.join(", "))
            }
            Value::Tuple(items) => {
                let item_strings: Vec<String> = items.iter().map(|v| v.to_string_repr()).collect();
                if items.len() == 1 {
                    format!("({},)", item_strings[0]) // Single element tuple needs comma
                } else {
                    format!("({})", item_strings.join(", "))
                }
            }
            Value::Map(map) => {
                let entries: Vec<String> = map
                    .iter()
                    .map(|(k, v)| format!("{} => {}", k.to_string_repr(), v.to_string_repr()))
                    .collect();
                format!("{{{}}}", entries.join(", "))
            }
            Value::BuiltinFunction { name, arity, .. } => {
                format!("Function<{}/{}>", name, arity)
            }
            Value::Unit => "unit".to_string(),
            Value::None => "none".to_string(),
        }
    }

    /// Convert to display string (for REPL output, more detailed)
    pub fn to_display_string(&self) -> String {
        match self {
            Value::String(s) => format!("\"{}\"", s), // Show quotes for strings in REPL
            _ => self.to_string_repr(),
        }
    }

    // Arithmetic operations

    /// Addition operation
    pub fn add(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a + *b as f64)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
            (Value::List(a), Value::List(b)) => {
                let mut result = a.clone();
                result.extend(b.iter().cloned());
                Ok(Value::List(result))
            }
            _ => Err(RuntimeError::invalid_operation(
                "+",
                vec![self.type_name().to_string(), other.type_name().to_string()],
            )),
        }
    }

    /// Subtraction operation
    pub fn subtract(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a - *b as f64)),
            _ => Err(RuntimeError::invalid_operation(
                "-",
                vec![self.type_name().to_string(), other.type_name().to_string()],
            )),
        }
    }

    /// Multiplication operation
    pub fn multiply(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a * *b as f64)),
            _ => Err(RuntimeError::invalid_operation(
                "*",
                vec![self.type_name().to_string(), other.type_name().to_string()],
            )),
        }
    }

    /// Division operation
    pub fn divide(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => {
                if *b == 0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Float(*a as f64 / *b as f64)) // Integer division returns float
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Float(a / b))
                }
            }
            (Value::Integer(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Float(*a as f64 / b))
                }
            }
            (Value::Float(a), Value::Integer(b)) => {
                if *b == 0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Float(a / *b as f64))
                }
            }
            _ => Err(RuntimeError::invalid_operation(
                "/",
                vec![self.type_name().to_string(), other.type_name().to_string()],
            )),
        }
    }

    /// Modulo operation
    pub fn modulo(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => {
                if *b == 0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Integer(a % b))
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Float(a % b))
                }
            }
            _ => Err(RuntimeError::invalid_operation(
                "%",
                vec![self.type_name().to_string(), other.type_name().to_string()],
            )),
        }
    }

    // Comparison operations

    /// Equality comparison (works for all types)
    pub fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Integer(a), Value::Float(b)) => *a as f64 == *b,
            (Value::Float(a), Value::Integer(b)) => *a == *b as f64,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Atom(a), Value::Atom(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (Value::None, Value::None) => true,
            _ => false, // Different types are never equal
        }
    }

    /// Ordering comparison (for <, >, <=, >=)
    pub fn compare(&self, other: &Value) -> Result<Ordering> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(a.cmp(b)),
            (Value::Float(a), Value::Float(b)) => Ok(a.partial_cmp(b).unwrap_or(Ordering::Equal)),
            (Value::Integer(a), Value::Float(b)) => {
                Ok((*a as f64).partial_cmp(b).unwrap_or(Ordering::Equal))
            }
            (Value::Float(a), Value::Integer(b)) => {
                Ok(a.partial_cmp(&(*b as f64)).unwrap_or(Ordering::Equal))
            }
            (Value::String(a), Value::String(b)) => Ok(a.cmp(b)),
            (Value::Atom(a), Value::Atom(b)) => Ok(a.cmp(b)),
            _ => Err(RuntimeError::invalid_operation(
                "comparison",
                vec![self.type_name().to_string(), other.type_name().to_string()],
            )),
        }
    }
}

// Implement Hash for Value so it can be used as Map keys
impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Integer(n) => {
                0u8.hash(state);
                n.hash(state);
            }
            Value::Float(f) => {
                1u8.hash(state);
                f.to_bits().hash(state); // Use bit representation for stable hashing
            }
            Value::Boolean(b) => {
                2u8.hash(state);
                b.hash(state);
            }
            Value::String(s) => {
                3u8.hash(state);
                s.hash(state);
            }
            Value::Atom(name) => {
                4u8.hash(state);
                name.hash(state);
            }
            Value::List(items) => {
                5u8.hash(state);
                items.hash(state);
            }
            Value::Tuple(items) => {
                6u8.hash(state);
                items.hash(state);
            }
            Value::Map(map) => {
                7u8.hash(state);
                // Hash entries in sorted order for stable hashing
                let mut entries: Vec<_> = map.iter().collect();
                entries.sort_by(|a, b| format!("{:?}", a.0).cmp(&format!("{:?}", b.0)));
                entries.hash(state);
            }
            Value::BuiltinFunction { name, arity, .. } => {
                8u8.hash(state);
                name.hash(state);
                arity.hash(state);
            }
            Value::Unit => {
                9u8.hash(state);
            }
            Value::None => {
                10u8.hash(state);
            }
        }
    }
}

// We need Eq for HashMap/IndexMap usage
impl Eq for Value {}

// Implement Display trait for convenience
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_repr())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_names() {
        assert_eq!(Value::Integer(42).type_name(), "Integer");
        assert_eq!(Value::Float(42.5).type_name(), "Float");
        assert_eq!(Value::Boolean(true).type_name(), "Boolean");
        assert_eq!(Value::String("hello".to_string()).type_name(), "String");
        assert_eq!(Value::Atom("test".to_string()).type_name(), "Atom");
    }

    #[test]
    fn test_truthiness() {
        assert!(Value::Boolean(true).is_truthy());
        assert!(!Value::Boolean(false).is_truthy());
        assert!(!Value::Integer(0).is_truthy());
        assert!(Value::Integer(42).is_truthy());
        assert!(!Value::Float(0.0).is_truthy());
        assert!(Value::Float(1.5).is_truthy());
        assert!(Value::String("hello".to_string()).is_truthy());
        assert!(!Value::String("".to_string()).is_truthy());
    }

    #[test]
    fn test_arithmetic() {
        let a = Value::Integer(5);
        let b = Value::Integer(3);

        assert_eq!(a.add(&b).unwrap(), Value::Integer(8));
        assert_eq!(a.subtract(&b).unwrap(), Value::Integer(2));
        assert_eq!(a.multiply(&b).unwrap(), Value::Integer(15));
        assert_eq!(a.divide(&b).unwrap(), Value::Float(5.0 / 3.0));
        assert_eq!(a.modulo(&b).unwrap(), Value::Integer(2));
    }

    #[test]
    fn test_mixed_numeric_arithmetic() {
        let int_val = Value::Integer(5);
        let float_val = Value::Float(2.5);

        assert_eq!(int_val.add(&float_val).unwrap(), Value::Float(7.5));
        assert_eq!(float_val.add(&int_val).unwrap(), Value::Float(7.5));
    }

    #[test]
    fn test_string_concatenation() {
        let a = Value::String("hello".to_string());
        let b = Value::String(" world".to_string());

        assert_eq!(a.add(&b).unwrap(), Value::String("hello world".to_string()));
    }

    #[test]
    fn test_equality() {
        assert!(Value::Integer(42).equals(&Value::Integer(42)));
        assert!(Value::Integer(5).equals(&Value::Float(5.0)));
        assert!(Value::Float(5.0).equals(&Value::Integer(5)));
        assert!(!Value::Integer(5).equals(&Value::String("5".to_string())));
    }

    #[test]
    fn test_division_by_zero() {
        let a = Value::Integer(5);
        let zero = Value::Integer(0);

        assert!(matches!(
            a.divide(&zero),
            Err(RuntimeError::DivisionByZero { .. })
        ));
    }
}
