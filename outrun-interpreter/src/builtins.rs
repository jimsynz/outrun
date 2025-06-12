//! Built-in functions for the Outrun interpreter.
//!
//! Provides core functionality like I/O, type conversion, and collection operations.

use crate::environment::Environment;
use crate::error::{Result, RuntimeError};
use crate::value::Value;

/// Register all built-in functions with the environment
pub fn register_builtins(env: &mut Environment) {
    // I/O functions
    env.register_builtin("print".to_string(), create_print_function());
    env.register_builtin("println".to_string(), create_println_function());

    // Type conversion functions
    env.register_builtin(
        "String.from_integer".to_string(),
        create_string_from_integer(),
    );
    env.register_builtin("Integer.parse".to_string(), create_integer_parse());
    env.register_builtin(
        "Float.from_integer".to_string(),
        create_float_from_integer(),
    );

    // Collection operations
    env.register_builtin("List.length".to_string(), create_list_length());
    env.register_builtin("List.head".to_string(), create_list_head());
    env.register_builtin("List.tail".to_string(), create_list_tail());

    // Type checking predicates
    env.register_builtin("is_integer?".to_string(), create_is_integer());
    env.register_builtin("is_string?".to_string(), create_is_string());
    env.register_builtin("is_boolean?".to_string(), create_is_boolean());
    env.register_builtin("is_atom?".to_string(), create_is_atom());
    env.register_builtin("is_list?".to_string(), create_is_list());
}

// I/O Functions

fn create_print_function() -> Value {
    Value::BuiltinFunction {
        name: "print".to_string(),
        arity: 1,
        function: builtin_print,
    }
}

fn create_println_function() -> Value {
    Value::BuiltinFunction {
        name: "println".to_string(),
        arity: 1,
        function: builtin_println,
    }
}

/// Print a value without newline
fn builtin_print(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("print", 1, args.len()));
    }

    print!("{}", args[0].to_string_repr());
    Ok(Value::Unit)
}

/// Print a value with newline
fn builtin_println(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("println", 1, args.len()));
    }

    println!("{}", args[0].to_string_repr());
    Ok(Value::Unit)
}

// Type Conversion Functions

fn create_string_from_integer() -> Value {
    Value::BuiltinFunction {
        name: "String.from_integer".to_string(),
        arity: 1,
        function: string_from_integer,
    }
}

fn create_integer_parse() -> Value {
    Value::BuiltinFunction {
        name: "Integer.parse".to_string(),
        arity: 1,
        function: integer_parse,
    }
}

fn create_float_from_integer() -> Value {
    Value::BuiltinFunction {
        name: "Float.from_integer".to_string(),
        arity: 1,
        function: float_from_integer,
    }
}

/// Convert integer to string
fn string_from_integer(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity(
            "String.from_integer",
            1,
            args.len(),
        ));
    }

    match &args[0] {
        Value::Integer(n) => Ok(Value::String(n.to_string())),
        _ => Err(RuntimeError::type_error("Integer", args[0].type_name())),
    }
}

/// Parse string to integer
fn integer_parse(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("Integer.parse", 1, args.len()));
    }

    match &args[0] {
        Value::String(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Err(RuntimeError::custom(format!(
                "Cannot parse '{}' as integer",
                s
            ))),
        },
        _ => Err(RuntimeError::type_error("String", args[0].type_name())),
    }
}

/// Convert integer to float
fn float_from_integer(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity(
            "Float.from_integer",
            1,
            args.len(),
        ));
    }

    match &args[0] {
        Value::Integer(n) => Ok(Value::Float(*n as f64)),
        _ => Err(RuntimeError::type_error("Integer", args[0].type_name())),
    }
}

// Collection Operations

fn create_list_length() -> Value {
    Value::BuiltinFunction {
        name: "List.length".to_string(),
        arity: 1,
        function: list_length,
    }
}

fn create_list_head() -> Value {
    Value::BuiltinFunction {
        name: "List.head".to_string(),
        arity: 1,
        function: list_head,
    }
}

fn create_list_tail() -> Value {
    Value::BuiltinFunction {
        name: "List.tail".to_string(),
        arity: 1,
        function: list_tail,
    }
}

/// Get length of a list
fn list_length(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("List.length", 1, args.len()));
    }

    match &args[0] {
        Value::List(list) => Ok(Value::Integer(list.len() as i64)),
        _ => Err(RuntimeError::type_error("List", args[0].type_name())),
    }
}

/// Get first element of a list
fn list_head(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("List.head", 1, args.len()));
    }

    match &args[0] {
        Value::List(list) => {
            if list.is_empty() {
                Ok(Value::None)
            } else {
                Ok(list[0].clone())
            }
        }
        _ => Err(RuntimeError::type_error("List", args[0].type_name())),
    }
}

/// Get all elements except the first (tail of a list)
fn list_tail(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("List.tail", 1, args.len()));
    }

    match &args[0] {
        Value::List(list) => {
            if list.is_empty() {
                Ok(Value::List(vec![]))
            } else {
                Ok(Value::List(list[1..].to_vec()))
            }
        }
        _ => Err(RuntimeError::type_error("List", args[0].type_name())),
    }
}

// Type Checking Predicates

fn create_is_integer() -> Value {
    Value::BuiltinFunction {
        name: "is_integer?".to_string(),
        arity: 1,
        function: is_integer,
    }
}

fn create_is_string() -> Value {
    Value::BuiltinFunction {
        name: "is_string?".to_string(),
        arity: 1,
        function: is_string,
    }
}

fn create_is_boolean() -> Value {
    Value::BuiltinFunction {
        name: "is_boolean?".to_string(),
        arity: 1,
        function: is_boolean,
    }
}

fn create_is_atom() -> Value {
    Value::BuiltinFunction {
        name: "is_atom?".to_string(),
        arity: 1,
        function: is_atom,
    }
}

fn create_is_list() -> Value {
    Value::BuiltinFunction {
        name: "is_list?".to_string(),
        arity: 1,
        function: is_list,
    }
}

/// Check if value is an integer
fn is_integer(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("is_integer?", 1, args.len()));
    }

    Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
}

/// Check if value is a string
fn is_string(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("is_string?", 1, args.len()));
    }

    Ok(Value::Boolean(matches!(args[0], Value::String(_))))
}

/// Check if value is a boolean
fn is_boolean(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("is_boolean?", 1, args.len()));
    }

    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}

/// Check if value is an atom
fn is_atom(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("is_atom?", 1, args.len()));
    }

    Ok(Value::Boolean(matches!(args[0], Value::Atom(_))))
}

/// Check if value is a list
fn is_list(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(RuntimeError::wrong_arity("is_list?", 1, args.len()));
    }

    Ok(Value::Boolean(matches!(args[0], Value::List(_))))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_functions() {
        // Test print
        let result = builtin_print(&[Value::String("hello".to_string())]).unwrap();
        assert_eq!(result, Value::Unit);

        // Test println
        let result = builtin_println(&[Value::Integer(42)]).unwrap();
        assert_eq!(result, Value::Unit);

        // Test wrong arity
        assert!(builtin_print(&[]).is_err());
        assert!(builtin_print(&[Value::Unit, Value::Unit]).is_err());
    }

    #[test]
    fn test_type_conversion() {
        // String.from_integer
        let result = string_from_integer(&[Value::Integer(42)]).unwrap();
        assert_eq!(result, Value::String("42".to_string()));

        // Integer.parse
        let result = integer_parse(&[Value::String("123".to_string())]).unwrap();
        assert_eq!(result, Value::Integer(123));

        // Float.from_integer
        let result = float_from_integer(&[Value::Integer(5)]).unwrap();
        assert_eq!(result, Value::Float(5.0));

        // Test invalid conversions
        assert!(integer_parse(&[Value::String("not_a_number".to_string())]).is_err());
        assert!(string_from_integer(&[Value::String("not_integer".to_string())]).is_err());
    }

    #[test]
    fn test_list_operations() {
        let test_list = Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);

        // List.length
        let result = list_length(&[test_list.clone()]).unwrap();
        assert_eq!(result, Value::Integer(3));

        // List.head
        let result = list_head(&[test_list.clone()]).unwrap();
        assert_eq!(result, Value::Integer(1));

        // List.tail
        let result = list_tail(&[test_list.clone()]).unwrap();
        assert_eq!(
            result,
            Value::List(vec![Value::Integer(2), Value::Integer(3)])
        );

        // Empty list
        let empty_list = Value::List(vec![]);
        let result = list_head(&[empty_list.clone()]).unwrap();
        assert_eq!(result, Value::None);

        let result = list_tail(&[empty_list.clone()]).unwrap();
        assert_eq!(result, Value::List(vec![]));
    }

    #[test]
    fn test_type_predicates() {
        // is_integer?
        assert_eq!(
            is_integer(&[Value::Integer(42)]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            is_integer(&[Value::String("42".to_string())]).unwrap(),
            Value::Boolean(false)
        );

        // is_string?
        assert_eq!(
            is_string(&[Value::String("hello".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            is_string(&[Value::Integer(42)]).unwrap(),
            Value::Boolean(false)
        );

        // is_boolean?
        assert_eq!(
            is_boolean(&[Value::Boolean(true)]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            is_boolean(&[Value::Integer(1)]).unwrap(),
            Value::Boolean(false)
        );

        // is_atom?
        assert_eq!(
            is_atom(&[Value::Atom("test".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            is_atom(&[Value::String("test".to_string())]).unwrap(),
            Value::Boolean(false)
        );

        // is_list?
        assert_eq!(
            is_list(&[Value::List(vec![])]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            is_list(&[Value::Integer(42)]).unwrap(),
            Value::Boolean(false)
        );
    }
}
