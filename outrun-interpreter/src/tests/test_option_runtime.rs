//! Tests for Option type runtime support
//! 
//! These tests verify that Option.some() and Option.none() work correctly
//! in the interpreter, including value creation, display, and type information.

use crate::test_harness::InterpreterSession;
use crate::Value;

#[test]
fn test_option_some_creation() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test creating Some with an integer
    let result = session.evaluate("Option.some(value: 42)");
    assert!(result.is_ok(), "Option.some should create a value");
    
    let value = result.unwrap();
    // We expect this to be a Struct of type Outrun.Option.Some<T> containing value field
    match value {
        Value::Struct { qualified_type_name, fields, .. } => {
            assert_eq!(qualified_type_name, "Outrun.Option.Some");
            assert!(fields.contains_key("value"));
            assert_eq!(fields.get("value").unwrap(), &Value::Integer64(42));
        }
        _ => panic!("Expected Struct for Option.Some, got {:?}", value),
    }
}

#[test]
fn test_option_none_creation() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test creating None
    let result = session.evaluate("Option.none()");
    assert!(result.is_ok(), "Option.none should create a value");
    
    let value = result.unwrap();
    // We expect this to be a Struct of type Outrun.Option.None<T> with no fields
    match value {
        Value::Struct { qualified_type_name, fields, .. } => {
            assert_eq!(qualified_type_name, "Outrun.Option.None");
            assert!(fields.is_empty(), "None should have no fields");
        }
        _ => panic!("Expected Struct for Option.None, got {:?}", value),
    }
}

#[test]
fn test_option_display() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test Some display
    let some_result = session.evaluate("Option.some(value: 42)");
    assert!(some_result.is_ok());
    let some_value = some_result.unwrap();
    assert_eq!(some_value.display(), "Outrun.Option.Some{ value: 42 }");
    
    // Test None display
    let none_result = session.evaluate("Option.none()");
    assert!(none_result.is_ok());
    let none_value = none_result.unwrap();
    assert_eq!(none_value.display(), "Outrun.Option.None{ }");
}

#[test]
fn test_option_type_name() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test Some type name
    let some_result = session.evaluate("Option.some(value: 42)");
    assert!(some_result.is_ok());
    let some_value = some_result.unwrap();
    assert_eq!(some_value.type_name(), "Struct");  // All structs have type_name "Struct"
    
    // Test None type name
    let none_result = session.evaluate("Option.none()");
    assert!(none_result.is_ok());
    let none_value = none_result.unwrap();
    assert_eq!(none_value.type_name(), "Struct");  // All structs have type_name "Struct"
}

#[test]
fn test_option_with_different_types() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test Some with string
    let string_result = session.evaluate(r#"Option.some(value: "hello")"#);
    assert!(string_result.is_ok());
    let string_value = string_result.unwrap();
    assert_eq!(string_value.display(), r#"Outrun.Option.Some{ value: "hello" }"#);
    
    // Test Some with boolean
    let bool_result = session.evaluate("Option.some(value: true)");
    assert!(bool_result.is_ok());
    let bool_value = bool_result.unwrap();
    assert_eq!(bool_value.display(), "Outrun.Option.Some{ value: true }");
    
    // Test Some with float
    let float_result = session.evaluate("Option.some(value: 3.14)");
    assert!(float_result.is_ok());
    let float_value = float_result.unwrap();
    assert_eq!(float_value.display(), "Outrun.Option.Some{ value: 3.14 }");
}