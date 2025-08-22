//! Tests for Result type runtime support
//! 
//! These tests verify that Result.ok() and Result.err() work correctly
//! in the interpreter, including value creation, display, and type information.

use crate::test_harness::InterpreterSession;
use crate::Value;

#[test]
fn test_result_ok_creation() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test creating Ok with an integer
    let result = session.evaluate("Result.ok(value: 42)");
    assert!(result.is_ok(), "Result.ok should create a value");
    
    let value = result.unwrap();
    // We expect this to be a Struct of type Outrun.Result.Ok<T, E> containing value field
    match value {
        Value::Struct { qualified_type_name, fields, .. } => {
            assert_eq!(qualified_type_name, "Outrun.Result.Ok");
            assert!(fields.contains_key("value"));
            assert_eq!(fields.get("value").unwrap(), &Value::Integer64(42));
        }
        _ => panic!("Expected Struct for Result.Ok, got {:?}", value),
    }
}

#[test]
fn test_result_err_creation() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test creating Err with a string error
    let result = session.evaluate(r#"Result.err(error: "failed")"#);
    assert!(result.is_ok(), "Result.err should create a value");
    
    let value = result.unwrap();
    // We expect this to be a Struct of type Outrun.Result.Err<T, E> containing error field
    match value {
        Value::Struct { qualified_type_name, fields, .. } => {
            assert_eq!(qualified_type_name, "Outrun.Result.Err");
            assert!(fields.contains_key("error"));
            assert_eq!(fields.get("error").unwrap(), &Value::String("failed".to_string()));
        }
        _ => panic!("Expected Struct for Result.Err, got {:?}", value),
    }
}

#[test]
fn test_result_display() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test Ok display
    let ok_result = session.evaluate("Result.ok(value: 42)");
    assert!(ok_result.is_ok());
    let ok_value = ok_result.unwrap();
    assert_eq!(ok_value.display(), "Outrun.Result.Ok{ value: 42 }");
    
    // Test Err display
    let err_result = session.evaluate(r#"Result.err(error: "failed")"#);
    assert!(err_result.is_ok());
    let err_value = err_result.unwrap();
    assert_eq!(err_value.display(), r#"Outrun.Result.Err{ error: "failed" }"#);
}

#[test]
fn test_result_type_name() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test Ok type name
    let ok_result = session.evaluate("Result.ok(value: 42)");
    assert!(ok_result.is_ok());
    let ok_value = ok_result.unwrap();
    assert_eq!(ok_value.type_name(), "Struct");  // All structs have type_name "Struct"
    
    // Test Err type name
    let err_result = session.evaluate(r#"Result.err(error: "failed")"#);
    assert!(err_result.is_ok());
    let err_value = err_result.unwrap();
    assert_eq!(err_value.type_name(), "Struct");  // All structs have type_name "Struct"
}

#[test]
fn test_result_with_different_types() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test Ok with string
    let ok_string = session.evaluate(r#"Result.ok(value: "success")"#);
    assert!(ok_string.is_ok());
    assert_eq!(ok_string.unwrap().display(), r#"Outrun.Result.Ok{ value: "success" }"#);
    
    // Test Ok with boolean
    let ok_bool = session.evaluate("Result.ok(value: true)");
    assert!(ok_bool.is_ok());
    assert_eq!(ok_bool.unwrap().display(), "Outrun.Result.Ok{ value: true }");
    
    // Test Err with integer (error code)
    let err_int = session.evaluate("Result.err(error: 404)");
    assert!(err_int.is_ok());
    assert_eq!(err_int.unwrap().display(), "Outrun.Result.Err{ error: 404 }");
    
    // Test Err with boolean
    let err_bool = session.evaluate("Result.err(error: false)");
    assert!(err_bool.is_ok());
    assert_eq!(err_bool.unwrap().display(), "Outrun.Result.Err{ error: false }");
}