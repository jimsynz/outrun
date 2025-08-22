//! Tests for Result pattern matching support
//! 
//! These tests verify that Result values can be pattern matched using:
//! 1. Guard-based matching with Result.ok?() and Result.err?()
//! 2. Struct destructuring with Outrun.Result.Ok and Outrun.Result.Err

use crate::test_harness::InterpreterSession;
use crate::Value;

#[test]
fn test_result_ok_guard_matching() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create a Result.Ok value
    session.evaluate("let res = Result.ok(value: 42)").unwrap();
    
    // Test guard-based pattern matching on Ok
    // The guard binds the entire Result, so we need to extract the value
    let result = session.evaluate(r#"
        case res {
            ok when Result.ok?(value: ok) -> 
                case ok {
                    Outrun.Result.Ok { value: v } -> v
                    _ -> -1
                }
            err when Result.err?(value: err) -> -1
        }
    "#);
    
    if let Err(e) = &result {
        println!("Result guard test failed with: {:?}", e);
    }
    assert!(result.is_ok(), "Guard-based pattern matching should work");
    assert_eq!(result.unwrap(), Value::Integer64(42));
}

#[test]
fn test_result_err_guard_matching() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create a Result.Err value
    session.evaluate(r#"let res = Result.err(error: "failed")"#).unwrap();
    
    // Test guard-based pattern matching on Err
    let result = session.evaluate(r#"
        case res {
            ok when Result.ok?(value: ok) -> 100
            err when Result.err?(value: err) -> 0  
        }
    "#);
    
    assert!(result.is_ok(), "Guard-based pattern matching should work");
    assert_eq!(result.unwrap(), Value::Integer64(0));
}

#[test]
fn test_result_ok_struct_destructuring() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create a Result.Ok value
    session.evaluate("let res = Result.ok(value: 42)").unwrap();
    
    // Test struct destructuring pattern matching on Ok
    let result = session.evaluate(r#"
        case res {
            Outrun.Result.Ok { value: v } -> v
            Outrun.Result.Err { error: e } -> -1
            _ -> -99
        }
    "#);
    
    assert!(result.is_ok(), "Struct destructuring should work");
    assert_eq!(result.unwrap(), Value::Integer64(42));
}

#[test]
fn test_result_err_struct_destructuring() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create a Result.Err value with integer error code
    session.evaluate("let res = Result.err(error: 404)").unwrap();
    
    // Test struct destructuring pattern matching on Err
    let result = session.evaluate(r#"
        case res {
            Outrun.Result.Ok { value: v } -> v
            Outrun.Result.Err { error: e } -> e
            _ -> -99
        }
    "#);
    
    assert!(result.is_ok(), "Struct destructuring should work");
    assert_eq!(result.unwrap(), Value::Integer64(404));
}

#[test]
fn test_result_with_string_error() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create a Result.Err with string error
    session.evaluate(r#"let res = Result.err(error: "not found")"#).unwrap();
    
    // Test pattern matching extracts the error string
    let result = session.evaluate(r#"
        case res {
            Outrun.Result.Ok { value: v } -> "success"
            Outrun.Result.Err { error: e } -> e
            _ -> "unexpected"
        }
    "#);
    
    assert!(result.is_ok(), "Pattern matching should work with string errors");
    assert_eq!(result.unwrap(), Value::String("not found".to_string()));
}

#[test]
fn test_nested_result_option_pattern_matching() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create a Result.Ok(Option.Some(42))
    session.evaluate("let opt = Option.some(value: 42)").unwrap();
    session.evaluate("let res = Result.ok(value: opt)").unwrap();
    
    // Test nested pattern matching
    let result = session.evaluate(r#"
        case res {
            Outrun.Result.Ok { value: opt_val } -> 
                case opt_val {
                    Outrun.Option.Some { value: v } -> v
                    Outrun.Option.None { } -> 0
                    _ -> -2
                }
            Outrun.Result.Err { error: e } -> -1
            _ -> -99
        }
    "#);
    
    assert!(result.is_ok(), "Nested Result/Option pattern matching should work");
    assert_eq!(result.unwrap(), Value::Integer64(42));
}

#[test]
fn test_result_with_different_types() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test with string success type
    session.evaluate(r#"let str_res = Result.ok(value: "success")"#).unwrap();
    let result = session.evaluate(r#"
        case str_res {
            Outrun.Result.Ok { value: s } -> s
            Outrun.Result.Err { error: e } -> "error"
            _ -> "unexpected"
        }
    "#);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::String("success".to_string()));
    
    // Test with boolean error type
    session.evaluate("let bool_res = Result.err(error: false)").unwrap();
    let result = session.evaluate(r#"
        case bool_res {
            Outrun.Result.Ok { value: v } -> true
            Outrun.Result.Err { error: e } -> e
            _ -> false
        }
    "#);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(false));
}