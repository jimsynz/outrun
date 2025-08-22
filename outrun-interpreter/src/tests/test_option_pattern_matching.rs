//! Tests for Option pattern matching support
//! 
//! These tests verify that Option values can be pattern matched using:
//! 1. Guard-based matching with Option.some?() and Option.none?()
//! 2. Struct destructuring with Outrun.Option.Some and Outrun.Option.None

use crate::test_harness::InterpreterSession;
use crate::Value;

#[test]
fn test_option_some_guard_matching() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create an Option.Some value
    session.evaluate("let opt = Option.some(value: 42)").unwrap();
    
    // Test guard-based pattern matching on Some
    // The guard pattern binds the entire Option to the variable.
    // Since field access isn't implemented yet, we'll use a nested case to extract the value
    let result = session.evaluate(r#"
        case opt {
            some when Option.some?(value: some) -> 
                case some {
                    Outrun.Option.Some { value: v } -> v
                    _ -> -1  # This shouldn't happen since we checked with some?
                }
            none when Option.none?(value: none) -> 0
        }
    "#);
    
    if let Err(e) = &result {
        println!("Guard test failed with: {:?}", e);
    }
    assert!(result.is_ok(), "Guard-based pattern matching should work");
    assert_eq!(result.unwrap(), Value::Integer64(42));
}

#[test]
fn test_option_none_guard_matching() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create an Option.None value
    session.evaluate("let opt = Option.none()").unwrap();
    
    // Test guard-based pattern matching on None
    let result = session.evaluate(r#"
        case opt {
            some when Option.some?(value: some) -> 100
            none when Option.none?(value: none) -> 0  
        }
    "#);
    
    assert!(result.is_ok(), "Guard-based pattern matching should work");
    assert_eq!(result.unwrap(), Value::Integer64(0));
}

#[test]
fn test_option_some_struct_destructuring() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create an Option.Some value
    session.evaluate("let opt = Option.some(value: 42)").unwrap();
    
    // Test struct destructuring pattern matching on Some
    let result = session.evaluate(r#"
        case opt {
            Outrun.Option.Some { value: v } -> v
            Outrun.Option.None { } -> 0
            _ -> -1
        }
    "#);
    
    if let Err(e) = &result {
        println!("Case expression failed with: {:?}", e);
    }
    assert!(result.is_ok(), "Struct destructuring should work");
    assert_eq!(result.unwrap(), Value::Integer64(42));
}

#[test]
fn test_option_none_struct_destructuring() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create an Option.None value
    session.evaluate("let opt = Option.none()").unwrap();
    
    // Test struct destructuring pattern matching on None
    let result = session.evaluate(r#"
        case opt {
            Outrun.Option.Some { value: v } -> v
            Outrun.Option.None { } -> 0
            _ -> -1
        }
    "#);
    
    assert!(result.is_ok(), "Struct destructuring should work");
    assert_eq!(result.unwrap(), Value::Integer64(0));
}

#[test]
fn test_nested_option_pattern_matching() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Create a nested Option.Some(Option.Some(42))
    session.evaluate("let inner = Option.some(value: 42)").unwrap();
    session.evaluate("let outer = Option.some(value: inner)").unwrap();
    
    // Test nested pattern matching with struct destructuring
    let result = session.evaluate(r#"
        case outer {
            Outrun.Option.Some { value: inner_opt } -> 
                case inner_opt {
                    Outrun.Option.Some { value: v } -> v
                    Outrun.Option.None { } -> 0
                    _ -> -2
                }
            Outrun.Option.None { } -> -1
            _ -> -3
        }
    "#);
    
    assert!(result.is_ok(), "Nested pattern matching should work");
    assert_eq!(result.unwrap(), Value::Integer64(42));
}

#[test]
fn test_option_with_different_types() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test with string type
    session.evaluate(r#"let str_opt = Option.some(value: "hello")"#).unwrap();
    let result = session.evaluate(r#"
        case str_opt {
            Outrun.Option.Some { value: s } -> s
            Outrun.Option.None { } -> "default"
            _ -> "error"
        }
    "#);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::String("hello".to_string()));
    
    // Test with boolean type
    session.evaluate("let bool_opt = Option.some(value: true)").unwrap();
    let result = session.evaluate(r#"
        case bool_opt {
            Outrun.Option.Some { value: b } -> b
            Outrun.Option.None { } -> false
            _ -> false
        }
    "#);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}