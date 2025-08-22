//! Tests for List type runtime support
//! 
//! These tests verify that List.empty() and List.prepend() work correctly
//! in the interpreter, including value creation, display, and list operations.

use crate::test_harness::InterpreterSession;
use crate::value::List;
use crate::Value;

#[test]
fn test_list_empty_creation() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test creating empty list with type hint
    session.evaluate("let empty: List<Integer64> = []").unwrap();
    // Now evaluate the variable to get the list value
    let result = session.evaluate("empty");
    assert!(result.is_ok(), "empty list should be accessible");
    
    let value = result.unwrap();
    // We expect this to be Value::List with Empty variant
    match value {
        Value::List { list, .. } => {
            match list.as_ref() {
                List::Empty => {
                    // Success - empty list created
                }
                _ => panic!("Expected Empty list, got non-empty"),
            }
        }
        _ => panic!("Expected List value, got {:?}", value),
    }
}

#[test]
fn test_list_prepend() {
    let mut session = InterpreterSession::new().unwrap();
    
    // First create an empty list with type hint
    session.evaluate("let empty: List<Integer64> = []").unwrap();
    
    // Then prepend an element using List.prepend protocol function
    let prepend_result = session.evaluate("List.prepend(list: empty, elem: 42)");
    if let Err(e) = &prepend_result {
        println!("List.prepend failed with: {:?}", e);
    }
    assert!(prepend_result.is_ok(), "List.prepend should work");
    
    let value = prepend_result.unwrap();
    // We expect this to be a list with 42 as the head
    match value {
        Value::List { list, .. } => {
            match list.as_ref() {
                List::Cons { head, tail } => {
                    assert_eq!(*head, Value::Integer64(42));
                    // Tail should be empty
                    match tail.as_ref() {
                        List::Empty => {
                            // Success - list structure is correct
                        }
                        _ => panic!("Expected tail to be Empty"),
                    }
                }
                _ => panic!("Expected Cons list with 42 as head"),
            }
        }
        _ => panic!("Expected List value, got {:?}", value),
    }
}

#[test]
fn test_list_display() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test empty list display
    session.evaluate("let empty: List<Integer64> = []").unwrap();
    let empty_result = session.evaluate("empty");
    assert!(empty_result.is_ok());
    assert_eq!(empty_result.unwrap().display(), "[]");
    
    // Test single element list
    let single_result = session.evaluate("[42]");
    assert!(single_result.is_ok());
    assert_eq!(single_result.unwrap().display(), "[42]");
    
    // Test multiple element list
    let multi_result = session.evaluate("[1, 2, 3]");
    assert!(multi_result.is_ok());
    assert_eq!(multi_result.unwrap().display(), "[1, 2, 3]");
}

#[test]
fn test_list_with_different_types() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test list of strings
    let string_list = session.evaluate(r#"["hello", "world"]"#);
    assert!(string_list.is_ok());
    assert_eq!(string_list.unwrap().display(), r#"["hello", "world"]"#);
    
    // Test list of booleans
    let bool_list = session.evaluate("[true, false]");
    assert!(bool_list.is_ok());
    assert_eq!(bool_list.unwrap().display(), "[true, false]");
    
    // Test list of floats
    let float_list = session.evaluate("[3.14, 2.71]");
    assert!(float_list.is_ok());
    assert_eq!(float_list.unwrap().display(), "[3.14, 2.71]");
}

#[test]
fn test_list_type_name() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Test list type name
    session.evaluate("let typed_list: List<Integer64> = []").unwrap();
    let list_result = session.evaluate("typed_list");
    assert!(list_result.is_ok());
    assert_eq!(list_result.unwrap().type_name(), "List");
}