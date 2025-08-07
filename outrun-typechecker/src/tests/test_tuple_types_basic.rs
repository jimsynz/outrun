//! Basic tests for structural tuple types
//!
//! Tests the fundamental tuple type functionality:
//! - Structural type display
//! - Basic type equality

use crate::Type;

#[test]
fn test_tuple_type_display() {
    // Test that tuple types display with structural syntax
    let tuple_type = Type::Tuple {
        element_types: vec![
            Type::concrete("Integer64"),
            Type::concrete("String"),
        ],
        span: None,
    };

    assert_eq!(tuple_type.to_string(), "(Integer64, String)");
}

#[test]
fn test_empty_tuple_display() {
    // Test empty tuple display
    let empty_tuple = Type::Tuple {
        element_types: vec![],
        span: None,
    };

    assert_eq!(empty_tuple.to_string(), "()");
}

#[test]
fn test_nested_tuple_display() {
    // Test nested tuple display
    let inner_tuple = Type::Tuple {
        element_types: vec![
            Type::concrete("Integer64"),
            Type::concrete("Integer64"),
        ],
        span: None,
    };

    let outer_tuple = Type::Tuple {
        element_types: vec![
            inner_tuple,
            Type::concrete("String"),
        ],
        span: None,
    };

    assert_eq!(outer_tuple.to_string(), "((Integer64, Integer64), String)");
}

#[test]
fn test_tuple_type_equality() {
    // Test structural equality of tuple types
    let tuple1 = Type::Tuple {
        element_types: vec![
            Type::concrete("Integer64"),
            Type::concrete("String"),
        ],
        span: None,
    };

    let tuple2 = Type::Tuple {
        element_types: vec![
            Type::concrete("Integer64"),
            Type::concrete("String"),
        ],
        span: None,
    };

    let tuple3 = Type::Tuple {
        element_types: vec![
            Type::concrete("String"),
            Type::concrete("Integer64"),
        ],
        span: None,
    };

    // Same structure should be equal
    assert_eq!(tuple1, tuple2);

    // Different order should not be equal
    assert_ne!(tuple1, tuple3);
}

#[test]
fn test_tuple_arity_differences() {
    // Test that tuples with different arities are not equal
    let pair = Type::Tuple {
        element_types: vec![
            Type::concrete("Integer64"),
            Type::concrete("String"),
        ],
        span: None,
    };

    let triple = Type::Tuple {
        element_types: vec![
            Type::concrete("Integer64"),
            Type::concrete("String"),
            Type::concrete("Boolean"),
        ],
        span: None,
    };

    assert_ne!(pair, triple);
    assert_eq!(pair.to_string(), "(Integer64, String)");
    assert_eq!(triple.to_string(), "(Integer64, String, Boolean)");
}