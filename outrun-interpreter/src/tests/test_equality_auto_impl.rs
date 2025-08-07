//! Test automatic Equality implementation runtime behavior

use crate::InterpreterSession;

#[test]
fn test_automatic_equality_runtime_behavior() {
    let mut harness = InterpreterSession::new().unwrap();
    
    // Test that automatic equality works at runtime
    harness.evaluate(r#"
        struct Point(x: Integer, y: Integer) {}
        let p1 = Point { x: 1, y: 2 }
        let p2 = Point { x: 1, y: 2 }
        let p3 = Point { x: 3, y: 4 }
    "#).unwrap();
    
    // These should work with automatic Equality implementation
    harness.assert_evaluates_to_boolean("p1 == p2", true).unwrap();
    harness.assert_evaluates_to_boolean("p1 == p3", false).unwrap();
    harness.assert_evaluates_to_boolean("p1 != p3", true).unwrap();
}

#[test]
fn test_equality_override_runtime_behavior() {
    let mut harness = InterpreterSession::new().unwrap();
    
    // Test that manual override works at runtime
    harness.evaluate(r#"
        struct AlwaysEqual(value: Integer) {}
        
        impl Equality for AlwaysEqual {
            def equal?(lhs: Self, rhs: Self): Boolean {
                true  # Always equal for testing
            }
        }
        
        let a1 = AlwaysEqual { value: 1 }
        let a2 = AlwaysEqual { value: 999 }
    "#).unwrap();
    
    // Should use the custom implementation
    harness.assert_evaluates_to_boolean("a1 == a2", true).unwrap();
}

#[test]
fn test_built_in_types_equality() {
    let mut harness = InterpreterSession::new().unwrap();
    
    // Test that built-in types work with equality
    harness.assert_evaluates_to_boolean("42 == 42", true).unwrap();
    harness.assert_evaluates_to_boolean("42 == 43", false).unwrap();
    harness.assert_evaluates_to_boolean("42 != 43", true).unwrap();
    
    harness.assert_evaluates_to_boolean(r#""hello" == "hello""#, true).unwrap();
    harness.assert_evaluates_to_boolean(r#""hello" == "world""#, false).unwrap();
    harness.assert_evaluates_to_boolean(r#""hello" != "world""#, true).unwrap();
    
    harness.assert_evaluates_to_boolean("true == true", true).unwrap();
    harness.assert_evaluates_to_boolean("true == false", false).unwrap();
    harness.assert_evaluates_to_boolean("true != false", true).unwrap();
}

#[test]
fn test_boolean_operations_now_work() {
    let mut harness = InterpreterSession::new().unwrap();
    
    // These should now work with the fixed intrinsic names
    harness.assert_evaluates_to_boolean("true && false", false).unwrap();
    harness.assert_evaluates_to_boolean("true || false", true).unwrap();
    harness.assert_evaluates_to_boolean("!true", false).unwrap();
    harness.assert_evaluates_to_boolean("!false", true).unwrap();
}

#[test]
fn test_complex_equality_expressions() {
    let mut harness = InterpreterSession::new().unwrap();
    
    // Test complex expressions involving equality
    harness.assert_evaluates_to_boolean("(1 + 2) == 3", true).unwrap();
    harness.assert_evaluates_to_boolean("(5 * 2) != 9", true).unwrap();
    
    // Test equality in boolean expressions
    harness.assert_evaluates_to_boolean("(1 == 1) && (2 == 2)", true).unwrap();
    harness.assert_evaluates_to_boolean("(1 == 2) || (3 == 3)", true).unwrap();
}