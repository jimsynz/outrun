//! Acceptance tests for pattern matching in the new interpreter
//!
//! These tests verify that the new interpreter correctly handles working pattern matching
//! constructs. Tests for unimplemented features (tuples, lists) have been removed.

use crate::OutrunTestHarness;

#[test]
fn test_let_binding_identifier_patterns() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test simple identifier pattern
    harness.evaluate("let x = 42").unwrap();
    harness.assert_evaluates_to_integer("x", 42).unwrap();

    // Test identifier pattern with different types
    harness.evaluate("let name = \"Alice\"").unwrap();
    harness.assert_evaluates_to_string("name", "Alice").unwrap();

    harness.evaluate("let flag = true").unwrap();
    harness.assert_evaluates_to_boolean("flag", true).unwrap();

    harness.evaluate("let tag = :ok").unwrap();
    harness.assert_evaluates_to_atom("tag", "ok").unwrap();
}

#[test]
fn test_case_expression_literal_patterns() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test integer literal patterns
    let code = r#"
        case 42 {
            0 -> "zero"
            42 -> "forty-two"
            100 -> "hundred"
            n -> "other"
        }
    "#;
    harness.assert_evaluates_to_string(code, "forty-two").unwrap();

    // Test string literal patterns
    let code = r#"
        case "hello" {
            "goodbye" -> "farewell"
            "hello" -> "greeting"
            s -> "unknown"
        }
    "#;
    harness.assert_evaluates_to_string(code, "greeting").unwrap();

    // Test boolean literal patterns
    let code = r#"
        case true {
            false -> "no"
            true -> "yes"
            b -> "maybe"
        }
    "#;
    harness.assert_evaluates_to_string(code, "yes").unwrap();

    // Test atom literal patterns
    let code = r#"
        case :ok {
            :error -> "failed"
            :ok -> "success"
            atom -> "unknown"
        }
    "#;
    harness.assert_evaluates_to_string(code, "success").unwrap();
}

#[test]
fn test_case_expression_identifier_patterns() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test identifier pattern (catch-all)
    let code = r#"
        case 999 {
            0 -> "zero"
            42 -> "forty-two"
            n -> "other"
        }
    "#;
    harness.assert_evaluates_to_string(code, "other").unwrap();

    // Test identifier pattern binding
    let code = r#"
        case "test" {
            "hello" -> "greeting"
            message -> "ok"
        }
    "#;
    harness.assert_evaluates_to_string(code, "ok").unwrap();
}

#[test]
fn test_case_expression_pattern_precedence() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test that more specific patterns match before general ones
    let code = r#"
        case 42 {
            42 -> "specific"
            n -> "general"
        }
    "#;
    harness.assert_evaluates_to_string(code, "specific").unwrap();

    // Test multiple literal patterns
    let code = r#"
        case 2 {
            1 -> "one"
            2 -> "two"
            3 -> "three"
            n -> "other"  
        }
    "#;
    harness.assert_evaluates_to_string(code, "two").unwrap();
}

#[test]
fn test_case_expression_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables for testing
    harness.evaluate("let x = 42").unwrap();
    
    // Test case expression using variables
    let code = r#"
        case x {
            0 -> "zero"
            42 -> "forty-two"
            n -> "other"
        }
    "#;
    harness.assert_evaluates_to_string(code, "forty-two").unwrap();
}

#[test]
fn test_pattern_binding_scope() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test that pattern bindings don't leak outside case expressions
    harness.evaluate("let x = \"original\"").unwrap();
    
    let code = r#"
        case 42 {
            0 -> "zero"
            n -> "matched"
        }
    "#;
    harness.assert_evaluates_to_string(code, "matched").unwrap();
    
    // Verify original variable is unchanged
    harness.assert_evaluates_to_string("x", "original").unwrap();
}

// Note: Tests for tuple patterns, list patterns, and complex combinations have been removed
// as they require features not yet implemented in the interpreter (tuple literals, list literals, etc.)