//! Acceptance tests for basic literal values in the new interpreter
//!
//! These tests verify that the new interpreter can correctly evaluate
//! all basic literal types supported by Outrun.

use crate::InterpreterSession;

#[test]
fn test_integer_literals() {
    let mut harness = InterpreterSession::new().unwrap();

    // Test positive integers
    harness.assert_evaluates_to_integer("42", 42).unwrap();
    harness.assert_evaluates_to_integer("0", 0).unwrap();
    harness.assert_evaluates_to_integer("1", 1).unwrap();
    harness.assert_evaluates_to_integer("999", 999).unwrap();

    // Test negative integers
    harness.assert_evaluates_to_integer("-1", -1).unwrap();
    harness.assert_evaluates_to_integer("-42", -42).unwrap();
    harness.assert_evaluates_to_integer("-999", -999).unwrap();

    // Large integer edge cases removed - will be replaced by arbitrary precision math
}

#[test]
fn test_boolean_literals() {
    let mut harness = InterpreterSession::new().unwrap();

    harness.assert_evaluates_to_boolean("true", true).unwrap();
    harness.assert_evaluates_to_boolean("false", false).unwrap();
}

#[test]
fn test_string_literals() {
    let mut harness = InterpreterSession::new().unwrap();

    // Test basic strings
    harness
        .assert_evaluates_to_string("\"hello\"", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("\"world\"", "world")
        .unwrap();
    harness.assert_evaluates_to_string("\"\"", "").unwrap();

    // Test strings with spaces
    harness
        .assert_evaluates_to_string("\"hello world\"", "hello world")
        .unwrap();
    harness.assert_evaluates_to_string("\" \"", " ").unwrap();

    // Test strings with special characters
    harness
        .assert_evaluates_to_string("\"hello\\nworld\"", "hello\nworld")
        .unwrap();
    harness
        .assert_evaluates_to_string("\"hello\\tworld\"", "hello\tworld")
        .unwrap();
    harness
        .assert_evaluates_to_string("\"\\\"quoted\\\"\"", "\"quoted\"")
        .unwrap();
}

#[test]
fn test_atom_literals() {
    let mut harness = InterpreterSession::new().unwrap();

    harness.assert_evaluates_to_atom(":hello", "hello").unwrap();
    harness.assert_evaluates_to_atom(":world", "world").unwrap();
    harness.assert_evaluates_to_atom(":test", "test").unwrap();
    harness.assert_evaluates_to_atom(":ok", "ok").unwrap();
    harness.assert_evaluates_to_atom(":error", "error").unwrap();
}

#[test]
fn test_literal_combinations_with_variables() {
    let mut harness = InterpreterSession::new().unwrap();

    // Test let bindings with different literal types
    harness.evaluate("let int_var = 42").unwrap();
    harness.assert_evaluates_to_integer("int_var", 42).unwrap();

    harness.evaluate("let bool_var = true").unwrap();
    harness
        .assert_evaluates_to_boolean("bool_var", true)
        .unwrap();

    harness.evaluate("let string_var = \"hello\"").unwrap();
    harness
        .assert_evaluates_to_string("string_var", "hello")
        .unwrap();

    harness.evaluate("let atom_var = :test").unwrap();
    harness
        .assert_evaluates_to_atom("atom_var", "test")
        .unwrap();

    // Test accessing multiple variables
    harness.assert_evaluates_to_integer("int_var", 42).unwrap();
    harness
        .assert_evaluates_to_boolean("bool_var", true)
        .unwrap();
    harness
        .assert_evaluates_to_string("string_var", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_atom("atom_var", "test")
        .unwrap();
}

#[test]
fn test_literal_edge_cases() {
    let mut harness = InterpreterSession::new().unwrap();

    // Test edge case integers
    harness.assert_evaluates_to_integer("0", 0).unwrap();
    harness.assert_evaluates_to_integer("-0", 0).unwrap();

    // Test empty string
    harness.assert_evaluates_to_string("\"\"", "").unwrap();

    // Test single character strings
    harness.assert_evaluates_to_string("\"a\"", "a").unwrap();
    harness.assert_evaluates_to_string("\"1\"", "1").unwrap();
    harness.assert_evaluates_to_string("\" \"", " ").unwrap();

    // Test single character atoms
    harness.assert_evaluates_to_atom(":a", "a").unwrap();
    harness.assert_evaluates_to_atom(":x", "x").unwrap();
}

#[test]
fn test_variable_shadowing_with_literals() {
    let mut harness = InterpreterSession::new().unwrap();

    // Define a variable
    harness.evaluate("let x = 42").unwrap();
    harness.assert_evaluates_to_integer("x", 42).unwrap();

    // Shadow it with a different type (this tests type flexibility)
    harness.evaluate("let x = \"hello\"").unwrap();
    harness.assert_evaluates_to_string("x", "hello").unwrap();

    // Shadow it again with another type
    harness.evaluate("let x = true").unwrap();
    harness.assert_evaluates_to_boolean("x", true).unwrap();

    // Shadow it with an atom
    harness.evaluate("let x = :test").unwrap();
    harness.assert_evaluates_to_atom("x", "test").unwrap();
}

#[test]
fn test_multiple_variable_scopes() {
    let mut harness = InterpreterSession::new().unwrap();

    // Test that variables persist across evaluations
    harness.evaluate("let a = 1").unwrap();
    harness.evaluate("let b = 2").unwrap();
    harness.evaluate("let c = 3").unwrap();

    harness.assert_evaluates_to_integer("a", 1).unwrap();
    harness.assert_evaluates_to_integer("b", 2).unwrap();
    harness.assert_evaluates_to_integer("c", 3).unwrap();

    // Test that we can still define more variables
    harness.evaluate("let d = \"hello\"").unwrap();
    harness.evaluate("let e = true").unwrap();

    // All should still be accessible
    harness.assert_evaluates_to_integer("a", 1).unwrap();
    harness.assert_evaluates_to_integer("b", 2).unwrap();
    harness.assert_evaluates_to_integer("c", 3).unwrap();
    harness.assert_evaluates_to_string("d", "hello").unwrap();
    harness.assert_evaluates_to_boolean("e", true).unwrap();
}
