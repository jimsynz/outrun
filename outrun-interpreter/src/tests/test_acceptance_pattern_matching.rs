//! Acceptance tests for pattern matching in the new interpreter
//!
//! These tests verify that the new interpreter correctly handles all pattern matching
//! constructs including let bindings, case expressions, and various pattern types.

use crate::{OutrunTestHarness, Value};

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

    harness.evaluate("let tag = :success").unwrap();
    harness.assert_evaluates_to_atom("tag", "success").unwrap();
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
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "forty-two")
        .unwrap();

    // Test boolean literal patterns
    let code = r#"
        case true {
            true -> "yes"
            false -> "no"
        }
    "#;
    harness.assert_evaluates_to_string(code, "yes").unwrap();

    // Test string literal patterns
    let code = r#"
        case "hello" {
            "world" -> "greeting world"
            "hello" -> "greeting hello"
            "" -> "empty"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "greeting hello")
        .unwrap();

    // Test atom literal patterns
    let code = r#"
        case :success {
            :error -> "failed"
            :success -> "succeeded"
            :pending -> "waiting"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "succeeded")
        .unwrap();
}

#[test]
fn test_case_expression_identifier_patterns() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test identifier pattern (catch-all)
    let code = r#"
        case 123 {
            0 -> "zero"
            x -> "other"
        }
    "#;
    harness.assert_evaluates_to_string(code, "other").unwrap();

    // Test that identifier pattern binds the value
    let code = r#"
        case "test" {
            "hello" -> "greeting"
            value -> value
        }
    "#;
    harness.assert_evaluates_to_string(code, "test").unwrap();
}

#[test]
fn test_case_expression_tuple_patterns() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test simple tuple pattern
    let code = r#"
        case (1, 2) {
            (0, 0) -> "origin"
            (1, 2) -> "one-two"
            (x, y) -> "other"
        }
    "#;
    harness.assert_evaluates_to_string(code, "one-two").unwrap();

    // Test tuple pattern with identifier binding
    let code = r#"
        case (42, "hello") {
            (0, msg) -> "zero"
            (num, "hello") -> "greeting"
            (x, y) -> "other"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "greeting")
        .unwrap();

    // Test nested tuple pattern
    let code = r#"
        case ((1, 2), 3) {
            ((0, 0), z) -> "nested-origin"
            ((1, 2), 3) -> "nested-match"
            (pair, z) -> "other"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "nested-match")
        .unwrap();
}

#[test]
fn test_case_expression_list_patterns() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test empty list pattern
    let code = r#"
        case [] {
            [] -> "empty"
            [x] -> "single"
            [x, y] -> "pair"
            list -> "other"
        }
    "#;
    harness.assert_evaluates_to_string(code, "empty").unwrap();

    // Test single element list pattern
    let code = r#"
        case [42] {
            [] -> "empty"
            [42] -> "forty-two"
            [x] -> "single"
            list -> "other"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "forty-two")
        .unwrap();

    // Test multiple element list pattern
    let code = r#"
        case [1, 2, 3] {
            [] -> "empty"
            [x] -> "single"
            [1, 2, 3] -> "one-two-three"
            list -> "other"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "one-two-three")
        .unwrap();

    // Test list pattern with identifier binding
    let code = r#"
        case [1, 2] {
            [] -> "empty"
            [x] -> "single"
            [first, second] -> "pair"
            list -> "other"
        }
    "#;
    harness.assert_evaluates_to_string(code, "pair").unwrap();
}

#[test]
fn test_case_expression_pattern_precedence() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test that more specific patterns match before general ones
    let code = r#"
        case 42 {
            42 -> "specific"
            x -> "general"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "specific")
        .unwrap();

    // Test with lists
    let code = r#"
        case [1, 2] {
            [1, 2] -> "specific"
            [x, y] -> "general-pair"
            list -> "general"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "specific")
        .unwrap();
}

#[test]
fn test_case_expression_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables for testing
    harness.evaluate("let test_value = 42").unwrap();
    harness.evaluate("let test_string = \"hello\"").unwrap();

    // Test case expression with variable input
    let code = r#"
        case test_value {
            0 -> "zero"
            42 -> "forty-two"
            x -> "other"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "forty-two")
        .unwrap();

    // Test case expression with string variable
    let code = r#"
        case test_string {
            "world" -> "greeting world"
            "hello" -> "greeting hello"
            s -> "other greeting"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "greeting hello")
        .unwrap();
}

#[test]
fn test_pattern_binding_scope() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test that pattern bindings are scoped to the case arm
    harness.evaluate("let x = 100").unwrap();

    let code = r#"
        case 42 {
            0 -> "zero"
            x -> "matched"
        }
    "#;
    harness.assert_evaluates_to_string(code, "matched").unwrap();

    // Verify original variable is unchanged
    harness.assert_evaluates_to_integer("x", 100).unwrap();
}

#[test]
fn test_complex_pattern_combinations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test tuple with list elements
    let code = r#"
        case ([1, 2], "test") {
            ([], msg) -> "empty list"
            ([x], msg) -> "single list"
            ([1, 2], "test") -> "matched"
            (list, msg) -> "other"
        }
    "#;
    harness.assert_evaluates_to_string(code, "matched").unwrap();

    // Test nested patterns with binding
    let code = r#"
        case ((1, [2, 3]), "outer") {
            ((0, list), msg) -> "zero first"
            ((1, [2, 3]), "outer") -> "exact match"
            ((x, list), msg) -> "partial match"
            other -> "no match"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "exact match")
        .unwrap();
}

#[test]
fn test_let_binding_with_complex_patterns() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // For now, test harness only supports identifier patterns in let bindings
    // This test verifies the current limitation and can be expanded later

    // Test simple identifier pattern (currently supported)
    harness.evaluate("let result = \"success\"").unwrap();
    harness
        .assert_evaluates_to_string("result", "success")
        .unwrap();

    // TODO: Add tests for tuple destructuring in let bindings when implemented
    // let (x, y) = (1, 2)
    // let [first, second] = [10, 20]
}

#[test]
fn test_pattern_matching_edge_cases() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test empty string pattern
    let code = r#"
        case "" {
            "" -> "empty string"
            s -> "non-empty"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "empty string")
        .unwrap();

    // Test zero pattern
    let code = r#"
        case 0 {
            0 -> "zero"
            n -> "non-zero"
        }
    "#;
    harness.assert_evaluates_to_string(code, "zero").unwrap();

    // Test negative number pattern
    let code = r#"
        case -42 {
            0 -> "zero"
            -42 -> "negative forty-two"
            n -> "other"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "negative forty-two")
        .unwrap();
}

#[test]
fn test_pattern_matching_type_consistency() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test that pattern matching works across different value types

    // Integer patterns
    let code = r#"
        case 123 {
            123 -> :integer_match
            x -> :no_match
        }
    "#;
    harness
        .assert_evaluates_to_atom(code, "integer_match")
        .unwrap();

    // Boolean patterns
    let code = r#"
        case false {
            true -> :true_match
            false -> :false_match
        }
    "#;
    harness
        .assert_evaluates_to_atom(code, "false_match")
        .unwrap();

    // String patterns
    let code = r#"
        case "pattern" {
            "pattern" -> :string_match
            s -> :no_match
        }
    "#;
    harness
        .assert_evaluates_to_atom(code, "string_match")
        .unwrap();

    // Atom patterns
    let code = r#"
        case :test {
            :other -> :wrong
            :test -> :atom_match
        }
    "#;
    harness
        .assert_evaluates_to_atom(code, "atom_match")
        .unwrap();
}
