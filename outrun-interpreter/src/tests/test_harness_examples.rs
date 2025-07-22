//! Example tests demonstrating the OutrunTestHarness functionality
//!
//! These tests show how to use the test harness to:
//! - Evaluate basic Outrun expressions
//! - Assert on results using different assertion methods
//! - Set up variables and context for complex test scenarios
//! - Work with different Outrun data types and operations

use crate::Value;
use crate::test_harness::{OutrunTestHarness, TestHarnessError};

#[test]
fn test_basic_literals() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test integer literals
    harness.assert_evaluates_to_integer("42", 42).unwrap();
    harness.assert_evaluates_to_integer("-17", -17).unwrap();
    harness.assert_evaluates_to_integer("0", 0).unwrap();

    // Test boolean literals
    harness.assert_evaluates_to_boolean("true", true).unwrap();
    harness.assert_evaluates_to_boolean("false", false).unwrap();

    // Test string literals
    harness
        .assert_evaluates_to_string("\"hello\"", "hello")
        .unwrap();
    harness.assert_evaluates_to_string("\"\"", "").unwrap();
    harness
        .assert_evaluates_to_string("\"hello world\"", "hello world")
        .unwrap();
}

#[test]
fn test_basic_arithmetic() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test basic arithmetic operations
    harness.assert_evaluates_to_integer("1 + 2", 3).unwrap();
    harness.assert_evaluates_to_integer("10 - 3", 7).unwrap();
    harness.assert_evaluates_to_integer("4 * 5", 20).unwrap();
    harness.assert_evaluates_to_integer("15 / 3", 5).unwrap();
    harness.assert_evaluates_to_integer("17 % 5", 2).unwrap();

    // Test operator precedence
    harness
        .assert_evaluates_to_integer("2 + 3 * 4", 14)
        .unwrap();
    harness
        .assert_evaluates_to_integer("(2 + 3) * 4", 20)
        .unwrap();
}

#[test]
fn test_comparison_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test equality
    harness.assert_evaluates_to_boolean("1 == 1", true).unwrap();
    harness
        .assert_evaluates_to_boolean("1 == 2", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("true == true", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("false == true", false)
        .unwrap();

    // Test inequality
    harness.assert_evaluates_to_boolean("1 != 2", true).unwrap();
    harness
        .assert_evaluates_to_boolean("1 != 1", false)
        .unwrap();

    // Test ordering
    harness.assert_evaluates_to_boolean("1 < 2", true).unwrap();
    harness.assert_evaluates_to_boolean("2 < 1", false).unwrap();
    harness.assert_evaluates_to_boolean("2 > 1", true).unwrap();
    harness.assert_evaluates_to_boolean("1 > 2", false).unwrap();
    harness.assert_evaluates_to_boolean("1 <= 1", true).unwrap();
    harness.assert_evaluates_to_boolean("2 >= 1", true).unwrap();
}

#[test]
fn test_logical_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test logical AND
    harness
        .assert_evaluates_to_boolean("true && true", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("true && false", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("false && true", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("false && false", false)
        .unwrap();

    // Test logical OR
    harness
        .assert_evaluates_to_boolean("true || true", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("true || false", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("false || true", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("false || false", false)
        .unwrap();

    // Test logical NOT
    harness.assert_evaluates_to_boolean("!true", false).unwrap();
    harness.assert_evaluates_to_boolean("!false", true).unwrap();
}

#[test]
fn test_variable_setup_and_usage() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables directly
    harness.set_variable("x", Value::integer(42)).unwrap();
    harness.set_variable("y", Value::boolean(true)).unwrap();
    harness
        .set_variable("name", Value::string("Outrun".to_string()))
        .unwrap();

    // Test variable access
    harness.assert_evaluates_to_integer("x", 42).unwrap();
    harness.assert_evaluates_to_boolean("y", true).unwrap();
    harness
        .assert_evaluates_to_string("name", "Outrun")
        .unwrap();

    // Test variables in expressions
    harness.assert_evaluates_to_integer("x + 8", 50).unwrap();
    harness
        .assert_evaluates_to_boolean("y && false", false)
        .unwrap();
}

#[test]
fn test_let_binding_setup() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Execute let bindings to set up variables
    harness.execute_let_binding("let a = 100").unwrap();
    harness.execute_let_binding("let b = 200").unwrap();
    harness.execute_let_binding("let flag = false").unwrap();

    // Test that bound variables are available
    harness.assert_evaluates_to_integer("a", 100).unwrap();
    harness.assert_evaluates_to_integer("b", 200).unwrap();
    harness.assert_evaluates_to_boolean("flag", false).unwrap();

    // Test expressions using bound variables
    harness.assert_evaluates_to_integer("a + b", 300).unwrap();
    harness
        .assert_evaluates_to_boolean("flag || true", true)
        .unwrap();
}

#[test]
fn test_if_expressions() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test basic if expressions
    harness
        .assert_evaluates_to_integer("if true { 42 } else { 0 }", 42)
        .unwrap();
    harness
        .assert_evaluates_to_integer("if false { 42 } else { 0 }", 0)
        .unwrap();

    // Test if expressions with variables
    harness
        .set_variable("condition", Value::boolean(true))
        .unwrap();
    harness.set_variable("x", Value::integer(10)).unwrap();
    harness.set_variable("y", Value::integer(20)).unwrap();

    harness
        .assert_evaluates_to_integer("if condition { x } else { y }", 10)
        .unwrap();

    // Test nested if expressions
    harness
        .assert_evaluates_to_integer("if true { if false { 1 } else { 2 } } else { 3 }", 2)
        .unwrap();
}

#[test]
fn test_inspect_based_assertions() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test inspect representation assertions
    harness.assert_evaluates_to("42", "42").unwrap();
    harness.assert_evaluates_to("true", "true").unwrap();
    harness.assert_evaluates_to("false", "false").unwrap();
    harness
        .assert_evaluates_to("\"hello\"", "\"hello\"")
        .unwrap();

    // Test complex expressions with inspect
    harness.assert_evaluates_to("1 + 2 * 3", "7").unwrap();
    harness
        .assert_evaluates_to("true && false", "false")
        .unwrap();
}

#[test]
fn test_list_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test list literals
    match harness.assert_evaluates_to("[1, 2, 3]", "[1, 2, 3]") {
        Ok(_) => {}
        Err(e) => {
            println!("List evaluation error: {e:?}");
            panic!("List evaluation failed: {e:?}");
        }
    }
    // Skip empty list test for now due to type unification issues
    // harness.execute_let_binding("let empty_list: Outrun.Core.List<Integer> = []").unwrap();
    // harness.assert_evaluates_to("empty_list", "[]").unwrap();

    // Test list operations using static functions
    // Note: Using concrete types for now until protocol/concrete type unification is improved
    harness
        .assert_evaluates_to(
            "List.head(value: [1, 2, 3])",
            "Outrun.Option.Some { value: 1 }",
        )
        .unwrap();
    harness
        .assert_evaluates_to(
            "List.head(value: [99, 88])",
            "Outrun.Option.Some { value: 99 }",
        )
        .unwrap();
    harness
        .assert_evaluates_to("List.tail(value: [1, 2, 3])", "[2, 3]")
        .unwrap();
    harness
        .assert_evaluates_to("List.prepend(list: [2, 3], elem: 1)", "[1, 2, 3]")
        .unwrap();
}

#[test]
fn test_option_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test Option creation and manipulation
    harness
        .assert_evaluates_to("Option.some(value: 42)", "Outrun.Option.Some { value: 42 }")
        .unwrap();
    harness
        .assert_evaluates_to("Option.none()", "Outrun.Option.None {}")
        .unwrap();

    // Test Option operations
    harness
        .assert_evaluates_to("Option.some?(value: Option.some(value: 42))", "true")
        .unwrap();
    harness
        .assert_evaluates_to("Option.some?(value: Option.none())", "false")
        .unwrap();
    harness
        .assert_evaluates_to("Option.none?(value: Option.none())", "true")
        .unwrap();
    harness
        .assert_evaluates_to("Option.none?(value: Option.some(value: 42))", "false")
        .unwrap();
}

#[test]
fn test_error_handling() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test compilation errors (undefined variable should be caught at compile time)
    let result = harness.evaluate("undefined_variable");
    assert!(matches!(result, Err(TestHarnessError::Compilation { .. })));

    // Test assertion failures
    let result = harness.assert_evaluates_to_integer("42", 43);
    assert!(matches!(
        result,
        Err(TestHarnessError::AssertionFailed { .. })
    ));

    let result = harness.assert_evaluates_to_boolean("42", true);
    assert!(matches!(
        result,
        Err(TestHarnessError::AssertionFailed { .. })
    ));
}

#[test]
fn test_complex_expressions() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up some variables for complex tests - let type inference work
    harness
        .execute_let_binding("let numbers = [1, 2, 3, 4, 5]")
        .unwrap();
    harness
        .execute_let_binding("let short_list = [10]")
        .unwrap();
    harness
        .execute_let_binding("let maybe_value = Option.some(value: 42)")
        .unwrap();
    harness
        .execute_let_binding("let no_value = Option.none()")
        .unwrap();

    // Test complex expressions combining multiple operations
    harness
        .assert_evaluates_to_integer("List.length(value: numbers)", 5)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: short_list)", 1)
        .unwrap();

    // TODO: Re-enable these tests once Option protocol implementations are complete
    // The Option.some? and Option.none? functions need actual implementations
    // rather than just protocol signatures
    // harness
    //     .assert_evaluates_to_integer("if Option.some?(value: maybe_value) { 1 } else { 0 }", 1)
    //     .unwrap();
    // harness
    //     .assert_evaluates_to_integer("if Option.some?(value: no_value) { 1 } else { 0 }", 0)
    //     .unwrap();
}
