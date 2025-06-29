//! Acceptance tests for Float unary operations

use crate::test_harness::OutrunTestHarness;

#[test]
fn test_unary_plus() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Positive numbers (should remain unchanged)
    harness.assert_evaluates_to_float("+3.25", 3.25).unwrap();
    harness.assert_evaluates_to_float("+1.0", 1.0).unwrap();
    harness.assert_evaluates_to_float("+42.5", 42.5).unwrap();

    // Negative numbers (should remain unchanged)
    harness.assert_evaluates_to_float("+-3.25", -3.25).unwrap();
    harness.assert_evaluates_to_float("+-1.0", -1.0).unwrap();

    // Zero (should remain unchanged)
    harness.assert_evaluates_to_float("+0.0", 0.0).unwrap();

    // Large numbers
    harness
        .assert_evaluates_to_float("+1000000.5", 1000000.5)
        .unwrap();

    // Small numbers
    harness.assert_evaluates_to_float("+0.001", 0.001).unwrap();
}

#[test]
fn test_unary_minus() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Positive numbers (should become negative)
    harness.assert_evaluates_to_float("-3.25", -3.25).unwrap();
    harness.assert_evaluates_to_float("-1.0", -1.0).unwrap();
    harness.assert_evaluates_to_float("-42.5", -42.5).unwrap();

    // Negative numbers (should become positive)
    harness.assert_evaluates_to_float("--3.25", 3.25).unwrap();
    harness.assert_evaluates_to_float("--1.0", 1.0).unwrap();

    // Zero (should remain zero)
    harness.assert_evaluates_to_float("-0.0", 0.0).unwrap();

    // Large numbers
    harness
        .assert_evaluates_to_float("-1000000.5", -1000000.5)
        .unwrap();

    // Small numbers
    harness.assert_evaluates_to_float("-0.001", -0.001).unwrap();
}

#[test]
fn test_abs() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Positive numbers (should remain unchanged)
    harness
        .assert_evaluates_to_float("Float.abs(value: 3.25)", 3.25)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.abs(value: 1.0)", 1.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.abs(value: 42.5)", 42.5)
        .unwrap();

    // Negative numbers (should become positive)
    harness
        .assert_evaluates_to_float("Float.abs(value: -3.25)", 3.25)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.abs(value: -1.0)", 1.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.abs(value: -42.5)", 42.5)
        .unwrap();

    // Zero (should remain zero)
    harness
        .assert_evaluates_to_float("Float.abs(value: 0.0)", 0.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.abs(value: -0.0)", 0.0)
        .unwrap();

    // Large numbers
    harness
        .assert_evaluates_to_float("Float.abs(value: 1000000.5)", 1000000.5)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.abs(value: -1000000.5)", 1000000.5)
        .unwrap();

    // Small numbers
    harness
        .assert_evaluates_to_float("Float.abs(value: 0.001)", 0.001)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.abs(value: -0.001)", 0.001)
        .unwrap();
}

#[test]
fn test_trunc() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic truncation operations
    harness
        .assert_evaluates_to_float("Float.trunc(value: 3.25)", 3.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.trunc(value: 3.9)", 3.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.trunc(value: -3.25)", -3.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.trunc(value: -3.9)", -3.0)
        .unwrap();

    // Zero
    harness
        .assert_evaluates_to_float("Float.trunc(value: 0.0)", 0.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.trunc(value: 0.9)", 0.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.trunc(value: -0.9)", 0.0)
        .unwrap();

    // Integer values (should remain unchanged)
    harness
        .assert_evaluates_to_float("Float.trunc(value: 3.0)", 3.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.trunc(value: -3.0)", -3.0)
        .unwrap();

    // Large numbers
    harness
        .assert_evaluates_to_float("Float.trunc(value: 999999.9)", 999999.0)
        .unwrap();
}

#[test]
fn test_zero_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Zero should return true
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: 0.0)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: -0.0)", true)
        .unwrap();

    // Non-zero positive numbers should return false
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: 1.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: 3.25)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: 0.001)", false)
        .unwrap();

    // Non-zero negative numbers should return false
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: -1.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: -3.25)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: -0.001)", false)
        .unwrap();
}

#[test]
fn test_positive_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Positive numbers should return true
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: 1.0)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: 3.25)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: 0.001)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: 1000000.5)", true)
        .unwrap();

    // Zero should return false
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: 0.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: -0.0)", false)
        .unwrap();

    // Negative numbers should return false
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: -1.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: -3.25)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: -0.001)", false)
        .unwrap();
}

#[test]
fn test_negative_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Negative numbers should return true
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: -1.0)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: -3.25)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: -0.001)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: -1000000.5)", true)
        .unwrap();

    // Zero should return false
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: 0.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: -0.0)", false)
        .unwrap();

    // Positive numbers should return false
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: 1.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: 3.25)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: 0.001)", false)
        .unwrap();
}

#[test]
fn test_finite_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Regular finite numbers should return true
    harness
        .assert_evaluates_to_boolean("Float.finite?(value: 0.0)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.finite?(value: 1.0)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.finite?(value: -1.0)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.finite?(value: 3.25)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.finite?(value: -3.25)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.finite?(value: 1000000.5)", true)
        .unwrap();

    // Very small numbers should still be finite
    harness
        .assert_evaluates_to_boolean("Float.finite?(value: 0.001)", true)
        .unwrap();
}

#[test]
fn test_infinite_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Regular finite numbers should return false
    harness
        .assert_evaluates_to_boolean("Float.infinite?(value: 0.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.infinite?(value: 1.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.infinite?(value: -1.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.infinite?(value: 3.25)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.infinite?(value: 1000000.5)", false)
        .unwrap();

    // Large finite numbers should still be finite (not infinite)
    harness
        .assert_evaluates_to_boolean("Float.infinite?(value: 999999999.9)", false)
        .unwrap();
}

#[test]
fn test_nan_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Regular finite numbers should return false
    harness
        .assert_evaluates_to_boolean("Float.nan?(value: 0.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.nan?(value: 1.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.nan?(value: -1.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.nan?(value: 3.25)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.nan?(value: -3.25)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.nan?(value: 1000000.5)", false)
        .unwrap();

    // Zero should not be NaN
    harness
        .assert_evaluates_to_boolean("Float.nan?(value: 0.0)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.nan?(value: -0.0)", false)
        .unwrap();
}

#[test]
fn test_unary_operations_with_expressions() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test unary operators with expressions
    harness
        .assert_evaluates_to_float("+(1.5 + 1.5)", 3.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("-(2.0 + 1.0)", -3.0)
        .unwrap();

    // Test Float functions with expressions
    harness
        .assert_evaluates_to_float("Float.abs(value: 1.0 - 4.0)", 3.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.trunc(value: 1.0 + 2.9)", 3.0)
        .unwrap();

    // Test predicate functions with expressions
    harness
        .assert_evaluates_to_boolean("Float.zero?(value: 1.0 - 1.0)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.positive?(value: 1.0 + 2.0)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Float.negative?(value: 1.0 - 4.0)", true)
        .unwrap();
}

#[test]
fn test_chained_unary_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Chained unary operators
    harness.assert_evaluates_to_float("++3.25", 3.25).unwrap();
    harness.assert_evaluates_to_float("--3.25", 3.25).unwrap();
    harness.assert_evaluates_to_float("+-3.25", -3.25).unwrap();
    harness.assert_evaluates_to_float("-+3.25", -3.25).unwrap();

    // Triple unary operators
    harness.assert_evaluates_to_float("+++3.25", 3.25).unwrap();
    harness.assert_evaluates_to_float("---3.25", -3.25).unwrap();

    // Combination with Float functions
    harness
        .assert_evaluates_to_float("Float.abs(value: --3.25)", 3.25)
        .unwrap();
    harness
        .assert_evaluates_to_float("-Float.abs(value: -3.25)", -3.25)
        .unwrap();
}

#[test]
fn test_type_consistency() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test that unary operators return floats usable in arithmetic
    harness
        .assert_evaluates_to_float("+3.0 + 1.0", 4.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("-3.0 + 1.0", -2.0)
        .unwrap();

    // Test that Float functions return floats usable in arithmetic
    harness
        .assert_evaluates_to_float("Float.abs(value: -3.0) + 1.0", 4.0)
        .unwrap();
    harness
        .assert_evaluates_to_float("Float.trunc(value: 3.5) + 1.0", 4.0)
        .unwrap();

    // Test that predicate functions return booleans
    let result1 = harness.evaluate("Float.zero?(value: 0.0)").unwrap();
    let result2 = harness.evaluate("Float.positive?(value: 3.25)").unwrap();
    let result3 = harness.evaluate("Float.negative?(value: -3.25)").unwrap();

    // These should be boolean values
    assert!(matches!(result1, crate::Value::Boolean(true)));
    assert!(matches!(result2, crate::Value::Boolean(true)));
    assert!(matches!(result3, crate::Value::Boolean(true)));
}
