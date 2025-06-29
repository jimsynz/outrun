//! Acceptance tests for Integer trait functions

use crate::test_harness::OutrunTestHarness;

#[test]
fn test_to_string_radix() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Decimal (base 10) - default representation
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 42, radix: 10)", "42")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 0, radix: 10)", "0")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: -17, radix: 10)", "-17")
        .unwrap();

    // Binary (base 2)
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 42, radix: 2)", "101010")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 8, radix: 2)", "1000")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 1, radix: 2)", "1")
        .unwrap();

    // Hexadecimal (base 16)
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 42, radix: 16)", "2a")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 255, radix: 16)", "ff")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 16, radix: 16)", "10")
        .unwrap();

    // Octal (base 8)
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 42, radix: 8)", "52")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 64, radix: 8)", "100")
        .unwrap();

    // Other common bases
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 42, radix: 3)", "1120")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 42, radix: 5)", "132")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 42, radix: 7)", "60")
        .unwrap();

    // Maximum base (36) - uses digits 0-9 and letters a-z
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 42, radix: 36)", "16")
        .unwrap();
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: 35, radix: 36)", "z")
        .unwrap();
}

#[test]
fn test_to_string_radix_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables
    harness.execute_let_binding("let num = 42").unwrap();
    harness.execute_let_binding("let hex_base = 16").unwrap();
    harness.execute_let_binding("let bin_base = 2").unwrap();

    // Test with variables
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: num, radix: hex_base)", "2a")
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Integer.to_string_radix(value: num, radix: bin_base)",
            "101010",
        )
        .unwrap();

    // Test with expression as value
    harness
        .assert_evaluates_to_string("Integer.to_string_radix(value: num + 8, radix: 16)", "32")
        .unwrap();
}

#[test]
fn test_abs() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Positive numbers (should remain unchanged)
    harness
        .assert_evaluates_to_integer("Integer.abs(value: 42)", 42)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Integer.abs(value: 1)", 1)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Integer.abs(value: 999)", 999)
        .unwrap();

    // Negative numbers (should become positive)
    harness
        .assert_evaluates_to_integer("Integer.abs(value: -42)", 42)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Integer.abs(value: -1)", 1)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Integer.abs(value: -999)", 999)
        .unwrap();

    // Zero (should remain zero)
    harness
        .assert_evaluates_to_integer("Integer.abs(value: 0)", 0)
        .unwrap();

    // Large numbers
    harness
        .assert_evaluates_to_integer("Integer.abs(value: 1000000)", 1000000)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Integer.abs(value: -1000000)", 1000000)
        .unwrap();
}

#[test]
fn test_abs_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables
    harness.execute_let_binding("let pos_num = 42").unwrap();
    harness.execute_let_binding("let neg_num = -17").unwrap();
    harness.execute_let_binding("let zero_num = 0").unwrap();

    // Test with variables
    harness
        .assert_evaluates_to_integer("Integer.abs(value: pos_num)", 42)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Integer.abs(value: neg_num)", 17)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Integer.abs(value: zero_num)", 0)
        .unwrap();

    // Test with expressions
    harness
        .assert_evaluates_to_integer("Integer.abs(value: neg_num * 2)", 34)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Integer.abs(value: pos_num - 100)", 58)
        .unwrap();
}

#[test]
fn test_zero_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Zero should return true
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: 0)", true)
        .unwrap();

    // Positive numbers should return false
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: 1)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: 42)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: 999)", false)
        .unwrap();

    // Negative numbers should return false
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: -1)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: -42)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: -999)", false)
        .unwrap();

    // Test with expressions that result in zero
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: 5 - 5)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: 0 * 999)", true)
        .unwrap();

    // Test with expressions that don't result in zero
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: 5 + 3)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: 1 - 2)", false)
        .unwrap();
}

#[test]
fn test_positive_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Positive numbers should return true
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: 1)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: 42)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: 999)", true)
        .unwrap();

    // Zero should return false
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: 0)", false)
        .unwrap();

    // Negative numbers should return false
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: -1)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: -42)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: -999)", false)
        .unwrap();

    // Test with expressions that result in positive numbers
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: 5 + 3)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: 10 - 3)", true)
        .unwrap();

    // Test with expressions that result in negative numbers
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: 3 - 10)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: -5 * 2)", false)
        .unwrap();

    // Test with expressions that result in zero
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: 5 - 5)", false)
        .unwrap();
}

#[test]
fn test_negative_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Negative numbers should return true
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: -1)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: -42)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: -999)", true)
        .unwrap();

    // Zero should return false
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: 0)", false)
        .unwrap();

    // Positive numbers should return false
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: 1)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: 42)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: 999)", false)
        .unwrap();

    // Test with expressions that result in negative numbers
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: 3 - 10)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: -5 * 2)", true)
        .unwrap();

    // Test with expressions that result in positive numbers
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: 5 + 3)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: 10 - 3)", false)
        .unwrap();

    // Test with expressions that result in zero
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: 5 - 5)", false)
        .unwrap();
}

#[test]
fn test_predicate_functions_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables
    harness.execute_let_binding("let pos_num = 42").unwrap();
    harness.execute_let_binding("let neg_num = -17").unwrap();
    harness.execute_let_binding("let zero_num = 0").unwrap();

    // Test zero? with variables
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: zero_num)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: pos_num)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: neg_num)", false)
        .unwrap();

    // Test positive? with variables
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: pos_num)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: zero_num)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: neg_num)", false)
        .unwrap();

    // Test negative? with variables
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: neg_num)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: zero_num)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.negative?(value: pos_num)", false)
        .unwrap();
}

#[test]
fn test_combined_integer_trait_functions() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables for complex tests
    harness.execute_let_binding("let num = -42").unwrap();

    // Combine abs with other functions
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: Integer.abs(value: num))", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.zero?(value: Integer.abs(value: 0))", true)
        .unwrap();

    // Use abs result in to_string_radix
    harness
        .assert_evaluates_to_string(
            "Integer.to_string_radix(value: Integer.abs(value: num), radix: 16)",
            "2a",
        )
        .unwrap();

    // Complex expressions with multiple functions
    harness
        .execute_let_binding("let abs_result = Integer.abs(value: num)")
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Integer.positive?(value: abs_result)", true)
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Integer.to_string_radix(value: abs_result, radix: 2)",
            "101010",
        )
        .unwrap();
}

#[test]
fn test_type_consistency() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Integer.abs should return integers that can be used in arithmetic
    harness
        .execute_let_binding("let abs_result = Integer.abs(value: -42)")
        .unwrap();
    harness
        .assert_evaluates_to_integer("abs_result + 8", 50)
        .unwrap();

    // Predicate functions should return booleans that can be used in boolean contexts
    harness
        .execute_let_binding("let is_pos = Integer.positive?(value: 42)")
        .unwrap();
    harness
        .execute_let_binding("let is_neg = Integer.negative?(value: -17)")
        .unwrap();
    harness
        .execute_let_binding("let is_zero = Integer.zero?(value: 0)")
        .unwrap();

    // These should all be usable as boolean values
    harness.assert_evaluates_to_boolean("is_pos", true).unwrap();
    harness.assert_evaluates_to_boolean("is_neg", true).unwrap();
    harness
        .assert_evaluates_to_boolean("is_zero", true)
        .unwrap();

    // to_string_radix should return strings that can be used in string contexts
    harness
        .execute_let_binding("let hex_str = Integer.to_string_radix(value: 42, radix: 16)")
        .unwrap();
    harness.assert_evaluates_to_string("hex_str", "2a").unwrap();
}
