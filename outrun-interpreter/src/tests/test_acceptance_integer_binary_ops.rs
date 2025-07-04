//! Acceptance tests for integer binary operations

use crate::test_harness::OutrunTestHarness;

#[test]
fn test_arithmetic_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic arithmetic operations
    harness.assert_evaluates_to_integer("5 + 3", 8).unwrap();
    harness.assert_evaluates_to_integer("10 - 4", 6).unwrap();
    harness.assert_evaluates_to_integer("6 * 7", 42).unwrap();
    harness.assert_evaluates_to_some_integer("20 / 4", 5).unwrap();
    harness.assert_evaluates_to_some_integer("17 % 5", 2).unwrap();

    // Operations with negative numbers
    harness.assert_evaluates_to_integer("-5 + 3", -2).unwrap();
    harness.assert_evaluates_to_integer("5 + -3", 2).unwrap();
    harness.assert_evaluates_to_integer("-5 - 3", -8).unwrap();
    harness.assert_evaluates_to_integer("5 - -3", 8).unwrap();
    harness.assert_evaluates_to_integer("-5 * 3", -15).unwrap();
    harness.assert_evaluates_to_integer("-5 * -3", 15).unwrap();
    harness.assert_evaluates_to_some_integer("-20 / 4", -5).unwrap();
    harness.assert_evaluates_to_some_integer("20 / -4", -5).unwrap();
    harness.assert_evaluates_to_some_integer("-20 / -4", 5).unwrap();
    harness.assert_evaluates_to_some_integer("-17 % 5", -2).unwrap();

    // Zero operations
    harness.assert_evaluates_to_integer("0 + 5", 5).unwrap();
    harness.assert_evaluates_to_integer("5 + 0", 5).unwrap();
    harness.assert_evaluates_to_integer("0 - 5", -5).unwrap();
    harness.assert_evaluates_to_integer("5 - 0", 5).unwrap();
    harness.assert_evaluates_to_integer("0 * 5", 0).unwrap();
    harness.assert_evaluates_to_integer("5 * 0", 0).unwrap();
    harness.assert_evaluates_to_some_integer("0 / 5", 0).unwrap();
    harness.assert_evaluates_to_some_integer("0 % 5", 0).unwrap();
}

#[test]
fn test_simple_operator_precedence() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Multiplication and division before addition and subtraction
    harness
        .assert_evaluates_to_integer("2 + 3 * 4", 14)
        .unwrap();
    harness
        .assert_evaluates_to_integer("3 * 4 + 2", 14)
        .unwrap();
    harness
        .assert_evaluates_to_integer("10 - 2 * 3", 4)
        .unwrap();
    harness
        .assert_evaluates_to_integer("2 * 3 - 10", -4)
        .unwrap();
}

#[test]
fn test_simple_parentheses() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Parentheses override default precedence
    harness
        .assert_evaluates_to_integer("(2 + 3) * 4", 20)
        .unwrap();
    harness
        .assert_evaluates_to_integer("2 * (3 + 4)", 14)
        .unwrap();
    harness
        .assert_evaluates_to_integer("(10 - 2) * 3", 24)
        .unwrap();
    harness
        .assert_evaluates_to_integer("10 - (2 * 3)", 4)
        .unwrap();
}

#[test]
fn test_comparison_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Equality comparisons
    harness.assert_evaluates_to_boolean("5 == 5", true).unwrap();
    harness
        .assert_evaluates_to_boolean("5 == 3", false)
        .unwrap();
    harness.assert_evaluates_to_boolean("5 != 3", true).unwrap();
    harness
        .assert_evaluates_to_boolean("5 != 5", false)
        .unwrap();

    // Ordering comparisons
    harness.assert_evaluates_to_boolean("5 < 10", true).unwrap();
    harness
        .assert_evaluates_to_boolean("10 < 5", false)
        .unwrap();
    harness.assert_evaluates_to_boolean("5 < 5", false).unwrap();
    harness
        .assert_evaluates_to_boolean("5 <= 10", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("10 <= 5", false)
        .unwrap();
    harness.assert_evaluates_to_boolean("5 <= 5", true).unwrap();
    harness.assert_evaluates_to_boolean("10 > 5", true).unwrap();
    harness
        .assert_evaluates_to_boolean("5 > 10", false)
        .unwrap();
    harness.assert_evaluates_to_boolean("5 > 5", false).unwrap();
    harness
        .assert_evaluates_to_boolean("10 >= 5", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("5 >= 10", false)
        .unwrap();
    harness.assert_evaluates_to_boolean("5 >= 5", true).unwrap();

    // Comparisons with negative numbers
    harness.assert_evaluates_to_boolean("-5 < 0", true).unwrap();
    harness.assert_evaluates_to_boolean("0 > -5", true).unwrap();
    harness
        .assert_evaluates_to_boolean("-10 < -5", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("-5 > -10", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("-5 == -5", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("-5 != -3", true)
        .unwrap();
}

#[test]
fn test_operations_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables
    harness.execute_let_binding("let x = 10").unwrap();
    harness.execute_let_binding("let y = 3").unwrap();
    harness.execute_let_binding("let z = -5").unwrap();

    // Arithmetic with variables
    harness.assert_evaluates_to_integer("x + y", 13).unwrap();
    harness.assert_evaluates_to_integer("x - y", 7).unwrap();
    harness.assert_evaluates_to_integer("x * y", 30).unwrap();
    harness.assert_evaluates_to_some_integer("x / y", 3).unwrap();
    harness.assert_evaluates_to_some_integer("x % y", 1).unwrap();

    // Operations with negative variables
    harness.assert_evaluates_to_integer("x + z", 5).unwrap();
    harness.assert_evaluates_to_integer("x - z", 15).unwrap();
    harness.assert_evaluates_to_integer("y * z", -15).unwrap();
    harness.assert_evaluates_to_some_integer("x / z", -2).unwrap();
    harness.assert_evaluates_to_some_integer("x % z", 0).unwrap();

    // Comparisons with variables
    harness.assert_evaluates_to_boolean("x > y", true).unwrap();
    harness.assert_evaluates_to_boolean("y < x", true).unwrap();
    harness.assert_evaluates_to_boolean("x == x", true).unwrap();
    harness.assert_evaluates_to_boolean("y != z", true).unwrap();
    harness.assert_evaluates_to_boolean("z < 0", true).unwrap();
}

#[test]
fn test_chained_operations_simple() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Left-to-right evaluation for same precedence
    harness
        .assert_evaluates_to_integer("10 - 3 - 2", 5)
        .unwrap();
    harness
        .assert_evaluates_to_integer("2 * 3 * 4", 24)
        .unwrap();
    harness
        .assert_evaluates_to_integer("15 + 5 + 10", 30)
        .unwrap();
}

#[test]
fn test_edge_cases() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Large numbers
    harness
        .assert_evaluates_to_integer("1000000 + 2000000", 3000000)
        .unwrap();
    harness
        .assert_evaluates_to_integer("1000000 * 3", 3000000)
        .unwrap();
    harness
        .assert_evaluates_to_some_integer("1000000 / 1000", 1000)
        .unwrap();

    // Operations resulting in zero
    harness.assert_evaluates_to_integer("5 - 5", 0).unwrap();
    harness.assert_evaluates_to_integer("0 * 999", 0).unwrap();
    harness.assert_evaluates_to_some_integer("0 / 999", 0).unwrap();
    harness.assert_evaluates_to_some_integer("5 % 5", 0).unwrap();

    // Comparisons at boundaries
    harness.assert_evaluates_to_boolean("0 == 0", true).unwrap();
    harness.assert_evaluates_to_boolean("0 > -1", true).unwrap();
    harness.assert_evaluates_to_boolean("0 < 1", true).unwrap();
    harness
        .assert_evaluates_to_boolean("-1 <= 0", true)
        .unwrap();
    harness.assert_evaluates_to_boolean("1 >= 0", true).unwrap();
}

#[test]
fn test_type_consistency() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // All arithmetic operations return integers when applied to integers
    harness
        .execute_let_binding("let add_result = 5 + 3")
        .unwrap();
    harness
        .execute_let_binding("let sub_result = 5 - 3")
        .unwrap();
    harness
        .execute_let_binding("let mul_result = 5 * 3")
        .unwrap();
    harness
        .execute_let_binding("let pow_result = 2 ** 3")
        .unwrap();
    harness
        .execute_let_binding("let neg_result = -5")
        .unwrap();

    // These should all be usable in integer contexts
    harness
        .assert_evaluates_to_integer("add_result + 1", 9)
        .unwrap();
    harness
        .assert_evaluates_to_integer("sub_result + 1", 3)
        .unwrap();
    harness
        .assert_evaluates_to_integer("mul_result + 1", 16)
        .unwrap();
    harness
        .assert_evaluates_to_integer("pow_result + 1", 9)
        .unwrap();
    harness
        .assert_evaluates_to_integer("neg_result + 1", -4)
        .unwrap();

    // All comparison operations return booleans
    harness
        .execute_let_binding("let eq_result = 5 == 3")
        .unwrap();
    harness
        .execute_let_binding("let ne_result = 5 != 3")
        .unwrap();
    harness
        .execute_let_binding("let lt_result = 5 < 3")
        .unwrap();
    harness
        .execute_let_binding("let le_result = 5 <= 3")
        .unwrap();
    harness
        .execute_let_binding("let gt_result = 5 > 3")
        .unwrap();
    harness
        .execute_let_binding("let ge_result = 5 >= 3")
        .unwrap();

    // These should all be usable in boolean contexts
    harness
        .assert_evaluates_to_boolean("eq_result", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("ne_result", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("lt_result", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("le_result", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("gt_result", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("ge_result", true)
        .unwrap();
}
