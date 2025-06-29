//! Acceptance tests for integer unary operations

use crate::test_harness::OutrunTestHarness;

#[test]
fn test_let_binding_simple() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // This should work since it doesn't use unary operators
    harness.execute_let_binding("let x = 42").unwrap();
    harness.assert_evaluates_to_integer("x", 42).unwrap();

    // Test binary addition which should work
    harness.execute_let_binding("let y = 10 + 5").unwrap();
    harness.assert_evaluates_to_integer("y", 15).unwrap();
}

#[test]
fn test_debug_simple_unary() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test simple unary that should work
    println!("Testing -42...");
    harness.assert_evaluates_to_integer("-42", -42).unwrap();
    println!("Success!");

    // Test the failing case
    println!("Testing --42 as expression...");
    let result = harness.evaluate("--42");
    match result {
        Ok(value) => println!("--42 as expression works: {:?}", value),
        Err(e) => println!("--42 as expression fails: {}", e),
    }

    // Test the failing case as let binding
    println!("Testing --42 as let binding...");
    let result = harness.execute_let_binding("let result3 = --42");
    match result {
        Ok(_) => println!("--42 as let binding works!"),
        Err(e) => println!("--42 as let binding fails: {}", e),
    }

    // Test if simple let binding with unary minus works (no double minus)
    println!("Testing -42 as let binding...");
    let result = harness.execute_let_binding("let result_simple = -42");
    match result {
        Ok(_) => println!("-42 as let binding works!"),
        Err(e) => println!("-42 as let binding fails: {}", e),
    }
}

#[test]
fn test_unary_operations_type_consistency() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Verify that unary operations always return integers when applied to integers
    harness.execute_let_binding("let result1 = +42").unwrap();
    harness.execute_let_binding("let result2 = -42").unwrap();
    harness.execute_let_binding("let result3 = --42").unwrap();

    // These should all be usable in integer contexts
    harness
        .assert_evaluates_to_integer("result1 + 1", 43)
        .unwrap();
    harness
        .assert_evaluates_to_integer("result2 + 1", -41)
        .unwrap();
    harness
        .assert_evaluates_to_integer("result3 + 1", 43)
        .unwrap();
}
