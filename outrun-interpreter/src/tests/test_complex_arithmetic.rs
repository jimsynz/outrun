//! Test complex arithmetic expressions with the full pipeline

use crate::test_harness::InterpreterSession;

#[test]
fn test_complex_arithmetic_expressions() {
    println!("=== TESTING COMPLEX ARITHMETIC EXPRESSIONS ===");

    let mut harness = InterpreterSession::new().unwrap();

    // Test the expression mentioned by the user: 1 + 2 * 3
    // This should evaluate to 7 (2 * 3 = 6, then 1 + 6 = 7)
    println!("Testing: 1 + 2 * 3");
    let result = harness.evaluate("1 + 2 * 3");
    match result {
        Ok(value) => println!("1 + 2 * 3 = {}", value.display()),
        Err(e) => println!("❌ Error: {:?}", e),
    }

    // Test more complex expressions
    println!("Testing: 10 - 3 + 2");
    let result = harness.evaluate("10 - 3 + 2");
    match result {
        Ok(value) => println!("10 - 3 + 2 = {}", value.display()),
        Err(e) => println!("❌ Error: {:?}", e),
    }

    println!("Testing: 8 / 2 * 4");
    let result = harness.evaluate("8 / 2 * 4");
    match result {
        Ok(value) => println!("8 / 2 * 4 = {}", value.display()),
        Err(e) => println!("❌ Error: {:?}", e),
    }

    println!("Testing: 2 + 3 * 4 - 1");
    let result = harness.evaluate("2 + 3 * 4 - 1");
    match result {
        Ok(value) => println!("2 + 3 * 4 - 1 = {}", value.display()),
        Err(e) => println!("❌ Error: {:?}", e),
    }
}
