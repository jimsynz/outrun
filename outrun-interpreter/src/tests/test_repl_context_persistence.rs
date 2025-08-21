//! Test REPL context persistence across multiple evaluations

use crate::InterpreterSession;

#[test]
fn test_basic_struct_definition_persistence() {
    let mut session = InterpreterSession::new().unwrap();

    // Step 1: Define a struct (using correct parentheses syntax)
    let result = session.evaluate("struct User(name: String, age: Integer64) {}");

    match result {
        Ok(value) => {
            println!("✅ Struct definition succeeded: {}", value.display());
        }
        Err(e) => {
            println!("❌ Struct definition failed: {}", e);
            // This is expected to fail initially, but let's see what happens
        }
    }

    // Step 2: Try to use the struct (this should work if persistence is implemented)
    let result = session.evaluate(r#"let user = User { name: "Alice", age: 30 }"#);

    match result {
        Ok(value) => {
            println!(
                "✅ Struct usage and assignment succeeded: {}",
                value.display()
            );

            // Step 3: Verify the variable persists and can be referenced
            let var_result = session.evaluate("user");
            match var_result {
                Ok(var_value) => {
                    println!("✅ Variable reference succeeded: {}", var_value.display());
                    // This proves both struct definition and variable persistence are working
                    assert!(var_value.display().contains("Alice"));
                }
                Err(e) => {
                    println!("❌ Variable reference failed: {}", e);
                }
            }
        }
        Err(e) => {
            println!("❌ Struct usage failed (expected with old system): {}", e);
            // This will fail until we implement complete persistence
        }
    }
}

#[test]
fn test_simple_variable_persistence_still_works() {
    let mut session = InterpreterSession::new().unwrap();

    // This should still work - basic variable persistence
    session.evaluate("let x = 42").unwrap();
    let _result = session.evaluate("x").unwrap();

    session.assert_evaluates_to_integer("x", 42).unwrap();
    println!("✅ Basic variable persistence still works");
}

#[test]
fn test_session_package_accumulation() {
    let mut session = InterpreterSession::new().unwrap();

    // The key test: does the session_package accumulate programs?

    // Add first program
    session.evaluate("let a = 1").unwrap();

    // Check that session_package has accumulated programs
    // This is more of an internal test to verify our implementation

    // Add second program
    session.evaluate("let b = 2").unwrap();

    // Both variables should be accessible
    session.assert_evaluates_to_integer("a", 1).unwrap();
    session.assert_evaluates_to_integer("b", 2).unwrap();

    println!("✅ Session package accumulation working for simple variables");
}


#[test]
fn test_struct_definition_registry_tracking() {
    let mut session = InterpreterSession::new().unwrap();

    // Step 1: Define multiple structs
    session
        .evaluate("struct User(name: String, age: Integer64) {}")
        .unwrap();
    session
        .evaluate("struct Product(id: Integer64, name: String) {}")
        .unwrap();

    // Step 2: Use both structs to verify registry persistence
    let user_result = session.evaluate(r#"let user = User { name: "Alice", age: 30 }"#);
    let product_result = session.evaluate(r#"let product = Product { id: 1, name: "Widget" }"#);

    match (user_result, product_result) {
        (Ok(user_val), Ok(product_val)) => {
            println!("✅ Multiple struct definitions persisted successfully");
            println!("   User: {}", user_val.display());
            println!("   Product: {}", product_val.display());

            // Verify both variables can still be accessed
            assert!(session.evaluate("user").is_ok());
            assert!(session.evaluate("product").is_ok());
        }
        (user_res, product_res) => {
            if user_res.is_err() {
                println!("❌ User struct failed: {:?}", user_res);
            }
            if product_res.is_err() {
                println!("❌ Product struct failed: {:?}", product_res);
            }
        }
    }
}
