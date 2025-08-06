//! Acceptance tests for expression evaluation in the new interpreter
//!
//! These tests systematically verify which expression types are implemented
//! and working correctly with the parser → interpreter pipeline.

use crate::InterpreterSession;

#[test]
fn test_supported_literal_expressions() {
    let mut harness = InterpreterSession::new().unwrap();

    // Integer literals (working)
    harness.assert_evaluates_to_integer("42", 42).unwrap();
    harness.assert_evaluates_to_integer("0", 0).unwrap();
    harness.assert_evaluates_to_integer("1", 1).unwrap();

    // Boolean literals (working)
    harness.assert_evaluates_to_boolean("true", true).unwrap();
    harness.assert_evaluates_to_boolean("false", false).unwrap();

    // String literals (working)
    harness
        .assert_evaluates_to_string("\"hello\"", "hello")
        .unwrap();
    harness.assert_evaluates_to_string("\"\"", "").unwrap();

    // Atom literals (working)
    harness.assert_evaluates_to_atom(":test", "test").unwrap();
    harness.assert_evaluates_to_atom(":ok", "ok").unwrap();
}

#[test]
fn test_variable_expressions() {
    let mut harness = InterpreterSession::new().unwrap();

    // Set up variables
    harness.evaluate("let x = 42").unwrap();
    harness.evaluate("let message = \"hello\"").unwrap();
    harness.evaluate("let flag = true").unwrap();

    // Variable access (working)
    harness.assert_evaluates_to_integer("x", 42).unwrap();
    harness
        .assert_evaluates_to_string("message", "hello")
        .unwrap();
    harness.assert_evaluates_to_boolean("flag", true).unwrap();
}

#[test]
fn test_let_binding_expressions() {
    let mut harness = InterpreterSession::new().unwrap();

    // Simple let bindings (working)
    harness.evaluate("let x = 42").unwrap();
    harness.assert_evaluates_to_integer("x", 42).unwrap();

    harness.evaluate("let name = \"Alice\"").unwrap();
    harness.assert_evaluates_to_string("name", "Alice").unwrap();

    harness.evaluate("let enabled = true").unwrap();
    harness
        .assert_evaluates_to_boolean("enabled", true)
        .unwrap();

    harness.evaluate("let status = :ok").unwrap();
    harness.assert_evaluates_to_atom("status", "ok").unwrap();
}

#[test]
fn test_case_expression_basic() {
    let mut harness = InterpreterSession::new().unwrap();

    // Simple case expressions with literal patterns (working)
    let code = r#"
        case 42 {
            42 -> "found"
            x -> "not found"
        }
    "#;
    harness.assert_evaluates_to_string(code, "found").unwrap();

    let code = r#"
        case true {
            true -> "yes"
            false -> "no"
        }
    "#;
    harness.assert_evaluates_to_string(code, "yes").unwrap();

    let code = r#"
        case "hello" {
            "hello" -> "greeting"
            s -> "other"
        }
    "#;
    harness
        .assert_evaluates_to_string(code, "greeting")
        .unwrap();

    let code = r#"
        case :success {
            :success -> "ok"
            atom -> "not ok"
        }
    "#;
    harness.assert_evaluates_to_string(code, "ok").unwrap();
}

// Removed outdated "unsupported" tests that were testing for lack of functionality
// that the interpreter now supports. These tests no longer provide value.

#[test]
fn test_current_interpreter_capabilities_summary() {
    let mut harness = InterpreterSession::new().unwrap();

    // This test documents what's currently working
    println!("=== CURRENT INTERPRETER CAPABILITIES ===");

    // Basic literals work
    harness.assert_evaluates_to_integer("42", 42).unwrap();
    harness.assert_evaluates_to_boolean("true", true).unwrap();
    harness
        .assert_evaluates_to_string("\"test\"", "test")
        .unwrap();
    harness.assert_evaluates_to_atom(":ok", "ok").unwrap();
    println!("✅ Basic literals: integers, booleans, strings, atoms");

    // Variable binding and access works
    harness.evaluate("let x = 123").unwrap();
    harness.assert_evaluates_to_integer("x", 123).unwrap();
    println!("✅ Let bindings with identifier patterns");
    println!("✅ Variable access from context");

    // Basic case expressions work
    let code = r#"
        case 42 {
            42 -> "matched"
            x -> "default"
        }
    "#;
    harness.assert_evaluates_to_string(code, "matched").unwrap();
    println!("✅ Case expressions with literal and identifier patterns");

    // Pattern matching system works
    println!("✅ Pattern matching engine with multiple pattern types");
    println!("✅ Test harness with comprehensive assertion methods");

    println!("=== MISSING FEATURES (Phase 3) ===");
    println!("❌ Negative number literals");
    println!("❌ Tuple literals (1, 2)");
    println!("❌ List literals [1, 2, 3]");
    println!("❌ Function calls List.head(...)");
    println!("❌ Arithmetic expressions 1 + 2");
    println!("❌ If expressions");
    println!("❌ Complex patterns in let bindings");
    println!("❌ Variable shadowing (context limitation)");
}
