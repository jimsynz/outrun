//! Acceptance tests for Atom trait functions and behaviors

use crate::test_harness::OutrunTestHarness;

#[test]
fn test_atom_equality_simple_atoms() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Equal simple atoms
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :hello, rhs: :hello)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :test, rhs: :test)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :a, rhs: :a)", true)
        .unwrap();

    // Different simple atoms
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :hello, rhs: :world)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :foo, rhs: :bar)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :test, rhs: :testing)", false)
        .unwrap();

    // Equal atoms with underscores and numbers
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :user_id, rhs: :user_id)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :test123, rhs: :test123)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :_private, rhs: :_private)", true)
        .unwrap();
}

#[test]
fn test_atom_equality_quoted_atoms() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Equal quoted atoms
    harness
        .assert_evaluates_to_boolean(
            "Equality.equal?(lhs: :\"hello world\", rhs: :\"hello world\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Equality.equal?(lhs: :\"test with spaces\", rhs: :\"test with spaces\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"\", rhs: :\"\")", true)
        .unwrap();

    // Different quoted atoms
    harness
        .assert_evaluates_to_boolean(
            "Equality.equal?(lhs: :\"hello world\", rhs: :\"goodbye world\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Equality.equal?(lhs: :\"hello\", rhs: :\"hello world\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"\", rhs: :\"test\")", false)
        .unwrap();

    // Quoted atoms with special characters
    harness
        .assert_evaluates_to_boolean(
            "Equality.equal?(lhs: :\"hello!@#$%\", rhs: :\"hello!@#$%\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Equality.equal?(lhs: :\"with\\nnewline\", rhs: :\"with\\nnewline\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Equality.equal?(lhs: :\"say \\\"hello\\\"\", rhs: :\"say \\\"hello\\\"\")",
            true,
        )
        .unwrap();
}

#[test]
fn test_atom_equality_mixed_formats() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Simple vs quoted with same content should be equal (atom content matters, not format)
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :hello, rhs: :\"hello\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :test, rhs: :\"test\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :a, rhs: :\"a\")", true)
        .unwrap();

    // Different content regardless of format should be different
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :hello, rhs: :\"world\")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"foo\", rhs: :bar)", false)
        .unwrap();

    // Quoted atoms that couldn't be simple format
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"hello world\", rhs: :hello)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Equality.equal?(lhs: :simple, rhs: :\"hello world\")",
            false,
        )
        .unwrap();
}

#[test]
fn test_atom_not_equal() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Not equal cases
    harness
        .assert_evaluates_to_boolean("Equality.not_equal?(lhs: :hello, rhs: :world)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.not_equal?(lhs: :foo, rhs: :bar)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Equality.not_equal?(lhs: :\"hello world\", rhs: :\"goodbye world\")",
            true,
        )
        .unwrap();

    // Equal cases (should return false for not_equal)
    harness
        .assert_evaluates_to_boolean("Equality.not_equal?(lhs: :hello, rhs: :hello)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Equality.not_equal?(lhs: :\"hello world\", rhs: :\"hello world\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.not_equal?(lhs: :test, rhs: :\"test\")", false)
        .unwrap();
}

#[test]
fn test_atom_equality_operators() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Using == operator (should delegate to Equality.equal?)
    harness
        .assert_evaluates_to_boolean(":hello == :hello", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(":hello == :world", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(":\"hello world\" == :\"hello world\"", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(":hello == :\"hello\"", true)
        .unwrap();

    // Using != operator (should delegate to Equality.not_equal?)
    harness
        .assert_evaluates_to_boolean(":hello != :world", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(":hello != :hello", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(":\"hello world\" != :\"goodbye world\"", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(":hello != :\"hello\"", false)
        .unwrap();
}

#[test]
fn test_atom_display_to_string() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Simple atoms should display without quotes
    harness
        .assert_evaluates_to_string("Display.to_string(value: :hello)", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :test)", "test")
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :user_id)", "user_id")
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :test123)", "test123")
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :_private)", "_private")
        .unwrap();

    // Quoted atoms should display their content without the quotes
    harness
        .assert_evaluates_to_string("Display.to_string(value: :\"hello world\")", "hello world")
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Display.to_string(value: :\"test with spaces\")",
            "test with spaces",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :\"\")", "")
        .unwrap();

    // Quoted atoms with special characters
    harness
        .assert_evaluates_to_string("Display.to_string(value: :\"hello!@#$%\")", "hello!@#$%")
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Display.to_string(value: :\"with\\nnewline\")",
            "with\nnewline",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Display.to_string(value: :\"say \\\"hello\\\"\")",
            "say \"hello\"",
        )
        .unwrap();
}

#[test]
fn test_atom_inspect_representation() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Simple atoms should inspect as atom literals (with colon)
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :hello)", ":hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :test)", ":test")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :user_id)", ":user_id")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :test123)", ":test123")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :_private)", ":_private")
        .unwrap();

    // Quoted atoms should inspect with quotes when needed
    harness
        .assert_evaluates_to_string(
            "Inspect.inspect(value: :\"hello world\")",
            ":\"hello world\"",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Inspect.inspect(value: :\"test with spaces\")",
            ":\"test with spaces\"",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :\"\")", ":\"\"")
        .unwrap();

    // Special characters should be properly escaped in inspect
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :\"hello!@#$%\")", ":\"hello!@#$%\"")
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Inspect.inspect(value: :\"with\\nnewline\")",
            ":\"with\\nnewline\"",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Inspect.inspect(value: :\"say \\\"hello\\\"\")",
            ":\"say \\\"hello\\\"\"",
        )
        .unwrap();
}

#[test]
fn test_atom_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up atom variables
    harness.execute_let_binding("let status = :active").unwrap();
    harness
        .execute_let_binding("let role = :\"admin user\"")
        .unwrap();
    harness
        .execute_let_binding("let empty_atom = :\"\"")
        .unwrap();

    // Test equality with variables
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: status, rhs: :active)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: status, rhs: :inactive)", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("status == :active", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("status != :inactive", true)
        .unwrap();

    // Test display with variables
    harness
        .assert_evaluates_to_string("Display.to_string(value: status)", "active")
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: role)", "admin user")
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: empty_atom)", "")
        .unwrap();

    // Test inspect with variables
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: status)", ":active")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: role)", ":\"admin user\"")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: empty_atom)", ":\"\"")
        .unwrap();
}

#[test]
fn test_atom_edge_cases() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Empty quoted atom
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"\", rhs: :\"\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :\"\")", "")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :\"\")", ":\"\"")
        .unwrap();

    // Single character atoms
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :a, rhs: :a)", true)
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :a)", "a")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :a)", ":a")
        .unwrap();

    // Atoms with only underscores
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :_, rhs: :_)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :__, rhs: :__)", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :_, rhs: :__)", false)
        .unwrap();

    // Atoms with only numbers (quoted format required)
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"123\", rhs: :\"123\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"123\", rhs: :\"456\")", false)
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :\"123\")", "123")
        .unwrap();
}

#[test]
fn test_atom_unicode_support() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Unicode atoms (quoted format required)
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"café\", rhs: :\"café\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"🦀\", rhs: :\"🦀\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: :\"café\", rhs: :\"cafe\")", false)
        .unwrap();

    // Unicode display
    harness
        .assert_evaluates_to_string("Display.to_string(value: :\"café\")", "café")
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :\"🦀\")", "🦀")
        .unwrap();
    harness
        .assert_evaluates_to_string("Display.to_string(value: :\"こんにちは\")", "こんにちは")
        .unwrap();

    // Unicode inspect
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :\"café\")", ":\"café\"")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :\"🦀\")", ":\"🦀\"")
        .unwrap();
    harness
        .assert_evaluates_to_string("Inspect.inspect(value: :\"こんにちは\")", ":\"こんにちは\"")
        .unwrap();
}

#[test]
fn test_atom_complex_expressions() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Atoms in complex boolean expressions
    harness
        .assert_evaluates_to_boolean("(:status == :active) && (:role == :admin)", false)
        .unwrap();
    harness.execute_let_binding("let status = :active").unwrap();
    harness.execute_let_binding("let role = :admin").unwrap();
    harness
        .assert_evaluates_to_boolean("(status == :active) && (role == :admin)", true)
        .unwrap();

    // Atoms in conditional expressions
    harness
        .assert_evaluates_to_string(
            "if status == :active { \"User is active\" } else { \"User is inactive\" }",
            "User is active",
        )
        .unwrap();

    // Chain atom operations
    harness
        .assert_evaluates_to_string(
            "String.to_upper(value: Display.to_string(value: :hello))",
            "HELLO",
        )
        .unwrap();

    // Atom in function calls as arguments
    harness
        .execute_let_binding("let atom_list = [:first, :second, :third]")
        .unwrap();
}

#[test]
fn test_atom_type_consistency() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test that all atom operations return expected types
    let result1 = harness
        .evaluate("Equality.equal?(lhs: :hello, rhs: :hello)")
        .unwrap();
    let result2 = harness
        .evaluate("Equality.not_equal?(lhs: :hello, rhs: :world)")
        .unwrap();
    let result3 = harness
        .evaluate("Display.to_string(value: :hello)")
        .unwrap();
    let result4 = harness.evaluate("Inspect.inspect(value: :hello)").unwrap();

    assert!(matches!(result1, crate::Value::Boolean(true)));
    assert!(matches!(result2, crate::Value::Boolean(true)));
    assert!(matches!(result3, crate::Value::String(_)));
    assert!(matches!(result4, crate::Value::String(_)));

    // Test atom creation and usage
    let atom_result = harness.evaluate(":test_atom").unwrap();
    assert!(matches!(atom_result, crate::Value::Atom(_)));

    // Test atoms can be used in all contexts where values are expected
    harness
        .execute_let_binding("let my_atom = :dynamic_atom")
        .unwrap();
    let variable_result = harness.evaluate("my_atom").unwrap();
    assert!(matches!(variable_result, crate::Value::Atom(_)));
}
