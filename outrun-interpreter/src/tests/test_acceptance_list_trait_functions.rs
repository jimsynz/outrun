//! Acceptance tests for List trait functions and behaviors

use crate::test_harness::OutrunTestHarness;

#[test]
fn test_list_head() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Valid head operations
    harness
        .assert_evaluates_to_integer("Option.unwrap(value: List.head(value: [1, 2, 3]))", 1)
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Option.unwrap(value: List.head(value: [\"hello\", \"world\"]))",
            "hello",
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer("Option.unwrap(value: List.head(value: [42]))", 42)
        .unwrap();

    // Head of empty list should return None
    harness
        .assert_evaluates_to_boolean("Option.none?(value: List.head(value: []))", true)
        .unwrap();

    // Mixed content lists (same type)
    harness
        .assert_evaluates_to_integer("Option.unwrap(value: List.head(value: [99, 1, 2, 3]))", 99)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Option.unwrap(value: List.head(value: [true, false, true]))",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Display.to_string(value: Option.unwrap(value: List.head(value: [:atom1, :atom2])))",
            "atom1",
        )
        .unwrap();
}

#[test]
fn test_list_tail() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Valid tail operations
    harness
        .assert_evaluates_to("[2, 3]", "List.tail(value: [1, 2, 3])")
        .unwrap();
    harness
        .assert_evaluates_to("[\"world\"]", "List.tail(value: [\"hello\", \"world\"])")
        .unwrap();
    harness
        .assert_evaluates_to("[]", "List.tail(value: [42])")
        .unwrap();

    // Tail of empty list should return empty list
    harness
        .assert_evaluates_to("[]", "List.tail(value: [])")
        .unwrap();

    // Chained tail operations
    harness
        .assert_evaluates_to("[3]", "List.tail(value: List.tail(value: [1, 2, 3]))")
        .unwrap();
    harness
        .assert_evaluates_to(
            "[]",
            "List.tail(value: List.tail(value: List.tail(value: [1, 2, 3])))",
        )
        .unwrap();

    // Tail operations preserve type
    harness
        .assert_evaluates_to_integer(
            "Option.unwrap(value: List.head(value: List.tail(value: [1, 2, 3])))",
            2,
        )
        .unwrap();
}

#[test]
fn test_list_prepend() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Prepend to existing lists
    harness
        .assert_evaluates_to("[1, 2, 3]", "List.prepend(list: [2, 3], elem: 1)")
        .unwrap();
    harness
        .assert_evaluates_to(
            "[\"hello\", \"world\"]",
            "List.prepend(list: [\"world\"], elem: \"hello\")",
        )
        .unwrap();

    // Prepend to empty list
    harness
        .assert_evaluates_to("[42]", "List.prepend(list: [], elem: 42)")
        .unwrap();
    harness
        .assert_evaluates_to("[true]", "List.prepend(list: [], elem: true)")
        .unwrap();

    // Multiple prepends (cons operations)
    harness
        .assert_evaluates_to(
            "[1, 2, 3]",
            "List.prepend(list: List.prepend(list: List.prepend(list: [], elem: 3), elem: 2), elem: 1),"
        )
        .unwrap();

    // Prepend preserves original list immutability
    harness
        .execute_let_binding("let original = [2, 3]")
        .unwrap();
    harness
        .execute_let_binding("let new_list = List.prepend(list: original, elem: 1)")
        .unwrap();
    harness.assert_evaluates_to("[2, 3]", "original").unwrap();
    harness
        .assert_evaluates_to("[1, 2, 3]", "new_list")
        .unwrap();
}

#[test]
fn test_list_length() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic length operations
    harness
        .assert_evaluates_to_integer("List.length(value: [])", 0)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: [1])", 1)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: [1, 2, 3])", 3)
        .unwrap();

    // Length with different types
    harness
        .assert_evaluates_to_integer("List.length(value: [\"hello\", \"world\", \"test\"])", 3)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: [true, false])", 2)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: [:atom1, :atom2, :atom3, :atom4])", 4)
        .unwrap();

    // Length after operations
    harness
        .assert_evaluates_to_integer("List.prepend(list: [1, 2], elem: 0) |> List.length()", 3)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.tail(value: [1, 2, 3, 4]) |> List.length()", 3)
        .unwrap();

    // Large list length
    harness
        .execute_let_binding("let large_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: large_list)", 10)
        .unwrap();
}

#[test]
fn test_list_empty_predicate() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Empty list should return true
    harness
        .assert_evaluates_to_boolean("List.empty?(value: [])", true)
        .unwrap();

    // Non-empty lists should return false
    harness
        .assert_evaluates_to_boolean("List.empty?(value: [1])", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("List.empty?(value: [1, 2, 3])", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("List.empty?(value: [\"hello\"])", false)
        .unwrap();

    // Empty predicate after operations
    harness
        .assert_evaluates_to_boolean("List.tail(value: [1]) |> List.empty?()", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("List.tail(value: [1, 2]) |> List.empty?()", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("List.prepend(list: [], elem: 42) |> List.empty?()", false)
        .unwrap();

    // Default list should be empty
    harness
        .assert_evaluates_to_boolean("Default.default() as List<Integer> |> List.empty?()", true)
        .unwrap();
}

#[test]
fn test_list_equality() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Equal lists
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: [], rhs: [])", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: [1, 2, 3], rhs: [1, 2, 3])", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: [\"hello\"], rhs: [\"hello\"])", true)
        .unwrap();

    // Different lists
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: [1, 2, 3], rhs: [1, 2, 4])", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: [1, 2], rhs: [1, 2, 3])", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: [], rhs: [1])", false)
        .unwrap();

    // Different order should not be equal
    harness
        .assert_evaluates_to_boolean("Equality.equal?(lhs: [1, 2, 3], rhs: [3, 2, 1])", false)
        .unwrap();

    // Using == and != operators
    harness
        .assert_evaluates_to_boolean("[1, 2, 3] == [1, 2, 3]", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("[1, 2, 3] != [1, 2, 4]", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("[] == []", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("[\"hello\"] != [\"world\"]", true)
        .unwrap();
}

#[test]
fn test_list_pattern_matching() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Pattern matching with empty list
    harness
        .assert_evaluates_to_string(
            "case [] { [] => \"empty\", [head, ..tail] => \"non-empty\" }",
            "empty",
        )
        .unwrap();

    // Pattern matching with single element
    harness
        .assert_evaluates_to_string(
            "case [42] { [] => \"empty\", [head, ..tail] => \"head is #{head}\" }",
            "head is 42",
        )
        .unwrap();

    // Pattern matching with multiple elements
    harness
        .assert_evaluates_to_string(
            "case [1, 2, 3] { [] => \"empty\", [head, ..tail] => \"head: #{head}, tail length: #{List.length(value: tail)}\" }",
            "head: 1, tail length: 2"
        )
        .unwrap();

    // More specific pattern matching
    harness
        .assert_evaluates_to_string(
            "case [1, 2, 3] { [] => \"empty\", [x] => \"single: #{x}\", [x, y] => \"pair: #{x}, #{y}\", [x, y, z] => \"triple: #{x}, #{y}, #{z}\", _ => \"many\" }",
            "triple: 1, 2, 3"
        )
        .unwrap();

    // Pattern matching with specific values
    harness
        .assert_evaluates_to_string(
            "case [1, 2, 3] { [1, ..rest] => \"starts with 1\", [2, ..rest] => \"starts with 2\", _ => \"other\" }",
            "starts with 1"
        )
        .unwrap();
}

#[test]
fn test_list_nested_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Head of tail
    harness
        .assert_evaluates_to_integer(
            "List.tail(value: [1, 2, 3]) |> List.head() |> Option.unwrap()",
            2,
        )
        .unwrap();

    // Tail of tail
    harness
        .assert_evaluates_to("[3]", "List.tail(value: [1, 2, 3]) |> List.tail()")
        .unwrap();

    // Prepend to tail
    harness
        .assert_evaluates_to(
            "[0, 2, 3]",
            "List.tail(value: [1, 2, 3]) |> List.prepend(elem: 0)",
        )
        .unwrap();

    // Complex chaining
    harness
        .assert_evaluates_to("[5, 2, 3]", 
            "List.tail(value: [1, 2, 3]) |> List.prepend(elem: 0) |> List.tail() |> List.prepend(elem: 5)")
        .unwrap();

    // Length of results
    harness
        .assert_evaluates_to_integer(
            "List.prepend(list: [1, 2], elem: 0) |> List.tail() |> List.length()",
            2,
        )
        .unwrap();
}

#[test]
fn test_list_with_different_types() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Lists of integers
    harness
        .execute_let_binding("let int_list = [1, 2, 3, 4, 5]")
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.head(value: int_list) |> Option.unwrap()", 1)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: int_list)", 5)
        .unwrap();

    // Lists of strings
    harness
        .execute_let_binding("let string_list = [\"hello\", \"world\", \"test\"]")
        .unwrap();
    harness
        .assert_evaluates_to_string("List.head(value: string_list) |> Option.unwrap()", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: string_list)", 3)
        .unwrap();

    // Lists of booleans
    harness
        .execute_let_binding("let bool_list = [true, false, true, true]")
        .unwrap();
    harness
        .assert_evaluates_to_boolean("List.head(value: bool_list) |> Option.unwrap()", true)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: bool_list)", 4)
        .unwrap();

    // Lists of atoms
    harness
        .execute_let_binding("let atom_list = [:active, :inactive, :pending]")
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "List.head(value: atom_list) |> Option.unwrap() |> Display.to_string()",
            "active",
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: atom_list)", 3)
        .unwrap();
}

#[test]
fn test_list_immutability() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Original list should remain unchanged after operations
    harness
        .execute_let_binding("let original = [1, 2, 3]")
        .unwrap();

    // Head operation doesn't modify original
    harness
        .execute_let_binding("let head_result = List.head(value: original)")
        .unwrap();
    harness
        .assert_evaluates_to("[1, 2, 3]", "original")
        .unwrap();

    // Tail operation doesn't modify original
    harness
        .execute_let_binding("let tail_result = List.tail(value: original)")
        .unwrap();
    harness
        .assert_evaluates_to("[1, 2, 3]", "original")
        .unwrap();
    harness
        .assert_evaluates_to("[2, 3]", "tail_result")
        .unwrap();

    // Prepend operation doesn't modify original
    harness
        .execute_let_binding("let prepended = List.prepend(list: original, elem: 0)")
        .unwrap();
    harness
        .assert_evaluates_to("[1, 2, 3]", "original")
        .unwrap();
    harness
        .assert_evaluates_to("[0, 1, 2, 3]", "prepended")
        .unwrap();

    // Multiple operations on same list
    harness.execute_let_binding("let base = [5, 6, 7]").unwrap();
    harness
        .execute_let_binding("let operation1 = List.prepend(list: base, elem: 4)")
        .unwrap();
    harness
        .execute_let_binding("let operation2 = List.tail(value: base)")
        .unwrap();
    harness
        .execute_let_binding("let operation3 = List.prepend(list: base, elem: 8)")
        .unwrap();

    harness.assert_evaluates_to("[5, 6, 7]", "base").unwrap();
    harness
        .assert_evaluates_to("[4, 5, 6, 7]", "operation1")
        .unwrap();
    harness.assert_evaluates_to("[6, 7]", "operation2").unwrap();
    harness
        .assert_evaluates_to("[8, 5, 6, 7]", "operation3")
        .unwrap();
}

#[test]
fn test_list_edge_cases() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Single element list operations
    harness.execute_let_binding("let single = [42]").unwrap();
    harness
        .assert_evaluates_to_integer("List.head(value: single) |> Option.unwrap()", 42)
        .unwrap();
    harness
        .assert_evaluates_to("[]", "List.tail(value: single)")
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: single)", 1)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("List.empty?(value: single)", false)
        .unwrap();

    // Operations on tail of single element
    harness
        .assert_evaluates_to_boolean("List.tail(value: single) |> List.empty?()", true)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.tail(value: single) |> List.length()", 0)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: List.tail(value: single) |> List.head())",
            true,
        )
        .unwrap();

    // Multiple tail operations until empty
    harness
        .execute_let_binding("let three_elements = [1, 2, 3]")
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "List.tail(value: three_elements) |> List.tail() |> List.tail() |> List.length()",
            0,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "List.tail(value: three_elements) |> List.tail() |> List.tail() |> List.empty?()",
            true,
        )
        .unwrap();

    // Prepending to result of tail
    harness
        .assert_evaluates_to(
            "[99, 2, 3]",
            "List.tail(value: [1, 2, 3]) |> List.prepend(elem: 99)",
        )
        .unwrap();
}

#[test]
fn test_list_in_collections() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Lists as tuple elements
    harness
        .execute_let_binding("let tuple_with_lists = ([1, 2], [\"a\", \"b\"], [])")
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "case tuple_with_lists { (first, second, third) => List.length(value: first) }",
            2,
        )
        .unwrap();

    // Nested lists (lists of lists would need explicit typing, so using simpler collections)
    harness
        .execute_let_binding("let list_in_option = Option.some(value: [1, 2, 3])")
        .unwrap();
    harness
        .assert_evaluates_to_integer("Option.unwrap(value: list_in_option) |> List.length()", 3)
        .unwrap();

    // Lists with different element types
    harness
        .execute_let_binding("let atom_list = [:first, :second, :third]")
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "List.head(value: atom_list) |> Option.unwrap() |> Display.to_string()",
            "first",
        )
        .unwrap();
}

#[test]
fn test_list_complex_expressions() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Lists in conditional expressions
    harness
        .assert_evaluates_to_string(
            "if List.empty?(value: []) { \"empty list\" } else { \"not empty\" }",
            "empty list",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "if List.length(value: [1, 2, 3]) > 2 { \"long list\" } else { \"short list\" }",
            "long list",
        )
        .unwrap();

    // Chained operations in conditionals
    harness
        .assert_evaluates_to_string(
            "if List.tail(value: [1]) |> List.empty?() { \"tail is empty\" } else { \"tail has elements\" }",
            "tail is empty"
        )
        .unwrap();

    // Complex boolean expressions with lists
    harness
        .assert_evaluates_to_boolean(
            "(List.length(value: [1, 2, 3]) == 3) && (List.head(value: [1, 2, 3]) |> Option.some?())",
            true
        )
        .unwrap();

    // Lists in arithmetic expressions (length-based)
    harness
        .assert_evaluates_to_integer(
            "List.length(value: [1, 2]) + List.length(value: [3, 4, 5])",
            5,
        )
        .unwrap();
}

#[test]
fn test_list_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up list variables
    harness.execute_let_binding("let empty_list = []").unwrap();
    harness
        .execute_let_binding("let numbers = [1, 2, 3, 4, 5]")
        .unwrap();
    harness
        .execute_let_binding("let words = [\"hello\", \"world\"]")
        .unwrap();

    // Test operations with variables
    harness
        .assert_evaluates_to_boolean("List.empty?(value: empty_list)", true)
        .unwrap();
    harness
        .assert_evaluates_to_integer("List.length(value: numbers)", 5)
        .unwrap();
    harness
        .assert_evaluates_to_string("List.head(value: words) |> Option.unwrap()", "hello")
        .unwrap();

    // Operations that modify variables
    harness
        .execute_let_binding("let head_result = List.head(value: numbers)")
        .unwrap();
    harness
        .execute_let_binding("let tail_result = List.tail(value: numbers)")
        .unwrap();
    harness
        .execute_let_binding("let prepend_result = List.prepend(list: numbers, elem: 0)")
        .unwrap();

    harness
        .assert_evaluates_to_integer("Option.unwrap(value: head_result)", 1)
        .unwrap();
    harness
        .assert_evaluates_to("[2, 3, 4, 5]", "tail_result")
        .unwrap();
    harness
        .assert_evaluates_to("[0, 1, 2, 3, 4, 5]", "prepend_result")
        .unwrap();

    // Original list should be unchanged
    harness
        .assert_evaluates_to("[1, 2, 3, 4, 5]", "numbers")
        .unwrap();
}

#[test]
fn test_list_type_consistency() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test that all list operations return expected types
    let result1 = harness.evaluate("List.head(value: [1, 2, 3])").unwrap();
    let result2 = harness.evaluate("List.tail(value: [1, 2, 3])").unwrap();
    let result3 = harness
        .evaluate("List.prepend(list: [2, 3], elem: 1)")
        .unwrap();
    let result4 = harness.evaluate("List.length(value: [1, 2, 3])").unwrap();
    let result5 = harness.evaluate("List.empty?(value: [])").unwrap();

    // Check result types
    assert!(matches!(result1, crate::Value::Struct { .. })); // Option<Integer>
    assert!(matches!(result2, crate::Value::List { .. })); // List<Integer>
    assert!(matches!(result3, crate::Value::List { .. })); // List<Integer>
    assert!(matches!(result4, crate::Value::Integer64(_))); // Integer
    assert!(matches!(result5, crate::Value::Boolean(_))); // Boolean

    // Test empty list head returns None
    let empty_head = harness.evaluate("List.head(value: [])").unwrap();
    assert!(matches!(empty_head, crate::Value::Struct { .. })); // Option.None

    // Test list creation and usage
    let list_result = harness.evaluate("[1, 2, 3]").unwrap();
    assert!(matches!(list_result, crate::Value::List { .. }));

    // Test lists can be used in all contexts where values are expected
    harness
        .execute_let_binding("let my_list = [42, 43, 44]")
        .unwrap();
    let variable_result = harness.evaluate("my_list").unwrap();
    assert!(matches!(variable_result, crate::Value::List { .. }));
}
