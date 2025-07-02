//! Acceptance tests for String trait functions

use crate::test_harness::OutrunTestHarness;

#[test]
fn test_string_length() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic string lengths
    harness
        .assert_evaluates_to_integer("String.length(value: \"hello\")", 5)
        .unwrap();
    harness
        .assert_evaluates_to_integer("String.length(value: \"\")", 0)
        .unwrap();
    harness
        .assert_evaluates_to_integer("String.length(value: \"a\")", 1)
        .unwrap();

    // Unicode strings (code points, not bytes)
    harness
        .assert_evaluates_to_integer("String.length(value: \"café\")", 4)
        .unwrap();
    harness
        .assert_evaluates_to_integer("String.length(value: \"🦀\")", 1)
        .unwrap();
    harness
        .assert_evaluates_to_integer("String.length(value: \"hello 🌍\")", 7)
        .unwrap();

    // Whitespace and special characters
    harness
        .assert_evaluates_to_integer("String.length(value: \"  \")", 2)
        .unwrap();
    harness
        .assert_evaluates_to_integer("String.length(value: \"\\n\\t\")", 2)
        .unwrap();
}

#[test]
fn test_string_char_at() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Valid indices
    harness
        .assert_evaluates_to_string(
            "String.char_at(value: \"hello\", index: 0) |> Option.unwrap()",
            "h",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "String.char_at(value: \"hello\", index: 1) |> Option.unwrap()",
            "e",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "String.char_at(value: \"hello\", index: 4) |> Option.unwrap()",
            "o",
        )
        .unwrap();

    // Invalid indices (out of bounds)
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: String.char_at(value: \"hello\", index: 5))",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: String.char_at(value: \"hello\", index: -1))",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: String.char_at(value: \"\", index: 0))",
            true,
        )
        .unwrap();

    // Unicode characters
    harness
        .assert_evaluates_to_string(
            "String.char_at(value: \"café\", index: 3) |> Option.unwrap()",
            "é",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "String.char_at(value: \"🦀rust\", index: 0) |> Option.unwrap()",
            "🦀",
        )
        .unwrap();
}

#[test]
fn test_string_slice() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic slicing
    harness
        .assert_evaluates_to_string("String.slice(value: \"hello\", start: 1, end: 4)", "ell")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.slice(value: \"hello\", start: 0, end: 5)", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.slice(value: \"hello\", start: 2, end: 2)", "")
        .unwrap();

    // Edge cases
    harness
        .assert_evaluates_to_string("String.slice(value: \"hello\", start: 0, end: 0)", "")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.slice(value: \"hello\", start: 5, end: 5)", "")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.slice(value: \"\", start: 0, end: 0)", "")
        .unwrap();

    // Out of bounds (should return empty or partial)
    harness
        .assert_evaluates_to_string("String.slice(value: \"hello\", start: 3, end: 10)", "lo")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.slice(value: \"hello\", start: 10, end: 15)", "")
        .unwrap();

    // Unicode slicing
    harness
        .assert_evaluates_to_string("String.slice(value: \"café\", start: 1, end: 3)", "af")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.slice(value: \"🦀rust\", start: 1, end: 3)", "ru")
        .unwrap();
}

#[test]
fn test_string_concat() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic concatenation
    harness
        .assert_evaluates_to_string(
            "String.concat(lhs: \"hello\", rhs: \" world\")",
            "hello world",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string("String.concat(lhs: \"foo\", rhs: \"bar\")", "foobar")
        .unwrap();

    // Empty string concatenation
    harness
        .assert_evaluates_to_string("String.concat(lhs: \"\", rhs: \"test\")", "test")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.concat(lhs: \"test\", rhs: \"\")", "test")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.concat(lhs: \"\", rhs: \"\")", "")
        .unwrap();

    // Unicode concatenation
    harness
        .assert_evaluates_to_string("String.concat(lhs: \"café\", rhs: \" ☕\")", "café ☕")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.concat(lhs: \"🦀\", rhs: \"rust\")", "🦀rust")
        .unwrap();
}

#[test]
fn test_string_index_of() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Found cases
    harness
        .assert_evaluates_to_integer(
            "String.index_of(value: \"hello world\", search: \"world\") |> Option.unwrap()",
            6,
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "String.index_of(value: \"hello\", search: \"h\") |> Option.unwrap()",
            0,
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "String.index_of(value: \"hello\", search: \"o\") |> Option.unwrap()",
            4,
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "String.index_of(value: \"hello\", search: \"ell\") |> Option.unwrap()",
            1,
        )
        .unwrap();

    // Not found cases
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: String.index_of(value: \"hello\", search: \"xyz\"))",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: String.index_of(value: \"hello\", search: \"Hello\"))",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: String.index_of(value: \"\", search: \"a\"))",
            true,
        )
        .unwrap();

    // Empty search string
    harness
        .assert_evaluates_to_integer(
            "String.index_of(value: \"hello\", search: \"\") |> Option.unwrap()",
            0,
        )
        .unwrap();

    // Unicode search
    harness
        .assert_evaluates_to_integer(
            "String.index_of(value: \"café latte\", search: \"é\") |> Option.unwrap()",
            3,
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "String.index_of(value: \"🦀rust lang\", search: \"🦀\") |> Option.unwrap()",
            0,
        )
        .unwrap();
}

#[test]
fn test_string_contains() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Contains cases
    harness
        .assert_evaluates_to_boolean(
            "String.contains?(value: \"hello world\", search: \"world\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.contains?(value: \"hello\", search: \"ell\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.contains?(value: \"hello\", search: \"hello\")",
            true,
        )
        .unwrap();

    // Does not contain cases
    harness
        .assert_evaluates_to_boolean("String.contains?(value: \"hello\", search: \"xyz\")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.contains?(value: \"hello\", search: \"Hello\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.contains?(value: \"\", search: \"a\")", false)
        .unwrap();

    // Empty search string
    harness
        .assert_evaluates_to_boolean("String.contains?(value: \"hello\", search: \"\")", true)
        .unwrap();

    // Unicode contains
    harness
        .assert_evaluates_to_boolean(
            "String.contains?(value: \"café latte\", search: \"é\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.contains?(value: \"🦀rust\", search: \"🦀\")", true)
        .unwrap();
}

#[test]
fn test_string_starts_with() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Starts with cases
    harness
        .assert_evaluates_to_boolean(
            "String.starts_with?(value: \"hello world\", prefix: \"hello\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.starts_with?(value: \"hello\", prefix: \"h\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.starts_with?(value: \"hello\", prefix: \"hello\")",
            true,
        )
        .unwrap();

    // Does not start with cases
    harness
        .assert_evaluates_to_boolean(
            "String.starts_with?(value: \"hello\", prefix: \"world\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.starts_with?(value: \"hello\", prefix: \"Hello\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.starts_with?(value: \"hello\", prefix: \"ello\")",
            false,
        )
        .unwrap();

    // Empty prefix
    harness
        .assert_evaluates_to_boolean("String.starts_with?(value: \"hello\", prefix: \"\")", true)
        .unwrap();

    // Empty string
    harness
        .assert_evaluates_to_boolean("String.starts_with?(value: \"\", prefix: \"\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.starts_with?(value: \"\", prefix: \"a\")", false)
        .unwrap();

    // Unicode starts with
    harness
        .assert_evaluates_to_boolean(
            "String.starts_with?(value: \"café latte\", prefix: \"café\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.starts_with?(value: \"🦀rust\", prefix: \"🦀\")",
            true,
        )
        .unwrap();
}

#[test]
fn test_string_ends_with() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Ends with cases
    harness
        .assert_evaluates_to_boolean(
            "String.ends_with?(value: \"hello world\", suffix: \"world\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.ends_with?(value: \"hello\", suffix: \"o\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.ends_with?(value: \"hello\", suffix: \"hello\")",
            true,
        )
        .unwrap();

    // Does not end with cases
    harness
        .assert_evaluates_to_boolean(
            "String.ends_with?(value: \"hello\", suffix: \"world\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.ends_with?(value: \"hello\", suffix: \"Hello\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.ends_with?(value: \"hello\", suffix: \"hell\")",
            false,
        )
        .unwrap();

    // Empty suffix
    harness
        .assert_evaluates_to_boolean("String.ends_with?(value: \"hello\", suffix: \"\")", true)
        .unwrap();

    // Empty string
    harness
        .assert_evaluates_to_boolean("String.ends_with?(value: \"\", suffix: \"\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.ends_with?(value: \"\", suffix: \"a\")", false)
        .unwrap();

    // Unicode ends with
    harness
        .assert_evaluates_to_boolean(
            "String.ends_with?(value: \"café latte\", suffix: \"latte\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.ends_with?(value: \"rust🦀\", suffix: \"🦀\")", true)
        .unwrap();
}

#[test]
fn test_string_to_upper() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic uppercase conversion
    harness
        .assert_evaluates_to_string("String.to_upper(value: \"hello\")", "HELLO")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_upper(value: \"Hello World\")", "HELLO WORLD")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_upper(value: \"ALREADY UPPER\")", "ALREADY UPPER")
        .unwrap();

    // Empty string
    harness
        .assert_evaluates_to_string("String.to_upper(value: \"\")", "")
        .unwrap();

    // Numbers and symbols (should remain unchanged)
    harness
        .assert_evaluates_to_string("String.to_upper(value: \"hello123!@#\")", "HELLO123!@#")
        .unwrap();

    // Unicode case conversion
    harness
        .assert_evaluates_to_string("String.to_upper(value: \"café\")", "CAFÉ")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_upper(value: \"ñiño\")", "ÑIÑO")
        .unwrap();
}

#[test]
fn test_string_to_lower() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic lowercase conversion
    harness
        .assert_evaluates_to_string("String.to_lower(value: \"HELLO\")", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_lower(value: \"Hello World\")", "hello world")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_lower(value: \"already lower\")", "already lower")
        .unwrap();

    // Empty string
    harness
        .assert_evaluates_to_string("String.to_lower(value: \"\")", "")
        .unwrap();

    // Numbers and symbols (should remain unchanged)
    harness
        .assert_evaluates_to_string("String.to_lower(value: \"HELLO123!@#\")", "hello123!@#")
        .unwrap();

    // Unicode case conversion
    harness
        .assert_evaluates_to_string("String.to_lower(value: \"CAFÉ\")", "café")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_lower(value: \"ÑIÑO\")", "ñiño")
        .unwrap();
}

#[test]
fn test_string_trim() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic trimming
    harness
        .assert_evaluates_to_string("String.trim(value: \"  hello  \")", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim(value: \"\\t\\nhello\\r\\n\")", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim(value: \"   \")", "")
        .unwrap();

    // No trimming needed
    harness
        .assert_evaluates_to_string("String.trim(value: \"hello\")", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim(value: \"\")", "")
        .unwrap();

    // Only start or end whitespace
    harness
        .assert_evaluates_to_string("String.trim(value: \"  hello\")", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim(value: \"hello  \")", "hello")
        .unwrap();

    // Internal whitespace preserved
    harness
        .assert_evaluates_to_string("String.trim(value: \"  hello world  \")", "hello world")
        .unwrap();
}

#[test]
fn test_string_trim_start() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic start trimming
    harness
        .assert_evaluates_to_string("String.trim_start(value: \"  hello  \")", "hello  ")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim_start(value: \"\\t\\nhello\")", "hello")
        .unwrap();

    // No trimming needed
    harness
        .assert_evaluates_to_string("String.trim_start(value: \"hello  \")", "hello  ")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim_start(value: \"hello\")", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim_start(value: \"\")", "")
        .unwrap();

    // Only whitespace
    harness
        .assert_evaluates_to_string("String.trim_start(value: \"   \")", "")
        .unwrap();
}

#[test]
fn test_string_trim_end() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic end trimming
    harness
        .assert_evaluates_to_string("String.trim_end(value: \"  hello  \")", "  hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim_end(value: \"hello\\t\\n\")", "hello")
        .unwrap();

    // No trimming needed
    harness
        .assert_evaluates_to_string("String.trim_end(value: \"  hello\")", "  hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim_end(value: \"hello\")", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.trim_end(value: \"\")", "")
        .unwrap();

    // Only whitespace
    harness
        .assert_evaluates_to_string("String.trim_end(value: \"   \")", "")
        .unwrap();
}

#[test]
fn test_string_valid_utf8() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Valid UTF-8 strings (all Outrun strings should be valid)
    harness
        .assert_evaluates_to_boolean("String.valid_utf8?(value: \"hello\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.valid_utf8?(value: \"\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.valid_utf8?(value: \"café\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.valid_utf8?(value: \"🦀rust\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.valid_utf8?(value: \"こんにちは\")", true)
        .unwrap();
}

#[test]
fn test_string_to_string() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // String to_string should return itself
    harness
        .assert_evaluates_to_string("String.to_string(value: \"hello\")", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_string(value: \"\")", "")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_string(value: \"café 🦀\")", "café 🦀")
        .unwrap();
}

#[test]
fn test_string_empty() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Empty string
    harness
        .assert_evaluates_to_boolean("String.empty?(value: \"\")", true)
        .unwrap();

    // Non-empty strings
    harness
        .assert_evaluates_to_boolean("String.empty?(value: \"hello\")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.empty?(value: \" \")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.empty?(value: \"a\")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.empty?(value: \"🦀\")", false)
        .unwrap();
}

#[test]
fn test_string_functions_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables
    harness
        .execute_let_binding("let text = \"Hello World\"")
        .unwrap();
    harness
        .execute_let_binding("let empty_text = \"\"")
        .unwrap();
    harness
        .execute_let_binding("let unicode_text = \"café 🦀\"")
        .unwrap();

    // Test with variables
    harness
        .assert_evaluates_to_integer("String.length(value: text)", 11)
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_upper(value: text)", "HELLO WORLD")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.to_lower(value: text)", "hello world")
        .unwrap();
    harness
        .assert_evaluates_to_string("String.slice(value: text, start: 0, end: 5)", "Hello")
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.starts_with?(value: text, prefix: \"Hello\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.ends_with?(value: text, suffix: \"World\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("String.contains?(value: text, search: \" \")", true)
        .unwrap();

    // Test with empty string variable
    harness
        .assert_evaluates_to_boolean("String.empty?(value: empty_text)", true)
        .unwrap();
    harness
        .assert_evaluates_to_integer("String.length(value: empty_text)", 0)
        .unwrap();

    // Test with unicode variable
    harness
        .assert_evaluates_to_integer("String.length(value: unicode_text)", 6)
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "String.char_at(value: unicode_text, index: 3) |> Option.unwrap()",
            "é",
        )
        .unwrap();
}

#[test]
fn test_string_chained_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Chain multiple string operations
    harness
        .assert_evaluates_to_string(
            "String.trim(value: \"  hello  \") |> String.to_upper()",
            "HELLO",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "String.concat(lhs: \"hello\", rhs: \" world\") |> String.slice(start: 0, end: 5)",
            "hello",
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "String.concat(lhs: \"foo\", rhs: \"bar\") |> String.length()",
            6,
        )
        .unwrap();

    // Complex chaining with predicates
    harness
        .assert_evaluates_to_boolean("String.trim(value: \"  test  \") |> String.empty?()", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "String.to_upper(value: \"hello\") |> String.starts_with?(prefix: \"HEL\")",
            true,
        )
        .unwrap();
}
