//! Acceptance tests for Binary trait functions

use crate::test_harness::OutrunTestHarness;

#[test]
fn test_binary_byte_size() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic byte sizes
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: \"hello\")", 5)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: \"\")", 0)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: \"a\")", 1)
        .unwrap();

    // Unicode strings (bytes, not code points)
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: \"café\")", 5)
        .unwrap(); // é is 2 bytes in UTF-8
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: \"🦀\")", 4)
        .unwrap(); // crab emoji is 4 bytes
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: \"hello 🌍\")", 10)
        .unwrap(); // hello(5) + space(1) + earth(4)

    // ASCII characters
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: \"ABC123\")", 6)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: \"!@#$%^&*()\")", 10)
        .unwrap();
}

#[test]
fn test_binary_byte_at() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Test basic functionality without pipe operator
    // First verify Binary.byte_at returns Some values for valid indices
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: \"hello\", index: 0)",
            104,
        )
        .unwrap(); // 'h'
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: \"hello\", index: 1)",
            101,
        )
        .unwrap(); // 'e'
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: \"hello\", index: 4)",
            111,
        )
        .unwrap(); // 'o'

    // Valid indices for numbers and symbols
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: \"ABC\", index: 0)",
            65,
        )
        .unwrap(); // 'A'
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: \"123\", index: 0)",
            49,
        )
        .unwrap(); // '1'
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: \"!@#\", index: 0)",
            33,
        )
        .unwrap(); // '!'

    // TODO: Test invalid indices when Option.none? guard function is fixed
    // For now, just test that Binary.byte_at returns values for valid indices

    // Multi-byte Unicode characters - checking individual bytes
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: \"café\", index: 3)",
            195,
        )
        .unwrap(); // First byte of 'é'
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: \"café\", index: 4)",
            169,
        )
        .unwrap(); // Second byte of 'é'
}

#[test]
fn test_binary_slice() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic slicing with ASCII
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"hello\", start: 1, end: 4)", "ell")
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"hello\", start: 0, end: 5)", "hello")
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"hello\", start: 2, end: 2)", "")
        .unwrap();

    // Edge cases
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"hello\", start: 0, end: 0)", "")
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"hello\", start: 5, end: 5)", "")
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"\", start: 0, end: 0)", "")
        .unwrap();

    // Out of bounds (should return empty or partial)
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"hello\", start: 3, end: 10)", "lo")
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"hello\", start: 10, end: 15)", "")
        .unwrap();

    // Byte-level slicing of Unicode (may produce invalid UTF-8)
    // This should work at byte level, not character level
    harness
        .assert_evaluates_to_string("Binary.slice(value: \"café\", start: 0, end: 3)", "caf")
        .unwrap(); // Just "caf" (before 'é')
    harness
        .assert_evaluates_to_string(
            "Binary.slice(value: \"hello🦀\", start: 0, end: 5)",
            "hello",
        )
        .unwrap(); // Just "hello" (before emoji)
}

#[test]
fn test_binary_concat() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic concatenation
    harness
        .assert_evaluates_to_string(
            "Binary.concat(lhs: \"hello\", rhs: \" world\")",
            "hello world",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.concat(lhs: \"foo\", rhs: \"bar\")", "foobar")
        .unwrap();

    // Empty string concatenation
    harness
        .assert_evaluates_to_string("Binary.concat(lhs: \"\", rhs: \"test\")", "test")
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.concat(lhs: \"test\", rhs: \"\")", "test")
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.concat(lhs: \"\", rhs: \"\")", "")
        .unwrap();

    // Unicode concatenation
    harness
        .assert_evaluates_to_string("Binary.concat(lhs: \"café\", rhs: \" ☕\")", "café ☕")
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.concat(lhs: \"🦀\", rhs: \"rust\")", "🦀rust")
        .unwrap();

    // Mixed ASCII and Unicode
    harness
        .assert_evaluates_to_string("Binary.concat(lhs: \"hello\", rhs: \"🌍\")", "hello🌍")
        .unwrap();
}

#[test]
fn test_binary_index_of() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Found cases with ASCII
    harness
        .assert_evaluates_to_integer(
            "Binary.index_of(value: \"hello world\", search: \"world\") |> Option.unwrap()",
            6,
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "Binary.index_of(value: \"hello\", search: \"h\") |> Option.unwrap()",
            0,
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "Binary.index_of(value: \"hello\", search: \"o\") |> Option.unwrap()",
            4,
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "Binary.index_of(value: \"hello\", search: \"ell\") |> Option.unwrap()",
            1,
        )
        .unwrap();

    // Not found cases
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: Binary.index_of(value: \"hello\", search: \"xyz\"))",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: Binary.index_of(value: \"hello\", search: \"Hello\"))",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: Binary.index_of(value: \"\", search: \"a\"))",
            true,
        )
        .unwrap();

    // Empty search string
    harness
        .assert_evaluates_to_integer(
            "Binary.index_of(value: \"hello\", search: \"\") |> Option.unwrap()",
            0,
        )
        .unwrap();

    // Unicode search (byte-level search)
    harness
        .assert_evaluates_to_integer(
            "Binary.index_of(value: \"hello🦀rust\", search: \"rust\") |> Option.unwrap()",
            9,
        )
        .unwrap(); // After hello(5) + crab(4)
    harness
        .assert_evaluates_to_integer(
            "Binary.index_of(value: \"café latte\", search: \"latte\") |> Option.unwrap()",
            6,
        )
        .unwrap(); // After café(5) + space(1)
}

#[test]
fn test_binary_contains() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Contains cases
    harness
        .assert_evaluates_to_boolean(
            "Binary.contains?(value: \"hello world\", search: \"world\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.contains?(value: \"hello\", search: \"ell\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.contains?(value: \"hello\", search: \"hello\")",
            true,
        )
        .unwrap();

    // Does not contain cases
    harness
        .assert_evaluates_to_boolean("Binary.contains?(value: \"hello\", search: \"xyz\")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.contains?(value: \"hello\", search: \"Hello\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.contains?(value: \"\", search: \"a\")", false)
        .unwrap();

    // Empty search string
    harness
        .assert_evaluates_to_boolean("Binary.contains?(value: \"hello\", search: \"\")", true)
        .unwrap();

    // Unicode contains (byte-level)
    harness
        .assert_evaluates_to_boolean(
            "Binary.contains?(value: \"café latte\", search: \"latte\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.contains?(value: \"🦀rust\", search: \"rust\")",
            true,
        )
        .unwrap();
}

#[test]
fn test_binary_starts_with() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Starts with cases
    harness
        .assert_evaluates_to_boolean(
            "Binary.starts_with?(value: \"hello world\", prefix: \"hello\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.starts_with?(value: \"hello\", prefix: \"h\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.starts_with?(value: \"hello\", prefix: \"hello\")",
            true,
        )
        .unwrap();

    // Does not start with cases
    harness
        .assert_evaluates_to_boolean(
            "Binary.starts_with?(value: \"hello\", prefix: \"world\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.starts_with?(value: \"hello\", prefix: \"Hello\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.starts_with?(value: \"hello\", prefix: \"ello\")",
            false,
        )
        .unwrap();

    // Empty prefix
    harness
        .assert_evaluates_to_boolean("Binary.starts_with?(value: \"hello\", prefix: \"\")", true)
        .unwrap();

    // Empty string
    harness
        .assert_evaluates_to_boolean("Binary.starts_with?(value: \"\", prefix: \"\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.starts_with?(value: \"\", prefix: \"a\")", false)
        .unwrap();

    // Unicode starts with (byte-level)
    harness
        .assert_evaluates_to_boolean(
            "Binary.starts_with?(value: \"café latte\", prefix: \"café\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.starts_with?(value: \"🦀rust\", prefix: \"🦀\")",
            true,
        )
        .unwrap();
}

#[test]
fn test_binary_ends_with() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Ends with cases
    harness
        .assert_evaluates_to_boolean(
            "Binary.ends_with?(value: \"hello world\", suffix: \"world\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.ends_with?(value: \"hello\", suffix: \"o\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.ends_with?(value: \"hello\", suffix: \"hello\")",
            true,
        )
        .unwrap();

    // Does not end with cases
    harness
        .assert_evaluates_to_boolean(
            "Binary.ends_with?(value: \"hello\", suffix: \"world\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.ends_with?(value: \"hello\", suffix: \"Hello\")",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean(
            "Binary.ends_with?(value: \"hello\", suffix: \"hell\")",
            false,
        )
        .unwrap();

    // Empty suffix
    harness
        .assert_evaluates_to_boolean("Binary.ends_with?(value: \"hello\", suffix: \"\")", true)
        .unwrap();

    // Empty string
    harness
        .assert_evaluates_to_boolean("Binary.ends_with?(value: \"\", suffix: \"\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.ends_with?(value: \"\", suffix: \"a\")", false)
        .unwrap();

    // Unicode ends with (byte-level)
    harness
        .assert_evaluates_to_boolean(
            "Binary.ends_with?(value: \"café latte\", suffix: \"latte\")",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.ends_with?(value: \"rust🦀\", suffix: \"🦀\")", true)
        .unwrap();
}

#[test]
fn test_binary_to_hex() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Basic ASCII to hex
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"hello\")", "68656c6c6f")
        .unwrap(); // h(0x68) e(0x65) l(0x6c) l(0x6c) o(0x6f)
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"ABC\")", "414243")
        .unwrap(); // A(0x41) B(0x42) C(0x43)
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"123\")", "313233")
        .unwrap(); // 1(0x31) 2(0x32) 3(0x33)

    // Empty string
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"\")", "")
        .unwrap();

    // Single character
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"a\")", "61")
        .unwrap(); // a(0x61)
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"A\")", "41")
        .unwrap(); // A(0x41)

    // Special characters
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"!@#\")", "214023")
        .unwrap(); // !(0x21) @(0x40) #(0x23)

    // Unicode characters (multi-byte UTF-8)
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"é\")", "c3a9")
        .unwrap(); // é in UTF-8: 0xC3 0xA9
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: \"🦀\")", "f09fa680")
        .unwrap(); // crab emoji in UTF-8: 0xF0 0x9F 0xA6 0x80
}

#[test]
fn test_binary_from_hex() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Valid hex strings
    harness
        .assert_evaluates_to_string(
            "Binary.from_hex(value: \"68656c6c6f\") |> Option.unwrap()",
            "hello",
        )
        .unwrap(); // hex for "hello"
    harness
        .assert_evaluates_to_string(
            "Binary.from_hex(value: \"414243\") |> Option.unwrap()",
            "ABC",
        )
        .unwrap(); // hex for "ABC"
    harness
        .assert_evaluates_to_string(
            "Binary.from_hex(value: \"313233\") |> Option.unwrap()",
            "123",
        )
        .unwrap(); // hex for "123"

    // Empty hex string
    harness
        .assert_evaluates_to_string("Binary.from_hex(value: \"\") |> Option.unwrap()", "")
        .unwrap();

    // Single byte
    harness
        .assert_evaluates_to_string("Binary.from_hex(value: \"61\") |> Option.unwrap()", "a")
        .unwrap(); // hex for "a"
    harness
        .assert_evaluates_to_string("Binary.from_hex(value: \"41\") |> Option.unwrap()", "A")
        .unwrap(); // hex for "A"

    // Unicode characters from hex
    harness
        .assert_evaluates_to_string("Binary.from_hex(value: \"c3a9\") |> Option.unwrap()", "é")
        .unwrap(); // hex for "é"
    harness
        .assert_evaluates_to_string(
            "Binary.from_hex(value: \"f09fa680\") |> Option.unwrap()",
            "🦀",
        )
        .unwrap(); // hex for crab emoji

    // Invalid hex strings (should return None)
    harness
        .assert_evaluates_to_boolean(
            "Option.none?(value: Binary.from_hex(value: \"invalid\"))",
            true,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Option.none?(value: Binary.from_hex(value: \"xyz\"))", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Option.none?(value: Binary.from_hex(value: \"1\"))", true)
        .unwrap(); // Odd length
    harness
        .assert_evaluates_to_boolean("Option.none?(value: Binary.from_hex(value: \"123\"))", true)
        .unwrap(); // Odd length
    harness
        .assert_evaluates_to_boolean("Option.none?(value: Binary.from_hex(value: \"1g\"))", true)
        .unwrap(); // Invalid hex character

    // Case insensitive hex (if supported)
    harness
        .assert_evaluates_to_string(
            "Binary.from_hex(value: \"41424344\") |> Option.unwrap()",
            "ABCD",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.from_hex(value: \"ABCD\") |> Option.unwrap()", "«CD")
        .unwrap(); // 0xAB 0xCD as bytes
}

#[test]
fn test_binary_empty() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Empty binary data
    harness
        .assert_evaluates_to_boolean("Binary.empty?(value: \"\")", true)
        .unwrap();

    // Non-empty binary data
    harness
        .assert_evaluates_to_boolean("Binary.empty?(value: \"hello\")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.empty?(value: \" \")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.empty?(value: \"a\")", false)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.empty?(value: \"🦀\")", false)
        .unwrap();
}

#[test]
fn test_binary_functions_with_variables() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Set up variables
    harness
        .execute_let_binding("let data = \"Hello World\"")
        .unwrap();
    harness
        .execute_let_binding("let empty_data = \"\"")
        .unwrap();
    harness
        .execute_let_binding("let unicode_data = \"café 🦀\"")
        .unwrap();
    harness
        .execute_let_binding("let hex_string = \"68656c6c6f\"")
        .unwrap(); // "hello"

    // Test with variables
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: data)", 11)
        .unwrap();
    harness
        .assert_evaluates_to_some_integer(
            "Binary.byte_at(value: data, index: 0)",
            72,
        )
        .unwrap(); // 'H'
    harness
        .assert_evaluates_to_string("Binary.slice(value: data, start: 0, end: 5)", "Hello")
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.starts_with?(value: data, prefix: \"Hello\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.ends_with?(value: data, suffix: \"World\")", true)
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.contains?(value: data, search: \" \")", true)
        .unwrap();

    // Test with empty data variable
    harness
        .assert_evaluates_to_boolean("Binary.empty?(value: empty_data)", true)
        .unwrap();
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: empty_data)", 0)
        .unwrap();

    // Test with unicode variable (byte counting)
    harness
        .assert_evaluates_to_integer("Binary.byte_size(value: unicode_data)", 9)
        .unwrap(); // café(5) + space(1) + crab(3)

    // Test hex conversion with variables
    harness
        .assert_evaluates_to_string(
            "Binary.from_hex(value: hex_string) |> Option.unwrap()",
            "hello",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string("Binary.to_hex(value: data)", "48656c6c6f20576f726c64")
        .unwrap(); // "Hello World" in hex
}

#[test]
fn test_binary_hex_roundtrip() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // ASCII roundtrip
    harness
        .assert_evaluates_to_string(
            "Binary.to_hex(value: \"hello\") |> Binary.from_hex() |> Option.unwrap()",
            "hello",
        )
        .unwrap();

    // Unicode roundtrip
    harness
        .assert_evaluates_to_string(
            "Binary.to_hex(value: \"café\") |> Binary.from_hex() |> Option.unwrap()",
            "café",
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Binary.to_hex(value: \"🦀\") |> Binary.from_hex() |> Option.unwrap()",
            "🦀",
        )
        .unwrap();

    // Empty string roundtrip
    harness
        .assert_evaluates_to_string(
            "Binary.to_hex(value: \"\") |> Binary.from_hex() |> Option.unwrap()",
            "",
        )
        .unwrap();

    // Complex string roundtrip
    harness
        .assert_evaluates_to_string(
            "Binary.to_hex(value: \"Hello 🌍!\") |> Binary.from_hex() |> Option.unwrap()",
            "Hello 🌍!",
        )
        .unwrap();
}

#[test]
fn test_binary_chained_operations() {
    let mut harness = OutrunTestHarness::new().unwrap();

    // Chain multiple binary operations
    harness
        .assert_evaluates_to_string(
            "Binary.concat(lhs: \"hello\", rhs: \" world\") |> Binary.slice(start: 0, end: 5)",
            "hello",
        )
        .unwrap();
    harness
        .assert_evaluates_to_integer(
            "Binary.concat(lhs: \"foo\", rhs: \"bar\") |> Binary.byte_size()",
            6,
        )
        .unwrap();
    harness
        .assert_evaluates_to_string(
            "Binary.slice(value: \"hello world\", start: 6, end: 11) |> Binary.to_hex()",
            "776f726c64",
        )
        .unwrap(); // "world" in hex

    // Complex chaining with predicates
    harness
        .assert_evaluates_to_boolean(
            "Binary.concat(lhs: \"test\", rhs: \"data\") |> Binary.empty?()",
            false,
        )
        .unwrap();
    harness
        .assert_evaluates_to_boolean("Binary.slice(value: \"hello\", start: 0, end: 2) |> Binary.starts_with?(prefix: \"he\")", true)
        .unwrap();

    // Hex conversion chains
    harness
        .assert_evaluates_to_integer("Binary.to_hex(value: \"abc\") |> Binary.byte_size()", 6)
        .unwrap(); // "616263" has 6 characters
}
