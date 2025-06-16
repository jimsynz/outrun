// Tests for multi-format integer literals

use crate::*;

// Helper function to extract integer from expression
fn extract_integer_from_expression(expr: &Expression) -> &IntegerLiteral {
    match &expr.kind {
        ExpressionKind::Integer(integer) => integer,
        _ => panic!("Expected integer in expression, got: {:?}", expr.kind),
    }
}

#[test]
fn test_parse_binary_integer() {
    let input = "0b1010";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let integer = extract_integer_from_expression(expr);
            assert_eq!(integer.value, 10); // 1010 in binary = 10 in decimal
            assert_eq!(integer.format, IntegerFormat::Binary);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_octal_integer() {
    let input = "0o755";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let integer = extract_integer_from_expression(expr);
            assert_eq!(integer.value, 493); // 755 in octal = 493 in decimal
            assert_eq!(integer.format, IntegerFormat::Octal);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_hexadecimal_integer() {
    let input = "0xFF";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let integer = extract_integer_from_expression(expr);
            assert_eq!(integer.value, 255); // FF in hex = 255 in decimal
            assert_eq!(integer.format, IntegerFormat::Hexadecimal);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_hexadecimal_lowercase() {
    let input = "0xabc";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let integer = extract_integer_from_expression(expr);
            assert_eq!(integer.value, 2748); // abc in hex = 2748 in decimal
            assert_eq!(integer.format, IntegerFormat::Hexadecimal);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_large_binary() {
    let input = "0b11111111";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let integer = extract_integer_from_expression(expr);
            assert_eq!(integer.value, 255); // 11111111 in binary = 255
            assert_eq!(integer.format, IntegerFormat::Binary);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_mixed_integer_formats() {
    let input = "42 0b1010 0o755 0xFF 123";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 5);

    // Expected values and formats
    let expected = [
        (42, IntegerFormat::Decimal),
        (10, IntegerFormat::Binary),
        (493, IntegerFormat::Octal),
        (255, IntegerFormat::Hexadecimal),
        (123, IntegerFormat::Decimal),
    ];

    for (i, (expected_value, expected_format)) in expected.iter().enumerate() {
        match &result.items[i].kind {
            ItemKind::Expression(expr) => {
                let integer = extract_integer_from_expression(expr);
                assert_eq!(
                    integer.value, *expected_value,
                    "Value mismatch at position {}",
                    i
                );
                assert_eq!(
                    integer.format, *expected_format,
                    "Format mismatch at position {}",
                    i
                );
            }
            _ => panic!("Expected expression at position {}", i),
        }
    }
}

#[test]
fn test_integer_format_display_preservation() {
    let test_cases = [
        ("42", "42"),         // Decimal stays decimal
        ("0b1010", "0b1010"), // Binary preserves format
        ("0o755", "0o755"),   // Octal preserves format
        ("0xFF", "0xff"),     // Hex preserves format (but lowercase)
        ("0xABC", "0xabc"),   // Hex uppercase becomes lowercase
    ];

    for (input, expected_display) in &test_cases {
        let result = parse_program(input).unwrap();

        assert_eq!(result.items.len(), 1);
        match &result.items[0].kind {
            ItemKind::Expression(expr) => {
                let integer = extract_integer_from_expression(expr);
                let display_result = format!("{}", integer);
                assert_eq!(
                    display_result, *expected_display,
                    "Display format mismatch for input '{}': expected '{}', got '{}'",
                    input, expected_display, display_result
                );
            }
            _ => panic!("Expected expression for input '{}'", input),
        }
    }
}

#[test]
fn test_comprehensive_with_all_formats() {
    let input = "struct MyType 42 true 0b1010 false 0o755 identifier 0xFF";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 9);

    // Verify the mixed sequence includes all integer formats
    let _expected = [
        ("struct", "keyword"),
        ("MyType", "type_identifier"),
        ("42", "decimal_integer"),
        ("true", "boolean"),
        ("0b1010", "binary_integer"),
        ("false", "boolean"),
        ("0o755", "octal_integer"),
        ("identifier", "identifier"),
        ("0xFF", "hex_integer"),
    ];

    // Extract just the integers to verify their formats
    let integers: Vec<_> = result
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Integer(int) => Some(int),
                _ => None,
            },
            _ => None,
        })
        .collect();

    assert_eq!(integers.len(), 4);
    assert_eq!(integers[0].format, IntegerFormat::Decimal);
    assert_eq!(integers[1].format, IntegerFormat::Binary);
    assert_eq!(integers[2].format, IntegerFormat::Octal);
    assert_eq!(integers[3].format, IntegerFormat::Hexadecimal);

    // Verify values
    assert_eq!(integers[0].value, 42);
    assert_eq!(integers[1].value, 10); // 0b1010
    assert_eq!(integers[2].value, 493); // 0o755
    assert_eq!(integers[3].value, 255); // 0xFF
}

#[test]
fn test_zero_in_all_formats() {
    let input = "0 0b0 0o0 0x0";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 4);

    let expected_formats = [
        IntegerFormat::Decimal,
        IntegerFormat::Binary,
        IntegerFormat::Octal,
        IntegerFormat::Hexadecimal,
    ];

    for (i, expected_format) in expected_formats.iter().enumerate() {
        match &result.items[i].kind {
            ItemKind::Expression(expr) => {
                let integer = extract_integer_from_expression(expr);
                assert_eq!(integer.value, 0, "All zeros should have value 0");
                assert_eq!(
                    integer.format, *expected_format,
                    "Format mismatch at position {}",
                    i
                );
            }
            _ => panic!("Expected expression at position {}", i),
        }
    }
}
