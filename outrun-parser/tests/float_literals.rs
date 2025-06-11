// Tests for float literals (standard and scientific notation)

use outrun_parser::*;

// Helper function to extract float from expression
fn extract_float_from_expression(expr: &Expression) -> &FloatLiteral {
    match &expr.kind {
        ExpressionKind::Float(float) => float,
        _ => panic!("Expected float in expression, got: {:?}", expr.kind),
    }
}

#[test]
fn test_parse_standard_float() {
    let input = "3.15";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 3.15);
            assert_eq!(float.format, FloatFormat::Standard);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_zero_float() {
    let input = "0.0";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 0.0);
            assert_eq!(float.format, FloatFormat::Standard);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_large_float() {
    let input = "123.456789";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 123.456789);
            assert_eq!(float.format, FloatFormat::Standard);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_scientific_lowercase_e() {
    let input = "1.23e-4";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 1.23e-4);
            assert_eq!(
                float.format,
                FloatFormat::Scientific {
                    exponent_case: ExponentCase::Lowercase
                }
            );
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_scientific_uppercase_e() {
    let input = "1.23E-4";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 1.23e-4);
            assert_eq!(
                float.format,
                FloatFormat::Scientific {
                    exponent_case: ExponentCase::Uppercase
                }
            );
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_scientific_positive_exponent() {
    let input = "6.022e23";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 6.022e23);
            assert_eq!(
                float.format,
                FloatFormat::Scientific {
                    exponent_case: ExponentCase::Lowercase
                }
            );
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_scientific_explicit_positive_exponent() {
    let input = "6.022e+23";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 6.022e23);
            assert_eq!(
                float.format,
                FloatFormat::Scientific {
                    exponent_case: ExponentCase::Lowercase
                }
            );
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_mixed_float_formats() {
    let input = "3.15 1.23e-4 42.0 6.022E23";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 4);

    // Expected values and formats
    let expected = [
        (3.15, FloatFormat::Standard),
        (
            1.23e-4,
            FloatFormat::Scientific {
                exponent_case: ExponentCase::Lowercase,
            },
        ),
        (42.0, FloatFormat::Standard),
        (
            6.022e23,
            FloatFormat::Scientific {
                exponent_case: ExponentCase::Uppercase,
            },
        ),
    ];

    for (i, (expected_value, expected_format)) in expected.iter().enumerate() {
        match &result.items[i].kind {
            ItemKind::Expression(expr) => {
                let float = extract_float_from_expression(expr);
                assert!(
                    (float.value - expected_value).abs() < f64::EPSILON,
                    "Value mismatch at position {}: expected {}, got {}",
                    i,
                    expected_value,
                    float.value
                );
                assert_eq!(
                    float.format, *expected_format,
                    "Format mismatch at position {}",
                    i
                );
            }
            _ => panic!("Expected expression at position {}", i),
        }
    }
}

#[test]
fn test_float_vs_integer_precedence() {
    // Test that floats are parsed correctly when mixed with integers
    let input = "42 3.15 123 1.5e2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 4);

    // Should be: integer, float, integer, float
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(int) => {
                assert_eq!(int.value, 42);
            }
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression at position 0"),
    }

    match &result.items[1].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 3.15);
        }
        _ => panic!("Expected expression at position 1"),
    }

    match &result.items[2].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(int) => {
                assert_eq!(int.value, 123);
            }
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression at position 2"),
    }

    match &result.items[3].kind {
        ItemKind::Expression(expr) => {
            let float = extract_float_from_expression(expr);
            assert_eq!(float.value, 150.0); // 1.5e2 = 150
        }
        _ => panic!("Expected expression at position 3"),
    }
}

#[test]
fn test_comprehensive_mix_with_floats() {
    let input = "struct true 3.15 my_var 0xFF false 1.23e-4 MyType";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 8);

    // Extract just the floats to verify their formats and values
    let floats: Vec<_> = result
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Float(float) => Some(float),
                _ => None,
            },
            _ => None,
        })
        .collect();

    assert_eq!(floats.len(), 2);

    // First float: 3.15 (standard)
    assert_eq!(floats[0].value, 3.15);
    assert_eq!(floats[0].format, FloatFormat::Standard);

    // Second float: 1.23e-4 (scientific)
    assert_eq!(floats[1].value, 1.23e-4);
    assert_eq!(
        floats[1].format,
        FloatFormat::Scientific {
            exponent_case: ExponentCase::Lowercase
        }
    );
}

#[test]
fn test_float_edge_cases() {
    let test_cases = [
        ("0.1", 0.1),
        ("0.001", 0.001),
        ("999.999", 999.999),
        ("1.0e0", 1.0),
        ("1.0E-0", 1.0),
        ("2.5e1", 25.0),
    ];

    for (input, expected_value) in &test_cases {
        let result = parse_program(input).unwrap();

        assert_eq!(result.items.len(), 1);
        match &result.items[0].kind {
            ItemKind::Expression(expr) => {
                let float = extract_float_from_expression(expr);
                assert!(
                    (float.value - expected_value).abs() < f64::EPSILON,
                    "Value mismatch for input '{}': expected {}, got {}",
                    input,
                    expected_value,
                    float.value
                );
            }
            _ => panic!("Expected expression for input '{}'", input),
        }
    }
}
