// String literal parsing tests
// Tests for basic string literals with escape sequences

use crate::{ast::*, parse_program};

// Helper function to extract string from expression
fn extract_string_from_expression(expr: &Expression) -> &StringLiteral {
    match &expr.kind {
        ExpressionKind::String(string) => string,
        _ => panic!("Expected string in expression, got: {:?}", expr.kind),
    }
}

// Helper function to extract text content from string parts
fn extract_text_content(parts: &[StringPart]) -> String {
    parts
        .iter()
        .map(|part| match part {
            StringPart::Text { content, .. } => content.clone(),
            StringPart::Interpolation { .. } => panic!("Expected text part, found interpolation"),
        })
        .collect()
}

// Helper function to extract raw content from string parts
fn extract_raw_content(parts: &[StringPart]) -> String {
    parts
        .iter()
        .map(|part| match part {
            StringPart::Text { raw_content, .. } => raw_content.clone(),
            StringPart::Interpolation { .. } => panic!("Expected text part, found interpolation"),
        })
        .collect()
}

#[test]
fn test_parse_empty_string() {
    let input = r#""""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 0);
            assert_eq!(string.format, StringFormat::Basic);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_simple_string() {
    let input = r#""Hello, World!""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "Hello, World!");
            assert_eq!(extract_raw_content(&string.parts), "Hello, World!");
            assert_eq!(string.format, StringFormat::Basic);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_string_with_newline_escape() {
    let input = r#""Hello\nWorld""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "Hello\nWorld");
            assert_eq!(extract_raw_content(&string.parts), r"Hello\nWorld");
            assert_eq!(string.format, StringFormat::Basic);
        }
        _ => panic!("Expected string literal, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_string_with_tab_escape() {
    let input = r#""Hello\tWorld""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "Hello\tWorld");
            assert_eq!(extract_raw_content(&string.parts), r"Hello\tWorld");
        }
        _ => panic!("Expected string literal, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_string_with_backslash_escape() {
    let input = r#""Path\\to\\file""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), r"Path\to\file");
            assert_eq!(extract_raw_content(&string.parts), r"Path\\to\\file");
        }
        _ => panic!("Expected string literal, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_string_with_quote_escape() {
    let input = r#""She said \"Hello\"""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), r#"She said "Hello""#);
            assert_eq!(extract_raw_content(&string.parts), r#"She said \"Hello\""#);
        }
        _ => panic!("Expected string literal, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_string_with_unicode_escape() {
    let input = r#""Hello \u0041\u0042\u0043""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "Hello ABC");
            assert_eq!(
                extract_raw_content(&string.parts),
                r"Hello \u0041\u0042\u0043"
            );
        }
        _ => panic!("Expected string literal, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_string_with_basic_escapes() {
    let input = r#""Line 1\nTab:\tQuote:\"""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(
                extract_text_content(&string.parts),
                "Line 1\nTab:\tQuote:\""
            );
            assert_eq!(
                extract_raw_content(&string.parts),
                r#"Line 1\nTab:\tQuote:\""#
            );
        }
        _ => panic!("Expected string literal, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_string_display_preserves_raw_format() {
    let input = r#""Hello\nWorld""#;
    let result = parse_program(input).unwrap();

    // Test that Display preserves the raw content format
    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

#[test]
fn test_comprehensive_mix_with_strings() {
    let input = r#"true "hello" 42 false "world\n""#;
    let result = parse_program(input).unwrap();

    // Should parse: boolean, string, integer, boolean, string
    assert_eq!(result.items.len(), 5);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(b) => assert!(b.value),
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }

    match &result.items[1].kind {
        ItemKind::Expression(expr) => {
            let s = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&s.parts), "hello");
            assert_eq!(extract_raw_content(&s.parts), "hello");
        }
        _ => panic!("Expected string literal, got: {:?}", result.items[1].kind),
    }

    match &result.items[2].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(i) => assert_eq!(i.value, 42),
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression, got: {:?}", result.items[2].kind),
    }

    match &result.items[3].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(b) => assert!(!b.value),
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression, got: {:?}", result.items[3].kind),
    }

    match &result.items[4].kind {
        ItemKind::Expression(expr) => {
            let s = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&s.parts), "world\n");
            assert_eq!(extract_raw_content(&s.parts), "world\\n");
        }
        _ => panic!("Expected expression, got: {:?}", result.items[4].kind),
    }
}
