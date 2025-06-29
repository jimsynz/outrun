use crate::{ast::*, parse_program};

fn extract_string_from_expression(expr: &Expression) -> &StringLiteral {
    match &expr.kind {
        ExpressionKind::String(string) => string,
        _ => panic!("Expected string in expression, got: {:?}", expr.kind),
    }
}

fn extract_text_content(parts: &[StringPart]) -> String {
    parts
        .iter()
        .map(|part| match part {
            StringPart::Text { content, .. } => content.clone(),
            StringPart::Interpolation { .. } => panic!("Expected text part, found interpolation"),
        })
        .collect()
}

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
fn test_parse_empty_multiline_string() {
    let input = "\"\"\"\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "");
            assert_eq!(extract_raw_content(&string.parts), "");
            assert_eq!(string.format, StringFormat::Multiline);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_simple_multiline_string() {
    let input = "\"\"\"Hello, World!\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "Hello, World!");
            assert_eq!(extract_raw_content(&string.parts), "Hello, World!");
            assert_eq!(string.format, StringFormat::Multiline);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_multiline_string_with_newlines() {
    let input = "\"\"\"Line 1\nLine 2\nLine 3\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(
                extract_text_content(&string.parts),
                "Line 1\nLine 2\nLine 3"
            );
            assert_eq!(extract_raw_content(&string.parts), "Line 1\nLine 2\nLine 3");
            assert_eq!(string.format, StringFormat::Multiline);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_multiline_string_with_escape_sequences() {
    let input = "\"\"\"Line 1\\nTab:\\tQuote:\\\"\"\"\"";
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
                "Line 1\\nTab:\\tQuote:\\\""
            );
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_multiline_string_with_single_quotes() {
    let input = "\"\"\"He said \"Hello\" to me\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(
                extract_text_content(&string.parts),
                "He said \"Hello\" to me"
            );
            assert_eq!(
                extract_raw_content(&string.parts),
                "He said \"Hello\" to me"
            );
            assert_eq!(string.format, StringFormat::Multiline);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_multiline_string_with_unicode() {
    let input = "\"\"\"Hello \\u0041\\u0042\\u0043\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "Hello ABC");
            assert_eq!(
                extract_raw_content(&string.parts),
                "Hello \\u0041\\u0042\\u0043"
            );
            assert_eq!(string.format, StringFormat::Multiline);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_multiline_string_indented() {
    let input = "\"\"\"  SELECT *\n  FROM users\n  WHERE name = 'John'\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(
                extract_text_content(&string.parts),
                "  SELECT *\n  FROM users\n  WHERE name = 'John'"
            );
            assert_eq!(
                extract_raw_content(&string.parts),
                "  SELECT *\n  FROM users\n  WHERE name = 'John'"
            );
            assert_eq!(string.format, StringFormat::Multiline);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_multiline_vs_basic_string_precedence() {
    let input1 = "\"\"\"Hello\"\"\"";
    let result1 = parse_program(input1).unwrap();

    assert_eq!(result1.items.len(), 1);
    match &result1.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "Hello");
            assert_eq!(string.format, StringFormat::Multiline);
        }
        _ => panic!("Expected expression"),
    }

    let input2 = "\"Hello\"";
    let result2 = parse_program(input2).unwrap();

    assert_eq!(result2.items.len(), 1);
    match &result2.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&string.parts), "Hello");
            assert_eq!(string.format, StringFormat::Basic);
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_multiline_string_display_preserves_format() {
    let input = "\"\"\"Hello\nWorld\"\"\"";
    let result = parse_program(input).unwrap();

    let formatted = format!("{result}");
    assert_eq!(formatted, input);
}

#[test]
fn test_comprehensive_mix_with_multiline_strings() {
    let input = "true \"\"\"multiline\nstring\"\"\" 42 \"basic\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 4);

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
            assert_eq!(extract_text_content(&s.parts), "multiline\nstring");
            assert_eq!(s.format, StringFormat::Multiline);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[1].kind),
    }

    match &result.items[2].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(i) => assert_eq!(i.value, 42),
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression, got: {:?}", result.items[2].kind),
    }

    match &result.items[3].kind {
        ItemKind::Expression(expr) => {
            let s = extract_string_from_expression(expr);
            assert_eq!(extract_text_content(&s.parts), "basic");
            assert_eq!(s.format, StringFormat::Basic);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[3].kind),
    }
}
