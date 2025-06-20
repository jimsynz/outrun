use crate::{ast::*, parse_program};

fn assert_identifier_expression(expr: &Expression, expected_name: &str) {
    match &expr.kind {
        ExpressionKind::Identifier(id) => assert_eq!(id.name, expected_name),
        _ => panic!(
            "Expected identifier '{}', got: {:?}",
            expected_name, expr.kind
        ),
    }
}

#[test]
fn test_parse_basic_sigil() {
    let input = "~SQL\"SELECT * FROM users\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::SigilLiteral(sigil) => {
            assert_eq!(sigil.sigil_type.name, "SQL");
            assert_eq!(sigil.string.format, StringFormat::Basic);
            assert_eq!(sigil.string.parts.len(), 1);

            match &sigil.string.parts[0] {
                StringPart::Text {
                    content,
                    raw_content,
                } => {
                    assert_eq!(content, "SELECT * FROM users");
                    assert_eq!(raw_content, "SELECT * FROM users");
                }
                _ => panic!("Expected text part"),
            }
        }
        _ => panic!("Expected sigil literal, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_sigil_with_interpolation() {
    let input = "~SQL\"SELECT * FROM users WHERE id = #{user_id}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::SigilLiteral(sigil) => {
            assert_eq!(sigil.sigil_type.name, "SQL");
            assert_eq!(sigil.string.parts.len(), 2);

            match &sigil.string.parts[0] {
                StringPart::Text { content, .. } => {
                    assert_eq!(content, "SELECT * FROM users WHERE id = ");
                }
                _ => panic!("Expected text part"),
            }

            match &sigil.string.parts[1] {
                StringPart::Interpolation { expression, .. } => {
                    assert_identifier_expression(expression, "user_id");
                }
                _ => panic!("Expected interpolation part"),
            }
        }
        _ => panic!("Expected sigil literal"),
    }
}

#[test]
fn test_parse_multiline_sigil() {
    let input = "~SQL\"\"\"
SELECT users.id, users.name
FROM users
WHERE users.active = true
\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::SigilLiteral(sigil) => {
            assert_eq!(sigil.sigil_type.name, "SQL");
            assert_eq!(sigil.string.format, StringFormat::Multiline);

            let content: String = sigil
                .string
                .parts
                .iter()
                .map(|part| match part {
                    StringPart::Text { content, .. } => content.clone(),
                    StringPart::Interpolation { .. } => panic!("No interpolation expected"),
                })
                .collect();

            assert!(content.contains("SELECT users.id, users.name"));
            assert!(content.contains("FROM users"));
            assert!(content.contains("WHERE users.active = true"));
        }
        _ => panic!("Expected sigil literal"),
    }
}

#[test]
fn test_parse_multiline_sigil_with_interpolation() {
    let input = "~SQL\"\"\"
SELECT * FROM #{table_name}
WHERE status = #{status}
\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::SigilLiteral(sigil) => {
            assert_eq!(sigil.sigil_type.name, "SQL");
            assert_eq!(sigil.string.format, StringFormat::Multiline);

            let interpolation_count = sigil
                .string
                .parts
                .iter()
                .filter(|part| matches!(part, StringPart::Interpolation { .. }))
                .count();
            assert_eq!(interpolation_count, 2);

            let has_table_name = sigil.string.parts.iter().any(|part|
                matches!(part, StringPart::Interpolation { expression, .. } if matches!(expression.kind, ExpressionKind::Identifier(ref id) if id.name == "table_name"))
            );
            let has_status = sigil.string.parts.iter().any(|part|
                matches!(part, StringPart::Interpolation { expression, .. } if matches!(expression.kind, ExpressionKind::Identifier(ref id) if id.name == "status"))
            );

            assert!(has_table_name);
            assert!(has_status);
        }
        _ => panic!("Expected sigil literal"),
    }
}

#[test]
fn test_parse_empty_sigil() {
    let input = "~HTML\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::SigilLiteral(sigil) => {
            assert_eq!(sigil.sigil_type.name, "HTML");
            assert_eq!(sigil.string.parts.len(), 0);
        }
        _ => panic!("Expected sigil literal"),
    }
}

#[test]
fn test_parse_json_sigil() {
    let input = "~JSON\"{ \\\"name\\\": \\\"#{name}\\\", \\\"age\\\": #{age} }\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::SigilLiteral(sigil) => {
            assert_eq!(sigil.sigil_type.name, "JSON");

            let has_name_interpolation = sigil.string.parts.iter().any(|part|
                matches!(part, StringPart::Interpolation { expression, .. } if matches!(expression.kind, ExpressionKind::Identifier(ref id) if id.name == "name"))
            );
            let has_age_interpolation = sigil.string.parts.iter().any(|part|
                matches!(part, StringPart::Interpolation { expression, .. } if matches!(expression.kind, ExpressionKind::Identifier(ref id) if id.name == "age"))
            );

            assert!(has_name_interpolation);
            assert!(has_age_interpolation);
        }
        _ => panic!("Expected sigil literal"),
    }
}

#[test]
fn test_parse_regex_sigil() {
    let input = "~Regex\"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::SigilLiteral(sigil) => {
            assert_eq!(sigil.sigil_type.name, "Regex");
            assert_eq!(sigil.string.parts.len(), 3);

            let full_content: String = sigil
                .string
                .parts
                .iter()
                .map(|part| match part {
                    StringPart::Text { content, .. } => content.clone(),
                    _ => panic!("Expected only text parts"),
                })
                .collect();

            assert!(full_content.contains("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+"));
            assert!(full_content.contains("\\.[a-zA-Z]{2,}"));
        }
        _ => panic!("Expected sigil literal"),
    }
}

#[test]
fn test_sigil_display_preserves_format() {
    let input = "~SQL\"SELECT * FROM users WHERE id = #{user_id}\"";
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

#[test]
fn test_multiline_sigil_display_preserves_format() {
    let input = "~JSON\"\"\"
{
  \"name\": \"#{name}\",
  \"age\": #{age}
}
\"\"\"";
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

#[test]
fn test_comprehensive_mix_with_sigils() {
    let input = "true ~SQL\"SELECT * FROM users\" 42 ~HTML\"<div>#{content}</div>\" false";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 5);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(b) => assert!(b.value),
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression"),
    }

    match &result.items[1].kind {
        ItemKind::SigilLiteral(s) => {
            assert_eq!(s.sigil_type.name, "SQL");
        }
        _ => panic!("Expected sigil literal"),
    }

    match &result.items[2].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(i) => assert_eq!(i.value, 42),
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression"),
    }

    match &result.items[3].kind {
        ItemKind::SigilLiteral(s) => {
            assert_eq!(s.sigil_type.name, "HTML");
        }
        _ => panic!("Expected sigil literal"),
    }

    match &result.items[4].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(b) => assert!(!b.value),
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression"),
    }
}
