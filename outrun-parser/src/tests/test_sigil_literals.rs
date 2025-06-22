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
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(sigil) => {
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
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_sigil_with_interpolation() {
    let input = "~SQL\"SELECT * FROM users WHERE id = #{user_id}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(sigil) => {
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
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[0].kind),
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
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(sigil) => {
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
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[0].kind),
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
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(sigil) => {
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
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_empty_sigil() {
    let input = "~HTML\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(sigil) => {
                assert_eq!(sigil.sigil_type.name, "HTML");
                assert_eq!(sigil.string.parts.len(), 0);
            }
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_json_sigil() {
    let input = "~JSON\"{ \\\"name\\\": \\\"#{name}\\\", \\\"age\\\": #{age} }\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(sigil) => {
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
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_regex_sigil() {
    let input = "~Regex\"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(sigil) => {
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
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[0].kind),
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
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(s) => {
                assert_eq!(s.sigil_type.name, "SQL");
            }
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[1].kind),
    }

    match &result.items[2].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(i) => assert_eq!(i.value, 42),
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression"),
    }

    match &result.items[3].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Sigil(s) => {
                assert_eq!(s.sigil_type.name, "HTML");
            }
            _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
        },
        _ => panic!("Expected expression item, got: {:?}", result.items[3].kind),
    }

    match &result.items[4].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(b) => assert!(!b.value),
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_sigil_with_complex_interpolation() {
    let input = r#"~HTML"<div class='#{user.role}'>Hello #{user.name.upcase()}! You have #{messages.length()} new messages.</div>""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::Sigil(sigil) => {
                    assert_eq!(sigil.sigil_type.name, "HTML");
                    assert_eq!(sigil.string.format, StringFormat::Basic);

                    // Count interpolations - should have 3: #{user.role}, #{user.name.upcase()}, #{messages.length()}
                    let interpolation_count = sigil
                        .string
                        .parts
                        .iter()
                        .filter(|part| matches!(part, StringPart::Interpolation { .. }))
                        .count();
                    assert_eq!(interpolation_count, 3);

                    // Check that we have the expected text parts and interpolations
                    let text_parts: Vec<&str> = sigil
                        .string
                        .parts
                        .iter()
                        .filter_map(|part| match part {
                            StringPart::Text { content, .. } => Some(content.as_str()),
                            _ => None,
                        })
                        .collect();

                    // Should have 4 text parts between and around the 3 interpolations
                    assert_eq!(text_parts.len(), 4);
                    assert_eq!(text_parts[0], "<div class='");
                    assert_eq!(text_parts[1], "'>Hello ");
                    assert_eq!(text_parts[2], "! You have ");
                    assert_eq!(text_parts[3], " new messages.</div>");

                    // Verify that interpolations parse correctly by checking they exist
                    let interpolation_parts: Vec<_> = sigil
                        .string
                        .parts
                        .iter()
                        .filter_map(|part| match part {
                            StringPart::Interpolation { expression, .. } => Some(expression),
                            _ => None,
                        })
                        .collect();

                    assert_eq!(interpolation_parts.len(), 3);

                    // First interpolation should be field access: user.role
                    match &interpolation_parts[0].kind {
                        ExpressionKind::FieldAccess(_) => {}
                        _ => panic!("Expected field access for user.role interpolation"),
                    }

                    // Second interpolation should be function call: user.name.upcase()
                    match &interpolation_parts[1].kind {
                        ExpressionKind::FunctionCall(_) => {}
                        _ => panic!("Expected function call for user.name.upcase() interpolation"),
                    }

                    // Third interpolation should be function call: messages.length()
                    match &interpolation_parts[2].kind {
                        ExpressionKind::FunctionCall(_) => {}
                        _ => panic!("Expected function call for messages.length() interpolation"),
                    }
                }
                _ => panic!("Expected sigil expression, got: {:?}", expr.kind),
            }
        }
        _ => panic!("Expected expression item, got: {:?}", result.items[0].kind),
    }
}
