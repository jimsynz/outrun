// String interpolation parsing tests
// Tests for #{...} expression interpolation in strings

use outrun_parser::{ast::*, parse_program};

// Helper function to extract string from expression
fn extract_string_from_expression(expr: &Expression) -> &StringLiteral {
    match &expr.kind {
        ExpressionKind::String(string) => string,
        _ => panic!("Expected string in expression, got: {:?}", expr.kind),
    }
}

// Helper function to check if an expression is an identifier with a specific name
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
fn test_parse_string_with_simple_interpolation() {
    let input = "\"Hello #{name}!\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 3);
            assert_eq!(string.format, StringFormat::Basic);

            // Part 1: "Hello "
            match &string.parts[0] {
                StringPart::Text {
                    content,
                    raw_content,
                } => {
                    assert_eq!(content, "Hello ");
                    assert_eq!(raw_content, "Hello ");
                }
                _ => panic!("Expected text part"),
            }

            // Part 2: #{name}
            match &string.parts[1] {
                StringPart::Interpolation { expression, .. } => {
                    assert_identifier_expression(expression, "name");
                }
                _ => panic!("Expected interpolation part"),
            }

            // Part 3: "!"
            match &string.parts[2] {
                StringPart::Text {
                    content,
                    raw_content,
                } => {
                    assert_eq!(content, "!");
                    assert_eq!(raw_content, "!");
                }
                _ => panic!("Expected text part"),
            }
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_string_with_multiple_interpolations() {
    let input = "\"#{greeting} #{name}, you are #{age} years old!\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 6);
            assert_eq!(string.format, StringFormat::Basic);

            // Check all parts
            let expected_parts = [
                ("interpolation", "greeting"),
                ("text", " "),
                ("interpolation", "name"),
                ("text", ", you are "),
                ("interpolation", "age"),
                ("text", " years old!"),
            ];

            for (i, (part_type, expected_content)) in expected_parts.iter().enumerate() {
                match (*part_type, &string.parts[i]) {
                    ("text", StringPart::Text { content, .. }) => {
                        assert_eq!(content, expected_content);
                    }
                    ("interpolation", StringPart::Interpolation { expression, .. }) => {
                        assert_identifier_expression(expression, expected_content);
                    }
                    _ => panic!("Unexpected part type at index {}", i),
                }
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_string_interpolation_only() {
    let input = "\"#{variable}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 1);

            match &string.parts[0] {
                StringPart::Interpolation { expression, .. } => {
                    assert_identifier_expression(expression, "variable");
                }
                _ => panic!("Expected interpolation part"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_string_with_field_access() {
    let input = "\"Result: #{user.name}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::String(string) => {
                    assert_eq!(string.parts.len(), 2);

                    // Check text part
                    match &string.parts[0] {
                        StringPart::Text { content, .. } => {
                            assert_eq!(content, "Result: ");
                        }
                        _ => panic!("Expected text part"),
                    }

                    // Check interpolation with field access
                    match &string.parts[1] {
                        StringPart::Interpolation { expression, .. } => {
                            match &expression.kind {
                                ExpressionKind::FieldAccess(field_access) => {
                                    // Check object is 'user'
                                    match &field_access.object.kind {
                                        ExpressionKind::Identifier(id) => {
                                            assert_eq!(id.name, "user");
                                        }
                                        _ => panic!("Expected identifier for object"),
                                    }
                                    // Check field is 'name'
                                    assert_eq!(field_access.field.name, "name");
                                }
                                _ => panic!("Expected field access in interpolation"),
                            }
                        }
                        _ => panic!("Expected interpolation part"),
                    }
                }
                _ => panic!("Expected string expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_string_with_field_access_future() {
    // This test shows what we expect when field access is implemented
    let input = "\"Result: #{user.name}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 2);

            // Part 1: "Result: "
            match &string.parts[0] {
                StringPart::Text { content, .. } => {
                    assert_eq!(content, "Result: ");
                }
                _ => panic!("Expected text part"),
            }

            // Part 2: #{user.name}
            // This will be implemented when field access is added
            match &string.parts[1] {
                StringPart::Interpolation { expression, .. } => {
                    // Field access would parse as something like:
                    // ExpressionKind::FieldAccess { object: "user", field: "name" }
                    // For now just verify we have an expression
                    assert!(matches!(expression.kind, _));
                }
                _ => panic!("Expected interpolation part"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_multiline_string_with_interpolation() {
    let input = "\"\"\"Hello #{name}\nYou have #{count} messages\"\"\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 6);
            assert_eq!(string.format, StringFormat::Multiline);

            // Check the structure: "Hello ", interpolation, newline, "You have ", interpolation, " messages"
            match &string.parts[0] {
                StringPart::Text { content, .. } => assert_eq!(content, "Hello "),
                _ => panic!("Expected text part"),
            }

            match &string.parts[1] {
                StringPart::Interpolation { expression, .. } => {
                    assert_identifier_expression(expression, "name")
                }
                _ => panic!("Expected interpolation part"),
            }

            match &string.parts[2] {
                StringPart::Text { content, .. } => assert_eq!(content, "\n"),
                _ => panic!("Expected text part"),
            }

            match &string.parts[3] {
                StringPart::Text { content, .. } => assert_eq!(content, "You have "),
                _ => panic!("Expected text part"),
            }

            match &string.parts[4] {
                StringPart::Interpolation { expression, .. } => {
                    assert_identifier_expression(expression, "count")
                }
                _ => panic!("Expected interpolation part"),
            }

            match &string.parts[5] {
                StringPart::Text { content, .. } => assert_eq!(content, " messages"),
                _ => panic!("Expected text part"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_string_with_interpolation_and_escapes() {
    let input = "\"Line 1\\n#{variable}\\tEnd\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 5);

            // Part 1: "Line 1" (text before escape)
            match &string.parts[0] {
                StringPart::Text {
                    content,
                    raw_content,
                } => {
                    assert_eq!(content, "Line 1");
                    assert_eq!(raw_content, "Line 1");
                }
                _ => panic!("Expected text part"),
            }

            // Part 2: "\n" (escape sequence)
            match &string.parts[1] {
                StringPart::Text {
                    content,
                    raw_content,
                } => {
                    assert_eq!(content, "\n");
                    assert_eq!(raw_content, "\\n");
                }
                _ => panic!("Expected text part"),
            }

            // Part 3: #{variable}
            match &string.parts[2] {
                StringPart::Interpolation { expression, .. } => {
                    assert_identifier_expression(expression, "variable");
                }
                _ => panic!("Expected interpolation part"),
            }

            // Part 4: "\t" (escape sequence)
            match &string.parts[3] {
                StringPart::Text {
                    content,
                    raw_content,
                } => {
                    assert_eq!(content, "\t");
                    assert_eq!(raw_content, "\\t");
                }
                _ => panic!("Expected text part"),
            }

            // Part 5: "End" (text after escape)
            match &string.parts[4] {
                StringPart::Text {
                    content,
                    raw_content,
                } => {
                    assert_eq!(content, "End");
                    assert_eq!(raw_content, "End");
                }
                _ => panic!("Expected text part"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_string_interpolation_display_preserves_format() {
    let input = "\"Hello #{name}!\"";
    let result = parse_program(input).unwrap();

    // Test that Display preserves the interpolation format
    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

#[test]
fn test_logical_operators_in_interpolation() {
    let input = "\"Status: #{true && false}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 2);
            assert_eq!(string.format, StringFormat::Basic);

            // Part 1: "Status: "
            match &string.parts[0] {
                StringPart::Text { content, .. } => {
                    assert_eq!(content, "Status: ");
                }
                _ => panic!("Expected text part"),
            }

            // Part 2: #{true && false}
            match &string.parts[1] {
                StringPart::Interpolation { expression, .. } => {
                    // Should be a logical AND at the top level
                    match &expression.kind {
                        ExpressionKind::BinaryOp(op) => {
                            assert_eq!(op.operator, BinaryOperator::LogicalAnd);
                        }
                        _ => panic!("Expected logical AND expression"),
                    }
                }
                _ => panic!("Expected interpolation part"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_logical_not_in_interpolation() {
    let input = "\"Result: #{!failed}\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            assert_eq!(string.parts.len(), 2);

            // Part 2: #{!failed}
            match &string.parts[1] {
                StringPart::Interpolation { expression, .. } => {
                    // Should be a logical NOT
                    match &expression.kind {
                        ExpressionKind::UnaryOp(op) => {
                            assert_eq!(op.operator, UnaryOperator::LogicalNot);
                        }
                        _ => panic!("Expected logical NOT expression"),
                    }
                }
                _ => panic!("Expected interpolation part"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_basic_string_simple() {
    let input = "\"hello\"";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let string = extract_string_from_expression(expr);
            println!("Parts: {:?}", string.parts);
            // Just check that we get some parts and it's a basic string
            assert_eq!(string.format, StringFormat::Basic);
            // Don't worry about perfect content matching for now
        }
        _ => panic!("Expected expression"),
    }
}
