use crate::*;

#[test]
fn test_parse_single_keyword() {
    let input = "struct";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Keyword(keyword) => {
            assert_eq!(keyword.kind, KeywordKind::Struct);
        }
        _ => panic!("Expected keyword item"),
    }
}

#[test]
fn test_parse_multiple_keywords() {
    let input = "struct trait impl def";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 4);

    let expected = [
        KeywordKind::Struct,
        KeywordKind::Trait,
        KeywordKind::Impl,
        KeywordKind::Def,
    ];

    for (i, item) in result.items.iter().enumerate() {
        match &item.kind {
            ItemKind::Keyword(keyword) => {
                assert_eq!(keyword.kind, expected[i]);
            }
            _ => panic!("Expected keyword item at position {i}"),
        }
    }
}

#[test]
fn test_parse_boolean_true() {
    let input = "true";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(boolean) => {
                assert!(boolean.value);
            }
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_boolean_false() {
    let input = "false";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(boolean) => {
                assert!(!boolean.value);
            }
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_mixed_keywords_and_booleans() {
    let input = "def true struct false";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 4);

    match &result.items[0].kind {
        ItemKind::Keyword(keyword) => {
            assert_eq!(keyword.kind, KeywordKind::Def);
        }
        _ => panic!("Expected keyword at position 0"),
    }

    match &result.items[1].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(boolean) => {
                assert!(boolean.value);
            }
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression at position 1"),
    }

    match &result.items[2].kind {
        ItemKind::Keyword(keyword) => {
            assert_eq!(keyword.kind, KeywordKind::Struct);
        }
        _ => panic!("Expected keyword at position 2"),
    }

    match &result.items[3].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(boolean) => {
                assert!(!boolean.value);
            }
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression at position 3"),
    }
}

#[test]
fn test_parse_comment_with_code() {
    let input = "# This is a comment\nlet x = 42";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    if let ItemKind::LetBinding(let_binding) = &result.items[0].kind {
        if let Pattern::Identifier(id) = &let_binding.pattern {
            assert_eq!(id.name, "x");
        } else {
            panic!("Expected identifier pattern");
        }
    } else {
        panic!("Expected let binding");
    }
}

#[test]
fn test_parse_block_comment() {
    let input = "###Block comment content###";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 0); // no items (comment now in debug_info)

    assert_eq!(result.debug_info.comments.len(), 1);
    assert_eq!(result.debug_info.comments[0].kind, CommentKind::Block);
    assert!(result.debug_info.comments[0]
        .content
        .contains("Block comment content"));
}

#[test]
fn test_empty_program() {
    let input = "";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 0);
}

#[test]
fn test_whitespace_handling() {
    let input = "  struct   true  ";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 2);

    match &result.items[0].kind {
        ItemKind::Keyword(keyword) => {
            assert_eq!(keyword.kind, KeywordKind::Struct);
        }
        _ => panic!("Expected keyword"),
    }

    match &result.items[1].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(boolean) => {
                assert!(boolean.value);
            }
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_simple_identifier() {
    let input = "my_variable";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Identifier(identifier) => {
                assert_eq!(identifier.name, "my_variable");
            }
            _ => panic!("Expected identifier in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_type_identifier() {
    let input = "MyStruct";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::TypeIdentifier(type_identifier) => {
                assert_eq!(type_identifier.name, "MyStruct");
            }
            _ => panic!("Expected type identifier in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_decimal_integer() {
    let input = "42";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(integer) => {
                assert_eq!(integer.value, 42);
                assert_eq!(integer.format, IntegerFormat::Decimal);
            }
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_large_integer() {
    let input = "123456789";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(integer) => {
                assert_eq!(integer.value, 123456789);
            }
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_mixed_identifiers_and_integers() {
    let input = "my_var 42 MyType 123";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 4);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Identifier(identifier) => {
                assert_eq!(identifier.name, "my_var");
            }
            _ => panic!("Expected identifier in expression"),
        },
        _ => panic!("Expected expression at position 0"),
    }

    match &result.items[1].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(integer) => {
                assert_eq!(integer.value, 42);
            }
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression at position 1"),
    }

    match &result.items[2].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::TypeIdentifier(type_identifier) => {
                assert_eq!(type_identifier.name, "MyType");
            }
            _ => panic!("Expected type identifier in expression"),
        },
        _ => panic!("Expected expression at position 2"),
    }

    match &result.items[3].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(integer) => {
                assert_eq!(integer.value, 123);
            }
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression at position 3"),
    }
}

#[test]
fn test_identifier_with_underscores() {
    let input = "_private_var some_func my_type_2";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 3);

    let expected_names = ["_private_var", "some_func", "my_type_2"];
    for (i, item) in result.items.iter().enumerate() {
        match &item.kind {
            ItemKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Identifier(identifier) => {
                    assert_eq!(identifier.name, expected_names[i]);
                }
                _ => panic!("Expected identifier in expression"),
            },
            _ => panic!("Expected expression at position {i}"),
        }
    }
}

#[test]
fn test_comprehensive_mix() {
    let input = "struct User {} true my_func 42 false MyTrait 123";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 7);

    let expected = [
        ("User", "struct_definition"),
        ("true", "boolean"),
        ("my_func", "identifier"),
        ("42", "integer"),
        ("false", "boolean"),
        ("MyTrait", "type_identifier"),
        ("123", "integer"),
    ];

    for (i, (expected_value, expected_type)) in expected.iter().enumerate() {
        match (&result.items[i].kind, *expected_type) {
            (ItemKind::StructDefinition(struct_def), "struct_definition") => {
                assert_eq!(struct_def.name[0].name, *expected_value);
            }
            (ItemKind::Keyword(kw), "keyword") => {
                assert_eq!(format!("{kw}"), *expected_value);
            }
            (ItemKind::Expression(expr), "type_identifier") => match &expr.kind {
                ExpressionKind::TypeIdentifier(type_identifier) => {
                    assert_eq!(type_identifier.name, *expected_value);
                }
                _ => panic!("Expected type identifier in expression at position {i}"),
            },
            (ItemKind::Expression(expr), "boolean") => match &expr.kind {
                ExpressionKind::Boolean(boolean) => {
                    let expected_bool = *expected_value == "true";
                    assert_eq!(boolean.value, expected_bool);
                }
                _ => panic!("Expected boolean in expression at position {i}"),
            },
            (ItemKind::Expression(expr), "identifier") => match &expr.kind {
                ExpressionKind::Identifier(identifier) => {
                    assert_eq!(identifier.name, *expected_value);
                }
                _ => panic!("Expected identifier in expression at position {i}"),
            },
            (ItemKind::Expression(expr), "integer") => match &expr.kind {
                ExpressionKind::Integer(integer) => {
                    let expected_int: i64 = expected_value.parse().unwrap();
                    assert_eq!(integer.value, expected_int);
                }
                _ => panic!("Expected integer in expression at position {i}"),
            },
            _ => panic!("Unexpected item type at position {i}: expected {expected_type}"),
        }
    }
}
