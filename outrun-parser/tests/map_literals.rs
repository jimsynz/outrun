// Tests for map literal parsing with spread operators
use outrun_parser::{parse_program, ast::*};

#[test]
fn test_map_literal_basic() {
    let input = "{ name: \"John\", age: 30 }";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::Map(map_lit) => {
                    assert_eq!(map_lit.entries.len(), 2);

                    // Check first entry (shorthand)
                    match &map_lit.entries[0] {
                        MapEntry::Shorthand { name, value } => {
                            assert_eq!(name.name, "name");
                            match &value.kind {
                                ExpressionKind::String(_) => {}, // Expected string
                                _ => panic!("Expected string"),
                            }
                        }
                        _ => panic!("Expected shorthand entry"),
                    }

                    // Check second entry (shorthand)
                    match &map_lit.entries[1] {
                        MapEntry::Shorthand { name, value } => {
                            assert_eq!(name.name, "age");
                            match &value.kind {
                                ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 30),
                                _ => panic!("Expected integer"),
                            }
                        }
                        _ => panic!("Expected shorthand entry"),
                    }
                }
                _ => panic!("Expected map literal"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_map_literal_explicit() {
    let input = "{ 1 => \"one\", 2 => \"two\" }";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::Map(map_lit) => {
                    assert_eq!(map_lit.entries.len(), 2);

                    // Check first entry (explicit)
                    match &map_lit.entries[0] {
                        MapEntry::Assignment { key, value } => {
                            match &key.kind {
                                ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 1),
                                _ => panic!("Expected integer key"),
                            }
                            match &value.kind {
                                ExpressionKind::String(_) => {}, // Expected string
                                _ => panic!("Expected string value"),
                            }
                        }
                        _ => panic!("Expected assignment entry"),
                    }

                    // Check second entry (explicit)
                    match &map_lit.entries[1] {
                        MapEntry::Assignment { key, value } => {
                            match &key.kind {
                                ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 2),
                                _ => panic!("Expected integer key"),
                            }
                            match &value.kind {
                                ExpressionKind::String(_) => {}, // Expected string
                                _ => panic!("Expected string value"),
                            }
                        }
                        _ => panic!("Expected assignment entry"),
                    }
                }
                _ => panic!("Expected map literal"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_map_literal_with_spread() {
    let input = "{ name: \"Jane\", ..defaults }";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::Map(map_lit) => {
                    assert_eq!(map_lit.entries.len(), 2);

                    // Check shorthand entry
                    match &map_lit.entries[0] {
                        MapEntry::Shorthand { name, value } => {
                            assert_eq!(name.name, "name");
                            match &value.kind {
                                ExpressionKind::String(_) => {}, // Expected string
                                _ => panic!("Expected string"),
                            }
                        }
                        _ => panic!("Expected shorthand entry"),
                    }

                    // Check spread entry
                    match &map_lit.entries[1] {
                        MapEntry::Spread(name) => {
                            assert_eq!(name.name, "defaults");
                        }
                        _ => panic!("Expected spread entry"),
                    }
                }
                _ => panic!("Expected map literal"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_map_literal_mixed() {
    let input = "{ 1 => \"explicit\", name: \"shorthand\", ..spread_map }";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::Map(map_lit) => {
                    assert_eq!(map_lit.entries.len(), 3);

                    // Check explicit entry
                    match &map_lit.entries[0] {
                        MapEntry::Assignment { key, value: _ } => {
                            match &key.kind {
                                ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 1),
                                _ => panic!("Expected integer key"),
                            }
                        }
                        _ => panic!("Expected assignment entry"),
                    }

                    // Check shorthand entry
                    match &map_lit.entries[1] {
                        MapEntry::Shorthand { name, .. } => {
                            assert_eq!(name.name, "name");
                        }
                        _ => panic!("Expected shorthand entry"),
                    }

                    // Check spread entry
                    match &map_lit.entries[2] {
                        MapEntry::Spread(name) => {
                            assert_eq!(name.name, "spread_map");
                        }
                        _ => panic!("Expected spread entry"),
                    }
                }
                _ => panic!("Expected map literal"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_map_literal_display() {
    let input = "{ name: \"John\", age: 30, ..defaults }";
    let result = parse_program(input).unwrap();

    let reconstructed = format!("{}", result);
    // The display format omits spaces around the braces
    assert!(reconstructed.contains("{name: \"John\", age: 30, ..defaults}"));
}