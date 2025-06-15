// Tests for struct literal parsing
use crate::{ast::*, parse_program};

#[test]
fn test_struct_literal_basic() {
    let input = "User { name: \"John\", age: 30 }";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::Struct(struct_lit) => {
                    assert_eq!(struct_lit.type_path.len(), 1);
                    assert_eq!(struct_lit.type_path[0].name, "User");
                    assert_eq!(struct_lit.fields.len(), 2);

                    // Check first field
                    match &struct_lit.fields[0] {
                        StructLiteralField::Assignment { name, value } => {
                            assert_eq!(name.name, "name");
                            match &value.kind {
                                ExpressionKind::String(string_lit) => {
                                    assert_eq!(string_lit.parts.len(), 1);
                                    match &string_lit.parts[0] {
                                        StringPart::Text { content, .. } => {
                                            assert_eq!(content, "John")
                                        }
                                        _ => panic!("Expected text part"),
                                    }
                                }
                                _ => panic!("Expected string"),
                            }
                        }
                        _ => panic!("Expected assignment field"),
                    }

                    // Check second field
                    match &struct_lit.fields[1] {
                        StructLiteralField::Assignment { name, value } => {
                            assert_eq!(name.name, "age");
                            match &value.kind {
                                ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 30),
                                _ => panic!("Expected integer"),
                            }
                        }
                        _ => panic!("Expected assignment field"),
                    }
                }
                _ => panic!("Expected struct literal"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_struct_literal_with_shorthand() {
    let input = "User { name, age: 25 }";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::Struct(struct_lit) => {
                    assert_eq!(struct_lit.type_path.len(), 1);
                    assert_eq!(struct_lit.type_path[0].name, "User");
                    assert_eq!(struct_lit.fields.len(), 2);

                    // Check shorthand field
                    match &struct_lit.fields[0] {
                        StructLiteralField::Shorthand(name) => {
                            assert_eq!(name.name, "name");
                        }
                        _ => panic!("Expected shorthand field"),
                    }

                    // Check assignment field
                    match &struct_lit.fields[1] {
                        StructLiteralField::Assignment { name, value } => {
                            assert_eq!(name.name, "age");
                            match &value.kind {
                                ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 25),
                                _ => panic!("Expected integer"),
                            }
                        }
                        _ => panic!("Expected assignment field"),
                    }
                }
                _ => panic!("Expected struct literal"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_struct_literal_with_spread() {
    let input = "User { name: \"Jane\", ..existing_user }";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            match &expr.kind {
                ExpressionKind::Struct(struct_lit) => {
                    assert_eq!(struct_lit.type_path.len(), 1);
                    assert_eq!(struct_lit.type_path[0].name, "User");
                    assert_eq!(struct_lit.fields.len(), 2);

                    // Check assignment field
                    match &struct_lit.fields[0] {
                        StructLiteralField::Assignment { name, value } => {
                            assert_eq!(name.name, "name");
                            match &value.kind {
                                ExpressionKind::String(_) => {} // Expected string
                                _ => panic!("Expected string"),
                            }
                        }
                        _ => panic!("Expected assignment field"),
                    }

                    // Check spread field
                    match &struct_lit.fields[1] {
                        StructLiteralField::Spread(name) => {
                            assert_eq!(name.name, "existing_user");
                        }
                        _ => panic!("Expected spread field"),
                    }
                }
                _ => panic!("Expected struct literal"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_struct_literal_display() {
    let input = "User { name: \"John\", age: 30, ..defaults }";
    let result = parse_program(input).unwrap();

    let reconstructed = format!("{}", result);
    // The display format omits spaces around the braces
    assert!(reconstructed.contains("User {name: \"John\", age: 30, ..defaults}"));
}
