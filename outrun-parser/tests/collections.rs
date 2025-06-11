// Collection literal parsing tests
// Tests for lists, maps, and tuples

use outrun_parser::{ast::*, parse_program};

// Helper function to extract collections from expressions
fn extract_list_from_expression(expr: &Expression) -> &ListLiteral {
    match &expr.kind {
        ExpressionKind::List(list) => list,
        _ => panic!("Expected list in expression, got: {:?}", expr.kind),
    }
}

fn extract_map_from_expression(expr: &Expression) -> &MapLiteral {
    match &expr.kind {
        ExpressionKind::Map(map) => map,
        _ => panic!("Expected map in expression, got: {:?}", expr.kind),
    }
}

fn extract_tuple_from_expression(expr: &Expression) -> &TupleLiteral {
    match &expr.kind {
        ExpressionKind::Tuple(tuple) => tuple,
        _ => panic!("Expected tuple in expression, got: {:?}", expr.kind),
    }
}

// === LIST TESTS ===

#[test]
fn test_parse_empty_list() {
    let input = "[]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 0);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_simple_list() {
    let input = "[1, 2, 3]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 3);

            // Check each element
            for (i, expected) in [1, 2, 3].iter().enumerate() {
                match &list.elements[i] {
                    ListElement::Expression(expr) => match &expr.kind {
                        ExpressionKind::Integer(int) => {
                            assert_eq!(int.value, *expected);
                        }
                        _ => panic!("Expected integer in list"),
                    },
                    ListElement::Spread(_) => panic!("Expected expression, not spread"),
                }
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_mixed_type_list() {
    let input = "[42, \"hello\", true, :atom]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 4);

            // Check integer
            match &list.elements[0] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::Integer(int) => assert_eq!(int.value, 42),
                    _ => panic!("Expected integer"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }

            // Check string
            match &list.elements[1] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::String(string) => {
                        assert_eq!(string.parts.len(), 1);
                        match &string.parts[0] {
                            StringPart::Text { content, .. } => assert_eq!(content, "hello"),
                            _ => panic!("Expected text part"),
                        }
                    }
                    _ => panic!("Expected string"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }

            // Check boolean
            match &list.elements[2] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::Boolean(boolean) => assert!(boolean.value),
                    _ => panic!("Expected boolean"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }

            // Check atom
            match &list.elements[3] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::Atom(atom) => assert_eq!(atom.name, "atom"),
                    _ => panic!("Expected atom"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_nested_list() {
    let input = "[[1, 2], [3, 4], []]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 3);

            // Check first nested list [1, 2]
            match &list.elements[0] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::List(nested) => {
                        assert_eq!(nested.elements.len(), 2);
                    }
                    _ => panic!("Expected nested list"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }

            // Check empty nested list []
            match &list.elements[2] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::List(nested) => {
                        assert_eq!(nested.elements.len(), 0);
                    }
                    _ => panic!("Expected empty nested list"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_list_with_trailing_comma() {
    let input = "[1, 2, 3,]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 3);
        }
        _ => panic!("Expected expression"),
    }
}

// === MAP TESTS ===

#[test]
fn test_parse_empty_map() {
    let input = "{}";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let map = extract_map_from_expression(expr);
            assert_eq!(map.entries.len(), 0);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_map_shorthand_syntax() {
    let input = "{name: \"James\", age: 35}";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let map = extract_map_from_expression(expr);
            assert_eq!(map.entries.len(), 2);

            // Check first entry (name: "James")
            match &map.entries[0] {
                MapEntry::Shorthand { name, value } => {
                    assert_eq!(name.name, "name");
                    match &value.kind {
                        ExpressionKind::String(string) => match &string.parts[0] {
                            StringPart::Text { content, .. } => assert_eq!(content, "James"),
                            _ => panic!("Expected text part"),
                        },
                        _ => panic!("Expected string value"),
                    }
                }
                _ => panic!("Expected shorthand entry"),
            }

            // Check second entry (age: 35)
            match &map.entries[1] {
                MapEntry::Shorthand { name, value } => {
                    assert_eq!(name.name, "age");
                    match &value.kind {
                        ExpressionKind::Integer(int) => assert_eq!(int.value, 35),
                        _ => panic!("Expected integer value"),
                    }
                }
                _ => panic!("Expected shorthand entry"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_map_explicit_syntax() {
    let input = "{1 => :one, 2 => :two}";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let map = extract_map_from_expression(expr);
            assert_eq!(map.entries.len(), 2);

            // Check first entry (1 => :one)
            match &map.entries[0] {
                MapEntry::Assignment { key, value } => {
                    match &key.kind {
                        ExpressionKind::Integer(int) => assert_eq!(int.value, 1),
                        _ => panic!("Expected integer key"),
                    }

                    match &value.kind {
                        ExpressionKind::Atom(atom) => assert_eq!(atom.name, "one"),
                        _ => panic!("Expected atom value"),
                    }
                }
                _ => panic!("Expected assignment entry"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_map_with_trailing_comma() {
    let input = "{x: 1, y: 2,}";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let map = extract_map_from_expression(expr);
            assert_eq!(map.entries.len(), 2);
        }
        _ => panic!("Expected expression"),
    }
}

// === TUPLE TESTS ===

#[test]
fn test_parse_empty_tuple() {
    let input = "()";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let tuple = extract_tuple_from_expression(expr);
            assert_eq!(tuple.elements.len(), 0);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_single_element_tuple() {
    let input = "(42,)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let tuple = extract_tuple_from_expression(expr);
            assert_eq!(tuple.elements.len(), 1);

            match &tuple.elements[0].kind {
                ExpressionKind::Integer(int) => assert_eq!(int.value, 42),
                _ => panic!("Expected integer"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_multi_element_tuple() {
    let input = "(\"name\", 35, true)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let tuple = extract_tuple_from_expression(expr);
            assert_eq!(tuple.elements.len(), 3);

            // Check string
            match &tuple.elements[0].kind {
                ExpressionKind::String(string) => match &string.parts[0] {
                    StringPart::Text { content, .. } => assert_eq!(content, "name"),
                    _ => panic!("Expected text part"),
                },
                _ => panic!("Expected string"),
            }

            // Check integer
            match &tuple.elements[1].kind {
                ExpressionKind::Integer(int) => assert_eq!(int.value, 35),
                _ => panic!("Expected integer"),
            }

            // Check boolean
            match &tuple.elements[2].kind {
                ExpressionKind::Boolean(boolean) => assert!(boolean.value),
                _ => panic!("Expected boolean"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_nested_tuple() {
    let input = "((1, 2), (3, 4))";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let tuple = extract_tuple_from_expression(expr);
            assert_eq!(tuple.elements.len(), 2);

            // Check first nested tuple
            match &tuple.elements[0].kind {
                ExpressionKind::Tuple(nested) => {
                    assert_eq!(nested.elements.len(), 2);
                }
                _ => panic!("Expected nested tuple"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_parse_tuple_with_trailing_comma() {
    let input = "(1, 2, 3,)";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let tuple = extract_tuple_from_expression(expr);
            assert_eq!(tuple.elements.len(), 3);
        }
        _ => panic!("Expected expression"),
    }
}

// === DISPLAY TESTS ===

#[test]
fn test_list_display_preserves_format() {
    let input = "[1, 2, 3]";
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

#[test]
fn test_map_shorthand_display_preserves_format() {
    let input = "{name: \"James\", age: 35}";
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

// === SPREAD OPERATOR TESTS ===

#[test]
fn test_list_with_spread_operator_basic() {
    let input = "[first, ..rest]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 2);

            // Check first element is an identifier
            match &list.elements[0] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::Identifier(id) => assert_eq!(id.name, "first"),
                    _ => panic!("Expected identifier"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }

            // Check second element is a spread
            match &list.elements[1] {
                ListElement::Spread(id) => assert_eq!(id.name, "rest"),
                ListElement::Expression(_) => panic!("Expected spread, not expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_list_with_spread_operator_mixed() {
    let input = "[1, 2, ..middle, 3, 4]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 5);

            // Check integer elements
            for (i, expected) in [1, 2].iter().enumerate() {
                match &list.elements[i] {
                    ListElement::Expression(expr) => match &expr.kind {
                        ExpressionKind::Integer(int) => assert_eq!(int.value, *expected),
                        _ => panic!("Expected integer"),
                    },
                    ListElement::Spread(_) => panic!("Expected expression, not spread"),
                }
            }

            // Check spread element
            match &list.elements[2] {
                ListElement::Spread(id) => assert_eq!(id.name, "middle"),
                ListElement::Expression(_) => panic!("Expected spread, not expression"),
            }

            // Check remaining integer elements
            for (i, expected) in [3, 4].iter().enumerate() {
                match &list.elements[i + 3] {
                    ListElement::Expression(expr) => match &expr.kind {
                        ExpressionKind::Integer(int) => assert_eq!(int.value, *expected),
                        _ => panic!("Expected integer"),
                    },
                    ListElement::Spread(_) => panic!("Expected expression, not spread"),
                }
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_list_with_multiple_spreads() {
    let input = "[..start, 42, ..middle, \"hello\", ..end]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 5);

            // Check spread elements
            let expected_spreads = ["start", "middle", "end"];
            let spread_indices = [0, 2, 4];
            
            for (index, expected_name) in spread_indices.iter().zip(expected_spreads.iter()) {
                match &list.elements[*index] {
                    ListElement::Spread(id) => assert_eq!(id.name, *expected_name),
                    ListElement::Expression(_) => panic!("Expected spread at index {}", index),
                }
            }

            // Check literal elements
            match &list.elements[1] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::Integer(int) => assert_eq!(int.value, 42),
                    _ => panic!("Expected integer"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }

            match &list.elements[3] {
                ListElement::Expression(expr) => match &expr.kind {
                    ExpressionKind::String(string) => {
                        assert_eq!(string.parts.len(), 1);
                        match &string.parts[0] {
                            StringPart::Text { content, .. } => assert_eq!(content, "hello"),
                            _ => panic!("Expected text part"),
                        }
                    }
                    _ => panic!("Expected string"),
                },
                ListElement::Spread(_) => panic!("Expected expression, not spread"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_list_with_only_spread() {
    let input = "[..items]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 1);

            match &list.elements[0] {
                ListElement::Spread(id) => assert_eq!(id.name, "items"),
                ListElement::Expression(_) => panic!("Expected spread, not expression"),
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_spread_operator_display() {
    let input = "[first, ..rest, last]";
    let result = parse_program(input).unwrap();

    let reconstructed = format!("{}", result);
    assert!(reconstructed.contains("[first, ..rest, last]"));
}

#[test]
fn test_nested_list_with_spread() {
    let input = "[[1, ..inner], [..outer, 2]]";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 2);

            // Check both elements are lists
            for element in &list.elements {
                match element {
                    ListElement::Expression(expr) => match &expr.kind {
                        ExpressionKind::List(_) => {}, // Expected nested list
                        _ => panic!("Expected nested list"),
                    },
                    ListElement::Spread(_) => panic!("Expected expression, not spread"),
                }
            }
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_map_explicit_display_preserves_format() {
    let input = "{1 => :one, 2 => :two}";
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

#[test]
fn test_tuple_display_preserves_format() {
    let input = "(\"name\", 35, true)";
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

#[test]
fn test_single_element_tuple_display() {
    let input = "(42,)";
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);
    assert_eq!(formatted, input);
}

// === COMPLEX NESTED COLLECTIONS ===

#[test]
fn test_parse_complex_nested_collections() {
    let input = "{users: [(1, true), (2, false)], groups: [], config: {debug: true}}";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let map = extract_map_from_expression(expr);
            assert_eq!(map.entries.len(), 3);

            // Check that we have the right number of entries
            // (detailed validation would be very complex for this nested structure)
            assert!(map.entries.iter().any(|entry| {
                match entry {
                    MapEntry::Shorthand { name, .. } => name.name == "users",
                    _ => false,
                }
            }));

            assert!(map.entries.iter().any(|entry| {
                match entry {
                    MapEntry::Shorthand { name, .. } => name.name == "groups",
                    _ => false,
                }
            }));

            assert!(map.entries.iter().any(|entry| {
                match entry {
                    MapEntry::Shorthand { name, .. } => name.name == "config",
                    _ => false,
                }
            }));
        }
        _ => panic!("Expected expression"),
    }
}

#[test]
fn test_comprehensive_mix_with_collections() {
    let input = "true [1, 2] {x: 42} (\"a\", \"b\") false";
    let result = parse_program(input).unwrap();

    // Should parse: boolean, list, map, tuple, boolean
    assert_eq!(result.items.len(), 5);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(b) => assert!(b.value),
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression"),
    }

    match &result.items[1].kind {
        ItemKind::Expression(expr) => {
            let list = extract_list_from_expression(expr);
            assert_eq!(list.elements.len(), 2);
        }
        _ => panic!("Expected expression"),
    }

    match &result.items[2].kind {
        ItemKind::Expression(expr) => {
            let map = extract_map_from_expression(expr);
            assert_eq!(map.entries.len(), 1);
        }
        _ => panic!("Expected expression"),
    }

    match &result.items[3].kind {
        ItemKind::Expression(expr) => {
            let tuple = extract_tuple_from_expression(expr);
            assert_eq!(tuple.elements.len(), 2);
        }
        _ => panic!("Expected expression"),
    }

    match &result.items[4].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(b) => assert!(!b.value),
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression"),
    }
}
