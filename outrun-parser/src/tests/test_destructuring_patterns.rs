// Tests for destructuring patterns in let bindings

use crate::ast::{ExpressionKind, ItemKind, Literal, Pattern};
use crate::parse_program;

// === TUPLE DESTRUCTURING ===

#[test]
fn test_tuple_destructuring_basic() {
    let input = "let (x, y) = (1, 2)";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Tuple(tuple_pattern) => {
                    assert_eq!(tuple_pattern.elements.len(), 2);
                    match &tuple_pattern.elements[0] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "x"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &tuple_pattern.elements[1] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "y"),
                        _ => panic!("Expected identifier pattern"),
                    }
                }
                _ => panic!("Expected tuple pattern"),
            }
            assert!(let_binding.type_annotation.is_none());
        }
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_tuple_destructuring_three_elements() {
    let input = "let (x, y, z) = (1, 2, 3)";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::Tuple(tuple_pattern) => {
                assert_eq!(tuple_pattern.elements.len(), 3);
                match &tuple_pattern.elements[0] {
                    Pattern::Identifier(id) => assert_eq!(id.name, "x"),
                    _ => panic!("Expected identifier pattern"),
                }
                match &tuple_pattern.elements[1] {
                    Pattern::Identifier(id) => assert_eq!(id.name, "y"),
                    _ => panic!("Expected identifier pattern"),
                }
                match &tuple_pattern.elements[2] {
                    Pattern::Identifier(id) => assert_eq!(id.name, "z"),
                    _ => panic!("Expected identifier pattern"),
                }
            }
            _ => panic!("Expected tuple pattern"),
        },
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_tuple_destructuring_with_type() {
    let input = "let (x, y): (Integer, String) = (42, \"hello\")";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Tuple(tuple_pattern) => {
                    assert_eq!(tuple_pattern.elements.len(), 2);
                    match &tuple_pattern.elements[0] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "x"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &tuple_pattern.elements[1] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "y"),
                        _ => panic!("Expected identifier pattern"),
                    }
                }
                _ => panic!("Expected tuple pattern"),
            }
            assert!(let_binding.type_annotation.is_some());
        }
        _ => panic!("Expected let binding expression"),
    }
}

// === STRUCT DESTRUCTURING ===

#[test]
fn test_struct_destructuring_basic() {
    let input = "let User { name, email } = user";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::Struct(struct_pattern) => {
                assert_eq!(struct_pattern.type_path.len(), 1);
                assert_eq!(struct_pattern.type_path[0].name, "User");
                assert_eq!(struct_pattern.fields.len(), 2);
                assert_eq!(struct_pattern.fields[0].name.name, "name");
                assert!(struct_pattern.fields[0].pattern.is_none()); // shorthand syntax
                assert_eq!(struct_pattern.fields[1].name.name, "email");
                assert!(struct_pattern.fields[1].pattern.is_none()); // shorthand syntax
            }
            _ => panic!("Expected struct pattern"),
        },
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_struct_destructuring_single_field() {
    let input = "let Point { x } = point";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::Struct(struct_pattern) => {
                assert_eq!(struct_pattern.type_path.len(), 1);
                assert_eq!(struct_pattern.type_path[0].name, "Point");
                assert_eq!(struct_pattern.fields.len(), 1);
                assert_eq!(struct_pattern.fields[0].name.name, "x");
                assert!(struct_pattern.fields[0].pattern.is_none()); // shorthand syntax
            }
            _ => panic!("Expected struct pattern"),
        },
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_struct_destructuring_with_type() {
    let input = "let User { name, email }: User = get_user()";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Struct(struct_pattern) => {
                    assert_eq!(struct_pattern.type_path.len(), 1);
                    assert_eq!(struct_pattern.type_path[0].name, "User");
                    assert_eq!(struct_pattern.fields.len(), 2);
                    assert_eq!(struct_pattern.fields[0].name.name, "name");
                    assert!(struct_pattern.fields[0].pattern.is_none()); // shorthand syntax
                    assert_eq!(struct_pattern.fields[1].name.name, "email");
                    assert!(struct_pattern.fields[1].pattern.is_none()); // shorthand syntax
                }
                _ => panic!("Expected struct pattern"),
            }
            assert!(let_binding.type_annotation.is_some());
        }
        _ => panic!("Expected let binding expression"),
    }
}

// === LIST DESTRUCTURING ===

#[test]
fn test_list_destructuring_basic() {
    let input = "let [first, second] = list";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::List(list_pattern) => {
                assert_eq!(list_pattern.elements.len(), 2);
                match &list_pattern.elements[0] {
                    Pattern::Identifier(id) => assert_eq!(id.name, "first"),
                    _ => panic!("Expected identifier pattern"),
                }
                match &list_pattern.elements[1] {
                    Pattern::Identifier(id) => assert_eq!(id.name, "second"),
                    _ => panic!("Expected identifier pattern"),
                }
                assert!(list_pattern.rest.is_none());
            }
            _ => panic!("Expected list pattern"),
        },
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_list_destructuring_with_rest() {
    let input = "let [first, ..rest] = list";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::List(list_pattern) => {
                assert_eq!(list_pattern.elements.len(), 1);
                match &list_pattern.elements[0] {
                    Pattern::Identifier(id) => assert_eq!(id.name, "first"),
                    _ => panic!("Expected identifier pattern"),
                }
                assert!(list_pattern.rest.is_some());
                assert_eq!(list_pattern.rest.as_ref().unwrap().name, "rest");
            }
            _ => panic!("Expected list pattern"),
        },
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_list_destructuring_multiple_with_rest() {
    let input = "let [first, second, ..rest] = list";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::List(list_pattern) => {
                assert_eq!(list_pattern.elements.len(), 2);
                match &list_pattern.elements[0] {
                    Pattern::Identifier(id) => assert_eq!(id.name, "first"),
                    _ => panic!("Expected identifier pattern"),
                }
                match &list_pattern.elements[1] {
                    Pattern::Identifier(id) => assert_eq!(id.name, "second"),
                    _ => panic!("Expected identifier pattern"),
                }
                assert!(list_pattern.rest.is_some());
                assert_eq!(list_pattern.rest.as_ref().unwrap().name, "rest");
            }
            _ => panic!("Expected list pattern"),
        },
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_list_destructuring_with_type() {
    let input = "let [first, second]: List<Integer> = numbers";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::List(list_pattern) => {
                    assert_eq!(list_pattern.elements.len(), 2);
                    match &list_pattern.elements[0] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "first"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &list_pattern.elements[1] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "second"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    assert!(list_pattern.rest.is_none());
                }
                _ => panic!("Expected list pattern"),
            }
            assert!(let_binding.type_annotation.is_some());
        }
        _ => panic!("Expected let binding expression"),
    }
}

// === SIMPLE IDENTIFIER PATTERNS (backward compatibility) ===

#[test]
fn test_simple_identifier_pattern() {
    let input = "let x = 42";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::Identifier(identifier) => {
                assert_eq!(identifier.name, "x");
            }
            _ => panic!("Expected identifier pattern"),
        },
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_simple_identifier_pattern_with_type() {
    let input = "let x: Integer = 42";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => {
                    assert_eq!(identifier.name, "x");
                }
                _ => panic!("Expected identifier pattern"),
            }
            assert!(let_binding.type_annotation.is_some());
        }
        _ => panic!("Expected let binding expression"),
    }
}

// === DISPLAY FORMATTING ===

#[test]
fn test_tuple_pattern_display() {
    let input = "let (x, y, z) = (1, 2, 3)";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);
    let formatted = format!("{}", result.items[0]);
    assert_eq!(formatted, input);
}

#[test]
fn test_struct_pattern_display() {
    let input = "let User { name, email } = user";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);
    let formatted = format!("{}", result.items[0]);
    assert_eq!(formatted, input);
}

#[test]
fn test_list_pattern_display() {
    let input = "let [first, second] = list";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);
    let formatted = format!("{}", result.items[0]);
    assert_eq!(formatted, input);
}

#[test]
fn test_list_pattern_with_rest_display() {
    let input = "let [first, ..rest] = list";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);
    let formatted = format!("{}", result.items[0]);
    assert_eq!(formatted, input);
}

#[test]
fn test_pattern_with_type_display() {
    let input = "let (x, y): (Integer, String) = (42, \"hello\")";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);
    let formatted = format!("{}", result.items[0]);
    assert_eq!(formatted, input);
}

// === COMPLEX EXPRESSIONS IN DESTRUCTURING ===

#[test]
fn test_destructuring_with_complex_expression() {
    let input = "let (x, y) = get_coordinates(position)";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Tuple(tuple_pattern) => {
                    assert_eq!(tuple_pattern.elements.len(), 2);
                    match &tuple_pattern.elements[0] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "x"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &tuple_pattern.elements[1] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "y"),
                        _ => panic!("Expected identifier pattern"),
                    }
                }
                _ => panic!("Expected tuple pattern"),
            }
            // Verify the expression is properly parsed (function call)
            match &let_binding.expression.kind {
                ExpressionKind::FunctionCall(_) => {
                    // Good, it parsed as a function call
                }
                _ => panic!("Expected function call expression"),
            }
        }
        _ => panic!("Expected let binding expression"),
    }
}

// === RECURSIVE DESTRUCTURING PATTERNS ===

#[test]
fn test_literal_pattern_integer() {
    let input = "let 42 = get_answer()";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => match &let_binding.pattern {
            Pattern::Literal(literal_pattern) => match &literal_pattern.literal {
                Literal::Integer(int_literal) => {
                    assert_eq!(int_literal.value, 42);
                }
                _ => panic!("Expected integer literal"),
            },
            _ => panic!("Expected literal pattern"),
        },
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_tuple_with_nested_list() {
    let input = "let (x, [a, b], y) = get_complex_data()";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Tuple(tuple_pattern) => {
                    assert_eq!(tuple_pattern.elements.len(), 3);

                    // First element: identifier x
                    match &tuple_pattern.elements[0] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "x"),
                        _ => panic!("Expected identifier pattern"),
                    }

                    // Second element: nested list [a, b]
                    match &tuple_pattern.elements[1] {
                        Pattern::List(list_pattern) => {
                            assert_eq!(list_pattern.elements.len(), 2);
                            match &list_pattern.elements[0] {
                                Pattern::Identifier(id) => assert_eq!(id.name, "a"),
                                _ => panic!("Expected identifier pattern"),
                            }
                            match &list_pattern.elements[1] {
                                Pattern::Identifier(id) => assert_eq!(id.name, "b"),
                                _ => panic!("Expected identifier pattern"),
                            }
                        }
                        _ => panic!("Expected list pattern"),
                    }

                    // Third element: identifier y
                    match &tuple_pattern.elements[2] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "y"),
                        _ => panic!("Expected identifier pattern"),
                    }
                }
                _ => panic!("Expected tuple pattern"),
            }
        }
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_list_with_mixed_patterns() {
    let input = "let [1, name, :status] = parse_response()";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::List(list_pattern) => {
                    assert_eq!(list_pattern.elements.len(), 3);

                    // First element: literal 1
                    match &list_pattern.elements[0] {
                        Pattern::Literal(literal_pattern) => match &literal_pattern.literal {
                            Literal::Integer(int_literal) => assert_eq!(int_literal.value, 1),
                            _ => panic!("Expected integer literal"),
                        },
                        _ => panic!("Expected literal pattern"),
                    }

                    // Second element: identifier name
                    match &list_pattern.elements[1] {
                        Pattern::Identifier(id) => assert_eq!(id.name, "name"),
                        _ => panic!("Expected identifier pattern"),
                    }

                    // Third element: atom :status
                    match &list_pattern.elements[2] {
                        Pattern::Literal(literal_pattern) => match &literal_pattern.literal {
                            Literal::Atom(atom_literal) => assert_eq!(atom_literal.name, "status"),
                            _ => panic!("Expected atom literal"),
                        },
                        _ => panic!("Expected literal pattern"),
                    }
                }
                _ => panic!("Expected list pattern"),
            }
        }
        _ => panic!("Expected let binding expression"),
    }
}
