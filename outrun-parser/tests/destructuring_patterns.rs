// Tests for destructuring patterns in let bindings

use outrun_parser::{parse_program};
use outrun_parser::ast::{ExpressionKind, ItemKind, Pattern};

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
                    assert_eq!(tuple_pattern.elements[0].name, "x");
                    assert_eq!(tuple_pattern.elements[1].name, "y");
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
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Tuple(tuple_pattern) => {
                    assert_eq!(tuple_pattern.elements.len(), 3);
                    assert_eq!(tuple_pattern.elements[0].name, "x");
                    assert_eq!(tuple_pattern.elements[1].name, "y");
                    assert_eq!(tuple_pattern.elements[2].name, "z");
                }
                _ => panic!("Expected tuple pattern"),
            }
        }
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
                    assert_eq!(tuple_pattern.elements[0].name, "x");
                    assert_eq!(tuple_pattern.elements[1].name, "y");
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
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Struct(struct_pattern) => {
                    assert_eq!(struct_pattern.type_name.name, "User");
                    assert_eq!(struct_pattern.fields.len(), 2);
                    assert_eq!(struct_pattern.fields[0].name, "name");
                    assert_eq!(struct_pattern.fields[1].name, "email");
                }
                _ => panic!("Expected struct pattern"),
            }
        }
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_struct_destructuring_single_field() {
    let input = "let Point { x } = point";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);
    
    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Struct(struct_pattern) => {
                    assert_eq!(struct_pattern.type_name.name, "Point");
                    assert_eq!(struct_pattern.fields.len(), 1);
                    assert_eq!(struct_pattern.fields[0].name, "x");
                }
                _ => panic!("Expected struct pattern"),
            }
        }
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
                    assert_eq!(struct_pattern.type_name.name, "User");
                    assert_eq!(struct_pattern.fields.len(), 2);
                    assert_eq!(struct_pattern.fields[0].name, "name");
                    assert_eq!(struct_pattern.fields[1].name, "email");
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
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::List(list_pattern) => {
                    assert_eq!(list_pattern.elements.len(), 2);
                    assert_eq!(list_pattern.elements[0].name, "first");
                    assert_eq!(list_pattern.elements[1].name, "second");
                    assert!(list_pattern.rest.is_none());
                }
                _ => panic!("Expected list pattern"),
            }
        }
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_list_destructuring_with_rest() {
    let input = "let [first, ..rest] = list";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);
    
    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::List(list_pattern) => {
                    assert_eq!(list_pattern.elements.len(), 1);
                    assert_eq!(list_pattern.elements[0].name, "first");
                    assert!(list_pattern.rest.is_some());
                    assert_eq!(list_pattern.rest.as_ref().unwrap().name, "rest");
                }
                _ => panic!("Expected list pattern"),
            }
        }
        _ => panic!("Expected let binding expression"),
    }
}

#[test]
fn test_list_destructuring_multiple_with_rest() {
    let input = "let [first, second, ..rest] = list";
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 1);
    
    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::List(list_pattern) => {
                    assert_eq!(list_pattern.elements.len(), 2);
                    assert_eq!(list_pattern.elements[0].name, "first");
                    assert_eq!(list_pattern.elements[1].name, "second");
                    assert!(list_pattern.rest.is_some());
                    assert_eq!(list_pattern.rest.as_ref().unwrap().name, "rest");
                }
                _ => panic!("Expected list pattern"),
            }
        }
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
                    assert_eq!(list_pattern.elements[0].name, "first");
                    assert_eq!(list_pattern.elements[1].name, "second");
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
        ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                Pattern::Identifier(identifier) => {
                    assert_eq!(identifier.name, "x");
                }
                _ => panic!("Expected identifier pattern"),
            }
        }
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
                    assert_eq!(tuple_pattern.elements[0].name, "x");
                    assert_eq!(tuple_pattern.elements[1].name, "y");
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