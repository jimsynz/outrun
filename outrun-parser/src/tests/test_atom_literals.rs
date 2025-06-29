use crate::{ast::*, parse_program};

fn extract_atom_from_expression(expr: &Expression) -> &AtomLiteral {
    match &expr.kind {
        ExpressionKind::Atom(atom) => atom,
        _ => panic!("Expected atom in expression, got: {:?}", expr.kind),
    }
}

#[test]
fn test_parse_simple_atom() {
    let input = ":hello";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, "hello");
            assert_eq!(atom.content, "hello");
            assert_eq!(atom.raw_content, "hello");
            assert_eq!(atom.format, AtomFormat::Simple);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_simple_atom_with_underscores() {
    let input = ":hello_world";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, "hello_world");
            assert_eq!(atom.content, "hello_world");
            assert_eq!(atom.raw_content, "hello_world");
            assert_eq!(atom.format, AtomFormat::Simple);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_simple_atom_with_numbers() {
    let input = ":test123";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, "test123");
            assert_eq!(atom.content, "test123");
            assert_eq!(atom.raw_content, "test123");
            assert_eq!(atom.format, AtomFormat::Simple);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_simple_atom_underscore_start() {
    let input = ":_private";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, "_private");
            assert_eq!(atom.content, "_private");
            assert_eq!(atom.raw_content, "_private");
            assert_eq!(atom.format, AtomFormat::Simple);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_quoted_atom_simple() {
    let input = r#":"hello world""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, "hello world");
            assert_eq!(atom.content, "hello world");
            assert_eq!(atom.raw_content, "hello world");
            assert_eq!(atom.format, AtomFormat::Quoted);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_quoted_atom_with_escapes() {
    let input = r#":"hello\nworld""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, "hello\nworld");
            assert_eq!(atom.content, "hello\nworld");
            assert_eq!(atom.raw_content, r"hello\nworld");
            assert_eq!(atom.format, AtomFormat::Quoted);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_quoted_atom_with_quote_escape() {
    let input = r#":"say \"hello\"""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, r#"say "hello""#);
            assert_eq!(atom.content, r#"say "hello""#);
            assert_eq!(atom.raw_content, r#"say \"hello\""#);
            assert_eq!(atom.format, AtomFormat::Quoted);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_quoted_atom_with_unicode() {
    let input = r#":"hello \u0041BC""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, "hello ABC");
            assert_eq!(atom.content, "hello ABC");
            assert_eq!(atom.raw_content, r"hello \u0041BC");
            assert_eq!(atom.format, AtomFormat::Quoted);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_parse_quoted_atom_empty() {
    let input = r#":"""#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let atom = extract_atom_from_expression(expr);
            assert_eq!(atom.name, "");
            assert_eq!(atom.content, "");
            assert_eq!(atom.raw_content, "");
            assert_eq!(atom.format, AtomFormat::Quoted);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }
}

#[test]
fn test_atom_display_preserves_format() {
    let input1 = ":hello";
    let result1 = parse_program(input1).unwrap();
    let formatted1 = format!("{result1}");
    assert_eq!(formatted1, input1);

    let input2 = r#":"hello world""#;
    let result2 = parse_program(input2).unwrap();
    let formatted2 = format!("{result2}");
    assert_eq!(formatted2, input2);
}

#[test]
fn test_comprehensive_mix_with_atoms() {
    let input = r#"true :simple :"quoted atom" 42 :test"#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 5);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Boolean(b) => assert!(b.value),
            _ => panic!("Expected boolean in expression"),
        },
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }

    match &result.items[1].kind {
        ItemKind::Expression(expr) => {
            let a = extract_atom_from_expression(expr);
            assert_eq!(a.name, "simple");
            assert_eq!(a.format, AtomFormat::Simple);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[1].kind),
    }

    match &result.items[2].kind {
        ItemKind::Expression(expr) => {
            let a = extract_atom_from_expression(expr);
            assert_eq!(a.name, "quoted atom");
            assert_eq!(a.format, AtomFormat::Quoted);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[2].kind),
    }

    match &result.items[3].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Integer(i) => assert_eq!(i.value, 42),
            _ => panic!("Expected integer in expression"),
        },
        _ => panic!("Expected expression, got: {:?}", result.items[3].kind),
    }

    match &result.items[4].kind {
        ItemKind::Expression(expr) => {
            let a = extract_atom_from_expression(expr);
            assert_eq!(a.name, "test");
            assert_eq!(a.format, AtomFormat::Simple);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[4].kind),
    }
}

#[test]
fn test_atom_vs_identifier_precedence() {
    let input = ":test identifier";
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 2);

    match &result.items[0].kind {
        ItemKind::Expression(expr) => {
            let a = extract_atom_from_expression(expr);
            assert_eq!(a.name, "test");
            assert_eq!(a.format, AtomFormat::Simple);
        }
        _ => panic!("Expected expression, got: {:?}", result.items[0].kind),
    }

    match &result.items[1].kind {
        ItemKind::Expression(expr) => match &expr.kind {
            ExpressionKind::Identifier(id) => assert_eq!(id.name, "identifier"),
            _ => panic!("Expected identifier in expression"),
        },
        _ => panic!("Expected expression, got: {:?}", result.items[1].kind),
    }
}
