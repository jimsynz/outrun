use crate::{parse_program, ExpressionKind, ItemKind, StatementKind};

#[test]
fn test_macro_with_nested_injections() {
    let input = r#"
macro nested(outer, inner) {
    let result = ^outer
    nested_call(value: ^inner)
}
"#;

    let result = parse_program(input).unwrap();

    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "nested");
            assert_eq!(macro_def.parameters.len(), 2);

            assert_eq!(macro_def.body.statements.len(), 2);
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should parse macro with nested structure");
}

#[test]
fn test_macro_injection_in_arithmetic() {
    let input = r#"
macro calc(a, b, c) {
    ^a + ^b * ^c
}
"#;

    let result = parse_program(input).unwrap();

    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "calc");
            assert_eq!(macro_def.parameters.len(), 3);

            assert_eq!(macro_def.body.statements.len(), 1);
            if let StatementKind::Expression(expr) = &macro_def.body.statements[0].kind {
                if let ExpressionKind::BinaryOp(_) = &expr.kind {
                } else {
                    panic!("Expected binary operation in macro body");
                }
            } else {
                panic!("Expected expression statement in macro body");
            }
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should parse macro with arithmetic injections");
}

#[test]
fn test_macro_injection_in_comparison() {
    let input = r#"
macro check(left, right) {
    ^left == ^right
}
"#;

    let result = parse_program(input).unwrap();

    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "check");
            assert_eq!(macro_def.parameters.len(), 2);
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should parse macro with comparison injections");
}

#[test]
fn test_macro_with_underscore_param() {
    let input = r#"
macro with_underscore(param_name) {
    IO.puts(message: ^param_name)
}
"#;

    let result = parse_program(input).unwrap();

    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "with_underscore");
            assert_eq!(macro_def.parameters.len(), 1);
            assert_eq!(macro_def.parameters[0].name, "param_name");
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should handle underscore in parameter names");
}

#[test]
fn test_macro_injection_in_string_interpolation() {
    let input = r#"
macro log_debug(var) {
    IO.puts(message: "Debug: #{^var}")
}
"#;

    let result = parse_program(input).unwrap();

    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "log_debug");
            assert_eq!(macro_def.parameters.len(), 1);
            assert_eq!(macro_def.parameters[0].name, "var");

            assert_eq!(macro_def.body.statements.len(), 1);
            found_macro = true;
            break;
        }
    }
    assert!(
        found_macro,
        "Should parse macro injection in string interpolation"
    );
}

#[test]
fn test_macro_with_simple_expression_body() {
    let input = r#"macro simple() { 42 }"#;

    let result = parse_program(input).unwrap();

    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "simple");
            assert_eq!(macro_def.parameters.len(), 0);

            assert_eq!(macro_def.body.statements.len(), 1);
            if let StatementKind::Expression(expr) = &macro_def.body.statements[0].kind {
                if let ExpressionKind::Integer(int_lit) = &expr.kind {
                    assert_eq!(int_lit.value, 42);
                } else {
                    panic!("Expected integer literal");
                }
            } else {
                panic!("Expected expression statement");
            }
            found_macro = true;
            break;
        }
    }
    assert!(
        found_macro,
        "Should parse macro with simple expression body"
    );
}

#[test]
fn test_macro_injection_display_format() {
    let input = r#"macro test(x) { ^x }"#;

    let result = parse_program(input).unwrap();
    let reconstructed = format!("{result}");

    assert!(
        reconstructed.contains("^x"),
        "Should preserve macro injection syntax in display"
    );
}
