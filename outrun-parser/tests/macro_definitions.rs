use outrun_parser::{parse_program, ItemKind, ExpressionKind, StatementKind};
use outrun_parser::ast::Pattern;

#[test]
fn test_macro_basic_no_params() {
    let input = r#"
macro debug() {
    IO.puts(message: "Debug!")
}
"#;
    
    let result = parse_program(input).unwrap();
    assert_eq!(result.items.len(), 3); // newline + macro + newline

    if let ItemKind::Expression(expr) = &result.items[1].kind {
        if let ExpressionKind::MacroInjection(_) = &expr.kind {
            panic!("Expected macro definition, got macro injection");
        }
    }

    // Check if we can find the macro definition in wrapped expressions
    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "debug");
            assert_eq!(macro_def.parameters.len(), 0);
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should find macro definition in items");
}

#[test]
fn test_macro_single_param() {
    let input = r#"
macro log(message) {
    IO.puts(message: ^message)
}
"#;
    
    let result = parse_program(input).unwrap();
    
    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "log");
            assert_eq!(macro_def.parameters.len(), 1);
            assert_eq!(macro_def.parameters[0].name, "message");
            
            // Check the body contains statements
            assert!(!macro_def.body.statements.is_empty());
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should find macro definition");
}

#[test]
fn test_macro_multiple_params() {
    let input = r#"
macro assert_equal(left, right, message) {
    let result = ^left == ^right
    if !result {
        IO.puts(message: ^message)
    }
}
"#;
    
    let result = parse_program(input).unwrap();
    
    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "assert_equal");
            assert_eq!(macro_def.parameters.len(), 3);
            assert_eq!(macro_def.parameters[0].name, "left");
            assert_eq!(macro_def.parameters[1].name, "right");
            assert_eq!(macro_def.parameters[2].name, "message");
            
            // Check that the body has statements
            assert_eq!(macro_def.body.statements.len(), 2); // let + if
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should find macro definition with multiple parameters");
}

#[test]
fn test_macro_with_injection_in_let_binding() {
    let input = r#"
macro capture(var) {
    let captured = ^var
}
"#;
    
    let result = parse_program(input).unwrap();
    
    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "capture");
            assert_eq!(macro_def.parameters.len(), 1);
            assert_eq!(macro_def.parameters[0].name, "var");
            
            // Check the let binding contains a macro injection
            assert_eq!(macro_def.body.statements.len(), 1);
            if let StatementKind::LetBinding(let_binding) = &macro_def.body.statements[0].kind {
                match &let_binding.pattern {
                    Pattern::Identifier(identifier) => assert_eq!(identifier.name, "captured"),
                    _ => panic!("Expected identifier pattern"),
                }
                if let ExpressionKind::MacroInjection(injection) = &let_binding.expression.kind {
                    assert_eq!(injection.parameter.name, "var");
                } else {
                    panic!("Expected macro injection in let binding expression");
                }
            } else {
                panic!("Expected let binding in macro body");
            }
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should find macro definition with injection in let binding");
}

#[test]
fn test_macro_with_complex_body() {
    let input = r#"
macro unless(condition, do_block) {
    if !^condition {
        ^do_block
    }
}
"#;
    
    let result = parse_program(input).unwrap();
    
    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "unless");
            assert_eq!(macro_def.parameters.len(), 2);
            assert_eq!(macro_def.parameters[0].name, "condition");
            assert_eq!(macro_def.parameters[1].name, "do_block");
            
            // Check the if expression structure
            assert_eq!(macro_def.body.statements.len(), 1);
            if let StatementKind::Expression(expr) = &macro_def.body.statements[0].kind {
                if let ExpressionKind::IfExpression(_) = &expr.kind {
                    // Detailed validation would require deeper inspection
                    // For now, just verify we parsed an if expression
                } else {
                    panic!("Expected if expression in macro body");
                }
            } else {
                panic!("Expected expression statement in macro body");
            }
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should find macro definition with complex body");
}

#[test]
fn test_macro_trailing_comma_in_params() {
    let input = r#"
macro test(a, b, c,) {
    ^a + ^b + ^c
}
"#;
    
    let result = parse_program(input).unwrap();
    
    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            assert_eq!(macro_def.name.name, "test");
            assert_eq!(macro_def.parameters.len(), 3);
            assert_eq!(macro_def.parameters[0].name, "a");
            assert_eq!(macro_def.parameters[1].name, "b");
            assert_eq!(macro_def.parameters[2].name, "c");
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should handle trailing comma in macro parameters");
}

#[test]
fn test_macro_injection_standalone() {
    let input = r#"
macro use_var(value) {
    ^value
}
"#;
    
    let result = parse_program(input).unwrap();
    
    let mut found_macro = false;
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            // Check that the body contains a macro injection as an expression
            assert_eq!(macro_def.body.statements.len(), 1);
            if let StatementKind::Expression(expr) = &macro_def.body.statements[0].kind {
                if let ExpressionKind::MacroInjection(injection) = &expr.kind {
                    assert_eq!(injection.parameter.name, "value");
                } else {
                    panic!("Expected macro injection expression");
                }
            } else {
                panic!("Expected expression statement");
            }
            found_macro = true;
            break;
        }
    }
    assert!(found_macro, "Should find macro with standalone injection");
}

#[test]
fn test_macro_source_reconstruction() {
    let input = r#"macro debug(msg) {
    IO.puts(message: ^msg)
}"#;
    
    let result = parse_program(input).unwrap();
    let reconstructed = format!("{}", result);
    
    // Should contain the basic structure
    assert!(reconstructed.contains("macro debug(msg)"));
    assert!(reconstructed.contains("IO.puts(message: ^msg)"));
}

#[test]
fn test_multiple_macros_in_program() {
    let input = r#"
macro first() {
    1
}

macro second(x) {
    ^x + 1
}
"#;
    
    let result = parse_program(input).unwrap();
    
    let mut macro_count = 0;
    let mut found_first = false;
    let mut found_second = false;
    
    for item in &result.items {
        if let ItemKind::MacroDefinition(macro_def) = &item.kind {
            macro_count += 1;
            match macro_def.name.name.as_str() {
                "first" => {
                    assert_eq!(macro_def.parameters.len(), 0);
                    found_first = true;
                }
                "second" => {
                    assert_eq!(macro_def.parameters.len(), 1);
                    assert_eq!(macro_def.parameters[0].name, "x");
                    found_second = true;
                }
                _ => panic!("Unexpected macro name: {}", macro_def.name.name),
            }
        }
    }
    
    assert_eq!(macro_count, 2);
    assert!(found_first && found_second, "Should find both macro definitions");
}