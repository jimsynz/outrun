// Integration test for spread operator in list construction
use outrun_parser::{parse_program, ast::*};

#[test]
fn test_spread_operator_integration() {
    // Test the basic example from PEST_PLAN.md
    let input = "let new_list = [first, ..existing_list]";
    let result = parse_program(input).unwrap();
    
    assert_eq!(result.items.len(), 1);
    
    // Should parse as a let binding with a list expression containing spread
    match &result.items[0].kind {
        ItemKind::LetBinding(let_binding) => {
            // Check the identifier pattern
            match &let_binding.pattern {
                Pattern::Identifier(id) => assert_eq!(id.name, "new_list"),
                _ => panic!("Expected identifier pattern"),
            }
            
            // Check the list expression
            match &let_binding.expression.kind {
                ExpressionKind::List(list) => {
                    assert_eq!(list.elements.len(), 2);
                    
                    // First element should be 'first' identifier
                    match &list.elements[0] {
                        ListElement::Expression(expr) => match &expr.kind {
                            ExpressionKind::Identifier(id) => assert_eq!(id.name, "first"),
                            _ => panic!("Expected identifier"),
                        },
                        ListElement::Spread(_) => panic!("Expected expression, not spread"),
                    }
                    
                    // Second element should be spread 'existing_list'
                    match &list.elements[1] {
                        ListElement::Spread(id) => assert_eq!(id.name, "existing_list"),
                        ListElement::Expression(_) => panic!("Expected spread, not expression"),
                    }
                }
                _ => panic!("Expected list expression"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_spread_operator_display_integration() {
    let input = "let result = [1, 2, ..middle, 3, 4]";
    let result = parse_program(input).unwrap();
    
    // Test that display format is preserved
    let formatted = format!("{}", result);
    assert!(formatted.contains("[1, 2, ..middle, 3, 4]"));
}