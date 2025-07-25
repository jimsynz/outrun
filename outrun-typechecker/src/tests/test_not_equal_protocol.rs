//! Test to verify the new != desugaring approach

use crate::DesugaringEngine;
use outrun_parser::parse_expression;

#[test]
fn test_not_equal_desugars_to_protocol_method() {
    let mut engine = DesugaringEngine::new();

    // Parse "x != y"
    let mut expr = parse_expression("x != y").unwrap();

    // Desugar the expression
    engine.desugar_expression(&mut expr).unwrap();

    // Should become Equality.not_equal?(left: x, right: y) - NOT negated equality!
    match expr.kind {
        outrun_parser::ExpressionKind::FunctionCall(func_call) => {
            match &func_call.path {
                outrun_parser::FunctionPath::Qualified { module, name } => {
                    assert_eq!(module.name, "Equality");
                    assert_eq!(name.name, "not_equal?");

                    // Verify arguments
                    assert_eq!(func_call.arguments.len(), 2);
                }
                _ => panic!("Expected qualified function path"),
            }
        }
        _ => panic!("Expected function call, not nested unary operation"),
    }

    // Verify this is a simple transformation, not the complex negated equality
    assert_eq!(engine.transformations.len(), 1);
    assert!(engine.transformations[0].contains("Equality.not_equal?"));
    assert!(!engine.transformations[0].contains("LogicalNot"));
}
