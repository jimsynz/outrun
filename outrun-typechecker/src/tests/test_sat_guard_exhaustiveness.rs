//! Unit tests for SAT-based guard exhaustiveness analysis

use crate::checker::{TypeContext, TypedExpression, TypedExpressionKind};
use crate::exhaustiveness::{GuardCounterExample, GuardSatAnalyzer, SatResult};
use crate::types::TypeId;
use outrun_parser::Span;
use std::collections::HashMap;

/// Helper function to create a test typed expression
fn create_test_expression(kind: TypedExpressionKind, type_id: TypeId) -> TypedExpression {
    TypedExpression {
        kind,
        type_id,
        span: Span::new(0, 1),
    }
}

/// Helper function to create a test context
fn create_test_context() -> TypeContext {
    TypeContext::new()
}

#[test]
fn test_guard_sat_analyzer_creation() {
    let analyzer = GuardSatAnalyzer::new();

    // Verify analyzer is properly initialized
    assert!(analyzer.var_mapping().is_empty());
}

#[test]
fn test_empty_guards_handling() {
    let mut analyzer = GuardSatAnalyzer::new();
    let context = create_test_context();

    // Test converting empty guards list
    let result = analyzer.convert_guards_to_sat(&[], &context);
    assert!(result.is_ok());
    let guard_vars = result.unwrap();
    assert!(guard_vars.is_empty());

    // Test completeness check with empty guards
    let completeness = analyzer.check_completeness(guard_vars);
    assert!(completeness.is_ok());

    match completeness.unwrap() {
        SatResult::Missing(examples) => {
            assert_eq!(examples.len(), 1);
            assert_eq!(examples[0].description, "No guards provided");
            assert!(examples[0].variable_assignments.is_empty());
        }
        SatResult::Exhaustive => panic!("Empty guards should not be exhaustive"),
    }
}

#[test]
fn test_boolean_literal_guards() {
    let mut analyzer = GuardSatAnalyzer::new();
    let context = create_test_context();
    let boolean_type_id = TypeId::from_usize(1); // Dummy type ID for testing

    // Test with true literal guard
    let true_guard = create_test_expression(TypedExpressionKind::Boolean(true), boolean_type_id);

    let result = analyzer.convert_guards_to_sat(&[true_guard], &context);
    assert!(result.is_ok());
    let guard_vars = result.unwrap();
    assert_eq!(guard_vars.len(), 1);

    // Test completeness - a single 'true' guard is not exhaustive
    let completeness = analyzer.check_completeness(guard_vars);
    assert!(completeness.is_ok());

    match completeness.unwrap() {
        SatResult::Missing(_) => {
            // Expected: 'true' alone doesn't cover 'false' case
        }
        SatResult::Exhaustive => panic!("Single 'true' guard should not be exhaustive"),
    }
}

#[test]
fn test_complementary_boolean_guards() {
    let mut analyzer = GuardSatAnalyzer::new();
    let context = create_test_context();
    let boolean_type_id = TypeId::from_usize(1); // Dummy type ID for testing

    // Test with both true and false guards
    let true_guard = create_test_expression(TypedExpressionKind::Boolean(true), boolean_type_id);
    let false_guard = create_test_expression(TypedExpressionKind::Boolean(false), boolean_type_id);

    let result = analyzer.convert_guards_to_sat(&[true_guard, false_guard], &context);
    assert!(result.is_ok());
    let guard_vars = result.unwrap();
    assert_eq!(guard_vars.len(), 2);

    // Test completeness - true and false guards should be exhaustive
    let completeness = analyzer.check_completeness(guard_vars);
    assert!(completeness.is_ok());

    match completeness.unwrap() {
        SatResult::Exhaustive => {
            // Expected: true + false covers all boolean cases
        }
        SatResult::Missing(examples) => {
            panic!(
                "True + false guards should be exhaustive, but got missing: {:?}",
                examples
            );
        }
    }
}

#[test]
fn test_identifier_guards() {
    let mut analyzer = GuardSatAnalyzer::new();
    let context = create_test_context();
    let boolean_type_id = TypeId::from_usize(1); // Dummy type ID for testing

    // Test with identifier guard (variable reference)
    let identifier_guard = create_test_expression(
        TypedExpressionKind::Identifier("is_valid".to_string()),
        boolean_type_id,
    );

    let result = analyzer.convert_guards_to_sat(&[identifier_guard], &context);
    assert!(result.is_ok());
    let guard_vars = result.unwrap();
    assert_eq!(guard_vars.len(), 1);

    // Verify variable mapping was created
    assert!(analyzer.var_mapping().contains_key("is_valid"));

    // Test completeness - single variable is not exhaustive
    let completeness = analyzer.check_completeness(guard_vars);
    assert!(completeness.is_ok());

    match completeness.unwrap() {
        SatResult::Missing(examples) => {
            assert!(!examples.is_empty());
            // Variable assignments may be empty in heuristic implementation
            // The important thing is that we detected it's not exhaustive
        }
        SatResult::Exhaustive => panic!("Single variable guard should not be exhaustive"),
    }
}

#[test]
fn test_placeholder_guards() {
    let mut analyzer = GuardSatAnalyzer::new();
    let context = create_test_context();
    let unknown_type_id = TypeId::from_usize(2); // Dummy type ID for testing

    // Test with placeholder guard (unsupported expression)
    let placeholder_guard = create_test_expression(
        TypedExpressionKind::Placeholder("complex_expression".to_string()),
        unknown_type_id,
    );

    let result = analyzer.convert_guards_to_sat(&[placeholder_guard], &context);
    assert!(result.is_ok());
    let guard_vars = result.unwrap();
    assert_eq!(guard_vars.len(), 1);

    // Verify placeholder variable mapping was created
    assert!(analyzer
        .var_mapping()
        .iter()
        .any(|(key, _)| key.contains("placeholder")));

    // Test completeness
    let completeness = analyzer.check_completeness(guard_vars);
    assert!(completeness.is_ok());
}

#[test]
fn test_mixed_expression_types() {
    let mut analyzer = GuardSatAnalyzer::new();
    let context = create_test_context();
    let boolean_type_id = TypeId::from_usize(1); // Dummy type ID for testing
    let integer_type_id = TypeId::from_usize(3); // Dummy type ID for testing

    // Test with mixed expression types
    let guards = vec![
        create_test_expression(TypedExpressionKind::Boolean(true), boolean_type_id),
        create_test_expression(
            TypedExpressionKind::Identifier("x".to_string()),
            integer_type_id,
        ),
        create_test_expression(TypedExpressionKind::Integer(42), integer_type_id),
        create_test_expression(
            TypedExpressionKind::Placeholder("function_call".to_string()),
            boolean_type_id,
        ),
    ];

    let result = analyzer.convert_guards_to_sat(&guards, &context);
    assert!(result.is_ok());
    let guard_vars = result.unwrap();
    assert_eq!(guard_vars.len(), 4);

    // Verify different variable mappings were created
    assert!(analyzer.var_mapping().contains_key("x"));
    assert!(analyzer
        .var_mapping()
        .iter()
        .any(|(key, _)| key.contains("bool_literal")));
    assert!(analyzer
        .var_mapping()
        .iter()
        .any(|(key, _)| key.contains("expr_")));
    assert!(analyzer
        .var_mapping()
        .iter()
        .any(|(key, _)| key.contains("placeholder")));

    // Test completeness
    let completeness = analyzer.check_completeness(guard_vars);
    assert!(completeness.is_ok());
}

#[test]
fn test_counter_example_generation() {
    let _analyzer = GuardSatAnalyzer::new();

    // Test counter-example with variable assignments
    let mut variable_assignments = HashMap::new();
    variable_assignments.insert("flag".to_string(), false);
    variable_assignments.insert("count".to_string(), true);

    let counter_example = GuardCounterExample {
        variable_assignments: variable_assignments.clone(),
        description: "Test missing case".to_string(),
    };

    let counter_examples = [counter_example];

    // Verify description contains variable information
    assert!(!counter_examples[0].description.is_empty());
    assert_eq!(counter_examples[0].variable_assignments.len(), 2);
    assert_eq!(
        counter_examples[0].variable_assignments.get("flag"),
        Some(&false)
    );
    assert_eq!(
        counter_examples[0].variable_assignments.get("count"),
        Some(&true)
    );
}

#[test]
fn test_variable_reuse_across_guards() {
    let mut analyzer = GuardSatAnalyzer::new();
    let context = create_test_context();
    let boolean_type_id = TypeId::from_usize(1); // Dummy type ID for testing

    // Test with multiple guards using the same variable
    let guards = vec![
        create_test_expression(
            TypedExpressionKind::Identifier("flag".to_string()),
            boolean_type_id,
        ),
        create_test_expression(
            TypedExpressionKind::Identifier("flag".to_string()),
            boolean_type_id,
        ),
    ];

    let result = analyzer.convert_guards_to_sat(&guards, &context);
    assert!(result.is_ok());
    let guard_vars = result.unwrap();
    assert_eq!(guard_vars.len(), 2);

    // Verify the same variable is reused (only one mapping for "flag")
    assert_eq!(analyzer.var_mapping().len(), 1);
    assert!(analyzer.var_mapping().contains_key("flag"));

    // Both guard variables should be the same
    assert_eq!(guard_vars[0], guard_vars[1]);
}

#[test]
fn test_default_implementation() {
    let analyzer = GuardSatAnalyzer::default();
    assert!(analyzer.var_mapping().is_empty());
}
