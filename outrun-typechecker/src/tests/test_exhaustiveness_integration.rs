//! Integration tests for exhaustiveness analysis
//!
//! These tests verify that the ExhaustivenessAnalyzer works correctly
//! with realistic Outrun code examples and integrates properly with
//! the type checker error reporting system.

use crate::checker::TypeContext;
use crate::exhaustiveness::{
    CaseType, CoveredPattern, ExhaustivenessAnalyzer, ExhaustivenessResult, MissingPattern,
};
use outrun_parser::Span;

fn setup_test_context() -> TypeContext {
    TypeContext::new()
}

#[test]
fn test_boolean_case_exhaustiveness_integration() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    // Test case where only 'true' is covered
    let covered_patterns = vec![CoveredPattern::Boolean(true)];
    let boolean_type_id = context.interner.get_type("Outrun.Core.Boolean").unwrap();
    let case_type = CaseType::Pattern(boolean_type_id);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type, &covered_patterns, Span::new(0, 20))
        .unwrap();

    // Should be missing 'false'
    match result {
        ExhaustivenessResult::Missing(missing) => {
            assert_eq!(missing.len(), 1);
            assert_eq!(missing[0].description, "Boolean value 'false'");
            assert_eq!(missing[0].example_pattern, "false");
        }
        _ => panic!("Expected missing patterns for incomplete boolean case"),
    }
}

#[test]
fn test_boolean_case_complete_exhaustiveness() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    // Test case where both 'true' and 'false' are covered
    let covered_patterns = vec![
        CoveredPattern::Boolean(true),
        CoveredPattern::Boolean(false),
    ];
    let boolean_type_id = context.interner.get_type("Outrun.Core.Boolean").unwrap();
    let case_type = CaseType::Pattern(boolean_type_id);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type, &covered_patterns, Span::new(0, 20))
        .unwrap();

    // Should be exhaustive
    assert_eq!(result, ExhaustivenessResult::Exhaustive);
}

#[test]
fn test_trait_based_option_exhaustiveness() {
    let mut context = setup_test_context();

    // Set up Option trait and implementing types (Option.Some and Option.None)
    let option_trait_id = context.interner.intern_trait("Option");
    let option_some_type_id = context.interner.intern_type("Outrun.Option.Some");
    let option_none_type_id = context.interner.intern_type("Outrun.Option.None");

    // Register trait and implementations
    use crate::types::traits::{TraitDefinition, TraitImplementation};
    use std::collections::HashMap;

    let trait_def = TraitDefinition::new(
        option_trait_id,
        "Option".to_string(),
        vec![],
        Span::new(0, 10),
    );
    context.trait_registry.register_trait(trait_def);

    let some_impl = TraitImplementation::new(
        option_trait_id,
        option_some_type_id,
        HashMap::new(),
        Span::new(0, 10),
    );
    let none_impl = TraitImplementation::new(
        option_trait_id,
        option_none_type_id,
        HashMap::new(),
        Span::new(0, 10),
    );

    context.trait_registry.register_implementation(some_impl);
    context.trait_registry.register_implementation(none_impl);

    // Create analyzer after setting up context
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    // Test case where only Option.Some is covered
    let covered_patterns = vec![CoveredPattern::TraitType(option_some_type_id)];
    let case_type = CaseType::Trait(option_trait_id);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type, &covered_patterns, Span::new(0, 30))
        .unwrap();

    // Should be missing Option.None
    match result {
        ExhaustivenessResult::Missing(missing) => {
            assert_eq!(missing.len(), 1);
            assert_eq!(
                context.get_type_name(missing[0].type_id),
                Some("Outrun.Option.None")
            );
        }
        _ => panic!("Expected missing Option.None for incomplete Option trait case"),
    }
}

#[test]
fn test_infinite_type_exhaustiveness() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    // Test infinite types like String, Integer - should require catch-all
    let string_type_id = context.interner.get_type("Outrun.Core.String").unwrap();
    let case_type = CaseType::Pattern(string_type_id);
    let covered_patterns = vec![];

    let result = analyzer
        .analyze_case_exhaustiveness(case_type, &covered_patterns, Span::new(0, 20))
        .unwrap();

    // Should be open type (infinite)
    assert_eq!(result, ExhaustivenessResult::OpenType);
}

#[test]
fn test_error_conversion_boolean_not_exhaustive() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    // Test error conversion for boolean case
    let covered_patterns = vec![CoveredPattern::Boolean(true)];
    let boolean_type_id = context.interner.get_type("Outrun.Core.Boolean").unwrap();
    let case_type = CaseType::Pattern(boolean_type_id);
    let span = Span::new(10, 30);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type.clone(), &covered_patterns, span)
        .unwrap();

    // Convert to TypeError
    let error = result.to_case_error(case_type, &context, span);
    assert!(error.is_some());

    match error.unwrap() {
        crate::error::TypeError::BooleanNotExhaustive { missing_values, .. } => {
            assert_eq!(missing_values, vec!["false"]);
        }
        _ => panic!("Expected BooleanNotExhaustive error"),
    }
}

#[test]
fn test_error_conversion_open_type() {
    let context = setup_test_context();

    // Test error conversion for open type (like String)
    let string_type_id = context.interner.get_type("Outrun.Core.String").unwrap();
    let case_type = CaseType::Pattern(string_type_id);
    let span = Span::new(15, 35);

    let result = ExhaustivenessResult::OpenType;

    // Convert to TypeError
    let error = result.to_case_error(case_type, &context, span);
    assert!(error.is_some());

    match error.unwrap() {
        crate::error::TypeError::NonExhaustivePatterns { missing_cases, .. } => {
            assert_eq!(missing_cases, vec!["catch-all pattern (_)"]);
        }
        _ => panic!("Expected NonExhaustivePatterns error"),
    }
}

#[test]
fn test_missing_pattern_error_messages() {
    let context = setup_test_context();
    let boolean_type_id = context.interner.get_type("Outrun.Core.Boolean").unwrap();

    let missing_pattern = MissingPattern::new(
        boolean_type_id,
        "Boolean value 'true'".to_string(),
        "true".to_string(),
    );

    let error_message = missing_pattern.error_message();
    assert_eq!(
        error_message,
        "Missing pattern: Boolean value 'true'. Consider adding: true"
    );
}

#[test]
fn test_exhaustiveness_result_open_type_handling() {
    let context = setup_test_context();
    let string_type_id = context.interner.get_type("Outrun.Core.String").unwrap();
    let case_type = CaseType::Pattern(string_type_id);
    let span = Span::new(0, 20);

    let result = ExhaustivenessResult::OpenType;
    let error = result.to_case_error(case_type, &context, span);

    assert!(error.is_some());
    match error.unwrap() {
        crate::error::TypeError::NonExhaustivePatterns { missing_cases, .. } => {
            assert_eq!(missing_cases, vec!["catch-all pattern (_)"]);
        }
        _ => panic!("Expected NonExhaustivePatterns error for open type"),
    }
}

#[test]
fn test_trait_case_exhaustiveness_with_error_conversion() {
    let mut context = setup_test_context();

    // Set up a simple trait and implementations
    let display_trait_id = context.interner.intern_trait("Display");
    let integer_type_id = context.interner.intern_type("Integer");
    let string_type_id = context.interner.intern_type("String");

    // Register trait (simplified - in real usage this would be done by trait checker)
    use crate::types::traits::{TraitDefinition, TraitImplementation};
    use std::collections::HashMap;

    let trait_def = TraitDefinition::new(
        display_trait_id,
        "Display".to_string(),
        vec![],
        Span::new(0, 10),
    );
    context.trait_registry.register_trait(trait_def);

    // Register implementations
    let integer_impl = TraitImplementation::new(
        display_trait_id,
        integer_type_id,
        HashMap::new(),
        Span::new(0, 10),
    );
    let string_impl = TraitImplementation::new(
        display_trait_id,
        string_type_id,
        HashMap::new(),
        Span::new(0, 10),
    );

    context.trait_registry.register_implementation(integer_impl);
    context.trait_registry.register_implementation(string_impl);

    // Now test exhaustiveness analysis
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    // Test case where only Integer is covered
    let covered_patterns = vec![CoveredPattern::TraitType(integer_type_id)];
    let case_type = CaseType::Trait(display_trait_id);
    let span = Span::new(20, 50);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type.clone(), &covered_patterns, span)
        .unwrap();

    // Should be missing String
    match &result {
        ExhaustivenessResult::Missing(missing) => {
            assert_eq!(missing.len(), 1);
            // The missing type should be String
            assert_eq!(context.get_type_name(missing[0].type_id), Some("String"));
        }
        _ => panic!("Expected missing implementations for incomplete trait case"),
    }

    // Test error conversion
    let error = result.to_case_error(case_type, &context, span);
    assert!(error.is_some());

    match error.unwrap() {
        crate::error::TypeError::CaseNotExhaustive {
            trait_name,
            missing_types,
            ..
        } => {
            assert_eq!(trait_name, "Display");
            assert_eq!(missing_types, "String");
        }
        _ => panic!("Expected CaseNotExhaustive error for trait case"),
    }
}

#[test]
fn test_function_guard_exhaustiveness_analysis() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    // Test function guard exhaustiveness (currently returns OpenType as placeholder)
    let result = analyzer
        .analyze_function_guard_exhaustiveness(
            "test_function",
            &[],   // Empty guards for now
            false, // No default case
            Span::new(0, 30),
        )
        .unwrap();

    // Should require default case for exhaustiveness
    assert_eq!(result, ExhaustivenessResult::OpenType);

    // Test error conversion
    let error = result.to_function_error("test_function", Span::new(0, 30));
    assert!(error.is_some());

    match error.unwrap() {
        crate::error::TypeError::FunctionNotExhaustive {
            function_name,
            missing_cases,
            ..
        } => {
            assert_eq!(function_name, "test_function");
            assert_eq!(missing_cases, vec!["default case (function without guard)"]);
        }
        _ => panic!("Expected FunctionNotExhaustive error"),
    }
}
