//! Integration tests for exhaustiveness analysis

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

    let covered_patterns = vec![CoveredPattern::Boolean(true)];
    let boolean_type_id = context.interner.get_type("Outrun.Core.Boolean").unwrap();
    let case_type = CaseType::Pattern(boolean_type_id);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type, &covered_patterns, Span::new(0, 20))
        .unwrap();

    assert!(
        matches!(result, ExhaustivenessResult::Missing(ref missing) if missing.len() == 1),
        "Expected missing patterns for incomplete boolean case, got {:?}",
        result
    );

    if let ExhaustivenessResult::Missing(missing) = result {
        assert_eq!(missing[0].description, "Boolean value 'false'");
        assert_eq!(missing[0].example_pattern, "false");
    }
}

#[test]
fn test_boolean_case_complete_exhaustiveness() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    let covered_patterns = vec![
        CoveredPattern::Boolean(true),
        CoveredPattern::Boolean(false),
    ];
    let boolean_type_id = context.interner.get_type("Outrun.Core.Boolean").unwrap();
    let case_type = CaseType::Pattern(boolean_type_id);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type, &covered_patterns, Span::new(0, 20))
        .unwrap();

    assert_eq!(result, ExhaustivenessResult::Exhaustive);
}

#[test]
fn test_trait_based_option_exhaustiveness() {
    let mut context = setup_test_context();

    let option_trait_id = context.interner.intern_trait("Option");
    let option_some_type_id = context.interner.intern_type("Outrun.Option.Some");
    let option_none_type_id = context.interner.intern_type("Outrun.Option.None");

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

    let analyzer = ExhaustivenessAnalyzer::new(&context);

    let covered_patterns = vec![CoveredPattern::TraitType(option_some_type_id)];
    let case_type = CaseType::Trait(option_trait_id);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type, &covered_patterns, Span::new(0, 30))
        .unwrap();

    assert!(
        matches!(result, ExhaustivenessResult::Missing(ref missing) if missing.len() == 1),
        "Expected missing Option.None for incomplete Option trait case, got {:?}",
        result
    );

    if let ExhaustivenessResult::Missing(missing) = result {
        assert_eq!(
            context.get_type_name(missing[0].type_id),
            Some("Outrun.Option.None")
        );
    }
}

#[test]
fn test_infinite_type_exhaustiveness() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    let string_type_id = context.interner.get_type("Outrun.Core.String").unwrap();
    let case_type = CaseType::Pattern(string_type_id);
    let covered_patterns = vec![];

    let result = analyzer
        .analyze_case_exhaustiveness(case_type, &covered_patterns, Span::new(0, 20))
        .unwrap();

    assert_eq!(result, ExhaustivenessResult::OpenType);
}

#[test]
fn test_error_conversion_boolean_not_exhaustive() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    let covered_patterns = vec![CoveredPattern::Boolean(true)];
    let boolean_type_id = context.interner.get_type("Outrun.Core.Boolean").unwrap();
    let case_type = CaseType::Pattern(boolean_type_id);
    let span = Span::new(10, 30);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type.clone(), &covered_patterns, span)
        .unwrap();

    let error = result.to_case_error(case_type, &context, span);
    assert!(error.is_some());

    assert!(
        matches!(
            error.unwrap(),
            crate::error::TypeError::BooleanNotExhaustive { ref missing_values, .. }
            if missing_values == &vec!["false"]
        ),
        "Expected BooleanNotExhaustive error with missing 'false'"
    );
}

#[test]
fn test_error_conversion_open_type() {
    let context = setup_test_context();

    let string_type_id = context.interner.get_type("Outrun.Core.String").unwrap();
    let case_type = CaseType::Pattern(string_type_id);
    let span = Span::new(15, 35);

    let result = ExhaustivenessResult::OpenType;

    let error = result.to_case_error(case_type, &context, span);
    assert!(error.is_some());

    assert!(
        matches!(
            error.unwrap(),
            crate::error::TypeError::NonExhaustivePatterns { ref missing_cases, .. }
            if missing_cases == &vec!["catch-all pattern (_)"]
        ),
        "Expected NonExhaustivePatterns error with catch-all pattern"
    );
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
    assert!(
        matches!(
            error.unwrap(),
            crate::error::TypeError::NonExhaustivePatterns { ref missing_cases, .. }
            if missing_cases == &vec!["catch-all pattern (_)"]
        ),
        "Expected NonExhaustivePatterns error for open type"
    );
}

#[test]
fn test_trait_case_exhaustiveness_with_error_conversion() {
    let mut context = setup_test_context();

    let display_trait_id = context.interner.intern_trait("Display");
    let integer_type_id = context.interner.intern_type("Integer");
    let string_type_id = context.interner.intern_type("String");

    use crate::types::traits::{TraitDefinition, TraitImplementation};
    use std::collections::HashMap;

    let trait_def = TraitDefinition::new(
        display_trait_id,
        "Display".to_string(),
        vec![],
        Span::new(0, 10),
    );
    context.trait_registry.register_trait(trait_def);

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

    let analyzer = ExhaustivenessAnalyzer::new(&context);

    let covered_patterns = vec![CoveredPattern::TraitType(integer_type_id)];
    let case_type = CaseType::Trait(display_trait_id);
    let span = Span::new(20, 50);

    let result = analyzer
        .analyze_case_exhaustiveness(case_type.clone(), &covered_patterns, span)
        .unwrap();

    assert!(
        matches!(result, ExhaustivenessResult::Missing(ref missing) if missing.len() == 1),
        "Expected missing implementations for incomplete trait case, got {:?}",
        result
    );

    if let ExhaustivenessResult::Missing(ref missing) = result {
        assert_eq!(context.get_type_name(missing[0].type_id), Some("String"));
    }

    let error = result.to_case_error(case_type, &context, span);
    assert!(error.is_some());

    assert!(
        matches!(
            error.unwrap(),
            crate::error::TypeError::CaseNotExhaustive {
                ref trait_name,
                ref missing_types,
                ..
            } if trait_name == "Display" && missing_types == "String"
        ),
        "Expected CaseNotExhaustive error for trait case"
    );
}

#[test]
fn test_function_guard_exhaustiveness_analysis() {
    let context = setup_test_context();
    let analyzer = ExhaustivenessAnalyzer::new(&context);

    let result = analyzer
        .analyze_function_guard_exhaustiveness("test_function", &[], false, Span::new(0, 30))
        .unwrap();

    assert_eq!(result, ExhaustivenessResult::OpenType);

    let error = result.to_function_error("test_function", Span::new(0, 30));
    assert!(error.is_some());

    assert!(
        matches!(
            error.unwrap(),
            crate::error::TypeError::FunctionNotExhaustive {
                ref function_name,
                ref missing_cases,
                ..
            } if function_name == "test_function" && missing_cases == &vec!["default case (function without guard)"]
        ),
        "Expected FunctionNotExhaustive error"
    );
}
