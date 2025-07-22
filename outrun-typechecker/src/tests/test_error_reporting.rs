//! Tests for enhanced error reporting system
//!
//! These tests verify that the typechecker produces helpful error messages with
//! source location information, suggestions, and contextual information.

use crate::{
    error::{ErrorContext, InferenceError, UnificationError},
    types::{Type},
};
use outrun_parser::{parse_program, Span};

#[test]
fn test_error_context_similar_name_suggestions() {
    let mut context = ErrorContext::new();
    context.available_variables = vec![
        "username".to_string(),
        "user_name".to_string(),
        "userName".to_string(),
        "completely_different".to_string(),
    ];

    // Test close match
    let (similar, ctx) = context.suggest_for_undefined_variable("usrname");
    assert!(!similar.is_empty());
    assert!(similar.contains(&"username".to_string()));
    assert!(ctx.is_some());
    assert!(ctx.unwrap().contains("Did you mean"));

    // Test no close match
    let (similar, ctx) = context.suggest_for_undefined_variable("xyz");
    assert!(similar.is_empty());
    assert!(ctx.is_some());
    assert!(ctx.unwrap().contains("Make sure"));
}

#[test]
fn test_levenshtein_distance_calculation() {
    use crate::error::ErrorContext;
    
    let context = ErrorContext::new();
    
    // Test exact matches and close matches
    let candidates = vec!["hello".to_string(), "world".to_string(), "help".to_string()];
    
    let similar = context.find_similar_names("helo", &candidates);
    assert!(similar.contains(&"hello".to_string()));
    assert!(similar.contains(&"help".to_string()));
    
    let similar = context.find_similar_names("word", &candidates);
    assert!(similar.contains(&"world".to_string()));
}

#[test]
fn test_undefined_variable_with_suggestions() {
    // This test demonstrates the error reporting infrastructure
    // The actual variable lookup might not trigger in simple cases yet
    let source = "undefined_var";
    let _program_result = parse_program(source);
    
    // For now, just verify we can create enhanced error structures
    let error = InferenceError::undefined_variable_with_suggestions(
        "undefined_var".to_string(),
        Some(outrun_parser::Span::new(0, 12)),
        &ErrorContext::new(),
    );
    
    match error {
        InferenceError::UndefinedVariable { variable_name, similar_names, context, .. } => {
            assert_eq!(variable_name, "undefined_var");
            assert!(similar_names.is_empty() || context.is_some());
        }
        _ => panic!("Expected UndefinedVariable error"),
    }
}

#[test]
fn test_undefined_variable_with_similar_names() {
    // Test the similarity detection directly
    let mut context = ErrorContext::new();
    context.available_variables = vec![
        "my_variable".to_string(),
        "my_var".to_string(),
        "other_var".to_string(),
    ];
    
    let error = InferenceError::undefined_variable_with_suggestions(
        "my_variabel".to_string(),
        Some(outrun_parser::Span::new(0, 11)),
        &context,
    );
    
    match error {
        InferenceError::UndefinedVariable { variable_name, similar_names, context, .. } => {
            assert_eq!(variable_name, "my_variabel");
            assert!(similar_names.contains(&"my_variable".to_string()));
            assert!(context.is_some());
            assert!(context.unwrap().contains("Did you mean"));
        }
        _ => panic!("Expected UndefinedVariable error"),
    }
}

#[test]
fn test_type_mismatch_error_with_context() {
    let expected = Type::concrete("Integer64");
    let found = Type::concrete("String");
    let span = Some(Span::new(0, 10));
    
    let error = UnificationError::type_mismatch_with_context(
        expected.clone(),
        found.clone(),
        Some("function parameter".to_string()),
        Some("string literal".to_string()),
        span,
    );
    
    match error {
        UnificationError::TypeMismatch { expected: exp, found: fnd, expected_context, found_context, .. } => {
            assert_eq!(exp, expected);
            assert_eq!(fnd, found);
            assert_eq!(expected_context, Some("function parameter".to_string()));
            assert_eq!(found_context, Some("string literal".to_string()));
        }
        _ => panic!("Expected TypeMismatch error"),
    }
}

#[test]
fn test_empty_collection_needs_annotation_error() {
    let error = InferenceError::empty_collection_needs_annotation(
        "List".to_string(),
        Some(Span::new(5, 7)),
    );
    
    match error {
        InferenceError::EmptyCollectionNeedsAnnotation { collection_type, examples, .. } => {
            assert_eq!(collection_type, "List");
            assert!(!examples.is_empty());
            assert!(examples.iter().any(|ex| ex.contains("List<Integer64>")));
            assert!(examples.iter().any(|ex| ex.contains("List<String>")));
        }
        _ => panic!("Expected EmptyCollectionNeedsAnnotation error"),
    }
}

#[test]
fn test_function_call_error_with_suggestions() {
    let suggestions = vec![
        "Check parameter types".to_string(),
        "Consider using type conversion".to_string(),
    ];
    
    let error = InferenceError::function_call_error_with_suggestions(
        "Parameter type mismatch".to_string(),
        Some("calculate".to_string()),
        Some("calculate(x: Integer64, y: Integer64) -> Integer64".to_string()),
        Some("calculate(a: String, b: Integer64)".to_string()),
        Some(Span::new(10, 25)),
        suggestions.clone(),
    );
    
    match error {
        InferenceError::FunctionCallError { 
            message, function_name, expected_signature, actual_arguments, suggestions: sug, .. 
        } => {
            assert_eq!(message, "Parameter type mismatch");
            assert_eq!(function_name, Some("calculate".to_string()));
            assert!(expected_signature.is_some());
            assert!(actual_arguments.is_some());
            assert_eq!(sug, suggestions);
        }
        _ => panic!("Expected FunctionCallError"),
    }
}

#[test]
fn test_collection_type_mismatch_error() {
    let expected_type = Type::concrete("Integer64");
    let found_type = Type::concrete("String");
    
    let error = InferenceError::collection_type_mismatch(
        "List elements must have the same type".to_string(),
        "List".to_string(),
        Some(expected_type.clone()),
        Some(found_type.clone()),
        Some(Span::new(0, 15)),
        Some(Span::new(5, 7)),
        Some(Span::new(10, 17)),
    );
    
    match error {
        InferenceError::CollectionMismatch { 
            message, collection_type, expected_element_type, found_element_type, .. 
        } => {
            assert_eq!(message, "List elements must have the same type");
            assert_eq!(collection_type, "List");
            assert_eq!(expected_element_type, Some(expected_type));
            assert_eq!(found_element_type, Some(found_type));
        }
        _ => panic!("Expected CollectionMismatch error"),
    }
}

#[test]
fn test_error_context_type_suggestions() {
    let mut context = ErrorContext::new();
    context.available_types = vec![
        "String".to_string(),
        "Integer64".to_string(),
        "Boolean".to_string(),
        "CustomType".to_string(),
    ];

    // Test typo in common type
    let (similar, ctx) = context.suggest_for_undefined_type("Stirng");
    assert!(similar.contains(&"String".to_string()));
    assert!(ctx.unwrap().contains("Did you mean"));

    // Test case variation
    let (similar, ctx) = context.suggest_for_undefined_type("boolean");
    assert!(similar.contains(&"Boolean".to_string()));
    assert!(ctx.unwrap().contains("Did you mean"));
}

#[test] 
fn test_error_context_protocol_suggestions() {
    let mut context = ErrorContext::new();
    context.available_protocols = vec![
        "BinaryAddition".to_string(),
        "Equality".to_string(),
        "ToString".to_string(),
        "Comparable".to_string(),
    ];

    // Test typo in protocol name
    let (similar, ctx) = context.suggest_for_undefined_protocol("BinaryAdition");
    assert!(similar.contains(&"BinaryAddition".to_string()));
    assert!(ctx.unwrap().contains("Did you mean"));

    // Test completely different name - should suggest checking imports
    let (similar, ctx) = context.suggest_for_undefined_protocol("NonExistentProtocol");
    assert!(similar.is_empty());
    assert!(ctx.unwrap().contains("Import the protocol"));
}

#[test]
fn test_error_context_integration_with_symbol_table() {
    use crate::TypeInferenceEngine;
    
    let mut engine = TypeInferenceEngine::new();
    
    // Simulate adding some variables to symbol table
    engine.bind_variable("count", Type::concrete("Integer64"));
    engine.bind_variable("name", Type::concrete("String"));
    engine.bind_variable("is_active", Type::concrete("Boolean"));
    
    // Test that the error context gets updated correctly
    // For now, just verify the engine can bind variables and update context
    assert!(engine.symbol_table.contains_key("count"));
    assert!(engine.symbol_table.contains_key("name"));
    assert!(engine.symbol_table.contains_key("is_active"));
}