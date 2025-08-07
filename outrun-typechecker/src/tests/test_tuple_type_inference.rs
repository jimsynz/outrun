//! Tests for tuple type inference in expressions
//!
//! Tests that tuple literals correctly infer their types based on element types.

use crate::{
    inference::{InferenceContext, TypeInferenceEngine},
    types::Type,
};
use outrun_parser::parse_expression;

/// Helper to create a test TypeInferenceEngine
fn create_test_engine() -> TypeInferenceEngine {
    TypeInferenceEngine::new()
}

/// Helper to create a test InferenceContext  
fn create_test_context() -> InferenceContext {
    InferenceContext::new()
}

#[test]
fn test_simple_tuple_inference() {
    let mut engine = create_test_engine();
    let mut context = create_test_context();
    
    // Parse tuple expression: (42, "hello")
    let input = r#"(42, "hello")"#;
    let expr = parse_expression(input).expect("Should parse tuple");
    let mut expr_mut = expr.clone();
    
    let result = engine.infer_expression(&mut expr_mut, &mut context).unwrap();
    
    // Should infer (Integer64, String)
    match &result.inferred_type {
        Type::Tuple { element_types, .. } => {
            assert_eq!(element_types.len(), 2);
            assert_eq!(element_types[0], Type::concrete("Outrun.Core.Integer64"));
            assert_eq!(element_types[1], Type::concrete("Outrun.Core.String"));
        }
        other => panic!("Expected Tuple type, got: {:?}", other),
    }
}

#[test]
fn test_empty_tuple_inference() {
    let mut engine = create_test_engine();
    let mut context = create_test_context();
    
    // Parse empty tuple expression: ()
    let input = "()";
    let expr = parse_expression(input).expect("Should parse empty tuple");
    let mut expr_mut = expr.clone();
    
    let result = engine.infer_expression(&mut expr_mut, &mut context).unwrap();
    
    // Should infer ()
    match &result.inferred_type {
        Type::Tuple { element_types, .. } => {
            assert_eq!(element_types.len(), 0);
        }
        other => panic!("Expected Tuple type, got: {:?}", other),
    }
}

#[test]
fn test_nested_tuple_inference() {
    let mut engine = create_test_engine();
    let mut context = create_test_context();
    
    // Parse nested tuple expression: ((1, 2), "test")
    let input = r#"((1, 2), "test")"#;
    let expr = parse_expression(input).expect("Should parse nested tuple");
    let mut expr_mut = expr.clone();
    
    let result = engine.infer_expression(&mut expr_mut, &mut context).unwrap();
    
    // Should infer ((Integer64, Integer64), String)
    match &result.inferred_type {
        Type::Tuple { element_types, .. } => {
            assert_eq!(element_types.len(), 2);
            
            // First element should be (Integer64, Integer64)
            match &element_types[0] {
                Type::Tuple { element_types: inner_types, .. } => {
                    assert_eq!(inner_types.len(), 2);
                    assert_eq!(inner_types[0], Type::concrete("Outrun.Core.Integer64"));
                    assert_eq!(inner_types[1], Type::concrete("Outrun.Core.Integer64"));
                }
                other => panic!("Expected inner Tuple type, got: {:?}", other),
            }
            
            // Second element should be String
            assert_eq!(element_types[1], Type::concrete("Outrun.Core.String"));
        }
        other => panic!("Expected Tuple type, got: {:?}", other),
    }
}

#[test]
fn test_single_element_tuple_inference() {
    let mut engine = create_test_engine();
    let mut context = create_test_context();
    
    // Parse single-element tuple expression: (42,)
    let input = "(42,)";
    let expr = parse_expression(input).expect("Should parse single-element tuple");
    let mut expr_mut = expr.clone();
    
    let result = engine.infer_expression(&mut expr_mut, &mut context).unwrap();
    
    // Should infer (Integer64,)
    match &result.inferred_type {
        Type::Tuple { element_types, .. } => {
            assert_eq!(element_types.len(), 1);
            assert_eq!(element_types[0], Type::concrete("Outrun.Core.Integer64"));
        }
        other => panic!("Expected Tuple type, got: {:?}", other),
    }
}

#[test]
fn test_mixed_type_tuple_inference() {
    let mut engine = create_test_engine();
    let mut context = create_test_context();
    
    // Parse mixed tuple expression: (true, 3.14, "world")
    let input = r#"(true, 3.14, "world")"#;
    let expr = parse_expression(input).expect("Should parse mixed tuple");
    let mut expr_mut = expr.clone();
    
    let result = engine.infer_expression(&mut expr_mut, &mut context).unwrap();
    
    // Should infer (Boolean, Float64, String)
    match &result.inferred_type {
        Type::Tuple { element_types, .. } => {
            assert_eq!(element_types.len(), 3);
            assert_eq!(element_types[0], Type::concrete("Outrun.Core.Boolean"));
            assert_eq!(element_types[1], Type::concrete("Outrun.Core.Float64"));
            assert_eq!(element_types[2], Type::concrete("Outrun.Core.String"));
        }
        other => panic!("Expected Tuple type, got: {:?}", other),
    }
}