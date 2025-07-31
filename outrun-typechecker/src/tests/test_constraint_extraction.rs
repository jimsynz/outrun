//! Test for constraint expression type variable extraction
//!
//! Tests that type variables from constraint expressions (when clauses) are properly extracted
//! in impl blocks using our semantic approach.

use crate::inference::TypeInferenceEngine;
use crate::types::ModuleName;
use outrun_parser::parse_program;

#[test]
fn test_constraint_expression_extraction_simple() {
    let mut engine = TypeInferenceEngine::new();
    engine.set_current_module(ModuleName::new("TestModule"));
    
    // Test: impl Display<T> for Wrapper<U> when T: Debug - both T and U should be extracted
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String;
        }
        
        protocol Debug {
            def debug(value: Self): String;
        }
        
        struct Wrapper<U> {
            value: U
        }
        
        impl Display<T> for Wrapper<U> when T: Debug {
            def display(value: T): String {
                Debug.debug(value: value)
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Process the program
    let result = engine.typecheck_program(&mut program);
    assert!(result.is_ok(), "Type checking should succeed: {:?}", result.err());
    
    // Check that the impl block function was registered with both T and U as generic parameters
    let registry = engine.get_function_registry();
    let impl_scope = "impl Display for Wrapper";
    let function_info = registry.get_function(impl_scope, "display");
    
    assert!(function_info.is_some(), "display function should be registered in impl scope");
    let function_info = function_info.unwrap();
    
    // Should have both T (from constraint) and U (from Wrapper<U>) as generic parameters
    assert!(!function_info.generic_parameters.is_empty(), "Function should have generic parameters");
    assert!(function_info.generic_parameters.contains(&"T".to_string()), 
           "Function should have T as generic parameter from constraint: {:?}", function_info.generic_parameters);
    assert!(function_info.generic_parameters.contains(&"U".to_string()), 
           "Function should have U as generic parameter from type spec: {:?}", function_info.generic_parameters);
    assert!(function_info.is_generic, "Function should be marked as generic");
}

#[test]
fn test_constraint_expression_extraction_complex() {
    let mut engine = TypeInferenceEngine::new();
    engine.set_current_module(ModuleName::new("TestModule"));
    
    // Test: impl Converter<A, B> for Adapter<C, D> when A: Debug && B: Display && C: Clone
    // Should extract A, B, C, D
    let program_text = r#"
        protocol Converter<T, U> {
            def convert(from: T): U;
        }
        
        protocol Debug {
            def debug(value: Self): String;
        }
        
        protocol Display {
            def display(value: Self): String;
        }
        
        protocol Clone {
            def clone(value: Self): Self;
        }
        
        struct Adapter<C, D> {
            input: C,
            output: D
        }
        
        impl Converter<A, B> for Adapter<C, D> when A: Debug && B: Display && C: Clone {
            def convert(from: A): B {
                // Mock conversion implementation
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Process the program
    let result = engine.typecheck_program(&mut program);
    assert!(result.is_ok(), "Type checking should succeed: {:?}", result.err());
    
    // Check that the impl block function was registered with A, B, C, D as generic parameters
    let registry = engine.get_function_registry();
    let impl_scope = "impl Converter for Adapter";
    let function_info = registry.get_function(impl_scope, "convert");
    
    assert!(function_info.is_some(), "convert function should be registered in impl scope");
    let function_info = function_info.unwrap();
    
    // Should have A, B (from protocol), C, D (from implementing type), and any from constraints
    assert!(!function_info.generic_parameters.is_empty(), "Function should have generic parameters");
    assert!(function_info.generic_parameters.contains(&"A".to_string()), 
           "Function should have A as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.generic_parameters.contains(&"B".to_string()), 
           "Function should have B as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.generic_parameters.contains(&"C".to_string()), 
           "Function should have C as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.generic_parameters.contains(&"D".to_string()), 
           "Function should have D as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.is_generic, "Function should be marked as generic");
}

#[test]
fn test_constraint_expression_extraction_nested() {
    let mut engine = TypeInferenceEngine::new();
    engine.set_current_module(ModuleName::new("TestModule"));
    
    // Test: impl Transform<Option<T>> for Container<U> when (T: Debug && U: Display)
    // Should extract T and U from constraints, plus the ones from type specs
    let program_text = r#"
        protocol Transform<T> {
            def transform(value: T): String;
        }
        
        protocol Debug {
            def debug(value: Self): String;
        }
        
        protocol Display {
            def display(value: Self): String;
        }
        
        struct Container<U> {
            value: U
        }
        
        impl Transform<Option<T>> for Container<U> when (T: Debug && U: Display) {
            def transform(value: Option<T>): String {
                "transformed"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Process the program
    let result = engine.typecheck_program(&mut program);
    assert!(result.is_ok(), "Type checking should succeed: {:?}", result.err());
    
    // Check that the impl block function was registered with T and U as generic parameters
    let registry = engine.get_function_registry();
    let impl_scope = "impl Transform for Container";
    let function_info = registry.get_function(impl_scope, "transform");
    
    assert!(function_info.is_some(), "transform function should be registered in impl scope");
    let function_info = function_info.unwrap();
    
    // Should extract T from Option<T> in protocol spec and from constraint T: Debug
    // Should extract U from Container<U> in type spec and from constraint U: Display
    assert!(!function_info.generic_parameters.is_empty(), "Function should have generic parameters");
    assert!(function_info.generic_parameters.contains(&"T".to_string()), 
           "Function should have T as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.generic_parameters.contains(&"U".to_string()), 
           "Function should have U as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.is_generic, "Function should be marked as generic");
}

#[test]
fn test_constraint_expression_no_duplicates() {
    let mut engine = TypeInferenceEngine::new();
    engine.set_current_module(ModuleName::new("TestModule"));
    
    // Test: impl Display<T> for Wrapper<T> when T: Debug
    // T appears in both type specs and constraint - should only appear once in generic_parameters
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String;
        }
        
        protocol Debug {
            def debug(value: Self): String;
        }
        
        struct Wrapper<T> {
            value: T
        }
        
        impl Display<T> for Wrapper<T> when T: Debug {
            def display(value: T): String {
                Debug.debug(value: value)
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Process the program
    let result = engine.typecheck_program(&mut program);
    assert!(result.is_ok(), "Type checking should succeed: {:?}", result.err());
    
    // Check that T appears only once in generic parameters
    let registry = engine.get_function_registry();
    let impl_scope = "impl Display for Wrapper";
    let function_info = registry.get_function(impl_scope, "display");
    
    assert!(function_info.is_some(), "display function should be registered in impl scope");
    let function_info = function_info.unwrap();
    
    // Should have T as generic parameter, but only once
    let t_count = function_info.generic_parameters.iter().filter(|&param| param == "T").count();
    assert_eq!(t_count, 1, "T should appear exactly once in generic parameters: {:?}", function_info.generic_parameters);
    assert!(function_info.generic_parameters.contains(&"T".to_string()), 
           "Function should have T as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.is_generic, "Function should be marked as generic");
}