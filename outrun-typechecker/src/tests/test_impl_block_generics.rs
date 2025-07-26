//! Test for Phase 3.6: Generic context capture for impl block functions
//!
//! Tests that impl block functions properly capture generic parameters from their
//! containing impl block context, including type variables from the impl syntax.

use crate::dispatch::FunctionRegistry;
use crate::inference::TypeInferenceEngine;
use crate::types::ModuleId;
use outrun_parser::parse_program;

#[test]
fn test_impl_block_with_same_type_variables() {
    let mut engine = TypeInferenceEngine::new();
    engine.set_current_module(ModuleId::new("TestModule"));
    
    // Test: impl Display<T> for Wrapper<T> - T should be shared generic parameter
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String;
        }
        
        struct Wrapper<T> {
            value: T
        }
        
        impl Display<T> for Wrapper<T> {
            def display(value: T): String {
                "wrapped"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Process the program
    let result = engine.process_package(&mut [program]);
    assert!(result.is_ok(), "Type checking should succeed: {:?}", result.err());
    
    // Check that the impl block function was registered with generic parameters
    let registry = engine.get_function_registry();
    let impl_scope = "impl Display for Wrapper";
    let function_info = registry.get_function(impl_scope, "display");
    
    assert!(function_info.is_some(), "display function should be registered in impl scope");
    let function_info = function_info.unwrap();
    
    // Should have generic parameter T (appears in both Display<T> and Wrapper<T>)
    assert!(!function_info.generic_parameters.is_empty(), "Function should have generic parameters");
    assert!(function_info.generic_parameters.contains(&"T".to_string()), 
           "Function should have T as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.is_generic, "Function should be marked as generic");
}

#[test]
fn test_impl_block_with_different_type_variables() {
    let mut engine = TypeInferenceEngine::new();
    engine.set_current_module(ModuleId::new("TestModule"));
    
    // Test: impl Display<A> for Wrapper<B> - A and B should be separate generic parameters
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String;
        }
        
        struct Wrapper<U> {
            value: U
        }
        
        impl Display<A> for Wrapper<B> {
            def convert(from: B): A {
                // Mock conversion
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Process the program
    let result = engine.process_package(&mut [program]);
    assert!(result.is_ok(), "Type checking should succeed: {:?}", result.err());
    
    // Check that the impl block function was registered with both generic parameters
    let registry = engine.get_function_registry();
    let impl_scope = "impl Display for Wrapper";
    let function_info = registry.get_function(impl_scope, "convert");
    
    assert!(function_info.is_some(), "convert function should be registered in impl scope");
    let function_info = function_info.unwrap();
    
    // Should have both A and B as generic parameters
    assert!(!function_info.generic_parameters.is_empty(), "Function should have generic parameters");
    assert!(function_info.generic_parameters.contains(&"A".to_string()), 
           "Function should have A as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.generic_parameters.contains(&"B".to_string()), 
           "Function should have B as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.is_generic, "Function should be marked as generic");
}

#[test]
fn test_impl_block_with_concrete_types() {
    let mut engine = TypeInferenceEngine::new();
    engine.set_current_module(ModuleId::new("TestModule"));
    
    // Test: impl Display<String> for Wrapper<Integer64> - no generic parameters
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String;
        }
        
        struct Wrapper<U> {
            value: U
        }
        
        impl Display<String> for Wrapper<Integer64> {
            def display(value: String): String {
                value
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Process the program
    let result = engine.process_package(&mut [program]);
    assert!(result.is_ok(), "Type checking should succeed: {:?}", result.err());
    
    // Check that the impl block function was registered without generic parameters
    let registry = engine.get_function_registry();
    let impl_scope = "impl Display for Wrapper";
    let function_info = registry.get_function(impl_scope, "display");
    
    assert!(function_info.is_some(), "display function should be registered in impl scope");
    let function_info = function_info.unwrap();
    
    // Should have no generic parameters (all concrete types)
    assert!(function_info.generic_parameters.is_empty(), 
           "Function should have no generic parameters: {:?}", function_info.generic_parameters);
    assert!(!function_info.is_generic, "Function should not be marked as generic");
}

#[test]
fn test_impl_block_nested_generics() {
    let mut engine = TypeInferenceEngine::new();
    engine.set_current_module(ModuleId::new("TestModule"));
    
    // Test: impl Display<Option<T>> for Wrapper<List<T>> - nested generics with shared T
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String;
        }
        
        struct Wrapper<U> {
            value: U
        }
        
        impl Display<Option<T>> for Wrapper<List<T>> {
            def unwrap_and_display(wrapper: List<T>): Option<T> {
                // Mock unwrap logic
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Process the program
    let result = engine.process_package(&mut [program]);
    assert!(result.is_ok(), "Type checking should succeed: {:?}", result.err());
    
    // Check that the impl block function was registered with T as generic parameter
    let registry = engine.get_function_registry();
    let impl_scope = "impl Display for Wrapper";
    let function_info = registry.get_function(impl_scope, "unwrap_and_display");
    
    assert!(function_info.is_some(), "unwrap_and_display function should be registered in impl scope");
    let function_info = function_info.unwrap();
    
    // Should extract T from nested Option<T> and List<T>
    assert!(!function_info.generic_parameters.is_empty(), "Function should have generic parameters");
    assert!(function_info.generic_parameters.contains(&"T".to_string()), 
           "Function should have T as generic parameter: {:?}", function_info.generic_parameters);
    assert!(function_info.is_generic, "Function should be marked as generic");
}