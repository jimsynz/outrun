//! Property-based tests for typechecker v3
//!
//! These tests use the proptest framework to generate random inputs and verify
//! that the typechecker maintains certain invariants and properties across
//! all possible inputs.

use crate::{typecheck_program, TypeInferenceEngine, types::Type};
use outrun_parser::parse_program;
use proptest::prelude::*;

/// Configuration for property-based tests
/// Runs fewer cases during development, full cases in CI
fn proptest_config() -> ProptestConfig {
    if std::env::var("CI").is_ok() {
        // Full test suite in CI
        ProptestConfig::default()
    } else {
        // Faster tests for development
        ProptestConfig {
            cases: 10, // Reduced from default 256
            max_shrink_iters: 100, // Reduced from default 1024
            ..ProptestConfig::default()
        }
    }
}

// Property: Type variable generation should always produce unique IDs
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_type_variable_uniqueness(count in 1usize..1000) {
        let mut engine = TypeInferenceEngine::new();
        let mut generated_vars = std::collections::HashSet::new();
        
        for _ in 0..count {
            let var = engine.fresh_type_var();
            prop_assert!(
                generated_vars.insert(var),
                "Type variable {:?} was generated twice",
                var
            );
        }
    }
}

// Property: Type variable generation should be deterministic within a session
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_type_variable_deterministic_sequence(count in 1usize..100) {
        let mut engine1 = TypeInferenceEngine::new();
        let mut engine2 = TypeInferenceEngine::new();
        
        for _ in 0..count {
            let var1 = engine1.fresh_type_var();
            let var2 = engine2.fresh_type_var();
            prop_assert_eq!(var1, var2, "Type variable sequences should match");
        }
    }
}

// Property: Typechecker should never panic on any parseable input
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_typechecker_never_panics_on_literals(
        int_val in any::<i64>(),
        float_val in any::<f64>(),
        bool_val in any::<bool>(),
        string_val in ".*"
    ) {
        // Test integer literals
        let int_source = format!("let x = {}", int_val);
        if let Ok(mut program) = parse_program(&int_source) {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program)
            }));
            prop_assert!(result.is_ok(), "Typechecker panicked on integer literal: {}", int_val);
        }
        
        // Test float literals (only if finite)
        if float_val.is_finite() {
            let float_source = format!("let x = {}", float_val);
            if let Ok(mut program) = parse_program(&float_source) {
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    typecheck_program(&mut program)
                }));
                prop_assert!(result.is_ok(), "Typechecker panicked on float literal: {}", float_val);
            }
        }
        
        // Test boolean literals
        let bool_source = format!("let x = {}", bool_val);
        if let Ok(mut program) = parse_program(&bool_source) {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program)
            }));
            prop_assert!(result.is_ok(), "Typechecker panicked on boolean literal: {}", bool_val);
        }
        
        // Test string literals (with basic escaping)
        let escaped_string = string_val.replace('\"', "\\\"").replace('\n', "\\n");
        let string_source = format!("let x = \"{}\"", escaped_string);
        if let Ok(mut program) = parse_program(&string_source) {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program)
            }));
            prop_assert!(result.is_ok(), "Typechecker panicked on string literal: {}", escaped_string);
        }
    }
}

// Property: Homogeneous lists should always infer consistently
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_homogeneous_list_inference(elements in prop::collection::vec(0i32..100, 1..20)) {
        let elements_str: Vec<String> = elements.iter().map(|x| x.to_string()).collect();
        let source = format!("let list = [{}]", elements_str.join(", "));
        
        if let Ok(mut program) = parse_program(&source) {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program)
            }));
            
            prop_assert!(result.is_ok(), "Typechecker panicked on homogeneous list");
            
            // If typechecking succeeds, the list should be well-typed
            // (We can't easily check the exact type without more introspection)
        }
    }
}

// Property: Type concrete creation should handle various names
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_concrete_type_creation(type_name in "[A-Za-z][A-Za-z0-9_]*") {
        let concrete_type = Type::concrete(&type_name);
        
        // Should successfully create a concrete type
        match concrete_type {
            Type::Concrete { id, args, .. } => {
                prop_assert_eq!(id.name(), type_name);
                prop_assert_eq!(args.len(), 0);
            }
            _ => prop_assert!(false, "Should create concrete type"),
        }
    }
}

// Property: Arithmetic expressions should be deterministic
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_arithmetic_determinism(
        a in 0i32..1000, 
        b in 0i32..1000,
        op in prop::sample::select(vec!["+", "-", "*"])
    ) {
        let source = format!("let result = {} {} {}", a, op, b);
        
        if let Ok(mut program1) = parse_program(&source) {
            if let Ok(mut program2) = parse_program(&source) {
                let result1 = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    typecheck_program(&mut program1)
                }));
                
                let result2 = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    typecheck_program(&mut program2)
                }));
                
                // Both should succeed or fail in the same way
                prop_assert_eq!(result1.is_ok(), result2.is_ok(), 
                    "Typechecking should be deterministic for: {}", source);
            }
        }
    }
}

// Property: Variable names should not affect type inference (beyond resolution)
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_variable_name_independence(
        var_name in "[a-z][a-z0-9_]*",
        value in 0i32..100
    ) {
        let source = format!("let {} = {}", var_name, value);
        
        if let Ok(mut program) = parse_program(&source) {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program)
            }));
            
            prop_assert!(result.is_ok(), "Variable name '{}' caused panic", var_name);
        }
    }
}

// Property: Nested collections maintain type consistency  
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_nested_collection_consistency(
        depth in 1usize..5,
        base_value in 0i32..100
    ) {
        // Create nested list: [[[base_value]]]
        let mut nested = base_value.to_string();
        for _ in 0..depth {
            nested = format!("[{}]", nested);
        }
        
        let source = format!("let nested = {}", nested);
        
        if let Ok(mut program) = parse_program(&source) {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program)
            }));
            
            prop_assert!(result.is_ok(), "Nested collection with depth {} caused panic", depth);
        }
    }
}

// Property: Error handling should be consistent
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_error_consistency(
        undefined_var in "[a-z][a-z0-9_]*",
        valid_value in 0i32..100
    ) {
        // Create program that references undefined variable
        let source = format!("let result = {} + {}", undefined_var, valid_value);
        
        if let Ok(program) = parse_program(&source) {
            let result1 = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program.clone())
            }));
            
            let result2 = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program.clone())
            }));
            
            prop_assert!(result1.is_ok(), "Error handling caused panic");
            prop_assert!(result2.is_ok(), "Error handling caused panic on second run");
            
            // Both runs should produce the same result (both should typically fail)
            match (result1.unwrap(), result2.unwrap()) {
                (Ok(_), Ok(_)) => {}, // Both succeeded
                (Err(_), Err(_)) => {}, // Both failed (expected)
                _ => prop_assert!(false, "Inconsistent error handling"),
            }
        }
    }
}

// Property: Large programs should not cause stack overflow
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_large_program_stack_safety(var_count in 10usize..100) {
        let mut source = String::new();
        for i in 0..var_count {
            source.push_str(&format!("let var_{} = {}\n", i, i));
        }
        
        if let Ok(mut program) = parse_program(&source) {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program)
            }));
            
            prop_assert!(result.is_ok(), "Large program with {} variables caused panic", var_count);
        }
    }
}

// Property: Type inference should be idempotent (running twice gives same result)
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_type_inference_idempotent(value in 0i32..1000) {
        let source = format!("let x = {}", value);
        
        if let Ok(mut program1) = parse_program(&source) {
            if let Ok(mut program2) = parse_program(&source) {
                let result1 = typecheck_program(&mut program1);
                let result2 = typecheck_program(&mut program2);
                
                // Both should succeed or fail consistently
                prop_assert_eq!(
                    result1.is_ok(), 
                    result2.is_ok(),
                    "Type inference should be idempotent"
                );
            }
        }
    }
}

// Property: String escaping should not break type inference
proptest! {
    #![proptest_config(proptest_config())]
    #[test]
    fn test_string_escaping_robustness(content in ".*") {
        // Basic escaping for common problematic characters
        let escaped = content
            .replace('\\', "\\\\")
            .replace('\"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t");
        
        let source = format!("let s = \"{}\"", escaped);
        
        if let Ok(mut program) = parse_program(&source) {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                typecheck_program(&mut program)
            }));
            
            prop_assert!(result.is_ok(), "String escaping caused panic with content: {}", escaped);
        }
    }
}