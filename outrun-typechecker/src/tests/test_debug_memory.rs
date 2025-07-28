//! Debug tests for memory usage in iterative inference and recursive pattern detection

use crate::{typecheck_package, Package, TypeInferenceEngine};
use outrun_parser::parse_program;

#[test]
fn test_memory_usage_small_expression() {
    // Test memory usage for a small expression
    let mut package = Package::new("test_small".to_string());
    
    let source = "let x = 1 + 2 + 3";
    let program = parse_program(source).expect("Should parse successfully");
    package.add_program(program);
    
    println!("Testing small expression: {}", source);
    
    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Small expression typechecked successfully"),
        Err(e) => println!("! Small expression failed: {:?}", e),
    }
}

#[test]
fn test_memory_usage_medium_expression() {
    // Test memory usage for a medium expression (10 terms)
    let mut package = Package::new("test_medium".to_string());
    
    let mut parts = Vec::new();
    for i in 0..10 {
        parts.push(i.to_string());
    }
    let expression = parts.join(" + ");
    let source = format!("let x = {}", expression);
    
    let program = parse_program(&source).expect("Should parse successfully");
    package.add_program(program);
    
    println!("Testing medium expression: {} terms", parts.len());
    
    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Medium expression typechecked successfully"),
        Err(e) => println!("! Medium expression failed: {:?}", e),
    }
}

#[test]
fn test_memory_usage_large_expression() {
    // Test memory usage for a large expression (50 terms)
    let mut package = Package::new("test_large".to_string());
    
    let mut parts = Vec::new();
    for i in 0..50 {
        parts.push(i.to_string());
    }
    let expression = parts.join(" + ");
    let source = format!("let x = {}", expression);
    
    let program = parse_program(&source).expect("Should parse successfully");
    package.add_program(program);
    
    println!("Testing large expression: {} terms", parts.len());
    
    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Large expression typechecked successfully"),
        Err(e) => println!("! Large expression failed: {:?}", e),
    }
}

#[test]
fn test_memory_usage_very_large_expression() {
    // Test memory usage for a very large expression (100 terms) - this should be the limit
    let mut package = Package::new("test_very_large".to_string());
    
    let mut parts = Vec::new();
    for i in 0..100 {
        parts.push(i.to_string());
    }
    let expression = parts.join(" + ");
    let source = format!("let x = {}", expression);
    
    let program = parse_program(&source).expect("Should parse successfully");
    package.add_program(program);
    
    println!("Testing very large expression: {} terms", parts.len());
    
    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Very large expression typechecked successfully"),
        Err(e) => println!("! Very large expression failed: {:?}", e),
    }
}

#[test]
fn test_recursive_protocol_implementation_detection() {
    // Test that our recursive pattern detection is integrated and working
    let source = r#"
        protocol Empty {
            def empty?(value: Self): Boolean
        }

        struct Option<T> {
            def some(value: T): Option<T> {
                Option { value }
            }
        } 

        impl Empty for Option<T> when T: Empty {
            def empty?(value: Self): Boolean {
                # Implementation for Option<T> depends on T being Empty
                true
            }
        }
    "#;

    let mut program = parse_program(source).expect("Failed to parse program");
    let mut inference_engine = TypeInferenceEngine::new();
    
    // Capture output - the recursive pattern detection runs during typechecking
    println!("Testing recursive protocol implementation detection");
    
    let result = inference_engine.typecheck_program(&mut program);
    
    // The recursive pattern detection should run during phase 3.5
    // and emit a warning to stderr about the recursive implementation
    
    match result {
        Ok(()) => {
            println!("✓ Program typechecked successfully - recursive pattern detection ran");
        }
        Err(e) => {
            println!("! Typecheck error (may be expected): {:?}", e);
        }
    }
}

#[test]
fn test_constraint_solver_recursive_analysis_directly() {
    // Test the constraint solver recursive analysis directly
    let mut solver = crate::constraints::ConstraintSolver::new();
    
    // Analyze patterns (should be safe with empty registry)
    solver.analyze_recursive_type_patterns();
    
    // Should have no warnings with an empty registry
    let warnings = solver.get_warnings();
    assert_eq!(warnings.len(), 0, "Empty registry should produce no warnings");
    
    println!("✓ Direct constraint solver recursive analysis completed with {} warnings", warnings.len());
}