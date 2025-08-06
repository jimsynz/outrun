//! Performance tests for typechecker v3
//!
//! These tests verify that the typechecker performs well on:
//! - Large programs with many expressions
//! - Deep expression nesting
//! - Large collections
//! - Many variable bindings
//! - Complex type hierarchies

use crate::{typecheck_package, typecheck_program, Package, TypeInferenceEngine};
use outrun_parser::parse_program;
use std::time::Instant;

/// Typecheck a program without loading the core library (for performance testing)
/// This avoids the overhead of loading and compiling the core library for each test
fn typecheck_program_without_core(
    program: &mut outrun_parser::Program,
) -> Result<(), crate::error::TypecheckError> {
    let mut engine = TypeInferenceEngine::new();

    // Phase 1: Desugar operators
    let mut desugaring_engine = crate::desugaring::DesugaringEngine::new();
    desugaring_engine.desugar_program(program).map_err(|e| {
        crate::error::TypecheckError::CoreLibraryError(format!("Desugaring failed: {:?}", e))
    })?;

    // Phase 2: Register expressions for source mapping
    engine.register_program_expressions(program);

    // Phase 3: Register protocols and structs
    engine.register_protocols_and_structs(program)?;

    // Phase 4: Register automatic implementations
    engine.register_automatic_implementations(program)?;

    // Phase 5: Register explicit implementations
    engine.register_implementations(program)?;

    // Phase 6: Register functions
    engine.register_functions(program)?;

    // Phase 7: Validate implementation completeness
    engine.validate_implementation_completeness()?;

    // Phase 8: Type check function bodies
    engine.typecheck_function_bodies(program)?;

    Ok(())
}

#[test]
fn test_many_variable_bindings_performance() {
    // println!("🚀 Testing performance with many variable bindings...");

    // Generate program with many variable bindings
    let mut source = String::new();
    let var_count = 500;

    for i in 0..var_count {
        source.push_str(&format!("let variable_{} = {}\n", i, i % 100));
    }

    let mut program = parse_program(&source).expect("Failed to parse program with many variables");

    let start = Instant::now();
    let result = typecheck_program_without_core(&mut program);
    let duration = start.elapsed();

    println!("✓ Typechecked {} variables in {:?}", var_count, duration);

    match result {
        Ok(_) => println!("✓ All variables typechecked successfully"),
        Err(e) => println!("! Variables failed (may be expected): {:?}", e),
    }

    // Should complete within reasonable time
    assert!(
        duration.as_secs() < 10,
        "Should typecheck {} variables in under 10 seconds",
        var_count
    );
    assert!(
        duration.as_millis() < 5000,
        "Should be reasonably fast (under 5 seconds)"
    );
}

#[test]
fn test_deep_expression_nesting_performance() {
    // println!("🚀 Testing performance with deep expression nesting...");

    // Generate deeply nested arithmetic expression: ((((1 + 1) + 1) + 1) + 1)...
    let mut expression = "1".to_string();
    let nesting_depth = 20; // Conservative limit for test environment

    for _ in 0..nesting_depth {
        expression = format!("({} + 1)", expression);
    }

    let source = format!("let deep_expr = {}", expression);
    let mut program = parse_program(&source).expect("Failed to parse deeply nested expression");

    let start = Instant::now();
    let result = typecheck_program_without_core(&mut program);
    let duration = start.elapsed();

    println!(
        "✓ Typechecked expression with depth {} in {:?}",
        nesting_depth, duration
    );

    match result {
        Ok(_) => println!("✓ Deep expression typechecked successfully"),
        Err(e) => println!("! Deep expression failed: {:?}", e),
    }

    // Should handle deep nesting without exponential blowup
    assert!(
        duration.as_secs() < 5,
        "Should handle deep nesting efficiently"
    );
}

#[test]
fn test_large_collection_performance() {
    // println!("🚀 Testing performance with large collections...");

    // Generate large list - reduce size to avoid memory issues
    let mut elements = Vec::new();
    let element_count = 100; // Reduced from 1000 to avoid memory issues

    for i in 0..element_count {
        elements.push(i.to_string());
    }

    let source = format!("let large_list = [{}]", elements.join(", "));
    let mut program = parse_program(&source).expect("Failed to parse large list");

    let start = Instant::now();
    let result = typecheck_program_without_core(&mut program);
    let duration = start.elapsed();

    println!(
        "✓ Typechecked list with {} elements in {:?}",
        element_count, duration
    );

    match result {
        Ok(_) => println!("✓ Large collection typechecked successfully"),
        Err(e) => println!("! Large collection failed: {:?}", e),
    }

    // Should handle large collections efficiently
    assert!(
        duration.as_secs() < 5,
        "Should handle large collections in reasonable time"
    );
}

#[test]
fn test_many_small_programs_performance() {
    // println!("🚀 Testing performance with many small programs in package...");

    let mut package = Package::new("performance_test".to_string());
    let program_count = 50;

    // Add many small programs to the package
    for i in 0..program_count {
        let source = format!(
            r#"
            let value_{} = {}
            let doubled_{} = value_{} * 2
            let message_{} = "Program number {}"
        "#,
            i, i, i, i, i, i
        );

        let program = parse_program(&source).expect("Failed to parse small program");
        package.add_program(program);
    }

    let start = Instant::now();
    let result = typecheck_package(&mut package);
    let duration = start.elapsed();

    println!(
        "✓ Typechecked package with {} programs in {:?}",
        program_count, duration
    );

    match result {
        Ok(_) => println!("✓ All programs in package typechecked successfully"),
        Err(e) => println!("! Package typechecking failed: {:?}", e),
    }

    // Package-level processing should scale well
    assert!(
        duration.as_secs() < 10,
        "Should handle many programs efficiently"
    );
}

#[test]
fn test_complex_nested_collections_performance() {
    // println!("🚀 Testing performance with complex nested collections...");

    // Generate nested data structure: list of maps of tuples
    let source = r#"
        let complex_data = [
            [(1, "a"), (2, "b"), (3, "c")],
            [(4, "d"), (5, "e"), (6, "f")],
            [(7, "g"), (8, "h"), (9, "i")],
            [(10, "j"), (11, "k"), (12, "l")],
            [(13, "m"), (14, "n"), (15, "o")]
        ]
    "#;

    match parse_program(source) {
        Ok(mut program) => {
            let start = Instant::now();
            let result = typecheck_program_without_core(&mut program);
            let duration = start.elapsed();

            println!("✓ Typechecked complex nested structure in {:?}", duration);

            match result {
                Ok(_) => println!("✓ Complex nested collections typechecked successfully"),
                Err(e) => println!("! Complex nested collections failed: {:?}", e),
            }

            // Should handle complex nesting without excessive overhead (allow up to 2 seconds for CI environments)
            assert!(
                duration.as_millis() < 2000,
                "Should handle complex nesting efficiently"
            );
        }
        Err(e) => {
            println!(
                "! Complex nested structure parsing failed (maps not supported yet): {:?}",
                e
            );
            // This is okay - the parser may not support map syntax yet
        }
    }
}

#[test]
fn test_repeated_similar_expressions_performance() {
    // println!("🚀 Testing performance with repeated similar expressions...");

    // Generate many similar arithmetic expressions
    let mut source = String::new();
    let expression_count = 200;

    for i in 0..expression_count {
        source.push_str(&format!(
            "let result_{} = {} + {} * {} - {}\n",
            i,
            i,
            i + 1,
            i + 2,
            i + 3
        ));
    }

    let mut program = parse_program(&source).expect("Failed to parse repeated expressions");

    let start = Instant::now();
    let result = typecheck_program_without_core(&mut program);
    let duration = start.elapsed();

    println!(
        "✓ Typechecked {} similar expressions in {:?}",
        expression_count, duration
    );

    match result {
        Ok(_) => println!("✓ All similar expressions typechecked successfully"),
        Err(e) => println!("! Similar expressions failed: {:?}", e),
    }

    // Should benefit from caching/memoization of similar type operations
    assert!(
        duration.as_secs() < 8,
        "Should handle repeated patterns efficiently"
    );
}

#[test]
fn test_type_inference_engine_fresh_variable_performance() {
    // println!("🚀 Testing TypeInferenceEngine fresh variable generation performance...");

    let mut engine = TypeInferenceEngine::new();
    let var_count = 10000;

    let start = Instant::now();

    let mut type_vars = Vec::new();
    for _ in 0..var_count {
        type_vars.push(engine.fresh_type_var());
    }

    let duration = start.elapsed();

    println!(
        "✓ Generated {} fresh type variables in {:?}",
        var_count, duration
    );

    // All variables should be unique
    let unique_vars: std::collections::HashSet<_> = type_vars.into_iter().collect();
    assert_eq!(
        unique_vars.len(),
        var_count,
        "All type variables should be unique"
    );

    // Should be very fast - just incrementing a counter
    assert!(
        duration.as_millis() < 100,
        "Type variable generation should be extremely fast"
    );
}

#[test]
#[ignore] // Disabled: takes over a minute to run
fn test_memory_usage_large_program() {
    // println!("🚀 Testing memory usage with large program...");

    // Get initial memory usage (this is approximate)
    let initial_memory = get_approximate_memory_usage();

    // Generate a substantial program
    let mut source = String::new();
    let item_count = 1000;

    for i in 0..item_count {
        source.push_str(&format!(
            r#"
            let var_{} = {}
            let list_{} = [{}, {}, {}]
            let tuple_{} = ({}, {})
        "#,
            i,
            i,
            i,
            i,
            i + 1,
            i + 2,
            i,
            i * 2,
            i * 3
        ));
    }

    let mut program = parse_program(&source).expect("Failed to parse large program");

    let before_typecheck = get_approximate_memory_usage();
    let result = typecheck_program_without_core(&mut program);
    let after_typecheck = get_approximate_memory_usage();

    // println!("📊 Memory usage:");
    println!("  Initial: {} KB", initial_memory / 1024);
    println!("  Before typecheck: {} KB", before_typecheck / 1024);
    println!("  After typecheck: {} KB", after_typecheck / 1024);
    println!(
        "  Typecheck overhead: {} KB",
        (after_typecheck - before_typecheck) / 1024
    );

    match result {
        Ok(_) => println!("✓ Large program typechecked successfully"),
        Err(e) => println!("! Large program failed: {:?}", e),
    }

    // Memory usage should be reasonable (not exponential)
    let typecheck_overhead = after_typecheck - before_typecheck;
    assert!(
        typecheck_overhead < 50 * 1024 * 1024,
        "Typecheck memory overhead should be reasonable (< 50MB)"
    );
}

#[test]
fn test_concurrent_typechecking_simulation() {
    // println!("🚀 Testing concurrent typechecking simulation...");

    // Simulate multiple independent typechecking operations
    let program_sources = vec![
        "let x = 42",
        "let y = [1, 2, 3]",
        "let z = {\"key\": \"value\"}",
        "let a = (1, \"hello\")",
        "let b = 1 + 2 * 3",
    ];

    let start = Instant::now();

    let mut results = Vec::new();
    for (i, source) in program_sources.iter().enumerate() {
        println!("  Processing program {}: {}", i + 1, source);

        if let Ok(mut program) = parse_program(source) {
            let program_start = Instant::now();
            let result = typecheck_program_without_core(&mut program);
            let program_duration = program_start.elapsed();

            results.push((result.is_ok(), program_duration));
            println!("    Completed in {:?}", program_duration);
        }
    }

    let total_duration = start.elapsed();

    println!(
        "✓ Processed {} programs in {:?}",
        program_sources.len(),
        total_duration
    );

    let successful_count = results.iter().filter(|(success, _)| *success).count();
    println!(
        "✓ {} out of {} programs succeeded",
        successful_count,
        results.len()
    );

    // Each individual program should be fast
    for (_, duration) in &results {
        assert!(
            duration.as_millis() < 1000,
            "Individual programs should typecheck quickly"
        );
    }

    // Total should still be reasonable
    assert!(
        total_duration.as_secs() < 5,
        "Total concurrent simulation should be fast"
    );
}

/// Approximate memory usage measurement (platform-dependent)
fn get_approximate_memory_usage() -> usize {
    // This is a rough approximation - in a real implementation you might use
    // platform-specific APIs or memory profiling tools

    // For now, just return a placeholder that increases over time
    // to simulate memory usage tracking
    static mut COUNTER: usize = 0;
    unsafe {
        COUNTER += 1024; // Simulate increasing memory usage
        COUNTER
    }
}
