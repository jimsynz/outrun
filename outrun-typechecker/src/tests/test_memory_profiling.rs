//! Memory profiling tests for iterative inference system

use crate::{typecheck_package, Package};
use outrun_parser::parse_program;

#[test]
fn test_memory_profiling_progressive() {
    // Test memory usage progression to find the bottleneck
    let sizes = vec![5, 10, 15, 20, 25];

    for size in sizes {
        println!("\n=== Testing {} terms ===", size);

        let mut package = Package::new(format!("test_{}_terms", size));

        let mut parts = Vec::new();
        for i in 0..size {
            parts.push(i.to_string());
        }
        let expression = parts.join(" + ");
        let source = format!("let x = {}", expression);

        let program = parse_program(&source).expect("Should parse successfully");
        package.add_program(program);

        println!("Expression length: {} characters", expression.len());
        println!("Starting type checking...");

        let start = std::time::Instant::now();
        let result = typecheck_package(&mut package);
        let duration = start.elapsed();

        match result {
            Ok(_) => {
                println!("✓ {} terms completed in {:.2?}", size, duration);
                if duration.as_secs() > 10 {
                    println!("⚠️  Performance degradation detected at {} terms", size);
                    break;
                }
            }
            Err(e) => {
                println!("❌ {} terms failed: {:?}", size, e);
                break;
            }
        }
    }
}

#[test]
fn test_identify_memory_bottleneck() {
    // Test with instrumentation to identify bottleneck
    let mut package = Package::new("bottleneck_test".to_string());

    // Use moderate size that should work but might be slow
    let size = 15;
    let mut parts = Vec::new();
    for i in 0..size {
        parts.push(i.to_string());
    }
    let expression = parts.join(" + ");
    let source = format!("let x = {}", expression);

    println!("Testing bottleneck with {} terms", size);
    println!("Expression: {}", expression);

    let program = parse_program(&source).expect("Should parse successfully");
    package.add_program(program);

    let start = std::time::Instant::now();
    let result = typecheck_package(&mut package);
    let duration = start.elapsed();

    match result {
        Ok(_) => println!("✓ Completed in {:.2?}", duration),
        Err(e) => println!("❌ Failed: {:?}", e),
    }

    // The goal is to identify where the time is being spent
    if duration.as_secs() > 5 {
        println!("⚠️  Slow performance detected - need to investigate task system");
    }
}
