//! Debug tests for memory usage in iterative inference

use crate::{typecheck_package, Package};
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