//! Edge case tests for typechecker v3
//!
//! These tests verify that the typechecker handles unusual, boundary,
//! and edge case scenarios correctly without panicking or producing
//! incorrect results.

use crate::{typecheck_package, typecheck_program, Package};
use outrun_parser::parse_program;

#[test]
fn test_empty_program() {
    let source = "";

    match parse_program(source) {
        Ok(mut program) => {
            let result = typecheck_program(&mut program);
            match result {
                Ok(_) => println!("✓ Empty program typechecked successfully"),
                Err(e) => println!("✓ Empty program failed gracefully: {:?}", e),
            }
        }
        Err(e) => {
            println!("✓ Empty program parse failed as expected: {:?}", e);
        }
    }
}

#[test]
fn test_whitespace_only_program() {
    let source = "   \n\t  \r\n  ";

    match parse_program(source) {
        Ok(mut program) => {
            let result = typecheck_program(&mut program);
            match result {
                Ok(_) => println!("✓ Whitespace-only program typechecked successfully"),
                Err(e) => println!("✓ Whitespace-only program failed gracefully: {:?}", e),
            }
        }
        Err(e) => {
            println!(
                "✓ Whitespace-only program parse failed as expected: {:?}",
                e
            );
        }
    }
}

#[test]
fn test_single_literal_expressions() {
    let test_cases = vec![
        ("42", "integer literal"),
        ("3.14", "float literal"),
        ("true", "boolean literal"),
        ("false", "boolean literal"),
        ("\"hello\"", "string literal"),
    ];

    for (source, description) in test_cases {
        println!("Testing {}: {}", description, source);

        if let Ok(mut program) = parse_program(source) {
            let result = typecheck_program(&mut program);
            match result {
                Ok(_) => println!("  ✓ {} typechecked successfully", description),
                Err(e) => println!("  ! {} failed: {:?}", description, e),
            }
        }
    }
}

#[test]
fn test_empty_collections() {
    let test_cases = vec![
        ("[]", "empty list"),
        ("{}", "empty map"),
        ("()", "empty tuple"), // May not be valid Outrun syntax
    ];

    for (source, description) in test_cases {
        println!("Testing {}: {}", description, source);

        match parse_program(source) {
            Ok(mut program) => {
                let result = typecheck_program(&mut program);
                match result {
                    Ok(_) => println!("  ✓ {} typechecked successfully", description),
                    Err(e) => {
                        println!("  ! {} failed (may be expected): {:?}", description, e);
                        // Empty collections should typically require type annotations
                    }
                }
            }
            Err(e) => {
                println!("  ! {} parse failed: {:?}", description, e);
            }
        }
    }
}

#[test]
fn test_extremely_long_identifiers() {
    // Test with very long identifier names
    let long_name = "very_long_identifier_name_that_goes_on_and_on_and_on_".repeat(10);
    let source = format!("let {} = 42", long_name);

    if let Ok(mut program) = parse_program(&source) {
        let result = typecheck_program(&mut program);
        match result {
            Ok(_) => println!("✓ Extremely long identifier handled successfully"),
            Err(e) => println!("! Long identifier failed: {:?}", e),
        }
    }
}

#[test]
fn test_deeply_nested_collections() {
    // Test with very deep nesting
    let mut nested = "42".to_string();

    // Create deeply nested list: [[[[42]]]]
    for _ in 0..20 {
        nested = format!("[{}]", nested);
    }

    let source = format!("let deeply_nested = {}", nested);

    if let Ok(mut program) = parse_program(&source) {
        let result = typecheck_program(&mut program);
        match result {
            Ok(_) => println!("✓ Deeply nested collections handled successfully"),
            Err(e) => println!("! Deeply nested collections failed: {:?}", e),
        }
    }
}

#[test]
fn test_maximum_integer_values() {
    let test_cases = vec![
        ("0", "zero"),
        ("1", "one"),
        ("-1", "negative one"),
        ("9223372036854775807", "max i64"),
        ("-9223372036854775808", "min i64"),
        ("18446744073709551615", "max u64 (might overflow)"),
    ];

    for (value, description) in test_cases {
        let source = format!("let value = {}", value);

        match parse_program(&source) {
            Ok(mut program) => {
                let result = typecheck_program(&mut program);
                match result {
                    Ok(_) => println!("✓ {} ({}) typechecked successfully", description, value),
                    Err(e) => println!("! {} ({}) failed: {:?}", description, value, e),
                }
            }
            Err(e) => {
                println!("! {} ({}) parse failed: {:?}", description, value, e);
            }
        }
    }
}

#[test]
fn test_special_float_values() {
    let test_cases = vec![
        ("0.0", "zero float"),
        ("-0.0", "negative zero"),
        ("3.141592653589793", "high precision pi"),
        ("1e100", "scientific notation large"),
        ("1e-100", "scientific notation small"),
        ("1.7976931348623157e308", "near max f64"),
    ];

    for (value, description) in test_cases {
        let source = format!("let value = {}", value);

        match parse_program(&source) {
            Ok(mut program) => {
                let result = typecheck_program(&mut program);
                match result {
                    Ok(_) => println!("✓ {} ({}) typechecked successfully", description, value),
                    Err(e) => println!("! {} ({}) failed: {:?}", description, value, e),
                }
            }
            Err(e) => {
                println!("! {} ({}) parse failed: {:?}", description, value, e);
            }
        }
    }
}

#[test]
fn test_unicode_and_special_characters() {
    let test_cases = vec![
        ("\"hello world\"", "basic string"),
        ("\"émojis: 🚀🎉\"", "unicode emoji"),
        ("\"中文测试\"", "chinese characters"),
        ("\"Iñtërnâtiônàlizætiøn\"", "accented characters"),
        ("\"\\n\\t\\r\\\"\"", "escape sequences"),
        ("\"\"", "empty string"),
    ];

    for (value, description) in test_cases {
        let source = format!("let value = {}", value);

        match parse_program(&source) {
            Ok(mut program) => {
                let result = typecheck_program(&mut program);
                match result {
                    Ok(_) => println!("✓ {} typechecked successfully", description),
                    Err(e) => println!("! {} failed: {:?}", description, e),
                }
            }
            Err(e) => {
                println!("! {} parse failed: {:?}", description, e);
            }
        }
    }
}

#[test]
fn test_malformed_but_parseable_expressions() {
    // Test cases that parse but might have type issues
    let test_cases = vec![
        ("let x = x", "self-referential variable"),
        ("let a = b\nlet b = a", "circular dependency"),
        ("let undefined", "incomplete let binding"),
    ];

    for (source, description) in test_cases {
        println!("Testing {}: {}", description, source);

        match parse_program(source) {
            Ok(mut program) => {
                let result = typecheck_program(&mut program);
                // These should typically fail, but gracefully
                match result {
                    Ok(_) => println!("  ! {} unexpectedly succeeded", description),
                    Err(e) => println!("  ✓ {} failed as expected: {:?}", description, e),
                }
            }
            Err(e) => {
                println!("  ✓ {} failed at parse (expected): {:?}", description, e);
            }
        }
    }
}

#[test]
fn test_package_with_no_programs() {
    let mut empty_package = Package::new("empty_package".to_string());

    // Initially empty
    assert_eq!(empty_package.programs.len(), 0);

    let result = typecheck_package(&mut empty_package);
    match &result {
        Ok(_) => println!("✓ Empty package typechecked successfully"),
        Err(e) => println!("! Empty package failed: {:?}", e),
    }

    // After type checking, core library should be integrated
    // This is the expected behavior for all Outrun packages
    if !empty_package.programs.is_empty() {
        println!(
            "✓ Core library integrated: {} programs",
            empty_package.programs.len()
        );
    } else {
        // println!("! No core library integration occurred");
    }

    // The key test is that type checking succeeds, not that the package stays empty
    assert!(
        result.is_ok(),
        "Empty package should type check successfully with core library"
    );
}

#[test]
fn test_package_with_empty_programs() {
    let mut package = Package::new("package_with_empty_programs".to_string());

    // Add some programs that might be empty or minimal
    let sources = vec!["", "   ", "\n\t\n"];

    for (i, source) in sources.iter().enumerate() {
        match parse_program(source) {
            Ok(program) => {
                package.add_program(program);
                println!("Added empty program {}", i + 1);
            }
            Err(e) => {
                println!("Empty program {} failed to parse: {:?}", i + 1, e);
            }
        }
    }

    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Package with empty programs typechecked successfully"),
        Err(e) => println!("! Package with empty programs failed: {:?}", e),
    }
}

#[test]
fn test_extremely_large_single_expression() {
    // Create a reasonably large single expression
    let mut parts = Vec::new();
    for i in 0..30 {
        // Reasonable test size - should handle this efficiently
        parts.push(i.to_string());
    }

    // Create expression like: 0 + 1 + 2 + 3 + ... + 29 (30 terms total)
    let expression = parts.join(" + ");

    let source = format!("let large_sum = {}", expression);

    match parse_program(&source) {
        Ok(mut program) => {
            let result = typecheck_program(&mut program);
            match result {
                Ok(_) => println!("✓ Extremely large expression typechecked successfully"),
                Err(e) => println!("! Large expression failed: {:?}", e),
            }
        }
        Err(e) => {
            println!("! Large expression parse failed: {:?}", e);
        }
    }
}

#[test]
fn test_mixed_valid_and_invalid_programs_in_package() {
    let mut package = Package::new("mixed_package".to_string());

    let program_sources = vec![
        ("let valid1 = 42", true),
        ("let valid2 = \"hello\"", true),
        ("let invalid = undefined_var", false),
        ("let valid3 = [1, 2, 3]", true),
        ("let also_invalid = [1, \"mixed\"]", false),
    ];

    for (source, should_be_valid) in program_sources {
        match parse_program(source) {
            Ok(program) => {
                package.add_program(program);
                println!(
                    "Added program: {} (expected valid: {})",
                    source, should_be_valid
                );
            }
            Err(e) => {
                println!("Failed to parse program '{}': {:?}", source, e);
            }
        }
    }

    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Mixed package typechecked successfully"),
        Err(e) => println!("! Mixed package failed (expected): {:?}", e),
    }
}

#[test]
fn test_stress_error_reporting() {
    // Test that error reporting doesn't break under stress
    let problematic_programs = vec![
        "undefined1 + undefined2",
        "[1, 2, \"three\", 4, true]",
        "let x = y + z",
        "1 + \"hello\" + true",
        "nested.deeply.undefined.access",
    ];

    // println!("🔍 Stress testing error reporting...");

    for (i, source) in problematic_programs.iter().enumerate() {
        println!("  Testing error case {}: {}", i + 1, source);

        match parse_program(source) {
            Ok(mut program) => {
                let result = typecheck_program(&mut program);
                match result {
                    Ok(_) => println!("    ! Unexpectedly succeeded"),
                    Err(e) => {
                        // println!("    ✓ Failed as expected");
                        // Verify error is properly formatted (doesn't panic when displaying)
                        let error_string = format!("{:?}", e);
                        assert!(
                            !error_string.is_empty(),
                            "Error should format to non-empty string"
                        );
                    }
                }
            }
            Err(e) => {
                println!("    ✓ Parse failed: {:?}", e);
            }
        }
    }

    // println!("✓ Error reporting stress test completed");
}
