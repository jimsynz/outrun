//! Test Self type resolution in impl blocks

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_self_type_resolution_in_impl_block() {
    let mut engine = TypeInferenceEngine::new();
    
    // Test code that demonstrates the Self type resolution issue
    let test_code = r#"
protocol Display {
    def to_string(value: Self): String
}

struct MyType() {}

impl Display for MyType {
    def to_string(value: Self): String {
        # This should work - Self should resolve to MyType, 
        # and MyType should be compatible with String if MyType implements String
        value
    }
}
"#;
    
    let mut program = parse_program(test_code).expect("Failed to parse test code");
    
    // Process the program
    let result = engine.typecheck_program(&mut program);
    
    match result {
        Ok(()) => {
            println!("✅ Self type resolution succeeded!");
        }
        Err(e) => {
            println!("❌ Self type resolution failed: {:?}", e);
            println!("This demonstrates the Self type resolution issue");
            
            // This should fail with a type mismatch between Self and String
            // The error should show that Self is not being resolved to MyType
        }
    }
}

#[test]
fn test_self_type_resolution_concrete_return() {
    let mut engine = TypeInferenceEngine::new();
    
    // Simpler test - Self should resolve to concrete type
    let test_code = r#"
struct MyType() {}

impl Display for MyType {
    def identity(value: Self): MyType {
        # This should work - Self resolves to MyType, MyType matches declared return type
        value
    }
}

protocol Display {
    def identity(value: Self): Self
}
"#;
    
    let mut program = parse_program(test_code).expect("Failed to parse test code");
    
    let result = engine.typecheck_program(&mut program);
    
    match result {
        Ok(()) => {
            println!("✅ Self to concrete type resolution succeeded!");
        }
        Err(e) => {
            println!("❌ Self to concrete type resolution failed: {:?}", e);
            println!("Expected: Self should resolve to MyType");
        }
    }
}

#[test]
fn test_self_type_resolution_core_string_pattern() {
    let mut engine = TypeInferenceEngine::new();
    
    // Test the exact pattern from core library that was failing
    let test_code = r#"
protocol String {
    def to_string(value: Self): Outrun.Core.String
}

struct Outrun.Core.String() {}

impl String for Outrun.Core.String {
    def to_string(value: Self): Outrun.Core.String {
        # This should work - Self resolves to Outrun.Core.String, matches declared return type
        value
    }
}
"#;
    
    let mut program = parse_program(test_code).expect("Failed to parse test code");
    
    let result = engine.typecheck_program(&mut program);
    
    match result {
        Ok(()) => {
            println!("✅ Core String Self type resolution succeeded!");
        }
        Err(e) => {
            println!("❌ Core String Self type resolution failed: {:?}", e);
            panic!("This pattern from core library should work with Self type resolution");
        }
    }
}