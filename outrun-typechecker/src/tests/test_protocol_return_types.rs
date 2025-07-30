//! Test protocol function return type inference vs declared return types

use crate::{CompilationResult, Package};

#[test]
fn test_protocol_function_return_type_mismatch() {
    // Reproduce the exact issue: Boolean.false? should return Outrun.Core.Boolean but infers Self
    let source = r#"
        protocol Simple {
            def helper(): Boolean
            
            def caller(): Boolean {
                helper()
            }
        }
    "#;

    let mut package = Package::new("test-return-types".to_string());

    // Parse the program
    let parsed = outrun_parser::parse_program(source).expect("Should parse successfully");
    package.add_program(parsed);

    // Try to compile
    let result = CompilationResult::compile_package(&mut package);

    match result {
        Ok(_) => {
            println!("✅ Simple protocol function return type test passed");
        }
        Err(err) => {
            println!(
                "❌ Simple protocol function return type test failed: {}",
                err
            );
            let error_string = format!("{}", err);
            if error_string.contains("Boolean") && error_string.contains("Self") {
                println!("🎯 Reproduced the protocol return type issue!");
            }
        }
    }
}

#[test]
fn test_concrete_return_type_vs_self() {
    // Test the specific case: protocol function declares concrete return type but body infers Self
    let source = r#"
        protocol TestProtocol {
            def concrete_return(): Outrun.Core.Boolean {
                true
            }
        }
    "#;

    let mut package = Package::new("test-concrete-return".to_string());

    // Parse the program
    let parsed = outrun_parser::parse_program(source).expect("Should parse successfully");
    package.add_program(parsed);

    // Try to compile
    let result = CompilationResult::compile_package(&mut package);

    match result {
        Ok(_) => {
            println!("✅ Concrete return type test passed");
        }
        Err(err) => {
            println!("❌ Concrete return type test failed: {}", err);
            println!("Error debug: {:?}", err);
        }
    }
}
