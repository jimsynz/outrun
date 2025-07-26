use crate::inference::TypeInferenceEngine;
use crate::Package;
use outrun_parser::parse_program;

#[test]
fn test_constraint_system_without_core_library() {
    // Test the constraint system redesign without loading the problematic core library
    let mut engine = TypeInferenceEngine::new();
    
    // Simple test code that doesn't require core library protocols
    let code = r#"
        protocol UnaryMinus<T> {
            def minus(value: T): T {
                # Abstract protocol function
            }
        }
        
        struct Integer64 {
            def new(value: String): Integer64 {
                # Constructor function
            }
        }
        
        impl UnaryMinus<Integer64> for Integer64 {
            def minus(value: Integer64): Integer64 {
                Integer64.new(value: "negative")
            }
        }
        
        def test_function(): Integer64 {
            let value = Integer64.new(value: "test")
            -value
        }
    "#;
    
    let program = parse_program(code).expect("Parse should succeed");
    let mut package = Package::new("test".to_string());
    package.add_program(program);
    
    // Process through typechecker using CompilationResult
    let result = crate::CompilationResult::compile_package(&mut package);
    
    match result {
        Ok(_compilation_result) => {
            println!("✅ Constraint system works without core library!");
        }
        Err(e) => {
            println!("❌ Even without core library, constraint system fails: {:?}", e);
            // This will help us isolate if the issue is core library or constraint system
        }
    }
}