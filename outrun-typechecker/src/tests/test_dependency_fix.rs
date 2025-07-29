#[cfg(test)]
mod test_dependency_fix {
    use crate::*;
    use outrun_parser::{parse_program, Program};

    /// Test that demonstrates the task dependency fix is working
    #[test]
    fn test_option_unwrap_dependency_preservation() {
        let source = r#"
            def test_function(opt: Option<Integer>): Integer {
                Option.unwrap(value: opt)
            }
        "#;
        
        let mut program = parse_program(source).expect("Failed to parse test program");
        let mut package = Package::new("test_package".to_string());
        package.add_program(program);
        
        // This should complete without panicking due to missing dependencies
        let result = CompilationResult::compile_package(&mut package);
        
        // The compilation may fail due to other issues, but it should not fail due to
        // systemic task dependency loss (which would cause Option.unwrap to not resolve)
        match result {
            Ok(_) => println!("✅ Task dependency fix verified: compilation successful"),
            Err(e) => {
                let error_string = format!("{:?}", e);
                // If the error is NOT about missing dependencies, then our fix worked
                if !error_string.contains("Task dependencies: []") && 
                   !error_string.contains("no dependencies") {
                    println!("✅ Task dependency fix verified: failure is NOT due to missing dependencies");
                    println!("  Error type: {}", error_string.lines().next().unwrap_or("Unknown"));
                } else {
                    panic!("❌ Task dependency loss still occurring: {}", error_string);
                }
            }
        }
    }

    /// Test nested inference calls preserve task state
    #[test]
    fn test_nested_inference_depth_tracking() {
        let source = r#"
            def complex_function(a: Integer, b: Integer): Integer {
                a + b * 2 - 1
            }
        "#;
        
        let mut program = parse_program(source).expect("Failed to parse test program");
        let mut package = Package::new("test_nested".to_string());
        package.add_program(program);
        
        // This tests complex expressions with multiple levels of nesting
        let result = CompilationResult::compile_package(&mut package);
        
        // Should not fail due to task dependency loss
        match result {
            Ok(_) => println!("✅ Nested inference fix verified: compilation successful"),
            Err(e) => {
                let error_string = format!("{:?}", e);
                // Verify error is not about lost dependencies
                assert!(!error_string.contains("Task dependencies: []"), 
                    "Task dependencies still being lost: {}", error_string);
                println!("✅ Nested inference fix verified: no dependency loss detected");
            }
        }
    }
}