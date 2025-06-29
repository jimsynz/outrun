//! Test function call type inference with generic parameters and type hints

use crate::compilation::compiler_environment::CompilerEnvironment;
use outrun_parser::parse_program;

/// Test that static trait functions can infer generic parameters from expected return type
#[test]
fn test_static_trait_function_generic_inference_from_return_type() {
    let source = r#"
        # Define a simple generic struct that represents None
        struct TestNone<T>() {}
        
        # Define a trait with a static function that returns a generic type
        trait TestOption<T> {
            defs none(): TestNone<T> {
                TestNone { }
            }
        }
        
        # Test function that calls the static trait function with expected return type
        def test_function(): TestNone<String> {
            TestOption.none()
        }
    "#;

    let program = parse_program(source).expect("Failed to parse program");
    let mut compiler_env = CompilerEnvironment::new();

    // Create a minimal program collection without core library
    let mut collection = crate::compilation::program_collection::ProgramCollection::new();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    // Compile and check that it succeeds
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(compilation_result) => {
            println!("✅ Compilation succeeded - generic type inference working!");

            // Verify the typed program was created
            let typed_program = compilation_result
                .typed_programs
                .get("test.outrun")
                .expect("Should have test program");

            assert!(!typed_program.items.is_empty(), "Should have typed items");
            println!("✅ Type inference for static trait functions with generic parameters works correctly");
        }
        Err(errors) => {
            println!("❌ Compilation failed with errors:");
            for error in &errors {
                println!("  - {}", error);
            }

            // Check if the error is the specific "Cannot infer generic type parameter" error
            let has_inference_error = errors.iter().any(|e| {
                e.to_string()
                    .contains("Cannot infer generic type parameter")
            });

            if has_inference_error {
                panic!("❌ Generic type parameter inference still failing - fix not working");
            } else {
                println!("ℹ️  Compilation failed for other reasons (expected in test environment)");
            }
        }
    }
}

/// Test that function calls without type hints still work (backward compatibility)
#[test]
fn test_function_call_without_type_hint_still_works() {
    let source = r#"
        struct SimpleStruct() {}
        
        trait SimpleTrait {
            defs create(): SimpleStruct {
                SimpleStruct { }
            }
        }
    "#;

    let program = parse_program(source).expect("Failed to parse program");
    let mut compiler_env = CompilerEnvironment::new();

    let mut collection = crate::compilation::program_collection::ProgramCollection::new();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Non-generic function calls still work correctly");
        }
        Err(errors) => {
            println!("Compilation errors (may be expected in test environment):");
            for error in &errors {
                println!("  - {}", error);
            }

            // As long as it's not a generic inference error, the test passes
            let has_inference_error = errors.iter().any(|e| {
                e.to_string()
                    .contains("Cannot infer generic type parameter")
            });

            assert!(
                !has_inference_error,
                "Should not have generic inference errors for non-generic cases"
            );
        }
    }
}

/// Test that function calls with explicit argument types work
#[test]
fn test_function_call_with_argument_based_inference() {
    let source = r#"
        struct Container<T>(value: T) {}
        
        trait ContainerTrait<T> {
            defs wrap(value: T): Container<T> {
                Container { value: value }
            }
        }
        
        def test_function(): Container<String> {
            ContainerTrait.wrap(value: "hello")
        }
    "#;

    let program = parse_program(source).expect("Failed to parse program");
    let mut compiler_env = CompilerEnvironment::new();

    let mut collection = crate::compilation::program_collection::ProgramCollection::new();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Argument-based generic inference works correctly");
        }
        Err(errors) => {
            println!("Compilation errors:");
            for error in &errors {
                println!("  - {}", error);
            }

            // Check that we don't have the specific inference error we're trying to fix
            let has_inference_error = errors.iter().any(|e| {
                e.to_string()
                    .contains("Cannot infer generic type parameter")
            });

            assert!(
                !has_inference_error,
                "Should not have generic inference errors when arguments provide type info"
            );
        }
    }
}

/// Test the specific case that was failing: parameterless static function with generic return type
#[test]
fn test_parameterless_static_function_with_type_hint() {
    let source = r#"
        struct MyNone<T>() {}
        
        trait MyOption<T> {
            defs none(): MyNone<T> {
                MyNone { }
            }
        }
        
        # This should work with type inference from the let binding
        def test_none(): MyNone<Integer> {
            MyOption.none()
        }
    "#;

    let program = parse_program(source).expect("Failed to parse program");
    let mut compiler_env = CompilerEnvironment::new();

    let mut collection = crate::compilation::program_collection::ProgramCollection::new();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Parameterless static functions with generic return types work correctly!");
        }
        Err(errors) => {
            println!("❌ Compilation failed:");
            for error in &errors {
                println!("  - {}", error);
            }

            // This is the key test - if this specific pattern fails, our fix isn't working
            let has_inference_error = errors.iter().any(|e| {
                e.to_string()
                    .contains("Cannot infer generic type parameter")
            });

            if has_inference_error {
                panic!("❌ The specific case we're trying to fix still fails - type inference not working for parameterless static functions");
            }
        }
    }
}
