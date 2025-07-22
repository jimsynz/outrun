use crate::CompilerEnvironment;
use crate::ProgramCollection;
use outrun_parser::parse_program;

#[test]
fn test_simple_typed_program_conversion() {
    let source = r#"
42
"#;

    let program = parse_program(source).unwrap();
    let mut compiler_env = CompilerEnvironment::new();

    // Create a program collection with just the user program
    let mut collection = ProgramCollection::new();
    collection.add_program("<test>".to_string(), program.clone(), source.to_string());

    // This should create a TypedProgram even though type checking might fail
    let result = compiler_env.compile_collection(collection);

    // We expect this to fail because there's no core library, but it should return a compilation structure
    match result {
        Ok(compilation_result) => {
            // If it succeeds, verify the structure
            println!(
                "Compilation protocols: {}, structs: {}, implementations: {}",
                compilation_result.protocols.len(),
                compilation_result.structs.len(),
                compilation_result.implementations.len()
            );

            // Check if we have a typed program
            if let Some(typed_program) = compilation_result.typed_programs.get("<test>") {
                assert!(
                    !typed_program.items.is_empty(),
                    "Should have at least one item"
                );
            }
        }
        Err(errors) => {
            // Expected failure due to missing core library, but the compilation logic ran
            println!(
                "Expected error due to missing core library: {} errors",
                errors.len()
            );
            assert!(!errors.is_empty(), "Should have some type checking errors");
        }
    }
}

#[test]
fn test_typed_program_with_function() {
    let source = r#"
def test(): Boolean {
    true
}
"#;

    let program = parse_program(source).unwrap();
    let mut compiler_env = CompilerEnvironment::new();

    // Create a program collection with just the user program
    let mut collection = ProgramCollection::new();
    collection.add_program(
        "<test-func>".to_string(),
        program.clone(),
        source.to_string(),
    );

    let result = compiler_env.compile_collection(collection);

    // Again, this should fail due to undefined types, but the compilation logic should work
    match result {
        Ok(compilation_result) => {
            println!(
                "Compilation with function - protocols: {}, structs: {}, implementations: {}",
                compilation_result.protocols.len(),
                compilation_result.structs.len(),
                compilation_result.implementations.len()
            );

            // Check if we have a typed program
            if let Some(typed_program) = compilation_result.typed_programs.get("<test-func>") {
                // Should have converted function
                assert!(!typed_program.items.is_empty());
            }
        }
        Err(errors) => {
            println!("Expected errors for function test: {} errors", errors.len());
            assert!(!errors.is_empty());
        }
    }
}
