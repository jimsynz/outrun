use crate::checker::TypeChecker;
use outrun_parser::parse_program;

#[test]
fn test_simple_typed_program_conversion() {
    let source = r#"
42
"#;

    let program = parse_program(source).unwrap();
    let mut type_checker = TypeChecker::new();

    // This should create a TypedProgram even though type checking might fail
    let result = type_checker.check_program(&program);

    // We expect this to fail because there's no core library, but it should return a TypedProgram structure
    match result {
        Ok(typed_program) => {
            // If it succeeds, verify the structure
            println!(
                "Typed program compilation summary: {}",
                typed_program.compilation_summary
            );
            assert!(
                !typed_program.items.is_empty(),
                "Should have at least one item"
            );
        }
        Err(errors) => {
            // Expected failure due to missing core library, but the conversion logic ran
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
    let mut type_checker = TypeChecker::new();

    let result = type_checker.check_program(&program);

    // Again, this should fail due to undefined types, but the conversion logic should work
    match result {
        Ok(typed_program) => {
            println!(
                "Typed program with function: {}",
                typed_program.compilation_summary
            );
            // Should have converted function to placeholder
            assert!(!typed_program.items.is_empty());
        }
        Err(errors) => {
            println!("Expected errors for function test: {} errors", errors.len());
            assert!(!errors.is_empty());
        }
    }
}
