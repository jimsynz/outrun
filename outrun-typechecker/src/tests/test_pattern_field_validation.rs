use crate::error::TypeError;
use crate::multi_program_compiler::{MultiProgramCompiler, ProgramCollection};
use outrun_parser::{parse_program, Program};

fn create_program_from_source(source: &str) -> Program {
    parse_program(source).unwrap_or_else(|e| {
        panic!("Failed to parse test program: {:?}\nSource: {}", e, source);
    })
}

#[test]
fn test_struct_field_validation_unknown_field() {
    let source = r#"
struct User(name: String, age: Integer) {}

def test(): String {
    case User { name: "test", email: "invalid" } {
        _ -> "matched"
    }
}
"#;

    let program = create_program_from_source(source);
    let mut collection = ProgramCollection::from_core_library();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

    match result {
        Ok(_) => {
            panic!("Expected compilation to fail due to unknown field 'email' in User struct");
        }
        Err(errors) => {
            assert!(!errors.is_empty(), "Expected at least one error");

            // Check that we get an UndefinedField error
            let undefined_field_error = errors.iter().find(|e| {
                matches!(e, TypeError::UndefinedField { field_name, struct_name, .. } 
                    if field_name == "email" && struct_name == "User")
            });

            assert!(
                undefined_field_error.is_some(),
                "Expected UndefinedField error for 'email' field in User struct but got: {:?}",
                errors
            );
        }
    }
}

#[test]
fn test_struct_field_validation_valid_fields() {
    let source = r#"
struct User(name: String, age: Integer) {}

def test(): String {
    case User { name: "test", age: 25 } {
        _ -> "matched"
    }
}
"#;

    let program = create_program_from_source(source);
    let mut collection = ProgramCollection::from_core_library();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

    match result {
        Ok(_compilation_result) => {
            // Test passed - compilation succeeded as expected
        }
        Err(errors) => {
            println!("Compilation failed with {} errors:", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
            }
            panic!("Expected compilation to succeed for valid struct field usage");
        }
    }
}

#[test]
fn test_struct_field_validation_shorthand_pattern() {
    let source = r#"
struct User(name: String, age: Integer) {}

def test(): String {
    let User { name, age } = User { name: "test", age: 25 }
    name
}
"#;

    let program = create_program_from_source(source);
    let mut collection = ProgramCollection::from_core_library();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

    match result {
        Ok(_compilation_result) => {
            // Test passed - compilation succeeded as expected
        }
        Err(errors) => {
            println!("Compilation failed with {} errors:", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
            }
            panic!("Expected compilation to succeed for valid shorthand struct field pattern");
        }
    }
}
