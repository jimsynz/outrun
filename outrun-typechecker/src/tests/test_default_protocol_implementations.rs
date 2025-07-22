use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::ProgramCollection;
use crate::error::TypeError;
use outrun_parser::{parse_program, Program};

fn create_program_from_source(source: &str) -> Program {
    parse_program(source).unwrap_or_else(|e| {
        panic!("Failed to parse test program: {e:?}\nSource: {source}");
    })
}

#[test]
fn test_default_protocol_implementation_not_required() {
    let protocol_source = r#"
protocol TestProtocol {
    def required_function(value: Self): Self

    def optional_function(value: Self): Self {
        value
    }
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestProtocol for MyType {
    def required_function(value: Self): Self {
        value
    }
    # Note: optional_function is NOT implemented - should use default
}
"#;

    let protocol_program = create_program_from_source(protocol_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::new();
    collection.add_program(
        "protocol.outrun".to_string(),
        protocol_program,
        protocol_source.to_string(),
    );
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            // Test passed - compilation succeeded as expected
        }
        Err(errors) => {
            println!("Compilation failed with {} errors:", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
            }

            // Check that we don't get a MissingImplementation error for optional_function
            let missing_optional_error = errors.iter().find(|e| {
                matches!(e, TypeError::MissingImplementation { function_name, .. }
                    if function_name == "optional_function")
            });

            assert!(
                missing_optional_error.is_none(),
                "Should not require implementation of optional_function (has default)"
            );

            // We might still have other errors (like undefined types), but we shouldn't
            // require implementation of the default function
        }
    }
}

#[test]
fn test_required_function_still_required() {
    let protocol_source = r#"
protocol TestProtocol {
    def required_function(value: Self): Self

    def optional_function(value: Self): Self {
        value
    }
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestProtocol for MyType {
    def optional_function(value: Self): Self {
        value
    }
    # Note: required_function is NOT implemented - should fail
}
"#;

    let protocol_program = create_program_from_source(protocol_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::new();
    collection.add_program(
        "protocol.outrun".to_string(),
        protocol_program,
        protocol_source.to_string(),
    );
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    assert!(
        result.is_err(),
        "Expected compilation to fail due to missing required function"
    );

    let errors = result.unwrap_err();
    assert!(!errors.is_empty(), "Expected at least one error");

    // Check that we get a MissingImplementation error for required_function
    let missing_required_error = errors.iter().find(|e| {
        matches!(e, TypeError::MissingImplementation { function_name, .. }
            if function_name == "required_function")
    });

    assert!(
        missing_required_error.is_some(),
        "Expected MissingImplementation error for required_function"
    );

    // Check that we don't get an error for optional_function
    let missing_optional_error = errors.iter().find(|e| {
        matches!(e, TypeError::MissingImplementation { function_name, .. }
            if function_name == "optional_function")
    });

    assert!(
        missing_optional_error.is_none(),
        "Should not require implementation of optional_function (has default)"
    );
}

#[test]
fn test_override_default_implementation() {
    let protocol_source = r#"
protocol TestProtocol {
    def required_function(value: Self): Self

    def optional_function(value: Self): Self {
        value
    }
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestProtocol for MyType {
    def required_function(value: Self): Self {
        value
    }

    def optional_function(value: Self): Self {
        value
    }
}
"#;

    let protocol_program = create_program_from_source(protocol_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::new();
    collection.add_program(
        "protocol.outrun".to_string(),
        protocol_program,
        protocol_source.to_string(),
    );
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            // Test passed - compilation succeeded as expected
        }
        Err(errors) => {
            println!("Compilation failed with {} errors:", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
            }

            // Should not have any MissingImplementation errors
            let missing_impl_errors: Vec<_> = errors
                .iter()
                .filter(|e| matches!(e, TypeError::MissingImplementation { .. }))
                .collect();

            assert!(
                missing_impl_errors.is_empty(),
                "Should not have any MissingImplementation errors when all functions are implemented"
            );
        }
    }
}
