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
fn test_impl_validation_missing_function() {
    let protocol_source = r#"
protocol TestProtocol {
    def show(value: Self): String
    def debug(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestProtocol for MyType {
    def show(value: Self): String {
        "MyType"
    }
    # Missing debug function
}
"#;

    let protocol_program = create_program_from_source(protocol_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
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
        Ok(success_result) => {
            println!("Compilation succeeded unexpectedly!");
            println!("Compilation order: {:?}", success_result.compilation_order);
            println!(
                "Functions found: {}",
                0 // Function count no longer accessible through type_context
            );
            println!("Implementations: {}", success_result.implementations.len());
            println!("Protocols found: {}", success_result.protocols.len());
            println!("Structs found: {}", success_result.structs.len());
            panic!("Expected compilation to fail due to missing function");
        }
        Err(errors) => {
            assert!(!errors.is_empty(), "Expected at least one error");
            // Check that we get a MissingImplementation error
            let missing_impl_error = errors.iter().find(|e| {
                matches!(e, TypeError::MissingImplementation { function_name, .. } if function_name == "debug")
            });

            assert!(
                missing_impl_error.is_some(),
                "Expected MissingImplementation error for 'debug' function"
            );
        }
    }
}

#[test]
fn test_impl_validation_extra_function() {
    let protocol_source = r#"
protocol TestProtocol {
    def show(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestProtocol for MyType {
    def show(value: Self): String {
        "MyType"
    }

    def extra_function(value: Self): String {
        "Extra"
    }
}
"#;

    let protocol_program = create_program_from_source(protocol_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
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
        "Expected compilation to fail due to extra function"
    );

    let errors = result.unwrap_err();
    assert!(!errors.is_empty(), "Expected at least one error");

    // Check that we get an ExtraImplementation error
    let extra_impl_error = errors.iter().find(|e| {
        matches!(e, TypeError::ExtraImplementation { function_name, .. } if function_name == "extra_function")
    });

    assert!(
        extra_impl_error.is_some(),
        "Expected ExtraImplementation error for 'extra_function'"
    );
}

#[test]
fn test_impl_validation_wrong_parameter_name() {
    let protocol_source = r#"
protocol TestProtocol {
    def show(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestProtocol for MyType {
    def show(item: Self): String {
        "MyType"
    }
}
"#;

    let protocol_program = create_program_from_source(protocol_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
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
        "Expected compilation to fail due to wrong parameter name"
    );

    let errors = result.unwrap_err();
    assert!(!errors.is_empty(), "Expected at least one error");

    // Check that we get a SignatureMismatch error
    let signature_error = errors.iter().find(|e| {
        matches!(e, TypeError::SignatureMismatch { function_name, expected, found, .. }
            if function_name == "show" && expected.contains("value") && found.contains("item"))
    });

    assert!(
        signature_error.is_some(),
        "Expected SignatureMismatch error for parameter name mismatch"
    );
}

#[test]
fn test_impl_validation_wrong_parameter_type() {
    let protocol_source = r#"
protocol TestProtocol {
    def show(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestProtocol for MyType {
    def show(value: String): String {
        "MyType"
    }
}
"#;

    let protocol_program = create_program_from_source(protocol_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
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
        "Expected compilation to fail due to wrong parameter type"
    );

    let errors = result.unwrap_err();
    assert!(!errors.is_empty(), "Expected at least one error");

    // Check that we get a SignatureMismatch error for parameter type
    let signature_error = errors.iter().find(|e| {
        matches!(e, TypeError::SignatureMismatch { function_name, .. } if function_name == "show")
    });

    assert!(
        signature_error.is_some(),
        "Expected SignatureMismatch error for parameter type mismatch"
    );
}

#[test]
fn test_impl_validation_wrong_return_type() {
    let protocol_source = r#"
protocol TestProtocol {
    def show(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestProtocol for MyType {
    def show(value: Self): Integer {
        42
    }
}
"#;

    let protocol_program = create_program_from_source(protocol_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
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
        "Expected compilation to fail due to wrong return type"
    );

    let errors = result.unwrap_err();
    assert!(!errors.is_empty(), "Expected at least one error");

    // Check that we get a SignatureMismatch error for return type
    let signature_error = errors.iter().find(|e| {
        matches!(e, TypeError::SignatureMismatch { function_name, expected, found, .. }
            if function_name == "show" && expected.contains("String") && found.contains("Integer"))
    });

    assert!(
        signature_error.is_some(),
        "Expected SignatureMismatch error for return type mismatch"
    );
}

#[test]
fn test_impl_validation_valid_implementation() {
    let combined_source = r#"
protocol TestProtocol {
    def process(value: Self): Self
}

struct MyType() {}

impl TestProtocol for MyType {
    def process(value: Self): Self {
        value
    }
}
"#;

    let program = create_program_from_source(combined_source);

    let mut collection = ProgramCollection::new();
    collection.add_program(
        "test.outrun".to_string(),
        program,
        combined_source.to_string(),
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
            panic!("Expected compilation to succeed for valid implementation");
        }
    }
}

#[test]
fn test_impl_validation_undefined_protocol() {
    let impl_source = r#"
struct MyType() {}

impl UndefinedProtocol for MyType {
    def some_function(value: Self): String {
        "MyType"
    }
}
"#;

    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::new();
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(compilation_result) => {
            println!("Compilation succeeded unexpectedly!");
            println!(
                "Protocols found: {:?}",
                compilation_result.protocols.keys().collect::<Vec<_>>()
            );
            println!(
                "Implementations: {}",
                compilation_result.implementations.len()
            );

            // Let's debug what protocols are available
            for protocol_def in compilation_result.protocols.values() {
                let protocol_name = "Unknown".to_string(); // Type name resolution no longer available through type_context
                println!(
                    "Available protocol: {} -> {:?}",
                    protocol_name, protocol_def.name
                );
            }

            panic!("Expected compilation to fail due to undefined protocol, but it succeeded");
        }
        Err(errors) => {
            println!("Got {} errors:", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {i}: {error:?}");
            }

            assert!(!errors.is_empty(), "Expected at least one error");

            // Check that we get an UndefinedProtocol error
            let undefined_protocol_error = errors.iter().find(|e| {
                matches!(e, TypeError::UndefinedProtocol { protocol_name, .. } if protocol_name == "UndefinedProtocol")
            });

            assert!(
                undefined_protocol_error.is_some(),
                "Expected UndefinedProtocol error"
            );
        }
    }
}

#[test]
fn test_impl_validation_undefined_protocol_span_information() {
    let impl_source = r#"
struct MyType() {}

impl UndefinedProtocol for MyType {
    def some_function(value: Self): String {
        "MyType"
    }
}
"#;

    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::new();
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_compilation_result) => {
            panic!("Expected compilation to fail due to undefined protocol, but it succeeded");
        }
        Err(errors) => {
            assert!(!errors.is_empty(), "Expected at least one error");

            // Check that we get an UndefinedProtocol error with proper span
            let undefined_protocol_error = errors.iter().find(|e| {
                matches!(e, TypeError::UndefinedProtocol { protocol_name, .. } if protocol_name == "UndefinedProtocol")
            });

            assert!(
                undefined_protocol_error.is_some(),
                "Expected UndefinedProtocol error but got: {errors:?}"
            );

            // Extract the span and verify it's not (0,0)
            if let Some(TypeError::UndefinedProtocol {
                span,
                protocol_name,
                ..
            }) = undefined_protocol_error
            {
                assert_eq!(protocol_name, "UndefinedProtocol");

                // Verify the span is not empty (0,0)
                assert!(
                    span.offset() > 0,
                    "Span offset should not be 0, got: {span:?}"
                );
                assert!(
                    !span.is_empty(),
                    "Span length should not be 0, got: {span:?}"
                );

                // The span should include the protocol name, verify it contains "UndefinedProtocol"
                // The TypeSpec span might include additional context, so we check it contains the protocol name
                let span_start = span.offset();
                let span_end = span_start + span.len();
                let protocol_name_start = impl_source
                    .find("UndefinedProtocol")
                    .expect("Should find UndefinedProtocol in source");
                let protocol_name_end = protocol_name_start + "UndefinedProtocol".len();

                assert!(
                    span_start <= protocol_name_start && span_end >= protocol_name_end,
                    "Span [{span_start}, {span_end}) should include 'UndefinedProtocol' at [{protocol_name_start}, {protocol_name_end})"
                );
            }
        }
    }
}
