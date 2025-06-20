use crate::error::TypeError;
use crate::multi_program_compiler::{MultiProgramCompiler, ProgramCollection};
use outrun_parser::{parse_program, Program};

fn create_program_from_source(source: &str) -> Program {
    parse_program(source).unwrap_or_else(|e| {
        panic!("Failed to parse test program: {:?}\nSource: {}", e, source);
    })
}

#[test]
fn test_impl_validation_missing_function() {
    let trait_source = r#"
trait TestTrait {
    def show(value: Self): String
    def debug(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestTrait for MyType {
    def show(value: Self): String {
        "MyType"
    }
    # Missing debug function
}
"#;

    let trait_program = create_program_from_source(trait_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
    collection.add_program(
        "trait.outrun".to_string(),
        trait_program,
        trait_source.to_string(),
    );
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    println!(
        "DEBUG: Collection file paths: {:?}",
        collection.file_paths()
    );
    println!(
        "DEBUG: Collection has {} programs",
        collection.programs.len()
    );

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

    match result {
        Ok(success_result) => {
            println!("Compilation succeeded unexpectedly!");
            println!("Compilation order: {:?}", success_result.compilation_order);
            println!(
                "Functions found: {}",
                success_result.function_registry.len()
            );
            println!("Implementations: {}", success_result.implementations.len());
            println!("Traits found: {}", success_result.traits.len());
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
    let trait_source = r#"
trait TestTrait {
    def show(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestTrait for MyType {
    def show(value: Self): String {
        "MyType"
    }
    
    def extra_function(value: Self): String {
        "Extra"
    }
}
"#;

    let trait_program = create_program_from_source(trait_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
    collection.add_program(
        "trait.outrun".to_string(),
        trait_program,
        trait_source.to_string(),
    );
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

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
    let trait_source = r#"
trait TestTrait {
    def show(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestTrait for MyType {
    def show(item: Self): String {
        "MyType"
    }
}
"#;

    let trait_program = create_program_from_source(trait_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
    collection.add_program(
        "trait.outrun".to_string(),
        trait_program,
        trait_source.to_string(),
    );
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

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
    let trait_source = r#"
trait TestTrait {
    def show(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestTrait for MyType {
    def show(value: String): String {
        "MyType"
    }
}
"#;

    let trait_program = create_program_from_source(trait_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
    collection.add_program(
        "trait.outrun".to_string(),
        trait_program,
        trait_source.to_string(),
    );
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

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
    let trait_source = r#"
trait TestTrait {
    def show(value: Self): String
}
"#;

    let impl_source = r#"
struct MyType() {}

impl TestTrait for MyType {
    def show(value: Self): Integer {
        42
    }
}
"#;

    let trait_program = create_program_from_source(trait_source);
    let impl_program = create_program_from_source(impl_source);

    let mut collection = ProgramCollection::from_core_library();
    collection.add_program(
        "trait.outrun".to_string(),
        trait_program,
        trait_source.to_string(),
    );
    collection.add_program(
        "impl.outrun".to_string(),
        impl_program,
        impl_source.to_string(),
    );

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

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
trait TestTrait {
    def process(value: Self): Self
}

struct MyType() {}

impl TestTrait for MyType {
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

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

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
fn test_impl_validation_undefined_trait() {
    let impl_source = r#"
struct MyType() {}

impl UndefinedTrait for MyType {
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

    let mut compiler = MultiProgramCompiler::new();
    let result = compiler.compile(&collection);

    match result {
        Ok(compilation_result) => {
            println!("Compilation succeeded unexpectedly!");
            println!(
                "Traits found: {:?}",
                compilation_result.traits.keys().collect::<Vec<_>>()
            );
            println!(
                "Implementations: {}",
                compilation_result.implementations.len()
            );

            // Let's debug what traits are available
            for (trait_id, trait_def) in &compilation_result.traits {
                let trait_name = compilation_result
                    .type_context
                    .type_interner
                    .type_name(*trait_id)
                    .unwrap_or("Unknown".to_string());
                println!("Available trait: {} -> {:?}", trait_name, trait_def.name);
            }

            panic!("Expected compilation to fail due to undefined trait, but it succeeded");
        }
        Err(errors) => {
            println!("Got {} errors:", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i, error);
            }

            assert!(!errors.is_empty(), "Expected at least one error");

            // Check that we get an UndefinedTrait error
            let undefined_trait_error = errors.iter().find(|e| {
                matches!(e, TypeError::UndefinedTrait { trait_name, .. } if trait_name == "UndefinedTrait")
            });

            assert!(
                undefined_trait_error.is_some(),
                "Expected UndefinedTrait error"
            );
        }
    }
}
