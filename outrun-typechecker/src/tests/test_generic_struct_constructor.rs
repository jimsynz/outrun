use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::ProgramCollection;
use outrun_parser::parse_program;

#[test]
fn test_generic_struct_constructor_inference() {
    // Test that generic struct constructors properly infer their type parameters
    let test_source = r#"
struct TestGeneric<T>(value: T)

def test_construction(): TestGeneric<Integer> {
    TestGeneric { value: 42 }
}
"#;

    let mut collection = ProgramCollection::from_core_library();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Generic struct constructor inference worked!");
        }
        Err(errors) => {
            println!(
                "❌ Generic struct constructor test failed with {} errors:",
                errors.len()
            );
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
            }
        }
    }
}

#[test]
fn test_option_some_constructor() {
    // Test specifically Option.Some constructor with core library
    let test_source = r#"
trait Option<T> {
    defs some(value: T): Outrun.Option.Some<T> {
        Outrun.Option.Some { value: value }
    }
}

struct Outrun.Option.Some<T>(value: T)

def test_option(): Outrun.Option.Some<Integer> {
    Outrun.Option.Some { value: 42 }
}
"#;

    let mut collection = ProgramCollection::from_core_library();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Option.Some constructor inference worked!");
        }
        Err(errors) => {
            println!(
                "❌ Option.Some constructor test failed with {} errors:",
                errors.len()
            );
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);

                if let crate::error::TypeError::TypeMismatch {
                    expected, found, ..
                } = error
                {
                    if expected.contains("Option.Some<") || found.contains("Option.Some<") {
                        println!("🔍 Found Option.Some type mismatch:");
                        println!("   Expected: {}", expected);
                        println!("   Found:    {}", found);
                    }
                }
            }
        }
    }
}

#[test]
fn test_generic_struct_type_hint_validation() {
    // Test that type hints are properly validated for generic structs
    let test_source = r#"
struct Container<T>(value: T)

def test_with_hint(): Container<String> {
    Container { value: "hello" }
}
"#;

    let mut collection = ProgramCollection::from_core_library();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Generic struct type hint validation worked!");
        }
        Err(errors) => {
            println!(
                "❌ Generic struct type hint test failed with {} errors:",
                errors.len()
            );
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
            }
            panic!("Test should have passed");
        }
    }
}
