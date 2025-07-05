//! Test for SMT-guided monomorphization dispatch issue

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::ProgramCollection;
use outrun_parser::parse_program;

#[test]
fn test_monomorphization_dispatch_simple() {
    // Simple test without using core library to avoid conflicts
    let test_source = r#"
trait Container<T> {
    def wrap(value: Self, input: T): Box<T>
}

struct Box<T>(value: T)

impl<T> Container<T> for Box<T> {
    def wrap(value: Self, input: T): Box<T> {
        Box { value: input }
    }
}

def test_monomorphization(): Box<Integer> {
    let container = Box { value: 0 }
    Container.wrap(value: container, input: 42)
}
"#;

    let mut collection = ProgramCollection::new();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Monomorphization dispatch test passed!");
        }
        Err(errors) => {
            println!(
                "❌ Monomorphization dispatch test failed with {} errors:",
                errors.len()
            );
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
                
                // Check if this is the "No generic trait implementation found" error
                if format!("{:?}", error).contains("No generic trait implementation found") {
                    println!("🔍 Found the monomorphization dispatch error!");
                    println!("This confirms the issue is in the dispatch lookup system.");
                }
                
                // Check if this is the "Could not resolve argument type" error
                if format!("{:?}", error).contains("Could not resolve argument type") {
                    println!("🔍 Found the argument type resolution error!");
                    println!("This suggests the dispatch system is running too early in the compilation pipeline.");
                }
            }
        }
    }
}

#[test]
fn test_monomorphization_dispatch_with_inference() {
    // Test with type inference to trigger monomorphization
    let test_source = r#"
trait Maybe<T> {
    def some(value: Self, input: T): Wrapper<T>
}

struct Wrapper<T>(data: T)

impl<T> Maybe<T> for Wrapper<T> {
    def some(value: Self, input: T): Wrapper<T> {
        Wrapper { data: input }
    }
}

def test_inference(): Wrapper<String> {
    let wrapper = Wrapper { data: "temp" }
    Maybe.some(value: wrapper, input: "hello")
}
"#;

    let mut collection = ProgramCollection::new();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Monomorphization with type inference test passed!");
        }
        Err(errors) => {
            println!(
                "❌ Monomorphization with type inference test failed with {} errors:",
                errors.len()
            );
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
                
                // Check if this is the "No generic trait implementation found" error
                if format!("{:?}", error).contains("No generic trait implementation found") {
                    println!("🔍 Found the monomorphization dispatch error!");
                    println!("This confirms the issue is in the dispatch lookup system.");
                }
            }
        }
    }
}