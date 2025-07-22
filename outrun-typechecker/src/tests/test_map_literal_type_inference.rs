use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::ProgramCollection;
/// Tests for map literal type inference with type hints
use crate::error::TypeError;
use outrun_parser::{parse_program, Program};

fn create_program_from_source(source: &str) -> Program {
    parse_program(source).unwrap_or_else(|e| {
        panic!("Failed to parse test program: {e:?}\nSource: {source}");
    })
}

/// Helper function to compile a single program and return errors
fn compile_program(source: &str) -> Result<(), Vec<TypeError>> {
    let program = create_program_from_source(source);
    let mut collection = ProgramCollection::from_core_library();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => Ok(()),
        Err(errors) => Err(errors),
    }
}

/// Test that empty map literals use type hints when available
#[test]
fn test_empty_map_with_type_hint_success() {
    let source = r#"
        let my_map: Map<String, Integer> = {}
    "#;

    let result = compile_program(source);
    assert!(
        result.is_ok(),
        "Expected successful type checking, got: {:?}",
        result.err()
    );
}

/// Test that empty map literals fall back to Map<Any, Any> without type hints
#[test]
fn test_empty_map_without_type_hint_fallback() {
    let source = r#"
        let my_map = {}
    "#;

    let result = compile_program(source);
    assert!(
        result.is_ok(),
        "Expected successful type checking, got: {:?}",
        result.err()
    );
}

/// Test that invalid type hints fall back to Map<Any, Any>
#[test]
fn test_empty_map_with_invalid_type_hint_fallback() {
    let source = r#"
        let my_map: String = {}
    "#;

    let result = compile_program(source);
    // This should fail due to type mismatch, but the map literal itself should be typed as Map<Any, Any>
    assert!(
        result.is_err(),
        "Expected type checking failure due to type mismatch"
    );
}

/// Test that non-empty maps still work correctly (regression test)
#[test]
fn test_non_empty_map_still_works() {
    let source = r#"
        let my_map = { "key" => 42 }
    "#;

    let result = compile_program(source);
    assert!(
        result.is_ok(),
        "Expected successful type checking, got: {:?}",
        result.err()
    );
}

/// Test type hint with generic type parameters
#[test]
fn test_empty_map_with_complex_type_hint() {
    let source = r#"
        let my_map: Map<Option<String>, List<Integer>> = {}
    "#;

    let result = compile_program(source);
    assert!(
        result.is_ok(),
        "Expected successful type checking, got: {:?}",
        result.err()
    );
}

/// Test that type hints are respected in function return types
#[test]
fn test_empty_map_with_return_type_hint() {
    let source = r#"
        protocol MapMaker {
            def make_empty_map(): Map<String, Integer>
        }

        struct MyMaker {}

        impl MapMaker for MyMaker {
            def make_empty_map(): Map<String, Integer> {
                {}
            }
        }
    "#;

    let result = compile_program(source);
    assert!(
        result.is_ok(),
        "Expected successful type checking, got: {:?}",
        result.err()
    );
}
