use crate::{CompilationResult, Package};
use outrun_parser::parse_program;

#[test]
fn test_shorthand_map_literal_inference() {
    let source = r#"
        def test_function(): Outrun.Core.Map<Outrun.Core.Atom, Outrun.Core.Integer64> {
            {a: 1}
        }
    "#;

    let parsed = parse_program(source).expect("Should parse successfully");
    let mut package = Package::new("test".to_string());
    package.add_program(parsed);

    let result = CompilationResult::compile_package(&mut package);
    assert!(
        result.is_ok(),
        "Expected successful compilation for shorthand map literal, got: {:?}",
        result.err()
    );
}

#[test]
fn test_multiple_shorthand_map_entries() {
    let source = r#"
        def test_function(): Outrun.Core.Map<Outrun.Core.Atom, Outrun.Core.Integer64> {
            {a: 1, b: 2, c: 3}
        }
    "#;

    let parsed = parse_program(source).expect("Should parse successfully");
    let mut package = Package::new("test".to_string());
    package.add_program(parsed);

    let result = CompilationResult::compile_package(&mut package);
    assert!(
        result.is_ok(),
        "Expected successful compilation for multiple shorthand entries, got: {:?}",
        result.err()
    );
}

#[test]
fn test_assignment_map_literal_inference() {
    let source = r#"
        def test_function(): Outrun.Core.Map<Outrun.Core.String, Outrun.Core.Integer64> {
            {"key" => 1}
        }
    "#;

    let parsed = parse_program(source).expect("Should parse successfully");
    let mut package = Package::new("test".to_string());
    package.add_program(parsed);

    let result = CompilationResult::compile_package(&mut package);
    assert!(
        result.is_ok(),
        "Expected successful compilation for assignment map literal, got: {:?}",
        result.err()
    );
}

#[test]
fn test_mixed_map_entries_type_error() {
    let source = r#"
        def test_function(): Outrun.Core.Map<Outrun.Core.Atom, Outrun.Core.Integer64> {
            {a: 1, "key" => 2}
        }
    "#;

    let parsed = parse_program(source).expect("Should parse successfully");
    let mut package = Package::new("test".to_string());
    package.add_program(parsed);

    let result = CompilationResult::compile_package(&mut package);
    assert!(result.is_err(), "Expected type error for mixed key types");

    // Verify it's the right kind of error
    if let Err(error) = result {
        let error_string = format!("{:?}", error);
        assert!(
            error_string.contains("AmbiguousType") || error_string.contains("type"),
            "Expected type-related error, got: {}",
            error_string
        );
    }
}

#[test]
fn test_empty_map_literal_inference() {
    let source = r#"
        def test_function(): Outrun.Core.Map<Outrun.Core.String, Outrun.Core.Integer64> {
            {}
        }
    "#;

    let parsed = parse_program(source).expect("Should parse successfully");
    let mut package = Package::new("test".to_string());
    package.add_program(parsed);

    let result = CompilationResult::compile_package(&mut package);
    assert!(
        result.is_ok(),
        "Expected successful compilation for empty map literal, got: {:?}",
        result.err()
    );
}
