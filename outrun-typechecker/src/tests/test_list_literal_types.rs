use crate::{CompilationResult, Package};
use outrun_parser::parse_program;

#[test]
fn test_list_literal_infers_concrete_type() {
    let source = r#"
        def test_function(): Outrun.Core.List<Outrun.Core.Integer64> {
            [1, 2, 3]
        }
    "#;

    let parsed = parse_program(source).expect("Should parse successfully");
    let mut package = Package::new("test".to_string());
    package.add_program(parsed);

    let result = CompilationResult::compile_package(&mut package);
    assert!(
        result.is_ok(),
        "Expected successful compilation, got: {:?}",
        result.err()
    );

    // Check that the compilation succeeded, which means our concrete type fix worked
    let _compilation_result = result.unwrap();

    // The main test is that compilation succeeded, which means our concrete type fix worked
    // If the list literal had an incorrect type, the typechecker would have failed
}

#[test]
fn test_empty_list_literal_inference() {
    let source = r#"
        def test_function(): Outrun.Core.List<Outrun.Core.Integer64> {
            []
        }
    "#;

    let parsed = parse_program(source).expect("Should parse successfully");
    let mut package = Package::new("test".to_string());
    package.add_program(parsed);

    let result = CompilationResult::compile_package(&mut package);
    assert!(
        result.is_ok(),
        "Expected successful compilation for empty list, got: {:?}",
        result.err()
    );
}
