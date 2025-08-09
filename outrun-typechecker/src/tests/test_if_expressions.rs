//! Tests for if expression type checking with Default constraints

use crate::{typecheck_package, Package};
use outrun_parser::parse_program;

#[test]
fn test_if_expression_with_both_branches() {
    // Test a simple program with if expression that has both branches
    let test_code = r#"
def test_if(): Outrun.Core.Integer64 {
    if true {
        42
    } else {
        0
    }
}
"#;

    let program = parse_program(test_code).expect("Parse should succeed");
    let mut package = Package::new("if-test-both-branches".to_string());
    package.add_program(program);
    let result = typecheck_package(&mut package);

    // This should succeed since both branches are present and compatible
    match result {
        Ok(_) => {} // Success
        Err(e) => panic!(
            "If expression with both branches should succeed, but got error: {:?}",
            e
        ),
    }
}

#[test]
fn test_if_expression_without_else_requires_default() {
    // Test a program with if expression that has no else branch
    // This should succeed because Integer64 implements Default in core library
    let test_code = r#"
def test_if(): Outrun.Core.Integer64 {
    if true {
        42
    }
}
"#;

    let program = parse_program(test_code).expect("Parse should succeed");
    let mut package = Package::new("if-test-default".to_string());
    package.add_program(program);
    let result = typecheck_package(&mut package);

    // This should succeed if Integer64 implements Default (which it does in core library)
    match result {
        Ok(_) => {}, // Success
        Err(e) => panic!("If expression without else should succeed when type implements Default, but got error: {:?}", e),
    }
}

#[test]
fn test_if_expression_incompatible_branch_types() {
    // Test that if/else branches must have compatible types
    let test_code = r#"
def test_if(): Outrun.Core.Integer64 {
    if true {
        42
    } else {
        "hello"
    }
}
"#;

    let program = parse_program(test_code).expect("Parse should succeed");
    let mut package = Package::new("if-test-incompatible".to_string());
    package.add_program(program);
    let result = typecheck_package(&mut package);

    // This should fail because Integer64 and String are incompatible
    assert!(
        result.is_err(),
        "If expression with incompatible branch types should fail"
    );
}

#[test]
fn test_if_expression_without_else_fails_when_no_default() {
    // Test a user-defined type that doesn't implement Default
    // This should fail when used in if without else branch
    let test_code = r#"
struct CustomType(value: Outrun.Core.Integer64) {}

def test_if(): CustomType {
    if true {
        CustomType { value: 42 }
    }
}
"#;

    let program = parse_program(test_code).expect("Parse should succeed");
    let mut package = Package::new("if-test-no-default".to_string());
    package.add_program(program);
    let result = typecheck_package(&mut package);

    // This should fail because CustomType doesn't implement Default
    match result {
        Ok(_) => panic!("If expression without else should fail when type doesn't implement Default, but it succeeded"),
        Err(e) => {
            // Verify it's an error related to Default implementation
            println!("✅ Correctly failed with error: {:?}", e);
            // The error should be about missing Default implementation
            let error_str = format!("{:?}", e);
            assert!(
                error_str.contains("Default") && (error_str.contains("ConstraintError") || error_str.contains("InferenceError")), 
                "Expected error about Default implementation, got: {:?}", e
            );
        }
    }
}
