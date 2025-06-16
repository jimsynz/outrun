use crate::checker::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_trait_generic_arity_mismatch_no_generics() {
    let input = r#"
        trait Display {
            def to_string(self: Self): Outrun.Core.String  
        }

        def foo(value: Display<String>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail when trait with no generics is given generic arguments"
    );
    let errors = result.unwrap_err();
    let has_arity_error = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::GenericArityMismatch { .. }));
    assert!(
        has_arity_error,
        "Should detect generic arity mismatch: {:?}",
        errors
    );
}

#[test]
fn test_trait_generic_arity_mismatch_wrong_count() {
    let input = r#"
        trait Convert<T> {
            def convert(self: Self): T
        }

        def foo(value: Convert<String, Integer>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail when trait expects 1 generic but 2 are provided"
    );
    let errors = result.unwrap_err();
    let has_arity_error = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::GenericArityMismatch { .. }));
    assert!(
        has_arity_error,
        "Should detect generic arity mismatch: {:?}",
        errors
    );
}

#[test]
fn test_trait_generic_arity_correct() {
    let input = r#"
        trait Convert<T> {
            def convert(self: Self): T
        }

        def foo(value: Convert<String>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Should succeed when trait generic arity is correct: {:?}",
        result.err()
    );
}

#[test]
fn test_builtin_type_generic_arity_mismatch() {
    let input = r#"
        def foo(value: List<String, Integer>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail when List<T> is given 2 generic arguments"
    );
    let errors = result.unwrap_err();
    let has_arity_error = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::GenericArityMismatch { .. }));
    assert!(
        has_arity_error,
        "Should detect generic arity mismatch for List: {:?}",
        errors
    );
}

#[test]
fn test_primitive_type_generic_arity_mismatch() {
    let input = r#"
        def foo(value: String<Integer>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail when String is given generic arguments"
    );
    let errors = result.unwrap_err();
    let has_arity_error = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::GenericArityMismatch { .. }));
    assert!(
        has_arity_error,
        "Should detect generic arity mismatch for String: {:?}",
        errors
    );
}

#[test]
fn test_builtin_type_correct_arity() {
    let input = r#"
        def foo(
            list: List<String>, 
            option: Option<Integer>, 
            result: Result<String, String>,
            map: Map<String, Integer>
        ): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Should succeed when all built-in types have correct arity: {:?}",
        result.err()
    );
}

#[test]
fn test_result_type_missing_generics() {
    let input = r#"
        def foo(value: Result): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail when Result<T, E> is given no generic arguments"
    );
    let errors = result.unwrap_err();
    let has_arity_error = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::GenericArityMismatch { .. }));
    assert!(
        has_arity_error,
        "Should detect generic arity mismatch for Result: {:?}",
        errors
    );
}

#[test]
fn test_struct_generic_arity_mismatch_no_generics() {
    let input = r#"
        struct User(name: String, age: Integer) {}

        def foo(user: User<String>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail when struct with no generics is given generic arguments"
    );
    let errors = result.unwrap_err();
    let has_arity_error = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::GenericArityMismatch { .. }));
    assert!(
        has_arity_error,
        "Should detect generic arity mismatch for struct: {:?}",
        errors
    );
}

#[test]
fn test_struct_generic_arity_mismatch_wrong_count() {
    let input = r#"
        struct Container<T>(value: T) {}

        let _container: Container<String> = Container { value: "test" }

        def foo(container: Container<String, Integer>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail when struct expects 1 generic but 2 are provided"
    );
    let errors = result.unwrap_err();
    let has_arity_error = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::GenericArityMismatch { .. }));
    assert!(
        has_arity_error,
        "Should detect generic arity mismatch for Container struct: {:?}",
        errors
    );
}

#[test]
fn test_struct_generic_arity_correct() {
    let input = r#"
        struct Container<T>(value: T) {}

        def foo(container: Container<Integer>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Should succeed when struct generic arity is correct: {:?}",
        result.err()
    );
}

#[test]
fn test_struct_multiple_generics_correct() {
    let input = r#"
        struct Pair<T, U>(first: T, second: U) {}

        def foo(pair: Pair<String, Boolean>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Should succeed when struct with multiple generics has correct arity: {:?}",
        result.err()
    );
}

#[test]
fn test_struct_multiple_generics_wrong_count() {
    let input = r#"
        struct Pair<T, U>(first: T, second: U) {}

        def foo(pair: Pair<String>): String {
            "result"
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail when struct expects 2 generics but 1 is provided"
    );
    let errors = result.unwrap_err();
    let has_arity_error = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::GenericArityMismatch { .. }));
    assert!(
        has_arity_error,
        "Should detect generic arity mismatch for Pair struct: {:?}",
        errors
    );
}
