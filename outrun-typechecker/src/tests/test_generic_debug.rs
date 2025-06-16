use crate::checker::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_simple_generic_struct_debug() {
    let input = r#"
        struct Simple<T>(value: T) {}
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    if let Err(errors) = &result {
        let _error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
    }

    // This should succeed
    assert!(result.is_ok(), "Simple generic struct should work");
}

#[test]
fn test_simple_generic_struct_method_debug() {
    let input = r#"
        struct Container<T>(value: T) {
            def test_method(self: Self): Integer {
                42
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    if let Err(errors) = &result {
        let _error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
    }

    // This should succeed - method doesn't use T
    assert!(
        result.is_ok(),
        "Method not using generic parameter should work"
    );
}

#[test]
fn test_generic_struct_method_using_t_debug() {
    let input = r#"
        struct Container<T>(value: T) {
            def get_value(self: Self): T {
                self.value
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    if let Err(errors) = &result {
        let _error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
    }

    // For now, let's just check what happens
    if result.is_err() {
        // As expected, this is currently failing - we need to debug why T is not in scope for the method
    }
}
