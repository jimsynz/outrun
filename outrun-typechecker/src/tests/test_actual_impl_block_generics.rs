use crate::checker::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_impl_block_generic_parameter_registration() {
    let input = r#"
        trait Display {
            def to_string(self: Self): String  
        }

        struct Container<T>(value: T) {}

        impl<T> Display for Container<T> {
            def to_string(self: Self): String {
                "container"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // This should succeed - T should be properly registered from impl<T>
    match result {
        Ok(_) => {
            // ✓ impl<T> generic parameter registration works correctly
        }
        Err(errors) => {
            let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
            // ❌ Errors available for debugging

            // Check if we have "Undefined type T" errors
            let has_undefined_t = error_messages
                .iter()
                .any(|msg| msg.contains("Undefined type T"));
            if has_undefined_t {
                assert!(
                    !has_undefined_t,
                    "Generic parameter T is not being properly registered in impl<T> scope"
                );
            } else {
                // Some other error, but not the one we're debugging
                assert!(
                    false,
                    "Unexpected errors (not undefined type T): {:?}",
                    error_messages
                );
            }
        }
    }
}
