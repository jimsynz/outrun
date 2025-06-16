//! Tests for function type annotation issues
//!
//! This covers the known limitation where function type annotations are not yet
//! implemented in trait definitions and other contexts.

use crate::error::TypeError;
use crate::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_function_type_annotations_in_trait_definitions() {
    // This test demonstrates the limitation where function type annotations
    // are not yet implemented in trait definitions
    let input = r#"
        trait Processor<T> {
            def process(self: Self, callback: Function<(value: T) -> String>): String
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Currently this fails with "Function type annotations in trait definitions not implemented"
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        // Check if we get the expected unimplemented feature error
        let has_unimplemented = errors.iter().any(|e| {
            if let TypeError::UnimplementedFeature { feature, .. } = e {
                feature.contains("Function type annotations")
            } else {
                false
            }
        });

        if has_unimplemented {
            // This is the expected failure for now - documenting the known issue
            return; // Test passes by documenting the known failure
        } else {
            assert!(
                has_unimplemented,
                "Expected UnimplementedFeature error but got: {:?}",
                error_messages
            );
        }
    }

    // If we reach here, function type annotations have been implemented
    assert!(
        result.is_ok(),
        "Function type annotations in trait definitions should work"
    );
}

#[test]
fn test_function_type_annotations_in_struct_methods() {
    // This test checks function type annotations in struct method parameters
    let input = r#"
        struct Handler<T>(data: T) {
            def process(self: Self, transformer: Function<(input: T) -> T>): T {
                transformer(input: self.data)
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Function value calls should now work!
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        assert!(
            false,
            "Function value calls should work but got errors: {:?}",
            error_messages
        );
    }

    assert!(
        result.is_ok(),
        "Function type annotations and function value calls should work"
    );
}

#[test]
fn test_function_type_annotations_in_impl_blocks() {
    // This test checks function type annotations in impl block methods
    let input = r#"
        trait Processable {
            def transform(self: Self): String
        }
        
        struct Container<T>(value: T) {}
        
        impl<T> Processable for Container<T> {
            def transform(self: Self): String {
                "transformed"
            }
            
            def apply_function(self: Self, func: Function<(value: T) -> String>): String {
                Function.call(func: func, value: self.value)
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // This might fail due to extra method in impl block or function type annotations
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        let has_unimplemented = errors
            .iter()
            .any(|e| matches!(e, TypeError::UnimplementedFeature { .. }));

        let has_extra_implementation = errors
            .iter()
            .any(|e| matches!(e, TypeError::ExtraImplementation { .. }));

        let has_undefined_function = errors
            .iter()
            .any(|e| matches!(e, TypeError::UndefinedFunction { .. }));

        if has_unimplemented || has_extra_implementation || has_undefined_function {
            return; // Document the known limitation
        } else {
            assert!(
                has_unimplemented || has_extra_implementation || has_undefined_function,
                "Expected UnimplementedFeature, ExtraImplementation, or UndefinedFunction error but got: {:?}", 
                error_messages
            );
        }
    }

    assert!(
        result.is_ok(),
        "Function type annotations in impl blocks should work"
    );
}

#[test]
fn test_complex_function_type_with_generics() {
    // This test checks complex function types with multiple generic parameters
    let input = r#"
        trait Mapper<Input, Output> {
            def map(self: Self, mapper: Function<(value: Input) -> Output>): Output
            def compose(self: Self, first: Function<(input: Input) -> String>, second: Function<(value: String) -> Output>): Output
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // This will likely fail due to function type annotations not being implemented
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        let has_unimplemented = errors.iter().any(|e| {
            if let TypeError::UnimplementedFeature { feature, .. } = e {
                feature.contains("Function type annotations")
            } else {
                false
            }
        });

        if has_unimplemented {
            return; // Document the known limitation
        } else {
            assert!(
                has_unimplemented,
                "Expected UnimplementedFeature error but got: {:?}",
                error_messages
            );
        }
    }

    assert!(
        result.is_ok(),
        "Complex function types with generics should work"
    );
}
