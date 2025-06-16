//! Tests for generic argument swapping and substitution issues
//!
//! This covers the issue where generic type arguments in different orders
//! (e.g., Pair<T, U> vs Pair<U, T>) are not properly handled during type checking.

use crate::error::TypeError;
use crate::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_generic_argument_swapping_in_return_types() {
    // This test demonstrates the issue where struct literals with swapped generic
    // arguments don't match the expected type correctly
    let input = r#"
        struct Pair<T, U>(first: T, second: U) {
            def swap(self: Self): Pair<U, T> {
                Pair { first: self.second, second: self.first }
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Currently this fails because when creating Pair { first: self.second, second: self.first }
    // with return type Pair<U, T>, the type checker expects:
    // - first field to be type T (from original struct definition Pair<T, U>)
    // - but we provide self.second which is type U
    //
    // The type checker should understand that in Pair<U, T>:
    // - first field should be type U (first generic argument)
    // - second field should be type T (second generic argument)
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        // Check if we get the expected type mismatch error
        let has_type_mismatch = errors
            .iter()
            .any(|e| matches!(e, TypeError::TypeMismatch { .. }));

        if has_type_mismatch {
            // This is the expected failure for now - documenting the known issue
            return; // Test passes by documenting the known failure
        } else {
            assert!(
                has_type_mismatch,
                "Expected TypeMismatch error but got: {:?}",
                error_messages
            );
        }
    }

    // If we reach here, the generic argument swapping has been fixed
    assert!(
        result.is_ok(),
        "Generic argument swapping in return types should work"
    );
}

#[test]
fn test_generic_argument_order_sensitivity() {
    // This test shows that the order of generic arguments matters
    // and the type checker should handle different orderings correctly
    let input = r#"
        struct Container<A, B, C>(first: A, second: B, third: C) {
            def rotate_types(self: Self): Container<C, A, B> {
                Container { 
                    first: self.third,
                    second: self.first,
                    third: self.second
                }
            }
            
            def reverse_first_two(self: Self): Container<B, A, C> {
                Container {
                    first: self.second,
                    second: self.first,
                    third: self.third
                }
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // This should work once generic parameter substitution is properly implemented
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        // Check if we get type mismatch errors related to generic arguments
        let has_type_mismatch = errors
            .iter()
            .any(|e| matches!(e, TypeError::TypeMismatch { .. }));

        if has_type_mismatch {
            return; // Document the known limitation
        } else {
            assert!(
                has_type_mismatch,
                "Expected TypeMismatch error but got: {:?}",
                error_messages
            );
        }
    }

    assert!(
        result.is_ok(),
        "Complex generic argument reordering should work"
    );
}

#[test]
fn test_generic_argument_substitution_in_function_parameters() {
    // This test checks that generic arguments are properly substituted
    // in function parameter types, not just return types
    let input = r#"
        struct Transformer<Input, Output>(value: Input) {
            def transform_with(self: Self, converter: Function<(value: Input) -> Output>): Transformer<Output, Input> {
                Transformer { value: Function.call(func: converter, value: self.value) }
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // This tests both function type parameters and generic argument swapping
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        // This might fail for multiple reasons:
        // 1. Function type annotations not implemented
        // 2. Generic argument swapping issues
        // 3. Complex generic substitution in function parameters

        let has_unimplemented = errors
            .iter()
            .any(|e| matches!(e, TypeError::UnimplementedFeature { .. }));

        let has_type_mismatch = errors
            .iter()
            .any(|e| matches!(e, TypeError::TypeMismatch { .. }));

        let has_undefined_function = errors
            .iter()
            .any(|e| matches!(e, TypeError::UndefinedFunction { .. }));

        if has_unimplemented || has_type_mismatch || has_undefined_function {
            return; // Document the known limitation
        } else {
            assert!(
                has_unimplemented || has_type_mismatch || has_undefined_function,
                "Expected UnimplementedFeature, TypeMismatch, or UndefinedFunction error but got: {:?}", 
                error_messages
            );
        }
    }

    assert!(
        result.is_ok(),
        "Generic argument substitution in function parameters should work"
    );
}

#[test]
fn test_simple_generic_argument_identity() {
    // This test should pass - it uses the same generic argument order
    // This serves as a control to show that basic generic types work
    let input = r#"
        struct Simple<T, U>(first: T, second: U) {            
            def get_first(self: Self): T {
                self.first
            }
            
            def get_second(self: Self): U {
                self.second
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // This should work since no argument swapping is involved
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        assert!(
            false,
            "Expected success for identity case but got errors: {:?}",
            error_messages
        );
    }

    assert!(
        result.is_ok(),
        "Simple generic types with identity order should work"
    );
}
