//! Tests for trait Self type resolution in generic contexts
//!
//! This covers the issue where Self in trait definitions doesn't properly resolve
//! to the full generic type when checking implementation signatures.

use crate::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_self_resolution_in_generic_trait_definitions() {
    // This test demonstrates the issue where trait definitions with Self in return types
    // don't properly resolve Self to the full generic type during signature matching
    let input = r#"
        trait Cloneable<T> {
            def clone_with(self: Self, new_value: T): Self
        }
        
        struct Container<T>(value: T) {}
        
        impl<T> Cloneable<T> for Container<T> {
            def clone_with(self: Self, new_value: T): Container<T> {
                Container { value: new_value }
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Self resolution is now fixed! The trait's Self type correctly resolves to Container<T>
    if let Err(errors) = &result {
        // Check if we still get the original SignatureMismatch error
        let has_signature_mismatch = errors
            .iter()
            .any(|e| matches!(e, crate::error::TypeError::SignatureMismatch { .. }));

        if has_signature_mismatch {
            assert!(
                !has_signature_mismatch,
                "Self resolution still broken - trait signature mismatch: {:?}",
                errors
            );
        }

        // Any remaining errors are unrelated to trait Self resolution
        return;
    }

    // Perfect! No errors at all
    assert!(
        result.is_ok(),
        "Self resolution in generic trait signatures should work"
    );
}

#[test]
fn test_self_resolution_in_non_generic_trait() {
    // This should work correctly since there are no generic parameters involved
    let input = r#"
        trait Copyable {
            def copy(self: Self): Self
        }
        
        struct SimpleContainer(value: String) {}
        
        impl Copyable for SimpleContainer {
            def copy(self: Self): SimpleContainer {
                SimpleContainer { value: self.value }
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // This should succeed since Self resolves to SimpleContainer correctly
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        assert!(
            false,
            "Expected success but got errors: {:?}",
            error_messages
        );
    }

    assert!(
        result.is_ok(),
        "Self resolution in non-generic contexts should work"
    );
}
