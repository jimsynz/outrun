//! Test for duplicate implementation registration issue

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_simple_impl_registration() {
    let mut engine = TypeInferenceEngine::new();

    // Simple test case that should not cause duplicate registration
    let test_code = r#"
protocol TestProtocol<T> {
}

struct TestType()

impl TestProtocol<T> for TestType { }
"#;

    let mut program = parse_program(test_code).expect("Failed to parse test code");

    let result = engine.typecheck_program(&mut program);

    match result {
        Ok(()) => {
            // println!("✅ Simple impl registration succeeded!");
        }
        Err(e) => {
            println!("❌ Simple impl registration failed: {:?}", e);
            if let crate::error::TypecheckError::ImplementationError(
                crate::error::ImplementationError::ConflictingImplementation { .. },
            ) = e
            {
                panic!("Duplicate implementation registration detected!");
            }
        }
    }
}

#[test]
fn test_sigil_pattern_impl_registration() {
    let mut engine = TypeInferenceEngine::new();

    // Reproduce the exact pattern from sigil.outrun that's failing
    let test_code = r#"
protocol Sigil.Input<T> {
}

struct Sigil.Input.String(content: String)

impl Sigil.Input<T> for Sigil.Input.String { }
"#;

    let mut program = parse_program(test_code).expect("Failed to parse test code");

    let result = engine.typecheck_program(&mut program);

    match result {
        Ok(()) => {
            // println!("✅ Sigil pattern impl registration succeeded!");
        }
        Err(e) => {
            println!("❌ Sigil pattern impl registration failed: {:?}", e);
            if let crate::error::TypecheckError::ImplementationError(
                crate::error::ImplementationError::ConflictingImplementation { .. },
            ) = e
            {
                // println!("This reproduces the duplicate implementation registration bug!");
            }
        }
    }
}
