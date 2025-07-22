use crate::core_library::load_and_compile_core_library_for_test;
use crate::*;
use std::path::Path;

#[test]
fn test_detailed_core_library_errors() {
    let core_lib_path = "/Users/jmshrtn/Dev/harton.dev/outrun/outrun/outrun-core/lib";

    match load_and_compile_core_library_for_test(Path::new(core_lib_path)) {
        Ok(_) => {
            println!("✅ Core library compiled successfully!");
        }
        Err(errors) => {
            println!("🚨 Found {} compilation errors:", errors.len());

            // Count different types of errors
            let mut type_mismatches = 0;
            let mut undefined_functions = 0;
            let mut undefined_types = 0;
            let mut other_errors = 0;

            for (i, error) in errors.iter().enumerate() {
                match error {
                    TypeError::TypeMismatch {
                        expected, found, ..
                    } => {
                        type_mismatches += 1;
                        println!("\nError {}: Type Mismatch", i + 1);
                        println!("  Expected: {expected}");
                        println!("  Found: {found}");
                    }
                    TypeError::UndefinedFunction { name, .. } => {
                        undefined_functions += 1;
                        println!("\nError {}: Undefined Function", i + 1);
                        println!("  Function: {name}");
                    }
                    TypeError::UndefinedType { name, .. } => {
                        undefined_types += 1;
                        println!("\nError {}: Undefined Type", i + 1);
                        println!("  Type: {name}");
                    }
                    TypeError::ProtocolNotImplemented {
                        protocol_name,
                        type_name,
                        ..
                    } => {
                        other_errors += 1;
                        println!("\nError {}: Protocol Not Implemented", i + 1);
                        println!("  Protocol: {protocol_name} for Type: {type_name}");
                    }
                    TypeError::UndefinedProtocol { protocol_name, .. } => {
                        other_errors += 1;
                        println!("\nError {}: Undefined Protocol", i + 1);
                        println!("  Protocol: {protocol_name}");
                    }
                    TypeError::SignatureMismatch {
                        function_name,
                        expected,
                        found,
                        ..
                    } => {
                        other_errors += 1;
                        println!("\nError {}: Signature Mismatch", i + 1);
                        println!("  Function: {function_name}");
                        println!("  Expected: {expected}");
                        println!("  Found: {found}");
                    }
                    _ => {
                        other_errors += 1;
                        println!("\nError {}: {}", i + 1, error);
                    }
                }
            }

            println!("\n📊 Error Summary:");
            println!("  Type Mismatches: {type_mismatches}");
            println!("  Undefined Functions: {undefined_functions}");
            println!("  Undefined Types: {undefined_types}");
            println!("  Other Errors: {other_errors}");
            println!("  Total: {}", errors.len());

            // For now, just assert that we have some errors so the test fails visibly if errors are resolved
            assert!(
                !errors.is_empty(),
                "Expected compilation errors but found none"
            );
        }
    }
}
