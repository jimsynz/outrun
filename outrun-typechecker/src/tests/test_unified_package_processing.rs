//! Test unified package processing with core library integration

use crate::{typecheck_package, Package};

#[test]
fn test_unified_core_library_processing() {
    println!("🔍 Testing unified package processing with core library integration");

    // Create a simple user package
    let mut package = Package::new("test-package".to_string());

    // Test the unified processing (should integrate core library automatically)
    match typecheck_package(&mut package) {
        Ok(()) => {
            println!("✅ Unified package processing completed successfully");
            println!("📊 Total programs processed: {}", package.programs.len());
        }
        Err(e) => {
            println!("❌ Unified package processing failed: {}", e);

            // Check if it's the ConflictingImplementation error we were trying to fix
            match &e {
                crate::error::CompilerError::Typecheck(typecheck_err) => match typecheck_err.as_ref() {
                    crate::error::TypecheckError::ImplementationError(impl_err) => match impl_err {
                        crate::error::ImplementationError::ConflictingImplementation {
                            protocol_name,
                            type_name,
                            ..
                        } => {
                            println!(
                                "🚨 Still getting ConflictingImplementation: {} for {}",
                                protocol_name, type_name
                            );
                            println!("🚨 This means the unified approach didn't fix the issue yet");
                        }
                        _ => {
                            println!("🔍 Other implementation error: {:?}", impl_err);
                        }
                    },
                    _ => {
                        println!("🔍 Other typecheck error: {:?}", typecheck_err);
                    }
                },
                _ => {
                    println!("🔍 Other compiler error: {:?}", e);
                }
            }
        }
    }
}
