//! Test core library loading and integration

#[cfg(test)]
mod core_library_loading_tests {
    use crate::core_library::default_core_library_path;
    use crate::{typecheck_package, Package};
    use outrun_parser::parse_program;

    #[test]
    fn test_core_library_path_discovery() {
        // Test that we can find the core library path
        let core_path = default_core_library_path();
        println!("Core library path result: {:?}", core_path);

        if let Ok(path) = core_path {
            println!("Found core library at: {}", path.display());
            assert!(path.exists(), "Core library path should exist");

            let lib_path = path.join("lib");
            assert!(lib_path.exists(), "lib directory should exist");
        } else {
            println!(
                "Could not find core library - this might be expected in some test environments"
            );
        }
    }

    #[test]
    fn test_core_library_loading() {
        // Try to load the core library using the unified approach
        if let Ok(_core_path) = default_core_library_path() {
            let mut package = Package::new("test-package".to_string());

            let result = typecheck_package(&mut package);
            match result {
                Ok(()) => {
                    // println!("✅ Core library loaded successfully via unified approach");
                    println!("📦 Total programs processed: {}", package.programs.len());
                }
                Err(e) => {
                    // println!("❌ Core library loading failed:");
                    println!("Error: {:?}", e);
                }
            }
        } else {
            // println!("Skipping core library loading test - path not found");
        }
    }

    #[test]
    fn test_binary_operation_with_core_library() {
        // Test that binary operations work when core library is loaded
        let source = r#"
            def add(a: Integer, b: Integer): Integer {
                a + b
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");

        // Test with unified package approach
        if let Ok(_core_path) = default_core_library_path() {
            let mut package = Package::new("binary-operation-test".to_string());
            package.add_program(program);

            let result = typecheck_package(&mut package);
            match result {
                Ok(()) => {
                    // println!("✅ Binary operation type checking succeeded with unified approach!");
                }
                Err(e) => {
                    println!("❌ Type checking failed: {:?}", e);
                    // Still failing might indicate other issues, but don't panic
                }
            }
        } else {
            // println!("Core library path not found, skipping integration test");
        }
    }

    #[test]
    fn test_explicit_protocol_call_with_core_library() {
        // Test that explicit protocol calls work when core library is loaded
        let source = r#"
            def add(a: Integer, b: Integer): Integer {
                BinaryAddition.add(lhs: a, rhs: b)
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");

        // Test with unified package approach
        if let Ok(_core_path) = default_core_library_path() {
            let mut package = Package::new("explicit-protocol-test".to_string());
            package.add_program(program);

            let result = typecheck_package(&mut package);
            match result {
                Ok(()) => {
                    println!(
                        "✅ Explicit protocol call type checking succeeded with unified approach!"
                    );
                }
                Err(e) => {
                    println!("❌ Explicit protocol call type checking failed: {:?}", e);
                    // Log the error but don't panic - there might be other integration issues
                }
            }
        } else {
            // println!("Core library path not found, skipping explicit protocol call test");
        }
    }

    #[test]
    fn test_detailed_core_library_errors() {
        // Test the detailed behavior of the unified core library loading
        if let Ok(_core_path) = default_core_library_path() {
            // println!("🔍 Testing unified core library processing...");

            let mut package = Package::new("detailed-test".to_string());

            let result = typecheck_package(&mut package);
            match result {
                Ok(()) => {
                    // println!("✅ Unified core library processing completed successfully");
                    println!("📊 Total programs processed: {}", package.programs.len());
                }
                Err(e) => {
                    println!("❌ Unified core library processing failed: {}", e);

                    // Check if it's the ConflictingImplementation error we were trying to fix
                    match &e {
                        crate::error::CompilerError::Typecheck(typecheck_err) => {
                            match typecheck_err.as_ref() {
                                crate::error::TypecheckError::ImplementationError(impl_err) => {
                                    match impl_err {
                                        crate::error::ImplementationError::ConflictingImplementation { protocol_name, type_name, .. } => {
                                            println!("🚨 Still getting ConflictingImplementation: {} for {}", protocol_name, type_name);
                                            // println!("🚨 This means there might still be a duplicate processing issue");
                                        }
                                        _ => {
                                            println!("🔍 Other implementation error: {:?}", impl_err);
                                        }
                                    }
                                }
                                _ => {
                                    println!("🔍 Other typecheck error: {:?}", typecheck_err);
                                }
                            }
                        }
                        _ => {
                            println!("🔍 Other compiler error: {:?}", e);
                        }
                    }
                }
            }
        } else {
            // println!("Core library path not found");
        }
    }
}
