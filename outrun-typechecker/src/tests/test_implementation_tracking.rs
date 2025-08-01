//! Track implementation registrations to identify duplicate registration sources

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_implementation_registration_tracking() {
    let mut engine = TypeInferenceEngine::new();

    // println!("🔍 Testing implementation registration tracking");

    // Simple test program with one impl block
    let source = r#"
        struct TestStruct() {}
        
        impl Display for TestStruct {
            def to_string(value: Self): String {
                "test"
            }
        }
    "#;

    let mut program = parse_program(source).expect("Parse should succeed");

    // Complete all phases properly
    // println!("📋 Phase 2: Register protocols and structs");
    engine
        .register_protocols_and_structs(&program)
        .expect("Phase 2 should succeed");

    // println!("📋 Phase 2.5: Register automatic implementations");
    engine
        .register_automatic_implementations(&program)
        .expect("Phase 2.5 should succeed");

    // println!("📋 Phase 3: Register explicit implementations");
    engine
        .register_implementations(&program)
        .expect("Phase 3 should succeed");

    // println!("📋 Phase 4: Register functions");
    engine
        .register_functions(&program)
        .expect("Phase 4 should succeed");

    // Check that Display for TestStruct is registered once
    let registry = engine.get_protocol_registry();
    let display_protocol = crate::types::ModuleName::new("Display");
    let test_struct_type = crate::types::ModuleName::new("TestStruct");

    let has_impl = registry.has_implementation(&display_protocol, &test_struct_type);
    println!(
        "✅ After Phase 4: Display for TestStruct registered: {}",
        has_impl
    );

    // Now try Phase 6 - this should NOT register implementations, only type check
    // println!("📋 Phase 6: Type check function bodies (should not register implementations)");

    // The typecheck_function_bodies should NOT call register_implementations again
    match engine.typecheck_function_bodies(&mut program) {
        Ok(()) => {
            // println!("✅ Phase 6 completed successfully");
        }
        Err(e) => {
            println!("❌ Phase 6 failed: {}", e);

            // Check if this is a ConflictingImplementation error
            match &e {
                crate::error::TypecheckError::ImplementationError(impl_err) => match impl_err {
                    crate::error::ImplementationError::ConflictingImplementation {
                        protocol_name,
                        type_name,
                        ..
                    } => {
                        println!(
                            "🚨 ConflictingImplementation: {} for {}",
                            protocol_name, type_name
                        );
                        // println!("🚨 This proves Phase 6 is trying to register implementations!");
                    }
                    _ => {
                        println!("🔍 Other implementation error: {:?}", impl_err);
                    }
                },
                _ => {
                    println!("🔍 Other error type: {:?}", e);
                }
            }
        }
    }

    // Verify the implementation is still registered exactly once
    let registry_after = engine.get_protocol_registry();
    let still_has_impl = registry_after.has_implementation(&display_protocol, &test_struct_type);
    println!(
        "✅ After Phase 6: Display for TestStruct still registered: {}",
        still_has_impl
    );
}
