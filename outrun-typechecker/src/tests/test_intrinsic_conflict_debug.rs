//! Test for intrinsic vs explicit implementation conflicts

use crate::core_library::{collect_outrun_files, default_core_library_path};
use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_intrinsic_explicit_implementation_conflict() {
    println!("🔍 Testing for intrinsic vs explicit implementation conflicts");

    let mut engine = TypeInferenceEngine::new();

    // First, let's see what implementations are already registered in a fresh engine
    let registry = engine.get_protocol_registry();
    let binary_addition = crate::types::ProtocolId::new("BinaryAddition");
    let integer64_type = crate::types::TypeId::new("Outrun.Core.Integer64");

    println!("🔬 Fresh engine state:");
    println!(
        "  BinaryAddition for Outrun.Core.Integer64 registered: {}",
        registry.has_implementation(&binary_addition, &integer64_type)
    );

    // Now let's manually process just the problematic file that's failing
    let core_path = default_core_library_path().expect("Should find core library");
    let lib_path = core_path.join("lib");

    let mut programs = Vec::new();
    collect_outrun_files(&lib_path, &mut programs).expect("Should collect files");

    // Find the specific failing file
    let integer64_file = programs
        .iter()
        .find(|(path, _)| path.ends_with("outrun/core/integer64.outrun"))
        .expect("Should find integer64.outrun");

    let mut integer64_program =
        parse_program(&integer64_file.1).expect("Should parse integer64.outrun");

    println!("📄 Processing integer64.outrun in isolation...");

    // Process through each phase carefully, checking state after each
    println!("📋 Phase 2: Register protocols and structs");
    engine
        .register_protocols_and_structs(&integer64_program)
        .expect("Phase 2 should succeed");

    let registry_after_phase2 = engine.get_protocol_registry();
    println!(
        "  After Phase 2 - BinaryAddition for Outrun.Core.Integer64: {}",
        registry_after_phase2.has_implementation(&binary_addition, &integer64_type)
    );

    println!("📋 Phase 2.5: Register automatic implementations");
    engine
        .register_automatic_implementations(&integer64_program)
        .expect("Phase 2.5 should succeed");

    let registry_after_phase25 = engine.get_protocol_registry();
    println!(
        "  After Phase 2.5 - BinaryAddition for Outrun.Core.Integer64: {}",
        registry_after_phase25.has_implementation(&binary_addition, &integer64_type)
    );

    println!("📋 Phase 3: Register explicit implementations");
    match engine.register_implementations(&integer64_program) {
        Ok(()) => {
            println!("  ✅ Phase 3 succeeded");
            let registry_after_phase3 = engine.get_protocol_registry();
            println!(
                "  After Phase 3 - BinaryAddition for Outrun.Core.Integer64: {}",
                registry_after_phase3.has_implementation(&binary_addition, &integer64_type)
            );
        }
        Err(e) => {
            println!("  ❌ Phase 3 failed: {}", e);
            match &e {
                crate::error::TypecheckError::ImplementationError(
                    crate::error::ImplementationError::ConflictingImplementation {
                        protocol_name,
                        type_name,
                        ..
                    }
                ) => {
                    println!(
                        "  🚨 CONFLICT: {} for {} during Phase 3!",
                        protocol_name, type_name
                    );

                    // This proves the conflict happens during Phase 3, not Phase 6
                    println!("  🔍 This means something registered BinaryAddition for Outrun.Core.Integer64 BEFORE Phase 3");
                    println!("  💡 Likely candidates: intrinsic registration, or automatic implementations");
                }
                _ => {}
            }
            return; // Stop here if Phase 3 fails
        }
    }

    println!("📋 Phase 4: Register functions");
    engine
        .register_functions(&integer64_program)
        .expect("Phase 4 should succeed");

    println!("📋 Phase 6: Type check function bodies");
    match engine.typecheck_function_bodies(&mut integer64_program) {
        Ok(()) => {
            println!("  ✅ Phase 6 succeeded");
        }
        Err(e) => {
            println!("  ❌ Phase 6 failed: {}", e);
            match &e {
                crate::error::TypecheckError::ImplementationError(
                    crate::error::ImplementationError::ConflictingImplementation {
                        protocol_name,
                        type_name,
                        ..
                    }
                ) => {
                    println!(
                        "  🚨 CONFLICT: {} for {} during Phase 6!",
                        protocol_name, type_name
                    );
                    println!(
                        "  🔍 This suggests Phase 6 is trying to register implementations"
                    );
                }
                _ => {}
            }
        }
    }
}

#[test]
fn test_intrinsic_registry_investigation() {
    println!("🔍 Investigating intrinsic registry for pre-registered implementations");

    let engine = TypeInferenceEngine::new();

    // Check what intrinsics might be pre-registered
    let registry = engine.get_protocol_registry();

    // List of protocols that are failing in the core library
    let problematic_protocols = ["BinaryAddition", "Display", "Equality", "Option"];

    // List of types that are failing
    let problematic_types = [
        "Outrun.Core.Integer64",
        "Outrun.Core.Float64",
        "Outrun.Core.String",
        "Outrun.Core.Boolean",
        "Outrun.Core.List",
        "Outrun.Core.Map",
        "Outrun.Core.Atom",
        "Outrun.Option.None",
        "Outrun.Option.Some",
    ];

    println!("🔬 Checking fresh engine for pre-registered implementations:");
    for protocol_name in &problematic_protocols {
        let protocol_id = crate::types::ProtocolId::new(*protocol_name);
        for type_name in &problematic_types {
            let type_id = crate::types::TypeId::new(*type_name);
            if registry.has_implementation(&protocol_id, &type_id) {
                println!("  🚨 PRE-REGISTERED: {} for {}", protocol_name, type_name);
            }
        }
    }

    println!("✅ Fresh engine investigation complete");
}
