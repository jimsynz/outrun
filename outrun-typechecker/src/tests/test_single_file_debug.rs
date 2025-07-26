//! Test single core library file to debug phase processing

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;
use std::fs;

#[test]
fn test_single_option_file_debug() {
    let mut engine = TypeInferenceEngine::new();

    // First manually register the Boolean protocol like in the real core library
    let boolean_code = fs::read_to_string("../outrun-core/lib/boolean.outrun")
        .expect("Should read boolean.outrun");
    let boolean_program = parse_program(&boolean_code).expect("Should parse boolean.outrun");

    println!("=== Phase 2: Registering Boolean protocol ===");
    engine
        .register_protocols_and_structs(&boolean_program)
        .expect("Boolean registration should succeed");

    // Verify Boolean protocol is registered
    let boolean_protocol_id = crate::types::ProtocolId::new("Boolean");
    let has_boolean = engine
        .get_protocol_registry()
        .has_protocol(&boolean_protocol_id);
    println!("Boolean protocol registered: {}", has_boolean);

    // Now try to process option.outrun
    let option_code =
        fs::read_to_string("../outrun-core/lib/option.outrun").expect("Should read option.outrun");
    let option_program = parse_program(&option_code).expect("Should parse option.outrun");

    println!("=== Phase 2: Registering Option protocol ===");
    engine
        .register_protocols_and_structs(&option_program)
        .expect("Option registration should succeed");

    println!("=== Phase 4: Registering Option functions ===");
    let result = engine.register_functions(&option_program);

    match result {
        Ok(()) => {
            println!("✅ Option function registration succeeded");

            // Check what type was registered for Option.some?
            let dispatcher = crate::dispatch::FunctionDispatcher::new(
                &engine.get_protocol_registry(),
                &engine.function_registry(),
                None,
                None,
            );
            match dispatcher.resolve_qualified_call("Option.some?", None, None) {
                Ok(dispatch_result) => match dispatch_result {
                    crate::dispatch::DispatchResult::Resolved(resolved) => {
                        println!(
                            "Option.some? return type: {:?}",
                            resolved.function_info.return_type
                        );
                        match &resolved.function_info.return_type {
                            crate::types::Type::Protocol { id, .. } => {
                                println!("✅ Correctly registered as Protocol: {}", id.0);
                            }
                            crate::types::Type::Concrete { id, .. } => {
                                println!("❌ INCORRECTLY registered as Concrete: {}", id.name());
                                if id.name() == "Boolean" {
                                    println!("🐛 EXACT BUG REPRODUCED: Boolean treated as concrete during function registration");
                                }
                            }
                            other => {
                                println!("Unexpected type: {:?}", other);
                            }
                        }
                    }
                    other => {
                        println!("Dispatch result not resolved: {:?}", other);
                    }
                },
                Err(e) => {
                    println!("Could not resolve Option.some?: {:?}", e);
                }
            }
        }
        Err(e) => {
            println!("❌ Option function registration failed: {:?}", e);
        }
    }
}
