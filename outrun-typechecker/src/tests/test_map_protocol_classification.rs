//! Test to debug Map protocol classification issue

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_map_protocol_registration_step_by_step() {
    // println!("=== Testing Map Protocol Registration Step by Step ===");

    // Create a simple Map protocol definition to test registration
    let map_protocol_source = r#"
        protocol Map<K, V> {
            def get(map: Self, key: K): Option<V>
            def put(map: Self, key: K, value: V): Self
        }
    "#;

    let program = parse_program(map_protocol_source).expect("Failed to parse Map protocol");
    let mut engine = TypeInferenceEngine::bootstrap();

    println!(
        "🔍 Before registration - Map protocol exists: {}",
        engine.type_registry_rc().is_protocol("Map")
    );

    // Phase 2: Register protocols and structs
    match engine.register_protocols_and_structs(&program) {
        Ok(_) => {
            // println!("✅ Protocol registration succeeded");

            // Check if Map is now registered
            let type_registry = engine.type_registry_rc();
            println!(
                "🔍 After registration - Map protocol exists: {}",
                type_registry.is_protocol("Map")
            );

            // Check what type Map is registered as
            if type_registry.is_protocol("Map") {
                // println!("✅ Map correctly identified as protocol");
            } else if type_registry.is_struct("Map") {
                // println!("❌ Map incorrectly identified as struct");
            } else {
                // println!("❌ Map not found in type registry at all");
            }
        }
        Err(e) => {
            println!("❌ Protocol registration failed: {}", e);
        }
    }
}

#[test]
fn test_map_protocol_reference_count_issue() {
    // println!("=== Testing Map Protocol Reference Count Issue ===");

    // Test the specific scenario where Rc::get_mut might fail
    let source = r#"
        protocol Map<K, V> {
            def get(map: Self, key: K): Option<V>
        }
        
        protocol Default {
            def default(): Self
        }
    "#;

    let program = parse_program(source).expect("Failed to parse test program");
    let mut engine = TypeInferenceEngine::bootstrap();

    // Create multiple references to the type registry to force Rc::get_mut to fail
    let _type_registry_ref1 = engine.type_registry_rc();
    let _type_registry_ref2 = engine.type_registry_rc();

    println!(
        "🔍 Type registry ref count: {}",
        std::rc::Rc::strong_count(&engine.type_registry_rc())
    );

    // Phase 2: Register protocols and structs (this should trigger the Rc::get_mut failure)
    match engine.register_protocols_and_structs(&program) {
        Ok(_) => {
            // println!("✅ Phase 2 succeeded");

            // Check if Map is still registered after the Rc replacement
            let type_registry = engine.type_registry_rc();
            println!(
                "🔍 Map protocol exists after registration: {}",
                type_registry.is_protocol("Map")
            );

            // Drop the old references
            drop(_type_registry_ref1);
            drop(_type_registry_ref2);

            // Check again
            let type_registry = engine.type_registry_rc();
            println!(
                "🔍 Map protocol exists after dropping refs: {}",
                type_registry.is_protocol("Map")
            );
        }
        Err(e) => {
            println!("❌ Phase 2 failed: {}", e);
        }
    }
}

#[test]
fn test_map_protocol_with_implementation() {
    // println!("=== Testing Map Protocol with Implementation ===");

    // Simulate the scenario from the core library: Map protocol + Default implementation
    let source = r#"
        protocol Map<K, V> {
            def get(map: Self, key: K): Option<V>
            def put(map: Self, key: K, value: V): Self
        }
        
        protocol Default {
            def default(): Self
        }
        
        struct Outrun.Core.Map<K, V> {
            def dummy(): Boolean { true }
        }
        
        impl Default for Outrun.Core.Map<K, V> {
            def default(): Self {
                # This should return Map<K, V> but might be incorrectly typed as concrete
                {}
            }
        }
    "#;

    let mut program = parse_program(source).expect("Failed to parse test program");
    let mut engine = TypeInferenceEngine::bootstrap();

    // Phase 2: Register protocols and structs
    // println!("=== Phase 2: Register protocols and structs ===");
    match engine.register_protocols_and_structs(&program) {
        Ok(_) => {
            // println!("✅ Phase 2 succeeded");

            // Check if Map is registered as protocol
            let type_registry = engine.type_registry_rc();
            println!(
                "🔍 Map protocol exists: {}",
                type_registry.is_protocol("Map")
            );
            println!(
                "🔍 Default protocol exists: {}",
                type_registry.is_protocol("Default")
            );
        }
        Err(e) => {
            println!("❌ Phase 2 failed: {}", e);
            return;
        }
    }

    // Phase 3: Register implementations
    // println!("=== Phase 3: Register implementations ===");
    match engine.register_implementations(&program) {
        Ok(_) => {
            // println!("✅ Phase 3 succeeded");
        }
        Err(e) => {
            println!("❌ Phase 3 failed: {}", e);
            return;
        }
    }

    // Phase 4: Register functions (this is where convert_type_annotation is called)
    // println!("=== Phase 4: Register functions ===");
    match engine.register_functions(&program) {
        Ok(_) => {
            // println!("✅ Phase 4 succeeded");
        }
        Err(e) => {
            println!("❌ Phase 4 failed: {}", e);
            return;
        }
    }

    // Phase 6: Type check function bodies (this is where the error occurs)
    // println!("=== Phase 6: Type check function bodies ===");
    match engine.typecheck_function_bodies(&mut program) {
        Ok(_) => {
            // println!("✅ Phase 6 succeeded");
        }
        Err(e) => {
            println!("❌ Phase 6 failed: {}", e);
        }
    }
}
