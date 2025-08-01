//! Test Phase 3 implementation registration functionality

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_phase3_implementation_registration() {
    let mut engine = TypeInferenceEngine::new();

    // Test code that should work with orphan rules - both protocol and type defined in same module
    let test_code = r#"
protocol TestProtocol {
    def test_function(value: Self): Integer
}

struct TestType() {}

impl TestProtocol for TestType {
    def test_function(value: Self): Integer {
        42
    }
}
"#;

    let program = parse_program(test_code).expect("Failed to parse test code");

    // Phase 2: Register protocols and structs (this should now automatically mark modules as local)
    engine
        .register_protocols_and_structs(&program)
        .expect("Phase 2 should succeed");

    // Phase 2.5: Register automatic implementations
    engine
        .register_automatic_implementations(&program)
        .expect("Phase 2.5 should succeed");

    // Phase 3: Register implementations - this should call our new register_impl_block function
    let result = engine.register_implementations(&program);

    match result {
        Ok(()) => {
            // println!("✅ Phase 3 implementation registration succeeded!");

            // Verify the implementation was registered
            let registry = engine.get_protocol_registry();
            let type_id = crate::types::ModuleName::new("TestType");
            let protocol_id = crate::types::ModuleName::new("TestProtocol");

            let has_impl = registry.has_implementation(&protocol_id, &type_id);
            println!("Implementation registered: {}", has_impl);
            assert!(has_impl, "TestType should implement TestProtocol");
        }
        Err(e) => {
            println!("❌ Phase 3 implementation registration failed: {:?}", e);
            panic!("Phase 3 should work for valid implementations");
        }
    }
}
