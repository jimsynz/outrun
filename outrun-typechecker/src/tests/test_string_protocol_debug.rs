//! Debug test for String protocol recognition issue
//!
//! This test focuses on the specific problem where the protocol registry
//! isn't recognizing that Outrun.Core.String implements the String protocol.

use crate::inference::TypeInferenceEngine;
use crate::types::{ProtocolId, Type, TypeId};
use outrun_parser::parse_program;

#[test]
fn test_string_protocol_registry_step_by_step() {
    let mut engine = TypeInferenceEngine::new();

    // Step 1: Parse and load the String protocol definition
    let string_protocol_code = r#"
protocol String {
    def length(value: Self): Integer
    def to_string(value: Self): Outrun.Core.String
}
"#;

    let protocol_program =
        parse_program(string_protocol_code).expect("Failed to parse String protocol");

    // Collect protocol definitions
    engine
        .register_protocols_and_structs(&protocol_program)
        .expect("Phase 2 should succeed");
    engine
        .register_implementations(&protocol_program)
        .expect("Phase 3 should succeed");
    engine
        .register_functions(&protocol_program)
        .expect("Phase 4 should succeed");

    // Step 2: Parse and load the Outrun.Core.String implementation
    let string_impl_code = r#"
struct Outrun.Core.String() {}

impl String for Outrun.Core.String {
    def length(value: Self): Integer {
        42  # Mock implementation
    }
    
    def to_string(value: Self): Outrun.Core.String {
        value
    }
}
"#;

    let impl_program =
        parse_program(string_impl_code).expect("Failed to parse String implementation");

    // Collect struct definitions first
    engine
        .register_protocols_and_structs(&impl_program)
        .expect("Phase 2 should succeed");

    // Register automatic implementations
    engine
        .register_automatic_implementations(&impl_program)
        .expect("Phase 2.5 should succeed");

    // Register explicit implementations
    engine
        .register_implementations(&impl_program)
        .expect("Failed to register String implementation");

    // Step 3: Check protocol registry state
    let string_type = TypeId::new("Outrun.Core.String");
    let string_protocol = ProtocolId::new("String");

    println!("=== Protocol Registry Debug ===");
    println!("Checking if Outrun.Core.String implements String protocol...");

    // Check direct implementation lookup
    let has_direct_impl = {
        let protocol_registry = engine.get_protocol_registry();
        protocol_registry.has_implementation(&string_protocol, &string_type)
    };
    println!("Direct implementation lookup: {}", has_direct_impl);

    // Check type satisfaction (includes requirements)
    let satisfies_protocol = {
        let protocol_registry = engine.get_protocol_registry();
        protocol_registry.type_satisfies_protocol(&string_type, &string_protocol)
    };
    println!("Type satisfies protocol: {}", satisfies_protocol);

    // Check implementation count
    let implementation_count = {
        let protocol_registry = engine.get_protocol_registry();
        protocol_registry.implementation_count()
    };
    println!(
        "Total implementations in registry: {}",
        implementation_count
    );

    // List all implementations for String protocol
    let string_impls_len = {
        let protocol_registry = engine.get_protocol_registry();
        let string_impls = protocol_registry.get_protocol_implementations(&string_protocol);
        println!("Implementations of String protocol: {}", string_impls.len());
        for impl_info in &string_impls {
            println!(
                "  - {} implements String",
                impl_info.implementing_type.name()
            );
        }
        string_impls.len()
    };

    // List all implementations for Outrun.Core.String type
    let type_impls_len = {
        let protocol_registry = engine.get_protocol_registry();
        let type_impls = protocol_registry.get_type_implementations(&string_type);
        println!(
            "Protocols implemented by Outrun.Core.String: {}",
            type_impls.len()
        );
        for impl_info in &type_impls {
            println!(
                "  - Outrun.Core.String implements {}",
                impl_info.protocol_id.0
            );
        }
        type_impls.len()
    };

    // Step 4: Test type compatibility
    let concrete_string_type = Type::concrete("Outrun.Core.String");
    let protocol_string_type = Type::Protocol {
        id: ProtocolId::new("String"),
        args: vec![],
        span: None,
    };

    println!("=== Type Compatibility Test ===");
    println!("Testing compatibility: Outrun.Core.String -> String");

    // This should work if protocol registry is functioning correctly
    let compatible = engine.test_types_are_compatible(&concrete_string_type, &protocol_string_type);
    println!("Types are compatible: {}", compatible);

    // Step 5: Test with a simple function that should work
    let test_function_code = r#"
def simple_string_function(): String {
    let core_string: Outrun.Core.String = ""
    core_string
}
"#;

    println!("=== Function Type Checking Test ===");
    let mut test_program =
        parse_program(test_function_code).expect("Failed to parse test function");

    let typecheck_result = engine.typecheck_program(&mut test_program);
    match typecheck_result {
        Ok(()) => {
            println!("✅ Function type checking succeeded - protocol registry is working!");
        }
        Err(e) => {
            println!("❌ Function type checking failed: {:?}", e);
            println!("This indicates the protocol registry issue is confirmed.");
        }
    }

    // Assertions
    assert!(
        has_direct_impl,
        "Outrun.Core.String should directly implement String protocol"
    );
    assert!(
        satisfies_protocol,
        "Outrun.Core.String should satisfy String protocol"
    );
    assert!(
        string_impls_len > 0,
        "String protocol should have implementations"
    );
    assert!(
        type_impls_len > 0,
        "Outrun.Core.String should implement protocols"
    );
    assert!(
        compatible,
        "Outrun.Core.String should be compatible with String protocol"
    );
}

// Note: test_inspect_function_with_minimal_setup was removed because it duplicated
// protocol-concrete compatibility testing that is now covered by test_option_integer_function_calls
// The core issue (String protocol vs Outrun.Core.String concrete compatibility) is tested elsewhere
