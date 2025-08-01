//! Test protocol registration order bug

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_protocol_registration_order_bug() {
    let mut engine = TypeInferenceEngine::new();

    // Simulate the exact core library loading scenario
    // First parse Boolean protocol
    let boolean_protocol_code = r#"
protocol Boolean {
    def to_string(value: Self): String
}
"#;

    // Then parse Option protocol that uses Boolean
    let option_protocol_code = r#"
protocol Option<T> {
    def some?(value: Self): Boolean
}
"#;

    let boolean_program =
        parse_program(boolean_protocol_code).expect("Parse Boolean should succeed");
    let option_program = parse_program(option_protocol_code).expect("Parse Option should succeed");

    // Phase 2: Register protocols and structs - this should register Boolean protocol
    // println!("Phase 2: Registering Boolean protocol...");
    engine
        .register_protocols_and_structs(&boolean_program)
        .expect("Boolean protocol registration should succeed");

    // Check if Boolean is now recognized as a protocol
    let boolean_protocol_id = crate::types::ModuleName::new("Boolean");
    let has_boolean_protocol = engine
        .get_protocol_registry()
        .has_protocol(&boolean_protocol_id);
    println!("Boolean protocol registered: {}", has_boolean_protocol);

    // Phase 2: Register Option protocol
    // println!("Phase 2: Registering Option protocol...");
    engine
        .register_protocols_and_structs(&option_program)
        .expect("Option protocol registration should succeed");

    // Phase 4: Register functions - this is where the bug happens
    // println!("Phase 4: Registering Option functions...");
    let result = engine.register_functions(&option_program);

    match result {
        Ok(()) => {
            // println!("✅ Phase 4 succeeded - Boolean was correctly recognized as protocol");
        }
        Err(e) => {
            println!("❌ Phase 4 failed: {:?}", e);
            let error_str = format!("{:?}", e);
            if error_str.contains("Concrete { id: TypeId(\"Boolean\")") {
                // println!("🐛 CONFIRMED: Boolean was treated as concrete type instead of protocol");
                println!("This means is_protocol_type(\"Boolean\") returned false during convert_type_annotation");
            }
        }
    }

    // Let's also directly test is_protocol_type
    let option_signature_code = r#"
def test_function(): Boolean {
    true
}
"#;

    let test_program = parse_program(option_signature_code).expect("Parse test should succeed");

    // println!("Testing convert_type_annotation directly...");
    let result2 = engine.register_functions(&test_program);
    match result2 {
        Ok(()) => {
            // println!("✅ Direct test succeeded");
        }
        Err(e) => {
            println!("❌ Direct test failed: {:?}", e);
        }
    }
}
