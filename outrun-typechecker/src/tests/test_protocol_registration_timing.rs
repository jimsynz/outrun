//! Test protocol registration timing issues

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_integer_protocol_registration_timing() {
    let mut engine = TypeInferenceEngine::new();

    println!("=== Testing Integer protocol registration timing ===");

    // Test the case that's failing: string.outrun defines a function with Integer return type
    // but the Integer protocol might not be registered yet
    let string_function = r#"
impl String for Outrun.Core.String {
    def length(value: Self): Integer {
        Outrun.Intrinsic.string_length(value: value)
    }
}

struct Outrun.Core.String() {}
"#;

    println!("=== Testing without Integer protocol registered ===");
    let string_program = parse_program(string_function).expect("Parse should succeed");

    // Process Phase 2 without registering Integer protocol first
    match engine.register_protocols_and_structs(&string_program) {
        Ok(()) => println!("✅ Phase 2 succeeded"),
        Err(e) => {
            println!("❌ Phase 2 failed: {:?}", e);
        }
    }

    // Now try Phase 4 (register functions) - this is where convert_type_annotation is called
    println!("=== Phase 4: Register functions (calls convert_type_annotation) ===");
    match engine.register_functions(&string_program) {
        Ok(()) => println!("✅ Phase 4 succeeded"),
        Err(e) => {
            println!("❌ Phase 4 failed: {:?}", e);
        }
    }

    println!("=== Now testing with Integer protocol registered first ===");
    let mut engine2 = TypeInferenceEngine::new();

    // First register the Integer protocol
    let integer_protocol = r#"
protocol Integer {
    def add(lhs: Self, rhs: Self): Self
}
"#;

    let protocol_program =
        parse_program(integer_protocol).expect("Parse protocol should succeed");
    engine2
        .register_protocols_and_structs(&protocol_program)
        .expect("Should register Integer protocol");

    // Now process the string function again
    let mut string_program2 = parse_program(string_function).expect("Parse should succeed");

    match engine2.register_protocols_and_structs(&string_program2) {
        Ok(()) => println!("✅ Phase 2 succeeded with Integer protocol registered"),
        Err(e) => {
            println!("❌ Phase 2 failed: {:?}", e);
        }
    }

    match engine2.register_functions(&string_program2) {
        Ok(()) => println!("✅ Phase 4 succeeded with Integer protocol registered"),
        Err(e) => {
            println!("❌ Phase 4 failed: {:?}", e);
        }
    }

    // Test Phase 6 - type checking function bodies (this is where the error actually occurs)
    println!("=== Phase 6: Type check function bodies ===");
    match engine2.typecheck_function_bodies(&mut string_program2) {
        Ok(()) => println!("✅ Phase 6 succeeded"),
        Err(e) => {
            println!("❌ Phase 6 failed: {:?}", e);
            println!("Error details: {}", e);
        }
    }

    println!(
        "=== This demonstrates that the issue occurs during type checking, not registration ==="
    );
}
