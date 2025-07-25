//! Test exact core library scenario replication

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_exact_core_library_boolean_option_scenario() {
    let mut engine = TypeInferenceEngine::new();

    // Replicate the exact scenario from core library loading
    // This is string.outrun - the failing file
    let string_code = r#"
protocol String {
    def contains?(value: Self, search: Self): Boolean {
        Option.some?(value: index_of(value: value, search: search))
    }
    def index_of(value: Self, search: Self): Option<Integer>
}
"#;

    // This is boolean.outrun
    let boolean_code = r#"
protocol Boolean {
    def to_string(value: Self): String
}
"#;

    // This is option.outrun
    let option_code = r#"
protocol Option<T> {
    def some?(value: Self): Boolean
}
"#;

    let string_program = parse_program(string_code).expect("Parse String should succeed");
    let boolean_program = parse_program(boolean_code).expect("Parse Boolean should succeed");
    let option_program = parse_program(option_code).expect("Parse Option should succeed");

    // Simulate core library loading process exactly
    println!("=== PHASE 2: Register all protocols and structs ===");

    // Process in alphabetical order like core library (boolean, option, string)
    println!("Processing boolean.outrun...");
    engine
        .register_protocols_and_structs(&boolean_program)
        .expect("Boolean registration should succeed");

    println!("Processing option.outrun...");
    engine
        .register_protocols_and_structs(&option_program)
        .expect("Option registration should succeed");

    println!("Processing string.outrun...");
    engine
        .register_protocols_and_structs(&string_program)
        .expect("String registration should succeed");

    // Verify protocols are registered
    let boolean_protocol_id = crate::types::ProtocolId::new("Boolean");
    let option_protocol_id = crate::types::ProtocolId::new("Option");
    let string_protocol_id = crate::types::ProtocolId::new("String");

    println!(
        "Boolean protocol registered: {}",
        engine
            .get_protocol_registry()
            .has_protocol(&boolean_protocol_id)
    );
    println!(
        "Option protocol registered: {}",
        engine
            .get_protocol_registry()
            .has_protocol(&option_protocol_id)
    );
    println!(
        "String protocol registered: {}",
        engine
            .get_protocol_registry()
            .has_protocol(&string_protocol_id)
    );

    println!("=== PHASE 2.5: Register automatic implementations ===");
    engine
        .register_automatic_implementations(&boolean_program)
        .expect("Boolean auto-impl should succeed");
    engine
        .register_automatic_implementations(&option_program)
        .expect("Option auto-impl should succeed");
    engine
        .register_automatic_implementations(&string_program)
        .expect("String auto-impl should succeed");

    println!("=== PHASE 3: Register explicit implementations ===");
    engine
        .register_implementations(&boolean_program)
        .expect("Boolean impl should succeed");
    engine
        .register_implementations(&option_program)
        .expect("Option impl should succeed");
    engine
        .register_implementations(&string_program)
        .expect("String impl should succeed");

    println!("=== PHASE 4: Register functions (THE CRITICAL PHASE) ===");

    // Process in same order: boolean, option, string
    println!("Processing boolean.outrun functions...");
    let result1 = engine.register_functions(&boolean_program);
    let success1 = match &result1 {
        Ok(()) => {
            println!("✅ Boolean functions registered successfully");
            true
        }
        Err(e) => {
            println!("❌ Boolean functions failed: {:?}", e);
            let error_str = format!("{:?}", e);
            if error_str.contains("Concrete { id: TypeId(\"Boolean\")") {
                println!("🐛 Boolean concrete type bug in boolean.outrun");
            }
            false
        }
    };

    println!("Processing option.outrun functions...");
    let result2 = engine.register_functions(&option_program);
    let success2 = match &result2 {
        Ok(()) => {
            println!("✅ Option functions registered successfully");
            true
        }
        Err(e) => {
            println!("❌ Option functions failed: {:?}", e);
            let error_str = format!("{:?}", e);
            if error_str.contains("Concrete { id: TypeId(\"Boolean\")") {
                println!("🐛 Boolean concrete type bug in option.outrun");
                println!("This means Option.some? return type 'Boolean' was treated as concrete");
            }
            false
        }
    };

    println!("Processing string.outrun functions...");
    let result3 = engine.register_functions(&string_program);
    let success3 = match &result3 {
        Ok(()) => {
            println!("✅ String functions registered successfully");
            true
        }
        Err(e) => {
            println!("❌ String functions failed: {:?}", e);
            let error_str = format!("{:?}", e);
            if error_str.contains("Concrete { id: TypeId(\"Boolean\")") {
                println!("🐛 Boolean concrete type bug in string.outrun");
                println!(
                    "This means String.contains? return type 'Boolean' was treated as concrete"
                );
            }
            false
        }
    };

    // If all succeed, proceed to Phase 6 to replicate the exact error
    if success1 && success2 && success3 {
        println!("=== PHASE 6: Type check function bodies ===");
        let mut string_program_mut = string_program.clone();
        let phase6_result = engine.typecheck_function_bodies(&mut string_program_mut);
        match phase6_result {
            Ok(()) => println!("✅ Type checking succeeded"),
            Err(e) => {
                println!("❌ Type checking failed: {:?}", e);
                let error_str = format!("{:?}", e);
                if error_str.contains("expected: Protocol { id: ProtocolId(\"Boolean\")")
                    && error_str.contains("found: Concrete { id: TypeId(\"Boolean\")")
                {
                    println!("🐛 EXACT CORE LIBRARY BUG REPRODUCED!");
                }
            }
        }
    }
}
