//! Test function call resolution bug with Boolean type

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_function_call_boolean_return_type_bug() {
    let mut engine = TypeInferenceEngine::new();
    
    // Exact scenario from string.outrun failure
    let test_code = r#"
protocol Boolean {
    def true?(value: Self): Outrun.Core.Boolean
}

protocol Option<T> {
    def some?(value: Self): Boolean
}

protocol String {
    def contains?(value: Self, search: Self): Boolean {
        Option.some?(value: some_option_value)
    }
}

struct Outrun.Core.Boolean() {}

impl Boolean for Outrun.Core.Boolean {
    def true?(value: Self): Outrun.Core.Boolean {
        value
    }
}
"#;
    
    let mut program = parse_program(test_code).expect("Parse should succeed");
    
    // Process through all phases like core library
    engine.register_protocols_and_structs(&program).expect("Phase 2 should succeed");
    engine.register_automatic_implementations(&program).expect("Phase 2.5 should succeed");
    engine.register_implementations(&program).expect("Phase 3 should succeed");
    engine.register_functions(&program).expect("Phase 4 should succeed");
    
    // This is where the original error occurs - Phase 6 type checking
    let result = engine.typecheck_function_bodies(&mut program);
    
    match result {
        Ok(()) => {
            println!("✅ Type checking succeeded - Boolean bug is fixed!");
        }
        Err(e) => {
            println!("❌ Type checking failed: {:?}", e);
            let error_str = format!("{:?}", e);
            if error_str.contains("expected: Protocol { id: ProtocolId(\"Boolean\")") && 
               error_str.contains("found: Concrete { id: TypeId(\"Boolean\")") {
                println!("🐛 EXACT BUG REPRODUCED: Function call resolution treats Boolean as concrete type");
                println!("The issue is in how Option.some?() return type is resolved during function call inference");
            } else if error_str.contains("found: Concrete { id: TypeId(\"Outrun.Core.Boolean\")") {
                println!("✅ Correct concrete type inferred, but compatibility check failed");
                println!("This means the issue is in protocol-to-concrete compatibility, not type name resolution");
            }
        }
    }
}

#[test]  
fn test_simplified_function_call_scenario() {
    let mut engine = TypeInferenceEngine::new();
    
    // Even simpler test to isolate the exact issue
    let simple_code = r#"
protocol Boolean {
    def to_string(value: Self): String
}

protocol Option<T> {
    def some?(value: Self): Boolean
}

def test_function(): Boolean {
    Option.some?(value: some_value)
}
"#;
    
    let mut program = parse_program(simple_code).expect("Parse should succeed");
    
    // Process through phases
    engine.register_protocols_and_structs(&program).expect("Phase 2 should succeed");
    engine.register_automatic_implementations(&program).expect("Phase 2.5 should succeed");
    engine.register_implementations(&program).expect("Phase 3 should succeed");
    
    // Check what happens during function registration (Phase 4)
    let phase4_result = engine.register_functions(&program);
    match phase4_result {
        Ok(()) => {
            println!("✅ Phase 4 succeeded - function signatures registered correctly");
            
            // Now check Phase 6 type checking
            let phase6_result = engine.typecheck_function_bodies(&mut program);
            match phase6_result {
                Ok(()) => println!("✅ Phase 6 succeeded - no Boolean type bug"),
                Err(e) => {
                    println!("❌ Phase 6 failed: {:?}", e);
                    let error_str = format!("{:?}", e);
                    if error_str.contains("Concrete { id: TypeId(\"Boolean\")") {
                        println!("🐛 Function call inference created incorrect concrete Boolean type");
                    }
                }
            }
        }
        Err(e) => {
            println!("❌ Phase 4 failed: {:?}", e);
            let error_str = format!("{:?}", e);
            if error_str.contains("Concrete { id: TypeId(\"Boolean\")") {
                println!("🐛 Function signature registration treated Boolean as concrete type");
                println!("This means convert_type_annotation incorrectly classified Boolean");
            }
        }
    }
}