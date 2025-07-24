//! Test exact string.outrun failure scenario

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_exact_string_contains_failure() {
    let mut engine = TypeInferenceEngine::new();
    
    // Use the exact protocols from core library
    let test_code = r#"
protocol Boolean when Self: LogicalAnd && Self: LogicalOr && Self: LogicalNot {
    def true?(value: Self): Outrun.Core.Boolean
    def false?(value: Self): Outrun.Core.Boolean {
        !Boolean.true?(value: value)
    }
}

protocol LogicalAnd {
    def and(left: Self, right: Self): Self
}

protocol LogicalOr {
    def or(left: Self, right: Self): Self
}

protocol LogicalNot {
    def not?(value: Self): Self
}

protocol Option<T> {
    def some?(value: Self): Boolean
    def index_of(value: Self, search: Self): Option<Integer>
}

protocol String {
    def contains?(value: Self, search: Self): Boolean {
        Option.some?(value: index_of(value: value, search: search))
    }
    def index_of(value: Self, search: Self): Option<Integer>
}

protocol Integer {
    def add(left: Self, right: Self): Self
}

struct Outrun.Core.Boolean() {}

impl Boolean for Outrun.Core.Boolean {
    def true?(value: Self): Outrun.Core.Boolean {
        value
    }
}
"#;
    
    let mut program = parse_program(test_code).expect("Parse should succeed");
    
    println!("=== Processing exact string.outrun failure scenario ===");
    
    // Process through all phases
    engine.register_protocols_and_structs(&program).expect("Phase 2 should succeed");
    engine.register_automatic_implementations(&program).expect("Phase 2.5 should succeed");  
    engine.register_implementations(&program).expect("Phase 3 should succeed");
    engine.register_functions(&program).expect("Phase 4 should succeed");
    
    // This should reproduce the exact error from string.outrun
    let result = engine.typecheck_function_bodies(&mut program);
    
    match result {
        Ok(()) => {
            println!("✅ Type checking succeeded - bug not reproduced");
        }
        Err(e) => {
            println!("❌ Type checking failed: {:?}", e);
            let error_str = format!("{:?}", e);
            if error_str.contains("expected: Protocol { id: ProtocolId(\"Boolean\")") && 
               error_str.contains("found: Concrete { id: TypeId(\"Boolean\")") {
                println!("🐛 EXACT BUG REPRODUCED!");
                println!("The issue is in function call resolution during type checking");
                println!("Option.some?() call is returning wrong type during expression inference");
            } else {
                println!("Different error - may be related but not the exact bug");
            }
        }
    }
}