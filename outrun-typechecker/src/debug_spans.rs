//! Debug utilities for investigating span corruption issues

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

pub fn debug_minimal_typecheck() {
    let source = r#"
struct Test {
    def simple(): Integer { 42 }
}
"#;

    println!("🔍 Testing span tracking without core library");
    println!("Source code:");
    println!("{}", source);
    println!();

    // Parse the program
    let program = match parse_program(source) {
        Ok(program) => {
            println!("✅ Parsing successful");
            println!("Program items: {}", program.items.len());
            for (i, item) in program.items.iter().enumerate() {
                println!("  Item {}: {:?} at span {:?}", i, item.kind, item.span);
            }
            program
        }
        Err(e) => {
            println!("❌ Parsing failed: {e}");
            return;
        }
    };

    // Create a minimal typechecker without core library
    let mut engine = TypeInferenceEngine::new();

    // Try to register the struct
    match engine.register_protocols_and_structs(&program) {
        Ok(()) => {
            println!("✅ Phase 2: Struct registration successful");
        }
        Err(e) => {
            println!("❌ Phase 2: Struct registration failed: {e}");
            println!("Error details: {e:#?}");
            return;
        }
    }

    // Try to register implementations
    match engine.register_implementations(&program) {
        Ok(()) => {
            println!("✅ Phase 3: Implementation registration successful");
        }
        Err(e) => {
            println!("❌ Phase 3: Implementation registration failed: {e}");
            println!("Error details: {e:#?}");
            return;
        }
    }

    // Try to typecheck the program
    let mut program_mut = program;
    match engine.typecheck_program(&mut program_mut) {
        Ok(()) => {
            println!("✅ Type checking successful without core library!");
        }
        Err(e) => {
            println!("❌ Type checking failed: {e}");
            println!("Error details: {e:#?}");
        }
    }
}