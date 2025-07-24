//! Debug sigil.outrun Protocol implementation error

use crate::core_library::{collect_outrun_files, default_core_library_path};
use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_sigil_protocol_implementation_error() {
    let mut engine = TypeInferenceEngine::new();
    
    println!("=== Debugging sigil.outrun Protocol implementation error ===");
    
    // Get the core library path and collect all files
    let core_path = default_core_library_path().expect("Should find core library");
    let lib_path = core_path.join("lib");
    
    let mut programs = Vec::new();
    collect_outrun_files(&lib_path, &mut programs).expect("Should collect files");
    
    // Parse all programs
    let mut parsed_programs = Vec::new();
    for (file_path, content) in programs {
        match parse_program(&content) {
            Ok(program) => {
                parsed_programs.push((file_path, program));
            }
            Err(parse_error) => {
                panic!("Failed to parse {}: {:?}", file_path, parse_error);
            }
        }
    }
    
    println!("Parsed {} files", parsed_programs.len());
    
    // Process through all phases like the real core library loading
    println!("=== Phase 2: Register protocols and structs ===");
    for (file_path, program) in &parsed_programs {
        if let Err(e) = engine.register_protocols_and_structs(program) {
            println!("❌ Phase 2 failed for {}: {:?}", file_path, e);
            return;
        }
    }
    println!("✅ Phase 2 completed successfully");
    
    println!("=== Phase 2.5: Register automatic implementations ===");
    for (file_path, program) in &parsed_programs {
        if let Err(e) = engine.register_automatic_implementations(program) {
            println!("❌ Phase 2.5 failed for {}: {:?}", file_path, e);
            return;
        }
    }
    println!("✅ Phase 2.5 completed successfully");
    
    println!("=== Phase 3: Register implementations ===");
    for (file_path, program) in &parsed_programs {
        if let Err(e) = engine.register_implementations(program) {
            println!("❌ Phase 3 failed for {}: {:?}", file_path, e);
            return;
        }
    }
    println!("✅ Phase 3 completed successfully");
    
    println!("=== Phase 4: Register functions ===");
    for (file_path, program) in &parsed_programs {
        if let Err(e) = engine.register_functions(program) {
            println!("❌ Phase 4 failed for {}: {:?}", file_path, e);
            return;
        }
    }
    println!("✅ Phase 4 completed successfully");
    
    println!("=== Phase 6: Type check function bodies (focus on sigil.outrun) ===");
    for (file_path, mut program) in parsed_programs {
        if file_path.ends_with("sigil.outrun") {
            println!("🎯 Testing sigil.outrun specifically...");
            match engine.typecheck_function_bodies(&mut program) {
                Ok(()) => {
                    println!("✅ sigil.outrun type checking succeeded!");
                }
                Err(e) => {
                    println!("❌ sigil.outrun type checking failed: {:?}", e);
                    println!("Full error: {}", e);
                    
                    // Let's also try to get more context about the specific error
                    let error_str = format!("{:?}", e);
                    if error_str.contains("Protocol implementation error") {
                        println!("🐛 Confirmed: This is a Protocol implementation error");
                    }
                    if error_str.contains("UnificationError") {
                        println!("🐛 This is a type unification error during body checking");
                    }
                }
            }
            break;
        }
    }
}