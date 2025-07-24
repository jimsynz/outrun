//! Test multi-file core library processing to identify dependency issues

use crate::core_library::{collect_outrun_files, default_core_library_path};
use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_multi_core_file_processing() {
    let mut engine = TypeInferenceEngine::new();
    
    println!("=== Testing multi-file core library processing ===");
    
    // Get the core library path
    let core_path = default_core_library_path().expect("Should find core library");
    let lib_path = core_path.join("lib");
    
    // Collect just a few specific files to test
    let mut programs = Vec::new();
    collect_outrun_files(&lib_path, &mut programs).expect("Should collect files");
    
    // Filter to just the failing files we saw in the error
    let failing_files = vec![
        "boolean.outrun",
        "outrun/core/boolean.outrun",
        "outrun/core/string.outrun",
    ];
    
    let mut test_programs = Vec::new();
    for (file_path, content) in programs {
        for failing_file in &failing_files {
            if file_path.ends_with(failing_file) {
                test_programs.push((file_path.clone(), content.clone()));
                println!("Added file: {}", file_path);
                break; // Only add each file once
            }
        }
    }
    
    println!("Testing with {} files", test_programs.len());
    
    // Parse all programs
    let mut parsed_programs = Vec::new();
    for (file_path, content) in test_programs {
        match parse_program(&content) {
            Ok(program) => {
                parsed_programs.push((file_path, program));
            }
            Err(parse_error) => {
                panic!("Failed to parse {}: {:?}", file_path, parse_error);
            }
        }
    }
    
    println!("=== Phase 2: Register protocols and structs ===");
    for (file_path, program) in &parsed_programs {
        match engine.register_protocols_and_structs(program) {
            Ok(()) => println!("✅ Phase 2: {}", file_path),
            Err(e) => {
                println!("❌ Phase 2: {} - {:?}", file_path, e);
                return;
            }
        }
    }
    
    println!("=== Phase 2.5: Register automatic implementations ===");
    for (file_path, program) in &parsed_programs {
        match engine.register_automatic_implementations(program) {
            Ok(()) => println!("✅ Phase 2.5: {}", file_path),
            Err(e) => {
                println!("❌ Phase 2.5: {} - {:?}", file_path, e);
                println!("This is likely a ConflictingImplementation error");
                // Let's continue to see if other files have the same issue
            }
        }
    }
    
    println!("=== Phase 3: Register implementations ===");
    for (file_path, program) in &parsed_programs {
        match engine.register_implementations(program) {
            Ok(()) => println!("✅ Phase 3: {}", file_path),
            Err(e) => {
                println!("❌ Phase 3: {} - {:?}", file_path, e);
            }
        }
    }
    
    println!("=== Phase 4: Register functions ===");
    for (file_path, program) in &parsed_programs {
        match engine.register_functions(program) {
            Ok(()) => println!("✅ Phase 4: {}", file_path),
            Err(e) => {
                println!("❌ Phase 4: {} - {:?}", file_path, e);
            }
        }
    }
    
    println!("=== Phase 6: Type check function bodies ===");
    for (file_path, mut program) in parsed_programs {
        match engine.typecheck_function_bodies(&mut program) {
            Ok(()) => println!("✅ Phase 6: {}", file_path),
            Err(e) => {
                println!("❌ Phase 6: {} - {:?}", file_path, e);
                println!("Error details: {}", e);
            }
        }
    }
}