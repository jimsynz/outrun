//! Debug Phase 3 registration to identify ConflictingImplementation sources

use crate::core_library::{collect_outrun_files, default_core_library_path};
use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_phase3_registration_detailed_tracking() {
    let mut engine = TypeInferenceEngine::new();

    println!("🔍 Debug: Phase 3 registration with detailed tracking");

    // Get core library and collect all files
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

    println!("📊 Parsed {} files successfully", parsed_programs.len());

    // Complete Phase 2 and 2.5 first
    println!("\n⚙️ Completing Phase 2 and 2.5...");
    for (_, program) in &parsed_programs {
        engine
            .register_protocols_and_structs(program)
            .expect("Phase 2 should succeed");
    }
    for (_, program) in &parsed_programs {
        engine
            .register_automatic_implementations(program)
            .expect("Phase 2.5 should succeed");
    }

    // Now process Phase 3 with detailed tracking
    println!("\n🎯 Phase 3: Tracking implementation registrations...");

    // Focus on the specific files that are failing
    let problematic_files = [
        "sigil.outrun",
        "outrun/core/list.outrun",
        "outrun/core/atom.outrun",
        "outrun/core/integer64.outrun",
        "outrun/core/float64.outrun",
        "outrun/core/string.outrun",
        "outrun/core/map.outrun",
        "outrun/core/boolean.outrun",
        "outrun/option/none.outrun",
        "outrun/option/some.outrun",
    ];

    for (file_path, program) in &parsed_programs {
        let file_name = file_path.split('/').last().unwrap_or(file_path);
        let is_problematic = problematic_files.iter().any(|pf| file_path.ends_with(pf));

        if is_problematic {
            println!("\n🚨 Processing problematic file: {}", file_path);

            // Count impl blocks in this file
            let impl_count = program
                .items
                .iter()
                .filter(|item| matches!(item.kind, outrun_parser::ItemKind::ImplBlock(_)))
                .count();

            println!("   📄 Contains {} impl blocks", impl_count);

            // Show what impl blocks are in this file
            for item in &program.items {
                if let outrun_parser::ItemKind::ImplBlock(impl_block) = &item.kind {
                    let protocol_name = impl_block
                        .protocol_spec
                        .path
                        .iter()
                        .map(|segment| segment.name.as_str())
                        .collect::<Vec<_>>()
                        .join(".");
                    let type_name = impl_block
                        .type_spec
                        .path
                        .iter()
                        .map(|segment| segment.name.as_str())
                        .collect::<Vec<_>>()
                        .join(".");
                    println!("   📋 impl {} for {}", protocol_name, type_name);
                }
            }
        }

        // Try to register implementations from this file
        match engine.register_implementations(program) {
            Ok(()) => {
                if is_problematic {
                    println!("   ✅ Registration succeeded");
                }
            }
            Err(e) => {
                if is_problematic {
                    println!("   ❌ Registration failed: {}", e);
                    match &e {
                        crate::error::TypecheckError::ImplementationError(impl_err) => {
                            match impl_err {
                                crate::error::ImplementationError::ConflictingImplementation {
                                    protocol_name,
                                    type_name,
                                    ..
                                } => {
                                    println!(
                                        "   🚨 Conflicting implementation: {} for {}",
                                        protocol_name, type_name
                                    );
                                }
                                _ => {
                                    println!("   🚨 Other implementation error: {:?}", impl_err);
                                }
                            }
                        }
                        _ => {
                            println!("   🚨 Other error type: {:?}", e);
                        }
                    }

                    // This is the first file that fails - break here to see the pattern
                    println!("\n🔍 First ConflictingImplementation error occurred!");
                    println!("File: {}", file_path);
                    return;
                } else {
                    // Non-problematic file failed - this is unexpected
                    println!("❌ Unexpected failure in {}: {}", file_path, e);
                }
            }
        }
    }

    println!("\n✅ All files processed successfully");
}
