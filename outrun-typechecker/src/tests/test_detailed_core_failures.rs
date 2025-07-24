//! Detailed analysis of remaining core library type checking failures

use crate::core_library::{collect_outrun_files, default_core_library_path};
use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_detailed_remaining_core_failures() {
    if let Ok(core_path) = default_core_library_path() {
        println!("🔍 Detailed analysis of remaining core library failures...");
        
        let lib_path = core_path.join("lib");
        let mut programs = Vec::new();
        
        // Collect files manually for detailed analysis
        if collect_outrun_files(&lib_path, &mut programs).is_ok() {
            println!("Found {} core library files", programs.len());
            
            // Parse all programs first
            let mut parsed_programs = Vec::new();
            for (file_path, content) in programs {
                match parse_program(&content) {
                    Ok(program) => {
                        parsed_programs.push((file_path, program));
                    }
                    Err(parse_error) => {
                        println!("❌ Parse error in {}: {:?}", file_path, parse_error);
                    }
                }
            }
            
            println!("Successfully parsed {} files", parsed_programs.len());
            
            // Create fresh engine for this test
            let mut engine = TypeInferenceEngine::new();
            
            // Phase 2: Register all protocols and structs
            println!("\n📋 Phase 2: Registering protocols and structs...");
            for (file_path, program) in &parsed_programs {
                match engine.register_protocols_and_structs(program) {
                    Ok(()) => println!("  ✅ {}", file_path.split('/').last().unwrap_or(file_path)),
                    Err(e) => println!("  ❌ {}: {:?}", file_path.split('/').last().unwrap_or(file_path), e),
                }
            }
            
            // Phase 2.5: Register automatic implementations
            println!("\n🤖 Phase 2.5: Registering automatic implementations...");
            for (file_path, program) in &parsed_programs {
                match engine.register_automatic_implementations(program) {
                    Ok(()) => println!("  ✅ {}", file_path.split('/').last().unwrap_or(file_path)),
                    Err(e) => println!("  ❌ {}: {:?}", file_path.split('/').last().unwrap_or(file_path), e),
                }
            }
            
            // Phase 3: Register implementations
            println!("\n🔗 Phase 3: Registering implementations...");
            for (file_path, program) in &parsed_programs {
                match engine.register_implementations(program) {
                    Ok(()) => println!("  ✅ {}", file_path.split('/').last().unwrap_or(file_path)),
                    Err(e) => println!("  ❌ {}: {:?}", file_path.split('/').last().unwrap_or(file_path), e),
                }
            }
            
            // Phase 4: Register functions
            println!("\n🎯 Phase 4: Registering functions...");
            for (file_path, program) in &parsed_programs {
                match engine.register_functions(program) {
                    Ok(()) => println!("  ✅ {}", file_path.split('/').last().unwrap_or(file_path)),
                    Err(e) => println!("  ❌ {}: {:?}", file_path.split('/').last().unwrap_or(file_path), e),
                }
            }
            
            // Phase 5: Validate implementation completeness
            println!("\n🔍 Phase 5: Validating implementation completeness...");
            match engine.validate_implementation_completeness() {
                Ok(()) => println!("  ✅ All implementations complete"),
                Err(e) => println!("  ❌ Implementation completeness error: {:?}", e),
            }
            
            // Phase 6: Type check function bodies (this is where the remaining failures occur)
            println!("\n🔬 Phase 6: Type checking function bodies...");
            let mut successful_files = Vec::new();
            let mut failed_files = Vec::new();
            
            for (file_path, mut program) in parsed_programs {
                let file_name = file_path.split('/').last().unwrap_or(&file_path);
                match engine.typecheck_function_bodies(&mut program) {
                    Ok(()) => {
                        println!("  ✅ {}", file_name);
                        successful_files.push(file_name.to_string());
                    }
                    Err(e) => {
                        println!("  ❌ {}: {}", file_name, e);
                        failed_files.push((file_name.to_string(), format!("{}", e)));
                        
                        // Show detailed error analysis
                        match &e {
                            crate::error::TypecheckError::InferenceError(inference_err) => {
                                match inference_err {
                                    crate::error::InferenceError::UndefinedVariable { variable_name, .. } => {
                                        println!("      🔍 Undefined variable: {}", variable_name);
                                    },
                                    crate::error::InferenceError::UndefinedType { type_name, .. } => {
                                        println!("      🔍 Undefined type: {}", type_name);
                                    },
                                    crate::error::InferenceError::UndefinedProtocol { protocol_name, .. } => {
                                        println!("      🔍 Undefined protocol: {}", protocol_name);
                                    },
                                    _ => {
                                        println!("      🔍 Other inference error: {:?}", inference_err);
                                    }
                                }
                            },
                            crate::error::TypecheckError::UnificationError(unification_err) => {
                                match unification_err {
                                    crate::error::UnificationError::TypeMismatch { expected, found, .. } => {
                                        println!("      🔍 Type mismatch: expected {}, found {}", expected, found);
                                    },
                                    crate::error::UnificationError::OccursCheckViolation { var_name, containing_type, .. } => {
                                        println!("      🔍 Occurs check: {} in {}", var_name, containing_type);
                                    },
                                    _ => {
                                        println!("      🔍 Other unification error: {:?}", unification_err);
                                    }
                                }
                            },
                            crate::error::TypecheckError::DispatchError(dispatch_err) => {
                                match dispatch_err {
                                    crate::error::DispatchError::NoImplementation { protocol_name, type_name, .. } => {
                                        println!("      🔍 No implementation: {} for {}", protocol_name, type_name);
                                    },
                                    crate::error::DispatchError::AmbiguousDispatch { protocol_name, candidates, .. } => {
                                        println!("      🔍 Ambiguous dispatch: {} with {} candidates", protocol_name, candidates.len());
                                    },
                                    _ => {
                                        println!("      🔍 Other dispatch error: {:?}", dispatch_err);
                                    }
                                }
                            },
                            crate::error::TypecheckError::ConstraintError(constraint_err) => {
                                match constraint_err {
                                    crate::error::ConstraintError::MissingImplementation { protocol_name, type_name, .. } => {
                                        println!("      🔍 Missing implementation: {} for {}", protocol_name, type_name);
                                    },
                                    _ => {
                                        println!("      🔍 Other constraint error: {:?}", constraint_err);
                                    }
                                }
                            },
                            _ => {
                                println!("      🔍 Other error type: {:?}", e);
                            }
                        }
                    }
                }
            }
            
            println!("\n📊 FINAL SUMMARY:");
            println!("  ✅ Successfully type checked: {} files ({:.1}%)", 
                     successful_files.len(), 
                     (successful_files.len() as f64 / (successful_files.len() + failed_files.len()) as f64) * 100.0);
            println!("  ❌ Failed to type check: {} files ({:.1}%)", 
                     failed_files.len(),
                     (failed_files.len() as f64 / (successful_files.len() + failed_files.len()) as f64) * 100.0);
            
            if !failed_files.is_empty() {
                println!("\n❌ FAILING FILES:");
                for (file_name, error) in &failed_files {
                    println!("  • {}: {}", file_name, error);
                }
                
                println!("\n🎯 NEXT STEPS:");
                println!("Pick one of the failing files to investigate and fix:");
                if let Some((first_file, first_error)) = failed_files.first() {
                    println!("  👉 Start with: {} ({})", first_file, first_error);
                }
            }
            
        } else {
            println!("Failed to collect core library files");
        }
    } else {
        println!("Core library path not found");
    }
}