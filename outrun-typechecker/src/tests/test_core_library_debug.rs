//! Debug test for core library precompilation issue

use crate::CompilationResult;

#[test]
fn test_core_library_precompilation() {
    println!("Testing core library precompilation...");

    match CompilationResult::precompile_core_library() {
        Ok(result) => {
            println!("✅ Core library precompiled successfully!");
            println!("📊 Programs: {}", result.programs.len());
            println!("📊 Function registry created successfully");
            // Test passes if we get here
        }
        Err(e) => {
            println!("❌ Core library precompilation failed: {}", e);
            println!("❌ Error debug: {:?}", e);
            panic!("Core library precompilation failed: {}", e);
        }
    }
}
