// Test function lookup scenarios that might trigger recursive SMT calls
use std::time::Instant;

fn main() {
    println!("🔬 Testing function lookup scenarios for recursive SMT calls...");
    
    // Clear cache first
    outrun_typechecker::smt::solver_pool::clear_cache();
    
    // Test core library compilation with more detailed logging
    println!("⏳ Testing core library compilation with cached SMT...");
    let start = Instant::now();
    let compilation = outrun_typechecker::core_library::get_core_library_compilation();
    let duration = start.elapsed();
    
    println!("✅ Core library compilation took: {:.2}s", duration.as_secs_f64());
    
    // Check cache stats
    let cache_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("📊 Final cache stats: {}", cache_stats);
    
    // Verify we have some implementations compiled
    println!("📋 Compiled {} traits, {} structs, {} implementations", 
        compilation.traits.len(),
        compilation.structs.len(), 
        compilation.implementations.len());
    
    if compilation.implementations.len() > 0 {
        println!("✅ Successfully compiled implementations without deadlock!");
    } else {
        println!("⚠️ No implementations found - possible compilation issue");
    }
}