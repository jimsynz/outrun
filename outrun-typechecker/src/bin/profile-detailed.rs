// Detailed profiling binary for core library compilation phases
use outrun_typechecker::core_library;
use std::time::Instant;

fn main() {
    println!("🔬 Starting detailed core library compilation profiling...");
    
    let total_start = Instant::now();
    
    // This is what actually gets timed in the lazy_static
    println!("⏳ Triggering core library compilation...");
    let compilation_start = Instant::now();
    let _compilation = core_library::get_core_library_compilation();
    let compilation_duration = compilation_start.elapsed();
    
    let total_duration = total_start.elapsed();
    
    println!("📊 Detailed Timing Results:");
    println!("  Core library compilation: {:.2}s", compilation_duration.as_secs_f64());
    println!("  Total process time: {:.2}s", total_duration.as_secs_f64());
    println!("  Overhead: {:.2}s", (total_duration - compilation_duration).as_secs_f64());
    
    // Now let's see if we can break down the compilation phases
    println!("\n🔍 Cache Statistics:");
    let cache_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("  SMT Cache: {}", cache_stats);
    
    if cache_stats.total_queries == 0 {
        println!("  ⚠️  No SMT queries were made! This suggests:");
        println!("     - SMT solving is not the bottleneck");
        println!("     - Most work is in parsing, AST building, or other phases");
        println!("     - Async parallelization won't help here");
    } else {
        println!("  ✅ SMT queries were made, async could potentially help");
    }
}