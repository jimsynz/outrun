// Benchmark parallel vs sequential file loading and parsing
use std::time::Instant;

#[tokio::main]
async fn main() {
    println!("🏁 Benchmarking parallel vs sequential file loading...");
    
    // Warm up by doing one compilation
    println!("🔥 Warming up (sequential)...");
    let _ = outrun_typechecker::core_library::load_core_library_collection();
    
    println!("\n📊 Running benchmarks...");
    
    // Test 1: Sequential loading (current approach)
    println!("\n🐌 Sequential file loading:");
    let sequential_start = Instant::now();
    let sequential_collection = outrun_typechecker::core_library::load_core_library_collection();
    let sequential_duration = sequential_start.elapsed();
    
    println!("  📁 Loaded {} programs", sequential_collection.programs.len());
    println!("  ⏱️  Time: {:.3}s", sequential_duration.as_secs_f64());
    
    // Test 2: Parallel loading (new approach)
    println!("\n⚡ Parallel file loading:");
    let parallel_start = Instant::now();
    let parallel_collection = outrun_typechecker::core_library::load_core_library_collection_parallel().await;
    let parallel_duration = parallel_start.elapsed();
    
    println!("  📁 Loaded {} programs", parallel_collection.programs.len());
    println!("  ⏱️  Time: {:.3}s", parallel_duration.as_secs_f64());
    
    // Compare results
    println!("\n📈 Performance Comparison:");
    if parallel_duration < sequential_duration {
        let speedup = sequential_duration.as_secs_f64() / parallel_duration.as_secs_f64();
        println!("  🚀 Parallel loading is {:.1}x faster!", speedup);
        let time_saved = sequential_duration - parallel_duration;
        println!("  💾 Time saved: {:.3}s ({:.1}% faster)", 
            time_saved.as_secs_f64(), 
            (time_saved.as_secs_f64() / sequential_duration.as_secs_f64()) * 100.0);
    } else {
        println!("  🐌 Sequential loading is still faster (or same)");
        println!("     This might happen with very few files or fast storage");
    }
    
    // Verify both approaches loaded the same data
    if sequential_collection.programs.len() == parallel_collection.programs.len() {
        println!("  ✅ Both approaches loaded the same number of programs");
    } else {
        println!("  ⚠️  Different number of programs loaded!");
        println!("     Sequential: {}, Parallel: {}", 
            sequential_collection.programs.len(), 
            parallel_collection.programs.len());
    }
    
    // Now test full compilation with parallel loading
    println!("\n🏗️  Full compilation with parallel loading:");
    let full_compile_start = Instant::now();
    let _compilation_result = outrun_typechecker::core_library::load_and_compile_core_library_parallel().await;
    let full_compile_duration = full_compile_start.elapsed();
    
    println!("  ⏱️  Full parallel compilation: {:.2}s", full_compile_duration.as_secs_f64());
    
    // Get cache stats
    let cache_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("  📊 SMT Cache: {}", cache_stats);
}