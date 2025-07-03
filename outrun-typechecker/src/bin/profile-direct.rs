// Direct profiling of the lazy static compilation
use std::time::Instant;

fn main() {
    println!("🔬 Direct profiling of core library lazy static...");
    
    let start = Instant::now();
    let _compilation = outrun_typechecker::core_library::get_core_library_compilation();
    let duration = start.elapsed();
    
    println!("✅ Lazy static compilation took: {:.2}s", duration.as_secs_f64());
    
    // Check SMT cache
    let cache_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("📊 SMT Cache: {}", cache_stats);
    
    // Try calling it again to see if it's cached
    println!("\n🔄 Calling it again (should be instant)...");
    let start2 = Instant::now();
    let _compilation2 = outrun_typechecker::core_library::get_core_library_compilation();
    let duration2 = start2.elapsed();
    
    println!("✅ Second call took: {:.6}s", duration2.as_secs_f64());
    
    if duration2.as_millis() < 1 {
        println!("✅ Confirmed: lazy_static is working properly");
        println!("🎯 The 33s is real compilation time, not repeated work");
    }
}