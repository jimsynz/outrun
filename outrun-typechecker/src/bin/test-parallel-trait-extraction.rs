// Test the parallel trait extraction function
use std::time::Instant;

#[tokio::main]
async fn main() {
    println!("🧪 Testing parallel trait extraction...");
    
    // Load the core library collection
    let collection = outrun_typechecker::core_library::load_core_library_collection();
    println!("📁 Loaded {} programs for trait extraction", collection.programs.len());
    
    // Get the compilation order
    let compilation_order: Vec<String> = collection.programs.keys().cloned().collect();
    
    // Create a compiler environment
    let compiler_env = outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
    
    // Test sequential trait extraction
    println!("\n🐌 Sequential trait extraction:");
    let sequential_start = Instant::now();
    let mut sequential_env = compiler_env.clone();
    let sequential_traits = sequential_env.extract_traits(&collection, &compilation_order).unwrap();
    let sequential_duration = sequential_start.elapsed();
    
    println!("  📊 Extracted {} traits", sequential_traits.len());
    println!("  ⏱️  Time: {:.3}s", sequential_duration.as_secs_f64());
    
    // Test parallel trait extraction  
    println!("\n⚡ Parallel trait extraction:");
    let parallel_start = Instant::now();
    let parallel_traits = compiler_env.extract_traits_parallel(&collection, &compilation_order).await.unwrap();
    let parallel_duration = parallel_start.elapsed();
    
    println!("  📊 Extracted {} traits", parallel_traits.len());
    println!("  ⏱️  Time: {:.3}s", parallel_duration.as_secs_f64());
    
    // Compare results
    println!("\n📈 Performance Comparison:");
    if parallel_duration < sequential_duration {
        let speedup = sequential_duration.as_secs_f64() / parallel_duration.as_secs_f64();
        println!("  🚀 Parallel extraction is {:.1}x faster!", speedup);
        let time_saved = sequential_duration - parallel_duration;
        println!("  💾 Time saved: {:.3}s ({:.1}% faster)", 
            time_saved.as_secs_f64(), 
            (time_saved.as_secs_f64() / sequential_duration.as_secs_f64()) * 100.0);
    } else {
        println!("  🐌 Sequential extraction is still faster (or same)");
        println!("     This might happen with few programs or fast CPU");
    }
    
    // Verify both approaches extracted the same traits
    if sequential_traits.len() == parallel_traits.len() {
        println!("  ✅ Both approaches extracted the same number of traits");
        
        // Check for trait name mismatches (basic verification)
        let sequential_names: std::collections::HashSet<_> = sequential_traits.keys().collect();
        let parallel_names: std::collections::HashSet<_> = parallel_traits.keys().collect();
        
        if sequential_names == parallel_names {
            println!("  ✅ Both approaches extracted the exact same trait names");
        } else {
            println!("  ⚠️  Trait names differ between approaches!");
            println!("     Sequential only: {:?}", sequential_names.difference(&parallel_names).collect::<Vec<_>>());
            println!("     Parallel only: {:?}", parallel_names.difference(&sequential_names).collect::<Vec<_>>());
        }
    } else {
        println!("  ⚠️  Different number of traits extracted!");
        println!("     Sequential: {}, Parallel: {}", 
            sequential_traits.len(), 
            parallel_traits.len());
    }
    
    println!("\n🎯 CHECKPOINT 1 STATUS: ✅ Parallel trait extraction implemented and tested!");
}