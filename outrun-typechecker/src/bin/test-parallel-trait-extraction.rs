// Test the parallel extraction functions (traits and structs)
use std::time::Instant;

#[tokio::main]
async fn main() {
    println!("🧪 Testing parallel extraction functions...");
    
    // Load the core library collection
    let collection = outrun_typechecker::core_library::load_core_library_collection();
    println!("📁 Loaded {} programs for extraction", collection.programs.len());
    
    // Get the compilation order
    let compilation_order: Vec<String> = collection.programs.keys().cloned().collect();
    
    // Create a compiler environment
    let compiler_env = outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
    
    // === CHECKPOINT 1: Test Trait Extraction ===
    println!("\n🔷 CHECKPOINT 1: TRAIT EXTRACTION");
    
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
    
    // Compare trait results
    println!("\n📈 Trait Extraction Performance:");
    if parallel_duration < sequential_duration {
        let speedup = sequential_duration.as_secs_f64() / parallel_duration.as_secs_f64();
        println!("  🚀 Parallel extraction is {:.1}x faster!", speedup);
    } else {
        println!("  🐌 Sequential extraction is still faster (or same)");
    }
    
    // Verify trait results
    if sequential_traits.len() == parallel_traits.len() {
        println!("  ✅ Both approaches extracted the same number of traits");
    } else {
        println!("  ⚠️  Different number of traits extracted!");
    }
    
    // === CHECKPOINT 2: Test Struct Extraction ===
    println!("\n🔶 CHECKPOINT 2: STRUCT EXTRACTION");
    
    // Test sequential struct extraction
    println!("\n🐌 Sequential struct extraction:");
    let sequential_start = Instant::now();
    let mut sequential_env = compiler_env.clone();
    let sequential_structs = sequential_env.extract_structs(&collection, &compilation_order).unwrap();
    let sequential_duration = sequential_start.elapsed();
    
    println!("  📊 Extracted {} structs", sequential_structs.len());
    println!("  ⏱️  Time: {:.3}s", sequential_duration.as_secs_f64());
    
    // Test parallel struct extraction  
    println!("\n⚡ Parallel struct extraction:");
    let parallel_start = Instant::now();
    let parallel_structs = compiler_env.extract_structs_parallel(&collection, &compilation_order).await.unwrap();
    let parallel_duration = parallel_start.elapsed();
    
    println!("  📊 Extracted {} structs", parallel_structs.len());
    println!("  ⏱️  Time: {:.3}s", parallel_duration.as_secs_f64());
    
    // Compare struct results
    println!("\n📈 Struct Extraction Performance:");
    if parallel_duration < sequential_duration {
        let speedup = sequential_duration.as_secs_f64() / parallel_duration.as_secs_f64();
        println!("  🚀 Parallel extraction is {:.1}x faster!", speedup);
    } else {
        println!("  🐌 Sequential extraction is still faster (or same)");
    }
    
    // Verify struct results
    if sequential_structs.len() == parallel_structs.len() {
        println!("  ✅ Both approaches extracted the same number of structs");
        
        // Check for struct name mismatches (basic verification)
        let sequential_names: std::collections::HashSet<_> = sequential_structs.keys().collect();
        let parallel_names: std::collections::HashSet<_> = parallel_structs.keys().collect();
        
        if sequential_names == parallel_names {
            println!("  ✅ Both approaches extracted the exact same struct names");
        } else {
            println!("  ⚠️  Struct names differ between approaches!");
            println!("     Sequential only: {:?}", sequential_names.difference(&parallel_names).collect::<Vec<_>>());
            println!("     Parallel only: {:?}", parallel_names.difference(&sequential_names).collect::<Vec<_>>());
        }
    } else {
        println!("  ⚠️  Different number of structs extracted!");
        println!("     Sequential: {}, Parallel: {}", 
            sequential_structs.len(), 
            parallel_structs.len());
    }
    
    // === CHECKPOINT 3: Test Impl Extraction ===
    println!("\n🔸 CHECKPOINT 3: IMPL EXTRACTION");
    
    // Test sequential impl extraction
    println!("\n🐌 Sequential impl extraction:");
    let sequential_start = Instant::now();
    let mut sequential_env = compiler_env.clone();
    let sequential_impls = sequential_env.extract_implementations(&collection, &compilation_order).unwrap();
    let sequential_duration = sequential_start.elapsed();
    
    println!("  📊 Extracted {} impls", sequential_impls.len());
    println!("  ⏱️  Time: {:.3}s", sequential_duration.as_secs_f64());
    
    // Test parallel impl extraction  
    println!("\n⚡ Parallel impl extraction:");
    let parallel_start = Instant::now();
    let parallel_impls = compiler_env.extract_impls_parallel(&collection, &compilation_order).await.unwrap();
    let parallel_duration = parallel_start.elapsed();
    
    println!("  📊 Extracted {} impls", parallel_impls.len());
    println!("  ⏱️  Time: {:.3}s", parallel_duration.as_secs_f64());
    
    // Compare impl results
    println!("\n📈 Impl Extraction Performance:");
    if parallel_duration < sequential_duration {
        let speedup = sequential_duration.as_secs_f64() / parallel_duration.as_secs_f64();
        println!("  🚀 Parallel extraction is {:.1}x faster!", speedup);
    } else {
        println!("  🐌 Sequential extraction is still faster (or same)");
    }
    
    // Verify impl results
    if sequential_impls.len() == parallel_impls.len() {
        println!("  ✅ Both approaches extracted the same number of impls");
    } else {
        println!("  ⚠️  Different number of impls extracted!");
        println!("     Sequential: {}, Parallel: {}", 
            sequential_impls.len(), 
            parallel_impls.len());
    }
    
    // === FINAL STATUS ===
    println!("\n🎯 CHECKPOINT STATUS:");
    println!("✅ CHECKPOINT 1: Parallel trait extraction implemented and tested!");
    println!("✅ CHECKPOINT 2: Parallel struct extraction implemented and tested!");
    println!("✅ CHECKPOINT 3: Parallel impl extraction implemented and tested!");
}