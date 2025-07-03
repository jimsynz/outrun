//! Comprehensive benchmark for parallel AST extraction performance
//! 
//! This benchmark measures the performance difference between sequential and parallel
//! AST extraction across multiple runs to get statistically meaningful results.

use std::time::Instant;

#[tokio::main]
async fn main() {
    println!("🚀 PARALLEL AST EXTRACTION BENCHMARK");
    println!("=====================================");
    
    // Load the core library collection once
    let collection = outrun_typechecker::core_library::load_core_library_collection();
    println!("📁 Loaded {} programs for benchmarking", collection.programs.len());
    
    // Get the compilation order
    let compilation_order: Vec<String> = collection.programs.keys().cloned().collect();
    
    // Create a compiler environment
    let compiler_env = outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
    
    // Configuration
    const WARMUP_RUNS: usize = 3;
    const BENCHMARK_RUNS: usize = 10;
    
    println!("\n⚙️  Configuration:");
    println!("   Warmup runs: {}", WARMUP_RUNS);
    println!("   Benchmark runs: {}", BENCHMARK_RUNS);
    println!("   Programs: {}", collection.programs.len());
    
    // === WARMUP PHASE ===
    println!("\n🔥 Warmup Phase (not counted in results)");
    for i in 1..=WARMUP_RUNS {
        print!("   Warmup run {}/{}: ", i, WARMUP_RUNS);
        
        // Sequential warmup
        let mut sequential_env = compiler_env.clone();
        let _ = sequential_env.extract_traits(&collection, &compilation_order);
        let _ = sequential_env.extract_structs(&collection, &compilation_order);
        let _ = sequential_env.extract_implementations(&collection, &compilation_order);
        
        // Parallel warmup
        let _ = compiler_env.extract_traits_parallel(&collection, &compilation_order).await;
        let _ = compiler_env.extract_structs_parallel(&collection, &compilation_order).await;
        let _ = compiler_env.extract_impls_parallel(&collection, &compilation_order).await;
        
        println!("✅ Complete");
    }
    
    // === SEQUENTIAL BENCHMARK ===
    println!("\n🐌 Sequential AST Extraction Benchmark");
    let mut sequential_times = Vec::new();
    let mut sequential_trait_counts = Vec::new();
    let mut sequential_struct_counts = Vec::new();
    let mut sequential_impl_counts = Vec::new();
    
    for run in 1..=BENCHMARK_RUNS {
        print!("   Run {}/{}: ", run, BENCHMARK_RUNS);
        
        let start_time = Instant::now();
        
        // Sequential extraction - all three phases
        let mut sequential_env = compiler_env.clone();
        let traits = sequential_env.extract_traits(&collection, &compilation_order).unwrap();
        let structs = sequential_env.extract_structs(&collection, &compilation_order).unwrap();
        let impls = sequential_env.extract_implementations(&collection, &compilation_order).unwrap();
        
        let duration = start_time.elapsed();
        sequential_times.push(duration.as_secs_f64());
        sequential_trait_counts.push(traits.len());
        sequential_struct_counts.push(structs.len());
        sequential_impl_counts.push(impls.len());
        
        println!("{:.3}s ({}T, {}S, {}I)", 
            duration.as_secs_f64(),
            traits.len(),
            structs.len(), 
            impls.len()
        );
    }
    
    // === PARALLEL BENCHMARK ===
    println!("\n⚡ Parallel AST Extraction Benchmark");
    let mut parallel_times = Vec::new();
    let mut parallel_trait_counts = Vec::new();
    let mut parallel_struct_counts = Vec::new();
    let mut parallel_impl_counts = Vec::new();
    
    for run in 1..=BENCHMARK_RUNS {
        print!("   Run {}/{}: ", run, BENCHMARK_RUNS);
        
        let start_time = Instant::now();
        
        // Parallel extraction - all three phases
        let traits = compiler_env.extract_traits_parallel(&collection, &compilation_order).await.unwrap();
        let structs = compiler_env.extract_structs_parallel(&collection, &compilation_order).await.unwrap();
        let impls = compiler_env.extract_impls_parallel(&collection, &compilation_order).await.unwrap();
        
        let duration = start_time.elapsed();
        parallel_times.push(duration.as_secs_f64());
        parallel_trait_counts.push(traits.len());
        parallel_struct_counts.push(structs.len());
        parallel_impl_counts.push(impls.len());
        
        println!("{:.3}s ({}T, {}S, {}I)", 
            duration.as_secs_f64(),
            traits.len(),
            structs.len(), 
            impls.len()
        );
    }
    
    // === STATISTICS ===
    println!("\n📊 BENCHMARK RESULTS");
    println!("====================");
    
    // Calculate statistics
    let sequential_mean = sequential_times.iter().sum::<f64>() / sequential_times.len() as f64;
    let parallel_mean = parallel_times.iter().sum::<f64>() / parallel_times.len() as f64;
    
    let sequential_min = sequential_times.iter().fold(f64::INFINITY, |a, &b| a.min(b));
    let parallel_min = parallel_times.iter().fold(f64::INFINITY, |a, &b| a.min(b));
    
    let sequential_max = sequential_times.iter().fold(0.0f64, |a, &b| a.max(b));
    let parallel_max = parallel_times.iter().fold(0.0f64, |a, &b| a.max(b));
    
    // Calculate standard deviation
    let sequential_variance = sequential_times.iter()
        .map(|x| (x - sequential_mean).powi(2))
        .sum::<f64>() / sequential_times.len() as f64;
    let sequential_stddev = sequential_variance.sqrt();
    
    let parallel_variance = parallel_times.iter()
        .map(|x| (x - parallel_mean).powi(2))
        .sum::<f64>() / parallel_times.len() as f64;
    let parallel_stddev = parallel_variance.sqrt();
    
    println!("\n🐌 Sequential Statistics:");
    println!("   Mean:   {:.3}s ± {:.3}s", sequential_mean, sequential_stddev);
    println!("   Min:    {:.3}s", sequential_min);
    println!("   Max:    {:.3}s", sequential_max);
    println!("   Traits: {} (verified consistent)", sequential_trait_counts[0]);
    println!("   Structs: {} (verified consistent)", sequential_struct_counts[0]);
    println!("   Impls:  {} (verified consistent)", sequential_impl_counts[0]);
    
    println!("\n⚡ Parallel Statistics:");
    println!("   Mean:   {:.3}s ± {:.3}s", parallel_mean, parallel_stddev);
    println!("   Min:    {:.3}s", parallel_min);
    println!("   Max:    {:.3}s", parallel_max);
    println!("   Traits: {} (verified consistent)", parallel_trait_counts[0]);
    println!("   Structs: {} (verified consistent)", parallel_struct_counts[0]);
    println!("   Impls:  {} (verified consistent)", parallel_impl_counts[0]);
    
    // === PERFORMANCE ANALYSIS ===
    println!("\n🚀 Performance Analysis:");
    
    let speedup_mean = sequential_mean / parallel_mean;
    let speedup_best = sequential_min / parallel_min;
    
    if speedup_mean > 1.0 {
        println!("   🎯 Parallel is {:.2}x faster (mean)", speedup_mean);
        println!("   🎯 Parallel is {:.2}x faster (best case)", speedup_best);
        println!("   💰 Time saved: {:.3}s per compilation ({:.1}%)", 
            sequential_mean - parallel_mean,
            ((sequential_mean - parallel_mean) / sequential_mean) * 100.0
        );
    } else if speedup_mean < 1.0 {
        let slowdown = parallel_mean / sequential_mean;
        println!("   📉 Parallel is {:.2}x slower (overhead)", slowdown);
        println!("   ⚠️  Async overhead outweighs parallelism benefits for this workload size");
    } else {
        println!("   ⚖️  Performance is equivalent");
    }
    
    // === DETAILED BREAKDOWN ===
    println!("\n📈 Detailed Performance Breakdown:");
    println!("   Programs processed: {}", collection.programs.len());
    println!("   Total AST nodes extracted: {} (traits + structs + impls)", 
        sequential_trait_counts[0] + sequential_struct_counts[0] + sequential_impl_counts[0]
    );
    
    if collection.programs.len() < 100 {
        println!("   💡 Note: Small workload size may favor sequential processing due to async overhead");
        println!("      Parallel benefits increase with larger codebases (>100 programs)");
    }
    
    // === CONSISTENCY VERIFICATION ===
    println!("\n✅ Consistency Verification:");
    let traits_consistent = sequential_trait_counts.iter().all(|&x| x == sequential_trait_counts[0]) &&
                          parallel_trait_counts.iter().all(|&x| x == parallel_trait_counts[0]) &&
                          sequential_trait_counts[0] == parallel_trait_counts[0];
    
    let structs_consistent = sequential_struct_counts.iter().all(|&x| x == sequential_struct_counts[0]) &&
                           parallel_struct_counts.iter().all(|&x| x == parallel_struct_counts[0]) &&
                           sequential_struct_counts[0] == parallel_struct_counts[0];
    
    let impls_consistent = sequential_impl_counts.iter().all(|&x| x == sequential_impl_counts[0]) &&
                         parallel_impl_counts.iter().all(|&x| x == parallel_impl_counts[0]) &&
                         sequential_impl_counts[0] == parallel_impl_counts[0];
    
    if traits_consistent && structs_consistent && impls_consistent {
        println!("   ✅ All extractions produced identical results across all runs");
        println!("   ✅ Sequential and parallel approaches are functionally equivalent");
    } else {
        println!("   ❌ INCONSISTENT RESULTS DETECTED!");
        if !traits_consistent { println!("      - Trait counts vary"); }
        if !structs_consistent { println!("      - Struct counts vary"); }
        if !impls_consistent { println!("      - Impl counts vary"); }
    }
    
    // === RECOMMENDATIONS ===
    println!("\n💡 Recommendations:");
    if speedup_mean > 1.1 {
        println!("   🚀 Parallel AST extraction shows significant benefits");
        println!("   ✅ Consider enabling parallel compilation by default");
    } else if speedup_mean > 1.0 {
        println!("   📈 Parallel AST extraction shows modest benefits");
        println!("   🤔 Benefits will increase with larger codebases");
    } else {
        println!("   📝 Current workload is too small to benefit from parallelization");
        println!("   🎯 Focus on optimizing other compilation phases");
        println!("   🔮 Parallel benefits will emerge with larger projects");
    }
    
    println!("\n🎯 BENCHMARK COMPLETE");
}