// SMT call tracing profiler to understand where SMT queries happen
use std::time::Instant;

fn main() {
    println!("🔬 SMT call tracing during core library compilation...");
    
    // Clear cache to start fresh
    outrun_typechecker::smt::solver_pool::clear_cache();
    println!("🧹 Cleared SMT cache");
    
    let start = Instant::now();
    
    // Check cache before compilation
    let initial_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("📊 Initial SMT Cache: {}", initial_stats);
    
    println!("\n⏳ Starting core library compilation...");
    let _compilation = outrun_typechecker::core_library::get_core_library_compilation();
    
    let duration = start.elapsed();
    
    // Check cache after compilation
    let final_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("\n📊 Final SMT Cache: {}", final_stats);
    
    println!("✅ Total compilation time: {:.2}s", duration.as_secs_f64());
    
    // Calculate changes
    let queries_made = final_stats.total_queries - initial_stats.total_queries;
    let hits_gained = final_stats.cache_hits - initial_stats.cache_hits;
    let misses_gained = final_stats.cache_misses - initial_stats.cache_misses;
    
    println!("\n🔍 SMT Activity Analysis:");
    println!("  Queries made: {}", queries_made);
    println!("  Cache hits: {}", hits_gained);
    println!("  Cache misses: {}", misses_gained);
    
    if queries_made == 0 {
        println!("  ⚠️  Still zero SMT queries! Let's investigate...");
        
        // Try a simple test to make sure SMT is working
        println!("\n🧪 Testing SMT system directly...");
        test_smt_system_directly();
    } else {
        println!("  ✅ SMT queries were made during compilation");
        if hits_gained > 0 {
            println!("  📈 Cache efficiency: {:.1}%", (hits_gained as f64 / queries_made as f64) * 100.0);
        }
    }
}

fn test_smt_system_directly() {
    use outrun_typechecker::compilation::compiler_environment::CompilerEnvironment;
    use outrun_typechecker::smt::constraints::SMTConstraint;
    use outrun_typechecker::smt::solver_pool::check_constraints_satisfiable_cached;
    use outrun_typechecker::unification::StructuredType;
    use std::collections::HashMap;
    use std::sync::{Arc, RwLock};
    
    let storage = Arc::new(RwLock::new(HashMap::new()));
    let type_id = outrun_typechecker::compilation::compiler_environment::TypeNameId::new(42, storage.clone());
    let trait_id = outrun_typechecker::compilation::compiler_environment::TypeNameId::new(43, storage.clone());
    
    let constraint = SMTConstraint::TraitImplemented {
        impl_type: StructuredType::Simple(type_id),
        trait_type: StructuredType::Simple(trait_id),
    };
    
    let compiler_env = CompilerEnvironment::new();
    
    println!("  🔧 Making direct SMT call...");
    let before_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    
    match check_constraints_satisfiable_cached(&[constraint], &compiler_env) {
        Ok(result) => {
            println!("  ✅ Direct SMT call succeeded: {}", result);
            
            let after_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
            let queries_made = after_stats.total_queries - before_stats.total_queries;
            println!("  📊 Queries made by direct call: {}", queries_made);
            
            if queries_made > 0 {
                println!("  ✅ SMT system is working - compilation must be using different code paths");
            } else {
                println!("  ⚠️  Even direct SMT calls aren't hitting the cache - there's a bug!");
            }
        }
        Err(e) => {
            println!("  ❌ Direct SMT call failed: {:?}", e);
        }
    }
}