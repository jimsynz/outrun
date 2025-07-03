// Debug SMT calls with manual tracing
use std::time::Instant;
use std::sync::atomic::{AtomicUsize, Ordering};

static SMT_CALL_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn main() {
    println!("🔬 Debugging SMT calls with manual tracing...");
    
    // Reset counter
    SMT_CALL_COUNTER.store(0, Ordering::SeqCst);
    
    println!("📊 Initial SMT calls: {}", SMT_CALL_COUNTER.load(Ordering::SeqCst));
    
    // Patch the SMT solver to count calls
    test_smt_call_counting();
    
    println!("\n⏳ Starting core library compilation...");
    let start = Instant::now();
    let _compilation = outrun_typechecker::core_library::get_core_library_compilation();
    let duration = start.elapsed();
    
    println!("✅ Compilation took: {:.2}s", duration.as_secs_f64());
    println!("📊 Final SMT calls: {}", SMT_CALL_COUNTER.load(Ordering::SeqCst));
    
    // Let's also test the cache directly to see if it's working at all
    println!("\n🧪 Testing cache behavior directly...");
    test_cache_behavior();
}

fn test_smt_call_counting() {
    println!("🧪 Testing SMT call counting...");
    
    // Make a few direct calls to see if we can detect them
    for i in 0..3 {
        println!("  📞 Making test SMT call {}...", i + 1);
        
        let before = SMT_CALL_COUNTER.load(Ordering::SeqCst);
        make_test_smt_call();
        let after = SMT_CALL_COUNTER.load(Ordering::SeqCst);
        
        println!("    Counter before: {}, after: {}", before, after);
    }
}

fn make_test_smt_call() {
    use outrun_typechecker::compilation::compiler_environment::CompilerEnvironment;
    use outrun_typechecker::smt::constraints::SMTConstraint;
    use outrun_typechecker::smt::solver_pool::check_constraints_satisfiable_cached;
    use outrun_typechecker::unification::StructuredType;
    use std::collections::HashMap;
    use std::sync::{Arc, RwLock};
    
    // Increment our counter
    SMT_CALL_COUNTER.fetch_add(1, Ordering::SeqCst);
    
    let storage = Arc::new(RwLock::new(HashMap::new()));
    let type_id = outrun_typechecker::compilation::compiler_environment::TypeNameId::new(42, storage.clone());
    let trait_id = outrun_typechecker::compilation::compiler_environment::TypeNameId::new(43, storage.clone());
    
    let constraint = SMTConstraint::TraitImplemented {
        impl_type: StructuredType::Simple(type_id),
        trait_type: StructuredType::Simple(trait_id),
    };
    
    let compiler_env = CompilerEnvironment::new();
    let _result = check_constraints_satisfiable_cached(&[constraint], &compiler_env);
}

fn test_cache_behavior() {
    use outrun_typechecker::compilation::compiler_environment::CompilerEnvironment;
    use outrun_typechecker::smt::constraints::SMTConstraint;
    use outrun_typechecker::smt::solver_pool::check_constraints_satisfiable_cached;
    use outrun_typechecker::unification::StructuredType;
    use std::collections::HashMap;
    use std::sync::{Arc, RwLock};
    
    println!("  🔧 Testing cache read/write behavior...");
    
    let storage = Arc::new(RwLock::new(HashMap::new()));
    let type_id = outrun_typechecker::compilation::compiler_environment::TypeNameId::new(99, storage.clone());
    let trait_id = outrun_typechecker::compilation::compiler_environment::TypeNameId::new(100, storage.clone());
    
    let constraint = SMTConstraint::TraitImplemented {
        impl_type: StructuredType::Simple(type_id),
        trait_type: StructuredType::Simple(trait_id),
    };
    
    let compiler_env = CompilerEnvironment::new();
    
    // First call should be a cache miss
    println!("    📞 First call (should be cache miss)...");
    let start1 = Instant::now();
    let result1 = check_constraints_satisfiable_cached(&[constraint.clone()], &compiler_env);
    let duration1 = start1.elapsed();
    println!("      Result: {:?}, Time: {:.3}ms", result1, duration1.as_secs_f64() * 1000.0);
    
    // Check cache stats instead of direct access
    let cache_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("      Cache stats after first call: {}", cache_stats);
    
    // Second call should be a cache hit (if cache is working)
    println!("    📞 Second call (should be cache hit)...");
    let start2 = Instant::now();
    let result2 = check_constraints_satisfiable_cached(&[constraint], &compiler_env);
    let duration2 = start2.elapsed();
    println!("      Result: {:?}, Time: {:.3}ms", result2, duration2.as_secs_f64() * 1000.0);
    
    // Compare timings
    if duration2 < duration1 {
        println!("      ✅ Second call was faster - cache is working!");
        println!("      📈 Speedup: {:.1}x", duration1.as_secs_f64() / duration2.as_secs_f64());
    } else {
        println!("      ⚠️  Second call wasn't faster - cache might not be working");
    }
}