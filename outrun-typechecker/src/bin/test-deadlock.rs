// Test for deadlock in SMT cache with Mutex
use std::time::Instant;

fn main() {
    println!("🔬 Testing for deadlock in SMT cache with Mutex...");
    
    // Clear cache first
    outrun_typechecker::smt::solver_pool::clear_cache();
    
    // Create a simple constraint that should work
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
    
    println!("📞 Making first cache call...");
    let start = Instant::now();
    let result1 = check_constraints_satisfiable_cached(&[constraint.clone()], &compiler_env);
    let duration1 = start.elapsed();
    
    println!("✅ First call completed in {:.3}ms: {:?}", duration1.as_secs_f64() * 1000.0, result1.is_ok());
    
    println!("📞 Making second cache call (should hit cache)...");
    let start = Instant::now();
    let result2 = check_constraints_satisfiable_cached(&[constraint.clone()], &compiler_env);
    let duration2 = start.elapsed();
    
    println!("✅ Second call completed in {:.3}ms: {:?}", duration2.as_secs_f64() * 1000.0, result2.is_ok());
    
    // Check cache stats
    let cache_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("📊 Cache stats: {}", cache_stats);
    
    if duration2 < duration1 {
        println!("🎉 Second call was faster - cache is working!");
    } else {
        println!("⚠️ Second call wasn't faster - cache might not be working properly");
    }
}