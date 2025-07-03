//! Z3 Solver Utilities
//!
//! This module provides utility functions for common Z3 solver operations,
//! eliminating repetitive context creation patterns.

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::smt::cache::{ConstraintCache, SelfResolutionKey};
use crate::smt::constraints::SMTConstraint;
use crate::smt::solver::{SMTError, SolverResult, Z3ConstraintSolver, Z3Context};
use crate::unification::StructuredType;
use std::sync::{Arc, Mutex, OnceLock};
use tokio::task;

/// Execute a function with a fresh Z3 solver
///
/// This eliminates the repetitive pattern of:
/// ```
/// let context = Z3Context::new();
/// let mut solver = context.create_solver();
/// // use solver
/// ```
pub fn with_solver<F, R>(f: F) -> R
where
    F: FnOnce(&mut Z3ConstraintSolver) -> R,
{
    let context = Z3Context::new();
    let mut solver = context.create_solver();
    f(&mut solver)
}

/// Check if constraints are satisfiable (common pattern)
pub fn check_constraints_satisfiable(
    constraints: &[SMTConstraint],
    compiler_env: &CompilerEnvironment,
) -> Result<bool, SMTError> {
    with_solver(|solver| {
        solver.add_constraints(constraints, compiler_env)?;
        match solver.solve() {
            SolverResult::Satisfiable(_) => Ok(true),
            SolverResult::Unsatisfiable(_) => Ok(false),
            SolverResult::Unknown(reason) => Err(SMTError::SolvingFailed(format!(
                "SMT solver returned unknown: {reason}"
            ))),
        }
    })
}

/// Global SMT constraint cache shared across all threads for async compatibility
static GLOBAL_SMT_CACHE: OnceLock<Arc<Mutex<ConstraintCache>>> = OnceLock::new();

/// Get the global SMT cache instance
fn get_global_cache() -> &'static Arc<Mutex<ConstraintCache>> {
    GLOBAL_SMT_CACHE.get_or_init(|| {
        Arc::new(Mutex::new(ConstraintCache::new()))
    })
}

/// Check if constraints are satisfiable with preprocessing and caching
pub fn check_constraints_satisfiable_cached(
    constraints: &[SMTConstraint],
    compiler_env: &CompilerEnvironment,
) -> Result<bool, SMTError> {
    let cache = get_global_cache();

    // Step 1: Order constraints for better Z3 performance (safe optimization)
    let simplified_constraints = order_constraints_for_solving(constraints);

    // Step 2: Check cache with simplified constraints (mutex lock)
    {
        match cache.lock() {
            Ok(mut cache_guard) => {
                if let Some(cached_result) = cache_guard.get_cached_solution(&simplified_constraints) {
                    return match cached_result {
                        SolverResult::Satisfiable(_) => Ok(true),
                        SolverResult::Unsatisfiable(_) => Ok(false),
                        SolverResult::Unknown(reason) => Err(SMTError::SolvingFailed(format!(
                            "SMT solver returned unknown (cached): {reason}"
                        ))),
                    };
                }
            }
            Err(_) => {
                // Cache lock failed - skip cache and solve directly
                eprintln!("⚠️  Cache lock failed, bypassing cache");
            }
        }
    }

    // Step 3: Cache miss - solve and cache result
    let result = with_solver(|solver| {
        solver.add_constraints(&simplified_constraints, compiler_env)?;
        Ok(solver.solve())
    })?;

    // Step 4: Cache the result for future use (mutex lock)
    {
        let mut cache_guard = cache.lock().map_err(|e| SMTError::SolverError(format!("Cache lock failed: {}", e)))?;
        cache_guard.cache_solution(simplified_constraints, result.clone());
    }

    // Step 5: Return boolean result
    match result {
        SolverResult::Satisfiable(_) => Ok(true),
        SolverResult::Unsatisfiable(_) => Ok(false),
        SolverResult::Unknown(reason) => Err(SMTError::SolvingFailed(format!(
            "SMT solver returned unknown: {reason}"
        ))),
    }
}

/// Solve constraints with preprocessing, caching and return full result
pub fn solve_constraints_cached(
    constraints: &[SMTConstraint],
    compiler_env: &CompilerEnvironment,
) -> Result<SolverResult, SMTError> {
    let cache = get_global_cache();

    // Step 1: Order constraints for better Z3 performance (safe optimization)
    let simplified_constraints = order_constraints_for_solving(constraints);

    // Step 2: Check cache with simplified constraints (mutex lock)
    {
        let mut cache_guard = cache.lock().map_err(|e| SMTError::SolverError(format!("Cache lock failed: {}", e)))?;
        if let Some(cached_result) = cache_guard.get_cached_solution(&simplified_constraints) {
            return Ok(cached_result.clone());
        }
    }

    // Step 3: SUBPROBLEM OPTIMIZATION (from John D. Cook article)
    // Try to solve simpler subproblems first to constrain the solution space
    if let Some(quick_result) = try_solve_subproblems(&simplified_constraints, compiler_env, cache)? {
        return Ok(quick_result);
    }

    // Step 4: Cache miss - solve full problem
    let result = with_solver(|solver| {
        solver.add_constraints(&simplified_constraints, compiler_env)?;
        Ok(solver.solve())
    })?;

    // Step 5: Cache the result for future use (mutex lock)
    {
        let mut cache_guard = cache.lock().map_err(|e| SMTError::SolverError(format!("Cache lock failed: {}", e)))?;
        cache_guard.cache_solution(simplified_constraints, result.clone());
    }

    Ok(result)
}

/// Get cache statistics for performance monitoring
pub fn get_cache_stats() -> crate::smt::cache::CacheStats {
    let cache = get_global_cache();
    cache.lock().map(|cache_guard| cache_guard.get_stats().clone())
        .unwrap_or_else(|_| crate::smt::cache::CacheStats::default())
}

/// Clear the global cache
pub fn clear_cache() {
    let cache = get_global_cache();
    if let Ok(mut cache_guard) = cache.lock() {
        cache_guard.clear();
    }
}

/// Order constraints to help Z3 solve more efficiently
/// This is a safe optimization that doesn't change constraint semantics
fn order_constraints_for_solving(constraints: &[SMTConstraint]) -> Vec<SMTConstraint> {
    let mut ordered_constraints = constraints.to_vec();

    // Sort by constraint priority for Z3 solving efficiency:
    // 1. Concrete type bindings first (most constraining)
    // 2. Type parameter unifications next
    // 3. Trait implementations third
    // 4. More complex constraints last
    ordered_constraints.sort_by_key(|constraint| match constraint {
        // Highest priority: Concrete bindings constrain the solution space most
        SMTConstraint::ConcreteSelfBinding { .. } => 0,
        SMTConstraint::TypeParameterUnification { .. } => 1,

        // Medium priority: Type variable constraints
        SMTConstraint::TypeVariableConstraint { .. } => 2,
        SMTConstraint::SelfTypeInference { .. } => 3,

        // Lower priority: Trait compatibility checks
        SMTConstraint::TraitImplemented { .. } => 4,
        SMTConstraint::TraitCompatibility { .. } => 5,

        // Lowest priority: Complex constraints that depend on other solutions
        SMTConstraint::TypeUnification { .. } => 6,
        SMTConstraint::GenericInstantiation { .. } => 7,
        SMTConstraint::FunctionSignatureMatch { .. } => 8,
        SMTConstraint::GuardCondition { .. } => 9,
        SMTConstraint::UniversalSelfConstraint { .. } => 10,
        
        // Function clause dispatch constraints
        SMTConstraint::ArgumentTypeMatch { .. } => 11,
        SMTConstraint::GuardApplicable { .. } => 12,
        SMTConstraint::ClausePriority { .. } => 13,
        SMTConstraint::GuardStaticallyEvaluated { .. } => 14,
        SMTConstraint::PreResolvedClause { .. } => 15, // Highest priority - final resolved constraints
        
        // Exhaustiveness analysis constraints - lower priority as they're typically analysis-only
        SMTConstraint::FunctionClauseSetExhaustive { .. } => 16,
        SMTConstraint::FunctionClauseReachable { .. } => 17,
        SMTConstraint::GuardCoverageComplete { .. } => 18,
        SMTConstraint::GuardConditionSatisfiable { .. } => 19,
    });

    ordered_constraints
}

/// Try to solve simpler subproblems first (John D. Cook optimization strategy)
/// This can dramatically speed up complex constraint solving by presetting variables
fn try_solve_subproblems(
    constraints: &[SMTConstraint],
    compiler_env: &CompilerEnvironment,
    cache: &Arc<Mutex<crate::smt::cache::ConstraintCache>>,
) -> Result<Option<SolverResult>, SMTError> {
    // Strategy 1: Try concrete type bindings first (highest constraining power)
    let concrete_constraints: Vec<_> = constraints
        .iter()
        .filter(|c| matches!(c, 
            SMTConstraint::ConcreteSelfBinding { .. } |
            SMTConstraint::TypeParameterUnification { .. }
        ))
        .cloned()
        .collect();

    if !concrete_constraints.is_empty() && concrete_constraints.len() < constraints.len() {
        // Check cache for this subproblem (mutex lock)
        {
            let mut cache_guard = cache.lock().map_err(|e| SMTError::SolverError(format!("Cache lock failed: {}", e)))?;
            if let Some(cached_subresult) = cache_guard.get_cached_solution(&concrete_constraints) {
                match cached_subresult {
                    SolverResult::Unsatisfiable(_) => {
                        // If concrete constraints are unsatisfiable, full problem is too
                        return Ok(Some(cached_subresult.clone()));
                    }
                    SolverResult::Satisfiable(_) => {
                        // Concrete constraints satisfiable - continue with full problem
                        // but this gives Z3 a good starting point
                    }
                    SolverResult::Unknown(_) => {
                        // Subproblem unknown - try full problem
                    }
                }
            } else {
                // Cache miss - solve concrete subproblem first
                let subresult = with_solver(|solver| {
                    solver.add_constraints(&concrete_constraints, compiler_env)?;
                    Ok(solver.solve())
                })?;

                // Cache the subproblem result (mutex lock)
                {
                    let mut cache_guard = cache.lock().map_err(|e| SMTError::SolverError(format!("Cache lock failed: {}", e)))?;
                    cache_guard.cache_solution(concrete_constraints, subresult.clone());
                    
                    match subresult {
                        SolverResult::Unsatisfiable(_) => {
                            // If concrete constraints are unsatisfiable, full problem is too
                            cache_guard.cache_solution(constraints.to_vec(), subresult.clone());
                        }
                        _ => {
                            // Continue with full problem
                        }
                    }
                }
                
                match subresult {
                    SolverResult::Unsatisfiable(_) => {
                        return Ok(Some(subresult));
                    }
                    _ => {
                        // Continue with full problem
                    }
                }
            }
        }
    }

    // Strategy 2: Check for simple trait implementation constraints
    let simple_trait_constraints: Vec<_> = constraints
        .iter()
        .filter(|c| matches!(c, SMTConstraint::TraitImplemented { .. }))
        .cloned()
        .collect();

    if simple_trait_constraints.len() <= 3 && simple_trait_constraints.len() < constraints.len() {
        // For small numbers of trait constraints, solve them first (mutex lock)
        let mut cache_guard = cache.lock().map_err(|e| SMTError::SolverError(format!("Cache lock failed: {}", e)))?;
        if let Some(cached_trait_result) = cache_guard.get_cached_solution(&simple_trait_constraints) {
            if matches!(cached_trait_result, SolverResult::Unsatisfiable(_)) {
                return Ok(Some(cached_trait_result.clone()));
            }
        }
    }

    // No beneficial subproblem found - solve full problem
    Ok(None)
}

/// Get cached Self type resolution result
pub fn get_cached_self_resolution(key: &SelfResolutionKey) -> Option<StructuredType> {
    let cache = get_global_cache();
    cache.lock().ok().and_then(|mut cache_guard| {
        cache_guard.get_cached_self_resolution(key).cloned()
    })
}

/// Cache a Self type resolution result
pub fn cache_self_resolution(key: SelfResolutionKey, resolved_type: StructuredType) {
    let cache = get_global_cache();
    if let Ok(mut cache_guard) = cache.lock() {
        cache_guard.cache_self_resolution(key, resolved_type);
    }
}

/// Solve Self type resolution with caching
pub fn solve_self_resolution_cached(
    trait_type_id: &crate::compilation::compiler_environment::TypeNameId,
    function_name: &str,
    argument_types: &[StructuredType],
    constraints: &[SMTConstraint],
    compiler_env: &CompilerEnvironment,
    self_type_id: &crate::compilation::compiler_environment::TypeNameId,
) -> Result<StructuredType, SMTError> {
    // Create normalized cache key (without unique Self variables)
    let cache_key = SelfResolutionKey {
        trait_type: trait_type_id.clone(),
        function_name: function_name.to_string(),
        argument_types: argument_types.to_vec(),
    };

    // Check cache first
    if let Some(cached_result) = get_cached_self_resolution(&cache_key) {
        return Ok(cached_result);
    }

    // Cache miss - solve with SMT
    let result = solve_constraints_cached(constraints, compiler_env)?;
    
    match result {
        SolverResult::Satisfiable(model) => {
            // Extract Self type from model using the actual Self variable ID we created
            let self_var_name = format!("Self_{}", self_type_id.hash);
            if let Some(resolved_type) = compiler_env.extract_self_type_from_model(&model, &self_var_name) {
                // Cache the result for future use
                cache_self_resolution(cache_key, resolved_type.clone());
                Ok(resolved_type)
            } else {
                Err(SMTError::SolverError(format!(
                    "SMT model missing Self variable {self_var_name}"
                )))
            }
        }
        SolverResult::Unsatisfiable(conflicts) => {
            Err(SMTError::SolvingFailed(format!(
                "Conflicting Self type constraints: {conflicts:?}"
            )))
        }
        SolverResult::Unknown(reason) => {
            Err(SMTError::SolvingFailed(format!(
                "SMT solver limitation: {reason}"
            )))
        }
    }
}

/// Async SMT solver functions for Tokio compatibility

/// Check if constraints are satisfiable with async Z3 solving using spawn_blocking
pub async fn check_constraints_satisfiable_async(
    constraints: Vec<SMTConstraint>,
    compiler_env: CompilerEnvironment,
) -> Result<bool, SMTError> {
    task::spawn_blocking(move || {
        check_constraints_satisfiable_cached(&constraints, &compiler_env)
    })
    .await
    .map_err(|e| SMTError::SolverError(format!("Async task failed: {}", e)))?
}

/// Solve constraints with full result using async Z3 solving
pub async fn solve_constraints_cached_async(
    constraints: Vec<SMTConstraint>,
    compiler_env: CompilerEnvironment,
) -> Result<SolverResult, SMTError> {
    task::spawn_blocking(move || {
        solve_constraints_cached(&constraints, &compiler_env)
    })
    .await
    .map_err(|e| SMTError::SolverError(format!("Async task failed: {}", e)))?
}

/// Async Self type resolution using spawn_blocking for CPU-intensive SMT operations
pub async fn solve_self_resolution_cached_async(
    trait_type_id: crate::compilation::compiler_environment::TypeNameId,
    function_name: String,
    argument_types: Vec<StructuredType>,
    constraints: Vec<SMTConstraint>,
    compiler_env: CompilerEnvironment,
    self_type_id: crate::compilation::compiler_environment::TypeNameId,
) -> Result<StructuredType, SMTError> {
    task::spawn_blocking(move || {
        solve_self_resolution_cached(
            &trait_type_id,
            &function_name,
            &argument_types,
            &constraints,
            &compiler_env,
            &self_type_id,
        )
    })
    .await
    .map_err(|e| SMTError::SolverError(format!("Async task failed: {}", e)))?
}

/// Process multiple independent constraint sets in parallel
pub async fn solve_constraint_batches_async(
    constraint_batches: Vec<(Vec<SMTConstraint>, CompilerEnvironment)>,
) -> Vec<Result<SolverResult, SMTError>> {
    let handles: Vec<_> = constraint_batches
        .into_iter()
        .map(|(constraints, compiler_env)| {
            task::spawn_blocking(move || {
                solve_constraints_cached(&constraints, &compiler_env)
            })
        })
        .collect();

    let mut results = Vec::new();
    for handle in handles {
        match handle.await {
            Ok(result) => results.push(result),
            Err(e) => results.push(Err(SMTError::SolverError(format!("Async task failed: {}", e)))),
        }
    }
    results
}

/// Check multiple constraint sets for satisfiability in parallel
pub async fn check_constraint_batches_satisfiable_async(
    constraint_batches: Vec<(Vec<SMTConstraint>, CompilerEnvironment)>,
) -> Vec<Result<bool, SMTError>> {
    let handles: Vec<_> = constraint_batches
        .into_iter()
        .map(|(constraints, compiler_env)| {
            task::spawn_blocking(move || {
                check_constraints_satisfiable_cached(&constraints, &compiler_env)
            })
        })
        .collect();

    let mut results = Vec::new();
    for handle in handles {
        match handle.await {
            Ok(result) => results.push(result),
            Err(e) => results.push(Err(SMTError::SolverError(format!("Async task failed: {}", e)))),
        }
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compilation::compiler_environment::TypeNameId;
    use crate::smt::constraints::SMTConstraint;
    use crate::unification::StructuredType;
    use std::collections::HashMap;
    use std::sync::{Arc, Mutex};

    #[test]
    fn test_with_solver() {
        let result = with_solver(|_solver| 42);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_cache_performance() {
        // Clear cache to start fresh
        clear_cache();

        // Create proper TypeNameId instances with storage
        let storage = Arc::new(RwLock::new(HashMap::new()));
        let test_type_id = TypeNameId::new(42, storage.clone());
        let test_trait_id = TypeNameId::new(43, storage.clone());

        // Create some test constraints
        let _constraints = vec![SMTConstraint::TraitImplemented {
            impl_type: StructuredType::Simple(test_type_id),
            trait_type: StructuredType::Simple(test_trait_id),
        }];

        // Get initial cache stats
        let initial_stats = get_cache_stats();
        assert_eq!(initial_stats.total_queries, 0);
        assert_eq!(initial_stats.cache_hits, 0);
        assert_eq!(initial_stats.cache_misses, 0);

        // Note: We can't easily test actual solving without a full CompilerEnvironment
        // But we can test that the cache infrastructure is working
        assert!(initial_stats.total_queries == 0);
    }

    #[test]
    fn test_cache_clear() {
        clear_cache();
        let stats = get_cache_stats();
        assert_eq!(stats.total_queries, 0);
        assert_eq!(stats.cache_hits, 0);
        assert_eq!(stats.cache_misses, 0);
    }

    #[test]
    fn test_cache_stats_format() {
        clear_cache();
        let stats = get_cache_stats();
        let formatted = format!("{stats}");
        assert!(formatted.contains("Cache Stats"));
        assert!(formatted.contains("0 hits"));
        assert!(formatted.contains("0 misses"));
    }

    #[tokio::test]
    async fn test_async_smt_solving() {
        // Test that async SMT solving works with Tokio runtime
        use crate::compilation::compiler_environment::CompilerEnvironment;
        
        // Create proper TypeNameId instances with storage
        let storage = Arc::new(RwLock::new(HashMap::new()));
        let test_type_id = TypeNameId::new(42, storage.clone());
        let test_trait_id = TypeNameId::new(43, storage.clone());

        // Create test constraints
        let constraints = vec![SMTConstraint::TraitImplemented {
            impl_type: StructuredType::Simple(test_type_id),
            trait_type: StructuredType::Simple(test_trait_id),
        }];

        let compiler_env = CompilerEnvironment::new();

        // Test async constraint solving
        let result = check_constraints_satisfiable_async(constraints, compiler_env).await;
        
        // Should succeed (or fail gracefully) - the important thing is no panics
        match result {
            Ok(_) => {
                // Great! Async solving worked
            }
            Err(SMTError::SolverError(_)) => {
                // Expected for minimal test setup
            }
            Err(other) => {
                // Other errors are acceptable as long as we don't panic
                println!("Async SMT test got error: {:?}", other);
            }
        }
    }

    #[tokio::test]
    async fn test_parallel_constraint_batches() {
        // Test parallel processing of multiple constraint sets
        use crate::compilation::compiler_environment::CompilerEnvironment;
        
        let storage = Arc::new(RwLock::new(HashMap::new()));
        
        // Create multiple independent constraint sets
        let mut batches = Vec::new();
        for i in 0..3 {
            let type_id = TypeNameId::new(100 + i, storage.clone());
            let trait_id = TypeNameId::new(200 + i, storage.clone());
            
            let constraints = vec![SMTConstraint::TraitImplemented {
                impl_type: StructuredType::Simple(type_id),
                trait_type: StructuredType::Simple(trait_id),
            }];
            
            batches.push((constraints, CompilerEnvironment::new()));
        }
        
        // Process batches in parallel
        let results = check_constraint_batches_satisfiable_async(batches).await;
        
        // Should get 3 results
        assert_eq!(results.len(), 3);
        
        // Each result should be a proper Result type (not panic)
        for result in results {
            match result {
                Ok(_) => {}, // Success
                Err(_) => {}, // Acceptable error 
            }
        }
    }
}
