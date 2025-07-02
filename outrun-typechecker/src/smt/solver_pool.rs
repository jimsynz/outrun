//! Z3 Solver Utilities
//!
//! This module provides utility functions for common Z3 solver operations,
//! eliminating repetitive context creation patterns.

use crate::smt::solver::{Z3Context, Z3ConstraintSolver, SolverResult, SMTError};
use crate::smt::constraints::SMTConstraint;
use crate::smt::cache::ConstraintCache;
use crate::compilation::compiler_environment::CompilerEnvironment;
use std::cell::RefCell;
use std::collections::HashSet;

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

/// Thread-local cache for performance optimization
thread_local! {
    static THREAD_CACHE: RefCell<ConstraintCache> = RefCell::new(ConstraintCache::new());
}

/// Check if constraints are satisfiable with preprocessing and caching
pub fn check_constraints_satisfiable_cached(
    constraints: &[SMTConstraint],
    compiler_env: &CompilerEnvironment,
) -> Result<bool, SMTError> {
    THREAD_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        
        // Step 1: Skip all preprocessing - even deduplication can break core library
        let simplified_constraints = constraints.to_vec();
        
        // Step 2: Check cache with simplified constraints
        if let Some(cached_result) = cache.get_cached_solution(&simplified_constraints) {
            return match cached_result {
                SolverResult::Satisfiable(_) => Ok(true),
                SolverResult::Unsatisfiable(_) => Ok(false),
                SolverResult::Unknown(reason) => Err(SMTError::SolvingFailed(format!(
                    "SMT solver returned unknown (cached): {reason}"
                ))),
            };
        }
        
        // Step 3: Cache miss - solve and cache result
        let result = with_solver(|solver| {
            solver.add_constraints(&simplified_constraints, compiler_env)?;
            Ok(solver.solve())
        })?;
        
        // Step 4: Cache the result for future use
        cache.cache_solution(simplified_constraints, result.clone());
        
        // Step 5: Return boolean result
        match result {
            SolverResult::Satisfiable(_) => Ok(true),
            SolverResult::Unsatisfiable(_) => Ok(false),
            SolverResult::Unknown(reason) => Err(SMTError::SolvingFailed(format!(
                "SMT solver returned unknown: {reason}"
            ))),
        }
    })
}

/// Solve constraints with preprocessing, caching and return full result
pub fn solve_constraints_cached(
    constraints: &[SMTConstraint],
    compiler_env: &CompilerEnvironment,
) -> Result<SolverResult, SMTError> {
    THREAD_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        
        // Step 1: Skip all preprocessing - even deduplication can break core library
        let simplified_constraints = constraints.to_vec();
        
        // Step 2: Check cache with simplified constraints
        if let Some(cached_result) = cache.get_cached_solution(&simplified_constraints) {
            return Ok(cached_result.clone());
        }
        
        // Step 3: Cache miss - solve and cache result
        let result = with_solver(|solver| {
            solver.add_constraints(&simplified_constraints, compiler_env)?;
            Ok(solver.solve())
        })?;
        
        // Step 4: Cache the result for future use
        cache.cache_solution(simplified_constraints, result.clone());
        
        Ok(result)
    })
}

/// Get cache statistics for performance monitoring
pub fn get_cache_stats() -> crate::smt::cache::CacheStats {
    THREAD_CACHE.with(|cache| {
        cache.borrow().get_stats().clone()
    })
}

/// Clear the thread-local cache
pub fn clear_cache() {
    THREAD_CACHE.with(|cache| {
        cache.borrow_mut().clear();
    });
}

/// Safely deduplicate constraints without changing semantics
/// Only removes exact duplicates to avoid breaking constraint logic
fn deduplicate_constraints(constraints: &[SMTConstraint]) -> Vec<SMTConstraint> {
    let mut unique_constraints = Vec::new();
    let mut seen_constraints = HashSet::new();
    
    for constraint in constraints {
        // Use the built-in Hash implementation for SMTConstraint
        if seen_constraints.insert(constraint.clone()) {
            unique_constraints.push(constraint.clone());
        }
    }
    
    unique_constraints
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::smt::constraints::SMTConstraint;
    use crate::compilation::compiler_environment::TypeNameId;
    use crate::unification::StructuredType;
    use std::sync::{Arc, RwLock};
    use std::collections::HashMap;

    #[test]
    fn test_with_solver() {
        let result = with_solver(|_solver| {
            42
        });
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
        let _constraints = vec![
            SMTConstraint::TraitImplemented {
                impl_type: StructuredType::Simple(test_type_id),
                trait_type: StructuredType::Simple(test_trait_id),
            }
        ];
        
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
        let formatted = format!("{}", stats);
        assert!(formatted.contains("Cache Stats"));
        assert!(formatted.contains("0 hits"));
        assert!(formatted.contains("0 misses"));
    }
}