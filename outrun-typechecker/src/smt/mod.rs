//! SMT-based constraint solving for type inference
//!
//! This module implements constraint-based type inference using the Z3 SMT solver.
//! It handles complex trait resolution, generic type expansion, and function dispatch
//! resolution that cannot be solved with traditional unification algorithms.

pub mod cache;
pub mod constraints;
pub mod solver;
pub mod solver_pool;
pub mod suggestions;
pub mod translator;

// Re-export key types for convenience
pub use cache::ConstraintCache;
pub use constraints::{ConstraintPriority, ConstraintSet, SMTConstraint};
pub use solver::{SolverResult, Z3ConstraintSolver, Z3Context};
pub use solver_pool::{
    check_constraints_satisfiable, check_constraints_satisfiable_cached, clear_cache,
    get_cache_stats, solve_constraints_cached, with_solver,
};
pub use suggestions::ErrorSuggestionGenerator;
pub use translator::SMTTranslator;

/// Main entry point for SMT constraint solving
pub struct SMTConstraintSystem {
    pub z3_context: Z3Context,
    pub cache: ConstraintCache,
    pub suggestion_generator: ErrorSuggestionGenerator,
}

impl SMTConstraintSystem {
    /// Create a new SMT constraint system
    pub fn new() -> Self {
        let z3_context = Z3Context::new();
        let cache = ConstraintCache::new();
        let suggestion_generator = ErrorSuggestionGenerator::new();

        Self {
            z3_context,
            cache,
            suggestion_generator,
        }
    }

    /// Create a new solver from this system
    pub fn create_solver(&self) -> Z3ConstraintSolver {
        self.z3_context.create_solver()
    }

    /// Solve a set of constraints and return the result
    pub fn solve_constraints(
        &mut self,
        constraints: ConstraintSet,
        compiler_env: &crate::compilation::compiler_environment::CompilerEnvironment,
    ) -> SolverResult {
        // Check cache first
        if let Some(cached_result) = self.cache.get_cached_solution(&constraints.constraints) {
            return cached_result.clone();
        }

        // Create a new solver and solve constraints
        let result = {
            let mut solver = self.create_solver();
            solver
                .add_constraints(&constraints.constraints, compiler_env)
                .unwrap();
            solver.solve()
        };

        // Cache the result
        self.cache
            .cache_solution(constraints.constraints.clone(), result.clone());

        result
    }
}

impl Default for SMTConstraintSystem {
    fn default() -> Self {
        Self::new()
    }
}
