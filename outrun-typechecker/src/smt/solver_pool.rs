//! Z3 Solver Utilities
//!
//! This module provides utility functions for common Z3 solver operations,
//! eliminating repetitive context creation patterns.

use crate::smt::solver::{Z3Context, Z3ConstraintSolver, SolverResult, SMTError};
use crate::smt::constraints::SMTConstraint;
use crate::compilation::compiler_environment::CompilerEnvironment;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_with_solver() {
        let result = with_solver(|_solver| {
            42
        });
        assert_eq!(result, 42);
    }
}