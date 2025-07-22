//! Outrun Typechecker v3
//!
//! Hindley-Milner type inference with protocol constraint solving and exhaustiveness checking.
//!
//! ## Architecture
//!
//! This typechecker follows Outrun's minimalist development philosophy by extending existing
//! parser infrastructure rather than creating parallel systems. Key components:
//!
//! - **Type Inference Engine**: HM algorithm with unification and constraint accumulation
//! - **Protocol System**: Implementation registry with orphan rule checking
//! - **Constraint Solver**: Logical constraint satisfaction for protocol bounds
//! - **Exhaustiveness Checker**: Pattern coverage analysis for case expressions and multi-head functions
//!
//! ## Integration
//!
//! The typechecker extends the existing parser AST by adding optional `TypeInfo` fields
//! to expressions, eliminating duplication while enabling seamless integration.

pub mod constraints;
pub mod error;
pub mod exhaustiveness;
pub mod inference;
pub mod registry;
pub mod types;
pub mod unification;

// Re-export public API
pub use constraints::{ConstraintSolver, ImplementationContext};
pub use error::{CompilerError, ConstraintError, TypecheckError, UnificationError};
pub use inference::InferenceContext;
pub use types::{Constraint, ProtocolId, Substitution, Type, TypeId, TypeInfo, TypeVarId};
pub use unification::Unifier;

/// A collection of parsed programs representing a complete Outrun package
pub struct Package {
    pub programs: Vec<outrun_parser::Program>,
    pub package_name: String,
}

impl Package {
    pub fn new(package_name: String) -> Self {
        Self {
            programs: Vec::new(),
            package_name,
        }
    }

    pub fn add_program(&mut self, program: outrun_parser::Program) {
        self.programs.push(program);
    }
}

/// Main entry point for type checking a complete Outrun package
#[allow(clippy::result_large_err)]
pub fn typecheck_package(package: &mut Package) -> Result<(), CompilerError> {
    let mut context = InferenceContext::new();

    // Phase 1: Collect all type and protocol definitions across all files
    for program in &package.programs {
        context.collect_definitions(program)?;
    }

    // Phase 2: Build protocol implementation registry
    for program in &package.programs {
        context.register_implementations(program)?;
    }

    // Phase 3: Type check all expressions with complete context
    for program in &mut package.programs {
        context.typecheck_program(program)?;
    }

    Ok(())
}

/// Convenience function for single-file type checking (mainly for testing)
#[allow(clippy::result_large_err)]
pub fn typecheck_program(program: &mut outrun_parser::Program) -> Result<(), CompilerError> {
    let mut package = Package::new("single_file".to_string());
    package.add_program(program.clone());

    typecheck_package(&mut package)?;

    // Replace the original program with the type-checked version
    *program = package.programs.into_iter().next().unwrap();

    Ok(())
}
