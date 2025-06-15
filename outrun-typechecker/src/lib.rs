//! Outrun Type Checker
//!
//! Static type checker for the Outrun programming language that validates trait constraints,
//! function signatures, and expressions at compile time, generating efficient dispatch tables
//! for the interpreter.

pub mod checker;
pub mod dispatch;
pub mod error;
pub mod exhaustiveness;
pub mod types;

#[cfg(test)]
mod tests;

// Re-export core types and functions
pub use checker::{TypeChecker, TypeContext, TypedProgram};
pub use dispatch::DispatchTable;
pub use error::{TypeError, TypeResult};
pub use exhaustiveness::{ExhaustivenessAnalyzer, ExhaustivenessResult, MissingPattern};
pub use types::{AtomId, ConcreteType, TraitId, TypeId, TypeInterner};

// Main type checking API
use outrun_parser::Program;

/// Type check a parsed program and return a typed AST with dispatch tables
///
/// # Arguments
/// * `program` - The parsed program from outrun-parser
///
/// # Returns
/// * `Ok(TypedProgram)` - Successfully typed program with dispatch information
/// * `Err(Vec<TypeError>)` - Type errors encountered during checking
///
/// # Example
/// ```rust
/// use outrun_parser::parse_program;
/// use outrun_typechecker::typecheck_program;
///
/// let source = r#"
///     def add(x: Integer, y: Integer): Integer {
///         x + y
///     }
/// "#;
///
/// let program = parse_program(source).unwrap();
/// let typed_program = typecheck_program(program);
/// // typed_program is Result<TypedProgram, Vec<TypeError>>
/// ```
pub fn typecheck_program(program: Program) -> Result<TypedProgram, Vec<TypeError>> {
    let mut checker = TypeChecker::new();
    checker.check_program(&program)
}

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const NAME: &str = env!("CARGO_PKG_NAME");
