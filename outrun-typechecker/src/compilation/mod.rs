//! Multi-program compilation infrastructure
//!
//! This module contains the decomposed components of the multi-program compiler,
//! organized by functionality for better maintainability and testing.

pub mod function_registry;
pub mod program_collection;
pub mod type_checking;
pub mod visitors;

// Re-export commonly used types for convenience
pub use function_registry::{
    FunctionEntry, FunctionExtractionVisitor, FunctionRegistry, FunctionType,
};
pub use program_collection::{CompilationResult, ProgramCollection};
pub use type_checking::TypeCheckingVisitor;
pub use visitors::{ImplExtractionVisitor, StructExtractionVisitor, TraitExtractionVisitor};
