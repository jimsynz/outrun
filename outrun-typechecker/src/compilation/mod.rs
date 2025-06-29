//! Multi-program compilation infrastructure
//!
//! This module contains the decomposed components of the multi-program compiler,
//! organized by functionality for better maintainability and testing.

pub mod compiler_environment;
pub mod program_collection;
pub mod type_checking;
pub mod visitors;

// Re-export commonly used types for convenience
pub use compiler_environment::{
    AtomId, CompilerEnvironment, FunctionType, Module, ModuleKey, ModuleKind, SourceLocation,
    TypeNameId, UnifiedFunctionEntry,
};
// FunctionEntry struct has been removed - replaced by UnifiedFunctionEntry enum
// FunctionType enum kept temporarily for compatibility with function_type() method
pub use program_collection::{CompilationResult, ProgramCollection};
pub use type_checking::TypeCheckingVisitor;
pub use visitors::{ImplExtractionVisitor, StructExtractionVisitor, TraitExtractionVisitor};
