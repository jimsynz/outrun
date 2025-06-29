//! Type system core components
//!
//! This module contains the fundamental type system definitions including:
//! - Concrete type definitions for all Outrun types
//! - Trait definitions and implementation tracking
//! - Collection type utilities

pub mod collections;
pub mod concrete;
pub mod traits;

// Re-export core types
pub use collections::validate_collection_type;
pub use concrete::{ConcreteType, FunctionSignature, StructField};
pub use traits::{TraitDefinition, TraitFunction, TraitImplementation};
