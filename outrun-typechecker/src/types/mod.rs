//! Type system core components
//!
//! This module contains the fundamental type system definitions including:
//! - Concrete type definitions for all Outrun types
//! - Protocol definitions and implementation tracking
//! - Collection type utilities

pub mod collections;
pub mod concrete;
pub mod protocols;

// Re-export core types
pub use collections::validate_collection_type;
pub use concrete::{ConcreteType, FunctionSignature, StructField};
pub use protocols::{ProtocolDefinition, ProtocolFunction, ProtocolImplementation};
