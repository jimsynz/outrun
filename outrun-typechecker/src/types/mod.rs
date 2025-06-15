//! Type system core components
//!
//! This module contains the fundamental type system definitions including:
//! - Type interning system for fast equality checks
//! - Concrete type definitions for all Outrun types
//! - Trait definitions and implementation tracking
//! - Collection type utilities

pub mod collections;
pub mod concrete;
pub mod interning;
pub mod introspection;
pub mod traits;

// Re-export core types
pub use collections::{validate_collection_type, CollectionType};
pub use concrete::{ConcreteType, FunctionSignature, StructField};
pub use interning::{AtomId, TraitId, TypeId, TypeInterner};
pub use introspection::{
    IntrospectionRegistry, StructFieldInfo, StructTypeInfo, TraitFunctionInfo, TraitTypeInfo,
    TypeKind,
};
pub use traits::{TraitDefinition, TraitFunction, TraitImplementation};
