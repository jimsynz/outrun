//! Type introspection system
//!
//! This module implements the Type trait system that enables runtime type introspection
//! and metaprogramming capabilities. It follows Outrun's "everything is traits" philosophy
//! by treating types as first-class values that implement the Type trait.

use crate::types::{AtomId, TypeId};
use outrun_parser::Span;

/// The core Type trait that all type values implement
/// This enables uniform introspection across all type kinds
#[derive(Debug, Clone, PartialEq)]
pub struct TypeTrait {
    pub name: String,
    pub module: String,
    pub functions: Vec<TypeTraitFunction>,
}

/// Functions available on the Type trait
#[derive(Debug, Clone, PartialEq)]
pub struct TypeTraitFunction {
    pub name: String,
    pub signature: TypeFunctionSignature,
}

/// Function signatures for Type trait methods
#[derive(Debug, Clone, PartialEq)]
pub enum TypeFunctionSignature {
    /// name(self: Self): String
    Name,
    /// module(self: Self): String
    Module,
    /// kind(self: Self): TypeKind
    Kind,
    /// implements_trait?(self: Self, trait_name: String): Boolean
    ImplementsTrait,
}

/// Kinds of types in the Outrun type system
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// Struct types (User, Config, etc.)
    Struct,
    /// Trait types (Display, Serializable, etc.)
    Trait,
    /// Primitive types (Integer64, String, Boolean, etc.)
    Primitive,
    /// Collection types (List<T>, Map<K,V>, Tuple<...>, etc.)
    Collection,
}

/// Concrete type information for struct types
/// This is what StructType values contain at runtime
#[derive(Debug, Clone, PartialEq)]
pub struct StructTypeInfo {
    pub name: String,
    pub module: String,
    pub fields: Vec<StructFieldInfo>,
    pub implemented_traits: Vec<TypeId>,
    pub span: Span,
}

/// Information about struct fields for introspection
#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldInfo {
    pub name: String,
    pub field_type: TypeId,
    pub span: Span,
}

/// Concrete type information for trait types
/// This is what TraitType values contain at runtime
#[derive(Debug, Clone, PartialEq)]
pub struct TraitTypeInfo {
    pub name: String,
    pub module: String,
    pub functions: Vec<TraitFunctionInfo>,
    pub constraints: Vec<TypeId>,
    pub span: Span,
}

/// Information about trait functions for introspection
#[derive(Debug, Clone, PartialEq)]
pub struct TraitFunctionInfo {
    pub name: String,
    pub params: Vec<(AtomId, TypeId)>, // (param_name_atom, param_type)
    pub return_type: TypeId,
    pub is_static: bool,
    pub span: Span,
}

/// Registry for type introspection information
/// Maintains the mapping from TypeId to introspection metadata
#[derive(Debug, Clone)]
pub struct IntrospectionRegistry {
    /// Struct type information indexed by TypeId
    pub struct_types: std::collections::HashMap<TypeId, StructTypeInfo>,
    /// Trait type information indexed by TypeId
    pub trait_types: std::collections::HashMap<TypeId, TraitTypeInfo>,
}

impl IntrospectionRegistry {
    /// Create a new empty introspection registry
    pub fn new() -> Self {
        Self {
            struct_types: std::collections::HashMap::new(),
            trait_types: std::collections::HashMap::new(),
        }
    }

    /// Register a struct type for introspection
    pub fn register_struct_type(&mut self, type_id: TypeId, info: StructTypeInfo) {
        self.struct_types.insert(type_id, info);
    }

    /// Register a trait type for introspection
    pub fn register_trait_type(&mut self, type_id: TypeId, info: TraitTypeInfo) {
        self.trait_types.insert(type_id, info);
    }

    /// Get struct type information
    pub fn get_struct_type(&self, type_id: TypeId) -> Option<&StructTypeInfo> {
        self.struct_types.get(&type_id)
    }

    /// Get trait type information
    pub fn get_trait_type(&self, type_id: TypeId) -> Option<&TraitTypeInfo> {
        self.trait_types.get(&type_id)
    }

    /// Check if a type is a struct type
    pub fn is_struct_type(&self, type_id: TypeId) -> bool {
        self.struct_types.contains_key(&type_id)
    }

    /// Check if a type is a trait type
    pub fn is_trait_type(&self, type_id: TypeId) -> bool {
        self.trait_types.contains_key(&type_id)
    }

    /// Determine the TypeKind for a given type
    pub fn get_type_kind(&self, type_id: TypeId) -> Option<TypeKind> {
        if self.is_struct_type(type_id) {
            Some(TypeKind::Struct)
        } else if self.is_trait_type(type_id) {
            Some(TypeKind::Trait)
        } else {
            // TODO: Add logic for Primitive and Collection types
            // For now, assume primitive
            Some(TypeKind::Primitive)
        }
    }

    /// Get the name of a type for introspection
    pub fn get_type_name(&self, type_id: TypeId) -> Option<&str> {
        if let Some(struct_info) = self.get_struct_type(type_id) {
            Some(&struct_info.name)
        } else if let Some(trait_info) = self.get_trait_type(type_id) {
            Some(&trait_info.name)
        } else {
            None
        }
    }

    /// Get the module of a type for introspection
    pub fn get_type_module(&self, type_id: TypeId) -> Option<&str> {
        if let Some(struct_info) = self.get_struct_type(type_id) {
            Some(&struct_info.module)
        } else if let Some(trait_info) = self.get_trait_type(type_id) {
            Some(&trait_info.module)
        } else {
            None
        }
    }

    /// Check if a struct type implements a specific trait
    pub fn struct_implements_trait(&self, struct_type_id: TypeId, trait_type_id: TypeId) -> bool {
        if let Some(struct_info) = self.get_struct_type(struct_type_id) {
            struct_info.implemented_traits.contains(&trait_type_id)
        } else {
            false
        }
    }
}

impl Default for IntrospectionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Struct => write!(f, "Struct"),
            TypeKind::Trait => write!(f, "Trait"),
            TypeKind::Primitive => write!(f, "Primitive"),
            TypeKind::Collection => write!(f, "Collection"),
        }
    }
}

impl std::fmt::Display for StructTypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "StructType({})", self.name)
    }
}

impl std::fmt::Display for TraitTypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TraitType({})", self.name)
    }
}
