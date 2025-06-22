//! Trait dispatch system
//!
//! This module builds and manages dispatch tables for efficient runtime
//! trait method resolution in the interpreter.

pub mod lookup;

use crate::types::traits::FunctionId;
use crate::types::TypeId;
use outrun_parser::{BinaryOperator, UnaryOperator};
use std::collections::HashMap;

/// Opaque module identifier for trait implementations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpaqueModuleId(pub u32);

/// Main dispatch table for runtime trait method resolution
#[derive(Debug, Clone)]
pub struct DispatchTable {
    // Core trait dispatch: (TypeId, TypeId) -> OpaqueModuleId
    trait_dispatch: HashMap<(TypeId, TypeId), OpaqueModuleId>,

    // Static function lookup: TypeId -> function mappings
    static_functions: HashMap<TypeId, HashMap<String, FunctionId>>,

    // Operator dispatch tables
    binary_ops: HashMap<(BinaryOperator, TypeId, TypeId), FunctionId>,
    unary_ops: HashMap<(UnaryOperator, TypeId), FunctionId>,

    // Next available opaque module ID
    next_module_id: u32,
}

impl DispatchTable {
    /// Create a new empty dispatch table
    pub fn new() -> Self {
        Self {
            trait_dispatch: HashMap::new(),
            static_functions: HashMap::new(),
            binary_ops: HashMap::new(),
            unary_ops: HashMap::new(),
            next_module_id: 0,
        }
    }

    /// Register a trait implementation for dispatch
    pub fn register_trait_impl(&mut self, trait_id: TypeId, type_id: TypeId) -> OpaqueModuleId {
        let module_id = self.next_opaque_module_id();
        self.trait_dispatch.insert((trait_id, type_id), module_id);
        module_id
    }

    /// Look up trait implementation
    pub fn lookup_trait_impl(&self, trait_id: TypeId, type_id: TypeId) -> Option<OpaqueModuleId> {
        self.trait_dispatch.get(&(trait_id, type_id)).copied()
    }

    /// Register a static function for a type
    pub fn register_static_function(
        &mut self,
        type_id: TypeId,
        function_name: String,
        function_id: FunctionId,
    ) {
        self.static_functions
            .entry(type_id)
            .or_default()
            .insert(function_name, function_id);
    }

    /// Look up static function
    pub fn lookup_static_function(&self, type_id: TypeId, name: &str) -> Option<FunctionId> {
        self.static_functions.get(&type_id)?.get(name).copied()
    }

    /// Register binary operator implementation
    pub fn register_binary_op(
        &mut self,
        operator: BinaryOperator,
        left_type: TypeId,
        right_type: TypeId,
        function_id: FunctionId,
    ) {
        self.binary_ops
            .insert((operator, left_type, right_type), function_id);
    }

    /// Look up binary operator implementation
    pub fn lookup_binary_op(
        &self,
        operator: BinaryOperator,
        left_type: TypeId,
        right_type: TypeId,
    ) -> Option<FunctionId> {
        self.binary_ops
            .get(&(operator, left_type, right_type))
            .copied()
    }

    /// Register unary operator implementation
    pub fn register_unary_op(
        &mut self,
        operator: UnaryOperator,
        operand_type: TypeId,
        function_id: FunctionId,
    ) {
        self.unary_ops.insert((operator, operand_type), function_id);
    }

    /// Look up unary operator implementation
    pub fn lookup_unary_op(
        &self,
        operator: UnaryOperator,
        operand_type: TypeId,
    ) -> Option<FunctionId> {
        self.unary_ops.get(&(operator, operand_type)).copied()
    }

    /// Generate next opaque module ID
    fn next_opaque_module_id(&mut self) -> OpaqueModuleId {
        let id = OpaqueModuleId(self.next_module_id);
        self.next_module_id += 1;
        id
    }

    /// Get statistics about the dispatch table
    pub fn stats(&self) -> DispatchStats {
        DispatchStats {
            trait_implementations: self.trait_dispatch.len(),
            static_functions: self.static_functions.values().map(|m| m.len()).sum(),
            binary_operators: self.binary_ops.len(),
            unary_operators: self.unary_ops.len(),
        }
    }
}

impl Default for DispatchTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about dispatch table contents
#[derive(Debug, Clone)]
pub struct DispatchStats {
    pub trait_implementations: usize,
    pub static_functions: usize,
    pub binary_operators: usize,
    pub unary_operators: usize,
}
