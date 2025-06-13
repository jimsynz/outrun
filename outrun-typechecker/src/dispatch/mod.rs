//! Trait dispatch system
//!
//! This module builds and manages dispatch tables for efficient runtime
//! trait method resolution in the interpreter.

pub mod lookup;
pub mod resolution;

// Re-export core types
// Re-export from lookup module
pub use resolution::{resolve_static_function, resolve_trait_function};

use crate::types::traits::FunctionId;
use crate::types::{TraitId, TypeId};
// use outrun_parser::BinaryOperator; // TODO: Use when operators support Hash + Eq
use std::collections::HashMap;

/// Opaque module identifier for trait implementations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpaqueModuleId(pub u32);

/// Main dispatch table for runtime trait method resolution
#[derive(Debug, Clone)]
pub struct DispatchTable {
    // Core trait dispatch: (TraitId, TypeId) -> OpaqueModuleId
    trait_dispatch: HashMap<(TraitId, TypeId), OpaqueModuleId>,

    // Static function lookup: TypeId -> function mappings
    static_functions: HashMap<TypeId, HashMap<String, FunctionId>>,

    // TODO: Add operator dispatch when BinaryOperator/UnaryOperator implement Hash + Eq
    // binary_ops: HashMap<(BinaryOperator, TypeId, TypeId), FunctionId>,
    // unary_ops: HashMap<(outrun_parser::UnaryOperator, TypeId), FunctionId>,

    // Next available opaque module ID
    next_module_id: u32,
}

impl DispatchTable {
    /// Create a new empty dispatch table
    pub fn new() -> Self {
        Self {
            trait_dispatch: HashMap::new(),
            static_functions: HashMap::new(),
            next_module_id: 0,
        }
    }

    /// Register a trait implementation for dispatch
    pub fn register_trait_impl(&mut self, trait_id: TraitId, type_id: TypeId) -> OpaqueModuleId {
        let module_id = self.next_opaque_module_id();
        self.trait_dispatch.insert((trait_id, type_id), module_id);
        module_id
    }

    /// Look up trait implementation
    pub fn lookup_trait_impl(&self, trait_id: TraitId, type_id: TypeId) -> Option<OpaqueModuleId> {
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

    // TODO: Add operator dispatch methods when BinaryOperator/UnaryOperator implement Hash + Eq
    /*
    /// Register binary operator implementation
    pub fn register_binary_op(
        &mut self,
        operator: BinaryOperator,
        left_type: TypeId,
        right_type: TypeId,
        function_id: FunctionId,
    ) {
        self.binary_ops.insert((operator, left_type, right_type), function_id);
    }

    /// Look up binary operator implementation
    pub fn lookup_binary_op(
        &self,
        operator: BinaryOperator,
        left_type: TypeId,
        right_type: TypeId,
    ) -> Option<FunctionId> {
        self.binary_ops.get(&(operator, left_type, right_type)).copied()
    }

    /// Register unary operator implementation
    pub fn register_unary_op(
        &mut self,
        operator: outrun_parser::UnaryOperator,
        operand_type: TypeId,
        function_id: FunctionId,
    ) {
        self.unary_ops.insert((operator, operand_type), function_id);
    }

    /// Look up unary operator implementation
    pub fn lookup_unary_op(
        &self,
        operator: outrun_parser::UnaryOperator,
        operand_type: TypeId,
    ) -> Option<FunctionId> {
        self.unary_ops.get(&(operator, operand_type)).copied()
    }
    */

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
            binary_operators: 0, // TODO: Add back when operators support Hash + Eq
            unary_operators: 0,  // TODO: Add back when operators support Hash + Eq
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeInterner;

    #[test]
    fn test_dispatch_table_creation() {
        let table = DispatchTable::new();
        let stats = table.stats();

        assert_eq!(stats.trait_implementations, 0);
        assert_eq!(stats.static_functions, 0);
        assert_eq!(stats.binary_operators, 0);
        assert_eq!(stats.unary_operators, 0);
    }

    #[test]
    fn test_trait_implementation_registration() {
        let mut table = DispatchTable::new();
        let mut interner = TypeInterner::new();

        let trait_id = interner.intern_trait("Display");
        let type_id = interner.intern_type("Integer");

        let module_id = table.register_trait_impl(trait_id, type_id);
        assert_eq!(module_id, OpaqueModuleId(0));

        let found = table.lookup_trait_impl(trait_id, type_id);
        assert_eq!(found, Some(module_id));

        // Non-existent lookup should return None
        let other_type = interner.intern_type("String");
        let not_found = table.lookup_trait_impl(trait_id, other_type);
        assert_eq!(not_found, None);
    }

    #[test]
    fn test_static_function_registration() {
        let mut table = DispatchTable::new();
        let mut interner = TypeInterner::new();

        let type_id = interner.intern_type("Integer");
        let function_id = crate::types::traits::FunctionId(42);

        table.register_static_function(type_id, "parse".to_string(), function_id);

        let found = table.lookup_static_function(type_id, "parse");
        assert_eq!(found, Some(function_id));

        let not_found = table.lookup_static_function(type_id, "nonexistent");
        assert_eq!(not_found, None);
    }

    // TODO: Re-enable when BinaryOperator implements Hash + Eq
    /*
    #[test]
    fn test_binary_operator_registration() {
        let mut table = DispatchTable::new();
        let mut interner = TypeInterner::new();

        let int_type = interner.intern_type("Integer");
        let function_id = crate::types::traits::FunctionId(100);

        table.register_binary_op(BinaryOperator::Add, int_type, int_type, function_id);

        let found = table.lookup_binary_op(BinaryOperator::Add, int_type, int_type);
        assert_eq!(found, Some(function_id));

        let not_found = table.lookup_binary_op(BinaryOperator::Subtract, int_type, int_type);
        assert_eq!(not_found, None);
    }
    */

    #[test]
    fn test_dispatch_stats() {
        let mut table = DispatchTable::new();
        let mut interner = TypeInterner::new();

        let trait_id = interner.intern_trait("Display");
        let type_id = interner.intern_type("Integer");
        let function_id = crate::types::traits::FunctionId(1);

        // Add some entries
        table.register_trait_impl(trait_id, type_id);
        table.register_static_function(type_id, "test".to_string(), function_id);
        // TODO: Re-enable when BinaryOperator supports Hash + Eq
        // table.register_binary_op(BinaryOperator::Add, type_id, type_id, function_id);

        let stats = table.stats();
        assert_eq!(stats.trait_implementations, 1);
        assert_eq!(stats.static_functions, 1);
        assert_eq!(stats.binary_operators, 0); // TODO: Change to 1 when operators are supported
        assert_eq!(stats.unary_operators, 0);
    }
}
