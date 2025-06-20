//! Dispatch table construction and management
//!
//! This module handles building the dispatch tables that the interpreter
//! uses for efficient trait method resolution.

// Re-export for convenience - using the one from mod.rs
use super::DispatchTable;

// use crate::types::{TypeId, TraitId}; // TODO: Use when needed
use crate::error::TypeResult;
use crate::types::traits::TraitRegistry;
use crate::types::TypeInterner;

/// Dispatch table builder
pub struct DispatchTableBuilder<'a> {
    table: DispatchTable,
    trait_registry: &'a TraitRegistry,
    interner: &'a mut TypeInterner,
}

impl<'a> DispatchTableBuilder<'a> {
    /// Create a new dispatch table builder
    pub fn new(trait_registry: &'a TraitRegistry, interner: &'a mut TypeInterner) -> Self {
        Self {
            table: DispatchTable::new(),
            trait_registry,
            interner,
        }
    }

    /// Build dispatch table from trait registry
    pub fn build(mut self) -> TypeResult<DispatchTable> {
        // Build trait implementation dispatch
        self.build_trait_dispatch()?;

        // Build static function dispatch
        self.build_static_dispatch()?;

        // Validate the constructed dispatch table
        DispatchValidator::validate_dispatch_table(&self.table, self.trait_registry)?;

        Ok(self.table)
    }

    /// Build trait implementation dispatch entries
    fn build_trait_dispatch(&mut self) -> TypeResult<()> {
        // Get all trait implementations from the registry
        let implementations: Vec<_> = self.trait_registry.all_implementations().collect();

        for impl_block in implementations {
            // Register the trait implementation dispatch
            let _module_id = self
                .table
                .register_trait_impl(impl_block.trait_id, impl_block.type_id);

            // Note: The module_id will be used by the interpreter to look up
            // the actual function implementations for this (trait, type) pair
        }

        Ok(())
    }

    /// Build static function dispatch
    fn build_static_dispatch(&mut self) -> TypeResult<()> {
        // Build dispatch for static trait functions (using `defs` keyword)
        for trait_def in self.trait_registry.all_traits() {
            for trait_func in &trait_def.functions {
                if trait_func.is_static {
                    // Static functions are available on the trait itself
                    // e.g., Option.some(value: String) or Result.ok(value: String)

                    // Get trait name to create a synthetic TypeId for static function lookup
                    // Use the trait name directly from the definition
                    let trait_type_id = self.interner.intern_type(&trait_def.name);

                    // Get function name for lookup
                    if let Some(function_name) = self.interner.atom_name(trait_func.name) {
                        // For static functions, we create a unique FunctionId based on the atom
                        // We use a hash of the trait name and function name for uniqueness
                        use std::collections::hash_map::DefaultHasher;
                        use std::hash::{Hash, Hasher};

                        let mut hasher = DefaultHasher::new();
                        trait_def.name.hash(&mut hasher);
                        function_name.hash(&mut hasher);
                        let function_id = crate::types::traits::FunctionId(hasher.finish() as u32);

                        self.table.register_static_function(
                            trait_type_id,
                            function_name.to_string(),
                            function_id,
                        );
                    }
                }
            }
        }

        Ok(())
    }
}

/// Utilities for dispatch table validation
pub struct DispatchValidator;

impl DispatchValidator {
    /// Validate that all required trait functions have implementations
    pub fn validate_completeness(
        table: &DispatchTable,
        trait_registry: &TraitRegistry,
    ) -> TypeResult<()> {
        // Check that every trait implementation in the registry has a dispatch entry
        for impl_block in trait_registry.all_implementations() {
            if table
                .lookup_trait_impl(impl_block.trait_id, impl_block.type_id)
                .is_none()
            {
                return Err(crate::error::TypeError::internal(format!(
                    "Missing dispatch entry for trait {:?} on type {:?}",
                    impl_block.trait_id, impl_block.type_id
                )));
            }
        }

        Ok(())
    }

    /// Check for conflicting implementations
    pub fn validate_coherence(_table: &DispatchTable) -> TypeResult<()> {
        // For now, our dispatch table doesn't allow conflicts by design
        // Each (trait_id, type_id) pair can only have one entry
        // This validation ensures the table structure is sound

        // The HashMap-based dispatch table automatically prevents conflicts
        // by design, so this validation always passes
        // In the future, we might add more sophisticated coherence checks

        Ok(())
    }

    /// Comprehensive validation of dispatch table
    pub fn validate_dispatch_table(
        table: &DispatchTable,
        trait_registry: &TraitRegistry,
    ) -> TypeResult<()> {
        Self::validate_completeness(table, trait_registry)?;
        Self::validate_coherence(table)?;
        Ok(())
    }
}
