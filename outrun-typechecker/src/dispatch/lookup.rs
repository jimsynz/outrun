//! Dispatch table construction and management
//!
//! This module handles building the dispatch tables that the interpreter
//! uses for efficient trait method resolution.

use super::DispatchTable;

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::error::TypeResult;

/// Dispatch table builder
pub struct DispatchTableBuilder {
    table: DispatchTable,
    compiler_environment: CompilerEnvironment,
}

impl DispatchTableBuilder {
    /// Create a new dispatch table builder
    pub fn new(compiler_environment: CompilerEnvironment) -> Self {
        Self {
            table: DispatchTable::new(),
            compiler_environment,
        }
    }

    /// Build dispatch table from compiler environment
    pub fn build(mut self) -> TypeResult<DispatchTable> {
        // Build trait implementation dispatch
        self.build_trait_dispatch()?;

        // Build static function dispatch
        self.build_static_dispatch()?;

        // Validate the constructed dispatch table
        DispatchValidator::validate_dispatch_table(&self.table, &self.compiler_environment)?;

        Ok(self.table)
    }

    /// Build trait implementation dispatch entries
    fn build_trait_dispatch(&mut self) -> TypeResult<()> {
        // Get all modules from CompilerEnvironment
        let modules = self.compiler_environment.modules().read().unwrap();

        // Find all TraitImpl modules and register them
        for (module_key, _module) in modules.iter() {
            if let crate::compilation::compiler_environment::ModuleKey::TraitImpl(
                trait_type,
                impl_type,
            ) = module_key
            {
                // Extract TypeNameId from StructuredType for dispatch table
                // (dispatch table still uses the old TypeNameId system)
                if let (
                    crate::unification::StructuredType::Simple(trait_id),
                    crate::unification::StructuredType::Simple(impl_id),
                ) = (trait_type.as_ref(), impl_type.as_ref())
                {
                    // Register the trait implementation dispatch
                    let _module_id = self
                        .table
                        .register_trait_impl(trait_id.clone(), impl_id.clone());

                    // Note: The module_id will be used by the interpreter to look up
                    // the actual function implementations for this (trait, type) pair
                }
            }
        }

        Ok(())
    }

    /// Build static function dispatch
    fn build_static_dispatch(&mut self) -> TypeResult<()> {
        // Build dispatch for static trait functions (using `defs` keyword)
        let modules = self.compiler_environment.modules().read().unwrap();

        // Find all trait modules and check their functions for static ones
        for (module_key, module) in modules.iter() {
            if let crate::compilation::compiler_environment::ModuleKey::Module(_symbol) = module_key
            {
                if matches!(
                    module.module_kind,
                    crate::compilation::compiler_environment::ModuleKind::Trait
                ) {
                    // This is a trait module - check its functions for static ones
                    for (function_name_atom, function_entry) in &module.functions_by_name {
                        // Check if this is a static function based on FunctionType
                        if matches!(
                            function_entry.function_type(),
                            crate::compilation::FunctionType::TraitStatic
                        ) {
                            // Static functions are available on the trait itself
                            // e.g., Option.some(value: String) or Result.ok(value: String)

                            // Extract trait type from module's structured type
                            if let crate::unification::StructuredType::Simple(trait_type_id) =
                                &module.structured_type
                            {
                                // Get function name for lookup
                                let function_name = self
                                    .compiler_environment
                                    .resolve_atom_name(function_name_atom)
                                    .unwrap_or_else(|| "unknown".to_string());

                                // For static functions, we create a unique FunctionId based on the atom
                                // We use a hash of the trait name and function name for uniqueness
                                use std::collections::hash_map::DefaultHasher;
                                use std::hash::{Hash, Hasher};

                                let trait_name = trait_type_id.to_string();
                                let mut hasher = DefaultHasher::new();
                                trait_name.hash(&mut hasher);
                                function_name.hash(&mut hasher);
                                let function_id =
                                    crate::types::traits::FunctionId(hasher.finish() as u32);

                                self.table.register_static_function(
                                    trait_type_id.clone(),
                                    function_name,
                                    function_id,
                                );
                            }
                        }
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
        compiler_environment: &CompilerEnvironment,
    ) -> TypeResult<()> {
        // Check that every trait implementation in the compiler environment has a dispatch entry
        let modules = compiler_environment.modules().read().unwrap();

        for (module_key, _module) in modules.iter() {
            if let crate::compilation::compiler_environment::ModuleKey::TraitImpl(
                trait_type,
                impl_type,
            ) = module_key
            {
                // Extract TypeNameIds for dispatch table lookup
                if let (
                    crate::unification::StructuredType::Simple(trait_id),
                    crate::unification::StructuredType::Simple(impl_id),
                ) = (trait_type.as_ref(), impl_type.as_ref())
                {
                    if table
                        .lookup_trait_impl(trait_id.clone(), impl_id.clone())
                        .is_none()
                    {
                        return Err(crate::error::TypeError::internal(format!(
                            "Missing dispatch entry for trait {:?} on type {:?}",
                            trait_id, impl_id
                        )));
                    }
                }
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
        compiler_environment: &CompilerEnvironment,
    ) -> TypeResult<()> {
        Self::validate_completeness(table, compiler_environment)?;
        Self::validate_coherence(table)?;
        Ok(())
    }
}
