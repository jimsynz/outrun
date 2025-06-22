//! Function registry for organizing and looking up functions by context
//!
//! This module provides a hierarchical function registry that organizes functions
//! by their context (trait, impl, module) and provides efficient lookup mechanisms
//! for qualified and local function resolution.

use crate::types::TypeId;
use crate::unification::UnificationContext;
use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{FunctionDefinition, FunctionVisibility, Item, ItemKind};
use std::collections::HashMap;

/// Function entry with dispatch metadata
#[derive(Debug, Clone)]
pub struct FunctionEntry {
    /// The function definition
    pub definition: FunctionDefinition,
    /// Type of function for dispatch resolution
    pub function_type: FunctionType,
    /// Unique function ID for dispatch
    pub function_id: String,
}

/// Type of function for dispatch resolution
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType {
    /// Static trait function (defs keyword)
    TraitStatic,
    /// Trait function signature only (def without body)
    TraitSignature,
    /// Trait function with default implementation (def with body)
    TraitDefault,
    /// Static function on a type (not in trait)
    TypeStatic,
    /// Implementation function in impl block
    ImplFunction,
}

/// TypeId-based function registry that organizes functions by their module context
#[derive(Debug, Default, Clone)]
pub struct FunctionRegistry {
    /// Functions defined in traits: trait_type_id -> function_name -> entry
    pub trait_functions: HashMap<TypeId, HashMap<String, FunctionEntry>>,
    /// Functions defined in impl blocks: (trait_type_id, impl_type_id) -> function_name -> entry  
    pub impl_functions: HashMap<(TypeId, TypeId), HashMap<String, FunctionEntry>>,
    /// Functions defined in type modules: module_type_id -> function_name -> entry
    pub module_functions: HashMap<TypeId, HashMap<String, FunctionEntry>>,
}

impl FunctionRegistry {
    /// Look up a function by qualified name (e.g., "Option.some?", "List.head")
    /// Note: module_name should be the base type name without generics (e.g., "List" not "List<T>")
    pub fn lookup_qualified_function(
        &self,
        module_type_id: TypeId,
        function_name: &str,
    ) -> Option<&FunctionEntry> {
        // Try trait functions first
        if let Some(trait_funcs) = self.trait_functions.get(&module_type_id) {
            if let Some(func) = trait_funcs.get(function_name) {
                return Some(func);
            }
        }

        // Try module functions
        if let Some(module_funcs) = self.module_functions.get(&module_type_id) {
            if let Some(func) = module_funcs.get(function_name) {
                return Some(func);
            }
        }

        None
    }

    /// Look up a function implementation for trait dispatch
    pub fn lookup_impl_function(
        &self,
        trait_type_id: TypeId,
        impl_type_id: TypeId,
        function_name: &str,
    ) -> Option<&FunctionEntry> {
        self.impl_functions
            .get(&(trait_type_id, impl_type_id))
            .and_then(|funcs| funcs.get(function_name))
    }

    /// Look up a local function in the current module context
    /// This searches both trait functions and module functions for the given function name
    pub fn lookup_local_function(&self, function_name: &str) -> Option<&FunctionEntry> {
        // Search through all trait functions
        for trait_funcs in self.trait_functions.values() {
            if let Some(func) = trait_funcs.get(function_name) {
                return Some(func);
            }
        }

        // Search through all module functions
        for module_funcs in self.module_functions.values() {
            if let Some(func) = module_funcs.get(function_name) {
                return Some(func);
            }
        }

        None
    }

    /// Check if an impl block has an override for a trait function
    pub fn has_impl_override(
        &self,
        trait_type_id: TypeId,
        impl_type_id: TypeId,
        function_name: &str,
    ) -> bool {
        self.impl_functions
            .get(&(trait_type_id, impl_type_id))
            .map(|funcs| funcs.contains_key(function_name))
            .unwrap_or(false)
    }

    /// Add a trait function entry
    pub fn add_trait_function(
        &mut self,
        trait_type_id: TypeId,
        function_name: String,
        entry: FunctionEntry,
    ) {
        self.trait_functions
            .entry(trait_type_id)
            .or_default()
            .insert(function_name, entry);
    }

    /// Add an impl function entry
    pub fn add_impl_function(
        &mut self,
        trait_type_id: TypeId,
        impl_type_id: TypeId,
        function_name: String,
        entry: FunctionEntry,
    ) {
        self.impl_functions
            .entry((trait_type_id, impl_type_id))
            .or_default()
            .insert(function_name, entry);
    }

    /// Add module function entry
    pub fn add_module_function(
        &mut self,
        type_id: TypeId,
        function_name: String,
        entry: FunctionEntry,
    ) {
        self.module_functions
            .entry(type_id)
            .or_default()
            .insert(function_name, entry);
    }

    /// Get total number of functions across all registries
    pub fn len(&self) -> usize {
        let trait_count: usize = self.trait_functions.values().map(|m| m.len()).sum();
        let impl_count: usize = self.impl_functions.values().map(|m| m.len()).sum();
        let module_count: usize = self.module_functions.values().map(|m| m.len()).sum();
        trait_count + impl_count + module_count
    }

    /// Check if registry is empty
    pub fn is_empty(&self) -> bool {
        self.trait_functions.is_empty()
            && self.impl_functions.is_empty()
            && self.module_functions.is_empty()
    }
}

/// Visitor for extracting function definitions (Phase 4)
pub struct FunctionExtractionVisitor<'a> {
    /// Function registry being built
    pub registry: FunctionRegistry,
    /// Reference to context for type interning (avoids large clone)
    pub context: &'a mut UnificationContext,
}

impl<'a> FunctionExtractionVisitor<'a> {
    /// Create a new function extraction visitor with borrowed context
    pub fn new(context: &'a mut UnificationContext) -> Self {
        Self {
            registry: FunctionRegistry::default(),
            context,
        }
    }

    /// Create a new function extraction visitor with pre-initialized registry
    pub fn with_registry(registry: FunctionRegistry, context: &'a mut UnificationContext) -> Self {
        Self { registry, context }
    }
}

impl<'a> Visitor<()> for FunctionExtractionVisitor<'a> {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::FunctionDefinition(func_def) => {
                // Create function entry
                let function_id = format!("function::{}", func_def.name.name);
                let entry = FunctionEntry {
                    definition: func_def.clone(),
                    function_type: FunctionType::TypeStatic,
                    function_id,
                };

                // Add to module functions for now (trait/impl context would be set by parent visitor)
                let module_type_id = self.context.type_interner.intern_type("Module");
                self.registry.add_module_function(
                    module_type_id,
                    func_def.name.name.clone(),
                    entry,
                );
            }
            ItemKind::TraitDefinition(trait_def) => {
                let trait_name = trait_def.name_as_string();
                let trait_id = self.context.type_interner.intern_type(&trait_name);

                // Process trait functions
                for trait_func in &trait_def.functions {
                    use outrun_parser::{Block, Span, TraitFunction};

                    let (function_def, function_type, function_name) = match trait_func {
                        TraitFunction::Signature(sig) => {
                            // Convert signature to function definition
                            let function_def = FunctionDefinition {
                                attributes: sig.attributes.clone(),
                                name: sig.name.clone(),
                                visibility: sig.visibility.clone(),
                                parameters: sig.parameters.clone(),
                                return_type: sig.return_type.clone(),
                                guard: sig.guard.clone(),
                                body: Block {
                                    statements: Vec::new(),
                                    span: Span {
                                        start: 0,
                                        end: 0,
                                        start_line_col: None,
                                        end_line_col: None,
                                    },
                                },
                                span: sig.span,
                            };
                            (
                                function_def,
                                FunctionType::TraitSignature,
                                sig.name.name.clone(),
                            )
                        }
                        TraitFunction::Definition(def) => (
                            def.clone(),
                            FunctionType::TraitDefault,
                            def.name.name.clone(),
                        ),
                        TraitFunction::StaticDefinition(static_def) => {
                            // Convert static definition to function definition
                            let function_def = FunctionDefinition {
                                attributes: static_def.attributes.clone(),
                                name: static_def.name.clone(),
                                visibility: FunctionVisibility::Public,
                                parameters: static_def.parameters.clone(),
                                return_type: static_def.return_type.clone(),
                                guard: None,
                                body: static_def.body.clone(),
                                span: static_def.span,
                            };
                            (
                                function_def,
                                FunctionType::TraitStatic,
                                static_def.name.name.clone(),
                            )
                        }
                    };

                    let function_id = format!("trait::{}::{}", trait_name, function_name);
                    let entry = FunctionEntry {
                        definition: function_def,
                        function_type,
                        function_id,
                    };

                    self.registry
                        .add_trait_function(trait_id, function_name, entry);
                }
            }
            ItemKind::ImplBlock(impl_block) => {
                // Register impl block functions
                let impl_type_name = impl_block
                    .type_spec
                    .path
                    .iter()
                    .map(|id| id.name.clone())
                    .collect::<Vec<_>>()
                    .join(".");
                let impl_type_id = self.context.type_interner.intern_type(&impl_type_name);

                let trait_name = impl_block
                    .trait_spec
                    .path
                    .iter()
                    .map(|id| id.name.clone())
                    .collect::<Vec<_>>()
                    .join(".");
                let trait_id = self.context.type_interner.intern_type(&trait_name);

                for impl_func in &impl_block.methods {
                    let function_id = format!(
                        "impl::{}::{}::{}",
                        impl_type_name, trait_name, impl_func.name.name
                    );
                    let entry = FunctionEntry {
                        definition: impl_func.clone(),
                        function_type: FunctionType::ImplFunction,
                        function_id,
                    };

                    self.registry.add_impl_function(
                        trait_id,
                        impl_type_id,
                        impl_func.name.name.clone(),
                        entry,
                    );
                }
            }
            _ => {}
        }
        Ok(())
    }
}
