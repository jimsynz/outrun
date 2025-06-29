//! Program collection management for multi-program compilation
//!
//! This module provides data structures and utilities for managing collections
//! of Outrun programs during compilation, including source code management and
//! compilation result tracking.

use crate::compilation::compiler_environment::{CompilerEnvironment, TypeNameId};
use crate::unification::UnificationContext;
use outrun_parser::{ImplBlock, Program, StructDefinition, TraitDefinition};
use std::collections::HashMap;

/// A collection of Outrun programs to be compiled together
#[derive(Debug, Clone)]
pub struct ProgramCollection {
    /// Map from file path to parsed program
    pub programs: HashMap<String, Program>,
    /// Source code for each file (for error reporting)
    pub sources: HashMap<String, String>,
}

impl Default for ProgramCollection {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of multi-program compilation
#[derive(Debug, Clone)]
pub struct CompilationResult {
    /// Programs in dependency order
    pub compilation_order: Vec<String>,
    /// Type checking context with all resolved types, expression types, and span mappings
    pub type_context: UnificationContext,
    /// Collected traits from all programs (trait name TypeNameId -> definition)
    pub traits: HashMap<TypeNameId, TraitDefinition>,
    /// Collected structs from all programs (struct name TypeNameId -> definition)
    pub structs: HashMap<TypeNameId, StructDefinition>,
    /// Collected implementations from all programs
    pub implementations: Vec<ImplBlock>,
    /// Typed AST for all programs (filename -> typed program)
    pub typed_programs: HashMap<String, crate::checker::TypedProgram>,
}

impl CompilationResult {
    /// Merge multiple compilation results into a single result
    /// This is essential for package composition and incremental compilation
    pub fn merge(
        base: CompilationResult,
        others: Vec<CompilationResult>,
    ) -> Result<CompilationResult, Vec<String>> {
        let mut merged = base;
        let mut conflicts = Vec::new();

        for other in others {
            // Merge compilation orders - preserve dependency relationships
            for file in other.compilation_order {
                if !merged.compilation_order.contains(&file) {
                    merged.compilation_order.push(file);
                }
            }

            // Merge type contexts - this requires careful TypeInterner handling
            merged.type_context = merged
                .type_context
                .merge_with(other.type_context)
                .map_err(|e| vec![format!("Type context merge error: {:?}", e)])?;

            // Merge traits - check for conflicts
            for (trait_id, trait_def) in other.traits {
                if let Some(existing) = merged.traits.get(&trait_id) {
                    if !traits_compatible(existing, &trait_def) {
                        conflicts.push(format!("Conflicting trait definition: {}", trait_id));
                    }
                } else {
                    merged.traits.insert(trait_id, trait_def);
                }
            }

            // Merge structs - check for conflicts
            for (struct_id, struct_def) in other.structs {
                if let Some(existing) = merged.structs.get(&struct_id) {
                    if !structs_compatible(existing, &struct_def) {
                        conflicts.push(format!("Conflicting struct definition: {}", struct_id));
                    }
                } else {
                    merged.structs.insert(struct_id, struct_def);
                }
            }

            // Merge implementations - duplicates are allowed (orphan rules prevent conflicts)
            merged.implementations.extend(other.implementations);

            // Merge typed programs
            for (filename, typed_program) in other.typed_programs {
                match merged.typed_programs.entry(filename.clone()) {
                    std::collections::hash_map::Entry::Occupied(_) => {
                        conflicts.push(format!("Duplicate program file: {}", filename));
                    }
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(typed_program);
                    }
                }
            }
        }

        if !conflicts.is_empty() {
            Err(conflicts)
        } else {
            Ok(merged)
        }
    }

    /// Create a package summary that can be used for fast imports
    /// This extracts just the public interface without implementation details
    pub fn create_package_summary(
        &self,
        package_name: &str,
        compiler_env: &CompilerEnvironment,
    ) -> PackageSummary {
        PackageSummary {
            package_name: package_name.to_string(),
            exported_traits: self.traits.clone(), // TODO: filter to public traits
            exported_structs: self.structs.clone(), // TODO: filter to public structs
            exported_functions: HashMap::new(), // TODO: extract public functions from CompilerEnvironment
            exported_implementations: self.implementations.clone(), // All impls are public by nature
            type_context_summary: self.type_context.create_summary(compiler_env), // TODO: implement
        }
    }

    /// Check if this compilation result is compatible with another for merging
    pub fn is_compatible_with(&self, other: &CompilationResult) -> Result<(), Vec<String>> {
        let mut conflicts = Vec::new();

        // Check trait conflicts
        for (trait_id, trait_def) in &other.traits {
            if let Some(existing) = self.traits.get(trait_id) {
                if !traits_compatible(existing, trait_def) {
                    conflicts.push(format!("Incompatible trait: {}", trait_id));
                }
            }
        }

        // Check struct conflicts
        for (struct_id, struct_def) in &other.structs {
            if let Some(existing) = self.structs.get(struct_id) {
                if !structs_compatible(existing, struct_def) {
                    conflicts.push(format!("Incompatible struct: {}", struct_id));
                }
            }
        }

        if conflicts.is_empty() {
            Ok(())
        } else {
            Err(conflicts)
        }
    }
}

/// Package summary for fast imports and dependency resolution
#[derive(Debug, Clone)]
pub struct PackageSummary {
    pub package_name: String,
    pub exported_traits: HashMap<TypeNameId, TraitDefinition>,
    pub exported_structs: HashMap<TypeNameId, StructDefinition>,
    pub exported_functions: HashMap<String, FunctionSignature>, // TODO: define FunctionSignature
    pub exported_implementations: Vec<ImplBlock>,
    pub type_context_summary: TypeContextSummary, // TODO: define TypeContextSummary
}

impl PackageSummary {
    /// Convert a package summary back into a compilation result for merging
    /// This allows importing pre-compiled packages efficiently
    pub fn to_compilation_result(&self, compiler_env: &CompilerEnvironment) -> CompilationResult {
        CompilationResult {
            compilation_order: vec![], // Package summaries don't need compilation order
            type_context: self
                .type_context_summary
                .to_unification_context(compiler_env), // TODO: implement
            traits: self.exported_traits.clone(),
            structs: self.exported_structs.clone(),
            implementations: self.exported_implementations.clone(),
            typed_programs: HashMap::new(), // Package summaries don't include typed AST
        }
    }
}

/// Lightweight type context summary for package interfaces
#[derive(Debug, Clone)]
pub struct TypeContextSummary {
    // TODO: Define minimal type context information needed for package imports
    pub type_definitions: HashMap<String, String>, // type_name -> type_definition
    pub trait_implementations: HashMap<String, Vec<String>>, // trait_name -> [implementing_type_names]
}

impl TypeContextSummary {
    /// Convert this summary back to a UnificationContext for merging
    pub fn to_unification_context(&self, compiler_env: &CompilerEnvironment) -> UnificationContext {
        let context = UnificationContext::new();

        // Reconstruct trait implementations from the summary
        for (trait_name, implementing_types) in &self.trait_implementations {
            let trait_id = compiler_env.intern_type_name(trait_name);
            let trait_structured = crate::unification::StructuredType::Simple(trait_id);

            for type_name in implementing_types {
                let type_id = compiler_env.intern_type_name(type_name);
                let impl_structured = crate::unification::StructuredType::Simple(type_id);

                // Register trait implementation using CompilerEnvironment
                compiler_env
                    .register_trait_implementation(impl_structured, trait_structured.clone());
            }
        }

        // TODO: Reconstruct type definitions when we have a proper format for them

        context
    }
}

/// Function signature for package exports
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<(String, String)>, // (param_name, type_name)
    pub return_type: String,
    pub generic_params: Vec<String>,
    pub constraints: Vec<String>, // "T: Display" etc
}

/// Check if two trait definitions are compatible for merging
fn traits_compatible(trait1: &TraitDefinition, trait2: &TraitDefinition) -> bool {
    // For now, require exact equality - in future we could allow compatible extensions
    trait1 == trait2
}

/// Check if two struct definitions are compatible for merging
fn structs_compatible(struct1: &StructDefinition, struct2: &StructDefinition) -> bool {
    // For now, require exact equality - in future we could allow compatible extensions
    struct1 == struct2
}

impl ProgramCollection {
    /// Create a new empty program collection
    pub fn new() -> Self {
        Self {
            programs: HashMap::new(),
            sources: HashMap::new(),
        }
    }

    /// Add a program to the collection
    pub fn add_program(&mut self, file_path: String, program: Program, source: String) {
        self.programs.insert(file_path.clone(), program);
        self.sources.insert(file_path, source);
    }

    /// Add multiple programs from a map
    pub fn add_programs(&mut self, programs: HashMap<String, (Program, String)>) {
        for (file_path, (program, source)) in programs {
            self.add_program(file_path, program, source);
        }
    }

    /// Create a collection from the core library
    pub fn from_core_library() -> Self {
        // Use the cached core library collection directly
        crate::core_library::load_core_library_collection()
    }

    /// Create a collection from a single program
    pub fn from_single_program(file_path: String, program: Program, source: String) -> Self {
        let mut collection = Self::new();
        collection.add_program(file_path, program, source);
        collection
    }

    /// Get all file paths in the collection
    pub fn file_paths(&self) -> Vec<String> {
        self.programs.keys().cloned().collect()
    }

    /// Get a program by file path
    pub fn get_program(&self, file_path: &str) -> Option<&Program> {
        self.programs.get(file_path)
    }

    /// Get source code by file path
    pub fn get_source(&self, file_path: &str) -> Option<&String> {
        self.sources.get(file_path)
    }
}
