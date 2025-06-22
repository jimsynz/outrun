//! Program collection management for multi-program compilation
//!
//! This module provides data structures and utilities for managing collections
//! of Outrun programs during compilation, including source code management and
//! compilation result tracking.

use crate::compilation::function_registry::FunctionRegistry;
use crate::types::TypeId;
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
#[derive(Debug)]
pub struct CompilationResult {
    /// Programs in dependency order
    pub compilation_order: Vec<String>,
    /// Type checking context with all resolved types, expression types, and span mappings
    pub type_context: UnificationContext,
    /// Collected traits from all programs (trait name TypeId -> definition)
    pub traits: HashMap<TypeId, TraitDefinition>,
    /// Collected structs from all programs (struct name TypeId -> definition)
    pub structs: HashMap<TypeId, StructDefinition>,
    /// Collected implementations from all programs
    pub implementations: Vec<ImplBlock>,
    /// Hierarchical function registry with trait, impl, and module functions
    pub function_registry: FunctionRegistry,
    /// Typed AST for all programs (filename -> typed program)
    pub typed_programs: HashMap<String, crate::checker::TypedProgram>,
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
