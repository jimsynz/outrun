//! Multi-program compiler for Outrun
//!
//! This module implements a general-purpose compiler that can handle collections
//! of Outrun programs (including both user code and core library). It uses
//! dependency resolution and phase-specific visitors to ensure correct
//! compilation order and type checking.

use crate::dependency_graph::DependencyGraph;
use crate::error::TypeError;
use crate::types::TypeId;
use crate::unification::UnificationContext;
use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{
    FunctionDefinition, FunctionVisibility, ImplBlock, Item, ItemKind, Program, StructDefinition,
    TraitDefinition,
};
use std::collections::HashMap;

// Import the desugarer for transforming operators to function calls
use crate::desugaring::DesugaringVisitor;

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
    /// Type checking context with all resolved types
    pub type_context: UnificationContext,
    /// Collected traits from all programs (trait name TypeId -> definition)
    pub traits: HashMap<TypeId, TraitDefinition>,
    /// Collected structs from all programs (struct name TypeId -> definition)
    pub structs: HashMap<TypeId, StructDefinition>,
    /// Collected implementations from all programs
    pub implementations: Vec<ImplBlock>,
    /// Hierarchical function registry with trait, impl, and module functions
    pub function_registry: FunctionRegistry,
}

/// Multi-program compiler that handles dependency resolution and phase-based compilation
#[derive(Debug)]
pub struct MultiProgramCompiler {
    /// Dependency graph for ordering
    dependency_graph: DependencyGraph,
    /// Unification context for type checking
    unification_context: UnificationContext,
    /// Error accumulator
    errors: Vec<TypeError>,
}

impl Default for MultiProgramCompiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Visitor for extracting trait definitions (Phase 1)
#[derive(Default)]
pub struct TraitExtractionVisitor {
    pub traits: HashMap<String, TraitDefinition>,
}

/// Visitor for extracting struct definitions (Phase 2)
#[derive(Default)]
pub struct StructExtractionVisitor {
    pub structs: HashMap<String, StructDefinition>,
}

/// Visitor for extracting impl blocks (Phase 3)
#[derive(Default)]
pub struct ImplExtractionVisitor {
    pub implementations: Vec<ImplBlock>,
}

/// TypeId-based function registry that organizes functions by their module context
#[derive(Debug, Default, Clone)]
pub struct FunctionRegistry {
    /// Functions defined in traits: trait_type_id -> function_name -> definition
    pub trait_functions: HashMap<TypeId, HashMap<String, FunctionDefinition>>,
    /// Functions defined in impl blocks: (trait_type_id, impl_type_id) -> function_name -> definition  
    pub impl_functions: HashMap<(TypeId, TypeId), HashMap<String, FunctionDefinition>>,
    /// Functions defined in type modules: module_type_id -> function_name -> definition
    pub module_functions: HashMap<TypeId, HashMap<String, FunctionDefinition>>,
}

impl FunctionRegistry {
    /// Look up a function by qualified name (e.g., "Option.some?", "List.head")
    /// Note: module_name should be the base type name without generics (e.g., "List" not "List<T>")
    pub fn lookup_qualified_function(
        &self,
        module_type_id: TypeId,
        function_name: &str,
    ) -> Option<&FunctionDefinition> {
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
    ) -> Option<&FunctionDefinition> {
        self.impl_functions
            .get(&(trait_type_id, impl_type_id))
            .and_then(|funcs| funcs.get(function_name))
    }

    /// Get total number of functions across all registries
    pub fn len(&self) -> usize {
        let trait_count: usize = self.trait_functions.values().map(|m| m.len()).sum();
        let impl_count: usize = self.impl_functions.values().map(|m| m.len()).sum();
        let module_count: usize = self.module_functions.values().map(|m| m.len()).sum();
        trait_count + impl_count + module_count
    }

    /// Check if the registry is empty
    pub fn is_empty(&self) -> bool {
        self.trait_functions.is_empty()
            && self.impl_functions.is_empty()
            && self.module_functions.is_empty()
    }

    /// Look up a local function in the current module context
    /// This searches both trait functions and module functions for the given function name
    pub fn lookup_local_function(&self, function_name: &str) -> Option<&FunctionDefinition> {
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
}

/// Visitor for extracting function definitions (Phase 4)
pub struct FunctionExtractionVisitor {
    pub registry: FunctionRegistry,
    /// Context for resolving type names to TypeIds
    pub context: UnificationContext,
}

impl FunctionExtractionVisitor {
    pub fn new(context: UnificationContext) -> Self {
        Self {
            registry: FunctionRegistry::default(),
            context,
        }
    }

    /// Convert trait function signature to function definition for type checking
    fn trait_signature_to_function_def(
        signature: &outrun_parser::FunctionSignature,
    ) -> FunctionDefinition {
        FunctionDefinition {
            attributes: signature.attributes.clone(),
            visibility: FunctionVisibility::Public,
            name: signature.name.clone(),
            parameters: signature.parameters.clone(),
            return_type: signature.return_type.clone(),
            guard: signature.guard.clone(),
            body: outrun_parser::Block {
                statements: vec![], // Empty body for signatures
                span: signature.span,
            },
            span: signature.span,
        }
    }
}

/// Visitor for type checking expressions (Phase 5)
pub struct TypeCheckingVisitor {
    pub context: UnificationContext,
    pub errors: Vec<TypeError>,
    /// TypeId-based function registry for validation
    pub function_registry: FunctionRegistry,
    /// Struct definitions for constructor validation (struct name TypeId -> definition)
    pub structs: HashMap<TypeId, StructDefinition>,
    /// Trait definitions for impl validation (trait name TypeId -> definition)
    pub traits: HashMap<TypeId, TraitDefinition>,
    /// Variable scope stack for tracking variable types
    variable_scopes: Vec<HashMap<String, crate::unification::StructuredType>>,
    /// Type parameter scope stack for tracking generic type parameters (Self, T, E, K, V, etc.)
    type_parameter_scopes: Vec<HashMap<String, crate::unification::StructuredType>>,
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

impl MultiProgramCompiler {
    /// Create a new compiler
    pub fn new() -> Self {
        Self {
            dependency_graph: DependencyGraph::new(),
            unification_context: UnificationContext::new(),
            errors: Vec::new(),
        }
    }

    /// Create a new compiler with intrinsic functions pre-loaded
    pub fn new_with_intrinsics() -> Self {
        let mut compiler = Self::new();
        compiler.bootstrap_intrinsic_functions();
        compiler
    }

    /// Bootstrap intrinsic function signatures into the compiler
    fn bootstrap_intrinsic_functions(&mut self) {
        // Intrinsic functions will be added during extract_functions phase
        // This method currently just marks that intrinsics should be included
    }

    /// Compile a collection of programs using phased compilation
    pub fn compile(
        &mut self,
        collection: &ProgramCollection,
    ) -> Result<CompilationResult, Vec<TypeError>> {
        // Step 1: Build dependency graph and get compilation order
        let compilation_order = self.resolve_dependencies(collection)?;

        // Step 2: Phase 1 - Extract all traits
        let traits = self.extract_traits(collection, &compilation_order)?;

        // Step 3: Phase 2 - Extract all structs
        let structs = self.extract_structs(collection, &compilation_order)?;

        // Step 4: Phase 3 - Extract all implementations
        let implementations = self.extract_implementations(collection, &compilation_order)?;

        // Step 5: Phase 4 - Extract all functions
        let functions = self.extract_functions(collection, &compilation_order)?;

        // Step 6: Phase 5 - Type check everything
        self.type_check_all(
            collection,
            &compilation_order,
            &traits,
            &structs,
            &implementations,
            &functions,
        )?;

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(CompilationResult {
            compilation_order,
            type_context: self.unification_context.clone(),
            traits,
            structs,
            implementations,
            function_registry: functions,
        })
    }

    /// Resolve dependencies and get compilation order
    fn resolve_dependencies(
        &mut self,
        collection: &ProgramCollection,
    ) -> Result<Vec<String>, Vec<TypeError>> {
        eprintln!(
            "DEBUG: resolve_dependencies called with {} programs",
            collection.programs.len()
        );
        // Phase 1: Add all programs to collect type definitions (without dependency validation)
        for (file_path, program) in &collection.programs {
            if let Err(err) = self
                .dependency_graph
                .add_program(file_path.clone(), program.clone())
            {
                use crate::dependency_graph::DependencyError;
                match err {
                    DependencyError::ConflictingDefinition { type_name, files } => {
                        self.errors.push(TypeError::internal(format!(
                            "Type {} is defined in multiple files: {}",
                            type_name,
                            files.join(", ")
                        )));
                    }
                    // add_program shouldn't return UnresolvedType or CircularDependency errors
                    _ => {
                        self.errors.push(TypeError::internal(format!(
                            "Unexpected dependency error in {}: {:?}",
                            file_path, err
                        )));
                    }
                }
            }
        }

        // Phase 2: Build dependency edges and validate that all referenced types exist
        self.dependency_graph.build_dependency_edges();

        // Phase 3: Check for unresolved dependencies manually
        self.validate_all_dependencies(collection)?;

        // Resolve dependencies
        eprintln!("DEBUG: About to call dependency_graph.resolve()");
        let result = self.dependency_graph.resolve_with_trait_cycles_allowed();
        eprintln!("DEBUG: Dependency resolution result - compilation_order: {:?}, circular_deps: {:?}, unresolved_deps: {:?}", 
                  result.compilation_order, result.circular_dependencies, result.unresolved_dependencies);

        // Check for fatal circular dependencies
        if !result.circular_dependencies.is_empty() {
            let structural_cycles: Vec<_> = result
                .circular_dependencies
                .into_iter()
                .filter(|cycle| self.dependency_graph.is_structural_cycle(cycle))
                .collect();

            if !structural_cycles.is_empty() {
                for cycle in structural_cycles {
                    self.errors.push(TypeError::internal(format!(
                        "Structural circular dependency: {}",
                        cycle.join(" -> ")
                    )));
                }
            }
        }

        if !self.errors.is_empty() {
            eprintln!(
                "DEBUG: Errors during dependency resolution: {:?}",
                self.errors
            );
            return Err(self.errors.clone());
        }

        eprintln!(
            "DEBUG: Dependency resolution successful, order: {:?}",
            result.compilation_order
        );
        Ok(result.compilation_order)
    }

    /// Validate that all type dependencies exist after all programs are loaded
    fn validate_all_dependencies(
        &mut self,
        collection: &ProgramCollection,
    ) -> Result<(), Vec<TypeError>> {
        for program in collection.programs.values() {
            for item in &program.items {
                if let outrun_parser::ItemKind::ImplBlock(impl_def) = &item.kind {
                    // Check that the trait being implemented exists
                    let trait_name = if impl_def.trait_spec.path.len() == 1 {
                        impl_def.trait_spec.path[0].name.clone()
                    } else {
                        impl_def
                            .trait_spec
                            .path
                            .iter()
                            .map(|id| id.name.as_str())
                            .collect::<Vec<_>>()
                            .join(".")
                    };

                    // Check if trait exists in dependency graph's type definitions
                    if !self.dependency_graph.has_type_definition(&trait_name)
                        && !self.is_builtin_type(&trait_name)
                    {
                        self.errors.push(TypeError::UndefinedTrait {
                            span: miette::SourceSpan::new(0.into(), 0), // TODO: Get proper span
                            trait_name,
                        });
                    }
                }
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(())
    }

    /// Check if a type name is a built-in type
    fn is_builtin_type(&self, type_name: &str) -> bool {
        matches!(
            type_name,
            "Self"
                | "Any"
                | "Outrun.Core.Integer64"
                | "Outrun.Core.Float64"
                | "Outrun.Core.Boolean"
                | "Outrun.Core.String"
                | "Outrun.Core.Atom"
        )
    }

    /// Phase 1: Extract all trait definitions
    fn extract_traits(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<HashMap<TypeId, TraitDefinition>, Vec<TypeError>> {
        let mut visitor = TraitExtractionVisitor::default();

        for file_path in order {
            if let Some(program) = collection.get_program(file_path) {
                <TraitExtractionVisitor as Visitor<()>>::visit_program(&mut visitor, program)
                    .map_err(|e| vec![e])?;
            }
        }

        // Convert string keys to TypeId and register traits in unification context
        let mut typed_traits = HashMap::new();
        for (trait_name, trait_def) in visitor.traits {
            let trait_id = self
                .unification_context
                .type_interner
                .intern_type(&trait_name);
            self.unification_context
                .trait_registry
                .register_trait(trait_id);
            typed_traits.insert(trait_id, trait_def);
        }

        Ok(typed_traits)
    }

    /// Phase 2: Extract all struct definitions  
    fn extract_structs(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<HashMap<TypeId, StructDefinition>, Vec<TypeError>> {
        let mut visitor = StructExtractionVisitor::default();

        for file_path in order {
            if let Some(program) = collection.get_program(file_path) {
                <StructExtractionVisitor as Visitor<()>>::visit_program(&mut visitor, program)
                    .map_err(|e| vec![e])?;
            }
        }

        // Convert string keys to TypeId and register struct types
        let mut typed_structs = HashMap::new();
        for (struct_name, struct_def) in visitor.structs {
            let struct_id = self
                .unification_context
                .type_interner
                .intern_type(&struct_name);
            typed_structs.insert(struct_id, struct_def);
        }

        Ok(typed_structs)
    }

    /// Phase 3: Extract all impl blocks
    fn extract_implementations(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<Vec<ImplBlock>, Vec<TypeError>> {
        let mut visitor = ImplExtractionVisitor::default();

        for file_path in order {
            if let Some(program) = collection.get_program(file_path) {
                <ImplExtractionVisitor as Visitor<()>>::visit_program(&mut visitor, program)
                    .map_err(|e| vec![e])?;
            }
        }

        // Register trait implementations in unification context
        for impl_block in &visitor.implementations {
            let trait_name = self.resolve_type_spec(&impl_block.trait_spec);
            let type_name = self.resolve_type_spec(&impl_block.type_spec);

            let trait_id = self
                .unification_context
                .type_interner
                .intern_type(&trait_name);
            let type_id = self
                .unification_context
                .type_interner
                .intern_type(&type_name);

            self.unification_context
                .trait_registry
                .register_implementation(type_id, trait_id);
        }

        Ok(visitor.implementations)
    }

    /// Phase 4: Extract all function definitions
    fn extract_functions(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<FunctionRegistry, Vec<TypeError>> {
        let mut visitor = FunctionExtractionVisitor::new(self.unification_context.clone());

        // Extract functions from all user programs
        for file_path in order {
            if let Some(program) = collection.get_program(file_path) {
                <FunctionExtractionVisitor as Visitor<()>>::visit_program(&mut visitor, program)
                    .map_err(|e| vec![e])?;
            }
        }

        // Add intrinsic function signatures to the module functions registry
        let intrinsic_module_type_id = self
            .unification_context
            .type_interner
            .intern_type("Outrun.Intrinsic");
        let intrinsic_functions = crate::intrinsics::IntrinsicRegistry::create_all_signatures(
            &mut self.unification_context,
        );

        // Setup core concrete types and automatically implement Any trait
        crate::intrinsics::IntrinsicRegistry::setup_core_types(&mut self.unification_context);

        let mut intrinsic_module_functions = HashMap::new();
        for (_func_atom_id, func_def) in intrinsic_functions {
            // Intrinsic functions have simple names like "atom_eq", use them directly
            let function_name = func_def.name.name.clone();
            intrinsic_module_functions.insert(function_name, func_def);
        }

        visitor
            .registry
            .module_functions
            .insert(intrinsic_module_type_id, intrinsic_module_functions);

        Ok(visitor.registry)
    }

    /// Phase 5: Type check everything
    fn type_check_all(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
        traits: &HashMap<TypeId, TraitDefinition>,
        structs: &HashMap<TypeId, StructDefinition>,
        _implementations: &[ImplBlock],
        function_registry: &FunctionRegistry,
    ) -> Result<(), Vec<TypeError>> {
        let mut visitor = TypeCheckingVisitor {
            context: self.unification_context.clone(),
            errors: Vec::new(),
            function_registry: function_registry.clone(),
            structs: structs.clone(),
            traits: traits.clone(),
            variable_scopes: vec![HashMap::new()], // Start with global scope
            type_parameter_scopes: vec![HashMap::new()], // Start with global type parameter scope
        };

        // Type check all programs in dependency order
        for file_path in order {
            if let Some(program) = collection.get_program(file_path) {
                // Desugar the program before type checking
                let desugared_program = DesugaringVisitor::desugar_program(program.clone());

                if let Err(error) = <TypeCheckingVisitor as Visitor<()>>::visit_program(
                    &mut visitor,
                    &desugared_program,
                ) {
                    visitor.errors.push(error);
                }
            }
        }

        // Accumulate errors from visitor
        self.errors.extend(visitor.errors);

        // Update our context with visitor's context changes
        self.unification_context = visitor.context;

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    /// Helper to resolve type spec to string
    fn resolve_type_spec(&self, type_spec: &outrun_parser::TypeSpec) -> String {
        type_spec
            .path
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }
}

// Implement visitor traits for our phase-specific visitors

impl<T> Visitor<T> for TraitExtractionVisitor {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::TraitDefinition(trait_def) => {
                let trait_name = trait_def.name_as_string();
                self.traits.insert(trait_name, trait_def.clone());
                Ok(())
            }
            _ => Ok(()), // Ignore non-trait items in this phase
        }
    }
}

impl<T> Visitor<T> for StructExtractionVisitor {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::StructDefinition(struct_def) => {
                let struct_name = struct_def.name_as_string();
                self.structs.insert(struct_name, struct_def.clone());
                Ok(())
            }
            _ => Ok(()), // Ignore non-struct items in this phase
        }
    }
}

impl<T> Visitor<T> for ImplExtractionVisitor {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::ImplBlock(impl_block) => {
                self.implementations.push(impl_block.clone());
                Ok(())
            }
            _ => Ok(()), // Ignore non-impl items in this phase
        }
    }
}

impl<T> Visitor<T> for FunctionExtractionVisitor {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::FunctionDefinition(func_def) => {
                // Top-level functions are module-level functions
                // We need to determine the module from the file path - for now skip
                let _func_name = func_def.name.name.clone();
                // TODO: Determine module TypeId from file path and register
                // self.registry.module_functions.entry(module_type_id).or_default().insert(func_name, func_def.clone());
                Ok(())
            }
            ItemKind::TraitDefinition(trait_def) => {
                // Get trait TypeId
                let trait_name = trait_def.name_as_string();
                let trait_type_id = self.context.type_interner.intern_type(&trait_name);

                // Extract functions from trait definitions
                for trait_func in &trait_def.functions {
                    match trait_func {
                        outrun_parser::TraitFunction::Definition(func_def) => {
                            let func_name = func_def.name.name.clone();
                            self.registry
                                .trait_functions
                                .entry(trait_type_id)
                                .or_default()
                                .insert(func_name, func_def.clone());
                        }
                        outrun_parser::TraitFunction::StaticDefinition(static_func) => {
                            // Convert StaticFunctionDefinition to FunctionDefinition
                            let func_def = FunctionDefinition {
                                attributes: static_func.attributes.clone(),
                                visibility: FunctionVisibility::Public,
                                name: static_func.name.clone(),
                                parameters: static_func.parameters.clone(),
                                return_type: static_func.return_type.clone(),
                                guard: None,
                                body: static_func.body.clone(),
                                span: static_func.span,
                            };
                            let func_name = func_def.name.name.clone();
                            self.registry
                                .trait_functions
                                .entry(trait_type_id)
                                .or_default()
                                .insert(func_name, func_def);
                        }
                        outrun_parser::TraitFunction::Signature(signature) => {
                            // Convert signature to function definition for type checking
                            let func_def = Self::trait_signature_to_function_def(signature);
                            let func_name = func_def.name.name.clone();
                            self.registry
                                .trait_functions
                                .entry(trait_type_id)
                                .or_default()
                                .insert(func_name, func_def);
                        }
                    }
                }
                Ok(())
            }
            ItemKind::ImplBlock(impl_block) => {
                // Extract functions from impl blocks
                let trait_name = impl_block
                    .trait_spec
                    .path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                let impl_type_name = impl_block
                    .type_spec
                    .path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                let trait_type_id = self.context.type_interner.intern_type(&trait_name);
                let impl_type_id = self.context.type_interner.intern_type(&impl_type_name);

                for func_def in &impl_block.methods {
                    let func_name = func_def.name.name.clone();
                    self.registry
                        .impl_functions
                        .entry((trait_type_id, impl_type_id))
                        .or_default()
                        .insert(func_name, func_def.clone());
                }
                Ok(())
            }
            _ => Ok(()), // Ignore other items in this phase
        }
    }
}

impl<T> Visitor<T> for TypeCheckingVisitor {
    fn visit_expression(&mut self, expr: &outrun_parser::Expression) -> VisitorResult {
        // Type check this expression using the unification system
        match self.check_expression_type(expr) {
            Ok(_) => {
                // Continue traversing child expressions
                crate::visitor::walk_expression::<Self, ()>(self, expr)
            }
            Err(error) => {
                self.errors.push(error);
                // Continue checking other expressions despite this error
                let _ = crate::visitor::walk_expression::<Self, ()>(self, expr);
                Ok(())
            }
        }
    }

    fn visit_let_binding(&mut self, let_binding: &outrun_parser::LetBinding) -> VisitorResult {
        // Custom let binding handling with type hints
        // If there's a type annotation, resolve it first to use as a hint
        let type_hint = if let Some(type_annotation) = &let_binding.type_annotation {
            match self.resolve_type_annotation(type_annotation) {
                Ok(hint) => Some(hint),
                Err(error) => {
                    self.errors.push(error);
                    None
                }
            }
        } else {
            None
        };

        // Check the expression type with the hint
        let expr_type = match self
            .check_expression_type_with_hint(&let_binding.expression, type_hint.as_ref())
        {
            Ok(t) => t,
            Err(error) => {
                self.errors.push(error);
                // Continue processing
                let _ = crate::visitor::walk_let_binding::<Self, ()>(self, let_binding);
                return Ok(());
            }
        };

        // If there was a type annotation, validate it matches the expression
        if let Some(expected_type) = &type_hint {
            if !crate::unification::unify_structured_types(&expr_type, expected_type, &self.context)
                .unwrap_or(false)
            {
                self.errors.push(TypeError::type_mismatch(
                    expected_type.to_string_representation(&self.context.type_interner),
                    expr_type.to_string_representation(&self.context.type_interner),
                    crate::error::span_to_source_span(let_binding.expression.span),
                ));
            }
        }

        // Visit the pattern (but not the expression since we already checked it with hint)
        <Self as crate::visitor::Visitor<()>>::visit_pattern(self, &let_binding.pattern)?;

        Ok(())
    }

    fn visit_function_definition(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
    ) -> VisitorResult {
        // Type check the function signature and body, collecting let-bound variables
        let let_bound_variables = match self.check_function_definition_with_variables(func) {
            Ok(variables) => variables,
            Err(error) => {
                self.errors.push(error);
                std::collections::HashMap::new()
            }
        };

        // Create a new scope for function parameters and let-bound variables
        self.push_scope();

        // Register function parameters in the scope
        for param in &func.parameters {
            match self.resolve_type_annotation(&param.type_annotation) {
                Ok(param_type) => {
                    self.register_variable(param.name.name.clone(), param_type);
                }
                Err(error) => {
                    self.errors.push(error);
                }
            }
        }

        // Register let-bound variables from type checking phase
        for (var_name, var_type) in let_bound_variables {
            self.register_variable(var_name, var_type);
        }

        // Continue traversing the function body with all variables in scope
        let result = crate::visitor::walk_function_definition::<Self, ()>(self, func);

        // Pop the function scope
        self.pop_scope();

        result
    }

    fn visit_struct_definition(
        &mut self,
        struct_def: &outrun_parser::StructDefinition,
    ) -> VisitorResult {
        // Establish type parameter scope for this struct definition
        self.push_type_parameter_scope();

        // Register generic parameters (T, E, K, V, etc.) but NOT Self
        if let Some(ref generic_params) = struct_def.generic_params {
            for param in &generic_params.params {
                let param_type_id = self.context.type_interner.intern_type(&param.name.name);
                self.register_type_parameter(
                    param.name.name.clone(),
                    crate::unification::StructuredType::Simple(param_type_id),
                );
            }
        }

        // Validate struct field types exist (now with generic parameters in scope)
        if let Err(error) = self.check_struct_definition(struct_def) {
            self.errors.push(error);
        }

        // Restore previous type parameter scope
        self.pop_type_parameter_scope();

        Ok(())
    }

    fn visit_trait_definition(
        &mut self,
        trait_def: &outrun_parser::TraitDefinition,
    ) -> VisitorResult {
        // Establish type parameter scope for this trait definition FIRST
        self.push_type_parameter_scope();

        // Register Self parameter
        let trait_name = trait_def.name_as_string();
        let trait_type_id = self.context.type_interner.intern_type(&trait_name);
        self.register_type_parameter(
            "Self".to_string(),
            crate::unification::StructuredType::Simple(trait_type_id),
        );

        // Register generic parameters (T, E, K, V, etc.)
        if let Some(ref generic_params) = trait_def.generic_params {
            for param in &generic_params.params {
                let param_type_id = self.context.type_interner.intern_type(&param.name.name);
                self.register_type_parameter(
                    param.name.name.clone(),
                    crate::unification::StructuredType::Simple(param_type_id),
                );
            }
        }

        // Now validate trait function signatures and constraints with Self context available
        if let Err(error) = self.check_trait_definition(trait_def) {
            self.errors.push(error);
        }

        // Visit trait functions with Self context established
        // Note: trait definitions are typically leaf nodes, so we manually traverse functions
        for trait_function in &trait_def.functions {
            match trait_function {
                outrun_parser::TraitFunction::Definition(func) => {
                    let _ = <Self as crate::visitor::Visitor<()>>::visit_function_definition(
                        self, func,
                    );
                    // Errors are already collected in visit_function_definition
                }
                outrun_parser::TraitFunction::Signature(_) => {
                    // Function signatures don't have bodies to traverse
                }
                outrun_parser::TraitFunction::StaticDefinition(static_func) => {
                    // Convert StaticFunctionDefinition to FunctionDefinition for visitor
                    // TODO: StaticFunctionDefinition should have a guard field for consistency
                    let func_def = outrun_parser::FunctionDefinition {
                        attributes: static_func.attributes.clone(),
                        visibility: outrun_parser::FunctionVisibility::Public,
                        name: static_func.name.clone(),
                        parameters: static_func.parameters.clone(),
                        return_type: static_func.return_type.clone(),
                        guard: None, // TODO: Static functions should support guards
                        body: static_func.body.clone(),
                        span: static_func.span,
                    };
                    let _ = <Self as crate::visitor::Visitor<()>>::visit_function_definition(
                        self, &func_def,
                    );
                    // Errors are already collected in visit_function_definition
                }
            }
        }

        // Restore previous type parameter scope
        self.pop_type_parameter_scope();

        Ok(())
    }

    fn visit_impl_block(&mut self, impl_block: &outrun_parser::ImplBlock) -> VisitorResult {
        // Validate trait implementation
        if let Err(error) = self.check_impl_block(impl_block) {
            self.errors.push(error);
        }

        // Establish type parameter scope for this impl block
        self.push_type_parameter_scope();

        // Register Self parameter with full structured type
        match self.resolve_type_spec(&impl_block.type_spec) {
            Ok(impl_type) => {
                self.register_type_parameter("Self".to_string(), impl_type);
            }
            Err(error) => {
                self.errors.push(error);
            }
        }

        // Register generic parameters from impl block (e.g., impl<T> Option<T> for Some<T>)
        if let Some(ref generic_params) = impl_block.generic_params {
            for param in &generic_params.params {
                let param_type_id = self.context.type_interner.intern_type(&param.name.name);
                self.register_type_parameter(
                    param.name.name.clone(),
                    crate::unification::StructuredType::Simple(param_type_id),
                );
            }
        }

        // Continue traversing impl functions with Self context established
        let result = crate::visitor::walk_impl_block::<Self, ()>(self, impl_block);

        // Restore previous type parameter scope
        self.pop_type_parameter_scope();

        result
    }
}

impl TypeCheckingVisitor {
    /// Push a new scope onto the variable scope stack
    fn push_scope(&mut self) {
        self.variable_scopes.push(HashMap::new());
    }

    /// Pop the current scope from the variable scope stack
    fn pop_scope(&mut self) {
        if self.variable_scopes.len() > 1 {
            self.variable_scopes.pop();
        }
    }

    /// Register a variable in the current scope
    fn register_variable(&mut self, name: String, var_type: crate::unification::StructuredType) {
        if let Some(current_scope) = self.variable_scopes.last_mut() {
            current_scope.insert(name, var_type);
        }
    }

    /// Check if a pattern matches the given type and extract bound variables
    fn check_pattern_type(
        &mut self,
        pattern: &outrun_parser::Pattern,
        expected_type: &crate::unification::StructuredType,
    ) -> Result<Vec<(String, crate::unification::StructuredType)>, TypeError> {
        match pattern {
            outrun_parser::Pattern::Identifier(id) => {
                // Identifier patterns bind a variable to the full type
                Ok(vec![(id.name.clone(), expected_type.clone())])
            }
            outrun_parser::Pattern::Literal(literal_pattern) => {
                // Check if the literal type matches the expected type
                let literal_type = self.check_literal_pattern_type(literal_pattern)?;
                if crate::unification::unify_structured_types(
                    &literal_type,
                    expected_type,
                    &self.context,
                )
                .unwrap_or(false)
                {
                    Ok(vec![]) // Literals don't bind variables
                } else {
                    Err(TypeError::type_mismatch(
                        expected_type.to_string_representation(&self.context.type_interner),
                        literal_type.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(literal_pattern.span),
                    ))
                }
            }
            outrun_parser::Pattern::Tuple(tuple_pattern) => {
                // Check if expected type is a tuple
                match expected_type {
                    crate::unification::StructuredType::Tuple(element_types) => {
                        if tuple_pattern.elements.len() != element_types.len() {
                            return Err(TypeError::type_mismatch(
                                format!("Tuple with {} elements", element_types.len()),
                                format!(
                                    "Tuple pattern with {} elements",
                                    tuple_pattern.elements.len()
                                ),
                                crate::error::span_to_source_span(tuple_pattern.span),
                            ));
                        }

                        let mut bound_variables = Vec::new();
                        for (pattern_element, element_type) in
                            tuple_pattern.elements.iter().zip(element_types.iter())
                        {
                            let element_vars =
                                self.check_pattern_type(pattern_element, element_type)?;
                            bound_variables.extend(element_vars);
                        }
                        Ok(bound_variables)
                    }
                    _ => Err(TypeError::type_mismatch(
                        "Tuple type".to_string(),
                        expected_type.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(tuple_pattern.span),
                    )),
                }
            }
            outrun_parser::Pattern::Struct(struct_pattern) => {
                // Check if expected type is a struct that matches the pattern
                self.check_struct_pattern_type(struct_pattern, expected_type)
            }
            outrun_parser::Pattern::List(list_pattern) => {
                // Check if expected type is a list that matches the pattern
                self.check_list_pattern_type(list_pattern, expected_type)
            }
        }
    }

    /// Check literal pattern type
    fn check_literal_pattern_type(
        &mut self,
        literal_pattern: &outrun_parser::LiteralPattern,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match &literal_pattern.literal {
            outrun_parser::Literal::Boolean(_) => {
                let type_id = self
                    .context
                    .type_interner
                    .intern_type("Outrun.Core.Boolean");
                Ok(crate::unification::StructuredType::Simple(type_id))
            }
            outrun_parser::Literal::Integer(_) => {
                let type_id = self
                    .context
                    .type_interner
                    .intern_type("Outrun.Core.Integer64");
                Ok(crate::unification::StructuredType::Simple(type_id))
            }
            outrun_parser::Literal::Float(_) => {
                let type_id = self
                    .context
                    .type_interner
                    .intern_type("Outrun.Core.Float64");
                Ok(crate::unification::StructuredType::Simple(type_id))
            }
            outrun_parser::Literal::String(_) => {
                let type_id = self.context.type_interner.intern_type("Outrun.Core.String");
                Ok(crate::unification::StructuredType::Simple(type_id))
            }
            outrun_parser::Literal::Atom(_) => {
                let type_id = self.context.type_interner.intern_type("Outrun.Core.Atom");
                Ok(crate::unification::StructuredType::Simple(type_id))
            }
        }
    }

    /// Check struct pattern type and extract bound variables
    fn check_struct_pattern_type(
        &mut self,
        struct_pattern: &outrun_parser::StructPattern,
        expected_type: &crate::unification::StructuredType,
    ) -> Result<Vec<(String, crate::unification::StructuredType)>, TypeError> {
        // Resolve the struct type from the pattern's type path
        let struct_type_name = struct_pattern
            .type_path
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<&str>>()
            .join(".");

        let struct_type_id = self.context.type_interner.intern_type(&struct_type_name);
        let pattern_type = crate::unification::StructuredType::Simple(struct_type_id);

        // Check if pattern type matches expected type
        if !crate::unification::unify_structured_types(&pattern_type, expected_type, &self.context)
            .unwrap_or(false)
        {
            return Err(TypeError::type_mismatch(
                expected_type.to_string_representation(&self.context.type_interner),
                pattern_type.to_string_representation(&self.context.type_interner),
                crate::error::span_to_source_span(struct_pattern.span),
            ));
        }

        // Extract variables from struct fields
        let mut bound_variables = Vec::new();

        // Get struct definition to validate fields
        if let Some(struct_def) = self.structs.get(&struct_type_id).cloned() {
            for field_pattern in &struct_pattern.fields {
                // Find the field in the struct definition
                if let Some(field_def) = struct_def
                    .fields
                    .iter()
                    .find(|f| f.name.name == field_pattern.name.name)
                {
                    // Resolve field type
                    // For now, use a simple type mapping - this should be improved
                    let field_type_name = match &field_def.type_annotation {
                        outrun_parser::TypeAnnotation::Simple { path, .. } => path
                            .iter()
                            .map(|id| id.name.as_str())
                            .collect::<Vec<_>>()
                            .join("."),
                        _ => "Any".to_string(), // Fallback for complex types
                    };
                    let field_type_id = self.context.type_interner.intern_type(&field_type_name);
                    let field_type = crate::unification::StructuredType::Simple(field_type_id);

                    match &field_pattern.pattern {
                        Some(nested_pattern) => {
                            // Field has a nested pattern: field: pattern
                            let nested_vars =
                                self.check_pattern_type(nested_pattern, &field_type)?;
                            bound_variables.extend(nested_vars);
                        }
                        None => {
                            // Shorthand syntax: field is bound as variable
                            bound_variables.push((field_pattern.name.name.clone(), field_type));
                        }
                    }
                } else {
                    return Err(TypeError::InternalError {
                        span: crate::error::span_to_source_span(field_pattern.span),
                        message: format!(
                            "Field '{}' not found in struct '{}'",
                            field_pattern.name.name, struct_type_name
                        ),
                    });
                }
            }
        } else {
            return Err(TypeError::InternalError {
                span: crate::error::span_to_source_span(struct_pattern.span),
                message: format!("Struct type '{}' not found in registry", struct_type_name),
            });
        }

        Ok(bound_variables)
    }

    /// Check list pattern type and extract bound variables
    fn check_list_pattern_type(
        &mut self,
        list_pattern: &outrun_parser::ListPattern,
        expected_type: &crate::unification::StructuredType,
    ) -> Result<Vec<(String, crate::unification::StructuredType)>, TypeError> {
        // Check if expected type is a List<T>
        match expected_type {
            crate::unification::StructuredType::Generic { base, args } => {
                let list_type_id = self.context.type_interner.intern_type("List");
                if base != &list_type_id || args.len() != 1 {
                    return Err(TypeError::type_mismatch(
                        "List<T>".to_string(),
                        expected_type.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(list_pattern.span),
                    ));
                }

                let element_type = &args[0];
                let mut bound_variables = Vec::new();

                // Check each element pattern
                for element_pattern in &list_pattern.elements {
                    let element_vars = self.check_pattern_type(element_pattern, element_type)?;
                    bound_variables.extend(element_vars);
                }

                // Handle rest pattern if present
                if let Some(rest_id) = &list_pattern.rest {
                    // Rest pattern binds to the same list type
                    bound_variables.push((rest_id.name.clone(), expected_type.clone()));
                }

                Ok(bound_variables)
            }
            _ => Err(TypeError::type_mismatch(
                "List<T>".to_string(),
                expected_type.to_string_representation(&self.context.type_interner),
                crate::error::span_to_source_span(list_pattern.span),
            )),
        }
    }

    /// Push a new type parameter scope onto the stack
    fn push_type_parameter_scope(&mut self) {
        self.type_parameter_scopes.push(HashMap::new());
    }

    /// Pop the current type parameter scope from the stack
    fn pop_type_parameter_scope(&mut self) {
        if self.type_parameter_scopes.len() > 1 {
            self.type_parameter_scopes.pop();
        }
        // Type parameter scope popped - no additional cleanup needed
    }

    /// Register a type parameter in the current scope
    fn register_type_parameter(
        &mut self,
        name: String,
        structured_type: crate::unification::StructuredType,
    ) {
        // No additional context updates needed - unified system handles all type parameters
        // Insert into current scope
        if let Some(current_scope) = self.type_parameter_scopes.last_mut() {
            current_scope.insert(name, structured_type);
        }
    }

    /// Look up a type parameter in the scope stack (from current to global)
    fn lookup_type_parameter(&self, name: &str) -> Option<&crate::unification::StructuredType> {
        for scope in self.type_parameter_scopes.iter().rev() {
            if let Some(structured_type) = scope.get(name) {
                return Some(structured_type);
            }
        }
        None
    }

    /// Look up a variable type in the scope stack (from current to global)
    fn lookup_variable(&self, name: &str) -> Option<&crate::unification::StructuredType> {
        for scope in self.variable_scopes.iter().rev() {
            if let Some(var_type) = scope.get(name) {
                return Some(var_type);
            }
        }
        None
    }

    /// Check the type of an expression using the unification system
    fn check_expression_type(
        &mut self,
        expr: &outrun_parser::Expression,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        self.check_expression_type_with_hint(expr, None)
    }

    /// Check the type of an expression with an optional type hint
    fn check_expression_type_with_hint(
        &mut self,
        expr: &outrun_parser::Expression,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        use outrun_parser::ExpressionKind;

        match &expr.kind {
            ExpressionKind::Boolean(literal) => self.check_boolean_literal_type(literal),
            ExpressionKind::Integer(literal) => self.check_integer_literal_type(literal),
            ExpressionKind::Float(literal) => self.check_float_literal_type(literal),
            ExpressionKind::String(literal) => self.check_string_literal_type(literal),
            ExpressionKind::Atom(literal) => self.check_atom_literal_type(literal),
            ExpressionKind::Sigil(literal) => self.check_sigil_literal_type(literal),
            ExpressionKind::List(literal) => self.check_list_literal_type(literal, type_hint),
            ExpressionKind::Map(literal) => self.check_map_literal_type(literal, type_hint),
            ExpressionKind::Tuple(literal) => self.check_tuple_literal_type(literal),
            ExpressionKind::Struct(literal) => self.check_struct_literal_type(literal, type_hint),
            ExpressionKind::Identifier(var) => self.check_variable_type(var),
            ExpressionKind::QualifiedIdentifier(qual_id) => {
                self.check_qualified_identifier_type(qual_id)
            }
            ExpressionKind::FunctionCall(call) => self.check_function_call_type(call),
            ExpressionKind::IfExpression(if_expr) => self.check_if_expression_type(if_expr),
            ExpressionKind::CaseExpression(case_expr) => self.check_case_expression_type(case_expr),
            ExpressionKind::FieldAccess(field_access) => self.check_field_access_type(field_access),
            ExpressionKind::Parenthesized(inner_expr) => self.check_expression_type(inner_expr),
            ExpressionKind::BinaryOp(_) => {
                // Binary operations are desugared to function calls
                // They should not appear in type checking phase
                Err(TypeError::internal(
                    "Binary operation found during type checking - should be desugared".to_string(),
                ))
            }
            ExpressionKind::UnaryOp(_) => {
                // Unary operations are desugared to function calls
                // They should not appear in type checking phase
                Err(TypeError::internal(
                    "Unary operation found during type checking - should be desugared".to_string(),
                ))
            }
            ExpressionKind::TypeIdentifier(_) => {
                // Type identifiers shouldn't appear in expression context during type checking
                Err(TypeError::internal(
                    "Type identifier found in expression context during type checking".to_string(),
                ))
            }
            _ => {
                // For advanced features not yet implemented (macros, anonymous functions, etc.)
                Ok(crate::unification::StructuredType::Simple(
                    self.context.type_interner.intern_type("Unknown"),
                ))
            }
        }
    }

    /// Check boolean literal type
    fn check_boolean_literal_type(
        &mut self,
        _literal: &outrun_parser::BooleanLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self
            .context
            .type_interner
            .intern_type("Outrun.Core.Boolean");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check integer literal type
    fn check_integer_literal_type(
        &mut self,
        _literal: &outrun_parser::IntegerLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self
            .context
            .type_interner
            .intern_type("Outrun.Core.Integer64");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check float literal type
    fn check_float_literal_type(
        &mut self,
        _literal: &outrun_parser::FloatLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self
            .context
            .type_interner
            .intern_type("Outrun.Core.Float64");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check string literal type
    fn check_string_literal_type(
        &mut self,
        _literal: &outrun_parser::StringLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self.context.type_interner.intern_type("Outrun.Core.String");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check atom literal type
    fn check_atom_literal_type(
        &mut self,
        _literal: &outrun_parser::AtomLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self.context.type_interner.intern_type("Outrun.Core.Atom");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check sigil literal type
    fn check_sigil_literal_type(
        &mut self,
        _literal: &outrun_parser::SigilLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Sigils are processed at parse time and return specific types
        // For now, return String type - this should be enhanced to call the sigil's parse function
        let type_id = self.context.type_interner.intern_type("Outrun.Core.String");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check list literal type with optional type hint
    fn check_list_literal_type(
        &mut self,
        literal: &outrun_parser::ListLiteral,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Case 1: Empty list without type hint - ERROR
        if literal.elements.is_empty() && type_hint.is_none() {
            return Err(TypeError::CannotInferListType {
                span: crate::error::span_to_source_span(literal.span),
            });
        }

        // Case 2: Empty list with type hint - extract element type from hint
        if literal.elements.is_empty() {
            if let Some(hint) = type_hint {
                // Extract element type from List<T> hint
                if let crate::unification::StructuredType::Generic { base, args } = hint {
                    let list_type_id = self.context.type_interner.intern_type("List");
                    if base == &list_type_id && args.len() == 1 {
                        // Return the hinted List<T> type
                        return Ok(hint.clone());
                    }
                }
                // If hint is not a List<T>, fall through to error
                return Err(TypeError::type_mismatch(
                    "List<T>".to_string(),
                    hint.to_string_representation(&self.context.type_interner),
                    crate::error::span_to_source_span(literal.span),
                ));
            }
        }

        // Extract element types from all expressions
        let mut element_types = Vec::new();
        for element in &literal.elements {
            let expr = match element {
                outrun_parser::ListElement::Expression(expr) => expr.as_ref(),
                outrun_parser::ListElement::Spread(_) => {
                    return Err(TypeError::InternalError {
                        span: crate::error::span_to_source_span(literal.span),
                        message: "Spread elements in lists not yet supported in type checking"
                            .to_string(),
                    });
                }
            };
            let element_type = self.check_expression_type(expr)?;
            element_types.push(element_type);
        }

        // Case 3: Type hint provided - validate all elements match hint
        if let Some(hint) = type_hint {
            // Extract element type from List<T> hint
            let hint_element_type = match hint {
                crate::unification::StructuredType::Generic { base, args } => {
                    let list_type_id = self.context.type_interner.intern_type("List");
                    if base == &list_type_id && args.len() == 1 {
                        &args[0]
                    } else {
                        return Err(TypeError::type_mismatch(
                            "List<T>".to_string(),
                            hint.to_string_representation(&self.context.type_interner),
                            crate::error::span_to_source_span(literal.span),
                        ));
                    }
                }
                _ => {
                    return Err(TypeError::type_mismatch(
                        "List<T>".to_string(),
                        hint.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(literal.span),
                    ));
                }
            };

            // Validate all elements are compatible with hint
            for (i, element_type) in element_types.iter().enumerate() {
                if !crate::unification::unify_structured_types(
                    element_type,
                    hint_element_type,
                    &self.context,
                )? {
                    let element_expr = match &literal.elements[i] {
                        outrun_parser::ListElement::Expression(expr) => expr.as_ref(),
                        _ => unreachable!(), // We already handled spreads above
                    };
                    return Err(TypeError::type_mismatch(
                        hint_element_type.to_string_representation(&self.context.type_interner),
                        element_type.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(element_expr.span),
                    ));
                }
            }

            // All elements match hint, return the hinted type
            return Ok(hint.clone());
        }

        // Case 4: No type hint - require homogeneous elements
        let first_element_type = &element_types[0];
        for (i, element_type) in element_types.iter().enumerate().skip(1) {
            if !crate::unification::unify_structured_types(
                first_element_type,
                element_type,
                &self.context,
            )? {
                let element_expr = match &literal.elements[i] {
                    outrun_parser::ListElement::Expression(expr) => expr.as_ref(),
                    _ => unreachable!(), // We already handled spreads above
                };
                return Err(TypeError::MixedListElements {
                    span: crate::error::span_to_source_span(element_expr.span),
                    expected_type: first_element_type
                        .to_string_representation(&self.context.type_interner),
                    found_type: element_type.to_string_representation(&self.context.type_interner),
                });
            }
        }

        // All elements are homogeneous, create List<T> with first element type
        let list_type_id = self.context.type_interner.intern_type("List");
        Ok(crate::unification::StructuredType::Generic {
            base: list_type_id,
            args: vec![first_element_type.clone()],
        })
    }

    /// Check map literal type
    fn check_map_literal_type(
        &mut self,
        literal: &outrun_parser::MapLiteral,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        if literal.entries.is_empty() {
            // Empty map - use type hint if available, otherwise Map<Any, Any>
            if let Some(hint) = type_hint {
                match hint {
                    crate::unification::StructuredType::Generic { base, args } => {
                        let map_type_id = self.context.type_interner.intern_type("Map");
                        if *base == map_type_id && args.len() == 2 {
                            // Type hint is Map<K, V>, use it directly
                            Ok(hint.clone())
                        } else {
                            // Type hint is not a valid Map type, fall back to Map<Any, Any>
                            let any_type_id = self.context.type_interner.intern_type("Any");
                            Ok(crate::unification::StructuredType::Generic {
                                base: map_type_id,
                                args: vec![
                                    crate::unification::StructuredType::Simple(any_type_id),
                                    crate::unification::StructuredType::Simple(any_type_id),
                                ],
                            })
                        }
                    }
                    _ => {
                        // Type hint is not a generic type, fall back to Map<Any, Any>
                        let any_type_id = self.context.type_interner.intern_type("Any");
                        let map_type_id = self.context.type_interner.intern_type("Map");
                        Ok(crate::unification::StructuredType::Generic {
                            base: map_type_id,
                            args: vec![
                                crate::unification::StructuredType::Simple(any_type_id),
                                crate::unification::StructuredType::Simple(any_type_id),
                            ],
                        })
                    }
                }
            } else {
                // No type hint - use Map<Any, Any>
                let any_type_id = self.context.type_interner.intern_type("Any");
                let map_type_id = self.context.type_interner.intern_type("Map");
                Ok(crate::unification::StructuredType::Generic {
                    base: map_type_id,
                    args: vec![
                        crate::unification::StructuredType::Simple(any_type_id),
                        crate::unification::StructuredType::Simple(any_type_id),
                    ],
                })
            }
        } else {
            // Handle MapEntry enum - for now, only support Assignment entries
            let (first_key, first_value) = match &literal.entries[0] {
                outrun_parser::MapEntry::Assignment { key, value, .. } => {
                    (key.as_ref(), value.as_ref())
                }
                outrun_parser::MapEntry::Shorthand { .. } => {
                    return Err(TypeError::InternalError {
                        span: crate::error::span_to_source_span(literal.span),
                        message: "Shorthand map entries not yet supported in type checking"
                            .to_string(),
                    });
                }
                outrun_parser::MapEntry::Spread { .. } => {
                    return Err(TypeError::InternalError {
                        span: crate::error::span_to_source_span(literal.span),
                        message: "Spread map entries not yet supported in type checking"
                            .to_string(),
                    });
                }
            };

            // Extract type hints for key and value if available
            let (key_hint, value_hint) = if let Some(hint) = type_hint {
                match hint {
                    crate::unification::StructuredType::Generic { base, args } => {
                        let map_type_id = self.context.type_interner.intern_type("Map");
                        if *base == map_type_id && args.len() == 2 {
                            (Some(&args[0]), Some(&args[1]))
                        } else {
                            (None, None)
                        }
                    }
                    _ => (None, None),
                }
            } else {
                (None, None)
            };

            let key_type = self.check_expression_type_with_hint(first_key, key_hint)?;
            let value_type = self.check_expression_type_with_hint(first_value, value_hint)?;

            // Verify all entries have compatible key and value types
            for entry in &literal.entries[1..] {
                let (entry_key, entry_value) = match entry {
                    outrun_parser::MapEntry::Assignment { key, value, .. } => {
                        (key.as_ref(), value.as_ref())
                    }
                    outrun_parser::MapEntry::Shorthand { .. } => {
                        return Err(TypeError::InternalError {
                            span: crate::error::span_to_source_span(literal.span),
                            message: "Shorthand map entries not yet supported in type checking"
                                .to_string(),
                        });
                    }
                    outrun_parser::MapEntry::Spread { .. } => {
                        return Err(TypeError::InternalError {
                            span: crate::error::span_to_source_span(literal.span),
                            message: "Spread map entries not yet supported in type checking"
                                .to_string(),
                        });
                    }
                };

                let entry_key_type = self.check_expression_type_with_hint(entry_key, key_hint)?;
                let entry_value_type =
                    self.check_expression_type_with_hint(entry_value, value_hint)?;

                if !crate::unification::unify_structured_types(
                    &key_type,
                    &entry_key_type,
                    &self.context,
                )? {
                    return Err(TypeError::type_mismatch(
                        key_type.to_string_representation(&self.context.type_interner),
                        entry_key_type.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(entry_key.span),
                    ));
                }

                if !crate::unification::unify_structured_types(
                    &value_type,
                    &entry_value_type,
                    &self.context,
                )? {
                    return Err(TypeError::type_mismatch(
                        value_type.to_string_representation(&self.context.type_interner),
                        entry_value_type.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(entry_value.span),
                    ));
                }
            }

            let map_type_id = self.context.type_interner.intern_type("Map");
            Ok(crate::unification::StructuredType::Generic {
                base: map_type_id,
                args: vec![key_type, value_type],
            })
        }
    }

    /// Check tuple literal type
    // TODO: Add type hint parameter to support specific tuple type hints like (String, Integer) for better validation and inference
    fn check_tuple_literal_type(
        &mut self,
        literal: &outrun_parser::TupleLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let mut element_types = Vec::new();
        for element in &literal.elements {
            let element_type = self.check_expression_type(element)?;
            element_types.push(element_type);
        }
        Ok(crate::unification::StructuredType::Tuple(element_types))
    }

    /// Check struct literal type
    fn check_struct_literal_type(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Create a TypeSpec from the type_path for base resolution
        let type_spec = outrun_parser::TypeSpec {
            path: literal.type_path.clone(),
            generic_args: None, // We'll resolve generics via type hint or inference
            span: literal.span,
        };

        // Resolve base struct type (without generics first)
        let base_struct_type = self.resolve_type_spec(&type_spec)?;

        // Get struct TypeId for lookup
        let base_struct_type_id = match &base_struct_type {
            crate::unification::StructuredType::Simple(type_id) => *type_id,
            _ => {
                return Err(TypeError::InternalError {
                    span: crate::error::span_to_source_span(literal.span),
                    message: "Expected simple type for struct base".to_string(),
                });
            }
        };

        // Look up struct definition
        let struct_def = self
            .structs
            .get(&base_struct_type_id)
            .cloned()
            .ok_or_else(|| {
                let struct_name = literal
                    .type_path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                TypeError::UndefinedType {
                    span: crate::error::span_to_source_span(literal.span),
                    name: struct_name,
                }
            })?;

        // Handle generic struct literals
        if let Some(_generic_params) = &struct_def.generic_params {
            // Case 1: Type hint provides complete generic type information
            if let Some(hint) = type_hint {
                return self.check_struct_literal_with_hint(literal, &struct_def, hint);
            }

            // Case 2: No type hint - infer from field values
            return self.infer_struct_generic_types(literal, &struct_def);
        }

        // Non-generic struct - validate fields and return simple type
        self.validate_struct_fields(literal, &struct_def)?;
        Ok(crate::unification::StructuredType::Simple(
            base_struct_type_id,
        ))
    }

    /// Check struct literal against a type hint (e.g., Some<String>)
    fn check_struct_literal_with_hint(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        struct_def: &outrun_parser::StructDefinition,
        type_hint: &crate::unification::StructuredType,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Extract generic arguments from the hint
        let generic_args = match type_hint {
            crate::unification::StructuredType::Generic { base, args } => {
                // Verify the base type matches our struct
                let struct_name = struct_def.name_as_string();
                let struct_type_id = self.context.type_interner.intern_type(&struct_name);
                if base != &struct_type_id {
                    return Err(TypeError::type_mismatch(
                        struct_name,
                        type_hint.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(literal.span),
                    ));
                }
                args
            }
            _ => {
                return Err(TypeError::type_mismatch(
                    format!("{}<...>", struct_def.name_as_string()),
                    type_hint.to_string_representation(&self.context.type_interner),
                    crate::error::span_to_source_span(literal.span),
                ));
            }
        };

        // Validate that field values match the hinted generic types
        self.validate_struct_fields_with_generic_context(literal, struct_def, generic_args)?;

        // Return the hinted type
        Ok(type_hint.clone())
    }

    /// Infer generic types from field values when no hint is provided
    fn infer_struct_generic_types(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let generic_params = struct_def.generic_params.as_ref().unwrap();
        let mut type_bindings: std::collections::HashMap<
            String,
            crate::unification::StructuredType,
        > = std::collections::HashMap::new();

        // Check each field to extract type parameter bindings
        for field in &literal.fields {
            if let outrun_parser::StructLiteralField::Assignment { name, value } = field {
                let field_value_type = self.check_expression_type(value)?;

                // Find the corresponding field definition
                if let Some(field_def) = struct_def.fields.iter().find(|f| f.name.name == name.name)
                {
                    // Extract type parameter bindings from field type vs value type
                    self.extract_type_parameter_bindings(
                        &field_def.type_annotation,
                        &field_value_type,
                        &mut type_bindings,
                        &generic_params.params,
                    )?;
                }
            }
        }

        // Ensure all generic parameters have been inferred
        let mut inferred_args = Vec::new();
        for param in &generic_params.params {
            if let Some(inferred_type) = type_bindings.get(&param.name.name) {
                inferred_args.push(inferred_type.clone());
            } else {
                return Err(TypeError::CannotInferGenericType {
                    span: crate::error::span_to_source_span(literal.span),
                    type_param: param.name.name.clone(),
                });
            }
        }

        // Create the inferred generic type
        let struct_name = struct_def.name_as_string();
        let struct_type_id = self.context.type_interner.intern_type(&struct_name);
        Ok(crate::unification::StructuredType::Generic {
            base: struct_type_id,
            args: inferred_args,
        })
    }

    /// Extract type parameter bindings from field type annotation vs actual value type
    fn extract_type_parameter_bindings(
        &mut self,
        field_type_annotation: &outrun_parser::TypeAnnotation,
        field_value_type: &crate::unification::StructuredType,
        type_bindings: &mut std::collections::HashMap<String, crate::unification::StructuredType>,
        generic_params: &[outrun_parser::GenericParam],
    ) -> Result<(), TypeError> {
        // For now, implement simple case: T field gets bound to the value type
        // TODO: Handle more complex cases like Option<T>, List<T>, etc.
        if let outrun_parser::TypeAnnotation::Simple { path, .. } = field_type_annotation {
            // Check if this is a single identifier that matches a generic parameter
            if path.len() == 1 {
                let type_name = &path[0].name;
                if generic_params.iter().any(|p| p.name.name == *type_name) {
                    type_bindings.insert(type_name.clone(), field_value_type.clone());
                }
            }
        }
        Ok(())
    }

    /// Validate struct fields with generic context from type hint
    fn validate_struct_fields_with_generic_context(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        struct_def: &outrun_parser::StructDefinition,
        generic_args: &[crate::unification::StructuredType],
    ) -> Result<(), TypeError> {
        // Create generic substitution map
        let generic_params = struct_def.generic_params.as_ref().unwrap();
        let mut generic_substitutions = std::collections::HashMap::new();

        for (param, arg) in generic_params.params.iter().zip(generic_args.iter()) {
            generic_substitutions.insert(param.name.name.clone(), arg.clone());
        }

        // Validate each field
        for field in &literal.fields {
            if let outrun_parser::StructLiteralField::Assignment { name, value } = field {
                let field_value_type = self.check_expression_type(value)?;

                // Find field definition
                if let Some(field_def) = struct_def.fields.iter().find(|f| f.name.name == name.name)
                {
                    // Resolve field type with generic substitutions
                    let expected_field_type = self
                        .resolve_type_annotation_with_generic_substitution(
                            &field_def.type_annotation,
                            &generic_substitutions,
                        )?;

                    // Validate field type matches expected
                    if !crate::unification::unify_structured_types(
                        &expected_field_type,
                        &field_value_type,
                        &self.context,
                    )? {
                        return Err(TypeError::type_mismatch(
                            expected_field_type
                                .to_string_representation(&self.context.type_interner),
                            field_value_type.to_string_representation(&self.context.type_interner),
                            crate::error::span_to_source_span(value.span),
                        ));
                    }
                } else {
                    return Err(TypeError::UndefinedField {
                        span: crate::error::span_to_source_span(name.span),
                        struct_name: struct_def.name_as_string(),
                        field_name: name.name.clone(),
                    });
                }
            } else {
                return Err(TypeError::InternalError {
                    span: crate::error::span_to_source_span(literal.span),
                    message: "Non-assignment struct fields not yet supported".to_string(),
                });
            }
        }

        Ok(())
    }

    /// Validate struct fields for non-generic structs
    fn validate_struct_fields(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<(), TypeError> {
        for field in &literal.fields {
            if let outrun_parser::StructLiteralField::Assignment { name, value } = field {
                let field_value_type = self.check_expression_type(value)?;

                // Find field definition
                if let Some(field_def) = struct_def.fields.iter().find(|f| f.name.name == name.name)
                {
                    let expected_field_type =
                        self.resolve_type_annotation(&field_def.type_annotation)?;

                    if !crate::unification::unify_structured_types(
                        &expected_field_type,
                        &field_value_type,
                        &self.context,
                    )? {
                        return Err(TypeError::type_mismatch(
                            expected_field_type
                                .to_string_representation(&self.context.type_interner),
                            field_value_type.to_string_representation(&self.context.type_interner),
                            crate::error::span_to_source_span(value.span),
                        ));
                    }
                } else {
                    return Err(TypeError::UndefinedField {
                        span: crate::error::span_to_source_span(name.span),
                        struct_name: struct_def.name_as_string(),
                        field_name: name.name.clone(),
                    });
                }
            } else {
                return Err(TypeError::InternalError {
                    span: crate::error::span_to_source_span(literal.span),
                    message: "Non-assignment struct fields not yet supported".to_string(),
                });
            }
        }

        Ok(())
    }

    /// Check qualified identifier type (e.g., Module.function_name)
    fn check_qualified_identifier_type(
        &mut self,
        qual_id: &outrun_parser::QualifiedIdentifier,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // For now, treat qualified identifiers as variables
        // This will need enhancement for proper module resolution
        let var_name = format!("{}.{}", qual_id.module.name, qual_id.name.name);
        self.lookup_variable(&var_name)
            .cloned()
            .ok_or_else(|| TypeError::UndefinedVariable {
                span: crate::error::span_to_source_span(qual_id.span),
                name: var_name,
            })
    }

    /// Check if expression type
    fn check_if_expression_type(
        &mut self,
        if_expr: &outrun_parser::IfExpression,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Check condition is Boolean
        let condition_type = self.check_expression_type(&if_expr.condition)?;
        let boolean_type_id = self.context.type_interner.intern_type("Boolean");
        let expected_condition_type = crate::unification::StructuredType::Simple(boolean_type_id);

        if !crate::unification::unify_structured_types(
            &expected_condition_type,
            &condition_type,
            &self.context,
        )? {
            return Err(TypeError::type_mismatch(
                expected_condition_type.to_string_representation(&self.context.type_interner),
                condition_type.to_string_representation(&self.context.type_interner),
                crate::error::span_to_source_span(if_expr.condition.span),
            ));
        }

        // Check then branch type
        let then_type = self.check_block_type(&if_expr.then_block)?;

        // Check else branch type if present
        if let Some(else_block) = &if_expr.else_block {
            let else_type = self.check_block_type(else_block)?;

            // Both branches must have compatible types
            if !crate::unification::unify_structured_types(&then_type, &else_type, &self.context)? {
                return Err(TypeError::type_mismatch(
                    then_type.to_string_representation(&self.context.type_interner),
                    else_type.to_string_representation(&self.context.type_interner),
                    crate::error::span_to_source_span(else_block.span),
                ));
            }

            Ok(then_type)
        } else {
            // If no else branch, the then branch type must implement Default trait
            let default_trait_id = self.context.type_interner.intern_type("Default");

            // Check if then_type implements Default using the trait registry
            match &then_type {
                crate::unification::StructuredType::Simple(type_id) => {
                    if !self
                        .context
                        .trait_registry
                        .implements_trait(*type_id, default_trait_id)
                    {
                        let type_name = self
                            .context
                            .type_interner
                            .type_name(*type_id)
                            .unwrap_or_else(|| format!("Unknown({:?})", type_id));
                        return Err(TypeError::TraitNotImplemented {
                            span: crate::error::span_to_source_span(if_expr.then_block.span),
                            trait_name: "Default".to_string(),
                            type_name,
                        });
                    }
                }
                _ => {
                    // For generic types, tuple types, and function types, we need more sophisticated
                    // trait implementation checking. For now, reject non-simple types.
                    return Err(TypeError::TraitNotImplemented {
                        span: crate::error::span_to_source_span(if_expr.then_block.span),
                        trait_name: "Default".to_string(),
                        type_name: then_type.to_string_representation(&self.context.type_interner),
                    });
                }
            }

            Ok(then_type)
        }
    }

    /// Check case expression type
    fn check_case_expression_type(
        &mut self,
        case_expr: &outrun_parser::CaseExpression,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match case_expr {
            outrun_parser::CaseExpression::Concrete(concrete_case) => {
                self.check_concrete_case_expression_type(concrete_case)
            }
            outrun_parser::CaseExpression::Trait(trait_case) => {
                self.check_trait_case_expression_type(trait_case)
            }
        }
    }

    /// Check concrete case expression type
    fn check_concrete_case_expression_type(
        &mut self,
        case_expr: &outrun_parser::ConcreteCaseExpression,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Check the target expression type
        let _target_type = self.check_expression_type(&case_expr.expression)?;

        // Determine result type from when clauses
        if case_expr.when_clauses.is_empty() {
            return Err(TypeError::InternalError {
                span: crate::error::span_to_source_span(case_expr.span),
                message: "Case expression must have at least one when clause".to_string(),
            });
        }

        // Check first clause to determine result type
        let first_result_type = match &case_expr.when_clauses[0].result {
            outrun_parser::CaseResult::Expression(expr) => self.check_expression_type(expr)?,
            outrun_parser::CaseResult::Block(block) => self.check_block_type(block)?,
        };

        // Verify all clauses have compatible result types
        for clause in &case_expr.when_clauses[1..] {
            let clause_result_type = match &clause.result {
                outrun_parser::CaseResult::Expression(expr) => self.check_expression_type(expr)?,
                outrun_parser::CaseResult::Block(block) => self.check_block_type(block)?,
            };

            if !crate::unification::unify_structured_types(
                &first_result_type,
                &clause_result_type,
                &self.context,
            )? {
                return Err(TypeError::type_mismatch(
                    first_result_type.to_string_representation(&self.context.type_interner),
                    clause_result_type.to_string_representation(&self.context.type_interner),
                    crate::error::span_to_source_span(clause.span),
                ));
            }
        }

        // TODO: Add guard condition type checking and exhaustiveness analysis

        Ok(first_result_type)
    }

    /// Check trait case expression type with pattern validation
    fn check_trait_case_expression_type(
        &mut self,
        case_expr: &outrun_parser::TraitCaseExpression,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Check the target expression type
        let target_type = self.check_expression_type(&case_expr.expression)?;

        // Determine result type from type clauses
        if case_expr.type_clauses.is_empty() {
            return Err(TypeError::InternalError {
                span: crate::error::span_to_source_span(case_expr.span),
                message: "Trait case expression must have at least one type clause".to_string(),
            });
        }

        // Check each clause and validate patterns
        let mut result_type = None;

        for clause in &case_expr.type_clauses {
            // Create new scope for pattern-bound variables
            self.push_scope();

            // Resolve the concrete type from the clause
            let concrete_type_id = self
                .context
                .type_interner
                .intern_type(&clause.type_name.name);
            let concrete_type = crate::unification::StructuredType::Simple(concrete_type_id);

            // Validate that the concrete type implements the trait
            // TODO: Add trait implementation checking

            // Check if target type is compatible with the concrete type
            if !crate::unification::unify_structured_types(
                &target_type,
                &concrete_type,
                &self.context,
            )
            .unwrap_or(false)
            {
                // Type doesn't match - this clause won't match at runtime
                // For exhaustiveness checking, we'd track this
            }

            // If there's a pattern, validate it against the concrete type
            if let Some(pattern) = &clause.pattern {
                let bound_variables = self.check_pattern_type(
                    &outrun_parser::Pattern::Struct(pattern.clone()),
                    &concrete_type,
                )?;

                // Register pattern-bound variables in scope
                for (var_name, var_type) in bound_variables {
                    self.register_variable(var_name, var_type);
                }
            }

            // Check guard condition if present
            if let Some(guard) = &clause.guard {
                let guard_type = self.check_expression_type(guard)?;
                let boolean_type_id = self.context.type_interner.intern_type("Boolean");
                let expected_guard_type =
                    crate::unification::StructuredType::Simple(boolean_type_id);

                if !crate::unification::unify_structured_types(
                    &guard_type,
                    &expected_guard_type,
                    &self.context,
                )
                .unwrap_or(false)
                {
                    self.pop_scope();
                    return Err(TypeError::type_mismatch(
                        expected_guard_type.to_string_representation(&self.context.type_interner),
                        guard_type.to_string_representation(&self.context.type_interner),
                        crate::error::span_to_source_span(guard.span),
                    ));
                }
            }

            // Check clause result type
            let clause_result_type = match &clause.result {
                outrun_parser::CaseResult::Expression(expr) => self.check_expression_type(expr)?,
                outrun_parser::CaseResult::Block(block) => self.check_block_type(block)?,
            };

            // Clean up scope
            self.pop_scope();

            // Verify result type compatibility
            match &result_type {
                None => {
                    // First clause determines the result type
                    result_type = Some(clause_result_type);
                }
                Some(expected_type) => {
                    // Subsequent clauses must have compatible types
                    if !crate::unification::unify_structured_types(
                        &clause_result_type,
                        expected_type,
                        &self.context,
                    )
                    .unwrap_or(false)
                    {
                        return Err(TypeError::type_mismatch(
                            expected_type.to_string_representation(&self.context.type_interner),
                            clause_result_type
                                .to_string_representation(&self.context.type_interner),
                            crate::error::span_to_source_span(clause.span),
                        ));
                    }
                }
            }
        }

        Ok(result_type.unwrap()) // Safe because we checked for empty clauses above
    }

    /// Check field access type
    fn check_field_access_type(
        &mut self,
        field_access: &outrun_parser::FieldAccess,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Check the object expression type
        let object_type = self.check_expression_type(&field_access.object)?;

        // Get the struct TypeId and generic arguments
        let (struct_type_id, generic_args) = match &object_type {
            crate::unification::StructuredType::Simple(type_id) => (*type_id, None),
            crate::unification::StructuredType::Generic { base, args } => (*base, Some(args)),
            _ => {
                return Err(TypeError::InternalError {
                    span: crate::error::span_to_source_span(field_access.object.span),
                    message: "Field access only supported on struct types".to_string(),
                });
            }
        };

        // Look up struct definition
        let struct_def =
            self.structs
                .get(&struct_type_id)
                .cloned()
                .ok_or_else(|| TypeError::InternalError {
                    span: crate::error::span_to_source_span(field_access.object.span),
                    message: "Object type is not a struct".to_string(),
                })?;

        // Find the field
        let field_name = &field_access.field.name;
        let field_def = struct_def
            .fields
            .iter()
            .find(|f| f.name.name == *field_name)
            .ok_or_else(|| TypeError::UndefinedField {
                span: crate::error::span_to_source_span(field_access.field.span),
                struct_name: self
                    .context
                    .type_interner
                    .type_name(struct_type_id)
                    .unwrap_or_else(|| format!("Unknown({:?})", struct_type_id)),
                field_name: field_name.clone(),
            })?;

        // Return the field's type, applying generic substitutions if needed
        if let Some(args) = generic_args {
            // Build generic substitutions map
            let mut generic_substitutions = std::collections::HashMap::new();
            if let Some(ref generic_params) = struct_def.generic_params {
                for (i, generic_param) in generic_params.params.iter().enumerate() {
                    if let Some(arg_type) = args.get(i) {
                        generic_substitutions
                            .insert(generic_param.name.name.clone(), arg_type.clone());
                    }
                }
            }

            // Resolve field type with generic substitutions
            self.resolve_type_annotation_with_generic_substitution(
                &field_def.type_annotation,
                &generic_substitutions,
            )
        } else {
            // No generics, use regular resolution
            self.resolve_type_annotation(&field_def.type_annotation)
        }
    }

    /// Check function call type
    fn check_function_call_type(
        &mut self,
        call: &outrun_parser::FunctionCall,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Resolve function name to AtomId
        let func_name = match &call.path {
            outrun_parser::FunctionPath::Simple { name } => name.name.clone(),
            outrun_parser::FunctionPath::Qualified { module, name } => {
                format!("{}.{}", module.name, name.name)
            }
            outrun_parser::FunctionPath::Expression { expression: _ } => {
                // Function expressions (like captured functions) need special handling
                // For now, return a placeholder name
                "anonymous_function".to_string()
            }
        };

        // Debug: log ALL function calls to see what's happening
        if cfg!(debug_assertions) {
            eprintln!("Function call: {} (path: {:?})", func_name, call.path);
        }
        let _func_atom = self.context.type_interner.intern_atom(&func_name);

        // Look up function definition using hierarchical registry
        match &call.path {
            outrun_parser::FunctionPath::Qualified { module, name } => {
                // Qualified call like "Option.some?" or "Outrun.Intrinsic.list_inspect"
                let module_name = &module.name;
                let function_name = &name.name;

                // Special handling for intrinsic functions - bypass type checking
                if cfg!(debug_assertions) {
                    eprintln!(
                        "Qualified call: module='{}', function='{}'",
                        module_name, function_name
                    );
                }
                if module_name == "Outrun.Intrinsic" {
                    // Intrinsic functions are compiler-provided and guaranteed correct
                    let full_intrinsic_name = format!("Outrun.Intrinsic.{}", function_name);
                    let generic_intrinsics =
                        crate::intrinsics::IntrinsicRegistry::get_generic_intrinsics();

                    if cfg!(debug_assertions) {
                        eprintln!(
                            "Looking up intrinsic: '{}' in registry with {} entries",
                            full_intrinsic_name,
                            generic_intrinsics.len()
                        );
                    }
                    if let Some(intrinsic_def) = generic_intrinsics.get(&full_intrinsic_name) {
                        // Handle generic intrinsic functions with proper type parameter inference
                        if !intrinsic_def.generic_params.is_empty() {
                            return self.handle_generic_intrinsic_call(call, intrinsic_def);
                        } else {
                            // Non-generic intrinsic - just return the declared return type
                            return self.resolve_type_annotation(&intrinsic_def.return_type);
                        }
                    } else {
                        // Unknown intrinsic
                        if cfg!(debug_assertions) {
                            eprintln!(
                                "Unknown intrinsic: '{}' - falling through to regular lookup",
                                full_intrinsic_name
                            );
                        }
                        return Err(TypeError::UndefinedFunction {
                            span: crate::error::span_to_source_span(call.span),
                            name: full_intrinsic_name,
                        });
                    }
                }

                // Get module TypeId (base type without generics)
                let module_type_id = self.context.type_interner.intern_type(module_name);

                // Check if this is a trait that has implementations
                let is_trait = self.context.trait_registry.is_trait(module_type_id);

                if is_trait {
                    // For trait calls, first infer the implementing type from arguments
                    let trait_func_def = self
                        .function_registry
                        .lookup_qualified_function(module_type_id, function_name)
                        .cloned();

                    if let Some(trait_func) = trait_func_def {
                        // Infer the concrete implementing type from the first Self parameter
                        let implementing_structured_type = self
                            .infer_implementing_type_from_arguments(
                                module_type_id,
                                &trait_func,
                                call,
                            )?;

                        // Extract base TypeId for implementation lookup
                        let implementing_type = match &implementing_structured_type {
                            crate::unification::StructuredType::Simple(type_id) => *type_id,
                            crate::unification::StructuredType::Generic { base, args: _ } => *base,
                            _ => {
                                return Err(TypeError::InternalError {
                                    span: crate::error::span_to_source_span(call.span),
                                    message: "Cannot extract base type for implementation lookup"
                                        .to_string(),
                                });
                            }
                        };

                        // Look up the actual implementation function
                        if cfg!(debug_assertions) {
                            eprintln!(
                                "Looking for impl function: trait={:?}, impl={:?}, func={}",
                                self.context.type_interner.type_name(module_type_id),
                                self.context.type_interner.type_name(implementing_type),
                                function_name
                            );
                        }
                        if let Some(impl_func_def) = self
                            .function_registry
                            .lookup_impl_function(module_type_id, implementing_type, function_name)
                            .cloned()
                        {
                            if cfg!(debug_assertions) {
                                eprintln!("Found impl function: {}", impl_func_def.name.name);
                            }
                            // Use the full structured type for Self context (preserving generics)
                            let inferred_self_type = implementing_structured_type;
                            if cfg!(debug_assertions) {
                                eprintln!(
                                    "Using impl Self type: {}",
                                    inferred_self_type
                                        .to_string_representation(&self.context.type_interner)
                                );
                            }
                            self.validate_function_call_arguments_with_self(
                                call,
                                &impl_func_def,
                                &inferred_self_type,
                            )?;
                            self.resolve_type_annotation_with_self(
                                &impl_func_def.return_type,
                                &inferred_self_type,
                            )
                        } else {
                            // Fall back to trait function if no implementation found
                            if cfg!(debug_assertions) {
                                eprintln!("No impl function found, falling back to trait function");
                            }
                            let inferred_self_type = self.infer_trait_self_type_from_arguments(
                                module_type_id,
                                &trait_func,
                                call,
                            )?;
                            self.validate_function_call_arguments_with_self(
                                call,
                                &trait_func,
                                &inferred_self_type,
                            )?;
                            self.resolve_type_annotation_with_self(
                                &trait_func.return_type,
                                &inferred_self_type,
                            )
                        }
                    } else {
                        Err(TypeError::UndefinedFunction {
                            span: crate::error::span_to_source_span(call.span),
                            name: func_name,
                        })
                    }
                } else if let Some(func_def) = self
                    .function_registry
                    .lookup_qualified_function(module_type_id, function_name)
                    .cloned()
                {
                    // Regular module function call
                    self.validate_function_call_arguments(call, &func_def)?;
                    self.resolve_type_annotation(&func_def.return_type)
                } else {
                    Err(TypeError::UndefinedFunction {
                        span: crate::error::span_to_source_span(call.span),
                        name: func_name,
                    })
                }
            }
            outrun_parser::FunctionPath::Simple { name } => {
                // Simple call like "some_function" - search in function registry
                let function_name = &name.name;

                if let Some(func_def) = self
                    .function_registry
                    .lookup_local_function(function_name)
                    .cloned()
                {
                    // Validate arguments match parameters
                    self.validate_function_call_arguments(call, &func_def)?;
                    // Return the declared return type
                    self.resolve_type_annotation(&func_def.return_type)
                } else {
                    Err(TypeError::UndefinedFunction {
                        span: crate::error::span_to_source_span(call.span),
                        name: function_name.clone(),
                    })
                }
            }
            outrun_parser::FunctionPath::Expression { .. } => {
                // Function expressions need special handling
                Err(TypeError::UndefinedFunction {
                    span: crate::error::span_to_source_span(call.span),
                    name: "anonymous_function".to_string(),
                })
            }
        }
    }

    /// Handle generic intrinsic function calls with type parameter inference
    fn handle_generic_intrinsic_call(
        &mut self,
        call: &outrun_parser::FunctionCall,
        intrinsic_def: &crate::intrinsics::GenericIntrinsicDef,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        use std::collections::HashMap;

        // Build a map of generic parameter substitutions by inferring from arguments
        let mut substitutions: HashMap<String, crate::unification::StructuredType> = HashMap::new();

        // Type check each argument and infer generic parameters
        for arg in &call.arguments {
            if let outrun_parser::Argument::Named {
                name, expression, ..
            } = arg
            {
                let param_name = &name.name;

                // Find the corresponding parameter definition
                if let Some(param_def) = intrinsic_def
                    .parameters
                    .iter()
                    .find(|p| p.name.name == *param_name)
                {
                    // Type check the argument expression
                    let arg_type = self.check_expression_type(expression)?;

                    // Infer generic parameters from this argument
                    self.infer_generic_parameters_from_types(
                        &param_def.type_annotation,
                        &arg_type,
                        &mut substitutions,
                    )?;
                }
            }
        }

        // Apply substitutions to the return type
        self.substitute_generic_parameters_in_type_annotation(
            &intrinsic_def.return_type,
            &substitutions,
        )
    }

    /// Infer generic parameter mappings by comparing expected vs actual types
    #[allow(clippy::only_used_in_recursion)]
    fn infer_generic_parameters_from_types(
        &self,
        expected_annotation: &outrun_parser::TypeAnnotation,
        actual_type: &crate::unification::StructuredType,
        substitutions: &mut std::collections::HashMap<String, crate::unification::StructuredType>,
    ) -> Result<(), TypeError> {
        match expected_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // If this is a generic parameter (like "T"), record the substitution
                if path.len() == 1 && path[0].name.chars().all(|c| c.is_uppercase()) {
                    substitutions.insert(type_name, actual_type.clone());
                    return Ok(());
                }

                // If this has generic arguments, recursively infer from them
                if let Some(args) = generic_args {
                    if let crate::unification::StructuredType::Generic {
                        args: actual_args, ..
                    } = actual_type
                    {
                        for (expected_arg, actual_arg) in args.args.iter().zip(actual_args.iter()) {
                            self.infer_generic_parameters_from_types(
                                expected_arg,
                                actual_arg,
                                substitutions,
                            )?;
                        }
                    }
                }
            }
            _ => {
                // Handle other type annotation variants as needed
            }
        }
        Ok(())
    }

    /// Apply generic parameter substitutions to a type annotation
    fn substitute_generic_parameters_in_type_annotation(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
        substitutions: &std::collections::HashMap<String, crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // If this is a generic parameter, substitute it
                if let Some(substituted_type) = substitutions.get(&type_name) {
                    return Ok(substituted_type.clone());
                }

                // If this has generic arguments, recursively substitute them
                if let Some(args) = generic_args {
                    let mut substituted_args = Vec::new();
                    for arg in &args.args {
                        let substituted_arg = self
                            .substitute_generic_parameters_in_type_annotation(arg, substitutions)?;
                        substituted_args.push(substituted_arg);
                    }

                    let base_type_id = self.context.type_interner.intern_type(&type_name);
                    Ok(crate::unification::StructuredType::Generic {
                        base: base_type_id,
                        args: substituted_args,
                    })
                } else {
                    // Simple non-generic type
                    let type_id = self.context.type_interner.intern_type(&type_name);
                    Ok(crate::unification::StructuredType::Simple(type_id))
                }
            }
            _ => {
                // For now, fall back to regular resolution for other types
                self.resolve_type_annotation(type_annotation)
            }
        }
    }

    /// Validate function call arguments against function definition parameters
    fn validate_function_call_arguments(
        &mut self,
        call: &outrun_parser::FunctionCall,
        func_def: &outrun_parser::FunctionDefinition,
    ) -> Result<(), TypeError> {
        use std::collections::HashSet;

        // Track which parameters have been provided
        let mut provided_params: HashSet<String> = HashSet::new();

        // Validate each argument
        for arg in &call.arguments {
            match arg {
                outrun_parser::Argument::Named {
                    name,
                    expression,
                    format,
                    span,
                } => {
                    let param_name = &name.name;

                    // Check if parameter exists in function definition
                    let param_def = func_def
                        .parameters
                        .iter()
                        .find(|p| p.name.name == *param_name)
                        .ok_or_else(|| TypeError::UnknownParameter {
                            span: crate::error::span_to_source_span(*span),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        })?;

                    // Check for duplicate arguments
                    if provided_params.contains(param_name) {
                        return Err(TypeError::DuplicateArgument {
                            span: crate::error::span_to_source_span(*span),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        });
                    }
                    provided_params.insert(param_name.clone());

                    // Type check the argument expression
                    let arg_type = self.check_expression_type(expression)?;
                    let expected_param_type =
                        self.resolve_type_annotation(&param_def.type_annotation)?;

                    // Validate argument type matches parameter type
                    match crate::unification::unify_structured_types(
                        &arg_type,
                        &expected_param_type,
                        &self.context,
                    ) {
                        Ok(false) | Err(_) => {
                            return Err(TypeError::ArgumentTypeMismatch {
                                span: crate::error::span_to_source_span(expression.span),
                                function_name: func_def.name.name.clone(),
                                parameter_name: param_name.clone(),
                                expected_type: expected_param_type
                                    .to_string_representation(&self.context.type_interner),
                                found_type: arg_type
                                    .to_string_representation(&self.context.type_interner),
                            });
                        }
                        Ok(true) => {} // Types unify successfully
                    }

                    // For shorthand format, validate that argument name matches parameter name
                    if let outrun_parser::ArgumentFormat::Shorthand = format {
                        // Shorthand is valid since we already found the matching parameter
                    }
                }
                outrun_parser::Argument::Spread {
                    expression: _,
                    kind: _,
                    span,
                } => {
                    // TODO: Implement spread argument validation
                    // For now, return an error as spread arguments need special handling
                    return Err(TypeError::UnsupportedFeature {
                        span: crate::error::span_to_source_span(*span),
                        feature: "Spread arguments".to_string(),
                    });
                }
            }
        }

        // Check that all required parameters have been provided
        for param in &func_def.parameters {
            if !provided_params.contains(&param.name.name) {
                // Check if parameter type implements Default trait (making it optional)
                let param_type = self.resolve_type_annotation(&param.type_annotation)?;
                let default_trait_id = self.context.type_interner.intern_type("Default");

                // Convert StructuredType to TypeId for trait checking
                // For now, only handle simple types; complex generic types would need more sophisticated checking
                let implements_default = match &param_type {
                    crate::unification::StructuredType::Simple(type_id) => self
                        .context
                        .trait_registry
                        .implements_trait(*type_id, default_trait_id),
                    _ => {
                        // For complex types (generics, functions, tuples), assume they don't implement Default
                        // This is a simplification - in a full implementation, we'd need to check recursively
                        false
                    }
                };

                // If the parameter type doesn't implement Default, it's required
                if !implements_default {
                    return Err(TypeError::MissingArgument {
                        span: crate::error::span_to_source_span(call.span),
                        function_name: func_def.name.name.clone(),
                        parameter_name: param.name.name.clone(),
                    });
                }
                // If it implements Default, the parameter is optional and can be skipped
            }
        }

        Ok(())
    }

    /// Validate function call arguments against function definition parameters with Self context
    /// This is used for trait function calls where Self needs to be resolved to the trait type
    fn validate_function_call_arguments_with_self(
        &mut self,
        call: &outrun_parser::FunctionCall,
        func_def: &outrun_parser::FunctionDefinition,
        self_type: &crate::unification::StructuredType,
    ) -> Result<(), TypeError> {
        use std::collections::HashSet;

        // Track which parameters have been provided
        let mut provided_params: HashSet<String> = HashSet::new();

        // Validate each argument
        for arg in &call.arguments {
            match arg {
                outrun_parser::Argument::Named {
                    name,
                    expression,
                    format: _,
                    span,
                } => {
                    let param_name = &name.name;

                    // Check if parameter exists in function definition
                    let param_def = func_def
                        .parameters
                        .iter()
                        .find(|p| p.name.name == *param_name)
                        .ok_or_else(|| TypeError::UnknownParameter {
                            span: crate::error::span_to_source_span(*span),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        })?;

                    // Check for duplicate arguments
                    if provided_params.contains(param_name) {
                        return Err(TypeError::DuplicateArgument {
                            span: crate::error::span_to_source_span(*span),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        });
                    }
                    provided_params.insert(param_name.clone());

                    // Type check the argument expression
                    let arg_type = self.check_expression_type(expression)?;

                    // Resolve parameter type with Self context
                    let expected_param_type = self
                        .resolve_type_annotation_with_self(&param_def.type_annotation, self_type)?;

                    // Validate argument type matches parameter type
                    match crate::unification::unify_structured_types(
                        &arg_type,
                        &expected_param_type,
                        &self.context,
                    ) {
                        Ok(false) | Err(_) => {
                            return Err(TypeError::ArgumentTypeMismatch {
                                span: crate::error::span_to_source_span(expression.span),
                                function_name: func_def.name.name.clone(),
                                parameter_name: param_name.clone(),
                                expected_type: expected_param_type
                                    .to_string_representation(&self.context.type_interner),
                                found_type: arg_type
                                    .to_string_representation(&self.context.type_interner),
                            });
                        }
                        Ok(true) => {} // Types unify successfully
                    }
                }
                outrun_parser::Argument::Spread { .. } => {
                    // Spread arguments not yet implemented
                    return Err(TypeError::InternalError {
                        span: crate::error::span_to_source_span(call.span),
                        message: "Spread arguments not yet supported in function calls".to_string(),
                    });
                }
            }
        }

        // Check that all required parameters have been provided
        for param in &func_def.parameters {
            if !provided_params.contains(&param.name.name) {
                // Check if parameter type implements Default trait (making it optional)
                let param_type =
                    self.resolve_type_annotation_with_self(&param.type_annotation, self_type)?;
                let default_trait_id = self.context.type_interner.intern_type("Default");

                // Convert StructuredType to TypeId for trait checking
                let implements_default = match &param_type {
                    crate::unification::StructuredType::Simple(type_id) => self
                        .context
                        .trait_registry
                        .implements_trait(*type_id, default_trait_id),
                    _ => {
                        // For complex types (generics, tuples, functions), conservatively assume they don't implement Default
                        false
                    }
                };

                // If the parameter type doesn't implement Default, it's required
                if !implements_default {
                    return Err(TypeError::MissingArgument {
                        span: crate::error::span_to_source_span(call.span),
                        function_name: func_def.name.name.clone(),
                        parameter_name: param.name.name.clone(),
                    });
                }
                // If it implements Default, the parameter is optional and can be skipped
            }
        }

        Ok(())
    }

    /// Check variable type using scope tracking
    fn check_variable_type(
        &mut self,
        var: &outrun_parser::Identifier,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        if let Some(var_type) = self.lookup_variable(&var.name) {
            Ok(var_type.clone())
        } else {
            Err(TypeError::UndefinedVariable {
                span: crate::error::span_to_source_span(var.span),
                name: var.name.clone(),
            })
        }
    }

    /// Check the type of a block (sequence of statements) - for simple sequential blocks
    /// Note: if/case expressions handle their own branch scoping in check_expression_type
    fn check_block_type(
        &mut self,
        block: &outrun_parser::Block,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        self.check_block_type_with_hint(block, None)
    }

    /// Check the type of a block with an optional expected type hint for the final expression
    fn check_block_type_with_hint(
        &mut self,
        block: &outrun_parser::Block,
        expected_type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let mut last_type = None;

        for (i, statement) in block.statements.iter().enumerate() {
            let is_final_statement = i == block.statements.len() - 1;

            match &statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    // For the final expression, use the expected type hint if provided
                    last_type = if is_final_statement && expected_type_hint.is_some() {
                        Some(self.check_expression_type_with_hint(expr, expected_type_hint)?)
                    } else {
                        Some(self.check_expression_type(expr)?)
                    };
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    // If there's a type annotation, resolve it first to use as a hint
                    let type_hint = if let Some(type_annotation) = &let_binding.type_annotation {
                        Some(self.resolve_type_annotation(type_annotation)?)
                    } else {
                        None
                    };

                    // Check the expression type with the hint
                    let expr_type = self.check_expression_type_with_hint(
                        &let_binding.expression,
                        type_hint.as_ref(),
                    )?;

                    // If there was a type annotation, validate it matches the expression
                    if let Some(expected_type) = &type_hint {
                        if !crate::unification::unify_structured_types(
                            &expr_type,
                            expected_type,
                            &self.context,
                        )
                        .unwrap_or(false)
                        {
                            return Err(TypeError::type_mismatch(
                                expected_type.to_string_representation(&self.context.type_interner),
                                expr_type.to_string_representation(&self.context.type_interner),
                                crate::error::span_to_source_span(let_binding.expression.span),
                            ));
                        }
                    }

                    // Validate pattern against expression type and extract bound variables
                    match self.check_pattern_type(&let_binding.pattern, &expr_type) {
                        Ok(bound_variables) => {
                            // Register all pattern-bound variables in scope
                            for (var_name, var_type) in bound_variables {
                                self.register_variable(var_name, var_type);
                            }
                        }
                        Err(pattern_error) => {
                            // Pattern doesn't match expression type
                            return Err(pattern_error);
                        }
                    }

                    last_type = Some(expr_type);
                }
            }
        }

        // Return the type of the last expression, or Unit if empty block
        Ok(last_type.unwrap_or_else(|| {
            let unit_type_id = self.context.type_interner.intern_type("Unit");
            crate::unification::StructuredType::Simple(unit_type_id)
        }))
    }

    /// Check function definition and return let-bound variables
    fn check_function_definition_with_variables(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
    ) -> Result<std::collections::HashMap<String, crate::unification::StructuredType>, TypeError>
    {
        // Functions must always have a return type in Outrun
        let expected_return_type = self.resolve_type_annotation(&func.return_type)?;

        // Create new scope for function parameters and body
        self.push_scope();

        // Validate parameter types and register them in scope
        for param in &func.parameters {
            let param_type = self.resolve_type_annotation(&param.type_annotation)?;
            self.register_variable(param.name.name.clone(), param_type);
        }

        // Type check function body with expected return type as hint (this will register let-bound variables)
        let body_type = self.check_block_type_with_hint(&func.body, Some(&expected_return_type))?;

        // Validate body type matches return type
        match crate::unification::unify_structured_types(
            &body_type,
            &expected_return_type,
            &self.context,
        ) {
            Ok(false) | Err(_) => {
                self.errors.push(TypeError::type_mismatch(
                    expected_return_type.to_string_representation(&self.context.type_interner),
                    body_type.to_string_representation(&self.context.type_interner),
                    crate::error::span_to_source_span(func.body.span),
                ));
            }
            Ok(true) => {} // Types unify successfully
        }

        // Capture let-bound variables from the current scope (excluding parameters)
        let mut let_bound_variables = std::collections::HashMap::new();
        if let Some(current_scope) = self.variable_scopes.last() {
            for (var_name, var_type) in current_scope {
                // Skip parameters, only capture let-bound variables
                let is_parameter = func.parameters.iter().any(|p| p.name.name == *var_name);
                if !is_parameter {
                    let_bound_variables.insert(var_name.clone(), var_type.clone());
                }
            }
        }

        // Pop function scope
        self.pop_scope();

        Ok(let_bound_variables)
    }

    /// Check struct definition
    fn check_struct_definition(
        &mut self,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<(), TypeError> {
        // Validate all field types exist
        for field in &struct_def.fields {
            let _field_type = self.resolve_type_annotation(&field.type_annotation)?;
        }
        Ok(())
    }

    /// Check trait definition
    fn check_trait_definition(
        &mut self,
        trait_def: &outrun_parser::TraitDefinition,
    ) -> Result<(), TypeError> {
        // Validate all function signatures
        for trait_func in &trait_def.functions {
            match trait_func {
                outrun_parser::TraitFunction::Signature(sig) => {
                    // Validate return type
                    let _return_type = self.resolve_type_annotation(&sig.return_type)?;

                    // Validate parameter types
                    for param in &sig.parameters {
                        let _param_type = self.resolve_type_annotation(&param.type_annotation)?;
                    }
                }
                outrun_parser::TraitFunction::Definition(func) => {
                    // Validate return type
                    let _return_type = self.resolve_type_annotation(&func.return_type)?;

                    // Validate parameter types
                    for param in &func.parameters {
                        let _param_type = self.resolve_type_annotation(&param.type_annotation)?;
                    }
                }
                outrun_parser::TraitFunction::StaticDefinition(static_func) => {
                    // Validate return type
                    let _return_type = self.resolve_type_annotation(&static_func.return_type)?;

                    // Validate parameter types
                    for param in &static_func.parameters {
                        let _param_type = self.resolve_type_annotation(&param.type_annotation)?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Check impl block
    fn check_impl_block(&mut self, impl_block: &outrun_parser::ImplBlock) -> Result<(), TypeError> {
        // Validate trait and type exist
        let trait_type = self.resolve_type_spec(&impl_block.trait_spec)?;
        let impl_type = self.resolve_type_spec(&impl_block.type_spec)?;

        // Get trait TypeId for lookup
        let trait_id = match trait_type {
            crate::unification::StructuredType::Simple(type_id) => type_id,
            crate::unification::StructuredType::Generic { base, .. } => {
                // For generic traits like List<T>, use the base trait type
                base
            }
            _ => {
                return Err(TypeError::InternalError {
                    span: crate::error::span_to_source_span(impl_block.trait_spec.span),
                    message: "Expected simple or generic type for trait in impl block".to_string(),
                });
            }
        };

        // Look up trait definition (clone to avoid borrowing issues)
        let trait_def = self.traits.get(&trait_id).cloned().ok_or_else(|| {
            let trait_name = impl_block
                .trait_spec
                .path
                .iter()
                .map(|id| id.name.as_str())
                .collect::<Vec<_>>()
                .join(".");
            TypeError::UndefinedTrait {
                span: crate::error::span_to_source_span(impl_block.trait_spec.span),
                trait_name,
            }
        })?;

        // Validate impl functions match trait signatures
        self.validate_impl_functions(impl_block, &trait_def, trait_id, impl_type)?;

        Ok(())
    }

    /// Validate that impl block functions match trait signatures exactly
    fn validate_impl_functions(
        &mut self,
        impl_block: &outrun_parser::ImplBlock,
        trait_def: &outrun_parser::TraitDefinition,
        trait_id: TypeId,
        impl_type: crate::unification::StructuredType,
    ) -> Result<(), TypeError> {
        use std::collections::{HashMap, HashSet};

        // Collect trait function signatures by name
        let mut trait_functions: HashMap<String, &outrun_parser::TraitFunction> = HashMap::new();
        for trait_func in &trait_def.functions {
            let func_name = match trait_func {
                outrun_parser::TraitFunction::Signature(sig) => &sig.name.name,
                outrun_parser::TraitFunction::Definition(func) => &func.name.name,
                outrun_parser::TraitFunction::StaticDefinition(static_func) => {
                    &static_func.name.name
                }
            };
            trait_functions.insert(func_name.clone(), trait_func);
        }

        // Track which trait functions have been implemented
        let mut implemented_functions: HashSet<String> = HashSet::new();

        // Validate each impl function
        for impl_func in &impl_block.methods {
            let func_name = &impl_func.name.name;
            implemented_functions.insert(func_name.clone());

            // Check if function exists in trait
            let trait_func =
                trait_functions
                    .get(func_name)
                    .ok_or_else(|| TypeError::ExtraImplementation {
                        span: crate::error::span_to_source_span(impl_func.span),
                        trait_name: self
                            .context
                            .type_interner
                            .type_name(trait_id)
                            .unwrap_or_else(|| format!("Unknown({:?})", trait_id)),
                        function_name: func_name.clone(),
                    })?;

            // Validate function signatures match
            self.validate_function_signature_match(impl_func, trait_func, &impl_type)?;
        }

        // Check for missing implementations - only check functions without default implementations

        for (trait_func_name, trait_func) in &trait_functions {
            if !implemented_functions.contains(trait_func_name) {
                // Only require implementation if this is a signature without a default implementation
                let requires_implementation =
                    matches!(trait_func, outrun_parser::TraitFunction::Signature(_));

                if requires_implementation {
                    return Err(TypeError::MissingImplementation {
                        span: crate::error::span_to_source_span(impl_block.span),
                        trait_name: self
                            .context
                            .type_interner
                            .type_name(trait_id)
                            .unwrap_or_else(|| format!("Unknown({:?})", trait_id)),
                        type_name: impl_type.to_string_representation(&self.context.type_interner),
                        function_name: trait_func_name.clone(),
                    });
                }
                // Functions with default implementations are optional - no need to require them
            }
        }

        Ok(())
    }

    /// Validate that an impl function signature matches the trait function signature exactly
    fn validate_function_signature_match(
        &mut self,
        impl_func: &outrun_parser::FunctionDefinition,
        trait_func: &outrun_parser::TraitFunction,
        impl_type: &crate::unification::StructuredType,
    ) -> Result<(), TypeError> {
        // Get trait function signature details
        let (trait_params, trait_return_type, trait_visibility) = match trait_func {
            outrun_parser::TraitFunction::Signature(sig) => {
                (&sig.parameters, &sig.return_type, sig.visibility.clone())
            }
            outrun_parser::TraitFunction::Definition(func) => {
                (&func.parameters, &func.return_type, func.visibility.clone())
            }
            outrun_parser::TraitFunction::StaticDefinition(_static_func) => {
                // Static functions should not be implemented in impl blocks
                return Err(TypeError::SignatureMismatch {
                    span: crate::error::span_to_source_span(impl_func.span),
                    function_name: impl_func.name.name.clone(),
                    expected: "Static function (should not be implemented in impl)".to_string(),
                    found: "Instance function".to_string(),
                });
            }
        };

        // Check visibility matches
        if impl_func.visibility != trait_visibility {
            return Err(TypeError::SignatureMismatch {
                span: crate::error::span_to_source_span(impl_func.span),
                function_name: impl_func.name.name.clone(),
                expected: format!("Visibility: {:?}", trait_visibility),
                found: format!("Visibility: {:?}", impl_func.visibility),
            });
        }

        // Check parameter count matches
        if impl_func.parameters.len() != trait_params.len() {
            return Err(TypeError::SignatureMismatch {
                span: crate::error::span_to_source_span(impl_func.span),
                function_name: impl_func.name.name.clone(),
                expected: format!("{} parameters", trait_params.len()),
                found: format!("{} parameters", impl_func.parameters.len()),
            });
        }

        // Check each parameter matches (name and type)
        for (impl_param, trait_param) in impl_func.parameters.iter().zip(trait_params.iter()) {
            // Check parameter names match
            if impl_param.name.name != trait_param.name.name {
                return Err(TypeError::SignatureMismatch {
                    span: crate::error::span_to_source_span(impl_param.span),
                    function_name: impl_func.name.name.clone(),
                    expected: format!("Parameter name: {}", trait_param.name.name),
                    found: format!("Parameter name: {}", impl_param.name.name),
                });
            }

            // Check parameter types match (with Self substitution)
            let impl_param_type =
                self.resolve_type_annotation_with_self(&impl_param.type_annotation, impl_type)?;
            let trait_param_type =
                self.resolve_type_annotation_with_self(&trait_param.type_annotation, impl_type)?;

            match crate::unification::unify_structured_types(
                &impl_param_type,
                &trait_param_type,
                &self.context,
            ) {
                Ok(false) | Err(_) => {
                    return Err(TypeError::SignatureMismatch {
                        span: crate::error::span_to_source_span(impl_param.span),
                        function_name: impl_func.name.name.clone(),
                        expected: format!(
                            "Parameter {}: {}",
                            trait_param.name.name,
                            trait_param_type.to_string_representation(&self.context.type_interner)
                        ),
                        found: format!(
                            "Parameter {}: {}",
                            impl_param.name.name,
                            impl_param_type.to_string_representation(&self.context.type_interner)
                        ),
                    });
                }
                Ok(true) => {} // Types unify successfully
            }
        }

        // Check return types match (with Self substitution)
        let impl_return_type =
            self.resolve_type_annotation_with_self(&impl_func.return_type, impl_type)?;
        let trait_return_type =
            self.resolve_type_annotation_with_self(trait_return_type, impl_type)?;

        match crate::unification::unify_structured_types(
            &impl_return_type,
            &trait_return_type,
            &self.context,
        ) {
            Ok(false) | Err(_) => {
                return Err(TypeError::SignatureMismatch {
                    span: crate::error::span_to_source_span(impl_func.span),
                    function_name: impl_func.name.name.clone(),
                    expected: format!(
                        "Return type: {}",
                        trait_return_type.to_string_representation(&self.context.type_interner)
                    ),
                    found: format!(
                        "Return type: {}",
                        impl_return_type.to_string_representation(&self.context.type_interner)
                    ),
                });
            }
            Ok(true) => {} // Types unify successfully
        }

        Ok(())
    }

    /// Resolve type annotation with Self substitution for impl blocks
    fn resolve_type_annotation_with_self(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
        self_type: &crate::unification::StructuredType,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Special handling for Self type resolution with structured types
        self.resolve_type_annotation_with_self_substitution(type_annotation, self_type)
    }

    /// Recursively resolve type annotation with Self substitution
    fn resolve_type_annotation_with_self_substitution(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
        self_type: &crate::unification::StructuredType,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path,
                generic_args,
                span,
            } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Handle Self substitution directly
                if type_name == "Self" {
                    return Ok(self_type.clone());
                }

                // Handle other type parameters using the existing system
                if let Some(type_param_type) = self.lookup_type_parameter(&type_name) {
                    return Ok(type_param_type.clone());
                }

                // Check if type exists in the type interner
                if self.context.type_interner.get_type(&type_name).is_some() {
                    let type_id = self.context.type_interner.intern_type(&type_name);

                    // Handle generic arguments with Self substitution
                    if let Some(ref args) = generic_args {
                        let mut arg_types = Vec::new();
                        for arg in &args.args {
                            let arg_type = self
                                .resolve_type_annotation_with_self_substitution(arg, self_type)?;
                            arg_types.push(arg_type);
                        }

                        Ok(crate::unification::StructuredType::Generic {
                            base: type_id,
                            args: arg_types,
                        })
                    } else {
                        Ok(crate::unification::StructuredType::Simple(type_id))
                    }
                } else {
                    Err(TypeError::UndefinedType {
                        span: crate::error::span_to_source_span(*span),
                        name: type_name,
                    })
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                let mut element_types = Vec::new();
                for element_type in types {
                    let element_struct_type = self
                        .resolve_type_annotation_with_self_substitution(element_type, self_type)?;
                    element_types.push(element_struct_type);
                }
                Ok(crate::unification::StructuredType::Tuple(element_types))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                let mut param_types = Vec::new();
                for param in params {
                    let param_struct_type = self.resolve_type_annotation_with_self_substitution(
                        &param.type_annotation,
                        self_type,
                    )?;
                    param_types.push(crate::unification::FunctionParam {
                        name: self.context.type_interner.intern_atom(&param.name.name),
                        param_type: param_struct_type,
                    });
                }

                let return_struct_type =
                    self.resolve_type_annotation_with_self_substitution(return_type, self_type)?;

                Ok(crate::unification::StructuredType::Function {
                    params: param_types,
                    return_type: Box::new(return_struct_type),
                })
            }
        }
    }

    /// Infer trait Self type from function call arguments
    /// This solves the "generic vs self" issue by inferring Self type from arguments
    fn infer_trait_self_type_from_arguments(
        &mut self,
        trait_type_id: crate::types::TypeId,
        func_def: &outrun_parser::FunctionDefinition,
        call: &outrun_parser::FunctionCall,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Build argument type mapping
        let mut arg_type_map = std::collections::HashMap::new();
        for arg in &call.arguments {
            if let outrun_parser::Argument::Named {
                name, expression, ..
            } = arg
            {
                let arg_type = self.check_expression_type(expression)?;
                arg_type_map.insert(name.name.clone(), arg_type);
            }
        }

        // Look for parameters with Self type annotation and infer Self from their argument types
        for param in &func_def.parameters {
            let param_name = &param.name.name;
            if let Some(arg_type) = arg_type_map.get(param_name) {
                // Check if this parameter is typed as Self
                if self.is_self_type_annotation(&param.type_annotation) {
                    // The argument type should be the Self type we're looking for
                    // For Option.some?(value: Self), if argument is Option<Integer>, then Self = Option<Integer>
                    return Ok(arg_type.clone());
                }
            }
        }

        // If no Self parameter found, try to infer from generic trait structure
        let trait_def =
            self.traits
                .get(&trait_type_id)
                .cloned()
                .ok_or_else(|| TypeError::InternalError {
                    span: crate::error::span_to_source_span(call.span),
                    message: format!("Trait not found: {:?}", trait_type_id),
                })?;

        // Extract generic parameter names from trait definition (e.g., ["T"] for Option<T>)
        let generic_param_names: Vec<String> = trait_def
            .generic_params
            .as_ref()
            .map(|params| params.params.iter().map(|p| p.name.name.clone()).collect())
            .unwrap_or_default();

        if generic_param_names.is_empty() {
            // No generic parameters, just use simple trait type
            return Ok(crate::unification::StructuredType::Simple(trait_type_id));
        }

        // Try to infer generic parameter types from function signature
        let mut inferred_generic_types = std::collections::HashMap::new();

        for param in &func_def.parameters {
            let param_name = &param.name.name;
            if let Some(arg_type) = arg_type_map.get(param_name) {
                // Try to match parameter type annotation with argument type to infer generics
                Self::infer_generic_types_from_parameter(
                    &param.type_annotation,
                    arg_type,
                    &generic_param_names,
                    &mut inferred_generic_types,
                )?;
            }
        }

        // Construct the inferred trait type
        if inferred_generic_types.is_empty() {
            // No type inference possible, use simple trait type
            Ok(crate::unification::StructuredType::Simple(trait_type_id))
        } else {
            // Build generic trait type with inferred type arguments
            let mut generic_args = Vec::new();
            for param_name in &generic_param_names {
                if let Some(inferred_type) = inferred_generic_types.get(param_name) {
                    generic_args.push(inferred_type.clone());
                } else {
                    // Use Any for unresolved generic parameters
                    let any_type_id = self.context.type_interner.intern_type("Any");
                    generic_args.push(crate::unification::StructuredType::Simple(any_type_id));
                }
            }

            Ok(crate::unification::StructuredType::Generic {
                base: trait_type_id,
                args: generic_args,
            })
        }
    }

    /// Infer the concrete implementing type from trait function call arguments
    /// This is used for trait dispatch to find the specific implementation to use
    fn infer_implementing_type_from_arguments(
        &mut self,
        _trait_type_id: crate::types::TypeId,
        trait_func_def: &outrun_parser::FunctionDefinition,
        call: &outrun_parser::FunctionCall,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Build argument type mapping
        let mut arg_type_map = std::collections::HashMap::new();
        for arg in &call.arguments {
            if let outrun_parser::Argument::Named {
                name, expression, ..
            } = arg
            {
                let arg_type = self.check_expression_type(expression)?;
                arg_type_map.insert(name.name.clone(), arg_type);
            }
        }

        // Look for the first parameter with Self type annotation to infer implementing type
        for param in &trait_func_def.parameters {
            let param_name = &param.name.name;
            if let Some(arg_type) = arg_type_map.get(param_name) {
                // Check if this parameter is typed as Self
                if self.is_self_type_annotation(&param.type_annotation) {
                    // Return the full structured type (preserving generics)
                    return Ok(arg_type.clone());
                }
            }
        }

        // If no Self parameter found, this is an error - trait functions should have Self parameters
        Err(TypeError::InternalError {
            span: crate::error::span_to_source_span(call.span),
            message: format!(
                "Trait function {} has no Self parameter for implementation dispatch",
                trait_func_def.name.name
            ),
        })
    }

    /// Check if a type annotation is Self
    fn is_self_type_annotation(&self, type_annotation: &outrun_parser::TypeAnnotation) -> bool {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, .. } => {
                path.len() == 1 && path[0].name == "Self"
            }
            _ => false,
        }
    }

    /// Resolve type annotation with generic parameter substitutions
    fn resolve_type_annotation_with_generic_substitution(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
        generic_substitutions: &std::collections::HashMap<
            String,
            crate::unification::StructuredType,
        >,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this is a generic parameter to substitute
                if path.len() == 1 {
                    if let Some(substitution) = generic_substitutions.get(&type_name) {
                        return Ok(substitution.clone());
                    }
                }

                // Resolve base type
                let base_type_id =
                    self.context
                        .type_interner
                        .get_type(&type_name)
                        .ok_or_else(|| TypeError::UndefinedType {
                            span: crate::error::span_to_source_span(outrun_parser::Span {
                                start: 0,
                                end: 0,
                                start_line_col: None,
                                end_line_col: None,
                            }),
                            name: type_name.clone(),
                        })?;

                // Handle generic arguments
                if let Some(args) = generic_args {
                    let mut resolved_args = Vec::new();
                    for arg in &args.args {
                        let resolved_arg = self.resolve_type_annotation_with_generic_substitution(
                            arg,
                            generic_substitutions,
                        )?;
                        resolved_args.push(resolved_arg);
                    }
                    Ok(crate::unification::StructuredType::Generic {
                        base: base_type_id,
                        args: resolved_args,
                    })
                } else {
                    Ok(crate::unification::StructuredType::Simple(base_type_id))
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                let mut resolved_types = Vec::new();
                for t in types {
                    let resolved_type = self.resolve_type_annotation_with_generic_substitution(
                        t,
                        generic_substitutions,
                    )?;
                    resolved_types.push(resolved_type);
                }
                Ok(crate::unification::StructuredType::Tuple(resolved_types))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                let mut resolved_params = Vec::new();
                for param in params {
                    let resolved_param_type = self
                        .resolve_type_annotation_with_generic_substitution(
                            &param.type_annotation,
                            generic_substitutions,
                        )?;
                    let param_name = self.context.type_interner.intern_atom(&param.name.name);
                    resolved_params.push(crate::unification::FunctionParam {
                        name: param_name,
                        param_type: resolved_param_type,
                    });
                }
                let resolved_return_type = self.resolve_type_annotation_with_generic_substitution(
                    return_type,
                    generic_substitutions,
                )?;
                Ok(crate::unification::StructuredType::Function {
                    params: resolved_params,
                    return_type: Box::new(resolved_return_type),
                })
            }
        }
    }

    /// Infer generic type parameters from a function parameter type annotation and argument type
    fn infer_generic_types_from_parameter(
        param_type_annotation: &outrun_parser::TypeAnnotation,
        arg_type: &crate::unification::StructuredType,
        generic_param_names: &[String],
        inferred_types: &mut std::collections::HashMap<String, crate::unification::StructuredType>,
    ) -> Result<(), TypeError> {
        match param_type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, .. } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this is a generic parameter (e.g., "T", "E", "K", "V")
                if generic_param_names.contains(&type_name) {
                    // This parameter uses a generic type, infer it from the argument
                    inferred_types.insert(type_name, arg_type.clone());
                } else if type_name == "Self" {
                    // Self parameter - this is where we need to extract the generic argument
                    // For Option.some?(value: Self), Self should be Option<T>, and we infer T from value type
                    // But we can't infer Self from Self, so we handle this differently
                    // The argument type should match what Self will be resolved to

                    // For now, we can't directly infer from Self, but we could look at other parameters
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                // Handle tuple type inference recursively
                if let crate::unification::StructuredType::Tuple(arg_elements) = arg_type {
                    if types.len() == arg_elements.len() {
                        for (param_elem, arg_elem) in types.iter().zip(arg_elements.iter()) {
                            Self::infer_generic_types_from_parameter(
                                param_elem,
                                arg_elem,
                                generic_param_names,
                                inferred_types,
                            )?;
                        }
                    }
                }
            }
            outrun_parser::TypeAnnotation::Function { .. } => {
                // Function type inference would be more complex, skip for now
            }
        }

        Ok(())
    }

    /// Resolve type annotation to StructuredType
    fn resolve_type_annotation(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path,
                generic_args,
                span,
            } => {
                // Convert type path to string
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Handle type parameters (Self, T, E, K, V, etc.)
                if let Some(type_param_type) = self.lookup_type_parameter(&type_name) {
                    return Ok(type_param_type.clone());
                }

                // Check if type exists
                if self.context.type_interner.get_type(&type_name).is_some() {
                    let type_id = self.context.type_interner.intern_type(&type_name);

                    // Handle generic arguments if present
                    if let Some(ref args) = generic_args {
                        let mut arg_types = Vec::new();
                        for arg in &args.args {
                            let arg_type = self.resolve_type_annotation(arg)?;
                            arg_types.push(arg_type);
                        }

                        Ok(crate::unification::StructuredType::Generic {
                            base: type_id,
                            args: arg_types,
                        })
                    } else {
                        Ok(crate::unification::StructuredType::Simple(type_id))
                    }
                } else {
                    Err(TypeError::UndefinedType {
                        span: crate::error::span_to_source_span(*span),
                        name: type_name,
                    })
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, span: _ } => {
                // Resolve all tuple element types
                let mut element_types = Vec::new();
                for element_type in types {
                    let element_struct_type = self.resolve_type_annotation(element_type)?;
                    element_types.push(element_struct_type);
                }

                Ok(crate::unification::StructuredType::Tuple(element_types))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                span: _,
            } => {
                // Resolve parameter types
                let mut param_types = Vec::new();
                for param in params {
                    let param_struct_type = self.resolve_type_annotation(&param.type_annotation)?;
                    param_types.push(crate::unification::FunctionParam {
                        name: self.context.type_interner.intern_atom(&param.name.name),
                        param_type: param_struct_type,
                    });
                }

                // Resolve return type
                let return_struct_type = self.resolve_type_annotation(return_type)?;

                Ok(crate::unification::StructuredType::Function {
                    params: param_types,
                    return_type: Box::new(return_struct_type),
                })
            }
        }
    }

    /// Resolve type spec to StructuredType
    fn resolve_type_spec(
        &mut self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_name = type_spec
            .path
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        if self.context.type_interner.get_type(&type_name).is_some() {
            let type_id = self.context.type_interner.intern_type(&type_name);

            // Handle generic arguments if present
            if let Some(ref generic_args) = type_spec.generic_args {
                let mut arg_types = Vec::new();
                for arg in &generic_args.args {
                    // Convert TypeAnnotation to StructuredType
                    let arg_type = self.resolve_type_annotation(arg)?;
                    arg_types.push(arg_type);
                }
                Ok(crate::unification::StructuredType::Generic {
                    base: type_id,
                    args: arg_types,
                })
            } else {
                Ok(crate::unification::StructuredType::Simple(type_id))
            }
        } else {
            Err(TypeError::UndefinedType {
                span: crate::error::span_to_source_span(type_spec.span),
                name: type_name,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::{DebugInfo, Program, Span};

    fn create_test_span() -> Span {
        Span::new(0, 10)
    }

    #[test]
    fn test_program_collection_creation() {
        let collection = ProgramCollection::new();
        assert!(collection.programs.is_empty());
        assert!(collection.sources.is_empty());
    }

    #[test]
    fn test_single_program_collection() {
        let program = Program {
            items: vec![],
            debug_info: DebugInfo::new(),
            span: create_test_span(),
        };

        let collection = ProgramCollection::from_single_program(
            "test.outrun".to_string(),
            program,
            "# empty program".to_string(),
        );

        assert_eq!(collection.programs.len(), 1);
        assert!(collection.get_program("test.outrun").is_some());
        assert!(collection.get_source("test.outrun").is_some());
    }

    #[test]
    fn test_multi_program_compiler_creation() {
        let compiler = MultiProgramCompiler::new();
        assert!(compiler.errors.is_empty());
    }
}
