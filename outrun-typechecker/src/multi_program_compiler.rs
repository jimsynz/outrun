//! Multi-program compiler for Outrun
//!
//! This module implements a general-purpose compiler that can handle collections
//! of Outrun programs (including both user code and core library). It uses
//! dependency resolution and phase-specific visitors to ensure correct
//! compilation order and type checking.

// Re-export for backward compatibility
pub use crate::compilation::function_registry::{
    FunctionEntry, FunctionExtractionVisitor, FunctionRegistry, FunctionType,
};
pub use crate::compilation::program_collection::{CompilationResult, ProgramCollection};
pub use crate::compilation::type_checking::TypeCheckingVisitor;
pub use crate::compilation::visitors::{
    ImplExtractionVisitor, StructExtractionVisitor, TraitExtractionVisitor,
};
use crate::dependency_graph::DependencyGraph;
use crate::error::{SpanExt, TypeError};
use crate::types::TypeId;
use crate::unification::UnificationContext;
use crate::visitor::Visitor;
use outrun_parser::{ImplBlock, StructDefinition, TraitDefinition};
use std::collections::HashMap;

// Import the desugarer for transforming operators to function calls
use crate::desugaring::DesugaringVisitor;
use crate::typed_ast_builder::TypedASTBuilder;

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
            return Err(std::mem::take(&mut self.errors));
        }

        // Step 7: Phase 6 - Build comprehensive typed AST
        let typed_programs =
            self.build_typed_ast(collection, &compilation_order, &functions, &structs)?;

        Ok(CompilationResult {
            compilation_order,
            type_context: self.unification_context.clone(),
            traits,
            structs,
            implementations,
            function_registry: functions,
            typed_programs,
        })
    }

    /// Resolve dependencies and get compilation order
    fn resolve_dependencies(
        &mut self,
        collection: &ProgramCollection,
    ) -> Result<Vec<String>, Vec<TypeError>> {
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
        let result = self.dependency_graph.resolve_with_trait_cycles_allowed();

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
                            span: impl_def.trait_spec.span.to_source_span(),
                            trait_name,
                        });
                    }
                }
            }
        }

        if !self.errors.is_empty() {
            return Err(std::mem::take(&mut self.errors));
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
        // Extract function registry from user programs
        let mut function_registry = {
            let mut visitor = FunctionExtractionVisitor::new(&mut self.unification_context);

            // Extract functions from all user programs
            for file_path in order {
                if let Some(program) = collection.get_program(file_path) {
                    <FunctionExtractionVisitor as Visitor<()>>::visit_program(
                        &mut visitor,
                        program,
                    )
                    .map_err(|e| vec![e])?;
                }
            }

            visitor.registry
        };

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

        for (_func_atom_id, func_def) in intrinsic_functions {
            // Intrinsic functions have simple names like "atom_eq", use them directly
            let function_name = func_def.name.name.clone();
            let function_id = format!("intrinsic::{}", function_name);
            let entry = FunctionEntry {
                definition: func_def,
                function_type: FunctionType::TypeStatic,
                function_id,
            };
            function_registry.add_module_function(intrinsic_module_type_id, function_name, entry);
        }

        Ok(function_registry)
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
        let mut visitor = TypeCheckingVisitor::new(
            self.unification_context.clone(),
            function_registry.clone(),
            structs.clone(),
            traits.clone(),
        );

        // Type check all programs in dependency order
        for file_path in order {
            if let Some(program) = collection.get_program(file_path) {
                // Desugar the program before type checking with span mapping
                let (desugared_program, span_mapping) =
                    DesugaringVisitor::desugar_program_with_span_mapping(program.clone());

                // Merge this program's span mapping into the context
                visitor.context.merge_span_mapping(span_mapping);

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

    /// Phase 6: Build comprehensive typed AST using type checking results
    fn build_typed_ast(
        &mut self,
        collection: &ProgramCollection,
        compilation_order: &[String],
        function_registry: &FunctionRegistry,
        structs: &HashMap<TypeId, StructDefinition>,
    ) -> Result<HashMap<String, crate::checker::TypedProgram>, Vec<TypeError>> {
        let mut builder = TypedASTBuilder::new(
            self.unification_context.clone(),
            function_registry.clone(),
            structs.clone(),
        );

        match builder.build_typed_ast(collection, compilation_order) {
            Ok(typed_programs) => {
                // Accumulate any errors from the builder
                self.errors.extend(builder.errors);
                Ok(typed_programs)
            }
            Err(errors) => {
                self.errors.extend(errors.clone());
                Err(errors)
            }
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

// Implement visitor trait for TypeCheckingVisitor

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
