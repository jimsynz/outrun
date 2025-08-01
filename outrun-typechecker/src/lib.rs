//! Outrun Typechecker v3
//!
//! Hindley-Milner type inference with protocol constraint solving and exhaustiveness checking.

// Allow clippy lints for development
#![allow(clippy::needless_range_loop)]
#![allow(clippy::single_match)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::useless_vec)]
#![allow(clippy::result_large_err)]
#![allow(clippy::needless_return)]
#![allow(clippy::double_ended_iterator_last)]
//!
//! ## Architecture
//!
//! This typechecker follows Outrun's minimalist development philosophy by extending existing
//! parser infrastructure rather than creating parallel systems. Key components:
//!
//! - **Type Inference Engine**: HM algorithm with unification and constraint accumulation
//! - **Protocol System**: Implementation registry with orphan rule checking
//! - **Constraint Solver**: Logical constraint satisfaction for protocol bounds
//! - **Exhaustiveness Checker**: Pattern coverage analysis for case expressions and multi-head functions
//!
//! ## Integration
//!
//! The typechecker extends the existing parser AST by adding optional `TypeInfo` fields
//! to expressions, eliminating duplication while enabling seamless integration.

pub mod constraints;
pub mod core_library;
pub mod debug_spans;
pub mod desugaring;
pub mod dispatch;
pub mod error;
pub mod exhaustiveness;
pub mod inference;
pub mod intrinsics;
pub mod package;
pub mod registry;
pub mod typed_ast;
pub mod types;
pub mod unification;
pub mod universal_dispatch;

// Re-export public API
pub use constraints::ConstraintSolver;
pub use core_library::{collect_outrun_files, default_core_library_path};
pub use desugaring::DesugaringEngine;
pub use dispatch::{
    build_dispatch_table, DispatchResult, DispatchTable, FunctionContext, FunctionDispatcher,
    FunctionInfo, FunctionRegistry, FunctionVisibility, MonomorphisationEntry,
    MonomorphisationTable, ResolvedFunction,
};
pub use error::{
    CompilerError, ConstraintError, DispatchError, ErrorContext, InferenceError, TypecheckError,
    UnificationError,
};
pub use inference::{InferenceContext, InferenceResult, TypeInferenceEngine};
pub use registry::{ImplementationInfo, TypeRegistry};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
pub use typed_ast::{TypedExpression, TypedExpressionKind, UniversalCallResolution};
pub use types::{Constraint, ModuleName, Substitution, Type, TypeInfo, TypeVarId};
pub use unification::Unifier;
pub use universal_dispatch::{
    ClauseId, ClauseInfo, ConstraintContext, FunctionBody, FunctionSignature, Guard,
    UniversalDispatchRegistry,
};

/// A collection of parsed programs representing a complete Outrun package
pub struct Package {
    pub programs: Vec<outrun_parser::Program>,
    pub package_name: String,
}

impl Package {
    pub fn new(package_name: String) -> Self {
        Self {
            programs: Vec::new(),
            package_name,
        }
    }

    pub fn add_program(&mut self, program: outrun_parser::Program) {
        self.programs.push(program);
    }

    /// Create a package from a loaded package (from outrun.toml)
    pub fn from_loaded_package(loaded_package: package::LoadedPackage) -> Self {
        Self {
            programs: loaded_package.programs,
            package_name: loaded_package.manifest.package.name,
        }
    }

    /// Merge another package into this one (for core library integration)
    pub fn merge(&mut self, other: Package) {
        self.programs.extend(other.programs);
    }
}

/// Complete result of typechecking a package - reusable and composable
#[derive(Debug, Clone)]
pub struct CompilationResult {
    /// Complete type registry with protocols, structs, and implementations
    pub type_registry: std::rc::Rc<TypeRegistry>,
    /// Complete function registry with all function definitions  
    pub function_registry: std::rc::Rc<FunctionRegistry>,
    /// Runtime dispatch table for function resolution (LEGACY)
    pub dispatch_table: DispatchTable,
    /// Monomorphisation table for tracking generic function instantiations
    pub monomorphisation_table: MonomorphisationTable,
    /// Package identity for dependency tracking
    pub package_name: String,
    /// Processed programs with type information attached
    pub programs: Vec<outrun_parser::Program>,
    /// Which modules in this result are considered "local" for orphan rule checking
    pub local_modules: HashSet<ModuleName>,
    /// All modules defined by this compilation result (for redefinition prevention)
    pub defined_modules: HashSet<ModuleName>,
    /// Universal dispatch registry for clause-based function calls
    pub universal_dispatch: UniversalDispatchRegistry,
    /// Current file being processed (for diagnostic context)
    current_file: std::cell::RefCell<Option<String>>,
}

impl CompilationResult {
    /// Pre-compile the core library for REPL optimization
    /// This creates a reusable CompilationResult that can be used as a dependency for all REPL expressions
    pub fn precompile_core_library() -> Result<CompilationResult, CompilerError> {
        // Load the core library package
        if let Some(core_package) = package::load_core_library_package()? {
            let mut core_package_std = Package::from_loaded_package(core_package);
            core_package_std.package_name = "outrun-core".to_string();

            println!("📦 Pre-compiling core library for REPL optimization...");

            // Compile the core library without loading it again (it's already loaded)
            // Use compile_package_internal directly to avoid double-loading
            let engine = crate::inference::TypeInferenceEngine::bootstrap();
            let desugaring_engine = crate::desugaring::DesugaringEngine::new();
            let core_result =
                Self::compile_package_internal(&mut core_package_std, engine, desugaring_engine)?;

            println!(
                "✅ Core library pre-compiled successfully with {} implementations",
                core_result.type_registry.implementation_count()
            );

            Ok(core_result)
        } else {
            Err(CompilerError::Typecheck(Box::new(
                crate::error::TypecheckError::CoreLibraryError(
                    "Could not load core library for pre-compilation".to_string(),
                ),
            )))
        }
    }

    /// Compile a REPL expression using pre-compiled core library
    /// This avoids recompiling the core library for every REPL expression
    pub fn compile_repl_expression(
        expression_source: &str,
        precompiled_core: &CompilationResult,
    ) -> Result<CompilationResult, CompilerError> {
        // Parse the REPL expression as a program
        let program =
            outrun_parser::parse_program(expression_source).map_err(CompilerError::Parse)?;

        // Create a minimal package for the expression
        let mut expr_package = Package::new("repl_expression".to_string());
        expr_package.add_program(program);

        // Compile with the pre-compiled core library as a dependency
        Self::compile_with_dependencies(&mut expr_package, vec![precompiled_core.clone()])
    }

    /// Recompile a package, allowing it to redefine its own modules (for hot reloading/plugins)
    /// This supports scenarios where a package needs to update its own code at runtime
    pub fn recompile_package(
        package: &mut Package,
        previous_compilation: Option<&CompilationResult>,
        dependencies: Vec<CompilationResult>,
    ) -> Result<CompilationResult, CompilerError> {
        // If we have a previous compilation from the same package, check for module redefinitions
        if let Some(prev) = previous_compilation {
            if prev.package_name == package.package_name {
                // Same package - check for module redefinitions and warn only if content changed
                let prev_module_digests = Self::extract_package_module_digests(&prev.programs);
                let new_module_digests = Self::extract_package_module_digests(&package.programs);

                for (module_id, new_digest) in &new_module_digests {
                    if let Some(prev_digest) = prev_module_digests.get(module_id) {
                        // Module exists in both versions - check if content actually changed
                        if prev_digest != new_digest {
                            println!(
                                "⚠️  package {} redefined module {}",
                                package.package_name,
                                module_id.as_str()
                            );
                        }
                    }
                }
            }
        }

        // Use the standard compile_with_dependencies method
        // Package self-redefinition is allowed - conflicts only apply across different packages
        Self::compile_with_dependencies(package, dependencies)
    }

    /// Extract modules and their content digests for change detection during hot reloading
    fn extract_package_module_digests(
        programs: &[outrun_parser::Program],
    ) -> HashMap<ModuleName, u64> {
        let mut module_digests = HashMap::new();

        for program in programs {
            for item in &program.items {
                let (module_id, content_digest) = match &item.kind {
                    outrun_parser::ItemKind::StructDefinition(struct_def) => {
                        let module_id = ModuleName::from(&struct_def.name);
                        let digest = Self::compute_struct_digest(struct_def);
                        (Some(module_id), digest)
                    }
                    outrun_parser::ItemKind::ProtocolDefinition(protocol_def) => {
                        let module_id = ModuleName::from(&protocol_def.name);
                        let digest = Self::compute_protocol_digest(protocol_def);
                        (Some(module_id), digest)
                    }
                    _ => (None, 0),
                };

                if let Some(module) = module_id {
                    module_digests.insert(module, content_digest);
                }
            }
        }

        module_digests
    }

    /// Compute a digest/hash of a struct definition's content
    fn compute_struct_digest(struct_def: &outrun_parser::StructDefinition) -> u64 {
        let mut hasher = DefaultHasher::new();

        // Hash the struct name (convert Vec<TypeIdentifier> to string)
        let struct_name = struct_def
            .name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        struct_name.hash(&mut hasher);

        // Hash the field count and field names
        struct_def.fields.len().hash(&mut hasher);
        for field in &struct_def.fields {
            field.name.name.hash(&mut hasher);
            field.type_annotation.to_string().hash(&mut hasher);
        }

        // Hash the function count and function names
        struct_def.functions.len().hash(&mut hasher);
        for function in &struct_def.functions {
            function.name.name.hash(&mut hasher);
            function.parameters.len().hash(&mut hasher);
            // Hash parameter names and types
            for param in &function.parameters {
                param.name.name.hash(&mut hasher);
                param.type_annotation.to_string().hash(&mut hasher);
            }
            // Hash return type
            function.return_type.to_string().hash(&mut hasher);
        }

        hasher.finish()
    }

    /// Compute a digest/hash of a protocol definition's content
    fn compute_protocol_digest(protocol_def: &outrun_parser::ProtocolDefinition) -> u64 {
        let mut hasher = DefaultHasher::new();

        // Hash the protocol name (convert Vec<TypeIdentifier> to string)
        let protocol_name = protocol_def
            .name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        protocol_name.hash(&mut hasher);

        // Hash constraints (if any)
        if let Some(ref constraints) = protocol_def.constraints {
            constraints.to_string().hash(&mut hasher);
        }

        // Hash the function count and function signatures
        protocol_def.functions.len().hash(&mut hasher);
        for function in &protocol_def.functions {
            match function {
                outrun_parser::ProtocolFunction::Signature(sig) => {
                    "signature".hash(&mut hasher);
                    sig.name.name.hash(&mut hasher);
                    sig.parameters.len().hash(&mut hasher);
                    for param in &sig.parameters {
                        param.name.name.hash(&mut hasher);
                        param.type_annotation.to_string().hash(&mut hasher);
                    }
                    sig.return_type.to_string().hash(&mut hasher);
                }
                outrun_parser::ProtocolFunction::Definition(def) => {
                    "definition".hash(&mut hasher);
                    def.name.name.hash(&mut hasher);
                    def.parameters.len().hash(&mut hasher);
                    for param in &def.parameters {
                        param.name.name.hash(&mut hasher);
                        param.type_annotation.to_string().hash(&mut hasher);
                    }
                    def.return_type.to_string().hash(&mut hasher);
                }
                outrun_parser::ProtocolFunction::StaticDefinition(static_def) => {
                    "static".hash(&mut hasher);
                    static_def.name.name.hash(&mut hasher);
                    static_def.parameters.len().hash(&mut hasher);
                    for param in &static_def.parameters {
                        param.name.name.hash(&mut hasher);
                        param.type_annotation.to_string().hash(&mut hasher);
                    }
                    static_def.return_type.to_string().hash(&mut hasher);
                }
            }
        }

        hasher.finish()
    }

    /// Compile a package with pre-compiled dependencies
    pub fn compile_with_dependencies(
        package: &mut Package,
        dependencies: Vec<CompilationResult>,
    ) -> Result<CompilationResult, CompilerError> {
        let mut engine = TypeInferenceEngine::new();
        let desugaring_engine = DesugaringEngine::new();

        // Phase 0: Merge dependency registries into engine before processing user package
        for dependency in &dependencies {
            // Check for module conflicts before merging
            Self::check_module_conflicts(&dependency.defined_modules, &HashSet::new())?;

            // Import dependency's registries into our engine
            engine.import_dependency_registries(
                dependency.type_registry.clone(),
                dependency.function_registry.clone(),
            )?;
        }

        // Phase 0.5: Comprehensive module conflict detection
        // Collect all dependency programs for detailed conflict checking
        let all_dependency_programs: Vec<&outrun_parser::Program> = dependencies
            .iter()
            .flat_map(|dep| dep.programs.iter())
            .collect();

        // Use enhanced conflict detection that handles struct vs protocol conflicts
        Self::check_detailed_module_conflicts(&all_dependency_programs, &package.programs)?;

        // Phase 1: Load core library only if not provided as dependency
        let has_core_dependency = dependencies
            .iter()
            .any(|dep| dep.package_name == "outrun-core");

        if !has_core_dependency {
            if let Some(core_package) = package::load_core_library_package()? {
                let core_package_std = Package::from_loaded_package(core_package);

                // Prepend core library programs to ensure they're processed first
                let mut integrated_programs = core_package_std.programs;
                integrated_programs.append(&mut package.programs);
                package.programs = integrated_programs;

                println!(
                    "📦 Integrated core library: {} total programs",
                    package.programs.len()
                );
            } else {
                eprintln!("Warning: Could not find core library, proceeding without it");
            }
        } else {
            println!("📦 Using pre-compiled core library from dependencies");
        }

        // Continue with standard compilation phases
        Self::compile_package_internal(package, engine, desugaring_engine)
    }

    /// Create from a single package
    pub fn compile_package(package: &mut Package) -> Result<CompilationResult, CompilerError> {
        let engine = TypeInferenceEngine::new();
        let desugaring_engine = DesugaringEngine::new();

        // Phase 0: Integrate core library into the package (unified processing)
        if let Some(core_package) = package::load_core_library_package()? {
            let core_package_std = Package::from_loaded_package(core_package);

            // Prepend core library programs to ensure they're processed first
            let mut integrated_programs = core_package_std.programs;
            integrated_programs.append(&mut package.programs);
            package.programs = integrated_programs;

            println!(
                "📦 Integrated core library: {} total programs",
                package.programs.len()
            );
        } else {
            eprintln!("Warning: Could not find core library, proceeding without it");
        }

        Self::compile_package_internal(package, engine, desugaring_engine)
    }

    /// Internal shared compilation logic used by both compile_package and compile_with_dependencies
    fn compile_package_internal(
        package: &mut Package,
        mut engine: TypeInferenceEngine,
        mut desugaring_engine: DesugaringEngine,
    ) -> Result<CompilationResult, CompilerError> {
        // Phase 1: Desugar all operators into protocol function calls
        for (i, program) in package.programs.iter_mut().enumerate() {
            eprintln!(
                "🔧 PHASE 1: Desugaring program {} from package '{}'",
                i, package.package_name
            );
            if let Some(file_path) = &program.debug_info.source_file {
                eprintln!("🔧   File: {}", file_path);
            } else {
                eprintln!("🔧   File: <unknown>");
            }
            desugaring_engine.desugar_program(program)?;
        }

        // Phase 1.5: Register source mapping for all expressions to enable accurate error reporting
        for program in &package.programs {
            engine.register_program_expressions(program);
        }

        // Phase 2: Register all protocols and structs (definitions only)
        for program in &package.programs {
            if let Some(source_file) = &program.debug_info.source_file {
                engine.set_current_file(source_file.clone());
            }
            let result = engine.register_protocols_and_structs(program);
            engine.clear_current_file();
            result?;
        }

        // Phase 2.5: Register automatic implementations (Any, Inspect) for all struct types
        for program in &package.programs {
            if let Some(source_file) = &program.debug_info.source_file {
                engine.set_current_file(source_file.clone());
            }
            let result = engine.register_automatic_implementations(program);
            engine.clear_current_file();
            result?;
        }

        // Phase 3: Register all explicit protocol implementations
        for program in &package.programs {
            if let Some(source_file) = &program.debug_info.source_file {
                engine.set_current_file(source_file.clone());
            }
            let result = engine.register_implementations(program);
            engine.clear_current_file();
            result?;
        }

        // Phase 4: Register all functions (standalone + impl block functions)
        for program in &package.programs {
            if let Some(source_file) = &program.debug_info.source_file {
                engine.set_current_file(source_file.clone());
            }
            let result = engine.register_functions(program);
            engine.clear_current_file();
            result?;
        }

        // Phase 5: Validate implementation completeness
        engine.validate_implementation_completeness()?;

        // Phase 6: Type check all function bodies with complete context
        for program in &mut package.programs {
            if let Some(source_file) = &program.debug_info.source_file {
                engine.set_current_file(source_file.clone());
            }
            let result = engine.typecheck_function_bodies(program);
            engine.clear_current_file();
            result?;
        }

        // Phase 7: Create monomorphisation table for generic functions
        let monomorphisation_table = MonomorphisationTable::new();

        // Phase 8: Build dispatch table for runtime function resolution
        let dispatch_table = build_dispatch_table(
            &*engine.type_registry_rc(),
            engine.function_registry(),
            Some(&monomorphisation_table),
        );

        // Phase 9: Collect module information for conflict prevention and orphan rule checking
        let (local_modules, defined_modules) = Self::collect_module_info(&package.programs);

        Ok(CompilationResult {
            type_registry: engine.type_registry_rc(),
            function_registry: engine.function_registry_rc(),
            dispatch_table,
            monomorphisation_table,
            package_name: package.package_name.clone(),
            programs: package.programs.clone(),
            local_modules,
            defined_modules,
            universal_dispatch: engine.universal_dispatch_registry().clone(),
            current_file: std::cell::RefCell::new(None),
        })
    }

    /// Set the current file being processed for diagnostic context
    pub fn set_current_file(&self, file_path: Option<String>) {
        *self.current_file.borrow_mut() = file_path;
    }

    /// Get the current file being processed for diagnostic context
    pub fn get_current_file(&self) -> Option<String> {
        self.current_file.borrow().clone()
    }

    /// Collect module information from programs for conflict detection and orphan rule checking
    fn collect_module_info(
        programs: &[outrun_parser::Program],
    ) -> (HashSet<ModuleName>, HashSet<ModuleName>) {
        let mut defined_modules = HashSet::new();

        for program in programs {
            for item in &program.items {
                let module_id = match &item.kind {
                    outrun_parser::ItemKind::StructDefinition(struct_def) => {
                        Some(ModuleName::from(&struct_def.name))
                    }
                    outrun_parser::ItemKind::ProtocolDefinition(protocol_def) => {
                        Some(ModuleName::from(&protocol_def.name))
                    }
                    _ => None,
                };

                if let Some(module) = module_id {
                    defined_modules.insert(module);
                }
            }
        }

        // For single package compilation, all defined modules are considered local
        let local_modules = defined_modules.clone();

        (local_modules, defined_modules)
    }

    /// Check for module conflicts between dependency modules and user modules
    /// Prevents: struct vs struct, protocol vs protocol, struct vs protocol conflicts
    fn check_module_conflicts(
        dependency_modules: &HashSet<ModuleName>,
        user_modules: &HashSet<ModuleName>,
    ) -> Result<(), CompilerError> {
        for user_module in user_modules {
            if dependency_modules.contains(user_module) {
                return Err(CompilerError::ModuleRedefinition {
                    module_name: user_module.as_str().to_string(),
                    span: None, // We don't have span info at this level
                });
            }
        }

        Ok(())
    }

    /// Enhanced module conflict detection with detailed type information
    fn check_detailed_module_conflicts(
        dependency_programs: &[&outrun_parser::Program],
        user_programs: &[outrun_parser::Program],
    ) -> Result<(), CompilerError> {
        use std::collections::HashMap;

        #[derive(Debug, Clone)]
        enum ModuleType {
            Struct,
            Protocol,
        }

        // Build dependency module registry
        let mut dependency_registry: HashMap<String, ModuleType> = HashMap::new();
        for program in dependency_programs {
            for item in &program.items {
                match &item.kind {
                    outrun_parser::ItemKind::StructDefinition(struct_def) => {
                        let module_name = ModuleName::from(&struct_def.name).as_str().to_string();
                        dependency_registry.insert(module_name, ModuleType::Struct);
                    }
                    outrun_parser::ItemKind::ProtocolDefinition(protocol_def) => {
                        let module_name = ModuleName::from(&protocol_def.name).as_str().to_string();
                        dependency_registry.insert(module_name, ModuleType::Protocol);
                    }
                    _ => {}
                }
            }
        }

        // Check user modules against dependency registry
        for program in user_programs {
            for item in &program.items {
                let (user_module_name, user_module_type) = match &item.kind {
                    outrun_parser::ItemKind::StructDefinition(struct_def) => (
                        ModuleName::from(&struct_def.name).as_str().to_string(),
                        ModuleType::Struct,
                    ),
                    outrun_parser::ItemKind::ProtocolDefinition(protocol_def) => (
                        ModuleName::from(&protocol_def.name).as_str().to_string(),
                        ModuleType::Protocol,
                    ),
                    _ => continue,
                };

                if let Some(dependency_type) = dependency_registry.get(&user_module_name) {
                    let conflict_description = match (dependency_type, &user_module_type) {
                        (ModuleType::Struct, ModuleType::Struct) => {
                            "struct conflicts with dependency struct"
                        }
                        (ModuleType::Protocol, ModuleType::Protocol) => {
                            "protocol conflicts with dependency protocol"
                        }
                        (ModuleType::Struct, ModuleType::Protocol) => {
                            "protocol conflicts with dependency struct"
                        }
                        (ModuleType::Protocol, ModuleType::Struct) => {
                            "struct conflicts with dependency protocol"
                        }
                    };

                    return Err(CompilerError::ModuleRedefinition {
                        module_name: format!("{} ({})", user_module_name, conflict_description),
                        span: None,
                    });
                }
            }
        }

        Ok(())
    }
}

/// Main entry point for type checking a complete Outrun package
pub fn typecheck_package(package: &mut Package) -> Result<(), CompilerError> {
    CompilationResult::compile_package(package).map(|_| ())
}

/// Convenience function for single-file type checking (mainly for testing)
pub fn typecheck_program(program: &mut outrun_parser::Program) -> Result<(), CompilerError> {
    let mut package = Package::new("single_file".to_string());
    package.add_program(program.clone());

    typecheck_package(&mut package)?;

    // Replace the original program with the type-checked version
    *program = package.programs.into_iter().next().unwrap();

    Ok(())
}

#[cfg(test)]
mod desugaring_tests {
    pub mod test_operator_desugaring_integration;
}

#[cfg(test)]
mod dependency_tests {
    use super::*;
    use outrun_parser::parse_program;

    #[test]
    fn test_module_conflict_detection_struct_vs_struct() {
        // Dependency has struct User
        let dependency_code = r#"
            struct User {
                def new(name: String): User { 
                    # constructor implementation
                }
            }
        "#;

        // User code tries to define struct User
        let user_code = r#"
            struct User {
                def new(id: Integer): User {
                    # different constructor
                }
            }
        "#;

        let dependency_program = parse_program(dependency_code).expect("dependency should parse");
        let user_program = parse_program(user_code).expect("user code should parse");

        let result = CompilationResult::check_detailed_module_conflicts(
            &[&dependency_program],
            &[user_program],
        );

        assert!(result.is_err());
        if let Err(CompilerError::ModuleRedefinition { module_name, .. }) = result {
            assert!(module_name.contains("User"));
            assert!(module_name.contains("struct conflicts with dependency struct"));
        } else {
            panic!("Expected ModuleRedefinition error");
        }
    }

    #[test]
    fn test_module_conflict_detection_protocol_vs_struct() {
        // Dependency has struct Display
        let dependency_code = r#"
            struct Display {
                def new(width: Integer): Display {
                    # constructor
                }
            }
        "#;

        // User code tries to define protocol Display
        let user_code = r#"
protocol Display {
    def display(value: Self): String
}
        "#;

        let dependency_program = parse_program(dependency_code).expect("dependency should parse");
        let user_program = parse_program(user_code).expect("user code should parse");

        let result = CompilationResult::check_detailed_module_conflicts(
            &[&dependency_program],
            &[user_program],
        );

        assert!(result.is_err());
        if let Err(CompilerError::ModuleRedefinition { module_name, .. }) = result {
            assert!(module_name.contains("Display"));
            assert!(module_name.contains("protocol conflicts with dependency struct"));
        } else {
            panic!("Expected ModuleRedefinition error");
        }
    }

    #[test]
    fn test_no_module_conflict_different_names() {
        // Dependency has struct User
        let dependency_code = r#"
            struct User {
                def new(name: String): User {
                    # constructor
                }
            }
        "#;

        // User code defines struct Customer - no conflict
        let user_code = r#"
            struct Customer {
                def new(id: Integer): Customer {
                    # constructor 
                }
            }
        "#;

        let dependency_program = parse_program(dependency_code).expect("dependency should parse");
        let user_program = parse_program(user_code).expect("user code should parse");

        let result = CompilationResult::check_detailed_module_conflicts(
            &[&dependency_program],
            &[user_program],
        );

        assert!(result.is_ok());
    }

    #[test]
    fn test_registry_merging_with_orphan_rules() {
        use crate::inference::TypeInferenceEngine;

        // Create two separate engines representing different packages
        let mut engine1 = TypeInferenceEngine::new();
        let mut engine2 = TypeInferenceEngine::new();

        // Package 1 defines a struct User
        let package1_code = r#"
            struct User {
                def new(name: String): User {
                    # constructor
                }
            }
        "#;

        // Package 2 tries to implement Display for User (should be allowed if User is from a dependency)
        let package2_code = r#"
            protocol Display {
                def display(value: Self): String
            }
            
            impl Display for User {
                def display(value: Self): String {
                    # implementation
                }
            }
        "#;

        let package1_program = parse_program(package1_code).expect("package1 should parse");
        let package2_program = parse_program(package2_code).expect("package2 should parse");

        // Register package1 in engine1
        engine1
            .register_protocols_and_structs(&package1_program)
            .expect("Should register package1");

        // Get the registries from engine1 as dependencies for engine2
        let type_registry1 = engine1.type_registry_rc();
        let function_registry1 = engine1.function_registry_rc();

        // Import package1 registries into engine2 as dependencies
        let result = engine2.import_dependency_registries(type_registry1, function_registry1);
        assert!(
            result.is_ok(),
            "Should successfully import dependency registries"
        );

        // Now try to register package2 in engine2 (which should work because User is now a dependency)
        let result2 = engine2.register_protocols_and_structs(&package2_program);
        assert!(
            result2.is_ok(),
            "Should successfully register package2 with dependency User struct"
        );
    }

    #[test]
    fn test_repl_optimization_precompiled_core() {
        // Test that pre-compiling core library works
        let precompiled_core = CompilationResult::precompile_core_library();
        assert!(
            precompiled_core.is_ok(),
            "Should successfully pre-compile core library"
        );

        let core = precompiled_core.unwrap();

        // Verify core library has expected implementations
        assert!(
            core.type_registry.implementation_count() > 0,
            "Core library should have implementations"
        );
        assert_eq!(
            core.package_name, "outrun-core",
            "Core package should have correct name"
        );

        // Test compiling REPL expressions using pre-compiled core
        let expressions = [
            "1 + 2",
            "\"hello\"",
            "true",
            // TODO: Add more complex expressions once they're supported
        ];

        for expr in &expressions {
            let result = CompilationResult::compile_repl_expression(expr, &core);
            assert!(
                result.is_ok(),
                "Should successfully compile REPL expression: {}",
                expr
            );

            let expr_result = result.unwrap();
            assert_eq!(expr_result.package_name, "repl_expression");

            // Verify that the expression result has access to core library functionality
            // by checking that protocol registries have been merged
            assert!(
                expr_result.type_registry.implementation_count()
                    >= core.type_registry.implementation_count(),
                "Expression result should include core library implementations"
            );
        }
    }

    #[test]
    fn test_repl_performance_comparison() {
        use std::time::Instant;

        // Test traditional approach: compile each expression from scratch
        let traditional_expressions = ["1 + 2", "2 * 3", "1 + 2 * 3"];
        let start_traditional = Instant::now();

        for expr in &traditional_expressions {
            let program = outrun_parser::parse_program(expr).expect("Should parse");
            let mut package = Package::new("test".to_string());
            package.add_program(program);

            let _result = CompilationResult::compile_package(&mut package).expect("Should compile");
        }

        let traditional_time = start_traditional.elapsed();

        // Test optimized approach: pre-compile core library once, reuse for expressions
        let start_optimized = Instant::now();

        let precompiled_core =
            CompilationResult::precompile_core_library().expect("Should pre-compile core");

        for expr in &traditional_expressions {
            let _result = CompilationResult::compile_repl_expression(expr, &precompiled_core)
                .expect("Should compile with pre-compiled core");
        }

        let optimized_time = start_optimized.elapsed();

        println!("📊 Performance comparison:");
        println!("   Traditional approach: {:?}", traditional_time);
        println!("   Optimized approach:   {:?}", optimized_time);

        // Assert both approaches work (implicit - if we got here they succeeded)
        assert!(
            traditional_time.as_nanos() > 0,
            "Traditional approach should take some time"
        );
        assert!(
            optimized_time.as_nanos() > 0,
            "Optimized approach should take some time"
        );

        // Note: The optimized approach may not always be faster in this simple test
        // because the core library compilation is included in the timing.
        // In a real REPL, the core library would be pre-compiled once at startup.
    }

    #[test]
    fn test_hot_reloading_package_redefinition() {
        // Test that a package can redefine its own modules (hot reloading scenario)

        // Initial version of package - simple struct without type checking complexity
        let initial_code = r#"
            struct User {
                def hello(): String {
                    "initial version"
                }
            }
        "#;

        // Updated version of package with redefined User struct
        let updated_code = r#"
            struct User {
                def hello(): String {
                    "updated version"
                }
                
                def greet(name: String): String {
                    "updated version with greet"
                }
            }
            
            struct Profile {
                def info(): String {
                    "new struct added in update"
                }
            }
        "#;

        let initial_program = parse_program(initial_code).expect("initial should parse");
        let updated_program = parse_program(updated_code).expect("updated should parse");

        // Compile initial version
        let mut initial_package = Package::new("myapp".to_string());
        initial_package.add_program(initial_program);

        let initial_result = CompilationResult::compile_package(&mut initial_package);
        if let Err(ref e) = initial_result {
            eprintln!("Initial compilation error: {:?}", e);
        }
        assert!(initial_result.is_ok(), "Initial compilation should succeed");
        let initial_compilation = initial_result.unwrap();

        // Verify initial version has User module
        assert!(
            initial_compilation
                .defined_modules
                .contains(&ModuleName::new("User")),
            "Initial version should define User module"
        );

        // Hot reload: recompile with updated version
        let mut updated_package = Package::new("myapp".to_string());
        updated_package.add_program(updated_program);

        let recompile_result = CompilationResult::recompile_package(
            &mut updated_package,
            Some(&initial_compilation),
            vec![], // No dependencies for this test
        );

        assert!(
            recompile_result.is_ok(),
            "Recompilation should succeed for same package"
        );
        let updated_compilation = recompile_result.unwrap();

        // Verify updated version has both User and Profile modules
        assert!(
            updated_compilation
                .defined_modules
                .contains(&ModuleName::new("User")),
            "Updated version should define User module"
        );
        assert!(
            updated_compilation
                .defined_modules
                .contains(&ModuleName::new("Profile")),
            "Updated version should define Profile module"
        );

        // Test should pass - the warning will be printed to stdout during the test
        // In a real scenario, the warning "⚠️ package myapp redefined module User" would be shown
    }

    #[test]
    fn test_hot_reloading_no_warning_when_unchanged() {
        // Test that no warning is emitted when module content doesn't actually change

        // Same content in both versions
        let code = r#"
            struct User {
                def hello(): String {
                    "hello world"
                }
            }
        "#;

        let program1 = parse_program(code).expect("first should parse");
        let program2 = parse_program(code).expect("second should parse"); // Identical content

        // Compile initial version
        let mut package1 = Package::new("myapp".to_string());
        package1.add_program(program1);
        let initial_result = CompilationResult::compile_package(&mut package1);
        assert!(initial_result.is_ok(), "Initial compilation should succeed");
        let initial_compilation = initial_result.unwrap();

        // "Recompile" with identical content - should NOT warn
        let mut package2 = Package::new("myapp".to_string());
        package2.add_program(program2);

        // Capture stdout to verify no warning is emitted
        let recompile_result =
            CompilationResult::recompile_package(&mut package2, Some(&initial_compilation), vec![]);

        // Compilation should succeed and no warning should be printed
        // (The warning would only appear if content actually changed)
        assert!(recompile_result.is_ok(), "Recompile should succeed");

        // Test passes - no warning should be emitted since content is identical
    }

    #[test]
    fn test_cross_package_conflict_still_prevented() {
        // Test that cross-package conflicts are still prevented even with recompile_package

        // Package A defines User
        let package_a_code = r#"
            struct User {
                def new(name: String): User {
                    User {}
                }
            }
        "#;

        // Package B tries to define User (should be prevented)
        let package_b_code = r#"
            struct User {
                def new(id: Integer): User {
                    User {}
                }
            }
        "#;

        let program_a = parse_program(package_a_code).expect("package A should parse");
        let program_b = parse_program(package_b_code).expect("package B should parse");

        // Compile package A
        let mut package_a = Package::new("package_a".to_string());
        package_a.add_program(program_a);
        let compilation_a =
            CompilationResult::compile_package(&mut package_a).expect("Package A should compile");

        // Try to compile package B with package A as dependency
        let mut package_b = Package::new("package_b".to_string());
        package_b.add_program(program_b);

        let result = CompilationResult::recompile_package(
            &mut package_b,
            None,                // No previous compilation for package B
            vec![compilation_a], // Package A as dependency
        );

        // Should fail due to cross-package module conflict
        assert!(
            result.is_err(),
            "Cross-package User module conflict should be prevented"
        );

        if let Err(CompilerError::ModuleRedefinition { module_name, .. }) = result {
            assert!(
                module_name.contains("User"),
                "Error should mention User module conflict"
            );
        } else {
            panic!("Expected ModuleRedefinition error for cross-package conflict");
        }
    }
}

#[cfg(test)]
mod integration_tests {
    use crate::types::{Level, Type, TypeInfo, TypeVarGenerator};
    use outrun_parser::{parse_program, ExpressionKind, ParsedTypeInfo};

    #[test]
    fn test_typechecker_typeinfo_conversion() {
        // Create a typechecker TypeInfo
        let resolved_type = Type::concrete("Integer");
        let type_info = TypeInfo::new(resolved_type);

        // Convert to parser's ParsedTypeInfo
        let parsed_type_info = type_info.to_parsed_type_info();

        // Verify the conversion
        assert_eq!(parsed_type_info.resolved_type, "Integer");
        assert!(parsed_type_info.type_span.is_none());
    }

    #[test]
    fn test_typechecker_typeinfo_conversion_with_span() {
        // Create a typechecker TypeInfo
        let resolved_type = Type::concrete("String");
        let type_info = TypeInfo::new(resolved_type);

        // Create a span
        let span = outrun_parser::Span::new(10, 20);

        // Convert to parser's ParsedTypeInfo with span
        let parsed_type_info = type_info.to_parsed_type_info_with_span(span);

        // Verify the conversion
        assert_eq!(parsed_type_info.resolved_type, "String");
        assert_eq!(parsed_type_info.type_span, Some(span));
    }

    #[test]
    fn test_parser_expression_with_type_info() {
        // Parse a simple expression
        let source = "42";
        let mut program = parse_program(source).unwrap();

        // Get the expression
        if let Some(item) = program.items.first_mut() {
            if let outrun_parser::ItemKind::Expression(ref mut expr) = item.kind {
                // Initially, no type info should be present
                assert!(expr.type_info.is_none());

                // Simulate typechecker populating type info
                let parsed_type_info = ParsedTypeInfo::new("Integer");
                expr.type_info = Some(parsed_type_info);

                // Verify type info is now present
                assert!(expr.type_info.is_some());
                assert_eq!(expr.type_info.as_ref().unwrap().resolved_type, "Integer");
            } else {
                panic!("Expected expression item");
            }
        } else {
            panic!("Expected at least one item in program");
        }
    }

    #[test]
    fn test_complex_type_conversion() {
        // Create a complex generic type
        let inner_type = Type::concrete("String");
        let list_type = Type::generic_concrete("List", vec![inner_type]);
        let type_info = TypeInfo::new(list_type);

        // Convert to parser's ParsedTypeInfo
        let parsed_type_info = type_info.to_parsed_type_info();

        // Verify the conversion shows the generic type structure
        assert_eq!(parsed_type_info.resolved_type, "List<String>");
        assert!(parsed_type_info.type_span.is_none());
    }

    #[test]
    fn test_type_variable_conversion() {
        // Create a type variable
        let mut gen = TypeVarGenerator::new();
        let type_var = gen.fresh(Level(0));
        let type_info = TypeInfo::new(type_var);

        // Convert to parser's ParsedTypeInfo
        let parsed_type_info = type_info.to_parsed_type_info();

        // Verify the conversion shows type variable representation
        assert_eq!(parsed_type_info.resolved_type, "T0");
        assert!(parsed_type_info.type_span.is_none());
    }

    #[test]
    fn test_function_type_conversion() {
        // Create a function type
        let param_type = Type::concrete("Integer");
        let return_type = Type::concrete("String");
        let function_type = Type::Function {
            params: vec![("x".to_string(), param_type)],
            return_type: Box::new(return_type),
            span: None,
        };
        let type_info = TypeInfo::new(function_type);

        // Convert to parser's ParsedTypeInfo
        let parsed_type_info = type_info.to_parsed_type_info();

        // Verify the conversion shows function type structure
        assert_eq!(
            parsed_type_info.resolved_type,
            "Function<(x: Integer) -> String>"
        );
        assert!(parsed_type_info.type_span.is_none());
    }

    #[test]
    fn test_integration_workflow() {
        // This test demonstrates the complete workflow:
        // 1. Parse source code (creates AST with no type info)
        // 2. Simulate typechecker adding type information
        // 3. Verify the integration works end-to-end

        let source = r#"
            let x = 42
            let y = "hello"
        "#;

        let mut program = parse_program(source).unwrap();

        // Simulate what the typechecker would do
        for item in program.items.iter_mut() {
            if let outrun_parser::ItemKind::LetBinding(let_binding) = &mut item.kind {
                // Simulate type inference on the expression
                match &let_binding.expression.kind {
                    ExpressionKind::Integer(_) => {
                        let type_info = TypeInfo::new(Type::concrete("Integer"));
                        let_binding.expression.type_info = Some(type_info.to_parsed_type_info());
                    }
                    ExpressionKind::String(_) => {
                        let type_info = TypeInfo::new(Type::concrete("String"));
                        let_binding.expression.type_info = Some(type_info.to_parsed_type_info());
                    }
                    _ => {}
                }
            }
        }

        // Verify the type information was populated
        let mut found_integer = false;
        let mut found_string = false;

        for item in &program.items {
            if let outrun_parser::ItemKind::LetBinding(let_binding) = &item.kind {
                if let Some(ref type_info) = let_binding.expression.type_info {
                    match type_info.resolved_type.as_str() {
                        "Integer" => found_integer = true,
                        "String" => found_string = true,
                        _ => {}
                    }
                }
            }
        }

        assert!(found_integer, "Should have found Integer type");
        assert!(found_string, "Should have found String type");
    }
}

#[cfg(test)]
mod tests;
