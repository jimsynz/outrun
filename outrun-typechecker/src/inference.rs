//! Type inference engine orchestration layer
//!
//! This module provides the main orchestration layer that coordinates all typechecker components
//! to perform Hindley-Milner type inference with protocol constraints and dispatch resolution.

use crate::{
    dispatch::{
        FunctionDispatcher, FunctionInfo, FunctionRegistry,
        FunctionVisibility as DispatchVisibility,
    },
    error::{
        to_source_span, ErrorContext, InferenceError, TypecheckError,
        UnificationError as OriginalUnificationError,
    },
    registry::{ProtocolRegistry, TypeRegistry},
    types::{Constraint, Level, ModuleName, SelfBindingContext, Substitution, Type, TypeVarId},
};
use outrun_parser::{
    CaseExpression, ConstDefinition, Expression, FunctionDefinition, FunctionSignature, ImplBlock,
    Item, Program, ProtocolDefinition, ProtocolFunction, StaticFunctionDefinition,
    StructDefinition, TypeAnnotation, TypeSpec,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Main type inference engine that orchestrates all typechecker components
pub struct TypeInferenceEngine {
    /// Registry of function signatures (shared)
    function_registry: Rc<FunctionRegistry>,

    /// Unified type registry for protocols and concrete types (shared)
    type_registry: Rc<TypeRegistry>,

    /// Current module being processed
    current_module: ModuleName,

    /// Current file being processed for error reporting
    current_file: Option<String>,

    /// Counter for generating fresh type variables
    type_variable_counter: usize,

    /// Symbol table mapping variable names to types in current scope
    pub symbol_table: HashMap<String, Type>,

    /// Universal dispatch registry for clause-based function calls
    universal_dispatch_registry: crate::universal_dispatch::UniversalDispatchRegistry,

    /// Error context for enhanced error reporting
    error_context: ErrorContext,

    /// Current Self binding context for type resolution
    current_self_context: SelfBindingContext,

    /// Registry of struct definitions for field access resolution
    struct_registry: HashMap<String, outrun_parser::StructDefinition>,

    /// Current generic parameter context for type resolution
    generic_parameter_context: HashMap<String, Type>,

    /// Call stack backtracking support for enhanced constraint resolution
    constraint_solver_with_backtracking: Option<crate::constraints::ConstraintSolver>,

    /// Depth tracking for nested inference calls to prevent premature task state clearing
    inference_depth: u32,

    /// Exhaustiveness checker for pattern matching and guards
    exhaustiveness_checker: crate::exhaustiveness::ExhaustivenessChecker,
}

/// Unique identifier for inference tasks in the iterative system
pub type TaskId = u64;

/// State of an inference task in the iterative processing system
#[derive(Debug)]
pub enum TaskState {
    /// Task is waiting for its dependencies to complete
    Pending,
    /// All dependencies completed, task is ready for processing
    Ready,
    /// Task is currently being processed
    Processing,
    /// Task completed successfully with a result
    Completed(Box<InferenceResult>),
    /// Task failed with an error
    Failed(Box<TypecheckError>),
}

/// A single inference task in the iterative processing system
#[derive(Debug)]
pub struct InferenceTask {
    /// Unique identifier for this task
    pub id: TaskId,
    /// Pointer to the expression being inferred (avoids cloning large ASTs)
    pub expression: *mut Expression,
    /// Inference context for this task
    pub context: InferenceContext,
    /// Tasks that must complete before this task can be processed
    pub dependencies: Vec<TaskId>,
    /// Tasks that depend on this task completing
    pub dependents: Vec<TaskId>,
    /// Current state of this task
    pub state: TaskState,
    /// CRITICAL FIX: Track completed dependencies to retrieve argument types during processing
    /// Maps argument index to dependency task ID for function calls
    pub completed_dependencies: Vec<TaskId>,
}

/// Work item for stack-based continuation system
#[derive(Debug)]
enum WorkItem {
    /// Evaluate an expression by first processing its dependencies
    EvaluateExpression {
        expression: *mut Expression,
        context: InferenceContext,
    },

    /// Continue processing an expression using results from its dependencies
    ContinueWithResults {
        expression: *mut Expression,
        context: InferenceContext,
        dependency_count: usize,
    },
}

/// Context for type inference operations
#[derive(Debug, Clone)]
pub struct InferenceContext {
    /// Current substitution from unification
    pub substitution: Substitution,

    /// Accumulated constraints to be solved
    pub constraints: Vec<Constraint>,

    /// Expected type for current expression (for bidirectional inference)
    pub expected_type: Option<Type>,

    /// Self type binding context
    pub self_binding: SelfBindingContext,

    /// Local variable bindings in current scope
    pub bindings: HashMap<String, Type>,
}

/// Result of type inference on an expression
#[derive(Debug, Clone, PartialEq)]
pub struct InferenceResult {
    /// Inferred type of the expression
    pub inferred_type: Type,

    /// Constraints generated during inference
    pub constraints: Vec<Constraint>,

    /// Updated substitution
    pub substitution: Substitution,
}

/// Analysis result of a function signature for Self and generic type inference
#[derive(Debug, Clone, PartialEq)]
pub struct SignatureAnalysis {
    /// All positions where Self appears in the signature
    pub self_positions: Vec<crate::types::SelfPosition>,
    /// All type variable positions mapped by variable name
    pub type_variable_positions: HashMap<String, Vec<crate::types::TypePosition>>,
    /// Protocol constraints found in the signature
    pub protocol_constraints: Vec<ProtocolConstraint>,
    /// Unification constraints for same-variable occurrences
    pub unification_constraints: Vec<Constraint>,
}

/// Protocol constraint extracted from function signature
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolConstraint {
    /// The protocol being constrained
    pub protocol_id: ModuleName,
    /// The type that must implement the protocol
    pub constraining_type: Type,
    /// Position in signature where this constraint appears
    pub position: crate::types::TypePosition,
}

/// Function signature analyzer for comprehensive Self and generic type detection
pub struct FunctionSignatureAnalyzer {
    /// Current analysis being built
    current_analysis: SignatureAnalysis,
}

impl Default for FunctionSignatureAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionSignatureAnalyzer {
    /// Create a new signature analyzer
    pub fn new() -> Self {
        Self {
            current_analysis: SignatureAnalysis {
                self_positions: Vec::new(),
                type_variable_positions: HashMap::new(),
                protocol_constraints: Vec::new(),
                unification_constraints: Vec::new(),
            },
        }
    }

    /// Analyze a complete function signature
    pub fn analyze_signature(params: &[(String, Type)], return_type: &Type) -> SignatureAnalysis {
        let mut analyzer = Self::new();

        // Analyze all parameter types
        for (i, (param_name, param_type)) in params.iter().enumerate() {
            analyzer.analyze_type_in_position(
                param_type,
                &crate::types::TypePosition::ArgumentPosition {
                    index: i, // Keep index for backward compatibility
                    path: vec![],
                    param_name: Some(param_name.clone()), // Add parameter name!
                },
            );
        }

        // Analyze return type
        analyzer.analyze_type_in_position(
            return_type,
            &crate::types::TypePosition::ReturnPosition { path: vec![] },
        );

        // Generate unification constraints for same-variable occurrences
        analyzer.generate_unification_constraints();

        analyzer.current_analysis
    }

    /// Analyze a type in a specific position, collecting Self and type variable positions
    fn analyze_type_in_position(&mut self, typ: &Type, position: &crate::types::TypePosition) {
        // Collect Self positions
        for self_pos in typ.extract_self_positions() {
            let mut full_self_pos = self_pos.clone();
            // Combine position path with type's internal path
            match position {
                crate::types::TypePosition::ArgumentPosition {
                    index,
                    path,
                    param_name,
                } => {
                    // Use parameter name if available, otherwise fall back to index for compatibility
                    let position_identifier = if let Some(name) = param_name {
                        format!("param_{}", name)
                    } else {
                        format!("arg_{}", index)
                    };
                    let mut full_path = vec![position_identifier];
                    full_path.extend(path.clone());
                    full_path.extend(full_self_pos.path);
                    full_self_pos.path = full_path;
                }
                crate::types::TypePosition::ReturnPosition { path } => {
                    let mut full_path = vec!["return".to_string()];
                    full_path.extend(path.clone());
                    full_path.extend(full_self_pos.path);
                    full_self_pos.path = full_path;
                }
                _ => {} // Other position types handled similarly
            }
            self.current_analysis.self_positions.push(full_self_pos);
        }

        // Collect type variable positions
        for (var_name, positions) in typ.collect_type_variable_positions() {
            let entry = self
                .current_analysis
                .type_variable_positions
                .entry(var_name)
                .or_default();

            for var_pos in positions {
                let mut full_pos = var_pos.clone();
                // Add position context to variable position
                match position {
                    crate::types::TypePosition::ArgumentPosition { index, path, .. } => {
                        if let crate::types::TypePosition::VariablePosition {
                            path: ref mut var_path,
                        } = &mut full_pos
                        {
                            let mut full_path = vec![format!("arg_{}", index)];
                            full_path.extend(path.clone());
                            full_path.extend(var_path.clone());
                            *var_path = full_path;
                        }
                    }
                    crate::types::TypePosition::ReturnPosition { path } => {
                        if let crate::types::TypePosition::VariablePosition {
                            path: ref mut var_path,
                        } = &mut full_pos
                        {
                            let mut full_path = vec!["return".to_string()];
                            full_path.extend(path.clone());
                            full_path.extend(var_path.clone());
                            *var_path = full_path;
                        }
                    }
                    _ => {} // Other position types handled similarly
                }
                entry.push(full_pos);
            }
        }

        // Collect protocol constraints
        if typ.is_protocol_constraint() {
            match typ {
                Type::Protocol { name, args, .. } => {
                    for (i, arg) in args.iter().enumerate() {
                        self.current_analysis
                            .protocol_constraints
                            .push(ProtocolConstraint {
                                protocol_id: name.clone(),
                                constraining_type: arg.clone(),
                                position: crate::types::TypePosition::GenericArgument {
                                    container: name.as_str().to_string(),
                                    arg_index: i,
                                    path: vec![],
                                },
                            });
                    }
                }
                _ => {} // Other protocol constraint patterns
            }
        }
    }

    /// Generate unification constraints for same-variable occurrences
    fn generate_unification_constraints(&mut self) {
        // For each type variable that appears multiple times, generate equality constraints
        for (var_name, positions) in &self.current_analysis.type_variable_positions {
            if positions.len() > 1 {
                // Create equality constraints between all occurrences
                for _i in 1..positions.len() {
                    self.current_analysis
                        .unification_constraints
                        .push(Constraint::Equality {
                            left: Box::new(Type::Variable {
                                var_id: TypeVarId(0), // Placeholder - will be resolved during inference
                                level: Level(0),
                                name: Some(var_name.clone()),
                                span: None,
                            }),
                            right: Box::new(Type::Variable {
                                var_id: TypeVarId(0), // Placeholder - will be resolved during inference
                                level: Level(0),
                                name: Some(var_name.clone()),
                                span: None,
                            }),
                            span: None,
                        });
                }
            }
        }

        // Generate constraints for Self occurrences (all Self in a signature must unify)
        if self.current_analysis.self_positions.len() > 1 {
            for i in 1..self.current_analysis.self_positions.len() {
                let first_self = &self.current_analysis.self_positions[0];
                let other_self = &self.current_analysis.self_positions[i];

                self.current_analysis
                    .unification_constraints
                    .push(Constraint::SelfBinding {
                        self_context: first_self.binding_context.clone(),
                        bound_type: Box::new(Type::SelfType {
                            binding_context: other_self.binding_context.clone(),
                            span: None,
                        }),
                        span: None,
                    });
            }
        }
    }

    /// Infer Self type from function call arguments using signature analysis
    pub fn infer_self_type(
        analysis: &SignatureAnalysis,
        call_args: &[Type],
    ) -> Result<Option<Type>, TypecheckError> {
        // Find Self positions in the signature
        for self_pos in &analysis.self_positions {
            // Try to extract Self type from corresponding argument
            if let Some(self_type) =
                Self::extract_self_from_argument_position(&self_pos.path, call_args)?
            {
                return Ok(Some(self_type));
            }
        }

        Ok(None) // No Self type could be inferred
    }

    /// Extract Self type from a specific argument position
    fn extract_self_from_argument_position(
        path: &[String],
        call_args: &[Type],
    ) -> Result<Option<Type>, TypecheckError> {
        if path.is_empty() {
            return Ok(None);
        }

        // Parse argument index from path (e.g., "arg_0" -> 0)
        if let Some(arg_part) = path.first() {
            if let Some(index_str) = arg_part.strip_prefix("arg_") {
                if let Ok(arg_index) = index_str.parse::<usize>() {
                    if let Some(arg_type) = call_args.get(arg_index) {
                        // Navigate through the remaining path to find Self
                        return Self::navigate_type_path(arg_type, &path[1..]);
                    }
                }
            }
        }

        Ok(None)
    }

    /// Navigate through a type following a path to extract the target type
    fn navigate_type_path(typ: &Type, path: &[String]) -> Result<Option<Type>, TypecheckError> {
        if path.is_empty() {
            return Ok(Some(typ.clone()));
        }

        match typ {
            Type::Concrete { args, .. } | Type::Protocol { args, .. } => {
                if let Some(index_str) = path.first() {
                    if let Ok(index) = index_str.parse::<usize>() {
                        if let Some(arg_type) = args.get(index) {
                            return Self::navigate_type_path(arg_type, &path[1..]);
                        }
                    }
                }
            }
            Type::Tuple { element_types, .. } => {
                if let Some(index_str) = path.first() {
                    if let Ok(index) = index_str.parse::<usize>() {
                        if let Some(element_type) = element_types.get(index) {
                            return Self::navigate_type_path(element_type, &path[1..]);
                        }
                    }
                }
            }
            _ => {} // Other types don't have navigable structure
        }

        Ok(None)
    }
}

impl TypeInferenceEngine {
    /// Create a new type inference engine with provided registries
    pub fn with_registries(
        function_registry: Rc<FunctionRegistry>,
        type_registry: Rc<TypeRegistry>,
    ) -> Self {
        let mut exhaustiveness_checker = crate::exhaustiveness::ExhaustivenessChecker::new();
        exhaustiveness_checker.set_type_registry(type_registry.clone());

        Self {
            function_registry,
            type_registry,
            current_module: ModuleName::new("main"),
            current_file: None,
            type_variable_counter: 0,
            symbol_table: HashMap::new(),
            universal_dispatch_registry: crate::universal_dispatch::UniversalDispatchRegistry::new(
            ),
            error_context: ErrorContext::new(),
            current_self_context: SelfBindingContext::ProtocolDefinition {
                protocol_name: ModuleName::new("Unknown"),
                protocol_args: Vec::new(),
            },
            struct_registry: HashMap::new(),
            generic_parameter_context: HashMap::new(),
            constraint_solver_with_backtracking: None,
            inference_depth: 0,
            exhaustiveness_checker,
        }
    }

    /// Create a new type inference engine with hardcoded core types (legacy)
    pub fn new() -> Self {
        let mut function_registry = FunctionRegistry::new();

        // Register all intrinsic functions that are provided by the runtime
        crate::intrinsics::register_intrinsics(&mut function_registry);

        let function_registry = Rc::new(function_registry);
        let type_registry = Rc::new(TypeRegistry::with_core_types());

        Self::with_registries(function_registry, type_registry)
    }

    /// Create a bootstrap engine for compiling core library (no hardcoded types)
    pub fn bootstrap() -> Self {
        let mut function_registry = FunctionRegistry::new();

        // Register all intrinsic functions that are provided by the runtime
        crate::intrinsics::register_intrinsics(&mut function_registry);

        let function_registry = Rc::new(function_registry);
        let type_registry = Rc::new(TypeRegistry::new()); // Empty registry!

        Self::with_registries(function_registry, type_registry)
    }

    /// Generate a fresh type variable
    pub fn fresh_type_var(&mut self) -> TypeVarId {
        let var_id = TypeVarId(self.type_variable_counter.try_into().unwrap());
        self.type_variable_counter += 1;
        var_id
    }

    /// Set the current module being processed
    pub fn set_current_module(&mut self, module: ModuleName) {
        self.current_module = module.clone();
        // Update the protocol registry within the type registry
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry.set_current_module(module);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry.set_current_module(module);
            self.type_registry = Rc::new(new_type_registry);
        }
    }

    /// Set the current file being processed for error reporting
    pub fn set_current_file(&mut self, file: String) {
        self.current_file = Some(file);
    }

    /// Clear the current file context
    pub fn clear_current_file(&mut self) {
        self.current_file = None;
    }

    /// Get the current file context
    pub fn get_current_file(&self) -> Option<&String> {
        self.current_file.as_ref()
    }

    /// Create a FileSpan using current file context
    pub fn create_file_span(&self, span: Option<outrun_parser::Span>) -> crate::error::FileSpan {
        crate::error::FileSpan {
            span: span.unwrap_or(outrun_parser::Span {
                start: 0,
                end: 0,
                start_line_col: None,
                end_line_col: None,
            }),
            source_file: self
                .current_file
                .clone()
                .unwrap_or_else(|| "unknown".to_string()),
        }
    }

    /// Register program expressions for source mapping (stub for now)
    pub fn register_program_expressions(&mut self, _program: &outrun_parser::Program) {
        // TODO: Implement expression-to-source mapping if needed
        // For now, we're using file context tracking instead
    }

    /// Get a mutable reference to the protocol registry within the type registry
    pub fn protocol_registry_mut(&mut self) -> &mut TypeRegistry {
        // Clone and replace the type registry to get mutable access
        let new_registry = (*self.type_registry).clone();
        self.type_registry = Rc::new(new_registry);
        Rc::get_mut(&mut self.type_registry).expect("Should be uniquely owned after replacement")
    }

    /// Get mutable access to type registry for testing and configuration
    pub fn type_registry_mut(&mut self) -> &mut TypeRegistry {
        // Always clone and replace to avoid borrow checker issues
        let new_registry = (*self.type_registry).clone();
        self.type_registry = Rc::new(new_registry);
        Rc::get_mut(&mut self.type_registry).expect("Should be uniquely owned after replacement")
    }

    pub fn function_registry_mut(&mut self) -> &mut FunctionRegistry {
        // Always clone and replace to avoid borrow checker issues
        let new_registry = (*self.function_registry).clone();
        self.function_registry = Rc::new(new_registry);
        Rc::get_mut(&mut self.function_registry)
            .expect("Should be uniquely owned after replacement")
    }

    pub fn function_registry(&self) -> &FunctionRegistry {
        &self.function_registry
    }

    /// Get a clone of the function registry Rc for external use
    pub fn function_registry_rc(&self) -> std::rc::Rc<FunctionRegistry> {
        self.function_registry.clone()
    }

    /// Get a cloned Rc to the type registry for compilation results
    pub fn type_registry_rc(&self) -> std::rc::Rc<TypeRegistry> {
        self.type_registry.clone()
    }

    /// Register a forward binding for a type referenced before definition
    #[allow(clippy::result_large_err)]
    fn register_forward_binding(
        &mut self,
        type_name: &str,
        expected_arity: usize,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> Result<(), TypecheckError> {
        use outrun_parser::TypeAnnotation;

        let span = match type_annotation {
            TypeAnnotation::Simple { span, .. } => *span,
            TypeAnnotation::Tuple { span, .. } => *span,
            TypeAnnotation::Function { span, .. } => *span,
        };

        let module_name = ModuleName::new(type_name);

        // Check if we already have this forward binding
        if let Some(existing_module) = self.type_registry.get_module(type_name) {
            match existing_module {
                crate::types::TypeModule::ForwardBinding {
                    expected_arity: existing_arity,
                    references,
                    ..
                } => {
                    // Update existing forward binding with new reference
                    let mut updated_references = references.clone();
                    updated_references.push(span);

                    // Check for arity conflicts
                    if let Some(existing) = existing_arity {
                        if *existing != expected_arity {
                            return Err(TypecheckError::TypeError(
                                crate::error::TypeError::ArityConflict {
                                    type_name: type_name.to_string(),
                                    expected_arity: *existing,
                                    found_arity: expected_arity,
                                    span: crate::error::to_source_span(Some(span))
                                        .unwrap_or_else(|| miette::SourceSpan::from(0..0)),
                                },
                            ));
                        }
                    }

                    // Update the forward binding with new reference
                    let updated_forward_binding = crate::types::TypeModule::ForwardBinding {
                        name: module_name.clone(),
                        expected_arity: Some(expected_arity),
                        source_location: span,
                        references: updated_references,
                    };

                    self.type_registry_mut()
                        .register_module(updated_forward_binding)?;
                }
                _ => {
                    // Type already exists as concrete type or protocol - no need for forward binding
                    return Ok(());
                }
            }
        } else {
            // Create new forward binding
            let forward_binding = crate::types::TypeModule::ForwardBinding {
                name: module_name,
                expected_arity: Some(expected_arity),
                source_location: span,
                references: vec![span],
            };

            self.type_registry_mut().register_module(forward_binding)?;
        }

        Ok(())
    }

    /// Check if a type satisfies a protocol (legacy compatibility)
    pub fn type_satisfies_protocol(
        &self,
        type_name: &ModuleName,
        protocol_name: &ModuleName,
    ) -> bool {
        // Simple implementation - check if there's an implementation module
        let impl_name = ModuleName::implementation(type_name.as_str(), protocol_name.as_str());
        self.type_registry.get_module(impl_name.as_str()).is_some()
    }

    /// Check case exhaustiveness using proper type compatibility from the inference engine
    fn check_case_exhaustiveness_with_proper_types(
        &mut self,
        case_expr: &CaseExpression,
        scrutinee_type: &Type,
    ) -> Result<(), TypecheckError> {
        // PERFORMANCE FIX: Use the type registry directly instead of creating temporary engines
        // The exhaustiveness checker already has access to the type registry and can use
        // has_implementation() to check type compatibility efficiently
        self.exhaustiveness_checker
            .check_case_exhaustiveness_with_registry(case_expr, scrutinee_type)
    }

    /// Analyze recursive type patterns and emit compiler warnings
    #[allow(clippy::result_large_err)]
    fn analyze_recursive_patterns_and_emit_warnings(&mut self) -> Result<(), TypecheckError> {
        // Create a constraint solver to analyze the protocol registry
        let mut constraint_solver =
            crate::constraints::ConstraintSolver::with_registry((*self.type_registry).clone());

        // Analyze recursive patterns in protocol implementations
        constraint_solver.analyze_recursive_type_patterns();

        // Collect any warnings and add them to the error context
        let warnings = constraint_solver.get_warnings();
        for warning in warnings {
            // For now, we'll print warnings to stderr
            // In the future, this could be integrated with a proper compiler warning system
            match warning {
                crate::constraints::CompilerWarning::RecursiveProtocolImplementation {
                    protocol_name,
                    implementing_type,
                    explanation,
                    impact,
                    suggestions,
                    ..
                } => {
                    // TODO: replace with compiler-built-in warning system.
                    eprintln!("⚠️  Recursive protocol implementation detected:");
                    eprintln!("   Protocol: {}", protocol_name);
                    eprintln!("   Type: {}", implementing_type);
                    eprintln!("   {}", explanation);
                    eprintln!("   Impact: {}", impact);
                    for suggestion in suggestions {
                        eprintln!("   Suggestion: {}", suggestion);
                    }
                    eprintln!();
                }
                crate::constraints::CompilerWarning::HighConcreteCombinationCount { .. } => {
                    // Handle other warning types as they're implemented
                }
                crate::constraints::CompilerWarning::ComplexGenericConstraints { .. } => {
                    // Handle other warning types as they're implemented
                }
            }
        }

        Ok(())
    }

    /// Initialize call stack backtracking for enhanced constraint resolution
    /// This should be called before type checking function bodies
    fn initialize_call_stack_backtracking(&mut self) {
        // Create a constraint solver with backtracking capabilities
        let mut constraint_solver =
            crate::constraints::ConstraintSolver::with_registry((*self.type_registry).clone());

        // Initialize call stack context with reasonable depth limit
        let call_stack_context = crate::constraints::CallStackContext::new(10);
        constraint_solver.set_call_stack_context(call_stack_context);

        // Store the constraint solver for use during type inference
        self.constraint_solver_with_backtracking = Some(constraint_solver);
    }

    /// Get mutable access to the constraint solver with backtracking (if initialized)
    pub fn constraint_solver_with_backtracking_mut(
        &mut self,
    ) -> Option<&mut crate::constraints::ConstraintSolver> {
        self.constraint_solver_with_backtracking.as_mut()
    }

    /// Get immutable access to the constraint solver with backtracking (if initialized)
    pub fn constraint_solver_with_backtracking(
        &self,
    ) -> Option<&crate::constraints::ConstraintSolver> {
        self.constraint_solver_with_backtracking.as_ref()
    }

    /// Get the universal dispatch registry containing all registered clauses
    pub fn universal_dispatch_registry(
        &self,
    ) -> &crate::universal_dispatch::UniversalDispatchRegistry {
        &self.universal_dispatch_registry
    }

    /// Import dependency registries into this engine for package composition
    pub fn import_dependency_registries(
        &mut self,
        dependency_type_registry: std::rc::Rc<TypeRegistry>,
        dependency_function_registry: std::rc::Rc<FunctionRegistry>,
    ) -> Result<(), crate::error::TypecheckError> {
        // Merge the dependency registries into our existing ones while preserving orphan rules
        // Uses intelligent registry merging with conflict detection and proper precedence
        if let Some(type_registry_mut) = std::rc::Rc::get_mut(&mut self.type_registry) {
            if let Err(e) = type_registry_mut.merge_with_dependency(&dependency_type_registry) {
                return Err(crate::error::TypecheckError::TypeError(e));
            }
        } else {
            // Fallback to replacement if we can't get mutable reference
            self.type_registry = dependency_type_registry;
        }

        // Merge function registry with proper precedence (local over dependencies)
        if let Some(function_registry_mut) = std::rc::Rc::get_mut(&mut self.function_registry) {
            function_registry_mut.merge_from_dependency(&dependency_function_registry);
        } else {
            // Fallback to replacement if we can't get mutable reference
            self.function_registry = dependency_function_registry;
        }

        Ok(())
    }

    /// Import dependency registries including universal dispatch registry for package composition
    pub fn import_dependency_registries_with_universal_dispatch(
        &mut self,
        dependency_type_registry: std::rc::Rc<TypeRegistry>,
        dependency_function_registry: std::rc::Rc<FunctionRegistry>,
        dependency_universal_dispatch: crate::universal_dispatch::UniversalDispatchRegistry,
    ) -> Result<(), crate::error::TypecheckError> {
        // Import type and function registries
        self.type_registry = dependency_type_registry;
        self.function_registry = dependency_function_registry;

        // Import universal dispatch registry with intelligent merging
        // Preserves clause IDs and maintains deterministic dispatch order
        let merge_result = self
            .universal_dispatch_registry
            .merge_with_dependency(&dependency_universal_dispatch);

        // Log merging statistics for debugging
        if merge_result.added_clauses > 0 || merge_result.has_conflicts() {
            println!(
                "Universal dispatch merge: {} clauses added, {} conflicts",
                merge_result.added_clauses,
                merge_result.conflicts.len()
            );
        }

        Ok(())
    }

    /// Set generic parameter context for type resolution
    pub fn set_generic_parameter_context(&mut self, context: HashMap<String, Type>) {
        self.generic_parameter_context = context;
    }

    /// Clear generic parameter context
    pub fn clear_generic_parameter_context(&mut self) {
        self.generic_parameter_context.clear();
    }

    /// Add a single generic parameter to the context
    pub fn add_generic_parameter(&mut self, name: String, type_: Type) {
        self.generic_parameter_context.insert(name, type_);
    }

    /// Create generic parameter context from a list of parameter names
    /// This is used when entering struct/protocol/impl blocks with generic parameters
    pub fn create_generic_context_from_names(
        &mut self,
        param_names: &[String],
    ) -> HashMap<String, Type> {
        let mut context = HashMap::new();
        for name in param_names {
            // Create a fresh type variable for each generic parameter
            let type_var = Type::Variable {
                var_id: self.fresh_type_var(),
                level: Level(0),
                name: Some(name.clone()),
                span: None,
            };
            context.insert(name.clone(), type_var);
        }
        context
    }

    /// Get a reference to the protocol registry for testing
    #[cfg(test)]
    pub fn get_protocol_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    /// Get a reference to the protocol registry for dispatch table building
    pub fn protocol_registry(&self) -> &ProtocolRegistry {
        &self.type_registry
    }

    /// Test helper to expose types_are_compatible for testing
    #[cfg(test)]
    pub fn test_types_are_compatible(&self, found_type: &Type, expected_type: &Type) -> bool {
        self.types_are_compatible(found_type, expected_type)
    }

    /// Update error context when we discover new symbols
    fn update_error_context(&mut self) {
        // Collect available variables from symbol table
        self.error_context.available_variables = self.symbol_table.keys().cloned().collect();

        // Collect available types from type registry
        self.error_context.available_types = self.type_registry.get_concrete_type_names();

        self.error_context.available_protocols = vec![
            "BinaryAddition".to_string(),
            "BinarySubtraction".to_string(),
            "BinaryMultiplication".to_string(),
            "BinaryDivision".to_string(),
            "Equality".to_string(),
            "Comparison".to_string(),
            "LogicalNot".to_string(),
            "UnaryMinus".to_string(),
            "ToString".to_string(),
        ];

        // Set current module context
        self.error_context.current_module = Some(self.current_module.as_str().to_string());
    }

    /// Bind a variable in the current scope (used for testing and debugging)
    pub fn bind_variable(&mut self, name: &str, var_type: Type) {
        self.symbol_table.insert(name.to_string(), var_type);
        self.update_error_context();
    }

    /// Type check a complete program using new phase structure
    #[allow(clippy::result_large_err)]
    pub fn typecheck_program(&mut self, program: &mut Program) -> Result<(), TypecheckError> {
        // Updated to use new 6-phase structure (skipping Phase 1: Desugar since that's handled at package level)

        // Phase 2: Register all protocols and structs (definitions only)
        self.register_protocols_and_structs(program)?;

        // Phase 2.5: Register automatic implementations (Any, Inspect) for all struct types
        self.register_automatic_implementations(program)?;

        // Phase 3: Register all explicit protocol implementations
        self.register_implementations(program)?;

        // Phase 4: Register all functions (standalone + impl block functions)
        self.register_functions(program)?;

        // Phase 5: Validate implementation completeness
        self.validate_implementation_completeness()?;

        // Phase 6: Type check all function bodies with complete context
        self.typecheck_function_bodies(program)?;

        Ok(())
    }

    /// Type check only the items in a program (for use within package processing)
    #[allow(clippy::result_large_err)]
    pub fn typecheck_program_items_only(
        &mut self,
        program: &mut Program,
    ) -> Result<(), TypecheckError> {
        // Type check all items without re-registering implementations
        for item in &mut program.items {
            self.typecheck_item(item)?;
        }

        Ok(())
    }

    /// Collect all type and protocol definitions from the program
    #[allow(clippy::result_large_err)]
    /// Phase 2: Register all protocols and structs (definitions only)
    pub fn register_protocols_and_structs(
        &mut self,
        program: &Program,
    ) -> Result<(), TypecheckError> {
        // Only collect protocol and struct definitions, skip functions
        for item in &program.items {
            self.register_item_protocols_and_structs(item)?;
        }

        Ok(())
    }

    /// Register protocols and structs from a single item (Phase 2)
    #[allow(clippy::result_large_err)]
    fn register_item_protocols_and_structs(&mut self, item: &Item) -> Result<(), TypecheckError> {
        use outrun_parser::ItemKind;

        match &item.kind {
            ItemKind::StructDefinition(struct_def) => {
                self.collect_struct_definition(struct_def)?;
            }
            ItemKind::ProtocolDefinition(protocol_def) => {
                self.collect_protocol_definition(protocol_def)?;
            }
            // Skip functions, impl blocks, and constants in this phase
            ItemKind::FunctionDefinition(_) => {
                // Functions will be registered in Phase 4
            }
            ItemKind::ImplBlock(_) => {
                // Impl blocks will be processed in Phase 3 and 4
            }
            ItemKind::ConstDefinition(_) => {
                // Constants can be registered here if needed
                // For now, skip
            }
            // Skip expression items and other items
            _ => {}
        }

        Ok(())
    }

    /// Extract protocol requirements from constraint expressions (e.g., "when Self: BinaryAddition && Self: BinarySubtraction")
    fn extract_protocol_requirements(
        &self,
        constraint_expr: &outrun_parser::ConstraintExpression,
    ) -> Result<HashSet<ModuleName>, TypecheckError> {
        let mut required_protocols = HashSet::new();
        self.collect_protocol_requirements_recursive(constraint_expr, &mut required_protocols)?;
        Ok(required_protocols)
    }

    /// Recursively collect protocol requirements from constraint expressions
    #[allow(clippy::only_used_in_recursion)]
    fn collect_protocol_requirements_recursive(
        &self,
        constraint_expr: &outrun_parser::ConstraintExpression,
        required_protocols: &mut HashSet<ModuleName>,
    ) -> Result<(), TypecheckError> {
        use outrun_parser::ConstraintExpression;

        match constraint_expr {
            ConstraintExpression::And { left, right, .. } => {
                // Handle "Self: A && Self: B" patterns
                self.collect_protocol_requirements_recursive(left, required_protocols)?;
                self.collect_protocol_requirements_recursive(right, required_protocols)?;
            }
            ConstraintExpression::Constraint {
                type_param,
                protocol_bound,
                ..
            } => {
                // Handle "Self: BinarySubtraction" patterns
                if type_param.name == "Self" {
                    // Convert protocol_bound (Vec<TypeIdentifier>) to protocol name
                    let protocol_name = protocol_bound
                        .iter()
                        .map(|segment| segment.name.as_str())
                        .collect::<Vec<_>>()
                        .join(".");
                    required_protocols.insert(ModuleName::new(&protocol_name));
                }
                // Note: We only handle Self constraints for protocol requirements
                // Other type parameter constraints would be handled differently
            }
            ConstraintExpression::Parenthesized { expression, .. } => {
                // Handle parenthesized expressions by recursing
                self.collect_protocol_requirements_recursive(expression, required_protocols)?;
            }
        }

        Ok(())
    }

    /// Phase 4: Register all functions (standalone + impl block functions)
    pub fn register_functions(&mut self, program: &Program) -> Result<(), TypecheckError> {
        // Register both standalone functions and impl block functions
        for item in &program.items {
            self.register_item_functions(item)?;
        }

        Ok(())
    }

    /// Register functions from a single item (Phase 4)
    #[allow(clippy::result_large_err)]
    fn register_item_functions(&mut self, item: &Item) -> Result<(), TypecheckError> {
        use outrun_parser::ItemKind;

        match &item.kind {
            ItemKind::FunctionDefinition(func_def) => {
                self.collect_function_definition(func_def)?;
            }
            ItemKind::ImplBlock(impl_block) => {
                // Register impl block functions
                self.collect_impl_block_functions(impl_block)?;
            }
            ItemKind::ConstDefinition(const_def) => {
                self.collect_const_definition(const_def)?;
            }
            // Process struct and protocol functions (metadata was registered in Phase 2)
            ItemKind::StructDefinition(struct_def) => {
                // Register struct functions
                let struct_name = struct_def
                    .name
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                for function in &struct_def.functions {
                    self.collect_struct_function(&struct_name, function)?;
                }
            }
            ItemKind::ProtocolDefinition(protocol_def) => {
                // Register protocol function signatures (metadata was registered in Phase 2)
                let protocol_name = protocol_def
                    .name
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                for protocol_function in &protocol_def.functions {
                    self.register_protocol_function_signature(
                        &protocol_name,
                        protocol_function,
                        protocol_def,
                    )?;
                }
            }
            // Skip expression items and other items
            _ => {}
        }

        Ok(())
    }

    /// Phase 5: Validate implementation completeness  
    #[allow(clippy::result_large_err)]
    pub fn validate_implementation_completeness(&mut self) -> Result<(), TypecheckError> {
        // TODO: Check that all protocol implementations are complete
        // For now, this is a placeholder that always succeeds
        Ok(())
    }

    /// Phase 2.5: Register automatic implementations (Any, Inspect) for all struct types
    #[allow(clippy::result_large_err)]
    pub fn register_automatic_implementations(
        &mut self,
        program: &Program,
    ) -> Result<(), TypecheckError> {
        // Register automatic Any and Inspect implementations for all struct types
        for item in &program.items {
            if let outrun_parser::ItemKind::StructDefinition(struct_def) = &item.kind {
                self.register_automatic_implementations_for_struct(struct_def)?;
            }
        }
        Ok(())
    }

    /// Phase 6: Type check all function bodies with complete context
    #[allow(clippy::result_large_err)]
    pub fn typecheck_function_bodies(
        &mut self,
        program: &mut Program,
    ) -> Result<(), TypecheckError> {
        // This is similar to typecheck_program_items_only but focuses on function bodies
        // All protocols, structs, implementations, and function signatures are already registered
        self.typecheck_program_items_only(program)
    }

    /// Collect a struct definition
    #[allow(clippy::result_large_err)]
    fn collect_struct_definition(
        &mut self,
        struct_def: &StructDefinition,
    ) -> Result<(), TypecheckError> {
        // Extract struct name (handle nested names like Module.SubModule.StructName)
        let struct_name = struct_def
            .name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        // Mark struct's module as local (for orphan rule)
        let struct_module = ModuleName::new(&struct_name);

        // Add to unified type registry as local module
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry.add_local_module(struct_module.clone());
        } else {
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry.add_local_module(struct_module.clone());
            self.type_registry = Rc::new(new_type_registry);
        }

        // Set up generic parameter context
        let old_generic_context = self.generic_parameter_context.clone();
        let generic_params = self.extract_struct_generic_parameters(struct_def);
        let generic_context = self.create_generic_context_from_names(&generic_params);
        self.set_generic_parameter_context(generic_context);

        // Store struct definition in legacy registry for field access resolution (temporary)
        self.struct_registry
            .insert(struct_name.clone(), struct_def.clone());

        // Create concrete type definition for unified registry
        let concrete_type_definition = crate::types::ConcreteTypeDefinition {
            type_name: struct_module.clone(),
            defining_module: struct_module.clone(), // Struct defines itself
            is_generic: !generic_params.is_empty(),
            span: Some(struct_def.span),
            never_info: None, // TODO: Extract from attributes when macro system is ready
        };

        // Create struct module for unified registry
        let struct_type_module = crate::types::TypeModule::Struct {
            name: struct_module.clone(),
            definition: concrete_type_definition,
            source_location: struct_def.span,
            generic_arity: generic_params.len(),
        };

        // Register struct in unified type registry (skip if already exists for idempotent registration)
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            // Check if module already exists to avoid redefinition errors
            if type_registry.get_module(&struct_name).is_none() {
                type_registry
                    .register_module(struct_type_module)
                    .map_err(TypecheckError::TypeError)?;
            }
        } else {
            let mut new_type_registry = (*self.type_registry).clone();
            // Check if module already exists to avoid redefinition errors
            if new_type_registry.get_module(&struct_name).is_none() {
                new_type_registry
                    .register_module(struct_type_module)
                    .map_err(TypecheckError::TypeError)?;
            }
            self.type_registry = Rc::new(new_type_registry);
        }

        // Collect struct associated functions (functions defined in the struct block)
        for function in &struct_def.functions {
            self.collect_struct_function(&struct_name, function)?;
        }

        // Restore previous generic context
        self.set_generic_parameter_context(old_generic_context);

        Ok(())
    }

    /// Collect a struct function (function defined within a struct block)
    #[allow(clippy::result_large_err)]
    fn collect_struct_function(
        &mut self,
        struct_name: &str,
        function: &FunctionDefinition,
    ) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = function.name.name.clone();

        // Struct functions are registered as static functions with qualified names
        // e.g., User.new() becomes function "new" in scope "User"
        let _qualified_function_name = format!("{}.{}", struct_name, function_name);

        // Convert visibility
        let visibility = match function.visibility {
            outrun_parser::FunctionVisibility::Public => DispatchVisibility::Public,
            outrun_parser::FunctionVisibility::Private => DispatchVisibility::Private,
        };

        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = function
            .parameters
            .iter()
            .map(|param| {
                let param_name = param.name.name.clone();
                let param_type = self.convert_type_annotation(&param.type_annotation)?;
                Ok((param_name, param_type))
            })
            .collect();
        let parameters = parameters?;

        // Convert return type
        let return_type = self.convert_type_annotation(&function.return_type)?;

        // Determine if this is a generic function by checking if struct has generic parameters
        let struct_generic_params = self.extract_struct_generic_parameters_by_name(struct_name)?;
        let is_generic = !struct_generic_params.is_empty();

        // Create function info with qualified name
        let function_info = FunctionInfo {
            defining_scope: struct_name.to_string(), // Function belongs to struct scope
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            body: Some(function.body.clone()), // Store function body for evaluation
            span: Some(function.span),
            generic_parameters: struct_generic_params.clone(),
            is_generic,
        };

        // Register the function in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                struct_name.to_string(),
                function_name.clone(),
                function_info,
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                struct_name.to_string(),
                function_name.clone(),
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }

        // Generate public function template for dependency usage (if public)
        if matches!(
            function.visibility,
            outrun_parser::FunctionVisibility::Public
        ) {
            let function_signature = crate::universal_dispatch::FunctionSignature::qualified(
                struct_name.to_string(),
                function_name,
            );

            let template_visibility = crate::constraints::FunctionVisibility::Public;
            let available_generics = struct_generic_params.clone();

            if let Some(constraint_solver) = &mut self.constraint_solver_with_backtracking {
                if let Err(e) = constraint_solver.generate_public_function_template(
                    function_signature,
                    function,
                    template_visibility,
                    &available_generics,
                ) {
                    // TODO: Move to generic compiler warning system.
                    eprintln!(
                        "Warning: Failed to generate template for {}.{}: {}",
                        struct_name, function.name.name, e
                    );
                }
            }
        }

        Ok(())
    }

    /// Collect a protocol definition
    #[allow(clippy::result_large_err)]
    fn collect_protocol_definition(
        &mut self,
        protocol_def: &ProtocolDefinition,
    ) -> Result<(), TypecheckError> {
        // Extract protocol name (handle nested names like Module.SubModule.ProtocolName)
        let protocol_name = protocol_def
            .name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        // Mark protocol's module as local (for orphan rule)
        let protocol_module = ModuleName::new(&protocol_name);

        // Add to unified type registry as local module
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry.add_local_module(protocol_module.clone());
        } else {
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry.add_local_module(protocol_module.clone());
            self.type_registry = Rc::new(new_type_registry);
        }

        // Set up generic parameter context
        let old_generic_context = self.generic_parameter_context.clone();
        let generic_params = self.extract_protocol_generic_parameters(protocol_def);
        let generic_context = self.create_generic_context_from_names(&generic_params);
        self.set_generic_parameter_context(generic_context);

        // Analyze protocol functions to determine requirements and defaults
        let mut required_functions = HashSet::new();
        let mut default_implementations = HashSet::new();

        for protocol_function in &protocol_def.functions {
            match protocol_function {
                outrun_parser::ProtocolFunction::Signature(signature) => {
                    // Function signature only - must be implemented by types
                    required_functions.insert(signature.name.name.clone());
                }
                outrun_parser::ProtocolFunction::Definition(definition) => {
                    // Function with body - has default implementation
                    required_functions.insert(definition.name.name.clone());
                    default_implementations.insert(definition.name.name.clone());
                }
                outrun_parser::ProtocolFunction::StaticDefinition(_) => {
                    // Static functions don't create implementation requirements
                }
            }
        }

        // Extract required protocols from constraint expressions
        let required_protocols = if let Some(constraints) = &protocol_def.constraints {
            self.extract_protocol_requirements(constraints)?
        } else {
            HashSet::new()
        };

        // Create protocol definition for unified registry
        let protocol_definition = crate::types::ProtocolDefinition {
            protocol_name: protocol_module.clone(),
            required_protocols,
            defining_module: protocol_module.clone(), // Protocol defines itself
            default_implementations,
            required_functions,
            span: Some(protocol_def.span),
        };

        // Create protocol module for unified registry
        let protocol_type_module = crate::types::TypeModule::Protocol {
            name: protocol_module.clone(),
            definition: protocol_definition,
            source_location: protocol_def.span,
            generic_arity: generic_params.len(),
        };

        // Register protocol in unified type registry
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry
                .register_module(protocol_type_module)
                .map_err(TypecheckError::TypeError)?;
        } else {
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry
                .register_module(protocol_type_module)
                .map_err(TypecheckError::TypeError)?;
            self.type_registry = Rc::new(new_type_registry);
        }

        // Phase 2: Skip function processing - functions will be registered in Phase 4
        // Only register the protocol definition itself during this phase

        // Restore previous generic context
        self.set_generic_parameter_context(old_generic_context);

        Ok(())
    }

    /// Register a protocol function signature during Phase 4
    #[allow(clippy::result_large_err)]
    fn register_protocol_function_signature(
        &mut self,
        protocol_name: &str,
        protocol_function: &ProtocolFunction,
        protocol_def: &ProtocolDefinition,
    ) -> Result<(), TypecheckError> {
        use outrun_parser::ProtocolFunction;

        match protocol_function {
            ProtocolFunction::Signature(signature) => {
                self.register_protocol_signature(protocol_name, signature, protocol_def)?;
            }
            ProtocolFunction::Definition(definition) => {
                // For protocol function definitions, register the signature part
                self.register_protocol_function_definition(
                    protocol_name,
                    definition,
                    protocol_def,
                )?;
            }
            ProtocolFunction::StaticDefinition(static_def) => {
                // For static definitions, register the signature part
                self.register_protocol_static_definition(protocol_name, static_def, protocol_def)?;
            }
        }

        Ok(())
    }

    /// Register a protocol function signature in the function registry
    #[allow(clippy::result_large_err)]
    fn register_protocol_signature(
        &mut self,
        protocol_name: &str,
        signature: &FunctionSignature,
        protocol_def: &ProtocolDefinition,
    ) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = signature.name.name.clone();

        // Convert visibility
        let visibility = match signature.visibility {
            outrun_parser::FunctionVisibility::Public => DispatchVisibility::Public,
            outrun_parser::FunctionVisibility::Private => DispatchVisibility::Private,
        };

        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = signature
            .parameters
            .iter()
            .map(|param| {
                let param_name = param.name.name.clone();
                let param_type = self.convert_type_annotation(&param.type_annotation)?;
                Ok((param_name, param_type))
            })
            .collect();
        let parameters = parameters?;

        // Convert return type
        let return_type = self.convert_type_annotation(&signature.return_type)?;

        // Extract protocol generic parameters
        let protocol_generic_params = self.extract_protocol_generic_parameters(protocol_def);
        let is_generic = !protocol_generic_params.is_empty();

        // Create function info for protocol function
        let function_info = FunctionInfo {
            defining_scope: protocol_name.to_string(),
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            body: None, // Protocol function signatures don't have bodies
            span: Some(signature.span),
            generic_parameters: protocol_generic_params,
            is_generic,
        };

        // Register the protocol function signature in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(protocol_name.to_string(), function_name, function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(protocol_name.to_string(), function_name, function_info);
            self.function_registry = Rc::new(new_registry);
        }

        Ok(())
    }

    /// Register a protocol function definition (extract signature part)
    #[allow(clippy::result_large_err)]
    fn register_protocol_function_definition(
        &mut self,
        protocol_name: &str,
        definition: &FunctionDefinition,
        protocol_def: &ProtocolDefinition,
    ) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = definition.name.name.clone();

        // Convert visibility
        let visibility = match definition.visibility {
            outrun_parser::FunctionVisibility::Public => DispatchVisibility::Public,
            outrun_parser::FunctionVisibility::Private => DispatchVisibility::Private,
        };

        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = definition
            .parameters
            .iter()
            .map(|param| {
                let param_name = param.name.name.clone();
                let param_type = self.convert_type_annotation(&param.type_annotation)?;
                Ok((param_name, param_type))
            })
            .collect();
        let parameters = parameters?;

        // Convert return type
        let return_type = self.convert_type_annotation(&definition.return_type)?;

        // Extract protocol generic parameters
        let protocol_generic_params = self.extract_protocol_generic_parameters(protocol_def);
        let is_generic = !protocol_generic_params.is_empty();

        // Create function info for protocol function
        let function_info = FunctionInfo {
            defining_scope: protocol_name.to_string(),
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            body: Some(definition.body.clone()), // Store function body for evaluation
            span: Some(definition.span),
            generic_parameters: protocol_generic_params,
            is_generic,
        };

        // Register the protocol function in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(protocol_name.to_string(), function_name, function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(protocol_name.to_string(), function_name, function_info);
            self.function_registry = Rc::new(new_registry);
        }

        Ok(())
    }

    /// Register a protocol static function definition (extract signature part)
    #[allow(clippy::result_large_err)]
    fn register_protocol_static_definition(
        &mut self,
        protocol_name: &str,
        static_def: &StaticFunctionDefinition,
        protocol_def: &ProtocolDefinition,
    ) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = static_def.name.name.clone();

        // Static functions are always public in protocols
        let visibility = DispatchVisibility::Public;

        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = static_def
            .parameters
            .iter()
            .map(|param| {
                let param_name = param.name.name.clone();
                let param_type = self.convert_type_annotation(&param.type_annotation)?;
                Ok((param_name, param_type))
            })
            .collect();
        let parameters = parameters?;

        // Convert return type
        let return_type = self.convert_type_annotation(&static_def.return_type)?;

        // Extract protocol generic parameters
        let protocol_generic_params = self.extract_protocol_generic_parameters(protocol_def);
        let is_generic = !protocol_generic_params.is_empty();

        // Create function info for static function
        let function_info = FunctionInfo {
            defining_scope: protocol_name.to_string(),
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            body: Some(static_def.body.clone()), // Store function body for evaluation
            span: Some(static_def.span),
            generic_parameters: protocol_generic_params,
            is_generic,
        };

        // Register the static function in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(protocol_name.to_string(), function_name, function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(protocol_name.to_string(), function_name, function_info);
            self.function_registry = Rc::new(new_registry);
        }

        Ok(())
    }

    /// Collect impl block functions
    #[allow(clippy::result_large_err)]
    fn collect_impl_block_functions(
        &mut self,
        impl_block: &ImplBlock,
    ) -> Result<(), TypecheckError> {
        // Extract impl block scope information
        // For impl blocks, the scope is typically "ProtocolName for TypeName"
        // but for function registration, we need a unique scope identifier

        // Extract protocol name
        let protocol_name = self.extract_type_spec_name(&impl_block.protocol_spec);

        // Extract type name
        let type_name = self.extract_type_spec_name(&impl_block.type_spec);

        // Create a qualified scope name for impl block functions
        // Format: "impl ProtocolName for TypeName" (matches dispatch table lookup)
        let impl_scope = format!("impl {} for {}", protocol_name, type_name);

        // Set up generic parameter context from impl block declaration
        let old_generic_context = self.generic_parameter_context.clone();

        // Extract generic parameters from the impl block using our semantic approach
        // Type variables are extracted directly from the impl Protocol<T> for Type<U> syntax
        let impl_generic_params = self.extract_impl_block_generic_parameters(impl_block);

        // Validate that all constrained type variables appear in the impl type specifications
        self.validate_impl_constraint_variables(impl_block, &impl_generic_params)?;

        let generic_context = self.create_generic_context_from_names(&impl_generic_params);

        // Extract type variables for implementing_args
        let implementing_args: Vec<Type> = impl_generic_params
            .iter()
            .map(|name| generic_context.get(name).unwrap().clone())
            .collect();

        self.set_generic_parameter_context(generic_context);

        // Set Self binding context for impl block
        let old_self_context = self.current_self_context.clone();
        let new_context = SelfBindingContext::Implementation {
            implementing_type: crate::types::ModuleName::new(&type_name),
            implementing_args,
            protocol_name: ModuleName::new(&protocol_name),
            protocol_args: vec![], // TODO: Extract from protocol spec if needed
        };
        self.current_self_context = new_context;

        // Collect impl block functions
        for function in &impl_block.functions {
            self.collect_impl_block_function(&impl_scope, function, impl_block)?;
        }

        // Restore previous contexts
        self.current_self_context = old_self_context;
        self.set_generic_parameter_context(old_generic_context);

        Ok(())
    }

    /// Collect a single impl block function
    #[allow(clippy::result_large_err)]
    fn collect_impl_block_function(
        &mut self,
        impl_scope: &str,
        function: &FunctionDefinition,
        impl_block: &ImplBlock,
    ) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = function.name.name.clone();

        // Convert visibility - impl functions can be public or private
        let visibility = match function.visibility {
            outrun_parser::FunctionVisibility::Public => DispatchVisibility::Public,
            outrun_parser::FunctionVisibility::Private => DispatchVisibility::Private,
        };

        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = function
            .parameters
            .iter()
            .map(|param| {
                let param_name = param.name.name.clone();
                let param_type = self.convert_type_annotation(&param.type_annotation)?;
                Ok((param_name, param_type))
            })
            .collect();
        let parameters = parameters?;

        // Convert return type
        let return_type = self.convert_type_annotation(&function.return_type)?;

        // Extract generic parameters from impl block context
        let impl_generic_params = self.extract_impl_block_generic_parameters(impl_block);

        // Validate that all constrained type variables appear in the impl type specifications
        self.validate_impl_constraint_variables(impl_block, &impl_generic_params)?;

        let is_generic = !impl_generic_params.is_empty();

        // Create function info for impl block function
        let function_info = FunctionInfo {
            defining_scope: impl_scope.to_string(), // Function belongs to impl scope
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            body: Some(function.body.clone()), // Store function body for evaluation
            span: Some(function.span),
            generic_parameters: impl_generic_params,
            is_generic,
        };

        // Register the impl function in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                impl_scope.to_string(),
                function_name.clone(),
                function_info,
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                impl_scope.to_string(),
                function_name.clone(),
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }

        // Generate public function template for dependency usage (if public)
        if matches!(
            function.visibility,
            outrun_parser::FunctionVisibility::Public
        ) {
            let function_signature = crate::universal_dispatch::FunctionSignature::qualified(
                impl_scope.to_string(),
                function_name,
            );

            let template_visibility = crate::constraints::FunctionVisibility::Public;
            let available_generics = self.extract_impl_block_generic_parameters(impl_block);

            if let Some(constraint_solver) = &mut self.constraint_solver_with_backtracking {
                if let Err(e) = constraint_solver.generate_public_function_template(
                    function_signature,
                    function,
                    template_visibility,
                    &available_generics,
                ) {
                    // TODO: Replace with compiler generic warning system.
                    eprintln!(
                        "Warning: Failed to generate template for {}.{}: {}",
                        impl_scope, function.name.name, e
                    );
                }
            }
        }

        Ok(())
    }

    /// Extract a simple name from a TypeSpec for scope identification
    fn extract_type_spec_name(&self, type_spec: &TypeSpec) -> String {
        // Extract the path from the TypeSpec
        type_spec
            .path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Extract generic parameter names from struct definition
    fn extract_struct_generic_parameters(&self, struct_def: &StructDefinition) -> Vec<String> {
        if let Some(generic_params) = &struct_def.generic_params {
            generic_params
                .params
                .iter()
                .map(|param| param.name.name.clone())
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Extract generic parameter names from struct by name (for function registration)
    fn extract_struct_generic_parameters_by_name(
        &self,
        struct_name: &str,
    ) -> Result<Vec<String>, TypecheckError> {
        if let Some(struct_def) = self.struct_registry.get(struct_name) {
            Ok(self.extract_struct_generic_parameters(struct_def))
        } else {
            // No struct found - assume non-generic
            Ok(Vec::new())
        }
    }

    /// Extract generic parameter names from protocol definition
    fn extract_protocol_generic_parameters(
        &self,
        protocol_def: &ProtocolDefinition,
    ) -> Vec<String> {
        if let Some(generic_params) = &protocol_def.generic_params {
            generic_params
                .params
                .iter()
                .map(|param| param.name.name.clone())
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Extract generic parameter names from impl block context
    /// Uses our semantic design: type variable names in impl syntax declare generic parameters
    /// Same name = same type constraint, different names = independent type variables
    /// Example: impl Display<T> for Wrapper<T> -> T appears twice, creates constraint
    /// Example: impl Display<A> for Wrapper<B> -> A and B are independent
    fn extract_impl_block_generic_parameters(&self, impl_block: &ImplBlock) -> Vec<String> {
        let mut generic_params = Vec::new();

        // Extract type variables from protocol specification: impl Protocol<T, U> for ...
        if let Some(protocol_generic_args) = &impl_block.protocol_spec.generic_args {
            for type_arg in &protocol_generic_args.args {
                let type_vars = self.extract_type_variables_from_annotation(type_arg);
                for var in type_vars {
                    if !generic_params.contains(&var) {
                        generic_params.push(var);
                    }
                }
            }
        }

        // Extract type variables from implementing type specification: impl ... for Type<T, U>
        if let Some(type_generic_args) = &impl_block.type_spec.generic_args {
            for type_arg in &type_generic_args.args {
                let type_vars = self.extract_type_variables_from_annotation(type_arg);
                for var in type_vars {
                    if !generic_params.contains(&var) {
                        generic_params.push(var);
                    }
                }
            }
        }
        generic_params
    }

    /// Extract type variable names from a type annotation
    /// This identifies type variables vs concrete types based on naming conventions
    fn extract_type_variables_from_annotation(
        &self,
        annotation: &outrun_parser::TypeAnnotation,
    ) -> Vec<String> {
        use outrun_parser::TypeAnnotation;
        let mut type_vars = Vec::new();

        match annotation {
            TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                // Check if this is a type variable (single identifier, looks like a type parameter)
                if path.len() == 1 {
                    let name = &path[0].name;
                    if self.is_type_variable_name(name) {
                        type_vars.push(name.clone());
                    }
                }

                // Recursively extract from generic arguments
                if let Some(args) = generic_args {
                    for arg in &args.args {
                        let nested_vars = self.extract_type_variables_from_annotation(arg);
                        for var in nested_vars {
                            if !type_vars.contains(&var) {
                                type_vars.push(var);
                            }
                        }
                    }
                }
            }
            TypeAnnotation::Tuple { types, .. } => {
                for type_ann in types {
                    let nested_vars = self.extract_type_variables_from_annotation(type_ann);
                    for var in nested_vars {
                        if !type_vars.contains(&var) {
                            type_vars.push(var);
                        }
                    }
                }
            }
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                for param in params {
                    let nested_vars =
                        self.extract_type_variables_from_annotation(&param.type_annotation);
                    for var in nested_vars {
                        if !type_vars.contains(&var) {
                            type_vars.push(var);
                        }
                    }
                }
                let return_vars = self.extract_type_variables_from_annotation(return_type);
                for var in return_vars {
                    if !type_vars.contains(&var) {
                        type_vars.push(var);
                    }
                }
            }
        }

        type_vars
    }

    /// Extract type variable names from constraint expressions
    /// Handles: T: Display, U: Debug, T: Display && U: Debug, (T: Display)
    fn extract_type_variables_from_constraint_expression(
        &self,
        constraint: &outrun_parser::ConstraintExpression,
    ) -> Vec<String> {
        use outrun_parser::ConstraintExpression;
        let mut type_vars = Vec::new();

        match constraint {
            ConstraintExpression::And { left, right, .. } => {
                // Handle: T: Display && U: Debug
                let left_vars = self.extract_type_variables_from_constraint_expression(left);
                let right_vars = self.extract_type_variables_from_constraint_expression(right);

                for var in left_vars {
                    if !type_vars.contains(&var) {
                        type_vars.push(var);
                    }
                }
                for var in right_vars {
                    if !type_vars.contains(&var) {
                        type_vars.push(var);
                    }
                }
            }
            ConstraintExpression::Constraint { type_param, .. } => {
                // Handle: T: Display and Self: Display
                let param_name = &type_param.name;
                if (param_name == "Self" || self.is_type_variable_name(param_name))
                    && !type_vars.contains(param_name)
                {
                    type_vars.push(param_name.clone());
                }
            }
            ConstraintExpression::Parenthesized { expression, .. } => {
                // Handle: (T: Display)
                let nested_vars =
                    self.extract_type_variables_from_constraint_expression(expression);
                for var in nested_vars {
                    if !type_vars.contains(&var) {
                        type_vars.push(var);
                    }
                }
            }
        }

        type_vars
    }

    /// Validate that all constrained type variables appear in the impl type specifications
    /// Also supports Self constraints in addition to regular type variable constraints
    #[allow(clippy::result_large_err)]
    fn validate_impl_constraint_variables(
        &self,
        impl_block: &ImplBlock,
        available_type_vars: &[String],
    ) -> Result<(), TypecheckError> {
        if let Some(constraints) = &impl_block.constraints {
            let constraint_vars =
                self.extract_type_variables_from_constraint_expression(constraints);

            // Check each constrained variable exists in the available type variables or is Self
            for constrained_var in constraint_vars {
                if constrained_var == "Self" {
                    // Self is always valid in impl block constraints
                    continue;
                }

                if !available_type_vars.contains(&constrained_var) {
                    // Find which type variables are available for helpful error message
                    let available_list = if available_type_vars.is_empty() {
                        "none (no type variables in impl specifications)".to_string()
                    } else {
                        available_type_vars.join(", ")
                    };

                    return Err(TypecheckError::InferenceError(
                        crate::error::InferenceError::InvalidConstraintVariable {
                            span: crate::error::to_source_span(Some(*constraints.span())),
                            variable_name: constrained_var,
                            available_variables: available_list.clone(),
                            suggestions: vec![
                                "Constrained type variables must appear in the impl type specifications".to_string(),
                                format!("Available type variables: {}", available_list),
                                "You can also constrain 'Self' in protocol and impl contexts".to_string(),
                            ],
                        },
                    ));
                }
            }
        }

        Ok(())
    }

    /// Check if a name looks like a type variable vs a concrete type
    /// Type variables: T, U, A, B, TKey, TValue, etc.
    /// Concrete types: String, Integer64, List, Option, etc.
    fn is_type_variable_name(&self, name: &str) -> bool {
        // Single uppercase letters (T, U, K, V, etc.)
        if name.len() == 1 && name.chars().next().unwrap().is_uppercase() {
            return true;
        }

        // T-prefixed names (T1, TKey, TValue, etc.)
        if name.starts_with('T') && name.len() > 1 && name.chars().nth(1).unwrap().is_uppercase() {
            return true;
        }

        // Common type parameter conventions
        matches!(
            name,
            "Key" | "Value" | "Input" | "Output" | "Result" | "Error" | "Element"
        )
    }

    /// Collect a function definition
    #[allow(clippy::result_large_err)]
    fn collect_function_definition(
        &mut self,
        func_def: &FunctionDefinition,
    ) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = func_def.name.name.clone();

        // Convert visibility
        let visibility = match func_def.visibility {
            outrun_parser::FunctionVisibility::Public => DispatchVisibility::Public,
            outrun_parser::FunctionVisibility::Private => DispatchVisibility::Private,
        };

        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = func_def
            .parameters
            .iter()
            .map(|param| {
                let param_name = param.name.name.clone();
                let param_type = self.convert_type_annotation(&param.type_annotation)?;
                Ok((param_name, param_type))
            })
            .collect();
        let parameters = parameters?;

        // Convert return type
        let return_type = self.convert_type_annotation(&func_def.return_type)?;

        // Create function info
        let function_info = FunctionInfo {
            defining_scope: self.current_module.as_str().to_string(),
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            body: Some(func_def.body.clone()), // Store function body for evaluation
            span: Some(func_def.span),
            generic_parameters: Vec::new(), // TODO: Handle top-level generic functions
            is_generic: false,
        };

        // Get mutable reference to function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                self.current_module.as_str().to_string(),
                function_name.clone(),
                function_info,
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                self.current_module.as_str().to_string(),
                function_name.clone(),
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }

        // Generate public function template for dependency usage (if public)
        if matches!(
            func_def.visibility,
            outrun_parser::FunctionVisibility::Public
        ) {
            let function_signature = crate::universal_dispatch::FunctionSignature::qualified(
                self.current_module.as_str().to_string(),
                function_name,
            );

            let template_visibility = crate::constraints::FunctionVisibility::Public;
            let available_generics = Vec::new(); // TODO: Handle top-level generic functions

            if let Some(constraint_solver) = &mut self.constraint_solver_with_backtracking {
                if let Err(e) = constraint_solver.generate_public_function_template(
                    function_signature,
                    func_def,
                    template_visibility,
                    &available_generics,
                ) {
                    // TODO: Replace with generic compiler warning system.
                    eprintln!(
                        "Warning: Failed to generate template for {}.{}: {}",
                        self.current_module.as_str(),
                        func_def.name.name,
                        e
                    );
                }
            }
        }

        Ok(())
    }

    /// Collect a constant definition
    #[allow(clippy::result_large_err)]
    fn collect_const_definition(
        &mut self,
        _const_def: &ConstDefinition,
    ) -> Result<(), TypecheckError> {
        // TODO: Extract constant name and type
        // TODO: Add to symbol table

        // For now, just record that we processed it
        Ok(())
    }

    /// Register all protocol implementations in the registry
    #[allow(clippy::result_large_err)]
    pub fn register_implementations(&mut self, program: &Program) -> Result<(), TypecheckError> {
        // Process all implementation items and register them with orphan rule checking
        for item in &program.items {
            self.register_item_implementations(item)?;
        }

        // Phase 3.5: Analyze recursive type patterns after all implementations are registered
        self.analyze_recursive_patterns_and_emit_warnings()?;

        // Phase 3.75: Initialize call stack backtracking for enhanced context resolution
        self.initialize_call_stack_backtracking();

        Ok(())
    }

    /// Register implementations from a single item
    #[allow(clippy::result_large_err)]
    fn register_item_implementations(&mut self, item: &Item) -> Result<(), TypecheckError> {
        use outrun_parser::ItemKind;

        match &item.kind {
            ItemKind::ImplBlock(impl_block) => {
                self.register_impl_block(impl_block)?;
            }
            // Skip struct definitions in Phase 3 - automatic implementations moved to Phase 2
            _ => {
                // Other items don't affect the implementation registry in Phase 3
            }
        }

        Ok(())
    }

    /// Automatically register Any and Inspect implementations for a struct type
    #[allow(clippy::result_large_err)]
    fn register_automatic_implementations_for_struct(
        &mut self,
        struct_def: &StructDefinition,
    ) -> Result<(), TypecheckError> {
        // Extract struct name (handle nested names like Module.SubModule.StructName)
        let struct_name = struct_def
            .name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        let implementing_type = ModuleName::new(&struct_name);
        let defining_module = ModuleName::new(&struct_name);

        // Register Any implementation
        let any_protocol = ModuleName::new("Any");
        self.register_automatic_implementation(
            &implementing_type,
            &any_protocol,
            &defining_module,
            struct_def.span,
        )?;

        // Register Inspect implementation
        let inspect_protocol = ModuleName::new("Inspect");
        self.register_automatic_implementation(
            &implementing_type,
            &inspect_protocol,
            &defining_module,
            struct_def.span,
        )?;

        // Register Equality implementation
        let equality_protocol = ModuleName::new("Equality");
        self.register_automatic_implementation(
            &implementing_type,
            &equality_protocol,
            &defining_module,
            struct_def.span,
        )?;

        Ok(())
    }

    /// Register a single automatic implementation (Any, Inspect, etc.)
    fn register_automatic_implementation(
        &mut self,
        implementing_type: &ModuleName,
        protocol: &ModuleName,
        defining_module: &ModuleName,
        span: outrun_parser::Span,
    ) -> Result<(), TypecheckError> {
        // Check if implementation already exists to avoid conflicts
        if self
            .type_registry
            .has_implementation(protocol, implementing_type)
        {
            // Implementation already exists, skip registration
            return Ok(());
        }

        // Create implementation module name using colon syntax
        let impl_module_name =
            ModuleName::implementation(implementing_type.as_str(), protocol.as_str());

        // Add to unified type registry as local module
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry.add_local_module(impl_module_name.clone());
        } else {
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry.add_local_module(impl_module_name.clone());
            self.type_registry = Rc::new(new_type_registry);
        }

        // Create implementation module (automatic implementations have no explicit functions)
        let impl_type_module = crate::types::TypeModule::Implementation {
            name: impl_module_name.clone(),
            implementing_type: implementing_type.clone(),
            protocol: protocol.clone(),
            generic_bindings: Vec::new(), // No generic parameters for automatic implementations
            functions: Vec::new(),        // Automatic implementations have no explicit functions
            source_location: span,
            defining_module: defining_module.clone(),
        };

        // Register implementation module in unified type registry
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry
                .register_module(impl_type_module)
                .map_err(TypecheckError::TypeError)?;
        } else {
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry
                .register_module(impl_type_module)
                .map_err(TypecheckError::TypeError)?;
            self.type_registry = Rc::new(new_type_registry);
        }

        // Note: No need for legacy register_implementation() call since register_module()
        // already handles implementation registration in the unified system

        Ok(())
    }

    /// Register an implementation block
    #[allow(clippy::result_large_err)]
    fn register_impl_block(&mut self, impl_block: &ImplBlock) -> Result<(), TypecheckError> {
        // Extract protocol and type identifiers from TypeSpec
        let protocol_name = impl_block
            .protocol_spec
            .path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        let protocol_module = ModuleName::new(&protocol_name);

        let implementing_type_name = impl_block
            .type_spec
            .path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        let implementing_type_module = ModuleName::new(&implementing_type_name);

        // Create implementation module name using colon syntax: "List:Display"
        let impl_module_name = ModuleName::implementation(&implementing_type_name, &protocol_name);

        // Create defining module (where this impl block is defined)
        let defining_module = implementing_type_module.clone(); // For now, assume impl is defined with the type

        // Add to unified type registry as local module
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry.add_local_module(impl_module_name.clone());
        } else {
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry.add_local_module(impl_module_name.clone());
            self.type_registry = Rc::new(new_type_registry);
        }

        // Convert impl block functions to FunctionDefinition format
        let mut impl_functions = Vec::new();
        for function in &impl_block.functions {
            let func_def = crate::types::FunctionDefinition {
                name: function.name.name.clone(),
                parameters: function
                    .parameters
                    .iter()
                    .map(|param| {
                        let param_type = self
                            .convert_type_annotation(&param.type_annotation)
                            .unwrap_or_else(|_| Type::concrete("Unknown")); // Fallback for conversion errors
                        (param.name.name.clone(), param_type)
                    })
                    .collect(),
                return_type: self
                    .convert_type_annotation(&function.return_type)
                    .unwrap_or_else(|_| Type::concrete("Unknown")), // Fallback for conversion errors
                body: None,       // TODO: Convert Block to Expression when needed
                is_static: false, // impl block functions are not static
                span: Some(function.span),
            };
            impl_functions.push(func_def);
        }

        // Create implementation module for unified registry
        let impl_type_module = crate::types::TypeModule::Implementation {
            name: impl_module_name.clone(),
            implementing_type: implementing_type_module.clone(),
            protocol: protocol_module.clone(),
            generic_bindings: Vec::new(), // TODO: Handle generic parameters
            functions: impl_functions,
            source_location: impl_block.span,
            defining_module: defining_module.clone(),
        };

        // Register implementation module in unified type registry
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry
                .register_module(impl_type_module)
                .map_err(TypecheckError::TypeError)?;
        } else {
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry
                .register_module(impl_type_module)
                .map_err(TypecheckError::TypeError)?;
            self.type_registry = Rc::new(new_type_registry);
        }

        // Note: No need for legacy register_implementation() call since register_module()
        // already handles implementation registration in the unified system

        Ok(())
    }

    /// Type check a single item
    #[allow(clippy::result_large_err)]
    pub fn typecheck_item(&mut self, item: &mut Item) -> Result<(), TypecheckError> {
        use outrun_parser::ItemKind;

        match &mut item.kind {
            ItemKind::StructDefinition(struct_def) => self.typecheck_struct_functions(struct_def),
            ItemKind::ProtocolDefinition(protocol_def) => {
                self.typecheck_protocol_functions(protocol_def)
            }
            ItemKind::ImplBlock(impl_block) => self.typecheck_impl_block_functions(impl_block),
            ItemKind::FunctionDefinition(func_def) => self.typecheck_standalone_function(func_def),
            ItemKind::LetBinding(let_binding) => self.typecheck_let_binding(let_binding),
            ItemKind::Expression(expr) => {
                // Type check standalone expressions (needed for desugared expressions like UnaryMinus.minus)
                let mut context = InferenceContext {
                    substitution: Substitution::new(),
                    constraints: Vec::new(),
                    expected_type: None,
                    self_binding: self.current_self_context.clone(), // Inherit current Self context
                    bindings: HashMap::new(),
                };
                // Use iterative inference to avoid stack overflow and improve performance
                self.infer_expression(expr, &mut context)?;
                Ok(())
            }
            // Other items don't need body type checking
            ItemKind::ConstDefinition(_)
            | ItemKind::Comment(_)
            | ItemKind::ImportDefinition(_)
            | ItemKind::AliasDefinition(_)
            | ItemKind::MacroDefinition(_) => Ok(()),
            // All other item types (literals, keywords, etc.) don't need type checking
            _ => Ok(()),
        }
    }

    /// Type check struct functions
    #[allow(clippy::result_large_err)]
    fn typecheck_struct_functions(
        &mut self,
        struct_def: &StructDefinition,
    ) -> Result<(), TypecheckError> {
        let struct_name = struct_def
            .name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        for function in &struct_def.functions {
            self.typecheck_function_body(&struct_name, function)?;
        }

        Ok(())
    }

    /// Type check protocol functions
    #[allow(clippy::result_large_err)]
    fn typecheck_protocol_functions(
        &mut self,
        protocol_def: &ProtocolDefinition,
    ) -> Result<(), TypecheckError> {
        let protocol_name = protocol_def
            .name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        // Set Self binding context for protocol function type checking
        let old_self_context = self.current_self_context.clone();
        self.current_self_context = SelfBindingContext::ProtocolDefinition {
            protocol_name: ModuleName::new(&protocol_name),
            protocol_args: vec![],
        };

        for protocol_function in &protocol_def.functions {
            match protocol_function {
                ProtocolFunction::Definition(_definition) => {
                    // PROTOCOL FUNCTION MONOMORPHIZATION:
                    // Protocol function definitions are NOT typechecked at definition time
                    // because they contain Self types that can't be resolved yet.
                    // Instead, they are stored as templates and monomorphized + typechecked
                    // when called with concrete argument types.
                    //
                    // The monomorphization happens during protocol function calls in
                    // resolve_protocol_call when we have concrete Self types.
                }
                ProtocolFunction::StaticDefinition(static_def) => {
                    // Static functions can be typechecked normally since they don't use Self
                    self.typecheck_static_function_body(&protocol_name, static_def)?;
                }
                ProtocolFunction::Signature(_) => {
                    // Signatures don't have bodies to check
                }
            }
        }

        // Restore previous Self context
        self.current_self_context = old_self_context;

        Ok(())
    }

    /// Type check impl block functions
    #[allow(clippy::result_large_err)]
    fn typecheck_impl_block_functions(
        &mut self,
        impl_block: &ImplBlock,
    ) -> Result<(), TypecheckError> {
        let protocol_name = self.extract_type_spec_name(&impl_block.protocol_spec);
        let type_name = self.extract_type_spec_name(&impl_block.type_spec);
        let impl_scope = format!("{} for {}", protocol_name, type_name);

        // Set up generic parameter context from impl block declaration (same as in collect phase)
        let old_generic_context = self.generic_parameter_context.clone();

        // Extract generic parameters from the impl block using our semantic approach
        // Type variables are extracted directly from the impl Protocol<T> for Type<U> syntax
        let impl_generic_params = self.extract_impl_block_generic_parameters(impl_block);

        // Validate that all constrained type variables appear in the impl type specifications
        self.validate_impl_constraint_variables(impl_block, &impl_generic_params)?;

        let generic_context = self.create_generic_context_from_names(&impl_generic_params);

        // Extract type variables for implementing_args
        let implementing_args: Vec<Type> = impl_generic_params
            .iter()
            .map(|name| generic_context.get(name).unwrap().clone())
            .collect();

        self.set_generic_parameter_context(generic_context);

        // Set Self binding context for impl block type checking
        let old_self_context = self.current_self_context.clone();
        self.current_self_context = SelfBindingContext::Implementation {
            implementing_type: crate::types::ModuleName::new(&type_name),
            implementing_args,
            protocol_name: ModuleName::new(&protocol_name),
            protocol_args: vec![], // TODO: Extract from protocol spec if needed
        };

        for function in &impl_block.functions {
            self.typecheck_function_body(&impl_scope, function)?;
        }

        // Restore previous contexts
        self.current_self_context = old_self_context;
        self.set_generic_parameter_context(old_generic_context);

        Ok(())
    }

    /// Type check standalone function
    #[allow(clippy::result_large_err)]
    fn typecheck_standalone_function(
        &mut self,
        func_def: &FunctionDefinition,
    ) -> Result<(), TypecheckError> {
        let module_scope = self.current_module.as_str().to_string();
        self.typecheck_function_body(&module_scope, func_def)
    }

    /// Type check let binding
    #[allow(clippy::result_large_err)]
    fn typecheck_let_binding(
        &mut self,
        let_binding: &mut outrun_parser::LetBinding,
    ) -> Result<(), TypecheckError> {
        // FIXED: Implement let binding type checking using iterative inference system
        // This resolves the missing universal_clause_ids issue for desugared operators

        let mut context = InferenceContext {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: None,
            self_binding: self.current_self_context.clone(), // Inherit current Self context
            bindings: HashMap::new(),
        };

        // Use iterative inference to process the expression
        // This ensures function calls get their universal_clause_ids set properly
        self.infer_expression(&mut let_binding.expression, &mut context)?;

        Ok(())
    }

    // ============================================================================
    // NEVER TYPE SUPPORT (WORKAROUND)
    // ============================================================================

    /// WORKAROUND: Check if two types can be unified, considering never types
    /// TODO: Replace with proper attribute system when macro system is implemented
    fn types_are_compatible_with_never(&self, expected: &Type, found: &Type) -> bool {
        // Never types can unify with any type (bottom type property)
        // TODO: Implement proper never type checking
        // For now, skip never type compatibility
        let _ = (found, expected);

        // Fall back to regular type compatibility
        self.types_are_compatible(expected, found)
    }

    // ============================================================================
    /// Direct case expression processing that bypasses the task system to handle pattern bindings correctly
    fn infer_case_expression_direct(
        &mut self,
        case_expr: &outrun_parser::CaseExpression,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // First, infer the scrutinee type
        let mut scrutinee_expr = case_expr.expression.clone();
        let scrutinee_result = self.infer_expression(&mut scrutinee_expr, context)?;
        let scrutinee_type = scrutinee_result.inferred_type;

        let mut clause_result_types = Vec::new();
        let mut all_constraints = context.constraints.clone();

        // Process each clause with pattern bindings
        for clause in &case_expr.clauses {
            // Create a new context for this clause
            let mut clause_context = context.clone();

            // Extract pattern bindings
            let pattern_bindings =
                self.extract_pattern_bindings(&clause.pattern, &scrutinee_type)?;
            // Add pattern bindings to clause context
            for (name, binding_type) in pattern_bindings {
                clause_context.bindings.insert(name, binding_type);
            }

            // Process guard if present
            if let Some(guard_expr) = &clause.guard {
                let mut guard_expr_mut = guard_expr.clone();
                self.infer_expression(&mut guard_expr_mut, &mut clause_context)?;
            }

            // Process clause result
            let mut clause_result_expr = match &clause.result {
                outrun_parser::CaseResult::Expression(expr) => (**expr).clone(),
                outrun_parser::CaseResult::Block(block) => {
                    // For blocks, we need to create an expression from the block
                    // For now, just return a fresh type variable
                    outrun_parser::Expression {
                        kind: outrun_parser::ExpressionKind::Boolean(
                            outrun_parser::BooleanLiteral {
                                value: true,
                                span: block.span,
                            },
                        ),
                        span: block.span,
                        type_info: None,
                    }
                }
            };

            let clause_result =
                self.infer_expression(&mut clause_result_expr, &mut clause_context)?;
            clause_result_types.push(clause_result.inferred_type);
            all_constraints.extend(clause_result.constraints);
        }

        // The result type is the type of the first clause (they should all be compatible)
        let result_type = clause_result_types
            .into_iter()
            .next()
            .unwrap_or_else(|| Type::variable(self.fresh_type_var(), Level(0)));

        // Check exhaustiveness of patterns using proper type compatibility
        // We need to extract the type compatibility logic to avoid borrowing issues
        self.check_case_exhaustiveness_with_proper_types(case_expr, &scrutinee_type)?;

        Ok(InferenceResult {
            inferred_type: result_type,
            constraints: all_constraints,
            substitution: context.substitution.clone(),
        })
    }

    ///
    /// Main iterative inference method (replaces recursive infer_expression)
    pub fn infer_expression(
        &mut self,
        expression: &mut Expression,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // CRITICAL FIX: Track inference depth for proper task state management
        self.inference_depth += 1;
        // Execute main logic with proper cleanup
        let result = self.infer_expression_impl(expression, context);

        // CRITICAL FIX: Always decrement depth on exit (both success and error paths)
        self.inference_depth -= 1;

        result
    }

    /// Extract pattern bindings from a case pattern and their types
    fn extract_pattern_bindings(
        &mut self,
        pattern: &outrun_parser::Pattern,
        scrutinee_type: &Type,
    ) -> Result<Vec<(String, Type)>, TypecheckError> {
        let mut bindings = Vec::new();

        match pattern {
            outrun_parser::Pattern::Identifier(identifier) => {
                // Simple identifier pattern binds the entire scrutinee
                bindings.push((identifier.name.clone(), scrutinee_type.clone()));
            }
            outrun_parser::Pattern::Literal(_) => {
                // Literal patterns don't bind any variables
            }
            outrun_parser::Pattern::Struct(struct_pattern) => {
                // For struct patterns, we need to extract field types
                // This is a simplified implementation - full destructuring would need
                // to resolve the struct type and match field names
                for field in &struct_pattern.fields {
                    if let Some(pattern) = &field.pattern {
                        let field_type = Type::variable(self.fresh_type_var(), Level(0));
                        let field_bindings = self.extract_pattern_bindings(pattern, &field_type)?;
                        bindings.extend(field_bindings);
                    }
                }
            }
            outrun_parser::Pattern::List(list_pattern) => {
                // For list patterns, extract element types
                if let Type::Concrete { name, args, .. } = scrutinee_type {
                    if name.as_str() == "List" && args.len() == 1 {
                        let element_type = &args[0];
                        for element_pattern in &list_pattern.elements {
                            let element_bindings =
                                self.extract_pattern_bindings(element_pattern, element_type)?;
                            bindings.extend(element_bindings);
                        }
                    }
                }
            }
            outrun_parser::Pattern::Tuple(tuple_pattern) => {
                // For tuple patterns, extract element types from the tuple
                match scrutinee_type {
                    Type::Tuple { element_types, .. } => {
                        if element_types.len() == tuple_pattern.elements.len() {
                            // Match each tuple element pattern with its corresponding type
                            for (element_pattern, element_type) in
                                tuple_pattern.elements.iter().zip(element_types.iter())
                            {
                                let element_bindings =
                                    self.extract_pattern_bindings(element_pattern, element_type)?;
                                bindings.extend(element_bindings);
                            }
                        } else {
                            // Arity mismatch - assign fresh type variables for now
                            for element_pattern in &tuple_pattern.elements {
                                let element_type = Type::variable(self.fresh_type_var(), Level(0));
                                let element_bindings =
                                    self.extract_pattern_bindings(element_pattern, &element_type)?;
                                bindings.extend(element_bindings);
                            }
                        }
                    }
                    Type::Concrete { name, args, .. } => {
                        if name.as_str().contains("Tuple")
                            && args.len() == tuple_pattern.elements.len()
                        {
                            // Legacy support for Tuple<T1, T2, ...> representation
                            for (i, element_pattern) in tuple_pattern.elements.iter().enumerate() {
                                if let Some(element_type) = args.get(i) {
                                    let element_bindings = self
                                        .extract_pattern_bindings(element_pattern, element_type)?;
                                    bindings.extend(element_bindings);
                                }
                            }
                        } else {
                            // If we can't match the tuple structure, assign fresh type variables
                            for element_pattern in &tuple_pattern.elements {
                                let element_type = Type::variable(self.fresh_type_var(), Level(0));
                                let element_bindings =
                                    self.extract_pattern_bindings(element_pattern, &element_type)?;
                                bindings.extend(element_bindings);
                            }
                        }
                    }
                    _ => {
                        // Non-tuple scrutinee type - assign fresh type variables
                        for element_pattern in &tuple_pattern.elements {
                            let element_type = Type::variable(self.fresh_type_var(), Level(0));
                            let element_bindings =
                                self.extract_pattern_bindings(element_pattern, &element_type)?;
                            bindings.extend(element_bindings);
                        }
                    }
                }
            }
        }

        Ok(bindings)
    }

    /// Implementation of inference logic using stack-based continuations
    fn infer_expression_impl(
        &mut self,
        expression: &mut Expression,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::ExpressionKind;

        // Special case: Handle case expressions directly to avoid task system issues with pattern bindings
        if let ExpressionKind::CaseExpression(case_expr) = &expression.kind {
            return self.infer_case_expression_direct(case_expr, context);
        }

        let mut work_stack: Vec<WorkItem> = vec![WorkItem::EvaluateExpression {
            expression: expression as *mut Expression,
            context: context.clone(),
        }];
        let mut result_stack: Vec<InferenceResult> = Vec::new();

        while let Some(work_item) = work_stack.pop() {
            match work_item {
                WorkItem::EvaluateExpression {
                    expression,
                    context,
                } => {
                    let expr_ref = unsafe { &mut *expression };

                    match &mut expr_ref.kind {
                        // Leaf expressions - no dependencies, process immediately
                        ExpressionKind::Boolean(_)
                        | ExpressionKind::Integer(_)
                        | ExpressionKind::Float(_)
                        | ExpressionKind::String(_)
                        | ExpressionKind::Atom(_)
                        | ExpressionKind::Sigil(_)
                        | ExpressionKind::Identifier(_)
                        | ExpressionKind::TypeIdentifier(_)
                        | ExpressionKind::QualifiedIdentifier(_)
                        | ExpressionKind::FunctionCapture(_) => {
                            let result = self.infer_leaf_expression(expr_ref, &context)?;
                            result_stack.push(result);
                        }

                        // Composite expressions - push continuation then dependencies
                        ExpressionKind::FunctionCall(func_call) => {
                            let arg_count = func_call.arguments.len();

                            // Push continuation first (processed after dependencies)
                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: arg_count,
                            });

                            // Push argument evaluations in reverse order
                            for arg in func_call.arguments.iter_mut().rev() {
                                match arg {
                                    outrun_parser::Argument::Named {
                                        expression: arg_expr,
                                        ..
                                    } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: arg_expr as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                    outrun_parser::Argument::Spread {
                                        expression: arg_expr,
                                        ..
                                    } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: arg_expr as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                }
                            }
                        }

                        ExpressionKind::List(list_literal) => {
                            let element_count = list_literal.elements.len();

                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: element_count,
                            });

                            // Push element evaluations in reverse order
                            for element in list_literal.elements.iter_mut().rev() {
                                match element {
                                    outrun_parser::ListElement::Expression(expr) => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: expr.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                    outrun_parser::ListElement::Spread(_identifier) => {
                                        // For now, skip spread elements - they need special handling
                                        // TODO: Implement proper spread element type inference
                                    }
                                }
                            }
                        }

                        ExpressionKind::Tuple(tuple_literal) => {
                            let element_count = tuple_literal.elements.len();

                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: element_count,
                            });

                            // Push element evaluations in reverse order
                            for element in tuple_literal.elements.iter_mut().rev() {
                                work_stack.push(WorkItem::EvaluateExpression {
                                    expression: element as *mut Expression,
                                    context: context.clone(),
                                });
                            }
                        }

                        ExpressionKind::Map(map_literal) => {
                            let mut dep_count = 0;
                            for entry in &map_literal.entries {
                                match entry {
                                    outrun_parser::MapEntry::Assignment { .. } => dep_count += 2, // key + value
                                    outrun_parser::MapEntry::Shorthand { .. } => dep_count += 1, // value only (key is identifier)
                                    outrun_parser::MapEntry::Spread(_) => dep_count += 0, // skip spreads for now
                                }
                            }

                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: dep_count,
                            });

                            // Push entry evaluations in reverse order
                            for entry in map_literal.entries.iter_mut().rev() {
                                match entry {
                                    outrun_parser::MapEntry::Assignment { key, value } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: value.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: key.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                    outrun_parser::MapEntry::Shorthand { value, .. } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: value.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                    outrun_parser::MapEntry::Spread(_) => {
                                        // Skip spread elements for now
                                        // TODO: Implement proper spread element type inference
                                    }
                                }
                            }
                        }

                        ExpressionKind::Struct(struct_literal) => {
                            // Count only fields that have expressions to evaluate
                            let mut field_count = 0;
                            for field in &struct_literal.fields {
                                match field {
                                    outrun_parser::StructLiteralField::Assignment { .. } => {
                                        field_count += 1
                                    }
                                    outrun_parser::StructLiteralField::Shorthand(_) => {} // No expression to evaluate
                                    outrun_parser::StructLiteralField::Spread(_) => {} // Skip spreads for now
                                }
                            }

                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: field_count,
                            });

                            // Push field value evaluations in reverse order
                            for field in struct_literal.fields.iter_mut().rev() {
                                match field {
                                    outrun_parser::StructLiteralField::Assignment {
                                        value, ..
                                    } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: value.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                    outrun_parser::StructLiteralField::Shorthand(_) => {
                                        // Shorthand fields don't have expressions to evaluate
                                        // The identifier itself is the value
                                    }
                                    outrun_parser::StructLiteralField::Spread(_) => {
                                        // Skip spread elements for now
                                        // TODO: Implement proper spread element type inference
                                    }
                                }
                            }
                        }

                        ExpressionKind::BinaryOp(_) => {
                            unreachable!(
                                "Binary operations should be desugared before type checking"
                            );
                        }

                        ExpressionKind::UnaryOp(_) => {
                            unreachable!(
                                "Unary operations should be desugared before type checking"
                            );
                        }

                        ExpressionKind::FieldAccess(field_access) => {
                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: 1, // object
                            });

                            work_stack.push(WorkItem::EvaluateExpression {
                                expression: field_access.object.as_mut() as *mut Expression,
                                context: context.clone(),
                            });
                        }

                        ExpressionKind::Parenthesized(inner_expr) => {
                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: 1, // inner expression
                            });

                            work_stack.push(WorkItem::EvaluateExpression {
                                expression: inner_expr.as_mut() as *mut Expression,
                                context: context.clone(),
                            });
                        }

                        ExpressionKind::IfExpression(if_expr) => {
                            let mut dep_count = 2; // condition + then
                            if if_expr.else_block.is_some() {
                                dep_count = 3; // condition + then + else
                            }

                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: dep_count,
                            });

                            // Push in reverse order: else, then, condition
                            if let Some(else_block) = &mut if_expr.else_block {
                                // For now, treat blocks as single expressions (simplified)
                                // TODO: Handle full block processing
                                if let Some(last_stmt) = else_block.statements.last_mut() {
                                    if let outrun_parser::StatementKind::Expression(expr) =
                                        &mut last_stmt.kind
                                    {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: expr.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                }
                            }

                            // Handle then block
                            if let Some(last_stmt) = if_expr.then_block.statements.last_mut() {
                                if let outrun_parser::StatementKind::Expression(expr) =
                                    &mut last_stmt.kind
                                {
                                    work_stack.push(WorkItem::EvaluateExpression {
                                        expression: expr.as_mut() as *mut Expression,
                                        context: context.clone(),
                                    });
                                }
                            }

                            work_stack.push(WorkItem::EvaluateExpression {
                                expression: if_expr.condition.as_mut() as *mut Expression,
                                context: context.clone(),
                            });
                        }

                        ExpressionKind::CaseExpression(case_expr) => {
                            // case_expr has: expression (scrutinee) + clauses
                            let clause_count = case_expr.clauses.len();

                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: 1 + clause_count, // scrutinee + clauses
                            });

                            // Push clause evaluations in reverse order
                            for clause in case_expr.clauses.iter_mut().rev() {
                                // For now, just evaluate the clause result
                                // TODO: Handle pattern matching and guards properly
                                match &mut clause.result {
                                    outrun_parser::CaseResult::Expression(expr) => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: expr.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                    outrun_parser::CaseResult::Block(block) => {
                                        // For now, treat blocks as single expressions (simplified)
                                        if let Some(last_stmt) = block.statements.last_mut() {
                                            if let outrun_parser::StatementKind::Expression(expr) =
                                                &mut last_stmt.kind
                                            {
                                                work_stack.push(WorkItem::EvaluateExpression {
                                                    expression: expr.as_mut() as *mut Expression,
                                                    context: context.clone(),
                                                });
                                            }
                                        }
                                    }
                                }
                            }

                            // Push scrutinee evaluation
                            work_stack.push(WorkItem::EvaluateExpression {
                                expression: case_expr.expression.as_mut() as *mut Expression,
                                context: context.clone(),
                            });
                        }

                        ExpressionKind::AnonymousFunction(anon_fn) => {
                            // Anonymous functions have clauses with bodies
                            let clause_count = anon_fn.clauses.len();

                            work_stack.push(WorkItem::ContinueWithResults {
                                expression,
                                context: context.clone(),
                                dependency_count: clause_count,
                            });

                            // Push clause body evaluations in reverse order
                            for clause in anon_fn.clauses.iter_mut().rev() {
                                match &mut clause.body {
                                    outrun_parser::AnonymousBody::Expression(expr) => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: expr.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                    outrun_parser::AnonymousBody::Block(block) => {
                                        // For now, treat blocks as single expressions (simplified)
                                        if let Some(last_stmt) = block.statements.last_mut() {
                                            if let outrun_parser::StatementKind::Expression(expr) =
                                                &mut last_stmt.kind
                                            {
                                                work_stack.push(WorkItem::EvaluateExpression {
                                                    expression: expr.as_mut() as *mut Expression,
                                                    context: context.clone(),
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        ExpressionKind::MacroInjection(_) => {
                            // Macro injections should be expanded before type checking
                            return Err(TypecheckError::InferenceError(
                                InferenceError::AmbiguousType {
                                    span: to_source_span(Some(expr_ref.span)),
                                    suggestions: vec![
                                        "Macro injections should be expanded before type checking"
                                            .to_string(),
                                    ],
                                },
                            ));
                        }
                    }
                }

                WorkItem::ContinueWithResults {
                    expression,
                    context,
                    dependency_count,
                } => {
                    // Collect dependency results from result_stack
                    let mut dependency_results = Vec::new();
                    for _ in 0..dependency_count {
                        if let Some(result) = result_stack.pop() {
                            dependency_results.push(result);
                        } else {
                            return Err(TypecheckError::InferenceError(
                                InferenceError::AmbiguousType {
                                    span: to_source_span(None),
                                    suggestions: vec!["Missing dependency result".to_string()],
                                },
                            ));
                        }
                    }
                    dependency_results.reverse(); // Restore correct order

                    // Process expression with dependency results
                    let expr_ref = unsafe { &mut *expression };
                    let result = self.infer_expression_with_dependencies(
                        expr_ref,
                        &context,
                        &dependency_results,
                    )?;
                    result_stack.push(result);
                }
            }
        }

        // Return the final result
        result_stack.pop().ok_or_else(|| {
            TypecheckError::InferenceError(InferenceError::AmbiguousType {
                span: to_source_span(Some(expression.span)),
                suggestions: vec!["No result produced".to_string()],
            })
        })
    }

    /// Process leaf expressions that have no dependencies
    fn infer_leaf_expression(
        &mut self,
        expression: &mut Expression,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::ExpressionKind;

        match &expression.kind {
            ExpressionKind::Boolean(_) => Ok(InferenceResult {
                inferred_type: Type::concrete("Outrun.Core.Boolean"),
                constraints: Vec::new(),
                substitution: Substitution::new(),
            }),
            ExpressionKind::Integer(_) => Ok(InferenceResult {
                inferred_type: Type::concrete("Outrun.Core.Integer64"),
                constraints: Vec::new(),
                substitution: Substitution::new(),
            }),
            ExpressionKind::Float(_) => Ok(InferenceResult {
                inferred_type: Type::concrete("Outrun.Core.Float64"),
                constraints: Vec::new(),
                substitution: Substitution::new(),
            }),
            ExpressionKind::String(_) => Ok(InferenceResult {
                inferred_type: Type::concrete("Outrun.Core.String"),
                constraints: Vec::new(),
                substitution: Substitution::new(),
            }),
            ExpressionKind::Atom(_) => Ok(InferenceResult {
                inferred_type: Type::concrete("Outrun.Core.Atom"),
                constraints: Vec::new(),
                substitution: Substitution::new(),
            }),
            ExpressionKind::Sigil(_) => {
                // Sigil literals need special handling - they're processed by sigil protocols
                // For now, return a generic type - this should be enhanced later
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Outrun.Core.String"), // Simplified - sigils often produce strings
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            }
            ExpressionKind::Identifier(identifier) => {
                // Look up identifier in context bindings or symbol table
                if let Some(var_type) = context.bindings.get(&identifier.name) {
                    Ok(InferenceResult {
                        inferred_type: var_type.clone(),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                } else if let Some(var_type) = self.symbol_table.get(&identifier.name) {
                    Ok(InferenceResult {
                        inferred_type: var_type.clone(),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                } else {
                    // Handle Self type
                    if identifier.name == "Self" {
                        let self_type = Type::SelfType {
                            binding_context: context.self_binding.clone(),
                            span: Some(expression.span),
                        };

                        // Try to resolve Self to concrete type
                        if let Some(resolved_self) = self_type.resolve_self() {
                            Ok(InferenceResult {
                                inferred_type: resolved_self,
                                constraints: Vec::new(),
                                substitution: Substitution::new(),
                            })
                        } else {
                            // Return unresolved Self type (will be resolved later)
                            Ok(InferenceResult {
                                inferred_type: self_type,
                                constraints: Vec::new(),
                                substitution: Substitution::new(),
                            })
                        }
                    } else {
                        Err(TypecheckError::InferenceError(
                            InferenceError::AmbiguousType {
                                span: to_source_span(Some(expression.span)),
                                suggestions: vec![format!(
                                    "Unknown identifier: {}",
                                    identifier.name
                                )],
                            },
                        ))
                    }
                }
            }
            ExpressionKind::TypeIdentifier(type_id) => {
                // Type identifiers represent types themselves
                // For now, treat them as concrete types
                Ok(InferenceResult {
                    inferred_type: Type::concrete(&type_id.name),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            }
            ExpressionKind::QualifiedIdentifier(qualified_id) => {
                // Use existing qualified identifier inference
                let mut mutable_context = context.clone();
                self.infer_qualified_identifier(qualified_id, &mut mutable_context)
            }
            ExpressionKind::FunctionCapture(_) => {
                // Function captures create function types
                // For now, return a generic function type - this needs proper implementation
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Function"), // Simplified
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            }
            _ => Err(TypecheckError::InferenceError(
                InferenceError::AmbiguousType {
                    span: to_source_span(Some(expression.span)),
                    suggestions: vec!["Expression type not supported as leaf".to_string()],
                },
            )),
        }
    }

    /// Process composite expressions using results from their dependencies
    fn infer_expression_with_dependencies(
        &mut self,
        expression: &mut Expression,
        context: &InferenceContext,
        dependency_results: &[InferenceResult],
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::ExpressionKind;

        match &mut expression.kind {
            ExpressionKind::FunctionCall(func_call) => {
                // Convert dependency results to argument types
                let arg_types: Vec<Option<Type>> = dependency_results
                    .iter()
                    .map(|result| Some(result.inferred_type.clone()))
                    .collect();

                // Use existing function call inference with precomputed argument types
                let mut mutable_context = context.clone();
                self.infer_function_call_with_precomputed_args(
                    func_call,
                    &mut mutable_context,
                    &arg_types,
                )
            }

            ExpressionKind::List(_) => {
                // All elements have been processed, infer list type from element types
                if dependency_results.is_empty() {
                    // Empty list - use generic List<T> with fresh type variable
                    let element_type = Type::variable(self.fresh_type_var(), Level(0));
                    Ok(InferenceResult {
                        inferred_type: Type::generic_concrete("List", vec![element_type]),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                } else {
                    // Non-empty list - all elements should have the same type
                    let first_element_type = &dependency_results[0].inferred_type;

                    // Check that all elements have compatible types
                    for (i, result) in dependency_results.iter().enumerate().skip(1) {
                        if !self.types_are_compatible(first_element_type, &result.inferred_type) {
                            return Err(TypecheckError::InferenceError(
                                InferenceError::AmbiguousType {
                                    span: to_source_span(Some(expression.span)),
                                    suggestions: vec![format!(
                                        "List element {} has type {}, but expected {}",
                                        i, result.inferred_type, first_element_type
                                    )],
                                },
                            ));
                        }
                    }

                    Ok(InferenceResult {
                        inferred_type: Type::generic_concrete(
                            "List",
                            vec![first_element_type.clone()],
                        ),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                }
            }

            ExpressionKind::Tuple(_) => {
                // Tuple type is the product of all element types
                let element_types: Vec<Type> = dependency_results
                    .iter()
                    .map(|result| result.inferred_type.clone())
                    .collect();

                Ok(InferenceResult {
                    inferred_type: Type::Tuple {
                        element_types,
                        span: Some(expression.span),
                    },
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            }

            ExpressionKind::Map(_) => {
                // Map type is Outrun.Core.Map<K, V> where K and V are inferred from entries
                if dependency_results.is_empty() {
                    // Empty map - check if we have an expected type
                    if let Some(expected_type) = &context.expected_type {
                        // Use the expected type directly
                        Ok(InferenceResult {
                            inferred_type: expected_type.clone(),
                            constraints: Vec::new(),
                            substitution: Substitution::new(),
                        })
                    } else {
                        // Empty map without type hint gets fresh type variables
                        let key_type = Type::variable(self.fresh_type_var(), Level(0));
                        let value_type = Type::variable(self.fresh_type_var(), Level(0));
                        Ok(InferenceResult {
                            inferred_type: Type::generic_concrete(
                                "Outrun.Core.Map",
                                vec![key_type, value_type],
                            ),
                            constraints: Vec::new(),
                            substitution: Substitution::new(),
                        })
                    }
                } else {
                    // Non-empty map - infer key and value types from first entry
                    // dependency_results are in pairs: [key1, value1, key2, value2, ...]
                    let first_key_type = &dependency_results[0].inferred_type;
                    let first_value_type = &dependency_results[1].inferred_type;

                    // Check that all keys and values have compatible types
                    for chunk in dependency_results.chunks(2).skip(1) {
                        if chunk.len() == 2 {
                            if !self.types_are_compatible(first_key_type, &chunk[0].inferred_type) {
                                return Err(TypecheckError::InferenceError(
                                    InferenceError::AmbiguousType {
                                        span: to_source_span(Some(expression.span)),
                                        suggestions: vec![format!(
                                            "Map key has type {}, but expected {}",
                                            chunk[0].inferred_type, first_key_type
                                        )],
                                    },
                                ));
                            }
                            if !self.types_are_compatible(first_value_type, &chunk[1].inferred_type)
                            {
                                return Err(TypecheckError::InferenceError(
                                    InferenceError::AmbiguousType {
                                        span: to_source_span(Some(expression.span)),
                                        suggestions: vec![format!(
                                            "Map value has type {}, but expected {}",
                                            chunk[1].inferred_type, first_value_type
                                        )],
                                    },
                                ));
                            }
                        }
                    }

                    Ok(InferenceResult {
                        inferred_type: Type::generic_concrete(
                            "Outrun.Core.Map",
                            vec![first_key_type.clone(), first_value_type.clone()],
                        ),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                }
            }

            ExpressionKind::Struct(struct_literal) => {
                // Struct type is determined by the struct name and field types
                let struct_name = struct_literal
                    .type_path
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // CRITICAL FIX: Use expected type for bidirectional inference of generic parameters
                if let Some(expected_type) = &context.expected_type {
                    // Check if the expected type matches this struct name
                    let expected_name = match expected_type {
                        Type::Concrete { name, .. } => Some(name.as_str()),
                        Type::Protocol { name, .. } => Some(name.as_str()),
                        _ => None,
                    };

                    if let Some(expected_name) = expected_name {
                        if expected_name == struct_name {
                            // Use the expected type which includes generic parameters
                            return Ok(InferenceResult {
                                inferred_type: expected_type.clone(),
                                constraints: Vec::new(),
                                substitution: Substitution::new(),
                            });
                        }
                    }

                    // Expected type doesn't match, fall back to concrete type
                    Ok(InferenceResult {
                        inferred_type: Type::concrete(&struct_name),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                } else {
                    // No expected type, use concrete struct type
                    Ok(InferenceResult {
                        inferred_type: Type::concrete(&struct_name),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                }
            }

            ExpressionKind::FieldAccess(field_access) => {
                // dependency_results[0] = object
                let object_type = &dependency_results[0].inferred_type;

                // Use existing field access inference
                let mut mutable_context = context.clone();
                self.infer_field_access_type(
                    object_type,
                    &field_access.field.name,
                    &mut mutable_context,
                )
            }

            ExpressionKind::Parenthesized(_) => {
                // Parenthesized expressions just pass through the inner type
                // dependency_results[0] = inner expression
                Ok(InferenceResult {
                    inferred_type: dependency_results[0].inferred_type.clone(),
                    constraints: dependency_results[0].constraints.clone(),
                    substitution: dependency_results[0].substitution.clone(),
                })
            }

            ExpressionKind::IfExpression(if_expr) => {
                // dependency_results[0] = condition, [1] = then, [2] = else (if present)
                let condition_type = &dependency_results[0].inferred_type;
                let then_type = &dependency_results[1].inferred_type;
                let else_type = if dependency_results.len() > 2 {
                    Some(&dependency_results[2].inferred_type)
                } else {
                    None
                };

                // Verify condition is Boolean
                let boolean_type = Type::concrete("Outrun.Core.Boolean");
                if !self.types_are_compatible(condition_type, &boolean_type) {
                    return Err(TypecheckError::InferenceError(
                        InferenceError::AmbiguousType {
                            span: to_source_span(Some(if_expr.condition.span)),
                            suggestions: vec![format!(
                                "Expected Boolean, found {}",
                                condition_type
                            )],
                        },
                    ));
                }

                // Determine result type
                let result_type = if let Some(else_type) = else_type {
                    // Both branches present - they must be compatible
                    if self.types_are_compatible(then_type, else_type) {
                        then_type.clone()
                    } else {
                        return Err(TypecheckError::InferenceError(
                            InferenceError::AmbiguousType {
                                span: to_source_span(Some(expression.span)),
                                suggestions: vec![format!(
                                    "If branches have incompatible types: {} vs {}",
                                    then_type, else_type
                                )],
                            },
                        ));
                    }
                } else {
                    // No else branch - result is Option<then_type>
                    Type::generic_concrete("Option", vec![then_type.clone()])
                };

                Ok(InferenceResult {
                    inferred_type: result_type,
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            }

            ExpressionKind::CaseExpression(_) => {
                // dependency_results[0] = scrutinee, [1..] = clause results

                if dependency_results.len() < 2 {
                    return Err(TypecheckError::InferenceError(
                        InferenceError::AmbiguousType {
                            span: to_source_span(Some(expression.span)),
                            suggestions: vec![
                                "Case expression must have at least one clause".to_string()
                            ],
                        },
                    ));
                }

                // All clause results should have compatible types
                let first_clause_type = &dependency_results[1].inferred_type;
                for (i, result) in dependency_results.iter().enumerate().skip(2) {
                    if !self.types_are_compatible(first_clause_type, &result.inferred_type) {
                        return Err(TypecheckError::InferenceError(
                            InferenceError::AmbiguousType {
                                span: to_source_span(Some(expression.span)),
                                suggestions: vec![format!(
                                    "Case clause {} has type {}, but expected {}",
                                    i - 1,
                                    result.inferred_type,
                                    first_clause_type
                                )],
                            },
                        ));
                    }
                }

                // TODO: Add pattern matching validation against scrutinee_type

                Ok(InferenceResult {
                    inferred_type: first_clause_type.clone(),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            }

            ExpressionKind::AnonymousFunction(_) => {
                // Anonymous functions create function types
                // For now, create a generic function type
                // TODO: Infer proper parameter and return types from clauses

                if dependency_results.is_empty() {
                    return Err(TypecheckError::InferenceError(
                        InferenceError::AmbiguousType {
                            span: to_source_span(Some(expression.span)),
                            suggestions: vec![
                                "Anonymous function must have at least one clause".to_string()
                            ],
                        },
                    ));
                }

                // All clause bodies should have compatible return types
                let first_return_type = &dependency_results[0].inferred_type;
                for (i, result) in dependency_results.iter().enumerate().skip(1) {
                    if !self.types_are_compatible(first_return_type, &result.inferred_type) {
                        return Err(TypecheckError::InferenceError(
                            InferenceError::AmbiguousType {
                                span: to_source_span(Some(expression.span)),
                                suggestions: vec![format!(
                                    "Function clause {} returns {}, but expected {}",
                                    i, result.inferred_type, first_return_type
                                )],
                            },
                        ));
                    }
                }

                // Create a simplified function type
                // TODO: Extract proper parameter types from clause parameters
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Function"), // Simplified
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            }

            _ => Err(TypecheckError::InferenceError(
                InferenceError::AmbiguousType {
                    span: to_source_span(Some(expression.span)),
                    suggestions: vec!["Expression type not supported with dependencies".to_string()],
                },
            )),
        }
    }

    /// Core function body type checking logic using RefCell for interior mutability
    #[allow(clippy::result_large_err)]
    fn typecheck_function_body(
        &mut self,
        scope: &str,
        function: &FunctionDefinition,
    ) -> Result<(), TypecheckError> {
        // Convert the declared return type for bidirectional inference
        let declared_return_type = self.convert_type_annotation(&function.return_type)?;

        // Resolve Self type if the return type is Self
        let resolved_return_type = if matches!(&declared_return_type, Type::SelfType { .. }) {
            // Use the Type's resolve_self method
            declared_return_type
                .resolve_self()
                .unwrap_or_else(|| declared_return_type.clone())
        } else {
            declared_return_type.clone()
        };

        // Create a new inference context for this function with expected return type
        let mut function_context = InferenceContext {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: Some(resolved_return_type.clone()), // Use resolved type for bidirectional inference
            self_binding: self.current_self_context.clone(), // Use current self context from impl block
            bindings: HashMap::new(),
        };

        // Set up parameter bindings in symbol table
        let old_symbol_table = self.symbol_table.clone();

        // Add function parameters to symbol table
        for param in &function.parameters {
            let param_name = param.name.name.clone();
            let param_type = self.convert_type_annotation(&param.type_annotation)?;
            self.symbol_table.insert(param_name, param_type);
        }

        // Type check the function body using RefCell for mutability
        let body_type = self.typecheck_block_readonly(&function.body, &mut function_context)?;

        // Verify that the body type matches the declared return type
        // WORKAROUND: Use never type compatibility check
        // TODO: Replace with proper attribute system when macro system is implemented
        if !self.types_are_compatible_with_never(&declared_return_type, &body_type) {
            self.symbol_table = old_symbol_table; // Restore before error
            return Err(TypecheckError::UnificationError(
                OriginalUnificationError::TypeMismatch {
                    expected: declared_return_type,
                    found: body_type,
                    expected_context: Some("declared return type".to_string()),
                    found_context: Some(format!("Function {}.{} body", scope, function.name.name)),
                    span: to_source_span(Some(function.body.span)),
                },
            ));
        }

        // Type check guard clause if present
        if let Some(guard) = &function.guard {
            self.typecheck_guard_clause_readonly(guard, &mut function_context)?;

            // Check guard exhaustiveness (simplified - single guard case)
            let param_types: Vec<(String, Type)> = function
                .parameters
                .iter()
                .map(|param| {
                    let param_type = self
                        .convert_type_annotation(&param.type_annotation)
                        .unwrap_or_else(|_| Type::concrete("Unknown"));
                    (param.name.name.clone(), param_type)
                })
                .collect();

            self.exhaustiveness_checker
                .check_guard_exhaustiveness(&[guard.clone()], &param_types)?;
        }

        // Restore the previous symbol table
        self.symbol_table = old_symbol_table;

        Ok(())
    }

    /// Type check static function body
    #[allow(clippy::result_large_err)]
    fn typecheck_static_function_body(
        &mut self,
        scope: &str,
        static_def: &StaticFunctionDefinition,
    ) -> Result<(), TypecheckError> {
        // Convert the declared return type for bidirectional inference
        let declared_return_type = self.convert_type_annotation(&static_def.return_type)?;

        // Create a new inference context for this function with expected return type
        let mut function_context = InferenceContext {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: Some(declared_return_type.clone()), // CRITICAL FIX: Set expected type for bidirectional inference
            self_binding: SelfBindingContext::ProtocolDefinition {
                protocol_name: ModuleName::new(scope),
                protocol_args: vec![],
            },
            bindings: HashMap::new(),
        };

        // Set up parameter bindings in symbol table
        let old_symbol_table = self.symbol_table.clone();

        // Add function parameters to symbol table
        for param in &static_def.parameters {
            let param_name = param.name.name.clone();
            let param_type = self.convert_type_annotation(&param.type_annotation)?;
            self.symbol_table.insert(param_name, param_type);
        }

        // Type check the function body
        let body_type = self.typecheck_block_readonly(&static_def.body, &mut function_context)?;

        // declared_return_type was already converted above for bidirectional inference

        // Verify that the body type matches the declared return type
        // WORKAROUND: Use never type compatibility check
        // TODO: Replace with proper attribute system when macro system is implemented
        if !self.types_are_compatible_with_never(&declared_return_type, &body_type) {
            self.symbol_table = old_symbol_table; // Restore before error
            return Err(TypecheckError::UnificationError(
                OriginalUnificationError::TypeMismatch {
                    expected: declared_return_type,
                    found: body_type,
                    expected_context: Some("declared return type".to_string()),
                    found_context: Some(format!(
                        "Static function {}.{} body",
                        scope, static_def.name.name
                    )),
                    span: to_source_span(Some(static_def.body.span)),
                },
            ));
        }

        // Restore the previous symbol table
        self.symbol_table = old_symbol_table;

        Ok(())
    }

    /// Type check a block without requiring mutable access to expressions
    #[allow(clippy::result_large_err)]
    fn typecheck_block_readonly(
        &mut self,
        block: &outrun_parser::Block,
        context: &mut InferenceContext,
    ) -> Result<Type, TypecheckError> {
        use std::cell::RefCell;
        use std::rc::Rc;

        if block.statements.is_empty() {
            return Err(TypecheckError::UnificationError(
                OriginalUnificationError::TypeMismatch {
                    expected: Type::concrete("NonEmptyBlock"),
                    found: Type::concrete("EmptyBlock"),
                    expected_context: Some("non-empty function body".to_string()),
                    found_context: Some("empty block".to_string()),
                    span: to_source_span(Some(block.span)),
                },
            ));
        }

        let mut last_type = None;

        // Type check each statement
        for (i, statement) in block.statements.iter().enumerate() {
            let is_last = i == block.statements.len() - 1;

            match &statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    // Create RefCell wrapper for the expression to enable mutability
                    let expr_cell = Rc::new(RefCell::new((**expr).clone()));
                    let result = {
                        let mut expr_mut = expr_cell.borrow_mut();
                        // Only pass expected type to the last expression in the block
                        if is_last {
                            self.infer_expression(&mut expr_mut, context)?
                        } else {
                            // For non-last expressions, create a context without expected type
                            let mut no_expected_context = context.clone();
                            no_expected_context.expected_type = None;
                            self.infer_expression(&mut expr_mut, &mut no_expected_context)?
                        }
                    };
                    last_type = Some(result.inferred_type);
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    // Type check the let binding expression and add to symbol table
                    // Let bindings never get the expected type since they don't contribute to return value
                    let mut no_expected_context = context.clone();
                    no_expected_context.expected_type = None;
                    self.typecheck_let_binding_statement_readonly(
                        let_binding,
                        &mut no_expected_context,
                    )?;
                    // Let bindings don't change the block's return type unless they're the last statement
                    // If this is the last statement in the block, then the block has no meaningful return value
                    // which should be an error since Outrun doesn't have Unit types.
                    // We'll handle this case in validation after the loop.
                }
            }
        }

        // The type of a block is the type of its last statement
        // If there's no meaningful return type (e.g., last statement was a let binding),
        // that's an error since Outrun doesn't have Unit types
        match last_type {
            Some(t) => Ok(t),
            None => Err(TypecheckError::UnificationError(
                OriginalUnificationError::TypeMismatch {
                    expected: Type::concrete("BlockWithReturnValue"),
                    found: Type::concrete("BlockWithoutReturnValue"),
                    expected_context: Some("block with meaningful return value".to_string()),
                    found_context: Some("block ending with let binding".to_string()),
                    span: to_source_span(Some(block.span)),
                },
            )),
        }
    }

    /// Type check a let binding statement without requiring mutability
    #[allow(clippy::result_large_err)]
    fn typecheck_let_binding_statement_readonly(
        &mut self,
        let_binding: &outrun_parser::LetBinding,
        context: &mut InferenceContext,
    ) -> Result<(), TypecheckError> {
        use std::cell::RefCell;
        use std::rc::Rc;

        // Create RefCell wrapper for the expression to enable mutability
        let expr_cell = Rc::new(RefCell::new(let_binding.expression.clone()));
        let value_result = {
            let mut expr_mut = expr_cell.borrow_mut();
            self.infer_expression(&mut expr_mut, context)?
        };

        // Extract variable name from pattern
        match &let_binding.pattern {
            outrun_parser::Pattern::Identifier(identifier) => {
                let var_name = identifier.name.clone();

                // Add the variable to the symbol table
                self.symbol_table
                    .insert(var_name, value_result.inferred_type);
            }
            _ => {
                // TODO: Handle more complex patterns (tuples, structs, etc.)
                // For now, just skip complex patterns
            }
        }

        Ok(())
    }

    /// Type check guard clause without requiring mutability
    #[allow(clippy::result_large_err)]
    fn typecheck_guard_clause_readonly(
        &mut self,
        guard: &outrun_parser::GuardClause,
        context: &mut InferenceContext,
    ) -> Result<(), TypecheckError> {
        use std::cell::RefCell;
        use std::rc::Rc;

        // Create RefCell wrapper for the expression to enable mutability
        let expr_cell = Rc::new(RefCell::new(guard.condition.clone()));
        let guard_result = {
            let mut expr_mut = expr_cell.borrow_mut();
            self.infer_expression(&mut expr_mut, context)?
        };

        let boolean_type = Type::concrete("Outrun.Core.Boolean");

        if !self.types_are_compatible(&guard_result.inferred_type, &boolean_type) {
            return Err(TypecheckError::UnificationError(
                OriginalUnificationError::TypeMismatch {
                    expected: boolean_type,
                    found: guard_result.inferred_type,
                    expected_context: Some("Guard clause must return Boolean".to_string()),
                    found_context: Some("guard expression".to_string()),
                    span: to_source_span(Some(guard.condition.span)),
                },
            ));
        }

        Ok(())
    }

    /// Check if two types are compatible for assignment/unification
    /// This includes protocol compatibility and type variable resolution
    pub fn types_are_compatible(&self, found_type: &Type, expected_type: &Type) -> bool {
        use crate::types::Type;

        match (found_type, expected_type) {
            // Same concrete types are compatible
            (
                Type::Concrete {
                    name: id1,
                    args: args1,
                    ..
                },
                Type::Concrete {
                    name: id2,
                    args: args2,
                    ..
                },
            ) => {
                if !self.type_ids_are_equivalent(id1, id2) || args1.len() != args2.len() {
                    return false;
                }

                // Use unification to check if type arguments are compatible
                let mut unifier = crate::unification::Unifier::new();
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    if unifier.unify(arg1, arg2).is_err() {
                        return false;
                    }
                }
                true
            }

            // Protocol types: check if found type can implement expected protocol
            (
                Type::Concrete {
                    name: concrete_type,
                    ..
                },
                Type::Protocol { name: protocol, .. },
            ) => {
                // Check if the concrete type implements the protocol (with all requirements)
                self.type_satisfies_protocol(concrete_type, protocol)
            }

            // Protocol satisfied by concrete type: check if concrete type implements the protocol
            (
                Type::Protocol { name: protocol, .. },
                Type::Concrete {
                    name: concrete_type,
                    ..
                },
            ) => {
                // Check if the concrete type implements the protocol (reverse direction)
                self.type_satisfies_protocol(concrete_type, protocol)
            }

            // Two protocol types: check if they're the same protocol with compatible arguments
            (
                Type::Protocol {
                    name: id1,
                    args: args1,
                    ..
                },
                Type::Protocol {
                    name: id2,
                    args: args2,
                    ..
                },
            ) => {
                // Same protocol with same number of arguments
                if id1 == id2 && args1.len() == args2.len() {
                    // Check if all type arguments are compatible (allowing subtyping)
                    args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(arg1, arg2)| self.types_are_compatible(arg1, arg2))
                } else {
                    false
                }
            }

            // Type variables are compatible with anything (will be resolved during unification)
            (Type::Variable { .. }, _) | (_, Type::Variable { .. }) => true,

            // Self types need special handling based on context
            (Type::SelfType { .. }, other) => {
                // Resolve Self to its concrete type and check compatibility
                if let Some(resolved_self) = found_type.resolve_self() {
                    self.types_are_compatible(&resolved_self, other)
                } else {
                    // Self is unbound (e.g., in protocol definition context)
                    false
                }
            }
            (other, Type::SelfType { .. }) => {
                // Resolve Self to its concrete type and check compatibility
                if let Some(resolved_self) = expected_type.resolve_self() {
                    self.types_are_compatible(other, &resolved_self)
                } else {
                    // Self is unbound (e.g., in protocol definition context)
                    false
                }
            }

            // Function types must match exactly
            (
                Type::Function {
                    params: p1,
                    return_type: r1,
                    ..
                },
                Type::Function {
                    params: p2,
                    return_type: r2,
                    ..
                },
            ) => {
                p1.len() == p2.len()
                    && p1
                        .iter()
                        .zip(p2.iter())
                        .all(|((n1, t1), (n2, t2))| n1 == n2 && self.types_are_compatible(t1, t2))
                    && self.types_are_compatible(r1, r2)
            }

            // Tuple types must have same arity and compatible element types
            (
                Type::Tuple {
                    element_types: e1, ..
                },
                Type::Tuple {
                    element_types: e2, ..
                },
            ) => {
                e1.len() == e2.len()
                    && e1
                        .iter()
                        .zip(e2.iter())
                        .all(|(t1, t2)| self.types_are_compatible(t1, t2))
            }

            // Different categories are incompatible
            _ => false,
        }
    }

    /// Resolve simple function calls to clause lists
    fn resolve_simple_function_clauses(
        &mut self,
        function_name: &str,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<crate::typed_ast::UniversalCallResolution, TypecheckError> {
        // Use existing dispatcher logic but convert to universal result
        let function_context = self.create_function_context_from_inference_context(context);
        let dispatcher =
            FunctionDispatcher::new(&self.type_registry, &self.function_registry, None, None)
                .with_context(function_context);

        match dispatcher.resolve_local_call(function_name, Some(function_call.span)) {
            Ok(crate::dispatch::DispatchResult::Resolved(resolved_func)) => {
                // Single resolved function - create deterministic clause
                let clause_id = self.create_clause_id_for_function(&resolved_func);

                // CRITICAL FIX: Apply generic parameter substitution for local function calls
                // This resolves the issue where local functions returning Option<T> don't get
                // their generic parameters substituted (e.g., Option<T> -> Option<Integer>)
                let return_type = self.substitute_local_function_generics(
                    &resolved_func.function_info.return_type,
                    function_name,
                    &resolved_func.function_info.parameters,
                    function_call,
                    context,
                )?;

                // Register this clause in the universal dispatch registry
                self.register_function_clause(clause_id, &resolved_func);

                Ok(crate::typed_ast::UniversalCallResolution::single(
                    clause_id,
                    return_type,
                ))
            }
            Ok(crate::dispatch::DispatchResult::Ambiguous(candidates)) => {
                // Multiple candidates - create multiple clauses
                let mut clause_ids = Vec::new();
                let return_type = if !candidates.is_empty() {
                    candidates[0].function_info.return_type.clone()
                } else {
                    Type::variable(self.fresh_type_var(), Level(0))
                };

                for candidate in candidates {
                    let clause_id = self.create_clause_id_for_function(&candidate);
                    self.register_function_clause(clause_id, &candidate);
                    clause_ids.push(clause_id);
                }

                Ok(crate::typed_ast::UniversalCallResolution::multi(
                    clause_ids,
                    return_type,
                ))
            }
            Ok(crate::dispatch::DispatchResult::NotFound) => {
                // Function not found - create error clause
                return Err(TypecheckError::DispatchError(
                    crate::error::DispatchError::NoImplementation {
                        protocol_name: "unknown".to_string(),
                        type_name: function_name.to_string(),
                        file_span: Some(self.create_file_span(Some(function_call.span))),
                        similar_implementations: Vec::new(),
                        suggestions: Vec::new(),
                    },
                ));
            }
            Err(dispatch_error) => Err(TypecheckError::DispatchError(dispatch_error)),
        }
    }

    /// Resolve qualified function calls to clause lists with precomputed argument types
    fn resolve_qualified_function_clauses_with_precomputed_args(
        &mut self,
        module_name: &str,
        function_name: &str,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
        precomputed_arg_types: &[Option<Type>],
    ) -> Result<crate::typed_ast::UniversalCallResolution, TypecheckError> {
        let qualified_name = format!("{}.{}", module_name, function_name);
        let protocol_id = crate::types::ModuleName::new(module_name);

        // Check if this is a protocol call

        let target_type = if self.type_registry.has_protocol(&protocol_id) {
            let resolved_target = self.resolve_protocol_call_target_type(
                &qualified_name,
                function_call,
                context,
                Some(precomputed_arg_types),
            )?;
            // If we resolved a SelfType to a concrete type through constraint analysis,
            // we need to substitute it before passing to the dispatcher
            resolved_target.map(|t| {
                if t.is_self_type() {
                    // This is still a SelfType - the constraint system couldn't resolve it
                    t
                } else {
                    // This is the concrete type that Self resolved to
                    t
                }
            })
        } else {
            None
        };

        let dispatcher =
            FunctionDispatcher::new(&self.type_registry, &self.function_registry, None, None);

        let result = match dispatcher.resolve_qualified_call(
            &qualified_name,
            target_type.as_ref(),
            Some(function_call.span),
        ) {
            Ok(crate::dispatch::DispatchResult::Resolved(resolved_func)) => {
                let clause_id = self.create_clause_id_for_function(&resolved_func);

                // CRITICAL FIX: Substitute generic parameters in return type for protocol calls
                let return_type = if self.type_registry.has_protocol(&protocol_id) {
                    self.substitute_protocol_generics_in_return_type(
                        &resolved_func.function_info.return_type,
                        &qualified_name,
                        target_type.as_ref(),
                        precomputed_arg_types,
                        function_call,
                        context,
                    )?
                } else {
                    resolved_func.function_info.return_type.clone()
                };

                self.register_function_clause(clause_id, &resolved_func);

                Ok(crate::typed_ast::UniversalCallResolution::single(
                    clause_id,
                    return_type,
                ))
            }
            Ok(crate::dispatch::DispatchResult::Ambiguous(candidates)) => {
                let mut clause_ids = Vec::new();
                let return_type = if !candidates.is_empty() {
                    candidates[0].function_info.return_type.clone()
                } else {
                    Type::variable(self.fresh_type_var(), Level(0))
                };

                for candidate in candidates {
                    let clause_id = self.create_clause_id_for_function(&candidate);
                    self.register_function_clause(clause_id, &candidate);
                    clause_ids.push(clause_id);
                }

                Ok(crate::typed_ast::UniversalCallResolution::multi(
                    clause_ids,
                    return_type,
                ))
            }
            Ok(crate::dispatch::DispatchResult::NotFound) => {
                // Check for lazy intrinsic registration
                if module_name == "Outrun.Intrinsic" {
                    match self.try_lazy_intrinsic_registration(
                        function_name,
                        &function_call.arguments,
                        function_call.span,
                        context,
                    ) {
                        Ok(result) => {
                            // Convert InferenceResult to UniversalCallResolution
                            let signature = crate::universal_dispatch::FunctionSignature::new(
                                vec![module_name.to_string()],
                                function_name.to_string(),
                            );
                            let clause_id =
                                crate::universal_dispatch::ClauseId::deterministic(&signature, &[]);
                            return Ok(crate::typed_ast::UniversalCallResolution::single(
                                clause_id,
                                result.inferred_type,
                            ));
                        }
                        Err(_) => {
                            // Fall through to NotFound error
                        }
                    }
                }

                Err(TypecheckError::InferenceError(
                    crate::error::InferenceError::AmbiguousType {
                        span: crate::error::to_source_span(Some(function_call.span)),
                        suggestions: vec![format!("Unknown function: {}", qualified_name)],
                    },
                ))
            }
            Err(dispatch_error) => Err(TypecheckError::DispatchError(dispatch_error)),
        };

        result
    }

    /// Create a deterministic clause ID for a resolved function
    fn create_clause_id_for_function(
        &self,
        resolved_func: &crate::dispatch::ResolvedFunction,
    ) -> crate::universal_dispatch::ClauseId {
        use crate::universal_dispatch::*;

        let signature = FunctionSignature::new(
            vec![resolved_func.function_info.defining_scope.clone()],
            resolved_func.function_info.function_name.clone(),
        );

        let mut guards = Vec::new();
        if let Some(implementing_type) = &resolved_func.implementing_type {
            guards.push(Guard::TypeCompatible {
                target_type: Type::concrete(implementing_type.as_str()),
                implementing_type: Type::concrete(implementing_type.as_str()),
                constraint_context: ConstraintContext::new(),
            });
        }
        guards.push(Guard::AlwaysTrue);

        // Check if this clause already exists in the registry
        let existing_clauses = self
            .universal_dispatch_registry
            .get_clauses_for_function(&signature);

        if !existing_clauses.is_empty() {
            // Check if any existing clause matches our guards
            for &clause_id in existing_clauses {
                if let Some(clause_info) = self.universal_dispatch_registry.get_clause(clause_id) {
                    eprintln!(
                        "  Existing clause {}: guards={:?}",
                        clause_id.0, clause_info.guards
                    );
                    // For now, just use the first one
                    // TODO: Match guards properly
                    return clause_id;
                }
            }
        }

        // No existing clause found, create a new deterministic one
        ClauseId::deterministic(&signature, &guards)
    }

    /// Register a function clause in the universal dispatch registry
    fn register_function_clause(
        &mut self,
        clause_id: crate::universal_dispatch::ClauseId,
        resolved_func: &crate::dispatch::ResolvedFunction,
    ) {
        use crate::universal_dispatch::*;

        // Create function signature from resolved function
        let signature = FunctionSignature::new(
            vec![resolved_func.function_info.defining_scope.clone()],
            resolved_func.function_info.function_name.clone(),
        );

        // Create guards based on function type
        let mut guards = Vec::new();

        // Add type compatibility guard if there's an implementing type
        if let Some(implementing_type) = &resolved_func.implementing_type {
            guards.push(Guard::TypeCompatible {
                target_type: Type::concrete(implementing_type.as_str()),
                implementing_type: Type::concrete(implementing_type.as_str()),
                constraint_context: ConstraintContext::new(),
            });
        }

        // Add always-true guard as fallback
        guards.push(Guard::AlwaysTrue);

        // Create function body based on resolved function
        let body = if let Some(block) = &resolved_func.function_info.body {
            FunctionBody::UserFunction(block.clone())
        } else if resolved_func
            .qualified_name
            .starts_with("Outrun.Intrinsic.")
        {
            // Only treat as intrinsic if it's actually in the Outrun.Intrinsic namespace
            FunctionBody::IntrinsicFunction(resolved_func.qualified_name.clone())
        } else {
            // For protocol functions without bodies, don't register universal clauses
            // These should be resolved through the legacy dispatch system to concrete implementations
            return; // Don't register a universal clause for abstract protocol functions
        };

        // Create clause info
        let clause_info = ClauseInfo {
            clause_id,
            function_signature: signature,
            guards,
            body,
            estimated_cost: 1,
            priority: 0,
            span: resolved_func.function_info.span,
        };

        // Register the clause
        self.universal_dispatch_registry
            .register_clause(clause_info);
    }

    /// Infer the type of a function call using universal dispatch with precomputed argument types
    /// Uses iterative system - argument types come from dependency task results
    #[allow(clippy::result_large_err)]
    fn infer_function_call_with_precomputed_args(
        &mut self,
        function_call: &mut outrun_parser::FunctionCall,
        context: &mut InferenceContext,
        precomputed_arg_types: &[Option<Type>],
    ) -> Result<InferenceResult, TypecheckError> {
        // Phase 2: Universal Interpreter Simplification
        // ALL function calls now go through the universal dispatch system

        // Use precomputed argument types from task results to avoid recursion
        let universal_resolution = self.resolve_universal_function_call_with_precomputed_args(
            function_call,
            context,
            precomputed_arg_types,
        )?;

        // Set the resolved function key for interpreter dispatch (LEGACY)
        // For now, use the first clause ID as the key (will be enhanced in Phase 3)
        if let Some(first_clause) = universal_resolution.possible_clauses.first() {
            function_call.resolved_function_key = Some(format!("clause_{}", first_clause.0));
        }

        // Set the universal clause IDs for universal dispatch system
        function_call.universal_clause_ids = Some(
            universal_resolution
                .possible_clauses
                .iter()
                .map(|clause_id| clause_id.0)
                .collect(),
        );

        Ok(InferenceResult {
            inferred_type: universal_resolution.return_type,
            constraints: context.constraints.clone(),
            substitution: context.substitution.clone(),
        })
    }

    /// Universal function call resolution - converts any function call into clause lists
    /// Uses iterative system to avoid recursive argument processing  
    fn resolve_universal_function_call_with_precomputed_args(
        &mut self,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
        precomputed_arg_types: &[Option<Type>],
    ) -> Result<crate::typed_ast::UniversalCallResolution, TypecheckError> {
        // Get initial clause resolution
        let initial_resolution = match &function_call.path {
            outrun_parser::FunctionPath::Simple { name } => {
                // Simple function call - resolve to local context clauses
                self.resolve_simple_function_clauses(&name.name, function_call, context)
            }
            outrun_parser::FunctionPath::Qualified { module, name } => {
                // Qualified function call - resolve to module/protocol clauses
                let _qualified_name = format!("{}.{}", module.name, name.name);
                let module_name = &module.name;
                // Check if this is a protocol call - pass precomputed args to avoid recursion!

                self.resolve_qualified_function_clauses_with_precomputed_args(
                    module_name,
                    &name.name,
                    function_call,
                    context,
                    precomputed_arg_types,
                )
            }
            outrun_parser::FunctionPath::Expression { .. } => {
                // Dynamic function call - not yet supported
                return Err(TypecheckError::InferenceError(
                    crate::error::InferenceError::AmbiguousType {
                        span: crate::error::to_source_span(Some(function_call.span)),
                        suggestions: vec!["Dynamic function calls not yet supported".to_string()],
                    },
                ));
            }
        }?;

        Ok(initial_resolution)
    }

    /// Resolve target type for protocol calls using comprehensive Self/generic analysis
    #[allow(clippy::result_large_err)]
    fn resolve_protocol_call_target_type(
        &mut self,
        qualified_name: &str,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
        precomputed_arg_types: Option<&[Option<Type>]>,
    ) -> Result<Option<Type>, TypecheckError> {
        // Look up the function signature for analysis
        let (module_name, function_name) = if let Some(dot_pos) = qualified_name.rfind('.') {
            (&qualified_name[..dot_pos], &qualified_name[dot_pos + 1..])
        } else {
            return Ok(None);
        };

        // Get function info from registry and clone the data we need
        let (function_parameters, function_return_type) = if let Some(info) = self
            .function_registry
            .get_function(module_name, function_name)
        {
            (info.parameters.clone(), info.return_type.clone())
        } else {
            return Ok(None); // Function not found, let dispatcher handle the error
        };

        // Use Function Signature Analyzer to analyze the signature
        let signature_analysis = FunctionSignatureAnalyzer::analyze_signature(
            &function_parameters,
            &function_return_type,
        );

        // If there are no Self positions, this is not a protocol call requiring Self inference
        if signature_analysis.self_positions.is_empty() {
            return Ok(None);
        }

        // Get argument types - use precomputed if available, otherwise infer recursively
        let inferred_argument_types = if let Some(precomputed) = precomputed_arg_types {
            precomputed.to_vec()
        } else {
            let mut types = Vec::new();
            for argument in &function_call.arguments {
                match argument {
                    outrun_parser::Argument::Named { expression, .. } => {
                        let mut expr_clone = expression.clone();
                        match self.infer_expression(&mut expr_clone, context) {
                            Ok(result) => types.push(Some(result.inferred_type)),
                            Err(_) => types.push(None),
                        }
                    }
                    _ => types.push(None),
                }
            }
            types
        };

        // Extract Self type from argument names (not positions!)
        let self_type = self.extract_self_from_argument_names(
            &signature_analysis.self_positions,
            &function_parameters,
            function_call,
            context,
        )?;

        // Validate type variable unification constraints
        if let Some(ref self_type) = self_type {
            self.validate_type_variable_unification(
                &signature_analysis.unification_constraints,
                self_type,
                &function_parameters,
                &inferred_argument_types,
            )?;
        }

        Ok(self_type)
    }

    /// Substitute generic parameters in protocol function return types
    /// Essential for correct type inference of protocol calls like Option.unwrap
    #[allow(clippy::result_large_err)]
    fn substitute_protocol_generics_in_return_type(
        &mut self,
        return_type: &Type,
        qualified_name: &str,
        _target_type: Option<&Type>,
        precomputed_arg_types: &[Option<Type>],
        _function_call: &outrun_parser::FunctionCall,
        _context: &mut InferenceContext,
    ) -> Result<Type, TypecheckError> {
        // Get the protocol definition to understand generic parameters
        let (module_name, function_name) = if let Some(dot_pos) = qualified_name.rfind('.') {
            (&qualified_name[..dot_pos], &qualified_name[dot_pos + 1..])
        } else {
            return Ok(return_type.clone()); // No module, can't be protocol
        };

        // Get function info to understand parameter types and generic parameters
        let function_info = if let Some(info) = self
            .function_registry
            .get_function(module_name, function_name)
        {
            info
        } else {
            return Ok(return_type.clone()); // Function not found
        };

        // If the function has no generic parameters, no substitution needed
        if function_info.generic_parameters.is_empty() {
            return Ok(return_type.clone());
        }

        // Build a substitution map from generic parameters to concrete types
        let mut substitution_map = std::collections::HashMap::new();

        // Look for Self-typed parameters to extract generic type arguments
        for (i, (_param_name, param_type)) in function_info.parameters.iter().enumerate() {
            // Check if this parameter has type Self (the protocol type)
            if matches!(param_type, Type::SelfType { .. }) {
                // Get the actual argument type
                if let Some(Some(
                    Type::Protocol { name, args, .. } | Type::Concrete { name, args, .. },
                )) = precomputed_arg_types.get(i)
                {
                    // Check if this is the protocol we're working with
                    if name.as_str() == module_name {
                        // Map generic parameters to their concrete types
                        for (j, generic_param) in
                            function_info.generic_parameters.iter().enumerate()
                        {
                            if let Some(concrete_type) = args.get(j) {
                                substitution_map
                                    .insert(generic_param.clone(), concrete_type.clone());
                            }
                        }
                    }
                }
            }
        }

        // If we found substitutions, apply them to the return type
        if !substitution_map.is_empty() {
            let mut result = return_type.clone();
            for (generic_param, concrete_type) in substitution_map.iter() {
                result = self.substitute_generic_parameter_in_type(
                    &result,
                    generic_param,
                    concrete_type,
                )?;
            }
            return Ok(result);
        }

        // No substitution needed or possible
        Ok(return_type.clone())
    }

    /// Helper method to substitute a specific generic parameter in a type
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_generic_parameter_in_type(
        &self,
        target_type: &Type,
        param_name: &str,
        replacement: &Type,
    ) -> Result<Type, TypecheckError> {
        match target_type {
            Type::Protocol { name, args, span } => {
                // Check if this protocol ID matches the parameter name
                if name.as_str() == param_name {
                    return Ok(replacement.clone());
                }

                // Recursively substitute in arguments
                let substituted_args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| {
                        self.substitute_generic_parameter_in_type(arg, param_name, replacement)
                    })
                    .collect();

                Ok(Type::Protocol {
                    name: name.clone(),
                    args: substituted_args?,
                    span: *span,
                })
            }
            Type::Concrete { name, args, span } => {
                // Recursively substitute in generic arguments
                let substituted_args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| {
                        self.substitute_generic_parameter_in_type(arg, param_name, replacement)
                    })
                    .collect();

                Ok(Type::Concrete {
                    name: name.clone(),
                    args: substituted_args?,
                    span: *span,
                })
            }
            Type::Variable { .. } => {
                // Type variables don't need substitution (they're different from protocol parameters)
                Ok(target_type.clone())
            }
            Type::SelfType { .. } => {
                // Self types are handled differently
                Ok(target_type.clone())
            }
            Type::Function { .. } => {
                // Function types don't have generic parameters to substitute
                Ok(target_type.clone())
            }
            Type::Tuple {
                element_types,
                span,
            } => {
                // Recursively substitute in element types
                let substituted_elements: Result<Vec<_>, _> = element_types
                    .iter()
                    .map(|elem| {
                        self.substitute_generic_parameter_in_type(elem, param_name, replacement)
                    })
                    .collect();

                Ok(Type::Tuple {
                    element_types: substituted_elements?,
                    span: *span,
                })
            }
        }
    }

    /// Substitute generic parameters in local function return types
    /// This fixes the issue where local functions returning generic types (like Option<T>)
    /// don't get their generic parameters properly resolved based on argument types
    #[allow(clippy::result_large_err)]
    fn substitute_local_function_generics(
        &mut self,
        return_type: &Type,
        _function_name: &str,
        function_parameters: &[(String, Type)],
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<Type, TypecheckError> {
        // Get the argument types by inferring them from the function call
        let mut argument_types = Vec::new();
        for argument in &function_call.arguments {
            // Infer the type of each argument based on the argument variant
            let expression = match argument {
                outrun_parser::Argument::Named { expression, .. } => expression,
                outrun_parser::Argument::Spread { expression, .. } => expression,
            };

            // Create a mutable copy for inference
            let mut expr_copy = expression.clone();
            let arg_result = self.infer_expression(&mut expr_copy, context)?;
            argument_types.push(arg_result.inferred_type);
        }

        // Build a map of generic parameter substitutions
        let mut substitutions = std::collections::HashMap::new();

        // Match parameter types with argument types to find generic substitutions
        for (i, (_param_name, param_type)) in function_parameters.iter().enumerate() {
            if let Some(arg_type) = argument_types.get(i) {
                // Find generic parameter substitutions by matching param_type with arg_type
                self.extract_generic_substitutions(param_type, arg_type, &mut substitutions)?;
            }
        }

        // Apply all substitutions to the return type
        let mut result_type = return_type.clone();
        for (generic_param, concrete_type) in substitutions {
            result_type = self.substitute_generic_parameter_in_type(
                &result_type,
                &generic_param,
                &concrete_type,
            )?;
        }

        Ok(result_type)
    }

    /// Extract generic parameter substitutions by matching a parameter type with an argument type
    /// For example, matching Option<T> with Option<Integer> would extract T -> Integer
    #[allow(clippy::result_large_err)]
    #[allow(clippy::only_used_in_recursion)]
    fn extract_generic_substitutions(
        &self,
        param_type: &Type,
        arg_type: &Type,
        substitutions: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), TypecheckError> {
        match (param_type, arg_type) {
            // Match Protocol<T> with Protocol<ConcreteType>
            (
                Type::Protocol {
                    name: param_id,
                    args: param_args,
                    ..
                },
                Type::Protocol {
                    name: arg_id,
                    args: arg_args,
                    ..
                },
            ) => {
                if param_id.as_str() == arg_id.as_str() && param_args.len() == arg_args.len() {
                    // Recursively match generic arguments
                    for (param_arg, arg_arg) in param_args.iter().zip(arg_args.iter()) {
                        self.extract_generic_substitutions(param_arg, arg_arg, substitutions)?;
                    }
                }
            }

            // Match Concrete<T> with Concrete<ConcreteType>
            (
                Type::Concrete {
                    name: param_id,
                    args: param_args,
                    ..
                },
                Type::Concrete {
                    name: arg_id,
                    args: arg_args,
                    ..
                },
            ) => {
                if param_id.as_str() == arg_id.as_str() && param_args.len() == arg_args.len() {
                    // Recursively match generic arguments
                    for (param_arg, arg_arg) in param_args.iter().zip(arg_args.iter()) {
                        self.extract_generic_substitutions(param_arg, arg_arg, substitutions)?;
                    }
                }
            }

            // Match generic parameter T with concrete type
            (
                Type::Protocol {
                    name: param_id,
                    args,
                    ..
                },
                concrete_type,
            ) if args.is_empty() => {
                // This is a generic parameter (like T) - record the substitution
                substitutions.insert(param_id.as_str().to_string(), concrete_type.clone());
            }

            // Other cases don't contribute to generic substitutions
            _ => {}
        }

        Ok(())
    }

    /// Extract Self type from argument names (keyword arguments)
    #[allow(clippy::result_large_err)]
    fn extract_self_from_argument_names(
        &mut self,
        self_positions: &[crate::types::SelfPosition],
        _function_parameters: &[(String, Type)],
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<Option<Type>, TypecheckError> {
        let mut candidate_self_types = Vec::new();

        // For each Self position in the function signature
        for self_position in self_positions {
            // Find which parameter contains Self
            if let Some(param_name) = self.extract_parameter_name_from_position(&self_position.path)
            {
                // Find the corresponding argument by name in the function call
                if let Some(argument_expr) = self.find_argument_by_name(function_call, &param_name)
                {
                    // Infer the type of this argument
                    let mut expr_clone = argument_expr.clone();
                    match self.infer_expression(&mut expr_clone, context) {
                        Ok(result) => {
                            // This is a candidate for the Self type
                            candidate_self_types.push(result.inferred_type);
                        }
                        Err(_e) => {}
                    }
                }
            }
        }

        // Unify all candidate Self types to ensure consistency

        if candidate_self_types.is_empty() {
            Ok(None)
        } else if candidate_self_types.len() == 1 {
            let result = candidate_self_types.into_iter().next().unwrap();
            Ok(Some(result))
        } else {
            // Multiple Self positions must unify to the same type
            let first_type = &candidate_self_types[0];
            for candidate in &candidate_self_types[1..] {
                if !self.types_are_compatible(first_type, candidate) {
                    return Err(TypecheckError::InferenceError(
                        InferenceError::SelfTypeUnificationFailure {
                            first_self_type: first_type.clone(),
                            conflicting_self_type: candidate.clone(),
                            span: None, // TODO: Add span information
                        },
                    ));
                }
            }
            Ok(Some(first_type.clone()))
        }
    }

    /// Extract parameter name from Self position path for keyword argument matching
    fn extract_parameter_name_from_position(&self, path: &[String]) -> Option<String> {
        // Look for parameter name in the path using new parameter-name-based format
        if let Some(first_segment) = path.first() {
            if first_segment.starts_with("param_") && first_segment.len() > 6 {
                // Extract the parameter name from "param_<name>" format
                return Some(first_segment[6..].to_string());
            }
            // Fall back to old "arg_<index>" format (should be rare now)
            if first_segment.starts_with("arg_") {
                // This is positional format - we can't extract a meaningful name
                return None;
            }
        }
        None
    }

    /// Find argument expression by parameter name in function call
    fn find_argument_by_name(
        &self,
        function_call: &outrun_parser::FunctionCall,
        param_name: &str,
    ) -> Option<outrun_parser::Expression> {
        for argument in &function_call.arguments {
            match argument {
                outrun_parser::Argument::Named {
                    name, expression, ..
                } => {
                    if name.name == param_name {
                        return Some(expression.clone());
                    }
                }
                _ => continue, // Skip spread arguments for now
            }
        }
        None
    }

    /// Validate that type variable unification constraints are satisfied
    #[allow(clippy::result_large_err)]
    fn validate_type_variable_unification(
        &self,
        unification_constraints: &[Constraint],
        self_type: &Type,
        _function_parameters: &[(String, Type)],
        _inferred_argument_types: &[Option<Type>],
    ) -> Result<(), TypecheckError> {
        // For each equality constraint, verify that the types can be unified
        for constraint in unification_constraints {
            match constraint {
                Constraint::Equality { left, right, .. } => {
                    // Apply Self substitution if needed
                    let left_resolved = if left.is_self_type() {
                        self_type.clone()
                    } else {
                        left.as_ref().clone()
                    };

                    let right_resolved = if right.is_self_type() {
                        self_type.clone()
                    } else {
                        right.as_ref().clone()
                    };

                    // Check if the types are compatible
                    if !self.types_are_compatible(&left_resolved, &right_resolved) {
                        return Err(TypecheckError::InferenceError(
                            InferenceError::TypeVariableUnificationFailure {
                                variable_name: format!("{} = {}", left_resolved, right_resolved),
                                first_type: left_resolved,
                                conflicting_type: right_resolved,
                                span: None, // TODO: Add span information
                            },
                        ));
                    }
                }
                _ => {
                    // Other constraint types don't affect unification directly
                }
            }
        }

        Ok(())
    }

    /// Try to lazily register an intrinsic function based on common patterns
    #[allow(clippy::result_large_err)]
    fn try_lazy_intrinsic_registration(
        &mut self,
        intrinsic_name: &str,
        arguments: &[outrun_parser::Argument],
        call_span: outrun_parser::Span,
        _context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // For now, we'll register common intrinsics with generic Any types
        // This is a simplified approach to avoid the mutability issues
        // A future enhancement could use a two-phase approach

        let (return_type, parameter_types) = match intrinsic_name {
            // Map operations with Any types (simplified)
            "map_get" if arguments.len() == 2 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                let option_any_type =
                    Type::protocol_with_args_and_span("Option", vec![any_type.clone()], call_span);
                (
                    option_any_type,
                    vec![
                        ("map".to_string(), any_type.clone()),
                        ("key".to_string(), any_type.clone()),
                    ],
                )
            }

            // List operations with Any types (simplified)
            "list_head" if arguments.len() == 1 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                let option_any_type =
                    Type::protocol_with_args_and_span("Option", vec![any_type.clone()], call_span);
                (
                    option_any_type,
                    vec![("value".to_string(), any_type.clone())],
                )
            }

            "list_prepend" if arguments.len() == 2 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                (
                    any_type.clone(), // Returns same list type (Any for now)
                    vec![
                        ("list".to_string(), any_type.clone()),
                        ("elem".to_string(), any_type.clone()),
                    ],
                )
            }

            "list_length" if arguments.len() == 1 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                (integer_type, vec![("value".to_string(), any_type)])
            }

            "list_empty" if arguments.is_empty() => {
                let any_type = Type::protocol_with_span("Any", call_span);
                (any_type, vec![])
            }

            // String operations
            "string_length" if arguments.len() == 1 => {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                (integer_type, vec![("value".to_string(), string_type)])
            }

            "string_char_at" if arguments.len() == 2 => {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                let option_string_type = Type::protocol_with_args_and_span(
                    "Option",
                    vec![string_type.clone()],
                    call_span,
                );
                (
                    option_string_type,
                    vec![
                        ("value".to_string(), string_type),
                        ("index".to_string(), integer_type),
                    ],
                )
            }

            "string_slice" if arguments.len() == 3 => {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                (
                    string_type.clone(),
                    vec![
                        ("value".to_string(), string_type.clone()),
                        ("start".to_string(), integer_type.clone()),
                        ("end".to_string(), integer_type),
                    ],
                )
            }

            "string_concat" if arguments.len() == 2 => {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                (
                    string_type.clone(),
                    vec![
                        ("lhs".to_string(), string_type.clone()),
                        ("rhs".to_string(), string_type.clone()),
                    ],
                )
            }

            "string_index_of" if arguments.len() == 2 => {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                let option_integer_type =
                    Type::protocol_with_args_and_span("Option", vec![integer_type], call_span);
                (
                    option_integer_type,
                    vec![
                        ("value".to_string(), string_type.clone()),
                        ("search".to_string(), string_type),
                    ],
                )
            }

            "string_to_upper" | "string_to_lower" | "string_trim" | "string_trim_start"
            | "string_trim_end"
                if arguments.len() == 1 =>
            {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                (
                    string_type.clone(),
                    vec![("value".to_string(), string_type.clone())],
                )
            }

            "string_valid_utf8" if arguments.len() == 1 => {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                let boolean_type = Type::protocol_with_span("Boolean", call_span);
                (boolean_type, vec![("value".to_string(), string_type)])
            }

            // Binary operations
            "binary_byte_size" if arguments.len() == 1 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                (integer_type, vec![("value".to_string(), any_type)])
            }

            "binary_byte_at" if arguments.len() == 2 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                let option_integer_type = Type::protocol_with_args_and_span(
                    "Option",
                    vec![integer_type.clone()],
                    call_span,
                );
                (
                    option_integer_type,
                    vec![
                        ("value".to_string(), any_type),
                        ("index".to_string(), integer_type),
                    ],
                )
            }

            "binary_slice" if arguments.len() == 3 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                (
                    any_type.clone(),
                    vec![
                        ("value".to_string(), any_type.clone()),
                        ("start".to_string(), integer_type.clone()),
                        ("end".to_string(), integer_type),
                    ],
                )
            }

            "binary_concat" if arguments.len() == 2 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                (
                    any_type.clone(),
                    vec![
                        ("lhs".to_string(), any_type.clone()),
                        ("rhs".to_string(), any_type.clone()),
                    ],
                )
            }

            "binary_index_of" if arguments.len() == 2 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                let integer_type = Type::protocol_with_span("Integer", call_span);
                let option_integer_type =
                    Type::protocol_with_args_and_span("Option", vec![integer_type], call_span);
                (
                    option_integer_type,
                    vec![
                        ("value".to_string(), any_type.clone()),
                        ("search".to_string(), any_type),
                    ],
                )
            }

            "binary_to_hex" if arguments.len() == 1 => {
                let any_type = Type::protocol_with_span("Any", call_span);
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                (string_type, vec![("value".to_string(), any_type)])
            }

            "binary_from_hex" if arguments.len() == 1 => {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                let any_type = Type::protocol_with_span("Any", call_span);
                let option_any_type =
                    Type::protocol_with_args_and_span("Option", vec![any_type], call_span);
                (option_any_type, vec![("value".to_string(), string_type)])
            }

            // Boolean operations
            "bool_and" if arguments.len() == 2 => {
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type.clone(),
                    vec![
                        ("left".to_string(), boolean_type.clone()),
                        ("right".to_string(), boolean_type.clone()),
                    ],
                )
            }

            "bool_or" if arguments.len() == 2 => {
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type.clone(),
                    vec![
                        ("left".to_string(), boolean_type.clone()),
                        ("right".to_string(), boolean_type.clone()),
                    ],
                )
            }

            "bool_not" if arguments.len() == 1 => {
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type.clone(),
                    vec![("value".to_string(), boolean_type.clone())],
                )
            }

            // Integer64 arithmetic intrinsics
            "i64_add" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_sub" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_mul" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_div" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                let option_type = Type::generic_concrete("Option", vec![integer64_type.clone()]);
                (
                    option_type,
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_mod" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                let option_type = Type::generic_concrete("Option", vec![integer64_type.clone()]);
                (
                    option_type,
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_pow" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            // Integer64 unary intrinsics
            "i64_pos" if arguments.len() == 1 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![("value".to_string(), integer64_type.clone())],
                )
            }

            "i64_neg" if arguments.len() == 1 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![("value".to_string(), integer64_type.clone())],
                )
            }

            "i64_abs" if arguments.len() == 1 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![("value".to_string(), integer64_type.clone())],
                )
            }

            // Integer64 comparison intrinsics
            "i64_eq" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_gt" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_ge" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_lt" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_le" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            // Integer64 bitwise intrinsics
            "i64_and" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_or" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_xor" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_not" if arguments.len() == 1 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![("value".to_string(), integer64_type.clone())],
                )
            }

            "i64_shl" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            "i64_shr" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                (
                    integer64_type.clone(),
                    vec![
                        ("lhs".to_string(), integer64_type.clone()),
                        ("rhs".to_string(), integer64_type.clone()),
                    ],
                )
            }

            // Integer64 string conversion intrinsics
            "i64_to_string_radix" if arguments.len() == 2 => {
                let integer64_type = Type::concrete_with_span("Outrun.Core.Integer64", call_span);
                let integer_protocol = Type::protocol_with_span("Integer", call_span);
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                (
                    string_type,
                    vec![
                        ("value".to_string(), integer64_type),
                        ("radix".to_string(), integer_protocol),
                    ],
                )
            }

            // Float64 arithmetic intrinsics
            "f64_add" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_sub" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_mul" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_div" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let option_type = Type::generic_concrete("Option", vec![float64_type.clone()]);
                (
                    option_type,
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_mod" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let option_type = Type::generic_concrete("Option", vec![float64_type.clone()]);
                (
                    option_type,
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_pow" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            // Float64 unary intrinsics
            "f64_pos" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![("value".to_string(), float64_type.clone())],
                )
            }

            "f64_neg" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![("value".to_string(), float64_type.clone())],
                )
            }

            "f64_abs" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![("value".to_string(), float64_type.clone())],
                )
            }

            // Float64 comparison intrinsics
            "f64_eq" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_gt" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_ge" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_lt" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            "f64_le" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (
                    boolean_type,
                    vec![
                        ("lhs".to_string(), float64_type.clone()),
                        ("rhs".to_string(), float64_type.clone()),
                    ],
                )
            }

            // Float64 utility intrinsics
            "f64_to_string" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                (string_type, vec![("value".to_string(), float64_type)])
            }

            "f64_is_finite" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (boolean_type, vec![("value".to_string(), float64_type)])
            }

            "f64_is_infinite" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (boolean_type, vec![("value".to_string(), float64_type)])
            }

            "f64_is_nan" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let boolean_type = Type::concrete_with_span("Outrun.Core.Boolean", call_span);
                (boolean_type, vec![("value".to_string(), float64_type)])
            }

            // Float64 rounding intrinsics
            "f64_ceil" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![("value".to_string(), float64_type)],
                )
            }

            "f64_ceil_precision" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let integer_protocol = Type::protocol_with_span("Integer", call_span);
                (
                    float64_type.clone(),
                    vec![
                        ("value".to_string(), float64_type),
                        ("precision".to_string(), integer_protocol),
                    ],
                )
            }

            "f64_floor" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![("value".to_string(), float64_type)],
                )
            }

            "f64_floor_precision" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let integer_protocol = Type::protocol_with_span("Integer", call_span);
                (
                    float64_type.clone(),
                    vec![
                        ("value".to_string(), float64_type),
                        ("precision".to_string(), integer_protocol),
                    ],
                )
            }

            "f64_round" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![("value".to_string(), float64_type)],
                )
            }

            "f64_round_precision" if arguments.len() == 2 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                let integer_protocol = Type::protocol_with_span("Integer", call_span);
                (
                    float64_type.clone(),
                    vec![
                        ("value".to_string(), float64_type),
                        ("precision".to_string(), integer_protocol),
                    ],
                )
            }

            "f64_trunc" if arguments.len() == 1 => {
                let float64_type = Type::concrete_with_span("Outrun.Core.Float64", call_span);
                (
                    float64_type.clone(),
                    vec![("value".to_string(), float64_type)],
                )
            }

            // System intrinsics
            "panic" if arguments.len() == 1 => {
                let string_type = Type::concrete_with_span("Outrun.Core.String", call_span);
                let panic_type = Type::protocol_with_span("Panic", call_span);
                (panic_type, vec![("message".to_string(), string_type)])
            }

            // Default: unknown intrinsic
            _ => {
                return Err(TypecheckError::InferenceError(
                    crate::error::InferenceError::AmbiguousType {
                        span: None,
                        suggestions: vec![format!(
                            "Unknown intrinsic function: {}",
                            intrinsic_name
                        )],
                    },
                ));
            }
        };

        // Register the intrinsic with the generated signature
        let function_info = FunctionInfo {
            defining_scope: "Outrun.Intrinsic".to_string(),
            function_name: intrinsic_name.to_string(),
            visibility: DispatchVisibility::Public,
            parameters: parameter_types,
            return_type: return_type.clone(),
            body: None, // Intrinsic functions don't have user-defined bodies
            span: Some(call_span),
            generic_parameters: Vec::new(), // Intrinsics are not generic
            is_generic: false,
        };

        // Register the intrinsic in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                "Outrun.Intrinsic".to_string(),
                intrinsic_name.to_string(),
                function_info,
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                "Outrun.Intrinsic".to_string(),
                intrinsic_name.to_string(),
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }

        // Return a simple result - we'll let the dispatcher resolve it again
        // This is a simplified approach that gets us working functionality
        Ok(InferenceResult {
            inferred_type: return_type,
            constraints: vec![],
            substitution: Substitution::new(),
        })
    }

    /// Generate constraints for generic function calls (Phase 3.5)
    #[allow(clippy::result_large_err)]
    pub fn generate_generic_function_constraints(
        &mut self,
        function_info: &FunctionInfo,
        inferred_arguments: &[Type],
        _context: &InferenceContext,
    ) -> Result<Vec<Constraint>, TypecheckError> {
        let mut constraints = Vec::new();

        // Extract type parameter mappings from arguments to generic parameters
        let mut type_param_mappings: HashMap<String, Type> = HashMap::new();

        // Iterate through parameters and arguments to build type parameter mapping
        for (i, ((_param_name, param_type), arg_type)) in function_info
            .parameters
            .iter()
            .zip(inferred_arguments.iter())
            .enumerate()
        {
            // For each parameter-argument pair, extract type parameter constraints
            let param_constraints = Self::extract_type_parameter_constraints(
                param_type,
                arg_type,
                &mut type_param_mappings,
                i,
                &function_info.generic_parameters,
            )?;
            constraints.extend(param_constraints);
        }

        // Generate consistency constraints: all occurrences of same type parameter must unify
        for (type_param, inferred_type) in &type_param_mappings {
            // For each occurrence of this type parameter in the function signature,
            // create a constraint that it must equal the inferred type
            for (param_index, (_param_name, param_type)) in
                function_info.parameters.iter().enumerate()
            {
                let param_constraints = Self::generate_type_parameter_equality_constraints(
                    param_type,
                    type_param,
                    inferred_type,
                    param_index,
                )?;
                constraints.extend(param_constraints);
            }

            // Also constrain the return type if it contains this type parameter
            let return_constraints = Self::generate_type_parameter_equality_constraints(
                &function_info.return_type,
                type_param,
                inferred_type,
                usize::MAX, // Special marker for return type
            )?;
            constraints.extend(return_constraints);
        }

        Ok(constraints)
    }

    /// Extract type parameter constraints from parameter-argument type pairing
    #[allow(clippy::result_large_err)]
    fn extract_type_parameter_constraints(
        param_type: &Type,
        arg_type: &Type,
        type_param_mappings: &mut HashMap<String, Type>,
        _param_index: usize,
        generic_parameters: &[String],
    ) -> Result<Vec<Constraint>, TypecheckError> {
        let mut constraints = Vec::new();

        match param_type {
            Type::Concrete { name, args, .. } => {
                // Check if this is actually a generic parameter from the function signature
                if generic_parameters.contains(&name.as_str().to_string()) {
                    // This is a type parameter - record the mapping
                    if let Some(existing_type) = type_param_mappings.get(name.as_str()) {
                        // Type parameter already mapped - create equality constraint
                        constraints.push(Constraint::Equality {
                            left: Box::new(existing_type.clone()),
                            right: Box::new(arg_type.clone()),
                            span: None,
                        });
                    } else {
                        // First occurrence - record the mapping
                        type_param_mappings.insert(name.as_str().to_string(), arg_type.clone());
                    }
                } else {
                    // Concrete type with possible generic arguments
                    match arg_type {
                        Type::Concrete {
                            name: arg_id,
                            args: arg_args,
                            ..
                        } => {
                            // Check that concrete types match
                            if name.as_str() != arg_id.as_str() {
                                constraints.push(Constraint::Equality {
                                    left: Box::new(param_type.clone()),
                                    right: Box::new(arg_type.clone()),
                                    span: None,
                                });
                            } else {
                                // Recursively process generic arguments
                                for (param_arg, arg_arg) in args.iter().zip(arg_args.iter()) {
                                    let nested_constraints =
                                        Self::extract_type_parameter_constraints(
                                            param_arg,
                                            arg_arg,
                                            type_param_mappings,
                                            _param_index,
                                            generic_parameters,
                                        )?;
                                    constraints.extend(nested_constraints);
                                }
                            }
                        }
                        _ => {
                            // Non-concrete argument type - create equality constraint
                            constraints.push(Constraint::Equality {
                                left: Box::new(param_type.clone()),
                                right: Box::new(arg_type.clone()),
                                span: None,
                            });
                        }
                    }
                }
            }
            _ => {
                // Non-concrete parameter type - create equality constraint
                constraints.push(Constraint::Equality {
                    left: Box::new(param_type.clone()),
                    right: Box::new(arg_type.clone()),
                    span: None,
                });
            }
        }

        Ok(constraints)
    }

    /// Generate equality constraints for all occurrences of a type parameter
    #[allow(clippy::result_large_err)]
    fn generate_type_parameter_equality_constraints(
        typ: &Type,
        type_param: &str,
        inferred_type: &Type,
        _position: usize,
    ) -> Result<Vec<Constraint>, TypecheckError> {
        let mut constraints = Vec::new();

        match typ {
            Type::Concrete { name, args, .. } => {
                if name.as_str() == type_param {
                    // This is an occurrence of our type parameter
                    constraints.push(Constraint::Equality {
                        left: Box::new(typ.clone()),
                        right: Box::new(inferred_type.clone()),
                        span: None,
                    });
                } else {
                    // Recursively check generic arguments
                    for arg in args {
                        let nested_constraints =
                            Self::generate_type_parameter_equality_constraints(
                                arg,
                                type_param,
                                inferred_type,
                                _position,
                            )?;
                        constraints.extend(nested_constraints);
                    }
                }
            }
            _ => {
                // Other type forms don't contain type parameters we can substitute
            }
        }

        Ok(constraints)
    }

    /// Instantiate a generic return type with inferred type parameters
    #[allow(clippy::result_large_err)]
    pub fn instantiate_generic_return_type(
        &self,
        return_type: &Type,
        function_info: &FunctionInfo,
        inferred_arguments: &[Type],
    ) -> Result<Type, TypecheckError> {
        // Build type parameter substitution map
        let mut substitutions: HashMap<String, Type> = HashMap::new();

        // Extract substitutions from parameter-argument pairs
        for ((_param_name, param_type), arg_type) in function_info
            .parameters
            .iter()
            .zip(inferred_arguments.iter())
        {
            Self::collect_type_parameter_substitutions(
                param_type,
                arg_type,
                &mut substitutions,
                &function_info.generic_parameters,
            )?;
        }

        // Apply substitutions to return type
        return_type.substitute_type_parameters(&substitutions, false)
    }

    /// Collect type parameter substitutions from parameter-argument type matching
    #[allow(clippy::result_large_err)]
    fn collect_type_parameter_substitutions(
        param_type: &Type,
        arg_type: &Type,
        substitutions: &mut HashMap<String, Type>,
        generic_parameters: &[String],
    ) -> Result<(), TypecheckError> {
        match param_type {
            Type::Concrete { name, args, .. } => {
                // Check if this is actually a generic parameter from the function signature
                if generic_parameters.contains(&name.as_str().to_string()) {
                    substitutions.insert(name.as_str().to_string(), arg_type.clone());
                } else {
                    // Recursively process generic arguments - both param and arg must be concrete with matching base types
                    if let Type::Concrete {
                        name: arg_id,
                        args: arg_args,
                        ..
                    } = arg_type
                    {
                        // Base types should match (e.g., both "Type")
                        if name.as_str() == arg_id.as_str() {
                            // Recursively match type arguments
                            for (param_arg, arg_arg) in args.iter().zip(arg_args.iter()) {
                                Self::collect_type_parameter_substitutions(
                                    param_arg,
                                    arg_arg,
                                    substitutions,
                                    generic_parameters,
                                )?;
                            }
                        }
                        // If base types don't match, we can't extract substitutions
                    }
                }
            }
            _ => {
                // Other type forms don't contain substitutable type parameters
            }
        }
        Ok(())
    }

    /// Convert inference context to dispatch function context
    fn create_function_context_from_inference_context(
        &self,
        context: &InferenceContext,
    ) -> crate::dispatch::FunctionContext {
        // For now, use TopLevel context
        // In a full implementation, we would extract the context from the inference context
        match &context.self_binding {
            SelfBindingContext::ProtocolDefinition {
                protocol_name,
                protocol_args,
            } => crate::dispatch::FunctionContext::Protocol {
                protocol_name: protocol_name.clone(),
                protocol_args: protocol_args.clone(),
            },
            SelfBindingContext::Implementation {
                implementing_type,
                implementing_args,
                protocol_name,
                protocol_args,
            } => crate::dispatch::FunctionContext::Implementation {
                implementing_type: implementing_type.clone(),
                implementing_args: implementing_args.clone(),
                protocol_name: protocol_name.clone(),
                protocol_args: protocol_args.clone(),
            },
            SelfBindingContext::FunctionContext { parent_context, .. } => {
                // For function contexts, use the parent context
                Self::create_function_context_from_self_binding(parent_context)
            }
        }
    }

    /// Helper method to recursively extract function context from self binding context
    fn create_function_context_from_self_binding(
        self_binding: &SelfBindingContext,
    ) -> crate::dispatch::FunctionContext {
        match self_binding {
            SelfBindingContext::ProtocolDefinition {
                protocol_name,
                protocol_args,
            } => crate::dispatch::FunctionContext::Protocol {
                protocol_name: protocol_name.clone(),
                protocol_args: protocol_args.clone(),
            },
            SelfBindingContext::Implementation {
                implementing_type,
                implementing_args,
                protocol_name,
                protocol_args,
            } => crate::dispatch::FunctionContext::Implementation {
                implementing_type: implementing_type.clone(),
                implementing_args: implementing_args.clone(),
                protocol_name: protocol_name.clone(),
                protocol_args: protocol_args.clone(),
            },
            SelfBindingContext::FunctionContext { parent_context, .. } => {
                // For function contexts, use the parent context
                Self::create_function_context_from_self_binding(parent_context)
            }
        }
    }

    /// Infer the type of an anonymous function
    #[allow(clippy::result_large_err)]
    fn infer_qualified_identifier(
        &mut self,
        qualified_id: &outrun_parser::QualifiedIdentifier,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // For now, treat qualified identifiers as protocol static function calls
        // e.g., BinaryAddition.add should resolve to the add function in BinaryAddition protocol

        let module_name = &qualified_id.module.name;
        let function_name = &qualified_id.name.name;

        // Try to find this function in our function registry
        if let Some(function_info) = self
            .function_registry
            .get_function(module_name, function_name)
        {
            // Found a registered function - return its type
            Ok(InferenceResult {
                inferred_type: function_info.return_type.clone(),
                constraints: context.constraints.clone(),
                substitution: context.substitution.clone(),
            })
        } else {
            // Function not found - create a fresh type variable for now
            // In a more complete implementation, we'd check protocol definitions
            let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
            Ok(InferenceResult {
                inferred_type,
                constraints: context.constraints.clone(),
                substitution: context.substitution.clone(),
            })
        }
    }

    /// Convert a parser type annotation to a typechecker type
    #[allow(clippy::result_large_err, clippy::only_used_in_recursion)]
    pub(crate) fn convert_type_annotation(
        &mut self,
        type_annotation: &TypeAnnotation,
    ) -> Result<Type, TypecheckError> {
        match type_annotation {
            TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.clone())
                    .collect::<Vec<_>>()
                    .join(".");

                // Handle Self type specially
                if type_name == "Self" {
                    Ok(Type::SelfType {
                        binding_context: self.current_self_context.clone(),
                        span: None,
                    })
                }
                // Check if this is a generic parameter in the current context
                else if let Some(resolved_type) = self.generic_parameter_context.get(&type_name) {
                    Ok(resolved_type.clone())
                }
                // Process generic arguments if present
                else {
                    let mut type_args = Vec::new();
                    if let Some(args) = generic_args {
                        for arg in &args.args {
                            let arg_type = self.convert_type_annotation(arg)?;
                            type_args.push(arg_type);
                        }
                    }

                    // Use unified TypeRegistry for consistent type resolution
                    if self.type_registry.is_protocol(&type_name) {
                        // Known protocol type
                        Ok(Type::Protocol {
                            name: ModuleName::new(type_name),
                            args: type_args,
                            span: None,
                        })
                    } else if self.type_registry.is_struct(&type_name) {
                        // Known concrete type
                        Ok(Type::Concrete {
                            name: ModuleName::new(type_name),
                            args: type_args,
                            span: None,
                        })
                    } else {
                        // Unknown type - register as forward binding and create placeholder
                        self.register_forward_binding(
                            &type_name,
                            type_args.len(),
                            type_annotation,
                        )?;

                        Ok(Type::Concrete {
                            name: crate::types::ModuleName::new(type_name),
                            args: type_args,
                            span: None,
                        })
                    }
                }
            }
            TypeAnnotation::Tuple { types, span } => {
                // Resolve each element type annotation to create structural tuple type
                let mut element_types = Vec::new();
                for type_annotation in types {
                    element_types.push(self.convert_type_annotation(type_annotation)?);
                }
                Ok(Type::Tuple {
                    element_types,
                    span: Some(*span),
                })
            }
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                // Parse Function<(params) -> ReturnType> syntax
                let mut param_types = Vec::new();
                for param in params {
                    let param_name = param.name.name.clone();
                    let param_type = self.convert_type_annotation(&param.type_annotation)?;
                    param_types.push((param_name, param_type));
                }

                let ret_type = self.convert_type_annotation(return_type)?;

                Ok(Type::Function {
                    params: param_types,
                    return_type: Box::new(ret_type),
                    span: None,
                })
            }
        }
    }

    /// Check if two type IDs represent the same type
    fn type_ids_are_equivalent(
        &self,
        id1: &crate::types::ModuleName,
        id2: &crate::types::ModuleName,
    ) -> bool {
        id1 == id2
    }

    /// Infer the type of a field access expression (object.field)
    #[allow(clippy::result_large_err)]
    fn infer_field_access_type(
        &mut self,
        object_type: &Type,
        field_name: &str,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Resolve Self types to their concrete implementations
        let resolved_object_type = if let Some(resolved) = object_type.resolve_self() {
            resolved
        } else {
            object_type.clone()
        };

        match &resolved_object_type {
            Type::Concrete { name, args, .. } => {
                // Look up the struct definition to find the field type
                self.infer_struct_field_type(name, args, field_name, context)
            }
            Type::Variable { .. } => {
                // For type variables, we can't determine the field type yet
                // Return a fresh type variable and add constraints
                let field_type = Type::variable(self.fresh_type_var(), Level(0));
                Ok(InferenceResult {
                    inferred_type: field_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            _ => {
                // Non-struct types don't have fields
                Err(TypecheckError::InferenceError(
                    crate::error::InferenceError::AmbiguousType {
                        span: None,
                        suggestions: vec![
                            format!("Type '{}' does not have fields", resolved_object_type),
                            "Only struct types support field access".to_string(),
                        ],
                    },
                ))
            }
        }
    }

    /// Infer the type of a specific field in a struct
    #[allow(clippy::result_large_err)]
    fn infer_struct_field_type(
        &mut self,
        struct_type_id: &crate::types::ModuleName,
        struct_args: &[Type],
        field_name: &str,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Look up the actual struct definition from the registry
        let struct_name = struct_type_id.as_str();

        if let Some(struct_def) = self.struct_registry.get(struct_name).cloned() {
            // Find the field in the struct definition
            for field in &struct_def.fields {
                if field.name.name == field_name {
                    // Found the field! Now resolve its type
                    let field_type = self.convert_type_annotation(&field.type_annotation)?;

                    // If the struct has generic arguments, substitute them into the field type
                    let resolved_field_type = if !struct_args.is_empty()
                        && !struct_def
                            .generic_params
                            .as_ref()
                            .is_none_or(|gp| gp.params.is_empty())
                    {
                        self.substitute_generic_args_in_field_type(
                            field_type,
                            &struct_def,
                            struct_args,
                        )?
                    } else {
                        field_type
                    };

                    return Ok(InferenceResult {
                        inferred_type: resolved_field_type,
                        constraints: context.constraints.clone(),
                        substitution: context.substitution.clone(),
                    });
                }
            }

            // Field not found in struct definition
            let available_fields: Vec<String> = struct_def
                .fields
                .iter()
                .map(|f| f.name.name.clone())
                .collect();

            Err(TypecheckError::InferenceError(
                crate::error::InferenceError::UndefinedVariable {
                    variable_name: format!("{}.{}", struct_name, field_name),
                    span: None,
                    similar_names: available_fields,
                    context: Some(format!("Field access on struct {}", struct_name)),
                },
            ))
        } else {
            // Struct definition not found in registry
            Err(TypecheckError::InferenceError(
                crate::error::InferenceError::UndefinedVariable {
                    variable_name: format!("{}.{}", struct_name, field_name),
                    span: None,
                    similar_names: vec![],
                    context: Some(format!("Unknown struct type: {}", struct_name)),
                },
            ))
        }
    }

    /// Substitute generic arguments into a field type
    #[allow(clippy::result_large_err)]
    fn substitute_generic_args_in_field_type(
        &self,
        field_type: Type,
        struct_def: &outrun_parser::StructDefinition,
        struct_args: &[Type],
    ) -> Result<Type, TypecheckError> {
        // Get generic parameter names from struct definition
        let generic_param_names: Vec<String> =
            if let Some(generic_params) = &struct_def.generic_params {
                generic_params
                    .params
                    .iter()
                    .map(|param| param.name.name.clone())
                    .collect()
            } else {
                vec![]
            };

        // Create substitution map from generic parameter names to actual types
        let mut substitution = HashMap::new();
        for (param_name, actual_type) in generic_param_names.iter().zip(struct_args.iter()) {
            substitution.insert(param_name.clone(), actual_type.clone());
        }

        // Apply substitution to field type
        self.apply_generic_substitution_to_type(field_type, &substitution)
    }

    /// Apply generic substitution to a type (recursive)
    fn apply_generic_substitution_to_type(
        &self,
        ty: Type,
        substitution: &HashMap<String, Type>,
    ) -> Result<Type, TypecheckError> {
        Self::apply_substitution_static(ty, substitution)
    }

    /// Static helper for applying generic substitution to a type
    fn apply_substitution_static(
        ty: Type,
        substitution: &HashMap<String, Type>,
    ) -> Result<Type, TypecheckError> {
        match ty {
            Type::Concrete { name, args, span } => {
                // If this is a generic parameter, substitute it
                if let Some(substituted_type) = substitution.get(name.as_str()) {
                    Ok(substituted_type.clone())
                } else {
                    // Apply substitution to generic arguments
                    let substituted_args: Result<Vec<Type>, TypecheckError> = args
                        .into_iter()
                        .map(|arg| Self::apply_substitution_static(arg, substitution))
                        .collect();

                    Ok(Type::Concrete {
                        name: name.clone(),
                        args: substituted_args?,
                        span,
                    })
                }
            }
            Type::Protocol { name, args, span } => {
                // Apply substitution to protocol arguments
                let substituted_args: Result<Vec<Type>, TypecheckError> = args
                    .into_iter()
                    .map(|arg| Self::apply_substitution_static(arg, substitution))
                    .collect();

                Ok(Type::Protocol {
                    name: name.clone(),
                    args: substituted_args?,
                    span,
                })
            }
            Type::Tuple {
                element_types,
                span,
            } => {
                // Apply substitution to tuple element types
                let substituted_elements: Result<Vec<Type>, TypecheckError> = element_types
                    .into_iter()
                    .map(|elem| Self::apply_substitution_static(elem, substitution))
                    .collect();

                Ok(Type::Tuple {
                    element_types: substituted_elements?,
                    span,
                })
            }
            // Other types don't need substitution
            _ => Ok(ty),
        }
    }
}

impl Default for TypeInferenceEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl InferenceContext {
    /// Create a new inference context
    pub fn new() -> Self {
        Self {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: None,
            self_binding: SelfBindingContext::ProtocolDefinition {
                protocol_name: ModuleName::new("Unknown"),
                protocol_args: Vec::new(),
            },
            bindings: HashMap::new(),
        }
    }

    /// Create a context with an expected type
    pub fn with_expected_type(expected_type: Type) -> Self {
        Self {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: Some(expected_type),
            self_binding: SelfBindingContext::ProtocolDefinition {
                protocol_name: ModuleName::new("Unknown"),
                protocol_args: Vec::new(),
            },
            bindings: HashMap::new(),
        }
    }

    /// Add a constraint to be solved
    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    /// Bind a variable to a type in local scope
    pub fn bind_variable(&mut self, name: String, type_: Type) {
        self.bindings.insert(name, type_);
    }

    /// Look up a variable type in local scope
    pub fn lookup_variable(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name)
    }
}

impl Default for InferenceContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_inference_engine_creation() {
        let engine = TypeInferenceEngine::new();
        assert_eq!(engine.type_variable_counter, 0);
        assert_eq!(engine.current_module.as_str(), "main");
    }

    #[test]
    fn test_fresh_type_var_generation() {
        let mut engine = TypeInferenceEngine::new();
        let var1 = engine.fresh_type_var();
        let var2 = engine.fresh_type_var();

        assert_eq!(var1.0, 0);
        assert_eq!(var2.0, 1);
        assert_eq!(engine.type_variable_counter, 2);
    }

    #[test]
    fn test_inference_context_creation() {
        let context = InferenceContext::new();
        assert!(context.expected_type.is_none());
        assert!(context.constraints.is_empty());
        assert!(context.bindings.is_empty());
    }

    #[test]
    fn test_inference_context_with_expected_type() {
        let expected = Type::concrete("Outrun.Core.Integer64");
        let context = InferenceContext::with_expected_type(expected.clone());
        assert_eq!(context.expected_type, Some(expected));
    }

    #[test]
    fn test_variable_binding() {
        let mut context = InferenceContext::new();
        let type_ = Type::concrete("Outrun.Core.String");

        context.bind_variable("x".to_string(), type_.clone());
        assert_eq!(context.lookup_variable("x"), Some(&type_));
        assert_eq!(context.lookup_variable("y"), None);
    }

    #[test]
    fn test_type_variable_generation() {
        let mut engine = TypeInferenceEngine::new();

        // Test that type variables are unique
        let var1 = engine.fresh_type_var();
        let var2 = engine.fresh_type_var();

        assert_ne!(var1, var2);
        assert_eq!(engine.type_variable_counter, 2);
    }

    #[test]
    fn test_module_management() {
        let mut engine = TypeInferenceEngine::new();

        assert_eq!(engine.current_module.as_str(), "main");

        engine.set_current_module(ModuleName::new("Http::Client"));
        assert_eq!(engine.current_module.as_str(), "Http::Client");
    }
}

impl TypeInferenceEngine {
    /// Check if a type name is a generic parameter in the current context
    /// This uses the actual compiler context to determine if a name refers to a generic parameter,
    /// providing 100% accuracy without relying on heuristics or naming patterns.
    pub fn is_type_parameter_name(&self, name: &str) -> bool {
        self.generic_parameter_context.contains_key(name)
    }
}

#[cfg(test)]
mod debug_tests {
    #[test]
    fn test_debug_unary_minus_constraint_resolution() {
        use crate::desugaring::DesugaringEngine;
        use crate::{CompilationResult, Package};
        use outrun_parser::parse_program;

        let source_code = "-1"; // This should trigger UnaryMinus.minus(value: 1)

        // Parse the source code
        let mut program = parse_program(source_code).expect("Parse should succeed");

        // Manually desugar to see what happens
        let mut desugaring_engine = DesugaringEngine::new();
        let _desugar_result = desugaring_engine.desugar_program(&mut program);

        // Create a package and add the program
        let mut package = Package::new("debug_test".to_string());
        package.add_program(program);

        // Try to compile the package to trigger the debug output
        let result = CompilationResult::compile_package(&mut package);

        // We expect this to succeed or fail with a specific error
        match result {
            Ok(_) => {}
            Err(_e) => {}
        }
    }
}
