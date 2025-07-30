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
    types::{
        Constraint, Level, ModuleId, ProtocolId, SelfBindingContext, Substitution, Type, TypeVarId,
    },
};
use outrun_parser::{
    ConstDefinition, Expression, FunctionDefinition, FunctionSignature, ImplBlock, Item, Program,
    ProtocolDefinition, ProtocolFunction, StaticFunctionDefinition, StructDefinition,
    TypeAnnotation, TypeSpec,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Literal values for static evaluation in clause elimination
#[derive(Debug, Clone, PartialEq)]
enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

/// Result type for function clause inference to reduce complexity
type FunctionClauseInferenceResult = (Vec<(String, Type)>, Type, Vec<crate::types::Constraint>);

/// Main type inference engine that orchestrates all typechecker components
pub struct TypeInferenceEngine {
    /// Registry of function signatures (shared)
    function_registry: Rc<FunctionRegistry>,

    /// Unified type registry for protocols and concrete types (shared)
    type_registry: Rc<TypeRegistry>,

    /// Current module being processed
    current_module: ModuleId,

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

    /// Iterative inference support: All inference tasks indexed by ID
    inference_tasks: HashMap<TaskId, InferenceTask>,
    /// Iterative inference support: Queue of tasks ready for processing
    ready_task_queue: std::collections::VecDeque<TaskId>,
    /// Iterative inference support: Completed results indexed by task ID
    task_results: HashMap<TaskId, InferenceResult>,
    /// Iterative inference support: Mapping from expression pointers to task IDs
    expression_to_task_map: HashMap<*mut Expression, TaskId>,
    /// Iterative inference support: Counter for generating unique task IDs
    next_task_id: u64,

    /// Call stack backtracking support for enhanced constraint resolution
    constraint_solver_with_backtracking: Option<crate::constraints::ConstraintSolver>,

    /// Depth tracking for nested inference calls to prevent premature task state clearing
    inference_depth: u32,
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
    Completed(InferenceResult),
    /// Task failed with an error
    Failed(TypecheckError),
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
    pub protocol_id: ProtocolId,
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
    pub fn analyze_signature(
        params: &[(String, Type)],
        return_type: &Type,
    ) -> SignatureAnalysis {
        let mut analyzer = Self::new();
        
        // Analyze all parameter types  
        for (i, (param_name, param_type)) in params.iter().enumerate() {
            analyzer.analyze_type_in_position(
                param_type,
                &crate::types::TypePosition::ArgumentPosition { 
                    index: i,        // Keep index for backward compatibility
                    path: vec![],
                    param_name: Some(param_name.clone()), // Add parameter name!
                }
            );
        }
        
        // Analyze return type
        analyzer.analyze_type_in_position(
            return_type,
            &crate::types::TypePosition::ReturnPosition { path: vec![] }
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
                crate::types::TypePosition::ArgumentPosition { index, path, param_name } => {
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
            let entry = self.current_analysis.type_variable_positions
                .entry(var_name)
                .or_default();
            
            for var_pos in positions {
                let mut full_pos = var_pos.clone();
                // Add position context to variable position
                match position {
                    crate::types::TypePosition::ArgumentPosition { index, path, .. } => {
                        if let crate::types::TypePosition::VariablePosition { path: ref mut var_path } = &mut full_pos {
                            let mut full_path = vec![format!("arg_{}", index)];
                            full_path.extend(path.clone());
                            full_path.extend(var_path.clone());
                            *var_path = full_path;
                        }
                    }
                    crate::types::TypePosition::ReturnPosition { path } => {
                        if let crate::types::TypePosition::VariablePosition { path: ref mut var_path } = &mut full_pos {
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
                Type::Protocol { id, args, .. } => {
                    for (i, arg) in args.iter().enumerate() {
                        self.current_analysis.protocol_constraints.push(ProtocolConstraint {
                            protocol_id: id.clone(),
                            constraining_type: arg.clone(),
                            position: crate::types::TypePosition::GenericArgument {
                                container: id.0.clone(),
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
                    self.current_analysis.unification_constraints.push(Constraint::Equality {
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
                
                self.current_analysis.unification_constraints.push(Constraint::SelfBinding {
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
            if let Some(self_type) = Self::extract_self_from_argument_position(&self_pos.path, call_args)? {
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
            _ => {} // Other types don't have navigable structure
        }

        Ok(None)
    }
}

impl TypeInferenceEngine {
    /// Create a new type inference engine
    pub fn new() -> Self {
        let mut function_registry = FunctionRegistry::new();

        // Register all intrinsic functions that are provided by the runtime
        crate::intrinsics::register_intrinsics(&mut function_registry);

        let function_registry = Rc::new(function_registry);
        let type_registry = Rc::new(TypeRegistry::with_core_types());

        Self {
            function_registry,
            type_registry,
            current_module: ModuleId::new("main"),
            current_file: None,
            type_variable_counter: 0,
            symbol_table: HashMap::new(),
            universal_dispatch_registry: crate::universal_dispatch::UniversalDispatchRegistry::new(),
            error_context: ErrorContext::new(),
            current_self_context: SelfBindingContext::ProtocolDefinition {
                protocol_id: ProtocolId::new("Unknown"),
                protocol_args: vec![],
            },
            struct_registry: HashMap::new(),
            generic_parameter_context: HashMap::new(),
            // Initialize iterative inference support
            inference_tasks: HashMap::new(),
            ready_task_queue: std::collections::VecDeque::new(),
            task_results: HashMap::new(),
            expression_to_task_map: HashMap::new(),
            next_task_id: 0,
            // Call stack backtracking support (initialized later)
            constraint_solver_with_backtracking: None,
            // Depth tracking for nested inference calls
            inference_depth: 0,
        }
    }

    /// Generate a fresh type variable
    pub fn fresh_type_var(&mut self) -> TypeVarId {
        let var_id = TypeVarId(self.type_variable_counter.try_into().unwrap());
        self.type_variable_counter += 1;
        var_id
    }

    /// Set the current module being processed
    pub fn set_current_module(&mut self, module: ModuleId) {
        self.current_module = module.clone();
        // Update the protocol registry within the type registry
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry.protocol_registry_mut().set_current_module(module);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry.protocol_registry_mut().set_current_module(module);
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
            span: span.unwrap_or_else(|| outrun_parser::Span { start: 0, end: 0, start_line_col: None, end_line_col: None }),
            source_file: self.current_file.clone().unwrap_or_else(|| "unknown".to_string()),
        }
    }

    /// Register program expressions for source mapping (stub for now)
    pub fn register_program_expressions(&mut self, _program: &outrun_parser::Program) {
        // TODO: Implement expression-to-source mapping if needed
        // For now, we're using file context tracking instead
    }

    /// Get a mutable reference to the protocol registry within the type registry
    pub fn protocol_registry_mut(&mut self) -> &mut ProtocolRegistry {
        // Clone and replace the type registry to get mutable access
        let new_registry = (*self.type_registry).clone();
        self.type_registry = Rc::new(new_registry);
        Rc::get_mut(&mut self.type_registry)
            .expect("Should be uniquely owned after replacement")
            .protocol_registry_mut()
    }

    /// Get mutable access to type registry for testing and configuration
    pub fn type_registry_mut(&mut self) -> &mut TypeRegistry {
        // Always clone and replace to avoid borrow checker issues
        let new_registry = (*self.type_registry).clone();
        self.type_registry = Rc::new(new_registry);
        Rc::get_mut(&mut self.type_registry)
            .expect("Should be uniquely owned after replacement")
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

    /// Analyze recursive type patterns and emit compiler warnings
    #[allow(clippy::result_large_err)]
    fn analyze_recursive_patterns_and_emit_warnings(&mut self) -> Result<(), TypecheckError> {
        // Create a constraint solver to analyze the protocol registry
        let mut constraint_solver = crate::constraints::ConstraintSolver::with_registry(
            self.type_registry.protocol_registry().clone()
        );

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
        let mut constraint_solver = crate::constraints::ConstraintSolver::with_registry(
            self.type_registry.protocol_registry().clone()
        );
        
        // Initialize call stack context with reasonable depth limit
        let call_stack_context = crate::constraints::CallStackContext::new(10);
        constraint_solver.set_call_stack_context(call_stack_context);
        
        // Store the constraint solver for use during type inference
        self.constraint_solver_with_backtracking = Some(constraint_solver);
        
        eprintln!("🔧 Call stack backtracking system initialized (Phase 2 complete)");
    }

    /// Get mutable access to the constraint solver with backtracking (if initialized)
    pub fn constraint_solver_with_backtracking_mut(&mut self) -> Option<&mut crate::constraints::ConstraintSolver> {
        self.constraint_solver_with_backtracking.as_mut()
    }

    /// Get immutable access to the constraint solver with backtracking (if initialized)
    pub fn constraint_solver_with_backtracking(&self) -> Option<&crate::constraints::ConstraintSolver> {
        self.constraint_solver_with_backtracking.as_ref()
    }

    /// Get the universal dispatch registry containing all registered clauses
    pub fn universal_dispatch_registry(&self) -> &crate::universal_dispatch::UniversalDispatchRegistry {
        &self.universal_dispatch_registry
    }

    /// Import dependency registries into this engine for package composition
    pub fn import_dependency_registries(
        &mut self,
        dependency_protocol_registry: std::rc::Rc<ProtocolRegistry>,
        dependency_function_registry: std::rc::Rc<FunctionRegistry>,
    ) -> Result<(), crate::error::TypecheckError> {
        // Merge the dependency registries into our existing ones while preserving orphan rules
        
        // Merge dependency protocol registry (preserves orphan rule information)
        {
            let protocol_registry = self.protocol_registry_mut();
            protocol_registry.merge_from_dependency(&dependency_protocol_registry)
                .map_err(crate::error::TypecheckError::ImplementationError)?;
        }
        
        // Merge dependency function registry
        {
            let function_registry = self.function_registry_mut();
            function_registry.merge_from_dependency(&dependency_function_registry);
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
    pub fn get_protocol_registry(&self) -> &ProtocolRegistry {
        self.type_registry.protocol_registry()
    }

    /// Get a reference to the protocol registry for dispatch table building
    pub fn protocol_registry(&self) -> &ProtocolRegistry {
        self.type_registry.protocol_registry()
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

        // TODO: Collect available types from type definitions
        // TODO: Collect available protocols from protocol definitions
        // For now, we'll add some common types and protocols
        self.error_context.available_types = vec![
            "String".to_string(),
            "Integer64".to_string(),
            "Float64".to_string(),
            "Boolean".to_string(),
            "List".to_string(),
            "Map".to_string(),
            "Tuple".to_string(),
            "Option".to_string(),
            "Result".to_string(),
        ];

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
        self.error_context.current_module = Some(self.current_module.name().to_string());
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
    ) -> Result<HashSet<ProtocolId>, TypecheckError> {
        let mut required_protocols = HashSet::new();
        self.collect_protocol_requirements_recursive(constraint_expr, &mut required_protocols)?;
        Ok(required_protocols)
    }

    /// Recursively collect protocol requirements from constraint expressions
    fn collect_protocol_requirements_recursive(
        &self,
        constraint_expr: &outrun_parser::ConstraintExpression,
        required_protocols: &mut HashSet<ProtocolId>,
    ) -> Result<(), TypecheckError> {
        use outrun_parser::ConstraintExpression;

        match constraint_expr {
            ConstraintExpression::And { left, right, .. } => {
                // Handle "Self: A && Self: B" patterns
                self.collect_protocol_requirements_recursive(left, required_protocols)?;
                self.collect_protocol_requirements_recursive(right, required_protocols)?;
            }
            ConstraintExpression::Constraint { type_param, protocol_bound, .. } => {
                // Handle "Self: BinarySubtraction" patterns
                if type_param.name == "Self" {
                    // Convert protocol_bound (Vec<TypeIdentifier>) to protocol name
                    let protocol_name = protocol_bound
                        .iter()
                        .map(|segment| segment.name.as_str())
                        .collect::<Vec<_>>()
                        .join(".");
                    required_protocols.insert(ProtocolId::new(&protocol_name));
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
                    self.register_protocol_function_signature(&protocol_name, protocol_function, protocol_def)?;
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
        let struct_module = ModuleId::new(&struct_name);
        self.protocol_registry_mut().add_local_module(struct_module);

        // Set up generic parameter context
        let old_generic_context = self.generic_parameter_context.clone();
        let generic_params = self.extract_struct_generic_parameters(struct_def);
        let generic_context = self.create_generic_context_from_names(&generic_params);
        self.set_generic_parameter_context(generic_context);

        // Store struct definition in registry for field access resolution
        self.struct_registry
            .insert(struct_name.clone(), struct_def.clone());

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
            registry.register_function(struct_name.to_string(), function_name.clone(), function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(struct_name.to_string(), function_name.clone(), function_info);
            self.function_registry = Rc::new(new_registry);
        }

        // Generate public function template for dependency usage (if public)
        if matches!(function.visibility, outrun_parser::FunctionVisibility::Public) {
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
                    // Log template generation failure but don't fail compilation
                    eprintln!("Warning: Failed to generate template for {}.{}: {}", struct_name, function.name.name, e);
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
        let protocol_module = ModuleId::new(&protocol_name);
        self.protocol_registry_mut()
            .add_local_module(protocol_module);

        // WORKAROUND: Identify never types during protocol processing
        // TODO: Replace with proper attribute system when macro system is implemented
        // Never type identification is now handled in registry.is_never_type()

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

        // Register protocol in protocol registry
        let protocol_id = ProtocolId::new(&protocol_name);
        let module_id = ModuleId::new(&protocol_name); // Protocol name as module

        // Extract required protocols from constraint expressions  
        let required_protocols = if let Some(constraints) = &protocol_def.constraints {
            self.extract_protocol_requirements(constraints)?
        } else {
            HashSet::new()
        };

        // Clone data for dual registration  
        let required_protocols_clone = required_protocols.clone();
        let default_implementations_clone = default_implementations.clone();
        let required_functions_clone = required_functions.clone();

        self.protocol_registry_mut().register_protocol_definition(
            protocol_id.clone(),
            required_protocols,
            module_id.clone(),
            default_implementations,
            required_functions,
            Some(protocol_def.span),
        );

        // Also register the protocol in the type registry so that convert_type_annotation
        // can distinguish protocols from concrete types - WITH SAME REQUIREMENTS
        if let Some(type_registry) = Rc::get_mut(&mut self.type_registry) {
            type_registry.protocol_registry_mut().register_protocol_definition(
                protocol_id,
                required_protocols_clone, // Include the same requirements for consistency
                module_id,
                default_implementations_clone, // Include same implementations for consistency  
                required_functions_clone, // Include same functions for consistency
                Some(protocol_def.span),
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_type_registry = (*self.type_registry).clone();
            new_type_registry.protocol_registry_mut().register_protocol_definition(
                protocol_id,
                required_protocols_clone, // Include the same requirements for consistency
                module_id,
                default_implementations_clone, // Include same implementations for consistency  
                required_functions_clone, // Include same functions for consistency
                Some(protocol_def.span),
            );
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
                self.register_protocol_function_definition(protocol_name, definition, protocol_def)?;
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
            implementing_type: crate::types::TypeId::new(&type_name),
            implementing_args,
            protocol_id: ProtocolId::new(&protocol_name),
            protocol_args: vec![], // TODO: Extract from protocol spec if needed
        };
        eprintln!("🔄 CONTEXT CHANGE: Entering impl block");
        eprintln!("  📤 Old context: {:?}", old_self_context);
        eprintln!("  📥 New context: {} implementing {}", type_name, protocol_name);
        self.current_self_context = new_context;

        // Collect impl block functions
        for function in &impl_block.functions {
            self.collect_impl_block_function(&impl_scope, function, impl_block)?;
        }

        // Restore previous contexts
        eprintln!("🔄 CONTEXT RESTORE: Exiting impl block collection");
        eprintln!("  📤 Current context: {} implementing {}", type_name, protocol_name);
        eprintln!("  📥 Restoring context: {:?}", old_self_context);
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
            registry.register_function(impl_scope.to_string(), function_name.clone(), function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(impl_scope.to_string(), function_name.clone(), function_info);
            self.function_registry = Rc::new(new_registry);
        }

        // Generate public function template for dependency usage (if public)
        if matches!(function.visibility, outrun_parser::FunctionVisibility::Public) {
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
                    // Log template generation failure but don't fail compilation
                    eprintln!("Warning: Failed to generate template for {}.{}: {}", impl_scope, function.name.name, e);
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
    fn extract_struct_generic_parameters_by_name(&self, struct_name: &str) -> Result<Vec<String>, TypecheckError> {
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
    fn extract_type_variables_from_annotation(&self, annotation: &outrun_parser::TypeAnnotation) -> Vec<String> {
        use outrun_parser::TypeAnnotation;
        let mut type_vars = Vec::new();

        match annotation {
            TypeAnnotation::Simple { path, generic_args, .. } => {
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
            TypeAnnotation::Function { params, return_type, .. } => {
                for param in params {
                    let nested_vars = self.extract_type_variables_from_annotation(&param.type_annotation);
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
    fn extract_type_variables_from_constraint_expression(&self, constraint: &outrun_parser::ConstraintExpression) -> Vec<String> {
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
                if (param_name == "Self" || self.is_type_variable_name(param_name)) && !type_vars.contains(param_name) {
                    type_vars.push(param_name.clone());
                }
            }
            ConstraintExpression::Parenthesized { expression, .. } => {
                // Handle: (T: Display)
                let nested_vars = self.extract_type_variables_from_constraint_expression(expression);
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
    fn validate_impl_constraint_variables(&self, impl_block: &ImplBlock, available_type_vars: &[String]) -> Result<(), TypecheckError> {
        if let Some(constraints) = &impl_block.constraints {
            let constraint_vars = self.extract_type_variables_from_constraint_expression(constraints);
            
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
        matches!(name, "Key" | "Value" | "Input" | "Output" | "Result" | "Error" | "Element")
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
            defining_scope: self.current_module.0.clone(),
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
            registry.register_function(self.current_module.0.clone(), function_name.clone(), function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                self.current_module.0.clone(),
                function_name.clone(),
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }

        // Generate public function template for dependency usage (if public)
        if matches!(func_def.visibility, outrun_parser::FunctionVisibility::Public) {
            let function_signature = crate::universal_dispatch::FunctionSignature::qualified(
                self.current_module.0.clone(),
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
                    // Log template generation failure but don't fail compilation
                    eprintln!("Warning: Failed to generate template for {}.{}: {}", self.current_module.0, func_def.name.name, e);
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

        let implementing_type = crate::types::TypeId::new(&struct_name);
        let module_id = crate::types::ModuleId::new(&struct_name);

        // Register Any implementation
        let any_protocol = crate::types::ProtocolId::new("Any");
        self.protocol_registry_mut().register_implementation(
            implementing_type.clone(),
            vec![], // No generic args for concrete types
            any_protocol,
            vec![], // Any protocol has no generic args
            module_id.clone(),
            Some(struct_def.span),
        )?;

        // Register Inspect implementation
        let inspect_protocol = crate::types::ProtocolId::new("Inspect");
        self.protocol_registry_mut().register_implementation(
            implementing_type,
            vec![], // No generic args for concrete types
            inspect_protocol,
            vec![], // Inspect protocol has no generic args
            module_id,
            Some(struct_def.span),
        )?;

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
        let protocol_id = ProtocolId::new(&protocol_name);

        let implementing_type_name = impl_block
            .type_spec
            .path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        let implementing_type_id = crate::types::TypeId::new(&implementing_type_name);

        // Create module ID from implementing type name
        let module_id = ModuleId::new(implementing_type_id.name());

        // Mark the defining module as local (for orphan rule)
        self.protocol_registry_mut()
            .add_local_module(module_id.clone());

        // Register the implementation relationship (without validating function bodies)
        self.protocol_registry_mut().register_implementation(
            implementing_type_id.clone(),
            vec![], // TODO: Handle generic parameters
            protocol_id.clone(),
            vec![], // TODO: Handle protocol generic parameters
            module_id,
            Some(impl_block.span),
        )?;

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
            protocol_id: ProtocolId::new(&protocol_name),
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
            implementing_type: crate::types::TypeId::new(&type_name),
            implementing_args,
            protocol_id: ProtocolId::new(&protocol_name),
            protocol_args: vec![], // TODO: Extract from protocol spec if needed
        };

        for function in &impl_block.functions {
            self.typecheck_function_body(&impl_scope, function)?;
        }

        // Restore previous contexts
        eprintln!("🔄 CONTEXT RESTORE: Exiting impl block typechecking");
        eprintln!("  📤 Current context: {} implementing {}", type_name, protocol_name);
        eprintln!("  📥 Restoring context: {:?}", old_self_context);
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
        let module_scope = self.current_module.0.clone();
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
        if self.type_registry.is_never_type(found) {
            return true;
        }
        if self.type_registry.is_never_type(expected) {
            return true;
        }
        
        // Fall back to regular type compatibility
        self.types_are_compatible(expected, found)
    }

    // ============================================================================
    // ITERATIVE INFERENCE INFRASTRUCTURE
    // ============================================================================

    /// Reset the iterative inference state for a new inference operation  
    /// CRITICAL FIX: Only clear task state at top level (depth 0) to preserve dependencies
    /// across nested inference calls within the same inference session
    fn reset_iterative_state(&mut self) {
        if self.inference_depth == 0 {
            println!("🧹 CLEARING TASK STATE: {} tasks will be lost (depth={})", self.inference_tasks.len(), self.inference_depth);
            self.inference_tasks.clear();
            self.ready_task_queue.clear();
            self.task_results.clear();
            self.expression_to_task_map.clear();
            self.next_task_id = 0;
        } else {
            println!("🔄 PRESERVING TASK STATE: {} tasks preserved at depth {}", self.inference_tasks.len(), self.inference_depth);
            // CRITICAL FIX: Ensure next_task_id is set to avoid ID collisions with preserved tasks
            // Find the highest existing task ID and set next_task_id to the next available ID
            if let Some(&max_task_id) = self.inference_tasks.keys().max() {
                self.next_task_id = max_task_id + 1;
                println!("🔧 ADJUSTED next_task_id to {} to avoid collisions", self.next_task_id);
            }
        }
    }

    /// Generate a unique task ID
    fn generate_task_id(&mut self) -> TaskId {
        let id = self.next_task_id;
        self.next_task_id += 1;
        id
    }



    /// Create a new inference task for an expression
    fn create_task(&mut self, expression: *mut Expression, context: InferenceContext) -> TaskId {
        // Check if this expression should not be reused
        // Identifiers are context-dependent due to variable bindings
        // Function calls should not be reused because they represent different operations
        let should_not_reuse = unsafe { 
            matches!((*expression).kind, 
                outrun_parser::ExpressionKind::Identifier(_) |
                outrun_parser::ExpressionKind::FunctionCall(_)
            )
        };
        
        // DEBUGGING: Check if task already exists for this expression
        if !should_not_reuse {
            if let Some(&existing_task_id) = self.expression_to_task_map.get(&expression) {
                // CRITICAL FIX: Check if the Self context has changed since the task was created
                // If the Self context is different, we must create a new task to avoid
                // reusing cached SelfType instances with the wrong binding context
                if let Some(existing_task) = self.inference_tasks.get(&existing_task_id) {
                    if existing_task.context.self_binding == context.self_binding {
                        return existing_task_id;
                    }
                    // Self context has changed - fall through to create new task
                }
            }
        }        
        let task_id = self.generate_task_id();
        println!("      ➕ Creating new task {} for expression at {:p}", task_id, expression);
        
        let task = InferenceTask {
            id: task_id,
            expression,
            context,
            dependencies: Vec::new(),
            dependents: Vec::new(),
            state: TaskState::Pending,
            completed_dependencies: Vec::new(),
        };

        self.inference_tasks.insert(task_id, task);
        
        // DEBUG: Check if we're overwriting an existing mapping
        if let Some(old_task_id) = self.expression_to_task_map.insert(expression, task_id) {
            println!("      ⚠️  OVERWRITING: Expression had task {} -> now has task {}", old_task_id, task_id);
        }
        
        task_id
    }

    /// Add a dependency relationship between tasks
    fn add_task_dependency(&mut self, dependent_task: TaskId, dependency_task: TaskId) {
        // Add dependency to the dependent task
        if let Some(dependent) = self.inference_tasks.get_mut(&dependent_task) {
            // DEBUG: Check if this dependency is already present
            if dependent.dependencies.contains(&dependency_task) {
                println!("      🔄 Task {} already has dependency {}, not adding duplicate", dependent_task, dependency_task);
                return;
            }
            
            dependent.dependencies.push(dependency_task);
            println!("      ✅ Task {} now has {} dependencies: {:?}", dependent_task, dependent.dependencies.len(), dependent.dependencies);
        } else {
            println!("      ❌ Failed to find dependent task {}", dependent_task);
        }
        
        // Add dependent to the dependency task
        if let Some(dependency) = self.inference_tasks.get_mut(&dependency_task) {
            if !dependency.dependents.contains(&dependent_task) {
                dependency.dependents.push(dependent_task);
            }
        } else {
            println!("      ❌ Failed to find dependency task {}", dependency_task);
        }
    }

    /// Mark tasks with no dependencies as ready
    fn mark_ready_tasks(&mut self) {
        let task_ids: Vec<TaskId> = self.inference_tasks.keys().cloned().collect();
        
        for task_id in task_ids {
            let task = self.inference_tasks.get_mut(&task_id).unwrap();
            if task.dependencies.is_empty() && matches!(task.state, TaskState::Pending) {
                task.state = TaskState::Ready;
                self.ready_task_queue.push_back(task_id);
            }
        }
    }

    /// Complete a task and update its dependents
    fn complete_task(&mut self, task_id: TaskId, result: InferenceResult) {
        // Store the result
        self.task_results.insert(task_id, result.clone());
        
        // Update task state
        if let Some(task) = self.inference_tasks.get_mut(&task_id) {
            task.state = TaskState::Completed(result);
            
            // Process dependents
            let dependents = task.dependents.clone();
            for dependent_id in dependents {
                self.update_dependent_task(dependent_id, task_id);
            }
        }
    }

    /// Update a dependent task when one of its dependencies completes
    fn update_dependent_task(&mut self, dependent_id: TaskId, completed_dependency: TaskId) {
        if let Some(dependent_task) = self.inference_tasks.get_mut(&dependent_id) {
            // CRITICAL FIX: Move completed dependency to completed_dependencies list instead of removing it
            if dependent_task.dependencies.contains(&completed_dependency) {
                dependent_task.dependencies.retain(|&dep| dep != completed_dependency);
                dependent_task.completed_dependencies.push(completed_dependency);
                println!("      📋 Task {} moved dependency {} to completed (remaining: {}, completed: {})", 
                    dependent_id, completed_dependency, dependent_task.dependencies.len(), dependent_task.completed_dependencies.len());
            }
            
            // If no more dependencies, mark as ready
            if dependent_task.dependencies.is_empty() && matches!(dependent_task.state, TaskState::Pending) {
                dependent_task.state = TaskState::Ready;
                self.ready_task_queue.push_back(dependent_id);
            }
        }
    }

    /// Process function call with pre-processed argument results from dependency tasks
    /// Used by iterative inference to avoid recursive argument processing
    fn infer_function_call_with_processed_args(
        &mut self,
        func_call: &mut outrun_parser::FunctionCall,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // DEBUG: Function call processing
        let call_name = match &func_call.path {
            outrun_parser::FunctionPath::Simple { name } => name.name.clone(),
            outrun_parser::FunctionPath::Qualified { module, name } => format!("{}.{}", module.name, name.name),
            _ => "dynamic".to_string(),
        };
        
        // Get dependency results for arguments
        let task = &self.inference_tasks[&task_id];
        println!("🔧 Processing function call: {} with {} dependencies (task_id: {})", call_name, task.dependencies.len(), task_id);
        println!("    🔍 Task dependencies: {:?}", task.dependencies);
        
        // DEBUG: Check if this task exists in the map and compare
        if let Some(stored_task) = self.inference_tasks.get(&task_id) {
            println!("    🔍 Stored task dependencies: {:?}", stored_task.dependencies);
            if stored_task.dependencies != task.dependencies {
                println!("    ⚠️  MISMATCH: Retrieved task differs from stored task!");
            }
        } else {
            println!("    ❌ Task {} not found in inference_tasks map!", task_id);
        }
        
        let mut arg_results = Vec::new();
        
        for dependency_id in &task.completed_dependencies {
            if let Some(result) = self.task_results.get(dependency_id) {
                arg_results.push(result.clone());
            }
        }
        
        // Update argument expressions with inferred types from results
        for (_i, _result) in arg_results.iter().enumerate() {
            if let Some(arg) = func_call.arguments.get_mut(_i) {
                match arg {
                    outrun_parser::Argument::Named { expression, .. } => {
                        // TODO: Convert Type to ParsedTypeInfo when needed
                        // expression.type_info = Some(result.inferred_type.clone());
                        let _ = expression; // Suppress unused variable warning
                    }
                    outrun_parser::Argument::Spread { expression, .. } => {
                        // TODO: Convert Type to ParsedTypeInfo when needed
                        // expression.type_info = Some(result.inferred_type.clone());
                        let _ = expression; // Suppress unused variable warning
                    }
                }
            }
        }
        
        // CRITICAL FIX: Extract argument types from completed dependency results
        let mut arg_types = Vec::new();
        println!("    🔍 Extracting {} argument types from completed dependencies", task.completed_dependencies.len());
        for (i, dependency_id) in task.completed_dependencies.iter().enumerate() {
            if let Some(result) = self.task_results.get(dependency_id) {
                println!("      ✅ Arg {}: {:?}", i, result.inferred_type);
                arg_types.push(Some(result.inferred_type.clone()));
            } else {
                println!("      ❌ Arg {}: Missing dependency result", i);
                arg_types.push(None);
            }
        }
        // Use existing function call inference with processed arguments  
        // CRITICAL FIX: Pass the original func_call directly so modifications are preserved
        // Skip recursive argument processing since dependencies are already processed
        let mut mutable_context = context.clone();
        self.infer_function_call_with_precomputed_args(func_call, &mut mutable_context, &arg_types)
    }

    /// Process list literal with pre-processed element results from dependency tasks
    /// Used by iterative inference to avoid recursive element processing
    fn infer_list_literal_with_processed_elements(
        &mut self,
        list_literal: &outrun_parser::ListLiteral,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get dependency results for elements
        let task = &self.inference_tasks[&task_id];
        let mut element_results = Vec::new();
        
        for dependency_id in &task.completed_dependencies {
            if let Some(result) = self.task_results.get(dependency_id) {
                element_results.push(result.clone());
            }
        }
        
        // Create modified list literal with processed element types
        let mut processed_literal = list_literal.clone();
        
        // Update list element expressions with inferred types from results
        for (_i, _result) in element_results.iter().enumerate() {
            if let Some(element) = processed_literal.elements.get_mut(_i) {
                if let outrun_parser::ListElement::Expression(expr) = element {
                    // TODO: Convert Type to ParsedTypeInfo when needed
                    // expr.type_info = Some(result.inferred_type.clone());
                    let _ = expr; // Suppress unused variable warning
                }
            }
        }
        
        // Use existing recursive list literal inference with processed elements
        let mut mutable_context = context.clone();
        self.infer_list_literal(&processed_literal, &mut mutable_context)
    }

    /// Process tuple literal with pre-processed element results from dependency tasks
    /// Used by iterative inference to avoid recursive element processing  
    fn infer_tuple_literal_with_processed_elements(
        &mut self,
        tuple_literal: &outrun_parser::TupleLiteral,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get dependency results for elements
        let task = &self.inference_tasks[&task_id];
        let mut element_results = Vec::new();
        
        for dependency_id in &task.completed_dependencies {
            if let Some(result) = self.task_results.get(dependency_id) {
                element_results.push(result.clone());
            }
        }
        
        // Create modified tuple literal with processed element types
        let mut processed_literal = tuple_literal.clone();
        
        // Update tuple element expressions with inferred types from results
        for (_i, _result) in element_results.iter().enumerate() {
            if let Some(element) = processed_literal.elements.get_mut(_i) {
                // TODO: Convert Type to ParsedTypeInfo when needed
                // element.type_info = Some(result.inferred_type.clone());
                let _ = element; // Suppress unused variable warning
            }
        }
        
        // Use existing recursive tuple literal inference with processed elements
        let mut mutable_context = context.clone();
        self.infer_tuple_literal(&processed_literal, &mut mutable_context)
    }

    /// Process map literal with pre-processed key-value pair results from dependency tasks
    /// Used by iterative inference to avoid recursive key-value processing
    fn infer_map_literal_with_processed_entries(
        &mut self,
        map_literal: &outrun_parser::MapLiteral,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get dependency results for key-value pairs
        let task = &self.inference_tasks[&task_id];
        let mut entry_results = Vec::new();
        
        for dependency_id in &task.completed_dependencies {
            if let Some(result) = self.task_results.get(dependency_id) {
                entry_results.push(result.clone());
            }
        }
        
        // Create modified map literal with processed entry types
        let mut processed_literal = map_literal.clone();
        
        // Update map entry expressions with inferred types from results
        let mut result_index = 0;
        for entry in &mut processed_literal.entries {
            match entry {
                outrun_parser::MapEntry::Assignment { key, value } => {
                    // Two results: key and value
                    if let (Some(key_result), Some(value_result)) = (
                        entry_results.get(result_index),
                        entry_results.get(result_index + 1),
                    ) {
                        // TODO: Convert Type to ParsedTypeInfo when needed
                        let _ = (key, value, key_result, value_result); // Suppress unused variable warnings
                        result_index += 2;
                    }
                }
                outrun_parser::MapEntry::Shorthand { value, .. } => {
                    // One result: value only
                    if let Some(value_result) = entry_results.get(result_index) {
                        // TODO: Convert Type to ParsedTypeInfo when needed
                        let _ = (value, value_result); // Suppress unused variable warnings
                        result_index += 1;
                    }
                }
                outrun_parser::MapEntry::Spread(_) => {
                    // Spread entries don't have expressions, skip
                }
            }
        }
        
        // Use existing recursive map literal inference with processed entries
        let mut mutable_context = context.clone();
        self.infer_map_literal(&processed_literal, &mut mutable_context)
    }

    /// Process parenthesized expression with pre-processed inner result from dependency task
    /// Used by iterative inference to avoid recursive inner expression processing
    fn infer_parenthesized_expression_with_processed_inner(
        &mut self,
        task_id: TaskId,
        _context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get dependency result for inner expression
        let task = &self.inference_tasks[&task_id];
        
        if let Some(dependency_id) = task.completed_dependencies.first() {
            if let Some(inner_result) = self.task_results.get(dependency_id) {
                // Parenthesized expressions just pass through the inner type
                Ok(InferenceResult {
                    inferred_type: inner_result.inferred_type.clone(),
                    constraints: inner_result.constraints.clone(),
                    substitution: inner_result.substitution.clone(),
                })
            } else {
                Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(None),
                    suggestions: vec!["Parenthesized expression inner dependency not completed".to_string()],
                }))
            }
        } else {
            Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                span: to_source_span(None),
                suggestions: vec!["Parenthesized expression has no dependencies".to_string()],
            }))
        }
    }

    /// Process field access with pre-processed object result from dependency task
    /// Used by iterative inference to avoid recursive object expression processing
    fn infer_field_access_with_processed_object(
        &mut self,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get dependency result for object expression
        let task = &self.inference_tasks[&task_id];
        
        if let Some(dependency_id) = task.completed_dependencies.first() {
            if let Some(object_result) = self.task_results.get(dependency_id) {
                // Clone the object result to avoid borrowing conflicts
                let object_type = object_result.inferred_type.clone();
                
                // Get the field access expression to extract field name
                let expression = unsafe { &*task.expression };
                
                if let outrun_parser::ExpressionKind::FieldAccess(field_access) = &expression.kind {
                    // Use existing field access type inference with processed object type
                    let mut mutable_context = context.clone();
                    self.infer_field_access_type(
                        &object_type,
                        &field_access.field.name,
                        &mut mutable_context,
                    )
                } else {
                    Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                        span: to_source_span(None),
                        suggestions: vec!["Expected field access expression".to_string()],
                    }))
                }
            } else {
                Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(None),
                    suggestions: vec!["Field access object dependency not completed".to_string()],
                }))
            }
        } else {
            Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                span: to_source_span(None),
                suggestions: vec!["Field access has no dependencies".to_string()],
            }))
        }
    }

    /// Process anonymous function with pre-processed clause bodies/guards from dependency tasks
    /// Used by iterative inference to avoid recursive clause processing
    fn infer_anonymous_function_with_processed_clauses(
        &mut self,
        anonymous_fn: &outrun_parser::AnonymousFunction,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get dependency results for clause bodies and guards
        let task = &self.inference_tasks[&task_id];
        let mut dependency_results = Vec::new();
        
        for dependency_id in &task.completed_dependencies {
            if let Some(result) = self.task_results.get(dependency_id) {
                dependency_results.push(result.clone());
            }
        }
        
        // Create modified anonymous function with processed clause types
        let processed_fn = anonymous_fn.clone();
        
        // For now, just use the results to verify dependencies are processed
        // The actual type inference logic can delegate to existing implementation
        let _ = dependency_results; // Suppress unused variable warning
        
        // Use existing recursive anonymous function inference
        let mut mutable_context = context.clone();
        self.infer_anonymous_function(&processed_fn, &mut mutable_context)
    }

    /// Process struct literal with pre-processed field values from dependency tasks
    /// Used by iterative inference to avoid recursive field processing
    fn infer_struct_literal_with_processed_fields(
        &mut self,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get dependency results for field values
        let task = &self.inference_tasks[&task_id];
        let mut field_results = Vec::new();
        
        for dependency_id in &task.completed_dependencies {
            if let Some(result) = self.task_results.get(dependency_id) {
                field_results.push(result.clone());
            }
        }
        
        // Get the struct literal expression 
        let expression = unsafe { &*task.expression };
        
        if let outrun_parser::ExpressionKind::Struct(struct_literal) = &expression.kind {
            // Create modified struct literal with processed field types
            let mut processed_literal = struct_literal.clone();
            
            // Update struct field expressions with inferred types from results
            let mut result_index = 0;
            for field in &mut processed_literal.fields {
                match field {
                    outrun_parser::StructLiteralField::Assignment { value, .. } => {
                        if let Some(field_result) = field_results.get(result_index) {
                            // TODO: Convert Type to ParsedTypeInfo when needed
                            let _ = (value, field_result); // Suppress unused variable warnings
                            result_index += 1;
                        }
                    }
                    outrun_parser::StructLiteralField::Shorthand(_) => {
                        // Shorthand fields don't have expressions, skip
                    }
                    outrun_parser::StructLiteralField::Spread(_) => {
                        // Spread fields don't have expressions, skip
                    }
                }
            }
            
            // Use existing recursive struct literal inference with processed fields
            let mut mutable_context = context.clone();
            // For now, assign a fresh type variable since struct literal inference is not yet implemented
            let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
            Ok(InferenceResult {
                inferred_type,
                constraints: mutable_context.constraints.clone(),
                substitution: mutable_context.substitution.clone(),
            })
        } else {
            Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                span: to_source_span(None),
                suggestions: vec!["Expected struct literal expression".to_string()],
            }))
        }
    }

    /// Process if expression with pre-processed condition from dependency task
    /// Used by iterative inference to avoid recursive condition processing
    fn infer_if_expression_with_processed_branches(
        &mut self,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get dependency result for condition
        let task = &self.inference_tasks[&task_id];
        
        if let Some(dependency_id) = task.completed_dependencies.first() {
            if let Some(condition_result) = self.task_results.get(dependency_id) {
                // Get the if expression
                let expression = unsafe { &*task.expression };
                
                if let outrun_parser::ExpressionKind::IfExpression(if_expr) = &expression.kind {
                    println!("🔧 Implementing if expression type inference");
                    
                    // CRITICAL FIX: Collect dependency results first to avoid borrowing conflicts
                    let mut dependency_results: Vec<Type> = Vec::new();
                    for dependency_id in &task.completed_dependencies {
                        if let Some(result) = self.task_results.get(dependency_id) {
                            dependency_results.push(result.inferred_type.clone());
                        }
                    }
                    
                    // Extract branch types from dependency results
                    // dependency_results[0] = condition, dependency_results[1] = then, dependency_results[2] = else (if present)
                    let then_result = if dependency_results.len() > 1 {
                        dependency_results[1].clone()
                    } else {
                        // Fallback: if no dependency result, use fresh type variable
                        Type::variable(self.fresh_type_var(), Level(0))
                    };
                    println!("  Then branch type: {:?}", then_result);
                    
                    let else_result = if dependency_results.len() > 2 {
                        Some(dependency_results[2].clone())
                    } else if if_expr.else_block.is_some() {
                        // Else block exists but no dependency result - use fresh type variable
                        Some(Type::variable(self.fresh_type_var(), Level(0)))
                    } else {
                        println!("  No else branch");
                        None
                    };
                    
                    if let Some(ref else_type) = else_result {
                        println!("  Else branch type: {:?}", else_type);
                    }
                    
                    // Determine the unified type
                    let unified_type = match else_result {
                        Some(else_result) => {
                            // Both branches exist - they must have compatible types
                            if self.types_are_compatible(&then_result, &else_result) {
                                then_result // Use then branch type as the unified type
                            } else {
                                println!("  ❌ Branch types are incompatible");
                                return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                                    span: to_source_span(Some(expression.span)),
                                    suggestions: vec![format!(
                                        "If branches have incompatible types: then={:?}, else={:?}",
                                        then_result, else_result
                                    )],
                                }));
                            }
                        }
                        None => {
                            // No else branch - the then block type must implement Default
                            println!("  🔧 No else branch - checking Default protocol requirement");
                            
                            // Check if the then block type implements Default protocol
                            let default_protocol_id = crate::types::ProtocolId::new("Default");
                            
                            // Extract the type ID from the then_result type
                            let type_implements_default = match &then_result {
                                Type::Concrete { id, .. } => {
                                    self.protocol_registry().type_satisfies_protocol(id, &default_protocol_id)
                                }
                                Type::Protocol { id, .. } => {
                                    // For protocol types, check if the protocol itself satisfies Default
                                    let type_id = crate::types::TypeId::new(id.name());
                                    self.protocol_registry().type_satisfies_protocol(&type_id, &default_protocol_id)
                                }
                                Type::SelfType { binding_context, .. } => {
                                    // For Self types, check if the implementing type satisfies Default
                                    match binding_context {
                                        crate::types::SelfBindingContext::Implementation { implementing_type, .. } => {
                                            self.protocol_registry().type_satisfies_protocol(implementing_type, &default_protocol_id)
                                        }
                                        _ => false
                                    }
                                }
                                _ => false
                            };
                            
                            if type_implements_default {
                                println!("  ✅ Type implements Default protocol");
                                then_result
                            } else {
                                println!("  ❌ Type does not implement Default protocol");
                                return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                                    span: to_source_span(Some(expression.span)),
                                    suggestions: vec![format!(
                                        "If expression without else block requires type {:?} to implement Default protocol",
                                        then_result
                                    )],
                                }));
                            }
                        }
                    };
                    
                    println!("  ✅ If expression unified type: {:?}", unified_type);
                    
                    Ok(InferenceResult {
                        inferred_type: unified_type,
                        constraints: context.constraints.clone(),
                        substitution: context.substitution.clone(),
                    })
                } else {
                    Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                        span: to_source_span(None),
                        suggestions: vec!["Expected if expression".to_string()],
                    }))
                }
            } else {
                Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(None),
                    suggestions: vec!["If expression condition dependency not completed".to_string()],
                }))
            }
        } else {
            Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                span: to_source_span(None),
                suggestions: vec!["If expression has no dependencies".to_string()],
            }))
        }
    }

    /// Process case expression with pre-processed scrutinee and guards from dependency tasks
    /// Used by iterative inference to avoid recursive scrutinee processing
    fn infer_case_expression_with_processed_clauses(
        &mut self,
        task_id: TaskId,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Get the case expression
        let task = &self.inference_tasks[&task_id];
        let expression = unsafe { &*task.expression };
        
        if let outrun_parser::ExpressionKind::CaseExpression(case_expr) = &expression.kind {
            // Get scrutinee result from dependencies - clone to avoid borrow conflicts
            let scrutinee_result = if let Some(dependency_id) = task.completed_dependencies.first() {
                self.task_results.get(dependency_id)
                    .ok_or_else(|| TypecheckError::InferenceError(InferenceError::AmbiguousType {
                        span: Some(self.create_file_span(None).to_source_span()),
                        suggestions: vec!["Missing scrutinee result".to_string()],
                    }))?.clone()
            } else {
                return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: Some(self.create_file_span(None).to_source_span()),
                    suggestions: vec!["Case expression missing scrutinee".to_string()],
                }));
            };
            
            let scrutinee_type = scrutinee_result.inferred_type;
            let mut result_constraints = context.constraints.clone();
            let result_substitution = context.substitution.clone();
            let mut clause_result_types = Vec::new();
            
            // Process each case clause with proper scoping
            for clause in &case_expr.clauses {
                // Create a new context scope for this clause
                let mut clause_context = context.clone();
                
                // Extract pattern bindings and add them to the bindings map
                let pattern_bindings = self.extract_pattern_bindings(&clause.pattern, &scrutinee_type)?;
                for (name, binding_type) in pattern_bindings {
                    clause_context.bindings.insert(name, binding_type);
                }
                
                // Process guard expression if present (with pattern bindings in scope)
                if let Some(guard_expr) = &clause.guard {
                    let guard_result = self.infer_expression_recursive(
                        &mut guard_expr.clone(), 
                        &mut clause_context
                    )?;
                    
                    // Guard must be Boolean - simplified constraint for now
                    let _boolean_type = Type::concrete("Boolean");
                    let fresh_var = self.fresh_type_var();
                    result_constraints.push(Constraint::Implements {
                        type_var: match guard_result.inferred_type {
                            Type::Variable { var_id, .. } => var_id,
                            _ => fresh_var,
                        },
                        protocol: crate::types::ProtocolId::new("Boolean"),
                        span: None,
                    });
                    
                    // Merge constraints
                    result_constraints.extend(guard_result.constraints);
                    // Merge substitutions - simplified for now
                    // TODO: Implement proper substitution merging
                }
                
                // Process clause result expression (with pattern bindings in scope)
                let clause_result_expr = match &clause.result {
                    outrun_parser::CaseResult::Expression(expr) => expr.as_ref(),
                    outrun_parser::CaseResult::Block(_) => {
                        // For blocks, assign a fresh type variable for now
                        let fresh_var = self.fresh_type_var();
                        let fresh_type = Type::variable(fresh_var, Level(0));
                        clause_result_types.push(fresh_type);
                        continue;
                    }
                };
                
                let clause_result = self.infer_expression_recursive(
                    &mut clause_result_expr.clone(),
                    &mut clause_context
                )?;
                
                clause_result_types.push(clause_result.inferred_type.clone());
                result_constraints.extend(clause_result.constraints);
                // Merge substitutions - simplified for now
                // TODO: Implement proper substitution merging
            }
            
            // All clause results must have the same type
            let case_result_type = if clause_result_types.is_empty() {
                let fresh_var = self.fresh_type_var();
                Type::variable(fresh_var, Level(0))
            } else {
                let first_type = clause_result_types[0].clone();
                // For now, just return the first type - proper unification would be needed
                // to ensure all clause types are compatible
                first_type
            };
            
            Ok(InferenceResult {
                inferred_type: case_result_type,
                constraints: result_constraints,
                substitution: result_substitution,
            })
        } else {
            Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                span: Some(self.create_file_span(None).to_source_span()),
                suggestions: vec!["Expected case expression".to_string()],
            }))
        }
    }

    /// Main iterative inference method (replaces recursive infer_expression)
    pub fn infer_expression(
        &mut self,
        expression: &mut Expression,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // CRITICAL FIX: Track inference depth for proper task state management
        self.inference_depth += 1;
        println!("🔥 INFER_EXPRESSION CALLED (depth={}) - Expression type: {:?}", 
            self.inference_depth, std::mem::discriminant(&expression.kind));
        
        // Execute main logic with proper cleanup
        let result = self.infer_expression_impl(expression, context);
        
        // CRITICAL FIX: Always decrement depth on exit (both success and error paths)
        self.inference_depth -= 1;
        println!("✅ INFER_EXPRESSION COMPLETED (depth={})", self.inference_depth);
        
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
                if let Type::Concrete { id, args, .. } = scrutinee_type {
                    if id.name() == "List" && args.len() == 1 {
                        let element_type = &args[0];
                        for element_pattern in &list_pattern.elements {
                            let element_bindings = self.extract_pattern_bindings(element_pattern, element_type)?;
                            bindings.extend(element_bindings);
                        }
                    }
                }
            }
            outrun_parser::Pattern::Tuple(_) => {
                // Tuple patterns - simplified for now
                // Would need to extract tuple element types
            }
        }
        
        Ok(bindings)
    }
    
    /// Recursive inference method for processing expressions within case clauses
    /// This is needed because the iterative system can't handle nested inference
    fn infer_expression_recursive(
        &mut self,
        expression: &mut outrun_parser::Expression,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // For now, use a simplified recursive approach
        // In a full implementation, this would need to handle all expression types
        match &expression.kind {
            outrun_parser::ExpressionKind::Identifier(name) => {
                if let Some(var_type) = context.bindings.get(&name.name) {
                    eprintln!("🔍 process_single_task Identifier: {} -> {}", name.name, var_type);
                    Ok(InferenceResult {
                        inferred_type: var_type.clone(),
                        constraints: vec![],
                        substitution: Substitution::new(),
                    })
                } else {
                    eprintln!("🔍 process_single_task Identifier: {} -> NOT FOUND in context bindings", name.name);
                    Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                        span: Some(self.create_file_span(None).to_source_span()),
                        suggestions: vec![format!("Unknown variable: {}", name.name)],
                    }))
                }
            }
            outrun_parser::ExpressionKind::Boolean(_) => {
                let boolean_type = Type::concrete("Boolean");
                Ok(InferenceResult {
                    inferred_type: boolean_type,
                    constraints: vec![],
                    substitution: Substitution::new(),
                })
            }
            outrun_parser::ExpressionKind::FunctionCall(func_call) => {
                // Handle function calls - this is where protocol dispatch happens
                self.infer_function_call_recursive(func_call, context)
            }
            _ => {
                // For other expression types, assign a fresh type variable
                let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
                Ok(InferenceResult {
                    inferred_type,
                    constraints: vec![],
                    substitution: Substitution::new(),
                })
            }
        }
    }
    
    /// Recursive function call inference for case clause processing
    fn infer_function_call_recursive(
        &mut self,
        func_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // This is a simplified implementation for protocol calls like Option.some?
        // Check if this is a guard function (ends with ?)
        match &func_call.path {
            outrun_parser::FunctionPath::Simple { name } => {
                if name.name.ends_with('?') {
                    // Guard function - must return Boolean
                    let boolean_type = Type::concrete("Boolean");
                    return Ok(InferenceResult {
                        inferred_type: boolean_type,
                        constraints: vec![],
                        substitution: Substitution::new(),
                    });
                }
            }
            outrun_parser::FunctionPath::Qualified { name, .. } => {
                if name.name.ends_with('?') {
                    // Guard function - must return Boolean
                    let boolean_type = Type::concrete("Boolean");
                    return Ok(InferenceResult {
                        inferred_type: boolean_type,
                        constraints: vec![],
                        substitution: Substitution::new(),
                    });
                }
            }
            outrun_parser::FunctionPath::Expression { .. } => {
                // Expression-based function calls - not handled for now
            }
        }
        
        // For other function calls, assign a fresh type variable
        let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
        Ok(InferenceResult {
            inferred_type,
            constraints: vec![],
            substitution: Substitution::new(),
        })
    }

    /// Implementation of inference logic using stack-based continuations
    fn infer_expression_impl(
        &mut self,
        expression: &mut Expression,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::ExpressionKind;
        
        let mut work_stack: Vec<WorkItem> = vec![WorkItem::EvaluateExpression {
            expression: expression as *mut Expression,
            context: context.clone(),
        }];
        let mut result_stack: Vec<InferenceResult> = Vec::new();
        
        while let Some(work_item) = work_stack.pop() {
            match work_item {
                WorkItem::EvaluateExpression { expression, context } => {
                    let expr_ref = unsafe { &mut *expression };
                    
                    match &mut expr_ref.kind {
                        // Leaf expressions - no dependencies, process immediately
                        ExpressionKind::Boolean(_) | ExpressionKind::Integer(_) | 
                        ExpressionKind::Float(_) | ExpressionKind::String(_) |
                        ExpressionKind::Atom(_) | ExpressionKind::Sigil(_) |
                        ExpressionKind::Identifier(_) | ExpressionKind::TypeIdentifier(_) |
                        ExpressionKind::QualifiedIdentifier(_) | ExpressionKind::FunctionCapture(_) => {
                            let result = self.infer_leaf_expression(expr_ref, &context)?;
                            result_stack.push(result);
                        },
                        
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
                                    outrun_parser::Argument::Named { expression: arg_expr, .. } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: arg_expr as *mut Expression,
                                            context: context.clone(),
                                        });
                                    },
                                    outrun_parser::Argument::Spread { expression: arg_expr, .. } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: arg_expr as *mut Expression,
                                            context: context.clone(),
                                        });
                                    },
                                }
                            }
                        },
                        
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
                                    },
                                    outrun_parser::ListElement::Spread(identifier) => {
                                        // Create a temporary identifier expression for the spread
                                        let identifier_expr = Expression {
                                            kind: outrun_parser::ExpressionKind::Identifier(identifier.clone()),
                                            span: identifier.span,
                                            type_info: None,
                                        };
                                        // For now, skip spread elements - they need special handling
                                        // TODO: Implement proper spread element type inference
                                    },
                                }
                            }
                        },
                        
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
                        },
                        
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
                                    },
                                    outrun_parser::MapEntry::Shorthand { value, .. } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: value.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    },
                                    outrun_parser::MapEntry::Spread(_) => {
                                        // Skip spread elements for now
                                        // TODO: Implement proper spread element type inference
                                    },
                                }
                            }
                        },
                        
                        ExpressionKind::Struct(struct_literal) => {
                            // Count only fields that have expressions to evaluate
                            let mut field_count = 0;
                            for field in &struct_literal.fields {
                                match field {
                                    outrun_parser::StructLiteralField::Assignment { .. } => field_count += 1,
                                    outrun_parser::StructLiteralField::Shorthand(_) => {}, // No expression to evaluate
                                    outrun_parser::StructLiteralField::Spread(_) => {}, // Skip spreads for now
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
                                    outrun_parser::StructLiteralField::Assignment { value, .. } => {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: value.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    },
                                    outrun_parser::StructLiteralField::Shorthand(_) => {
                                        // Shorthand fields don't have expressions to evaluate
                                        // The identifier itself is the value
                                    },
                                    outrun_parser::StructLiteralField::Spread(_) => {
                                        // Skip spread elements for now
                                        // TODO: Implement proper spread element type inference
                                    },
                                }
                            }
                        },
                        
                        ExpressionKind::BinaryOp(_) => {
                            unreachable!("Binary operations should be desugared before type checking");
                        },
                        
                        ExpressionKind::UnaryOp(_) => {
                            unreachable!("Unary operations should be desugared before type checking");
                        },
                        
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
                        },
                        
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
                        },
                        
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
                                    if let outrun_parser::StatementKind::Expression(expr) = &mut last_stmt.kind {
                                        work_stack.push(WorkItem::EvaluateExpression {
                                            expression: expr.as_mut() as *mut Expression,
                                            context: context.clone(),
                                        });
                                    }
                                }
                            }
                            
                            // Handle then block
                            if let Some(last_stmt) = if_expr.then_block.statements.last_mut() {
                                if let outrun_parser::StatementKind::Expression(expr) = &mut last_stmt.kind {
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
                        },
                        
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
                                    },
                                    outrun_parser::CaseResult::Block(block) => {
                                        // For now, treat blocks as single expressions (simplified)
                                        if let Some(last_stmt) = block.statements.last_mut() {
                                            if let outrun_parser::StatementKind::Expression(expr) = &mut last_stmt.kind {
                                                work_stack.push(WorkItem::EvaluateExpression {
                                                    expression: expr.as_mut() as *mut Expression,
                                                    context: context.clone(),
                                                });
                                            }
                                        }
                                    },
                                }
                            }
                            
                            // Push scrutinee evaluation
                            work_stack.push(WorkItem::EvaluateExpression {
                                expression: case_expr.expression.as_mut() as *mut Expression,
                                context: context.clone(),
                            });
                        },
                        
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
                                    },
                                    outrun_parser::AnonymousBody::Block(block) => {
                                        // For now, treat blocks as single expressions (simplified)
                                        if let Some(last_stmt) = block.statements.last_mut() {
                                            if let outrun_parser::StatementKind::Expression(expr) = &mut last_stmt.kind {
                                                work_stack.push(WorkItem::EvaluateExpression {
                                                    expression: expr.as_mut() as *mut Expression,
                                                    context: context.clone(),
                                                });
                                            }
                                        }
                                    },
                                }
                            }
                        },
                        
                        ExpressionKind::MacroInjection(_) => {
                            // Macro injections should be expanded before type checking
                            return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                                span: to_source_span(Some(expr_ref.span)),
                                suggestions: vec!["Macro injections should be expanded before type checking".to_string()],
                            }));
                        },
                    }
                },
                
                WorkItem::ContinueWithResults { expression, context, dependency_count } => {
                    // Collect dependency results from result_stack
                    let mut dependency_results = Vec::new();
                    for _ in 0..dependency_count {
                        if let Some(result) = result_stack.pop() {
                            dependency_results.push(result);
                        } else {
                            return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                                span: to_source_span(None),
                                suggestions: vec!["Missing dependency result".to_string()],
                            }));
                        }
                    }
                    dependency_results.reverse(); // Restore correct order
                    
                    // Process expression with dependency results
                    let expr_ref = unsafe { &mut *expression };
                    let result = self.infer_expression_with_dependencies(expr_ref, &context, &dependency_results)?;
                    result_stack.push(result);
                }
            }
        }
        
        // Return the final result
        result_stack.pop().ok_or_else(|| TypecheckError::InferenceError(
            InferenceError::AmbiguousType {
                span: to_source_span(Some(expression.span)),
                suggestions: vec!["No result produced".to_string()],
            }
        ))
    }

    /// Process leaf expressions that have no dependencies
    fn infer_leaf_expression(
        &mut self,
        expression: &mut Expression,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::ExpressionKind;
        
        match &expression.kind {
            ExpressionKind::Boolean(_) => {
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Boolean"),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            ExpressionKind::Integer(_) => {
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Integer64"),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            ExpressionKind::Float(_) => {
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Float64"),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            ExpressionKind::String(_) => {
                Ok(InferenceResult {
                    inferred_type: Type::concrete("String"),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            ExpressionKind::Atom(_) => {
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Atom"),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            ExpressionKind::Sigil(sigil_literal) => {
                // Sigil literals need special handling - they're processed by sigil protocols
                // For now, return a generic type - this should be enhanced later
                Ok(InferenceResult {
                    inferred_type: Type::concrete("String"), // Simplified - sigils often produce strings
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
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
                        Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                            span: to_source_span(Some(expression.span)),
                            suggestions: vec![format!("Unknown identifier: {}", identifier.name)],
                        }))
                    }
                }
            },
            ExpressionKind::TypeIdentifier(type_id) => {
                // Type identifiers represent types themselves
                // For now, treat them as concrete types
                Ok(InferenceResult {
                    inferred_type: Type::concrete(&type_id.name),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            ExpressionKind::QualifiedIdentifier(qualified_id) => {
                // Use existing qualified identifier inference
                let mut mutable_context = context.clone();
                self.infer_qualified_identifier(qualified_id, &mut mutable_context)
            },
            ExpressionKind::FunctionCapture(_) => {
                // Function captures create function types
                // For now, return a generic function type - this needs proper implementation
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Function"), // Simplified
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            _ => {
                Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(Some(expression.span)),
                    suggestions: vec!["Expression type not supported as leaf".to_string()],
                }))
            }
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
                self.infer_function_call_with_precomputed_args(func_call, &mut mutable_context, &arg_types)
            },
            
            ExpressionKind::List(list_literal) => {
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
                            return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                                span: to_source_span(Some(expression.span)),
                                suggestions: vec![format!("List element {} has type {}, but expected {}", 
                                    i, result.inferred_type, first_element_type)],
                            }));
                        }
                    }
                    
                    Ok(InferenceResult {
                        inferred_type: Type::generic_concrete("List", vec![first_element_type.clone()]),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                }
            },
            
            ExpressionKind::Tuple(tuple_literal) => {
                // Tuple type is the product of all element types
                let element_types: Vec<Type> = dependency_results
                    .iter()
                    .map(|result| result.inferred_type.clone())
                    .collect();
                
                Ok(InferenceResult {
                    inferred_type: Type::generic_concrete("Tuple", element_types),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            
            ExpressionKind::Map(map_literal) => {
                // Map type is Map<K, V> where K and V are inferred from entries
                if dependency_results.is_empty() {
                    // Empty map - use generic Map<K, V> with fresh type variables
                    let key_type = Type::variable(self.fresh_type_var(), Level(0));
                    let value_type = Type::variable(self.fresh_type_var(), Level(0));
                    Ok(InferenceResult {
                        inferred_type: Type::generic_concrete("Map", vec![key_type, value_type]),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                } else {
                    // Non-empty map - infer key and value types from first entry
                    // dependency_results are in pairs: [key1, value1, key2, value2, ...]
                    let first_key_type = &dependency_results[0].inferred_type;
                    let first_value_type = &dependency_results[1].inferred_type;
                    
                    // Check that all keys and values have compatible types
                    for chunk in dependency_results.chunks(2).skip(1) {
                        if chunk.len() == 2 {
                            if !self.types_are_compatible(first_key_type, &chunk[0].inferred_type) {
                                return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                                    span: to_source_span(Some(expression.span)),
                                    suggestions: vec![format!("Map key has type {}, but expected {}", 
                                        chunk[0].inferred_type, first_key_type)],
                                }));
                            }
                            if !self.types_are_compatible(first_value_type, &chunk[1].inferred_type) {
                                return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                                    span: to_source_span(Some(expression.span)),
                                    suggestions: vec![format!("Map value has type {}, but expected {}", 
                                        chunk[1].inferred_type, first_value_type)],
                                }));
                            }
                        }
                    }
                    
                    Ok(InferenceResult {
                        inferred_type: Type::generic_concrete("Map", vec![first_key_type.clone(), first_value_type.clone()]),
                        constraints: Vec::new(),
                        substitution: Substitution::new(),
                    })
                }
            },
            
            ExpressionKind::Struct(struct_literal) => {
                // Struct type is determined by the struct name and field types
                let struct_name = struct_literal.type_path.iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                
                // CRITICAL FIX: Use expected type for bidirectional inference of generic parameters
                if let Some(expected_type) = &context.expected_type {
                    // Check if the expected type matches this struct name
                    let expected_name = match expected_type {
                        Type::Concrete { id, .. } => Some(id.name()),
                        Type::Protocol { id, .. } => Some(id.name()),
                        _ => None,
                    };
                    
                    if let Some(expected_name) = expected_name {
                        if expected_name == struct_name {
                            println!("🚨🚨🚨 STRUCT LITERAL BIDIRECTIONAL INFERENCE 🚨🚨🚨");
                            println!("  Struct name: {}", struct_name);
                            println!("  Expected type: {:?}", expected_type);
                            println!("  Using expected type with generic parameters!");
                            
                            // Use the expected type which includes generic parameters
                            return Ok(InferenceResult {
                                inferred_type: expected_type.clone(),
                                constraints: Vec::new(),
                                substitution: Substitution::new(),
                            });
                        }
                    }
                    
                    // Expected type doesn't match, fall back to concrete type
                    println!("🔍 Struct literal: expected type {} doesn't match struct {}", expected_type, struct_name);
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
            },
            
            ExpressionKind::FieldAccess(field_access) => {
                // dependency_results[0] = object
                let object_type = &dependency_results[0].inferred_type;
                
                // Use existing field access inference
                let mut mutable_context = context.clone();
                self.infer_field_access_type(object_type, &field_access.field.name, &mut mutable_context)
            },
            
            ExpressionKind::Parenthesized(_) => {
                // Parenthesized expressions just pass through the inner type
                // dependency_results[0] = inner expression
                Ok(InferenceResult {
                    inferred_type: dependency_results[0].inferred_type.clone(),
                    constraints: dependency_results[0].constraints.clone(),
                    substitution: dependency_results[0].substitution.clone(),
                })
            },
            
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
                let boolean_type = Type::concrete("Boolean");
                if !self.types_are_compatible(condition_type, &boolean_type) {
                    return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                        span: to_source_span(Some(if_expr.condition.span)),
                        suggestions: vec![format!("Expected Boolean, found {}", condition_type)],
                    }));
                }
                
                // Determine result type
                let result_type = if let Some(else_type) = else_type {
                    // Both branches present - they must be compatible
                    if self.types_are_compatible(then_type, else_type) {
                        then_type.clone()
                    } else {
                        return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                            span: to_source_span(Some(expression.span)),
                            suggestions: vec![format!("If branches have incompatible types: {} vs {}", then_type, else_type)],
                        }));
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
            },
            
            ExpressionKind::CaseExpression(case_expr) => {
                // dependency_results[0] = scrutinee, [1..] = clause results
                let scrutinee_type = &dependency_results[0].inferred_type;
                
                if dependency_results.len() < 2 {
                    return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                        span: to_source_span(Some(expression.span)),
                        suggestions: vec!["Case expression must have at least one clause".to_string()],
                    }));
                }
                
                // All clause results should have compatible types
                let first_clause_type = &dependency_results[1].inferred_type;
                for (i, result) in dependency_results.iter().enumerate().skip(2) {
                    if !self.types_are_compatible(first_clause_type, &result.inferred_type) {
                        return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                            span: to_source_span(Some(expression.span)),
                            suggestions: vec![format!("Case clause {} has type {}, but expected {}", 
                                i - 1, result.inferred_type, first_clause_type)],
                        }));
                    }
                }
                
                // TODO: Add pattern matching validation against scrutinee_type
                
                Ok(InferenceResult {
                    inferred_type: first_clause_type.clone(),
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            
            ExpressionKind::AnonymousFunction(anon_fn) => {
                // Anonymous functions create function types
                // For now, create a generic function type
                // TODO: Infer proper parameter and return types from clauses
                
                if dependency_results.is_empty() {
                    return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                        span: to_source_span(Some(expression.span)),
                        suggestions: vec!["Anonymous function must have at least one clause".to_string()],
                    }));
                }
                
                // All clause bodies should have compatible return types
                let first_return_type = &dependency_results[0].inferred_type;
                for (i, result) in dependency_results.iter().enumerate().skip(1) {
                    if !self.types_are_compatible(first_return_type, &result.inferred_type) {
                        return Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                            span: to_source_span(Some(expression.span)),
                            suggestions: vec![format!("Function clause {} returns {}, but expected {}", 
                                i, result.inferred_type, first_return_type)],
                        }));
                    }
                }
                
                // Create a simplified function type
                // TODO: Extract proper parameter types from clause parameters
                Ok(InferenceResult {
                    inferred_type: Type::concrete("Function"), // Simplified
                    constraints: Vec::new(),
                    substitution: Substitution::new(),
                })
            },
            
            _ => {
                Err(TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(Some(expression.span)),
                    suggestions: vec!["Expression type not supported with dependencies".to_string()],
                }))
            }
        }
    }

    /// Collect all inference tasks through non-recursive tree traversal
    fn collect_inference_tasks(
        &mut self,
        root_expression: &mut Expression,
        root_context: InferenceContext,
    ) -> Result<TaskId, TypecheckError> {
        use outrun_parser::ExpressionKind;
        
        // DEBUG: Track task creation
        let start_tasks = self.inference_tasks.len();
        println!("🔍 Starting task collection (current tasks: {})", start_tasks);
        
        // Create root task
        let root_task_id = self.create_task(root_expression as *mut Expression, root_context);
        
        // Use work stack for non-recursive traversal (similar to desugaring pattern)
        let mut work_stack: Vec<&mut Expression> = vec![root_expression];
        
        while let Some(current_expr) = work_stack.pop() {
            let current_task_id = *self.expression_to_task_map.get(&(current_expr as *mut Expression))
                .expect("Expression should have corresponding task");
            
            // Add child expressions based on expression type and create dependencies
            match &mut current_expr.kind {
                ExpressionKind::FunctionCall(function_call) => {
                    // DEBUG: Function call task collection
                    let call_name = match &function_call.path {
                        outrun_parser::FunctionPath::Simple { name } => name.name.clone(),
                        outrun_parser::FunctionPath::Qualified { module, name } => format!("{}.{}", module.name, name.name),
                        _ => "dynamic".to_string(),
                    };
                    println!("🔧 Task collection for function call: {} with {} arguments (task_id: {})", call_name, function_call.arguments.len(), current_task_id);
                    
                    // Function call depends on all its arguments
                    for (i, argument) in function_call.arguments.iter_mut().enumerate() {
                        if let outrun_parser::Argument::Named { name, expression, .. } = argument {
                            println!("  📋 Creating dependency for argument {}: {} (type: {:?})", i, name.name, expression.kind);
                            
                            // Create task for argument expression
                            let arg_context = self.inference_tasks[&current_task_id].context.clone();
                            let arg_task_id = self.create_task(expression as *mut Expression, arg_context);
                            
                            // Function call depends on its argument
                            println!("    ➕ Adding dependency: {} -> {}", current_task_id, arg_task_id);
                            self.add_task_dependency(current_task_id, arg_task_id);
                            
                            // Add argument to work stack for further processing
                            work_stack.push(expression);
                        }
                    }
                }
                ExpressionKind::List(list_literal) => {
                    // List depends on all its elements
                    for element in &mut list_literal.elements {
                        if let outrun_parser::ListElement::Expression(expr) = element {
                            let elem_context = self.inference_tasks[&current_task_id].context.clone();
                            let elem_task_id = self.create_task(expr.as_mut() as *mut Expression, elem_context);
                            
                            self.add_task_dependency(current_task_id, elem_task_id);
                            work_stack.push(expr);
                        }
                    }
                }
                ExpressionKind::Tuple(tuple_literal) => {
                    // Tuple depends on all its elements
                    for element in &mut tuple_literal.elements {
                        let elem_context = self.inference_tasks[&current_task_id].context.clone();
                        let elem_task_id = self.create_task(element as *mut Expression, elem_context);
                        
                        self.add_task_dependency(current_task_id, elem_task_id);
                        work_stack.push(element);
                    }
                }
                ExpressionKind::Map(map_literal) => {
                    // Map depends on all its key-value pairs
                    for entry in &mut map_literal.entries {
                        match entry {
                            outrun_parser::MapEntry::Assignment { key, value } => {
                                let key_context = self.inference_tasks[&current_task_id].context.clone();
                                let value_context = self.inference_tasks[&current_task_id].context.clone();
                                
                                let key_task_id = self.create_task(key.as_mut() as *mut Expression, key_context);
                                let value_task_id = self.create_task(value.as_mut() as *mut Expression, value_context);
                                
                                self.add_task_dependency(current_task_id, key_task_id);
                                self.add_task_dependency(current_task_id, value_task_id);
                                
                                work_stack.push(key);
                                work_stack.push(value);
                            }
                            outrun_parser::MapEntry::Shorthand { value, .. } => {
                                let value_context = self.inference_tasks[&current_task_id].context.clone();
                                let value_task_id = self.create_task(value.as_mut() as *mut Expression, value_context);
                                
                                self.add_task_dependency(current_task_id, value_task_id);
                                work_stack.push(value);
                            }
                            outrun_parser::MapEntry::Spread(_) => {
                                // Spread entries don't have expressions to process for now
                            }
                        }
                    }
                }
                ExpressionKind::AnonymousFunction(anon_fn) => {
                    // Anonymous function depends on its clause bodies and guards
                    for clause in &mut anon_fn.clauses {
                        // Process guard expression if present
                        if let Some(ref mut guard) = clause.guard {
                            let guard_context = self.inference_tasks[&current_task_id].context.clone();
                            let guard_task_id = self.create_task(guard as *mut Expression, guard_context);
                            
                            self.add_task_dependency(current_task_id, guard_task_id);
                            work_stack.push(guard);
                        }
                        
                        // Process body expression if it's an expression
                        if let outrun_parser::AnonymousBody::Expression(ref mut body_expr) = clause.body {
                            let body_context = self.inference_tasks[&current_task_id].context.clone();
                            let body_task_id = self.create_task(body_expr.as_mut() as *mut Expression, body_context);
                            
                            self.add_task_dependency(current_task_id, body_task_id);
                            work_stack.push(body_expr);
                        }
                    }
                }
                ExpressionKind::Parenthesized(inner_expr) => {
                    // Parenthesized expression depends on its inner expression
                    let inner_context = self.inference_tasks[&current_task_id].context.clone();
                    let inner_task_id = self.create_task(inner_expr.as_mut() as *mut Expression, inner_context);
                    
                    self.add_task_dependency(current_task_id, inner_task_id);
                    work_stack.push(inner_expr);
                }
                ExpressionKind::FieldAccess(field_access) => {
                    // Field access depends on its object expression
                    let object_context = self.inference_tasks[&current_task_id].context.clone();
                    let object_task_id = self.create_task(field_access.object.as_mut() as *mut Expression, object_context);
                    
                    self.add_task_dependency(current_task_id, object_task_id);
                    work_stack.push(&mut field_access.object);
                }
                ExpressionKind::Struct(struct_literal) => {
                    // Struct literal depends on all its field values
                    for field in &mut struct_literal.fields {
                        match field {
                            outrun_parser::StructLiteralField::Assignment { value, .. } => {
                                let field_context = self.inference_tasks[&current_task_id].context.clone();
                                let field_task_id = self.create_task(value.as_mut() as *mut Expression, field_context);
                                
                                self.add_task_dependency(current_task_id, field_task_id);
                                work_stack.push(value);
                            }
                            outrun_parser::StructLiteralField::Shorthand(_) => {
                                // Shorthand fields don't have expressions to process
                            }
                            outrun_parser::StructLiteralField::Spread(_) => {
                                // Spread fields don't have expressions to process for now
                            }
                        }
                    }
                }
                ExpressionKind::IfExpression(if_expr) => {
                    // If expression depends on condition and both branches
                    let condition_context = self.inference_tasks[&current_task_id].context.clone();
                    let condition_task_id = self.create_task(if_expr.condition.as_mut() as *mut Expression, condition_context);
                    
                    self.add_task_dependency(current_task_id, condition_task_id);
                    work_stack.push(&mut if_expr.condition);
                    
                    // CRITICAL FIX: Process then and else blocks for proper if expression type inference
                    // The then block is required
                    if let Some(then_expr) = if_expr.then_block.statements.last_mut() {
                        if let outrun_parser::StatementKind::Expression(expr) = &mut then_expr.kind {
                            let then_context = self.inference_tasks[&current_task_id].context.clone();
                            let then_task_id = self.create_task(expr.as_mut() as *mut Expression, then_context);
                            
                            self.add_task_dependency(current_task_id, then_task_id);
                            work_stack.push(expr.as_mut());
                        }
                    }
                    
                    // The else block is optional
                    if let Some(else_block) = &mut if_expr.else_block {
                        if let Some(else_expr) = else_block.statements.last_mut() {
                            if let outrun_parser::StatementKind::Expression(expr) = &mut else_expr.kind {
                                let else_context = self.inference_tasks[&current_task_id].context.clone();
                                let else_task_id = self.create_task(expr.as_mut() as *mut Expression, else_context);
                                
                                self.add_task_dependency(current_task_id, else_task_id);
                                work_stack.push(expr.as_mut());
                            }
                        }
                    }
                }
                ExpressionKind::CaseExpression(case_expr) => {
                    println!("  📋 Setting up case expression task dependencies (ONLY scrutinee)");
                    // IMPORTANT: For case expressions, we only process the scrutinee upfront
                    // Guard expressions and clause results will be processed later with pattern bindings in scope
                    
                    let scrutinee_context = self.inference_tasks[&current_task_id].context.clone();
                    let scrutinee_task_id = self.create_task(case_expr.expression.as_mut() as *mut Expression, scrutinee_context);
                    
                    self.add_task_dependency(current_task_id, scrutinee_task_id);
                    work_stack.push(&mut case_expr.expression);
                    println!("    ✅ Added scrutinee dependency: task {:?} depends on task {:?}", current_task_id, scrutinee_task_id);
                    
                    // NOTE: We deliberately DO NOT process guard expressions here
                    // They will be processed in infer_case_expression_with_processed_clauses
                    // with the proper pattern bindings in scope
                }
                // Simple literals and identifiers have no dependencies
                ExpressionKind::Boolean(_) | ExpressionKind::Integer(_) | ExpressionKind::Float(_) 
                | ExpressionKind::String(_) | ExpressionKind::Atom(_) | ExpressionKind::Sigil(_)
                | ExpressionKind::Identifier(_) | ExpressionKind::QualifiedIdentifier(_) | ExpressionKind::TypeIdentifier(_)
                | ExpressionKind::MacroInjection(_) | ExpressionKind::FunctionCapture(_) => {
                    // No child expressions to process
                }
                // Binary/Unary operations should be desugared before reaching here
                ExpressionKind::BinaryOp(_) | ExpressionKind::UnaryOp(_) => {
                    // These should not appear in type inference after desugaring
                }
            }
        }
        
        // DEBUG: Report task collection results
        let end_tasks = self.inference_tasks.len();
        let created_tasks = end_tasks - start_tasks;
        println!("🔍 Task collection complete: created {} tasks (total: {})", created_tasks, end_tasks);
        println!("🔍 Expression map size: {}", self.expression_to_task_map.len());
        
        Ok(root_task_id)
    }

    /// Process a single task (this replaces the recursive logic in each expression case)
    fn process_single_task(&mut self, task_id: TaskId) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::ExpressionKind;
        
        let task = self.inference_tasks.get_mut(&task_id).unwrap();
        task.state = TaskState::Processing;
        
        let expression = unsafe { &mut *task.expression };
        let context = task.context.clone(); // Clone to avoid borrow issues
        
        // Debug: Check context during task processing
        if let ExpressionKind::FunctionCall(func_call) = &expression.kind {
            let func_name = match &func_call.path {
                outrun_parser::FunctionPath::Simple { name } => name.name.clone(),
                outrun_parser::FunctionPath::Qualified { module, name } => format!("{}.{}", module.name, name.name),
                outrun_parser::FunctionPath::Expression { .. } => "dynamic".to_string(),
            };
            if func_name.contains("list_head") {
                eprintln!("🔍 TASK PROCESSING: Processing {} call (task {})", func_name, task_id);
                eprintln!("  📋 Current self context: {:?}", self.current_self_context);
                eprintln!("  📋 Task context: {:?}", context.self_binding);
            }
        }
        
        // STRICT CHECK: No binary or unary operations should reach type inference
        // If they do, it indicates a bug in the desugaring phase
        match &expression.kind {
            ExpressionKind::BinaryOp(binary_op) => {
                eprintln!("🚨 DESUGARING BUG: Binary operation {:?} at span {:?} was not desugared!", 
                         binary_op.operator, expression.span);
                eprintln!("   This indicates the desugaring engine missed this expression.");
                eprintln!("   All binary operations should be converted to protocol calls before type inference.");
                eprintln!("   DEBUGGING INFO:");
                eprintln!("     - Operator: {:?}", binary_op.operator);  
                eprintln!("     - Span: start={}, end={}", expression.span.start, expression.span.end);
                eprintln!("     - Line info: {:?}", expression.span.start_line_col);
                eprintln!("   IMPORTANT: This is occurring during core library precompilation!");
                eprintln!("   The user test code contains no subtract operations.");
                eprintln!("   The subtract operation is likely being generated during:");
                eprintln!("     1. Macro expansion creating new AST nodes");
                eprintln!("     2. Some AST transformation step");
                eprintln!("     3. An AST node type not being traversed by desugaring");
                
                let error = TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(Some(expression.span)),
                    suggestions: vec![format!("Binary operation '{:?}' was not desugared. This is a bug in the desugaring phase.", binary_op.operator)],
                });
                let task_error = TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(Some(expression.span)),
                    suggestions: vec![format!("Binary operation '{:?}' was not desugared. This is a bug in the desugaring phase.", binary_op.operator)],
                });
                let task = self.inference_tasks.get_mut(&task_id).unwrap();
                task.state = TaskState::Failed(task_error);
                return Err(error);
            }
            ExpressionKind::UnaryOp(unary_op) => {
                eprintln!("🚨 DESUGARING BUG: Unary operation {:?} at span {:?} was not desugared!", 
                         unary_op.operator, expression.span);
                eprintln!("   This indicates the desugaring engine missed this expression.");
                eprintln!("   All unary operations should be converted to protocol calls before type inference.");
                
                let error = TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(Some(expression.span)),
                    suggestions: vec![format!("Unary operation '{:?}' was not desugared. This is a bug in the desugaring phase.", unary_op.operator)],
                });
                let task_error = TypecheckError::InferenceError(InferenceError::AmbiguousType {
                    span: to_source_span(Some(expression.span)),
                    suggestions: vec![format!("Unary operation '{:?}' was not desugared. This is a bug in the desugaring phase.", unary_op.operator)],
                });
                let task = self.inference_tasks.get_mut(&task_id).unwrap();
                task.state = TaskState::Failed(task_error);
                return Err(error);
            }
            _ => {} // OK - no undesugared operations
        }
        
        let result = match &mut expression.kind {
            ExpressionKind::Boolean(_) => {
                let inferred_type = Type::concrete("Outrun.Core.Boolean");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Integer(_) => {
                let inferred_type = Type::concrete("Outrun.Core.Integer64");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Float(_) => {
                let inferred_type = Type::concrete("Outrun.Core.Float64");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::String(_) => {
                let inferred_type = Type::concrete("Outrun.Core.String");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Atom(_) => {
                let inferred_type = Type::concrete("Outrun.Core.Atom");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Identifier(identifier) => {
                // Variable lookup - use existing method
                let mut mutable_context = context;
                self.infer_variable(identifier, &mut mutable_context)
            }
            ExpressionKind::FunctionCall(function_call) => {
                // Function call - all arguments should be processed by now
                // Get results for arguments and call existing function call logic
                self.infer_function_call_with_processed_args(function_call, task_id, &context)
            }
            ExpressionKind::List(list_literal) => {
                // List literal - all elements should be processed by now
                self.infer_list_literal_with_processed_elements(list_literal, task_id, &context)
            }
            ExpressionKind::Tuple(tuple_literal) => {
                // Tuple literal - all elements should be processed by now
                self.infer_tuple_literal_with_processed_elements(tuple_literal, task_id, &context)
            }
            ExpressionKind::Map(map_literal) => {
                // Map literal - all key-value pairs should be processed by now
                self.infer_map_literal_with_processed_entries(map_literal, task_id, &context)
            }
            ExpressionKind::Parenthesized(_) => {
                // Parenthesized expression - dependency should be processed by now
                self.infer_parenthesized_expression_with_processed_inner(task_id, &context)
            }
            ExpressionKind::QualifiedIdentifier(qualified_id) => {
                // Qualified identifier - no dependencies, can process directly  
                let mut mutable_context = context;
                self.infer_qualified_identifier(qualified_id, &mut mutable_context)
            }
            ExpressionKind::FieldAccess(_) => {
                // Field access - object expression should be processed by now
                self.infer_field_access_with_processed_object(task_id, &context)
            }
            ExpressionKind::AnonymousFunction(anonymous_fn) => {
                // Anonymous function - all clause bodies/guards should be processed by now
                self.infer_anonymous_function_with_processed_clauses(anonymous_fn, task_id, &context)
            }
            ExpressionKind::Sigil(_) => {
                // Sigil literals have concrete type based on sigil type
                let inferred_type = Type::concrete("Outrun.Core.String"); // Most sigils are strings
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Struct(_) => {
                // Struct literals - all field values should be processed by now
                self.infer_struct_literal_with_processed_fields(task_id, &context)
            }
            ExpressionKind::TypeIdentifier(type_id) => {
                // Type identifiers reference types directly
                let inferred_type = Type::concrete(&type_id.name);
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::IfExpression(_) => {
                // If expressions - condition and branches should be processed by now
                self.infer_if_expression_with_processed_branches(task_id, &context)
            }
            ExpressionKind::CaseExpression(_) => {
                // Case expressions - scrutinee and clause results should be processed by now
                self.infer_case_expression_with_processed_clauses(task_id, &context)
            }
            ExpressionKind::MacroInjection(_) => {
                // Macro injections - assign fresh type variable for now
                let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::FunctionCapture(_) => {
                // Function captures - assign function type for now
                let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::BinaryOp(_) => {
                unreachable!("Binary operations should have been caught by desugaring bug check above")
            }
            ExpressionKind::UnaryOp(_) => {
                unreachable!("Unary operations should have been caught by desugaring bug check above")  
            }
        };
        
        result
    }

    /// Core function body type checking logic using RefCell for interior mutability
    #[allow(clippy::result_large_err)]
    fn typecheck_function_body(
        &mut self,
        scope: &str,
        function: &FunctionDefinition,
    ) -> Result<(), TypecheckError> {
        eprintln!("🔍 FUNCTION TYPECHECK: Starting {}.{}", scope, function.name.name);
        eprintln!("  📋 Current self context: {:?}", self.current_self_context);
        
        // Convert the declared return type for bidirectional inference
        let declared_return_type = self.convert_type_annotation(&function.return_type)?;
        
        // Create a new inference context for this function with expected return type
        let mut function_context = InferenceContext {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: Some(declared_return_type.clone()), // CRITICAL FIX: Set expected type for bidirectional inference
            self_binding: SelfBindingContext::ProtocolDefinition {
                protocol_id: ProtocolId::new(scope),
                protocol_args: vec![],
            },
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

        // declared_return_type was already converted above for bidirectional inference

        // CRITICAL DEBUG: Log detailed type information for any ceil function
        if function.name.name == "ceil" {
            println!("🚨 CRITICAL DEBUG: {}.{} function body type checking", scope, function.name.name);
            println!("  Declared return type: {:?}", declared_return_type);
            println!("  Inferred body type: {:?}", body_type);
            println!("  Function parameters: {:?}", function.parameters.iter().map(|p| (&p.name.name, &p.type_annotation)).collect::<Vec<_>>());
            println!("  Symbol table: {:?}", self.symbol_table);
        }

        // Verify that the body type matches the declared return type
        // WORKAROUND: Use never type compatibility check
        // TODO: Replace with proper attribute system when macro system is implemented
        if !self.types_are_compatible_with_never(&declared_return_type, &body_type) {
            // CRITICAL DEBUG: Log the failing function
            println!("🚨 TYPE MISMATCH ERROR in {}.{}", scope, function.name.name);
            println!("  Expected: {:?}", declared_return_type);
            println!("  Found: {:?}", body_type);
            
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
                protocol_id: ProtocolId::new(scope),
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
        for statement in &block.statements {
            match &statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    // Create RefCell wrapper for the expression to enable mutability
                    let expr_cell = Rc::new(RefCell::new((**expr).clone()));
                    let result = {
                        let mut expr_mut = expr_cell.borrow_mut();
                        self.infer_expression(&mut expr_mut, context)?
                    };
                    last_type = Some(result.inferred_type);
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    // Type check the let binding expression and add to symbol table
                    self.typecheck_let_binding_statement_readonly(let_binding, context)?;
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
    fn types_are_compatible(&self, found_type: &Type, expected_type: &Type) -> bool {
        use crate::types::Type;

        match (found_type, expected_type) {
            // Same concrete types are compatible
            (
                Type::Concrete {
                    id: id1,
                    args: args1,
                    ..
                },
                Type::Concrete {
                    id: id2,
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
                    id: concrete_type, ..
                },
                Type::Protocol { id: protocol, .. },
            ) => {
                // Check if the concrete type implements the protocol (with all requirements)
                self.protocol_registry()
                    .type_satisfies_protocol(concrete_type, protocol)
            }

            // Protocol satisfied by concrete type: check if concrete type implements the protocol
            (
                Type::Protocol { id: protocol, .. },
                Type::Concrete {
                    id: concrete_type, ..
                },
            ) => {
                // Check if the concrete type implements the protocol (reverse direction)
                self.protocol_registry()
                    .type_satisfies_protocol(concrete_type, protocol)
            }

            // Two protocol types: check if they're the same protocol with compatible arguments
            (
                Type::Protocol {
                    id: id1,
                    args: args1,
                    ..
                },
                Type::Protocol {
                    id: id2,
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
                eprintln!("🔍 types_are_compatible: SelfType vs {}", other);
                // Resolve Self to its concrete type and check compatibility
                if let Some(resolved_self) = found_type.resolve_self() {
                    eprintln!("🔍 types_are_compatible: Self resolved to {}, checking compatibility", resolved_self);
                    self.types_are_compatible(&resolved_self, other)
                } else {
                    eprintln!("🔍 types_are_compatible: Self could not be resolved");
                    // Self is unbound (e.g., in protocol definition context)
                    false
                }
            }
            (other, Type::SelfType { .. }) => {
                eprintln!("🔍 types_are_compatible: {} vs SelfType", other);
                // Resolve Self to its concrete type and check compatibility
                if let Some(resolved_self) = expected_type.resolve_self() {
                    eprintln!("🔍 types_are_compatible: Self resolved to {}, checking compatibility", resolved_self);
                    self.types_are_compatible(other, &resolved_self)
                } else {
                    eprintln!("🔍 types_are_compatible: Self could not be resolved");
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

            // Different categories are incompatible
            _ => false,
        }
    }

    /// Infer the type of an expression
    #[allow(clippy::result_large_err)]

    /// Infer the type of a variable (identifier lookup)
    #[allow(clippy::result_large_err)]
    fn infer_variable(
        &mut self,
        identifier: &outrun_parser::Identifier,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        let var_name = &identifier.name;

        // First check local context bindings
        if let Some(var_type) = context.lookup_variable(var_name) {
            return Ok(InferenceResult {
                inferred_type: var_type.clone(),
                constraints: context.constraints.clone(),
                substitution: context.substitution.clone(),
            });
        }

        // Then check the symbol table
        if let Some(var_type) = self.symbol_table.get(var_name) {
            return Ok(InferenceResult {
                inferred_type: var_type.clone(),
                constraints: context.constraints.clone(),
                substitution: context.substitution.clone(),
            });
        }

        // Variable not found - create enhanced error with suggestions
        self.update_error_context();
        Err(TypecheckError::InferenceError(
            InferenceError::undefined_variable_with_suggestions(
                var_name.clone(),
                Some(identifier.span),
                &self.error_context,
            ),
        ))
    }


    /// Phase 3: Static Clause Elimination
    /// Eliminate clauses that can never match based on static type and value analysis
    fn eliminate_impossible_clauses(
        &mut self,
        clauses: &[crate::universal_dispatch::ClauseId],
        arguments: &[outrun_parser::Argument],
        context: &mut InferenceContext,
    ) -> Result<Vec<crate::universal_dispatch::ClauseId>, TypecheckError> {
        if clauses.is_empty() {
            return Ok(Vec::new());
        }

        // Step 1: Infer argument types once for all clauses
        let arg_types = self.infer_argument_types(arguments, context)?;

        // Step 2: Filter clauses that could possibly match
        let mut viable_clauses = Vec::new();
        
        for &clause_id in clauses {
            if let Some(clause) = self.universal_dispatch_registry.get_clause(clause_id) {
                // Conservative elimination: only eliminate clauses we're 100% sure can't match
                match self.clause_could_possibly_match(clause, &arg_types, context) {
                    Ok(true) => {
                        viable_clauses.push(clause_id);
                    }
                    Ok(false) => {
                        // Clause can never match - eliminated!
                    }
                    Err(_) => {
                        // If we can't determine, conservatively keep the clause
                        viable_clauses.push(clause_id);
                    }
                }
            } else {
                // Clause not found - keep it to avoid breaking things
                viable_clauses.push(clause_id);
            }
        }

        // Preserve source order of viable clauses
        Ok(viable_clauses)
    }

    /// Check if a clause could possibly match the given argument types
    fn clause_could_possibly_match(
        &self,
        clause: &crate::universal_dispatch::ClauseInfo,
        arg_types: &[Type],
        context: &InferenceContext,
    ) -> Result<bool, TypecheckError> {
        // Analyze each guard in the clause
        for guard in &clause.guards {
            match guard {
                crate::universal_dispatch::Guard::TypeCompatible { 
                    target_type, 
                    implementing_type, 
                    .. 
                } => {
                    // Static type compatibility check
                    if !self.types_could_be_compatible(target_type, implementing_type)? {
                        return Ok(false); // Clause can never match due to type incompatibility
                    }
                }
                crate::universal_dispatch::Guard::ValueGuard { expression, .. } => {
                    // Try to evaluate guard expression statically
                    match self.evaluate_guard_statically(expression, arg_types, context) {
                        Ok(Some(false)) => {
                            return Ok(false); // Guard always fails
                        }
                        Ok(Some(true)) => {
                            // Guard always passes - continue checking other guards
                        }
                        Ok(None) | Err(_) => {
                            // Can't determine statically - assume it could match
                        }
                    }
                }
                crate::universal_dispatch::Guard::AlwaysTrue => {
                    // Always matches - continue
                }
            }
        }
        
        Ok(true) // Clause could potentially match
    }

    /// Check if two types could potentially be compatible
    fn types_could_be_compatible(
        &self,
        type1: &Type,
        type2: &Type,
    ) -> Result<bool, TypecheckError> {
        // Use existing type compatibility logic but in a more permissive way
        // This is a conservative check - we only eliminate obvious impossibilities
        
        match (type1, type2) {
            // Same concrete types are always compatible
            (Type::Concrete { id: id1, .. }, Type::Concrete { id: id2, .. }) => {
                Ok(self.type_ids_are_equivalent(id1, id2))
            }
            // Variables could unify with anything
            (Type::Variable { .. }, _) | (_, Type::Variable { .. }) => Ok(true),
            // Self types could resolve to anything
            (Type::SelfType { .. }, _) | (_, Type::SelfType { .. }) => Ok(true),
            // Protocol types could be implemented by concrete types
            (Type::Protocol { .. }, Type::Concrete { .. }) => Ok(true),
            (Type::Concrete { .. }, Type::Protocol { .. }) => Ok(true),
            // Same protocol types could be compatible
            (Type::Protocol { id: id1, .. }, Type::Protocol { id: id2, .. }) => {
                Ok(id1 == id2)
            }
            // Function types - basic compatibility check
            (Type::Function { .. }, Type::Function { .. }) => Ok(true),
            // Different concrete types are incompatible
            _ => Ok(false),
        }
    }

    /// Try to evaluate a guard expression statically
    fn evaluate_guard_statically(
        &self,
        expression: &outrun_parser::Expression,
        _arg_types: &[Type],
        _context: &InferenceContext,
    ) -> Result<Option<bool>, TypecheckError> {
        // Static evaluation of common guard patterns
        match &expression.kind {
            // Literal boolean values
            outrun_parser::ExpressionKind::Boolean(bool_lit) => {
                Ok(Some(bool_lit.value))
            }
            // Function calls (desugared operators)
            outrun_parser::ExpressionKind::FunctionCall(func_call) => {
                match &func_call.path {
                    outrun_parser::FunctionPath::Qualified { module, name } => {
                        // Handle common comparison operators with literal values
                        if module.name == "Equality" && name.name == "not_equal?" {
                            // Check for patterns like "x != 0" where one argument is literal
                            self.evaluate_static_not_equal(&func_call.arguments)
                        } else if module.name == "Equality" && name.name == "equal?" {
                            // Check for patterns like "x == 0" where one argument is literal
                            self.evaluate_static_equal(&func_call.arguments)
                        } else {
                            // Can't evaluate other function calls statically
                            Ok(None)
                        }
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None), // Can't evaluate other expression types statically
        }
    }

    /// Evaluate static "not equal" comparisons
    fn evaluate_static_not_equal(
        &self,
        arguments: &[outrun_parser::Argument],
    ) -> Result<Option<bool>, TypecheckError> {
        if arguments.len() != 2 {
            return Ok(None);
        }

        let (left_lit, right_lit) = self.extract_literal_arguments(arguments)?;
        
        match (left_lit, right_lit) {
            (Some(left), Some(right)) => {
                // Both sides are literals - can evaluate statically
                Ok(Some(left != right))
            }
            _ => Ok(None), // Can't evaluate if not both literals
        }
    }

    /// Evaluate static "equal" comparisons
    fn evaluate_static_equal(
        &self,
        arguments: &[outrun_parser::Argument],
    ) -> Result<Option<bool>, TypecheckError> {
        if arguments.len() != 2 {
            return Ok(None);
        }

        let (left_lit, right_lit) = self.extract_literal_arguments(arguments)?;
        
        match (left_lit, right_lit) {
            (Some(left), Some(right)) => {
                // Both sides are literals - can evaluate statically
                Ok(Some(left == right))
            }
            _ => Ok(None), // Can't evaluate if not both literals
        }
    }

    /// Extract literal values from function arguments for static evaluation
    fn extract_literal_arguments(
        &self,
        arguments: &[outrun_parser::Argument],
    ) -> Result<(Option<LiteralValue>, Option<LiteralValue>), TypecheckError> {
        let mut literals = Vec::new();
        
        for arg in arguments {
            let expr = match arg {
                outrun_parser::Argument::Named { expression, .. } => expression,
                outrun_parser::Argument::Spread { expression, .. } => expression,
            };
            
            literals.push(self.extract_literal_value(expr));
        }
        
        Ok((
            literals.first().cloned().flatten(),
            literals.get(1).cloned().flatten(),
        ))
    }

    /// Extract a literal value from an expression for static evaluation
    fn extract_literal_value(&self, expr: &outrun_parser::Expression) -> Option<LiteralValue> {
        match &expr.kind {
            outrun_parser::ExpressionKind::Integer(int_lit) => {
                Some(LiteralValue::Integer(int_lit.value))
            }
            outrun_parser::ExpressionKind::Float(float_lit) => {
                Some(LiteralValue::Float(float_lit.value))
            }
            outrun_parser::ExpressionKind::String(str_lit) => {
                // For now, only handle simple string literals (not interpolated)
                if str_lit.parts.len() == 1 {
                    if let outrun_parser::StringPart::Text { content, .. } = &str_lit.parts[0] {
                        Some(LiteralValue::String(content.clone()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            outrun_parser::ExpressionKind::Boolean(bool_lit) => {
                Some(LiteralValue::Boolean(bool_lit.value))
            }
            _ => None,
        }
    }

    /// Infer types of function call arguments
    fn infer_argument_types(
        &mut self,
        arguments: &[outrun_parser::Argument],
        context: &mut InferenceContext,
    ) -> Result<Vec<Type>, TypecheckError> {
        let mut arg_types = Vec::new();
        
        for argument in arguments {
            let expr = match argument {
                outrun_parser::Argument::Named { expression, .. } => expression,
                outrun_parser::Argument::Spread { expression, .. } => expression,
            };
            
            // Create a mutable clone for type inference
            let mut expr_clone = expr.clone();
            let result = self.infer_expression(&mut expr_clone, context)?;
            arg_types.push(result.inferred_type);
        }
        
        Ok(arg_types)
    }

    /// Resolve simple function calls to clause lists
    fn resolve_simple_function_clauses(
        &mut self,
        function_name: &str,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<crate::typed_ast::UniversalCallResolution, TypecheckError> {
        println!("🔥🔥🔥 RESOLVE_SIMPLE_FUNCTION_CLAUSES: {} 🔥🔥🔥", function_name);
        // Use existing dispatcher logic but convert to universal result
        let function_context = self.create_function_context_from_inference_context(context);
        let dispatcher = FunctionDispatcher::new(self.protocol_registry(), &self.function_registry, None, None)
            .with_context(function_context);

        match dispatcher.resolve_local_call(function_name, Some(function_call.span)) {
            Ok(crate::dispatch::DispatchResult::Resolved(resolved_func)) => {
                // Single resolved function - create single clause
                let clause_id = crate::universal_dispatch::ClauseId::new();
                
                // CRITICAL FIX: Apply generic parameter substitution for local function calls
                // This resolves the issue where local functions returning Option<T> don't get
                // their generic parameters substituted (e.g., Option<T> -> Option<Integer>)
                let return_type = self.substitute_local_function_generics(
                    &resolved_func.function_info.return_type,
                    function_name,
                    &resolved_func.function_info.parameters,
                    function_call,
                    context
                )?;
                
                // Register this clause in the universal dispatch registry
                self.register_function_clause(clause_id, &resolved_func);
                
                Ok(crate::typed_ast::UniversalCallResolution::single(clause_id, return_type))
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
                    let clause_id = crate::universal_dispatch::ClauseId::new();
                    self.register_function_clause(clause_id, &candidate);
                    clause_ids.push(clause_id);
                }
                
                Ok(crate::typed_ast::UniversalCallResolution::multi(clause_ids, return_type))
            }
            Ok(crate::dispatch::DispatchResult::NotFound) => {
                // Function not found - create error clause
                return Err(TypecheckError::DispatchError(crate::error::DispatchError::NoImplementation {
                    protocol_name: "unknown".to_string(),
                    type_name: function_name.to_string(),
                    file_span: Some(self.create_file_span(Some(function_call.span))),
                    similar_implementations: Vec::new(),
                    suggestions: Vec::new(),
                }));
            }
            Err(dispatch_error) => {
                Err(TypecheckError::DispatchError(dispatch_error))
            }
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
        let qualified_start = std::time::Instant::now();
        println!("🔥🔥🔥 RESOLVE_QUALIFIED_FUNCTION_CLAUSES: {}.{} ({} arg types) 🔥🔥🔥", module_name, function_name, precomputed_arg_types.len());
        
        // Special debugging for Option.some? calls
        if module_name == "Option" && function_name == "some?" {
            println!("              🚨 CRITICAL: Option.some? call detected!");
            println!("                Precomputed arg types: {:?}", precomputed_arg_types);
            for (i, arg_type) in precomputed_arg_types.iter().enumerate() {
                println!("                  Arg {}: {:?}", i, arg_type);
            }
        }
        
        // CRITICAL DEBUG: Log every Option.unwrap call
        if module_name == "Option" && function_name == "unwrap" {
            println!("              🚨 CRITICAL: Option.unwrap call detected!");
            println!("                Precomputed arg types: {:?}", precomputed_arg_types);
        }
        
        let qualified_name = format!("{}.{}", module_name, function_name);
        let protocol_id = crate::types::ProtocolId::new(module_name);
        
        // Check if this is a protocol call
        let target_type_start = std::time::Instant::now();
        let target_type = if self.protocol_registry().has_protocol(&protocol_id) {
            println!("                🎯 Protocol call - resolving target type with precomputed args");
            let resolved_target = self.resolve_protocol_call_target_type(&qualified_name, function_call, context, Some(precomputed_arg_types))?;
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
            println!("                📦 Non-protocol call");
            None
        };
        let target_type_time = target_type_start.elapsed();
        if target_type_time.as_millis() > 1 {
            println!("                ⏱️  Target type resolution with precomputed args: {:.2?}", target_type_time);
        }

        let dispatcher_start = std::time::Instant::now();
        let dispatcher = FunctionDispatcher::new(self.protocol_registry(), &self.function_registry, None, None);
        
        let resolve_start = std::time::Instant::now();
        let result = match dispatcher.resolve_qualified_call(&qualified_name, target_type.as_ref(), Some(function_call.span)) {
            Ok(crate::dispatch::DispatchResult::Resolved(resolved_func)) => {
                let clause_id = crate::universal_dispatch::ClauseId::new();
                
                // CRITICAL FIX: Substitute generic parameters in return type for protocol calls
                let return_type = if self.protocol_registry().has_protocol(&protocol_id) {
                    self.substitute_protocol_generics_in_return_type(
                        &resolved_func.function_info.return_type,
                        &qualified_name,
                        target_type.as_ref(),
                        precomputed_arg_types,
                        function_call,
                        context
                    )?
                } else {
                    resolved_func.function_info.return_type.clone()
                };
                
                self.register_function_clause(clause_id, &resolved_func);
                
                Ok(crate::typed_ast::UniversalCallResolution::single(clause_id, return_type))
            }
            Ok(crate::dispatch::DispatchResult::Ambiguous(candidates)) => {
                let mut clause_ids = Vec::new();
                let return_type = if !candidates.is_empty() {
                    candidates[0].function_info.return_type.clone()
                } else {
                    Type::variable(self.fresh_type_var(), Level(0))
                };
                
                for candidate in candidates {
                    let clause_id = crate::universal_dispatch::ClauseId::new();
                    self.register_function_clause(clause_id, &candidate);
                    clause_ids.push(clause_id);
                }
                
                Ok(crate::typed_ast::UniversalCallResolution::multi(clause_ids, return_type))
            }
            Ok(crate::dispatch::DispatchResult::NotFound) => {
                // Check for lazy intrinsic registration
                if module_name == "Outrun.Intrinsic" {
                    match self.try_lazy_intrinsic_registration(function_name, &function_call.arguments, function_call.span, context) {
                        Ok(result) => {
                            // Convert InferenceResult to UniversalCallResolution
                            let clause_id = crate::universal_dispatch::ClauseId::new();
                            return Ok(crate::typed_ast::UniversalCallResolution::single(clause_id, result.inferred_type));
                        }
                        Err(_) => {
                            // Fall through to NotFound error
                        }
                    }
                }
                
                Err(TypecheckError::InferenceError(crate::error::InferenceError::AmbiguousType {
                    span: crate::error::to_source_span(Some(function_call.span)),
                    suggestions: vec![format!("Unknown function: {}", qualified_name)],
                }))
            }
            Err(dispatch_error) => {
                Err(TypecheckError::DispatchError(dispatch_error))
            }
        };
        
        let resolve_time = resolve_start.elapsed();
        if resolve_time.as_millis() > 1 {
            println!("                ⏱️  Dispatcher resolve (precomputed): {:.2?}", resolve_time);
        }
        
        let dispatcher_time = dispatcher_start.elapsed();
        if dispatcher_time.as_millis() > 1 {
            println!("                ⏱️  Dispatcher total (precomputed): {:.2?}", dispatcher_time);
        }
        
        let qualified_time = qualified_start.elapsed();
        if qualified_time.as_millis() > 1 {
            println!("              🏁 resolve_qualified_function_clauses_with_precomputed_args total: {:.2?}", qualified_time);  
        }
        
        result
    }

    /// Resolve qualified function calls to clause lists
    fn resolve_qualified_function_clauses(
        &mut self,
        module_name: &str,
        function_name: &str,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<crate::typed_ast::UniversalCallResolution, TypecheckError> {
        let qualified_start = std::time::Instant::now();
        println!("              🔧 resolve_qualified_function_clauses: {}.{}", module_name, function_name);
        
        // CRITICAL DEBUG: Log every Option.unwrap call
        if module_name == "Option" && function_name == "unwrap" {
            println!("              🚨 CRITICAL: Option.unwrap call detected (NON-PRECOMPUTED)!");
            println!("                Function call arguments: {:?}", function_call.arguments.len());
        }
        
        let qualified_name = format!("{}.{}", module_name, function_name);
        let protocol_id = crate::types::ProtocolId::new(module_name);
        
        // Check if this is a protocol call
        let target_type_start = std::time::Instant::now();
        let target_type = if self.protocol_registry().has_protocol(&protocol_id) {
            println!("                🎯 Protocol call - resolving target type");
            let resolved_target = self.resolve_protocol_call_target_type(&qualified_name, function_call, context, None)?;
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
            println!("                📦 Non-protocol call");
            None
        };
        let target_type_time = target_type_start.elapsed();
        if target_type_time.as_millis() > 1 {
            println!("                ⏱️  Target type resolution: {:.2?}", target_type_time);
        }

        let dispatcher_start = std::time::Instant::now();
        let dispatcher = FunctionDispatcher::new(self.protocol_registry(), &self.function_registry, None, None);
        
        let resolve_start = std::time::Instant::now();
        let result = match dispatcher.resolve_qualified_call(&qualified_name, target_type.as_ref(), Some(function_call.span)) {
            Ok(crate::dispatch::DispatchResult::Resolved(resolved_func)) => {
                let clause_id = crate::universal_dispatch::ClauseId::new();
                
                // CRITICAL FIX: Substitute generic parameters in return type for protocol calls
                let return_type = if self.protocol_registry().has_protocol(&protocol_id) {
                    // For non-precomputed version, we need to infer argument types first
                    let arg_types: Vec<Option<Type>> = function_call.arguments.iter().map(|arg| {
                        match arg {
                            outrun_parser::Argument::Named { expression, .. } => {
                                let mut expr_clone = expression.clone();
                                match self.infer_expression(&mut expr_clone, context) {
                                    Ok(result) => Some(result.inferred_type),
                                    Err(_) => None,
                                }
                            }
                            _ => None,
                        }
                    }).collect();
                    
                    self.substitute_protocol_generics_in_return_type(
                        &resolved_func.function_info.return_type,
                        &qualified_name,
                        target_type.as_ref(),
                        &arg_types,
                        function_call,
                        context
                    )?
                } else {
                    resolved_func.function_info.return_type.clone()
                };
                
                self.register_function_clause(clause_id, &resolved_func);
                
                Ok(crate::typed_ast::UniversalCallResolution::single(clause_id, return_type))
            }
            Ok(crate::dispatch::DispatchResult::Ambiguous(candidates)) => {
                let mut clause_ids = Vec::new();
                let return_type = if !candidates.is_empty() {
                    candidates[0].function_info.return_type.clone()
                } else {
                    Type::variable(self.fresh_type_var(), Level(0))
                };
                
                for candidate in candidates {
                    let clause_id = crate::universal_dispatch::ClauseId::new();
                    self.register_function_clause(clause_id, &candidate);
                    clause_ids.push(clause_id);
                }
                
                Ok(crate::typed_ast::UniversalCallResolution::multi(clause_ids, return_type))
            }
            Ok(crate::dispatch::DispatchResult::NotFound) => {
                // Check for lazy intrinsic registration
                if module_name == "Outrun.Intrinsic" {
                    match self.try_lazy_intrinsic_registration(function_name, &function_call.arguments, function_call.span, context) {
                        Ok(result) => {
                            // Create single clause for lazily registered intrinsic
                            let clause_id = crate::universal_dispatch::ClauseId::new();
                            // Register intrinsic clause (simplified for now)
                            Ok(crate::typed_ast::UniversalCallResolution::single(clause_id, result.inferred_type))
                        }
                        Err(error) => Err(error),
                    }
                } else {
                    Err(TypecheckError::DispatchError(crate::error::DispatchError::NoImplementation {
                        protocol_name: module_name.to_string(),
                        type_name: function_name.to_string(),
                        file_span: Some(self.create_file_span(Some(function_call.span))),
                        similar_implementations: Vec::new(),
                        suggestions: Vec::new(),
                    }))
                }
            }
            Err(dispatch_error) => {
                Err(TypecheckError::DispatchError(dispatch_error))
            }
        };
        
        let resolve_time = resolve_start.elapsed();
        if resolve_time.as_millis() > 1 {
            println!("                ⏱️  Dispatcher resolve: {:.2?}", resolve_time);
        }
        
        let dispatcher_time = dispatcher_start.elapsed();
        if dispatcher_time.as_millis() > 1 {
            println!("                ⏱️  Dispatcher total: {:.2?}", dispatcher_time);
        }
        
        let qualified_time = qualified_start.elapsed();
        if qualified_time.as_millis() > 1 {
            println!("              🏁 resolve_qualified_function_clauses total: {:.2?}", qualified_time);  
        }
        
        result
    }

    /// Register a function clause in the universal dispatch registry
    fn register_function_clause(
        &mut self,
        clause_id: crate::universal_dispatch::ClauseId,
        resolved_func: &crate::dispatch::ResolvedFunction,
    ) {
        use crate::universal_dispatch::*;
        
        // DEBUG: Add extensive logging for function clause registration
        
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
                target_type: Type::concrete(implementing_type.name()),
                implementing_type: Type::concrete(implementing_type.name()),
                constraint_context: ConstraintContext::new(),
            });
        }
        
        // Add always-true guard as fallback
        guards.push(Guard::AlwaysTrue);
        
        // Create function body based on resolved function
        let body = if let Some(block) = &resolved_func.function_info.body {
            FunctionBody::UserFunction(block.clone())
        } else if resolved_func.qualified_name.starts_with("Outrun.Intrinsic.") {
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
        self.universal_dispatch_registry.register_clause(clause_info);
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
        // DEBUG: Track function call operations for performance monitoring (disabled for production)
        
        // Phase 2: Universal Interpreter Simplification
        // ALL function calls now go through the universal dispatch system
        
        // Use precomputed argument types from task results to avoid recursion
        let universal_resolution = self.resolve_universal_function_call_with_precomputed_args(function_call, context, precomputed_arg_types)?;
        
        // Set the resolved function key for interpreter dispatch (LEGACY)
        // For now, use the first clause ID as the key (will be enhanced in Phase 3)
        if let Some(first_clause) = universal_resolution.possible_clauses.first() {
            function_call.resolved_function_key = Some(format!("clause_{}", first_clause.0));
        }
        
        // Set the universal clause IDs for universal dispatch system
        function_call.universal_clause_ids = Some(
            universal_resolution.possible_clauses.iter()
                .map(|clause_id| clause_id.0)
                .collect()
        );
        
        
        Ok(InferenceResult {
            inferred_type: universal_resolution.return_type,
            constraints: context.constraints.clone(),
            substitution: context.substitution.clone(),
        })
    }

    /// Infer the type of a function call using universal dispatch
    /// Uses iterative system - arguments must be processed by task system first
    #[allow(clippy::result_large_err)]
    fn infer_function_call(
        &mut self,
        function_call: &mut outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // DEBUG: Track function call operations
        let func_call_start = std::time::Instant::now();
        println!("          🔍 infer_function_call_skip_args (NO RECURSION)");
        
        // Phase 2: Universal Interpreter Simplification
        // ALL function calls now go through the universal dispatch system
        
        let resolve_start = std::time::Instant::now();
        // Extract precomputed argument types from task results to avoid recursion
        // This is the key fix - use already-computed dependency results instead of recomputing
        let universal_resolution = self.resolve_universal_function_call(function_call, context)?;
        let resolve_time = resolve_start.elapsed();
        if resolve_time.as_millis() > 1 {
            println!("            ⏱️  Universal resolution (no recursion): {:.2?}", resolve_time);
        }
        
        // Set the resolved function key for interpreter dispatch (LEGACY)
        // For now, use the first clause ID as the key (will be enhanced in Phase 3)
        if let Some(first_clause) = universal_resolution.possible_clauses.first() {
            function_call.resolved_function_key = Some(format!("clause_{}", first_clause.0));
        }
        
        // Set the universal clause IDs for universal dispatch system
        function_call.universal_clause_ids = Some(
            universal_resolution.possible_clauses.iter()
                .map(|clause_id| clause_id.0)
                .collect()
        );
        
        // SKIP ARGUMENT PROCESSING - arguments already processed by iterative system
        println!("            ⚡ SKIPPING recursive argument processing - already done by task system!");
        
        // For now, return the resolved return type
        // In Phase 3, this will be enhanced with full constraint solving
        let result_start = std::time::Instant::now();
        let result = Ok(InferenceResult {
            inferred_type: universal_resolution.return_type,
            constraints: context.constraints.clone(),
            substitution: context.substitution.clone(),
        });
        let result_time = result_start.elapsed();
        if result_time.as_millis() > 1 {
            println!("            ⏱️  Result construction: {:.2?}", result_time);
        }
        
        let func_total_time = func_call_start.elapsed();  
        if func_total_time.as_millis() > 1 {
            println!("          🏁 infer_function_call_skip_args total: {:.2?}", func_total_time);
        }
        
        result
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
                let protocol_id = crate::types::ProtocolId::new(module_name);
                
                // Check if this is a protocol call - pass precomputed args to avoid recursion!
                
                self.resolve_qualified_function_clauses_with_precomputed_args(module_name, &name.name, function_call, context, precomputed_arg_types)
            }
            outrun_parser::FunctionPath::Expression { .. } => {
                // Dynamic function call - not yet supported
                return Err(TypecheckError::InferenceError(crate::error::InferenceError::AmbiguousType {
                    span: crate::error::to_source_span(Some(function_call.span)),
                    suggestions: vec!["Dynamic function calls not yet supported".to_string()],
                }));
            }
        }?;
        
        Ok(initial_resolution)
    }

    fn resolve_universal_function_call(
        &mut self,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<crate::typed_ast::UniversalCallResolution, TypecheckError> {
        println!("            🚀 resolve_universal_function_call (no recursion)");
        
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
                let protocol_id = crate::types::ProtocolId::new(module_name);
                
                // Check if this is a protocol call - log it but proceed normally
                if self.protocol_registry().has_protocol(&protocol_id) {
                    println!("              ⚡ Protocol call detected - will skip recursive resolution inside resolve_qualified_function_clauses!");
                }
                
                self.resolve_qualified_function_clauses(module_name, &name.name, function_call, context)
            }
            outrun_parser::FunctionPath::Expression { .. } => {
                // Dynamic function call - not yet supported
                return Err(TypecheckError::InferenceError(crate::error::InferenceError::AmbiguousType {
                    span: crate::error::to_source_span(Some(function_call.span)),
                    suggestions: vec!["Dynamic function calls not yet supported".to_string()],
                }));
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
        let (function_parameters, function_return_type) = if let Some(info) = self.function_registry.get_function(module_name, function_name) {
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
            println!("              ⚡ Using precomputed argument types - NO RECURSION!");
            precomputed.to_vec()
        } else {
            println!("              🔄 Inferring argument types recursively");
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
        function_call: &outrun_parser::FunctionCall,
        _context: &mut InferenceContext,
    ) -> Result<Type, TypecheckError> {
        // Get the protocol definition to understand generic parameters
        let (module_name, function_name) = if let Some(dot_pos) = qualified_name.rfind('.') {
            (&qualified_name[..dot_pos], &qualified_name[dot_pos + 1..])
        } else {
            return Ok(return_type.clone()); // No module, can't be protocol
        };

        // Get function info to understand parameter types
        let function_parameters = if let Some(info) = self.function_registry.get_function(module_name, function_name) {
            info.parameters.clone()
        } else {
            return Ok(return_type.clone()); // Function not found
        };

        // Handle the most common case: Option<T> where T needs substitution
        if module_name == "Option" {
            println!("              🔧 Option protocol detected - attempting generic substitution");
            println!("                Function: {}", function_name);
            println!("                Precomputed arg types count: {}", precomputed_arg_types.len());
            println!("                Function parameters: {:?}", function_parameters);
            println!("                Original return type: {:?}", return_type);
            
            // Find the argument that contains the protocol type (usually named 'value' for Option.unwrap)
            for (i, (param_name, _param_type)) in function_parameters.iter().enumerate() {
                println!("                  Checking parameter {}: {} (type: {:?})", i, param_name, _param_type);
                if param_name == "value" {
                    // Get the actual argument type
                    if let Some(Some(arg_type)) = precomputed_arg_types.get(i) {
                        println!("                    Found argument type: {:?}", arg_type);
                        // Extract T from Option<T>
                        if let Type::Protocol { id, args, .. } = arg_type {
                            if id.name() == "Option" && !args.is_empty() {
                                // The first argument of Option<T> is T
                                let inner_type = &args[0];
                                println!("                    ✅ Substituting T with: {:?}", inner_type);
                                let result = self.substitute_generic_parameter_in_type(return_type, "T", inner_type)?;
                                println!("                    ✅ Final return type: {:?}", result);
                                return Ok(result);
                            }
                        }
                    } else {
                        println!("                    ❌ No argument type available at index {}", i);
                    }
                    break;
                }
            }
            println!("              ❌ No 'value' parameter found or substitution failed");
        }
        
        // For other protocols, implement generic substitution as needed
        // This can be extended for other protocol patterns
        Ok(return_type.clone())
    }
    
    /// Helper method to substitute a specific generic parameter in a type
    fn substitute_generic_parameter_in_type(
        &self,
        target_type: &Type,
        param_name: &str,
        replacement: &Type,
    ) -> Result<Type, TypecheckError> {
        match target_type {
            Type::Protocol { id, args, span } => {
                // Check if this protocol ID matches the parameter name
                if id.name() == param_name {
                    return Ok(replacement.clone());
                }
                
                // Recursively substitute in arguments
                let substituted_args: Result<Vec<_>, _> = args.iter()
                    .map(|arg| self.substitute_generic_parameter_in_type(arg, param_name, replacement))
                    .collect();
                
                Ok(Type::Protocol {
                    id: id.clone(),
                    args: substituted_args?,
                    span: *span,
                })
            }
            Type::Concrete { id, args, span } => {
                // Recursively substitute in generic arguments
                let substituted_args: Result<Vec<_>, _> = args.iter()
                    .map(|arg| self.substitute_generic_parameter_in_type(arg, param_name, replacement))
                    .collect();
                
                Ok(Type::Concrete {
                    id: id.clone(),
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
        }
    }

    /// Substitute generic parameters in local function return types
    /// This fixes the issue where local functions returning generic types (like Option<T>)
    /// don't get their generic parameters properly resolved based on argument types
    #[allow(clippy::result_large_err)]
    fn substitute_local_function_generics(
        &mut self,
        return_type: &Type,
        function_name: &str,
        function_parameters: &[(String, Type)],
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<Type, TypecheckError> {
        println!("🚨🚨🚨 LOCAL FUNCTION GENERIC SUBSTITUTION for: {} 🚨🚨🚨", function_name);
        println!("                Original return type: {:?}", return_type);
        println!("                Function parameters: {:?}", function_parameters);
        
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
        
        println!("                Inferred argument types: {:?}", argument_types);
        
        // Build a map of generic parameter substitutions
        let mut substitutions = std::collections::HashMap::new();
        
        // Match parameter types with argument types to find generic substitutions
        for (i, (param_name, param_type)) in function_parameters.iter().enumerate() {
            if let Some(arg_type) = argument_types.get(i) {
                println!("                  Matching param {}: {} (type: {:?}) with arg type: {:?}", 
                         i, param_name, param_type, arg_type);
                
                // Find generic parameter substitutions by matching param_type with arg_type
                self.extract_generic_substitutions(param_type, arg_type, &mut substitutions)?;
            }
        }
        
        println!("                Found substitutions: {:?}", substitutions);
        
        // Apply all substitutions to the return type
        let mut result_type = return_type.clone();
        for (generic_param, concrete_type) in substitutions {
            result_type = self.substitute_generic_parameter_in_type(&result_type, &generic_param, &concrete_type)?;
        }
        
        println!("                ✅ Final substituted return type: {:?}", result_type);
        Ok(result_type)
    }
    
    /// Extract generic parameter substitutions by matching a parameter type with an argument type
    /// For example, matching Option<T> with Option<Integer> would extract T -> Integer
    #[allow(clippy::result_large_err)]
    fn extract_generic_substitutions(
        &self,
        param_type: &Type,
        arg_type: &Type,
        substitutions: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), TypecheckError> {
        match (param_type, arg_type) {
            // Match Protocol<T> with Protocol<ConcreteType>
            (Type::Protocol { id: param_id, args: param_args, .. }, 
             Type::Protocol { id: arg_id, args: arg_args, .. }) => {
                if param_id.name() == arg_id.name() && param_args.len() == arg_args.len() {
                    // Recursively match generic arguments
                    for (param_arg, arg_arg) in param_args.iter().zip(arg_args.iter()) {
                        self.extract_generic_substitutions(param_arg, arg_arg, substitutions)?;
                    }
                }
            }
            
            // Match Concrete<T> with Concrete<ConcreteType>
            (Type::Concrete { id: param_id, args: param_args, .. }, 
             Type::Concrete { id: arg_id, args: arg_args, .. }) => {
                if param_id.name() == arg_id.name() && param_args.len() == arg_args.len() {
                    // Recursively match generic arguments
                    for (param_arg, arg_arg) in param_args.iter().zip(arg_args.iter()) {
                        self.extract_generic_substitutions(param_arg, arg_arg, substitutions)?;
                    }
                }
            }
            
            // Match generic parameter T with concrete type
            (Type::Protocol { id: param_id, args, .. }, concrete_type) if args.is_empty() => {
                // This is a generic parameter (like T) - record the substitution
                substitutions.insert(param_id.name().to_string(), concrete_type.clone());
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
            if let Some(param_name) = self.extract_parameter_name_from_position(&self_position.path) {
                
                // Find the corresponding argument by name in the function call
                if let Some(argument_expr) = self.find_argument_by_name(function_call, &param_name) {
                    
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
                    return Err(TypecheckError::InferenceError(InferenceError::SelfTypeUnificationFailure {
                        first_self_type: first_type.clone(),
                        conflicting_self_type: candidate.clone(),
                        span: None, // TODO: Add span information
                    }));
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
                outrun_parser::Argument::Named { name, expression, .. } => {
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
                        return Err(TypecheckError::InferenceError(InferenceError::TypeVariableUnificationFailure {
                            variable_name: format!("{} = {}", left_resolved, right_resolved),
                            first_type: left_resolved,
                            conflicting_type: right_resolved,
                            span: None, // TODO: Add span information
                        }));
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
            for (param_index, (_param_name, param_type)) in function_info.parameters.iter().enumerate() {
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
            Type::Concrete { id, args, .. } => {
                // Check if this is actually a generic parameter from the function signature
                if generic_parameters.contains(&id.name().to_string()) {
                    // This is a type parameter - record the mapping
                    if let Some(existing_type) = type_param_mappings.get(id.name()) {
                        // Type parameter already mapped - create equality constraint
                        constraints.push(Constraint::Equality {
                            left: Box::new(existing_type.clone()),
                            right: Box::new(arg_type.clone()),
                            span: None,
                        });
                    } else {
                        // First occurrence - record the mapping
                        type_param_mappings.insert(id.name().to_string(), arg_type.clone());
                    }
                } else {
                    // Concrete type with possible generic arguments
                    match arg_type {
                        Type::Concrete { id: arg_id, args: arg_args, .. } => {
                            // Check that concrete types match
                            if id.name() != arg_id.name() {
                                constraints.push(Constraint::Equality {
                                    left: Box::new(param_type.clone()),
                                    right: Box::new(arg_type.clone()),
                                    span: None,
                                });
                            } else {
                                // Recursively process generic arguments
                                for (param_arg, arg_arg) in args.iter().zip(arg_args.iter()) {
                                    let nested_constraints = Self::extract_type_parameter_constraints(
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
            Type::Concrete { id, args, .. } => {
                if id.name() == type_param {
                    // This is an occurrence of our type parameter
                    constraints.push(Constraint::Equality {
                        left: Box::new(typ.clone()),
                        right: Box::new(inferred_type.clone()),
                        span: None,
                    });
                } else {
                    // Recursively check generic arguments
                    for arg in args {
                        let nested_constraints = Self::generate_type_parameter_equality_constraints(
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
            Self::collect_type_parameter_substitutions(param_type, arg_type, &mut substitutions, &function_info.generic_parameters)?;
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
            Type::Concrete { id, args, .. } => {
                // Check if this is actually a generic parameter from the function signature
                if generic_parameters.contains(&id.name().to_string()) {
                    substitutions.insert(id.name().to_string(), arg_type.clone());
                } else {
                    // Recursively process generic arguments - both param and arg must be concrete with matching base types
                    if let Type::Concrete { id: arg_id, args: arg_args, .. } = arg_type {
                        // Base types should match (e.g., both "Type")
                        if id.name() == arg_id.name() {
                            // Recursively match type arguments
                            for (param_arg, arg_arg) in args.iter().zip(arg_args.iter()) {
                                Self::collect_type_parameter_substitutions(param_arg, arg_arg, substitutions, generic_parameters)?;
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
                protocol_id,
                protocol_args,
            } => crate::dispatch::FunctionContext::Protocol {
                protocol_id: protocol_id.clone(),
                protocol_args: protocol_args.clone(),
            },
            SelfBindingContext::Implementation {
                implementing_type,
                implementing_args,
                protocol_id,
                protocol_args,
            } => crate::dispatch::FunctionContext::Implementation {
                implementing_type: implementing_type.clone(),
                implementing_args: implementing_args.clone(),
                protocol_id: protocol_id.clone(),
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
                protocol_id,
                protocol_args,
            } => crate::dispatch::FunctionContext::Protocol {
                protocol_id: protocol_id.clone(),
                protocol_args: protocol_args.clone(),
            },
            SelfBindingContext::Implementation {
                implementing_type,
                implementing_args,
                protocol_id,
                protocol_args,
            } => crate::dispatch::FunctionContext::Implementation {
                implementing_type: implementing_type.clone(),
                implementing_args: implementing_args.clone(),
                protocol_id: protocol_id.clone(),
                protocol_args: protocol_args.clone(),
            },
            SelfBindingContext::FunctionContext { parent_context, .. } => {
                // For function contexts, use the parent context
                Self::create_function_context_from_self_binding(parent_context)
            }
        }
    }

    /// Infer the type of a list literal
    #[allow(clippy::result_large_err)]
    fn infer_list_literal(
        &mut self,
        list_literal: &outrun_parser::ListLiteral,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::ListElement;

        // Handle empty list - require type hint or assign fresh type variable
        if list_literal.elements.is_empty() {
            if let Some(expected_type) = &context.expected_type {
                // Use expected type if available
                Ok(InferenceResult {
                    inferred_type: expected_type.clone(),
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            } else {
                // Empty list without type hint gets fresh type variable
                let element_type = Type::variable(self.fresh_type_var(), Level(0));
                let list_type = Type::Concrete {
                    id: crate::types::TypeId::new("Outrun.Core.List"),
                    args: vec![element_type],
                    span: None,
                };
                Ok(InferenceResult {
                    inferred_type: list_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
        } else {
            // Non-empty list - infer element types
            let mut element_types = Vec::new();
            let mut all_constraints = context.constraints.clone();

            for element in &list_literal.elements {
                match element {
                    ListElement::Expression(expr) => {
                        let element_result =
                            self.infer_expression(&mut (**expr).clone(), context)?;
                        element_types.push(element_result.inferred_type);
                        all_constraints.extend(element_result.constraints);
                    }
                    ListElement::Spread(_spread_id) => {
                        // Spread elements require the spread variable to be a List<T>
                        // For now, we'll create a type variable for the spread
                        // TODO: Implement proper spread inference
                        let spread_element_type = Type::variable(self.fresh_type_var(), Level(0));
                        element_types.push(spread_element_type);
                    }
                }
            }

            // Check if all elements have the same type (homogeneous list)
            if element_types.is_empty() {
                // This shouldn't happen given our check above, but handle it defensively
                let element_type = Type::variable(self.fresh_type_var(), Level(0));
                let list_type = Type::Concrete {
                    id: crate::types::TypeId::new("Outrun.Core.List"),
                    args: vec![element_type],
                    span: None,
                };
                return Ok(InferenceResult {
                    inferred_type: list_type,
                    constraints: all_constraints,
                    substitution: context.substitution.clone(),
                });
            }

            // Take the first element as the expected type
            let first_element_type = element_types[0].clone();

            // Create constraints that all other elements must match the first
            for element_type in element_types.iter().skip(1) {
                let constraint = Constraint::Equality {
                    left: Box::new(first_element_type.clone()),
                    right: Box::new(element_type.clone()),
                    span: None,
                };
                all_constraints.push(constraint);
            }

            // Create the List<ElementType> type
            let list_type = Type::Concrete {
                id: crate::types::TypeId::new("Outrun.Core.List"),
                args: vec![first_element_type],
                span: None,
            };

            Ok(InferenceResult {
                inferred_type: list_type,
                constraints: all_constraints,
                substitution: context.substitution.clone(),
            })
        }
    }

    /// Infer the type of a tuple literal
    #[allow(clippy::result_large_err)]
    fn infer_tuple_literal(
        &mut self,
        tuple_literal: &outrun_parser::TupleLiteral,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Handle empty tuple
        if tuple_literal.elements.is_empty() {
            let tuple_type = Type::Concrete {
                id: crate::types::TypeId::new("Outrun.Core.Tuple"),
                args: vec![], // Empty tuple has no type arguments
                span: None,
            };
            return Ok(InferenceResult {
                inferred_type: tuple_type,
                constraints: context.constraints.clone(),
                substitution: context.substitution.clone(),
            });
        }

        // Infer types of all tuple elements
        let mut element_types = Vec::new();
        let mut all_constraints = context.constraints.clone();

        for element_expr in &tuple_literal.elements {
            let element_result = self.infer_expression(&mut element_expr.clone(), context)?;
            element_types.push(element_result.inferred_type);
            all_constraints.extend(element_result.constraints);
        }

        // Create Tuple<T1, T2, ..., Tn> type
        let tuple_type = Type::Concrete {
            id: crate::types::TypeId::new("Outrun.Core.Tuple"),
            args: element_types,
            span: None,
        };

        Ok(InferenceResult {
            inferred_type: tuple_type,
            constraints: all_constraints,
            substitution: context.substitution.clone(),
        })
    }

    /// Infer the type of a map literal
    #[allow(clippy::result_large_err)]
    fn infer_map_literal(
        &mut self,
        map_literal: &outrun_parser::MapLiteral,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::MapEntry;

        // Handle empty map
        if map_literal.entries.is_empty() {
            if let Some(expected_type) = &context.expected_type {
                // Use expected type if available
                Ok(InferenceResult {
                    inferred_type: expected_type.clone(),
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            } else {
                // Empty map without type hint gets fresh type variables
                let key_type = Type::variable(self.fresh_type_var(), Level(0));
                let value_type = Type::variable(self.fresh_type_var(), Level(0));
                let map_type = Type::Concrete {
                    id: crate::types::TypeId::new("Outrun.Core.Map"),
                    args: vec![key_type, value_type],
                    span: None,
                };
                Ok(InferenceResult {
                    inferred_type: map_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
        } else {
            // Non-empty map - infer key and value types
            let mut key_types = Vec::new();
            let mut value_types = Vec::new();
            let mut all_constraints = context.constraints.clone();

            for entry in &map_literal.entries {
                match entry {
                    MapEntry::Assignment { key, value } => {
                        // Infer key type
                        let key_result = self.infer_expression(&mut (**key).clone(), context)?;
                        key_types.push(key_result.inferred_type);
                        all_constraints.extend(key_result.constraints);

                        // Infer value type
                        let value_result =
                            self.infer_expression(&mut (**value).clone(), context)?;
                        value_types.push(value_result.inferred_type);
                        all_constraints.extend(value_result.constraints);
                    }
                    MapEntry::Shorthand { name: _, value } => {
                        // Shorthand syntax: {name} where name is both key and value
                        // Key is atom (symbol), value is expression
                        key_types.push(Type::concrete("Outrun.Core.Atom"));

                        let value_result =
                            self.infer_expression(&mut (**value).clone(), context)?;
                        value_types.push(value_result.inferred_type);
                        all_constraints.extend(value_result.constraints);
                    }
                    MapEntry::Spread(_spread_id) => {
                        // Spread entries require the spread variable to be a Map<K, V>
                        // TODO: Implement proper spread inference
                        let spread_key_type = Type::variable(self.fresh_type_var(), Level(0));
                        let spread_value_type = Type::variable(self.fresh_type_var(), Level(0));
                        key_types.push(spread_key_type);
                        value_types.push(spread_value_type);
                    }
                }
            }

            // Check if all keys and values have consistent types
            if key_types.is_empty() || value_types.is_empty() {
                // This shouldn't happen, but handle defensively
                let key_type = Type::variable(self.fresh_type_var(), Level(0));
                let value_type = Type::variable(self.fresh_type_var(), Level(0));
                let map_type = Type::Concrete {
                    id: crate::types::TypeId::new("Outrun.Core.Map"),
                    args: vec![key_type, value_type],
                    span: None,
                };
                return Ok(InferenceResult {
                    inferred_type: map_type,
                    constraints: all_constraints,
                    substitution: context.substitution.clone(),
                });
            }

            // Take the first key and value types as expected
            let first_key_type = key_types[0].clone();
            let first_value_type = value_types[0].clone();

            // Create constraints that all other keys must match the first key type
            for key_type in key_types.iter().skip(1) {
                let constraint = Constraint::Equality {
                    left: Box::new(first_key_type.clone()),
                    right: Box::new(key_type.clone()),
                    span: None,
                };
                all_constraints.push(constraint);
            }

            // Create constraints that all other values must match the first value type
            for value_type in value_types.iter().skip(1) {
                let constraint = Constraint::Equality {
                    left: Box::new(first_value_type.clone()),
                    right: Box::new(value_type.clone()),
                    span: None,
                };
                all_constraints.push(constraint);
            }

            // Create Map<KeyType, ValueType> type
            let map_type = Type::Concrete {
                id: crate::types::TypeId::new("Outrun.Core.Map"),
                args: vec![first_key_type, first_value_type],
                span: None,
            };

            Ok(InferenceResult {
                inferred_type: map_type,
                constraints: all_constraints,
                substitution: context.substitution.clone(),
            })
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

    fn infer_anonymous_function(
        &mut self,
        anonymous_fn: &outrun_parser::AnonymousFunction,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Anonymous functions must have at least one clause
        if anonymous_fn.clauses.is_empty() {
            return Err(TypecheckError::InferenceError(
                crate::error::InferenceError::AmbiguousType {
                    span: crate::error::to_source_span(Some(anonymous_fn.span)),
                    suggestions: vec![
                        "Anonymous function must have at least one clause".to_string()
                    ],
                },
            ));
        }

        // Infer function type from the first clause
        let first_clause = &anonymous_fn.clauses[0];
        let (param_types, return_type, first_constraints) =
            self.infer_function_clause_types(first_clause, context)?;

        // Create the function type from the first clause
        let function_type = Type::Function {
            params: param_types.clone(),
            return_type: Box::new(return_type.clone()),
            span: Some(anonymous_fn.span),
        };

        // Validate that all remaining clauses have consistent signatures
        let mut all_constraints = first_constraints;
        for (clause_index, clause) in anonymous_fn.clauses.iter().enumerate().skip(1) {
            let validation_result = self.validate_function_clause_consistency(
                clause,
                &param_types,
                &return_type,
                context,
                clause_index + 1,
            )?;
            all_constraints.extend(validation_result.constraints);
        }

        Ok(InferenceResult {
            inferred_type: function_type,
            constraints: all_constraints,
            substitution: context.substitution.clone(),
        })
    }

    /// Infer parameter and return types from a function clause
    #[allow(clippy::result_large_err)]
    fn infer_function_clause_types(
        &mut self,
        clause: &outrun_parser::AnonymousClause,
        context: &mut InferenceContext,
    ) -> Result<FunctionClauseInferenceResult, TypecheckError> {
        let mut local_context = context.clone();
        let mut constraints = context.constraints.clone();

        // Extract parameter types from the clause parameters
        let param_types = self.extract_anonymous_function_parameters(&clause.parameters)?;

        // Bind parameters in local scope for body type inference
        for (param_name, param_type) in &param_types {
            local_context.bind_variable(param_name.clone(), param_type.clone());
        }

        // Infer the return type from the body
        let body_result = self.infer_anonymous_function_body(&clause.body, &mut local_context)?;
        let return_type = body_result.inferred_type;
        constraints.extend(body_result.constraints);

        // Handle guards if present
        if let Some(guard_expr) = &clause.guard {
            let guard_result =
                self.infer_expression(&mut guard_expr.clone(), &mut local_context)?;
            constraints.extend(guard_result.constraints);

            // Guards must return Boolean
            let boolean_constraint = crate::types::Constraint::Equality {
                left: Box::new(Type::concrete("Outrun.Core.Boolean")),
                right: Box::new(guard_result.inferred_type),
                span: Some(guard_expr.span),
            };
            constraints.push(boolean_constraint);
        }

        Ok((param_types, return_type, constraints))
    }

    /// Extract parameter types from anonymous function parameters
    #[allow(clippy::result_large_err)]
    fn extract_anonymous_function_parameters(
        &mut self,
        parameters: &outrun_parser::AnonymousParameters,
    ) -> Result<Vec<(String, Type)>, TypecheckError> {
        use outrun_parser::AnonymousParameters;

        match parameters {
            AnonymousParameters::None { .. } => Ok(vec![]),
            AnonymousParameters::Single { parameter, .. } => {
                let param_name = parameter.name.name.clone();
                let param_type = self.convert_type_annotation(&parameter.type_annotation)?;
                Ok(vec![(param_name, param_type)])
            }
            AnonymousParameters::Multiple { parameters, .. } => {
                let mut param_types = Vec::new();
                for param in parameters {
                    let param_name = param.name.name.clone();
                    let param_type = self.convert_type_annotation(&param.type_annotation)?;
                    param_types.push((param_name, param_type));
                }
                Ok(param_types)
            }
        }
    }

    /// Infer the return type from an anonymous function body
    #[allow(clippy::result_large_err)]
    fn infer_anonymous_function_body(
        &mut self,
        body: &outrun_parser::AnonymousBody,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::AnonymousBody;

        match body {
            AnonymousBody::Expression(expr) => {
                // Single expression body
                self.infer_expression(&mut (**expr).clone(), context)
            }
            AnonymousBody::Block(_block) => {
                // Block body - for now, return a fresh type variable
                // TODO: Implement proper block inference
                let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
        }
    }

    /// Validate that a function clause has consistent signature with the first clause
    #[allow(clippy::result_large_err)]
    fn validate_function_clause_consistency(
        &mut self,
        clause: &outrun_parser::AnonymousClause,
        expected_param_types: &[(String, Type)],
        expected_return_type: &Type,
        context: &mut InferenceContext,
        clause_number: usize,
    ) -> Result<InferenceResult, TypecheckError> {
        // Extract parameter types from this clause
        let clause_param_types = self.extract_anonymous_function_parameters(&clause.parameters)?;

        // Check parameter count consistency
        if clause_param_types.len() != expected_param_types.len() {
            return Err(TypecheckError::InferenceError(
                crate::error::InferenceError::FunctionCallError {
                    message: format!(
                        "Function clause {} has {} parameters, but first clause has {}",
                        clause_number,
                        clause_param_types.len(),
                        expected_param_types.len()
                    ),
                    function_name: None,
                    expected_signature: Some(format!(
                        "({}) -> {}",
                        expected_param_types
                            .iter()
                            .map(|(name, ty)| format!("{}: {}", name, ty))
                            .collect::<Vec<_>>()
                            .join(", "),
                        expected_return_type
                    )),
                    actual_arguments: Some(format!(
                        "({})",
                        clause_param_types
                            .iter()
                            .map(|(name, ty)| format!("{}: {}", name, ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                    span: crate::error::to_source_span(Some(clause.span)),
                    suggestions: vec![
                        "All function clauses must have the same parameter signature".to_string(),
                        "Consider adding or removing parameters to match the first clause"
                            .to_string(),
                    ],
                },
            ));
        }

        // Create constraints for parameter type consistency
        let mut constraints = context.constraints.clone();
        for ((expected_name, expected_type), (clause_name, clause_type)) in
            expected_param_types.iter().zip(clause_param_types.iter())
        {
            // Parameter names should be consistent (warning, not error)
            if expected_name != clause_name {
                // For now, we allow different parameter names in different clauses
                // In a full implementation, we might want to warn about this
            }

            // Parameter types must be identical
            let param_constraint = crate::types::Constraint::Equality {
                left: Box::new(expected_type.clone()),
                right: Box::new(clause_type.clone()),
                span: Some(clause.span),
            };
            constraints.push(param_constraint);
        }

        // Create local context with parameter bindings
        let mut local_context = context.clone();
        for (param_name, param_type) in &clause_param_types {
            local_context.bind_variable(param_name.clone(), param_type.clone());
        }

        // Infer return type from clause body
        let body_result = self.infer_anonymous_function_body(&clause.body, &mut local_context)?;
        constraints.extend(body_result.constraints);

        // Return type must be consistent
        let return_constraint = crate::types::Constraint::Equality {
            left: Box::new(expected_return_type.clone()),
            right: Box::new(body_result.inferred_type.clone()),
            span: Some(clause.span),
        };
        constraints.push(return_constraint);

        // Handle guards if present
        if let Some(guard_expr) = &clause.guard {
            let guard_result =
                self.infer_expression(&mut guard_expr.clone(), &mut local_context)?;
            constraints.extend(guard_result.constraints);

            // Guards must return Boolean
            let guard_constraint = crate::types::Constraint::Equality {
                left: Box::new(Type::concrete("Outrun.Core.Boolean")),
                right: Box::new(guard_result.inferred_type),
                span: Some(guard_expr.span),
            };
            constraints.push(guard_constraint);
        }

        Ok(InferenceResult {
            inferred_type: body_result.inferred_type,
            constraints,
            substitution: local_context.substitution,
        })
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
                    match self.type_registry.get_type_kind(&type_name) {
                        Some(crate::registry::TypeKind::Protocol(_protocol_def)) => {
                            // Known protocol type
                            Ok(Type::Protocol {
                                id: ProtocolId::new(type_name),
                                args: type_args,
                                span: None,
                            })
                        }
                        Some(crate::registry::TypeKind::ConcreteType(concrete_def)) => {
                            // Known concrete type
                            Ok(Type::Concrete {
                                id: concrete_def.type_id.clone(),
                                args: type_args,
                                span: None,
                            })
                        }
                        None => {
                            // Unknown type - create concrete type for now
                            // Type registry will be populated during package processing
                            Ok(Type::Concrete {
                                id: crate::types::TypeId::new(type_name),
                                args: type_args,
                                span: None,
                            })
                        }
                    }
                }
            }
            TypeAnnotation::Tuple { .. } => {
                // TODO: Implement tuple types in the typechecker
                // For now, treat as a placeholder concrete type
                Ok(Type::concrete("Outrun.Core.Tuple"))
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
        id1: &crate::types::TypeId,
        id2: &crate::types::TypeId,
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
            Type::Concrete { id, args, .. } => {
                // Look up the struct definition to find the field type
                self.infer_struct_field_type(id, args, field_name, context)
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
        struct_type_id: &crate::types::TypeId,
        struct_args: &[Type],
        field_name: &str,
        context: &InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // Look up the actual struct definition from the registry
        let struct_name = struct_type_id.name();

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
            Type::Concrete { id, args, span } => {
                // If this is a generic parameter, substitute it
                if let Some(substituted_type) = substitution.get(id.name()) {
                    Ok(substituted_type.clone())
                } else {
                    // Apply substitution to generic arguments
                    let substituted_args: Result<Vec<Type>, TypecheckError> = args
                        .into_iter()
                        .map(|arg| Self::apply_substitution_static(arg, substitution))
                        .collect();

                    Ok(Type::Concrete {
                        id,
                        args: substituted_args?,
                        span,
                    })
                }
            }
            Type::Protocol { id, args, span } => {
                // Apply substitution to protocol arguments
                let substituted_args: Result<Vec<Type>, TypecheckError> = args
                    .into_iter()
                    .map(|arg| Self::apply_substitution_static(arg, substitution))
                    .collect();

                Ok(Type::Protocol {
                    id,
                    args: substituted_args?,
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
                protocol_id: ProtocolId::new("Unknown"),
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
                protocol_id: ProtocolId::new("Unknown"),
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
        assert_eq!(engine.current_module.0, "main");
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

        assert_eq!(engine.current_module.0, "main");

        engine.set_current_module(ModuleId::new("Http::Client"));
        assert_eq!(engine.current_module.0, "Http::Client");
    }

    #[test]
    fn test_function_call_inference_integration() {
        let mut engine = TypeInferenceEngine::new();
        let mut context = InferenceContext::new();

        // Create a mock function call AST node
        use outrun_parser::{FunctionCall, FunctionPath, Identifier, Span};

        let mut function_call = FunctionCall {
            path: FunctionPath::Simple {
                name: Identifier {
                    name: "test_function".to_string(),
                    span: Span::new(0, 13),
                },
            },
            arguments: vec![], // No arguments for simplicity
            span: Span::new(0, 15),
            resolved_function_key: None,
            universal_clause_ids: None,
        };

        // Test function call inference (should handle dispatch errors gracefully)
        let result = engine.infer_function_call(&mut function_call, &mut context);

        // This should return an error since no function is registered
        assert!(result.is_err());
        match result {
            Err(crate::error::TypecheckError::DispatchError(_)) => {
                // Expected - no function registered
            }
            _ => panic!("Expected dispatch error for unregistered function"),
        }
    }

    #[test]
    fn test_list_literal_homogeneous_inference() {
        let mut engine = TypeInferenceEngine::new();
        let mut context = InferenceContext::new();

        // Create mock list literal: [1, 2, 3]
        use outrun_parser::{
            Expression, ExpressionKind, IntegerFormat, IntegerLiteral, ListElement, ListLiteral,
            Span,
        };

        let list_literal = ListLiteral {
            elements: vec![
                ListElement::Expression(Box::new(Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 1,
                        format: IntegerFormat::Decimal,
                        raw_text: "1".to_string(),
                        span: Span::new(1, 2),
                    }),
                    span: Span::new(1, 2),
                    type_info: None,
                })),
                ListElement::Expression(Box::new(Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 2,
                        format: IntegerFormat::Decimal,
                        raw_text: "2".to_string(),
                        span: Span::new(4, 5),
                    }),
                    span: Span::new(4, 5),
                    type_info: None,
                })),
                ListElement::Expression(Box::new(Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 3,
                        format: IntegerFormat::Decimal,
                        raw_text: "3".to_string(),
                        span: Span::new(7, 8),
                    }),
                    span: Span::new(7, 8),
                    type_info: None,
                })),
            ],
            span: Span::new(0, 9),
        };

        let result = engine
            .infer_list_literal(&list_literal, &mut context)
            .unwrap();

        // Should infer List<Outrun.Core.Integer64>
        match &result.inferred_type {
            Type::Concrete { id, args, .. } => {
                assert_eq!(id.0, "Outrun.Core.List");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Type::Concrete { id, .. } => {
                        assert_eq!(id.0, "Outrun.Core.Integer64");
                    }
                    _ => panic!("Expected Integer64 element type"),
                }
            }
            _ => panic!("Expected List<Integer64> type"),
        }
    }

    #[test]
    fn test_empty_list_inference() {
        let mut engine = TypeInferenceEngine::new();
        let mut context = InferenceContext::new();

        // Create empty list literal: []
        use outrun_parser::{ListLiteral, Span};

        let list_literal = ListLiteral {
            elements: vec![],
            span: Span::new(0, 2),
        };

        let result = engine
            .infer_list_literal(&list_literal, &mut context)
            .unwrap();

        // Should infer List<T> where T is a type variable
        match &result.inferred_type {
            Type::Concrete { id, args, .. } => {
                assert_eq!(id.0, "Outrun.Core.List");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Type::Variable { .. } => {
                        // Expected - fresh type variable for element type
                    }
                    _ => panic!("Expected type variable for empty list element type"),
                }
            }
            _ => panic!("Expected List<T> type"),
        }
    }

    #[test]
    fn test_tuple_literal_inference() {
        let mut engine = TypeInferenceEngine::new();
        let mut context = InferenceContext::new();

        // Create tuple literal: (1, "hello", true)
        use outrun_parser::{
            BooleanLiteral, Expression, ExpressionKind, IntegerFormat, IntegerLiteral, Span,
            StringFormat, StringLiteral, TupleLiteral,
        };

        let tuple_literal = TupleLiteral {
            elements: vec![
                Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 1,
                        format: IntegerFormat::Decimal,
                        raw_text: "1".to_string(),
                        span: Span::new(1, 2),
                    }),
                    span: Span::new(1, 2),
                    type_info: None,
                },
                Expression {
                    kind: ExpressionKind::String(StringLiteral {
                        parts: vec![outrun_parser::StringPart::Text {
                            content: "hello".to_string(),
                            raw_content: "hello".to_string(),
                        }],
                        format: StringFormat::Basic,
                        span: Span::new(4, 11),
                    }),
                    span: Span::new(4, 11),
                    type_info: None,
                },
                Expression {
                    kind: ExpressionKind::Boolean(BooleanLiteral {
                        value: true,
                        span: Span::new(13, 17),
                    }),
                    span: Span::new(13, 17),
                    type_info: None,
                },
            ],
            span: Span::new(0, 18),
        };

        let result = engine
            .infer_tuple_literal(&tuple_literal, &mut context)
            .unwrap();

        // Should infer Tuple<Outrun.Core.Integer64, Outrun.Core.String, Outrun.Core.Boolean>
        match &result.inferred_type {
            Type::Concrete { id, args, .. } => {
                assert_eq!(id.0, "Outrun.Core.Tuple");
                assert_eq!(args.len(), 3);

                // Check first element is Integer64
                match &args[0] {
                    Type::Concrete { id, .. } => assert_eq!(id.0, "Outrun.Core.Integer64"),
                    _ => panic!("Expected Integer64 first element"),
                }

                // Check second element is String
                match &args[1] {
                    Type::Concrete { id, .. } => assert_eq!(id.0, "Outrun.Core.String"),
                    _ => panic!("Expected String second element"),
                }

                // Check third element is Boolean
                match &args[2] {
                    Type::Concrete { id, .. } => assert_eq!(id.0, "Outrun.Core.Boolean"),
                    _ => panic!("Expected Boolean third element"),
                }
            }
            _ => panic!("Expected Tuple<Integer64, String, Boolean> type"),
        }
    }

    #[test]
    fn test_map_literal_inference() {
        let mut engine = TypeInferenceEngine::new();
        let mut context = InferenceContext::new();

        // Create map literal: {"key1": 42, "key2": 24}
        use outrun_parser::{
            Expression, ExpressionKind, IntegerFormat, IntegerLiteral, MapEntry, MapLiteral, Span,
            StringFormat, StringLiteral, StringPart,
        };

        let map_literal = MapLiteral {
            entries: vec![
                MapEntry::Assignment {
                    key: Box::new(Expression {
                        kind: ExpressionKind::String(StringLiteral {
                            parts: vec![StringPart::Text {
                                content: "key1".to_string(),
                                raw_content: "key1".to_string(),
                            }],
                            format: StringFormat::Basic,
                            span: Span::new(1, 7),
                        }),
                        span: Span::new(1, 7),
                        type_info: None,
                    }),
                    value: Box::new(Expression {
                        kind: ExpressionKind::Integer(IntegerLiteral {
                            value: 42,
                            format: IntegerFormat::Decimal,
                            raw_text: "42".to_string(),
                            span: Span::new(9, 11),
                        }),
                        span: Span::new(9, 11),
                        type_info: None,
                    }),
                },
                MapEntry::Assignment {
                    key: Box::new(Expression {
                        kind: ExpressionKind::String(StringLiteral {
                            parts: vec![StringPart::Text {
                                content: "key2".to_string(),
                                raw_content: "key2".to_string(),
                            }],
                            format: StringFormat::Basic,
                            span: Span::new(13, 19),
                        }),
                        span: Span::new(13, 19),
                        type_info: None,
                    }),
                    value: Box::new(Expression {
                        kind: ExpressionKind::Integer(IntegerLiteral {
                            value: 24,
                            format: IntegerFormat::Decimal,
                            raw_text: "24".to_string(),
                            span: Span::new(21, 23),
                        }),
                        span: Span::new(21, 23),
                        type_info: None,
                    }),
                },
            ],
            span: Span::new(0, 24),
        };

        let result = engine
            .infer_map_literal(&map_literal, &mut context)
            .unwrap();

        // Should infer Map<Outrun.Core.String, Outrun.Core.Integer64>
        match &result.inferred_type {
            Type::Concrete { id, args, .. } => {
                assert_eq!(id.0, "Outrun.Core.Map");
                assert_eq!(args.len(), 2);

                // Check key type is String
                match &args[0] {
                    Type::Concrete { id, .. } => assert_eq!(id.0, "Outrun.Core.String"),
                    _ => panic!("Expected String key type"),
                }

                // Check value type is Integer64
                match &args[1] {
                    Type::Concrete { id, .. } => assert_eq!(id.0, "Outrun.Core.Integer64"),
                    _ => panic!("Expected Integer64 value type"),
                }
            }
            _ => panic!("Expected Map<String, Integer64> type"),
        }
    }
}

/// Helper function to check if a string looks like a type parameter name
/// Type parameters typically follow patterns like: T, U, K, V, or start with T (T1, TKey, etc.)
pub fn is_type_parameter_name(name: &str) -> bool {
    // Single uppercase letter (most common type parameters)
    if name.len() == 1 && name.chars().next().unwrap().is_ascii_uppercase() {
        return true;
    }
    
    // Starts with T (common type parameter convention)
    if name.starts_with('T') && name.len() > 1 {
        // But not if it's a concrete type like "Type"
        return name != "Type";
    }
    
    // Common type parameter names (but exclude concrete types)
    matches!(name, "Key" | "Value" | "Input" | "Output" | "Result" | "Error") &&
    // Exclude common concrete types that might match these patterns
    !matches!(name, "Type" | "String" | "Integer" | "Boolean" | "List" | "Map" | "Option" | "Either")
}

#[cfg(test)]
mod debug_tests {
    #[test]
    fn test_debug_unary_minus_constraint_resolution() {
        use outrun_parser::parse_program;
        use crate::{CompilationResult, Package};
        use crate::desugaring::DesugaringEngine;

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
            Ok(_) => {},
            Err(_e) => {},
        }
    }
}

