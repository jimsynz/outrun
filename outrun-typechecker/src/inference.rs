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

/// Result type for function clause inference to reduce complexity
type FunctionClauseInferenceResult = (Vec<(String, Type)>, Type, Vec<crate::types::Constraint>);

/// Main type inference engine that orchestrates all typechecker components
pub struct TypeInferenceEngine {
    /// Registry of protocol implementations (shared)
    protocol_registry: Rc<ProtocolRegistry>,

    /// Registry of function signatures (shared)
    function_registry: Rc<FunctionRegistry>,

    /// Unified type registry for protocols and concrete types
    type_registry: TypeRegistry,

    /// Current module being processed
    current_module: ModuleId,

    /// Counter for generating fresh type variables
    type_variable_counter: usize,

    /// Symbol table mapping variable names to types in current scope
    pub symbol_table: HashMap<String, Type>,

    /// Error context for enhanced error reporting
    error_context: ErrorContext,

    /// Current Self binding context for type resolution
    current_self_context: SelfBindingContext,

    /// Registry of struct definitions for field access resolution
    struct_registry: HashMap<String, outrun_parser::StructDefinition>,

    /// Current generic parameter context for type resolution
    generic_parameter_context: HashMap<String, Type>,
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
        for (i, (_param_name, param_type)) in params.iter().enumerate() {
            analyzer.analyze_type_in_position(
                param_type,
                &crate::types::TypePosition::ArgumentPosition { 
                    index: i, 
                    path: vec![] 
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
                crate::types::TypePosition::ArgumentPosition { index, path } => {
                    let mut full_path = vec![format!("arg_{}", index)];
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
                    crate::types::TypePosition::ArgumentPosition { index, path } => {
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
        let protocol_registry = Rc::new(ProtocolRegistry::new());
        let mut function_registry = FunctionRegistry::new();

        // Register all intrinsic functions that are provided by the runtime
        crate::intrinsics::register_intrinsics(&mut function_registry);

        let function_registry = Rc::new(function_registry);

        Self {
            protocol_registry,
            function_registry,
            type_registry: TypeRegistry::with_core_types(),
            current_module: ModuleId::new("main"),
            type_variable_counter: 0,
            symbol_table: HashMap::new(),
            error_context: ErrorContext::new(),
            current_self_context: SelfBindingContext::ProtocolDefinition {
                protocol_id: ProtocolId::new("Unknown"),
                protocol_args: vec![],
            },
            struct_registry: HashMap::new(),
            generic_parameter_context: HashMap::new(),
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
        // We need to get a mutable reference to the registry
        if let Some(registry) = Rc::get_mut(&mut self.protocol_registry) {
            registry.set_current_module(module);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.protocol_registry).clone();
            new_registry.set_current_module(module);
            self.protocol_registry = Rc::new(new_registry);
        }
    }

    /// Get a mutable reference to the protocol registry
    pub fn protocol_registry_mut(&mut self) -> &mut ProtocolRegistry {
        // Always clone and replace to avoid borrow checker issues
        let new_registry = (*self.protocol_registry).clone();
        self.protocol_registry = Rc::new(new_registry);
        Rc::get_mut(&mut self.protocol_registry)
            .expect("Should be uniquely owned after replacement")
    }

    /// Get mutable access to type registry for testing and configuration
    pub fn type_registry_mut(&mut self) -> &mut TypeRegistry {
        &mut self.type_registry
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

    /// Get a cloned Rc to the protocol registry for compilation results
    pub fn protocol_registry_rc(&self) -> std::rc::Rc<ProtocolRegistry> {
        self.protocol_registry.clone()
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
        &self.protocol_registry
    }

    /// Get a reference to the protocol registry for dispatch table building
    pub fn protocol_registry(&self) -> &ProtocolRegistry {
        &self.protocol_registry
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
            generic_parameters: struct_generic_params,
            is_generic,
        };

        // Register the function in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(struct_name.to_string(), function_name, function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(struct_name.to_string(), function_name, function_info);
            self.function_registry = Rc::new(new_registry);
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

        // TODO: Extract required protocols from constraints/requirements
        let required_protocols = HashSet::new();

        self.protocol_registry_mut().register_protocol_definition(
            protocol_id.clone(),
            required_protocols,
            module_id.clone(),
            default_implementations,
            required_functions,
            Some(protocol_def.span),
        );

        // Also register the protocol in the type registry so that convert_type_annotation
        // can distinguish protocols from concrete types
        self.type_registry.protocol_registry_mut().register_protocol_definition(
            protocol_id,
            HashSet::new(), // Don't duplicate requirement tracking
            module_id,
            HashSet::new(), // Don't duplicate default implementation tracking  
            HashSet::new(), // Don't duplicate function tracking
            Some(protocol_def.span),
        );

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
        self.current_self_context = SelfBindingContext::Implementation {
            implementing_type: crate::types::TypeId::new(&type_name),
            implementing_args,
            protocol_id: ProtocolId::new(&protocol_name),
            protocol_args: vec![], // TODO: Extract from protocol spec if needed
        };

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
            registry.register_function(impl_scope.to_string(), function_name, function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(impl_scope.to_string(), function_name, function_info);
            self.function_registry = Rc::new(new_registry);
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
            registry.register_function(self.current_module.0.clone(), function_name, function_info);
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                self.current_module.0.clone(),
                function_name,
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
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

        match &item.kind {
            ItemKind::StructDefinition(struct_def) => self.typecheck_struct_functions(struct_def),
            ItemKind::ProtocolDefinition(protocol_def) => {
                self.typecheck_protocol_functions(protocol_def)
            }
            ItemKind::ImplBlock(impl_block) => self.typecheck_impl_block_functions(impl_block),
            ItemKind::FunctionDefinition(func_def) => self.typecheck_standalone_function(func_def),
            ItemKind::LetBinding(let_binding) => self.typecheck_let_binding(let_binding),
            // Other items don't need body type checking
            ItemKind::ConstDefinition(_)
            | ItemKind::Expression(_)
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

        for protocol_function in &protocol_def.functions {
            match protocol_function {
                ProtocolFunction::Definition(definition) => {
                    self.typecheck_function_body(&protocol_name, definition)?;
                }
                ProtocolFunction::StaticDefinition(static_def) => {
                    self.typecheck_static_function_body(&protocol_name, static_def)?;
                }
                ProtocolFunction::Signature(_) => {
                    // Signatures don't have bodies to check
                }
            }
        }

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
        _let_binding: &outrun_parser::LetBinding,
    ) -> Result<(), TypecheckError> {
        // TODO: Implement let binding type checking
        Ok(())
    }

    /// Core function body type checking logic using RefCell for interior mutability
    #[allow(clippy::result_large_err)]
    fn typecheck_function_body(
        &mut self,
        scope: &str,
        function: &FunctionDefinition,
    ) -> Result<(), TypecheckError> {
        // Create a new inference context for this function
        let mut function_context = InferenceContext {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: None,
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

        // Convert the declared return type
        let declared_return_type = self.convert_type_annotation(&function.return_type)?;

        // Verify that the body type matches the declared return type
        if !self.types_are_compatible(&body_type, &declared_return_type) {
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
        // Create a new inference context for this function
        let mut function_context = InferenceContext {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: None,
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

        // Convert the declared return type
        let declared_return_type = self.convert_type_annotation(&static_def.return_type)?;

        // Verify that the body type matches the declared return type
        if !self.types_are_compatible(&body_type, &declared_return_type) {
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
                self.protocol_registry
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
                self.protocol_registry
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

            // Different categories are incompatible
            _ => false,
        }
    }

    /// Infer the type of an expression
    #[allow(clippy::result_large_err)]
    pub fn infer_expression(
        &mut self,
        expression: &mut Expression,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::ExpressionKind;

        match &mut expression.kind {
            ExpressionKind::Boolean(_) => {
                // Boolean literals have concrete type Outrun.Core.Boolean
                let inferred_type = Type::concrete("Outrun.Core.Boolean");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Integer(_) => {
                // Integer literals have concrete type Outrun.Core.Integer64
                let inferred_type = Type::concrete("Outrun.Core.Integer64");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Float(_) => {
                // Float literals have concrete type Outrun.Core.Float64
                let inferred_type = Type::concrete("Outrun.Core.Float64");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::String(_) => {
                // String literals have concrete type Outrun.Core.String
                let inferred_type = Type::concrete("Outrun.Core.String");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Atom(_) => {
                // Atom literals have concrete type Outrun.Core.Atom
                let inferred_type = Type::concrete("Outrun.Core.Atom");
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
            ExpressionKind::Identifier(identifier) => {
                // Variable lookup
                self.infer_variable(identifier, context)
            }
            ExpressionKind::FunctionCall(function_call) => {
                // Function call inference with dispatch integration
                self.infer_function_call(function_call, context)
            }
            ExpressionKind::List(list_literal) => {
                // List literal type inference
                self.infer_list_literal(list_literal, context)
            }
            ExpressionKind::Tuple(tuple_literal) => {
                // Tuple literal type inference
                self.infer_tuple_literal(tuple_literal, context)
            }
            ExpressionKind::Map(map_literal) => {
                // Map literal type inference
                self.infer_map_literal(map_literal, context)
            }
            ExpressionKind::AnonymousFunction(anonymous_fn) => {
                // Anonymous function type inference
                self.infer_anonymous_function(anonymous_fn, context)
            }
            ExpressionKind::QualifiedIdentifier(qualified_id) => {
                // Qualified identifier inference (e.g., Module.function, BinaryAddition.add)
                self.infer_qualified_identifier(qualified_id, context)
            }
            ExpressionKind::Parenthesized(inner_expr) => {
                // Parenthesized expression - just infer the inner expression
                let mut inner_expr_mut = (**inner_expr).clone();
                self.infer_expression(&mut inner_expr_mut, context)
            }
            ExpressionKind::FieldAccess(field_access) => {
                // Field access: object.field
                let mut object_expr = (*field_access.object).clone();
                let object_result = self.infer_expression(&mut object_expr, context)?;

                // Resolve the field type from the object type
                self.infer_field_access_type(
                    &object_result.inferred_type,
                    &field_access.field.name,
                    context,
                )
            }
            // TODO: Implement other expression types (IfExpression, CaseExpression, etc.)
            _ => {
                // For remaining expressions, assign a fresh type variable
                let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
        }
    }

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

    /// Infer the type of a function call with dispatch integration
    #[allow(clippy::result_large_err)]
    fn infer_function_call(
        &mut self,
        function_call: &mut outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::FunctionPath;

        match function_call.path.clone() {
            FunctionPath::Simple { name } => {
                // Local function call - resolve in current context
                self.infer_local_function_call(function_call, &name, context)
            }
            FunctionPath::Qualified { module, name } => {
                // Qualified function call - static protocol or module call
                self.infer_qualified_function_call(function_call, &module, &name, context)
            }
            FunctionPath::Expression { expression: _ } => {
                // Higher-order function call - not implemented yet
                // For now, return a fresh type variable
                let inferred_type = Type::variable(self.fresh_type_var(), Level(0));
                Ok(InferenceResult {
                    inferred_type,
                    constraints: context.constraints.clone(),
                    substitution: context.substitution.clone(),
                })
            }
        }
    }

    /// Infer a local function call (unqualified name)
    #[allow(clippy::result_large_err)]
    fn infer_local_function_call(
        &mut self,
        function_call: &mut outrun_parser::FunctionCall,
        name: &outrun_parser::Identifier,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        let function_name = &name.name;

        // Create a function dispatcher with the appropriate context
        let function_context = self.create_function_context_from_inference_context(context);
        let dispatcher = FunctionDispatcher::new(&self.protocol_registry, &self.function_registry, None, None)
            .with_context(function_context);

        // Try to resolve local function call using dispatcher
        match dispatcher.resolve_local_call(function_name, Some(name.span)) {
            Ok(dispatch_result) => {
                // Set the resolved function key for direct interpreter dispatch
                if let crate::dispatch::DispatchResult::Resolved(resolved_func) = &dispatch_result {
                    function_call.resolved_function_key = Some(resolved_func.qualified_name.clone());
                    eprintln!("DEBUG: Local function call resolved to key: '{}'", resolved_func.qualified_name);
                }
                self.handle_dispatch_result(dispatch_result, &mut function_call.arguments, context)
            }
            Err(dispatch_error) => {
                // Convert dispatch error to type error
                Err(TypecheckError::DispatchError(dispatch_error))
            }
        }
    }

    /// Infer a qualified function call (Module.function) using comprehensive signature analysis
    #[allow(clippy::result_large_err)]
    fn infer_qualified_function_call(
        &mut self,
        function_call: &mut outrun_parser::FunctionCall,
        module: &outrun_parser::TypeIdentifier,
        name: &outrun_parser::Identifier,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        let qualified_name = format!("{}.{}", module.name, name.name);
        let protocol_id = crate::types::ProtocolId::new(&module.name);

        // Check if this is a protocol call requiring Self/generic type resolution
        let target_type = if self.protocol_registry.has_protocol(&protocol_id) {
            // Use comprehensive signature analysis for protocol calls
            self.resolve_protocol_call_target_type(&qualified_name, function_call, context)?
        } else {
            None
        };

        // Create a function dispatcher
        let dispatcher = FunctionDispatcher::new(&self.protocol_registry, &self.function_registry, None, None);

        // Try to resolve qualified function call using dispatcher
        match dispatcher.resolve_qualified_call(&qualified_name, target_type.as_ref(), Some(name.span)) {
            Ok(dispatch_result) => {
                // Generate proper monomorphised key for dispatch table
                if let crate::dispatch::DispatchResult::Resolved(resolved_func) = &dispatch_result {
                    let monomorphised_key = if let Some(ref target) = target_type {
                        // Protocol call: use Protocol.function:TargetType format
                        format!("{}:{}", resolved_func.qualified_name, target)
                    } else {
                        // Static call: use qualified name as-is
                        resolved_func.qualified_name.clone()
                    };
                    function_call.resolved_function_key = Some(monomorphised_key);
                }
                self.handle_dispatch_result(dispatch_result, &mut function_call.arguments, context)
            }
            Err(dispatch_error) => {
                // Check if this is an Outrun.Intrinsic call that can be lazily registered
                if module.name == "Outrun.Intrinsic" {
                    match self
                        .try_lazy_intrinsic_registration(&name.name, &function_call.arguments, name.span, context)
                    {
                        Ok(result) => {
                            function_call.resolved_function_key = Some(qualified_name.clone());
                            return Ok(result);
                        }
                        Err(_) => {
                            // Fall through to original error if lazy registration fails
                        }
                    }
                }

                Err(TypecheckError::DispatchError(dispatch_error))
            }
        }
    }

    /// Resolve target type for protocol calls using comprehensive Self/generic analysis
    #[allow(clippy::result_large_err)]
    fn resolve_protocol_call_target_type(
        &mut self,
        qualified_name: &str,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
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

        // Infer argument types to find Self type candidates
        let mut inferred_argument_types = Vec::new();
        for argument in &function_call.arguments {
            match argument {
                outrun_parser::Argument::Named { expression, .. } => {
                    let mut expr_clone = expression.clone();
                    match self.infer_expression(&mut expr_clone, context) {
                        Ok(result) => inferred_argument_types.push(Some(result.inferred_type)),
                        Err(_) => inferred_argument_types.push(None),
                    }
                }
                _ => inferred_argument_types.push(None),
            }
        }

        // Extract Self type from argument positions
        let self_type = self.extract_self_from_argument_positions(
            &signature_analysis.self_positions,
            &function_parameters,
            &inferred_argument_types,
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

    /// Extract Self type from argument positions using comprehensive position analysis
    #[allow(clippy::result_large_err)]
    fn extract_self_from_argument_positions(
        &self,
        self_positions: &[crate::types::SelfPosition],
        function_parameters: &[(String, Type)],
        inferred_argument_types: &[Option<Type>],
    ) -> Result<Option<Type>, TypecheckError> {
        let mut candidate_self_types = Vec::new();

        for self_position in self_positions {
            // Parse position path to find argument index and nested path
            if let Some(arg_index) = self.parse_argument_position(&self_position.path) {
                if let (Some(Some(arg_type)), Some((_, param_type))) = 
                    (inferred_argument_types.get(arg_index), function_parameters.get(arg_index)) {
                    
                    // Extract Self type from this position
                    if let Some(extracted_self) = self.extract_self_from_position_path(
                        arg_type,
                        param_type,
                        &self_position.path,
                    ) {
                        candidate_self_types.push(extracted_self);
                    }
                }
            }
        }

        // Unify all candidate Self types to ensure consistency
        if candidate_self_types.is_empty() {
            Ok(None)
        } else if candidate_self_types.len() == 1 {
            Ok(Some(candidate_self_types.into_iter().next().unwrap()))
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

    /// Parse argument position from type position path (e.g., ["param_0"] -> Some(0))
    fn parse_argument_position(&self, path: &[String]) -> Option<usize> {
        if let Some(first_segment) = path.first() {
            if first_segment.starts_with("param_") {
                first_segment[6..].parse().ok()
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Extract Self type from nested position within a type structure
    fn extract_self_from_position_path(
        &self,
        inferred_type: &Type,
        expected_type: &Type,
        position_path: &[String],
    ) -> Option<Type> {
        // If path is empty, the whole type should be Self
        if position_path.is_empty() || (position_path.len() == 1 && position_path[0].starts_with("param_")) {
            return Some(inferred_type.clone());
        }

        // Navigate through generic type structure to extract Self
        // For example: Option<Self> with path ["param_0", "0"] means Self is the first generic argument
        match (inferred_type, expected_type) {
            (
                Type::Concrete { args: inferred_args, .. },
                Type::Concrete { args: expected_args, .. },
            ) | (
                Type::Protocol { args: inferred_args, .. },
                Type::Protocol { args: expected_args, .. },
            ) => {
                // Find the matching generic argument position
                for (i, expected_arg) in expected_args.iter().enumerate() {
                    if expected_arg.is_self_type() {
                        return inferred_args.get(i).cloned();
                    }
                }
                None
            }
            _ => None,
        }
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

    /// Extract type from constraint position for unification validation
    fn extract_type_from_constraint_position(
        &self,
        position: &crate::types::TypePosition,
        self_type: &Type,
        function_parameters: &[(String, Type)],
        inferred_argument_types: &[Option<Type>],
    ) -> Option<Type> {
        match position {
            crate::types::TypePosition::ArgumentPosition { index, path } => {
                if let Some(Some(arg_type)) = inferred_argument_types.get(*index) {
                    if path.is_empty() {
                        Some(arg_type.clone())
                    } else {
                        // Navigate through nested type structure
                        self.navigate_type_path(arg_type, path)
                    }
                } else {
                    None
                }
            }
            crate::types::TypePosition::SelfPosition { .. } => Some(self_type.clone()),
            _ => None, // Other position types not yet implemented
        }
    }

    /// Navigate through nested type structure using path components
    fn navigate_type_path(&self, base_type: &Type, path: &[String]) -> Option<Type> {
        let mut current_type = base_type;
        
        for path_component in path {
            match current_type {
                Type::Concrete { args, .. } | Type::Protocol { args, .. } => {
                    if let Ok(index) = path_component.parse::<usize>() {
                        if let Some(arg) = args.get(index) {
                            current_type = arg;
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        }
        
        Some(current_type.clone())
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

    /// Handle a dispatch result and infer the function call type
    #[allow(clippy::result_large_err)]
    fn handle_dispatch_result(
        &mut self,
        dispatch_result: crate::dispatch::DispatchResult,
        arguments: &mut [outrun_parser::Argument],
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use crate::dispatch::DispatchResult;

        match dispatch_result {
            DispatchResult::Resolved(resolved_function) => {
                self.infer_resolved_function_call(&resolved_function, arguments, context)
            }
            DispatchResult::Ambiguous(candidates) => {
                // Multiple candidates found - this is an error for now
                // In a full implementation, we might try to disambiguate based on argument types
                Err(TypecheckError::InferenceError(
                    crate::error::InferenceError::AmbiguousType {
                        span: None,
                        suggestions: vec![format!(
                            "Multiple candidates found: {}",
                            candidates
                                .iter()
                                .map(|c| c.qualified_name.clone())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )],
                    },
                ))
            }
            DispatchResult::NotFound => {
                // Function not found
                Err(TypecheckError::InferenceError(
                    crate::error::InferenceError::AmbiguousType {
                        span: None,
                        suggestions: vec!["Function not found".to_string()],
                    },
                ))
            }
        }
    }

    /// Infer the result of a resolved function call
    #[allow(clippy::result_large_err)]
    fn infer_resolved_function_call(
        &mut self,
        resolved_function: &crate::dispatch::ResolvedFunction,
        arguments: &mut [outrun_parser::Argument],
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // First, infer types of all arguments
        let mut inferred_arguments = Vec::new();
        let mut all_constraints = context.constraints.clone();
        let current_substitution = context.substitution.clone();

        for argument in arguments.iter_mut() {
            let arg_result = self.infer_argument(argument, context)?;
            inferred_arguments.push(arg_result.inferred_type.clone());
            all_constraints.extend(arg_result.constraints);
            // TODO: In a full implementation, we would compose substitutions properly
        }

        // Check argument count matches expected parameters
        if inferred_arguments.len() != resolved_function.function_info.parameters.len() {
            return Err(TypecheckError::InferenceError(
                crate::error::InferenceError::AmbiguousType {
                    span: None,
                    suggestions: vec![format!(
                        "Expected {} arguments, found {}",
                        resolved_function.function_info.parameters.len(),
                        inferred_arguments.len()
                    )],
                },
            ));
        }

        // Phase 3.5: Generate constraints for generic function calls
        if resolved_function.function_info.is_generic {
            let generic_constraints = self.generate_generic_function_constraints(
                &resolved_function.function_info,
                &inferred_arguments,
                context,
            )?;
            all_constraints.extend(generic_constraints);
        } else {
            // Create constraints for parameter type matching (non-generic case)
            for (arg_type, (_param_name, param_type)) in inferred_arguments
                .iter()
                .zip(resolved_function.function_info.parameters.iter())
            {
                // Create constraint that argument type must match parameter type
                let constraint = Constraint::Equality {
                    left: Box::new(param_type.clone()),
                    right: Box::new(arg_type.clone()),
                    span: None,
                };
                all_constraints.push(constraint);
            }
        }

        // For generic functions, we need to handle type parameter instantiation
        let return_type = if resolved_function.function_info.is_generic {
            self.instantiate_generic_return_type(
                &resolved_function.function_info.return_type,
                &resolved_function.function_info,
                &inferred_arguments,
            )?
        } else {
            resolved_function.function_info.return_type.clone()
        };

        Ok(InferenceResult {
            inferred_type: return_type,
            constraints: all_constraints,
            substitution: current_substitution,
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
            let param_constraints = self.extract_type_parameter_constraints(
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
                let param_constraints = self.generate_type_parameter_equality_constraints(
                    param_type,
                    type_param,
                    inferred_type,
                    param_index,
                )?;
                constraints.extend(param_constraints);
            }
            
            // Also constrain the return type if it contains this type parameter
            let return_constraints = self.generate_type_parameter_equality_constraints(
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
        &self,
        param_type: &Type,
        arg_type: &Type,
        type_param_mappings: &mut HashMap<String, Type>,
        param_index: usize,
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
                                    let nested_constraints = self.extract_type_parameter_constraints(
                                        param_arg,
                                        arg_arg,
                                        type_param_mappings,
                                        param_index,
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
        &self,
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
                        let nested_constraints = self.generate_type_parameter_equality_constraints(
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
            self.collect_type_parameter_substitutions(param_type, arg_type, &mut substitutions, &function_info.generic_parameters)?;
        }
        
        
        // Apply substitutions to return type
        self.substitute_type_parameters(return_type, &substitutions)
    }
    
    /// Collect type parameter substitutions from parameter-argument type matching
    #[allow(clippy::result_large_err)]
    fn collect_type_parameter_substitutions(
        &self,
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
                                self.collect_type_parameter_substitutions(param_arg, arg_arg, substitutions, generic_parameters)?;
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
    
    /// Apply type parameter substitutions to a type
    #[allow(clippy::result_large_err)]
    fn substitute_type_parameters(
        &self,
        typ: &Type,
        substitutions: &HashMap<String, Type>,
    ) -> Result<Type, TypecheckError> {
        match typ {
            Type::Concrete { id, args, span } => {
                if let Some(substitution) = substitutions.get(id.name()) {
                    // This type parameter should be substituted
                    Ok(substitution.clone())
                } else {
                    // Recursively substitute in generic arguments
                    let mut substituted_args = Vec::new();
                    for arg in args {
                        substituted_args.push(self.substitute_type_parameters(arg, substitutions)?);
                    }
                    Ok(Type::Concrete {
                        id: id.clone(),
                        args: substituted_args,
                        span: *span,
                    })
                }
            }
            other => Ok(other.clone()),
        }
    }

    /// Infer the type of a function argument
    #[allow(clippy::result_large_err)]
    fn infer_argument(
        &mut self,
        argument: &mut outrun_parser::Argument,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        match argument {
            outrun_parser::Argument::Named { expression, .. } => {
                // Infer the type of the argument expression (mutating the original)
                self.infer_expression(expression, context)
            }
            outrun_parser::Argument::Spread { expression, .. } => {
                // Spread arguments are complex - for now, just infer the expression type
                self.infer_expression(expression, context)
            }
        }
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

    /// Check if a type name refers to a protocol rather than a concrete type
    fn is_protocol_type(&self, type_name: &str) -> bool {
        self.is_known_protocol(type_name)
    }

    /// Check if a type name is a known protocol (by looking in the registry)
    fn is_known_protocol(&self, type_name: &str) -> bool {
        let protocol_id = ProtocolId::new(type_name);
        self.protocol_registry.has_protocol(&protocol_id)
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
                        Some(crate::registry::TypeKind::Protocol(protocol_def)) => {
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
        match ty {
            Type::Concrete { id, args, span } => {
                // If this is a generic parameter, substitute it
                if let Some(substituted_type) = substitution.get(id.name()) {
                    Ok(substituted_type.clone())
                } else {
                    // Apply substitution to generic arguments
                    let substituted_args: Result<Vec<Type>, TypecheckError> = args
                        .into_iter()
                        .map(|arg| self.apply_generic_substitution_to_type(arg, substitution))
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
                    .map(|arg| self.apply_generic_substitution_to_type(arg, substitution))
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

/// Helper function to check if a type name is a generic parameter in a function
/// This is a simple implementation - in a full system, we'd check against the function's generic_parameters
fn function_info_contains_generic_param(name: &str, _generic_params: &[String]) -> bool {
    // For now, use the same heuristic as is_type_parameter_name
    // In a complete implementation, we would check if name is in the generic_parameters list
    is_type_parameter_name(name)
}
