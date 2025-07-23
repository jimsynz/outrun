//! Type inference engine orchestration layer
//!
//! This module provides the main orchestration layer that coordinates all typechecker components
//! to perform Hindley-Milner type inference with protocol constraints and dispatch resolution.

use crate::{
    dispatch::{FunctionDispatcher, FunctionRegistry, FunctionInfo, FunctionVisibility as DispatchVisibility},
    error::{TypecheckError, ErrorContext, InferenceError, UnificationError as OriginalUnificationError, to_source_span},
    registry::ProtocolRegistry,
    types::{Type, TypeVarId, Substitution, Constraint, SelfBindingContext, ModuleId, Level, ProtocolId},
};
use outrun_parser::{
    Item, Program, Expression, StructDefinition, ProtocolDefinition, ProtocolFunction,
    FunctionDefinition, FunctionSignature, StaticFunctionDefinition, ConstDefinition, 
    ImplBlock, TypeAnnotation, TypeSpec
};
use std::collections::HashMap;
use std::rc::Rc;

/// Result type for function clause inference to reduce complexity
type FunctionClauseInferenceResult = (Vec<(String, Type)>, Type, Vec<crate::types::Constraint>);

/// Main type inference engine that orchestrates all typechecker components
pub struct TypeInferenceEngine {
    /// Registry of protocol implementations (shared)
    protocol_registry: Rc<ProtocolRegistry>,
    
    /// Registry of function signatures (shared)
    function_registry: Rc<FunctionRegistry>,
    
    /// Current module being processed
    current_module: ModuleId,
    
    /// Counter for generating fresh type variables
    type_variable_counter: usize,
    
    /// Symbol table mapping variable names to types in current scope
    pub symbol_table: HashMap<String, Type>,
    
    /// Error context for enhanced error reporting
    error_context: ErrorContext,
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

impl TypeInferenceEngine {
    /// Create a new type inference engine
    pub fn new() -> Self {
        let protocol_registry = Rc::new(ProtocolRegistry::new());
        let function_registry = Rc::new(FunctionRegistry::new());
        
        Self {
            protocol_registry,
            function_registry,
            current_module: ModuleId::new("main"),
            type_variable_counter: 0,
            symbol_table: HashMap::new(),
            error_context: ErrorContext::new(),
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
    
    /// Get a mutable reference to the protocol registry for testing
    #[cfg(test)]
    pub fn protocol_registry_mut(&mut self) -> &mut ProtocolRegistry {
        // Always clone and replace to avoid borrow checker issues
        let new_registry = (*self.protocol_registry).clone();
        self.protocol_registry = Rc::new(new_registry);
        Rc::get_mut(&mut self.protocol_registry).expect("Should be uniquely owned after replacement")
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
    
    /// Type check a complete program
    #[allow(clippy::result_large_err)]
    pub fn typecheck_program(&mut self, program: &mut Program) -> Result<(), TypecheckError> {
        // Phase 1: Collect all definitions
        self.collect_definitions(program)?;
        
        // Phase 2: Register all implementations
        self.register_implementations(program)?;
        
        // Phase 3: Type check all items
        for item in &mut program.items {
            self.typecheck_item(item)?;
        }
        
        Ok(())
    }
    
    /// Collect all type and protocol definitions from the program
    #[allow(clippy::result_large_err)]
    pub fn collect_definitions(&mut self, program: &Program) -> Result<(), TypecheckError> {
        // Traverse AST and collect all definitions
        for item in &program.items {
            self.collect_item_definitions(item)?;
        }
        
        Ok(())
    }
    
    /// Collect definitions from a single item
    #[allow(clippy::result_large_err)]
    fn collect_item_definitions(&mut self, item: &Item) -> Result<(), TypecheckError> {
        use outrun_parser::ItemKind;
        
        match &item.kind {
            ItemKind::StructDefinition(struct_def) => {
                self.collect_struct_definition(struct_def)?;
            }
            ItemKind::ProtocolDefinition(protocol_def) => {
                self.collect_protocol_definition(protocol_def)?;
            }
            ItemKind::FunctionDefinition(func_def) => {
                self.collect_function_definition(func_def)?;
            }
            ItemKind::ConstDefinition(const_def) => {
                self.collect_const_definition(const_def)?;
            }
            ItemKind::ImplBlock(impl_block) => {
                // Collect impl block functions during definition collection
                self.collect_impl_block_functions(impl_block)?;
                // Also note for later implementation registration
                self.note_implementation_block(impl_block)?;
            }
            // Other items don't define types or affect the symbol table at this stage
            ItemKind::LetBinding(_) |
            ItemKind::Expression(_) |
            ItemKind::Comment(_) |
            ItemKind::ImportDefinition(_) |
            ItemKind::AliasDefinition(_) |
            ItemKind::MacroDefinition(_) => {
                // These will be handled in later phases
            }
            // Literals at the top level are expressions
            ItemKind::Keyword(_) |
            ItemKind::BooleanLiteral(_) |
            ItemKind::IntegerLiteral(_) |
            ItemKind::FloatLiteral(_) |
            ItemKind::StringLiteral(_) |
            ItemKind::AtomLiteral(_) |
            ItemKind::SigilLiteral(_) |
            ItemKind::ListLiteral(_) |
            ItemKind::MapLiteral(_) |
            ItemKind::TupleLiteral(_) |
            ItemKind::Identifier(_) |
            ItemKind::TypeIdentifier(_) => {
                // These are treated as top-level expressions
            }
        }
        
        Ok(())
    }
    
    /// Collect a struct definition
    #[allow(clippy::result_large_err)]
    fn collect_struct_definition(&mut self, struct_def: &StructDefinition) -> Result<(), TypecheckError> {
        // Extract struct name (handle nested names like Module.SubModule.StructName)
        let struct_name = struct_def.name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        
        // Collect struct associated functions (functions defined in the struct block)
        for function in &struct_def.functions {
            self.collect_struct_function(&struct_name, function)?;
        }
        
        // TODO: Extract struct fields and add to type registry
        // TODO: Handle generic parameters
        Ok(())
    }
    
    /// Collect a struct function (function defined within a struct block)
    #[allow(clippy::result_large_err)]
    fn collect_struct_function(&mut self, struct_name: &str, function: &FunctionDefinition) -> Result<(), TypecheckError> {
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
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = function.parameters
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
        
        // Create function info with qualified name
        let function_info = FunctionInfo {
            defining_scope: struct_name.to_string(), // Function belongs to struct scope
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            span: Some(function.span),
        };
        
        // Register the function in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                struct_name.to_string(),
                function_name,
                function_info,
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                struct_name.to_string(),
                function_name,
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }
        
        Ok(())
    }
    
    /// Collect a protocol definition
    #[allow(clippy::result_large_err)]
    fn collect_protocol_definition(&mut self, protocol_def: &ProtocolDefinition) -> Result<(), TypecheckError> {
        // Extract protocol name (handle nested names like Module.SubModule.ProtocolName)
        let protocol_name = protocol_def.name
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        
        // Collect protocol functions (signatures and implementations)
        for protocol_function in &protocol_def.functions {
            self.collect_protocol_function(&protocol_name, protocol_function)?;
        }
        
        // TODO: Register protocol in protocol registry
        // TODO: Handle generic parameters and constraints
        
        Ok(())
    }
    
    /// Collect a protocol function (signature, definition, or static definition)
    #[allow(clippy::result_large_err)]
    fn collect_protocol_function(&mut self, protocol_name: &str, protocol_function: &ProtocolFunction) -> Result<(), TypecheckError> {
        use outrun_parser::ProtocolFunction;
        
        match protocol_function {
            ProtocolFunction::Signature(signature) => {
                self.collect_protocol_function_signature(protocol_name, signature)?;
            }
            ProtocolFunction::Definition(definition) => {
                self.collect_protocol_function_definition(protocol_name, definition)?;
            }
            ProtocolFunction::StaticDefinition(static_def) => {
                // Static definitions are like regular functions but belong to protocol namespace
                self.collect_protocol_static_function(protocol_name, static_def)?;
            }
        }
        
        Ok(())
    }
    
    /// Collect a protocol function signature
    #[allow(clippy::result_large_err)]
    fn collect_protocol_function_signature(&mut self, protocol_name: &str, signature: &FunctionSignature) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = signature.name.name.clone();
        
        // Convert visibility
        let visibility = match signature.visibility {
            outrun_parser::FunctionVisibility::Public => DispatchVisibility::Public,
            outrun_parser::FunctionVisibility::Private => DispatchVisibility::Private,
        };
        
        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = signature.parameters
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
        
        // Create function info for protocol signature
        let function_info = FunctionInfo {
            defining_scope: protocol_name.to_string(), // Function belongs to protocol scope
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            span: Some(signature.span),
        };
        
        // Register the protocol function signature in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                protocol_name.to_string(),
                function_name,
                function_info,
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                protocol_name.to_string(),
                function_name,
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }
        
        Ok(())
    }
    
    /// Collect a protocol function definition (full implementation in protocol)
    #[allow(clippy::result_large_err)]
    fn collect_protocol_function_definition(&mut self, protocol_name: &str, definition: &FunctionDefinition) -> Result<(), TypecheckError> {
        // Protocol function definitions are handled like struct functions
        self.collect_struct_function(protocol_name, definition)
    }
    
    /// Collect a protocol static function definition
    #[allow(clippy::result_large_err)]
    fn collect_protocol_static_function(&mut self, protocol_name: &str, static_def: &StaticFunctionDefinition) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = static_def.name.name.clone();
        
        // Static functions are always public in protocols
        let visibility = DispatchVisibility::Public;
        
        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = static_def.parameters
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
        
        // Create function info for static function
        let function_info = FunctionInfo {
            defining_scope: protocol_name.to_string(), // Function belongs to protocol scope
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            span: Some(static_def.span),
        };
        
        // Register the static function in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                protocol_name.to_string(),
                function_name,
                function_info,
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                protocol_name.to_string(),
                function_name,
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }
        
        Ok(())
    }
    
    /// Collect impl block functions
    #[allow(clippy::result_large_err)]
    fn collect_impl_block_functions(&mut self, impl_block: &ImplBlock) -> Result<(), TypecheckError> {
        // Extract impl block scope information
        // For impl blocks, the scope is typically "ProtocolName for TypeName"
        // but for function registration, we need a unique scope identifier
        
        // Extract protocol name
        let protocol_name = self.extract_type_spec_name(&impl_block.protocol_spec);
        
        // Extract type name
        let type_name = self.extract_type_spec_name(&impl_block.type_spec);
        
        // Create a qualified scope name for impl block functions
        // Format: "ProtocolName for TypeName"
        let impl_scope = format!("{} for {}", protocol_name, type_name);
        
        // Collect impl block functions
        for function in &impl_block.functions {
            self.collect_impl_block_function(&impl_scope, function)?;
        }
        
        Ok(())
    }
    
    /// Collect a single impl block function
    #[allow(clippy::result_large_err)]
    fn collect_impl_block_function(&mut self, impl_scope: &str, function: &FunctionDefinition) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = function.name.name.clone();
        
        // Convert visibility - impl functions can be public or private
        let visibility = match function.visibility {
            outrun_parser::FunctionVisibility::Public => DispatchVisibility::Public,
            outrun_parser::FunctionVisibility::Private => DispatchVisibility::Private,
        };
        
        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = function.parameters
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
        
        // Create function info for impl block function
        let function_info = FunctionInfo {
            defining_scope: impl_scope.to_string(), // Function belongs to impl scope
            function_name: function_name.clone(),
            visibility,
            parameters,
            return_type,
            span: Some(function.span),
        };
        
        // Register the impl function in the function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                impl_scope.to_string(),
                function_name,
                function_info,
            );
        } else {
            // If there are multiple references, we need to clone and replace
            let mut new_registry = (*self.function_registry).clone();
            new_registry.register_function(
                impl_scope.to_string(),
                function_name,
                function_info,
            );
            self.function_registry = Rc::new(new_registry);
        }
        
        Ok(())
    }
    
    /// Extract a simple name from a TypeSpec for scope identification
    fn extract_type_spec_name(&self, type_spec: &TypeSpec) -> String {
        // Extract the path from the TypeSpec
        type_spec.path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }
    
    /// Collect a function definition
    #[allow(clippy::result_large_err)]
    fn collect_function_definition(&mut self, func_def: &FunctionDefinition) -> Result<(), TypecheckError> {
        // Extract function name
        let function_name = func_def.name.name.clone();
        
        // Convert visibility
        let visibility = match func_def.visibility {
            outrun_parser::FunctionVisibility::Public => DispatchVisibility::Public,
            outrun_parser::FunctionVisibility::Private => DispatchVisibility::Private,
        };
        
        // Extract parameters
        let parameters: Result<Vec<(String, Type)>, TypecheckError> = func_def.parameters
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
            span: Some(func_def.span),
        };
        
        // Get mutable reference to function registry
        if let Some(registry) = Rc::get_mut(&mut self.function_registry) {
            registry.register_function(
                self.current_module.0.clone(),
                function_name,
                function_info,
            );
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
    fn collect_const_definition(&mut self, _const_def: &ConstDefinition) -> Result<(), TypecheckError> {
        // TODO: Extract constant name and type
        // TODO: Add to symbol table
        
        // For now, just record that we processed it
        Ok(())
    }
    
    /// Note an implementation block (will be processed later)
    #[allow(clippy::result_large_err)]
    fn note_implementation_block(&mut self, _impl_block: &ImplBlock) -> Result<(), TypecheckError> {
        // Implementation blocks are processed in register_implementations phase
        // For now, just record that we saw one
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
            // Only impl blocks create implementations
            _ => {
                // Other items don't affect the implementation registry
            }
        }
        
        Ok(())
    }
    
    /// Register an implementation block
    #[allow(clippy::result_large_err)]
    fn register_impl_block(&mut self, _impl_block: &ImplBlock) -> Result<(), TypecheckError> {
        // TODO: Extract implementing type and protocol
        // TODO: Register implementation with orphan rule checking
        // TODO: Register function implementations
        
        // For now, just record that we processed it
        Ok(())
    }
    
    /// Type check a single item
    #[allow(clippy::result_large_err)]
    pub fn typecheck_item(&mut self, item: &mut Item) -> Result<(), TypecheckError> {
        use outrun_parser::ItemKind;
        
        match &item.kind {
            ItemKind::StructDefinition(struct_def) => {
                self.typecheck_struct_functions(struct_def)
            }
            ItemKind::ProtocolDefinition(protocol_def) => {
                self.typecheck_protocol_functions(protocol_def)
            }
            ItemKind::ImplBlock(impl_block) => {
                self.typecheck_impl_block_functions(impl_block)
            }
            ItemKind::FunctionDefinition(func_def) => {
                self.typecheck_standalone_function(func_def)
            }
            ItemKind::LetBinding(let_binding) => {
                self.typecheck_let_binding(let_binding)
            }
            // Other items don't need body type checking
            ItemKind::ConstDefinition(_) |
            ItemKind::Expression(_) |
            ItemKind::Comment(_) |
            ItemKind::ImportDefinition(_) |
            ItemKind::AliasDefinition(_) |
            ItemKind::MacroDefinition(_) => {
                Ok(())
            }
            // All other item types (literals, keywords, etc.) don't need type checking
            _ => Ok(())
        }
    }
    
    /// Type check struct functions
    #[allow(clippy::result_large_err)]
    fn typecheck_struct_functions(&mut self, struct_def: &StructDefinition) -> Result<(), TypecheckError> {
        let struct_name = struct_def.name
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
    fn typecheck_protocol_functions(&mut self, protocol_def: &ProtocolDefinition) -> Result<(), TypecheckError> {
        let protocol_name = protocol_def.name
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
    fn typecheck_impl_block_functions(&mut self, impl_block: &ImplBlock) -> Result<(), TypecheckError> {
        let protocol_name = self.extract_type_spec_name(&impl_block.protocol_spec);
        let type_name = self.extract_type_spec_name(&impl_block.type_spec);
        let impl_scope = format!("{} for {}", protocol_name, type_name);
        
        for function in &impl_block.functions {
            self.typecheck_function_body(&impl_scope, function)?;
        }
        
        Ok(())
    }
    
    /// Type check standalone function
    #[allow(clippy::result_large_err)]
    fn typecheck_standalone_function(&mut self, func_def: &FunctionDefinition) -> Result<(), TypecheckError> {
        let module_scope = self.current_module.0.clone();
        self.typecheck_function_body(&module_scope, func_def)
    }
    
    /// Type check let binding
    #[allow(clippy::result_large_err)]
    fn typecheck_let_binding(&mut self, _let_binding: &outrun_parser::LetBinding) -> Result<(), TypecheckError> {
        // TODO: Implement let binding type checking
        Ok(())
    }
    
    /// Core function body type checking logic using RefCell for interior mutability
    #[allow(clippy::result_large_err)]
    fn typecheck_function_body(&mut self, scope: &str, function: &FunctionDefinition) -> Result<(), TypecheckError> {
        
        
        
        // Create a new inference context for this function
        let mut function_context = InferenceContext {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: None,
            self_binding: SelfBindingContext::ProtocolDefinition {
                protocol_id: ProtocolId::new("Unknown"),
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
            return Err(TypecheckError::UnificationError(OriginalUnificationError::TypeMismatch {
                expected: declared_return_type,
                found: body_type,
                expected_context: Some("declared return type".to_string()),
                found_context: Some(format!("Function {}.{} body", scope, function.name.name)),
                span: to_source_span(Some(function.body.span)),
            }));
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
    fn typecheck_static_function_body(&mut self, scope: &str, static_def: &StaticFunctionDefinition) -> Result<(), TypecheckError> {
        // Create a new inference context for this function
        let mut function_context = InferenceContext {
            substitution: Substitution::new(),
            constraints: Vec::new(),
            expected_type: None,
            self_binding: SelfBindingContext::ProtocolDefinition {
                protocol_id: ProtocolId::new("Unknown"),
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
            return Err(TypecheckError::UnificationError(OriginalUnificationError::TypeMismatch {
                expected: declared_return_type,
                found: body_type,
                expected_context: Some("declared return type".to_string()),
                found_context: Some(format!("Static function {}.{} body", scope, static_def.name.name)),
                span: to_source_span(Some(static_def.body.span)),
            }));
        }
        
        // Restore the previous symbol table
        self.symbol_table = old_symbol_table;
        
        Ok(())
    }
    
    /// Type check a block without requiring mutable access to expressions
    #[allow(clippy::result_large_err)]
    fn typecheck_block_readonly(&mut self, block: &outrun_parser::Block, context: &mut InferenceContext) -> Result<Type, TypecheckError> {
        use std::rc::Rc;
        use std::cell::RefCell;
        
        if block.statements.is_empty() {
            return Ok(Type::concrete("Unit"));
        }
        
        let mut last_type = Type::concrete("Unit");
        
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
                    last_type = result.inferred_type;
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    // Type check the let binding expression and add to symbol table
                    self.typecheck_let_binding_statement_readonly(let_binding, context)?;
                    last_type = Type::concrete("Unit"); // Let bindings return unit
                }
            }
        }
        
        // The type of a block is the type of its last statement
        Ok(last_type)
    }
    
    /// Type check a let binding statement without requiring mutability
    #[allow(clippy::result_large_err)]
    fn typecheck_let_binding_statement_readonly(&mut self, let_binding: &outrun_parser::LetBinding, context: &mut InferenceContext) -> Result<(), TypecheckError> {
        use std::rc::Rc;
        use std::cell::RefCell;
        
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
                self.symbol_table.insert(var_name, value_result.inferred_type);
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
    fn typecheck_guard_clause_readonly(&mut self, guard: &outrun_parser::GuardClause, context: &mut InferenceContext) -> Result<(), TypecheckError> {
        use std::rc::Rc;
        use std::cell::RefCell;
        
        // Create RefCell wrapper for the expression to enable mutability
        let expr_cell = Rc::new(RefCell::new(guard.condition.clone()));
        let guard_result = {
            let mut expr_mut = expr_cell.borrow_mut();
            self.infer_expression(&mut expr_mut, context)?
        };
        
        let boolean_type = Type::concrete("Outrun.Core.Boolean");
        
        if !self.types_are_compatible(&guard_result.inferred_type, &boolean_type) {
            return Err(TypecheckError::UnificationError(OriginalUnificationError::TypeMismatch {
                expected: boolean_type,
                found: guard_result.inferred_type,
                expected_context: Some("Guard clause must return Boolean".to_string()),
                found_context: Some("guard expression".to_string()),
                span: to_source_span(Some(guard.condition.span)),
            }));
        }
        
        Ok(())
    }
    
    /// Check if two types are compatible for assignment/unification
    /// This includes protocol compatibility and type variable resolution
    fn types_are_compatible(&self, found_type: &Type, expected_type: &Type) -> bool {
        use crate::types::Type;
        
        match (found_type, expected_type) {
            // Same concrete types are compatible
            (Type::Concrete { id: id1, args: args1, .. }, Type::Concrete { id: id2, args: args2, .. }) => {
                id1 == id2 && args1 == args2
            }
            
            // Protocol types: check if found type can implement expected protocol
            (Type::Concrete { id: concrete_type, .. }, Type::Protocol { id: protocol, .. }) => {
                // Check if the concrete type implements the protocol (with all requirements)
                self.protocol_registry.type_satisfies_protocol(concrete_type, protocol)
            }
            
            // Two protocol types: check if they're the same protocol
            (Type::Protocol { id: id1, args: args1, .. }, Type::Protocol { id: id2, args: args2, .. }) => {
                id1 == id2 && args1 == args2
            }
            
            // Type variables are compatible with anything (will be resolved during unification)
            (Type::Variable { .. }, _) | (_, Type::Variable { .. }) => true,
            
            // Self types need special handling based on context
            (Type::SelfType { .. }, _) | (_, Type::SelfType { .. }) => {
                // For now, allow Self type compatibility
                // TODO: Implement proper Self type resolution
                true
            }
            
            // Function types must match exactly
            (Type::Function { params: p1, return_type: r1, .. }, Type::Function { params: p2, return_type: r2, .. }) => {
                p1.len() == p2.len() && 
                p1.iter().zip(p2.iter()).all(|((n1, t1), (n2, t2))| n1 == n2 && self.types_are_compatible(t1, t2)) &&
                self.types_are_compatible(r1, r2)
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
        
        match &expression.kind {
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
            // TODO: Implement other expression types
            _ => {
                // For now, assign a fresh type variable to unknown expressions
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
            )
        ))
    }
    
    /// Infer the type of a function call with dispatch integration
    #[allow(clippy::result_large_err)]
    fn infer_function_call(
        &mut self,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        use outrun_parser::FunctionPath;
        
        match &function_call.path {
            FunctionPath::Simple { name } => {
                // Local function call - resolve in current context
                self.infer_local_function_call(name, &function_call.arguments, context)
            }
            FunctionPath::Qualified { module, name } => {
                // Qualified function call - static protocol or module call
                self.infer_qualified_function_call(module, name, &function_call.arguments, context)
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
        name: &outrun_parser::Identifier,
        arguments: &[outrun_parser::Argument],
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        let function_name = &name.name;
        
        // Create a function dispatcher with the appropriate context
        let function_context = self.create_function_context_from_inference_context(context);
        let dispatcher = FunctionDispatcher::new(&self.protocol_registry, &self.function_registry)
            .with_context(function_context);
        
        // Try to resolve local function call using dispatcher
        match dispatcher.resolve_local_call(function_name, Some(name.span)) {
            Ok(dispatch_result) => {
                self.handle_dispatch_result(dispatch_result, arguments, context)
            }
            Err(dispatch_error) => {
                // Convert dispatch error to type error
                Err(TypecheckError::DispatchError(dispatch_error))
            }
        }
    }
    
    /// Infer a qualified function call (Module.function)
    #[allow(clippy::result_large_err)]
    fn infer_qualified_function_call(
        &mut self,
        module: &outrun_parser::TypeIdentifier,
        name: &outrun_parser::Identifier,
        arguments: &[outrun_parser::Argument],
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        let qualified_name = format!("{}.{}", module.name, name.name);
        
        // Create a function dispatcher
        let dispatcher = FunctionDispatcher::new(&self.protocol_registry, &self.function_registry);
        
        // Try to resolve qualified function call using dispatcher
        match dispatcher.resolve_qualified_call(&qualified_name, None, Some(name.span)) {
            Ok(dispatch_result) => {
                self.handle_dispatch_result(dispatch_result, arguments, context)
            }
            Err(dispatch_error) => {
                // Convert dispatch error to type error
                Err(TypecheckError::DispatchError(dispatch_error))
            }
        }
    }
    
    /// Handle a dispatch result and infer the function call type
    #[allow(clippy::result_large_err)]
    fn handle_dispatch_result(
        &mut self,
        dispatch_result: crate::dispatch::DispatchResult,
        arguments: &[outrun_parser::Argument],
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
                            candidates.iter()
                                .map(|c| c.qualified_name.clone())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )],
                    }
                ))
            }
            DispatchResult::NotFound => {
                // Function not found
                Err(TypecheckError::InferenceError(
                    crate::error::InferenceError::AmbiguousType {
                        span: None,
                        suggestions: vec!["Function not found".to_string()],
                    }
                ))
            }
        }
    }
    
    /// Infer the result of a resolved function call
    #[allow(clippy::result_large_err)]
    fn infer_resolved_function_call(
        &mut self,
        resolved_function: &crate::dispatch::ResolvedFunction,
        arguments: &[outrun_parser::Argument],
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        // First, infer types of all arguments
        let mut inferred_arguments = Vec::new();
        let mut all_constraints = context.constraints.clone();
        let current_substitution = context.substitution.clone();
        
        for argument in arguments {
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
                }
            ));
        }
        
        // Create constraints for parameter type matching
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
        
        // For now, we'll return the function's return type
        // In a full implementation, we would need to handle generic instantiation
        let return_type = resolved_function.function_info.return_type.clone();
        
        Ok(InferenceResult {
            inferred_type: return_type,
            constraints: all_constraints,
            substitution: current_substitution,
        })
    }
    
    /// Infer the type of a function argument
    #[allow(clippy::result_large_err)]
    fn infer_argument(
        &mut self,
        argument: &outrun_parser::Argument,
        context: &mut InferenceContext,
    ) -> Result<InferenceResult, TypecheckError> {
        match argument {
            outrun_parser::Argument::Named { expression, .. } => {
                // Infer the type of the argument expression
                self.infer_expression(&mut expression.clone(), context)
            }
            outrun_parser::Argument::Spread { expression, .. } => {
                // Spread arguments are complex - for now, just infer the expression type
                self.infer_expression(&mut expression.clone(), context)
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
            SelfBindingContext::ProtocolDefinition { protocol_id, protocol_args } => {
                crate::dispatch::FunctionContext::Protocol { 
                    protocol_id: protocol_id.clone(), 
                    protocol_args: protocol_args.clone() 
                }
            }
            SelfBindingContext::Implementation { 
                implementing_type, 
                implementing_args, 
                protocol_id, 
                protocol_args 
            } => {
                crate::dispatch::FunctionContext::Implementation { 
                    implementing_type: implementing_type.clone(), 
                    implementing_args: implementing_args.clone(), 
                    protocol_id: protocol_id.clone(), 
                    protocol_args: protocol_args.clone() 
                }
            }
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
            SelfBindingContext::ProtocolDefinition { protocol_id, protocol_args } => {
                crate::dispatch::FunctionContext::Protocol { 
                    protocol_id: protocol_id.clone(), 
                    protocol_args: protocol_args.clone() 
                }
            }
            SelfBindingContext::Implementation { 
                implementing_type, 
                implementing_args, 
                protocol_id, 
                protocol_args 
            } => {
                crate::dispatch::FunctionContext::Implementation { 
                    implementing_type: implementing_type.clone(), 
                    implementing_args: implementing_args.clone(), 
                    protocol_id: protocol_id.clone(), 
                    protocol_args: protocol_args.clone() 
                }
            }
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
                    id: crate::types::TypeId::new("List"),
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
                        let element_result = self.infer_expression(&mut (**expr).clone(), context)?;
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
                    id: crate::types::TypeId::new("List"),
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
                id: crate::types::TypeId::new("List"),
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
                id: crate::types::TypeId::new("Tuple"),
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
            id: crate::types::TypeId::new("Tuple"),
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
                    id: crate::types::TypeId::new("Map"),
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
                        let value_result = self.infer_expression(&mut (**value).clone(), context)?;
                        value_types.push(value_result.inferred_type);
                        all_constraints.extend(value_result.constraints);
                    }
                    MapEntry::Shorthand { name: _, value } => {
                        // Shorthand syntax: {name} where name is both key and value
                        // Key is atom (symbol), value is expression
                        key_types.push(Type::concrete("Outrun.Core.Atom"));
                        
                        let value_result = self.infer_expression(&mut (**value).clone(), context)?;
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
                    id: crate::types::TypeId::new("Map"),
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
                id: crate::types::TypeId::new("Map"),
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
                    suggestions: vec!["Anonymous function must have at least one clause".to_string()],
                }
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
                clause_index + 1
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
            let guard_result = self.infer_expression(&mut guard_expr.clone(), &mut local_context)?;
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
                        expected_param_types.iter()
                            .map(|(name, ty)| format!("{}: {}", name, ty))
                            .collect::<Vec<_>>()
                            .join(", "),
                        expected_return_type
                    )),
                    actual_arguments: Some(format!(
                        "({})",
                        clause_param_types.iter()
                            .map(|(name, ty)| format!("{}: {}", name, ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                    span: crate::error::to_source_span(Some(clause.span)),
                    suggestions: vec![
                        "All function clauses must have the same parameter signature".to_string(),
                        "Consider adding or removing parameters to match the first clause".to_string(),
                    ],
                }
            ));
        }
        
        // Create constraints for parameter type consistency
        let mut constraints = context.constraints.clone();
        for ((expected_name, expected_type), (clause_name, clause_type)) in 
            expected_param_types.iter().zip(clause_param_types.iter()) {
            
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
            let guard_result = self.infer_expression(&mut guard_expr.clone(), &mut local_context)?;
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
        // Protocol names are typically simple identifiers without dots
        // Concrete types usually have namespace separators
        // Examples:
        // - "Integer" -> protocol
        // - "String" -> protocol  
        // - "Outrun.Core.Integer64" -> concrete type
        // - "Display" -> protocol
        
        // For now, use a simple heuristic: if it doesn't contain "." and is a known protocol, treat as protocol
        if type_name.contains('.') {
            false // Namespaced types are concrete
        } else {
            // Check if this is a known protocol name
            self.is_known_protocol(type_name)
        }
    }
    
    /// Check if a type name is a known protocol
    fn is_known_protocol(&self, type_name: &str) -> bool {
        // Common Outrun protocols - we can expand this list or make it configurable
        matches!(type_name, 
            "Integer" | "Float" | "String" | "Boolean" | 
            "Display" | "Debug" | "Clone" | "Equality" |
            "BinaryAddition" | "BinarySubtraction" | "BinaryMultiplication" | "BinaryDivision" |
            "UnaryMinus" | "UnaryPlus" | "LogicalNot" | "LogicalAnd" | "LogicalOr" |
            "Comparison" | "Iterator" | "Collection" | "Indexable" |
            "Maybe" | "Result" | "Option"
        )
    }
    
    /// Convert a parser type annotation to a typechecker type
    #[allow(clippy::result_large_err, clippy::only_used_in_recursion)]
    fn convert_type_annotation(&mut self, type_annotation: &TypeAnnotation) -> Result<Type, TypecheckError> {
        match type_annotation {
            TypeAnnotation::Simple { path, .. } => {
                let type_name = path.iter().map(|id| id.name.clone()).collect::<Vec<_>>().join("::");
                
                // Determine if this is a protocol or concrete type
                // Protocol names are typically simple identifiers like "Integer", "Display"
                // Concrete types are typically namespaced like "Outrun.Core.Integer64"
                if self.is_protocol_type(&type_name) {
                    Ok(Type::Protocol {
                        id: ProtocolId::new(type_name),
                        args: vec![],
                        span: None,
                    })
                } else {
                    Ok(Type::concrete(&type_name))
                }
            }
            TypeAnnotation::Tuple { .. } => {
                // TODO: Implement tuple types in the typechecker
                // For now, treat as a placeholder concrete type
                Ok(Type::concrete("Tuple"))
            }
            TypeAnnotation::Function { params, return_type, .. } => {
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
        
        let function_call = FunctionCall {
            path: FunctionPath::Simple {
                name: Identifier {
                    name: "test_function".to_string(),
                    span: Span::new(0, 13),
                },
            },
            arguments: vec![], // No arguments for simplicity
            span: Span::new(0, 15),
        };
        
        // Test function call inference (should handle dispatch errors gracefully)
        let result = engine.infer_function_call(&function_call, &mut context);
        
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
        use outrun_parser::{ListLiteral, ListElement, Expression, ExpressionKind, IntegerLiteral, IntegerFormat, Span};
        
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
        
        let result = engine.infer_list_literal(&list_literal, &mut context).unwrap();
        
        // Should infer List<Outrun.Core.Integer64>
        match &result.inferred_type {
            Type::Concrete { id, args, .. } => {
                assert_eq!(id.0, "List");
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
        
        let result = engine.infer_list_literal(&list_literal, &mut context).unwrap();
        
        // Should infer List<T> where T is a type variable
        match &result.inferred_type {
            Type::Concrete { id, args, .. } => {
                assert_eq!(id.0, "List");
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
        use outrun_parser::{TupleLiteral, Expression, ExpressionKind, IntegerLiteral, StringLiteral, BooleanLiteral, IntegerFormat, StringFormat, Span};
        
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
                        parts: vec![outrun_parser::StringPart::Text { content: "hello".to_string(), raw_content: "hello".to_string() }],
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
        
        let result = engine.infer_tuple_literal(&tuple_literal, &mut context).unwrap();
        
        // Should infer Tuple<Outrun.Core.Integer64, Outrun.Core.String, Outrun.Core.Boolean>
        match &result.inferred_type {
            Type::Concrete { id, args, .. } => {
                assert_eq!(id.0, "Tuple");
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
        use outrun_parser::{MapLiteral, MapEntry, Expression, ExpressionKind, StringLiteral, IntegerLiteral, StringFormat, IntegerFormat, Span, StringPart};
        
        let map_literal = MapLiteral {
            entries: vec![
                MapEntry::Assignment {
                    key: Box::new(Expression {
                        kind: ExpressionKind::String(StringLiteral {
                            parts: vec![StringPart::Text { content: "key1".to_string(), raw_content: "key1".to_string() }],
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
                            parts: vec![StringPart::Text { content: "key2".to_string(), raw_content: "key2".to_string() }],
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
        
        let result = engine.infer_map_literal(&map_literal, &mut context).unwrap();
        
        // Should infer Map<Outrun.Core.String, Outrun.Core.Integer64>
        match &result.inferred_type {
            Type::Concrete { id, args, .. } => {
                assert_eq!(id.0, "Map");
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