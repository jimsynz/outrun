//! Type inference engine orchestration layer
//!
//! This module provides the main orchestration layer that coordinates all typechecker components
//! to perform Hindley-Milner type inference with protocol constraints and dispatch resolution.

use crate::{
    dispatch::{FunctionDispatcher, FunctionRegistry, FunctionInfo, FunctionVisibility as DispatchVisibility},
    error::TypecheckError,
    registry::ProtocolRegistry,
    types::{Type, TypeVarId, Substitution, Constraint, SelfBindingContext, ModuleId, Level, ProtocolId},
};
use outrun_parser::{
    Item, Program, Expression, StructDefinition, ProtocolDefinition,
    FunctionDefinition, ConstDefinition, ImplBlock, TypeAnnotation
};
use std::collections::HashMap;
use std::rc::Rc;

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
    symbol_table: HashMap<String, Type>,
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
                // Implementation blocks will be handled in register_implementations
                // For now, just record that we saw it
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
    fn collect_struct_definition(&mut self, _struct_def: &StructDefinition) -> Result<(), TypecheckError> {
        // TODO: Extract struct name and fields
        // TODO: Add to type registry 
        // TODO: Handle generic parameters
        
        // For now, just record that we processed it
        Ok(())
    }
    
    /// Collect a protocol definition
    #[allow(clippy::result_large_err)]
    fn collect_protocol_definition(&mut self, _protocol_def: &ProtocolDefinition) -> Result<(), TypecheckError> {
        // TODO: Extract protocol name and methods
        // TODO: Register protocol in protocol registry
        // TODO: Handle generic parameters and constraints
        
        // For now, just record that we processed it
        Ok(())
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
    pub fn typecheck_item(&mut self, _item: &mut Item) -> Result<(), TypecheckError> {
        // TODO: Type check different kinds of items:
        // - Function definitions
        // - Let bindings
        // - Protocol definitions
        // - Implementation blocks
        
        // This will be implemented in Phase 4+
        Ok(())
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
        
        // Variable not found - this is a type error
        Err(TypecheckError::InferenceError(crate::error::InferenceError::UndefinedVariable {
            variable_name: var_name.clone(),
            span: Some(crate::error::to_source_span(Some(identifier.span))).flatten(),
        }))
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
    
    /// Convert a parser type annotation to a typechecker type
    #[allow(clippy::result_large_err)]
    fn convert_type_annotation(&mut self, type_annotation: &TypeAnnotation) -> Result<Type, TypecheckError> {
        match type_annotation {
            TypeAnnotation::Simple { path, .. } => {
                // For now, treat all type annotations as concrete types
                // TODO: Handle generics, protocols, function types, etc.
                let type_name = path.iter().map(|id| id.name.clone()).collect::<Vec<_>>().join("::");
                Ok(Type::concrete(&type_name))
            }
            TypeAnnotation::Tuple { .. } => {
                // TODO: Implement tuple types in the typechecker
                // For now, treat as a placeholder concrete type
                Ok(Type::concrete("Tuple"))
            }
            TypeAnnotation::Function { .. } => {
                // TODO: Implement function types in the typechecker
                // For now, treat as a placeholder concrete type
                Ok(Type::concrete("Function"))
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
}