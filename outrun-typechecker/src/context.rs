//! Unified context objects for consolidated parameter passing
//!
//! This module provides context objects that bundle together related parameters
//! that are frequently passed together throughout the type checking pipeline.
//! This reduces parameter list complexity and ensures consistency.

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::compiler_environment::TypeNameId;
use crate::unification::UnificationContext;
use outrun_parser::{ProtocolDefinition, StructDefinition};
use std::collections::HashMap;

// Import types needed for FunctionCallContext (using placeholder types for now)
// These will be replaced with proper types once the outrun-interpreter Value type is accessible
type Value = String; // Placeholder - will be replaced with actual Value type when refactoring interpreter

/// Context for type checking operations that bundles commonly-used parameters
///
/// This context object consolidates the parameters that are most frequently
/// passed together during type checking, AST building, and compilation phases.
#[derive(Debug, Clone)]
pub struct TypeCheckingContext {
    /// Unification context for type resolution and compatibility checking
    pub unification_context: UnificationContext,

    /// Compiler environment for function lookup system
    pub compiler_environment: Option<CompilerEnvironment>,

    /// Struct definitions indexed by TypeNameId for type checking
    pub structs: HashMap<TypeNameId, StructDefinition>,

    /// Protocol definitions indexed by TypeNameId for protocol resolution
    pub protocols: HashMap<TypeNameId, ProtocolDefinition>,
}

impl TypeCheckingContext {
    /// Create a new type checking context with the provided components
    pub fn new(
        unification_context: UnificationContext,
        structs: HashMap<TypeNameId, StructDefinition>,
        protocols: HashMap<TypeNameId, ProtocolDefinition>,
    ) -> Self {
        Self {
            unification_context,
            compiler_environment: None,
            structs,
            protocols,
        }
    }

    /// Create a new type checking context with CompilerEnvironment
    pub fn with_compiler_environment(
        unification_context: UnificationContext,
        compiler_environment: CompilerEnvironment,
        structs: HashMap<TypeNameId, StructDefinition>,
        protocols: HashMap<TypeNameId, ProtocolDefinition>,
    ) -> Self {
        Self {
            unification_context,
            compiler_environment: Some(compiler_environment),
            structs,
            protocols,
        }
    }

    /// Look up a qualified function using the CompilerEnvironment system
    pub fn lookup_qualified_function(
        &self,
        module_type: &crate::unification::StructuredType,
        function_name: crate::compilation::compiler_environment::AtomId,
    ) -> Option<crate::compilation::UnifiedFunctionEntry> {
        if let Some(compiler_env) = &self.compiler_environment {
            compiler_env.lookup_qualified_function(module_type, function_name)
        } else {
            None
        }
    }

    /// Look up a protocol implementation function
    pub fn lookup_impl_function(
        &self,
        protocol_type: &crate::unification::StructuredType,
        impl_type: &crate::unification::StructuredType,
        function_name: crate::compilation::compiler_environment::AtomId,
    ) -> Option<crate::compilation::UnifiedFunctionEntry> {
        if let Some(compiler_env) = &self.compiler_environment {
            compiler_env.lookup_impl_function(protocol_type, impl_type, function_name)
        } else {
            None
        }
    }

    /// Look up a local function
    pub fn lookup_local_function(
        &self,
        function_name: crate::compilation::compiler_environment::AtomId,
    ) -> Option<crate::compilation::UnifiedFunctionEntry> {
        if let Some(compiler_env) = &self.compiler_environment {
            compiler_env.lookup_local_function(function_name)
        } else {
            None
        }
    }

    /// Create a type checking context from a compilation result
    ///
    /// This is a convenience method for creating a context from an existing
    /// compilation result, which is common during multi-phase compilation.
    pub fn from_compilation_result(
        compilation_result: &crate::compilation::program_collection::CompilationResult,
    ) -> Self {
        Self {
            unification_context: compilation_result.type_context.clone(),
            compiler_environment: None, // No longer stored in UnificationContext
            structs: compilation_result.structs.clone(),
            protocols: compilation_result.protocols.clone(),
        }
    }

    /// Get a reference to the compiler environment for type operations
    pub fn compiler_environment(
        &self,
    ) -> &Option<crate::compilation::compiler_environment::CompilerEnvironment> {
        &self.compiler_environment
    }

    /// Get a mutable reference to the unification context
    pub fn unification_context_mut(&mut self) -> &mut UnificationContext {
        &mut self.unification_context
    }

    /// Add a struct definition to the context
    pub fn add_struct(&mut self, type_id: TypeNameId, struct_def: StructDefinition) {
        self.structs.insert(type_id, struct_def);
    }

    /// Add a protocol definition to the context
    pub fn add_protocol(&mut self, type_id: TypeNameId, protocol_def: ProtocolDefinition) {
        self.protocols.insert(type_id, protocol_def);
    }

    /// Look up a struct definition by TypeNameId
    pub fn get_struct(&self, type_id: &TypeNameId) -> Option<&StructDefinition> {
        self.structs.get(type_id)
    }

    /// Look up a protocol definition by TypeNameId
    pub fn get_protocol(&self, type_id: &TypeNameId) -> Option<&ProtocolDefinition> {
        self.protocols.get(type_id)
    }

    /// Create a minimal context for testing
    #[cfg(test)]
    pub fn minimal_for_testing() -> Self {
        let compiler_env = crate::compilation::compiler_environment::CompilerEnvironment::new();
        Self {
            unification_context: UnificationContext::new(),
            compiler_environment: Some(compiler_env),
            structs: HashMap::new(),
            protocols: HashMap::new(),
        }
    }
}

/// Context for function dispatch operations
///
/// This context bundles the parameters commonly used for function dispatch,
/// evaluation, and runtime function resolution.
#[derive(Debug, Clone)]
pub struct FunctionDispatchContext {
    /// Compiler environment for type and atom resolution
    pub compiler_environment: Option<crate::compilation::compiler_environment::CompilerEnvironment>,
}

/// Context for function call execution that consolidates the many parameters
/// passed to FunctionDispatcher methods
#[derive(Debug)]
pub struct FunctionCallContext<'a> {
    /// Function path specifying which function to call
    pub function_path: &'a crate::checker::TypedFunctionPath,

    /// Evaluated arguments mapped by parameter name
    pub arguments: HashMap<String, Value>,

    /// Dispatch strategy determined by typechecker
    pub dispatch_strategy: &'a crate::checker::DispatchMethod,

    /// Complete typed expression for context and span information
    pub typed_expr: &'a crate::checker::TypedExpression,

    /// Source span for error reporting
    pub span: outrun_parser::Span,
}

// Note: InterpreterContext and ExpressionEvaluator are not included in this context
// as they live in the outrun-interpreter crate and would create circular dependencies.
// They are passed separately to the dispatch methods that use FunctionCallContext.

impl<'a> FunctionCallContext<'a> {
    /// Create a new function call context with the provided components
    pub fn new(
        function_path: &'a crate::checker::TypedFunctionPath,
        arguments: HashMap<String, Value>,
        dispatch_strategy: &'a crate::checker::DispatchMethod,
        typed_expr: &'a crate::checker::TypedExpression,
    ) -> Self {
        Self {
            function_path,
            arguments,
            dispatch_strategy,
            typed_expr,
            span: typed_expr.span,
        }
    }

    /// Get the function name from the function path
    pub fn function_name(&self) -> String {
        match self.function_path {
            crate::checker::TypedFunctionPath::Simple { name } => name.clone(),
            crate::checker::TypedFunctionPath::Qualified { module: _, name } => name.clone(),
            crate::checker::TypedFunctionPath::Expression { .. } => "<expression>".to_string(),
        }
    }

    /// Get the module name if this is a qualified function call
    pub fn module_name(&self) -> Option<&str> {
        match self.function_path {
            crate::checker::TypedFunctionPath::Qualified { module, name: _ } => {
                Some(module.as_str())
            }
            _ => None,
        }
    }

    /// Check if this is a static function call
    pub fn is_static_call(&self) -> bool {
        matches!(
            self.dispatch_strategy,
            crate::checker::DispatchMethod::Static { .. }
        )
    }

    /// Check if this is a protocol method call
    pub fn is_protocol_call(&self) -> bool {
        matches!(
            self.dispatch_strategy,
            crate::checker::DispatchMethod::Protocol { .. }
        )
    }
}

impl FunctionDispatchContext {
    /// Create a new function dispatch context
    pub fn new(
        compiler_environment: Option<crate::compilation::compiler_environment::CompilerEnvironment>,
    ) -> Self {
        Self {
            compiler_environment,
        }
    }

    /// Create a function dispatch context from a type checking context
    ///
    /// This extracts the relevant components for function dispatch from
    /// a larger type checking context.
    pub fn from_type_checking_context(tc_context: &TypeCheckingContext) -> Self {
        Self {
            compiler_environment: tc_context.compiler_environment.clone(),
        }
    }
}

/// Context for compilation phase operations
///
/// This context bundles parameters commonly used during multi-program compilation
/// phases, including dependency ordering, external variable tracking, and type registries.
#[derive(Debug, Clone)]
pub struct CompilationPhaseContext {
    /// Base type checking context
    pub type_checking_context: TypeCheckingContext,

    /// Compilation order for dependency resolution
    pub compilation_order: Vec<String>,

    /// External variables available in this compilation phase
    pub external_variables: HashMap<String, crate::unification::StructuredType>,

    /// Implementation blocks extracted during compilation
    pub implementations: Vec<outrun_parser::ImplBlock>,
}

impl CompilationPhaseContext {
    /// Create a new compilation phase context
    pub fn new(
        type_checking_context: TypeCheckingContext,
        compilation_order: Vec<String>,
        external_variables: HashMap<String, crate::unification::StructuredType>,
        implementations: Vec<outrun_parser::ImplBlock>,
    ) -> Self {
        Self {
            type_checking_context,
            compilation_order,
            external_variables,
            implementations,
        }
    }

    /// Create a compilation phase context from a compilation result
    pub fn from_compilation_result(
        compilation_result: &crate::compilation::program_collection::CompilationResult,
        compilation_order: Vec<String>,
        external_variables: HashMap<String, crate::unification::StructuredType>,
    ) -> Self {
        Self {
            type_checking_context: TypeCheckingContext::from_compilation_result(compilation_result),
            compilation_order,
            external_variables,
            implementations: compilation_result.implementations.clone(),
        }
    }

    /// Create a compilation phase context for type checking with all required components
    pub fn for_type_checking(
        unification_context: crate::unification::UnificationContext,
        structs: HashMap<TypeNameId, StructDefinition>,
        protocols: HashMap<TypeNameId, ProtocolDefinition>,
        implementations: Vec<outrun_parser::ImplBlock>,
        compilation_order: Vec<String>,
        external_variables: HashMap<String, crate::unification::StructuredType>,
    ) -> Self {
        let type_checking_context =
            TypeCheckingContext::new(unification_context, structs, protocols);

        Self {
            type_checking_context,
            compilation_order,
            external_variables,
            implementations,
        }
    }

    /// Get a reference to the underlying type checking context
    pub fn type_checking_context(&self) -> &TypeCheckingContext {
        &self.type_checking_context
    }

    /// Get a mutable reference to the underlying type checking context
    pub fn type_checking_context_mut(&mut self) -> &mut TypeCheckingContext {
        &mut self.type_checking_context
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_checking_context_creation() {
        let context = TypeCheckingContext::minimal_for_testing();

        // Verify context is properly initialized
        assert!(context.structs.is_empty());
        assert!(context.protocols.is_empty());
        // CompilerEnvironment starts empty by default
    }

    #[test]
    fn test_function_dispatch_context_from_type_checking() {
        let tc_context = TypeCheckingContext::minimal_for_testing();
        let dispatch_context = FunctionDispatchContext::from_type_checking_context(&tc_context);

        // Verify dispatch context extracts the right components
        assert!(dispatch_context.compiler_environment.is_some());
    }

    #[test]
    fn test_struct_and_protocol_operations() {
        let mut context = TypeCheckingContext::minimal_for_testing();

        // Add a mock struct definition
        let type_id = context
            .compiler_environment()
            .as_ref()
            .unwrap()
            .intern_type_name("TestStruct");
        let struct_def = StructDefinition {
            attributes: vec![],
            name: vec![outrun_parser::TypeIdentifier {
                name: "TestStruct".to_string(),
                span: outrun_parser::Span {
                    start: 0,
                    end: 0,
                    start_line_col: None,
                    end_line_col: None,
                },
            }],
            generic_params: None,
            fields: vec![],
            methods: vec![],
            span: outrun_parser::Span {
                start: 0,
                end: 0,
                start_line_col: None,
                end_line_col: None,
            },
        };
        context.add_struct(type_id.clone(), struct_def);

        // Verify we can retrieve it
        assert!(context.get_struct(&type_id).is_some());
        assert_eq!(
            context.get_struct(&type_id).unwrap().name[0].name,
            "TestStruct"
        );
    }

    #[test]
    fn test_compilation_phase_context() {
        let tc_context = TypeCheckingContext::minimal_for_testing();
        let phase_context = CompilationPhaseContext::new(
            tc_context,
            vec!["program1".to_string(), "program2".to_string()],
            HashMap::new(),
            vec![], // Empty implementations for test
        );

        // Verify compilation phase context bundles everything correctly
        assert_eq!(phase_context.compilation_order.len(), 2);
        assert!(phase_context.external_variables.is_empty());
        assert!(phase_context.implementations.is_empty());
        assert!(phase_context.type_checking_context().structs.is_empty());
    }
}
