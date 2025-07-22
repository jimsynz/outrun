//! Function call context for consolidating function dispatch parameters
//!
//! This module provides a context object that bundles together the many parameters
//! that are passed to function dispatch methods, making the code cleaner and more maintainable.

use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::checker::{DispatchMethod, TypedExpression, TypedFunctionPath};
use std::collections::HashMap;

/// Context for function call execution that consolidates the many parameters
/// passed to FunctionDispatcher methods
#[derive(Debug)]
pub struct FunctionCallContext<'a> {
    /// Function path specifying which function to call
    pub function_path: &'a TypedFunctionPath,

    /// Evaluated arguments mapped by parameter name
    pub arguments: HashMap<String, Value>,

    /// Dispatch strategy determined by typechecker
    pub dispatch_strategy: &'a DispatchMethod,

    /// Complete typed expression for context and span information
    pub typed_expr: &'a TypedExpression,

    /// Source span for error reporting
    pub span: Span,
}

impl<'a> FunctionCallContext<'a> {
    /// Create a new function call context with the provided components
    pub fn new(
        function_path: &'a TypedFunctionPath,
        arguments: HashMap<String, Value>,
        dispatch_strategy: &'a DispatchMethod,
        typed_expr: &'a TypedExpression,
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
            TypedFunctionPath::Simple { name } => name.clone(),
            TypedFunctionPath::Qualified { module: _, name } => name.clone(),
            TypedFunctionPath::Expression { .. } => "<expression>".to_string(),
        }
    }

    /// Get the module name if this is a qualified function call
    pub fn module_name(&self) -> Option<&str> {
        match self.function_path {
            TypedFunctionPath::Qualified { module, name: _ } => Some(module.as_str()),
            _ => None,
        }
    }

    /// Check if this is a static function call
    pub fn is_static_call(&self) -> bool {
        matches!(self.dispatch_strategy, DispatchMethod::Static { .. })
    }

    /// Check if this is a protocol method call
    pub fn is_protocol_call(&self) -> bool {
        matches!(self.dispatch_strategy, DispatchMethod::Protocol { .. })
    }

    /// Get the function ID for static calls
    pub fn static_function_id(&self) -> Option<&str> {
        match self.dispatch_strategy {
            DispatchMethod::Static { function_id } => Some(function_id.as_str()),
            _ => None,
        }
    }

    /// Get protocol information for protocol calls
    pub fn protocol_info(
        &self,
    ) -> Option<(&str, &str, &outrun_typechecker::unification::StructuredType)> {
        match self.dispatch_strategy {
            DispatchMethod::Protocol {
                protocol_name,
                function_name,
                impl_type,
            } => Some((
                protocol_name.as_str(),
                function_name.as_str(),
                impl_type.as_ref(),
            )),
            _ => None,
        }
    }
}

/// Context for intrinsic function execution that consolidates the common parameters
/// passed to intrinsic methods
#[derive(Debug)]
pub struct IntrinsicExecutionContext<'a> {
    /// Function name being executed
    pub function_name: &'a str,

    /// Evaluated arguments mapped by parameter name
    pub arguments: &'a HashMap<String, Value>,

    /// Optional typed expression for additional type information
    pub typed_expr: Option<&'a TypedExpression>,

    /// Source span for error reporting
    pub span: Span,
}

impl<'a> IntrinsicExecutionContext<'a> {
    /// Create a new intrinsic execution context
    pub fn new(
        function_name: &'a str,
        arguments: &'a HashMap<String, Value>,
        typed_expr: Option<&'a TypedExpression>,
        span: Span,
    ) -> Self {
        Self {
            function_name,
            arguments,
            typed_expr,
            span,
        }
    }

    /// Create an intrinsic execution context from a function call context
    ///
    /// This is useful when an intrinsic is being called as part of function dispatch
    pub fn from_function_call_context(
        function_name: &'a str,
        call_context: &'a FunctionCallContext,
    ) -> Self {
        Self {
            function_name,
            arguments: &call_context.arguments,
            typed_expr: Some(call_context.typed_expr),
            span: call_context.span,
        }
    }

    /// Get the base function name without the Outrun.Intrinsic prefix
    pub fn base_function_name(&self) -> &str {
        if let Some(name) = self.function_name.strip_prefix("Outrun.Intrinsic.") {
            name
        } else {
            self.function_name
        }
    }

    /// Check if this context has type information available
    pub fn has_type_info(&self) -> bool {
        self.typed_expr.is_some()
    }

    /// Get the expected return type if available
    pub fn return_type(&self) -> Option<&outrun_typechecker::unification::StructuredType> {
        self.typed_expr
            .and_then(|expr| expr.structured_type.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_typechecker::checker::TypedExpressionKind;
    use outrun_typechecker::unification::StructuredType;

    fn create_test_typed_expression() -> TypedExpression {
        let compiler_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        let type_id = compiler_env.intern_type_name("TestType");

        TypedExpression {
            kind: TypedExpressionKind::Integer(42),
            structured_type: Some(StructuredType::Simple(type_id)),
            span: Span::new(0, 10),
            debug_info: None,
        }
    }

    #[test]
    fn test_function_call_context_creation() {
        let function_path = TypedFunctionPath::Simple {
            name: "test_function".to_string(),
        };
        let arguments = HashMap::new();
        let dispatch_strategy = DispatchMethod::Static {
            function_id: "test::test_function".to_string(),
        };
        let typed_expr = create_test_typed_expression();

        let context =
            FunctionCallContext::new(&function_path, arguments, &dispatch_strategy, &typed_expr);

        assert_eq!(context.function_name(), "test_function");
        assert!(context.is_static_call());
        assert!(!context.is_protocol_call());
        assert_eq!(context.static_function_id(), Some("test::test_function"));
    }

    #[test]
    fn test_qualified_function_call() {
        let function_path = TypedFunctionPath::Qualified {
            module: "Option".to_string(),
            name: "some".to_string(),
        };
        let arguments = HashMap::new();
        let dispatch_strategy = DispatchMethod::Static {
            function_id: "Option::some".to_string(),
        };
        let typed_expr = create_test_typed_expression();

        let context =
            FunctionCallContext::new(&function_path, arguments, &dispatch_strategy, &typed_expr);

        assert_eq!(context.function_name(), "some");
        assert_eq!(context.module_name(), Some("Option"));
        assert!(context.is_static_call());
    }

    #[test]
    fn test_protocol_method_call() {
        let function_path = TypedFunctionPath::Simple {
            name: "head".to_string(),
        };
        let arguments = HashMap::new();
        let compiler_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        let list_type_id = compiler_env.intern_type_name("List");
        let impl_type = StructuredType::Simple(list_type_id);
        let dispatch_strategy = DispatchMethod::Protocol {
            protocol_name: "List".to_string(),
            function_name: "head".to_string(),
            impl_type: Box::new(impl_type.clone()),
        };
        let typed_expr = create_test_typed_expression();

        let context =
            FunctionCallContext::new(&function_path, arguments, &dispatch_strategy, &typed_expr);

        assert_eq!(context.function_name(), "head");
        assert!(!context.is_static_call());
        assert!(context.is_protocol_call());

        let (protocol_name, function_name, _impl_type) = context.protocol_info().unwrap();
        assert_eq!(protocol_name, "List");
        assert_eq!(function_name, "head");
    }
}
