//! Runtime context for the Outrun interpreter
//!
//! This module provides the core runtime context that manages interpreter state,
//! integrates with the typechecker, and handles variable bindings and function calls.

use crate::call_stack::{CallStack, CallStackError};
use crate::types::TypeIntegration;
use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::unification::{StructuredType, UnificationContext};
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur during runtime context operations
#[derive(Debug, Error)]
pub enum ContextError {
    #[error("Variable '{name}' not found in current scope")]
    VariableNotFound { name: String },

    #[error("Cannot access variable '{name}' from outer scope")]
    VariableNotAccessible { name: String },

    #[error("Variable '{name}' already defined in current scope")]
    VariableAlreadyDefined { name: String },

    #[error("Call stack error: {source}")]
    CallStack {
        #[from]
        source: CallStackError,
    },

    #[error("Type integration error: {source}")]
    TypeIntegration {
        #[from]
        source: crate::types::TypeIntegrationError,
    },
}

/// The main runtime context for the Outrun interpreter
///
/// This context integrates with the typechecker's `UnificationContext` and `CompilerEnvironment`
/// while adding runtime-specific state management for variable bindings and function calls.
pub struct InterpreterContext {
    /// Integration with the typechecker's unification context for type information
    type_context: UnificationContext,

    /// Variable environment for runtime bindings (lexical scoping)
    variable_environment: VariableEnvironment,

    /// Call stack for function calls and error reporting
    call_stack: CallStack,

    /// Global constants and module-level bindings
    global_environment: HashMap<String, Value>,

    /// Type integration utilities for Value ↔ StructuredType conversion
    type_integration: TypeIntegration,
}

impl InterpreterContext {
    /// Create a new interpreter context from a typed program
    ///
    /// This takes ownership of the typechecker's context,
    /// creating a runtime environment ready for expression evaluation.
    pub fn new(
        type_context: UnificationContext,
        compiler_environment: outrun_typechecker::compilation::compiler_environment::CompilerEnvironment,
        max_stack_depth: Option<usize>,
    ) -> Self {
        // NOTE: TypeIntegration now takes CompilerEnvironment as a separate parameter
        // This ensures type IDs are consistent across all components
        let type_integration = TypeIntegration::new(compiler_environment);

        Self {
            type_context,
            variable_environment: VariableEnvironment::new(),
            call_stack: CallStack::with_max_depth(max_stack_depth.unwrap_or(1000)),
            global_environment: HashMap::new(),
            type_integration,
        }
    }

    /// Create a new context for REPL usage with a fresh environment
    pub fn new_repl() -> Self {
        let compiler_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        Self::new(
            UnificationContext::new(),
            compiler_env,
            Some(100), // Smaller stack for REPL
        )
    }

    // Variable Environment Management

    /// Define a variable in the current scope
    pub fn define_variable(&mut self, name: String, value: Value) -> Result<(), ContextError> {
        self.variable_environment.define(name, value)
    }

    /// Look up a variable value by name (searches up the scope chain)
    pub fn lookup_variable(&self, name: &str) -> Result<&Value, ContextError> {
        self.variable_environment.lookup(name)
    }

    /// Update an existing variable (searches up the scope chain)
    pub fn update_variable(&mut self, name: &str, value: Value) -> Result<(), ContextError> {
        self.variable_environment.update(name, value)
    }

    /// Push a new lexical scope (for blocks, functions, etc.)
    pub fn push_scope(&mut self, scope_type: ScopeType) {
        self.variable_environment.push_scope(scope_type);
    }

    /// Pop the current lexical scope
    pub fn pop_scope(&mut self) -> Result<(), ContextError> {
        self.variable_environment.pop_scope()
    }

    // Call Stack Management

    /// Push a new call frame onto the stack
    pub fn push_call_frame(
        &mut self,
        frame: crate::call_stack::CallFrame,
    ) -> Result<(), ContextError> {
        Ok(self.call_stack.push_frame(frame)?)
    }

    /// Pop the current call frame from the stack
    pub fn pop_call_frame(
        &mut self,
        span: Span,
    ) -> Result<crate::call_stack::CallFrame, ContextError> {
        Ok(self.call_stack.pop_frame(span)?)
    }

    /// Get the current call stack depth
    pub fn call_stack_depth(&self) -> usize {
        self.call_stack.depth()
    }

    /// Get a reference to the current call frame (top of stack)
    pub fn current_call_frame(&self) -> Option<&crate::call_stack::CallFrame> {
        self.call_stack.current_frame()
    }

    /// Get the current Self type context from the call stack
    pub fn current_self_type(&self) -> Option<&outrun_typechecker::unification::StructuredType> {
        self.call_stack.current_self_type()
    }

    /// Clear the call stack (for test isolation and REPL reset)
    pub fn clear_call_stack(&mut self) {
        self.call_stack.clear();
    }

    // Type System Integration

    /// Get the StructuredType for a runtime Value
    pub fn value_to_type(&mut self, value: &Value) -> StructuredType {
        self.type_integration.value_to_structured_type(value)
    }

    /// Validate that a Value matches an expected StructuredType
    pub fn validate_value_type(&mut self, value: &Value, expected_type: &StructuredType) -> bool {
        self.type_integration
            .validate_value_type(value, expected_type)
    }

    /// Get the resolved type for an expression by its span
    pub fn get_expression_type(&self, span: &Span) -> Option<&StructuredType> {
        self.type_context.expression_types.get(span)
    }

    // Function operations are now handled through CompilerEnvironment in UnificationContext

    /// Get a reference to the type context for advanced type operations
    pub fn type_context(&self) -> &UnificationContext {
        &self.type_context
    }

    // Global Environment Management

    /// Define a global constant or module-level binding
    pub fn define_global(&mut self, name: String, value: Value) {
        self.global_environment.insert(name, value);
    }

    /// Look up a global constant or module-level binding
    pub fn lookup_global(&self, name: &str) -> Option<&Value> {
        self.global_environment.get(name)
    }

    /// Get a debug representation of the current state
    pub fn debug_state(&self) -> String {
        format!(
            "InterpreterContext {{\n  call_stack_depth: {},\n  variable_scopes: {},\n  global_bindings: {},\n}}",
            self.call_stack.depth(),
            self.variable_environment.scope_count(),
            self.global_environment.len()
        )
    }

    /// Get current scope information for debugging
    pub fn current_scope_info(&self) -> String {
        self.variable_environment.current_scope_info()
    }
}

/// Variable environment with lexical scoping support
#[derive(Debug)]
struct VariableEnvironment {
    /// Stack of scopes for lexical scoping (innermost scope at the end)
    scopes: Vec<Scope>,
}

impl VariableEnvironment {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::new(ScopeType::Global)],
        }
    }

    fn define(&mut self, name: String, value: Value) -> Result<(), ContextError> {
        if let Some(current_scope) = self.scopes.last_mut() {
            if current_scope.bindings.contains_key(&name) {
                return Err(ContextError::VariableAlreadyDefined { name });
            }
            current_scope.bindings.insert(name, value);
            Ok(())
        } else {
            // This should never happen as we always have at least the global scope
            unreachable!("No scopes available - this is a bug");
        }
    }

    fn lookup(&self, name: &str) -> Result<&Value, ContextError> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.bindings.get(name) {
                return Ok(value);
            }
        }
        Err(ContextError::VariableNotFound {
            name: name.to_string(),
        })
    }

    fn update(&mut self, name: &str, value: Value) -> Result<(), ContextError> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter_mut().rev() {
            if scope.bindings.contains_key(name) {
                scope.bindings.insert(name.to_string(), value);
                return Ok(());
            }
        }
        Err(ContextError::VariableNotFound {
            name: name.to_string(),
        })
    }

    fn push_scope(&mut self, scope_type: ScopeType) {
        self.scopes.push(Scope::new(scope_type));
    }

    fn pop_scope(&mut self) -> Result<(), ContextError> {
        if self.scopes.len() <= 1 {
            // Never pop the global scope
            return Err(ContextError::VariableNotAccessible {
                name: "global".to_string(),
            });
        }
        self.scopes.pop();
        Ok(())
    }

    fn scope_count(&self) -> usize {
        self.scopes.len()
    }

    fn current_scope_info(&self) -> String {
        if let Some(scope) = self.scopes.last() {
            format!(
                "{:?} scope with {} bindings",
                scope.scope_type,
                scope.bindings.len()
            )
        } else {
            "No active scope".to_string()
        }
    }
}

/// A single lexical scope containing variable bindings
#[derive(Debug)]
struct Scope {
    /// Variable name -> runtime value mapping
    bindings: HashMap<String, Value>,
    /// Scope type for debugging and error reporting
    scope_type: ScopeType,
}

impl Scope {
    fn new(scope_type: ScopeType) -> Self {
        Self {
            bindings: HashMap::new(),
            scope_type,
        }
    }
}

/// Types of lexical scopes for debugging and error reporting
#[derive(Debug, Clone)]
pub enum ScopeType {
    /// Global/module-level scope
    Global,
    /// Function call scope
    Function { name: String },
    /// Block scope (from let bindings, if expressions, etc.)
    Block,
    /// Pattern matching scope (from case expressions, destructuring)
    Pattern { pattern_span: Span },
    /// REPL expression scope
    Repl,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn test_variable_environment_basic_operations() {
        let mut ctx = InterpreterContext::new_repl();

        // Define a variable
        let value = Value::integer(42);
        ctx.define_variable("x".to_string(), value.clone()).unwrap();

        // Look it up
        let looked_up = ctx.lookup_variable("x").unwrap();
        assert_eq!(looked_up, &value);

        // Update it
        let new_value = Value::integer(100);
        ctx.update_variable("x", new_value.clone()).unwrap();
        let updated = ctx.lookup_variable("x").unwrap();
        assert_eq!(updated, &new_value);
    }

    #[test]
    fn test_lexical_scoping() {
        let mut ctx = InterpreterContext::new_repl();

        // Define in global scope
        ctx.define_variable("global_var".to_string(), Value::integer(1))
            .unwrap();

        // Push a new scope
        ctx.push_scope(ScopeType::Block);

        // Define in local scope (shadows global if same name)
        ctx.define_variable("local_var".to_string(), Value::integer(2))
            .unwrap();
        ctx.define_variable("global_var".to_string(), Value::integer(3))
            .unwrap(); // shadows

        // Look up variables
        assert_eq!(
            ctx.lookup_variable("local_var").unwrap(),
            &Value::integer(2)
        );
        assert_eq!(
            ctx.lookup_variable("global_var").unwrap(),
            &Value::integer(3)
        ); // shadowed

        // Pop scope
        ctx.pop_scope().unwrap();

        // Local variable should be gone, global should be restored
        assert!(ctx.lookup_variable("local_var").is_err());
        assert_eq!(
            ctx.lookup_variable("global_var").unwrap(),
            &Value::integer(1)
        ); // original
    }

    #[test]
    fn test_call_stack_management() {
        use outrun_typechecker::checker::TypedFunctionPath;

        let mut ctx = InterpreterContext::new_repl();

        assert_eq!(ctx.call_stack_depth(), 0);
        assert!(ctx.current_call_frame().is_none());

        // Push a call frame
        let function_path = TypedFunctionPath::Simple {
            name: "test_function".to_string(),
        };
        let frame = crate::call_stack::CallFrame::new(
            function_path,
            HashMap::new(),
            None,
            Span::new(0, 0),
            None,
            None,
        );
        ctx.push_call_frame(frame).unwrap();

        assert_eq!(ctx.call_stack_depth(), 1);
        assert!(ctx.current_call_frame().is_some());
        assert_eq!(
            ctx.current_call_frame().unwrap().function_name(),
            "test_function"
        );

        // Pop the frame
        let popped = ctx.pop_call_frame(Span::new(0, 0)).unwrap();
        assert_eq!(popped.function_name(), "test_function");
        assert_eq!(ctx.call_stack_depth(), 0);
    }

    #[test]
    fn test_global_environment() {
        let mut ctx = InterpreterContext::new_repl();

        // Define global
        let value = Value::string("global_constant".to_string());
        ctx.define_global("CONSTANT".to_string(), value.clone());

        // Look it up
        let looked_up = ctx.lookup_global("CONSTANT").unwrap();
        assert_eq!(looked_up, &value);

        // Non-existent global
        assert!(ctx.lookup_global("MISSING").is_none());
    }

    #[test]
    fn test_variable_shadowing() {
        let mut ctx = InterpreterContext::new_repl();

        // Define variable in global scope
        ctx.define_variable("x".to_string(), Value::integer(1))
            .unwrap();

        // Push function scope
        ctx.push_scope(ScopeType::Function {
            name: "test".to_string(),
        });
        ctx.define_variable("x".to_string(), Value::integer(2))
            .unwrap();

        // Push block scope
        ctx.push_scope(ScopeType::Block);
        ctx.define_variable("x".to_string(), Value::integer(3))
            .unwrap();

        // Should see innermost value
        assert_eq!(ctx.lookup_variable("x").unwrap(), &Value::integer(3));

        // Pop block scope
        ctx.pop_scope().unwrap();
        assert_eq!(ctx.lookup_variable("x").unwrap(), &Value::integer(2));

        // Pop function scope
        ctx.pop_scope().unwrap();
        assert_eq!(ctx.lookup_variable("x").unwrap(), &Value::integer(1));
    }

    #[test]
    fn test_error_conditions() {
        let mut ctx = InterpreterContext::new_repl();

        // Variable not found
        assert!(matches!(
            ctx.lookup_variable("missing"),
            Err(ContextError::VariableNotFound { .. })
        ));

        // Variable already defined
        ctx.define_variable("x".to_string(), Value::integer(1))
            .unwrap();
        assert!(matches!(
            ctx.define_variable("x".to_string(), Value::integer(2)),
            Err(ContextError::VariableAlreadyDefined { .. })
        ));

        // Cannot pop global scope
        assert!(ctx.pop_scope().is_err());

        // Cannot pop empty call stack
        assert!(matches!(
            ctx.pop_call_frame(Span::new(0, 0)),
            Err(ContextError::CallStack { .. })
        ));
    }

    #[test]
    fn test_stack_overflow_protection() {
        use outrun_typechecker::checker::TypedFunctionPath;

        let compiler_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        let mut ctx = InterpreterContext::new(
            UnificationContext::new(),
            compiler_env,
            Some(2), // Very small stack for testing
        );

        // Push frames up to the limit
        let frame1 = crate::call_stack::CallFrame::new(
            TypedFunctionPath::Simple {
                name: "func1".to_string(),
            },
            HashMap::new(),
            None,
            Span::new(0, 0),
            None,
            None,
        );
        let frame2 = crate::call_stack::CallFrame::new(
            TypedFunctionPath::Simple {
                name: "func2".to_string(),
            },
            HashMap::new(),
            None,
            Span::new(0, 0),
            None,
            None,
        );

        ctx.push_call_frame(frame1).unwrap();
        ctx.push_call_frame(frame2).unwrap();

        // Next push should fail
        let frame3 = crate::call_stack::CallFrame::new(
            TypedFunctionPath::Simple {
                name: "func3".to_string(),
            },
            HashMap::new(),
            None,
            Span::new(0, 0),
            None,
            None,
        );
        assert!(matches!(
            ctx.push_call_frame(frame3),
            Err(ContextError::CallStack { .. })
        ));
    }
}
