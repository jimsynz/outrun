//! Runtime context for the new Outrun interpreter
//!
//! This module provides the InterpreterContext that integrates with typechecker v3
//! and manages runtime state, variable bindings, and function dispatch.

use crate::value::Value;
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur during interpreter context operations
#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("Variable '{name}' not found in current scope")]
    VariableNotFound { name: String },

    #[error("Type error: {message}")]
    TypeError { message: String },

    #[error("Function call stack overflow (max depth: {max_depth})")]
    StackOverflow { max_depth: usize },

    #[error("Cannot access variable from outer scope: {name}")]
    ScopeError { name: String },
}

/// Runtime context for the new Outrun interpreter
///
/// This context integrates with typechecker v3's package-based compilation
/// system while providing runtime variable bindings and function dispatch.
pub struct InterpreterContext {
    /// Variable environment for runtime bindings (lexical scoping)
    variable_environment: VariableEnvironment,

    /// Function call stack for error reporting and recursion detection
    call_stack: Vec<CallFrame>,

    /// Maximum recursion depth to prevent stack overflow
    max_stack_depth: usize,
}

/// Variable environment managing lexical scoping
#[derive(Debug)]
struct VariableEnvironment {
    /// Stack of scopes, with each scope containing variable bindings
    scopes: Vec<HashMap<String, Value>>,
}

/// Call frame for function calls and error reporting
#[derive(Debug)]
pub struct CallFrame {
    /// Function or expression being evaluated
    pub name: String,
    /// Source location for error reporting
    pub span: Option<outrun_parser::Span>,
    /// Local variable bindings for this call
    pub locals: HashMap<String, Value>,
}

impl InterpreterContext {
    /// Create a new interpreter context
    pub fn new() -> Self {
        Self {
            variable_environment: VariableEnvironment::new(),
            call_stack: Vec::new(),
            max_stack_depth: 1000, // Reasonable default
        }
    }

    /// Check if the context is empty (no variables or call stack)
    pub fn is_empty(&self) -> bool {
        self.variable_environment.is_empty() && self.call_stack.is_empty()
    }

    /// Define a variable in the current scope (allows rebinding with let statements)
    pub fn define_variable(&mut self, name: String, value: Value) -> Result<(), InterpreterError> {
        // In Outrun, variable rebinding with `let` is allowed (but not mutation)
        self.variable_environment.define(name, value);
        Ok(())
    }

    /// Get a variable from the environment (searches all scopes)
    pub fn get_variable(&self, name: &str) -> Result<&Value, InterpreterError> {
        self.variable_environment
            .get(name)
            .ok_or_else(|| InterpreterError::VariableNotFound {
                name: name.to_string(),
            })
    }

    /// Update an existing variable (searches all scopes)
    pub fn update_variable(&mut self, name: String, value: Value) -> Result<(), InterpreterError> {
        if !self.variable_environment.contains(&name) {
            return Err(InterpreterError::VariableNotFound { name });
        }

        self.variable_environment.update(name, value);
        Ok(())
    }

    /// Get all variables in the current context (for REPL display)
    pub fn list_variables(&self) -> Vec<(String, &Value)> {
        self.variable_environment.list_all()
    }

    /// Remove a variable from the current environment (for type invalidation)
    pub fn remove_variable(&mut self, name: &str) -> Result<Value, InterpreterError> {
        self.variable_environment.remove(name)
            .ok_or_else(|| InterpreterError::VariableNotFound { 
                name: name.to_string() 
            })
    }

    /// Get all available variable names (for error reporting)
    pub fn get_available_variables(&self) -> Vec<String> {
        self.variable_environment.list_all()
            .into_iter()
            .map(|(name, _)| name)
            .collect()
    }

    /// Push a new variable scope (for function calls, let bindings, etc.)
    pub fn push_scope(&mut self) {
        self.variable_environment.push_scope();
    }

    /// Pop the current variable scope
    pub fn pop_scope(&mut self) {
        self.variable_environment.pop_scope();
    }

    /// Push a new call frame
    pub fn push_call_frame(
        &mut self,
        name: String,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), InterpreterError> {
        if self.call_stack.len() >= self.max_stack_depth {
            return Err(InterpreterError::StackOverflow {
                max_depth: self.max_stack_depth,
            });
        }

        let frame = CallFrame {
            name,
            span,
            locals: HashMap::new(),
        };

        self.call_stack.push(frame);
        Ok(())
    }

    /// Pop the current call frame
    pub fn pop_call_frame(&mut self) -> Option<CallFrame> {
        self.call_stack.pop()
    }

    /// Get the current call stack for error reporting
    pub fn call_stack(&self) -> &[CallFrame] {
        &self.call_stack
    }
}

impl VariableEnvironment {
    /// Create a new variable environment with a global scope
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // Start with global scope
        }
    }

    /// Check if the environment is empty (only global scope with no variables)
    fn is_empty(&self) -> bool {
        self.scopes.len() == 1 && self.scopes[0].is_empty()
    }

    /// Define a variable in the current (top) scope
    fn define(&mut self, name: String, value: Value) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, value);
        }
    }

    /// Get a variable by searching all scopes from current to global
    fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// Update an existing variable (searches all scopes)
    fn update(&mut self, name: String, value: Value) {
        for scope in self.scopes.iter_mut().rev() {
            if let std::collections::hash_map::Entry::Occupied(mut entry) =
                scope.entry(name.clone())
            {
                entry.insert(value);
                return;
            }
        }
    }

    /// Check if any scope contains a variable
    fn contains(&self, name: &str) -> bool {
        self.get(name).is_some()
    }

    /// Remove a variable from environment (searches all scopes)
    fn remove(&mut self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.remove(name) {
                return Some(value);
            }
        }
        None
    }

    /// Push a new scope
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pop the current scope (but keep at least the global scope)
    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// List all variables across all scopes (for REPL display)
    fn list_all(&self) -> Vec<(String, &Value)> {
        let mut variables = Vec::new();
        let mut seen_names = std::collections::HashSet::new();

        // Iterate through scopes from current to global to show shadowing correctly
        // This way, we get the most recent binding for each variable name
        for scope in self.scopes.iter().rev() {
            for (name, value) in scope {
                // Only add if not already seen (to handle shadowing)
                if !seen_names.contains(name) {
                    variables.push((name.clone(), value));
                    seen_names.insert(name.clone());
                }
            }
        }

        variables.sort_by(|a, b| a.0.cmp(&b.0)); // Sort by name
        variables
    }
}

impl Default for InterpreterContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_creation() {
        let context = InterpreterContext::new();
        assert!(context.is_empty());
        assert_eq!(context.call_stack().len(), 0);
    }

    #[test]
    fn test_variable_operations() {
        let mut context = InterpreterContext::new();

        // Define a variable
        let value = Value::integer(42);
        context
            .define_variable("x".to_string(), value.clone())
            .unwrap();

        // Get the variable
        let retrieved = context.get_variable("x").unwrap();
        assert_eq!(retrieved, &value);

        // Update the variable
        let new_value = Value::integer(84);
        context
            .update_variable("x".to_string(), new_value.clone())
            .unwrap();

        let updated = context.get_variable("x").unwrap();
        assert_eq!(updated, &new_value);
    }

    #[test]
    fn test_scope_management() {
        let mut context = InterpreterContext::new();

        // Define variable in global scope
        context
            .define_variable("global".to_string(), Value::integer(1))
            .unwrap();

        // Push new scope
        context.push_scope();

        // Define variable in local scope
        context
            .define_variable("local".to_string(), Value::integer(2))
            .unwrap();

        // Both variables should be accessible
        assert_eq!(context.get_variable("global").unwrap(), &Value::integer(1));
        assert_eq!(context.get_variable("local").unwrap(), &Value::integer(2));

        // Pop scope
        context.pop_scope();

        // Global should still be accessible, local should not
        assert_eq!(context.get_variable("global").unwrap(), &Value::integer(1));
        assert!(context.get_variable("local").is_err());
    }

    #[test]
    fn test_call_frame_management() {
        let mut context = InterpreterContext::new();

        // Push call frame
        context
            .push_call_frame("test_function".to_string(), None)
            .unwrap();
        assert_eq!(context.call_stack().len(), 1);

        // Pop call frame
        let frame = context.pop_call_frame().unwrap();
        assert_eq!(frame.name, "test_function");
        assert_eq!(context.call_stack().len(), 0);
    }

    #[test]
    fn test_error_conditions() {
        let mut context = InterpreterContext::new();

        // Try to get non-existent variable
        let result = context.get_variable("nonexistent");
        assert!(matches!(
            result,
            Err(InterpreterError::VariableNotFound { .. })
        ));

        // Test variable rebinding (should be allowed with let statements)
        context
            .define_variable("x".to_string(), Value::integer(1))
            .unwrap();
        // Rebinding should succeed
        context
            .define_variable("x".to_string(), Value::integer(2))
            .unwrap();
        // Value should be updated
        assert_eq!(context.get_variable("x").unwrap(), &Value::integer(2));

        // Try to update non-existent variable
        let result = context.update_variable("nonexistent".to_string(), Value::integer(1));
        assert!(matches!(
            result,
            Err(InterpreterError::VariableNotFound { .. })
        ));
    }
}
