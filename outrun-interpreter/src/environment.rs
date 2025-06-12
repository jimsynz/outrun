//! Environment and scope management for the Outrun interpreter.
//!
//! Handles variable bindings, built-in function registry, and scope management.

use crate::error::{Result, RuntimeError};
use crate::value::Value;
use std::collections::HashMap;

/// Variable environment with scope management
#[derive(Debug, Clone)]
pub struct Environment {
    /// Stack of variable scopes (most recent scope is last)
    scopes: Vec<Scope>,
    /// Built-in functions and constants (always accessible)
    builtin_scope: Scope,
}

/// A single scope containing variable bindings
#[derive(Debug, Clone)]
pub struct Scope {
    /// Variable name to value mapping
    variables: HashMap<String, Value>,
}

impl Environment {
    /// Create a new environment with built-in scope
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()], // Start with one scope
            builtin_scope: Scope::new(),
        }
    }

    /// Push a new scope onto the stack (for functions, blocks, etc.)
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    /// Pop the most recent scope from the stack
    /// Returns error if trying to pop the global scope
    pub fn pop_scope(&mut self) -> Result<()> {
        if self.scopes.len() <= 1 {
            return Err(RuntimeError::custom("Cannot pop global scope".to_string()));
        }
        self.scopes.pop();
        Ok(())
    }

    /// Get the current scope depth (0 = global scope)
    pub fn scope_depth(&self) -> usize {
        self.scopes.len() - 1
    }

    /// Define a variable in the current scope
    /// In Outrun, rebinding is allowed, so this always succeeds
    pub fn define(&mut self, name: String, value: Value) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.variables.insert(name, value);
        }
    }

    /// Get a variable value by name
    /// Searches from most recent scope to oldest, then built-ins
    pub fn get(&self, name: &str) -> Result<Value> {
        // Search through scopes from most recent to oldest
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.variables.get(name) {
                return Ok(value.clone());
            }
        }

        // Check built-in scope
        if let Some(value) = self.builtin_scope.variables.get(name) {
            return Ok(value.clone());
        }

        Err(RuntimeError::undefined_variable(name.to_string()))
    }

    /// Set an existing variable (for rebinding)
    /// Searches for the variable in scopes and updates the first match
    /// If not found, defines it in the current scope
    pub fn set(&mut self, name: &str, value: Value) {
        // Search through scopes from most recent to oldest
        for scope in self.scopes.iter_mut().rev() {
            if scope.variables.contains_key(name) {
                scope.variables.insert(name.to_string(), value);
                return;
            }
        }

        // If not found in any scope, define in current scope
        self.define(name.to_string(), value);
    }

    /// Register a built-in function
    pub fn register_builtin(&mut self, name: String, function: Value) {
        self.builtin_scope.variables.insert(name, function);
    }

    /// Check if a variable exists in any scope
    pub fn exists(&self, name: &str) -> bool {
        // Check all scopes
        for scope in &self.scopes {
            if scope.variables.contains_key(name) {
                return true;
            }
        }

        // Check built-in scope
        self.builtin_scope.variables.contains_key(name)
    }

    /// Get all variable names in the current scope (for REPL :vars command)
    pub fn current_scope_vars(&self) -> Vec<String> {
        if let Some(current_scope) = self.scopes.last() {
            current_scope.variables.keys().cloned().collect()
        } else {
            vec![]
        }
    }

    /// Get all variable names in all scopes (for debugging)
    pub fn all_vars(&self) -> Vec<(String, String)> {
        let mut vars = vec![];

        // Add built-ins
        for (name, value) in &self.builtin_scope.variables {
            vars.push((name.clone(), format!("builtin: {}", value.type_name())));
        }

        // Add scope variables (most recent first)
        for (scope_idx, scope) in self.scopes.iter().enumerate() {
            for (name, value) in &scope.variables {
                vars.push((
                    name.clone(),
                    format!("scope {}: {}", scope_idx, value.type_name()),
                ));
            }
        }

        vars
    }

    /// Clear all user-defined variables (keep built-ins)
    pub fn clear_user_vars(&mut self) {
        self.scopes.clear();
        self.scopes.push(Scope::new()); // Reset to just global scope
    }
}

impl Scope {
    /// Create a new empty scope
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    /// Get the number of variables in this scope
    #[allow(dead_code)]
    fn len(&self) -> usize {
        self.variables.len()
    }

    /// Check if this scope is empty
    #[allow(dead_code)]
    fn is_empty(&self) -> bool {
        self.variables.is_empty()
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_variable_operations() {
        let mut env = Environment::new();

        // Define a variable
        env.define("x".to_string(), Value::Integer(42));
        assert_eq!(env.get("x").unwrap(), Value::Integer(42));

        // Redefine (rebind) the same variable
        env.define("x".to_string(), Value::String("hello".to_string()));
        assert_eq!(env.get("x").unwrap(), Value::String("hello".to_string()));

        // Variable that doesn't exist
        assert!(env.get("nonexistent").is_err());
    }

    #[test]
    fn test_scope_management() {
        let mut env = Environment::new();

        // Define in global scope
        env.define("global_var".to_string(), Value::Integer(1));

        // Push new scope
        env.push_scope();
        env.define("local_var".to_string(), Value::Integer(2));

        // Both variables accessible
        assert_eq!(env.get("global_var").unwrap(), Value::Integer(1));
        assert_eq!(env.get("local_var").unwrap(), Value::Integer(2));

        // Shadow global variable
        env.define(
            "global_var".to_string(),
            Value::String("shadowed".to_string()),
        );
        assert_eq!(
            env.get("global_var").unwrap(),
            Value::String("shadowed".to_string())
        );

        // Pop scope
        env.pop_scope().unwrap();

        // Local variable no longer accessible
        assert!(env.get("local_var").is_err());

        // Global variable restored
        assert_eq!(env.get("global_var").unwrap(), Value::Integer(1));
    }

    #[test]
    fn test_builtin_registration() {
        let mut env = Environment::new();

        // Register a built-in function
        let print_fn = Value::BuiltinFunction {
            name: "print".to_string(),
            arity: 1,
            function: |_args| Ok(Value::Unit),
        };
        env.register_builtin("print".to_string(), print_fn.clone());

        // Built-in should be accessible
        assert_eq!(env.get("print").unwrap(), print_fn);

        // Built-ins accessible from any scope
        env.push_scope();
        assert_eq!(env.get("print").unwrap(), print_fn);
    }

    #[test]
    fn test_variable_exists() {
        let mut env = Environment::new();

        // Variable doesn't exist initially
        assert!(!env.exists("test"));

        // Define variable
        env.define("test".to_string(), Value::Integer(42));
        assert!(env.exists("test"));

        // Register built-in
        env.register_builtin("builtin_fn".to_string(), Value::Unit);
        assert!(env.exists("builtin_fn"));
    }

    #[test]
    fn test_cannot_pop_global_scope() {
        let mut env = Environment::new();

        // Can't pop the global scope
        assert!(env.pop_scope().is_err());

        // But can pop after pushing
        env.push_scope();
        assert!(env.pop_scope().is_ok());
    }

    #[test]
    fn test_scope_depth() {
        let mut env = Environment::new();

        assert_eq!(env.scope_depth(), 0); // Global scope

        env.push_scope();
        assert_eq!(env.scope_depth(), 1);

        env.push_scope();
        assert_eq!(env.scope_depth(), 2);

        env.pop_scope().unwrap();
        assert_eq!(env.scope_depth(), 1);
    }
}
