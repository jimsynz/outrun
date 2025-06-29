//! Runtime type assertions for debug builds
//!
//! This module provides debug-only assertions that validate the typechecker's
//! work at runtime. These help catch typechecker bugs during development but
//! are compiled out in release builds for performance.

use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::unification::{StructuredType, UnificationContext, unify_structured_types};

/// Runtime assertion errors that indicate typechecker bugs
#[derive(Debug)]
pub enum AssertionError {
    /// Expected type doesn't match actual runtime value type
    TypeMismatch {
        expected: StructuredType,
        actual_value: String, // Debug representation of the value
        context: String,
        span: Span,
    },

    /// Function argument type doesn't match parameter type
    ArgumentTypeMismatch {
        function_name: String,
        parameter_name: String,
        expected: StructuredType,
        actual_value: String,
        span: Span,
    },

    /// Return value type doesn't match function signature
    ReturnTypeMismatch {
        function_name: String,
        expected: StructuredType,
        actual_value: String,
        span: Span,
    },

    /// Pattern matching failed when typechecker said it would succeed
    PatternMatchFailure {
        pattern_description: String,
        value_description: String,
        span: Span,
    },

    /// Control flow assertion failed (e.g., if condition not boolean)
    ControlFlowError {
        expected: String,
        actual: String,
        context: String,
        span: Span,
    },
}

impl std::fmt::Display for AssertionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssertionError::TypeMismatch {
                expected,
                actual_value,
                context,
                span,
            } => {
                write!(
                    f,
                    "TYPECHECKER BUG: Type mismatch in {context} at {span:?}\n  Expected: {expected:?}\n  Actual value: {actual_value}"
                )
            }
            AssertionError::ArgumentTypeMismatch {
                function_name,
                parameter_name,
                expected,
                actual_value,
                span,
            } => {
                write!(
                    f,
                    "TYPECHECKER BUG: Argument type mismatch in {function_name}({parameter_name}) at {span:?}\n  Expected: {expected:?}\n  Actual value: {actual_value}"
                )
            }
            AssertionError::ReturnTypeMismatch {
                function_name,
                expected,
                actual_value,
                span,
            } => {
                write!(
                    f,
                    "TYPECHECKER BUG: Return type mismatch in {function_name} at {span:?}\n  Expected: {expected:?}\n  Actual value: {actual_value}"
                )
            }
            AssertionError::PatternMatchFailure {
                pattern_description,
                value_description,
                span,
            } => {
                write!(
                    f,
                    "TYPECHECKER BUG: Pattern match failure at {span:?}\n  Pattern: {pattern_description}\n  Value: {value_description}"
                )
            }
            AssertionError::ControlFlowError {
                expected,
                actual,
                context,
                span,
            } => {
                write!(
                    f,
                    "TYPECHECKER BUG: Control flow error in {context} at {span:?}\n  Expected: {expected}\n  Actual: {actual}"
                )
            }
        }
    }
}

impl std::error::Error for AssertionError {}

/// Debug-only assertion macros and functions
pub struct RuntimeAssertions;

/// Context for runtime assertions that integrates with the typechecker
pub struct AssertionContext {
    /// Unification context for proper type checking
    unification_context: UnificationContext,
    /// Compiler environment for type name resolution
    compiler_environment:
        outrun_typechecker::compilation::compiler_environment::CompilerEnvironment,
}

impl Default for AssertionContext {
    fn default() -> Self {
        Self::new()
    }
}

impl AssertionContext {
    /// Create a new assertion context
    pub fn new() -> Self {
        let compiler_environment =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        Self {
            unification_context: UnificationContext::new(),
            compiler_environment,
        }
    }

    /// Create an assertion context from existing typechecker components
    pub fn from_context(
        unification_context: UnificationContext,
        compiler_environment: outrun_typechecker::compilation::compiler_environment::CompilerEnvironment,
    ) -> Self {
        Self {
            unification_context,
            compiler_environment,
        }
    }

    /// Get the StructuredType for a runtime value using proper type integration
    fn value_to_structured_type(&mut self, value: &Value) -> StructuredType {
        // Convert runtime value to its StructuredType representation
        // This uses the same logic as our TypeIntegration but with local state
        match value {
            Value::Boolean(_) => {
                let type_id = self
                    .compiler_environment
                    .intern_type_name("Outrun.Core.Boolean");
                StructuredType::Simple(type_id)
            }
            Value::Integer64(_) => {
                let type_id = self
                    .compiler_environment
                    .intern_type_name("Outrun.Core.Integer64");
                StructuredType::Simple(type_id)
            }
            Value::Float64(_) => {
                let type_id = self
                    .compiler_environment
                    .intern_type_name("Outrun.Core.Float64");
                StructuredType::Simple(type_id)
            }
            Value::String(_) => {
                let type_id = self
                    .compiler_environment
                    .intern_type_name("Outrun.Core.String");
                StructuredType::Simple(type_id)
            }
            Value::Atom(_) => {
                let type_id = self
                    .compiler_environment
                    .intern_type_name("Outrun.Core.Atom");
                StructuredType::Simple(type_id)
            }
            Value::List { element_type, .. } => {
                // Use the stored element type information
                element_type.clone()
            }
            Value::Map {
                key_type,
                value_type,
                ..
            } => {
                // Create a Map<K, V> type from stored type information
                let map_base = self.compiler_environment.intern_type_name("Map");
                StructuredType::Generic {
                    base: map_base,
                    args: vec![key_type.clone(), value_type.clone()],
                }
            }
            Value::Tuple { tuple_type, .. } => tuple_type.clone(),
            Value::Struct { struct_type, .. } => struct_type.clone(),
        }
    }

    /// Check if a runtime value can unify with an expected type
    fn can_unify(&mut self, value: &Value, expected_type: &StructuredType) -> bool {
        let actual_type = self.value_to_structured_type(value);

        // Use the unification system to check type compatibility
        // This is the proper way to check types in the Outrun typechecker
        unify_structured_types(
            &actual_type,
            expected_type,
            &self.unification_context,
            &self.compiler_environment,
        )
        .unwrap_or(None)
        .is_some()
    }
}

impl RuntimeAssertions {
    /// Assert that a value matches the expected type using unification (debug only)
    ///
    /// In debug builds, this validates that the runtime value can unify with what
    /// the typechecker said it should be. In release builds, this is a no-op.
    #[cfg(debug_assertions)]
    #[allow(clippy::result_large_err)] // Error contains type information needed for debugging
    pub fn assert_value_type_with_context(
        assertion_context: &mut AssertionContext,
        value: &Value,
        expected_type: &StructuredType,
        context: &str,
        span: Span,
    ) -> Result<(), AssertionError> {
        if !assertion_context.can_unify(value, expected_type) {
            return Err(AssertionError::TypeMismatch {
                expected: expected_type.clone(),
                actual_value: format!("{:?}", value),
                context: context.to_string(),
                span,
            });
        }
        Ok(())
    }

    #[cfg(not(debug_assertions))]
    pub fn assert_value_type_with_context(
        _assertion_context: &mut AssertionContext,
        _value: &Value,
        _expected_type: &StructuredType,
        _context: &str,
        _span: Span,
    ) -> Result<(), AssertionError> {
        Ok(())
    }

    /// Simplified assertion that uses a temporary context (debug only)
    /// Use this when you don't have a shared assertion context
    #[cfg(debug_assertions)]
    #[allow(clippy::result_large_err)] // Error contains type information needed for debugging
    pub fn assert_value_type(
        value: &Value,
        expected_type: &StructuredType,
        context: &str,
        span: Span,
    ) -> Result<(), AssertionError> {
        let mut assertion_context = AssertionContext::new();
        Self::assert_value_type_with_context(
            &mut assertion_context,
            value,
            expected_type,
            context,
            span,
        )
    }

    #[cfg(not(debug_assertions))]
    pub fn assert_value_type(
        _value: &Value,
        _expected_type: &StructuredType,
        _context: &str,
        _span: Span,
    ) -> Result<(), AssertionError> {
        Ok(())
    }

    /// Assert that function arguments match their parameter types using unification (debug only)
    #[cfg(debug_assertions)]
    #[allow(clippy::result_large_err)] // Error contains type information needed for debugging
    pub fn assert_function_arguments_with_context(
        assertion_context: &mut AssertionContext,
        function_name: &str,
        arguments: &[(String, &Value, &StructuredType)], // (name, value, expected_type)
        span: Span,
    ) -> Result<(), AssertionError> {
        for (param_name, value, expected_type) in arguments {
            if !assertion_context.can_unify(value, expected_type) {
                return Err(AssertionError::ArgumentTypeMismatch {
                    function_name: function_name.to_string(),
                    parameter_name: param_name.clone(),
                    expected: (*expected_type).clone(),
                    actual_value: format!("{:?}", value),
                    span,
                });
            }
        }
        Ok(())
    }

    #[cfg(not(debug_assertions))]
    pub fn assert_function_arguments_with_context(
        _assertion_context: &mut AssertionContext,
        _function_name: &str,
        _arguments: &[(String, &Value, &StructuredType)],
        _span: Span,
    ) -> Result<(), AssertionError> {
        Ok(())
    }

    /// Simplified function argument assertion (debug only)
    #[cfg(debug_assertions)]
    #[allow(clippy::result_large_err)] // Error contains type information needed for debugging
    pub fn assert_function_arguments(
        function_name: &str,
        arguments: &[(String, &Value, &StructuredType)], // (name, value, expected_type)
        span: Span,
    ) -> Result<(), AssertionError> {
        let mut assertion_context = AssertionContext::new();
        Self::assert_function_arguments_with_context(
            &mut assertion_context,
            function_name,
            arguments,
            span,
        )
    }

    #[cfg(not(debug_assertions))]
    pub fn assert_function_arguments(
        _function_name: &str,
        _arguments: &[(String, &Value, &StructuredType)],
        _span: Span,
    ) -> Result<(), AssertionError> {
        Ok(())
    }

    /// Assert that a function return value matches its signature using unification (debug only)
    #[cfg(debug_assertions)]
    #[allow(clippy::result_large_err)] // Error contains type information needed for debugging
    pub fn assert_return_type_with_context(
        assertion_context: &mut AssertionContext,
        function_name: &str,
        return_value: &Value,
        expected_type: &StructuredType,
        span: Span,
    ) -> Result<(), AssertionError> {
        if !assertion_context.can_unify(return_value, expected_type) {
            return Err(AssertionError::ReturnTypeMismatch {
                function_name: function_name.to_string(),
                expected: expected_type.clone(),
                actual_value: format!("{:?}", return_value),
                span,
            });
        }
        Ok(())
    }

    #[cfg(not(debug_assertions))]
    pub fn assert_return_type_with_context(
        _assertion_context: &mut AssertionContext,
        _function_name: &str,
        _return_value: &Value,
        _expected_type: &StructuredType,
        _span: Span,
    ) -> Result<(), AssertionError> {
        Ok(())
    }

    /// Simplified return type assertion (debug only)
    #[cfg(debug_assertions)]
    #[allow(clippy::result_large_err)] // Error contains type information needed for debugging
    pub fn assert_return_type(
        function_name: &str,
        return_value: &Value,
        expected_type: &StructuredType,
        span: Span,
    ) -> Result<(), AssertionError> {
        let mut assertion_context = AssertionContext::new();
        Self::assert_return_type_with_context(
            &mut assertion_context,
            function_name,
            return_value,
            expected_type,
            span,
        )
    }

    #[cfg(not(debug_assertions))]
    pub fn assert_return_type(
        _function_name: &str,
        _return_value: &Value,
        _expected_type: &StructuredType,
        _span: Span,
    ) -> Result<(), AssertionError> {
        Ok(())
    }

    /// Assert that a condition value is boolean (debug only)
    #[cfg(debug_assertions)]
    #[allow(clippy::result_large_err)] // Error contains type information needed for debugging
    pub fn assert_boolean_condition(
        condition_value: &Value,
        context: &str,
        span: Span,
    ) -> Result<(), AssertionError> {
        if !matches!(condition_value, Value::Boolean(_)) {
            return Err(AssertionError::ControlFlowError {
                expected: "Boolean".to_string(),
                actual: format!("{:?}", condition_value),
                context: context.to_string(),
                span,
            });
        }
        Ok(())
    }

    #[cfg(not(debug_assertions))]
    pub fn assert_boolean_condition(
        _condition_value: &Value,
        _context: &str,
        _span: Span,
    ) -> Result<(), AssertionError> {
        Ok(())
    }

    /// Assert that a pattern match should succeed (debug only)
    #[cfg(debug_assertions)]
    #[allow(clippy::result_large_err)] // Error contains type information needed for debugging
    pub fn assert_pattern_match(
        value: &Value,
        pattern_description: &str,
        should_match: bool,
        span: Span,
    ) -> Result<(), AssertionError> {
        // This would implement actual pattern matching validation
        // For now, we'll just check basic structure
        if !should_match {
            return Err(AssertionError::PatternMatchFailure {
                pattern_description: pattern_description.to_string(),
                value_description: format!("{:?}", value),
                span,
            });
        }
        Ok(())
    }

    #[cfg(not(debug_assertions))]
    pub fn assert_pattern_match(
        _value: &Value,
        _pattern_description: &str,
        _should_match: bool,
        _span: Span,
    ) -> Result<(), AssertionError> {
        Ok(())
    }
}

/// Convenience macro for asserting value types in debug builds
///
/// Usage: `debug_assert_type!(value, expected_type, "context", span)`
/// Or with context: `debug_assert_type!(context, value, expected_type, "context", span)`
#[macro_export]
macro_rules! debug_assert_type {
    // Version with assertion context for better performance
    ($assertion_context:expr, $value:expr, $expected_type:expr, $context:expr, $span:expr) => {
        #[cfg(debug_assertions)]
        {
            if let Err(err) =
                $crate::runtime_assertions::RuntimeAssertions::assert_value_type_with_context(
                    $assertion_context,
                    $value,
                    $expected_type,
                    $context,
                    $span,
                )
            {
                eprintln!("Runtime type assertion failed: {}", err);
                panic!("Typechecker bug detected: {}", err);
            }
        }
    };

    // Version without assertion context (creates temporary one)
    ($value:expr, $expected_type:expr, $context:expr, $span:expr) => {
        #[cfg(debug_assertions)]
        {
            if let Err(err) = $crate::runtime_assertions::RuntimeAssertions::assert_value_type(
                $value,
                $expected_type,
                $context,
                $span,
            ) {
                eprintln!("Runtime type assertion failed: {}", err);
                panic!("Typechecker bug detected: {}", err);
            }
        }
    };
}

/// Convenience macro for asserting function arguments in debug builds
#[macro_export]
macro_rules! debug_assert_function_args {
    // Version with assertion context
    ($assertion_context:expr, $function_name:expr, $arguments:expr, $span:expr) => {
        #[cfg(debug_assertions)]
        {
            if let Err(err) = $crate::runtime_assertions::RuntimeAssertions::assert_function_arguments_with_context(
                $assertion_context, $function_name, $arguments, $span
            ) {
                eprintln!("Runtime function argument assertion failed: {}", err);
                panic!("Typechecker bug detected: {}", err);
            }
        }
    };

    // Version without assertion context
    ($function_name:expr, $arguments:expr, $span:expr) => {
        #[cfg(debug_assertions)]
        {
            if let Err(err) = $crate::runtime_assertions::RuntimeAssertions::assert_function_arguments(
                $function_name, $arguments, $span
            ) {
                eprintln!("Runtime function argument assertion failed: {}", err);
                panic!("Typechecker bug detected: {}", err);
            }
        }
    };
}

/// Convenience macro for asserting return types in debug builds
#[macro_export]
macro_rules! debug_assert_return_type {
    // Version with assertion context
    ($assertion_context:expr, $function_name:expr, $return_value:expr, $expected_type:expr, $span:expr) => {
        #[cfg(debug_assertions)]
        {
            if let Err(err) =
                $crate::runtime_assertions::RuntimeAssertions::assert_return_type_with_context(
                    $assertion_context,
                    $function_name,
                    $return_value,
                    $expected_type,
                    $span,
                )
            {
                eprintln!("Runtime return type assertion failed: {}", err);
                panic!("Typechecker bug detected: {}", err);
            }
        }
    };

    // Version without assertion context
    ($function_name:expr, $return_value:expr, $expected_type:expr, $span:expr) => {
        #[cfg(debug_assertions)]
        {
            if let Err(err) = $crate::runtime_assertions::RuntimeAssertions::assert_return_type(
                $function_name,
                $return_value,
                $expected_type,
                $span,
            ) {
                eprintln!("Runtime return type assertion failed: {}", err);
                panic!("Typechecker bug detected: {}", err);
            }
        }
    };
}

/// Convenience macro for asserting boolean conditions in debug builds
#[macro_export]
macro_rules! debug_assert_boolean {
    ($condition_value:expr, $context:expr, $span:expr) => {
        #[cfg(debug_assertions)]
        {
            if let Err(err) =
                $crate::runtime_assertions::RuntimeAssertions::assert_boolean_condition(
                    $condition_value,
                    $context,
                    $span,
                )
            {
                eprintln!("Runtime boolean assertion failed: {}", err);
                panic!("Typechecker bug detected: {}", err);
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_typechecker::compilation::compiler_environment::CompilerEnvironment;

    #[test]
    fn test_primitive_type_assertions() {
        // Create compiler environment and use consistent type IDs
        let env = CompilerEnvironment::new();
        let boolean_type_id = env.intern_type_name("Outrun.Core.Boolean");
        let integer_type_id = env.intern_type_name("Outrun.Core.Integer64");

        let boolean_type = StructuredType::Simple(boolean_type_id);
        let integer_type = StructuredType::Simple(integer_type_id);

        let bool_val = Value::boolean(true);
        let int_val = Value::integer(42);
        let span = outrun_parser::Span::new(0, 0);

        // Create assertion context with unification context that has the same environment
        let unification_context = UnificationContext::new();
        let mut assertion_context = AssertionContext::from_context(unification_context, env);

        // These should pass
        assert!(
            RuntimeAssertions::assert_value_type_with_context(
                &mut assertion_context,
                &bool_val,
                &boolean_type,
                "test",
                span
            )
            .is_ok()
        );

        assert!(
            RuntimeAssertions::assert_value_type_with_context(
                &mut assertion_context,
                &int_val,
                &integer_type,
                "test",
                span
            )
            .is_ok()
        );
    }

    #[test]
    fn test_function_argument_assertions() {
        // Create shared type interner and consistent type IDs
        let env = CompilerEnvironment::new();
        let integer_type_id = env.intern_type_name("Outrun.Core.Integer64");
        let string_type_id = env.intern_type_name("Outrun.Core.String");

        let int_type = StructuredType::Simple(integer_type_id);
        let string_type = StructuredType::Simple(string_type_id);
        let span = outrun_parser::Span::new(0, 0);

        let int_val = Value::integer(42);
        let string_val = Value::string("test".to_string());
        let arguments = vec![
            ("x".to_string(), &int_val, &int_type),
            ("name".to_string(), &string_val, &string_type),
        ];

        // Create assertion context with shared interner
        let unification_context = UnificationContext::new();
        let mut assertion_context = AssertionContext::from_context(unification_context, env);

        // This should pass
        assert!(
            RuntimeAssertions::assert_function_arguments_with_context(
                &mut assertion_context,
                "test_function",
                &arguments,
                span
            )
            .is_ok()
        );
    }

    #[test]
    #[cfg(debug_assertions)]
    fn test_assertion_failure() {
        // Create shared type interner and consistent type IDs
        let env = CompilerEnvironment::new();
        let integer_type_id = env.intern_type_name("Outrun.Core.Integer64");
        let int_type = StructuredType::Simple(integer_type_id);

        let string_val = Value::string("not an integer".to_string());
        let span = outrun_parser::Span::new(0, 0);

        // Create assertion context with shared interner
        let unification_context = UnificationContext::new();
        let mut assertion_context = AssertionContext::from_context(unification_context, env);

        // This should fail in debug builds
        let result = RuntimeAssertions::assert_value_type_with_context(
            &mut assertion_context,
            &string_val,
            &int_type,
            "test",
            span,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_macro_usage() {
        // Create shared type interner and consistent type IDs
        let env = CompilerEnvironment::new();
        let integer_type_id = env.intern_type_name("Outrun.Core.Integer64");
        let int_type = StructuredType::Simple(integer_type_id);

        let int_val = Value::integer(42);
        let span = outrun_parser::Span::new(0, 0);

        // Create assertion context
        let unification_context = UnificationContext::new();
        let mut assertion_context = AssertionContext::from_context(unification_context, env);

        // Test that the macro doesn't panic for correct types
        debug_assert_type!(
            &mut assertion_context,
            &int_val,
            &int_type,
            "test macro",
            span
        );
    }
}
