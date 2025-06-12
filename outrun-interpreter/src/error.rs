//! Runtime error types for the Outrun interpreter.
//!
//! Provides comprehensive error handling with beautiful diagnostics
//! via miette integration.

use miette::{Diagnostic, SourceSpan};
use outrun_parser::Span;
use thiserror::Error;

/// Runtime errors that can occur during interpretation
#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum RuntimeError {
    #[error("Type error: expected {expected}, found {found}")]
    #[diagnostic(
        code(outrun::runtime::type_error),
        help("Check that the value has the expected type")
    )]
    TypeError {
        expected: String,
        found: String,
        #[label("type mismatch here")]
        span: Option<SourceSpan>,
    },

    #[error("Undefined variable: {name}")]
    #[diagnostic(
        code(outrun::runtime::undefined_variable),
        help("Make sure the variable is defined before use")
    )]
    UndefinedVariable {
        name: String,
        #[label("undefined variable")]
        span: Option<SourceSpan>,
    },

    #[error("Wrong arity: function {function} expects {expected} arguments, got {found}")]
    #[diagnostic(
        code(outrun::runtime::wrong_arity),
        help("Check the function signature for the correct number of arguments")
    )]
    WrongArity {
        function: String,
        expected: usize,
        found: usize,
        #[label("wrong number of arguments")]
        span: Option<SourceSpan>,
    },

    #[error("Division by zero")]
    #[diagnostic(
        code(outrun::runtime::division_by_zero),
        help("Ensure the divisor is not zero before division")
    )]
    DivisionByZero {
        #[label("division by zero here")]
        span: Option<SourceSpan>,
    },

    #[error("Index out of bounds: index {index} is not valid for collection of length {length}")]
    #[diagnostic(
        code(outrun::runtime::index_out_of_bounds),
        help("Ensure the index is within the valid range [0, {length})")
    )]
    IndexOutOfBounds {
        index: i64,
        length: usize,
        #[label("invalid index")]
        span: Option<SourceSpan>,
    },

    #[error("Invalid operation: {operation} cannot be applied to types [{operand_types}]")]
    #[diagnostic(
        code(outrun::runtime::invalid_operation),
        help("Check that the operation is supported for these types")
    )]
    InvalidOperation {
        operation: String,
        operand_types: String, // Join types with ", " when creating the error
        #[label("invalid operation")]
        span: Option<SourceSpan>,
    },

    #[error("{message}")]
    #[diagnostic(code(outrun::runtime::custom_error))]
    Custom {
        message: String,
        #[label("error occurred here")]
        span: Option<SourceSpan>,
    },
}

impl RuntimeError {
    /// Create a type error
    pub fn type_error(expected: &str, found: &str) -> Self {
        Self::TypeError {
            expected: expected.to_string(),
            found: found.to_string(),
            span: None,
        }
    }

    /// Create a type error with span
    pub fn type_error_with_span(expected: &str, found: &str, span: Span) -> Self {
        Self::TypeError {
            expected: expected.to_string(),
            found: found.to_string(),
            span: Some(span_to_source_span(span)),
        }
    }

    /// Create an undefined variable error
    pub fn undefined_variable(name: String) -> Self {
        Self::UndefinedVariable { name, span: None }
    }

    /// Create an undefined variable error with span
    pub fn undefined_variable_with_span(name: String, span: Span) -> Self {
        Self::UndefinedVariable {
            name,
            span: Some(span_to_source_span(span)),
        }
    }

    /// Create a wrong arity error
    pub fn wrong_arity(function: &str, expected: usize, found: usize) -> Self {
        Self::WrongArity {
            function: function.to_string(),
            expected,
            found,
            span: None,
        }
    }

    /// Create a wrong arity error with span
    pub fn wrong_arity_with_span(
        function: &str,
        expected: usize,
        found: usize,
        span: Span,
    ) -> Self {
        Self::WrongArity {
            function: function.to_string(),
            expected,
            found,
            span: Some(span_to_source_span(span)),
        }
    }

    /// Create a division by zero error
    pub fn division_by_zero() -> Self {
        Self::DivisionByZero { span: None }
    }

    /// Create a division by zero error with span
    pub fn division_by_zero_with_span(span: Span) -> Self {
        Self::DivisionByZero {
            span: Some(span_to_source_span(span)),
        }
    }

    /// Create an index out of bounds error
    pub fn index_out_of_bounds(index: i64, length: usize) -> Self {
        Self::IndexOutOfBounds {
            index,
            length,
            span: None,
        }
    }

    /// Create an index out of bounds error with span
    pub fn index_out_of_bounds_with_span(index: i64, length: usize, span: Span) -> Self {
        Self::IndexOutOfBounds {
            index,
            length,
            span: Some(span_to_source_span(span)),
        }
    }

    /// Create an invalid operation error
    pub fn invalid_operation(operation: &str, operand_types: Vec<String>) -> Self {
        Self::InvalidOperation {
            operation: operation.to_string(),
            operand_types: operand_types.join(", "),
            span: None,
        }
    }

    /// Create an invalid operation error with span
    pub fn invalid_operation_with_span(
        operation: &str,
        operand_types: Vec<String>,
        span: Span,
    ) -> Self {
        Self::InvalidOperation {
            operation: operation.to_string(),
            operand_types: operand_types.join(", "),
            span: Some(span_to_source_span(span)),
        }
    }

    /// Create a custom error
    pub fn custom(message: String) -> Self {
        Self::Custom {
            message,
            span: None,
        }
    }

    /// Create a custom error with span
    pub fn custom_with_span(message: String, span: Span) -> Self {
        Self::Custom {
            message,
            span: Some(span_to_source_span(span)),
        }
    }
}

/// Convert parser Span to miette SourceSpan
fn span_to_source_span(span: Span) -> SourceSpan {
    SourceSpan::new(span.start.into(), span.end - span.start)
}

/// Type alias for interpreter results
pub type Result<T> = std::result::Result<T, RuntimeError>;
