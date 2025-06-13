//! Type checking error definitions
//!
//! This module defines all possible type checking errors with miette integration
//! for beautiful error reporting with source highlighting.

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Result type for type checking operations
pub type TypeResult<T> = Result<T, TypeError>;

/// All possible type checking errors
#[derive(Error, Diagnostic, Debug, Clone)]
pub enum TypeError {
    #[error("Type mismatch")]
    #[diagnostic(
        code(outrun::types::mismatch),
        help("Expected type {expected}, but found {found}")
    )]
    TypeMismatch {
        #[label("type mismatch here")]
        span: SourceSpan,
        expected: String,
        found: String,
    },

    #[error("Trait {trait_name} not implemented for type {type_name}")]
    #[diagnostic(
        code(outrun::types::trait_not_implemented),
        help("Implement the {trait_name} trait for {type_name} or use a type that implements it")
    )]
    TraitNotImplemented {
        #[label("trait {trait_name} not implemented for {type_name}")]
        span: SourceSpan,
        trait_name: String,
        type_name: String,
    },

    #[error("Function parameter mismatch")]
    #[diagnostic(
        code(outrun::types::param_mismatch),
        help("Function {function_name} requires parameter {param_name}")
    )]
    ParameterMismatch {
        #[label("missing parameter {param_name}")]
        span: SourceSpan,
        param_name: String,
        function_name: String,
    },

    #[error("Variable {name} is already defined")]
    #[diagnostic(
        code(outrun::types::variable_redefinition),
        help("Variable names must be unique within the same scope")
    )]
    VariableAlreadyDefined {
        #[label("redefined here")]
        span: SourceSpan,
        #[label("previously defined here")]
        previous_span: SourceSpan,
        name: String,
    },

    #[error("Function {name} is already defined")]
    #[diagnostic(
        code(outrun::typechecker::function_redefinition),
        help("Function names must be unique within the same scope")
    )]
    FunctionAlreadyDefined {
        #[label("redefined here")]
        span: SourceSpan,
        #[label("previously defined here")]
        previous_span: SourceSpan,
        name: String,
    },

    #[error("Undefined type {name}")]
    #[diagnostic(
        code(outrun::typechecker::undefined_type),
        help("Define the type {name} or import it from another module")
    )]
    UndefinedType {
        #[label("undefined type")]
        span: SourceSpan,
        name: String,
    },

    #[error("Undefined variable {name}")]
    #[diagnostic(
        code(outrun::typechecker::undefined_variable),
        help("Define the variable {name} before using it")
    )]
    UndefinedVariable {
        #[label("undefined variable")]
        span: SourceSpan,
        name: String,
    },

    #[error("Undefined function {name}")]
    #[diagnostic(
        code(outrun::typechecker::undefined_function),
        help("Define the function {name} or import it from another module")
    )]
    UndefinedFunction {
        #[label("undefined function")]
        span: SourceSpan,
        name: String,
    },

    #[error("Missing parameter {parameter_name} for function {function_name}")]
    #[diagnostic(
        code(outrun::typechecker::missing_parameter),
        help("Function {function_name} requires parameter {parameter_name}")
    )]
    MissingParameter {
        #[label("missing required parameter")]
        span: SourceSpan,
        function_name: String,
        parameter_name: String,
    },

    #[error("Unexpected parameter {parameter_name} for function {function_name}")]
    #[diagnostic(
        code(outrun::typechecker::unexpected_parameter),
        help("Function {function_name} does not accept parameter {parameter_name}")
    )]
    UnexpectedParameter {
        #[label("unexpected parameter")]
        span: SourceSpan,
        function_name: String,
        parameter_name: String,
    },

    #[error("Invalid condition type")]
    #[diagnostic(
        code(outrun::typechecker::invalid_condition),
        help("Only Boolean values can be used in conditionals. Outrun has no truthiness.")
    )]
    InvalidConditionType {
        #[label("condition must be Boolean")]
        span: SourceSpan,
        found_type: String,
    },

    #[error("Guard expression must return Boolean")]
    #[diagnostic(
        code(outrun::typechecker::invalid_guard),
        help("Guard expressions (when clauses) must evaluate to Boolean values")
    )]
    InvalidGuardType {
        #[label("guard must return Boolean")]
        span: SourceSpan,
        found_type: String,
    },

    #[error("Pattern exhaustiveness error")]
    #[diagnostic(
        code(outrun::typechecker::non_exhaustive_patterns),
        help("Add patterns to handle all possible cases, or use a wildcard pattern (_)")
    )]
    NonExhaustivePatterns {
        #[label("missing patterns for these cases")]
        span: SourceSpan,
        missing_cases: Vec<String>,
    },

    #[error("Unreachable pattern")]
    #[diagnostic(
        code(outrun::typechecker::unreachable_pattern),
        help("This pattern will never match because previous patterns are more general")
    )]
    UnreachablePattern {
        #[label("unreachable pattern")]
        span: SourceSpan,
        #[label("this pattern covers all cases")]
        covering_span: SourceSpan,
    },

    #[error("Invalid arity for function {name}")]
    #[diagnostic(
        code(outrun::typechecker::invalid_arity),
        help("Function {name} expects {expected} parameters, but {found} were provided")
    )]
    InvalidArity {
        #[label("incorrect number of arguments")]
        span: SourceSpan,
        name: String,
        expected: usize,
        found: usize,
    },

    #[error("Circular trait dependency")]
    #[diagnostic(
        code(outrun::typechecker::circular_dependency),
        help("Trait dependencies must not form cycles")
    )]
    CircularTraitDependency {
        #[label("circular dependency detected")]
        span: SourceSpan,
        cycle: Vec<String>,
    },

    #[error("Invalid trait constraint")]
    #[diagnostic(
        code(outrun::typechecker::invalid_constraint),
        help("Trait constraint {constraint} is not satisfied")
    )]
    InvalidTraitConstraint {
        #[label("constraint not satisfied")]
        span: SourceSpan,
        constraint: String,
    },

    #[error("No current scope available")]
    #[diagnostic(
        code(outrun::typechecker::no_scope),
        help("This is an internal error - please report this bug")
    )]
    NoCurrentScope {
        #[label("no scope context")]
        span: SourceSpan,
    },

    #[error("Invalid guard function {function_name}")]
    #[diagnostic(
        code(outrun::typechecker::invalid_guard_function),
        help("Guard functions (ending with '?') must return Boolean values")
    )]
    InvalidGuardFunction {
        #[label("guard function must return Boolean")]
        span: SourceSpan,
        function_name: String,
        actual_return_type: String,
    },

    #[error("Undefined type parameter {parameter_name}")]
    #[diagnostic(
        code(outrun::typechecker::undefined_type_parameter),
        help("Type parameter {parameter_name} must be declared in the generic parameters list")
    )]
    UndefinedTypeParameter {
        #[label("undefined type parameter")]
        span: SourceSpan,
        parameter_name: String,
    },

    #[error("Feature not yet implemented: {feature}")]
    #[diagnostic(
        code(outrun::typechecker::unimplemented),
        help("This type checking feature is still under development")
    )]
    UnimplementedFeature {
        #[label("unimplemented feature")]
        span: SourceSpan,
        feature: String,
    },

    #[error("Internal type checker error: {message}")]
    #[diagnostic(
        code(outrun::typechecker::internal_error),
        help("This is a bug in the type checker - please report it")
    )]
    InternalError {
        #[label("internal error")]
        span: SourceSpan,
        message: String,
    },
}

impl TypeError {
    /// Create a type mismatch error
    pub fn type_mismatch(expected: String, found: String, span: SourceSpan) -> Self {
        Self::TypeMismatch {
            span,
            expected,
            found,
        }
    }

    /// Create a trait not implemented error
    pub fn trait_not_implemented(trait_name: String, type_name: String, span: SourceSpan) -> Self {
        Self::TraitNotImplemented {
            span,
            trait_name,
            type_name,
        }
    }

    /// Create a parameter mismatch error
    pub fn parameter_mismatch(param_name: String, function_name: String, span: SourceSpan) -> Self {
        Self::ParameterMismatch {
            span,
            param_name,
            function_name,
        }
    }

    /// Create an undefined type error
    pub fn undefined_type(name: String, span: SourceSpan) -> Self {
        Self::UndefinedType { span, name }
    }

    /// Create an undefined variable error
    pub fn undefined_variable(name: String, span: SourceSpan) -> Self {
        Self::UndefinedVariable { span, name }
    }

    /// Create an undefined function error
    pub fn undefined_function(name: String, span: SourceSpan) -> Self {
        Self::UndefinedFunction { span, name }
    }

    /// Create a missing parameter error
    pub fn missing_parameter(
        function_name: String,
        parameter_name: String,
        span: SourceSpan,
    ) -> Self {
        Self::MissingParameter {
            span,
            function_name,
            parameter_name,
        }
    }

    /// Create an unexpected parameter error
    pub fn unexpected_parameter(
        function_name: String,
        parameter_name: String,
        span: SourceSpan,
    ) -> Self {
        Self::UnexpectedParameter {
            span,
            function_name,
            parameter_name,
        }
    }

    /// Create an invalid condition type error
    pub fn invalid_condition_type(found_type: String, span: SourceSpan) -> Self {
        Self::InvalidConditionType { span, found_type }
    }

    /// Create an invalid guard type error
    pub fn invalid_guard_type(found_type: String, span: SourceSpan) -> Self {
        Self::InvalidGuardType { span, found_type }
    }

    /// Create a non-exhaustive patterns error
    pub fn non_exhaustive_patterns(missing_cases: Vec<String>, span: SourceSpan) -> Self {
        Self::NonExhaustivePatterns {
            span,
            missing_cases,
        }
    }

    /// Create an invalid arity error
    pub fn invalid_arity(name: String, expected: usize, found: usize, span: SourceSpan) -> Self {
        Self::InvalidArity {
            span,
            name,
            expected,
            found,
        }
    }

    /// Create an internal error (for debugging type checker issues)
    pub fn internal(message: String) -> Self {
        Self::InternalError {
            message,
            span: SourceSpan::from(0..0), // Use empty span for internal errors
        }
    }
}

/// Convert outrun_parser::Span to miette::SourceSpan
pub fn span_to_source_span(span: outrun_parser::Span) -> SourceSpan {
    SourceSpan::new(span.start.into(), span.end - span.start)
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::Span;

    #[test]
    fn test_type_error_creation() {
        let span = SourceSpan::new(0.into(), 5);

        let error = TypeError::type_mismatch("Integer".to_string(), "String".to_string(), span);

        match error {
            TypeError::TypeMismatch {
                expected, found, ..
            } => {
                assert_eq!(expected, "Integer");
                assert_eq!(found, "String");
            }
            _ => panic!("Expected TypeMismatch error"),
        }
    }

    #[test]
    fn test_span_conversion() {
        let parser_span = Span::new(10, 20);
        let source_span = span_to_source_span(parser_span);

        assert_eq!(source_span.offset(), 10);
        assert_eq!(source_span.len(), 10);
    }

    #[test]
    fn test_error_display() {
        let span = SourceSpan::new(0.into(), 5);
        let error = TypeError::undefined_variable("x".to_string(), span);

        let message = format!("{}", error);
        assert!(message.contains("Undefined variable x"));
    }

    #[test]
    fn test_convenience_constructors() {
        let span = SourceSpan::new(0.into(), 5);

        let error1 =
            TypeError::trait_not_implemented("Display".to_string(), "MyType".to_string(), span);
        assert!(matches!(error1, TypeError::TraitNotImplemented { .. }));

        let error2 = TypeError::invalid_arity("func".to_string(), 2, 3, span);
        assert!(matches!(error2, TypeError::InvalidArity { .. }));

        let error3 = TypeError::non_exhaustive_patterns(vec!["Some".to_string()], span);
        assert!(matches!(error3, TypeError::NonExhaustivePatterns { .. }));
    }
}
