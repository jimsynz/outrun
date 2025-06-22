//! Type checking error definitions
//!
//! This module defines all possible type checking errors with miette integration
//! for beautiful error reporting with source highlighting.

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Result type for type checking operations
pub type TypeResult<T> = Result<T, TypeError>;

/// All possible type checking errors
#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
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

    #[error("Generic parameter {name} is already defined")]
    #[diagnostic(
        code(outrun::typechecker::generic_parameter_redefinition),
        help("Generic parameter names must be unique within the same scope")
    )]
    GenericParameterAlreadyDefined {
        #[label("redefined here")]
        span: SourceSpan,
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

    #[error("Conflicting function overload for {name}")]
    #[diagnostic(
        code(outrun::typechecker::conflicting_function_overload),
        help(
            "Function overloads must have different guard conditions or different parameter types"
        )
    )]
    ConflictingFunctionOverload {
        #[label("conflicting overload here")]
        span: SourceSpan,
        #[label("previous overload here")]
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

    #[error("Instance function {function_name} must have at least one Self parameter")]
    #[diagnostic(
        code(outrun::typechecker::missing_self_parameter),
        help("Add a Self parameter like 'self: Self' to make this an instance function, or use 'defs' for static functions")
    )]
    MissingSelfParameter {
        #[label("missing Self parameter")]
        span: SourceSpan,
        function_name: String,
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

    #[error("Generic arity mismatch for {type_name}")]
    #[diagnostic(
        code(outrun::typechecker::generic_arity_mismatch),
        help("{type_name} expects {expected} generic parameters, but {found} were provided")
    )]
    GenericArityMismatch {
        #[label("incorrect number of generic arguments")]
        span: SourceSpan,
        type_name: String,
        expected: usize,
        found: usize,
    },

    #[error("Field {field_name} not found on type {struct_name}")]
    #[diagnostic(
        code(outrun::typechecker::undefined_field),
        help("Check the field name and ensure it exists on type {struct_name}")
    )]
    UndefinedField {
        #[label("undefined field")]
        span: SourceSpan,
        struct_name: String,
        field_name: String,
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

    #[error("Case statement is not exhaustive")]
    #[diagnostic(
        code(outrun::types::case_not_exhaustive),
        help("Add cases for missing types: {missing_types}")
    )]
    CaseNotExhaustive {
        #[label("missing cases for trait {trait_name}")]
        span: SourceSpan,
        trait_name: String,
        missing_types: String,
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

    #[error("Cannot infer type for empty list")]
    #[diagnostic(
        code(outrun::types::cannot_infer_list_type),
        help("Add a type annotation to specify the list element type, e.g., `let empty: List<String> = []`")
    )]
    CannotInferListType {
        #[label("empty list needs type annotation")]
        span: SourceSpan,
    },

    #[error("Cannot infer generic type parameter {type_param}")]
    #[diagnostic(
        code(outrun::types::cannot_infer_generic_type),
        help("Add a type annotation to specify the generic type, e.g., `let result: Some<String> = Some {{ value: get_value() }}`")
    )]
    CannotInferGenericType {
        #[label("cannot infer type parameter {type_param}")]
        span: SourceSpan,
        type_param: String,
    },

    #[error("List elements have incompatible types")]
    #[diagnostic(
        code(outrun::types::mixed_list_elements),
        help("Either make all elements the same type, or add a type annotation like `let mixed: List<SomeTraitType> = [...]` if the elements implement a common trait")
    )]
    MixedListElements {
        #[label("this element has type {found_type}")]
        span: SourceSpan,
        expected_type: String,
        found_type: String,
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

    #[error("Empty block")]
    #[diagnostic(
        code(outrun::typechecker::empty_block),
        help("Every expression must have a value. Add a statement or expression to this block.")
    )]
    EmptyBlock {
        #[label("empty block")]
        span: SourceSpan,
        message: String,
    },

    #[error("Undefined trait {trait_name}")]
    #[diagnostic(
        code(outrun::typechecker::undefined_trait),
        help("Define the trait {trait_name} or import it from another module")
    )]
    UndefinedTrait {
        #[label("undefined trait")]
        span: SourceSpan,
        trait_name: String,
    },

    #[error("Duplicate implementation of trait {trait_name} for type {type_name}")]
    #[diagnostic(
        code(outrun::typechecker::duplicate_implementation),
        help("Each type can only implement a trait once")
    )]
    DuplicateImplementation {
        #[label("duplicate implementation")]
        span: SourceSpan,
        trait_name: String,
        type_name: String,
    },

    #[error("Missing implementation of function {function_name} in trait {trait_name} for type {type_name}")]
    #[diagnostic(
        code(outrun::typechecker::missing_implementation),
        help("All trait functions must be implemented")
    )]
    MissingImplementation {
        #[label("missing function implementation")]
        span: SourceSpan,
        trait_name: String,
        type_name: String,
        function_name: String,
    },

    #[error("Extra implementation of function {function_name} not declared in trait {trait_name}")]
    #[diagnostic(
        code(outrun::typechecker::extra_implementation),
        help("Only functions declared in the trait can be implemented")
    )]
    ExtraImplementation {
        #[label("extra function implementation")]
        span: SourceSpan,
        trait_name: String,
        function_name: String,
    },

    #[error("Function signature mismatch for {function_name}")]
    #[diagnostic(
        code(outrun::typechecker::signature_mismatch),
        help("Implementation signature must match trait signature exactly")
    )]
    SignatureMismatch {
        #[label("signature mismatch")]
        span: SourceSpan,
        function_name: String,
        expected: String,
        found: String,
    },

    #[error("Parameter signature mismatch in anonymous function")]
    #[diagnostic(
        code(outrun::typechecker::parameter_signature_mismatch),
        help("All clauses in an anonymous function must have identical parameter signatures")
    )]
    ParameterSignatureMismatch {
        #[label("this clause has different parameter signature")]
        span: SourceSpan,
        #[label("first clause signature defined here")]
        first_clause_span: SourceSpan,
        clause_index: usize,
        expected_signature: String,
        found_signature: String,
    },

    #[error("Return type mismatch in anonymous function")]
    #[diagnostic(
        code(outrun::typechecker::return_type_mismatch),
        help("All clauses in an anonymous function must return the same type")
    )]
    ReturnTypeMismatch {
        #[label("this clause returns different type")]
        span: SourceSpan,
        #[label("first clause return type defined here")]
        first_clause_span: SourceSpan,
        clause_index: usize,
        expected_type: String,
        found_type: String,
    },

    #[error("Invalid guard in anonymous function")]
    #[diagnostic(
        code(outrun::typechecker::invalid_anonymous_guard),
        help("Guards in anonymous functions must return Boolean values")
    )]
    InvalidAnonymousGuard {
        #[label("guard must return Boolean")]
        span: SourceSpan,
        clause_index: usize,
        found_type: String,
    },

    #[error("Pattern mismatch in anonymous function parameters")]
    #[diagnostic(
        code(outrun::typechecker::pattern_parameter_mismatch),
        help("All clauses must use the same pattern structure in parameters")
    )]
    PatternParameterMismatch {
        #[label("this clause uses different pattern structure")]
        span: SourceSpan,
        #[label("first clause pattern defined here")]
        first_clause_span: SourceSpan,
        clause_index: usize,
        expected_pattern: String,
        found_pattern: String,
    },

    #[error("String interpolation error: type {type_name} does not implement Display trait")]
    #[diagnostic(
        code(outrun::typechecker::string_interpolation_display),
        help("Only types that implement the Display trait can be interpolated in strings. Implement Display for {type_name} or convert the value explicitly.")
    )]
    StringInterpolationDisplayError {
        #[label("expression of type {type_name} cannot be displayed")]
        span: SourceSpan,
        type_name: String,
    },

    #[error("Function with guards is not exhaustive")]
    #[diagnostic(
        code(outrun::exhaustiveness::function_not_exhaustive),
        help("Add a default case (function clause without guard) or ensure all possible values are covered by guards")
    )]
    FunctionNotExhaustive {
        #[label("this function is missing coverage")]
        span: SourceSpan,
        function_name: String,
        missing_cases: Vec<String>,
    },

    #[error("Boolean case statement is not exhaustive")]
    #[diagnostic(
        code(outrun::exhaustiveness::boolean_not_exhaustive),
        help("Boolean case statements must handle both 'true' and 'false' values. Missing: {}", missing_values.join(", "))
    )]
    BooleanNotExhaustive {
        #[label("missing boolean patterns")]
        span: SourceSpan,
        missing_values: Vec<String>,
    },

    #[error("Trait case statement is not exhaustive")]
    #[diagnostic(
        code(outrun::exhaustiveness::trait_not_exhaustive),
        help("Trait case statements must handle all implementing types. Missing: {}", missing_types.join(", "))
    )]
    TraitNotExhaustive {
        #[label("missing trait implementation patterns")]
        span: SourceSpan,
        trait_name: String,
        missing_types: Vec<String>,
    },

    #[error("Unknown parameter {parameter_name} for function {function_name}")]
    #[diagnostic(
        code(outrun::typechecker::unknown_parameter),
        help("Function {function_name} does not have a parameter named {parameter_name}")
    )]
    UnknownParameter {
        #[label("unknown parameter")]
        span: SourceSpan,
        function_name: String,
        parameter_name: String,
    },

    #[error("Duplicate argument for parameter {parameter_name} in function {function_name}")]
    #[diagnostic(
        code(outrun::typechecker::duplicate_argument),
        help("Each parameter can only be provided once in a function call to {function_name}")
    )]
    DuplicateArgument {
        #[label("duplicate argument")]
        span: SourceSpan,
        function_name: String,
        parameter_name: String,
    },

    #[error("Argument type mismatch for parameter {parameter_name} in function {function_name}")]
    #[diagnostic(
        code(outrun::typechecker::argument_type_mismatch),
        help("Function {function_name} expects parameter {parameter_name} to be of type {expected_type}, but found {found_type}")
    )]
    ArgumentTypeMismatch {
        #[label("argument type mismatch")]
        span: SourceSpan,
        function_name: String,
        parameter_name: String,
        expected_type: String,
        found_type: String,
    },

    #[error("Missing argument for parameter {parameter_name} in function {function_name}")]
    #[diagnostic(
        code(outrun::typechecker::missing_argument),
        help("Function {function_name} requires an argument for parameter {parameter_name}")
    )]
    MissingArgument {
        #[label("missing argument")]
        span: SourceSpan,
        function_name: String,
        parameter_name: String,
    },

    #[error("Unsupported feature: {feature}")]
    #[diagnostic(
        code(outrun::typechecker::unsupported_feature),
        help("This feature is not yet supported in the type checker")
    )]
    UnsupportedFeature {
        #[label("unsupported feature")]
        span: SourceSpan,
        feature: String,
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

    /// Create an undefined field error
    pub fn undefined_field(field_name: String, struct_name: String, span: SourceSpan) -> Self {
        Self::UndefinedField {
            span,
            struct_name,
            field_name,
        }
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

    /// Create an internal error with a specific span
    pub fn internal_with_span(message: String, span: SourceSpan) -> Self {
        Self::InternalError { message, span }
    }

    /// Create a string interpolation display error
    pub fn string_interpolation_display(type_name: String, span: SourceSpan) -> Self {
        Self::StringInterpolationDisplayError { span, type_name }
    }

    /// Create a function not exhaustive error
    pub fn function_not_exhaustive(
        function_name: String,
        missing_cases: Vec<String>,
        span: SourceSpan,
    ) -> Self {
        Self::FunctionNotExhaustive {
            span,
            function_name,
            missing_cases,
        }
    }

    /// Create a boolean not exhaustive error
    pub fn boolean_not_exhaustive(missing_values: Vec<String>, span: SourceSpan) -> Self {
        Self::BooleanNotExhaustive {
            span,
            missing_values,
        }
    }

    /// Create a trait not exhaustive error
    pub fn trait_not_exhaustive(
        trait_name: String,
        missing_types: Vec<String>,
        span: SourceSpan,
    ) -> Self {
        Self::TraitNotExhaustive {
            span,
            trait_name,
            missing_types,
        }
    }

    /// Create a case not exhaustive error (for trait cases)
    pub fn case_not_exhaustive_trait(
        trait_name: String,
        missing_types: String,
        span: SourceSpan,
    ) -> Self {
        Self::CaseNotExhaustive {
            span,
            trait_name,
            missing_types,
        }
    }
}

/// Convert outrun_parser::Span to miette::SourceSpan
pub fn span_to_source_span(span: outrun_parser::Span) -> SourceSpan {
    SourceSpan::new(span.start.into(), span.end - span.start)
}

/// Extension trait for easier span conversion
pub trait SpanExt {
    /// Convert to miette::SourceSpan
    fn to_source_span(self) -> SourceSpan;
}

impl SpanExt for outrun_parser::Span {
    fn to_source_span(self) -> SourceSpan {
        span_to_source_span(self)
    }
}

/// Standardized error context utilities
pub mod context {
    use crate::types::{TypeId, TypeInterner};

    /// Get a type name with fallback for unknown types
    pub fn type_name_or_unknown(interner: &TypeInterner, type_id: TypeId) -> String {
        interner
            .type_name(type_id)
            .unwrap_or_else(|| format!("Unknown({:?})", type_id))
    }

    /// Get an atom name with fallback for unknown atoms
    pub fn atom_name_or_fallback(interner: &TypeInterner, atom_id: crate::types::AtomId) -> String {
        interner
            .atom_name(atom_id)
            .unwrap_or_else(|| format!("atom_{:?}", atom_id))
    }

    /// Common error message constants and builders
    pub mod messages {
        pub const NOT_YET_SUPPORTED: &str = "not yet supported";
        pub const NOT_YET_IMPLEMENTED: &str = "not yet implemented";
        pub const SHOULD_BE_DESUGARED: &str = "should be desugared";
        pub const FALLBACK_TYPE: &str = "fallback type";

        /// Create a "not yet supported" message for a feature
        pub fn not_yet_supported(feature: &str) -> String {
            format!("{} {}", feature, NOT_YET_SUPPORTED)
        }

        /// Create a "not yet implemented" message for a feature
        pub fn not_yet_implemented(feature: &str) -> String {
            format!("{} {}", feature, NOT_YET_IMPLEMENTED)
        }

        /// Create a "should be desugared" message for an operation
        pub fn should_be_desugared(operation: &str) -> String {
            format!(
                "{} found during type checking - {}",
                operation, SHOULD_BE_DESUGARED
            )
        }
    }
}

/// Enhanced type error report with source context for beautiful error display
#[derive(Debug, Clone)]
pub struct TypeErrorReport {
    /// The individual type errors
    pub errors: Vec<TypeErrorWithSource>,

    /// The original source code for context
    pub source: String,

    /// The filename for error reporting
    pub filename: String,
}

impl std::fmt::Display for TypeErrorReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Type checking failed with {} error{}",
            self.errors.len(),
            if self.errors.len() == 1 { "" } else { "s" }
        )
    }
}

impl std::error::Error for TypeErrorReport {}

impl Diagnostic for TypeErrorReport {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new("outrun::typechecker::multiple_errors"))
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Error)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new("Fix the type errors listed above to proceed"))
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        Some(Box::new(self.errors.iter().map(|e| e as &dyn Diagnostic)))
    }
}

/// A type error with source context for miette integration
#[derive(Debug, Clone)]
pub struct TypeErrorWithSource {
    /// The underlying type error
    pub inner: TypeError,

    /// Source context for miette (derived from inner.span)
    pub source: String,

    /// Filename for display
    pub filename: String,
}

// Implement miette traits manually since we need special handling
impl std::fmt::Display for TypeErrorWithSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl std::error::Error for TypeErrorWithSource {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.inner)
    }
}

impl Diagnostic for TypeErrorWithSource {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.inner.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.url()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.inner.labels()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source)
    }
}

impl TypeErrorWithSource {
    /// Create a new TypeErrorWithSource with explicit source context
    pub fn new(error: TypeError, source: String, filename: String) -> Self {
        Self {
            inner: error,
            source,
            filename,
        }
    }
}

impl TypeErrorReport {
    /// Create a new type error report with source context
    pub fn new(errors: Vec<TypeError>, source: String, filename: String) -> Self {
        let errors_with_source = errors
            .into_iter()
            .map(|error| TypeErrorWithSource {
                inner: error,
                source: source.clone(),
                filename: filename.clone(),
            })
            .collect();

        Self {
            errors: errors_with_source,
            source,
            filename,
        }
    }

    /// Create a new type error report from errors that already have source context
    pub fn from_errors_with_source(errors: Vec<TypeErrorWithSource>) -> Self {
        // Use the first error's source and filename as fallback for the report itself
        let (source, filename) = if let Some(first_error) = errors.first() {
            (first_error.source.clone(), first_error.filename.clone())
        } else {
            (String::new(), "<unknown>".to_string())
        };

        Self {
            errors,
            source,
            filename,
        }
    }

    /// Get the number of errors in this report
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    /// Check if this report has any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the underlying TypeError objects
    pub fn type_errors(&self) -> Vec<&TypeError> {
        self.errors.iter().map(|e| &e.inner).collect()
    }

    /// Create individual miette reports for each error with source context
    ///
    /// This is useful for CLI applications that want to display errors with
    /// beautiful formatting using miette's built-in rendering.
    pub fn create_individual_reports(&self) -> Vec<miette::Report> {
        self.errors
            .iter()
            .map(|error_with_source| {
                miette::Report::new(error_with_source.inner.clone()).with_source_code(
                    miette::NamedSource::new(
                        &error_with_source.filename,
                        error_with_source.source.clone(),
                    ),
                )
            })
            .collect()
    }

    /// Create a summary of error types for reporting
    pub fn error_summary(&self) -> ErrorSummary {
        let mut type_mismatches = 0;
        let mut undefined_functions = 0;
        let mut undefined_variables = 0;
        let mut trait_errors = 0;
        let mut other_errors = 0;

        for error in &self.errors {
            match &error.inner {
                TypeError::TypeMismatch { .. }
                | TypeError::InvalidConditionType { .. }
                | TypeError::InvalidGuardType { .. } => {
                    type_mismatches += 1;
                }
                TypeError::UndefinedFunction { .. } => {
                    undefined_functions += 1;
                }
                TypeError::UndefinedVariable { .. } => {
                    undefined_variables += 1;
                }
                TypeError::TraitNotImplemented { .. }
                | TypeError::UndefinedTrait { .. }
                | TypeError::DuplicateImplementation { .. }
                | TypeError::MissingImplementation { .. }
                | TypeError::ExtraImplementation { .. }
                | TypeError::SignatureMismatch { .. } => {
                    trait_errors += 1;
                }
                _ => {
                    other_errors += 1;
                }
            }
        }

        ErrorSummary {
            total: self.errors.len(),
            type_mismatches,
            undefined_functions,
            undefined_variables,
            trait_errors,
            other_errors,
        }
    }

    /// Group related errors together for better display
    pub fn group_related_errors(&self) -> Vec<ErrorGroup> {
        let mut groups = Vec::new();
        let mut function_errors = Vec::new();
        let mut type_mismatch_errors = Vec::new();
        let mut trait_errors = Vec::new();
        let mut other_errors = Vec::new();

        for error in &self.errors {
            match &error.inner {
                TypeError::UndefinedFunction { .. }
                | TypeError::MissingParameter { .. }
                | TypeError::UnexpectedParameter { .. }
                | TypeError::InvalidArity { .. }
                | TypeError::ConflictingFunctionOverload { .. } => {
                    function_errors.push(error.clone());
                }
                TypeError::TypeMismatch { .. }
                | TypeError::InvalidConditionType { .. }
                | TypeError::InvalidGuardType { .. } => {
                    type_mismatch_errors.push(error.clone());
                }
                TypeError::TraitNotImplemented { .. }
                | TypeError::UndefinedTrait { .. }
                | TypeError::DuplicateImplementation { .. }
                | TypeError::MissingImplementation { .. }
                | TypeError::ExtraImplementation { .. }
                | TypeError::SignatureMismatch { .. } => {
                    trait_errors.push(error.clone());
                }
                _ => {
                    other_errors.push(error.clone());
                }
            }
        }

        if !function_errors.is_empty() {
            groups.push(ErrorGroup {
                category: "Function Errors".to_string(),
                description: "Issues with function definitions or calls".to_string(),
                errors: function_errors,
            });
        }

        if !type_mismatch_errors.is_empty() {
            groups.push(ErrorGroup {
                category: "Type Mismatch Errors".to_string(),
                description: "Values have incompatible types".to_string(),
                errors: type_mismatch_errors,
            });
        }

        if !trait_errors.is_empty() {
            groups.push(ErrorGroup {
                category: "Trait System Errors".to_string(),
                description: "Issues with trait definitions or implementations".to_string(),
                errors: trait_errors,
            });
        }

        if !other_errors.is_empty() {
            groups.push(ErrorGroup {
                category: "Other Errors".to_string(),
                description: "Additional issues found".to_string(),
                errors: other_errors,
            });
        }

        groups
    }
}

/// A group of related type errors for organized display
#[derive(Debug, Clone)]
pub struct ErrorGroup {
    pub category: String,
    pub description: String,
    pub errors: Vec<TypeErrorWithSource>,
}

impl ErrorGroup {
    /// Get error count for this group
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }
}

/// Summary of error types for reporting and statistics
#[derive(Debug, Clone)]
pub struct ErrorSummary {
    pub total: usize,
    pub type_mismatches: usize,
    pub undefined_functions: usize,
    pub undefined_variables: usize,
    pub trait_errors: usize,
    pub other_errors: usize,
}

impl std::fmt::Display for ErrorSummary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} total errors", self.total)?;

        let mut parts = Vec::new();
        if self.type_mismatches > 0 {
            parts.push(format!(
                "{} type mismatch{}",
                self.type_mismatches,
                if self.type_mismatches == 1 { "" } else { "es" }
            ));
        }
        if self.undefined_functions > 0 {
            parts.push(format!(
                "{} undefined function{}",
                self.undefined_functions,
                if self.undefined_functions == 1 {
                    ""
                } else {
                    "s"
                }
            ));
        }
        if self.undefined_variables > 0 {
            parts.push(format!(
                "{} undefined variable{}",
                self.undefined_variables,
                if self.undefined_variables == 1 {
                    ""
                } else {
                    "s"
                }
            ));
        }
        if self.trait_errors > 0 {
            parts.push(format!(
                "{} trait error{}",
                self.trait_errors,
                if self.trait_errors == 1 { "" } else { "s" }
            ));
        }
        if self.other_errors > 0 {
            parts.push(format!(
                "{} other error{}",
                self.other_errors,
                if self.other_errors == 1 { "" } else { "s" }
            ));
        }

        if !parts.is_empty() {
            write!(f, " ({})", parts.join(", "))?;
        }

        Ok(())
    }
}

// Convert unification errors to type errors
impl From<crate::unification::UnificationError> for TypeError {
    fn from(err: crate::unification::UnificationError) -> Self {
        use crate::unification::UnificationError;

        match err {
            UnificationError::ArityMismatch {
                expected,
                found,
                base_type,
            } => {
                let span = miette::SourceSpan::new(0.into(), 0);
                TypeError::InternalError {
                    span,
                    message: format!(
                        "Arity mismatch for type {:?}: expected {}, found {}",
                        base_type, expected, found
                    ),
                }
            }
            UnificationError::ParameterNameMismatch { expected, found } => {
                let span = miette::SourceSpan::new(0.into(), 0);
                TypeError::InternalError {
                    span,
                    message: format!(
                        "Parameter name mismatch: expected {:?}, found {:?}",
                        expected, found
                    ),
                }
            }
            UnificationError::TraitNotImplemented { trait_id, type_id } => {
                let span = miette::SourceSpan::new(0.into(), 0);
                TypeError::TraitNotImplemented {
                    span,
                    trait_name: format!("{:?}", trait_id), // TODO: Use proper name lookup
                    type_name: format!("{:?}", type_id),   // TODO: Use proper name lookup
                }
            }
            UnificationError::UnboundTypeVariable { name } => {
                let span = miette::SourceSpan::new(0.into(), 0);
                TypeError::InternalError {
                    span,
                    message: format!("Unbound type variable: {}", name),
                }
            }
        }
    }
}
