//! Error types for Outrun Typechecker v3
//!
//! Following existing miette patterns from the parser for consistent error reporting.

use crate::types::{Type, TypeVarId};
use miette::{Diagnostic, SourceSpan};
use outrun_parser::{ParseError, Span};
use thiserror::Error;

/// Main typechecker error type extending the existing parser error system
#[derive(Error, Diagnostic, Debug)]
#[allow(clippy::result_large_err)]
pub enum TypecheckError {
    #[error("Type unification failed")]
    #[diagnostic(code(outrun::typecheck::unification_failed))]
    UnificationError(#[from] UnificationError),

    #[error("Constraint solving failed")]
    #[diagnostic(code(outrun::typecheck::constraint_failed))]
    ConstraintError(#[from] ConstraintError),

    #[error("Protocol implementation error")]
    #[diagnostic(code(outrun::typecheck::implementation_error))]
    ImplementationError(#[from] ImplementationError),

    #[error("Exhaustiveness check failed")]
    #[diagnostic(code(outrun::typecheck::exhaustiveness_failed))]
    ExhaustivenessError(#[from] ExhaustivenessError),

    #[error("Type inference failed")]
    #[diagnostic(code(outrun::typecheck::inference_failed))]
    InferenceError(#[from] InferenceError),
}

/// Unification errors following miette patterns
#[derive(Error, Diagnostic, Debug)]
pub enum UnificationError {
    #[error("Type mismatch: expected {expected}, found {found}")]
    #[diagnostic(
        code(outrun::typecheck::unification::type_mismatch),
        help("The types {expected} and {found} cannot be unified")
    )]
    TypeMismatch {
        expected: Type,
        found: Type,
        expected_context: Option<String>,
        found_context: Option<String>,
        #[label("expected type {expected}")]
        span: Option<SourceSpan>,
    },

    #[error("Occurs check violation: variable {var_name} occurs in {containing_type}")]
    #[diagnostic(
        code(outrun::typecheck::unification::occurs_check),
        help("This would create an infinite type. Consider using a recursive type definition.")
    )]
    OccursCheckViolation {
        var_id: TypeVarId,
        var_name: String,
        containing_type: Type,
        #[label("variable {var_name} would create infinite type")]
        span: Option<SourceSpan>,
    },

    #[error(
        "Arity mismatch: {type_name} expects {expected_arity} type arguments, found {found_arity}"
    )]
    #[diagnostic(
        code(outrun::typecheck::unification::arity_mismatch),
        help("Ensure all generic type arguments are provided correctly")
    )]
    ArityMismatch {
        type_name: String,
        expected_arity: usize,
        found_arity: usize,
        #[label("expected {expected_arity} arguments, found {found_arity}")]
        span: Option<SourceSpan>,
    },

    #[error(
        "Function arity mismatch: expected {expected_params} parameters, found {found_params}"
    )]
    #[diagnostic(
        code(outrun::typecheck::unification::function_arity),
        help("Function types must have the same number of parameters to unify")
    )]
    FunctionArityMismatch {
        expected_params: usize,
        found_params: usize,
        #[label("function type has {found_params} parameters")]
        span: Option<SourceSpan>,
    },

    #[error("Protocol mismatch: expected {expected}, found {found}")]
    #[diagnostic(
        code(outrun::typecheck::unification::protocol_mismatch),
        help("Different protocols cannot be unified")
    )]
    ProtocolMismatch {
        expected: String,
        found: String,
        #[label("found protocol {found}")]
        span: Option<SourceSpan>,
    },

    #[error("Category mismatch: cannot unify {expected} with {found}")]
    #[diagnostic(
        code(outrun::typecheck::unification::category_mismatch),
        help(
            "Cannot unify different categories of types (e.g., concrete type with function type)"
        )
    )]
    CategoryMismatch {
        expected: String,
        found: String,
        expected_type: Type,
        found_type: Type,
        #[label("found {found}")]
        span: Option<SourceSpan>,
    },
}

/// Constraint solving errors
#[derive(Error, Diagnostic, Debug)]
pub enum ConstraintError {
    #[error("Unsatisfiable constraint: {constraint}")]
    #[diagnostic(
        code(outrun::typecheck::constraint::unsatisfiable),
        help("No type can satisfy this constraint")
    )]
    Unsatisfiable {
        constraint: String,
        #[label("unsatisfiable constraint")]
        span: Option<SourceSpan>,
    },

    #[error("Conflicting constraints: {constraint1} conflicts with {constraint2}")]
    #[diagnostic(
        code(outrun::typecheck::constraint::conflict),
        help("These constraints cannot both be satisfied by the same type")
    )]
    ConflictingConstraints {
        constraint1: String,
        constraint2: String,
        #[label("first constraint")]
        span1: Option<SourceSpan>,
        #[label("conflicting constraint")]
        span2: Option<SourceSpan>,
    },

    #[error(
        "Missing protocol implementation: type {type_name} does not implement {protocol_name}"
    )]
    #[diagnostic(
        code(outrun::typecheck::constraint::missing_implementation),
        help("Add an implementation block: impl {protocol_name} for {type_name}")
    )]
    MissingImplementation {
        type_name: String,
        protocol_name: String,
        #[label("type {type_name} must implement {protocol_name}")]
        span: Option<SourceSpan>,
    },
}

/// Implementation errors (orphan rules, conflicts, etc.)
#[derive(Error, Diagnostic, Debug)]
pub enum ImplementationError {
    #[error("Orphan rule violation: cannot implement foreign protocol {protocol_name} for foreign type {type_name}")]
    #[diagnostic(
        code(outrun::typecheck::implementation::orphan_violation),
        help("At least one of the protocol or type must be defined in this module")
    )]
    OrphanRuleViolation {
        protocol_name: String,
        type_name: String,
        #[label("both protocol and type are foreign")]
        span: Option<SourceSpan>,
    },

    #[error("Conflicting implementation: {protocol_name} is already implemented for {type_name}")]
    #[diagnostic(
        code(outrun::typecheck::implementation::conflicting),
        help("Each protocol can only be implemented once per type")
    )]
    ConflictingImplementation {
        protocol_name: String,
        type_name: String,
        #[label("conflicting implementation")]
        span: Option<SourceSpan>,
        #[label("previous implementation")]
        previous_span: Option<SourceSpan>,
    },

    #[error("Self type mismatch in implementation")]
    #[diagnostic(
        code(outrun::typecheck::implementation::self_mismatch),
        help("Self must refer to the implementing type")
    )]
    SelfTypeMismatch {
        expected_self: Type,
        found_self: Type,
        #[label("Self should be {expected_self}")]
        span: Option<SourceSpan>,
    },
}

/// Exhaustiveness checking errors
#[derive(Error, Diagnostic, Debug)]
pub enum ExhaustivenessError {
    #[error("Non-exhaustive pattern: missing pattern for {missing_pattern}")]
    #[diagnostic(
        code(outrun::typecheck::exhaustive::missing_pattern),
        help("Add a pattern to handle {missing_pattern}")
    )]
    MissingPattern {
        missing_pattern: String,
        #[label("case expression is not exhaustive")]
        span: Option<SourceSpan>,
    },

    #[error("Unreachable pattern: this pattern will never match")]
    #[diagnostic(
        code(outrun::typecheck::exhaustive::unreachable),
        help("Consider removing this pattern or reordering the cases")
    )]
    UnreachablePattern {
        #[label("unreachable pattern")]
        span: Option<SourceSpan>,
    },

    #[error("Non-exhaustive function: guards do not cover all possible inputs")]
    #[diagnostic(
        code(outrun::typecheck::exhaustive::incomplete_guards),
        help("Add additional guard clauses or a catch-all case")
    )]
    IncompleteGuards {
        #[label("function may not handle all inputs")]
        span: Option<SourceSpan>,
    },
}

/// Type inference errors
#[derive(Error, Diagnostic, Debug)]
pub enum InferenceError {
    #[error("Cannot infer type: ambiguous expression")]
    #[diagnostic(
        code(outrun::typecheck::inference::ambiguous),
        help("Add a type annotation to resolve ambiguity")
    )]
    AmbiguousType {
        #[label("type cannot be inferred")]
        span: Option<SourceSpan>,
        suggestions: Vec<String>,
    },

    #[error("Undefined variable: {variable_name}")]
    #[diagnostic(
        code(outrun::typecheck::inference::undefined_variable),
        help("Ensure the variable is defined before use")
    )]
    UndefinedVariable {
        variable_name: String,
        #[label("undefined variable")]
        span: Option<SourceSpan>,
    },

    #[error("Undefined type: {type_name}")]
    #[diagnostic(
        code(outrun::typecheck::inference::undefined_type),
        help("Import the type or define it in this module")
    )]
    UndefinedType {
        type_name: String,
        #[label("undefined type")]
        span: Option<SourceSpan>,
    },

    #[error("Undefined protocol: {protocol_name}")]
    #[diagnostic(
        code(outrun::typecheck::inference::undefined_protocol),
        help("Import the protocol or define it in this module")
    )]
    UndefinedProtocol {
        protocol_name: String,
        #[label("undefined protocol")]
        span: Option<SourceSpan>,
    },
}

/// Unified compiler error combining parser and typechecker errors
#[derive(Error, Diagnostic, Debug)]
#[allow(clippy::result_large_err)]
#[allow(clippy::large_enum_variant)]
pub enum CompilerError {
    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error(transparent)]
    Typecheck(#[from] TypecheckError),
}

/// Utility functions for creating errors with source context
impl UnificationError {
    /// Create a type mismatch error with source context
    pub fn type_mismatch_with_context(
        expected: Type,
        found: Type,
        expected_context: Option<String>,
        found_context: Option<String>,
        span: Option<Span>,
    ) -> Self {
        Self::TypeMismatch {
            expected,
            found,
            expected_context,
            found_context,
            span: to_source_span(span),
        }
    }
}

/// Helper for creating source spans from optional spans  
pub fn to_source_span(span: Option<Span>) -> Option<SourceSpan> {
    span.map(|s| SourceSpan::new(s.start.into(), s.end - s.start))
}
