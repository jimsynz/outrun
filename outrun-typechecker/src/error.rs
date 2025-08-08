//! Error types for Outrun Typechecker v3
//!
//! Following existing miette patterns from the parser for consistent error reporting.

use crate::types::{Type, TypeVarId};
use miette::{Diagnostic, SourceSpan};
use outrun_parser::{ParseError, Span};
use thiserror::Error;

/// File-relative span information for clean error reporting
#[derive(Debug, Clone, PartialEq)]
pub struct FileSpan {
    pub span: Span,          // File-relative span from parser
    pub source_file: String, // Which file this span belongs to
}

impl FileSpan {
    pub fn new(span: Span, source_file: String) -> Self {
        Self { span, source_file }
    }

    /// Create a FileSpan from parser AST with source file information
    pub fn from_debug_info(span: Span, debug_info: &outrun_parser::DebugInfo) -> Option<Self> {
        debug_info
            .source_file
            .as_ref()
            .map(|file| Self::new(span, file.clone()))
    }

    /// Convert to miette SourceSpan (file-relative, no global conversion needed)
    pub fn to_source_span(&self) -> SourceSpan {
        SourceSpan::new(self.span.start.into(), self.span.end - self.span.start)
    }

    /// Get the filename without the full path for display
    pub fn filename(&self) -> &str {
        std::path::Path::new(&self.source_file)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
    }
}

/// Type system error for module registration and type operations
#[derive(Error, Diagnostic, Debug)]
pub enum TypeError {
    #[error("Module '{module_name}' is already defined")]
    #[diagnostic(code(outrun::types::module_redefinition))]
    ModuleRedefinition {
        module_name: String,
        #[label("module redefined here")]
        span: Option<SourceSpan>,
    },

    #[error("Module '{module_name}' has conflicting definitions between packages '{existing_package}' and '{conflicting_package}'")]
    #[diagnostic(code(outrun::types::module_conflict))]
    ModuleConflict {
        module_name: String,
        existing_package: String,
        conflicting_package: String,
        #[label("conflicting definition here")]
        span: Option<SourceSpan>,
    },

    #[error("Type '{type_name}' has conflicting generic arity: expected {expected_arity}, found {found_arity}")]
    #[diagnostic(code(outrun::types::arity_conflict))]
    ArityConflict {
        type_name: String,
        expected_arity: usize,
        found_arity: usize,
        #[label("conflicting arity here")]
        span: SourceSpan,
    },
}

/// Main typechecker error type extending the existing parser error system
#[derive(Error, Diagnostic, Debug)]
pub enum TypecheckError {
    #[error("Type system error")]
    #[diagnostic(code(outrun::typecheck::type_error))]
    TypeError(#[from] TypeError),

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

    #[error("Protocol dispatch failed")]
    #[diagnostic(code(outrun::typecheck::dispatch_failed))]
    DispatchError(#[from] DispatchError),

    #[error("Core library error: {0}")]
    #[diagnostic(code(outrun::typecheck::core_library_error))]
    CoreLibraryError(String),

    #[error("Generic typechecker error: {message}")]
    #[diagnostic(code(outrun::typecheck::generic))]
    Generic {
        message: String,
        #[label("error occurred here")]
        span: Option<SourceSpan>,
    },
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
        expected_self: Box<Type>,
        found_self: Box<Type>,
        #[label("Self should be {expected_self}")]
        span: Option<SourceSpan>,
    },

    #[error("Cannot implement marker protocol {protocol_name}: marker protocols cannot be implemented by any type")]
    #[diagnostic(
        code(outrun::typecheck::implementation::marker_protocol),
        help("Marker protocols like 'Panic' are special and cannot be implemented by user types")
    )]
    MarkerProtocolImplementation {
        protocol_name: String,
        type_name: String,
        #[label("cannot implement marker protocol {protocol_name}")]
        span: Option<SourceSpan>,
    },

    #[error("Conflicting protocol definition: protocol {protocol_name} is already defined with different requirements")]
    #[diagnostic(
        code(outrun::typecheck::implementation::conflicting_protocol_definition),
        help("Protocol definitions must be identical across all packages. Check that the protocol requirements and function signatures match.")
    )]
    ConflictingProtocolDefinition {
        protocol_name: String,
        #[label("conflicting protocol definition")]
        span: Option<SourceSpan>,
        #[label("previous protocol definition")]
        previous_span: Option<SourceSpan>,
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
        #[label("undefined variable '{variable_name}'")]
        span: Option<SourceSpan>,
        similar_names: Vec<String>,
        context: Option<String>,
    },

    #[error("Undefined type: {type_name}")]
    #[diagnostic(
        code(outrun::typecheck::inference::undefined_type),
        help("Import the type or define it in this module")
    )]
    UndefinedType {
        type_name: String,
        #[label("undefined type '{type_name}'")]
        span: Option<SourceSpan>,
        similar_names: Vec<String>,
        context: Option<String>,
    },

    #[error("Undefined protocol: {protocol_name}")]
    #[diagnostic(
        code(outrun::typecheck::inference::undefined_protocol),
        help("Import the protocol or define it in this module")
    )]
    UndefinedProtocol {
        protocol_name: String,
        #[label("undefined protocol '{protocol_name}'")]
        span: Option<SourceSpan>,
        similar_names: Vec<String>,
        context: Option<String>,
    },

    #[error("Type annotation required: cannot infer type of empty collection")]
    #[diagnostic(
        code(outrun::typecheck::inference::empty_collection),
        help("Add a type annotation like: let my_list: List<T> = []")
    )]
    EmptyCollectionNeedsAnnotation {
        collection_type: String,
        #[label("empty {collection_type} needs type annotation")]
        span: Option<SourceSpan>,
        examples: Vec<String>,
    },

    #[error("Function call error: {message}")]
    #[diagnostic(code(outrun::typecheck::inference::function_call_error))]
    FunctionCallError {
        message: String,
        function_name: Option<String>,
        expected_signature: Option<String>,
        actual_arguments: Option<String>,
        #[label("{message}")]
        span: Option<SourceSpan>,
        suggestions: Vec<String>,
    },

    #[error("Collection type mismatch: {message}")]
    #[diagnostic(code(outrun::typecheck::inference::collection_mismatch))]
    CollectionMismatch {
        message: String,
        collection_type: String,
        expected_element_type: Option<Type>,
        found_element_type: Option<Type>,
        #[label("inconsistent {collection_type} element types")]
        span: Option<SourceSpan>,
        #[label("expected {expected_element_type:?}")]
        expected_span: Option<SourceSpan>,
        #[label("found {found_element_type:?}")]
        found_span: Option<SourceSpan>,
    },

    #[error("Self type unification failure: conflicting Self types {first_self_type} and {conflicting_self_type}")]
    #[diagnostic(
        code(outrun::typecheck::inference::self_unification_failure),
        help("All occurrences of Self in a function signature must resolve to the same concrete type")
    )]
    SelfTypeUnificationFailure {
        first_self_type: Type,
        conflicting_self_type: Type,
        #[label("Self types must be consistent across function signature")]
        span: Option<SourceSpan>,
    },

    #[error("Type variable unification failure: variable {variable_name} has conflicting types {first_type} and {conflicting_type}")]
    #[diagnostic(
        code(outrun::typecheck::inference::type_variable_unification_failure),
        help("All occurrences of the same type variable must unify to the same concrete type")
    )]
    TypeVariableUnificationFailure {
        variable_name: String,
        first_type: Type,
        conflicting_type: Type,
        #[label("type variable {variable_name} has conflicting types")]
        span: Option<SourceSpan>,
    },

    #[error(
        "Invalid constraint variable: {variable_name} does not appear in impl type specifications"
    )]
    #[diagnostic(
        code(outrun::typecheck::inference::invalid_constraint_variable),
        help("Constrained type variables must appear in either the protocol or implementing type specifications")
    )]
    InvalidConstraintVariable {
        variable_name: String,
        available_variables: String,
        #[label("variable '{variable_name}' is constrained but not defined in impl types")]
        span: Option<SourceSpan>,
        suggestions: Vec<String>,
    },
}

/// Unified compiler error combining parser and typechecker errors
#[derive(Error, Diagnostic, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error(transparent)]
    Typecheck(Box<TypecheckError>),

    #[error(
        "Module redefinition: module '{module_name}' is already defined by a dependency package"
    )]
    #[diagnostic(
        code(outrun::compiler::module_redefinition),
        help("Module names must be unique across all packages. Consider renaming this module or using a different package structure.")
    )]
    ModuleRedefinition {
        module_name: String,
        #[label("module redefined here")]
        span: Option<SourceSpan>,
    },
}

impl From<TypecheckError> for CompilerError {
    fn from(err: TypecheckError) -> Self {
        CompilerError::Typecheck(Box::new(err))
    }
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

    /// Create enhanced type mismatch error with helpful suggestions
    pub fn type_mismatch_with_suggestions(
        expected: Type,
        found: Type,
        span: Option<Span>,
        context: &str,
    ) -> Self {
        let suggestions = generate_type_conversion_suggestions(&expected, &found);
        let expected_context = Some(context.to_string());
        let found_context = if !suggestions.is_empty() {
            Some(format!("Try: {}", suggestions.join(" or ")))
        } else {
            None
        };

        Self::TypeMismatch {
            expected,
            found,
            expected_context,
            found_context,
            span: to_source_span(span),
        }
    }
}

/// Protocol dispatch errors
#[derive(Error, Diagnostic, Debug)]
pub enum DispatchError {
    #[error(
        "No implementation found: type {type_name} does not implement protocol {protocol_name}"
    )]
    #[diagnostic(
        code(outrun::typecheck::dispatch::no_implementation),
        help("Add an implementation block: impl {protocol_name} for {type_name}")
    )]
    NoImplementation {
        protocol_name: String,
        type_name: String,
        file_span: Option<FileSpan>,
        similar_implementations: Vec<String>,
        suggestions: Vec<String>,
    },

    #[error("Ambiguous dispatch: multiple implementations found for {protocol_name}")]
    #[diagnostic(
        code(outrun::typecheck::dispatch::ambiguous),
        help("Add type annotations to disambiguate the call")
    )]
    AmbiguousDispatch {
        protocol_name: String,
        candidates: Vec<String>,
        #[label("ambiguous protocol call")]
        span: Option<SourceSpan>,
    },

    #[error(
        "Unresolved type variable: cannot dispatch on unknown type for protocol {protocol_name}"
    )]
    #[diagnostic(
        code(outrun::typecheck::dispatch::unresolved_variable),
        help("Type inference could not determine the concrete type")
    )]
    UnresolvedTypeVariable {
        protocol_name: String,
        #[label("type not resolved")]
        span: Option<SourceSpan>,
    },

    #[error("Unbound Self type: cannot dispatch on unresolved Self for protocol {protocol_name}")]
    #[diagnostic(
        code(outrun::typecheck::dispatch::unbound_self),
        help("Self type cannot be resolved in this context")
    )]
    UnboundSelfType {
        protocol_name: String,
        #[label("Self type not bound")]
        span: Option<SourceSpan>,
    },

    #[error("Invalid dispatch target: {protocol_name} cannot be called on {target_description}")]
    #[diagnostic(
        code(outrun::typecheck::dispatch::invalid_target),
        help("Check that the protocol call is valid for this type")
    )]
    InvalidTarget {
        protocol_name: String,
        target_description: String,
        #[label("invalid dispatch target")]
        span: Option<SourceSpan>,
    },
}

/// Helper for creating source spans from optional spans  
pub fn to_source_span(span: Option<Span>) -> Option<SourceSpan> {
    span.map(|s| SourceSpan::new(s.start.into(), s.end - s.start))
}

/// Helper for creating FileSpan from optional spans with unknown file
pub fn to_file_span(span: Option<Span>) -> Option<FileSpan> {
    span.map(|s| FileSpan::new(s, "unknown".to_string()))
}

/// Enhanced error information that includes source file context
#[derive(Debug, Clone)]
pub struct EnhancedErrorInfo {
    pub source_span: Option<SourceSpan>,
    pub source_file: Option<String>,
    pub message: String,
}

impl EnhancedErrorInfo {
    pub fn new(message: String, span: Option<Span>, source_file: Option<String>) -> Self {
        Self {
            source_span: to_source_span(span),
            source_file,
            message,
        }
    }
}

/// Error context builder for enhanced error reporting
pub struct ErrorContext {
    pub available_variables: Vec<String>,
    pub available_types: Vec<String>,
    pub available_protocols: Vec<String>,
    pub current_function: Option<String>,
    pub current_module: Option<String>,
}

impl ErrorContext {
    pub fn new() -> Self {
        Self {
            available_variables: Vec::new(),
            available_types: Vec::new(),
            available_protocols: Vec::new(),
            current_function: None,
            current_module: None,
        }
    }

    /// Find similar names using edit distance
    pub fn find_similar_names(&self, target: &str, candidates: &[String]) -> Vec<String> {
        let mut similar = candidates
            .iter()
            .filter_map(|name| {
                let distance = levenshtein_distance(target, name);
                // Consider similar if distance <= 2 and length difference <= 3
                if distance <= 2 && target.len().abs_diff(name.len()) <= 3 {
                    Some((name.clone(), distance))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        // Sort by distance (closest first)
        similar.sort_by_key(|(_, distance)| *distance);
        similar.into_iter().map(|(name, _)| name).take(3).collect()
    }

    /// Generate context-aware suggestions for undefined variables
    pub fn suggest_for_undefined_variable(
        &self,
        variable_name: &str,
    ) -> (Vec<String>, Option<String>) {
        let similar = self.find_similar_names(variable_name, &self.available_variables);
        let context = if similar.is_empty() {
            Some("Make sure the variable is defined before use".to_string())
        } else {
            Some(format!("Did you mean one of: {}", similar.join(", ")))
        };
        (similar, context)
    }

    /// Generate context-aware suggestions for undefined types
    pub fn suggest_for_undefined_type(&self, type_name: &str) -> (Vec<String>, Option<String>) {
        let similar = self.find_similar_names(type_name, &self.available_types);
        let context = if similar.is_empty() {
            Some("Import the type or check for typos".to_string())
        } else {
            Some(format!("Did you mean one of: {}", similar.join(", ")))
        };
        (similar, context)
    }

    /// Generate context-aware suggestions for undefined protocols
    pub fn suggest_for_undefined_protocol(
        &self,
        protocol_name: &str,
    ) -> (Vec<String>, Option<String>) {
        let similar = self.find_similar_names(protocol_name, &self.available_protocols);
        let context = if similar.is_empty() {
            Some("Import the protocol or check for typos".to_string())
        } else {
            Some(format!("Did you mean one of: {}", similar.join(", ")))
        };
        (similar, context)
    }
}

impl Default for ErrorContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Simple Levenshtein distance calculation for finding similar names
fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let a_len = a_chars.len();
    let b_len = b_chars.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut matrix = vec![vec![0; b_len + 1]; a_len + 1];

    // Initialize first row and column
    for i in 0..=a_len {
        matrix[i][0] = i;
    }
    for j in 0..=b_len {
        matrix[0][j] = j;
    }

    for i in 1..=a_len {
        for j in 1..=b_len {
            let cost = if a_chars[i - 1] == b_chars[j - 1] {
                0
            } else {
                1
            };
            matrix[i][j] = (matrix[i - 1][j] + 1)
                .min(matrix[i][j - 1] + 1)
                .min(matrix[i - 1][j - 1] + cost);
        }
    }

    matrix[a_len][b_len]
}

/// Generate helpful suggestions for type conversion
fn generate_type_conversion_suggestions(expected: &Type, found: &Type) -> Vec<String> {
    let mut suggestions = Vec::new();

    match (expected, found) {
        // String to Integer conversion
        (Type::Concrete { name: exp, .. }, Type::Concrete { name: fnd, .. })
            if exp.as_str() == "Integer64" && fnd.as_str() == "String" =>
        {
            suggestions.push("String.to_integer(value)".to_string());
        }

        // Integer to String conversion
        (Type::Concrete { name: exp, .. }, Type::Concrete { name: fnd, .. })
            if exp.as_str() == "String" && fnd.as_str() == "Integer64" =>
        {
            suggestions.push("Integer64.to_string(value)".to_string());
        }

        // Float to Integer conversion
        (Type::Concrete { name: exp, .. }, Type::Concrete { name: fnd, .. })
            if exp.as_str() == "Integer64" && fnd.as_str() == "Float64" =>
        {
            suggestions.push("Float64.to_integer(value)".to_string());
        }

        // Integer to Float conversion
        (Type::Concrete { name: exp, .. }, Type::Concrete { name: fnd, .. })
            if exp.as_str() == "Float64" && fnd.as_str() == "Integer64" =>
        {
            suggestions.push("Integer64.to_float(value)".to_string());
        }

        // Boolean to String conversion
        (Type::Concrete { name: exp, .. }, Type::Concrete { name: fnd, .. })
            if exp.as_str() == "String" && fnd.as_str() == "Boolean" =>
        {
            suggestions.push("Boolean.to_string(value)".to_string());
        }

        // Collection element type suggestions
        (
            Type::Concrete {
                name: exp,
                args: exp_args,
                ..
            },
            Type::Concrete {
                name: fnd,
                args: fnd_args,
                ..
            },
        ) if exp == fnd && exp_args.len() == 1 && fnd_args.len() == 1 => match exp.as_str() {
            "List" => {
                suggestions.push("convert list elements to expected type".to_string());
            }
            "Map" => {
                suggestions.push("ensure map values have consistent type".to_string());
            }
            _ => {}
        },

        _ => {
            // Generic suggestion for any type mismatch
            suggestions.push("add explicit type conversion".to_string());
        }
    }

    suggestions
}

/// Enhanced error creation utilities
impl InferenceError {
    pub fn undefined_variable_with_suggestions(
        variable_name: String,
        span: Option<Span>,
        context: &ErrorContext,
    ) -> Self {
        let (similar_names, ctx) = context.suggest_for_undefined_variable(&variable_name);
        Self::UndefinedVariable {
            variable_name,
            span: to_source_span(span),
            similar_names,
            context: ctx,
        }
    }

    pub fn undefined_type_with_suggestions(
        type_name: String,
        span: Option<Span>,
        context: &ErrorContext,
    ) -> Self {
        let (similar_names, ctx) = context.suggest_for_undefined_type(&type_name);
        Self::UndefinedType {
            type_name,
            span: to_source_span(span),
            similar_names,
            context: ctx,
        }
    }

    pub fn undefined_protocol_with_suggestions(
        protocol_name: String,
        span: Option<Span>,
        context: &ErrorContext,
    ) -> Self {
        let (similar_names, ctx) = context.suggest_for_undefined_protocol(&protocol_name);
        Self::UndefinedProtocol {
            protocol_name,
            span: to_source_span(span),
            similar_names,
            context: ctx,
        }
    }

    pub fn empty_collection_needs_annotation(collection_type: String, span: Option<Span>) -> Self {
        let examples = match collection_type.as_str() {
            "List" => vec![
                "let numbers: List<Integer64> = []".to_string(),
                "let names: List<String> = []".to_string(),
            ],
            "Map" => vec![
                "let scores: Map<String, Integer64> = {}".to_string(),
                "let config: Map<String, Boolean> = {}".to_string(),
            ],
            "Tuple" => vec!["let empty: Tuple<Integer64, String> = (0, \"\")".to_string()],
            _ => vec![format!("let value: {}<T> = empty_literal", collection_type)],
        };

        Self::EmptyCollectionNeedsAnnotation {
            collection_type,
            span: to_source_span(span),
            examples,
        }
    }

    pub fn collection_type_mismatch(
        message: String,
        collection_type: String,
        expected_element_type: Option<Type>,
        found_element_type: Option<Type>,
        span: Option<Span>,
        expected_span: Option<Span>,
        found_span: Option<Span>,
    ) -> Self {
        Self::CollectionMismatch {
            message,
            collection_type,
            expected_element_type,
            found_element_type,
            span: to_source_span(span),
            expected_span: to_source_span(expected_span),
            found_span: to_source_span(found_span),
        }
    }

    pub fn function_call_error_with_suggestions(
        message: String,
        function_name: Option<String>,
        expected_signature: Option<String>,
        actual_arguments: Option<String>,
        span: Option<Span>,
        suggestions: Vec<String>,
    ) -> Self {
        Self::FunctionCallError {
            message,
            function_name,
            expected_signature,
            actual_arguments,
            span: to_source_span(span),
            suggestions,
        }
    }
}

/// Enhanced error creation utilities for dispatch errors
impl DispatchError {
    /// Create a NoImplementation error with helpful suggestions
    pub fn no_implementation_with_suggestions(
        protocol_name: String,
        type_name: String,
        span: Option<Span>,
        similar_implementations: Vec<String>,
    ) -> Self {
        let mut suggestions = vec![format!("impl {} for {}", protocol_name, type_name)];

        // Add suggestions based on common patterns
        match protocol_name.as_str() {
            "BinaryAddition" => {
                suggestions.push("Define how to add two instances of this type".to_string());
            }
            "Equality" => {
                suggestions.push("Define how to compare instances for equality".to_string());
            }
            "ToString" => {
                suggestions.push("Define how to convert this type to a String".to_string());
            }
            _ => {}
        }

        if !similar_implementations.is_empty() {
            suggestions.push(format!(
                "Similar implementations exist for: {}",
                similar_implementations.join(", ")
            ));
        }

        Self::NoImplementation {
            protocol_name,
            type_name,
            file_span: span.map(|s| FileSpan::new(s, "unknown".to_string())),
            similar_implementations,
            suggestions,
        }
    }
}
