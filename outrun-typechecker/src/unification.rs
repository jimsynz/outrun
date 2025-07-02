//! Type unification algorithm for Outrun
//!
//! This module implements the comprehensive type unification algorithm described
//! in the typechecker CLAUDE.md. It handles recursive unification of generic types,
//! trait-based compatibility, and Self type resolution.

use crate::compilation::compiler_environment::{
    AtomId as CompilerAtomId, CompilerEnvironment, TypeNameId,
};
use crate::smt::constraints::SMTConstraint;
use std::collections::HashMap;

// Forward declaration for TypeContextSummary (defined in program_collection.rs)
pub use crate::compilation::program_collection::TypeContextSummary;

/// Result type for unification operations
pub type UnificationResult<T = ()> = Result<T, UnificationError>;

/// Errors that can occur during type unification
#[derive(Debug, Clone, PartialEq)]
pub enum UnificationError {
    /// Generic arity mismatch (e.g., Option<T> vs Map<K, V>)
    ArityMismatch {
        expected: usize,
        found: usize,
        base_type: TypeNameId,
    },
    /// Parameter name mismatch in function types
    ParameterNameMismatch {
        expected: CompilerAtomId,
        found: CompilerAtomId,
    },
    /// Trait implementation missing
    TraitNotImplemented {
        trait_id: TypeNameId,
        type_id: TypeNameId,
    },
    /// Unbound type variable or Self in invalid context
    UnboundTypeVariable { name: String },
}

/// Unified type system for Outrun
///
/// This replaces ConcreteType and CollectionType with a single
/// comprehensive type representation that handles both compilation-time and runtime types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StructuredType {
    /// Simple type without generics (e.g., "String", "Boolean", "Integer64")
    Simple(TypeNameId),

    /// Type variable for generic parameters and Self references
    /// Used for constraining both generic parameters (T, K, V) and Self in trait/impl contexts
    /// Reuses TypeNameId infrastructure for consistent handling
    TypeVariable(TypeNameId),

    /// Generic type with structured arguments (e.g., Option<T>, Map<K, V>)
    Generic {
        base: TypeNameId,
        args: Vec<StructuredType>,
    },

    /// Tuple type with ordered elements
    Tuple(Vec<StructuredType>),

    /// Function type with named parameters and return type
    Function {
        params: Vec<FunctionParam>,
        return_type: Box<StructuredType>,
    },

    /// Concrete primitive types (for runtime representation)
    Integer64,
    Float64,
    Boolean,
    String,
    Atom,

    /// Concrete collection types with element type information
    List {
        element_type: Box<StructuredType>,
    },
    Map {
        key_type: Box<StructuredType>,
        value_type: Box<StructuredType>,
    },

    /// Concrete option and result types
    Option {
        inner_type: Box<StructuredType>,
    },
    Result {
        ok_type: Box<StructuredType>,
        err_type: Box<StructuredType>,
    },

    /// Concrete struct type
    Struct {
        name: TypeNameId,
        fields: Vec<StructFieldDescriptor>,
    },

    /// Concrete trait type
    Trait {
        name: TypeNameId,
        functions: Vec<TraitFunctionDescriptor>,
    },

    /// Error type for recovery - represents a type that failed to resolve
    TypeError {
        /// The original error that caused this type failure
        error: crate::error::TypeError,
        /// Fallback type to use for continued type checking (if any)
        fallback_type: Option<Box<StructuredType>>,
        /// Original span where the type error occurred
        error_span: outrun_parser::Span,
    },
}

/// Field descriptor for struct types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructFieldDescriptor {
    pub name: CompilerAtomId,
    pub field_type: StructuredType,
}

/// Function descriptor for trait types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitFunctionDescriptor {
    pub name: CompilerAtomId,
    pub params: Vec<FunctionParam>,
    pub return_type: StructuredType,
    pub is_guard: bool,
    pub is_static: bool,
    pub has_default_impl: bool,
}

/// Function parameter with name and type (required for Outrun)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionParam {
    pub name: CompilerAtomId,
    pub param_type: StructuredType,
}

/// Type unification context containing necessary information for resolution
///
/// Note: CompilerEnvironment is now passed separately to reduce redundancy
#[derive(Debug, Clone)]
pub struct UnificationContext {
    /// Generic parameter substitutions
    pub generic_substitutions: HashMap<TypeNameId, StructuredType>,
    /// Resolved expression types from type checking phase
    pub expression_types: HashMap<outrun_parser::Span, StructuredType>,
    /// Dispatch strategies computed during type checking for function calls
    pub dispatch_strategies: HashMap<outrun_parser::Span, crate::checker::DispatchMethod>,
    /// Span mapping from desugaring phase for tracking original to desugared spans
    pub span_mapping: crate::desugaring::SpanMapping,

    // =============================================================================
    // SMT Integration Fields (Phase 4)
    // =============================================================================
    /// SMT constraints collected during type checking
    pub smt_constraints: Vec<SMTConstraint>,
    /// Constraint collector for complex constraint scenarios
    pub constraint_collector: Option<ConstraintCollector>,
    /// Cache for constraint solving results
    pub solver_cache: HashMap<ConstraintSetHash, crate::smt::solver::SolverResult>,
}

/// Constraint collector for building complex SMT constraint sets
#[derive(Debug, Clone)]
pub struct ConstraintCollector {
    constraints: Vec<SMTConstraint>,
    scope_stack: Vec<ConstraintScope>,
    deferred_constraints: Vec<DeferredConstraint>,
}

/// Constraint scope for managing constraint lifecycles
#[derive(Debug, Clone)]
pub struct ConstraintScope {
    pub scope_id: usize,
    pub constraint_count: usize,
    pub is_function_scope: bool,
}

/// Deferred constraint for complex resolution scenarios
#[derive(Debug, Clone)]
pub struct DeferredConstraint {
    pub constraint: SMTConstraint,
    pub dependency_types: Vec<TypeNameId>,
    pub resolution_priority: u8,
}

/// Hash for constraint set caching
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstraintSetHash {
    pub content_hash: u64,
    pub constraint_count: usize,
}

impl Default for UnificationContext {
    fn default() -> Self {
        Self::new()
    }
}

impl StructuredType {
    /// Create a simple type from a type name ID
    pub fn simple(type_name_id: TypeNameId) -> Self {
        StructuredType::Simple(type_name_id)
    }

    /// Create a type variable from a type name ID
    pub fn type_variable(type_name_id: TypeNameId) -> Self {
        StructuredType::TypeVariable(type_name_id)
    }

    /// Create a generic type with arguments
    pub fn generic(base: TypeNameId, args: Vec<StructuredType>) -> Self {
        StructuredType::Generic { base, args }
    }

    /// Create a tuple type
    pub fn tuple(elements: Vec<StructuredType>) -> Self {
        StructuredType::Tuple(elements)
    }

    /// Create a function type
    pub fn function(params: Vec<FunctionParam>, return_type: StructuredType) -> Self {
        StructuredType::Function {
            params,
            return_type: Box::new(return_type),
        }
    }

    /// Create an error type for recovery
    pub fn type_error(
        error: crate::error::TypeError,
        fallback_type: Option<StructuredType>,
        error_span: outrun_parser::Span,
    ) -> Self {
        StructuredType::TypeError {
            error,
            fallback_type: fallback_type.map(Box::new),
            error_span,
        }
    }

    // =============================================================================
    // Concrete type constructors (replacing ConcreteType functionality)
    // =============================================================================

    /// Create a list type with element type
    pub fn list(element_type: StructuredType) -> Self {
        StructuredType::List {
            element_type: Box::new(element_type),
        }
    }

    /// Create a map type with key and value types
    pub fn map(key_type: StructuredType, value_type: StructuredType) -> Self {
        StructuredType::Map {
            key_type: Box::new(key_type),
            value_type: Box::new(value_type),
        }
    }

    /// Create an option type with inner type
    pub fn option(inner_type: StructuredType) -> Self {
        StructuredType::Option {
            inner_type: Box::new(inner_type),
        }
    }

    /// Create a result type with ok and error types
    pub fn result(ok_type: StructuredType, err_type: StructuredType) -> Self {
        StructuredType::Result {
            ok_type: Box::new(ok_type),
            err_type: Box::new(err_type),
        }
    }

    /// Create a struct type
    pub fn struct_type(name: TypeNameId, fields: Vec<StructFieldDescriptor>) -> Self {
        StructuredType::Struct { name, fields }
    }

    /// Create a trait type
    pub fn trait_type(name: TypeNameId, functions: Vec<TraitFunctionDescriptor>) -> Self {
        StructuredType::Trait { name, functions }
    }

    // =============================================================================
    // Type classification methods (replacing ConcreteType functionality)
    // =============================================================================

    /// Check if this type is a primitive type
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            StructuredType::Integer64
                | StructuredType::Float64
                | StructuredType::Boolean
                | StructuredType::String
                | StructuredType::Atom
        )
    }

    /// Check if this type is a collection type
    pub fn is_collection(&self) -> bool {
        matches!(
            self,
            StructuredType::List { .. } | StructuredType::Tuple(_) | StructuredType::Map { .. }
        )
    }

    /// Check if this type is a generic container
    pub fn is_generic_container(&self) -> bool {
        matches!(
            self,
            StructuredType::Generic { .. }
                | StructuredType::List { .. }
                | StructuredType::Map { .. }
                | StructuredType::Option { .. }
                | StructuredType::Result { .. }
        )
    }

    /// Get the inner type for Option types
    pub fn option_inner_type(&self) -> Option<&StructuredType> {
        match self {
            StructuredType::Option { inner_type } => Some(inner_type),
            _ => None,
        }
    }

    /// Get the ok and error types for Result types
    pub fn result_types(&self) -> Option<(&StructuredType, &StructuredType)> {
        match self {
            StructuredType::Result { ok_type, err_type } => Some((ok_type, err_type)),
            _ => None,
        }
    }

    /// Get element type for List
    pub fn list_element_type(&self) -> Option<&StructuredType> {
        match self {
            StructuredType::List { element_type } => Some(element_type),
            _ => None,
        }
    }

    /// Get element types for Tuple
    pub fn tuple_element_types(&self) -> Option<&[StructuredType]> {
        match self {
            StructuredType::Tuple(elements) => Some(elements),
            _ => None,
        }
    }

    /// Get key and value types for Map
    pub fn map_types(&self) -> Option<(&StructuredType, &StructuredType)> {
        match self {
            StructuredType::Map {
                key_type,
                value_type,
            } => Some((key_type, value_type)),
            _ => None,
        }
    }

    /// Get all type dependencies for this type (useful for dependency analysis)
    pub fn type_dependencies(&self) -> Vec<TypeNameId> {
        let mut deps = Vec::new();
        self.collect_type_dependencies(&mut deps);
        deps
    }

    /// Recursively collect type dependencies
    fn collect_type_dependencies(&self, deps: &mut Vec<TypeNameId>) {
        match self {
            StructuredType::Simple(type_id) => {
                if !deps.contains(type_id) {
                    deps.push(type_id.clone());
                }
            }
            StructuredType::TypeVariable(type_id) => {
                if !deps.contains(type_id) {
                    deps.push(type_id.clone());
                }
            }
            StructuredType::Generic { base, args } => {
                if !deps.contains(base) {
                    deps.push(base.clone());
                }
                for arg in args {
                    arg.collect_type_dependencies(deps);
                }
            }
            StructuredType::Tuple(elements) => {
                for elem in elements {
                    elem.collect_type_dependencies(deps);
                }
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                for param in params {
                    param.param_type.collect_type_dependencies(deps);
                }
                return_type.collect_type_dependencies(deps);
            }
            StructuredType::List { element_type } => {
                element_type.collect_type_dependencies(deps);
            }
            StructuredType::Map {
                key_type,
                value_type,
            } => {
                key_type.collect_type_dependencies(deps);
                value_type.collect_type_dependencies(deps);
            }
            StructuredType::Option { inner_type } => {
                inner_type.collect_type_dependencies(deps);
            }
            StructuredType::Result { ok_type, err_type } => {
                ok_type.collect_type_dependencies(deps);
                err_type.collect_type_dependencies(deps);
            }
            StructuredType::Struct { name, fields } => {
                if !deps.contains(name) {
                    deps.push(name.clone());
                }
                for field in fields {
                    field.field_type.collect_type_dependencies(deps);
                }
            }
            StructuredType::Trait { name, functions } => {
                if !deps.contains(name) {
                    deps.push(name.clone());
                }
                for func in functions {
                    for param in &func.params {
                        param.param_type.collect_type_dependencies(deps);
                    }
                    func.return_type.collect_type_dependencies(deps);
                }
            }
            StructuredType::TypeError { fallback_type, .. } => {
                if let Some(fallback) = fallback_type {
                    fallback.collect_type_dependencies(deps);
                }
            }
            // Primitive types have no dependencies
            StructuredType::Integer64
            | StructuredType::Float64
            | StructuredType::Boolean
            | StructuredType::String
            | StructuredType::Atom => {}
        }
    }

    // =============================================================================
    // Additional utility methods (replacing ConcreteType functionality)
    // =============================================================================

    /// Check if a type can be used in conditionals (implements Boolean trait)
    pub fn can_be_condition(&self) -> bool {
        // Only Boolean type can be used in conditionals (no truthiness in Outrun)
        matches!(self, StructuredType::Boolean)
    }

    /// Check if this type is homogeneous (for collections)
    pub fn is_homogeneous(&self) -> bool {
        match self {
            StructuredType::List { .. } => true, // Lists are always homogeneous
            StructuredType::Map { .. } => true,  // Maps are homogeneous per key/value type
            StructuredType::Tuple(elements) => {
                // Tuples are homogeneous if all elements are the same type
                elements.windows(2).all(|w| w[0] == w[1])
            }
            _ => true, // Non-collection types are considered homogeneous
        }
    }

    /// Check if this is a simple type (non-generic, non-compound)
    pub fn is_simple(&self) -> bool {
        matches!(self, StructuredType::Simple(_))
    }

    /// Check if this is a function type
    pub fn is_function(&self) -> bool {
        matches!(self, StructuredType::Function { .. })
    }

    /// Check if this is a struct type
    pub fn is_struct(&self) -> bool {
        matches!(self, StructuredType::Struct { .. })
    }

    /// Check if this is a trait type
    pub fn is_trait(&self) -> bool {
        matches!(self, StructuredType::Trait { .. })
    }

    /// Check if this is an error type (for error recovery)
    pub fn is_error(&self) -> bool {
        matches!(self, StructuredType::TypeError { .. })
    }

    /// Convert to string representation for error messages
    pub fn to_string_representation(&self) -> String {
        match self {
            StructuredType::Simple(type_name_id) => type_name_id.to_string(),
            StructuredType::TypeVariable(type_name_id) => format!("${type_name_id}"),
            StructuredType::Generic { base, args } => {
                let arg_names: Vec<String> = args
                    .iter()
                    .map(|arg| arg.to_string_representation())
                    .collect();
                format!("{}<{}>", base, arg_names.join(", "))
            }
            StructuredType::Tuple(elements) => {
                let element_names: Vec<String> = elements
                    .iter()
                    .map(|elem| elem.to_string_representation())
                    .collect();
                format!("({})", element_names.join(", "))
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                let param_strs: Vec<String> = params
                    .iter()
                    .map(|p| format!("{}: {}", p.name, p.param_type.to_string_representation()))
                    .collect();
                format!(
                    "({}) -> {}",
                    param_strs.join(", "),
                    return_type.to_string_representation()
                )
            }
            // Concrete primitive types
            StructuredType::Integer64 => "Integer64".to_string(),
            StructuredType::Float64 => "Float64".to_string(),
            StructuredType::Boolean => "Boolean".to_string(),
            StructuredType::String => "String".to_string(),
            StructuredType::Atom => "Atom".to_string(),
            // Concrete collection types
            StructuredType::List { element_type } => {
                format!("List<{}>", element_type.to_string_representation())
            }
            StructuredType::Map {
                key_type,
                value_type,
            } => {
                format!(
                    "Map<{}, {}>",
                    key_type.to_string_representation(),
                    value_type.to_string_representation()
                )
            }
            // Concrete option and result types
            StructuredType::Option { inner_type } => {
                format!("Option<{}>", inner_type.to_string_representation())
            }
            StructuredType::Result { ok_type, err_type } => {
                format!(
                    "Result<{}, {}>",
                    ok_type.to_string_representation(),
                    err_type.to_string_representation()
                )
            }
            // Concrete struct and trait types
            StructuredType::Struct { name, .. } => {
                format!("struct {name}")
            }
            StructuredType::Trait { name, .. } => {
                format!("trait {name}")
            }
            StructuredType::TypeError { fallback_type, .. } => {
                // For error types, show the fallback type if available, otherwise show error marker
                if let Some(fallback) = fallback_type {
                    format!("<ERROR: {}>", fallback.to_string_representation())
                } else {
                    "<ERROR>".to_string()
                }
            }
        }
    }
}

/// Resolve generic type parameter to concrete type if possible (NEW VERSION)
fn resolve_generic_parameter_new(
    type_name_id: TypeNameId,
    _compiler_env: &CompilerEnvironment,
) -> TypeNameId {
    // Check if this is a generic type parameter that needs resolution
    let type_name = type_name_id.to_string();

    // Single letter types (T, U, V, K, V, etc.) are generic parameters
    if type_name.len() == 1 && type_name.chars().next().unwrap().is_ascii_uppercase() {
        // For now, return the original type - sophisticated generic resolution can be added later
        return type_name_id;
    }

    // Known pattern matching for common generic resolution
    if type_name == "T" || type_name == "U" || type_name == "K" || type_name == "V" {
        // Generic parameters unify with Any by default
        // More sophisticated resolution would check constraints here
        return type_name_id;
    }

    type_name_id
}

impl UnificationContext {
    /// Create a new unification context
    pub fn new() -> Self {
        Self {
            generic_substitutions: HashMap::new(),
            expression_types: HashMap::new(),
            dispatch_strategies: HashMap::new(),
            span_mapping: crate::desugaring::SpanMapping::new(),
            // SMT integration fields
            smt_constraints: Vec::new(),
            constraint_collector: None,
            solver_cache: HashMap::new(),
        }
    }

    /// Add a generic parameter substitution
    pub fn add_generic_substitution(&mut self, param: TypeNameId, substitution: StructuredType) {
        self.generic_substitutions.insert(param, substitution);
    }

    /// Resolve a type with current context (handle Self and generic substitutions)
    pub fn resolve_type(
        &self,
        type_ref: &StructuredType,
        _compiler_env: &CompilerEnvironment,
    ) -> UnificationResult<StructuredType> {
        match type_ref {
            StructuredType::Simple(type_id) => {
                // Check for explicit generic substitution first
                if let Some(substitution) = self.generic_substitutions.get(type_id) {
                    Ok(substitution.clone())
                } else {
                    // Check for generic parameter resolution (T -> Outrun.Core.Integer64, etc.)
                    let resolved_type_id =
                        resolve_generic_parameter_new(type_id.clone(), _compiler_env);
                    if resolved_type_id != type_id.clone() {
                        Ok(StructuredType::Simple(resolved_type_id))
                    } else {
                        Ok(type_ref.clone())
                    }
                }
            }
            StructuredType::TypeVariable(_type_id) => {
                // TypeVariable resolution is handled by SMT constraints
                // Return the TypeVariable as-is for SMT solver to handle
                Ok(type_ref.clone())
            }
            StructuredType::Generic { base, args } => {
                let resolved_args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| self.resolve_type(arg, _compiler_env))
                    .collect();
                Ok(StructuredType::Generic {
                    base: base.clone(),
                    args: resolved_args?,
                })
            }
            StructuredType::Tuple(elements) => {
                let resolved_elements: Result<Vec<_>, _> = elements
                    .iter()
                    .map(|elem| self.resolve_type(elem, _compiler_env))
                    .collect();
                Ok(StructuredType::Tuple(resolved_elements?))
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                let resolved_params: Result<Vec<_>, _> = params
                    .iter()
                    .map(|param| {
                        Ok(FunctionParam {
                            name: param.name.clone(),
                            param_type: self.resolve_type(&param.param_type, _compiler_env)?,
                        })
                    })
                    .collect();
                let resolved_return = self.resolve_type(return_type, _compiler_env)?;
                Ok(StructuredType::Function {
                    params: resolved_params?,
                    return_type: Box::new(resolved_return),
                })
            }
            // Concrete collection types need recursive resolution
            StructuredType::List { element_type } => {
                let resolved_element = self.resolve_type(element_type, _compiler_env)?;
                Ok(StructuredType::List {
                    element_type: Box::new(resolved_element),
                })
            }
            StructuredType::Map {
                key_type,
                value_type,
            } => {
                let resolved_key = self.resolve_type(key_type, _compiler_env)?;
                let resolved_value = self.resolve_type(value_type, _compiler_env)?;
                Ok(StructuredType::Map {
                    key_type: Box::new(resolved_key),
                    value_type: Box::new(resolved_value),
                })
            }
            StructuredType::Option { inner_type } => {
                let resolved_inner = self.resolve_type(inner_type, _compiler_env)?;
                Ok(StructuredType::Option {
                    inner_type: Box::new(resolved_inner),
                })
            }
            StructuredType::Result { ok_type, err_type } => {
                let resolved_ok = self.resolve_type(ok_type, _compiler_env)?;
                let resolved_err = self.resolve_type(err_type, _compiler_env)?;
                Ok(StructuredType::Result {
                    ok_type: Box::new(resolved_ok),
                    err_type: Box::new(resolved_err),
                })
            }
            // Concrete struct types need field type resolution
            StructuredType::Struct { name, fields } => {
                let resolved_fields: Result<Vec<_>, _> = fields
                    .iter()
                    .map(|field| {
                        Ok(StructFieldDescriptor {
                            name: field.name.clone(),
                            field_type: self.resolve_type(&field.field_type, _compiler_env)?,
                        })
                    })
                    .collect();
                Ok(StructuredType::Struct {
                    name: name.clone(),
                    fields: resolved_fields?,
                })
            }
            // Concrete trait types need function signature resolution
            StructuredType::Trait { name, functions } => {
                let resolved_functions: Result<Vec<_>, _> = functions
                    .iter()
                    .map(|func| {
                        let resolved_params: Result<Vec<_>, _> = func
                            .params
                            .iter()
                            .map(|param| {
                                Ok(FunctionParam {
                                    name: param.name.clone(),
                                    param_type: self
                                        .resolve_type(&param.param_type, _compiler_env)?,
                                })
                            })
                            .collect();
                        let resolved_return =
                            self.resolve_type(&func.return_type, _compiler_env)?;
                        Ok(TraitFunctionDescriptor {
                            name: func.name.clone(),
                            params: resolved_params?,
                            return_type: resolved_return,
                            is_guard: func.is_guard,
                            is_static: func.is_static,
                            has_default_impl: func.has_default_impl,
                        })
                    })
                    .collect();
                Ok(StructuredType::Trait {
                    name: name.clone(),
                    functions: resolved_functions?,
                })
            }
            StructuredType::TypeError {
                error,
                fallback_type,
                error_span,
            } => {
                // For error types, resolve the fallback type if it exists
                if let Some(fallback) = fallback_type {
                    let resolved_fallback = self.resolve_type(fallback, _compiler_env)?;
                    Ok(StructuredType::TypeError {
                        error: error.clone(),
                        fallback_type: Some(Box::new(resolved_fallback)),
                        error_span: *error_span,
                    })
                } else {
                    // No fallback to resolve
                    Ok(type_ref.clone())
                }
            }
            // Primitive types don't need resolution
            StructuredType::Integer64
            | StructuredType::Float64
            | StructuredType::Boolean
            | StructuredType::String
            | StructuredType::Atom => Ok(type_ref.clone()),
        }
    }

    /// Add an expression type mapping
    pub fn add_expression_type(&mut self, span: outrun_parser::Span, expr_type: StructuredType) {
        self.expression_types.insert(span, expr_type);
    }

    /// Get the resolved type for an expression by its span
    pub fn get_expression_type(&self, span: &outrun_parser::Span) -> Option<&StructuredType> {
        self.expression_types.get(span)
    }

    /// Store a dispatch strategy for a function call expression
    pub fn add_dispatch_strategy(
        &mut self,
        span: outrun_parser::Span,
        strategy: crate::checker::DispatchMethod,
    ) {
        self.dispatch_strategies.insert(span, strategy);
    }

    /// Get the dispatch strategy for a function call by its span
    pub fn get_dispatch_strategy(
        &self,
        span: &outrun_parser::Span,
    ) -> Option<&crate::checker::DispatchMethod> {
        self.dispatch_strategies.get(span)
    }

    /// Merge span mapping from desugaring phase
    pub fn merge_span_mapping(&mut self, span_mapping: crate::desugaring::SpanMapping) {
        self.span_mapping.merge(span_mapping);
    }

    /// Get the desugared span for an original span
    pub fn get_desugared_span(&self, original: outrun_parser::Span) -> Option<outrun_parser::Span> {
        self.span_mapping.get_desugared_span(original)
    }

    /// Get the original span for a desugared span
    pub fn get_original_span(&self, desugared: outrun_parser::Span) -> Option<outrun_parser::Span> {
        self.span_mapping.get_original_span(desugared)
    }

    /// Merge another UnificationContext into this one
    /// This is essential for package composition and incremental compilation
    pub fn merge_with(
        mut self,
        other: UnificationContext,
    ) -> Result<UnificationContext, UnificationError> {
        // CompilerEnvironment merging is handled at a higher level since it contains shared Arc data
        // The CompilerEnvironment should be the same instance or compatible

        // Note: TypeInterner merging is handled globally through Arc<Mutex<>> sharing
        // We don't need to merge type_interner here since all contexts share the same global state

        // Merge generic substitutions - conflicts indicate incompatible generic contexts
        for (param_id, substitution) in other.generic_substitutions {
            if let Some(existing) = self.generic_substitutions.get(&param_id) {
                // Use structural equality to check compatibility
                // This avoids the infinite recursion issue that occurred with unify_structured_types
                if existing != &substitution {
                    return Err(UnificationError::UnboundTypeVariable {
                        name: format!("Conflicting generic substitution for {param_id:?}"),
                    });
                }
            } else {
                self.generic_substitutions.insert(param_id, substitution);
            }
        }

        // Merge expression types - overlapping spans should have compatible types
        for (span, expr_type) in other.expression_types {
            if let Some(existing) = self.expression_types.get(&span) {
                // Use structural equality to check compatibility
                // This avoids the infinite recursion issue that occurred with unify_structured_types
                if existing != &expr_type {
                    return Err(UnificationError::UnboundTypeVariable {
                        name: format!("Conflicting expression type for span {span:?}"),
                    });
                }
            } else {
                self.expression_types.insert(span, expr_type);
            }
        }

        // Merge span mappings
        self.span_mapping.merge(other.span_mapping);

        Ok(self)
    }

    /// Create a lightweight summary of this context for package interfaces
    pub fn create_summary(&self, compiler_env: &CompilerEnvironment) -> TypeContextSummary {
        let type_definitions = HashMap::new();
        let mut trait_implementations = HashMap::new();

        // Extract type definitions (simplified for now)
        // TODO: Implement proper type definition extraction

        // Extract trait implementations from CompilerEnvironment
        let modules = compiler_env.modules().read().unwrap();
        for (module_key, _module) in modules.iter() {
            if let crate::compilation::compiler_environment::ModuleKey::TraitImpl(
                trait_type,
                impl_type,
            ) = module_key
            {
                let trait_name = trait_type.to_string_representation();
                let impl_name = impl_type.to_string_representation();
                trait_implementations
                    .entry(trait_name)
                    .or_insert_with(Vec::new)
                    .push(impl_name);
            }
        }

        TypeContextSummary {
            type_definitions,
            trait_implementations,
        }
    }

    // =============================================================================
    // SMT Constraint Methods (Phase 4)
    // =============================================================================

    /// Add an SMT constraint to the context
    pub fn add_smt_constraint(&mut self, constraint: SMTConstraint) {
        self.smt_constraints.push(constraint);
    }

    /// NEW: SMT-based type checking - check if two types are compatible
    pub fn smt_types_compatible(
        &mut self,
        type1: &StructuredType,
        type2: &StructuredType,
        context: String,
        compiler_env: &CompilerEnvironment,
    ) -> Result<bool, crate::smt::solver::SMTError> {
        // Create a unification constraint
        let constraint = SMTConstraint::TypeUnification {
            type1: type1.clone(),
            type2: type2.clone(),
            context,
        };

        // Solve with a temporary solver
        let system = crate::smt::SMTConstraintSystem::new();
        let mut solver = system.create_solver();
        solver.add_constraints(&[constraint], compiler_env)?;

        match solver.solve() {
            crate::smt::solver::SolverResult::Satisfiable(_) => Ok(true),
            crate::smt::solver::SolverResult::Unsatisfiable(_) => Ok(false),
            crate::smt::solver::SolverResult::Unknown(_) => Ok(false), // Conservative: assume incompatible
        }
    }

    /// NEW: SMT-based trait implementation checking
    pub fn smt_implements_trait(
        &mut self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
        compiler_env: &CompilerEnvironment,
    ) -> Result<bool, crate::smt::solver::SMTError> {
        // Create a trait implementation constraint
        let constraint = SMTConstraint::TraitImplemented {
            impl_type: impl_type.clone(),
            trait_type: trait_type.clone(),
        };

        // Solve with a temporary solver
        let system = crate::smt::SMTConstraintSystem::new();
        let mut solver = system.create_solver();
        solver.add_constraints(&[constraint], compiler_env)?;

        match solver.solve() {
            crate::smt::solver::SolverResult::Satisfiable(_) => Ok(true),
            crate::smt::solver::SolverResult::Unsatisfiable(_) => Ok(false),
            crate::smt::solver::SolverResult::Unknown(_) => Ok(false), // Conservative: assume not implemented
        }
    }

    /// NEW: SMT-based function dispatch resolution
    pub fn smt_resolve_function_call(
        &mut self,
        candidates: &[crate::smt::constraints::FunctionSignature],
        call_signature: &crate::smt::constraints::FunctionSignature,
        call_site: outrun_parser::Span,
        compiler_env: &CompilerEnvironment,
    ) -> Result<Option<usize>, crate::smt::solver::SMTError> {
        if candidates.is_empty() {
            return Ok(None);
        }

        if candidates.len() == 1 {
            // Single candidate - just check if it matches
            let constraint = SMTConstraint::FunctionSignatureMatch {
                expected: candidates[0].clone(),
                actual: call_signature.clone(),
                call_site,
            };

            let system = crate::smt::SMTConstraintSystem::new();
            let mut solver = system.create_solver();
            solver.add_constraints(&[constraint], compiler_env)?;

            match solver.solve() {
                crate::smt::solver::SolverResult::Satisfiable(_) => Ok(Some(0)),
                _ => Ok(None),
            }
        } else {
            // Multiple candidates - create constraints for each and solve
            let mut constraints = Vec::new();
            for (i, candidate) in candidates.iter().enumerate() {
                let constraint = SMTConstraint::FunctionSignatureMatch {
                    expected: candidate.clone(),
                    actual: call_signature.clone(),
                    call_site,
                };
                constraints.push((i, constraint));
            }

            // Try each constraint individually to find the first satisfiable one
            for (index, constraint) in constraints {
                let system = crate::smt::SMTConstraintSystem::new();
                let mut solver = system.create_solver();
                solver.add_constraints(&[constraint], compiler_env)?;

                if let crate::smt::solver::SolverResult::Satisfiable(_) = solver.solve() {
                    return Ok(Some(index));
                }
            }

            Ok(None) // No candidates matched
        }
    }

    /// Solve accumulated SMT constraints using the provided solver
    pub fn solve_accumulated_constraints(
        &mut self,
        compiler_env: &CompilerEnvironment,
    ) -> Result<crate::smt::solver::ConstraintModel, crate::smt::solver::SMTError> {
        if self.smt_constraints.is_empty() {
            return Ok(crate::smt::solver::ConstraintModel::empty());
        }

        let system = crate::smt::SMTConstraintSystem::new();
        let mut solver = system.create_solver();

        // Add all constraints to the solver
        solver.add_constraints(&self.smt_constraints, compiler_env)?;

        // Solve constraints
        match solver.solve() {
            crate::smt::solver::SolverResult::Satisfiable(model) => Ok(model),
            crate::smt::solver::SolverResult::Unsatisfiable(conflicting) => {
                Err(crate::smt::solver::SMTError::SolverError(format!(
                    "Unsatisfiable constraints: {} conflicts",
                    conflicting.len()
                )))
            }
            crate::smt::solver::SolverResult::Unknown(reason) => {
                Err(crate::smt::solver::SMTError::SolverError(reason))
            }
        }
    }

    /// Check if there are pending SMT constraints to solve
    pub fn has_pending_constraints(&self) -> bool {
        !self.smt_constraints.is_empty()
            || self
                .constraint_collector
                .as_ref()
                .is_some_and(|collector| !collector.constraints.is_empty())
    }

    /// Initialize constraint collector for complex constraint scenarios
    pub fn start_constraint_collection(&mut self) {
        self.constraint_collector = Some(ConstraintCollector::new());
    }

    /// Finalize constraint collection and add to main constraint list
    pub fn finalize_constraint_collection(&mut self) {
        if let Some(collector) = self.constraint_collector.take() {
            self.smt_constraints.extend(collector.constraints);
        }
    }

    /// Get current constraint count for debugging
    pub fn constraint_count(&self) -> usize {
        self.smt_constraints.len()
            + self
                .constraint_collector
                .as_ref()
                .map_or(0, |collector| collector.constraints.len())
    }

    /// Clear all SMT constraints (for testing/reset scenarios)
    pub fn clear_smt_constraints(&mut self) {
        self.smt_constraints.clear();
        self.constraint_collector = None;
        self.solver_cache.clear();
    }
}

impl ConstraintCollector {
    /// Create a new constraint collector
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            scope_stack: Vec::new(),
            deferred_constraints: Vec::new(),
        }
    }

    /// Collect a trait constraint
    pub fn collect_trait_constraint(
        &mut self,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
    ) {
        let constraint = SMTConstraint::TraitImplemented {
            impl_type: impl_type.clone(),
            trait_type: trait_type.clone(),
        };
        self.constraints.push(constraint);
    }

    /// Collect a unification constraint
    pub fn collect_unification_constraint(
        &mut self,
        type1: &StructuredType,
        type2: &StructuredType,
        context: String,
    ) {
        let constraint = SMTConstraint::TypeUnification {
            type1: type1.clone(),
            type2: type2.clone(),
            context,
        };
        self.constraints.push(constraint);
    }

    /// Collect a function constraint from a function call
    pub fn collect_function_constraint(
        &mut self,
        expected_signature: &crate::smt::constraints::FunctionSignature,
        actual_signature: &crate::smt::constraints::FunctionSignature,
        call_site: outrun_parser::Span,
    ) {
        let constraint = SMTConstraint::FunctionSignatureMatch {
            expected: expected_signature.clone(),
            actual: actual_signature.clone(),
            call_site,
        };
        self.constraints.push(constraint);
    }

    /// Defer a complex constraint for later resolution
    pub fn defer_complex_constraint(
        &mut self,
        constraint: SMTConstraint,
        dependency_types: Vec<TypeNameId>,
        priority: u8,
    ) {
        let deferred = DeferredConstraint {
            constraint,
            dependency_types,
            resolution_priority: priority,
        };
        self.deferred_constraints.push(deferred);
    }

    /// Push a new constraint scope
    pub fn push_scope(&mut self, is_function_scope: bool) {
        let scope = ConstraintScope {
            scope_id: self.scope_stack.len(),
            constraint_count: self.constraints.len(),
            is_function_scope,
        };
        self.scope_stack.push(scope);
    }

    /// Pop the most recent constraint scope
    pub fn pop_scope(&mut self) {
        if let Some(scope) = self.scope_stack.pop() {
            // Truncate constraints to the count when this scope was created
            self.constraints.truncate(scope.constraint_count);
        }
    }

    /// Get all collected constraints (including deferred ones)
    pub fn get_all_constraints(&self) -> Vec<SMTConstraint> {
        let mut all_constraints = self.constraints.clone();

        // Add deferred constraints sorted by priority
        let mut deferred_sorted = self.deferred_constraints.clone();
        deferred_sorted.sort_by_key(|d| d.resolution_priority);

        for deferred in deferred_sorted {
            all_constraints.push(deferred.constraint);
        }

        all_constraints
    }

    /// Check if collector is empty
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty() && self.deferred_constraints.is_empty()
    }
}

impl Default for ConstraintCollector {
    fn default() -> Self {
        Self::new()
    }
}

/// Self type substitution system for trait function calls
/// This module provides comprehensive Self -> concrete type substitution
/// in generic contexts like Option<Self> -> Option<ConcreteType>
pub mod self_substitution {
    use super::*;

    /// Recursively substitute Self with a concrete type in a StructuredType
    /// This handles complex cases like Option<Self> -> Option<ConcreteType>
    pub fn substitute_self_in_type(
        structured_type: &StructuredType,
        self_type: &StructuredType,
    ) -> Result<StructuredType, UnificationError> {
        match structured_type {
            StructuredType::Simple(type_id) => {
                // Check if this is Self
                if type_id.to_string() == "Self" {
                    Ok(self_type.clone())
                } else {
                    Ok(structured_type.clone())
                }
            }
            StructuredType::TypeVariable(_type_id) => {
                // TypeVariable is handled by SMT constraints, don't substitute
                Ok(structured_type.clone())
            }
            StructuredType::Generic { base, args } => {
                // Check if base is Self
                let substituted_base = if base.to_string() == "Self" {
                    match self_type {
                        StructuredType::Simple(self_base) => self_base.clone(),
                        _ => {
                            return Err(UnificationError::UnboundTypeVariable {
                                name: "Self in generic context with non-simple type".to_string(),
                            });
                        }
                    }
                } else {
                    base.clone()
                };

                // Recursively substitute Self in all generic arguments
                let mut substituted_args = Vec::new();
                for arg in args {
                    substituted_args.push(substitute_self_in_type(arg, self_type)?);
                }

                Ok(StructuredType::Generic {
                    base: substituted_base,
                    args: substituted_args,
                })
            }
            StructuredType::Tuple(elements) => {
                // Recursively substitute Self in all tuple elements
                let mut substituted_elements = Vec::new();
                for element in elements {
                    substituted_elements.push(substitute_self_in_type(element, self_type)?);
                }
                Ok(StructuredType::Tuple(substituted_elements))
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                // Substitute Self in all parameter types
                let mut substituted_params = Vec::new();
                for param in params {
                    substituted_params.push(FunctionParam {
                        name: param.name.clone(),
                        param_type: substitute_self_in_type(&param.param_type, self_type)?,
                    });
                }

                // Substitute Self in return type
                let substituted_return = substitute_self_in_type(return_type, self_type)?;

                Ok(StructuredType::Function {
                    params: substituted_params,
                    return_type: Box::new(substituted_return),
                })
            }
            StructuredType::TypeError { .. } => {
                // Don't substitute in error types
                Ok(structured_type.clone())
            }
            // Primitive types don't contain Self
            StructuredType::Integer64
            | StructuredType::Float64
            | StructuredType::Boolean
            | StructuredType::String
            | StructuredType::Atom => Ok(structured_type.clone()),
            // Complex types may contain Self in their type parameters
            StructuredType::List { element_type } => {
                let substituted_element = substitute_self_in_type(element_type, self_type)?;
                Ok(StructuredType::List {
                    element_type: Box::new(substituted_element),
                })
            }
            StructuredType::Map {
                key_type,
                value_type,
            } => {
                let substituted_key = substitute_self_in_type(key_type, self_type)?;
                let substituted_value = substitute_self_in_type(value_type, self_type)?;
                Ok(StructuredType::Map {
                    key_type: Box::new(substituted_key),
                    value_type: Box::new(substituted_value),
                })
            }
            StructuredType::Option { inner_type } => {
                let substituted_inner = substitute_self_in_type(inner_type, self_type)?;
                Ok(StructuredType::Option {
                    inner_type: Box::new(substituted_inner),
                })
            }
            StructuredType::Result { ok_type, err_type } => {
                let substituted_ok = substitute_self_in_type(ok_type, self_type)?;
                let substituted_err = substitute_self_in_type(err_type, self_type)?;
                Ok(StructuredType::Result {
                    ok_type: Box::new(substituted_ok),
                    err_type: Box::new(substituted_err),
                })
            }
            StructuredType::Struct { name, fields } => {
                // Substitute Self in field types
                let mut substituted_fields = Vec::new();
                for field in fields {
                    substituted_fields.push(StructFieldDescriptor {
                        name: field.name.clone(),
                        field_type: substitute_self_in_type(&field.field_type, self_type)?,
                    });
                }
                Ok(StructuredType::Struct {
                    name: name.clone(),
                    fields: substituted_fields,
                })
            }
            StructuredType::Trait { name, functions } => {
                // Substitute Self in function signatures
                let mut substituted_functions = Vec::new();
                for func in functions {
                    let mut substituted_params = Vec::new();
                    for param in &func.params {
                        substituted_params.push(FunctionParam {
                            name: param.name.clone(),
                            param_type: substitute_self_in_type(&param.param_type, self_type)?,
                        });
                    }
                    let substituted_return = substitute_self_in_type(&func.return_type, self_type)?;
                    substituted_functions.push(TraitFunctionDescriptor {
                        name: func.name.clone(),
                        params: substituted_params,
                        return_type: substituted_return,
                        is_guard: func.is_guard,
                        is_static: func.is_static,
                        has_default_impl: func.has_default_impl,
                    });
                }
                Ok(StructuredType::Trait {
                    name: name.clone(),
                    functions: substituted_functions,
                })
            }
        }
    }

    /// Infer Self type from function arguments for trait dispatch
    /// This performs bidirectional type inference to determine what Self should be
    pub fn infer_self_from_arguments(
        trait_function_params: &[FunctionParam],
        call_arguments: &[StructuredType],
    ) -> Result<Option<StructuredType>, UnificationError> {
        // Find the first parameter that uses Self and infer from the corresponding argument
        for (param, arg_type) in trait_function_params.iter().zip(call_arguments.iter()) {
            if let Some(inferred_self) = extract_self_from_parameter(&param.param_type, arg_type)? {
                return Ok(Some(inferred_self));
            }
        }
        Ok(None)
    }

    /// Extract Self type from a parameter-argument pair
    /// For example: param is Option<Self>, arg is Option<Integer> -> Self = Integer
    fn extract_self_from_parameter(
        param_type: &StructuredType,
        arg_type: &StructuredType,
    ) -> Result<Option<StructuredType>, UnificationError> {
        match (param_type, arg_type) {
            (StructuredType::Simple(param_id), arg_type) => {
                // Check if parameter is exactly Self
                let param_name = param_id.to_string();
                if param_name == "Self" {
                    return Ok(Some(arg_type.clone()));
                }
                Ok(None)
            }
            (
                StructuredType::Generic {
                    base: param_base,
                    args: param_args,
                },
                StructuredType::Generic {
                    base: arg_base,
                    args: arg_args,
                },
            ) => {
                // Both are generic - bases must match, then recursively extract from args
                if param_base == arg_base && param_args.len() == arg_args.len() {
                    for (param_arg, arg_arg) in param_args.iter().zip(arg_args.iter()) {
                        if let Some(inferred_self) =
                            extract_self_from_parameter(param_arg, arg_arg)?
                        {
                            return Ok(Some(inferred_self));
                        }
                    }
                }
                Ok(None)
            }
            (StructuredType::Tuple(param_elements), StructuredType::Tuple(arg_elements)) => {
                // Both are tuples - recursively extract from corresponding elements
                if param_elements.len() == arg_elements.len() {
                    for (param_elem, arg_elem) in param_elements.iter().zip(arg_elements.iter()) {
                        if let Some(inferred_self) =
                            extract_self_from_parameter(param_elem, arg_elem)?
                        {
                            return Ok(Some(inferred_self));
                        }
                    }
                }
                Ok(None)
            }
            _ => Ok(None), // Other combinations don't allow Self extraction
        }
    }

    /// Create a fully substituted function signature for trait dispatch
    /// This replaces all occurrences of Self with the concrete type
    pub fn create_concrete_function_signature(
        trait_function_params: &[FunctionParam],
        trait_function_return: &StructuredType,
        self_type: &StructuredType,
    ) -> Result<(Vec<FunctionParam>, StructuredType), UnificationError> {
        // Substitute Self in all parameters
        let mut concrete_params = Vec::new();
        for param in trait_function_params {
            concrete_params.push(FunctionParam {
                name: param.name.clone(),
                param_type: substitute_self_in_type(&param.param_type, self_type)?,
            });
        }

        // Substitute Self in return type
        let concrete_return = substitute_self_in_type(trait_function_return, self_type)?;

        Ok((concrete_params, concrete_return))
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn create_test_context() -> (
            UnificationContext,
            crate::compilation::compiler_environment::CompilerEnvironment,
        ) {
            let context = UnificationContext::new();
            let compiler_env = crate::compilation::compiler_environment::CompilerEnvironment::new();
            (context, compiler_env)
        }

        #[test]
        fn test_simple_self_substitution() {
            let (_context, compiler_env) = create_test_context();
            let self_id = compiler_env.intern_type_name("Self");
            let integer_id = compiler_env.intern_type_name("Integer");

            let self_type = StructuredType::Simple(self_id);
            let integer_type = StructuredType::Simple(integer_id);

            let result = substitute_self_in_type(&self_type, &integer_type);
            assert_eq!(result, Ok(integer_type));
        }

        #[test]
        fn test_generic_self_substitution() {
            let (_context, compiler_env) = create_test_context();
            let option_id = compiler_env.intern_type_name("Option");
            let self_id = compiler_env.intern_type_name("Self");
            let integer_id = compiler_env.intern_type_name("Integer");

            // Option<Self>
            let option_self =
                StructuredType::generic(option_id.clone(), vec![StructuredType::Simple(self_id)]);
            let integer_type = StructuredType::Simple(integer_id.clone());

            // Should become Option<Integer>
            let result = substitute_self_in_type(&option_self, &integer_type);
            let expected =
                StructuredType::generic(option_id, vec![StructuredType::Simple(integer_id)]);
            assert_eq!(result, Ok(expected));
        }

        #[test]
        fn test_self_inference_from_arguments() {
            let (_context, compiler_env) = create_test_context();
            let self_id = compiler_env.intern_type_name("Self");
            let integer_id = compiler_env.intern_type_name("Integer");
            let lhs_atom = compiler_env.intern_atom_name("lhs");
            let rhs_atom = compiler_env.intern_atom_name("rhs");

            // Function params: (lhs: Self, rhs: Self)
            let params = vec![
                FunctionParam {
                    name: lhs_atom,
                    param_type: StructuredType::Simple(self_id.clone()),
                },
                FunctionParam {
                    name: rhs_atom,
                    param_type: StructuredType::Simple(self_id),
                },
            ];

            // Call arguments: (Integer, Integer)
            let args = vec![
                StructuredType::Simple(integer_id.clone()),
                StructuredType::Simple(integer_id.clone()),
            ];

            let result = infer_self_from_arguments(&params, &args);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), Some(StructuredType::Simple(integer_id)));
        }
    }
}
