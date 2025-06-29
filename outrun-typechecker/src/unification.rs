//! Type unification algorithm for Outrun
//!
//! This module implements the comprehensive type unification algorithm described
//! in the typechecker CLAUDE.md. It handles recursive unification of generic types,
//! trait-based compatibility, and Self type resolution.

use crate::compilation::compiler_environment::{
    AtomId as CompilerAtomId, CompilerEnvironment, TypeNameId,
};
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

/// Main unification algorithm
///
/// This implements the comprehensive unification rules described in CLAUDE.md:
/// 1. Exact match (fast path)
/// 2. Generic type unification with recursive argument checking
/// 3. Tuple type unification with positional matching
/// 4. Function type unification with named parameter matching
/// 5. Simple type unification with trait-based compatibility
/// 6. Cross-type unification (always fails)
pub fn unify_structured_types(
    type1: &StructuredType,
    type2: &StructuredType,
    context: &UnificationContext,
    compiler_env: &CompilerEnvironment,
) -> UnificationResult<Option<StructuredType>> {
    // 1. Exact Match (Fast Path)
    if type1 == type2 {
        // Exact match - return either type (they're identical)
        return Ok(Some(type1.clone()));
    }

    // 2. Resolve generic parameters before unification
    let resolved_type1 = context.resolve_type(type1, compiler_env)?;
    let resolved_type2 = context.resolve_type(type2, compiler_env)?;

    // Try unification again with resolved types
    if resolved_type1 == resolved_type2 {
        return Ok(Some(resolved_type1));
    }

    let final_result = match (&resolved_type1, &resolved_type2) {
        // 2. Generic Type Unification
        (
            StructuredType::Generic {
                base: base1,
                args: args1,
            },
            StructuredType::Generic {
                base: base2,
                args: args2,
            },
        ) => {
            // Check if base types are identical first (e.g., Option vs Option)
            if base1 == base2 {
                // Same base type - skip trait implementation checking and proceed to argument unification
            } else {
                // Different base types - check trait implementation between them
                let base1_module = compiler_env.get_module(base1.clone());
                let base2_module = compiler_env.get_module(base2.clone());

                match (base1_module, base2_module) {
                    (Some(module1), Some(module2)) => {
                        // Get the structured types from the modules (these should be the generic versions like Map<K,V>)
                        let struct_type1 = &module1.structured_type;
                        let struct_type2 = &module2.structured_type;

                        // Check if one implements the other as a trait
                        // Try: does struct_type1 implement struct_type2? (e.g., Outrun.Core.Map<K,V> implements Map<K,V>)
                        if compiler_env.implements_trait(struct_type1, struct_type2)
                            || compiler_env.implements_trait(struct_type2, struct_type1)
                        {
                            // Base types can unify via trait implementation - proceed with argument unification
                        } else {
                            return Ok(None);
                        }
                    }
                    _ => {
                        return Ok(None);
                    }
                }
            }

            // Argument count must match (arity check)
            if args1.len() != args2.len() {
                return Err(UnificationError::ArityMismatch {
                    expected: args1.len(),
                    found: args2.len(),
                    base_type: base1.clone(),
                });
            }

            // All arguments must unify recursively and collect unified types
            let mut unified_args = Vec::new();
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                if let Some(unified_arg) =
                    unify_structured_types(arg1, arg2, context, compiler_env)?
                {
                    unified_args.push(unified_arg);
                } else {
                    return Ok(None);
                }
            }

            // Choose the more concrete base type
            let concrete_base = if compiler_env.is_trait(&StructuredType::Simple(base1.clone()))
                && !compiler_env.is_trait(&StructuredType::Simple(base2.clone()))
            {
                base2.clone() // base2 is concrete, base1 is trait - prefer concrete
            } else {
                base1.clone() // base1 is concrete or both are same type category - prefer first
            };

            Ok(Some(StructuredType::Generic {
                base: concrete_base,
                args: unified_args,
            }))
        }

        // 3. Tuple Type Unification
        (StructuredType::Tuple(elems1), StructuredType::Tuple(elems2)) => {
            // Element count must match
            if elems1.len() != elems2.len() {
                return Ok(None);
            }

            // All elements must unify positionally and collect unified types
            let mut unified_elems = Vec::new();
            for (elem1, elem2) in elems1.iter().zip(elems2.iter()) {
                if let Some(unified_elem) =
                    unify_structured_types(elem1, elem2, context, compiler_env)?
                {
                    unified_elems.push(unified_elem);
                } else {
                    return Ok(None);
                }
            }

            Ok(Some(StructuredType::Tuple(unified_elems)))
        }

        // 4. Function Type Unification
        (
            StructuredType::Function {
                params: params1,
                return_type: ret1,
            },
            StructuredType::Function {
                params: params2,
                return_type: ret2,
            },
        ) => {
            // Parameter count must match
            if params1.len() != params2.len() {
                return Ok(None);
            }

            // All parameters must have matching names AND types
            let mut unified_params = Vec::new();
            for (param1, param2) in params1.iter().zip(params2.iter()) {
                if param1.name.clone() != param2.name.clone() {
                    return Err(UnificationError::ParameterNameMismatch {
                        expected: param1.name.clone(),
                        found: param2.name.clone(),
                    });
                }

                if let Some(unified_param_type) = unify_structured_types(
                    &param1.param_type,
                    &param2.param_type,
                    context,
                    compiler_env,
                )? {
                    unified_params.push(crate::unification::FunctionParam {
                        name: param1.name.clone(),
                        param_type: unified_param_type,
                    });
                } else {
                    return Ok(None);
                }
            }

            // Return types must unify
            if let Some(unified_return_type) =
                unify_structured_types(ret1, ret2, context, compiler_env)?
            {
                Ok(Some(StructuredType::Function {
                    params: unified_params,
                    return_type: Box::new(unified_return_type),
                }))
            } else {
                Ok(None)
            }
        }

        // 5. Simple Type Unification (handle trait compatibility)
        (StructuredType::Simple(type1), StructuredType::Simple(type2)) => {
            unify_simple_types(type1.clone(), type2.clone(), context, compiler_env)
        }

        // 6. Trait-to-Generic Implementation Unification
        // Handle cases like List (trait) vs Outrun.Core.List<T> (generic implementation)
        (StructuredType::Simple(trait_type), StructuredType::Generic { base, args: _ }) => {
            unify_trait_with_generic_implementation(
                trait_type.clone(),
                base.clone(),
                context,
                compiler_env,
            )
        }
        (StructuredType::Generic { base, args: _ }, StructuredType::Simple(trait_type)) => {
            unify_trait_with_generic_implementation(
                trait_type.clone(),
                base.clone(),
                context,
                compiler_env,
            )
        }

        // 7. TypeError Unification - Use fallback type if available
        (StructuredType::TypeError { fallback_type, .. }, other) => {
            if let Some(fallback) = fallback_type {
                unify_structured_types(fallback, other, context, compiler_env)
            } else {
                // No fallback - can't unify with error type
                Ok(None)
            }
        }
        (other, StructuredType::TypeError { fallback_type, .. }) => {
            if let Some(fallback) = fallback_type {
                unify_structured_types(other, fallback, context, compiler_env)
            } else {
                // No fallback - can't unify with error type
                Ok(None)
            }
        }

        // 8. Concrete Type Unification (new additions for unified type system)

        // Primitive type unification - exact match required
        (StructuredType::Integer64, StructuredType::Integer64) => {
            Ok(Some(StructuredType::Integer64))
        }
        (StructuredType::Float64, StructuredType::Float64) => Ok(Some(StructuredType::Float64)),
        (StructuredType::Boolean, StructuredType::Boolean) => Ok(Some(StructuredType::Boolean)),
        (StructuredType::String, StructuredType::String) => Ok(Some(StructuredType::String)),
        (StructuredType::Atom, StructuredType::Atom) => Ok(Some(StructuredType::Atom)),

        // Collection type unification
        (
            StructuredType::List {
                element_type: elem1,
            },
            StructuredType::List {
                element_type: elem2,
            },
        ) => {
            if let Some(unified_elem) = unify_structured_types(elem1, elem2, context, compiler_env)?
            {
                Ok(Some(StructuredType::List {
                    element_type: Box::new(unified_elem),
                }))
            } else {
                Ok(None)
            }
        }
        (
            StructuredType::Map {
                key_type: k1,
                value_type: v1,
            },
            StructuredType::Map {
                key_type: k2,
                value_type: v2,
            },
        ) => {
            if let (Some(unified_key), Some(unified_value)) = (
                unify_structured_types(k1, k2, context, compiler_env)?,
                unify_structured_types(v1, v2, context, compiler_env)?,
            ) {
                Ok(Some(StructuredType::Map {
                    key_type: Box::new(unified_key),
                    value_type: Box::new(unified_value),
                }))
            } else {
                Ok(None)
            }
        }

        // Option and Result type unification
        (
            StructuredType::Option { inner_type: inner1 },
            StructuredType::Option { inner_type: inner2 },
        ) => {
            if let Some(unified_inner) =
                unify_structured_types(inner1, inner2, context, compiler_env)?
            {
                Ok(Some(StructuredType::Option {
                    inner_type: Box::new(unified_inner),
                }))
            } else {
                Ok(None)
            }
        }
        (
            StructuredType::Result {
                ok_type: ok1,
                err_type: err1,
            },
            StructuredType::Result {
                ok_type: ok2,
                err_type: err2,
            },
        ) => {
            if let (Some(unified_ok), Some(unified_err)) = (
                unify_structured_types(ok1, ok2, context, compiler_env)?,
                unify_structured_types(err1, err2, context, compiler_env)?,
            ) {
                Ok(Some(StructuredType::Result {
                    ok_type: Box::new(unified_ok),
                    err_type: Box::new(unified_err),
                }))
            } else {
                Ok(None)
            }
        }

        // Struct type unification - name must match, fields must be compatible
        (
            StructuredType::Struct { name: name1, .. },
            StructuredType::Struct { name: name2, .. },
        ) => {
            if name1 == name2 {
                Ok(Some(type1.clone())) // Return either struct (they're the same)
            } else {
                Ok(None)
            }
        }

        // Trait type unification - name must match
        (StructuredType::Trait { name: name1, .. }, StructuredType::Trait { name: name2, .. }) => {
            if name1 == name2 {
                Ok(Some(type1.clone())) // Return either trait (they're the same)
            } else {
                Ok(None)
            }
        }

        // Mixed concrete/abstract unification - let trait-based compatibility handle this
        (StructuredType::Simple(simple_type), concrete_type) => {
            // Try to unify simple type name with concrete type equivalent
            unify_simple_with_concrete(simple_type.clone(), concrete_type, context, compiler_env)
        }
        (concrete_type, StructuredType::Simple(simple_type)) => {
            // Try to unify concrete type with simple type equivalent
            unify_simple_with_concrete(simple_type.clone(), concrete_type, context, compiler_env)
        }

        // 9. Cross-Type Unification (always fails)
        _ => Ok(None),
    };

    final_result
}

/// Simple type unification with trait-based compatibility
///
/// This handles the relationship between traits and concrete types:
/// - Boolean trait unifies with Outrun.Core.Boolean concrete type
/// - Integer trait unifies with Outrun.Core.Integer64 concrete type
/// - Generic type parameters (T, U, etc.) unify with concrete types implementing required traits
/// - Etc.
fn unify_simple_types(
    type1: TypeNameId,
    type2: TypeNameId,
    context: &UnificationContext,
    compiler_env: &CompilerEnvironment,
) -> UnificationResult<Option<StructuredType>> {
    // Fast path: exact match
    if type1 == type2 {
        // Exact match in simple types - return either type
        return Ok(Some(StructuredType::Simple(type1)));
    }

    // Check for generic parameter substitution first
    let resolved_type1 = resolve_generic_parameter_new(type1.clone(), compiler_env);
    let resolved_type2 = resolve_generic_parameter_new(type2.clone(), compiler_env);

    // If either type was resolved from a generic parameter, try unification again
    if resolved_type1 != type1 || resolved_type2 != type2 {
        if resolved_type1 == resolved_type2 {
            return Ok(Some(StructuredType::Simple(resolved_type1)));
        }
        // Continue with trait-based unification using resolved types
        return unify_resolved_simple_types_new(
            resolved_type1,
            resolved_type2,
            context,
            compiler_env,
        );
    }

    // Original trait-based unification logic
    unify_resolved_simple_types_new(type1, type2, context, compiler_env)
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

/*
fn resolve_generic_parameter(type_id: TypeNameId, context: &UnificationContext) -> TypeNameId {
    // Check if this is a generic type parameter that needs resolution
    let type_name = type_id.to_string();
    if true { // Always process since TypeNameId is now always valid
        // Single letter types (T, U, V, K, V, etc.) are generic parameters
        if type_name.len() == 1 && type_name.chars().next().unwrap().is_ascii_uppercase() {
            // Generic parameters can always unify with Any (since all types implement Any)
            // For specific cases, we can provide more targeted resolution
            match type_name.as_str() {
                "T" => {
                    // Common case: T in Option<Integer> context should resolve to Integer64
                    if let Some(integer64_id) =
                        compiler_env.intern_type_name("Outrun.Core.Integer64")
                    {
                        return integer64_id;
                    }
                }
                "K" | "V" => {
                    // Map key/value types - these are often used with Any in intrinsics
                    // For now, resolve to String as a common case
                    if let Some(string_id) = compiler_env.intern_type_name("Outrun.Core.String") {
                        return string_id;
                    }
                }
                _ => {} // Other generic parameters
            }
        }
    }
    type_id // Return original if no resolution possible
}
*/

/// Check if a type name is a generic parameter (single uppercase letter)
/// This is a heuristic for common generic parameter names like T, K, V, U, etc.
/// It specifically excludes known concrete type names.
fn is_generic_type_parameter(type_name: &str) -> bool {
    // Must be single uppercase letter
    if type_name.len() != 1 {
        return false;
    }

    let first_char = type_name.chars().next().unwrap();
    if !first_char.is_ascii_uppercase() {
        return false;
    }

    // Exclude common concrete type names that happen to be single letters
    // (though in practice, most concrete types are longer names)
    match type_name {
        // Most common generic parameter names
        "T" | "U" | "V" | "K" | "E" | "R" | "A" | "B" | "C" | "D" | "F" | "G" | "H" | "I" | "J"
        | "L" | "M" | "N" | "O" | "P" | "Q" | "S" | "W" | "X" | "Y" | "Z" => true,
        _ => false,
    }
}

/// Unify already-resolved simple types with trait-based compatibility
/// Unified simple types using CompilerEnvironment (NEW VERSION)
fn unify_resolved_simple_types_new(
    type1: TypeNameId,
    type2: TypeNameId,
    _context: &UnificationContext,
    compiler_env: &CompilerEnvironment,
) -> UnificationResult<Option<StructuredType>> {
    // Fast path: exact match
    if type1 == type2 {
        return Ok(Some(StructuredType::Simple(type1)));
    }

    let type1_name = type1.to_string();
    let type2_name = type2.to_string();

    // Special case: Generic type parameter unification
    // A single uppercase letter (like "T", "K", "V") unifies with any type
    let type1_is_generic = is_generic_type_parameter(&type1_name);
    let type2_is_generic = is_generic_type_parameter(&type2_name);

    if type1_is_generic || type2_is_generic {
        // Generic type parameters can unify with any type - choose the non-generic type
        if type1_is_generic && !type2_is_generic {
            return Ok(Some(StructuredType::Simple(type2))); // type2 is concrete
        } else if !type1_is_generic && type2_is_generic {
            return Ok(Some(StructuredType::Simple(type1))); // type1 is concrete
        } else {
            return Ok(Some(StructuredType::Simple(type1))); // Both generic or other case
        }
    }

    // Convert TypeNameId to StructuredType for trait checking
    let type1_structured = StructuredType::Simple(type1.clone());
    let type2_structured = StructuredType::Simple(type2.clone());

    // Check if types are traits using CompilerEnvironment
    let type1_is_trait = compiler_env.is_trait(&type1_structured);
    let type2_is_trait = compiler_env.is_trait(&type2_structured);

    match (type1_is_trait, type2_is_trait) {
        // Both traits: must be exactly equal (already checked above)
        (true, true) => Ok(None),

        // Type1 is trait, type2 is concrete: type2 must implement type1
        (true, false) => {
            if compiler_env.implements_trait(&type2_structured, &type1_structured) {
                Ok(Some(type2_structured)) // Return the concrete type
            } else {
                Err(UnificationError::TraitNotImplemented {
                    trait_id: type1,
                    type_id: type2,
                })
            }
        }

        // Type1 is concrete, type2 is trait: type1 must implement type2
        (false, true) => {
            if compiler_env.implements_trait(&type1_structured, &type2_structured) {
                Ok(Some(type1_structured)) // Return the concrete type
            } else {
                Err(UnificationError::TraitNotImplemented {
                    trait_id: type2,
                    type_id: type1,
                })
            }
        }

        // Both concrete: must be exactly equal (already checked above)
        (false, false) => Ok(None),
    }
}

/// Unify a trait type with a generic implementation type
///
/// This handles cases like:
/// - List (trait) vs Outrun.Core.List<T> (generic implementation)
/// - Option (trait) vs Outrun.Core.Option<T> (generic implementation)
///
/// The unification succeeds if the generic type implements the trait.
fn unify_trait_with_generic_implementation(
    trait_type: TypeNameId,
    implementation_base: TypeNameId,
    _context: &UnificationContext,
    compiler_env: &CompilerEnvironment,
) -> UnificationResult<Option<StructuredType>> {
    // Convert to StructuredType for CompilerEnvironment trait checking
    let trait_structured = StructuredType::Simple(trait_type.clone());
    let impl_structured = StructuredType::Simple(implementation_base.clone());

    // Check if the trait is actually a trait
    if !compiler_env.is_trait(&trait_structured) {
        return Ok(None);
    }

    // Check if the implementation type implements the trait
    if compiler_env.implements_trait(&impl_structured, &trait_structured) {
        Ok(Some(impl_structured)) // Return the concrete implementation type
    } else {
        Err(UnificationError::TraitNotImplemented {
            trait_id: trait_type,
            type_id: implementation_base,
        })
    }
}

/// Unify a simple type name with a concrete type
///
/// This handles cases where we have a simple type name (like "Boolean" or "Integer")
/// that needs to unify with a concrete type variant (like StructuredType::Boolean).
fn unify_simple_with_concrete(
    simple_type: TypeNameId,
    concrete_type: &StructuredType,
    _context: &UnificationContext,
    compiler_env: &CompilerEnvironment,
) -> UnificationResult<Option<StructuredType>> {
    let type_name = simple_type.to_string();

    // Map common type names to their concrete equivalents
    let unified_result = match (type_name.as_str(), concrete_type) {
        ("Integer64", StructuredType::Integer64) => Some(StructuredType::Integer64),
        ("Float64", StructuredType::Float64) => Some(StructuredType::Float64),
        ("Boolean", StructuredType::Boolean) => Some(StructuredType::Boolean),
        ("String", StructuredType::String) => Some(StructuredType::String),
        ("Atom", StructuredType::Atom) => Some(StructuredType::Atom),

        // Handle generic mappings - "List" simple type with List<T> concrete type
        ("List", StructuredType::List { .. }) => Some(concrete_type.clone()),
        ("Map", StructuredType::Map { .. }) => Some(concrete_type.clone()),
        ("Option", StructuredType::Option { .. }) => Some(concrete_type.clone()),
        ("Result", StructuredType::Result { .. }) => Some(concrete_type.clone()),

        // Check if the simple type is a trait that the concrete type implements
        _ => {
            let simple_structured = StructuredType::Simple(simple_type.clone());

            // If simple type is a trait, check if concrete type implements it
            if compiler_env.is_trait(&simple_structured) {
                // For concrete types, create a simple type representation for trait checking
                let concrete_as_simple = match concrete_type {
                    StructuredType::Integer64 => {
                        StructuredType::Simple(compiler_env.intern_type_name("Integer64"))
                    }
                    StructuredType::Float64 => {
                        StructuredType::Simple(compiler_env.intern_type_name("Float64"))
                    }
                    StructuredType::Boolean => {
                        StructuredType::Simple(compiler_env.intern_type_name("Boolean"))
                    }
                    StructuredType::String => {
                        StructuredType::Simple(compiler_env.intern_type_name("String"))
                    }
                    StructuredType::Atom => {
                        StructuredType::Simple(compiler_env.intern_type_name("Atom"))
                    }
                    StructuredType::List { .. } => {
                        StructuredType::Simple(compiler_env.intern_type_name("List"))
                    }
                    StructuredType::Map { .. } => {
                        StructuredType::Simple(compiler_env.intern_type_name("Map"))
                    }
                    StructuredType::Option { .. } => {
                        StructuredType::Simple(compiler_env.intern_type_name("Option"))
                    }
                    StructuredType::Result { .. } => {
                        StructuredType::Simple(compiler_env.intern_type_name("Result"))
                    }
                    _ => return Ok(None), // Other concrete types don't have simple equivalents
                };

                return Ok(
                    if compiler_env.implements_trait(&concrete_as_simple, &simple_structured) {
                        Some(concrete_type.clone()) // Return the concrete type
                    } else {
                        None
                    },
                );
            }
            None
        }
    };

    Ok(unified_result)
}

impl UnificationContext {
    /// Create a new unification context
    pub fn new() -> Self {
        Self {
            generic_substitutions: HashMap::new(),
            expression_types: HashMap::new(),
            dispatch_strategies: HashMap::new(),
            span_mapping: crate::desugaring::SpanMapping::new(),
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

        // Set up some basic types
        let string_id = compiler_env.intern_type_name("String");
        let boolean_id = compiler_env.intern_type_name("Boolean");
        let _option_id = compiler_env.intern_type_name("Option");
        let core_string_id = compiler_env.intern_type_name("Outrun.Core.String");
        let core_boolean_id = compiler_env.intern_type_name("Outrun.Core.Boolean");

        // Register trait implementations using CompilerEnvironment
        let string_structured = StructuredType::Simple(string_id);
        let boolean_structured = StructuredType::Simple(boolean_id);
        let core_string_structured = StructuredType::Simple(core_string_id);
        let core_boolean_structured = StructuredType::Simple(core_boolean_id);

        compiler_env.register_trait_implementation(core_string_structured, string_structured);
        compiler_env.register_trait_implementation(core_boolean_structured, boolean_structured);

        (context, compiler_env)
    }

    #[test]
    fn test_exact_match_unification() {
        let (context, compiler_env) = create_test_context();
        let string_id = compiler_env.intern_type_name("String");

        let type1 = StructuredType::Simple(string_id.clone());
        let type2 = StructuredType::Simple(string_id);

        assert!(
            unify_structured_types(&type1, &type2, &context, &compiler_env)
                .unwrap()
                .is_some()
        );
    }

    #[test]
    fn test_trait_concrete_unification() {
        let (context, compiler_env) = create_test_context();
        let string_trait_id = compiler_env.intern_type_name("String");
        let core_string_id = compiler_env.intern_type_name("Outrun.Core.String");

        let trait_type = StructuredType::Simple(string_trait_id);
        let concrete_type = StructuredType::Simple(core_string_id);

        // Trait should unify with implementing concrete type
        assert!(
            unify_structured_types(&trait_type, &concrete_type, &context, &compiler_env)
                .unwrap()
                .is_some()
        );
        assert!(
            unify_structured_types(&concrete_type, &trait_type, &context, &compiler_env)
                .unwrap()
                .is_some()
        );
    }

    #[test]
    fn test_generic_type_unification() {
        let (context, compiler_env) = create_test_context();
        let option_id = compiler_env.intern_type_name("Option");
        let string_trait_id = compiler_env.intern_type_name("String");
        let core_string_id = compiler_env.intern_type_name("Outrun.Core.String");

        let option_trait = StructuredType::generic(
            option_id.clone(),
            vec![StructuredType::Simple(string_trait_id)],
        );
        let option_concrete =
            StructuredType::generic(option_id, vec![StructuredType::Simple(core_string_id)]);

        // Option<String> should unify with Option<Outrun.Core.String>
        assert!(
            unify_structured_types(&option_trait, &option_concrete, &context, &compiler_env)
                .unwrap()
                .is_some()
        );
    }

    #[test]
    fn test_arity_mismatch_error() {
        let (context, compiler_env) = create_test_context();
        let option_id = compiler_env.intern_type_name("Option");
        let map_id = compiler_env.intern_type_name("Map");
        let string_id = compiler_env.intern_type_name("String");

        let option_type =
            StructuredType::generic(option_id, vec![StructuredType::Simple(string_id.clone())]);
        let map_type = StructuredType::generic(
            map_id,
            vec![
                StructuredType::Simple(string_id.clone()),
                StructuredType::Simple(string_id),
            ],
        );

        // Different base types should fail
        assert!(
            unify_structured_types(&option_type, &map_type, &context, &compiler_env)
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn test_tuple_unification() {
        let (context, compiler_env) = create_test_context();
        let string_id = compiler_env.intern_type_name("String");
        let boolean_id = compiler_env.intern_type_name("Boolean");

        let tuple1 = StructuredType::tuple(vec![
            StructuredType::Simple(string_id.clone()),
            StructuredType::Simple(boolean_id.clone()),
        ]);
        let tuple2 = StructuredType::tuple(vec![
            StructuredType::Simple(string_id),
            StructuredType::Simple(boolean_id),
        ]);

        assert!(
            unify_structured_types(&tuple1, &tuple2, &context, &compiler_env)
                .unwrap()
                .is_some()
        );
    }

    // Self type resolution is now handled by the type parameter system in multi_program_compiler
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
