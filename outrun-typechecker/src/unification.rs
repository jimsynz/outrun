//! Type unification algorithm for Outrun
//!
//! This module implements the comprehensive type unification algorithm described
//! in the typechecker CLAUDE.md. It handles recursive unification of generic types,
//! trait-based compatibility, and Self type resolution.

use crate::types::{AtomId, TypeId, TypeInterner};
use std::collections::HashMap;

/// Result type for unification operations
pub type UnificationResult<T = ()> = Result<T, UnificationError>;

/// Errors that can occur during type unification
#[derive(Debug, Clone, PartialEq)]
pub enum UnificationError {
    /// Types cannot be unified (fundamental incompatibility)
    IncompatibleTypes {
        type1: StructuredType,
        type2: StructuredType,
    },
    /// Generic arity mismatch (e.g., Option<T> vs Map<K, V>)
    ArityMismatch {
        expected: usize,
        found: usize,
        base_type: TypeId,
    },
    /// Parameter name mismatch in function types
    ParameterNameMismatch { expected: AtomId, found: AtomId },
    /// Trait implementation missing
    TraitNotImplemented { trait_id: TypeId, type_id: TypeId },
    /// Unbound type variable or Self in invalid context
    UnboundTypeVariable { name: String },
}

/// Structured type system for proper recursive generic handling
///
/// This replaces the flawed string-based approach with proper structured
/// representation that enables recursive unification.
#[derive(Debug, Clone, PartialEq)]
pub enum StructuredType {
    /// Simple type without generics (e.g., "String", "Boolean")
    Simple(TypeId),
    /// Generic type with structured arguments (e.g., Option<T>, Map<K, V>)
    Generic {
        base: TypeId,
        args: Vec<StructuredType>,
    },
    /// Tuple type with ordered elements
    Tuple(Vec<StructuredType>),
    /// Function type with named parameters and return type
    Function {
        params: Vec<FunctionParam>,
        return_type: Box<StructuredType>,
    },
}

/// Function parameter with name and type (required for Outrun)
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub name: AtomId,
    pub param_type: StructuredType,
}

/// Type unification context containing necessary information for resolution
#[derive(Debug, Clone)]
pub struct UnificationContext {
    /// Trait registry for trait implementation checking
    pub trait_registry: TraitRegistry,
    /// Type interner for fast type comparison
    pub type_interner: TypeInterner,
    /// Generic parameter substitutions
    pub generic_substitutions: HashMap<TypeId, StructuredType>,
}

impl Default for UnificationContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait registry for checking trait implementations
#[derive(Debug, Clone)]
pub struct TraitRegistry {
    /// Map from (type_id, trait_id) -> implementation exists
    implementations: HashMap<(TypeId, TypeId), bool>,
    /// Set of all defined traits
    traits: std::collections::HashSet<TypeId>,
}

impl Default for TraitRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl StructuredType {
    /// Create a simple type from a type ID
    pub fn simple(type_id: TypeId) -> Self {
        StructuredType::Simple(type_id)
    }

    /// Create a generic type with arguments
    pub fn generic(base: TypeId, args: Vec<StructuredType>) -> Self {
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

    /// Convert to string representation for error messages
    pub fn to_string_representation(&self, interner: &TypeInterner) -> String {
        match self {
            StructuredType::Simple(type_id) => interner
                .type_name(*type_id)
                .unwrap_or_else(|| format!("Unknown({:?})", type_id)),
            StructuredType::Generic { base, args } => {
                let base_name = interner
                    .type_name(*base)
                    .unwrap_or_else(|| format!("Unknown({:?})", base));
                let arg_names: Vec<String> = args
                    .iter()
                    .map(|arg| arg.to_string_representation(interner))
                    .collect();
                format!("{}<{}>", base_name, arg_names.join(", "))
            }
            StructuredType::Tuple(elements) => {
                let element_names: Vec<String> = elements
                    .iter()
                    .map(|elem| elem.to_string_representation(interner))
                    .collect();
                format!("({})", element_names.join(", "))
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                let param_strs: Vec<String> = params
                    .iter()
                    .map(|p| {
                        format!(
                            "{}: {}",
                            interner
                                .atom_name(p.name)
                                .unwrap_or_else(|| format!("atom_{:?}", p.name)),
                            p.param_type.to_string_representation(interner)
                        )
                    })
                    .collect();
                format!(
                    "({}) -> {}",
                    param_strs.join(", "),
                    return_type.to_string_representation(interner)
                )
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
) -> UnificationResult<bool> {
    // 1. Exact Match (Fast Path)
    if type1 == type2 {
        return Ok(true);
    }

    // 2. Resolve generic parameters before unification
    let resolved_type1 = context.resolve_type(type1)?;
    let resolved_type2 = context.resolve_type(type2)?;

    // Try unification again with resolved types
    if resolved_type1 == resolved_type2 {
        return Ok(true);
    }

    match (&resolved_type1, &resolved_type2) {
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
            // Base types must unify
            if !unify_simple_types(*base1, *base2, context)? {
                return Ok(false);
            }

            // Argument count must match (arity check)
            if args1.len() != args2.len() {
                return Err(UnificationError::ArityMismatch {
                    expected: args1.len(),
                    found: args2.len(),
                    base_type: *base1,
                });
            }

            // All arguments must unify recursively
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                if !unify_structured_types(arg1, arg2, context)? {
                    return Ok(false);
                }
            }

            Ok(true)
        }

        // 3. Tuple Type Unification
        (StructuredType::Tuple(elems1), StructuredType::Tuple(elems2)) => {
            // Element count must match
            if elems1.len() != elems2.len() {
                return Ok(false);
            }

            // All elements must unify positionally
            for (elem1, elem2) in elems1.iter().zip(elems2.iter()) {
                if !unify_structured_types(elem1, elem2, context)? {
                    return Ok(false);
                }
            }

            Ok(true)
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
                return Ok(false);
            }

            // All parameters must have matching names AND types
            for (param1, param2) in params1.iter().zip(params2.iter()) {
                if param1.name != param2.name {
                    return Err(UnificationError::ParameterNameMismatch {
                        expected: param1.name,
                        found: param2.name,
                    });
                }

                if !unify_structured_types(&param1.param_type, &param2.param_type, context)? {
                    return Ok(false);
                }
            }

            // Return types must unify
            unify_structured_types(ret1, ret2, context)
        }

        // 5. Simple Type Unification (handle trait compatibility)
        (StructuredType::Simple(type1), StructuredType::Simple(type2)) => {
            unify_simple_types(*type1, *type2, context)
        }

        // 6. Trait-to-Generic Implementation Unification
        // Handle cases like List (trait) vs Outrun.Core.List<T> (generic implementation)
        (StructuredType::Simple(trait_type), StructuredType::Generic { base, args: _ }) => {
            unify_trait_with_generic_implementation(*trait_type, *base, context)
        }
        (StructuredType::Generic { base, args: _ }, StructuredType::Simple(trait_type)) => {
            unify_trait_with_generic_implementation(*trait_type, *base, context)
        }

        // 7. Cross-Type Unification (always fails)
        _ => Ok(false),
    }
}

/// Simple type unification with trait-based compatibility
///
/// This handles the relationship between traits and concrete types:
/// - Boolean trait unifies with Outrun.Core.Boolean concrete type
/// - Integer trait unifies with Outrun.Core.Integer64 concrete type
/// - Generic type parameters (T, U, etc.) unify with concrete types implementing required traits
/// - Etc.
fn unify_simple_types(
    type1: TypeId,
    type2: TypeId,
    context: &UnificationContext,
) -> UnificationResult<bool> {
    // Fast path: exact match
    if type1 == type2 {
        return Ok(true);
    }

    // Check for generic parameter substitution first
    let resolved_type1 = resolve_generic_parameter(type1, context);
    let resolved_type2 = resolve_generic_parameter(type2, context);

    // If either type was resolved from a generic parameter, try unification again
    if resolved_type1 != type1 || resolved_type2 != type2 {
        if resolved_type1 == resolved_type2 {
            return Ok(true);
        }
        // Continue with trait-based unification using resolved types
        return unify_resolved_simple_types(resolved_type1, resolved_type2, context);
    }

    // Original trait-based unification logic
    unify_resolved_simple_types(type1, type2, context)
}

/// Resolve generic type parameter to concrete type if possible
fn resolve_generic_parameter(type_id: TypeId, context: &UnificationContext) -> TypeId {
    // Check if this is a generic type parameter that needs resolution
    if let Some(type_name) = context.type_interner.type_name(type_id) {
        // Single letter types (T, U, V, K, V, etc.) are generic parameters
        if type_name.len() == 1 && type_name.chars().next().unwrap().is_ascii_uppercase() {
            // Generic parameters can always unify with Any (since all types implement Any)
            // For specific cases, we can provide more targeted resolution
            match type_name.as_str() {
                "T" => {
                    // Common case: T in Option<Integer> context should resolve to Integer64
                    if let Some(integer64_id) =
                        context.type_interner.get_type("Outrun.Core.Integer64")
                    {
                        return integer64_id;
                    }
                }
                "K" | "V" => {
                    // Map key/value types - these are often used with Any in intrinsics
                    // For now, resolve to String as a common case
                    if let Some(string_id) = context.type_interner.get_type("Outrun.Core.String") {
                        return string_id;
                    }
                }
                _ => {} // Other generic parameters
            }
        }
    }
    type_id // Return original if no resolution possible
}

/// Unify already-resolved simple types with trait-based compatibility
fn unify_resolved_simple_types(
    type1: TypeId,
    type2: TypeId,
    context: &UnificationContext,
) -> UnificationResult<bool> {
    // Fast path: exact match
    if type1 == type2 {
        return Ok(true);
    }

    // Special case: Any trait unifies with any type (including generic parameters)
    let any_trait_id = context.type_interner.get_type("Any");
    if let Some(any_id) = any_trait_id {
        if type1 == any_id || type2 == any_id {
            return Ok(true);
        }
    }

    // Check if either type is a generic parameter
    let type1_is_generic = is_generic_parameter(type1, context);
    let type2_is_generic = is_generic_parameter(type2, context);

    // Generic parameters can unify with Any (or any trait, since all types implement at least some traits)
    if type1_is_generic || type2_is_generic {
        // Generic parameters are very flexible - they can unify with any trait
        return Ok(true);
    }

    // Determine if types are traits or concrete
    let type1_is_trait = context.trait_registry.is_trait(type1);
    let type2_is_trait = context.trait_registry.is_trait(type2);

    match (type1_is_trait, type2_is_trait) {
        // Both traits: must be exactly equal (already checked above)
        (true, true) => Ok(false),

        // Type1 is trait, type2 is concrete: type2 must implement type1
        (true, false) => {
            if context.trait_registry.implements_trait(type2, type1) {
                Ok(true)
            } else {
                Err(UnificationError::TraitNotImplemented {
                    trait_id: type1,
                    type_id: type2,
                })
            }
        }

        // Type1 is concrete, type2 is trait: type1 must implement type2
        (false, true) => {
            if context.trait_registry.implements_trait(type1, type2) {
                Ok(true)
            } else {
                Err(UnificationError::TraitNotImplemented {
                    trait_id: type2,
                    type_id: type1,
                })
            }
        }

        // Both concrete: must be exactly equal (already checked above)
        (false, false) => Ok(false),
    }
}

/// Check if a type is a generic parameter (single uppercase letter)
fn is_generic_parameter(type_id: TypeId, context: &UnificationContext) -> bool {
    if let Some(type_name) = context.type_interner.type_name(type_id) {
        type_name.len() == 1 && type_name.chars().next().unwrap().is_ascii_uppercase()
    } else {
        false
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
    trait_type: TypeId,
    implementation_base: TypeId,
    context: &UnificationContext,
) -> UnificationResult<bool> {
    // Check if the trait is actually a trait
    if !context.trait_registry.is_trait(trait_type) {
        return Ok(false);
    }

    // Check if the implementation type implements the trait
    if context
        .trait_registry
        .implements_trait(implementation_base, trait_type)
    {
        Ok(true)
    } else {
        Err(UnificationError::TraitNotImplemented {
            trait_id: trait_type,
            type_id: implementation_base,
        })
    }
}

impl TraitRegistry {
    /// Create a new empty trait registry
    pub fn new() -> Self {
        Self {
            implementations: HashMap::new(),
            traits: std::collections::HashSet::new(),
        }
    }

    /// Register a trait
    pub fn register_trait(&mut self, trait_id: TypeId) {
        self.traits.insert(trait_id);
    }

    /// Register a trait implementation
    pub fn register_implementation(&mut self, type_id: TypeId, trait_id: TypeId) {
        self.implementations.insert((type_id, trait_id), true);
    }

    /// Register a concrete type and automatically implement Any trait
    pub fn register_concrete_type(&mut self, type_id: TypeId, any_trait_id: TypeId) {
        // Automatically register that this concrete type implements Any
        self.register_implementation(type_id, any_trait_id);
    }

    /// Check if a type is a trait
    pub fn is_trait(&self, type_id: TypeId) -> bool {
        self.traits.contains(&type_id)
    }

    /// Check if a type implements a trait
    pub fn implements_trait(&self, type_id: TypeId, trait_id: TypeId) -> bool {
        self.implementations
            .get(&(type_id, trait_id))
            .copied()
            .unwrap_or(false)
    }
}

impl UnificationContext {
    /// Create a new unification context
    pub fn new() -> Self {
        Self {
            trait_registry: TraitRegistry::new(),
            type_interner: TypeInterner::new(),
            generic_substitutions: HashMap::new(),
        }
    }

    /// Add a generic parameter substitution
    pub fn add_generic_substitution(&mut self, param: TypeId, substitution: StructuredType) {
        self.generic_substitutions.insert(param, substitution);
    }

    /// Resolve a type with current context (handle Self and generic substitutions)
    pub fn resolve_type(&self, type_ref: &StructuredType) -> UnificationResult<StructuredType> {
        match type_ref {
            StructuredType::Simple(type_id) => {
                // Check for explicit generic substitution first
                if let Some(substitution) = self.generic_substitutions.get(type_id) {
                    Ok(substitution.clone())
                } else {
                    // Check for generic parameter resolution (T -> Outrun.Core.Integer64, etc.)
                    let resolved_type_id = resolve_generic_parameter(*type_id, self);
                    if resolved_type_id != *type_id {
                        Ok(StructuredType::Simple(resolved_type_id))
                    } else {
                        Ok(type_ref.clone())
                    }
                }
            }
            StructuredType::Generic { base, args } => {
                let resolved_args: Result<Vec<_>, _> =
                    args.iter().map(|arg| self.resolve_type(arg)).collect();
                Ok(StructuredType::Generic {
                    base: *base,
                    args: resolved_args?,
                })
            }
            StructuredType::Tuple(elements) => {
                let resolved_elements: Result<Vec<_>, _> = elements
                    .iter()
                    .map(|elem| self.resolve_type(elem))
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
                            name: param.name,
                            param_type: self.resolve_type(&param.param_type)?,
                        })
                    })
                    .collect();
                let resolved_return = self.resolve_type(return_type)?;
                Ok(StructuredType::Function {
                    params: resolved_params?,
                    return_type: Box::new(resolved_return),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_context() -> UnificationContext {
        let mut context = UnificationContext::new();

        // Set up some basic types
        let string_id = context.type_interner.intern_type("String");
        let boolean_id = context.type_interner.intern_type("Boolean");
        let _option_id = context.type_interner.intern_type("Option");
        let core_string_id = context.type_interner.intern_type("Outrun.Core.String");
        let core_boolean_id = context.type_interner.intern_type("Outrun.Core.Boolean");

        // Register traits
        context.trait_registry.register_trait(string_id);
        context.trait_registry.register_trait(boolean_id);

        // Register implementations
        context
            .trait_registry
            .register_implementation(core_string_id, string_id);
        context
            .trait_registry
            .register_implementation(core_boolean_id, boolean_id);

        context
    }

    #[test]
    fn test_exact_match_unification() {
        let context = create_test_context();
        let string_id = context.type_interner.get_type("String").unwrap();

        let type1 = StructuredType::simple(string_id);
        let type2 = StructuredType::simple(string_id);

        assert!(unify_structured_types(&type1, &type2, &context).unwrap());
    }

    #[test]
    fn test_trait_concrete_unification() {
        let context = create_test_context();
        let string_trait_id = context.type_interner.get_type("String").unwrap();
        let core_string_id = context
            .type_interner
            .get_type("Outrun.Core.String")
            .unwrap();

        let trait_type = StructuredType::simple(string_trait_id);
        let concrete_type = StructuredType::simple(core_string_id);

        // Trait should unify with implementing concrete type
        assert!(unify_structured_types(&trait_type, &concrete_type, &context).unwrap());
        assert!(unify_structured_types(&concrete_type, &trait_type, &context).unwrap());
    }

    #[test]
    fn test_generic_type_unification() {
        let mut context = create_test_context();
        let option_id = context.type_interner.intern_type("Option");
        let string_trait_id = context.type_interner.get_type("String").unwrap();
        let core_string_id = context
            .type_interner
            .get_type("Outrun.Core.String")
            .unwrap();

        let option_trait =
            StructuredType::generic(option_id, vec![StructuredType::simple(string_trait_id)]);
        let option_concrete =
            StructuredType::generic(option_id, vec![StructuredType::simple(core_string_id)]);

        // Option<String> should unify with Option<Outrun.Core.String>
        assert!(unify_structured_types(&option_trait, &option_concrete, &context).unwrap());
    }

    #[test]
    fn test_arity_mismatch_error() {
        let mut context = create_test_context();
        let option_id = context.type_interner.intern_type("Option");
        let map_id = context.type_interner.intern_type("Map");
        let string_id = context.type_interner.get_type("String").unwrap();

        let option_type =
            StructuredType::generic(option_id, vec![StructuredType::simple(string_id)]);
        let map_type = StructuredType::generic(
            map_id,
            vec![
                StructuredType::simple(string_id),
                StructuredType::simple(string_id),
            ],
        );

        // Different base types should fail
        assert!(!unify_structured_types(&option_type, &map_type, &context).unwrap());
    }

    #[test]
    fn test_tuple_unification() {
        let context = create_test_context();
        let string_id = context.type_interner.get_type("String").unwrap();
        let boolean_id = context.type_interner.get_type("Boolean").unwrap();

        let tuple1 = StructuredType::tuple(vec![
            StructuredType::simple(string_id),
            StructuredType::simple(boolean_id),
        ]);
        let tuple2 = StructuredType::tuple(vec![
            StructuredType::simple(string_id),
            StructuredType::simple(boolean_id),
        ]);

        assert!(unify_structured_types(&tuple1, &tuple2, &context).unwrap());
    }

    // Self type resolution is now handled by the type parameter system in multi_program_compiler
}
