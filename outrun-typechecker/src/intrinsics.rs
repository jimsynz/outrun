//! Intrinsic function registry for compiler-implemented functions
//!
//! This module provides all the function signatures for operations that are
//! implemented by the compiler runtime rather than in Outrun code.

use crate::types::AtomId;
use crate::unification::UnificationContext;
use outrun_parser::{
    FunctionDefinition, GenericArgs, Identifier, Parameter, Span, TypeAnnotation, TypeIdentifier,
};
use std::collections::HashMap;

/// Definition of a generic intrinsic function using proper AST types
#[derive(Debug, Clone)]
pub struct GenericIntrinsicDef {
    pub name: String,
    pub generic_params: Vec<String>,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeAnnotation,
}

/// All type patterns needed for intrinsics
pub struct IntrinsicTypes {
    // Basic types
    pub atom: TypeAnnotation,
    pub boolean: TypeAnnotation,
    pub integer64: TypeAnnotation,
    pub float64: TypeAnnotation,
    pub string: TypeAnnotation,
    pub list: TypeAnnotation,
    pub map: TypeAnnotation,
    pub any: TypeAnnotation,

    // Generic types
    pub list_t: TypeAnnotation, // Outrun.Core.List<T>
    pub map_kv: TypeAnnotation, // Outrun.Core.Map<K, V>

    // Option variants
    pub option_any: TypeAnnotation,
    pub option_integer64: TypeAnnotation,
    pub option_string: TypeAnnotation,
    pub option_float64: TypeAnnotation,
}

/// Registry of all intrinsic function signatures
pub struct IntrinsicRegistry;

impl IntrinsicRegistry {
    /// Create helper functions to build AST nodes
    fn create_type_annotation(
        type_name: &str,
        generic_args: Vec<TypeAnnotation>,
    ) -> TypeAnnotation {
        let span = Span::new(0, 0); // Dummy span for intrinsics

        let generic_args_option = if generic_args.is_empty() {
            None
        } else {
            Some(GenericArgs {
                args: generic_args,
                span,
            })
        };

        TypeAnnotation::Simple {
            path: vec![TypeIdentifier {
                name: type_name.to_string(),
                span,
            }],
            generic_args: generic_args_option,
            span,
        }
    }

    fn create_parameter(name: &str, type_annotation: TypeAnnotation) -> Parameter {
        Parameter {
            name: Identifier {
                name: name.to_string(),
                span: Span::new(0, 0),
            },
            type_annotation,
            span: Span::new(0, 0),
        }
    }

    /// Register all types needed for intrinsics in the unification context
    fn register_intrinsic_types(context: &mut UnificationContext) -> IntrinsicTypes {
        let span = Span::new(0, 0);

        // Register base types in type interner
        let _ = context.type_interner.intern_type("Option");
        let _ = context.type_interner.intern_type("List");
        let _ = context.type_interner.intern_type("Map");
        let _ = context.type_interner.intern_type("Any");
        let _ = context.type_interner.intern_type("Outrun.Core.Integer64");
        let _ = context.type_interner.intern_type("Outrun.Core.String");
        let _ = context.type_interner.intern_type("Outrun.Core.Float64");
        let _ = context.type_interner.intern_type("Outrun.Core.Atom");
        let _ = context.type_interner.intern_type("Outrun.Core.Boolean");
        let _ = context.type_interner.intern_type("Outrun.Core.List");
        let _ = context.type_interner.intern_type("Outrun.Core.Map");

        // Helper function to create simple type
        let simple_type = |name: &str| TypeAnnotation::Simple {
            path: vec![TypeIdentifier {
                name: name.to_string(),
                span,
            }],
            generic_args: None,
            span,
        };

        // Helper function to create Option<T>
        let option_type = |inner: TypeAnnotation| TypeAnnotation::Simple {
            path: vec![TypeIdentifier {
                name: "Option".to_string(),
                span,
            }],
            generic_args: Some(GenericArgs {
                args: vec![inner],
                span,
            }),
            span,
        };

        // Build all basic types
        let atom = simple_type("Outrun.Core.Atom");
        let boolean = simple_type("Outrun.Core.Boolean");
        let integer64 = simple_type("Outrun.Core.Integer64");
        let float64 = simple_type("Outrun.Core.Float64");
        let string = simple_type("Outrun.Core.String");
        let list = simple_type("Outrun.Core.List");
        let map = simple_type("Outrun.Core.Map");
        let any = simple_type("Any");

        // Build Option variants
        let option_any = option_type(any.clone());
        let option_integer64 = option_type(integer64.clone());
        let option_string = option_type(string.clone());
        let option_float64 = option_type(float64.clone());

        // Build generic types using Any as the type parameter (since all types implement Any)
        let list_t = TypeAnnotation::Simple {
            path: vec![TypeIdentifier {
                name: "Outrun.Core.List".to_string(),
                span,
            }],
            generic_args: Some(GenericArgs {
                args: vec![any.clone()],
                span,
            }),
            span,
        };
        let map_kv = TypeAnnotation::Simple {
            path: vec![TypeIdentifier {
                name: "Outrun.Core.Map".to_string(),
                span,
            }],
            generic_args: Some(GenericArgs {
                args: vec![any.clone(), any.clone()],
                span,
            }),
            span,
        };

        IntrinsicTypes {
            atom,
            boolean,
            integer64,
            float64,
            string,
            list,
            map,
            any,
            list_t,
            map_kv,
            option_any,
            option_integer64,
            option_string,
            option_float64,
        }
    }

    /// Get all generic intrinsic function definitions using proper AST
    pub fn get_generic_intrinsics() -> HashMap<String, GenericIntrinsicDef> {
        let mut intrinsics = HashMap::new();

        // Type T for lists
        let t_type = Self::create_type_annotation("T", vec![]);

        // List<T> type (using concrete type name)
        let list_t_type = Self::create_type_annotation("Outrun.Core.List", vec![t_type.clone()]);

        // Option<T> type
        let option_t_type = Self::create_type_annotation("Option", vec![t_type.clone()]);

        // List operations with proper generics
        intrinsics.insert(
            "Outrun.Intrinsic.list_empty".to_string(),
            GenericIntrinsicDef {
                name: "list_empty".to_string(),
                generic_params: vec!["T".to_string()],
                parameters: vec![],
                return_type: list_t_type.clone(),
            },
        );

        intrinsics.insert(
            "Outrun.Intrinsic.list_head".to_string(),
            GenericIntrinsicDef {
                name: "list_head".to_string(),
                generic_params: vec!["T".to_string()],
                parameters: vec![Self::create_parameter("value", list_t_type.clone())],
                return_type: option_t_type.clone(),
            },
        );

        intrinsics.insert(
            "Outrun.Intrinsic.list_tail".to_string(),
            GenericIntrinsicDef {
                name: "list_tail".to_string(),
                generic_params: vec!["T".to_string()],
                parameters: vec![Self::create_parameter("value", list_t_type.clone())],
                return_type: list_t_type.clone(),
            },
        );

        intrinsics.insert(
            "Outrun.Intrinsic.list_prepend".to_string(),
            GenericIntrinsicDef {
                name: "list_prepend".to_string(),
                generic_params: vec!["T".to_string()],
                parameters: vec![
                    Self::create_parameter("list", list_t_type.clone()),
                    Self::create_parameter("elem", t_type.clone()),
                ],
                return_type: list_t_type.clone(),
            },
        );

        // Map operations with K, V generics
        let k_type = Self::create_type_annotation("K", vec![]);
        let v_type = Self::create_type_annotation("V", vec![]);
        let map_kv_type =
            Self::create_type_annotation("Outrun.Core.Map", vec![k_type.clone(), v_type.clone()]);
        let option_v_type = Self::create_type_annotation("Option", vec![v_type.clone()]);

        intrinsics.insert(
            "Outrun.Intrinsic.map_get".to_string(),
            GenericIntrinsicDef {
                name: "map_get".to_string(),
                generic_params: vec!["K".to_string(), "V".to_string()],
                parameters: vec![
                    Self::create_parameter("map", map_kv_type.clone()),
                    Self::create_parameter("key", k_type.clone()),
                ],
                return_type: option_v_type,
            },
        );

        intrinsics.insert(
            "Outrun.Intrinsic.map_put".to_string(),
            GenericIntrinsicDef {
                name: "map_put".to_string(),
                generic_params: vec!["K".to_string(), "V".to_string()],
                parameters: vec![
                    Self::create_parameter("map", map_kv_type.clone()),
                    Self::create_parameter("key", k_type.clone()),
                    Self::create_parameter("value", v_type.clone()),
                ],
                return_type: map_kv_type.clone(),
            },
        );

        // Add missing intrinsic functions needed by core library
        let atom_type = Self::create_type_annotation("Outrun.Core.Atom", vec![]);
        let boolean_type = Self::create_type_annotation("Outrun.Core.Boolean", vec![]);
        let integer64_type = Self::create_type_annotation("Outrun.Core.Integer64", vec![]);
        let float64_type = Self::create_type_annotation("Outrun.Core.Float64", vec![]);
        let string_type = Self::create_type_annotation("Outrun.Core.String", vec![]);

        // Atom operations
        intrinsics.insert(
            "Outrun.Intrinsic.atom_eq".to_string(),
            GenericIntrinsicDef {
                name: "atom_eq".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", atom_type.clone()),
                    Self::create_parameter("rhs", atom_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.atom_to_string".to_string(),
            GenericIntrinsicDef {
                name: "atom_to_string".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", atom_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.atom_inspect".to_string(),
            GenericIntrinsicDef {
                name: "atom_inspect".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", atom_type)],
                return_type: string_type.clone(),
            },
        );

        // Boolean operations
        intrinsics.insert(
            "Outrun.Intrinsic.bool_eq".to_string(),
            GenericIntrinsicDef {
                name: "bool_eq".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", boolean_type.clone()),
                    Self::create_parameter("rhs", boolean_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.bool_and".to_string(),
            GenericIntrinsicDef {
                name: "bool_and".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", boolean_type.clone()),
                    Self::create_parameter("rhs", boolean_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.bool_or".to_string(),
            GenericIntrinsicDef {
                name: "bool_or".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", boolean_type.clone()),
                    Self::create_parameter("rhs", boolean_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.bool_not".to_string(),
            GenericIntrinsicDef {
                name: "bool_not".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", boolean_type.clone())],
                return_type: boolean_type.clone(),
            },
        );

        // Integer operations (basic set)
        intrinsics.insert(
            "Outrun.Intrinsic.i64_add".to_string(),
            GenericIntrinsicDef {
                name: "i64_add".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_sub".to_string(),
            GenericIntrinsicDef {
                name: "i64_sub".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_mul".to_string(),
            GenericIntrinsicDef {
                name: "i64_mul".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_div".to_string(),
            GenericIntrinsicDef {
                name: "i64_div".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: Self::create_type_annotation("Option", vec![integer64_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_mod".to_string(),
            GenericIntrinsicDef {
                name: "i64_mod".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: Self::create_type_annotation("Option", vec![integer64_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_pow".to_string(),
            GenericIntrinsicDef {
                name: "i64_pow".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_pos".to_string(),
            GenericIntrinsicDef {
                name: "i64_pos".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", integer64_type.clone())],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_neg".to_string(),
            GenericIntrinsicDef {
                name: "i64_neg".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", integer64_type.clone())],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_eq".to_string(),
            GenericIntrinsicDef {
                name: "i64_eq".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_gt".to_string(),
            GenericIntrinsicDef {
                name: "i64_gt".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_ge".to_string(),
            GenericIntrinsicDef {
                name: "i64_ge".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_lt".to_string(),
            GenericIntrinsicDef {
                name: "i64_lt".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_le".to_string(),
            GenericIntrinsicDef {
                name: "i64_le".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_to_string".to_string(),
            GenericIntrinsicDef {
                name: "i64_to_string".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", integer64_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_inspect".to_string(),
            GenericIntrinsicDef {
                name: "i64_inspect".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", integer64_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_to_string_radix".to_string(),
            GenericIntrinsicDef {
                name: "i64_to_string_radix".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("value", integer64_type.clone()),
                    Self::create_parameter("radix", integer64_type.clone()),
                ],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_abs".to_string(),
            GenericIntrinsicDef {
                name: "i64_abs".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", integer64_type.clone())],
                return_type: integer64_type.clone(),
            },
        );

        // Bitwise integer operations
        intrinsics.insert(
            "Outrun.Intrinsic.i64_and".to_string(),
            GenericIntrinsicDef {
                name: "i64_and".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_or".to_string(),
            GenericIntrinsicDef {
                name: "i64_or".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_xor".to_string(),
            GenericIntrinsicDef {
                name: "i64_xor".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_not".to_string(),
            GenericIntrinsicDef {
                name: "i64_not".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", integer64_type.clone())],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_shl".to_string(),
            GenericIntrinsicDef {
                name: "i64_shl".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.i64_shr".to_string(),
            GenericIntrinsicDef {
                name: "i64_shr".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", integer64_type.clone()),
                    Self::create_parameter("rhs", integer64_type.clone()),
                ],
                return_type: integer64_type.clone(),
            },
        );

        // Float operations (basic set)
        intrinsics.insert(
            "Outrun.Intrinsic.f64_add".to_string(),
            GenericIntrinsicDef {
                name: "f64_add".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_sub".to_string(),
            GenericIntrinsicDef {
                name: "f64_sub".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_mul".to_string(),
            GenericIntrinsicDef {
                name: "f64_mul".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_div".to_string(),
            GenericIntrinsicDef {
                name: "f64_div".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: Self::create_type_annotation("Option", vec![float64_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_mod".to_string(),
            GenericIntrinsicDef {
                name: "f64_mod".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: Self::create_type_annotation("Option", vec![float64_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_pow".to_string(),
            GenericIntrinsicDef {
                name: "f64_pow".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_pos".to_string(),
            GenericIntrinsicDef {
                name: "f64_pos".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_neg".to_string(),
            GenericIntrinsicDef {
                name: "f64_neg".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_eq".to_string(),
            GenericIntrinsicDef {
                name: "f64_eq".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_gt".to_string(),
            GenericIntrinsicDef {
                name: "f64_gt".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_ge".to_string(),
            GenericIntrinsicDef {
                name: "f64_ge".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_lt".to_string(),
            GenericIntrinsicDef {
                name: "f64_lt".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_le".to_string(),
            GenericIntrinsicDef {
                name: "f64_le".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", float64_type.clone()),
                    Self::create_parameter("rhs", float64_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_to_string".to_string(),
            GenericIntrinsicDef {
                name: "f64_to_string".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_inspect".to_string(),
            GenericIntrinsicDef {
                name: "f64_inspect".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_abs".to_string(),
            GenericIntrinsicDef {
                name: "f64_abs".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_is_nan".to_string(),
            GenericIntrinsicDef {
                name: "f64_is_nan".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_is_infinite".to_string(),
            GenericIntrinsicDef {
                name: "f64_is_infinite".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_is_finite".to_string(),
            GenericIntrinsicDef {
                name: "f64_is_finite".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_ceil".to_string(),
            GenericIntrinsicDef {
                name: "f64_ceil".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_floor".to_string(),
            GenericIntrinsicDef {
                name: "f64_floor".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_round".to_string(),
            GenericIntrinsicDef {
                name: "f64_round".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_trunc".to_string(),
            GenericIntrinsicDef {
                name: "f64_trunc".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_fract".to_string(),
            GenericIntrinsicDef {
                name: "f64_fract".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", float64_type.clone())],
                return_type: float64_type.clone(),
            },
        );

        // Precision float operations as mentioned in TYPECHECKER_REWRITE_PLAN.md
        intrinsics.insert(
            "Outrun.Intrinsic.f64_ceil_precision".to_string(),
            GenericIntrinsicDef {
                name: "f64_ceil_precision".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("value", float64_type.clone()),
                    Self::create_parameter("precision", integer64_type.clone()),
                ],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_floor_precision".to_string(),
            GenericIntrinsicDef {
                name: "f64_floor_precision".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("value", float64_type.clone()),
                    Self::create_parameter("precision", integer64_type.clone()),
                ],
                return_type: float64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.f64_round_precision".to_string(),
            GenericIntrinsicDef {
                name: "f64_round_precision".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("value", float64_type.clone()),
                    Self::create_parameter("precision", integer64_type.clone()),
                ],
                return_type: float64_type.clone(),
            },
        );

        // String operations (basic set)
        intrinsics.insert(
            "Outrun.Intrinsic.string_length".to_string(),
            GenericIntrinsicDef {
                name: "string_length".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", string_type.clone())],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_concat".to_string(),
            GenericIntrinsicDef {
                name: "string_concat".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", string_type.clone()),
                    Self::create_parameter("rhs", string_type.clone()),
                ],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_eq".to_string(),
            GenericIntrinsicDef {
                name: "string_eq".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", string_type.clone()),
                    Self::create_parameter("rhs", string_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_inspect".to_string(),
            GenericIntrinsicDef {
                name: "string_inspect".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", string_type.clone())],
                return_type: string_type.clone(),
            },
        );

        // Additional String operations needed by core library
        intrinsics.insert(
            "Outrun.Intrinsic.string_char_at".to_string(),
            GenericIntrinsicDef {
                name: "string_char_at".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("string", string_type.clone()),
                    Self::create_parameter("index", integer64_type.clone()),
                ],
                return_type: Self::create_type_annotation("Option", vec![string_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_slice".to_string(),
            GenericIntrinsicDef {
                name: "string_slice".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("string", string_type.clone()),
                    Self::create_parameter("start", integer64_type.clone()),
                    Self::create_parameter("end", integer64_type.clone()),
                ],
                return_type: string_type.clone(),
            },
        );

        // Additional string operations needed by core library
        intrinsics.insert(
            "Outrun.Intrinsic.string_index_of".to_string(),
            GenericIntrinsicDef {
                name: "string_index_of".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("haystack", string_type.clone()),
                    Self::create_parameter("needle", string_type.clone()),
                ],
                return_type: Self::create_type_annotation("Option", vec![integer64_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_to_upper".to_string(),
            GenericIntrinsicDef {
                name: "string_to_upper".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", string_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_to_lower".to_string(),
            GenericIntrinsicDef {
                name: "string_to_lower".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", string_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_trim".to_string(),
            GenericIntrinsicDef {
                name: "string_trim".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", string_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_trim_start".to_string(),
            GenericIntrinsicDef {
                name: "string_trim_start".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", string_type.clone())],
                return_type: string_type.clone(),
            },
        );

        // Binary operations needed by core library (implemented by String)
        intrinsics.insert(
            "Outrun.Intrinsic.binary_byte_at".to_string(),
            GenericIntrinsicDef {
                name: "binary_byte_at".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("binary", string_type.clone()),
                    Self::create_parameter("index", integer64_type.clone()),
                ],
                return_type: Self::create_type_annotation("Option", vec![integer64_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.binary_byte_size".to_string(),
            GenericIntrinsicDef {
                name: "binary_byte_size".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("binary", string_type.clone())],
                return_type: integer64_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.binary_slice".to_string(),
            GenericIntrinsicDef {
                name: "binary_slice".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("binary", string_type.clone()),
                    Self::create_parameter("start", integer64_type.clone()),
                    Self::create_parameter("end", integer64_type.clone()),
                ],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.binary_concat".to_string(),
            GenericIntrinsicDef {
                name: "binary_concat".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("lhs", string_type.clone()),
                    Self::create_parameter("rhs", string_type.clone()),
                ],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.binary_index_of".to_string(),
            GenericIntrinsicDef {
                name: "binary_index_of".to_string(),
                generic_params: vec![],
                parameters: vec![
                    Self::create_parameter("haystack", string_type.clone()),
                    Self::create_parameter("needle", string_type.clone()),
                ],
                return_type: Self::create_type_annotation("Option", vec![integer64_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.binary_to_hex".to_string(),
            GenericIntrinsicDef {
                name: "binary_to_hex".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("binary", string_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_trim_end".to_string(),
            GenericIntrinsicDef {
                name: "string_trim_end".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", string_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.string_valid_utf8".to_string(),
            GenericIntrinsicDef {
                name: "string_valid_utf8".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", string_type.clone())],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.binary_from_hex".to_string(),
            GenericIntrinsicDef {
                name: "binary_from_hex".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("hex", string_type.clone())],
                return_type: Self::create_type_annotation("Option", vec![string_type.clone()]),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.panic_unwrap_none".to_string(),
            GenericIntrinsicDef {
                name: "panic_unwrap_none".to_string(),
                generic_params: vec!["T".to_string()],
                parameters: vec![Self::create_parameter("message", string_type.clone())],
                return_type: Self::create_type_annotation("T", vec![]),
            },
        );

        // List operations needed by core library
        intrinsics.insert(
            "Outrun.Intrinsic.list_inspect".to_string(),
            GenericIntrinsicDef {
                name: "list_inspect".to_string(),
                generic_params: vec!["T".to_string()],
                parameters: vec![Self::create_parameter("value", list_t_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.list_length".to_string(),
            GenericIntrinsicDef {
                name: "list_length".to_string(),
                generic_params: vec!["T".to_string()],
                parameters: vec![Self::create_parameter("value", list_t_type.clone())],
                return_type: integer64_type.clone(),
            },
        );

        // Map operations
        intrinsics.insert(
            "Outrun.Intrinsic.map_inspect".to_string(),
            GenericIntrinsicDef {
                name: "map_inspect".to_string(),
                generic_params: vec!["K".to_string(), "V".to_string()],
                parameters: vec![Self::create_parameter("map", map_kv_type.clone())],
                return_type: string_type.clone(),
            },
        );

        // Additional Map operations needed by core library
        intrinsics.insert(
            "Outrun.Intrinsic.map_equal".to_string(),
            GenericIntrinsicDef {
                name: "map_equal".to_string(),
                generic_params: vec!["K".to_string(), "V".to_string()],
                parameters: vec![
                    Self::create_parameter("lhs", map_kv_type.clone()),
                    Self::create_parameter("rhs", map_kv_type.clone()),
                ],
                return_type: boolean_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.map_remove".to_string(),
            GenericIntrinsicDef {
                name: "map_remove".to_string(),
                generic_params: vec!["K".to_string(), "V".to_string()],
                parameters: vec![
                    Self::create_parameter("map", map_kv_type.clone()),
                    Self::create_parameter("key", k_type.clone()),
                ],
                return_type: map_kv_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.map_size".to_string(),
            GenericIntrinsicDef {
                name: "map_size".to_string(),
                generic_params: vec!["K".to_string(), "V".to_string()],
                parameters: vec![Self::create_parameter("map", map_kv_type.clone())],
                return_type: integer64_type.clone(),
            },
        );

        // Option operations for inspect - need specific functions for Option types
        let option_none_type = Self::create_type_annotation("Outrun.Option.None", vec![]);
        let option_some_type =
            Self::create_type_annotation("Outrun.Option.Some", vec![t_type.clone()]);

        intrinsics.insert(
            "Outrun.Intrinsic.option_none_inspect".to_string(),
            GenericIntrinsicDef {
                name: "option_none_inspect".to_string(),
                generic_params: vec![],
                parameters: vec![Self::create_parameter("value", option_none_type.clone())],
                return_type: string_type.clone(),
            },
        );
        intrinsics.insert(
            "Outrun.Intrinsic.option_some_inspect".to_string(),
            GenericIntrinsicDef {
                name: "option_some_inspect".to_string(),
                generic_params: vec!["T".to_string()],
                parameters: vec![Self::create_parameter("value", option_some_type.clone())],
                return_type: string_type.clone(),
            },
        );

        intrinsics
    }

    /// Setup core concrete types and automatically implement Any trait
    pub fn setup_core_types(context: &mut UnificationContext) {
        // Get the Any trait ID
        let any_trait_id = context.type_interner.intern_type("Any");

        // Register Any as a trait
        context.trait_registry.register_trait(any_trait_id);

        // All concrete types automatically implement Any
        let concrete_types = vec![
            "Outrun.Core.Atom",
            "Outrun.Core.Boolean",
            "Outrun.Core.Integer64",
            "Outrun.Core.Float64",
            "Outrun.Core.String",
            "Outrun.Core.List",
            "Outrun.Core.Map",
        ];

        for type_name in concrete_types {
            let type_id = context.type_interner.intern_type(type_name);
            context
                .trait_registry
                .register_concrete_type(type_id, any_trait_id);
        }
    }

    /// Create all intrinsic function signatures and return them as a HashMap
    pub fn create_all_signatures(
        context: &mut UnificationContext,
    ) -> HashMap<AtomId, FunctionDefinition> {
        let mut intrinsic_functions = HashMap::new();

        // Register all intrinsic types first
        let _types = Self::register_intrinsic_types(context);

        // Use the proper generic intrinsic system
        let generic_intrinsics = Self::get_generic_intrinsics();

        for (name, generic_def) in generic_intrinsics {
            let function_def =
                Self::convert_generic_intrinsic_to_function_definition(generic_def, context);

            let function_atom_id = context.type_interner.intern_atom(&name);
            intrinsic_functions.insert(function_atom_id, function_def);
        }

        intrinsic_functions
    }

    /// Get generic parameters for a specific intrinsic function by name
    pub fn get_intrinsic_generic_params(function_name: &str) -> Vec<String> {
        let generic_intrinsics = Self::get_generic_intrinsics();
        if let Some(intrinsic_def) = generic_intrinsics.get(function_name) {
            intrinsic_def.generic_params.clone()
        } else {
            vec![]
        }
    }

    /// Convert a generic intrinsic definition to a FunctionDefinition for the function registry
    fn convert_generic_intrinsic_to_function_definition(
        generic_def: GenericIntrinsicDef,
        _context: &mut UnificationContext,
    ) -> FunctionDefinition {
        // The parameters are already in the correct format (Parameter struct)
        let parameters = generic_def.parameters;

        // For now, ignore generic parameters in intrinsics - the type checker will handle them

        FunctionDefinition {
            attributes: vec![],
            visibility: outrun_parser::FunctionVisibility::Public,
            name: outrun_parser::Identifier {
                name: generic_def.name,
                span: outrun_parser::Span::new(0, 0),
            },
            parameters,
            return_type: generic_def.return_type,
            guard: None,
            body: outrun_parser::Block {
                statements: vec![],
                span: outrun_parser::Span::new(0, 0),
            },
            span: outrun_parser::Span::new(0, 0),
        }
    }
}
