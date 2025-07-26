//! Intrinsic function registry for Outrun runtime functions
//!
//! This module defines all the intrinsic functions that are provided by the Outrun runtime
//! and need to be known by the typechecker. These functions are implemented at the runtime
//! level but need type signatures for static type checking.

use crate::dispatch::{FunctionInfo, FunctionRegistry, FunctionVisibility};
use crate::types::{ProtocolId, Type};

/// Register all intrinsic functions in the function registry
pub fn register_intrinsics(registry: &mut FunctionRegistry) {
    // Integer64 intrinsics
    register_integer64_intrinsics(registry);

    // Float64 intrinsics
    register_float64_intrinsics(registry);

    // String intrinsics
    register_string_intrinsics(registry);

    // Boolean intrinsics
    register_boolean_intrinsics(registry);

    // List intrinsics
    register_list_intrinsics(registry);

    // Map intrinsics
    register_map_intrinsics(registry);

    // Atom intrinsics
    register_atom_intrinsics(registry);

    // General intrinsics
    register_general_intrinsics(registry);
}

/// Register Integer64 intrinsic functions
fn register_integer64_intrinsics(registry: &mut FunctionRegistry) {
    let integer64_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Integer64"),
        args: vec![],
        span: None,
    };
    let boolean_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Boolean"),
        args: vec![],
        span: None,
    };
    let string_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.String"),
        args: vec![],
        span: None,
    };
    let option_integer64_type = Type::Protocol {
        id: ProtocolId::new("Option"),
        args: vec![integer64_type.clone()],
        span: None,
    };

    // Arithmetic operations
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_add",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_sub",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_mul",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_div",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        option_integer64_type.clone(),
    ); // Returns Option<Integer64> for division by zero safety

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_mod",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        option_integer64_type.clone(),
    ); // Returns Option<Integer64> for modulo by zero safety

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_pow",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    // Unary operations
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_pos",
        vec![("value", integer64_type.clone())],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_neg",
        vec![("value", integer64_type.clone())],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_abs",
        vec![("value", integer64_type.clone())],
        integer64_type.clone(),
    );

    // Comparison operations
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_eq",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_gt",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_ge",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_lt",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_le",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        boolean_type.clone(),
    );

    // Bitwise operations
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_and",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_or",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_xor",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_not",
        vec![("value", integer64_type.clone())],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_shl",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_shr",
        vec![
            ("lhs", integer64_type.clone()),
            ("rhs", integer64_type.clone()),
        ],
        integer64_type.clone(),
    );

    // String conversion
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "i64_to_string_radix",
        vec![
            ("value", integer64_type.clone()),
            (
                "radix",
                Type::Protocol {
                    id: ProtocolId::new("Integer"),
                    args: vec![],
                    span: None,
                },
            ),
        ],
        string_type.clone(),
    );
}

/// Register Float64 intrinsic functions
fn register_float64_intrinsics(registry: &mut FunctionRegistry) {
    let float64_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Float64"),
        args: vec![],
        span: None,
    };
    let boolean_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Boolean"),
        args: vec![],
        span: None,
    };
    let string_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.String"),
        args: vec![],
        span: None,
    };
    let option_float64_type = Type::Protocol {
        id: ProtocolId::new("Option"),
        args: vec![float64_type.clone()],
        span: None,
    };

    // Arithmetic operations
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_add",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_sub",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_mul",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_div",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        option_float64_type.clone(),
    ); // Returns Option<Float64> for division by zero safety

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_mod",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        option_float64_type.clone(),
    ); // Returns Option<Float64> for modulo by zero safety

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_pow",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        float64_type.clone(),
    );

    // Unary operations
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_pos",
        vec![("value", float64_type.clone())],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_neg",
        vec![("value", float64_type.clone())],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_abs",
        vec![("value", float64_type.clone())],
        float64_type.clone(),
    );

    // Comparison operations
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_eq",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_gt",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_ge",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_lt",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_le",
        vec![("lhs", float64_type.clone()), ("rhs", float64_type.clone())],
        boolean_type.clone(),
    );

    // Float predicates
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_is_finite",
        vec![("value", float64_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_is_infinite",
        vec![("value", float64_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_is_nan",
        vec![("value", float64_type.clone())],
        boolean_type.clone(),
    );

    // Math functions
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_ceil",
        vec![("value", float64_type.clone())],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_ceil_precision",
        vec![
            ("value", float64_type.clone()),
            (
                "precision",
                Type::Protocol {
                    id: ProtocolId::new("Integer"),
                    args: vec![],
                    span: None,
                },
            ),
        ],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_floor",
        vec![("value", float64_type.clone())],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_floor_precision",
        vec![
            ("value", float64_type.clone()),
            (
                "precision",
                Type::Protocol {
                    id: ProtocolId::new("Integer"),
                    args: vec![],
                    span: None,
                },
            ),
        ],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_round",
        vec![("value", float64_type.clone())],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_round_precision",
        vec![
            ("value", float64_type.clone()),
            (
                "precision",
                Type::Protocol {
                    id: ProtocolId::new("Integer"),
                    args: vec![],
                    span: None,
                },
            ),
        ],
        float64_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_trunc",
        vec![("value", float64_type.clone())],
        float64_type.clone(),
    );

    // String conversion
    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "f64_to_string",
        vec![("value", float64_type.clone())],
        string_type.clone(),
    );
}

/// Register Boolean intrinsic functions
fn register_boolean_intrinsics(registry: &mut FunctionRegistry) {
    let boolean_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Boolean"),
        args: vec![],
        span: None,
    };
    let string_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.String"),
        args: vec![],
        span: None,
    };

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "bool_eq",
        vec![("lhs", boolean_type.clone()), ("rhs", boolean_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "bool_to_string",
        vec![("value", boolean_type.clone())],
        string_type.clone(),
    );
}

/// Register String intrinsic functions
fn register_string_intrinsics(registry: &mut FunctionRegistry) {
    let string_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.String"),
        args: vec![],
        span: None,
    };
    let boolean_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Boolean"),
        args: vec![],
        span: None,
    };

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_eq",
        vec![("lhs", string_type.clone()), ("rhs", string_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_concat",
        vec![("lhs", string_type.clone()), ("rhs", string_type.clone())],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_index_of",
        vec![
            ("value", string_type.clone()),
            ("search", string_type.clone()),
        ],
        Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![Type::Protocol {
                id: ProtocolId::new("Integer"),
                args: vec![],
                span: None,
            }],
            span: None,
        },
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_length",
        vec![("value", string_type.clone())],
        Type::Protocol {
            id: ProtocolId::new("Integer"),
            args: vec![],
            span: None,
        },
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_char_at",
        vec![
            ("value", string_type.clone()),
            (
                "index",
                Type::Protocol {
                    id: ProtocolId::new("Integer"),
                    args: vec![],
                    span: None,
                },
            ),
        ],
        Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![string_type.clone()],
            span: None,
        },
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_slice",
        vec![
            ("value", string_type.clone()),
            (
                "start",
                Type::Protocol {
                    id: ProtocolId::new("Integer"),
                    args: vec![],
                    span: None,
                },
            ),
            (
                "end",
                Type::Protocol {
                    id: ProtocolId::new("Integer"),
                    args: vec![],
                    span: None,
                },
            ),
        ],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_to_upper",
        vec![("value", string_type.clone())],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_to_lower",
        vec![("value", string_type.clone())],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_trim",
        vec![("value", string_type.clone())],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_trim_start",
        vec![("value", string_type.clone())],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_trim_end",
        vec![("value", string_type.clone())],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "string_valid_utf8",
        vec![("value", string_type.clone())],
        boolean_type.clone(),
    );
}

/// Register List intrinsic functions
fn register_list_intrinsics(registry: &mut FunctionRegistry) {
    // Note: These are simplified - real implementations would be generic
    let any_type = Type::Protocol {
        id: ProtocolId::new("Any"),
        args: vec![],
        span: None,
    };
    let integer_type = Type::Protocol {
        id: ProtocolId::new("Integer"),
        args: vec![],
        span: None,
    };

    // List intrinsics with proper return types
    let option_any_type = Type::Protocol {
        id: ProtocolId::new("Option"),
        args: vec![any_type.clone()],
        span: None,
    };

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "list_head",
        vec![("value", any_type.clone())],
        option_any_type,
    ); // Returns Option<Any>

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "list_tail",
        vec![("value", any_type.clone())],
        any_type.clone(),
    ); // Returns Self (List<T>) - the intrinsic system needs enhancement for proper Self support

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "list_prepend",
        vec![("list", any_type.clone()), ("elem", any_type.clone())],
        any_type.clone(),
    ); // Returns Self (List<T>)

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "list_length",
        vec![("value", any_type.clone())],
        integer_type,
    ); // Returns Integer

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "list_empty",
        vec![],
        any_type.clone(),
    ); // Returns Self (List<T>)
}

/// Register Map intrinsic functions
fn register_map_intrinsics(registry: &mut FunctionRegistry) {
    // Note: These are simplified
    let any_type = Type::Protocol {
        id: ProtocolId::new("Any"),
        args: vec![],
        span: None,
    };

    let integer_type = Type::Protocol {
        id: ProtocolId::new("Integer"),
        args: vec![],
        span: None,
    };
    let boolean_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Boolean"),
        args: vec![],
        span: None,
    };

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "map_get",
        vec![("map", any_type.clone()), ("key", any_type.clone())],
        Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![any_type.clone()],
            span: None,
        },
    ); // Returns Option<Any> - intrinsic system needs enhancement for proper generics

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "map_put",
        vec![
            ("map", any_type.clone()),
            ("key", any_type.clone()),
            ("value", any_type.clone()),
        ],
        any_type.clone(),
    ); // Returns Self (Map<K,V>)

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "map_remove",
        vec![("map", any_type.clone()), ("key", any_type.clone())],
        any_type.clone(),
    ); // Returns Self (Map<K,V>)

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "map_size",
        vec![("map", any_type.clone())],
        integer_type,
    ); // Returns Integer

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "map_equal",
        vec![("lhs", any_type.clone()), ("rhs", any_type.clone())],
        boolean_type,
    ); // Returns Boolean
}

/// Register Atom intrinsic functions
fn register_atom_intrinsics(registry: &mut FunctionRegistry) {
    let atom_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Atom"),
        args: vec![],
        span: None,
    };
    let boolean_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.Boolean"),
        args: vec![],
        span: None,
    };
    let string_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.String"),
        args: vec![],
        span: None,
    };

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "atom_eq",
        vec![("lhs", atom_type.clone()), ("rhs", atom_type.clone())],
        boolean_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "atom_to_string",
        vec![("value", atom_type.clone())],
        string_type,
    );
}

/// Register general intrinsic functions
fn register_general_intrinsics(registry: &mut FunctionRegistry) {
    let any_type = Type::Protocol {
        id: ProtocolId::new("Any"),
        args: vec![],
        span: None,
    };
    let string_type = Type::Concrete {
        id: crate::types::TypeId::new("Outrun.Core.String"),
        args: vec![],
        span: None,
    };

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "inspect_value",
        vec![("value", any_type.clone())],
        string_type.clone(),
    );

    // Binary operations (these work on String which implements Binary protocol)
    let integer_type = Type::Protocol {
        id: ProtocolId::new("Integer"),
        args: vec![],
        span: None,
    };
    let option_integer_type = Type::Protocol {
        id: ProtocolId::new("Option"),
        args: vec![integer_type.clone()],
        span: None,
    };

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "binary_byte_size",
        vec![("value", string_type.clone())],
        integer_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "binary_byte_at",
        vec![
            ("value", string_type.clone()),
            ("index", integer_type.clone()),
        ],
        option_integer_type,
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "binary_slice",
        vec![
            ("value", string_type.clone()),
            ("start", integer_type.clone()),
            ("end", integer_type.clone()),
        ],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "binary_concat",
        vec![("lhs", string_type.clone()), ("rhs", string_type.clone())],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "binary_index_of",
        vec![
            ("value", string_type.clone()),
            ("search", string_type.clone()),
        ],
        Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![integer_type.clone()],
            span: None,
        },
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "binary_to_hex",
        vec![("value", string_type.clone())],
        string_type.clone(),
    );

    register_intrinsic(
        registry,
        "Outrun.Intrinsic",
        "binary_from_hex",
        vec![("value", string_type.clone())],
        Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![string_type.clone()],
            span: None,
        },
    );
}

/// Helper function to register a single intrinsic function
fn register_intrinsic(
    registry: &mut FunctionRegistry,
    module: &str,
    name: &str,
    parameters: Vec<(&str, Type)>,
    return_type: Type,
) {
    let function_info = FunctionInfo {
        defining_scope: module.to_string(),
        function_name: name.to_string(),
        visibility: FunctionVisibility::Public,
        parameters: parameters
            .into_iter()
            .map(|(name, ty)| (name.to_string(), ty))
            .collect(),
        return_type,
        body: None, // Intrinsic functions don't have user-defined bodies
        span: None,
        generic_parameters: Vec::new(), // Intrinsics are not generic
        is_generic: false,
    };

    registry.register_function(module.to_string(), name.to_string(), function_info);
}
