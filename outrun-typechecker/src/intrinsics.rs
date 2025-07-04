//! Minimal intrinsic function bootstrap for compiler-implemented functions
//!
//! This module provides a single bootstrap function that populates a CompilerEnvironment
//! with all intrinsic function definitions in the Outrun.Intrinsic module.

use crate::compilation::compiler_environment::{
    CompilerEnvironment, ModuleKey, ModuleKind, SourceLocation, UnifiedFunctionEntry,
};
use crate::unification::StructuredType;
use lazy_static::lazy_static;
use outrun_parser::{
    Block, FunctionDefinition, FunctionVisibility, GenericArgs, Identifier, Parameter, Span,
    TypeAnnotation, TypeIdentifier,
};

// Lazy static cache of all intrinsic function definitions
// This avoids recreating 85 complex FunctionDefinition structures on every bootstrap
lazy_static! {
    static ref INTRINSIC_FUNCTIONS: Vec<FunctionDefinition> = build_intrinsic_function_definitions();
    
    // Also cache the typed definitions to avoid recreating them every time
    static ref TYPED_INTRINSIC_FUNCTIONS: Vec<crate::checker::TypedFunctionDefinition> = 
        INTRINSIC_FUNCTIONS.iter().map(create_typed_definition_for_intrinsic).collect();
}

/// Get all intrinsic function definitions 
/// Returns a reference to the cached intrinsic function definitions
pub fn get_intrinsic_function_definitions() -> &'static Vec<FunctionDefinition> {
    &INTRINSIC_FUNCTIONS
}

/// Bootstrap intrinsic functions into a CompilerEnvironment
/// Returns the updated environment with Outrun.Intrinsic module populated
pub fn bootstrap_intrinsics(env: CompilerEnvironment) -> CompilerEnvironment {
    // Create Outrun.Intrinsic module
    let intrinsic_module_type = env.intern_type_name("Outrun.Intrinsic");
    let module_key = ModuleKey::Module(intrinsic_module_type.hash);
    let structured_type = StructuredType::Simple(intrinsic_module_type.clone());

    env.get_or_create_module(
        module_key.clone(),
        ModuleKind::Struct,
        SourceLocation::Input("intrinsics".to_string()),
        structured_type.clone(),
    );

    // Add all intrinsic function definitions using cached data
    for (func_def, typed_definition) in INTRINSIC_FUNCTIONS.iter().zip(TYPED_INTRINSIC_FUNCTIONS.iter()) {
        let function_name = env.intern_atom_name(&func_def.name.name);

        let function_entry = UnifiedFunctionEntry::Intrinsic {
            definition: func_def.clone(),
            typed_definition: Some(typed_definition.clone()),
            function_id: format!("Outrun.Intrinsic.{}", func_def.name.name),
            is_guard: func_def.name.name.ends_with('?'),
        };

        env.add_unified_function_to_module(
            module_key.clone(),
            structured_type.clone(),
            function_name,
            function_entry,
        );
    }

    env
}

/// Create a lightweight typed definition for an intrinsic function with minimal allocations
fn create_typed_definition_for_intrinsic(
    func_def: &FunctionDefinition,
) -> crate::checker::TypedFunctionDefinition {
    use crate::checker::*;
    
    const ZERO_SPAN: Span = Span { start: 0, end: 0, start_line_col: Some((0, 0)), end_line_col: Some((0, 0)) };

    // Pre-allocate typed parameters with exact capacity for performance
    let mut typed_parameters = Vec::with_capacity(func_def.parameters.len());
    for param in &func_def.parameters {
        typed_parameters.push(TypedParameter {
            name: param.name.name.clone(),
            param_type: None, // TODO: Convert type annotations properly
            span: ZERO_SPAN,
        });
    }

    // Create minimal empty body for intrinsics
    let empty_body = TypedBlock {
        statements: Vec::new(), // Empty statements for intrinsics
        result_type: None,
        span: ZERO_SPAN,
    };

    TypedFunctionDefinition {
        name: func_def.name.name.clone(),
        parameters: typed_parameters,
        return_type: None, // TODO: Convert return type properly
        guard: None,       // Intrinsics don't have guards
        body: empty_body,
        function_id: format!("Outrun.Intrinsic.{}", func_def.name.name),
        span: ZERO_SPAN,
    }
}

/// Build all intrinsic function definitions (called once via lazy_static)
/// These reuse the existing FunctionDefinition structure from the parser  
fn build_intrinsic_function_definitions() -> Vec<FunctionDefinition> {
    vec![
        // List operations
        create_function_def(
            "list_empty",
            vec![],
            create_generic_type("Outrun.Core.List", vec!["T"]),
        ),
        create_function_def(
            "list_head",
            vec![("value", create_generic_type("Outrun.Core.List", vec!["T"]))],
            create_generic_type("Option", vec!["T"]),
        ),
        create_function_def(
            "list_tail",
            vec![("value", create_generic_type("Outrun.Core.List", vec!["T"]))],
            create_generic_type("Outrun.Core.List", vec!["T"]),
        ),
        create_function_def(
            "list_prepend",
            vec![
                ("list", create_generic_type("Outrun.Core.List", vec!["T"])),
                ("elem", create_simple_type("T")),
            ],
            create_generic_type("Outrun.Core.List", vec!["T"]),
        ),
        create_function_def(
            "list_length",
            vec![("value", create_generic_type("Outrun.Core.List", vec!["T"]))],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        // Map operations
        create_function_def(
            "map_get",
            vec![
                (
                    "map",
                    create_generic_type("Outrun.Core.Map", vec!["K", "V"]),
                ),
                ("key", create_simple_type("K")),
            ],
            create_generic_type("Option", vec!["V"]),
        ),
        create_function_def(
            "map_put",
            vec![
                (
                    "map",
                    create_generic_type("Outrun.Core.Map", vec!["K", "V"]),
                ),
                ("key", create_simple_type("K")),
                ("value", create_simple_type("V")),
            ],
            create_generic_type("Outrun.Core.Map", vec!["K", "V"]),
        ),
        // Integer operations
        create_function_def(
            "i64_add",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_sub",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_mul",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_eq",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "i64_gt",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        // Float operations
        create_function_def(
            "f64_add",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_sub",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Float64"),
        ),
        // Boolean operations
        create_function_def(
            "bool_and",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Boolean")),
                ("rhs", create_simple_type("Outrun.Core.Boolean")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "bool_or",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Boolean")),
                ("rhs", create_simple_type("Outrun.Core.Boolean")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "bool_not",
            vec![("value", create_simple_type("Outrun.Core.Boolean"))],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        // String operations
        create_function_def(
            "string_length",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "string_concat",
            vec![
                ("lhs", create_simple_type("Outrun.Core.String")),
                ("rhs", create_simple_type("Outrun.Core.String")),
            ],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "string_eq",
            vec![
                ("lhs", create_simple_type("Outrun.Core.String")),
                ("rhs", create_simple_type("Outrun.Core.String")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        // Atom operations
        create_function_def(
            "atom_eq",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Atom")),
                ("rhs", create_simple_type("Outrun.Core.Atom")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "atom_to_string",
            vec![("value", create_simple_type("Outrun.Core.Atom"))],
            create_simple_type("Outrun.Core.String"),
        ),
        // Additional integer operations
        create_function_def(
            "i64_div",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_generic_type("Option", vec!["Outrun.Core.Integer64"]),
        ),
        create_function_def(
            "i64_mod",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_generic_type("Option", vec!["Outrun.Core.Integer64"]),
        ),
        create_function_def(
            "i64_pow",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_pos",
            vec![("value", create_simple_type("Outrun.Core.Integer64"))],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_neg",
            vec![("value", create_simple_type("Outrun.Core.Integer64"))],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_ge",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "i64_lt",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "i64_le",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "i64_to_string",
            vec![("value", create_simple_type("Outrun.Core.Integer64"))],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "i64_to_string_radix",
            vec![
                ("value", create_simple_type("Outrun.Core.Integer64")),
                ("radix", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "i64_abs",
            vec![("value", create_simple_type("Outrun.Core.Integer64"))],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        // Bitwise integer operations
        create_function_def(
            "i64_and",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_or",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_xor",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_not",
            vec![("value", create_simple_type("Outrun.Core.Integer64"))],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_shl",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "i64_shr",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Integer64")),
                ("rhs", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        // Additional float operations
        create_function_def(
            "f64_mul",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_div",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_generic_type("Option", vec!["Outrun.Core.Float64"]),
        ),
        create_function_def(
            "f64_mod",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_generic_type("Option", vec!["Outrun.Core.Float64"]),
        ),
        create_function_def(
            "f64_pow",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_pos",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_neg",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_eq",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "f64_gt",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "f64_ge",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "f64_lt",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "f64_le",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Float64")),
                ("rhs", create_simple_type("Outrun.Core.Float64")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "f64_to_string",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "f64_abs",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_is_nan",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "f64_is_infinite",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "f64_is_finite",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "f64_ceil",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_floor",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_round",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_trunc",
            vec![("value", create_simple_type("Outrun.Core.Float64"))],
            create_simple_type("Outrun.Core.Float64"),
        ),
        // Additional string operations needed by tests
        create_function_def(
            "string_char_at",
            vec![
                ("value", create_simple_type("Outrun.Core.String")),
                ("index", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_generic_type("Option", vec!["Outrun.Core.String"]),
        ),
        create_function_def(
            "string_slice",
            vec![
                ("value", create_simple_type("Outrun.Core.String")),
                ("start", create_simple_type("Outrun.Core.Integer64")),
                ("end", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "string_index_of",
            vec![
                ("value", create_simple_type("Outrun.Core.String")),
                ("search", create_simple_type("Outrun.Core.String")),
            ],
            create_generic_type("Option", vec!["Outrun.Core.Integer64"]),
        ),
        create_function_def(
            "string_to_upper",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "string_to_lower",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "string_trim",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "string_trim_start",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "string_trim_end",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "string_valid_utf8",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        // Binary operations
        create_function_def(
            "binary_byte_at",
            vec![
                ("value", create_simple_type("Outrun.Core.String")),
                ("index", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_generic_type("Option", vec!["Outrun.Core.Integer64"]),
        ),
        create_function_def(
            "binary_byte_size",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        create_function_def(
            "binary_slice",
            vec![
                ("value", create_simple_type("Outrun.Core.String")),
                ("start", create_simple_type("Outrun.Core.Integer64")),
                ("end", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "binary_concat",
            vec![
                ("lhs", create_simple_type("Outrun.Core.String")),
                ("rhs", create_simple_type("Outrun.Core.String")),
            ],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "binary_index_of",
            vec![
                ("value", create_simple_type("Outrun.Core.String")),
                ("search", create_simple_type("Outrun.Core.String")),
            ],
            create_generic_type("Option", vec!["Outrun.Core.Integer64"]),
        ),
        create_function_def(
            "binary_to_hex",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_simple_type("Outrun.Core.String"),
        ),
        create_function_def(
            "binary_from_hex",
            vec![("value", create_simple_type("Outrun.Core.String"))],
            create_generic_type("Option", vec!["Outrun.Core.String"]),
        ),
        // Additional map operations
        create_function_def(
            "map_equal",
            vec![
                (
                    "lhs",
                    create_generic_type("Outrun.Core.Map", vec!["K", "V"]),
                ),
                (
                    "rhs",
                    create_generic_type("Outrun.Core.Map", vec!["K", "V"]),
                ),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        create_function_def(
            "map_remove",
            vec![
                (
                    "map",
                    create_generic_type("Outrun.Core.Map", vec!["K", "V"]),
                ),
                ("key", create_simple_type("K")),
            ],
            create_generic_type("Outrun.Core.Map", vec!["K", "V"]),
        ),
        create_function_def(
            "map_size",
            vec![(
                "map",
                create_generic_type("Outrun.Core.Map", vec!["K", "V"]),
            )],
            create_simple_type("Outrun.Core.Integer64"),
        ),
        // Panic/error operations
        create_function_def(
            "panic",
            vec![("message", create_simple_type("Outrun.Core.String"))],
            create_simple_type("T"),
        ),
        // Additional precision-based float operations that tests expect
        create_function_def(
            "f64_ceil_precision",
            vec![
                ("value", create_simple_type("Outrun.Core.Float64")),
                ("precision", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_floor_precision",
            vec![
                ("value", create_simple_type("Outrun.Core.Float64")),
                ("precision", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Float64"),
        ),
        create_function_def(
            "f64_round_precision",
            vec![
                ("value", create_simple_type("Outrun.Core.Float64")),
                ("precision", create_simple_type("Outrun.Core.Integer64")),
            ],
            create_simple_type("Outrun.Core.Float64"),
        ),
        // Additional boolean operations that tests expect
        create_function_def(
            "bool_eq",
            vec![
                ("lhs", create_simple_type("Outrun.Core.Boolean")),
                ("rhs", create_simple_type("Outrun.Core.Boolean")),
            ],
            create_simple_type("Outrun.Core.Boolean"),
        ),
        // Universal inspect operation for default trait implementation
        create_function_def(
            "inspect_value",
            vec![("value", create_simple_type("Self"))],
            create_simple_type("Outrun.Core.String"),
        ),
    ]
}

/// Create a lightweight FunctionDefinition struct for intrinsics with minimal allocations
fn create_function_def(
    name: &str,
    params: Vec<(&str, TypeAnnotation)>,
    return_type: TypeAnnotation,
) -> FunctionDefinition {
    // Use zero span for all intrinsics to minimize memory usage  
    const ZERO_SPAN: Span = Span { start: 0, end: 0, start_line_col: Some((0, 0)), end_line_col: Some((0, 0)) };
    
    // Pre-allocate parameters vector with exact capacity to avoid reallocations
    let mut parameters = Vec::with_capacity(params.len());
    for (param_name, param_type) in params {
        parameters.push(Parameter {
            name: Identifier {
                name: param_name.to_string(),
                span: ZERO_SPAN,
            },
            type_annotation: param_type,
            span: ZERO_SPAN,
        });
    }

    FunctionDefinition {
        attributes: Vec::new(), // Use Vec::new() instead of vec![] macro for empty vector
        visibility: FunctionVisibility::Public,
        name: Identifier {
            name: name.to_string(),
            span: ZERO_SPAN,
        },
        parameters,
        return_type,
        guard: None,
        body: Block {
            statements: Vec::new(), // Empty body for intrinsics - use Vec::new() for performance
            span: ZERO_SPAN,
        },
        span: ZERO_SPAN,
    }
}

/// Create a simple type annotation (e.g., "String", "T") with minimal allocations
fn create_simple_type(type_name: &str) -> TypeAnnotation {
    const ZERO_SPAN: Span = Span { start: 0, end: 0, start_line_col: Some((0, 0)), end_line_col: Some((0, 0)) };
    TypeAnnotation::Simple {
        path: vec![TypeIdentifier {
            name: type_name.to_string(),
            span: ZERO_SPAN,
        }],
        generic_args: None,
        span: ZERO_SPAN,
    }
}

/// Create a generic type annotation (e.g., "Option<T>", "Map<K, V>") with minimal allocations
fn create_generic_type(base_type: &str, type_args: Vec<&str>) -> TypeAnnotation {
    const ZERO_SPAN: Span = Span { start: 0, end: 0, start_line_col: Some((0, 0)), end_line_col: Some((0, 0)) };
    TypeAnnotation::Simple {
        path: vec![TypeIdentifier {
            name: base_type.to_string(),
            span: ZERO_SPAN,
        }],
        generic_args: if type_args.is_empty() {
            None
        } else {
            // Pre-allocate with exact capacity for performance
            let mut args = Vec::with_capacity(type_args.len());
            for type_arg in type_args {
                args.push(create_simple_type(type_arg));
            }
            Some(GenericArgs {
                args,
                span: ZERO_SPAN,
            })
        },
        span: ZERO_SPAN,
    }
}
