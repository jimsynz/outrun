//! Integration tests for the Outrun type checker
//!
//! All test files must be declared as modules here.

#![allow(clippy::assertions_on_constants)]

mod test_actual_impl_block_generics;
mod test_anonymous_functions;
mod test_ast_conversion_utilities;
mod test_basic_type_checking;
mod test_conditional_impl_blocks;
mod test_constraint_validation;
mod test_default_implementations;
mod test_default_trait_implementations;
mod test_dispatch_table_construction;
mod test_enhanced_function_calls;
mod test_exhaustiveness_integration;
mod test_function_definitions;
mod test_function_overloading;
mod test_function_type_annotations;
mod test_generic_argument_swapping;
mod test_generic_arity_validation;
mod test_generic_debug;
mod test_generic_function_validation;
mod test_generic_type_resolution;
mod test_list_literal_type_matching;
mod test_scope_debug;
mod test_simple_debug;
mod test_static_trait_functions;
mod test_string_interpolation;
mod test_trait_case_exhaustiveness;
mod test_trait_case_patterns;
mod test_trait_implementations;
mod test_trait_self_resolution;
mod test_traits_unit;
mod test_type_checker_core;
mod test_type_interning;
mod test_type_introspection;
mod test_typed_patterns;
