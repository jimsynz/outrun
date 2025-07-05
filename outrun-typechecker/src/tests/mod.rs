//! Integration tests for the Outrun multi-program visitor-based type checker
//!
//! This module contains tests for the new visitor-based type checking architecture
//! that supports multi-program compilation with dependency resolution.

#![allow(clippy::assertions_on_constants)]

// Core library runtime loading tests
mod test_core_library_runtime;
mod test_detailed_errors;

// LSP integration tests

// Span mapping tests
mod test_span_mapping;

// Tuple type inference tests
mod test_tuple_type_inference;

// If expression integration tests
mod test_if_expression_integration;

mod test_visitor_let_bindings;
// TODO: Create these test files
// mod test_structured_types;


// Impl block validation tests
mod test_default_trait_implementations;
mod test_impl_validation;

// Expression type checking tests
mod test_expression_checking;
mod test_list_literal_type_checking;
mod test_map_literal_type_inference;

// Generic Self resolution tests
mod test_generic_self_resolution;

// Generic struct constructor tests
mod test_generic_struct_constructor;

// Exponentiate generic types tests
mod test_exponentiate_generic_types;

mod test_pattern_field_validation;
mod test_pattern_variable_scoping;

// TypedProgram conversion tests
mod test_typed_program_conversion;

// Basic typed AST tests (Week 1 functionality)
mod test_basic_typed_ast;

// Collection typed AST tests (Week 2 functionality)
mod test_collection_typed_ast;

// Control flow typed AST tests (Week 4 functionality)
mod test_control_flow_typed_ast;

// Function definition typed AST tests (Week 5 functionality)
mod test_function_definitions_typed_ast;

// Function call type inference tests
mod test_function_call_type_inference;

// Type system item typed AST tests (Week 6 functionality)
mod test_type_system_typed_ast;

// Generics and type annotations typed AST tests (Week 7 functionality)
mod test_generics_and_type_annotations;

// Debug info and comment attachment tests (Week 9 functionality)

// Case expression verification tests
mod test_case_expression_verification;

// Case expression branch type unification tests
mod test_case_branch_unification;

// Raw text preservation tests
mod test_raw_text_preservation;

// Position-based type lookup tests for LSP integration
mod test_position_based_type_lookup;

// Sigil desugaring tests
mod test_sigil_desugaring;

// Type resolution integration tests
mod test_type_resolution_integration;

// Bug reproduction tests

// SMT integration tests
mod test_complex_trait_constraints;
mod test_smt_integration;
mod test_smt_typevariable_resolution;
mod test_smt_monomorphization_dispatch;

// Function clause debugging tests
mod test_runtime_function_clause_lookup;

// Performance and edge case tests
// TODO: Create these test files
// mod test_compilation_performance;
// mod test_circular_dependencies;
// mod test_complex_type_scenarios;
