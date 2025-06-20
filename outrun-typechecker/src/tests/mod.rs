//! Integration tests for the Outrun multi-program visitor-based type checker
//!
//! This module contains tests for the new visitor-based type checking architecture
//! that supports multi-program compilation with dependency resolution.

#![allow(clippy::assertions_on_constants)]

// Core library runtime loading tests
mod test_core_library_runtime;
mod test_detailed_errors;

// Core visitor and unification tests
// TODO: Fix visitor pattern tests
// mod test_visitor_patterns;
mod test_type_unification;
mod test_visitor_let_bindings;
// TODO: Create these test files
// mod test_structured_types;

// Multi-program compiler tests
// TODO: Fix multi-program compiler tests
// mod test_multi_program_compiler;
// TODO: Create these test files
// mod test_program_collection;
// mod test_dependency_resolution;

// Phase-specific visitor tests
// TODO: Create these test files
// mod test_trait_extraction_visitor;
// mod test_struct_extraction_visitor;
// mod test_impl_extraction_visitor;
// mod test_function_extraction_visitor;
// mod test_type_checking_visitor;

// Integration tests with new API
// TODO: Create these test files
// mod test_new_api_integration;
// mod test_core_library_integration;
// mod test_error_reporting;

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

// Pattern typechecking tests
// TODO: Fix test_pattern_typechecking after fixing helper function
// mod test_pattern_typechecking;
mod test_pattern_variable_scoping;

// TypedProgram conversion tests
mod test_typed_program_conversion;

// Performance and edge case tests
// TODO: Create these test files
// mod test_compilation_performance;
// mod test_circular_dependencies;
// mod test_complex_type_scenarios;
