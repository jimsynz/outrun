//! Acceptance tests for the new Outrun interpreter
//!
//! This module contains comprehensive acceptance tests that verify the new interpreter
//! works correctly with the parser → interpreter pipeline.

pub mod test_acceptance_basic_literals;
pub mod test_acceptance_expressions;
pub mod test_acceptance_pattern_matching;
pub mod test_complex_arithmetic;
pub mod test_comprehensive_repl_scenarios;
pub mod test_debug_literal_ast;
pub mod test_equality_auto_impl;
pub mod test_function_dispatch;
pub mod test_minimal_user_code;
pub mod test_pattern_matching;
pub mod test_phase4_error_handling;
pub mod test_pipeline_integration;
pub mod test_repl_context_persistence;
pub mod test_spread_operator;

// TDD tests for Option/Result/List runtime support
pub mod test_option_runtime;
pub mod test_result_runtime;
pub mod test_list_runtime;

// Pattern matching tests for Option/Result
pub mod test_option_pattern_matching;
pub mod test_result_pattern_matching;