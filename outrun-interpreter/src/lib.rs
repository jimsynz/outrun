//! Outrun Interpreter v2 - Integration with Typechecker v3
//!
//! This interpreter works with the new typechecker v3 system by evaluating parser
//! AST nodes with attached type information rather than separate TypedExpression objects.
//!
//! Key changes from the previous interpreter:
//! - Works with parser `Expression` + `ParsedTypeInfo` instead of `TypedExpression`
//! - Integrates with package-based compilation from typechecker v3
//! - Uses new type system and constraint solving approach
//! - Maintains compatibility with existing Value system and intrinsics

// Allow clippy lints for development
#![allow(clippy::needless_range_loop)]
#![allow(clippy::single_match)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::result_large_err)]

pub mod context;
pub mod evaluator;
pub mod intrinsics;
pub mod pattern;
pub mod test_harness;
pub mod value;

// Include tests directory with all test modules
#[cfg(test)]
#[path = "tests/mod.rs"]
pub mod tests;

// Re-export public API
pub use context::{InterpreterContext, InterpreterError};
pub use evaluator::{EvaluationError, ExpressionEvaluator};
pub use intrinsics::{IntrinsicError, IntrinsicsHandler};
pub use pattern::{PatternMatchError, PatternMatcher};
pub use test_harness::{InterpreterSession, TestHarnessError};
pub use value::Value;

/// Main entry point for evaluating expressions with the new system
///
/// This function provides a high-level interface for evaluating parser expressions
/// that have been type-checked by the typechecker v3 system.
pub fn evaluate_expression(
    expression: &outrun_parser::Expression,
    context: &mut InterpreterContext,
) -> Result<Value, EvaluationError> {
    let evaluator = ExpressionEvaluator::new();
    evaluator.evaluate(expression, context)
}

/// Convenience function for evaluating expressions from a string
///
/// This is mainly useful for testing and REPL usage.
pub fn evaluate_from_string(
    source: &str,
    context: &mut InterpreterContext,
) -> Result<Value, Box<dyn std::error::Error>> {
    // Parse the expression
    let expression = outrun_parser::parse_expression(source)?;

    // TODO: Integrate with typechecker v3 to add type information
    // For now, we'll evaluate without full type checking

    // Evaluate with current context
    let result = evaluate_expression(&expression, context)?;
    Ok(result)
}

