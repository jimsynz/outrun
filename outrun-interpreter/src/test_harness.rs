//! Testing harness for evaluating Outrun code and asserting on results
//!
//! This module provides a convenient testing harness that allows tests to:
//! - Execute arbitrary Outrun code expressions
//! - Assert on the evaluated results using Outrun's inspect representation
//! - Set up variables and context for testing
//! - Handle errors gracefully with meaningful messages

use crate::patterns::PatternMatchingUtils;
use crate::{ExpressionEvaluator, InterpreterContext, Value};
use outrun_parser::{parse_expression, parse_program};
use outrun_typechecker::{
    compilation::{
        compiler_environment::CompilerEnvironment, program_collection::ProgramCollection,
    },
    context::FunctionDispatchContext,
    core_library,
};
use std::sync::{Arc, Mutex};
use thiserror::Error;

/// Errors that can occur during test harness operations
#[derive(Debug, Error)]
#[allow(clippy::result_large_err)] // The EvaluationError is large but infrequent
pub enum TestHarnessError {
    #[error("Parse error: {message}")]
    Parse { message: String },

    #[error("Compilation error: {message}")]
    Compilation { message: String },

    #[error("Evaluation error: {source}")]
    Evaluation {
        #[from]
        source: crate::evaluator::EvaluationError,
    },

    #[error("Assertion failed: expected {expected}, but got {actual}")]
    AssertionFailed { expected: String, actual: String },

    #[error("Setup error: {message}")]
    Setup { message: String },

    #[error("Internal error: {message}")]
    Internal {
        message: String,
        span: outrun_parser::Span,
    },
}

/// Test harness for evaluating Outrun expressions and asserting results
pub struct OutrunTestHarness {
    /// Expression evaluator for running code
    evaluator: ExpressionEvaluator,
    /// Interpreter context with variables and state
    context: InterpreterContext,
    /// Compiler environment for type operations (shared reference)
    compiler_env: Arc<Mutex<CompilerEnvironment>>,
    /// External variables for type checking (maps variable names to their types)
    pub external_variables:
        std::collections::HashMap<String, outrun_typechecker::unification::StructuredType>,
}

#[allow(clippy::result_large_err)] // The EvaluationError is large but infrequent
impl OutrunTestHarness {
    /// Create a new test harness with core library loaded
    pub fn new() -> Result<Self, TestHarnessError> {
        // Use the same pattern as the working test to avoid TypeInterner synchronization issues
        // Create a fresh CompilerEnvironment and load the core library into it
        let mut compiler_environment = CompilerEnvironment::new();
        let core_result =
            core_library::compile_core_library_with_environment(&mut compiler_environment);

        // Load the compilation result to populate structs and traits
        compiler_environment.load_structs_and_traits(&core_result);

        // Create dispatch context and evaluator using the environment with core library loaded
        let dispatch_context = FunctionDispatchContext::new(Some(compiler_environment.clone()));
        let evaluator = ExpressionEvaluator::from_dispatch_context(dispatch_context);

        // Create interpreter context using the same type context from the core library compilation
        let context = InterpreterContext::new(
            core_result.type_context.clone(),
            compiler_environment.clone(),
            Some(1000), // Set reasonable call stack limit
        );

        Ok(Self {
            evaluator,
            context,
            compiler_env: Arc::new(Mutex::new(compiler_environment)),
            external_variables: std::collections::HashMap::new(),
        })
    }

    /// Execute an Outrun expression and return the result
    pub fn evaluate(&mut self, expression_code: &str) -> Result<Value, TestHarnessError> {
        // Use the same pattern as the REPL for direct expression evaluation

        // Parse the expression directly
        let parsed_expr =
            parse_expression(expression_code).map_err(|e| TestHarnessError::Parse {
                message: format!("Failed to parse expression '{expression_code}': {e:?}"),
            })?;

        // Create a simple program with just the expression (like REPL does)
        let temp_program = outrun_parser::Program {
            items: vec![outrun_parser::Item {
                kind: outrun_parser::ItemKind::Expression(parsed_expr.clone()),
                span: parsed_expr.span,
            }],
            span: parsed_expr.span,
            debug_info: outrun_parser::DebugInfo {
                source_file: Some("<test>".to_string()),
                comments: Vec::new(),
            },
        };

        // Create a program collection with just the expression
        // Note: The compilation pipeline will handle desugaring automatically
        let mut collection = ProgramCollection::new();
        collection.add_program(
            "<test>".to_string(),
            temp_program,
            expression_code.to_string(),
        );

        // Compile using the persistent compiler environment (like REPL)
        // Use compile_collection_with_external_variables to match REPL pattern
        let compilation_result = {
            let mut env = self.compiler_env.lock().unwrap();
            env.compile_collection_with_external_variables(
                collection,
                self.external_variables.clone(),
            )
            .map_err(|e| TestHarnessError::Compilation {
                message: format!("Failed to compile expression: {e:?}"),
            })?
        };

        // Since CompilerEnvironment uses Arc<RwLock> internally, cloning shares the same data.
        // The issue must be elsewhere - let's debug what's happening.

        // Extract the typed expression from the compilation result
        let typed_program = compilation_result
            .typed_programs
            .get("<test>")
            .ok_or_else(|| TestHarnessError::Compilation {
                message: "No typed program found".to_string(),
            })?;

        // Note: Compilation errors are handled by returning Err from compile_collection_with_external_variables

        // Extract the typed expression (should be the only item)
        let typed_expr = if let Some(typed_item) = typed_program.items.first() {
            if let outrun_typechecker::checker::TypedItemKind::Expression(expr) = &typed_item.kind {
                // Debug: Print the typed expression details
                eprintln!("🔍 DEBUG: Typed expression created:");
                eprintln!("  Kind: {:?}", expr.kind);
                eprintln!("  Structured type: {:?}", expr.structured_type);
                if let outrun_typechecker::checker::TypedExpressionKind::FunctionCall {
                    arguments,
                    dispatch_strategy,
                    ..
                } = &expr.kind
                {
                    eprintln!("  Function call arguments:");
                    for arg in arguments {
                        eprintln!(
                            "    {} -> structured_type: {:?}",
                            arg.name, arg.expression.structured_type
                        );
                    }
                    eprintln!("  Dispatch strategy: {dispatch_strategy:?}");
                }
                expr.clone()
            } else {
                return Err(TestHarnessError::Compilation {
                    message: "Expected expression item in typed program".to_string(),
                });
            }
        } else {
            return Err(TestHarnessError::Compilation {
                message: "No items found in typed program".to_string(),
            });
        };

        // Evaluate the typed expression
        self.evaluator
            .evaluate(&mut self.context, &typed_expr)
            .map_err(|source| TestHarnessError::Evaluation { source })
    }

    /// Execute Outrun code and assert the result equals the expected inspect representation
    pub fn assert_evaluates_to(
        &mut self,
        expression_code: &str,
        expected_inspect: &str,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;
        let actual_inspect = self.inspect_value(&result)?;

        if actual_inspect == expected_inspect {
            Ok(())
        } else {
            Err(TestHarnessError::AssertionFailed {
                expected: expected_inspect.to_string(),
                actual: actual_inspect,
            })
        }
    }

    /// Execute Outrun code and assert it returns a specific boolean value
    pub fn assert_evaluates_to_boolean(
        &mut self,
        expression_code: &str,
        expected: bool,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;
        match result {
            Value::Boolean(actual) if actual == expected => Ok(()),
            Value::Boolean(actual) => Err(TestHarnessError::AssertionFailed {
                expected: expected.to_string(),
                actual: actual.to_string(),
            }),
            other => Err(TestHarnessError::AssertionFailed {
                expected: format!("Boolean({expected})"),
                actual: format!("{other:?}"),
            }),
        }
    }

    /// Execute Outrun code and assert it returns a specific integer value
    pub fn assert_evaluates_to_integer(
        &mut self,
        expression_code: &str,
        expected: i64,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;
        match result {
            Value::Integer64(actual) if actual == expected => Ok(()),
            Value::Integer64(actual) => Err(TestHarnessError::AssertionFailed {
                expected: expected.to_string(),
                actual: actual.to_string(),
            }),
            other => Err(TestHarnessError::AssertionFailed {
                expected: format!("Integer64({expected})"),
                actual: format!("{other:?}"),
            }),
        }
    }

    /// Execute Outrun code and assert it returns a specific string value
    pub fn assert_evaluates_to_string(
        &mut self,
        expression_code: &str,
        expected: &str,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;
        match result {
            Value::String(actual) if actual == expected => Ok(()),
            Value::String(actual) => Err(TestHarnessError::AssertionFailed {
                expected: expected.to_string(),
                actual,
            }),
            other => Err(TestHarnessError::AssertionFailed {
                expected: format!("String(\"{expected}\")"),
                actual: format!("{other:?}"),
            }),
        }
    }

    /// Execute Outrun code and assert it returns a specific float value
    pub fn assert_evaluates_to_float(
        &mut self,
        expression_code: &str,
        expected: f64,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;
        match result {
            Value::Float64(actual) if (actual - expected).abs() < f64::EPSILON => Ok(()),
            Value::Float64(actual) => Err(TestHarnessError::AssertionFailed {
                expected: expected.to_string(),
                actual: actual.to_string(),
            }),
            other => Err(TestHarnessError::AssertionFailed {
                expected: format!("Float64({expected})"),
                actual: format!("{other:?}"),
            }),
        }
    }

    /// Set up a variable in the test context for use in subsequent evaluations
    pub fn set_variable(&mut self, name: &str, value: Value) -> Result<(), TestHarnessError> {
        // Store variable in interpreter context for evaluation
        self.context
            .define_variable(name.to_string(), value.clone())
            .map_err(|e| TestHarnessError::Setup {
                message: format!("Failed to bind variable '{name}': {e:?}"),
            })?;

        // Also store variable type in external_variables for type checking
        let variable_type = {
            let env = self.compiler_env.lock().unwrap();
            match &value {
                Value::Integer64(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.Integer64");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
                Value::Boolean(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.Boolean");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
                Value::String(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.String");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
                Value::Float64(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.Float64");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
                _ => {
                    // For other types, use a placeholder
                    let type_id = env.intern_type_name("UnknownType");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
            }
        };
        self.external_variables
            .insert(name.to_string(), variable_type);

        Ok(())
    }

    /// Execute a let binding to set up variables for testing
    pub fn execute_let_binding(&mut self, binding_code: &str) -> Result<(), TestHarnessError> {
        // Use the same pipeline as the working evaluate method, but for let bindings
        // Parse the let binding (as a program, not expression)
        let parsed_program = parse_program(binding_code).map_err(|e| TestHarnessError::Parse {
            message: format!("Failed to parse let binding '{binding_code}': {e:?}"),
        })?;

        // Create a program collection with the let binding (like evaluate method)
        let mut collection = ProgramCollection::new();
        collection.add_program(
            "<setup>".to_string(),
            parsed_program,
            binding_code.to_string(),
        );

        // Compile the let binding with external variables
        // The improved dispatch fallback should handle TypeInterner sync issues
        let compilation_result = {
            let mut env = self.compiler_env.lock().unwrap();
            env.compile_collection_with_external_variables(
                collection,
                self.external_variables.clone(),
            )
            .map_err(|e| TestHarnessError::Compilation {
                message: format!("Failed to compile let binding: {e:?}"),
            })?
        };

        // Extract the typed program
        let typed_program = compilation_result
            .typed_programs
            .get("<setup>")
            .ok_or_else(|| TestHarnessError::Compilation {
                message: "No typed program found for setup".to_string(),
            })?;

        // Extract and execute the let binding using the same evaluator as working expressions
        if let Some(typed_item) = typed_program.items.first() {
            if let outrun_typechecker::checker::TypedItemKind::LetBinding(let_binding) =
                &typed_item.kind
            {
                // Use the same approach as function_executor.rs:
                // 1. Evaluate the expression with the standard evaluator
                // 2. Extract variable name from pattern
                // 3. Bind variable in context
                let value = self
                    .evaluator
                    .evaluate(&mut self.context, &let_binding.expression)
                    .map_err(|source| TestHarnessError::Evaluation { source })?;

                // Extract variable names from pattern and bind in context
                let variables = PatternMatchingUtils::extract_bound_variables(&let_binding.pattern);

                if let Some(first_variable) = variables.first() {
                    // Bind the variable in the interpreter context
                    self.context
                        .define_variable(first_variable.clone(), value.clone())
                        .map_err(|e| TestHarnessError::Setup {
                            message: format!("Failed to bind variable '{first_variable}': {e:?}"),
                        })?;

                    // Store variable type in external_variables for future type checking
                    // With the improved dispatch fallback, this should work correctly now
                    let variable_type = {
                        let env = self.compiler_env.lock().unwrap();
                        match &value {
                            Value::Integer64(_) => {
                                let type_id = env.intern_type_name("Outrun.Core.Integer64");
                                outrun_typechecker::unification::StructuredType::Simple(type_id)
                            }
                            Value::Boolean(_) => {
                                let type_id = env.intern_type_name("Outrun.Core.Boolean");
                                outrun_typechecker::unification::StructuredType::Simple(type_id)
                            }
                            Value::String(_) => {
                                let type_id = env.intern_type_name("Outrun.Core.String");
                                outrun_typechecker::unification::StructuredType::Simple(type_id)
                            }
                            Value::List { element_type, .. } => {
                                // Create Outrun.Core.List<ElementType> from the actual element type
                                let list_type_id = env.intern_type_name("Outrun.Core.List");
                                outrun_typechecker::unification::StructuredType::Generic {
                                    base: list_type_id,
                                    args: vec![element_type.clone()],
                                }
                            }
                            _ => {
                                let type_id = env.intern_type_name("UnknownType");
                                outrun_typechecker::unification::StructuredType::Simple(type_id)
                            }
                        }
                    };
                    self.external_variables
                        .insert(first_variable.clone(), variable_type);
                } else {
                    return Err(TestHarnessError::Setup {
                        message: "No variables found in pattern".to_string(),
                    });
                }
            } else {
                return Err(TestHarnessError::Compilation {
                    message: "Expected let binding item in program".to_string(),
                });
            }
        } else {
            return Err(TestHarnessError::Compilation {
                message: "No items found in setup program".to_string(),
            });
        }

        Ok(())
    }

    /// Call the Outrun Inspect.inspect function on a value to get its inspect representation
    fn inspect_value(&mut self, value: &Value) -> Result<String, TestHarnessError> {
        // First, bind the value to a temporary variable so we can reference it in the inspect call
        // Use a unique variable name with timestamp to avoid conflicts
        let temp_var_name = format!(
            "__harness_temp_inspect_value_{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0)
        );
        self.context
            .define_variable(temp_var_name.clone(), value.clone())
            .map_err(|e| TestHarnessError::Setup {
                message: format!("Failed to bind temporary variable for inspect: {e:?}"),
            })?;

        // Also add to external_variables for the typechecker
        let variable_type = {
            let env = self.compiler_env.lock().unwrap();
            match value {
                Value::Integer64(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.Integer64");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
                Value::Boolean(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.Boolean");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
                Value::String(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.String");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
                Value::Atom(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.Atom");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
                Value::List { element_type, .. } => {
                    let list_type_id = env.intern_type_name("Outrun.Core.List");
                    outrun_typechecker::unification::StructuredType::Generic {
                        base: list_type_id,
                        args: vec![element_type.clone()],
                    }
                }
                Value::Map {
                    key_type,
                    value_type,
                    ..
                } => {
                    let map_type_id = env.intern_type_name("Outrun.Core.Map");
                    outrun_typechecker::unification::StructuredType::Generic {
                        base: map_type_id,
                        args: vec![key_type.clone(), value_type.clone()],
                    }
                }
                Value::Tuple { tuple_type, .. } => {
                    // Use the actual tuple type that was computed during creation
                    tuple_type.clone()
                }
                Value::Struct { struct_type, .. } => {
                    // Use the actual struct type that was computed during creation
                    struct_type.clone()
                }
                Value::Float64(_) => {
                    let type_id = env.intern_type_name("Outrun.Core.Float64");
                    outrun_typechecker::unification::StructuredType::Simple(type_id)
                }
            }
        };
        self.external_variables
            .insert(temp_var_name.clone(), variable_type);

        // Create and evaluate the expression: Inspect.inspect(value: __harness_temp_inspect_value)
        let inspect_expression = format!("Inspect.inspect(value: {temp_var_name})");

        // Evaluate the inspect call
        let inspect_result = self.evaluate(&inspect_expression)?;

        // Extract the string result
        match inspect_result {
            Value::String(s) => Ok(s),
            other => Err(TestHarnessError::Internal {
                message: format!("Inspect.inspect should return a String, but got: {other:?}"),
                span: outrun_parser::Span::new(0, 0),
            }),
        }
    }
}

impl Default for OutrunTestHarness {
    fn default() -> Self {
        Self::new().expect("Failed to create default test harness")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_harness_basic_integer_evaluation() {
        let mut harness = OutrunTestHarness::new().unwrap();
        harness.assert_evaluates_to_integer("42", 42).unwrap();
    }

    #[test]
    fn test_harness_basic_boolean_evaluation() {
        let mut harness = OutrunTestHarness::new().unwrap();
        harness.assert_evaluates_to_boolean("true", true).unwrap();
        harness.assert_evaluates_to_boolean("false", false).unwrap();
    }

    #[test]
    fn test_harness_basic_string_evaluation() {
        let mut harness = OutrunTestHarness::new().unwrap();
        harness
            .assert_evaluates_to_string("\"hello\"", "hello")
            .unwrap();
    }

    #[test]
    fn test_harness_arithmetic_evaluation() {
        let mut harness = OutrunTestHarness::new().unwrap();
        harness.assert_evaluates_to_integer("1 + 2", 3).unwrap();
        harness.assert_evaluates_to_integer("10 - 5", 5).unwrap();
        harness.assert_evaluates_to_integer("3 * 4", 12).unwrap();
    }

    #[test]
    fn test_harness_variable_setup() {
        let mut harness = OutrunTestHarness::new().unwrap();

        // Set up a variable directly
        harness.set_variable("x", Value::integer(42)).unwrap();

        // Test that we can use the variable
        harness.assert_evaluates_to_integer("x", 42).unwrap();
    }

    #[test]
    fn test_harness_let_binding_setup() {
        let mut harness = OutrunTestHarness::new().unwrap();

        // Execute a let binding
        harness.execute_let_binding("let y = 100").unwrap();

        // Test that we can use the bound variable
        harness.assert_evaluates_to_integer("y", 100).unwrap();
    }

    #[test]
    fn test_harness_inspect_assertion() {
        let mut harness = OutrunTestHarness::new().unwrap();

        // Test inspect-based assertion
        harness.assert_evaluates_to("42", "42").unwrap();

        harness.assert_evaluates_to("true", "true").unwrap();

        harness.assert_evaluates_to("\"test\"", "\"test\"").unwrap();
    }
}
