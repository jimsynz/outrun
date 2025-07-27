//! Testing harness for the new Outrun interpreter system
//!
//! This module provides a convenient testing harness that allows tests to:
//! - Execute arbitrary Outrun code expressions using the new interpreter
//! - Assert on the evaluated results with type checking
//! - Set up variables and context for testing scenarios
//! - Handle parser → interpreter pipeline gracefully
//! - Maintain variable persistence across evaluations (REPL-like behavior)

use crate::{EvaluationError, ExpressionEvaluator, InterpreterContext, Value};
use miette::Diagnostic;
use outrun_parser::{ParseError, parse_program};
use outrun_typechecker::{CompilerError, CompilationResult, Package};
use thiserror::Error;

/// Errors that can occur during test harness operations
#[derive(Debug, Error, Diagnostic)]
pub enum TestHarnessError {
    #[error("Parse error: {source}")]
    Parse {
        #[from]
        source: ParseError,
    },

    #[error("Evaluation error: {source}")]
    Evaluation {
        #[from]
        source: EvaluationError,
    },

    #[error("Typecheck error: {source}")]
    Typecheck {
        source: Box<outrun_typechecker::TypecheckError>,
    },

    #[error("Compiler error: {source}")]
    Compiler {
        source: Box<CompilerError>,
    },

    #[error("Assertion failed: expected {expected}, but got {actual}")]
    AssertionFailed { expected: String, actual: String },

    #[error("Setup error: {message}")]
    Setup { message: String },

    #[error("Type error: expected {expected_type}, got {actual_type}")]
    TypeError {
        expected_type: String,
        actual_type: String,
    },

    #[error("Variable not found: {name}")]
    VariableNotFound { name: String },

    #[error("Internal error: {message}")]
    Internal { message: String },
}

impl From<outrun_typechecker::TypecheckError> for TestHarnessError {
    fn from(err: outrun_typechecker::TypecheckError) -> Self {
        TestHarnessError::Typecheck {
            source: Box::new(err),
        }
    }
}

impl From<CompilerError> for TestHarnessError {
    fn from(err: CompilerError) -> Self {
        TestHarnessError::Compiler {
            source: Box::new(err),
        }
    }
}

/// Test harness for evaluating Outrun expressions with the new interpreter system
pub struct OutrunTestHarness {
    /// Expression evaluator for running code (cached after first use)
    evaluator: Option<ExpressionEvaluator>,
    /// Interpreter context with variables and state
    context: InterpreterContext,
    /// Pre-compiled core library (compiled once for efficiency)
    core_compilation: Option<CompilationResult>,
    /// Session package that accumulates all evaluations for variable persistence
    session_package: Package,
}

impl OutrunTestHarness {
    /// Create a new test harness with the new interpreter system
    pub fn new() -> Result<Self, TestHarnessError> {
        let context = InterpreterContext::new();

        // Pre-compile core library once for efficient REPL-like behavior
        let core_compilation = CompilationResult::precompile_core_library()
            .map_err(|e| TestHarnessError::Setup {
                message: format!("Failed to precompile core library: {}", e),
            })?;

        // Create session package for accumulating evaluations
        let session_package = Package::new("test_session".to_string());

        Ok(Self {
            evaluator: None, // Will be cached after first use
            context,
            core_compilation: Some(core_compilation),
            session_package,
        })
    }

    /// Execute an Outrun expression and return the result using the full pipeline
    pub fn evaluate(&mut self, expression_code: &str) -> Result<Value, TestHarnessError> {
        let core_compilation = self.core_compilation.as_ref()
            .ok_or_else(|| TestHarnessError::Setup {
                message: "Core library not precompiled".to_string(),
            })?;

        // Try to compile the expression first. If it fails with an undefined variable error,
        // and we have that variable in our context, skip typechecking and evaluate directly
        let compilation_result = CompilationResult::compile_repl_expression(
            expression_code,
            core_compilation,
        );

        let (should_skip_typecheck, parsed_program) = match compilation_result {
            Ok(compilation) => {
                // Compilation succeeded - use it normally
                self.evaluator = Some(ExpressionEvaluator::with_universal_dispatch(
                    compilation.dispatch_table,
                    compilation.function_registry,
                    compilation.universal_dispatch,
                ));
                // Use the desugared program from the compilation result, not the raw parsed expression
                let desugared_program = compilation.programs.first()
                    .ok_or_else(|| TestHarnessError::Internal {
                        message: "No programs found in compilation result".to_string(),
                    })?.clone();
                (false, desugared_program)
            }
            Err(CompilerError::Typecheck(boxed_err)) => {
                if let outrun_typechecker::TypecheckError::InferenceError(
                    outrun_typechecker::InferenceError::UndefinedVariable { variable_name, .. }
                ) = boxed_err.as_ref() {
                    // Check if we have this variable in our interpreter context
                    if self.context.get_variable(variable_name).is_ok() {
                        // We have the variable! Skip typechecking and evaluate directly
                        // Use the core compilation for the evaluator
                        self.evaluator = Some(ExpressionEvaluator::with_universal_dispatch(
                            core_compilation.dispatch_table.clone(),
                            core_compilation.function_registry.clone(),
                            core_compilation.universal_dispatch.clone(),
                        ));
                        (true, parse_program(expression_code)?)
                    } else {
                        // We don't have the variable either - propagate the error
                        return Err(TestHarnessError::Compiler { source: Box::new(CompilerError::Typecheck(Box::new(outrun_typechecker::TypecheckError::InferenceError(
                            outrun_typechecker::InferenceError::UndefinedVariable { 
                                variable_name: variable_name.clone(), 
                                span: None, 
                                similar_names: vec![], 
                                context: Some("Variable not found in test harness context".to_string()) 
                            }
                        )))) });
                    }
                } else {
                    // Other typecheck error - propagate it
                    return Err(TestHarnessError::Compiler { source: Box::new(CompilerError::Typecheck(boxed_err)) });
                }
            }
            Err(e) => {
                // Other compilation errors - propagate them
                return Err(TestHarnessError::Compiler { source: Box::new(e) });
            }
        };

        // Extract the first item for evaluation
        if let Some(first_item) = parsed_program.items.first() {
            match &first_item.kind {
                outrun_parser::ItemKind::Expression(expr) => {
                    // Evaluate the expression with our interpreter
                    let evaluator = self.evaluator.as_ref().unwrap(); // Should always be Some by this point
                    let value = evaluator.evaluate(expr, &mut self.context)?;
                    Ok(value)
                }
                outrun_parser::ItemKind::LetBinding(let_binding) => {
                    // Handle let bindings by evaluating them (only if not skipping typecheck)
                    if should_skip_typecheck {
                        return Err(TestHarnessError::Internal {
                            message: "Cannot evaluate let bindings when skipping typecheck".to_string(),
                        });
                    }
                    self.evaluate_let_binding(let_binding)
                }
                _ => {
                    Err(TestHarnessError::Internal {
                        message: format!(
                            "Unsupported item type in test harness: {:?}",
                            first_item.kind
                        ),
                    })
                }
            }
        } else {
            Err(TestHarnessError::Internal {
                message: "No items found in parsed program".to_string(),
            })
        }
    }

    /// Evaluate a let binding and return the bound value
    fn evaluate_let_binding(
        &mut self,
        let_binding: &outrun_parser::LetBinding,
    ) -> Result<Value, TestHarnessError> {
        // Evaluate the right-hand side expression
        let evaluator = self.evaluator.as_ref().unwrap(); // Should always be Some by this point
        let value = evaluator.evaluate(&let_binding.expression, &mut self.context)?;

        // Extract variable names from the pattern (simplified - only supports identifier patterns for now)
        let variable_names = match &let_binding.pattern {
            outrun_parser::Pattern::Identifier(identifier) => {
                vec![identifier.name.clone()]
            }
            _ => {
                return Err(TestHarnessError::Internal {
                    message: "Complex patterns not yet supported in test harness".to_string(),
                });
            }
        };

        // Bind variables in the interpreter context
        for variable_name in &variable_names {
            self.context
                .define_variable(variable_name.clone(), value.clone())
                .map_err(|e| TestHarnessError::Internal {
                    message: format!("Failed to bind variable '{}': {}", variable_name, e),
                })?;
        }

        Ok(value)
    }

    /// Execute code and assert it evaluates to a specific integer value
    pub fn assert_evaluates_to_integer(
        &mut self,
        expression_code: &str,
        expected: i64,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;

        match result {
            Value::Integer64(value) if value == expected => Ok(()),
            Value::Integer64(value) => Err(TestHarnessError::AssertionFailed {
                expected: expected.to_string(),
                actual: value.to_string(),
            }),
            _ => Err(TestHarnessError::TypeError {
                expected_type: "Integer".to_string(),
                actual_type: result.type_name().to_string(),
            }),
        }
    }

    /// Execute code and assert it evaluates to a specific boolean value
    pub fn assert_evaluates_to_boolean(
        &mut self,
        expression_code: &str,
        expected: bool,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;

        match result {
            Value::Boolean(value) if value == expected => Ok(()),
            Value::Boolean(value) => Err(TestHarnessError::AssertionFailed {
                expected: expected.to_string(),
                actual: value.to_string(),
            }),
            _ => Err(TestHarnessError::TypeError {
                expected_type: "Boolean".to_string(),
                actual_type: result.type_name().to_string(),
            }),
        }
    }

    /// Execute code and assert it evaluates to a specific string value
    pub fn assert_evaluates_to_string(
        &mut self,
        expression_code: &str,
        expected: &str,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;

        match result {
            Value::String(ref value) if value == expected => Ok(()),
            Value::String(value) => Err(TestHarnessError::AssertionFailed {
                expected: expected.to_string(),
                actual: value,
            }),
            _ => Err(TestHarnessError::TypeError {
                expected_type: "String".to_string(),
                actual_type: result.type_name().to_string(),
            }),
        }
    }

    /// Execute code and assert it evaluates to a specific atom value
    pub fn assert_evaluates_to_atom(
        &mut self,
        expression_code: &str,
        expected: &str,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;

        match result {
            Value::Atom(ref name) if name == expected => Ok(()),
            Value::Atom(name) => Err(TestHarnessError::AssertionFailed {
                expected: expected.to_string(),
                actual: name,
            }),
            _ => Err(TestHarnessError::TypeError {
                expected_type: "Atom".to_string(),
                actual_type: result.type_name().to_string(),
            }),
        }
    }

    /// Execute code and assert it evaluates to an empty list
    pub fn assert_evaluates_to_empty_list(
        &mut self,
        expression_code: &str,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;

        match result {
            Value::List { ref list, .. } => match list.as_ref() {
                crate::value::List::Empty => Ok(()),
                _ => Err(TestHarnessError::AssertionFailed {
                    expected: "Empty list".to_string(),
                    actual: result.display(),
                }),
            },
            _ => Err(TestHarnessError::TypeError {
                expected_type: "List".to_string(),
                actual_type: result.type_name().to_string(),
            }),
        }
    }

    /// Execute code and assert it evaluates to a list with specific head and tail
    pub fn assert_evaluates_to_list_with_head(
        &mut self,
        expression_code: &str,
        expected_head: Value,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;

        match result {
            Value::List { ref list, .. } => match list.as_ref() {
                crate::value::List::Cons { head, .. } => {
                    if head == &expected_head {
                        Ok(())
                    } else {
                        Err(TestHarnessError::AssertionFailed {
                            expected: expected_head.display(),
                            actual: head.display(),
                        })
                    }
                }
                crate::value::List::Empty => Err(TestHarnessError::AssertionFailed {
                    expected: format!("List with head {}", expected_head.display()),
                    actual: "Empty list".to_string(),
                }),
            },
            _ => Err(TestHarnessError::TypeError {
                expected_type: "List".to_string(),
                actual_type: result.type_name().to_string(),
            }),
        }
    }

    /// Execute code and assert the result matches a general expectation
    pub fn assert_evaluates_to_value(
        &mut self,
        expression_code: &str,
        expected: Value,
    ) -> Result<(), TestHarnessError> {
        let result = self.evaluate(expression_code)?;

        if result == expected {
            Ok(())
        } else {
            Err(TestHarnessError::AssertionFailed {
                expected: expected.display(),
                actual: result.display(),
            })
        }
    }

    /// Set up a variable in the test context for use in subsequent expressions
    pub fn define_variable(&mut self, name: &str, value: Value) -> Result<(), TestHarnessError> {
        self.context
            .define_variable(name.to_string(), value)
            .map_err(|e| TestHarnessError::Setup {
                message: format!("Failed to define variable '{}': {}", name, e),
            })
    }

    /// Get a variable from the test context
    pub fn get_variable(&self, name: &str) -> Result<&Value, TestHarnessError> {
        self.context
            .get_variable(name)
            .map_err(|_| TestHarnessError::VariableNotFound {
                name: name.to_string(),
            })
    }

    /// Clear all variables from the test context
    pub fn clear_variables(&mut self) {
        self.context = InterpreterContext::new();
        // Reset the session package
        self.session_package = Package::new("test_session".to_string());
    }

    /// Get the current context (for advanced testing scenarios)
    pub fn context(&self) -> &InterpreterContext {
        &self.context
    }

    /// Get mutable access to the context (for advanced testing scenarios)  
    pub fn context_mut(&mut self) -> &mut InterpreterContext {
        &mut self.context
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
    fn test_harness_creation() {
        let harness = OutrunTestHarness::new();
        assert!(harness.is_ok());
    }

    #[test]
    fn test_basic_integer_evaluation() {
        let mut harness = OutrunTestHarness::new().unwrap();

        let result = harness.evaluate("42").unwrap();
        assert_eq!(result, Value::integer(42));

        // Test assertion method
        harness.assert_evaluates_to_integer("42", 42).unwrap();
    }

    #[test]
    fn test_basic_string_evaluation() {
        let mut harness = OutrunTestHarness::new().unwrap();

        let result = harness.evaluate("\"hello\"").unwrap();
        assert_eq!(result, Value::string("hello".to_string()));

        // Test assertion method
        harness
            .assert_evaluates_to_string("\"hello\"", "hello")
            .unwrap();
    }

    #[test]
    fn test_basic_boolean_evaluation() {
        let mut harness = OutrunTestHarness::new().unwrap();

        harness.assert_evaluates_to_boolean("true", true).unwrap();
        harness.assert_evaluates_to_boolean("false", false).unwrap();
    }

    #[test]
    fn test_variable_binding() {
        let mut harness = OutrunTestHarness::new().unwrap();

        // Test let binding evaluation
        let result = harness.evaluate("let x = 42").unwrap();
        assert_eq!(result, Value::integer(42));

        // Test variable access
        let result = harness.evaluate("x").unwrap();
        assert_eq!(result, Value::integer(42));

        // Test variable assertion
        harness.assert_evaluates_to_integer("x", 42).unwrap();
    }

    #[test]
    fn test_context_management() {
        let mut harness = OutrunTestHarness::new().unwrap();

        // Define variable manually
        harness
            .define_variable("test_var", Value::string("test_value".to_string()))
            .unwrap();

        // Check variable exists
        let var_value = harness.get_variable("test_var").unwrap();
        assert_eq!(*var_value, Value::string("test_value".to_string()));

        // Use variable in expression
        harness
            .assert_evaluates_to_string("test_var", "test_value")
            .unwrap();

        // Clear variables
        harness.clear_variables();

        // Variable should no longer exist
        let result = harness.get_variable("test_var");
        assert!(result.is_err());
    }

    #[test]
    fn test_error_handling() {
        let mut harness = OutrunTestHarness::new().unwrap();

        // Test parse error
        let result = harness.evaluate("invalid syntax 123 @#$");
        assert!(result.is_err());

        // Test variable not found error
        let result = harness.evaluate("nonexistent_variable");
        assert!(result.is_err());

        // Test type assertion failure
        let result = harness.assert_evaluates_to_integer("\"not_an_integer\"", 42);
        assert!(result.is_err());
    }
}
