//! Interpreter session for the Outrun interpreter system
//!
//! This module provides an interpreter session that allows:
//! - Execute arbitrary Outrun code expressions using the interpreter
//! - Assert on the evaluated results with type checking
//! - Set up variables and context for testing scenarios
//! - Handle parser → typechecker → interpreter pipeline gracefully
//! - Maintain variable persistence across evaluations (REPL-like behavior)
//! - Track variable types for proper compilation with operators

use crate::{EvaluationError, ExpressionEvaluator, InterpreterContext, Value};
use miette::Diagnostic;
use outrun_parser::{Expression, ExpressionKind, ParseError, parse_program};
use outrun_typechecker::{CompilationResult, CompilerError, Package};
use thiserror::Error;

/// Check if an expression contains operators that need desugaring
fn expression_contains_operators(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::BinaryOp(_) | ExpressionKind::UnaryOp(_) => true,

        ExpressionKind::Parenthesized(inner) => expression_contains_operators(inner),

        ExpressionKind::FunctionCall(call) => {
            // Check if any arguments contain operators
            call.arguments.iter().any(|arg| match arg {
                outrun_parser::Argument::Named { expression, .. } => {
                    expression_contains_operators(expression)
                }
                _ => false,
            })
        }

        ExpressionKind::FieldAccess(field_access) => {
            expression_contains_operators(&field_access.object)
        }

        ExpressionKind::IfExpression(if_expr) => {
            expression_contains_operators(&if_expr.condition)
                || if_expr.then_block.statements.iter().any(|stmt| {
                    if let outrun_parser::StatementKind::Expression(e) = &stmt.kind {
                        expression_contains_operators(e)
                    } else {
                        false
                    }
                })
                || if_expr.else_block.as_ref().is_some_and(|block| {
                    block.statements.iter().any(|stmt| {
                        if let outrun_parser::StatementKind::Expression(e) = &stmt.kind {
                            expression_contains_operators(e)
                        } else {
                            false
                        }
                    })
                })
        }

        ExpressionKind::CaseExpression(case_expr) => {
            expression_contains_operators(&case_expr.expression)
                || case_expr.clauses.iter().any(|clause| {
                    let result_has_ops = match &clause.result {
                        outrun_parser::CaseResult::Expression(e) => {
                            expression_contains_operators(e)
                        }
                        outrun_parser::CaseResult::Block(block) => {
                            block.statements.iter().any(|stmt| {
                                if let outrun_parser::StatementKind::Expression(e) = &stmt.kind {
                                    expression_contains_operators(e)
                                } else {
                                    false
                                }
                            })
                        }
                    };
                    result_has_ops
                        || clause
                            .guard
                            .as_ref()
                            .is_some_and(expression_contains_operators)
                })
        }

        _ => false,
    }
}

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

    #[error(transparent)]
    #[diagnostic(transparent)]
    Compiler {
        #[from]
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

    #[error("Session compilation failed: {source}. Session state preserved.")]
    SessionCompilationFailed {
        source: Box<CompilerError>,
    },

    #[error("Type definition conflict: {conflict_type}. Use different name or clear session.")]
    TypeDefinitionConflict {
        conflict_type: String,
        existing_definition: String,
        suggested_resolution: String,
    },

    #[error("Session state corruption detected: {issue}. Session has been automatically reset.")]
    SessionStateCorruption {
        issue: String,
        recovery_action: String,
    },

    #[error("Memory limit exceeded in session: {current_size} bytes. Consider clearing session state.")]
    SessionMemoryLimit {
        current_size: usize,
        limit: usize,
        suggested_action: String,
    },

    #[error("Session validation failed: {validation_error}. Session state may be inconsistent.")]
    SessionValidationFailed {
        validation_error: String,
        session_state_summary: String,
    },

    #[error("Compilation dependency cycle detected: {cycle_description}. Cannot proceed with evaluation.")]
    DependencyCycle {
        cycle_description: String,
        affected_types: Vec<String>,
    },
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

/// Interpreter session for evaluating Outrun expressions
pub struct InterpreterSession {
    /// Expression evaluator for running code (cached after first use)
    evaluator: Option<ExpressionEvaluator>,
    /// Interpreter context with variables and state
    context: InterpreterContext,
    /// Pre-compiled core library (compiled once for efficiency)
    core_compilation: Option<CompilationResult>,
    /// Session package that accumulates all evaluations for variable persistence
    session_package: Package,
    /// Type information for session variables (for proper compilation with operators)
    session_variable_types: std::collections::HashMap<String, outrun_typechecker::types::Type>,
    /// Current session compilation result (persists type definitions)
    session_compilation: Option<CompilationResult>,
    /// Persistent struct definitions for registry tracking
    session_struct_definitions: std::collections::HashSet<String>,
}

impl InterpreterSession {
    /// Create a new interpreter session
    pub fn new() -> Result<Self, TestHarnessError> {
        let context = InterpreterContext::new();

        Ok(Self {
            evaluator: None,
            context,
            core_compilation: None,
            session_package: Package::new("test_session".to_string()),
            session_variable_types: std::collections::HashMap::new(),
            session_compilation: None,
            session_struct_definitions: std::collections::HashSet::new(),
        })
    }

    /// Execute an Outrun expression and return the result using the full pipeline
    pub fn evaluate(&mut self, expression_code: &str) -> Result<Value, TestHarnessError> {
        let (value, _type_info) = self.evaluate_with_type_info(expression_code)?;
        Ok(value)
    }

    /// Execute an Outrun expression and return both the result and type information
    pub fn evaluate_with_type_info(
        &mut self,
        expression_code: &str,
    ) -> Result<(Value, Option<String>), TestHarnessError> {
        // Precompile core library on first use
        if self.core_compilation.is_none() {
            self.core_compilation = Some(CompilationResult::precompile_core_library()?);
        }

        // Parse first (fast operation, fails quickly on syntax errors)
        let parsed_program = parse_program(expression_code)?;

        // Create temporary session package with new program  
        let mut candidate_package = Package::new(self.session_package.package_name.clone());
        for program in &self.session_package.programs {
            candidate_package.add_program(program.clone());
        }
        candidate_package.add_program(parsed_program.clone());

        // Prepare core library dependency
        let core_compilation = self.core_compilation.as_ref().unwrap();
        let dependencies = vec![core_compilation.clone()];

        // Before attempting recompilation, detect type redefinitions and invalidate variables
        let redefined_types = Self::detect_type_redefinitions(&candidate_package, &self.session_compilation)?;
        
        if !redefined_types.is_empty() {
            // Check which session variables are affected
            let affected_variables = Self::find_affected_variables(&redefined_types, &self.session_variable_types)?;
            
            if !affected_variables.is_empty() {
                // Warn user and invalidate affected variables
                self.warn_and_invalidate_variables(&affected_variables)?;
            }
        }

        // CRITICAL: Use recompile_package for hot-reload behavior
        // This allows redefinition with content-aware warnings
        let compilation_result = match CompilationResult::recompile_package(
            &mut candidate_package,
            self.session_compilation.as_ref(), // Previous compilation for change detection  
            dependencies
        ) {
            Ok(result) => result,
            Err(compiler_error) => {
                // Compilation failed - session state remains unchanged
                // Try fallback approach for simple variable references
                return self.try_simple_variable_fallback(expression_code, &parsed_program, &compiler_error);
            }
        };

        // Compilation succeeded - commit to session state
        self.session_package = candidate_package;
        self.session_compilation = Some(compilation_result.clone());

        // Update evaluator with new registries
        self.evaluator = Some(ExpressionEvaluator::with_universal_dispatch(
            compilation_result.dispatch_table,
            compilation_result.function_registry,
            compilation_result.universal_dispatch,
            compilation_result.type_registry,
        ));

        // Phase 2: Track struct definitions for persistent registry
        for program in &parsed_program.items {
            if let outrun_parser::ItemKind::StructDefinition(struct_def) = &program.kind {
                // Get struct name from first TypeIdentifier (simple struct names have length 1)
                if let Some(first_id) = struct_def.name.first() {
                    let struct_name = first_id.name.clone();
                    self.session_struct_definitions.insert(struct_name);
                    println!("📝 Registered struct definition: {}", first_id.name);
                }
            }
        }

        // Extract type information from let bindings if present  
        for item in &parsed_program.items {
            if let outrun_parser::ItemKind::LetBinding(let_binding) = &item.kind {
                if let outrun_parser::Pattern::Identifier(identifier) = &let_binding.pattern {
                    if let Some(ref type_info) = let_binding.expression.type_info {
                        // Store the type for this variable
                        let type_obj = outrun_typechecker::types::Type::concrete(
                            &type_info.resolved_type,
                        );
                        self.session_variable_types.insert(identifier.name.clone(), type_obj);
                    } else {
                        // Try to infer type from the expression if it's a literal
                        if let outrun_parser::ExpressionKind::Integer(_) = &let_binding.expression.kind {
                            let type_obj = outrun_typechecker::types::Type::concrete(
                                "Outrun.Core.Integer64",
                            );
                            self.session_variable_types.insert(identifier.name.clone(), type_obj);
                        }
                    }
                }
            }
        }

        // Process all items in the program
        let mut last_value = crate::Value::Boolean(true);
        let mut last_type_info: Option<String> = None;

        for item in &parsed_program.items {
            match &item.kind {
                outrun_parser::ItemKind::Expression(expr) => {
                    // Evaluate the expression with our interpreter
                    let evaluator = self.evaluator.as_ref().unwrap(); // Should always be Some by this point
                    last_value = evaluator.evaluate(expr, &mut self.context)?;

                    // Extract type information from the expression
                    last_type_info = expr.type_info.as_ref().map(|ti| ti.resolved_type.clone());
                }
                outrun_parser::ItemKind::LetBinding(let_binding) => {
                    // Handle let bindings - works whether we skip typecheck or not
                    last_value = self.evaluate_let_binding(let_binding)?;

                    // Extract type information from the let binding expression
                    last_type_info = let_binding
                        .expression
                        .type_info
                        .as_ref()
                        .map(|ti| ti.resolved_type.clone());
                }
                outrun_parser::ItemKind::StructDefinition(_) => {
                    // Struct definitions are handled during compilation, nothing to do at runtime
                    last_value = crate::Value::Boolean(true);
                    last_type_info = Some("Outrun.Core.Boolean".to_string());
                }
                outrun_parser::ItemKind::ImplBlock(_) => {
                    // Implementation blocks are handled during compilation, nothing to do at runtime
                    last_value = crate::Value::Boolean(true);
                    last_type_info = Some("Outrun.Core.Boolean".to_string());
                }
                _ => {
                    return Err(TestHarnessError::Internal {
                        message: format!("Unsupported item type in test harness: {:?}", item.kind),
                    });
                }
            }
        }

        if parsed_program.items.is_empty() {
            return Err(TestHarnessError::Internal {
                message: "No items found in parsed program".to_string(),
            });
        }

        Ok((last_value, last_type_info))
    }

    /// Detect type redefinitions between candidate package and previous compilation
    fn detect_type_redefinitions(
        candidate_package: &Package,
        previous_compilation: &Option<CompilationResult>
    ) -> Result<std::collections::HashSet<String>, TestHarnessError> {
        let mut redefined_types = std::collections::HashSet::new();
        
        if let Some(prev_comp) = previous_compilation {
            // Compare struct definitions in new package vs previous compilation
            for program in &candidate_package.programs {
                for item in &program.items {
                    if let outrun_parser::ItemKind::StructDefinition(struct_def) = &item.kind {
                        // Get struct name from first TypeIdentifier (simple struct names have length 1)
                        let type_name = if let Some(first_id) = struct_def.name.first() {
                            first_id.name.clone()
                        } else {
                            continue; // Skip empty name (shouldn't happen but be safe)
                        };
                        
                        // Check if this type existed before (simple heuristic for now)
                        // In a full implementation, we'd compare actual struct signatures
                        for prev_program in &prev_comp.programs {
                            for prev_item in &prev_program.items {
                                if let outrun_parser::ItemKind::StructDefinition(prev_struct_def) = &prev_item.kind {
                                    if let Some(prev_first_id) = prev_struct_def.name.first() {
                                        if prev_first_id.name == type_name {
                                        // Found same type name - assume it's a redefinition
                                        // TODO: Add content-based comparison here
                                        redefined_types.insert(type_name.clone());
                                        break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        Ok(redefined_types)
    }

    /// Find variables affected by type redefinitions
    fn find_affected_variables(
        redefined_types: &std::collections::HashSet<String>,
        session_variable_types: &std::collections::HashMap<String, outrun_typechecker::types::Type>
    ) -> Result<Vec<String>, TestHarnessError> {
        let mut affected_variables = Vec::new();
        
        // Check session variable types against redefined types
        for (var_name, var_type) in session_variable_types {
            if Self::type_references_redefined_struct(var_type, redefined_types) {
                affected_variables.push(var_name.clone());
            }
        }
        
        Ok(affected_variables)
    }

    /// Check if a type references any of the redefined struct types
    fn type_references_redefined_struct(
        var_type: &outrun_typechecker::types::Type,
        redefined_types: &std::collections::HashSet<String>
    ) -> bool {
        match var_type {
            outrun_typechecker::types::Type::Concrete { name, args, .. } => {
                // Direct reference to redefined type
                if redefined_types.contains(name.as_str()) {
                    return true;
                }
                // Check type arguments recursively
                args.iter().any(|arg| Self::type_references_redefined_struct(arg, redefined_types))
            },
            // TODO: Add support for other Type variants as needed
            _ => false
        }
    }

    /// Warn user and invalidate affected variables
    fn warn_and_invalidate_variables(
        &mut self,
        affected_variables: &[String]
    ) -> Result<(), TestHarnessError> {
        for var_name in affected_variables {
            // Remove from interpreter context  
            if let Ok(_removed_value) = self.context.remove_variable(var_name) {
                // Remove from session type tracking
                self.session_variable_types.remove(var_name);
                
                // Log warning for user
                eprintln!("⚠️  Variable '{}' removed due to type redefinition. Please recreate it with the new type definition.", var_name);
            }
        }
        
        Ok(())
    }

    /// Phase 4: Enhanced fallback approach with better error handling
    fn try_simple_variable_fallback(
        &mut self, 
        expression_code: &str,
        parsed_program: &outrun_parser::Program,
        compiler_error: &CompilerError
    ) -> Result<(Value, Option<String>), TestHarnessError> {
        eprintln!("🔄 Attempting variable fallback for expression: {}", expression_code);
        // Check if this is a variable-not-found error
        let variable_name = if let CompilerError::Typecheck(boxed_err) = compiler_error {
            if let outrun_typechecker::TypecheckError::InferenceError(inference_err) = boxed_err.as_ref() {
                match inference_err {
                    // Handle AmbiguousType with "Unknown identifier" message
                    outrun_typechecker::InferenceError::AmbiguousType { suggestions, .. } => {
                        suggestions.iter().find_map(|s| {
                            if s.starts_with("Unknown identifier: ") {
                                Some(s.trim_start_matches("Unknown identifier: ").to_string())
                            } else {
                                None
                            }
                        })
                    },
                    // Also handle UndefinedVariable (in case typechecker uses this in the future)
                    outrun_typechecker::InferenceError::UndefinedVariable { variable_name, .. } => {
                        Some(variable_name.clone())
                    },
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        };

        if let Some(var_name) = variable_name {
            // Check if we have this variable in our interpreter context
            if self.context.get_variable(&var_name).is_ok() {
                // Check if this is a simple variable reference or complex expression
                let needs_full_compilation = if let Some(first_item) = parsed_program.items.first() {
                    match &first_item.kind {
                        outrun_parser::ItemKind::Expression(expr) => {
                            expression_contains_operators(expr)
                        }
                        _ => false,
                    }
                } else {
                    false
                };

                if needs_full_compilation {
                    // Complex expression - can't handle with fallback
                    return Err(TestHarnessError::Internal {
                        message: format!(
                            "Variable '{}' found in interpreter but compilation still failed. \
                             This may indicate a type mismatch or other compilation issue.",
                            var_name
                        ),
                    });
                } else {
                    // Simple variable reference - use core compilation evaluator
                    let core_compilation = self.core_compilation.as_ref().unwrap();
                    self.evaluator = Some(ExpressionEvaluator::with_universal_dispatch(
                        core_compilation.dispatch_table.clone(),
                        core_compilation.function_registry.clone(),
                        core_compilation.universal_dispatch.clone(),
                        core_compilation.type_registry.clone(),
                    ));

                    // Process the program items
                    let mut last_value = crate::Value::Boolean(true);
                    let mut last_type_info: Option<String> = None;

                    for item in &parsed_program.items {
                        match &item.kind {
                            outrun_parser::ItemKind::Expression(expr) => {
                                let evaluator = self.evaluator.as_ref().unwrap();
                                last_value = evaluator.evaluate(expr, &mut self.context)?;
                                last_type_info = expr.type_info.as_ref().map(|ti| ti.resolved_type.clone());
                            }
                            _ => {
                                return Err(TestHarnessError::Internal {
                                    message: "Only simple expressions supported in fallback mode".to_string(),
                                });
                            }
                        }
                    }

                    return Ok((last_value, last_type_info));
                }
            }
        }

        // Phase 4: Enhanced error reporting for compilation failures
        eprintln!("❌ Fallback approach unsuccessful. Original compilation error: {}", compiler_error);
        
        // Check if this is a dependency cycle issue
        if format!("{}", compiler_error).contains("cycle") || format!("{}", compiler_error).contains("recursive") {
            // Try to extract type information for better error reporting
            let affected_types = self.extract_types_from_error_message(&format!("{}", compiler_error));
            
            return Err(TestHarnessError::DependencyCycle {
                cycle_description: format!("Compilation failed due to potential dependency cycle: {}", compiler_error),
                affected_types,
            });
        }
        
        // Provide enhanced session compilation error with context
        Err(TestHarnessError::Internal {
            message: format!("Session compilation failed: {}. Session state preserved.", compiler_error),
        })
    }
    
    /// Phase 4: Extract type names from error messages for better diagnostics
    fn extract_types_from_error_message(&self, error_message: &str) -> Vec<String> {
        let mut types = Vec::new();
        
        // Simple heuristic to extract type names from error messages
        // Look for capitalized words that might be type names
        for word in error_message.split_whitespace() {
            let clean_word = word.trim_matches(|c: char| !c.is_alphanumeric());
            if clean_word.len() > 1 && clean_word.chars().next().unwrap().is_uppercase() {
                // Also check if it's one of our known struct definitions
                if self.session_struct_definitions.contains(clean_word) {
                    types.push(clean_word.to_string());
                }
            }
        }
        
        types
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

        // Type information is already extracted in the evaluate method when compilation succeeds
        // No need to duplicate here

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

    /// Clear all variables from the test context with enhanced memory cleanup
    pub fn clear_variables(&mut self) {
        // Phase 4: Enhanced session cleanup with validation
        self.validate_session_state_before_clear();
        
        // Explicit memory cleanup before reset
        self.cleanup_session_memory();
        
        // Reset all session state
        self.context = InterpreterContext::new();
        self.session_package = Package::new("test_session".to_string());
        self.session_variable_types.clear();
        self.session_compilation = None;
        self.evaluator = None;
        self.session_struct_definitions.clear();
        
        // Log session reset for debugging
        eprintln!("🔄 Session state cleared and reset successfully");
    }
    
    /// Validate session state integrity before major operations
    fn validate_session_state_before_clear(&self) {
        let context_vars = self.context.get_available_variables().len();
        let session_types = self.session_variable_types.len();
        let struct_defs = self.session_struct_definitions.len();
        let programs = self.session_package.programs.len();
        
        // Log session state summary for debugging
        eprintln!("📊 Session state before clear: {} vars, {} types, {} structs, {} programs", 
                 context_vars, session_types, struct_defs, programs);
                 
        // Check for potential memory issues
        if programs > 100 {
            eprintln!("⚠️  Large session detected: {} programs. Consider periodic clearing.", programs);
        }
    }
    
    /// Clean up session memory before reset
    fn cleanup_session_memory(&mut self) {
        // Force cleanup of large data structures
        if self.session_package.programs.len() > 10 {
            eprintln!("🧹 Cleaning up {} accumulated programs", self.session_package.programs.len());
        }
        
        // Clear evaluator cache first to free dispatch tables
        if self.evaluator.is_some() {
            eprintln!("🧹 Clearing cached evaluator and dispatch tables");
            self.evaluator = None;
        }
        
        // Clear compilation cache
        if self.session_compilation.is_some() {
            eprintln!("🧹 Clearing cached compilation result");
            self.session_compilation = None;
        }
    }

    /// Get the current context (for advanced testing scenarios)
    pub fn context(&self) -> &InterpreterContext {
        &self.context
    }

    /// Get mutable access to the context (for advanced testing scenarios)  
    pub fn context_mut(&mut self) -> &mut InterpreterContext {
        &mut self.context
    }
    
    /// Phase 4: Validate current session state for integrity
    pub fn validate_session_state(&self) -> Result<(), TestHarnessError> {
        // Check for consistency between context variables and session variable types
        let context_vars = self.context.get_available_variables();
        let missing_types: Vec<_> = context_vars.iter()
            .filter(|var| !self.session_variable_types.contains_key(*var))
            .collect();
            
        if !missing_types.is_empty() {
            return Err(TestHarnessError::SessionValidationFailed {
                validation_error: format!("Variables without type tracking: {:?}", missing_types),
                session_state_summary: self.get_session_summary(),
            });
        }
        
        // Check for struct definitions without corresponding types
        let orphaned_structs: Vec<_> = self.session_struct_definitions.iter()
            .filter(|struct_name| {
                // Check if any variables have this struct type
                !self.session_variable_types.values().any(|var_type| {
                    match var_type {
                        outrun_typechecker::types::Type::Concrete { name, .. } => {
                            name.as_str().contains(struct_name.as_str())
                        },
                        _ => false
                    }
                })
            })
            .collect();
            
        if orphaned_structs.len() > 10 {  // Only warn for many orphaned structs
            eprintln!("⚠️  Found {} struct definitions without corresponding variables. Consider session cleanup.", 
                     orphaned_structs.len());
        }
        
        Ok(())
    }
    
    /// Get a summary of current session state
    pub fn get_session_summary(&self) -> String {
        format!("Session: {} vars, {} types, {} structs, {} programs",
               self.context.get_available_variables().len(),
               self.session_variable_types.len(),
               self.session_struct_definitions.len(),
               self.session_package.programs.len())
    }
    
    /// Phase 4: Check for potential memory issues in session
    pub fn check_session_memory_usage(&self) -> Result<(), TestHarnessError> {
        let program_count = self.session_package.programs.len();
        let variable_count = self.session_variable_types.len();
        let struct_count = self.session_struct_definitions.len();
        
        // Rough memory usage estimation (very conservative)
        let estimated_bytes = (program_count * 1024) + (variable_count * 256) + (struct_count * 512);
        const MEMORY_LIMIT: usize = 10 * 1024 * 1024; // 10MB rough limit
        
        if estimated_bytes > MEMORY_LIMIT {
            return Err(TestHarnessError::SessionMemoryLimit {
                current_size: estimated_bytes,
                limit: MEMORY_LIMIT,
                suggested_action: "Consider calling clear_variables() to reset session state".to_string(),
            });
        }
        
        if program_count > 50 {
            eprintln!("⚠️  Session has {} accumulated programs. Performance may degrade.", program_count);
        }
        
        Ok(())
    }
}

impl Default for InterpreterSession {
    fn default() -> Self {
        Self::new().expect("Failed to create default test harness")
    }
}

// Phase 4: Enhanced session management utilities
impl InterpreterSession {
    /// Create a new session with validation and error recovery
    pub fn new_with_validation() -> Result<Self, TestHarnessError> {
        let session = Self::new()?;
        
        // Validate initial state
        session.validate_session_state()?;
        
        eprintln!("✅ New interpreter session created and validated");
        Ok(session)
    }
    
    /// Evaluate with enhanced error handling and session recovery
    pub fn evaluate_with_recovery(&mut self, expression_code: &str) -> Result<Value, TestHarnessError> {
        // Phase 4: Pre-evaluation validation
        if let Err(validation_error) = self.validate_session_state() {
            eprintln!("⚠️  Session validation warning: {}", validation_error);
            // Continue anyway for now, but log the issue
        }
        
        // Check memory usage before evaluation
        self.check_session_memory_usage()?;
        
        // Attempt normal evaluation
        match self.evaluate(expression_code) {
            Ok(value) => {
                // Post-evaluation validation
                if let Err(post_validation_error) = self.validate_session_state() {
                    eprintln!("⚠️  Post-evaluation session inconsistency: {}", post_validation_error);
                }
                Ok(value)
            },
            Err(evaluation_error) => {
                // Enhanced error recovery
                eprintln!("❌ Evaluation failed: {}. Checking session integrity...", evaluation_error);
                
                // Check if session state is still valid after failure
                match self.validate_session_state() {
                    Ok(()) => {
                        eprintln!("✅ Session state remains valid after evaluation failure");
                        Err(evaluation_error)
                    },
                    Err(_validation_error) => {
                        eprintln!("💥 Session state corrupted after evaluation failure. Attempting recovery...");
                        
                        // Attempt automatic recovery
                        if let Err(recovery_error) = self.attempt_session_recovery() {
                            Err(TestHarnessError::SessionStateCorruption {
                                issue: format!("Evaluation failed and session recovery failed: {}", recovery_error),
                                recovery_action: "Session has been reset. Please retry operation.".to_string(),
                            })
                        } else {
                            eprintln!("🔧 Session recovery successful. Original error was: {}", evaluation_error);
                            Err(evaluation_error)
                        }
                    }
                }
            }
        }
    }
    
    /// Attempt to recover from session state corruption
    fn attempt_session_recovery(&mut self) -> Result<(), TestHarnessError> {
        eprintln!("🔧 Attempting session recovery...");
        
        // Strategy 1: Clear cached evaluator and try to rebuild
        self.evaluator = None;
        
        // Strategy 2: If that fails, clear compilation cache
        if self.validate_session_state().is_err() {
            self.session_compilation = None;
            eprintln!("🔧 Cleared compilation cache for recovery");
        }
        
        // Strategy 3: Last resort - full session reset
        if self.validate_session_state().is_err() {
            eprintln!("🔧 Full session reset required for recovery");
            self.clear_variables();
        }
        
        // Final validation
        self.validate_session_state()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_harness_creation() {
        let harness = InterpreterSession::new();
        assert!(harness.is_ok());
    }

    #[test]
    fn test_basic_integer_evaluation() {
        let mut harness = InterpreterSession::new().unwrap();

        let result = harness.evaluate("42").unwrap();
        assert_eq!(result, Value::integer(42));

        // Test assertion method
        harness.assert_evaluates_to_integer("42", 42).unwrap();
    }

    #[test]
    fn test_basic_string_evaluation() {
        let mut harness = InterpreterSession::new().unwrap();

        let result = harness.evaluate("\"hello\"").unwrap();
        assert_eq!(result, Value::string("hello".to_string()));

        // Test assertion method
        harness
            .assert_evaluates_to_string("\"hello\"", "hello")
            .unwrap();
    }

    #[test]
    fn test_basic_boolean_evaluation() {
        let mut harness = InterpreterSession::new().unwrap();

        harness.assert_evaluates_to_boolean("true", true).unwrap();
        harness.assert_evaluates_to_boolean("false", false).unwrap();
    }

    #[test]
    fn test_variable_binding() {
        let mut harness = InterpreterSession::new().unwrap();

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
        let mut harness = InterpreterSession::new().unwrap();

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
        let mut harness = InterpreterSession::new().unwrap();

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
