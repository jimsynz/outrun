//! Expression evaluator for parser AST with type information
//!
//! This module provides the core expression evaluation engine that takes
//! parser Expression nodes with attached ParsedTypeInfo and evaluates them
//! to runtime Values using the interpreter context.

use crate::context::{InterpreterContext, InterpreterError};
use crate::value::Value;
use miette::Diagnostic;
use outrun_parser::{Expression, ExpressionKind};
use outrun_typechecker::{DispatchTable, FunctionRegistry};
use thiserror::Error;

/// Errors that can occur during expression evaluation
#[derive(Debug, Error, Diagnostic)]
pub enum EvaluationError {
    #[error("Context error: {source}")]
    Context {
        #[from]
        source: InterpreterError,
    },

    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: outrun_parser::Span,
    },

    #[error("Variable '{name}' not found")]
    VariableNotFound {
        name: String,
        span: outrun_parser::Span,
    },

    #[error("Function '{name}' not found")]
    FunctionNotFound {
        name: String,
        span: outrun_parser::Span,
    },

    #[error("Invalid condition type: expected Boolean, found {found}")]
    InvalidConditionType {
        found: String,
        span: outrun_parser::Span,
    },

    #[error("Unsupported expression type: {expr_type}")]
    UnsupportedExpression {
        expr_type: String,
        span: outrun_parser::Span,
    },

    #[error("Runtime error: {message}")]
    Runtime {
        message: String,
        span: outrun_parser::Span,
    },

    #[error("Intrinsic error: {source}")]
    Intrinsic {
        #[from]
        source: crate::intrinsics::IntrinsicError,
    },

    #[error("Pattern matching error: {source}")]
    PatternMatch {
        #[from]
        source: crate::pattern::PatternMatchError,
    },
}

/// Expression evaluator for the new interpreter system
///
/// This evaluator works with parser AST nodes that have been augmented
/// with type information from typechecker v3.
pub struct ExpressionEvaluator {
    /// Intrinsics handler for executing built-in operations
    intrinsics: crate::intrinsics::IntrinsicsHandler,
    /// Dispatch table for runtime protocol function resolution
    dispatch_table: DispatchTable,
    /// Function registry for looking up user-defined function bodies
    function_registry: std::rc::Rc<FunctionRegistry>,
}

impl ExpressionEvaluator {
    /// Create a new expression evaluator with a dispatch table and function registry
    pub fn with_dispatch_table(
        dispatch_table: DispatchTable,
        function_registry: std::rc::Rc<FunctionRegistry>,
    ) -> Self {
        Self {
            intrinsics: crate::intrinsics::IntrinsicsHandler::new(),
            dispatch_table,
            function_registry,
        }
    }

    /// Create a new expression evaluator with empty dispatch table (for tests)
    pub fn new() -> Self {
        Self {
            intrinsics: crate::intrinsics::IntrinsicsHandler::new(),
            dispatch_table: DispatchTable::new(),
            function_registry: std::rc::Rc::new(FunctionRegistry::new()),
        }
    }

    /// Evaluate an item (top-level program construct)
    pub fn evaluate_item(
        &self,
        item: &outrun_parser::Item,
        context: &mut InterpreterContext,
    ) -> Result<Option<Value>, EvaluationError> {
        match &item.kind {
            outrun_parser::ItemKind::LetBinding(let_binding) => {
                self.evaluate_let_binding(let_binding, context)?;
                Ok(None) // Let bindings don't produce values
            }
            _ => {
                // For now, only handle let bindings at the top level
                Err(EvaluationError::UnsupportedExpression {
                    expr_type: "unsupported_item".to_string(),
                    span: item.span,
                })
            }
        }
    }

    /// Evaluate a statement
    pub fn evaluate_statement(
        &self,
        statement: &outrun_parser::Statement,
        context: &mut InterpreterContext,
    ) -> Result<Option<Value>, EvaluationError> {
        match &statement.kind {
            outrun_parser::StatementKind::Expression(expr) => {
                let value = self.evaluate(expr, context)?;
                Ok(Some(value))
            }
            outrun_parser::StatementKind::LetBinding(let_binding) => {
                self.evaluate_let_binding(let_binding, context)?;
                Ok(None) // Let bindings don't produce values
            }
        }
    }

    /// Evaluate an expression to a runtime value
    pub fn evaluate(
        &self,
        expression: &Expression,
        context: &mut InterpreterContext,
    ) -> Result<Value, EvaluationError> {
        // Push call frame for error reporting
        context.push_call_frame(
            format!("evaluate_{}", self.expression_type_name(&expression.kind)),
            Some(expression.span),
        )?;

        let result = self.evaluate_expression_kind(&expression.kind, expression.span, context);

        // Pop call frame
        context.pop_call_frame();

        result
    }

    /// Evaluate different kinds of expressions
    fn evaluate_expression_kind(
        &self,
        kind: &ExpressionKind,
        span: outrun_parser::Span,
        context: &mut InterpreterContext,
    ) -> Result<Value, EvaluationError> {
        match kind {
            // Literals - direct conversion to values
            ExpressionKind::Integer(literal) => Ok(Value::integer(literal.value)),

            ExpressionKind::Float(literal) => Ok(Value::float(literal.value)),

            ExpressionKind::Boolean(literal) => Ok(Value::boolean(literal.value)),

            ExpressionKind::String(literal) => {
                // Handle string content with basic interpolation support
                let content = literal
                    .parts
                    .iter()
                    .filter_map(|part| match part {
                        outrun_parser::StringPart::Text { content, .. } => Some(content.clone()),
                        outrun_parser::StringPart::Interpolation { .. } => None, // Skip for now
                    })
                    .collect::<Vec<_>>()
                    .join("");
                Ok(Value::string(content))
            }

            ExpressionKind::Atom(literal) => Ok(Value::atom(literal.name.clone())),

            // Variable references
            ExpressionKind::Identifier(identifier) => context
                .get_variable(&identifier.name)
                .map(|v| v.clone())
                .map_err(|_| EvaluationError::VariableNotFound {
                    name: identifier.name.clone(),
                    span,
                }),

            // Case expressions with pattern matching
            ExpressionKind::CaseExpression(case_expr) => {
                self.evaluate_case_expression(case_expr, context)
            }

            // List literals - convert to List values
            ExpressionKind::List(list_literal) => self.evaluate_list_literal(list_literal, context),

            // Tuple literals - convert to Tuple values
            ExpressionKind::Tuple(tuple_literal) => {
                self.evaluate_tuple_literal(tuple_literal, context)
            }

            // Function calls - dispatch through intrinsics system
            ExpressionKind::FunctionCall(function_call) => {
                self.evaluate_function_call(function_call, context, span)
            }

            // For now, return errors for unsupported expressions
            _ => Err(EvaluationError::UnsupportedExpression {
                expr_type: self.expression_type_name(kind).to_string(),
                span,
            }),
        }
    }

    /// Evaluate a case expression with pattern matching
    fn evaluate_case_expression(
        &self,
        case_expr: &outrun_parser::CaseExpression,
        context: &mut InterpreterContext,
    ) -> Result<Value, EvaluationError> {
        // First, evaluate the expression being matched
        let match_value = self.evaluate(&case_expr.expression, context)?;

        // Try each case clause in order
        for clause in &case_expr.clauses {
            // Try to match the pattern
            match crate::pattern::PatternMatcher::match_pattern(
                &clause.pattern,
                &match_value,
                context,
            ) {
                Ok(bindings) => {
                    // Pattern matched! Check optional guard condition
                    let guard_passed = if let Some(guard_expr) = &clause.guard {
                        // Create temporary scope for guard evaluation with bindings
                        context.push_scope();
                        crate::pattern::PatternMatcher::apply_bindings(bindings.clone(), context)?;

                        let guard_result = self.evaluate(guard_expr, context)?;
                        context.pop_scope();

                        // Guard must evaluate to a boolean true
                        guard_result.is_truthy()
                    } else {
                        true // No guard means it always passes
                    };

                    if guard_passed {
                        // Guard passed! Apply bindings and evaluate result
                        context.push_scope();
                        crate::pattern::PatternMatcher::apply_bindings(bindings, context)?;

                        let result = match &clause.result {
                            outrun_parser::CaseResult::Expression(expr) => {
                                self.evaluate(expr, context)?
                            }
                            outrun_parser::CaseResult::Block(block) => {
                                self.evaluate_block(block, context)?
                            }
                        };

                        context.pop_scope();
                        return Ok(result);
                    }
                }
                Err(_) => {
                    // Pattern didn't match, try next clause
                    continue;
                }
            }
        }

        // No pattern matched
        Err(EvaluationError::Runtime {
            message: format!("No case clause matched value: {}", match_value.display()),
            span: case_expr.span,
        })
    }

    /// Evaluate a block of statements/expressions
    fn evaluate_block(
        &self,
        block: &outrun_parser::Block,
        context: &mut InterpreterContext,
    ) -> Result<Value, EvaluationError> {
        // For now, just evaluate statements in order and return the last expression
        // TODO: Implement proper block evaluation with statement handling

        let mut last_result = Value::boolean(true); // Default return value

        for statement in &block.statements {
            match &statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    last_result = self.evaluate(expr, context)?;
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    self.evaluate_let_binding(let_binding, context)?;
                    // Let bindings don't produce values, keep last result
                }
            }
        }

        // For now, Block doesn't have a final_expression field
        // The last result from processing statements is returned

        Ok(last_result)
    }

    /// Evaluate a let binding with pattern matching
    fn evaluate_let_binding(
        &self,
        let_binding: &outrun_parser::LetBinding,
        context: &mut InterpreterContext,
    ) -> Result<(), EvaluationError> {
        // Evaluate the right-hand side expression
        let value = self.evaluate(&let_binding.expression, context)?;

        // Match the pattern and apply bindings
        let bindings =
            crate::pattern::PatternMatcher::match_pattern(&let_binding.pattern, &value, context)?;

        crate::pattern::PatternMatcher::apply_bindings(bindings, context)?;

        Ok(())
    }

    /// Extract string content from string literal (handling interpolation)
    fn extract_string_content(&self, literal: &outrun_parser::StringLiteral) -> String {
        // TODO: Handle string interpolation properly with parts
        // For now, just return a placeholder
        format!("string_literal_{}", literal.span.start)
    }

    /// Evaluate a list literal by evaluating all elements
    fn evaluate_list_literal(
        &self,
        list_literal: &outrun_parser::ListLiteral,
        context: &mut InterpreterContext,
    ) -> Result<Value, EvaluationError> {
        // Start with empty list
        let mut result_list = crate::value::List::Empty;

        // Evaluate elements in reverse order to build the list correctly
        for element in list_literal.elements.iter().rev() {
            let element_value = match element {
                outrun_parser::ListElement::Expression(expr) => self.evaluate(expr, context)?,
                outrun_parser::ListElement::Spread(_) => {
                    // TODO: Implement spread operator support
                    return Err(EvaluationError::UnsupportedExpression {
                        expr_type: "spread_operator".to_string(),
                        span: list_literal.span,
                    });
                }
            };

            // Prepend to the list (builds in reverse order)
            result_list = crate::value::List::Cons {
                head: element_value,
                tail: std::rc::Rc::new(result_list),
            };
        }

        Ok(Value::List {
            list: std::rc::Rc::new(result_list),
            element_type_info: None,
        })
    }

    /// Evaluate a tuple literal by evaluating all elements
    fn evaluate_tuple_literal(
        &self,
        tuple_literal: &outrun_parser::TupleLiteral,
        context: &mut InterpreterContext,
    ) -> Result<Value, EvaluationError> {
        // Evaluate all elements
        let mut elements = Vec::new();
        for element in &tuple_literal.elements {
            let element_value = self.evaluate(element, context)?;
            elements.push(element_value);
        }

        // Create a proper Tuple value
        Ok(Value::tuple(elements))
    }

    /// Evaluate a function call through the intrinsics system
    fn evaluate_function_call(
        &self,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        // Use pre-resolved function key (must be populated by typechecker)
        let function_name = function_call.resolved_function_key.as_ref()
            .unwrap_or_else(|| {
                eprintln!("ERROR: Function call has no resolved_function_key: {:?}", function_call.path);
                panic!("BUG: resolved_function_key must be populated by typechecker before interpretation");
            })
            .clone();

        // Evaluate arguments
        let mut args = Vec::new();
        for arg in &function_call.arguments {
            let arg_value = match arg {
                outrun_parser::Argument::Named { expression, .. } => {
                    self.evaluate(expression, context)?
                }
                outrun_parser::Argument::Spread { expression, .. } => {
                    // TODO: Handle spread arguments properly
                    return Err(EvaluationError::UnsupportedExpression {
                        expr_type: "spread_argument".to_string(),
                        span,
                    });
                }
            };
            args.push(arg_value);
        }

        // Direct dispatch based on pre-resolved function key
        if function_name.starts_with("Outrun.Intrinsic.") {
            // Direct intrinsic call
            self.intrinsics
                .execute_intrinsic(&function_name, &args, span)
                .map_err(|e| EvaluationError::Intrinsic { source: e })
        } else {
            // All other calls - the typechecker has resolved everything for us
            self.evaluate_resolved_function(&function_name, &args, span)
        }
    }

    /// Evaluate a resolved function (may require further dispatch)
    fn evaluate_resolved_function(
        &self,
        function_name: &str,
        args: &[Value],
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        self.evaluate_resolved_function_with_stack(function_name, args, span, &mut Vec::new())
    }

    /// Internal helper for evaluate_resolved_function with cycle detection
    fn evaluate_resolved_function_with_stack(
        &self,
        function_name: &str,
        args: &[Value],
        span: outrun_parser::Span,
        call_stack: &mut Vec<String>,
    ) -> Result<Value, EvaluationError> {
        // Check for infinite recursion
        if call_stack.contains(&function_name.to_string()) {
            return Err(EvaluationError::Runtime {
                message: format!(
                    "Infinite recursion detected in function dispatch: {}",
                    function_name
                ),
                span,
            });
        }

        // Add to call stack
        call_stack.push(function_name.to_string());

        let result = {
            // If the resolved function is an intrinsic, execute it directly
            if function_name.starts_with("Outrun.Intrinsic.") {
                self.intrinsics
                    .execute_intrinsic(function_name, args, span)
                    .map_err(|e| EvaluationError::Intrinsic { source: e })
            } else {
                // This is a user-defined function - evaluate its body directly
                self.evaluate_user_function(function_name, args, span)
            }
        };

        // Remove from call stack
        call_stack.pop();

        result
    }

    /// Evaluate a user-defined function by looking up its body and evaluating it
    fn evaluate_user_function(
        &self,
        function_name: &str,
        args: &[Value],
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        // Step 1: Look up the function in the function registry
        // For impl functions like "impl BinaryAddition for Outrun.Core.Integer64.add",
        // we need to parse the scope and function name
        let (scope, func_name) = if let Some(last_dot) = function_name.rfind('.') {
            let scope = &function_name[..last_dot];
            let func_name = &function_name[last_dot + 1..];
            (scope, func_name)
        } else {
            return Err(EvaluationError::Runtime {
                message: format!("Invalid function name format: {}", function_name),
                span,
            });
        };

        // Step 2: Get function info from registry
        let function_info = match self.function_registry.get_function(scope, func_name) {
            Some(info) => info,
            None => {
                return Err(EvaluationError::FunctionNotFound {
                    name: function_name.to_string(),
                    span,
                });
            }
        };

        // Step 3: Get function body
        let function_body = match &function_info.body {
            Some(body) => body,
            None => {
                return Err(EvaluationError::Runtime {
                    message: format!(
                        "Function has no body (may be intrinsic or signature-only): {}",
                        function_name
                    ),
                    span,
                });
            }
        };

        // Step 4: Validate argument count
        if args.len() != function_info.parameters.len() {
            return Err(EvaluationError::Runtime {
                message: format!(
                    "Argument count mismatch for {}: expected {}, got {}",
                    function_name,
                    function_info.parameters.len(),
                    args.len()
                ),
                span,
            });
        }

        // Step 5: Create parameter bindings and evaluate body
        self.evaluate_function_body_with_bindings(
            function_body,
            &function_info.parameters,
            args,
            span,
        )
    }

    /// Evaluate a function body with parameter bindings
    fn evaluate_function_body_with_bindings(
        &self,
        body: &outrun_parser::Block,
        parameters: &[(String, outrun_typechecker::Type)],
        args: &[Value],
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        let mut context = InterpreterContext::new();

        // Create parameter bindings by mapping args to parameter names
        for (i, (param_name, _param_type)) in parameters.iter().enumerate() {
            if let Some(arg_value) = args.get(i) {
                if let Err(e) = context.define_variable(param_name.clone(), arg_value.clone()) {
                    return Err(EvaluationError::Context { source: e });
                }
            }
        }

        // Evaluate the function body block
        self.evaluate_block(body, &mut context)
    }

    /// Get the runtime type name for dispatch table lookup
    fn get_runtime_type_name(&self, value: &Value) -> String {
        match value {
            Value::Integer64(_) => "Outrun.Core.Integer64".to_string(),
            Value::Float64(_) => "Outrun.Core.Float64".to_string(),
            Value::Boolean(_) => "Outrun.Core.Boolean".to_string(),
            Value::String(_) => "Outrun.Core.String".to_string(),
            Value::Atom(_) => "Outrun.Core.Atom".to_string(),
            Value::List { .. } => "Outrun.Core.List".to_string(), // TODO: Handle generic types properly
            Value::Map { .. } => "Outrun.Core.Map".to_string(), // TODO: Handle generic types properly
            Value::Tuple { .. } => "Outrun.Core.Tuple".to_string(), // TODO: Handle generic types properly
            Value::Struct { type_name, .. } => type_name.clone(),
            Value::Function { .. } => "Function".to_string(), // TODO: Handle function types properly
        }
    }

    /// Get a human-readable name for an expression type
    fn expression_type_name(&self, kind: &ExpressionKind) -> &'static str {
        match kind {
            ExpressionKind::Integer(_) => "integer_literal",
            ExpressionKind::Float(_) => "float_literal",
            ExpressionKind::Boolean(_) => "boolean_literal",
            ExpressionKind::String(_) => "string_literal",
            ExpressionKind::Atom(_) => "atom_literal",
            ExpressionKind::List(_) => "list_literal",
            ExpressionKind::Map(_) => "map_literal",
            ExpressionKind::Identifier(_) => "identifier",
            ExpressionKind::FunctionCall(_) => "function_call",
            ExpressionKind::IfExpression(_) => "if_expression",
            _ => "unsupported_expression",
        }
    }
}

impl Default for ExpressionEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_evaluation() {
        let evaluator = ExpressionEvaluator::new();
        let mut context = InterpreterContext::new();

        // Test integer literal
        let int_expr = create_test_integer_expression(42);
        let result = evaluator.evaluate(&int_expr, &mut context).unwrap();
        assert_eq!(result, Value::integer(42));

        // Test boolean literal
        let bool_expr = create_test_boolean_expression(true);
        let result = evaluator.evaluate(&bool_expr, &mut context).unwrap();
        assert_eq!(result, Value::boolean(true));
    }

    #[test]
    fn test_variable_evaluation() {
        let evaluator = ExpressionEvaluator::new();
        let mut context = InterpreterContext::new();

        // Define a variable
        context
            .define_variable("x".to_string(), Value::integer(42))
            .unwrap();

        // Create identifier expression
        let id_expr = create_test_identifier_expression("x");
        let result = evaluator.evaluate(&id_expr, &mut context).unwrap();
        assert_eq!(result, Value::integer(42));
    }

    // Helper functions for creating test expressions
    fn create_test_integer_expression(value: i64) -> Expression {
        Expression {
            kind: ExpressionKind::Integer(outrun_parser::IntegerLiteral {
                value,
                format: outrun_parser::IntegerFormat::Decimal,
                raw_text: value.to_string(),
                span: test_span(),
            }),
            span: test_span(),
            type_info: None,
        }
    }

    fn create_test_boolean_expression(value: bool) -> Expression {
        Expression {
            kind: ExpressionKind::Boolean(outrun_parser::BooleanLiteral {
                value,
                span: test_span(),
            }),
            span: test_span(),
            type_info: None,
        }
    }

    fn create_test_identifier_expression(name: &str) -> Expression {
        Expression {
            kind: ExpressionKind::Identifier(outrun_parser::Identifier {
                name: name.to_string(),
                span: test_span(),
            }),
            span: test_span(),
            type_info: None,
        }
    }

    fn test_span() -> outrun_parser::Span {
        outrun_parser::Span::new(0, 0)
    }
}
