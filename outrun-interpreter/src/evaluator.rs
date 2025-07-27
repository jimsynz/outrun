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
use outrun_typechecker::universal_dispatch::{UniversalDispatchRegistry, ClauseId, Guard};
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
    /// Universal dispatch registry for clause-based function dispatch
    universal_registry: UniversalDispatchRegistry,
}

impl ExpressionEvaluator {
    /// Create a new expression evaluator with a dispatch table and function registry
    pub fn with_dispatch_table(
        _dispatch_table: DispatchTable,
        _function_registry: std::rc::Rc<FunctionRegistry>,
    ) -> Self {
        Self {
            intrinsics: crate::intrinsics::IntrinsicsHandler::new(),
            universal_registry: UniversalDispatchRegistry::new(),
        }
    }

    /// Create a new expression evaluator with universal dispatch registry
    pub fn with_universal_dispatch(
        _dispatch_table: DispatchTable,
        _function_registry: std::rc::Rc<FunctionRegistry>,
        universal_registry: UniversalDispatchRegistry,
    ) -> Self {
        Self {
            intrinsics: crate::intrinsics::IntrinsicsHandler::new(),
            universal_registry,
        }
    }

    /// Create a new expression evaluator with empty dispatch table (for tests)
    pub fn new() -> Self {
        Self {
            intrinsics: crate::intrinsics::IntrinsicsHandler::new(),
            universal_registry: UniversalDispatchRegistry::new(),
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
                .cloned()
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

            // Unary operations - these should have universal_clause_ids populated by typechecker
            ExpressionKind::UnaryOp(unary_op) => {
                self.evaluate_unary_operation(unary_op, context, span)
            }

            // For now, return errors for unsupported expressions
            _ => {
                eprintln!("🚨 DEBUG: Unsupported expression kind encountered: {:?}", kind);
                eprintln!("🚨 DEBUG: Span: {:?}", span);
                Err(EvaluationError::UnsupportedExpression {
                    expr_type: self.expression_type_name(kind).to_string(),
                    span,
                })
            }
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

    /// Universal dispatch method - handles ALL function types through clause-based dispatch
    fn evaluate_function_call(
        &self,
        function_call: &outrun_parser::FunctionCall,
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        // Check if we have universal clause IDs from the typechecker
        if let Some(clause_ids) = &function_call.universal_clause_ids {
            // Universal dispatch for user functions and protocol implementations
            self.dispatch_clauses(clause_ids, &function_call.arguments, context, span)
        } else {
            // No clause IDs - check if this is an intrinsic function
            let function_name = match &function_call.path {
                outrun_parser::FunctionPath::Qualified { module, name } => {
                    format!("{}.{}", module.name, name.name)
                }
                outrun_parser::FunctionPath::Simple { name } => {
                    panic!("BUG: Simple function calls must have universal_clause_ids populated by typechecker: {}", name.name);
                }
                outrun_parser::FunctionPath::Expression { .. } => {
                    panic!("BUG: Expression function calls must have universal_clause_ids populated by typechecker");
                }
            };

            if function_name.starts_with("Outrun.Intrinsic.") {
                // Direct intrinsic function call
                let mut args = Vec::new();
                for arg in &function_call.arguments {
                    let arg_value = match arg {
                        outrun_parser::Argument::Named { expression, .. } => {
                            self.evaluate(expression, context)?
                        }
                        outrun_parser::Argument::Spread { .. } => {
                            return Err(EvaluationError::UnsupportedExpression {
                                expr_type: "spread_argument".to_string(),
                                span,
                            });
                        }
                    };
                    args.push(arg_value);
                }

                self.intrinsics
                    .execute_intrinsic(&function_name, &args, span)
                    .map_err(|e| EvaluationError::Intrinsic { source: e })
            } else {
                panic!("BUG: Non-intrinsic function call missing universal_clause_ids: {}", function_name);
            }
        }
    }

    /// Evaluate a unary operation using universal dispatch
    fn evaluate_unary_operation(
        &self,
        unary_op: &outrun_parser::UnaryOperation,
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        // Evaluate the operand first
        let operand_value = self.evaluate(&unary_op.operand, context)?;
        
        // Map unary operators to protocol function names
        let protocol_function = match unary_op.operator {
            outrun_parser::UnaryOperator::Minus => "UnaryMinus.minus",
            outrun_parser::UnaryOperator::Plus => "UnaryPlus.plus",
            outrun_parser::UnaryOperator::LogicalNot => "LogicalNot.not",
            _ => {
                return Err(EvaluationError::Runtime {
                    message: format!("Unsupported unary operator: {:?}", unary_op.operator),
                    span,
                });
            }
        };
        
        // Find the clauses for this protocol function in the universal registry
        let matching_clauses = self.find_clauses_for_protocol_function(protocol_function, &operand_value);
        
        if matching_clauses.is_empty() {
            return Err(EvaluationError::Runtime {
                message: format!("No implementation found for {} on type {}", 
                    protocol_function, 
                    self.get_runtime_type_name(&operand_value)
                ),
                span,
            });
        }
        
        // Use the universal dispatch system with pre-evaluated values
        self.dispatch_clauses_with_values(&matching_clauses, &[operand_value], context, span)
    }

    /// Find clause IDs for a protocol function that can handle the given argument type
    fn find_clauses_for_protocol_function(
        &self,
        protocol_function: &str,
        argument_value: &Value,
    ) -> Vec<outrun_typechecker::universal_dispatch::ClauseId> {
        // Parse the protocol function name (e.g., "UnaryMinus.minus")
        let parts: Vec<&str> = protocol_function.split('.').collect();
        if parts.len() != 2 {
            return vec![];
        }

        let protocol_name = parts[0];
        let function_name = parts[1];
        let argument_type = self.get_runtime_type_name(argument_value);

        // Look for implementation signatures that match the pattern:
        // "impl ProtocolName for TypeName"
        let mut matching_clauses = Vec::new();
        
        for sig in self.universal_registry.get_all_function_signatures() {
            // Check if this signature matches our protocol and function
            if sig.function_name == function_name {
                // Check if the module path contains the protocol implementation pattern
                if let Some(module) = sig.module_path.first() {
                    let expected_impl_pattern = format!("impl {} for {}", protocol_name, argument_type);
                    if module == &expected_impl_pattern {
                        let clause_ids = self.universal_registry.get_clauses_for_function(sig);
                        matching_clauses.extend(clause_ids.iter().copied());
                    }
                }
            }
        }

        eprintln!("🔍 DEBUG: Looking for protocol {} function {} on type {}", protocol_name, function_name, argument_type);
        eprintln!("🔍 DEBUG: Found {} matching clauses", matching_clauses.len());

        matching_clauses
    }

    /// Universal clause-based dispatch - the future of function dispatch
    fn dispatch_clauses(
        &self,
        clause_ids: &[u64],
        arguments: &[outrun_parser::Argument],
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        // DEBUG: Add logging to understand clause dispatch issues
        eprintln!("🔍 DEBUG: dispatch_clauses called with {} clause IDs: {:?}", clause_ids.len(), clause_ids);
        
        // Evaluate arguments first
        let mut args = Vec::new();
        for arg in arguments {
            let arg_value = match arg {
                outrun_parser::Argument::Named { expression, .. } => {
                    self.evaluate(expression, context)?
                }
                outrun_parser::Argument::Spread { .. } => {
                    // TODO: Handle spread arguments properly
                    return Err(EvaluationError::UnsupportedExpression {
                        expr_type: "spread_argument".to_string(),
                        span,
                    });
                }
            };
            args.push(arg_value);
        }

        eprintln!("🔍 DEBUG: Evaluated {} arguments: {:?}", args.len(), args.iter().map(|v| v.display()).collect::<Vec<_>>());

        // Try each clause in order until one succeeds
        for &clause_id_raw in clause_ids {
            let clause_id = ClauseId(clause_id_raw);
            eprintln!("🔍 DEBUG: Trying clause ID {}", clause_id_raw);
            
            // Get clause info from universal registry
            if let Some(clause_info) = self.universal_registry.get_clause(clause_id) {
                eprintln!("🔍 DEBUG: Found clause info: function_signature={:?}, guards={:?}, body={:?}", 
                         clause_info.function_signature, clause_info.guards, clause_info.body);
                
                // Evaluate all guards for this clause
                if self.evaluate_all_guards(&clause_info.guards, &args, context)? {
                    eprintln!("🔍 DEBUG: All guards passed for clause {}, executing body", clause_id_raw);
                    // All guards passed - execute this clause
                    return self.execute_clause_body_with_info(clause_info, &args, context, span);
                } else {
                    eprintln!("🔍 DEBUG: Guards failed for clause {}", clause_id_raw);
                }
            } else {
                eprintln!("🔍 DEBUG: Clause {} not found in universal registry", clause_id_raw);
            }
        }

        // No clause matched
        eprintln!("🔍 DEBUG: No clause matched, tried {} clauses", clause_ids.len());
        Err(EvaluationError::Runtime {
            message: format!(
                "No matching clause found for function call. Tried {} clauses.",
                clause_ids.len()
            ),
            span,
        })
    }

    /// Universal clause-based dispatch with pre-evaluated values (for unary/binary operations)
    fn dispatch_clauses_with_values(
        &self,
        clause_ids: &[outrun_typechecker::universal_dispatch::ClauseId],
        args: &[Value],
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        eprintln!("🔍 DEBUG: dispatch_clauses_with_values called with {} clause IDs and {} args", clause_ids.len(), args.len());
        
        // Try each clause in order until one succeeds
        for &clause_id in clause_ids {
            eprintln!("🔍 DEBUG: Trying clause ID {:?}", clause_id);
            
            // Get clause info from universal registry
            if let Some(clause_info) = self.universal_registry.get_clause(clause_id) {
                eprintln!("🔍 DEBUG: Found clause info: function_signature={:?}, guards={:?}, body={:?}", 
                         clause_info.function_signature, clause_info.guards, clause_info.body);
                
                // Evaluate all guards for this clause
                if self.evaluate_all_guards(&clause_info.guards, args, context)? {
                    eprintln!("🔍 DEBUG: All guards passed for clause {:?}, executing body", clause_id);
                    // All guards passed - execute this clause
                    return self.execute_clause_body_with_info(clause_info, args, context, span);
                } else {
                    eprintln!("🔍 DEBUG: Guards failed for clause {:?}", clause_id);
                }
            } else {
                eprintln!("🔍 DEBUG: Clause {:?} not found in universal registry", clause_id);
            }
        }

        // No clause matched
        eprintln!("🔍 DEBUG: No clause matched, tried {} clauses", clause_ids.len());
        Err(EvaluationError::Runtime {
            message: format!(
                "No matching clause found for unary operation. Tried {} clauses.",
                clause_ids.len()
            ),
            span,
        })
    }


    /// Universal guard evaluation system
    fn evaluate_all_guards(
        &self,
        guards: &[Guard],
        args: &[Value],
        context: &mut InterpreterContext,
    ) -> Result<bool, EvaluationError> {
        for guard in guards {
            if !self.evaluate_single_guard(guard, args, context)? {
                return Ok(false); // Short-circuit on first failed guard
            }
        }
        Ok(true) // All guards passed
    }

    /// Evaluate a single guard condition
    fn evaluate_single_guard(
        &self,
        guard: &Guard,
        _args: &[Value],
        _context: &mut InterpreterContext,
    ) -> Result<bool, EvaluationError> {
        match guard {
            Guard::TypeCompatible { target_type, implementing_type, .. } => {
                // For now, do simple type compatibility check
                // TODO: Implement proper runtime type checking based on values
                Ok(target_type == implementing_type)
            }
            Guard::ValueGuard { expression, .. } => {
                // TODO: Implement runtime value guard evaluation
                // For now, evaluate the expression and check if it's truthy
                let result = self.evaluate(expression, _context)?;
                Ok(result.is_truthy())
            }
            Guard::AlwaysTrue => Ok(true),
        }
    }

    /// Execute the body of a matched clause with full clause info for parameter binding
    fn execute_clause_body_with_info(
        &self,
        clause_info: &outrun_typechecker::universal_dispatch::ClauseInfo,
        args: &[Value],
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        use outrun_typechecker::universal_dispatch::FunctionBody;
        
        match &clause_info.body {
            FunctionBody::IntrinsicFunction(intrinsic_name) => {
                // Execute intrinsic function
                self.intrinsics
                    .execute_intrinsic(intrinsic_name, args, span)
                    .map_err(|e| EvaluationError::Intrinsic { source: e })
            }
            FunctionBody::UserFunction(block) => {
                // Execute user-defined function body with parameter binding
                // Create a new scope for the function execution
                context.push_scope();
                
                // Bind function parameters to argument values
                // We need to get parameter names from the function registry
                // For now, use a simple approach: bind based on parameter position and common names
                if let Err(e) = self.bind_function_parameters(clause_info, args, context) {
                    context.pop_scope();
                    return Err(e);
                }
                
                // Execute the function body block
                let result = self.evaluate_block(block, context);
                
                // Clean up the scope
                context.pop_scope();
                
                result
            }
            FunctionBody::StructConstructor { struct_name, field_mappings: _ } => {
                // TODO: Execute struct constructor
                // For now, return an error indicating this is not yet implemented
                Err(EvaluationError::UnsupportedExpression {
                    expr_type: format!("struct_constructor_{}", struct_name),
                    span,
                })
            }
            FunctionBody::ProtocolImplementation { implementation_name, body: _ } => {
                // TODO: Execute protocol implementation
                // For now, return an error indicating this is not yet implemented
                Err(EvaluationError::UnsupportedExpression {
                    expr_type: format!("protocol_implementation_{}", implementation_name),
                    span,
                })
            }
        }
    }

    /// Bind function parameters to argument values in the execution context
    fn bind_function_parameters(
        &self,
        clause_info: &outrun_typechecker::universal_dispatch::ClauseInfo,
        args: &[Value],
        context: &mut InterpreterContext,
    ) -> Result<(), EvaluationError> {
        // For protocol implementations like "UnaryMinus.minus", we need to get the parameter names
        // from the function registry. For now, use a simple heuristic based on common patterns.
        
        // Get function signature info
        let function_signature = &clause_info.function_signature;
        let function_name = &function_signature.function_name;
        eprintln!("🔍 DEBUG: Binding parameters for function: {} in module {:?}", 
                 function_name, function_signature.module_path);
        
        // Common parameter names for protocol functions
        let param_name = match function_name.as_str() {
            "minus" | "plus" | "not" => "value",
            "add" | "subtract" | "multiply" | "divide" => if !args.is_empty() { "left" } else { "value" },
            _ => "value", // Default fallback
        };
        
        // Bind the first argument to the parameter name
        if !args.is_empty() {
            eprintln!("🔍 DEBUG: Binding parameter '{}' to value {:?}", param_name, args[0].display());
            context.define_variable(param_name.to_string(), args[0].clone())
                .map_err(|e| EvaluationError::Context { source: e })?;
        } else {
            eprintln!("🔍 DEBUG: No arguments to bind for function {}", function_name);
        }
        
        Ok(())
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
            ExpressionKind::UnaryOp(_) => "unary_operation",
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
