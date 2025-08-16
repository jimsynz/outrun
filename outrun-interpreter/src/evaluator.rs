//! Expression evaluator for parser AST with type information
//!
//! This module provides the core expression evaluation engine that takes
//! parser Expression nodes with attached ParsedTypeInfo and evaluates them
//! to runtime Values using the interpreter context.

use crate::context::{InterpreterContext, InterpreterError};
use crate::value::Value;
use miette::Diagnostic;
use outrun_parser::{Expression, ExpressionKind};
use outrun_typechecker::universal_dispatch::{ClauseId, Guard, UniversalDispatchRegistry};
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

    #[error("Missing type information: {expression} - {reason}")]
    MissingTypeInformation {
        expression: String,
        reason: String,
        span: outrun_parser::Span,
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
    /// Type registry for protocol implementation checking
    type_registry: Option<std::rc::Rc<outrun_typechecker::TypeRegistry>>,
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
            type_registry: None,
        }
    }

    /// Create a new expression evaluator with universal dispatch registry
    pub fn with_universal_dispatch(
        _dispatch_table: DispatchTable,
        _function_registry: std::rc::Rc<FunctionRegistry>,
        universal_registry: UniversalDispatchRegistry,
        type_registry: std::rc::Rc<outrun_typechecker::TypeRegistry>,
    ) -> Self {
        Self {
            intrinsics: crate::intrinsics::IntrinsicsHandler::new(),
            universal_registry,
            type_registry: Some(type_registry),
        }
    }

    /// Create a new expression evaluator with empty dispatch table (for tests)
    pub fn new() -> Self {
        Self {
            intrinsics: crate::intrinsics::IntrinsicsHandler::new(),
            universal_registry: UniversalDispatchRegistry::new(),
            type_registry: None,
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
                let content = literal
                    .parts
                    .iter()
                    .map(|part| match part {
                        outrun_parser::StringPart::Text { content, .. } => content.clone(),
                        outrun_parser::StringPart::Interpolation { .. } => {
                            unreachable!("String interpolation should have been desugared before interpretation")
                        }
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

            // Unary operations should be desugared to function calls by the typechecker
            ExpressionKind::UnaryOp(_) => {
                Err(EvaluationError::Runtime {
                    message: "UnaryOp should have been desugared to FunctionCall by typechecker".to_string(),
                    span,
                })
            }

            // Parenthesized expressions - just evaluate the inner expression
            ExpressionKind::Parenthesized(inner) => self.evaluate(inner, context),

            // Struct construction - create struct values
            ExpressionKind::Struct(struct_literal) => {
                self.evaluate_struct_literal(struct_literal, context, span)
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
    /// Returns the value of the last expression, or true for empty blocks
    fn evaluate_block(
        &self,
        block: &outrun_parser::Block,
        context: &mut InterpreterContext,
    ) -> Result<Value, EvaluationError> {
        let mut last_result = Value::boolean(true); // Default for empty blocks

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
            match element {
                outrun_parser::ListElement::Expression(expr) => {
                    let element_value = self.evaluate(expr, context)?;
                    // Prepend to the list (builds in reverse order)
                    result_list = crate::value::List::Cons {
                        head: element_value,
                        tail: std::rc::Rc::new(result_list),
                    };
                }
                outrun_parser::ListElement::Spread(identifier) => {
                    // Evaluate the spread identifier to get a list
                    let spread_value = context.get_variable(&identifier.name)?;

                    // Ensure it's a list
                    match spread_value {
                        Value::List { list, .. } => {
                            // Append all elements from the spread list to our result
                            // We need to traverse the spread list and prepend each element
                            let elements = self.collect_list_elements(&list);
                            // Add elements in reverse since we're building in reverse
                            for element in elements.into_iter().rev() {
                                result_list = crate::value::List::Cons {
                                    head: element,
                                    tail: std::rc::Rc::new(result_list),
                                };
                            }
                        }
                        _ => {
                            return Err(EvaluationError::TypeMismatch {
                                expected: "List".to_string(),
                                found: spread_value.type_name().to_string(),
                                span: list_literal.span,
                            });
                        }
                    }
                }
            }
        }

        Ok(Value::List {
            list: std::rc::Rc::new(result_list),
            element_type_info: None,
        })
    }

    /// Helper to collect all elements from a list into a vector
    fn collect_list_elements(&self, list: &crate::value::List) -> Vec<Value> {
        let mut elements = Vec::new();
        let mut current = list;

        loop {
            match current {
                crate::value::List::Empty => break,
                crate::value::List::Cons { head, tail } => {
                    elements.push(head.clone());
                    current = tail;
                }
            }
        }

        elements
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
            // Convert u64 IDs to ClauseId structs
            let clause_ids: Vec<ClauseId> = clause_ids.iter().map(|&id| ClauseId(id)).collect();
            // Universal dispatch for user functions and protocol implementations
            self.dispatch_clauses(&clause_ids, &function_call.arguments, context, span)
        } else {
            // No clause IDs - handle different function path types
            match &function_call.path {
                outrun_parser::FunctionPath::Qualified { module, name } => {
                    let function_name = format!("{}.{}", module.name, name.name);

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
                        // Try to resolve protocol call without clause IDs as a fallback
                        match self.try_resolve_protocol_call_fallback(
                            &function_name,
                            &function_call.arguments,
                            context,
                            span,
                        ) {
                            Ok(value) => Ok(value),
                            Err(_) => Err(EvaluationError::MissingTypeInformation {
                                expression: format!(
                                    "Non-intrinsic function call: {}",
                                    function_name
                                ),
                                reason: "universal_clause_ids not populated by typechecker"
                                    .to_string(),
                                span,
                            }),
                        }
                    }
                }
                outrun_parser::FunctionPath::Simple { name } => {
                    // Try to resolve simple function call as a protocol method
                    let function_name = &name.name;

                    // Try common protocol method patterns
                    let qualified_name = if function_name == "equal?" {
                        "Equality.equal?"
                    } else if function_name == "not" {
                        "LogicalNot.not"
                    } else {
                        // For other simple calls, return error
                        return Err(EvaluationError::MissingTypeInformation {
                            expression: format!("Simple function call: {}", function_name),
                            reason: "universal_clause_ids not populated by typechecker".to_string(),
                            span,
                        });
                    };

                    match self.try_resolve_protocol_call_fallback(
                        qualified_name,
                        &function_call.arguments,
                        context,
                        span,
                    ) {
                        Ok(value) => Ok(value),
                        Err(_) => Err(EvaluationError::MissingTypeInformation {
                            expression: format!("Simple function call: {}", function_name),
                            reason: "universal_clause_ids not populated by typechecker".to_string(),
                            span,
                        }),
                    }
                }
                outrun_parser::FunctionPath::Expression { .. } => {
                    Err(EvaluationError::MissingTypeInformation {
                        expression: "Expression function call".to_string(),
                        reason: "universal_clause_ids not populated by typechecker".to_string(),
                        span,
                    })
                }
            }
        }
    }


    /// Universal clause-based dispatch - the future of function dispatch
    fn dispatch_clauses(
        &self,
        clause_ids: &[ClauseId],
        arguments: &[outrun_parser::Argument],
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        // DEBUG: Add logging to understand clause dispatch issues

        // Evaluate arguments and preserve their names
        let mut named_args = Vec::new();
        let mut arg_values = Vec::new(); // For guard evaluation
        for arg in arguments {
            match arg {
                outrun_parser::Argument::Named {
                    name, expression, ..
                } => {
                    let arg_value = self.evaluate(expression, context)?;
                    named_args.push((name.name.clone(), arg_value.clone()));
                    arg_values.push(arg_value);
                }
                outrun_parser::Argument::Spread { expression, .. } => {
                    // Evaluate the spread expression to get a map or struct
                    let spread_value = self.evaluate(expression, context)?;

                    match spread_value {
                        Value::Map { entries, .. } => {
                            // Spread all key-value pairs from the map as named arguments
                            for (key, value) in entries.iter() {
                                // Map keys should be strings for named arguments
                                if let Value::String(key_str) = key {
                                    named_args.push((key_str.clone(), value.clone()));
                                    arg_values.push(value.clone());
                                } else {
                                    return Err(EvaluationError::TypeMismatch {
                                        expected: "String keys in spread map".to_string(),
                                        found: key.type_name().to_string(),
                                        span,
                                    });
                                }
                            }
                        }
                        Value::Struct { fields, .. } => {
                            // Spread all fields from the struct as named arguments
                            for (field_name, field_value) in fields.iter() {
                                named_args.push((field_name.clone(), field_value.clone()));
                                arg_values.push(field_value.clone());
                            }
                        }
                        _ => {
                            return Err(EvaluationError::TypeMismatch {
                                expected: "Map or Struct for spread arguments".to_string(),
                                found: spread_value.type_name().to_string(),
                                span,
                            });
                        }
                    }
                }
            };
        }

        // Universal clause dispatch - try each clause until one succeeds

        // Try each clause in order until one succeeds
        for &clause_id in clause_ids {
            // Get clause info from universal registry
            if let Some(clause_info) = self.universal_registry.get_clause(clause_id) {
                // Evaluate all guards for this clause
                if self.evaluate_all_guards(&clause_info.guards, &arg_values, context)? {
                    // All guards passed - execute this clause with named arguments
                    return self.execute_clause_body_with_named_args(
                        clause_info,
                        &named_args,
                        context,
                        span,
                    );
                }
            }
        }

        // No clause matched
        Err(EvaluationError::Runtime {
            message: format!(
                "No matching clause found for function call. Tried {} clauses.",
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
            Guard::TypeCompatible {
                target_type,
                implementing_type,
                ..
            } => {
                // Check if the implementing type satisfies the target protocol
                if let Some(type_registry) = &self.type_registry {
                    use outrun_typechecker::types::{ModuleName, Type};

                    let result = match (implementing_type, target_type) {
                        // Concrete to Concrete: check if they're the same type
                        (
                            Type::Concrete {
                                name: impl_name, ..
                            },
                            Type::Concrete {
                                name: target_name, ..
                            },
                        ) => impl_name == target_name,

                        // Concrete to Protocol: check if concrete type implements protocol
                        (
                            Type::Concrete {
                                name: impl_name, ..
                            },
                            Type::Protocol {
                                name: protocol_name,
                                ..
                            },
                        ) => {
                            let impl_module_name = ModuleName::implementation(
                                impl_name.as_str(),
                                protocol_name.as_str(),
                            );
                            type_registry
                                .get_module(impl_module_name.as_str())
                                .is_some()
                        }

                        // Protocol to Protocol: check if they're the same protocol
                        (
                            Type::Protocol {
                                name: impl_name, ..
                            },
                            Type::Protocol {
                                name: target_name, ..
                            },
                        ) => impl_name == target_name,

                        // Other combinations
                        _ => false,
                    };

                    Ok(result)
                } else {
                    // Fallback to simple equality check if no type registry available
                    let result = target_type == implementing_type;
                    Ok(result)
                }
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
    fn execute_clause_body_with_named_args(
        &self,
        clause_info: &outrun_typechecker::universal_dispatch::ClauseInfo,
        named_args: &[(String, Value)],
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        use outrun_typechecker::universal_dispatch::FunctionBody;

        match &clause_info.body {
            FunctionBody::IntrinsicFunction(intrinsic_name) => {
                // Execute intrinsic function with positional args (intrinsics don't use named params)
                let args: Vec<Value> = named_args.iter().map(|(_, value)| value.clone()).collect();
                self.intrinsics
                    .execute_intrinsic(intrinsic_name, &args, span)
                    .map_err(|e| EvaluationError::Intrinsic { source: e })
            }
            FunctionBody::UserFunction(block) => {
                // Execute user-defined function body with named parameter binding
                context.push_scope();

                // Bind named arguments to their parameter names
                if let Err(e) = self.bind_named_parameters(named_args, context) {
                    context.pop_scope();
                    return Err(e);
                }

                // Execute the function body block
                let result = self.evaluate_block(block, context);

                // Clean up the scope
                context.pop_scope();

                result
            }
            FunctionBody::StructConstructor {
                struct_name,
                field_mappings,
            } => {
                // Execute struct constructor by mapping arguments to struct fields
                use std::collections::HashMap;
                let mut fields = HashMap::new();

                // Map each field using the field mappings (named parameters only)
                for (field_name, arg_ref) in field_mappings {
                    let value = match arg_ref {
                        outrun_typechecker::universal_dispatch::ArgumentRef::Name(param_name) => {
                            // Get argument by parameter name (the only valid approach in Outrun)
                            if let Some((_, value)) =
                                named_args.iter().find(|(name, _)| name == param_name)
                            {
                                value.clone()
                            } else {
                                return Err(EvaluationError::UnsupportedExpression {
                                    expr_type: format!(
                                        "struct_constructor_missing_param_{}",
                                        param_name
                                    ),
                                    span,
                                });
                            }
                        }
                    };
                    fields.insert(field_name.clone(), value);
                }

                // Create and return the struct value
                Ok(Value::Struct {
                    type_name: struct_name.clone(),
                    fields,
                    type_info: None,
                })
            }
            FunctionBody::ProtocolImplementation {
                implementation_name: _,
                body,
            } => {
                // Execute protocol implementation body with named parameter binding
                context.push_scope();

                // Bind named arguments to their parameter names
                if let Err(e) = self.bind_named_parameters(named_args, context) {
                    context.pop_scope();
                    return Err(e);
                }

                // Execute the protocol implementation body block
                let result = self.evaluate_block(body, context);

                // Clean up the scope
                context.pop_scope();

                result
            }
        }
    }

    /// Bind function parameters to argument values in the execution context
    fn bind_named_parameters(
        &self,
        named_args: &[(String, Value)],
        context: &mut InterpreterContext,
    ) -> Result<(), EvaluationError> {
        // Bind each named argument to its parameter name
        for (param_name, arg_value) in named_args {
            context
                .define_variable(param_name.clone(), arg_value.clone())
                .map_err(|e| EvaluationError::Context { source: e })?;
        }

        Ok(())
    }


    /// Evaluate a struct literal to create a struct value
    fn evaluate_struct_literal(
        &self,
        struct_literal: &outrun_parser::StructLiteral,
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        // For now, create a simple struct representation using a map-like structure
        // TODO: This should integrate with the type system to create proper struct values

        let mut fields = std::collections::HashMap::new();

        for field in &struct_literal.fields {
            match field {
                outrun_parser::StructLiteralField::Assignment { name, value } => {
                    let field_value = self.evaluate(value, context)?;
                    fields.insert(name.name.clone(), field_value);
                }
                outrun_parser::StructLiteralField::Shorthand(name) => {
                    // Shorthand: { x } means { x: x }
                    let field_value = context.get_variable(&name.name).cloned().map_err(|_| {
                        EvaluationError::VariableNotFound {
                            name: name.name.clone(),
                            span,
                        }
                    })?;
                    fields.insert(name.name.clone(), field_value);
                }
                outrun_parser::StructLiteralField::Spread(identifier) => {
                    // Get the value to spread
                    let spread_value = context.get_variable(&identifier.name)?;

                    // It should be a struct or map
                    match spread_value {
                        Value::Struct {
                            fields: spread_fields,
                            ..
                        } => {
                            // Merge all fields from the spread struct
                            // Note: later fields override earlier ones in case of conflicts
                            for (field_name, field_value) in spread_fields.iter() {
                                fields
                                    .entry(field_name.clone())
                                    .or_insert_with(|| field_value.clone());
                            }
                        }
                        Value::Map { entries, .. } => {
                            // Spread from a map - keys must be strings
                            for (key, value) in entries.iter() {
                                if let Value::String(key_str) = key {
                                    fields
                                        .entry(key_str.clone())
                                        .or_insert_with(|| value.clone());
                                } else {
                                    return Err(EvaluationError::TypeMismatch {
                                        expected: "String keys in spread map".to_string(),
                                        found: key.type_name().to_string(),
                                        span,
                                    });
                                }
                            }
                        }
                        _ => {
                            return Err(EvaluationError::TypeMismatch {
                                expected: "Struct or Map for spread".to_string(),
                                found: spread_value.type_name().to_string(),
                                span,
                            });
                        }
                    }
                }
            }
        }

        // Create a struct value - for now use a simple representation
        // TODO: This should create proper typed struct values
        let type_name = struct_literal
            .type_path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        Ok(Value::Struct {
            type_name,
            fields,
            type_info: None, // TODO: Get type info from typechecker
        })
    }

    /// Get a human-readable name for an expression type (for error messages)
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

    /// Fallback method to resolve protocol calls without clause IDs
    /// This handles cases where desugared operators in protocol function bodies
    /// don't have clause IDs assigned during typechecking
    fn try_resolve_protocol_call_fallback(
        &self,
        function_name: &str,
        arguments: &[outrun_parser::Argument],
        context: &mut InterpreterContext,
        span: outrun_parser::Span,
    ) -> Result<Value, EvaluationError> {
        // Parse the function name to extract protocol and method
        let parts: Vec<&str> = function_name.split('.').collect();
        if parts.len() != 2 {
            return Err(EvaluationError::Runtime {
                message: format!("Invalid function name format: {}", function_name),
                span,
            });
        }

        let protocol_name = parts[0];
        let method_name = parts[1];

        // Evaluate arguments to determine types for dispatch
        let mut arg_values = Vec::new();
        for arg in arguments {
            match arg {
                outrun_parser::Argument::Named { expression, .. } => {
                    let arg_value = self.evaluate(expression, context)?;
                    arg_values.push(arg_value);
                }
                outrun_parser::Argument::Spread { .. } => {
                    return Err(EvaluationError::UnsupportedExpression {
                        expr_type: "spread_argument".to_string(),
                        span,
                    });
                }
            }
        }

        // For LogicalNot.not with Boolean argument, call the intrinsic directly
        if protocol_name == "LogicalNot" && method_name == "not" && arg_values.len() == 1 {
            if let Value::Boolean(_) = arg_values[0] {
                return self
                    .intrinsics
                    .execute_intrinsic("Outrun.Intrinsic.bool_not", &arg_values, span)
                    .map_err(|e| EvaluationError::Intrinsic { source: e });
            }
        }

        // For Equality.equal? with any arguments, call the generic equal intrinsic
        if protocol_name == "Equality" && method_name == "equal?" && arg_values.len() == 2 {
            return self
                .intrinsics
                .execute_intrinsic("Outrun.Intrinsic.equal", &arg_values, span)
                .map_err(|e| EvaluationError::Intrinsic { source: e });
        }

        // Add more protocol-specific fallbacks here as needed

        Err(EvaluationError::Runtime {
            message: format!(
                "Cannot resolve protocol call without clause IDs: {}",
                function_name
            ),
            span,
        })
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
