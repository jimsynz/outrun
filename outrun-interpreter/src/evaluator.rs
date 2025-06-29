//! Expression evaluator for TypedExpression from the typechecker
//!
//! This module provides the core expression evaluation engine that takes
//! TypedExpression from the typechecker and evaluates them to runtime Values
//! using the interpreter context.

use crate::context::{ContextError, InterpreterContext};
use crate::function_dispatch::{DispatchError, FunctionDispatcher};
use crate::intrinsics::IntrinsicError;
use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::checker::{
    DispatchMethod, TypedArgument, TypedCaseVariant, TypedExpression, TypedExpressionKind,
    TypedFunctionPath, TypedMapEntry, TypedStructField,
};
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur during expression evaluation
#[derive(Debug, Error)]
pub enum EvaluationError {
    #[error("Context error: {source}")]
    Context {
        #[from]
        source: ContextError,
    },

    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Variable '{name}' not found")]
    VariableNotFound { name: String, span: Span },

    #[error("Function '{name}' not found")]
    FunctionNotFound { name: String, span: Span },

    #[error("Field '{field}' not found on type {type_name}")]
    FieldNotFound {
        field: String,
        type_name: String,
        span: Span,
    },

    #[error("Invalid condition type: expected Boolean, found {found}")]
    InvalidConditionType { found: String, span: Span },

    #[error("Division by zero")]
    DivisionByZero { span: Span },

    #[error("Index out of bounds: index {index}, length {length}")]
    IndexOutOfBounds {
        index: usize,
        length: usize,
        span: Span,
    },

    #[error("Pattern matching failed: no clause matched")]
    PatternMatchingFailed { span: Span },

    #[error("Intrinsic error: {source}")]
    Intrinsic {
        #[from]
        source: IntrinsicError,
    },

    #[error("Function dispatch error: {source}")]
    Dispatch {
        #[from]
        source: DispatchError,
    },

    #[error("Internal error: {message}")]
    Internal { message: String, span: Span },
}

/// Expression evaluator that takes TypedExpression and produces Value
pub struct ExpressionEvaluator {
    /// Function dispatcher for all function calls
    function_dispatcher: FunctionDispatcher,
    /// Consolidated function dispatch context
    dispatch_context: outrun_typechecker::context::FunctionDispatchContext,
}

impl ExpressionEvaluator {
    /// Create a new expression evaluator with the given compiler environment
    pub fn new(
        compiler_environment: outrun_typechecker::compilation::compiler_environment::CompilerEnvironment,
    ) -> Self {
        let dispatch_context = outrun_typechecker::context::FunctionDispatchContext::new(Some(
            compiler_environment.clone(),
        ));

        Self {
            function_dispatcher: FunctionDispatcher::new(compiler_environment),
            dispatch_context,
        }
    }

    /// Create a new expression evaluator from a FunctionDispatchContext
    pub fn from_dispatch_context(
        dispatch_context: outrun_typechecker::context::FunctionDispatchContext,
    ) -> Self {
        let function_dispatcher =
            FunctionDispatcher::new(dispatch_context.compiler_environment.clone().unwrap());

        Self {
            function_dispatcher,
            dispatch_context,
        }
    }
    /// Create a mock StructuredType for development
    /// TODO: Replace with proper type integration
    fn mock_structured_type() -> outrun_typechecker::unification::StructuredType {
        use outrun_typechecker::compilation::compiler_environment::CompilerEnvironment;
        use outrun_typechecker::unification::StructuredType;

        // Create a temporary compiler environment to get a TypeNameId
        let env = CompilerEnvironment::new();
        let type_id = env.intern_type_name("MockType");
        StructuredType::Simple(type_id)
    }
    /// Evaluate a typed expression in the given context
    pub fn evaluate(
        &mut self,
        context: &mut InterpreterContext,
        expression: &TypedExpression,
    ) -> Result<Value, EvaluationError> {
        match &expression.kind {
            // Basic literals - direct conversion to Value
            TypedExpressionKind::Integer(value) => Ok(Value::integer(*value)),
            TypedExpressionKind::Float(value) => Ok(Value::float(*value)),
            TypedExpressionKind::String(value) => Ok(Value::string(value.clone())),
            TypedExpressionKind::Boolean(value) => Ok(Value::boolean(*value)),
            TypedExpressionKind::Atom(value) => {
                // For now, create a mock AtomId - in full implementation this would use interner
                // This is a temporary solution until we integrate with the interner properly
                Err(EvaluationError::Internal {
                    message: format!("Atom literals not yet supported: {value}"),
                    span: expression.span,
                })
            }

            // Variable lookup
            TypedExpressionKind::Identifier(name) => match context.lookup_variable(name) {
                Ok(value) => Ok(value.clone()),
                Err(_) => Err(EvaluationError::VariableNotFound {
                    name: name.clone(),
                    span: expression.span,
                }),
            },

            // Function calls - handle different dispatch strategies
            TypedExpressionKind::FunctionCall {
                function_path,
                arguments,
                dispatch_strategy,
            } => self.evaluate_function_call(
                context,
                function_path,
                arguments,
                dispatch_strategy,
                expression,
            ),

            // Field access
            TypedExpressionKind::FieldAccess { object, field, .. } => {
                let object_value = self.evaluate(context, object)?;
                self.evaluate_field_access(&object_value, field, expression.span)
            }

            // Collection literals
            TypedExpressionKind::List { elements, .. } => {
                self.evaluate_list_literal(context, elements)
            }
            TypedExpressionKind::Map { entries, .. } => self.evaluate_map_literal(context, entries),
            TypedExpressionKind::Tuple { elements, .. } => {
                self.evaluate_tuple_literal(context, elements)
            }
            TypedExpressionKind::StructLiteral {
                type_path, fields, ..
            } => self.evaluate_struct_literal(context, type_path, fields, expression.span),

            // Control flow expressions
            TypedExpressionKind::IfExpression {
                condition,
                then_branch,
                else_branch,
                ..
            } => self.evaluate_if_expression(
                context,
                condition,
                then_branch,
                else_branch.as_ref().map(|v| &**v),
                expression.span,
            ),
            TypedExpressionKind::CaseExpression { variant, .. } => {
                self.evaluate_case_expression(context, variant, expression.span)
            }

            // Anonymous functions - not yet implemented
            TypedExpressionKind::AnonymousFunction(_) => Err(EvaluationError::Internal {
                message: "Anonymous functions not yet implemented".to_string(),
                span: expression.span,
            }),

            // Macro injection - not yet implemented
            TypedExpressionKind::MacroInjection { .. } => Err(EvaluationError::Internal {
                message: "Macro injection not yet implemented".to_string(),
                span: expression.span,
            }),

            // Error recovery - propagate the error
            TypedExpressionKind::TypeError { error, .. } => Err(EvaluationError::Internal {
                message: format!("Type error during evaluation: {error:?}"),
                span: expression.span,
            }),

            // Placeholder - used during development
            TypedExpressionKind::Placeholder(message) => Err(EvaluationError::Internal {
                message: format!("Placeholder expression: {message}"),
                span: expression.span,
            }),
        }
    }

    /// Evaluate a function call based on dispatch strategy
    fn evaluate_function_call(
        &mut self,
        context: &mut InterpreterContext,
        function_path: &TypedFunctionPath,
        arguments: &[TypedArgument],
        dispatch_strategy: &DispatchMethod,
        expression: &TypedExpression,
    ) -> Result<Value, EvaluationError> {
        // Evaluate all arguments first
        let mut evaluated_args = HashMap::new();
        for arg in arguments {
            let arg_value = self.evaluate(context, &arg.expression)?;
            evaluated_args.insert(arg.name.clone(), arg_value);
        }

        // Use the function dispatcher to handle the call
        // We need to work around the borrow checker issue by temporarily taking ownership
        let mut temp_dispatcher = std::mem::replace(
            &mut self.function_dispatcher,
            FunctionDispatcher::new(
                self.dispatch_context
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .clone(),
            ),
        );

        let result = temp_dispatcher.dispatch_function_call(
            context,
            self,
            function_path,
            evaluated_args,
            dispatch_strategy,
            expression,
        );

        // Restore the dispatcher
        self.function_dispatcher = temp_dispatcher;

        Ok(result?)
    }

    /// Evaluate field access on a value
    fn evaluate_field_access(
        &self,
        object: &Value,
        field: &str,
        span: Span,
    ) -> Result<Value, EvaluationError> {
        match object {
            Value::Struct {
                fields, type_id, ..
            } => {
                // Create an AtomId for the field name using the shared interner
                let field_atom = self
                    .dispatch_context
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_atom_name(field);
                match fields.get(&field_atom) {
                    Some(value) => Ok(value.clone()),
                    None => Err(EvaluationError::FieldNotFound {
                        field: field.to_string(),
                        type_name: format!("{type_id:?}"), // Convert TypeId to string for error
                        span,
                    }),
                }
            }
            _ => Err(EvaluationError::TypeMismatch {
                expected: "Struct".to_string(),
                found: format!("{object:?}"),
                span,
            }),
        }
    }

    /// Evaluate a list literal
    fn evaluate_list_literal(
        &mut self,
        context: &mut InterpreterContext,
        elements: &[TypedExpression],
    ) -> Result<Value, EvaluationError> {
        let mut list = crate::list::List::Empty;
        for element in elements.iter().rev() {
            let value = self.evaluate(context, element)?;
            list = crate::list::List::cons(value, list);
        }
        // Create list with temporary type information
        Ok(Value::list_from_vec(
            list.to_vec(),
            Self::mock_structured_type(),
        ))
    }

    /// Evaluate a map literal
    fn evaluate_map_literal(
        &mut self,
        context: &mut InterpreterContext,
        entries: &[TypedMapEntry],
    ) -> Result<Value, EvaluationError> {
        let mut map = HashMap::new();

        for entry in entries {
            match entry {
                TypedMapEntry::Assignment { key, value, .. } => {
                    let key_value = self.evaluate(context, key)?;
                    let value_value = self.evaluate(context, value)?;
                    map.insert(key_value, value_value);
                }
                TypedMapEntry::Shorthand {
                    name: _, value: _, ..
                } => {
                    // Temporarily disable shorthand map entries
                    return Err(EvaluationError::Internal {
                        message: "Shorthand map entries not yet implemented".to_string(),
                        span: outrun_parser::Span::new(0, 0),
                    });
                }
                TypedMapEntry::Spread { identifier, .. } => {
                    // Look up the identifier and spread its entries
                    match context.lookup_variable(identifier) {
                        Ok(Value::Map {
                            entries: spread_map,
                            ..
                        }) => {
                            for (k, v) in spread_map.iter() {
                                map.insert(k.clone(), v.clone());
                            }
                        }
                        Ok(other) => {
                            return Err(EvaluationError::TypeMismatch {
                                expected: "Map".to_string(),
                                found: format!("{other:?}"),
                                span: outrun_parser::Span::new(0, 0), // TODO: Get proper span
                            });
                        }
                        Err(_) => {
                            return Err(EvaluationError::VariableNotFound {
                                name: identifier.clone(),
                                span: outrun_parser::Span::new(0, 0), // TODO: Get proper span
                            });
                        }
                    }
                }
            }
        }

        Ok(Value::map_from_hashmap(
            map,
            Self::mock_structured_type(),
            Self::mock_structured_type(),
        ))
    }

    /// Evaluate a tuple literal
    fn evaluate_tuple_literal(
        &mut self,
        context: &mut InterpreterContext,
        elements: &[TypedExpression],
    ) -> Result<Value, EvaluationError> {
        let mut tuple_elements = Vec::new();
        for element in elements {
            let value = self.evaluate(context, element)?;
            tuple_elements.push(value);
        }
        Ok(Value::tuple_from_vec(
            tuple_elements,
            Self::mock_structured_type(),
        ))
    }

    /// Evaluate a struct literal
    fn evaluate_struct_literal(
        &mut self,
        context: &mut InterpreterContext,
        type_path: &[String],
        fields: &[TypedStructField],
        span: Span,
    ) -> Result<Value, EvaluationError> {
        let type_id = type_path.join(".");
        let mut field_values = HashMap::new();

        for field in fields {
            match field {
                TypedStructField::Assignment {
                    name, expression, ..
                } => {
                    let value = self.evaluate(context, expression)?;
                    let field_atom = self
                        .dispatch_context
                        .compiler_environment
                        .as_ref()
                        .unwrap()
                        .intern_atom_name(name);
                    field_values.insert(field_atom, value);
                }
                TypedStructField::Shorthand { name, .. } => {
                    // Look up the variable with the same name
                    match context.lookup_variable(name) {
                        Ok(value) => {
                            let field_atom = self
                                .dispatch_context
                                .compiler_environment
                                .as_ref()
                                .unwrap()
                                .intern_atom_name(name);
                            field_values.insert(field_atom, value.clone());
                        }
                        Err(_) => {
                            return Err(EvaluationError::VariableNotFound {
                                name: name.clone(),
                                span,
                            });
                        }
                    }
                }
                TypedStructField::Spread { identifier, .. } => {
                    // Look up the identifier and spread its fields
                    match context.lookup_variable(identifier) {
                        Ok(Value::Struct {
                            fields: spread_fields,
                            ..
                        }) => {
                            for (k, v) in spread_fields.iter() {
                                field_values.insert(k.clone(), v.clone());
                            }
                        }
                        Ok(other) => {
                            return Err(EvaluationError::TypeMismatch {
                                expected: "Struct".to_string(),
                                found: format!("{other:?}"),
                                span,
                            });
                        }
                        Err(_) => {
                            return Err(EvaluationError::VariableNotFound {
                                name: identifier.clone(),
                                span,
                            });
                        }
                    }
                }
            }
        }

        // Create struct using the shared type interner
        let struct_type_id = self
            .dispatch_context
            .compiler_environment
            .as_ref()
            .unwrap()
            .intern_type_name(&type_id);
        Ok(Value::struct_instance(
            struct_type_id,
            field_values,
            Self::mock_structured_type(),
        ))
    }

    /// Evaluate an if expression
    fn evaluate_if_expression(
        &mut self,
        context: &mut InterpreterContext,
        condition: &TypedExpression,
        then_branch: &TypedExpression,
        else_branch: Option<&TypedExpression>,
        span: Span,
    ) -> Result<Value, EvaluationError> {
        let condition_value = self.evaluate(context, condition)?;

        match condition_value {
            Value::Boolean(true) => self.evaluate(context, then_branch),
            Value::Boolean(false) => {
                if let Some(else_expr) = else_branch {
                    self.evaluate(context, else_expr)
                } else {
                    // Return unit value - but Outrun doesn't have unit types
                    // This should be caught by the typechecker
                    Err(EvaluationError::Internal {
                        message: "If expression without else branch should not be allowed"
                            .to_string(),
                        span,
                    })
                }
            }
            _ => Err(EvaluationError::InvalidConditionType {
                found: format!("{condition_value:?}"),
                span,
            }),
        }
    }

    /// Evaluate a case expression
    fn evaluate_case_expression(
        &mut self,
        context: &mut InterpreterContext,
        variant: &TypedCaseVariant,
        span: Span,
    ) -> Result<Value, EvaluationError> {
        match variant {
            TypedCaseVariant::Concrete {
                expression,
                when_clauses,
            } => {
                let _target_value = self.evaluate(context, expression)?;

                for when_clause in when_clauses {
                    // For now, just evaluate the guard as a boolean condition
                    // TODO: Implement proper pattern matching
                    let guard_value = self.evaluate(context, &when_clause.guard)?;

                    match guard_value {
                        Value::Boolean(true) => {
                            return self.evaluate(context, &when_clause.result);
                        }
                        Value::Boolean(false) => {
                            // Continue to next clause
                            continue;
                        }
                        _ => {
                            return Err(EvaluationError::InvalidConditionType {
                                found: format!("{guard_value:?}"),
                                span: when_clause.span,
                            });
                        }
                    }
                }

                // No clause matched
                Err(EvaluationError::PatternMatchingFailed { span })
            }
            TypedCaseVariant::Trait { .. } => {
                // Trait case expressions not yet implemented
                Err(EvaluationError::Internal {
                    message: "Trait case expressions not yet implemented".to_string(),
                    span,
                })
            }
        }
    }
}

// TODO: Add proper tests for the evaluator once type integration is complete
