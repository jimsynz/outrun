//! Expression type checking
//!
//! This module handles type checking for all expression types including:
//! - Literal expressions (integers, strings, booleans, etc.)
//! - Binary and unary operations with trait dispatch
//! - Function calls with parameter validation
//! - Collection literals (lists, maps, tuples)

// use crate::types::{TypeId, ConcreteType}; // TODO: Use when needed
use crate::checker::{TypeContext, TypedExpression, TypedExpressionKind};
use crate::error::{TypeError, TypeResult};
use outrun_parser::{Expression, ExpressionKind};

/// Expression type checker
pub struct ExpressionChecker;

impl ExpressionChecker {
    /// Type check an expression and return a typed expression
    pub fn check_expression(
        context: &mut TypeContext,
        expr: &Expression,
    ) -> TypeResult<TypedExpression> {
        match &expr.kind {
            ExpressionKind::Integer(int_lit) => {
                Self::check_integer_literal(context, int_lit, expr.span)
            }
            ExpressionKind::Float(float_lit) => {
                Self::check_float_literal(context, float_lit, expr.span)
            }
            ExpressionKind::String(str_lit) => {
                Self::check_string_literal(context, str_lit, expr.span)
            }
            ExpressionKind::Boolean(bool_lit) => {
                Self::check_boolean_literal(context, bool_lit, expr.span)
            }
            ExpressionKind::Atom(atom_lit) => {
                Self::check_atom_literal(context, atom_lit, expr.span)
            }
            ExpressionKind::Identifier(ident) => {
                Self::check_identifier(context, ident)
            }
            ExpressionKind::BinaryOp(bin_op) => {
                Self::check_binary_operation(context, &bin_op.left, &bin_op.operator, &bin_op.right)
            }
            ExpressionKind::FunctionCall(call) => {
                Self::check_function_call(context, call)
            }
            _ => {
                // TODO: Implement other expression types
                Err(TypeError::UnimplementedFeature {
                    feature: format!("Expression type checking for {:?}", expr.kind),
                    span: crate::error::span_to_source_span(expr.span),
                })
            }
        }
    }

    /// Type check integer literal
    fn check_integer_literal(
        context: &mut TypeContext,
        int_lit: &outrun_parser::IntegerLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Integer literals default to Outrun.Core.Integer64 type
        let type_id = context.interner.intern_type("Outrun.Core.Integer64");

        Ok(TypedExpression {
            kind: TypedExpressionKind::Integer(int_lit.value),
            type_id,
            span,
        })
    }

    /// Type check float literal
    fn check_float_literal(
        context: &mut TypeContext,
        float_lit: &outrun_parser::FloatLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Float literals default to Outrun.Core.Float64 type
        let type_id = context.interner.intern_type("Outrun.Core.Float64");

        Ok(TypedExpression {
            kind: TypedExpressionKind::Float(float_lit.value),
            type_id,
            span,
        })
    }

    /// Type check string literal
    fn check_string_literal(
        context: &mut TypeContext,
        str_lit: &outrun_parser::StringLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // String literals are Outrun.Core.String type
        let type_id = context.interner.intern_type("Outrun.Core.String");

        // TODO: Handle string interpolation type checking properly
        // For now, extract the basic text content
        let content = str_lit.parts.iter().fold(String::new(), |mut acc, part| {
            match part {
                outrun_parser::StringPart::Text { content, .. } => {
                    acc.push_str(content);
                }
                outrun_parser::StringPart::Interpolation { .. } => {
                    // TODO: Type check interpolated expressions
                    acc.push_str("${...}");
                }
            }
            acc
        });

        Ok(TypedExpression {
            kind: TypedExpressionKind::String(content),
            type_id,
            span,
        })
    }

    /// Type check atom literal
    fn check_atom_literal(
        context: &mut TypeContext,
        atom_lit: &outrun_parser::AtomLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Atom literals are Outrun.Core.Atom type
        let type_id = context.interner.intern_type("Outrun.Core.Atom");

        Ok(TypedExpression {
            kind: TypedExpressionKind::Atom(atom_lit.content.clone()),
            type_id,
            span,
        })
    }

    /// Type check boolean literal
    fn check_boolean_literal(
        context: &mut TypeContext,
        bool_lit: &outrun_parser::BooleanLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Boolean literals are Outrun.Core.Boolean type
        let type_id = context.interner.intern_type("Outrun.Core.Boolean");

        Ok(TypedExpression {
            kind: TypedExpressionKind::Boolean(bool_lit.value),
            type_id,
            span,
        })
    }

    /// Type check identifier (variable reference)
    fn check_identifier(
        context: &mut TypeContext,
        ident: &outrun_parser::Identifier,
    ) -> TypeResult<TypedExpression> {
        // Look up the identifier in the current scope
        if let Some(variable) = context.lookup_variable(&ident.name) {
            Ok(TypedExpression {
                kind: TypedExpressionKind::Identifier(ident.name.clone()),
                type_id: variable.type_id,
                span: ident.span,
            })
        } else {
            Err(TypeError::undefined_variable(
                ident.name.clone(),
                crate::error::span_to_source_span(ident.span),
            ))
        }
    }

    /// Type check binary operation
    fn check_binary_operation(
        context: &mut TypeContext,
        left: &Expression,
        operator: &outrun_parser::BinaryOperator,
        right: &Expression,
    ) -> TypeResult<TypedExpression> {
        // Type check operands first
        let typed_left = Self::check_expression(context, left)?;
        let typed_right = Self::check_expression(context, right)?;

        // For now, implement basic arithmetic and comparison operations
        // TODO: Replace with proper trait dispatch system
        let result_type = match operator {
            // Arithmetic operations - for now assume same type in/out
            outrun_parser::BinaryOperator::Add
            | outrun_parser::BinaryOperator::Subtract
            | outrun_parser::BinaryOperator::Multiply
            | outrun_parser::BinaryOperator::Divide
            | outrun_parser::BinaryOperator::Modulo => {
                // Check operands are the same numeric type
                if typed_left.type_id != typed_right.type_id {
                    return Err(TypeError::type_mismatch(
                        context.get_type_name(typed_left.type_id).unwrap_or("Unknown").to_string(),
                        context.get_type_name(typed_right.type_id).unwrap_or("Unknown").to_string(),
                        crate::error::span_to_source_span(right.span),
                    ));
                }
                
                // TODO: Validate types support the operation via traits
                typed_left.type_id
            }
            
            // Comparison operations - return Boolean
            outrun_parser::BinaryOperator::Equal
            | outrun_parser::BinaryOperator::NotEqual
            | outrun_parser::BinaryOperator::Less
            | outrun_parser::BinaryOperator::LessEqual
            | outrun_parser::BinaryOperator::Greater
            | outrun_parser::BinaryOperator::GreaterEqual => {
                // TODO: Validate types support comparison via traits
                context.interner.intern_type("Outrun.Core.Boolean")
            }
            
            // Logical operations - require Boolean operands, return Boolean
            outrun_parser::BinaryOperator::LogicalAnd
            | outrun_parser::BinaryOperator::LogicalOr => {
                let bool_type = context.interner.intern_type("Outrun.Core.Boolean");
                if typed_left.type_id != bool_type || typed_right.type_id != bool_type {
                    return Err(TypeError::type_mismatch(
                        "Outrun.Core.Boolean".to_string(),
                        context.get_type_name(
                            if typed_left.type_id != bool_type { typed_left.type_id } else { typed_right.type_id }
                        ).unwrap_or("Unknown").to_string(),
                        crate::error::span_to_source_span(
                            if typed_left.type_id != bool_type { left.span } else { right.span }
                        ),
                    ));
                }
                bool_type
            }

            _ => {
                return Err(TypeError::UnimplementedFeature {
                    feature: format!("Binary operator {:?}", operator),
                    span: crate::error::span_to_source_span(left.span),
                });
            }
        };

        Ok(TypedExpression {
            kind: TypedExpressionKind::BinaryOp {
                left: Box::new(typed_left),
                operator: operator.clone(),
                right: Box::new(typed_right),
            },
            type_id: result_type,
            span: outrun_parser::Span::new(left.span.start, right.span.end),
        })
    }

    /// Type check function call
    fn check_function_call(
        context: &mut TypeContext,
        call: &outrun_parser::FunctionCall,
    ) -> TypeResult<TypedExpression> {
        // Extract function name and span from path
        let (function_name, path_span) = match &call.path {
            outrun_parser::FunctionPath::Simple { name } => (name.name.clone(), name.span),
            outrun_parser::FunctionPath::Qualified { name, .. } => {
                return Err(TypeError::UnimplementedFeature {
                    feature: "Qualified function calls".to_string(),
                    span: crate::error::span_to_source_span(name.span),
                });
            }
            outrun_parser::FunctionPath::Expression { expression } => {
                return Err(TypeError::UnimplementedFeature {
                    feature: "Expression function calls".to_string(),
                    span: crate::error::span_to_source_span(expression.span),
                });
            }
        };

        // Look up the function in the current scope
        let function_return_type = if let Some(function) = context.lookup_function(&function_name) {
            function.return_type
        } else {
            return Err(TypeError::undefined_function(
                function_name,
                crate::error::span_to_source_span(path_span),
            ));
        };

        // Type check arguments
        let mut typed_args = Vec::new();
        
        for arg in &call.arguments {
            match arg {
                outrun_parser::Argument::Named { name, expression, format, span } => {
                    match format {
                        outrun_parser::ArgumentFormat::Explicit => {
                            let typed_expr = Self::check_expression(context, expression)?;
                            typed_args.push((name.name.clone(), typed_expr));
                        }
                        outrun_parser::ArgumentFormat::Shorthand => {
                            return Err(TypeError::UnimplementedFeature {
                                feature: "Shorthand function arguments".to_string(),
                                span: crate::error::span_to_source_span(*span),
                            });
                        }
                    }
                }
                outrun_parser::Argument::Spread { span, .. } => {
                    return Err(TypeError::UnimplementedFeature {
                        feature: "Spread function arguments".to_string(),
                        span: crate::error::span_to_source_span(*span),
                    });
                }
            }
        }

        // TODO: Validate argument types match function parameters
        // TODO: Ensure all required parameters are provided

        Ok(TypedExpression {
            kind: TypedExpressionKind::FunctionCall {
                name: function_name,
                args: typed_args,
            },
            type_id: function_return_type,
            span: call.span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::{
        AtomFormat, AtomLiteral, BooleanLiteral, FloatFormat, FloatLiteral, Identifier,
        IntegerFormat, IntegerLiteral, Span, StringFormat, StringLiteral, StringPart,
    };

    #[test]
    fn test_integer_literal_type_checking() {
        let mut context = TypeContext::new();

        let int_lit = IntegerLiteral {
            value: 42,
            format: IntegerFormat::Decimal,
            span: Span::new(0, 2),
        };

        let result =
            ExpressionChecker::check_integer_literal(&mut context, &int_lit, Span::new(0, 2));

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        match typed_expr.kind {
            TypedExpressionKind::Integer(value) => assert_eq!(value, 42),
            _ => panic!("Expected integer expression"),
        }

        // Should be Outrun.Core.Integer64 type
        let type_name = context.get_type_name(typed_expr.type_id);
        assert_eq!(type_name, Some("Outrun.Core.Integer64"));
    }

    #[test]
    fn test_boolean_literal_type_checking() {
        let mut context = TypeContext::new();

        let bool_lit = BooleanLiteral {
            value: true,
            span: Span::new(0, 4),
        };

        let result =
            ExpressionChecker::check_boolean_literal(&mut context, &bool_lit, Span::new(0, 4));

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        match typed_expr.kind {
            TypedExpressionKind::Boolean(value) => assert!(value),
            _ => panic!("Expected boolean expression"),
        }

        // Should be Outrun.Core.Boolean type
        let type_name = context.get_type_name(typed_expr.type_id);
        assert_eq!(type_name, Some("Outrun.Core.Boolean"));
    }

    #[test]
    fn test_string_literal_type_checking() {
        let mut context = TypeContext::new();

        let str_lit = StringLiteral {
            parts: vec![StringPart::Text {
                content: "hello".to_string(),
                raw_content: "hello".to_string(),
            }],
            format: StringFormat::Basic,
            span: Span::new(0, 7),
        };

        let result =
            ExpressionChecker::check_string_literal(&mut context, &str_lit, Span::new(0, 7));

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        match typed_expr.kind {
            TypedExpressionKind::String(_) => {} // Content doesn't matter for this test
            _ => panic!("Expected string expression"),
        }

        // Should be Outrun.Core.String type
        let type_name = context.get_type_name(typed_expr.type_id);
        assert_eq!(type_name, Some("Outrun.Core.String"));
    }

    #[test]
    fn test_float_literal_type_checking() {
        let mut context = TypeContext::new();

        let float_lit = FloatLiteral {
            value: 2.5, // Use a non-PI value to avoid clippy warning
            format: FloatFormat::Standard,
            span: Span::new(0, 4),
        };

        let result =
            ExpressionChecker::check_float_literal(&mut context, &float_lit, Span::new(0, 4));

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        match typed_expr.kind {
            TypedExpressionKind::Float(value) => assert!((value - 2.5).abs() < f64::EPSILON),
            _ => panic!("Expected float expression"),
        }

        // Should be Outrun.Core.Float64 type
        let type_name = context.get_type_name(typed_expr.type_id);
        assert_eq!(type_name, Some("Outrun.Core.Float64"));
    }

    #[test]
    fn test_atom_literal_type_checking() {
        let mut context = TypeContext::new();

        let atom_lit = AtomLiteral {
            name: "test_atom".to_string(),
            content: "test_atom".to_string(),
            raw_content: "test_atom".to_string(),
            format: AtomFormat::Simple,
            span: Span::new(0, 9),
        };

        let result =
            ExpressionChecker::check_atom_literal(&mut context, &atom_lit, Span::new(0, 9));

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        match typed_expr.kind {
            TypedExpressionKind::Atom(value) => assert_eq!(value, "test_atom"),
            _ => panic!("Expected atom expression"),
        }

        // Should be Outrun.Core.Atom type
        let type_name = context.get_type_name(typed_expr.type_id);
        assert_eq!(type_name, Some("Outrun.Core.Atom"));
    }

    #[test]
    fn test_identifier_type_checking_success() {
        let mut context = TypeContext::new();
        context.push_scope(false);
        
        // Register a variable in scope
        let var_type = context.interner.intern_type("Outrun.Core.Integer64");
        let variable = crate::checker::context::Variable {
            name: "x".to_string(),
            type_id: var_type,
            is_mutable: false,
            span: Span::new(0, 1),
        };
        context.register_variable(variable).unwrap();

        let ident = Identifier {
            name: "x".to_string(),
            span: Span::new(0, 1),
        };

        let result = ExpressionChecker::check_identifier(&mut context, &ident);

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        match typed_expr.kind {
            TypedExpressionKind::Identifier(name) => assert_eq!(name, "x"),
            _ => panic!("Expected identifier expression"),
        }

        // Should have the same type as the variable
        assert_eq!(typed_expr.type_id, var_type);
    }

    #[test]
    fn test_identifier_type_checking_undefined() {
        let mut context = TypeContext::new();

        let ident = Identifier {
            name: "undefined_var".to_string(),
            span: Span::new(0, 13),
        };

        let result = ExpressionChecker::check_identifier(&mut context, &ident);

        assert!(result.is_err());
        match result.unwrap_err() {
            TypeError::UndefinedVariable { name, .. } => {
                assert_eq!(name, "undefined_var");
            }
            _ => panic!("Expected UndefinedVariable error"),
        }
    }
}
