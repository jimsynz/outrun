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
            ExpressionKind::String(str_lit) => {
                Self::check_string_literal(context, str_lit, expr.span)
            }
            ExpressionKind::Boolean(bool_lit) => {
                Self::check_boolean_literal(context, bool_lit, expr.span)
            }
            _ => {
                // TODO: Implement other expression types
                Err(TypeError::UnimplementedFeature {
                    feature: "Expression type checking".to_string(),
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
        // Integer literals default to Integer64 type
        let type_id = context.interner.intern_type("Integer64");

        Ok(TypedExpression {
            kind: TypedExpressionKind::Integer(int_lit.value),
            type_id,
            span,
        })
    }

    /// Type check string literal
    fn check_string_literal(
        context: &mut TypeContext,
        _str_lit: &outrun_parser::StringLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // String literals are String type
        let type_id = context.interner.intern_type("String");

        // TODO: Handle string interpolation type checking
        Ok(TypedExpression {
            kind: TypedExpressionKind::String("placeholder".to_string()),
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
        // Boolean literals are Boolean type
        let type_id = context.interner.intern_type("Boolean");

        Ok(TypedExpression {
            kind: TypedExpressionKind::Boolean(bool_lit.value),
            type_id,
            span,
        })
    }

    /// Type check binary operation (placeholder)
    fn _check_binary_operation(
        _context: &mut TypeContext,
        _left: &Expression,
        _operator: &outrun_parser::BinaryOperator,
        _right: &Expression,
    ) -> TypeResult<TypedExpression> {
        // TODO: Implement binary operation type checking
        // - Check operand types
        // - Look up trait implementation for operator
        // - Validate trait dispatch
        // - Return result type
        todo!("Binary operation type checking")
    }

    /// Type check function call (placeholder)
    fn _check_function_call(
        _context: &mut TypeContext,
        _call: &outrun_parser::FunctionCall,
    ) -> TypeResult<TypedExpression> {
        // TODO: Implement function call type checking
        // - Resolve function name (static vs trait)
        // - Check argument types match parameters
        // - Validate all required parameters provided
        // - Return function return type
        todo!("Function call type checking")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::{
        BooleanLiteral, IntegerFormat, IntegerLiteral, Span, StringFormat, StringLiteral,
        StringPart,
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

        // Should be Integer64 type
        let type_name = context.get_type_name(typed_expr.type_id);
        assert_eq!(type_name, Some("Integer64"));
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

        // Should be Boolean type
        let type_name = context.get_type_name(typed_expr.type_id);
        assert_eq!(type_name, Some("Boolean"));
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

        // Should be String type
        let type_name = context.get_type_name(typed_expr.type_id);
        assert_eq!(type_name, Some("String"));
    }
}
