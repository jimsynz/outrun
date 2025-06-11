// Expression parsing module
// Handles operator precedence, binary operations, and complex expression parsing

use pest::pratt_parser::{Assoc, Op, PrattParser};

use crate::ast::*;
use crate::error::*;
use crate::parser::{OutrunParser, Rule};

impl OutrunParser {
    /// Binary operator precedence parser
    /// Precedence levels from lowest to highest (following Ruby/Outrun precedence)
    pub(crate) fn pratt_parser() -> PrattParser<Rule> {
        PrattParser::new()
            // Level 1: Pipe operators (lowest precedence)
            .op(Op::infix(Rule::op_pipe, Assoc::Left) | Op::infix(Rule::op_pipe_maybe, Assoc::Left))
            // Level 2: Logical OR
            .op(Op::infix(Rule::op_logical_or, Assoc::Left))
            // Level 3: Logical AND
            .op(Op::infix(Rule::op_logical_and, Assoc::Left))
            // Level 4: Equality
            .op(Op::infix(Rule::op_equal, Assoc::Left) | Op::infix(Rule::op_not_equal, Assoc::Left))
            // Level 5: Comparison
            .op(Op::infix(Rule::op_less, Assoc::Left)
                | Op::infix(Rule::op_less_equal, Assoc::Left)
                | Op::infix(Rule::op_greater, Assoc::Left)
                | Op::infix(Rule::op_greater_equal, Assoc::Left))
            // Level 6: Bitwise OR
            .op(Op::infix(Rule::op_bitwise_or, Assoc::Left))
            // Level 7: Bitwise XOR
            .op(Op::infix(Rule::op_bitwise_xor, Assoc::Left))
            // Level 8: Bitwise AND
            .op(Op::infix(Rule::op_bitwise_and, Assoc::Left))
            // Level 9: Shift
            .op(Op::infix(Rule::op_shift_left, Assoc::Left)
                | Op::infix(Rule::op_shift_right, Assoc::Left))
            // Level 10: Additive
            .op(Op::infix(Rule::op_add, Assoc::Left) | Op::infix(Rule::op_subtract, Assoc::Left))
            // Level 11: Multiplicative
            .op(Op::infix(Rule::op_multiply, Assoc::Left)
                | Op::infix(Rule::op_divide, Assoc::Left)
                | Op::infix(Rule::op_modulo, Assoc::Left))
            // Level 12: Exponentiation (highest precedence, right associative)
            .op(Op::infix(Rule::op_exponent, Assoc::Right))
    }

    /// Parse expression using precedence climbing
    pub(crate) fn parse_expression_with_precedence(
        pairs: pest::iterators::Pairs<Rule>,
    ) -> ParseResult<Expression> {
        let parser = Self::pratt_parser();

        parser
            .map_primary(|pair| {
                // Postfix expression parser (handles field access, function calls)
                Self::parse_postfix_expr(pair)
            })
            .map_infix(
                |left: ParseResult<Expression>,
                 op: pest::iterators::Pair<Rule>,
                 right: ParseResult<Expression>| {
                    // Binary operator parser
                    let left = left?;
                    let right = right?;

                    let operator = match op.as_rule() {
                        // Pipe operators
                        Rule::op_pipe => BinaryOperator::Pipe,
                        Rule::op_pipe_maybe => BinaryOperator::PipeMaybe,
                        // Logical operators
                        Rule::op_logical_and => BinaryOperator::LogicalAnd,
                        Rule::op_logical_or => BinaryOperator::LogicalOr,
                        // Equality operators
                        Rule::op_equal => BinaryOperator::Equal,
                        Rule::op_not_equal => BinaryOperator::NotEqual,
                        // Comparison operators
                        Rule::op_less => BinaryOperator::Less,
                        Rule::op_less_equal => BinaryOperator::LessEqual,
                        Rule::op_greater => BinaryOperator::Greater,
                        Rule::op_greater_equal => BinaryOperator::GreaterEqual,
                        // Bitwise operators
                        Rule::op_bitwise_and => BinaryOperator::BitwiseAnd,
                        Rule::op_bitwise_or => BinaryOperator::BitwiseOr,
                        Rule::op_bitwise_xor => BinaryOperator::BitwiseXor,
                        // Shift operators
                        Rule::op_shift_left => BinaryOperator::ShiftLeft,
                        Rule::op_shift_right => BinaryOperator::ShiftRight,
                        // Arithmetic operators
                        Rule::op_add => BinaryOperator::Add,
                        Rule::op_subtract => BinaryOperator::Subtract,
                        Rule::op_multiply => BinaryOperator::Multiply,
                        Rule::op_divide => BinaryOperator::Divide,
                        Rule::op_modulo => BinaryOperator::Modulo,
                        Rule::op_exponent => BinaryOperator::Exponent,
                        _ => unreachable!("Unexpected binary operator: {:?}", op.as_rule()),
                    };

                    let left_start = left.span.start;
                    let right_end = right.span.end;
                    let span = Self::span_from_range(left_start, right_end);

                    Ok(Expression {
                        kind: ExpressionKind::BinaryOp(BinaryOperation {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                            span: span.clone(),
                        }),
                        span,
                    })
                },
            )
            .parse(pairs)
    }

    /// Parse an expression from a Pest pair (full precedence support)
    pub(crate) fn parse_expression_from_pair(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Expression> {
        let span = Self::span_from_pair(&pair);

        match pair.as_rule() {
            Rule::expression => {
                // Use PrecClimber for binary operator precedence
                let pairs = pair.into_inner();
                Self::parse_expression_with_precedence(pairs)
            }
            Rule::interpolation_expr => {
                // Non-atomic expression inside string interpolation
                let inner = pair.into_inner().next().unwrap();
                Self::parse_expression_from_pair(inner)
            }
            _ => {
                // For direct rule parsing (used in collections)
                let kind = match pair.as_rule() {
                    Rule::boolean => {
                        let boolean = Self::parse_boolean(pair)?;
                        ExpressionKind::Boolean(boolean)
                    }
                    Rule::integer => {
                        let integer = Self::parse_integer(pair)?;
                        ExpressionKind::Integer(integer)
                    }
                    Rule::float => {
                        let float = Self::parse_float(pair)?;
                        ExpressionKind::Float(float)
                    }
                    Rule::string => {
                        let string = Self::parse_string(pair)?;
                        ExpressionKind::String(string)
                    }
                    Rule::atom => {
                        let atom = Self::parse_atom(pair)?;
                        ExpressionKind::Atom(atom)
                    }
                    Rule::identifier => {
                        let identifier = Self::parse_identifier(pair)?;
                        ExpressionKind::Identifier(identifier)
                    }
                    Rule::type_identifier => {
                        let type_identifier = Self::parse_type_identifier(pair)?;
                        ExpressionKind::TypeIdentifier(type_identifier)
                    }
                    Rule::list => {
                        let list = Self::parse_list(pair)?;
                        ExpressionKind::List(list)
                    }
                    Rule::map => {
                        let map = Self::parse_map(pair)?;
                        ExpressionKind::Map(map)
                    }
                    Rule::tuple => {
                        let tuple = Self::parse_tuple(pair)?;
                        ExpressionKind::Tuple(tuple)
                    }
                    Rule::postfix_expr => {
                        return Self::parse_postfix_expr(pair);
                    }
                    Rule::primary_expr => {
                        return Self::parse_primary_expr(pair);
                    }
                    Rule::if_expression => {
                        let if_expr = Self::parse_if_expression(pair)?;
                        return Ok(Expression {
                            kind: ExpressionKind::IfExpression(if_expr),
                            span,
                        });
                    }
                    Rule::case_expression => {
                        let case_expr = Self::parse_case_expression(pair)?;
                        return Ok(Expression {
                            kind: ExpressionKind::CaseExpression(case_expr),
                            span,
                        });
                    }
                    _ => {
                        return Err(ParseError::unexpected_token(
                            "".to_string(),
                            miette::SourceSpan::from(span.start..span.end),
                            format!("Unexpected rule in expression: {:?}", pair.as_rule()),
                        ))
                    }
                };

                Ok(Expression { kind, span })
            }
        }
    }

    /// Parse postfix expressions (field access, etc.)
    pub(crate) fn parse_postfix_expr(pair: pest::iterators::Pair<Rule>) -> ParseResult<Expression> {
        let mut pairs = pair.into_inner();

        // Start with the primary expression
        let mut expr = Self::parse_primary_expr(pairs.next().unwrap())?;

        // Apply postfix operations
        for postfix_pair in pairs {
            if postfix_pair.as_rule() == Rule::postfix_op {
                let op_inner = postfix_pair.into_inner().next().unwrap();
                match op_inner.as_rule() {
                    Rule::field_access => {
                        let field_pair = op_inner.into_inner().next().unwrap();
                        let field = Self::parse_identifier(field_pair)?;

                        let start = expr.span.start;
                        let end = field.span.end;
                        let span = Self::span_from_range(start, end);

                        expr = Expression {
                            kind: ExpressionKind::FieldAccess(FieldAccess {
                                object: Box::new(expr),
                                field,
                                span: span.clone(),
                            }),
                            span,
                        };
                    }
                    Rule::function_call_postfix => {
                        let start = expr.span.start;
                        let end = op_inner.as_span().end();
                        let span = Self::span_from_range(start, end);

                        // Parse arguments if present
                        let mut arguments = Vec::new();
                        if let Some(args_pair) = op_inner.into_inner().next() {
                            if args_pair.as_rule() == Rule::argument_list {
                                for arg_pair in args_pair.into_inner() {
                                    if arg_pair.as_rule() == Rule::argument {
                                        let arg = Self::parse_argument(arg_pair)?;
                                        arguments.push(arg);
                                    }
                                }
                            }
                        }

                        expr = Expression {
                            kind: ExpressionKind::FunctionCall(FunctionCall {
                                path: FunctionPath::Expression {
                                    expression: Box::new(expr),
                                },
                                arguments,
                                span: span.clone(),
                            }),
                            span,
                        };
                    }
                    _ => {} // Ignore unknown postfix operations
                }
            }
        }

        Ok(expr)
    }

    /// Parse primary expressions (highest precedence)
    pub(crate) fn parse_primary_expr(pair: pest::iterators::Pair<Rule>) -> ParseResult<Expression> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();
        let first_inner = inner_pairs.next().unwrap();

        // Check if this is a unary operation
        match first_inner.as_rule() {
            Rule::op_unary_plus
            | Rule::op_unary_minus
            | Rule::op_logical_not
            | Rule::op_bitwise_not => {
                let operator = match first_inner.as_rule() {
                    Rule::op_unary_plus => UnaryOperator::Plus,
                    Rule::op_unary_minus => UnaryOperator::Minus,
                    Rule::op_logical_not => UnaryOperator::LogicalNot,
                    Rule::op_bitwise_not => UnaryOperator::BitwiseNot,
                    _ => unreachable!(),
                };

                let operand_pair = inner_pairs.next().unwrap();
                let operand = Self::parse_primary_expr(operand_pair)?;

                Ok(Expression {
                    kind: ExpressionKind::UnaryOp(UnaryOperation {
                        operator,
                        operand: Box::new(operand),
                        span: span.clone(),
                    }),
                    span,
                })
            }
            _ => {
                // Not a unary operation, parse as regular primary expression
                match first_inner.as_rule() {
                    Rule::expression => {
                        // Parenthesized expression
                        let expr = Self::parse_expression_from_pair(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Parenthesized(Box::new(expr)),
                            span,
                        })
                    }
                    Rule::boolean => {
                        let boolean = Self::parse_boolean(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Boolean(boolean),
                            span,
                        })
                    }
                    Rule::integer => {
                        let integer = Self::parse_integer(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Integer(integer),
                            span,
                        })
                    }
                    Rule::float => {
                        let float = Self::parse_float(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Float(float),
                            span,
                        })
                    }
                    Rule::string => {
                        let string = Self::parse_string(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::String(string),
                            span,
                        })
                    }
                    Rule::atom => {
                        let atom = Self::parse_atom(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Atom(atom),
                            span,
                        })
                    }
                    Rule::qualified_identifier => {
                        let qualified_id = Self::parse_qualified_identifier(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::QualifiedIdentifier(qualified_id),
                            span,
                        })
                    }
                    Rule::identifier => {
                        let identifier = Self::parse_identifier(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Identifier(identifier),
                            span,
                        })
                    }
                    Rule::type_identifier => {
                        let type_identifier = Self::parse_type_identifier(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::TypeIdentifier(type_identifier),
                            span,
                        })
                    }
                    Rule::list => {
                        let list = Self::parse_list(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::List(list),
                            span,
                        })
                    }
                    Rule::map => {
                        let map = Self::parse_map(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Map(map),
                            span,
                        })
                    }
                    Rule::tuple => {
                        let tuple = Self::parse_tuple(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Tuple(tuple),
                            span,
                        })
                    }
                    Rule::struct_literal => {
                        let struct_lit = Self::parse_struct_literal(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::Struct(struct_lit),
                            span,
                        })
                    }
                    Rule::function_call => {
                        let function_call = Self::parse_function_call(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::FunctionCall(function_call),
                            span,
                        })
                    }
                    Rule::if_expression => {
                        let if_expr = Self::parse_if_expression(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::IfExpression(if_expr),
                            span,
                        })
                    }
                    Rule::case_expression => {
                        let case_expr = Self::parse_case_expression(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::CaseExpression(case_expr),
                            span,
                        })
                    }
                    Rule::macro_injection => {
                        let injection = Self::parse_macro_injection(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::MacroInjection(injection),
                            span,
                        })
                    }
                    Rule::anonymous_function => {
                        let anon_fn = Self::parse_anonymous_function(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::AnonymousFunction(anon_fn),
                            span,
                        })
                    }
                    Rule::function_capture => {
                        let capture = Self::parse_function_capture(first_inner)?;
                        Ok(Expression {
                            kind: ExpressionKind::FunctionCapture(capture),
                            span,
                        })
                    }
                    _ => Err(ParseError::unexpected_token(
                        "".to_string(),
                        miette::SourceSpan::from(span.start..span.end),
                        format!(
                            "Unexpected rule in primary expression: {:?}",
                            first_inner.as_rule()
                        ),
                    )),
                }
            }
        }
    }
}
