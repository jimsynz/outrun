//! Expression desugaring for Outrun
//!
//! This module transforms syntactic sugar operators into explicit trait function calls.
//! All binary operators like `+`, `-` become calls to trait functions like `BinaryAddition.add()`.

use outrun_parser::{
    Argument, ArgumentFormat, BinaryOperation, BinaryOperator, Block, CaseExpression, CaseResult,
    CaseWhenClause, ConcreteCaseExpression, Expression, ExpressionKind, FunctionCall,
    FunctionDefinition, FunctionPath, Identifier, IfExpression, ImplBlock, Item, ItemKind, Program,
    Statement, StatementKind, StaticFunctionDefinition, TraitCaseClause, TraitCaseExpression,
    TraitDefinition, TraitFunction, TypeIdentifier, UnaryOperation, UnaryOperator,
};

/// Transformer that desugars operator expressions into trait function calls
pub struct DesugaringVisitor;

impl DesugaringVisitor {
    /// Desugar a program by transforming all operator expressions
    pub fn desugar_program(program: Program) -> Program {
        Program {
            items: program.items.into_iter().map(Self::desugar_item).collect(),
            debug_info: program.debug_info,
            span: program.span,
        }
    }

    /// Desugar an item (trait, struct, function, etc.)
    fn desugar_item(item: Item) -> Item {
        Item {
            kind: Self::desugar_item_kind(item.kind),
            span: item.span,
        }
    }

    /// Desugar item kinds
    fn desugar_item_kind(kind: ItemKind) -> ItemKind {
        match kind {
            ItemKind::TraitDefinition(trait_def) => {
                ItemKind::TraitDefinition(Self::desugar_trait_definition(trait_def))
            }
            ItemKind::FunctionDefinition(func_def) => {
                ItemKind::FunctionDefinition(Self::desugar_function_definition(func_def))
            }
            ItemKind::ImplBlock(impl_block) => ItemKind::ImplBlock(ImplBlock {
                generic_params: impl_block.generic_params,
                trait_spec: impl_block.trait_spec,
                type_spec: impl_block.type_spec,
                constraints: impl_block.constraints,
                methods: impl_block
                    .methods
                    .into_iter()
                    .map(Self::desugar_function_definition)
                    .collect(),
                span: impl_block.span,
            }),
            // Other item kinds don't contain expressions that need desugaring
            other => other,
        }
    }

    /// Desugar trait definition to handle function bodies
    fn desugar_trait_definition(trait_def: TraitDefinition) -> TraitDefinition {
        TraitDefinition {
            name: trait_def.name,
            generic_params: trait_def.generic_params,
            constraints: trait_def.constraints,
            functions: trait_def
                .functions
                .into_iter()
                .map(Self::desugar_trait_function)
                .collect(),
            span: trait_def.span,
            attributes: trait_def.attributes,
        }
    }

    /// Desugar trait function (handles function bodies in trait definitions)
    fn desugar_trait_function(trait_func: TraitFunction) -> TraitFunction {
        match trait_func {
            TraitFunction::Definition(func_def) => {
                TraitFunction::Definition(Self::desugar_function_definition(func_def))
            }
            TraitFunction::StaticDefinition(static_func_def) => TraitFunction::StaticDefinition(
                Self::desugar_static_function_definition(static_func_def),
            ),
            // Signatures don't have bodies, no desugaring needed
            TraitFunction::Signature(sig) => TraitFunction::Signature(sig),
        }
    }

    /// Desugar function definition
    fn desugar_function_definition(func_def: FunctionDefinition) -> FunctionDefinition {
        FunctionDefinition {
            attributes: func_def.attributes,
            visibility: func_def.visibility,
            name: func_def.name,
            parameters: func_def.parameters,
            return_type: func_def.return_type,
            guard: func_def.guard,
            body: Self::desugar_block(func_def.body),
            span: func_def.span,
        }
    }

    /// Desugar static function definition
    fn desugar_static_function_definition(
        static_func_def: StaticFunctionDefinition,
    ) -> StaticFunctionDefinition {
        StaticFunctionDefinition {
            attributes: static_func_def.attributes,
            name: static_func_def.name,
            parameters: static_func_def.parameters,
            return_type: static_func_def.return_type,
            body: Self::desugar_block(static_func_def.body),
            span: static_func_def.span,
        }
    }

    /// Desugar a block of statements
    fn desugar_block(block: Block) -> Block {
        Block {
            statements: block
                .statements
                .into_iter()
                .map(Self::desugar_statement)
                .collect(),
            span: block.span,
        }
    }

    /// Desugar a statement
    fn desugar_statement(stmt: Statement) -> Statement {
        match stmt.kind {
            StatementKind::LetBinding(let_binding) => Statement {
                kind: StatementKind::LetBinding(Box::new(outrun_parser::LetBinding {
                    pattern: let_binding.pattern,
                    type_annotation: let_binding.type_annotation,
                    expression: Self::desugar_expression(let_binding.expression),
                    span: let_binding.span,
                })),
                span: stmt.span,
            },
            StatementKind::Expression(expr) => Statement {
                kind: StatementKind::Expression(Box::new(Self::desugar_expression(*expr))),
                span: stmt.span,
            },
        }
    }

    /// Desugar an expression recursively
    fn desugar_expression(expr: Expression) -> Expression {
        match expr.kind {
            ExpressionKind::BinaryOp(op) => Self::desugar_binary_operation(op),
            ExpressionKind::UnaryOp(op) => Self::desugar_unary_operation(op),
            ExpressionKind::FunctionCall(call) => Expression {
                kind: ExpressionKind::FunctionCall(FunctionCall {
                    path: call.path,
                    arguments: call
                        .arguments
                        .into_iter()
                        .map(|arg| match arg {
                            Argument::Named {
                                name,
                                expression,
                                format,
                                span,
                            } => Argument::Named {
                                name,
                                expression: Self::desugar_expression(expression),
                                format,
                                span,
                            },
                            Argument::Spread {
                                expression,
                                kind,
                                span,
                            } => Argument::Spread {
                                expression: Self::desugar_expression(expression),
                                kind,
                                span,
                            },
                        })
                        .collect(),
                    span: call.span,
                }),
                span: expr.span,
            },
            ExpressionKind::IfExpression(if_expr) => Expression {
                kind: ExpressionKind::IfExpression(IfExpression {
                    condition: Box::new(Self::desugar_expression(*if_expr.condition)),
                    then_block: Self::desugar_block(if_expr.then_block),
                    else_block: if_expr.else_block.map(Self::desugar_block),
                    span: if_expr.span,
                }),
                span: expr.span,
            },
            ExpressionKind::CaseExpression(case_expr) => Expression {
                kind: ExpressionKind::CaseExpression(match case_expr {
                    CaseExpression::Concrete(concrete) => {
                        CaseExpression::Concrete(ConcreteCaseExpression {
                            expression: Box::new(Self::desugar_expression(*concrete.expression)),
                            when_clauses: concrete
                                .when_clauses
                                .into_iter()
                                .map(|clause| CaseWhenClause {
                                    guard: Self::desugar_expression(clause.guard),
                                    result: Self::desugar_case_result(clause.result),
                                    span: clause.span,
                                })
                                .collect(),
                            span: concrete.span,
                        })
                    }
                    CaseExpression::Trait(trait_case) => {
                        CaseExpression::Trait(TraitCaseExpression {
                            expression: Box::new(Self::desugar_expression(*trait_case.expression)),
                            trait_name: trait_case.trait_name,
                            type_clauses: trait_case
                                .type_clauses
                                .into_iter()
                                .map(|clause| TraitCaseClause {
                                    type_name: clause.type_name,
                                    pattern: clause.pattern,
                                    guard: clause.guard.map(Self::desugar_expression),
                                    result: Self::desugar_case_result(clause.result),
                                    span: clause.span,
                                })
                                .collect(),
                            span: trait_case.span,
                        })
                    }
                }),
                span: expr.span,
            },
            // For simplicity, handle only the most common expression types
            // Lists, tuples, maps, etc. can be added later as needed
            _ => expr, // Pass through other expressions unchanged for now
        }
    }

    /// Desugar case result (can be block or expression)
    fn desugar_case_result(result: CaseResult) -> CaseResult {
        match result {
            CaseResult::Block(block) => CaseResult::Block(Self::desugar_block(block)),
            CaseResult::Expression(expr) => {
                CaseResult::Expression(Box::new(Self::desugar_expression(*expr)))
            }
        }
    }

    /// Transform binary operation to trait function call
    fn desugar_binary_operation(op: BinaryOperation) -> Expression {
        let trait_name = match op.operator {
            BinaryOperator::Add => "BinaryAddition",
            BinaryOperator::Subtract => "BinarySubtraction",
            BinaryOperator::Multiply => "BinaryMultiplication",
            BinaryOperator::Divide => "BinaryDivision",
            BinaryOperator::Modulo => "BinaryModulo",
            BinaryOperator::Exponent => "BinaryExponentiation",
            BinaryOperator::Equal => "Equality",
            BinaryOperator::NotEqual => "Equality",
            BinaryOperator::Less => "Comparison",
            BinaryOperator::LessEqual => "Comparison",
            BinaryOperator::Greater => "Comparison",
            BinaryOperator::GreaterEqual => "Comparison",
            BinaryOperator::LogicalAnd => "LogicalAnd",
            BinaryOperator::LogicalOr => "LogicalOr",
            BinaryOperator::BitwiseAnd => "BitwiseAnd",
            BinaryOperator::BitwiseOr => "BitwiseOr",
            BinaryOperator::BitwiseXor => "BitwiseXor",
            BinaryOperator::ShiftLeft => "ShiftLeft",
            BinaryOperator::ShiftRight => "ShiftRight",
            BinaryOperator::Pipe => "Pipe",
            BinaryOperator::PipeMaybe => "PipeMaybe",
        };

        let function_name = match op.operator {
            BinaryOperator::Add => "add",
            BinaryOperator::Subtract => "subtract",
            BinaryOperator::Multiply => "multiply",
            BinaryOperator::Divide => "divide",
            BinaryOperator::Modulo => "modulo",
            BinaryOperator::Exponent => "exponentiate",
            BinaryOperator::Equal => "equal?",
            BinaryOperator::NotEqual => "not_equal?",
            BinaryOperator::Less => "less?",
            BinaryOperator::LessEqual => "less_equal?",
            BinaryOperator::Greater => "greater?",
            BinaryOperator::GreaterEqual => "greater_equal?",
            BinaryOperator::LogicalAnd => "and",
            BinaryOperator::LogicalOr => "or",
            BinaryOperator::BitwiseAnd => "and",
            BinaryOperator::BitwiseOr => "or",
            BinaryOperator::BitwiseXor => "xor",
            BinaryOperator::ShiftLeft => "shift_left",
            BinaryOperator::ShiftRight => "shift_right",
            BinaryOperator::Pipe => "pipe_into",
            BinaryOperator::PipeMaybe => "maybe_pipe",
        };

        // Create trait function call: TraitName.function_name(lhs: left, rhs: right)
        Expression {
            kind: ExpressionKind::FunctionCall(FunctionCall {
                path: FunctionPath::Qualified {
                    module: TypeIdentifier {
                        name: trait_name.to_string(),
                        span: op.span,
                    },
                    name: Identifier {
                        name: function_name.to_string(),
                        span: op.span,
                    },
                },
                arguments: vec![
                    Argument::Named {
                        name: Identifier {
                            name: "lhs".to_string(),
                            span: op.span,
                        },
                        expression: Self::desugar_expression(*op.left),
                        format: ArgumentFormat::Explicit,
                        span: op.span,
                    },
                    Argument::Named {
                        name: Identifier {
                            name: "rhs".to_string(),
                            span: op.span,
                        },
                        expression: Self::desugar_expression(*op.right),
                        format: ArgumentFormat::Explicit,
                        span: op.span,
                    },
                ],
                span: op.span,
            }),
            span: op.span,
        }
    }

    /// Transform unary operation to trait function call
    fn desugar_unary_operation(op: UnaryOperation) -> Expression {
        let (trait_name, function_name) = match op.operator {
            UnaryOperator::Plus => ("UnaryPlus", "plus"),
            UnaryOperator::Minus => ("UnaryMinus", "minus"),
            UnaryOperator::LogicalNot => ("LogicalNot", "not"),
            UnaryOperator::BitwiseNot => ("BitwiseNot", "not"),
        };

        // Create trait function call: TraitName.function_name(value: operand)
        Expression {
            kind: ExpressionKind::FunctionCall(FunctionCall {
                path: FunctionPath::Qualified {
                    module: TypeIdentifier {
                        name: trait_name.to_string(),
                        span: op.span,
                    },
                    name: Identifier {
                        name: function_name.to_string(),
                        span: op.span,
                    },
                },
                arguments: vec![Argument::Named {
                    name: Identifier {
                        name: "value".to_string(),
                        span: op.span,
                    },
                    expression: Self::desugar_expression(*op.operand),
                    format: ArgumentFormat::Explicit,
                    span: op.span,
                }],
                span: op.span,
            }),
            span: op.span,
        }
    }
}
