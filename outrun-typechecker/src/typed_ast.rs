//! Typed AST structures for universal clause-based dispatch
//!
//! This module defines AST structures that extend the parser AST with
//! type information and universal clause-based dispatch information.

use crate::types::Type;
use crate::universal_dispatch::ClauseId;
use outrun_parser::{Expression as ParserExpression, Span};

/// Typed expression with universal dispatch information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression {
    /// Original expression from parser
    pub parser_expression: ParserExpression,

    /// Inferred type for this expression
    pub inferred_type: Type,

    /// Kind-specific typed information
    pub typed_kind: TypedExpressionKind,

    /// Source span
    pub span: Span,
}

/// Type-aware expression kinds with universal dispatch
#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpressionKind {
    /// Literal expressions (retain original parser structure)
    Literal,

    /// Variable reference
    Variable { name: String, resolved_type: Type },

    /// Universal function call - ALL function types use this
    FunctionCall {
        /// Possible clauses for this function call (could be 1 or many)
        possible_clauses: Vec<ClauseId>,

        /// Original function path from parser
        original_path: outrun_parser::FunctionPath,

        /// Arguments with their inferred types
        typed_arguments: Vec<TypedArgument>,

        /// Inferred return type
        return_type: Type,
    },

    /// Other expression types that don't need special dispatch handling
    Other,
}

/// Typed argument with inferred type information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedArgument {
    /// Original argument from parser
    pub parser_argument: outrun_parser::Argument,

    /// Expression with its inferred type
    pub typed_expression: Box<TypedExpression>,
}

/// Result of universal function call resolution
#[derive(Debug, Clone, PartialEq)]
pub struct UniversalCallResolution {
    /// Possible clauses that could match this call
    pub possible_clauses: Vec<ClauseId>,

    /// Inferred return type
    pub return_type: Type,

    /// Whether this is a single-clause call (optimization hint)
    pub is_single_clause: bool,

    /// Whether type resolution was ambiguous
    pub is_ambiguous: bool,
}

impl UniversalCallResolution {
    /// Create a single-clause resolution (most common case)
    pub fn single(clause_id: ClauseId, return_type: Type) -> Self {
        Self {
            possible_clauses: vec![clause_id],
            return_type,
            is_single_clause: true,
            is_ambiguous: false,
        }
    }

    /// Create a multi-clause resolution (for ambiguous cases like Option.some?)
    pub fn multi(clause_ids: Vec<ClauseId>, return_type: Type) -> Self {
        Self {
            possible_clauses: clause_ids,
            return_type,
            is_single_clause: false,
            is_ambiguous: true,
        }
    }

    /// Check if this resolution has any clauses
    pub fn has_clauses(&self) -> bool {
        !self.possible_clauses.is_empty()
    }

    /// Get the number of possible clauses
    pub fn clause_count(&self) -> usize {
        self.possible_clauses.len()
    }
}

impl TypedExpression {
    /// Create a typed expression from a parser expression
    pub fn from_parser_expression(
        parser_expression: ParserExpression,
        inferred_type: Type,
        typed_kind: TypedExpressionKind,
    ) -> Self {
        let span = parser_expression.span;

        Self {
            parser_expression,
            inferred_type,
            typed_kind,
            span,
        }
    }

    /// Create a universal function call typed expression
    pub fn universal_function_call(
        parser_expression: ParserExpression,
        resolution: UniversalCallResolution,
        original_path: outrun_parser::FunctionPath,
        typed_arguments: Vec<TypedArgument>,
    ) -> Self {
        let span = parser_expression.span;
        let return_type = resolution.return_type.clone();

        Self {
            parser_expression,
            inferred_type: return_type.clone(),
            typed_kind: TypedExpressionKind::FunctionCall {
                possible_clauses: resolution.possible_clauses,
                original_path,
                typed_arguments,
                return_type,
            },
            span,
        }
    }

    /// Check if this is a universal function call
    pub fn is_universal_function_call(&self) -> bool {
        matches!(self.typed_kind, TypedExpressionKind::FunctionCall { .. })
    }

    /// Get the clause IDs if this is a function call
    pub fn get_clause_ids(&self) -> Option<&[ClauseId]> {
        match &self.typed_kind {
            TypedExpressionKind::FunctionCall {
                possible_clauses, ..
            } => Some(possible_clauses),
            _ => None,
        }
    }

    /// Check if this function call is ambiguous (multiple possible clauses)
    pub fn is_ambiguous_call(&self) -> bool {
        match &self.typed_kind {
            TypedExpressionKind::FunctionCall {
                possible_clauses, ..
            } => possible_clauses.len() > 1,
            _ => false,
        }
    }
}

impl TypedArgument {
    /// Create a typed argument from parser argument and typed expression
    pub fn new(
        parser_argument: outrun_parser::Argument,
        typed_expression: TypedExpression,
    ) -> Self {
        Self {
            parser_argument,
            typed_expression: Box::new(typed_expression),
        }
    }

    /// Get the inferred type of this argument
    pub fn get_type(&self) -> &Type {
        &self.typed_expression.inferred_type
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::universal_dispatch::ClauseId;
    use outrun_parser::{FunctionPath, Identifier, Span};

    #[test]
    fn test_universal_call_resolution_single() {
        let signature = crate::universal_dispatch::FunctionSignature::simple("test_function".to_string());
        let clause_id = ClauseId::deterministic(&signature, &[crate::universal_dispatch::Guard::AlwaysTrue]);
        let return_type = crate::types::Type::concrete("String");

        let resolution = UniversalCallResolution::single(clause_id, return_type.clone());

        assert!(resolution.is_single_clause);
        assert!(!resolution.is_ambiguous);
        assert_eq!(resolution.clause_count(), 1);
        assert_eq!(resolution.possible_clauses[0], clause_id);
        assert_eq!(resolution.return_type, return_type);
    }

    #[test]
    fn test_universal_call_resolution_multi() {
        let signature = crate::universal_dispatch::FunctionSignature::simple("test_function".to_string());
        let clause_id1 = ClauseId::deterministic(&signature, &[crate::universal_dispatch::Guard::AlwaysTrue]);
        let clause_id2 = ClauseId::deterministic(&signature, &[crate::universal_dispatch::Guard::TypeCompatible {
            target_type: crate::types::Type::concrete("String"),
            implementing_type: crate::types::Type::concrete("String"),
            constraint_context: crate::universal_dispatch::ConstraintContext::new(),
        }]);
        let return_type = crate::types::Type::concrete("Boolean");

        let resolution =
            UniversalCallResolution::multi(vec![clause_id1, clause_id2], return_type.clone());

        assert!(!resolution.is_single_clause);
        assert!(resolution.is_ambiguous);
        assert_eq!(resolution.clause_count(), 2);
        assert_eq!(resolution.possible_clauses, vec![clause_id1, clause_id2]);
        assert_eq!(resolution.return_type, return_type);
    }

    #[test]
    fn test_typed_expression_function_call() {
        // Create a mock parser expression for function call
        let parser_expr = ParserExpression {
            kind: outrun_parser::ExpressionKind::FunctionCall(outrun_parser::FunctionCall {
                path: FunctionPath::Simple {
                    name: Identifier {
                        name: "test_function".to_string(),
                        span: Span::new(0, 13),
                    },
                },
                arguments: vec![],
                span: Span::new(0, 15),
                resolved_function_key: None,
                universal_clause_ids: None,
            }),
            span: Span::new(0, 15),
            type_info: None,
        };

        let signature = crate::universal_dispatch::FunctionSignature::simple("test_function".to_string());
        let clause_id = ClauseId::deterministic(&signature, &[crate::universal_dispatch::Guard::AlwaysTrue]);
        let return_type = crate::types::Type::concrete("Integer");
        let resolution = UniversalCallResolution::single(clause_id, return_type.clone());

        let typed_expr = TypedExpression::universal_function_call(
            parser_expr,
            resolution,
            FunctionPath::Simple {
                name: Identifier {
                    name: "test_function".to_string(),
                    span: Span::new(0, 13),
                },
            },
            vec![],
        );

        assert!(typed_expr.is_universal_function_call());
        assert!(!typed_expr.is_ambiguous_call());
        assert_eq!(typed_expr.get_clause_ids(), Some([clause_id].as_slice()));
        assert_eq!(typed_expr.inferred_type, return_type);
    }

    #[test]
    fn test_typed_expression_ambiguous_call() {
        // Create a mock parser expression
        let parser_expr = ParserExpression {
            kind: outrun_parser::ExpressionKind::FunctionCall(outrun_parser::FunctionCall {
                path: FunctionPath::Qualified {
                    module: outrun_parser::TypeIdentifier {
                        name: "Option".to_string(),
                        span: Span::new(0, 6),
                    },
                    name: Identifier {
                        name: "some?".to_string(),
                        span: Span::new(7, 12),
                    },
                },
                arguments: vec![],
                span: Span::new(0, 15),
                resolved_function_key: None,
                universal_clause_ids: None,
            }),
            span: Span::new(0, 15),
            type_info: None,
        };

        let signature = crate::universal_dispatch::FunctionSignature::new(
            vec!["Option".to_string()],
            "some?".to_string(),
        );
        let clause_id1 = ClauseId::deterministic(&signature, &[crate::universal_dispatch::Guard::TypeCompatible {
            target_type: crate::types::Type::concrete("Some"),
            implementing_type: crate::types::Type::concrete("Some"),
            constraint_context: crate::universal_dispatch::ConstraintContext::new(),
        }]); // Some<T> implementation
        let clause_id2 = ClauseId::deterministic(&signature, &[crate::universal_dispatch::Guard::TypeCompatible {
            target_type: crate::types::Type::concrete("None"),
            implementing_type: crate::types::Type::concrete("None"),
            constraint_context: crate::universal_dispatch::ConstraintContext::new(),
        }]); // None<T> implementation
        let return_type = crate::types::Type::concrete("Boolean");
        let resolution =
            UniversalCallResolution::multi(vec![clause_id1, clause_id2], return_type.clone());

        let typed_expr = TypedExpression::universal_function_call(
            parser_expr,
            resolution,
            FunctionPath::Qualified {
                module: outrun_parser::TypeIdentifier {
                    name: "Option".to_string(),
                    span: Span::new(0, 6),
                },
                name: Identifier {
                    name: "some?".to_string(),
                    span: Span::new(7, 12),
                },
            },
            vec![],
        );

        assert!(typed_expr.is_universal_function_call());
        assert!(typed_expr.is_ambiguous_call());
        assert_eq!(
            typed_expr.get_clause_ids(),
            Some([clause_id1, clause_id2].as_slice())
        );
        assert_eq!(typed_expr.inferred_type, return_type);
    }
}
