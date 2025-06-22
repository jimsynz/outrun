// Control flow parsing module
// Handles if expressions, case expressions, and related constructs

use crate::ast::*;
use crate::error::*;
use crate::parser::{OutrunParser, Rule};

impl OutrunParser {
    /// Parse an if expression from a Pest pair
    pub(crate) fn parse_if_expression(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<IfExpression> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // First pair should be the "if" keyword, skip it
        let _if_keyword = inner_pairs.next().unwrap();

        // Second pair should be the condition expression
        let condition_pair = inner_pairs.next().unwrap();
        let condition = Self::parse_expression_from_pair(condition_pair)?;

        // Third pair should be the then block
        let then_block_pair = inner_pairs.next().unwrap();
        let then_block = Self::parse_block(then_block_pair)?;

        // Fourth pair might be the "else" keyword and block (optional)
        let mut else_block = None;
        if let Some(_else_keyword_pair) = inner_pairs.next() {
            // Skip the "else" keyword
            let else_block_pair = inner_pairs.next().unwrap();
            else_block = Some(Self::parse_block(else_block_pair)?);
        }

        Ok(IfExpression {
            condition: Box::new(condition),
            then_block,
            else_block,
            span,
        })
    }

    /// Parse a case expression from a Pest pair
    pub(crate) fn parse_case_expression(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<CaseExpression> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // First pair should be the "case" keyword, skip it
        let _case_keyword = inner_pairs.next().unwrap();

        // Second pair should be the expression being matched
        let expression_pair = inner_pairs.next().unwrap();
        let expression = Self::parse_expression_from_pair(expression_pair)?;

        // Third pair should be the case_clauses
        let clauses_pair = inner_pairs.next().unwrap();
        let clauses = Self::parse_case_clauses_unified(clauses_pair)?;

        Ok(CaseExpression {
            expression: Box::new(expression),
            clauses,
            span,
        })
    }

    /// Parse unified case clauses (pattern-based matching)
    pub(crate) fn parse_case_clauses_unified(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<CaseClause>> {
        let mut clauses = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::case_clause {
                let clause = Self::parse_case_clause_unified(inner_pair)?;
                clauses.push(clause);
            }
            // Skip other rules
        }

        Ok(clauses)
    }

    /// Parse a case result (either block or expression)
    pub(crate) fn parse_case_result(pair: pest::iterators::Pair<Rule>) -> ParseResult<CaseResult> {
        match pair.as_rule() {
            Rule::block => {
                let block = Self::parse_block(pair)?;
                Ok(CaseResult::Block(block))
            }
            Rule::expression => {
                let expression = Self::parse_expression_from_pair(pair)?;
                Ok(CaseResult::Expression(Box::new(expression)))
            }
            _ => {
                // Handle direct expressions by trying to parse as expression
                let expression = Self::parse_expression_from_pair(pair)?;
                Ok(CaseResult::Expression(Box::new(expression)))
            }
        }
    }

    /// Parse a unified case clause with pattern matching
    pub(crate) fn parse_case_clause_unified(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<CaseClause> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // First pair should be the pattern
        let pattern_pair = inner_pairs.next().unwrap();
        let pattern = Self::parse_pattern(pattern_pair)?;

        // Look for optional guard clause and result
        let mut guard = None;
        let mut result = None;

        for inner_pair in inner_pairs {
            match inner_pair.as_rule() {
                Rule::guard_clause => {
                    // Parse guard clause
                    let mut guard_inner = inner_pair.into_inner();
                    let _when_keyword = guard_inner.next().unwrap(); // Skip "when"
                    let guard_expr_pair = guard_inner.next().unwrap();

                    // guard_expr_pair is a guard_expression rule, we need to get the inner expression
                    if guard_expr_pair.as_rule() == Rule::guard_expression {
                        let inner_expr_pair = guard_expr_pair.into_inner().next().unwrap();
                        guard = Some(Self::parse_expression_from_pair(inner_expr_pair)?);
                    } else {
                        return Err(Self::unexpected_token_from_pair(
                            &guard_expr_pair,
                            "Expected guard expression",
                        ));
                    }
                }
                Rule::arrow => {
                    // Skip arrow token
                }
                Rule::block | Rule::expression => {
                    result = Some(Self::parse_case_result(inner_pair)?);
                }
                _ => {} // Skip other rules
            }
        }

        let result = result.ok_or_else(|| {
            Self::unexpected_token_from_span(&span, "", "Case clause missing result")
        })?;

        Ok(CaseClause {
            pattern,
            guard,
            result,
            span,
        })
    }
}
