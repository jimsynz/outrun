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
        let (when_clauses, else_clause) = Self::parse_case_clauses(clauses_pair)?;

        Ok(CaseExpression {
            expression: Box::new(expression),
            when_clauses,
            else_clause,
            span,
        })
    }

    /// Parse case clauses (when clauses and else clause)
    pub(crate) fn parse_case_clauses(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<(Vec<CaseWhenClause>, CaseElseClause)> {
        let mut when_clauses = Vec::new();
        let mut else_clause = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::case_when_clause => {
                    let when_clause = Self::parse_case_when_clause(inner_pair)?;
                    when_clauses.push(when_clause);
                }
                Rule::case_else_clause => {
                    else_clause = Some(Self::parse_case_else_clause(inner_pair)?);
                }
                _ => {} // Skip other rules
            }
        }

        let else_clause = else_clause.ok_or_else(|| {
            let placeholder_span = Self::span_from_range(0, 1);
            Self::unexpected_token_from_span(
                &placeholder_span,
                "",
                "Case expression missing else clause",
            )
        })?;

        Ok((when_clauses, else_clause))
    }

    /// Parse a when clause in a case expression
    pub(crate) fn parse_case_when_clause(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<CaseWhenClause> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // First pair should be the "when" keyword, skip it
        let _when_keyword = inner_pairs.next().unwrap();

        // Second pair should be the guard expression
        let guard_pair = inner_pairs.next().unwrap();
        let guard = Self::parse_expression_from_pair(guard_pair)?;

        // Third pair should be the arrow rule, skip it
        let _arrow = inner_pairs.next().unwrap();

        // Fourth pair should be the result (block or expression)
        let result_pair = inner_pairs.next().unwrap();
        let result = Self::parse_case_result(result_pair)?;

        Ok(CaseWhenClause {
            guard,
            result,
            span,
        })
    }

    /// Parse an else clause in a case expression
    pub(crate) fn parse_case_else_clause(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<CaseElseClause> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // First pair should be the "else" keyword, skip it
        let _else_keyword = inner_pairs.next().unwrap();

        // Second pair should be the arrow rule, skip it
        let _arrow = inner_pairs.next().unwrap();

        // Third pair should be the result (block or expression)
        let result_pair = inner_pairs.next().unwrap();
        let result = Self::parse_case_result(result_pair)?;

        Ok(CaseElseClause { result, span })
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
}
