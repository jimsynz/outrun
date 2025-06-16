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
        // The case_expression rule expands to either concrete_case_expression or trait_case_expression
        // We need to examine the inner pairs to determine which one it is
        match pair.as_rule() {
            Rule::case_expression => {
                // Get the first inner pair to determine the actual type
                let inner_pair = pair.into_inner().next().unwrap();
                match inner_pair.as_rule() {
                    Rule::concrete_case_expression => {
                        let concrete_case = Self::parse_concrete_case_expression(inner_pair)?;
                        Ok(CaseExpression::Concrete(concrete_case))
                    }
                    Rule::trait_case_expression => {
                        let trait_case = Self::parse_trait_case_expression(inner_pair)?;
                        Ok(CaseExpression::Trait(trait_case))
                    }
                    _ => {
                        let span = Self::span_from_pair(&inner_pair);
                        Err(Self::unexpected_token_from_span(
                            &span,
                            inner_pair.as_str(),
                            "Expected concrete or trait case expression",
                        ))
                    }
                }
            }
            Rule::concrete_case_expression => {
                let concrete_case = Self::parse_concrete_case_expression(pair)?;
                Ok(CaseExpression::Concrete(concrete_case))
            }
            Rule::trait_case_expression => {
                let trait_case = Self::parse_trait_case_expression(pair)?;
                Ok(CaseExpression::Trait(trait_case))
            }
            _ => {
                let span = Self::span_from_pair(&pair);
                Err(Self::unexpected_token_from_span(
                    &span,
                    pair.as_str(),
                    "Expected concrete or trait case expression",
                ))
            }
        }
    }

    /// Parse a concrete case expression from a Pest pair
    pub(crate) fn parse_concrete_case_expression(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<ConcreteCaseExpression> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // First pair should be the "case" keyword, skip it
        let _case_keyword = inner_pairs.next().unwrap();

        // Second pair should be the expression being matched
        let expression_pair = inner_pairs.next().unwrap();
        let expression = Self::parse_expression_from_pair(expression_pair)?;

        // Third pair should be the case_clauses
        let clauses_pair = inner_pairs.next().unwrap();
        let when_clauses = Self::parse_case_clauses(clauses_pair)?;

        Ok(ConcreteCaseExpression {
            expression: Box::new(expression),
            when_clauses,
            span,
        })
    }

    /// Parse a trait case expression from a Pest pair
    pub(crate) fn parse_trait_case_expression(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<TraitCaseExpression> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // First pair should be the "case" keyword, skip it
        let _case_keyword = inner_pairs.next().unwrap();

        // Second pair should be the expression being matched
        let expression_pair = inner_pairs.next().unwrap();
        let expression = Self::parse_expression_from_pair(expression_pair)?;

        // Third pair should be the "as" keyword, skip it
        let _as_keyword = inner_pairs.next().unwrap();

        // Fourth pair should be the trait name
        let trait_name_pair = inner_pairs.next().unwrap();
        let trait_name = Self::parse_type_identifier(trait_name_pair)?;

        // Fifth pair should be the trait_case_clauses
        let clauses_pair = inner_pairs.next().unwrap();
        let type_clauses = Self::parse_trait_case_clauses(clauses_pair)?;

        Ok(TraitCaseExpression {
            expression: Box::new(expression),
            trait_name,
            type_clauses,
            span,
        })
    }

    /// Parse case clauses (when clauses only - exhaustiveness checked)
    pub(crate) fn parse_case_clauses(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<CaseWhenClause>> {
        let mut when_clauses = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::case_when_clause {
                let when_clause = Self::parse_case_when_clause(inner_pair)?;
                when_clauses.push(when_clause);
            }
            // Skip other rules
        }

        Ok(when_clauses)
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

    /// Parse trait case clauses
    pub(crate) fn parse_trait_case_clauses(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<TraitCaseClause>> {
        let mut type_clauses = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::trait_case_clause {
                let trait_clause = Self::parse_trait_case_clause(inner_pair)?;
                type_clauses.push(trait_clause);
            }
            // Skip other rules
        }

        Ok(type_clauses)
    }

    /// Parse a trait case clause
    pub(crate) fn parse_trait_case_clause(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<TraitCaseClause> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // First pair should be the type name
        let type_name_pair = inner_pairs.next().unwrap();
        let type_name = Self::parse_type_identifier(type_name_pair)?;

        // Second pair might be struct field patterns (optional)
        let mut pattern = None;
        let mut guard = None;
        let mut result = None;

        for inner_pair in inner_pairs {
            match inner_pair.as_rule() {
                Rule::struct_field_patterns => {
                    // Parse struct field patterns
                    let mut fields = Vec::new();
                    for field_pair in inner_pair.into_inner() {
                        let field_pattern = Self::parse_struct_field_pattern(field_pair)?;
                        fields.push(field_pattern);
                    }
                    pattern = Some(StructPattern {
                        type_path: vec![type_name.clone()],
                        fields,
                        span: type_name.span, // Use type name span for now
                    });
                }
                Rule::guard_clause => {
                    let guard_pair = inner_pair.into_inner().next().unwrap(); // Skip "when" keyword
                    guard = Some(Self::parse_expression_from_pair(guard_pair)?);
                }
                Rule::block | Rule::expression => {
                    result = Some(Self::parse_case_result(inner_pair)?);
                }
                _ => {} // Skip other rules like arrow
            }
        }

        let result = result.ok_or_else(|| {
            Self::unexpected_token_from_span(&span, "", "Trait case clause missing result")
        })?;

        Ok(TraitCaseClause {
            type_name,
            pattern,
            guard,
            result,
            span,
        })
    }
}
