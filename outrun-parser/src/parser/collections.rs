// Collection parsing functions for the Outrun parser
// Handles list, map, tuple, and struct literal parsing

use super::{OutrunParser, Rule};
use crate::ast::*;
use crate::error::*;

impl OutrunParser {
    /// Parse a list literal from a Pest pair
    pub(super) fn parse_list(pair: pest::iterators::Pair<Rule>) -> ParseResult<ListLiteral> {
        let span = Self::extract_span(&pair);
        let mut elements = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::list_elements {
                for element_pair in inner_pair.into_inner() {
                    if element_pair.as_rule() == Rule::list_element {
                        let list_element = Self::parse_list_element(element_pair)?;
                        elements.push(list_element);
                    }
                }
            } // Skip other rules (like brackets)
        }

        Ok(ListLiteral { elements, span })
    }

    pub(super) fn parse_list_element(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<ListElement> {
        let rule = pair.as_rule();
        let span = Self::extract_span(&pair);

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::expression => {
                    let expr = Self::parse_expression_from_pair(inner_pair)?;
                    return Ok(ListElement::Expression(Box::new(expr)));
                }
                Rule::spread_element => {
                    let spread = Self::parse_spread_element(inner_pair)?;
                    return Ok(ListElement::Spread(spread));
                }
                _ => {} // Skip other rules
            }
        }
        Err(ParseError::UnexpectedRule {
            expected: "expression or spread_element".to_string(),
            found: rule,
            span,
        })
    }

    pub(super) fn parse_spread_element(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Identifier> {
        // spread_element is atomic: ".." ~ identifier
        let text = pair.as_str();
        if let Some(identifier_text) = text.strip_prefix("..") {
            let span = Self::span_from_range(
                pair.as_span().start() + 2, // Skip the ".." prefix for the identifier span
                pair.as_span().end(),
            );
            Ok(Identifier {
                name: identifier_text.to_string(),
                span,
            })
        } else {
            Err(Self::invalid_spread_element_error(&pair))
        }
    }

    /// Parse a map literal from a Pest pair
    pub(super) fn parse_map(pair: pest::iterators::Pair<Rule>) -> ParseResult<MapLiteral> {
        let span = Self::extract_span(&pair);
        let mut entries = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::map_entries {
                for entry_pair in inner_pair.into_inner() {
                    if entry_pair.as_rule() == Rule::map_entry {
                        let entry = Self::parse_map_entry(entry_pair)?;
                        entries.push(entry);
                    }
                }
            } // Skip other rules (like braces)
        }

        Ok(MapLiteral { entries, span })
    }

    /// Parse a map entry from a Pest pair
    pub(super) fn parse_map_entry(pair: pest::iterators::Pair<Rule>) -> ParseResult<MapEntry> {
        let inner_pair = pair.into_inner().next().unwrap();

        match inner_pair.as_rule() {
            Rule::map_entry_shorthand => {
                let mut inner_pairs = inner_pair.into_inner();
                let identifier_pair = inner_pairs.next().unwrap();
                let value_pair = inner_pairs.next().unwrap();

                let name = Self::parse_identifier(identifier_pair)?;
                let value = Self::parse_expression_from_pair(value_pair)?;

                Ok(MapEntry::Shorthand {
                    name,
                    value: Box::new(value),
                })
            }
            Rule::map_entry_explicit => {
                let mut inner_pairs = inner_pair.into_inner();
                let key_pair = inner_pairs.next().unwrap();
                let value_pair = inner_pairs.next().unwrap();

                let key = Self::parse_expression_from_pair(key_pair)?;
                let value = Self::parse_expression_from_pair(value_pair)?;

                Ok(MapEntry::Assignment {
                    key: Box::new(key),
                    value: Box::new(value),
                })
            }
            Rule::map_entry_spread => {
                let text = inner_pair.as_str();
                if let Some(identifier_text) = text.strip_prefix("..") {
                    let span = Self::span_from_range(
                        inner_pair.as_span().start() + 2, // Skip the ".." prefix
                        inner_pair.as_span().end(),
                    );
                    Ok(MapEntry::Spread(Identifier {
                        name: identifier_text.to_string(),
                        span,
                    }))
                } else {
                    Err(Self::invalid_spread_element_error(&inner_pair))
                }
            }
            _ => Err(Self::unexpected_rule_error(
                &inner_pair,
                "map entry shorthand, explicit, or spread",
            )),
        }
    }

    /// Parse a tuple literal from a Pest pair
    pub(super) fn parse_tuple(pair: pest::iterators::Pair<Rule>) -> ParseResult<TupleLiteral> {
        let span = Self::extract_span(&pair);
        let mut elements = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::tuple_elements {
                for element_pair in inner_pair.into_inner() {
                    if element_pair.as_rule() == Rule::expression {
                        let expr = Self::parse_expression_from_pair(element_pair)?;
                        elements.push(expr);
                    }
                }
            } // Skip other rules (like parentheses)
        }

        Ok(TupleLiteral { elements, span })
    }

    /// Parse a struct literal from a Pest pair
    pub(super) fn parse_struct_literal(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<StructLiteral> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        // First should be the type identifier
        let type_name = Self::parse_type_identifier(inner_pairs.next().unwrap())?;

        let mut fields = Vec::new();

        // Parse struct fields if present
        for inner_pair in inner_pairs {
            if inner_pair.as_rule() == Rule::struct_literal_fields {
                for field_pair in inner_pair.into_inner() {
                    if field_pair.as_rule() == Rule::struct_literal_field {
                        let field = Self::parse_struct_literal_field(field_pair)?;
                        fields.push(field);
                    }
                }
            }
        }

        Ok(StructLiteral {
            type_name,
            fields,
            span,
        })
    }

    pub(super) fn parse_struct_literal_field(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<StructLiteralField> {
        let inner_pair = pair.into_inner().next().unwrap();

        match inner_pair.as_rule() {
            Rule::struct_field_assignment => {
                let mut assignment_pairs = inner_pair.into_inner();
                let name = Self::parse_identifier(assignment_pairs.next().unwrap())?;
                let value = Self::parse_expression_from_pair(assignment_pairs.next().unwrap())?;
                Ok(StructLiteralField::Assignment {
                    name,
                    value: Box::new(value),
                })
            }
            Rule::struct_field_shorthand => {
                let name = Self::parse_identifier(inner_pair.into_inner().next().unwrap())?;
                Ok(StructLiteralField::Shorthand(name))
            }
            Rule::struct_field_spread => {
                let text = inner_pair.as_str();
                if let Some(identifier_text) = text.strip_prefix("..") {
                    let span = Self::span_from_range(
                        inner_pair.as_span().start() + 2, // Skip the ".." prefix
                        inner_pair.as_span().end(),
                    );
                    Ok(StructLiteralField::Spread(Identifier {
                        name: identifier_text.to_string(),
                        span,
                    }))
                } else {
                    Err(Self::invalid_spread_element_error(&inner_pair))
                }
            }
            _ => Err(Self::unexpected_rule_error(
                &inner_pair,
                "struct field assignment, shorthand, or spread",
            )),
        }
    }
}
