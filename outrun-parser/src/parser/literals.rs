// Literal parsing functions for the Outrun parser
// Handles boolean, integer, float, string, atom, and sigil literals

use super::{OutrunParser, Rule};
use crate::ast::*;
use crate::error::*;

impl OutrunParser {
    /// Parse a boolean literal from a Pest pair
    pub(super) fn parse_boolean(pair: pest::iterators::Pair<Rule>) -> ParseResult<BooleanLiteral> {
        let span = Self::extract_span(&pair);

        // Get the inner boolean rule
        let inner = pair.into_inner().next().unwrap();
        let value = match inner.as_rule() {
            Rule::boolean_true => true,
            Rule::boolean_false => false,
            _ => unreachable!("Invalid boolean rule"),
        };

        Ok(BooleanLiteral { value, span })
    }

    /// Parse an integer literal from a Pest pair
    pub(super) fn parse_integer(pair: pest::iterators::Pair<Rule>) -> ParseResult<IntegerLiteral> {
        let (inner, text, span) = Self::extract_inner_with_text(pair)?;

        // Parse based on format
        let (value, format) = match inner.as_rule() {
            Rule::integer_decimal => {
                let value = text.parse::<i64>().map_err(|_| {
                    Self::create_parse_error(&span, &text, ParseError::invalid_integer)
                })?;
                (value, IntegerFormat::Decimal)
            }
            Rule::integer_binary => {
                let binary_part = &text[2..]; // Remove "0b" prefix
                let value = i64::from_str_radix(binary_part, 2).map_err(|_| {
                    Self::create_parse_error(&span, &text, ParseError::invalid_integer)
                })?;
                (value, IntegerFormat::Binary)
            }
            Rule::integer_octal => {
                let octal_part = &text[2..]; // Remove "0o" prefix
                let value = i64::from_str_radix(octal_part, 8).map_err(|_| {
                    Self::create_parse_error(&span, &text, ParseError::invalid_integer)
                })?;
                (value, IntegerFormat::Octal)
            }
            Rule::integer_hexadecimal => {
                let hex_part = &text[2..]; // Remove "0x" prefix
                let value = i64::from_str_radix(hex_part, 16).map_err(|_| {
                    Self::create_parse_error(&span, &text, ParseError::invalid_integer)
                })?;
                (value, IntegerFormat::Hexadecimal)
            }
            _ => unreachable!("Invalid integer rule"),
        };

        Ok(IntegerLiteral {
            value,
            format,
            span,
        })
    }

    /// Parse a float literal from a Pest pair
    pub(super) fn parse_float(pair: pest::iterators::Pair<Rule>) -> ParseResult<FloatLiteral> {
        let (inner, text, span) = Self::extract_inner_with_text(pair)?;

        // Parse based on format
        let (value, format) = match inner.as_rule() {
            Rule::float_standard => {
                let value = text.parse::<f64>().map_err(|_| {
                    Self::create_parse_error(&span, &text, ParseError::invalid_float)
                })?;
                (value, FloatFormat::Standard)
            }
            Rule::float_scientific => {
                let value = text.parse::<f64>().map_err(|_| {
                    Self::create_parse_error(&span, &text, ParseError::invalid_float)
                })?;

                // Determine if exponent was uppercase or lowercase
                let exponent_case = if text.contains('E') {
                    ExponentCase::Uppercase
                } else {
                    ExponentCase::Lowercase
                };

                (value, FloatFormat::Scientific { exponent_case })
            }
            _ => unreachable!("Invalid float rule"),
        };

        Ok(FloatLiteral {
            value,
            format,
            span,
        })
    }

    /// Parse a string literal from a Pest pair
    pub(super) fn parse_string(pair: pest::iterators::Pair<Rule>) -> ParseResult<StringLiteral> {
        let span = Self::extract_span(&pair);

        // Get the inner string rule
        let inner = pair.into_inner().next().unwrap();

        // Parse based on format
        match inner.as_rule() {
            Rule::string_basic => {
                let parts = Self::parse_string_parts(inner)?;
                Ok(StringLiteral {
                    parts,
                    format: StringFormat::Basic,
                    span,
                })
            }
            Rule::string_multiline => {
                let parts = Self::parse_multiline_string_parts(inner)?;
                Ok(StringLiteral {
                    parts,
                    format: StringFormat::Multiline,
                    span,
                })
            }
            _ => unreachable!("Invalid string rule"),
        }
    }

    /// Parse the parts of a basic string (text and interpolations)
    pub(super) fn parse_string_parts(
        string_pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<StringPart>> {
        let mut parts = Vec::new();

        for content_pair in string_pair.into_inner() {
            if content_pair.as_rule() == Rule::string_content {
                for part_pair in content_pair.into_inner() {
                    match part_pair.as_rule() {
                        Rule::string_text => {
                            let text = part_pair.as_str();
                            parts.push(StringPart::Text {
                                content: text.to_string(),
                                raw_content: text.to_string(),
                            });
                        }
                        Rule::escape_sequence => {
                            let raw_content = part_pair.as_str();
                            let processed = Self::process_escape_sequences(raw_content)?;
                            parts.push(StringPart::Text {
                                content: processed,
                                raw_content: raw_content.to_string(),
                            });
                        }
                        Rule::string_interpolation => {
                            let interpolation_span = Self::span_from_pair(&part_pair);

                            // Parse the expression inside #{...}
                            let expr_pair = part_pair.into_inner().next().unwrap();
                            let expression = Self::parse_expression_from_pair(expr_pair)?;

                            parts.push(StringPart::Interpolation {
                                expression: Box::new(expression),
                                span: interpolation_span,
                            });
                        }
                        _ => {} // Skip other rules
                    }
                }
            } // Skip quote characters
        }

        Ok(parts)
    }

    /// Parse the parts of a multiline string (text, newlines, and interpolations)
    pub(super) fn parse_multiline_string_parts(
        string_pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<StringPart>> {
        let mut parts = Vec::new();

        for content_pair in string_pair.into_inner() {
            if content_pair.as_rule() == Rule::multiline_content {
                for part_pair in content_pair.into_inner() {
                    match part_pair.as_rule() {
                        Rule::multiline_text => {
                            let text = part_pair.as_str();
                            parts.push(StringPart::Text {
                                content: text.to_string(),
                                raw_content: text.to_string(),
                            });
                        }
                        Rule::escape_sequence => {
                            let raw_content = part_pair.as_str();
                            let processed = Self::process_escape_sequences(raw_content)?;
                            parts.push(StringPart::Text {
                                content: processed,
                                raw_content: raw_content.to_string(),
                            });
                        }
                        Rule::string_interpolation => {
                            let interpolation_span = Self::span_from_pair(&part_pair);

                            // Parse the expression inside #{...}
                            let expr_pair = part_pair.into_inner().next().unwrap();
                            let expression = Self::parse_expression_from_pair(expr_pair)?;

                            parts.push(StringPart::Interpolation {
                                expression: Box::new(expression),
                                span: interpolation_span,
                            });
                        }
                        _ => {} // Skip other rules
                    }
                }
            } // Skip quote characters
        }

        Ok(parts)
    }

    /// Process escape sequences in strings
    pub(super) fn process_escape_sequences(input: &str) -> ParseResult<String> {
        let mut result = String::new();
        let mut chars = input.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                match chars.next() {
                    Some('n') => result.push('\n'),
                    Some('t') => result.push('\t'),
                    Some('r') => result.push('\r'),
                    Some('\\') => result.push('\\'),
                    Some('"') => result.push('"'),
                    Some('u') => {
                        // Unicode escape sequence: \uXXXX
                        let mut hex_digits = String::new();
                        for _ in 0..4 {
                            if let Some(hex_digit) = chars.next() {
                                hex_digits.push(hex_digit);
                            } else {
                                return Err(Self::invalid_string_escape_error(
                                    "Incomplete unicode escape sequence",
                                ));
                            }
                        }

                        if let Ok(code_point) = u32::from_str_radix(&hex_digits, 16) {
                            if let Some(unicode_char) = char::from_u32(code_point) {
                                result.push(unicode_char);
                            } else {
                                return Err(Self::invalid_string_escape_error(&format!(
                                    "Invalid unicode code point: {}",
                                    hex_digits
                                )));
                            }
                        } else {
                            return Err(Self::invalid_string_escape_error(&format!(
                                "Invalid hex digits in unicode escape: {}",
                                hex_digits
                            )));
                        }
                    }
                    Some(other) => {
                        return Err(Self::invalid_string_escape_error(&format!(
                            "Unknown escape sequence: \\{}",
                            other
                        )));
                    }
                    None => {
                        return Err(Self::invalid_string_escape_error(
                            "Incomplete escape sequence",
                        ));
                    }
                }
            } else {
                result.push(ch);
            }
        }

        Ok(result)
    }

    /// Parse an atom literal from a Pest pair
    pub(super) fn parse_atom(pair: pest::iterators::Pair<Rule>) -> ParseResult<AtomLiteral> {
        let span = Self::span_from_pair(&pair);

        // Get the inner atom rule
        let inner = pair.into_inner().next().unwrap();
        let text = inner.as_str();

        // Parse based on format
        match inner.as_rule() {
            Rule::atom_simple => {
                // Remove the : prefix to get the name
                let name = &text[1..];

                Ok(AtomLiteral {
                    name: name.to_string(),
                    content: name.to_string(), // For simple atoms, content = name
                    raw_content: name.to_string(), // For simple atoms, raw = name
                    format: AtomFormat::Simple,
                    span,
                })
            }
            Rule::atom_quoted => {
                // Remove the :" prefix and " suffix to get the raw content
                let raw_content = &text[2..text.len() - 1];

                // Process escape sequences to get the actual content
                let content = Self::process_escape_sequences(raw_content)?;

                Ok(AtomLiteral {
                    name: content.clone(), // For quoted atoms, the name is the processed content
                    content,
                    raw_content: raw_content.to_string(),
                    format: AtomFormat::Quoted,
                    span,
                })
            }
            _ => unreachable!("Invalid atom rule"),
        }
    }

    /// Parse a sigil literal from a Pest pair
    pub(super) fn parse_sigil(pair: pest::iterators::Pair<Rule>) -> ParseResult<SigilLiteral> {
        let span = Self::span_from_pair(&pair);

        let mut inner_pairs = pair.into_inner();

        // First should be the type identifier
        let type_pair = inner_pairs.next().unwrap();
        let sigil_type = Self::parse_type_identifier(type_pair)?;

        // Second should be the string (basic or multiline) - direct rule, not wrapped
        let string_pair = inner_pairs.next().unwrap();
        let string_span = Self::span_from_pair(&string_pair);
        let string = match string_pair.as_rule() {
            Rule::string_basic => {
                let parts = Self::parse_string_parts(string_pair)?;
                StringLiteral {
                    parts,
                    format: StringFormat::Basic,
                    span: string_span,
                }
            }
            Rule::string_multiline => {
                let parts = Self::parse_multiline_string_parts(string_pair)?;
                StringLiteral {
                    parts,
                    format: StringFormat::Multiline,
                    span: string_span,
                }
            }
            _ => unreachable!("Invalid string rule in sigil"),
        };

        Ok(SigilLiteral {
            sigil_type,
            string,
            span,
        })
    }
}
