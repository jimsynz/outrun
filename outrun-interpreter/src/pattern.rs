//! Pattern matching engine for the new Outrun interpreter
//!
//! This module implements pattern matching for let bindings and case expressions,
//! supporting all pattern types in the parser AST: identifier, literal, tuple,
//! struct, and list patterns with proper variable binding.

use crate::context::{InterpreterContext, InterpreterError};
use crate::value::{List, Value};
use outrun_parser::{
    ListPattern, Literal, LiteralPattern, Pattern, Span, StructPattern, TuplePattern,
};
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

/// Errors that can occur during pattern matching
#[derive(Debug, Error)]
pub enum PatternMatchError {
    #[error("Pattern match failed: {message}")]
    MatchFailed {
        message: String,
        pattern_span: Span,
        value_span: Option<Span>,
    },

    #[error("Type mismatch in pattern: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        pattern_span: Span,
    },

    #[error("List pattern requires at least {min} elements, found {actual}")]
    InsufficientElements {
        min: usize,
        actual: usize,
        pattern_span: Span,
    },

    #[error("Context error during pattern matching: {source}")]
    Context {
        #[from]
        source: InterpreterError,
    },
}

/// Result of pattern matching - success contains variable bindings
pub type PatternMatchResult = Result<HashMap<String, Value>, PatternMatchError>;

/// Pattern matching engine for evaluating patterns against values
pub struct PatternMatcher;

impl PatternMatcher {
    /// Match a pattern against a value, returning variable bindings on success
    pub fn match_pattern(
        pattern: &Pattern,
        value: &Value,
        context: &InterpreterContext,
    ) -> PatternMatchResult {
        match pattern {
            Pattern::Identifier(identifier) => {
                // Identifier patterns always match, binding the value to the name
                let mut bindings = HashMap::new();
                bindings.insert(identifier.name.clone(), value.clone());
                Ok(bindings)
            }

            Pattern::Literal(literal_pattern) => {
                Self::match_literal_pattern(literal_pattern, value)
            }

            Pattern::Tuple(tuple_pattern) => {
                Self::match_tuple_pattern(tuple_pattern, value, context)
            }

            Pattern::Struct(struct_pattern) => {
                Self::match_struct_pattern(struct_pattern, value, context)
            }

            Pattern::List(list_pattern) => Self::match_list_pattern(list_pattern, value, context),
        }
    }

    /// Match a literal pattern (exact value comparison)
    fn match_literal_pattern(pattern: &LiteralPattern, value: &Value) -> PatternMatchResult {
        let expected_value = Self::literal_to_value(&pattern.literal);

        if *value == expected_value {
            Ok(HashMap::new()) // Literal patterns don't bind variables
        } else {
            Err(PatternMatchError::MatchFailed {
                message: format!(
                    "Expected literal {}, found {}",
                    expected_value.display(),
                    value.display()
                ),
                pattern_span: pattern.span,
                value_span: None,
            })
        }
    }

    /// Match a tuple pattern against a tuple-like structure
    fn match_tuple_pattern(
        pattern: &TuplePattern,
        value: &Value,
        context: &InterpreterContext,
    ) -> PatternMatchResult {
        // For now, we'll treat lists as tuples since Outrun doesn't have separate tuple values
        // This matches the functional programming approach where tuples are just fixed-size lists
        match value {
            Value::List { list, .. } => {
                let elements = Self::list_to_vec(list);

                if elements.len() != pattern.elements.len() {
                    return Err(PatternMatchError::MatchFailed {
                        message: format!(
                            "Tuple pattern expects {} elements, found {}",
                            pattern.elements.len(),
                            elements.len()
                        ),
                        pattern_span: pattern.span,
                        value_span: None,
                    });
                }

                let mut all_bindings = HashMap::new();

                for (pattern_elem, value_elem) in pattern.elements.iter().zip(elements.iter()) {
                    let bindings = Self::match_pattern(pattern_elem, value_elem, context)?;
                    all_bindings.extend(bindings);
                }

                Ok(all_bindings)
            }
            _ => Err(PatternMatchError::TypeMismatch {
                expected: "Tuple (List)".to_string(),
                found: value.type_name().to_string(),
                pattern_span: pattern.span,
            }),
        }
    }

    /// Match a struct pattern against a struct value
    fn match_struct_pattern(
        pattern: &StructPattern,
        value: &Value,
        context: &InterpreterContext,
    ) -> PatternMatchResult {
        match value {
            Value::Struct { fields, .. } => {
                // For now, we'll do basic struct matching
                // TODO: Add proper type path matching when type system is integrated

                let mut all_bindings = HashMap::new();

                for field_pattern in &pattern.fields {
                    let field_name = &field_pattern.name.name;

                    if let Some(field_value) = fields.get(field_name) {
                        match &field_pattern.pattern {
                            Some(pattern) => {
                                // Full pattern matching on field
                                let bindings = Self::match_pattern(pattern, field_value, context)?;
                                all_bindings.extend(bindings);
                            }
                            None => {
                                // Shorthand syntax: bind field to same-named variable
                                all_bindings.insert(field_name.clone(), field_value.clone());
                            }
                        }
                    } else {
                        return Err(PatternMatchError::MatchFailed {
                            message: format!("Field '{}' not found in struct", field_name),
                            pattern_span: field_pattern.span,
                            value_span: None,
                        });
                    }
                }

                Ok(all_bindings)
            }
            _ => Err(PatternMatchError::TypeMismatch {
                expected: "Struct".to_string(),
                found: value.type_name().to_string(),
                pattern_span: pattern.span,
            }),
        }
    }

    /// Match a list pattern with support for rest patterns
    fn match_list_pattern(
        pattern: &ListPattern,
        value: &Value,
        context: &InterpreterContext,
    ) -> PatternMatchResult {
        match value {
            Value::List { list, .. } => {
                let elements = Self::list_to_vec(list);

                // Check minimum length requirement
                if elements.len() < pattern.elements.len() {
                    return Err(PatternMatchError::InsufficientElements {
                        min: pattern.elements.len(),
                        actual: elements.len(),
                        pattern_span: pattern.span,
                    });
                }

                let mut all_bindings = HashMap::new();

                // Match fixed elements
                for (i, pattern_elem) in pattern.elements.iter().enumerate() {
                    let bindings = Self::match_pattern(pattern_elem, &elements[i], context)?;
                    all_bindings.extend(bindings);
                }

                // Handle rest pattern if present
                if let Some(rest_identifier) = &pattern.rest {
                    let rest_elements = elements[pattern.elements.len()..].to_vec();
                    let rest_list = Self::vec_to_value_list(rest_elements);
                    all_bindings.insert(rest_identifier.name.clone(), rest_list);
                }

                Ok(all_bindings)
            }
            _ => Err(PatternMatchError::TypeMismatch {
                expected: "List".to_string(),
                found: value.type_name().to_string(),
                pattern_span: pattern.span,
            }),
        }
    }

    /// Convert a parser literal to an interpreter value
    fn literal_to_value(literal: &Literal) -> Value {
        match literal {
            Literal::Integer(int_lit) => Value::integer(int_lit.value),
            Literal::Float(float_lit) => Value::float(float_lit.value),
            Literal::Boolean(bool_lit) => Value::boolean(bool_lit.value),
            Literal::String(string_lit) => {
                // TODO: Handle string interpolation properly
                // For now, just extract basic string content
                let content = string_lit
                    .parts
                    .iter()
                    .filter_map(|part| match part {
                        outrun_parser::StringPart::Text { content, .. } => Some(content.clone()),
                        outrun_parser::StringPart::Interpolation { .. } => None, // Skip for now
                    })
                    .collect::<Vec<_>>()
                    .join("");
                Value::string(content)
            }
            Literal::Atom(atom_lit) => Value::atom(atom_lit.name.clone()),
        }
    }

    /// Convert a functional List to a Vec for easier processing
    fn list_to_vec(list: &List) -> Vec<Value> {
        let mut elements = Vec::new();
        let mut current = list;

        loop {
            match current {
                List::Empty => break,
                List::Cons { head, tail } => {
                    elements.push(head.clone());
                    current = tail;
                }
            }
        }

        elements
    }

    /// Convert a Vec of values back to a Value::List
    fn vec_to_value_list(elements: Vec<Value>) -> Value {
        if elements.is_empty() {
            return Value::empty_list();
        }

        let mut list = List::Empty;
        for element in elements.into_iter().rev() {
            list = List::Cons {
                head: element,
                tail: Rc::new(list),
            };
        }

        Value::List {
            list: Rc::new(list),
            element_type_info: None,
        }
    }

    /// Apply variable bindings to the interpreter context
    pub fn apply_bindings(
        bindings: HashMap<String, Value>,
        context: &mut InterpreterContext,
    ) -> Result<(), InterpreterError> {
        for (name, value) in bindings {
            context.define_variable(name, value)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;
    use outrun_parser::{Identifier, Span};

    fn test_span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn test_identifier_pattern_matching() {
        let pattern = Pattern::Identifier(Identifier {
            name: "x".to_string(),
            span: test_span(),
        });
        let value = Value::integer(42);
        let context = InterpreterContext::new();

        let result = PatternMatcher::match_pattern(&pattern, &value, &context).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result.get("x"), Some(&Value::integer(42)));
    }

    #[test]
    fn test_literal_pattern_matching_success() {
        let pattern = Pattern::Literal(LiteralPattern {
            literal: Literal::Integer(outrun_parser::IntegerLiteral {
                value: 42,
                format: outrun_parser::IntegerFormat::Decimal,
                raw_text: "42".to_string(),
                span: test_span(),
            }),
            span: test_span(),
        });
        let value = Value::integer(42);
        let context = InterpreterContext::new();

        let result = PatternMatcher::match_pattern(&pattern, &value, &context).unwrap();
        assert_eq!(result.len(), 0); // Literal patterns don't bind variables
    }

    #[test]
    fn test_literal_pattern_matching_failure() {
        let pattern = Pattern::Literal(LiteralPattern {
            literal: Literal::Integer(outrun_parser::IntegerLiteral {
                value: 42,
                format: outrun_parser::IntegerFormat::Decimal,
                raw_text: "42".to_string(),
                span: test_span(),
            }),
            span: test_span(),
        });
        let value = Value::integer(24);
        let context = InterpreterContext::new();

        let result = PatternMatcher::match_pattern(&pattern, &value, &context);
        assert!(result.is_err());
    }

    #[test]
    fn test_simple_list_pattern_matching() {
        // Create list pattern [x, y]
        let pattern = Pattern::List(ListPattern {
            elements: vec![
                Pattern::Identifier(Identifier {
                    name: "x".to_string(),
                    span: test_span(),
                }),
                Pattern::Identifier(Identifier {
                    name: "y".to_string(),
                    span: test_span(),
                }),
            ],
            rest: None,
            span: test_span(),
        });

        // Create value list [1, 2]
        let list = List::Cons {
            head: Value::integer(1),
            tail: Rc::new(List::Cons {
                head: Value::integer(2),
                tail: Rc::new(List::Empty),
            }),
        };
        let value = Value::List {
            list: Rc::new(list),
            element_type_info: None,
        };

        let context = InterpreterContext::new();
        let result = PatternMatcher::match_pattern(&pattern, &value, &context).unwrap();

        assert_eq!(result.len(), 2);
        assert_eq!(result.get("x"), Some(&Value::integer(1)));
        assert_eq!(result.get("y"), Some(&Value::integer(2)));
    }

    #[test]
    fn test_list_pattern_with_rest() {
        // Create list pattern [x, ..rest]
        let pattern = Pattern::List(ListPattern {
            elements: vec![Pattern::Identifier(Identifier {
                name: "x".to_string(),
                span: test_span(),
            })],
            rest: Some(Identifier {
                name: "rest".to_string(),
                span: test_span(),
            }),
            span: test_span(),
        });

        // Create value list [1, 2, 3]
        let list = List::Cons {
            head: Value::integer(1),
            tail: Rc::new(List::Cons {
                head: Value::integer(2),
                tail: Rc::new(List::Cons {
                    head: Value::integer(3),
                    tail: Rc::new(List::Empty),
                }),
            }),
        };
        let value = Value::List {
            list: Rc::new(list),
            element_type_info: None,
        };

        let context = InterpreterContext::new();
        let result = PatternMatcher::match_pattern(&pattern, &value, &context).unwrap();

        assert_eq!(result.len(), 2);
        assert_eq!(result.get("x"), Some(&Value::integer(1)));

        // Check that rest contains [2, 3]
        if let Some(Value::List {
            list: rest_list, ..
        }) = result.get("rest")
        {
            let rest_elements = PatternMatcher::list_to_vec(rest_list);
            assert_eq!(rest_elements.len(), 2);
            assert_eq!(rest_elements[0], Value::integer(2));
            assert_eq!(rest_elements[1], Value::integer(3));
        } else {
            panic!("Expected rest to be a list");
        }
    }
}
