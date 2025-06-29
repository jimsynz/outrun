//! Runtime pattern matching system for TypedPattern evaluation
//!
//! This module implements runtime pattern matching against runtime Values using
//! the TypedPattern structures from the typechecker. It handles:
//! - Literal pattern matching with type checking and proper atom support
//! - Tuple and struct destructuring patterns with AtomId field lookups
//! - List patterns with rest elements
//! - Variable binding extraction and scope management
//! - Full integration with CompilerEnvironment for atom management

use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::compilation::compiler_environment::{AtomId, CompilerEnvironment};
use outrun_typechecker::patterns::{
    BoundVariable, TypedLiteralPattern, TypedPattern, TypedPatternKind, TypedStructFieldPattern,
};
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur during pattern matching
#[derive(Debug, Error)]
pub enum PatternMatchError {
    #[error("Pattern match failed: value {value_type} does not match pattern")]
    MatchFailed {
        value_type: String,
        pattern_span: Span,
    },

    #[error("Type mismatch in literal pattern: expected {expected}, found {found}")]
    LiteralTypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Struct pattern field '{field}' not found in value")]
    StructFieldNotFound { field: String, span: Span },

    #[error("Tuple pattern length mismatch: expected {expected} elements, found {found}")]
    TupleLengthMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },

    #[error(
        "List pattern length mismatch: expected at least {min_expected} elements, found {found}"
    )]
    ListLengthMismatch {
        min_expected: usize,
        found: usize,
        span: Span,
    },

    #[error("Variable binding error: {message}")]
    VariableBinding { message: String, span: Span },

    #[error("Internal pattern matching error: {message}")]
    Internal { message: String, span: Span },
}

/// Result of a successful pattern match
#[derive(Debug, Clone)]
pub struct MatchResult {
    /// Variables bound by the pattern match
    pub bindings: HashMap<String, Value>,

    /// Whether the match was successful
    pub matched: bool,
}

impl MatchResult {
    /// Create a successful match with bindings
    pub fn success(bindings: HashMap<String, Value>) -> Self {
        Self {
            bindings,
            matched: true,
        }
    }

    /// Create a failed match
    pub fn failure() -> Self {
        Self {
            bindings: HashMap::new(),
            matched: false,
        }
    }

    /// Create an empty successful match (no bindings)
    pub fn empty_success() -> Self {
        Self {
            bindings: HashMap::new(),
            matched: true,
        }
    }

    /// Merge another match result into this one
    pub fn merge(&mut self, other: MatchResult) -> Result<(), PatternMatchError> {
        if !other.matched {
            self.matched = false;
            return Ok(());
        }

        for (name, value) in other.bindings {
            if self.bindings.contains_key(&name) {
                return Err(PatternMatchError::VariableBinding {
                    message: format!("Variable '{}' bound multiple times in pattern", name),
                    span: Span::new(0, 0), // TODO: Better span tracking
                });
            }
            self.bindings.insert(name, value);
        }

        Ok(())
    }
}

/// Runtime pattern matcher that evaluates TypedPattern against Value
pub struct PatternMatcher {
    /// Compiler environment for atom management and field lookups
    compiler_environment: CompilerEnvironment,
}

impl PatternMatcher {
    /// Create a new pattern matcher with its own CompilerEnvironment
    pub fn new() -> Self {
        Self {
            compiler_environment: CompilerEnvironment::new(),
        }
    }

    /// Create a pattern matcher with a shared CompilerEnvironment
    pub fn with_compiler_environment(compiler_environment: CompilerEnvironment) -> Self {
        Self {
            compiler_environment,
        }
    }

    /// Create a pattern matcher with relaxed type checking
    pub fn lenient() -> Self {
        Self {
            compiler_environment: CompilerEnvironment::new(),
        }
    }

    /// Get a reference to the compiler environment
    pub fn compiler_environment(&self) -> &CompilerEnvironment {
        &self.compiler_environment
    }

    /// Get a mutable reference to the compiler environment
    pub fn compiler_environment_mut(&mut self) -> &mut CompilerEnvironment {
        &mut self.compiler_environment
    }

    /// Match a value against a typed pattern
    pub fn match_pattern(
        &mut self,
        value: &Value,
        pattern: &TypedPattern,
    ) -> Result<MatchResult, PatternMatchError> {
        self.match_pattern_kind(value, &pattern.kind, pattern.span)
    }

    /// Match a value against a pattern kind
    fn match_pattern_kind(
        &mut self,
        value: &Value,
        pattern_kind: &TypedPatternKind,
        pattern_span: Span,
    ) -> Result<MatchResult, PatternMatchError> {
        match pattern_kind {
            TypedPatternKind::Identifier { name } => {
                // Identifier patterns always match and bind the entire value
                let mut bindings = HashMap::new();
                bindings.insert(name.clone(), value.clone());
                Ok(MatchResult::success(bindings))
            }

            TypedPatternKind::Literal { literal } => self.match_literal_pattern(value, literal),

            TypedPatternKind::Tuple { elements } => {
                self.match_tuple_pattern(value, elements, pattern_span)
            }

            TypedPatternKind::Struct { type_path, fields } => {
                self.match_struct_pattern(value, type_path, fields, pattern_span)
            }

            TypedPatternKind::List { elements, rest } => {
                self.match_list_pattern(value, elements, rest.as_ref(), pattern_span)
            }
        }
    }

    /// Match a literal pattern
    fn match_literal_pattern(
        &mut self,
        value: &Value,
        literal_pattern: &TypedLiteralPattern,
    ) -> Result<MatchResult, PatternMatchError> {
        // Convert the typed literal to a runtime value for comparison
        let pattern_value = self.literal_to_value(&literal_pattern.literal)?;

        if self.values_equal(value, &pattern_value) {
            Ok(MatchResult::empty_success())
        } else {
            Ok(MatchResult::failure())
        }
    }

    /// Match a tuple pattern
    fn match_tuple_pattern(
        &mut self,
        value: &Value,
        element_patterns: &[TypedPattern],
        pattern_span: Span,
    ) -> Result<MatchResult, PatternMatchError> {
        // Check if value is a tuple
        if let Some((tuple_elements, _tuple_type)) = value.as_tuple() {
            if tuple_elements.len() != element_patterns.len() {
                return Err(PatternMatchError::TupleLengthMismatch {
                    expected: element_patterns.len(),
                    found: tuple_elements.len(),
                    span: pattern_span,
                });
            }

            let mut result = MatchResult::empty_success();

            // Match each element
            for (value_elem, pattern_elem) in tuple_elements.iter().zip(element_patterns.iter()) {
                let elem_result = self.match_pattern(value_elem, pattern_elem)?;
                if !elem_result.matched {
                    return Ok(MatchResult::failure());
                }
                result.merge(elem_result)?;
            }

            Ok(result)
        } else {
            Err(PatternMatchError::MatchFailed {
                value_type: value.type_name().to_string(),
                pattern_span,
            })
        }
    }

    /// Match a struct pattern
    fn match_struct_pattern(
        &mut self,
        value: &Value,
        _type_path: &[String],
        field_patterns: &[TypedStructFieldPattern],
        pattern_span: Span,
    ) -> Result<MatchResult, PatternMatchError> {
        // Check if value is a struct
        if let Some((_struct_type_id, fields, _struct_type)) = value.as_struct() {
            // TODO: Verify struct type matches type_path
            // For now, we'll match any struct and check field availability

            let mut result = MatchResult::empty_success();

            // Match each field pattern
            for field_pattern in field_patterns {
                let field_result =
                    self.match_struct_field_pattern(fields, field_pattern, pattern_span)?;
                if !field_result.matched {
                    return Ok(MatchResult::failure());
                }
                result.merge(field_result)?;
            }

            Ok(result)
        } else {
            Err(PatternMatchError::MatchFailed {
                value_type: value.type_name().to_string(),
                pattern_span,
            })
        }
    }

    /// Match a struct field pattern
    fn match_struct_field_pattern(
        &mut self,
        fields: &HashMap<AtomId, Value>,
        field_pattern: &TypedStructFieldPattern,
        pattern_span: Span,
    ) -> Result<MatchResult, PatternMatchError> {
        // Convert field name to AtomId for lookup
        let field_atom = self
            .compiler_environment
            .intern_atom_name(&field_pattern.name);

        // Look up the field value in the struct
        let field_value =
            fields
                .get(&field_atom)
                .ok_or_else(|| PatternMatchError::StructFieldNotFound {
                    field: field_pattern.name.clone(),
                    span: pattern_span,
                })?;

        match &field_pattern.pattern {
            Some(pattern) => {
                // Field has an explicit pattern: {name: pattern}
                self.match_pattern(field_value, pattern)
            }
            None => {
                // Shorthand field pattern: {name} binds to variable 'name'
                let mut bindings = HashMap::new();
                bindings.insert(field_pattern.name.clone(), field_value.clone());
                Ok(MatchResult::success(bindings))
            }
        }
    }

    /// Match a list pattern
    fn match_list_pattern(
        &mut self,
        value: &Value,
        element_patterns: &[TypedPattern],
        rest_variable: Option<&BoundVariable>,
        pattern_span: Span,
    ) -> Result<MatchResult, PatternMatchError> {
        // Check if value is a list
        if let Some((list, _element_type)) = value.as_list() {
            let list_length = list.len();
            let min_required = element_patterns.len();

            if rest_variable.is_none() {
                // Exact length match required
                if list_length != min_required {
                    return Err(PatternMatchError::ListLengthMismatch {
                        min_expected: min_required,
                        found: list_length,
                        span: pattern_span,
                    });
                }
            } else {
                // Rest pattern allows for more elements
                if list_length < min_required {
                    return Err(PatternMatchError::ListLengthMismatch {
                        min_expected: min_required,
                        found: list_length,
                        span: pattern_span,
                    });
                }
            }

            let mut result = MatchResult::empty_success();
            let mut current_list = list;

            // Match the explicit element patterns
            for pattern in element_patterns {
                if let Some(head) = current_list.head() {
                    let elem_result = self.match_pattern(head, pattern)?;
                    if !elem_result.matched {
                        return Ok(MatchResult::failure());
                    }
                    result.merge(elem_result)?;

                    // Move to the tail for the next element
                    if let Some(tail) = current_list.tail() {
                        current_list = tail;
                    } else {
                        // This shouldn't happen given our length check above
                        break;
                    }
                } else {
                    // This shouldn't happen given our length check above
                    return Ok(MatchResult::failure());
                }
            }

            // Handle rest pattern if present
            if let Some(rest_var) = rest_variable {
                // Create a new list Value from the remaining elements
                let rest_value = if current_list.is_empty() {
                    // Empty list for the rest
                    Value::empty_list(value.as_list().unwrap().1.clone()) // Reuse element type
                } else {
                    // Create a new Value::List from the remaining elements
                    Value::from_list(current_list.clone(), value.as_list().unwrap().1.clone())
                };

                let mut rest_bindings = HashMap::new();
                rest_bindings.insert(rest_var.name.clone(), rest_value);
                result.merge(MatchResult::success(rest_bindings))?;
            }

            Ok(result)
        } else {
            Err(PatternMatchError::MatchFailed {
                value_type: value.type_name().to_string(),
                pattern_span,
            })
        }
    }

    /// Convert a typed literal to a runtime value
    fn literal_to_value(
        &mut self,
        literal: &outrun_parser::ast::Literal,
    ) -> Result<Value, PatternMatchError> {
        use outrun_parser::ast::Literal;

        match literal {
            Literal::Integer(int_lit) => Ok(Value::integer(int_lit.value)),
            Literal::Float(float_lit) => Ok(Value::float(float_lit.value)),
            Literal::String(string_lit) => {
                // Handle simple strings without interpolation
                if string_lit.parts.len() == 1 {
                    if let outrun_parser::ast::StringPart::Text { content, .. } =
                        &string_lit.parts[0]
                    {
                        Ok(Value::string(content.clone()))
                    } else {
                        Err(PatternMatchError::Internal {
                            message: "String interpolation in patterns not yet supported"
                                .to_string(),
                            span: Span::new(0, 0),
                        })
                    }
                } else {
                    Err(PatternMatchError::Internal {
                        message: "Complex string patterns with interpolation not yet supported"
                            .to_string(),
                        span: Span::new(0, 0),
                    })
                }
            }
            Literal::Boolean(bool_lit) => Ok(Value::boolean(bool_lit.value)),
            Literal::Atom(atom_lit) => {
                // Properly intern the atom name to get an AtomId
                let atom_id = self.compiler_environment.intern_atom_name(&atom_lit.name);
                Ok(Value::atom(atom_id))
            }
        }
    }

    /// Check if two values are equal for pattern matching purposes
    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        // Use Value's PartialEq implementation
        a == b
    }
}

impl Default for PatternMatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Utility functions for pattern matching in expressions and case statements
pub struct PatternMatchingUtils;

impl PatternMatchingUtils {
    /// Extract all variable names bound by a pattern
    pub fn extract_bound_variables(pattern: &TypedPattern) -> Vec<String> {
        let mut variables = Vec::new();
        Self::extract_variables_recursive(&pattern.kind, &mut variables);
        variables
    }

    /// Recursively extract variable names from pattern kinds
    fn extract_variables_recursive(pattern_kind: &TypedPatternKind, variables: &mut Vec<String>) {
        match pattern_kind {
            TypedPatternKind::Identifier { name } => {
                variables.push(name.clone());
            }
            TypedPatternKind::Literal { .. } => {
                // Literals don't bind variables
            }
            TypedPatternKind::Tuple { elements } => {
                for element in elements {
                    Self::extract_variables_recursive(&element.kind, variables);
                }
            }
            TypedPatternKind::Struct { fields, .. } => {
                for field in fields {
                    if let Some(pattern) = &field.pattern {
                        Self::extract_variables_recursive(&pattern.kind, variables);
                    } else {
                        // Shorthand field pattern binds the field name
                        variables.push(field.name.clone());
                    }
                }
            }
            TypedPatternKind::List { elements, rest } => {
                for element in elements {
                    Self::extract_variables_recursive(&element.kind, variables);
                }
                if let Some(rest_var) = rest {
                    variables.push(rest_var.name.clone());
                }
            }
        }
    }

    /// Check if a pattern is irrefutable (always matches)
    pub fn is_irrefutable_pattern(pattern: &TypedPattern) -> bool {
        Self::is_irrefutable_pattern_kind(&pattern.kind)
    }

    /// Check if a pattern kind is irrefutable
    fn is_irrefutable_pattern_kind(pattern_kind: &TypedPatternKind) -> bool {
        match pattern_kind {
            TypedPatternKind::Identifier { .. } => true, // Always matches
            TypedPatternKind::Literal { .. } => false,   // Can fail to match
            TypedPatternKind::Tuple { elements } => {
                // Tuple is irrefutable if all elements are irrefutable
                elements.iter().all(Self::is_irrefutable_pattern)
            }
            TypedPatternKind::Struct { fields, .. } => {
                // Struct pattern is irrefutable if all field patterns are irrefutable
                fields.iter().all(|field| {
                    field
                        .pattern
                        .as_ref()
                        .map(Self::is_irrefutable_pattern)
                        .unwrap_or(true) // Shorthand patterns are irrefutable
                })
            }
            TypedPatternKind::List { elements, rest } => {
                // List pattern is irrefutable only if it has a rest pattern and all elements are irrefutable
                rest.is_some() && elements.iter().all(Self::is_irrefutable_pattern)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::Span;
    use outrun_parser::ast::Literal;
    use outrun_typechecker::patterns::{BoundVariable, TypedLiteralPattern};
    use outrun_typechecker::unification::StructuredType;

    fn test_span() -> Span {
        Span::new(0, 10)
    }

    fn create_integer_literal_pattern(value: i64) -> TypedPattern {
        use outrun_parser::ast::{IntegerFormat, IntegerLiteral};

        TypedPattern {
            kind: TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Integer(IntegerLiteral {
                        value,
                        format: IntegerFormat::Decimal,
                        raw_text: value.to_string(),
                        span: test_span(),
                    }),
                    literal_type: None,
                    span: test_span(),
                },
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        }
    }

    fn create_identifier_pattern(name: &str) -> TypedPattern {
        TypedPattern {
            kind: TypedPatternKind::Identifier {
                name: name.to_string(),
            },
            pattern_type: None,
            bound_variables: vec![BoundVariable {
                name: name.to_string(),
                variable_type: None,
                span: test_span(),
            }],
            span: test_span(),
        }
    }

    #[test]
    fn test_identifier_pattern_matching() {
        let mut matcher = PatternMatcher::new();
        let pattern = create_identifier_pattern("x");
        let value = Value::integer(42);

        let result = matcher.match_pattern(&value, &pattern).unwrap();
        assert!(result.matched);
        assert_eq!(result.bindings.len(), 1);
        assert_eq!(result.bindings.get("x"), Some(&Value::integer(42)));
    }

    #[test]
    fn test_literal_pattern_matching_success() {
        let mut matcher = PatternMatcher::new();
        let pattern = create_integer_literal_pattern(42);
        let value = Value::integer(42);

        let result = matcher.match_pattern(&value, &pattern).unwrap();
        assert!(result.matched);
        assert!(result.bindings.is_empty()); // Literals don't bind variables
    }

    #[test]
    fn test_literal_pattern_matching_failure() {
        let mut matcher = PatternMatcher::new();
        let pattern = create_integer_literal_pattern(42);
        let value = Value::integer(24);

        let result = matcher.match_pattern(&value, &pattern).unwrap();
        assert!(!result.matched);
    }

    #[test]
    fn test_tuple_pattern_matching() {
        let mut matcher = PatternMatcher::new();

        // Create pattern (x, 42)
        let pattern = TypedPattern {
            kind: TypedPatternKind::Tuple {
                elements: vec![
                    create_identifier_pattern("x"),
                    create_integer_literal_pattern(42),
                ],
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        };

        // Create value (24, 42)
        let value = Value::tuple(vec![Value::integer(24), Value::integer(42)]);

        let result = matcher.match_pattern(&value, &pattern).unwrap();
        assert!(result.matched);
        assert_eq!(result.bindings.len(), 1);
        assert_eq!(result.bindings.get("x"), Some(&Value::integer(24)));
    }

    #[test]
    fn test_tuple_pattern_length_mismatch() {
        let mut matcher = PatternMatcher::new();

        // Create pattern (x, y)
        let pattern = TypedPattern {
            kind: TypedPatternKind::Tuple {
                elements: vec![
                    create_identifier_pattern("x"),
                    create_identifier_pattern("y"),
                ],
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        };

        // Create value (42,) - single element tuple
        let value = Value::tuple(vec![Value::integer(42)]);

        let result = matcher.match_pattern(&value, &pattern);
        assert!(matches!(
            result,
            Err(PatternMatchError::TupleLengthMismatch { .. })
        ));
    }

    #[test]
    fn test_list_pattern_matching_exact() {
        let mut matcher = PatternMatcher::new();

        // Create pattern [x, 42]
        let pattern = TypedPattern {
            kind: TypedPatternKind::List {
                elements: vec![
                    create_identifier_pattern("x"),
                    create_integer_literal_pattern(42),
                ],
                rest: None, // No rest pattern
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        };

        // Create value [24, 42]
        let mock_type = StructuredType::Simple(
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new()
                .intern_type_name("Integer"),
        );
        let value = Value::list_from_vec(vec![Value::integer(24), Value::integer(42)], mock_type);

        let result = matcher.match_pattern(&value, &pattern).unwrap();
        assert!(result.matched);
        assert_eq!(result.bindings.len(), 1);
        assert_eq!(result.bindings.get("x"), Some(&Value::integer(24)));
    }

    #[test]
    fn test_list_pattern_with_rest() {
        let mut matcher = PatternMatcher::new();

        // Create pattern [first, ..rest]
        let pattern = TypedPattern {
            kind: TypedPatternKind::List {
                elements: vec![create_identifier_pattern("first")],
                rest: Some(BoundVariable {
                    name: "rest".to_string(),
                    variable_type: None,
                    span: test_span(),
                }),
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        };

        // Create value [1, 2, 3]
        let mock_type = StructuredType::Simple(
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new()
                .intern_type_name("Integer"),
        );
        let value = Value::list_from_vec(
            vec![Value::integer(1), Value::integer(2), Value::integer(3)],
            mock_type,
        );

        let result = matcher.match_pattern(&value, &pattern).unwrap();
        assert!(result.matched);
        assert_eq!(result.bindings.len(), 2);
        assert_eq!(result.bindings.get("first"), Some(&Value::integer(1)));

        // Rest should be [2, 3]
        let rest_value = result.bindings.get("rest").unwrap();
        assert_eq!(rest_value.list_length(), Some(2));
    }

    #[test]
    fn test_extract_bound_variables() {
        // Pattern: (x, [first, ..rest])
        let pattern = TypedPattern {
            kind: TypedPatternKind::Tuple {
                elements: vec![
                    create_identifier_pattern("x"),
                    TypedPattern {
                        kind: TypedPatternKind::List {
                            elements: vec![create_identifier_pattern("first")],
                            rest: Some(BoundVariable {
                                name: "rest".to_string(),
                                variable_type: None,
                                span: test_span(),
                            }),
                        },
                        pattern_type: None,
                        bound_variables: vec![],
                        span: test_span(),
                    },
                ],
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        };

        let variables = PatternMatchingUtils::extract_bound_variables(&pattern);
        assert_eq!(variables.len(), 3);
        assert!(variables.contains(&"x".to_string()));
        assert!(variables.contains(&"first".to_string()));
        assert!(variables.contains(&"rest".to_string()));
    }

    #[test]
    fn test_irrefutable_patterns() {
        // Identifier patterns are irrefutable
        let id_pattern = create_identifier_pattern("x");
        assert!(PatternMatchingUtils::is_irrefutable_pattern(&id_pattern));

        // Literal patterns are refutable
        let lit_pattern = create_integer_literal_pattern(42);
        assert!(!PatternMatchingUtils::is_irrefutable_pattern(&lit_pattern));

        // Tuple of identifiers is irrefutable
        let tuple_pattern = TypedPattern {
            kind: TypedPatternKind::Tuple {
                elements: vec![
                    create_identifier_pattern("x"),
                    create_identifier_pattern("y"),
                ],
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        };
        assert!(PatternMatchingUtils::is_irrefutable_pattern(&tuple_pattern));

        // Tuple with literal is refutable
        let mixed_tuple_pattern = TypedPattern {
            kind: TypedPatternKind::Tuple {
                elements: vec![
                    create_identifier_pattern("x"),
                    create_integer_literal_pattern(42),
                ],
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        };
        assert!(!PatternMatchingUtils::is_irrefutable_pattern(
            &mixed_tuple_pattern
        ));
    }

    #[test]
    fn test_match_result_merging() {
        let mut result1 = MatchResult::success({
            let mut bindings = HashMap::new();
            bindings.insert("x".to_string(), Value::integer(1));
            bindings
        });

        let result2 = MatchResult::success({
            let mut bindings = HashMap::new();
            bindings.insert("y".to_string(), Value::integer(2));
            bindings
        });

        result1.merge(result2).unwrap();
        assert!(result1.matched);
        assert_eq!(result1.bindings.len(), 2);
        assert_eq!(result1.bindings.get("x"), Some(&Value::integer(1)));
        assert_eq!(result1.bindings.get("y"), Some(&Value::integer(2)));
    }

    #[test]
    fn test_atom_pattern_matching() {
        use outrun_parser::ast::{AtomFormat, AtomLiteral, Literal};

        let mut matcher = PatternMatcher::new();

        // Create an atom pattern for :ok
        let pattern = TypedPattern {
            kind: TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Atom(AtomLiteral {
                        name: "ok".to_string(),
                        content: "ok".to_string(),
                        raw_content: "ok".to_string(),
                        format: AtomFormat::Simple,
                        span: test_span(),
                    }),
                    literal_type: None,
                    span: test_span(),
                },
            },
            pattern_type: None,
            bound_variables: vec![],
            span: test_span(),
        };

        // Create an atom value :ok
        let atom_id = matcher.compiler_environment.intern_atom_name("ok");
        let value = Value::atom(atom_id);

        let result = matcher.match_pattern(&value, &pattern).unwrap();
        assert!(result.matched);
        assert!(result.bindings.is_empty()); // Literal patterns don't bind variables

        // Test that different atoms don't match
        let different_atom_id = matcher.compiler_environment.intern_atom_name("error");
        let different_value = Value::atom(different_atom_id);

        let result2 = matcher.match_pattern(&different_value, &pattern).unwrap();
        assert!(!result2.matched);
    }

    #[test]
    fn test_match_result_failure_propagation() {
        let mut result1 = MatchResult::success({
            let mut bindings = HashMap::new();
            bindings.insert("x".to_string(), Value::integer(1));
            bindings
        });

        let result2 = MatchResult::failure();

        result1.merge(result2).unwrap();
        assert!(!result1.matched);
    }
}
