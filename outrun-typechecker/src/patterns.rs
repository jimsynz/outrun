//! Typed pattern system for comprehensive pattern matching with type validation
//!
//! This module provides typed versions of all pattern types from the parser,
//! with additional type information and variable binding tracking for the
//! type checker and typed AST.

use crate::error::{SpanExt, TypeError};
use crate::types::TypeId;
use crate::unification::{unify_structured_types, StructuredType, UnificationContext};
use outrun_parser::{
    ListPattern, Literal, LiteralPattern, Pattern, Span, StructDefinition, StructFieldPattern,
    StructPattern, TuplePattern, TypeAnnotation,
};
use std::collections::HashMap;

/// A typed pattern with resolved type information and bound variables
#[derive(Debug, Clone, PartialEq)]
pub struct TypedPattern {
    pub kind: TypedPatternKind,
    pub pattern_type: Option<StructuredType>, // Type this pattern matches against
    pub bound_variables: Vec<BoundVariable>,  // Variables bound by this pattern
    pub span: Span,
}

/// The different kinds of typed patterns
#[derive(Debug, Clone, PartialEq)]
pub enum TypedPatternKind {
    /// Identifier pattern: `x`, `name`, `result`
    /// Binds a value to a variable name
    Identifier { name: String },

    /// Literal pattern: `42`, `"hello"`, `:ok`, `true`
    /// Matches exact literal values
    Literal { literal: TypedLiteralPattern },

    /// Tuple pattern: `(x, y)`, `(first, second, third)`, `(User { name }, [a, b])`
    /// Destructures tuple values with recursive pattern matching
    Tuple { elements: Vec<TypedPattern> },

    /// Struct pattern: `User { name, email }`, `Result.Ok { value }`
    /// Destructures struct values by field name
    Struct {
        type_path: Vec<String>, // Module path: ["Result", "Ok"], ["User"]
        fields: Vec<TypedStructFieldPattern>,
    },

    /// List pattern: `[first, second]`, `[head, ..tail]`, `[1, name, :status]`
    /// Destructures list values with optional rest patterns
    List {
        elements: Vec<TypedPattern>,
        rest: Option<BoundVariable>, // Rest pattern: ..remaining
    },
}

/// A literal pattern with type information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedLiteralPattern {
    pub literal: Literal,
    pub literal_type: Option<StructuredType>, // Type of the literal
    pub span: Span,
}

/// A struct field pattern with type validation
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructFieldPattern {
    pub name: String,
    pub pattern: Option<TypedPattern>, // None = shorthand {name}, Some = {name: pattern}
    pub field_type: Option<StructuredType>, // Expected field type from struct definition
    pub span: Span,
}

/// A variable bound by a pattern with its type information
#[derive(Debug, Clone, PartialEq)]
pub struct BoundVariable {
    pub name: String,
    pub variable_type: Option<StructuredType>, // Type of the bound variable
    pub span: Span,
}

impl TypedPattern {
    /// Create a new typed pattern
    pub fn new(kind: TypedPatternKind, pattern_type: Option<StructuredType>, span: Span) -> Self {
        let bound_variables = Self::collect_bound_variables(&kind);

        Self {
            kind,
            pattern_type,
            bound_variables,
            span,
        }
    }

    /// Collect all variables bound by this pattern and its nested patterns
    fn collect_bound_variables(kind: &TypedPatternKind) -> Vec<BoundVariable> {
        let mut variables = Vec::new();
        Self::collect_bound_variables_recursive(kind, &mut variables);
        variables
    }

    /// Recursively collect bound variables from nested patterns
    fn collect_bound_variables_recursive(
        kind: &TypedPatternKind,
        variables: &mut Vec<BoundVariable>,
    ) {
        match kind {
            TypedPatternKind::Identifier { name } => {
                variables.push(BoundVariable {
                    name: name.clone(),
                    variable_type: None, // Will be filled in during type checking
                    span: Span::new(0, 0), // Placeholder span - actual spans set during pattern checking
                });
            }

            TypedPatternKind::Literal { .. } => {
                // Literal patterns don't bind variables
            }

            TypedPatternKind::Tuple { elements } => {
                for element in elements {
                    Self::collect_bound_variables_recursive(&element.kind, variables);
                }
            }

            TypedPatternKind::Struct { fields, .. } => {
                for field in fields {
                    match &field.pattern {
                        Some(pattern) => {
                            Self::collect_bound_variables_recursive(&pattern.kind, variables);
                        }
                        None => {
                            // Shorthand field - binds variable with field name
                            variables.push(BoundVariable {
                                name: field.name.clone(),
                                variable_type: field.field_type.clone(),
                                span: field.span,
                            });
                        }
                    }
                }
            }

            TypedPatternKind::List { elements, rest } => {
                for element in elements {
                    Self::collect_bound_variables_recursive(&element.kind, variables);
                }

                if let Some(rest_var) = rest {
                    variables.push(rest_var.clone());
                }
            }
        }
    }

    /// Get all bound variable names for scope registration
    pub fn bound_variable_names(&self) -> Vec<String> {
        self.bound_variables
            .iter()
            .map(|var| var.name.clone())
            .collect()
    }

    /// Check if this pattern binds a specific variable name
    pub fn binds_variable(&self, name: &str) -> bool {
        self.bound_variables.iter().any(|var| var.name == name)
    }

    /// Get the type of a specific bound variable
    pub fn get_variable_type(&self, name: &str) -> Option<&StructuredType> {
        self.bound_variables
            .iter()
            .find(|var| var.name == name)
            .and_then(|var| var.variable_type.as_ref())
    }
}

impl TypedStructFieldPattern {
    /// Create a shorthand field pattern: {name}
    pub fn shorthand(name: String, field_type: Option<StructuredType>, span: Span) -> Self {
        Self {
            name,
            pattern: None,
            field_type,
            span,
        }
    }

    /// Create an explicit field pattern: {name: pattern}
    pub fn explicit(
        name: String,
        pattern: TypedPattern,
        field_type: Option<StructuredType>,
        span: Span,
    ) -> Self {
        Self {
            name,
            pattern: Some(pattern),
            field_type,
            span,
        }
    }

    /// Check if this is a shorthand field pattern
    pub fn is_shorthand(&self) -> bool {
        self.pattern.is_none()
    }
}

impl BoundVariable {
    /// Create a new bound variable
    pub fn new(name: String, variable_type: Option<StructuredType>, span: Span) -> Self {
        Self {
            name,
            variable_type,
            span,
        }
    }
}

/// Pattern checker for converting parser patterns to typed patterns with validation
pub struct PatternChecker<'a> {
    context: &'a mut UnificationContext,
    struct_registry: &'a HashMap<TypeId, StructDefinition>,
}

impl<'a> PatternChecker<'a> {
    /// Create a new pattern checker with the given type context and struct registry
    pub fn new(
        context: &'a mut UnificationContext,
        struct_registry: &'a HashMap<TypeId, StructDefinition>,
    ) -> Self {
        Self {
            context,
            struct_registry,
        }
    }

    /// Check a pattern against a target type and return typed pattern with bound variables
    pub fn check_pattern(
        &mut self,
        pattern: &Pattern,
        target_type: &Option<StructuredType>,
    ) -> Result<TypedPattern, TypeError> {
        let (typed_kind, span) = match pattern {
            Pattern::Identifier(identifier) => {
                let typed_kind = TypedPatternKind::Identifier {
                    name: identifier.name.clone(),
                };
                (typed_kind, identifier.span)
            }

            Pattern::Literal(literal_pattern) => {
                let typed_literal = self.check_literal_pattern(literal_pattern, target_type)?;
                let typed_kind = TypedPatternKind::Literal {
                    literal: typed_literal,
                };
                (typed_kind, literal_pattern.span)
            }

            Pattern::Tuple(tuple_pattern) => {
                let typed_elements = self.check_tuple_pattern(tuple_pattern, target_type)?;
                let typed_kind = TypedPatternKind::Tuple {
                    elements: typed_elements,
                };
                (typed_kind, tuple_pattern.span)
            }

            Pattern::Struct(struct_pattern) => {
                let (type_path, typed_fields) =
                    self.check_struct_pattern(struct_pattern, target_type)?;
                let typed_kind = TypedPatternKind::Struct {
                    type_path,
                    fields: typed_fields,
                };
                (typed_kind, struct_pattern.span)
            }

            Pattern::List(list_pattern) => {
                let (typed_elements, rest) = self.check_list_pattern(list_pattern, target_type)?;
                let typed_kind = TypedPatternKind::List {
                    elements: typed_elements,
                    rest,
                };
                (typed_kind, list_pattern.span)
            }
        };

        Ok(TypedPattern::new(typed_kind, target_type.clone(), span))
    }

    /// Check a literal pattern against target type
    fn check_literal_pattern(
        &mut self,
        literal_pattern: &LiteralPattern,
        target_type: &Option<StructuredType>,
    ) -> Result<TypedLiteralPattern, TypeError> {
        // Validate literal type against target type
        let literal_type = self.infer_literal_type(&literal_pattern.literal);

        // Check if literal_type unifies with target_type
        if let (Some(literal_type), Some(target_type)) = (&literal_type, target_type) {
            if !unify_structured_types(literal_type, target_type, self.context).unwrap_or(false) {
                return Err(TypeError::internal(format!(
                    "Literal pattern type {} does not match target type {}",
                    literal_type.to_string_representation(&self.context.type_interner),
                    target_type.to_string_representation(&self.context.type_interner)
                )));
            }
        }

        Ok(TypedLiteralPattern {
            literal: literal_pattern.literal.clone(),
            literal_type,
            span: literal_pattern.span,
        })
    }

    /// Check a tuple pattern and validate element types
    fn check_tuple_pattern(
        &mut self,
        tuple_pattern: &TuplePattern,
        _target_type: &Option<StructuredType>,
    ) -> Result<Vec<TypedPattern>, TypeError> {
        let mut typed_elements = Vec::new();

        // Element type extraction requires tuple type decomposition
        // Currently check each element against None type for structural validation
        for element in &tuple_pattern.elements {
            let typed_element = self.check_pattern(element, &None)?;
            typed_elements.push(typed_element);
        }

        Ok(typed_elements)
    }

    /// Check a struct pattern and validate field types
    fn check_struct_pattern(
        &mut self,
        struct_pattern: &StructPattern,
        _target_type: &Option<StructuredType>,
    ) -> Result<(Vec<String>, Vec<TypedStructFieldPattern>), TypeError> {
        // Convert type path to strings
        let type_path: Vec<String> = struct_pattern
            .type_path
            .iter()
            .map(|type_id| type_id.name.clone())
            .collect();

        // Look up struct definition to get field types
        let struct_name = type_path.join(".");
        let struct_type_id = self.context.type_interner.get_type(&struct_name);
        let struct_definition = struct_type_id.and_then(|id| self.struct_registry.get(&id));

        let mut typed_fields = Vec::new();

        for field in &struct_pattern.fields {
            let typed_field = self.check_struct_field_pattern(field, struct_definition)?;
            typed_fields.push(typed_field);
        }

        Ok((type_path, typed_fields))
    }

    /// Check a struct field pattern
    fn check_struct_field_pattern(
        &mut self,
        field_pattern: &StructFieldPattern,
        struct_definition: Option<&StructDefinition>,
    ) -> Result<TypedStructFieldPattern, TypeError> {
        // Look up field type from struct definition
        let field_type = self.get_field_type_from_struct(
            &field_pattern.name.name,
            struct_definition,
            field_pattern.span,
        )?;

        match &field_pattern.pattern {
            Some(pattern) => {
                // Explicit field: {name: pattern}
                let typed_pattern = self.check_pattern(pattern, &field_type)?;
                Ok(TypedStructFieldPattern::explicit(
                    field_pattern.name.name.clone(),
                    typed_pattern,
                    field_type,
                    field_pattern.span,
                ))
            }
            None => {
                // Shorthand field: {name}
                Ok(TypedStructFieldPattern::shorthand(
                    field_pattern.name.name.clone(),
                    field_type,
                    field_pattern.span,
                ))
            }
        }
    }

    /// Get field type from struct definition, handling unknown structs and invalid fields
    fn get_field_type_from_struct(
        &mut self,
        field_name: &str,
        struct_definition: Option<&StructDefinition>,
        field_span: Span,
    ) -> Result<Option<StructuredType>, TypeError> {
        match struct_definition {
            Some(struct_def) => {
                // Find the field in the struct definition
                let field_def = struct_def
                    .fields
                    .iter()
                    .find(|field| field.name.name == field_name);

                match field_def {
                    Some(field) => {
                        // Convert field type annotation to StructuredType
                        let field_type = self.resolve_type_annotation(&field.type_annotation)?;
                        Ok(Some(field_type))
                    }
                    None => {
                        // Field not found in struct definition
                        Err(TypeError::undefined_field(
                            field_name.to_string(),
                            struct_def.name_as_string(),
                            field_span.to_source_span(),
                        ))
                    }
                }
            }
            None => {
                // No struct definition found - could be unknown struct or built-in type
                // For now, return None to allow pattern checking to continue
                // In a complete implementation, this might be an error for unknown structs
                Ok(None)
            }
        }
    }

    /// Simple type annotation resolution for struct field types
    /// This is a simplified version that handles basic cases without Self resolution
    fn resolve_type_annotation(
        &mut self,
        type_annotation: &TypeAnnotation,
    ) -> Result<StructuredType, TypeError> {
        match type_annotation {
            TypeAnnotation::Simple {
                path,
                generic_args,
                span: _,
            } => {
                // Convert type path to string
                let type_name = path
                    .iter()
                    .map(|id| id.name.clone())
                    .collect::<Vec<_>>()
                    .join(".");

                // Intern the type name
                let type_id = self.context.type_interner.intern_type(&type_name);

                // Handle generic arguments if present
                if let Some(args) = generic_args {
                    let mut structured_args = Vec::new();
                    for arg in &args.args {
                        let arg_type = self.resolve_type_annotation(arg)?;
                        structured_args.push(arg_type);
                    }
                    Ok(StructuredType::Generic {
                        base: type_id,
                        args: structured_args,
                    })
                } else {
                    Ok(StructuredType::Simple(type_id))
                }
            }
            TypeAnnotation::Tuple { types, span: _ } => {
                let mut element_types = Vec::new();
                for element in types {
                    let element_type = self.resolve_type_annotation(element)?;
                    element_types.push(element_type);
                }
                Ok(StructuredType::Tuple(element_types))
            }
            TypeAnnotation::Function {
                params,
                return_type,
                span: _,
            } => {
                let mut param_types = Vec::new();
                for param in params {
                    let param_name = self.context.type_interner.intern_atom(&param.name.name);
                    let param_type = self.resolve_type_annotation(&param.type_annotation)?;
                    param_types.push(crate::unification::FunctionParam {
                        name: param_name,
                        param_type,
                    });
                }
                let return_structured_type = self.resolve_type_annotation(return_type)?;
                Ok(StructuredType::Function {
                    params: param_types,
                    return_type: Box::new(return_structured_type),
                })
            }
        }
    }

    /// Check a list pattern and validate element types
    fn check_list_pattern(
        &mut self,
        list_pattern: &ListPattern,
        _target_type: &Option<StructuredType>,
    ) -> Result<(Vec<TypedPattern>, Option<BoundVariable>), TypeError> {
        let mut typed_elements = Vec::new();

        // Element type extraction requires list type decomposition
        // Currently check each element against None type for structural validation
        for element in &list_pattern.elements {
            let typed_element = self.check_pattern(element, &None)?;
            typed_elements.push(typed_element);
        }

        // Handle rest pattern
        let rest = list_pattern.rest.as_ref().map(|rest_identifier| {
            BoundVariable::new(
                rest_identifier.name.clone(),
                None, // Rest type inference requires list element type analysis
                rest_identifier.span,
            )
        });

        Ok((typed_elements, rest))
    }

    /// Infer the structured type of a literal
    fn infer_literal_type(&mut self, literal: &Literal) -> Option<StructuredType> {
        match literal {
            Literal::Boolean(_) => {
                let boolean_type = self.context.type_interner.intern_type("Boolean");
                Some(StructuredType::Simple(boolean_type))
            }
            Literal::Integer(_) => {
                let integer_type = self.context.type_interner.intern_type("Integer");
                Some(StructuredType::Simple(integer_type))
            }
            Literal::Float(_) => {
                let float_type = self.context.type_interner.intern_type("Float");
                Some(StructuredType::Simple(float_type))
            }
            Literal::String(_) => {
                let string_type = self.context.type_interner.intern_type("String");
                Some(StructuredType::Simple(string_type))
            }
            Literal::Atom(_) => {
                let atom_type = self.context.type_interner.intern_type("Atom");
                Some(StructuredType::Simple(atom_type))
            }
        }
    }

    /// Get all bound variables from a typed pattern for scope registration
    pub fn collect_bound_variables(pattern: &TypedPattern) -> Vec<BoundVariable> {
        pattern.bound_variables.clone()
    }
}

/// Exhaustiveness checking utilities for pattern matching
pub struct ExhaustivenessChecker {
    #[allow(dead_code)] // TODO: Will be used when exhaustiveness checking is implemented
    context: UnificationContext,
}

impl ExhaustivenessChecker {
    /// Create a new exhaustiveness checker
    pub fn new(context: UnificationContext) -> Self {
        Self { context }
    }

    /// Check if a set of patterns is exhaustive for a given type
    /// TODO: Implement full exhaustiveness checking algorithm
    pub fn is_exhaustive(
        &self,
        patterns: &[TypedPattern],
        target_type: &StructuredType,
    ) -> Result<bool, TypeError> {
        // Placeholder implementation - for now, always consider exhaustive
        // TODO: Implement proper exhaustiveness checking based on:
        // 1. Pattern coverage analysis
        // 2. Type-based completeness checking
        // 3. Reachability analysis for overlapping patterns

        let _ = (patterns, target_type); // Silence unused warnings
        Ok(true)
    }

    /// Find missing patterns that would make the match exhaustive
    /// TODO: Implement missing pattern generation
    pub fn find_missing_patterns(
        &self,
        patterns: &[TypedPattern],
        target_type: &StructuredType,
    ) -> Result<Vec<TypedPattern>, TypeError> {
        // Placeholder implementation
        // TODO: Generate minimal set of patterns that would complete the match

        let _ = (patterns, target_type); // Silence unused warnings
        Ok(Vec::new())
    }

    /// Check if patterns have any unreachable cases (overlapping patterns)
    /// TODO: Implement reachability analysis
    pub fn find_unreachable_patterns(
        &self,
        patterns: &[TypedPattern],
    ) -> Result<Vec<usize>, TypeError> {
        // Placeholder implementation
        // TODO: Detect patterns that are shadowed by earlier patterns

        let _ = patterns; // Silence unused warnings
        Ok(Vec::new())
    }
}

/// Pattern utility functions for analysis
impl TypedPattern {
    /// Check if this pattern is more specific than another pattern
    /// Used for reachability analysis
    pub fn is_more_specific_than(&self, other: &TypedPattern) -> bool {
        match (&self.kind, &other.kind) {
            // Literal patterns are more specific than identifier patterns
            (TypedPatternKind::Literal { .. }, TypedPatternKind::Identifier { .. }) => true,

            // Struct patterns are more specific than identifier patterns
            (TypedPatternKind::Struct { .. }, TypedPatternKind::Identifier { .. }) => true,

            // TODO: Add more specificity rules for comprehensive analysis
            _ => false,
        }
    }

    /// Check if two patterns overlap (could match the same value)
    /// Used for reachability analysis
    pub fn overlaps_with(&self, other: &TypedPattern) -> bool {
        match (&self.kind, &other.kind) {
            // Identical literal patterns overlap
            (
                TypedPatternKind::Literal { literal: lit1 },
                TypedPatternKind::Literal { literal: lit2 },
            ) => lit1.literal == lit2.literal,

            // Identifier patterns overlap with everything
            (TypedPatternKind::Identifier { .. }, _) => true,
            (_, TypedPatternKind::Identifier { .. }) => true,

            // TODO: Add more overlap checking for comprehensive analysis
            _ => false,
        }
    }

    /// Get the constructor of this pattern for exhaustiveness checking
    /// Returns None for variable patterns, Some for structured patterns
    pub fn get_constructor(&self) -> Option<PatternConstructor> {
        match &self.kind {
            TypedPatternKind::Literal { literal } => {
                Some(PatternConstructor::Literal(literal.literal.clone()))
            }
            TypedPatternKind::Struct { type_path, .. } => {
                Some(PatternConstructor::Struct(type_path.clone()))
            }
            TypedPatternKind::Tuple { elements } => Some(PatternConstructor::Tuple(elements.len())),
            TypedPatternKind::List { elements, rest } => Some(PatternConstructor::List {
                min_length: elements.len(),
                has_rest: rest.is_some(),
            }),
            TypedPatternKind::Identifier { .. } => None, // Variable pattern - no constructor
        }
    }
}

/// Pattern constructor for exhaustiveness checking
#[derive(Debug, Clone, PartialEq)]
pub enum PatternConstructor {
    Literal(Literal),
    Struct(Vec<String>), // Type path
    Tuple(usize),        // Number of elements
    List { min_length: usize, has_rest: bool },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier_pattern_binds_variable() {
        let pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "x".to_string(),
            },
            None,
            Span::new(0, 0),
        );

        assert_eq!(pattern.bound_variable_names(), vec!["x"]);
        assert!(pattern.binds_variable("x"));
        assert!(!pattern.binds_variable("y"));
    }

    #[test]
    fn test_literal_pattern_binds_no_variables() {
        let pattern = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Integer(outrun_parser::IntegerLiteral {
                        value: 42,
                        format: outrun_parser::IntegerFormat::Decimal,
                        raw_text: "42".to_string(),
                        span: Span::new(0, 0),
                    }),
                    literal_type: None,
                    span: Span::new(0, 0),
                },
            },
            None,
            Span::new(0, 0),
        );

        assert!(pattern.bound_variable_names().is_empty());
        assert!(!pattern.binds_variable("x"));
    }

    #[test]
    fn test_tuple_pattern_collects_nested_variables() {
        let pattern = TypedPattern::new(
            TypedPatternKind::Tuple {
                elements: vec![
                    TypedPattern::new(
                        TypedPatternKind::Identifier {
                            name: "x".to_string(),
                        },
                        None,
                        Span::new(0, 0),
                    ),
                    TypedPattern::new(
                        TypedPatternKind::Identifier {
                            name: "y".to_string(),
                        },
                        None,
                        Span::new(0, 0),
                    ),
                ],
            },
            None,
            Span::new(0, 0),
        );

        let mut names = pattern.bound_variable_names();
        names.sort();
        assert_eq!(names, vec!["x", "y"]);
    }

    #[test]
    fn test_struct_shorthand_field_binds_variable() {
        let field = TypedStructFieldPattern::shorthand("name".to_string(), None, Span::new(0, 0));

        assert!(field.is_shorthand());

        let pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["User".to_string()],
                fields: vec![field],
            },
            None,
            Span::new(0, 0),
        );

        assert_eq!(pattern.bound_variable_names(), vec!["name"]);
    }

    #[test]
    fn test_nested_pattern_destructuring() {
        // Test complex nested pattern: (User { name, address: Address { city } }, [first, ..rest])

        // Create the Address { city } pattern
        let address_field =
            TypedStructFieldPattern::shorthand("city".to_string(), None, Span::new(0, 0));
        let address_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["Address".to_string()],
                fields: vec![address_field],
            },
            None,
            Span::new(0, 0),
        );

        // Create the User { name, address: Address { city } } pattern
        let name_field =
            TypedStructFieldPattern::shorthand("name".to_string(), None, Span::new(0, 0));
        let address_field_explicit = TypedStructFieldPattern::explicit(
            "address".to_string(),
            address_pattern,
            None,
            Span::new(0, 0),
        );
        let user_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["User".to_string()],
                fields: vec![name_field, address_field_explicit],
            },
            None,
            Span::new(0, 0),
        );

        // Create the [first, ..rest] pattern
        let first_pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "first".to_string(),
            },
            None,
            Span::new(0, 0),
        );
        let rest_var = BoundVariable::new("rest".to_string(), None, Span::new(0, 0));
        let list_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![first_pattern],
                rest: Some(rest_var),
            },
            None,
            Span::new(0, 0),
        );

        // Create the tuple pattern containing both
        let tuple_pattern = TypedPattern::new(
            TypedPatternKind::Tuple {
                elements: vec![user_pattern, list_pattern],
            },
            None,
            Span::new(0, 0),
        );

        // Check that all nested variables are collected
        let bound_names = tuple_pattern.bound_variable_names();
        assert_eq!(bound_names.len(), 4);
        assert!(bound_names.contains(&"name".to_string()));
        assert!(bound_names.contains(&"city".to_string()));
        assert!(bound_names.contains(&"first".to_string()));
        assert!(bound_names.contains(&"rest".to_string()));
    }

    #[test]
    fn test_complex_list_pattern_with_nested_structures() {
        // Test pattern: [Point { x, y }, Point { x: px, y: py }, ..remaining]

        // Create Point { x, y } pattern
        let point1_fields = vec![
            TypedStructFieldPattern::shorthand("x".to_string(), None, Span::new(0, 0)),
            TypedStructFieldPattern::shorthand("y".to_string(), None, Span::new(0, 0)),
        ];
        let point1_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["Point".to_string()],
                fields: point1_fields,
            },
            None,
            Span::new(0, 0),
        );

        // Create Point { x: px, y: py } pattern with explicit field patterns
        let px_pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "px".to_string(),
            },
            None,
            Span::new(0, 0),
        );
        let py_pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "py".to_string(),
            },
            None,
            Span::new(0, 0),
        );
        let point2_fields = vec![
            TypedStructFieldPattern::explicit("x".to_string(), px_pattern, None, Span::new(0, 0)),
            TypedStructFieldPattern::explicit("y".to_string(), py_pattern, None, Span::new(0, 0)),
        ];
        let point2_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["Point".to_string()],
                fields: point2_fields,
            },
            None,
            Span::new(0, 0),
        );

        // Create the list pattern
        let remaining_var = BoundVariable::new("remaining".to_string(), None, Span::new(0, 0));
        let list_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![point1_pattern, point2_pattern],
                rest: Some(remaining_var),
            },
            None,
            Span::new(0, 0),
        );

        // Check bound variables
        let bound_names = list_pattern.bound_variable_names();
        assert_eq!(bound_names.len(), 5);
        assert!(bound_names.contains(&"x".to_string()));
        assert!(bound_names.contains(&"y".to_string()));
        assert!(bound_names.contains(&"px".to_string()));
        assert!(bound_names.contains(&"py".to_string()));
        assert!(bound_names.contains(&"remaining".to_string()));
    }

    #[test]
    fn test_pattern_constructor_extraction() {
        // Test that pattern constructors are correctly extracted for exhaustiveness checking

        // Literal pattern
        let literal_pattern = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Integer(outrun_parser::IntegerLiteral {
                        value: 42,
                        format: outrun_parser::IntegerFormat::Decimal,
                        raw_text: "42".to_string(),
                        span: Span::new(0, 0),
                    }),
                    literal_type: None,
                    span: Span::new(0, 0),
                },
            },
            None,
            Span::new(0, 0),
        );

        match literal_pattern.get_constructor() {
            Some(PatternConstructor::Literal(Literal::Integer(lit))) => {
                assert_eq!(lit.value, 42);
            }
            _ => panic!("Expected literal constructor"),
        }

        // Struct pattern
        let struct_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["User".to_string()],
                fields: vec![],
            },
            None,
            Span::new(0, 0),
        );

        match struct_pattern.get_constructor() {
            Some(PatternConstructor::Struct(path)) => {
                assert_eq!(path, vec!["User"]);
            }
            _ => panic!("Expected struct constructor"),
        }

        // Tuple pattern
        let tuple_pattern = TypedPattern::new(
            TypedPatternKind::Tuple {
                elements: vec![
                    TypedPattern::new(
                        TypedPatternKind::Identifier {
                            name: "x".to_string(),
                        },
                        None,
                        Span::new(0, 0),
                    ),
                    TypedPattern::new(
                        TypedPatternKind::Identifier {
                            name: "y".to_string(),
                        },
                        None,
                        Span::new(0, 0),
                    ),
                ],
            },
            None,
            Span::new(0, 0),
        );

        match tuple_pattern.get_constructor() {
            Some(PatternConstructor::Tuple(arity)) => {
                assert_eq!(arity, 2);
            }
            _ => panic!("Expected tuple constructor"),
        }

        // List pattern with rest
        let list_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![TypedPattern::new(
                    TypedPatternKind::Identifier {
                        name: "first".to_string(),
                    },
                    None,
                    Span::new(0, 0),
                )],
                rest: Some(BoundVariable::new(
                    "rest".to_string(),
                    None,
                    Span::new(0, 0),
                )),
            },
            None,
            Span::new(0, 0),
        );

        match list_pattern.get_constructor() {
            Some(PatternConstructor::List {
                min_length,
                has_rest,
            }) => {
                assert_eq!(min_length, 1);
                assert!(has_rest);
            }
            _ => panic!("Expected list constructor"),
        }

        // Identifier pattern - no constructor
        let identifier_pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "x".to_string(),
            },
            None,
            Span::new(0, 0),
        );

        assert!(identifier_pattern.get_constructor().is_none());
    }

    #[test]
    fn test_pattern_specificity() {
        // Test pattern specificity for reachability analysis

        let identifier_pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "x".to_string(),
            },
            None,
            Span::new(0, 0),
        );

        let literal_pattern = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Integer(outrun_parser::IntegerLiteral {
                        value: 42,
                        format: outrun_parser::IntegerFormat::Decimal,
                        raw_text: "42".to_string(),
                        span: Span::new(0, 0),
                    }),
                    literal_type: None,
                    span: Span::new(0, 0),
                },
            },
            None,
            Span::new(0, 0),
        );

        let struct_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["User".to_string()],
                fields: vec![],
            },
            None,
            Span::new(0, 0),
        );

        // Literal patterns are more specific than identifier patterns
        assert!(literal_pattern.is_more_specific_than(&identifier_pattern));
        assert!(!identifier_pattern.is_more_specific_than(&literal_pattern));

        // Struct patterns are more specific than identifier patterns
        assert!(struct_pattern.is_more_specific_than(&identifier_pattern));
        assert!(!identifier_pattern.is_more_specific_than(&struct_pattern));

        // Literals and structs are not more specific than each other (different types)
        assert!(!literal_pattern.is_more_specific_than(&struct_pattern));
        assert!(!struct_pattern.is_more_specific_than(&literal_pattern));
    }

    #[test]
    fn test_pattern_overlap() {
        // Test pattern overlap detection for reachability analysis

        let identifier_pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "x".to_string(),
            },
            None,
            Span::new(0, 0),
        );

        let literal_42 = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Integer(outrun_parser::IntegerLiteral {
                        value: 42,
                        format: outrun_parser::IntegerFormat::Decimal,
                        raw_text: "42".to_string(),
                        span: Span::new(0, 0),
                    }),
                    literal_type: None,
                    span: Span::new(0, 0),
                },
            },
            None,
            Span::new(0, 0),
        );

        let literal_43 = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Integer(outrun_parser::IntegerLiteral {
                        value: 43,
                        format: outrun_parser::IntegerFormat::Decimal,
                        raw_text: "43".to_string(),
                        span: Span::new(0, 0),
                    }),
                    literal_type: None,
                    span: Span::new(0, 0),
                },
            },
            None,
            Span::new(0, 0),
        );

        // Identifier patterns overlap with everything
        assert!(identifier_pattern.overlaps_with(&literal_42));
        assert!(literal_42.overlaps_with(&identifier_pattern));

        // Identical literal patterns overlap
        assert!(literal_42.overlaps_with(&literal_42));

        // Different literal patterns don't overlap
        assert!(!literal_42.overlaps_with(&literal_43));
        assert!(!literal_43.overlaps_with(&literal_42));
    }

    #[test]
    fn test_exhaustiveness_checker_placeholder() {
        // Test the exhaustiveness checker infrastructure

        let context = UnificationContext::new();
        let checker = ExhaustivenessChecker::new(context);

        let patterns = vec![TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "x".to_string(),
            },
            None,
            Span::new(0, 0),
        )];

        let string_type =
            StructuredType::Simple(crate::types::TypeInterner::new().intern_type("String"));

        // Placeholder implementation always returns true for now
        assert!(checker.is_exhaustive(&patterns, &string_type).unwrap());

        // Placeholder implementations return empty results
        assert!(checker
            .find_missing_patterns(&patterns, &string_type)
            .unwrap()
            .is_empty());
        assert!(checker
            .find_unreachable_patterns(&patterns)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_pattern_bound_variable_utilities() {
        // Test various utilities for working with bound variables

        let pattern = TypedPattern::new(
            TypedPatternKind::Tuple {
                elements: vec![
                    TypedPattern::new(
                        TypedPatternKind::Identifier {
                            name: "x".to_string(),
                        },
                        None,
                        Span::new(0, 0),
                    ),
                    TypedPattern::new(
                        TypedPatternKind::Struct {
                            type_path: vec!["User".to_string()],
                            fields: vec![TypedStructFieldPattern::shorthand(
                                "name".to_string(),
                                None,
                                Span::new(0, 0),
                            )],
                        },
                        None,
                        Span::new(0, 0),
                    ),
                ],
            },
            None,
            Span::new(0, 0),
        );

        // Test bound variable checking
        assert!(pattern.binds_variable("x"));
        assert!(pattern.binds_variable("name"));
        assert!(!pattern.binds_variable("nonexistent"));

        // Test variable type lookup (returns None for now since types aren't resolved)
        assert!(pattern.get_variable_type("x").is_none());
        assert!(pattern.get_variable_type("name").is_none());

        // Test collecting all bound variables
        let bound_vars = crate::patterns::PatternChecker::collect_bound_variables(&pattern);
        assert_eq!(bound_vars.len(), 2);

        let names: Vec<String> = bound_vars.iter().map(|v| v.name.clone()).collect();
        assert!(names.contains(&"x".to_string()));
        assert!(names.contains(&"name".to_string()));
    }
}
