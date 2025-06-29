//! Typed pattern system for comprehensive pattern matching with type validation
//!
//! This module provides typed versions of all pattern types from the parser,
//! with additional type information and variable binding tracking for the
//! type checker and typed AST.

use crate::compilation::compiler_environment::{CompilerEnvironment, TypeNameId};
use crate::error::{SpanExt, TypeError};
use crate::unification::{StructuredType, UnificationContext};
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
    struct_registry: &'a HashMap<TypeNameId, StructDefinition>,
    compiler_environment: &'a CompilerEnvironment,
}

impl<'a> PatternChecker<'a> {
    /// Create a new pattern checker with the given type context and struct registry
    pub fn new(
        _context: &'a mut UnificationContext,
        struct_registry: &'a HashMap<TypeNameId, StructDefinition>,
        compiler_environment: &'a CompilerEnvironment,
    ) -> Self {
        Self {
            struct_registry,
            compiler_environment,
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
            // TODO: Update to use new CompilerEnvironment pattern
            // For now, do basic type equality check
            if literal_type != target_type {
                return Err(TypeError::internal(format!(
                    "Literal pattern type {} does not match target type {}",
                    literal_type.to_string_representation(),
                    target_type.to_string_representation()
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
        let struct_type_id = Some(self.compiler_environment.intern_type_name(&struct_name));
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
                let type_id = self.compiler_environment.intern_type_name(&type_name);

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
                    let param_name = self
                        .compiler_environment
                        .intern_atom_name(&param.name.name.clone());
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
                let boolean_type = self.compiler_environment.intern_type_name("Boolean");
                Some(StructuredType::Simple(boolean_type))
            }
            Literal::Integer(_) => {
                let integer_type = self.compiler_environment.intern_type_name("Integer");
                Some(StructuredType::Simple(integer_type))
            }
            Literal::Float(_) => {
                let float_type = self.compiler_environment.intern_type_name("Float");
                Some(StructuredType::Simple(float_type))
            }
            Literal::String(_) => {
                let string_type = self.compiler_environment.intern_type_name("String");
                Some(StructuredType::Simple(string_type))
            }
            Literal::Atom(_) => {
                let atom_type = self.compiler_environment.intern_type_name("Atom");
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
    compiler_environment: Option<crate::compilation::compiler_environment::CompilerEnvironment>,
}

impl ExhaustivenessChecker {
    /// Create a new exhaustiveness checker
    pub fn new(
        compiler_environment: Option<crate::compilation::compiler_environment::CompilerEnvironment>,
    ) -> Self {
        Self {
            compiler_environment,
        }
    }

    /// Create a new exhaustiveness checker with compiler environment for trait case checking
    pub fn with_compiler_environment(
        _context: UnificationContext,
        compiler_environment: crate::compilation::compiler_environment::CompilerEnvironment,
    ) -> Self {
        Self {
            compiler_environment: Some(compiler_environment),
        }
    }

    /// Check if a set of patterns is exhaustive for a given type
    pub fn is_exhaustive(
        &self,
        patterns: &[TypedPattern],
        target_type: &StructuredType,
    ) -> Result<bool, TypeError> {
        if patterns
            .iter()
            .any(|p| matches!(p.kind, TypedPatternKind::Identifier { .. }))
        {
            return Ok(true);
        }

        if patterns
            .iter()
            .any(|p| self.pattern_has_literal_constraints(p))
        {
            let all_constructors = self.get_all_constructors_for_type(target_type)?;
            if all_constructors.is_empty() {
                return Ok(false);
            }
        }

        let covered_constructors: Vec<PatternConstructor> = patterns
            .iter()
            .filter_map(|p| p.get_constructor())
            .collect();

        let all_constructors = self.get_all_constructors_for_type(target_type)?;

        if all_constructors.is_empty() {
            let has_struct_wildcards = patterns.iter().any(|p| self.is_wildcard_struct_pattern(p));
            if has_struct_wildcards {
                return Ok(true);
            }
            return Ok(false);
        }

        for required_constructor in &all_constructors {
            if !self.constructor_is_covered(required_constructor, &covered_constructors) {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Find missing patterns that would make the match exhaustive
    pub fn find_missing_patterns(
        &self,
        patterns: &[TypedPattern],
        target_type: &StructuredType,
    ) -> Result<Vec<TypedPattern>, TypeError> {
        if self.is_exhaustive(patterns, target_type)? {
            return Ok(Vec::new());
        }

        let covered_constructors: Vec<PatternConstructor> = patterns
            .iter()
            .filter_map(|p| p.get_constructor())
            .collect();

        let all_constructors = self.get_all_constructors_for_type(target_type)?;

        if all_constructors.is_empty() {
            let wildcard_pattern = TypedPattern::new(
                TypedPatternKind::Identifier {
                    name: "_".to_string(),
                },
                Some(target_type.clone()),
                Span::new(0, 0),
            );
            return Ok(vec![wildcard_pattern]);
        }

        let mut missing_patterns = Vec::new();
        for required_constructor in &all_constructors {
            if !self.constructor_is_covered(required_constructor, &covered_constructors) {
                let missing_pattern =
                    self.generate_pattern_for_constructor(required_constructor, target_type)?;
                missing_patterns.push(missing_pattern);
            }
        }

        Ok(missing_patterns)
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

    /// Get all possible constructors for a given type
    fn get_all_constructors_for_type(
        &self,
        target_type: &StructuredType,
    ) -> Result<Vec<PatternConstructor>, TypeError> {
        match target_type {
            // Simple types: check if it's a known type with limited constructors or a trait
            StructuredType::Simple(type_id) => {
                // Check if this is a trait type for trait case exhaustiveness
                if let Some(compiler_env) = &self.compiler_environment {
                    if compiler_env.is_trait(&StructuredType::Simple(type_id.clone())) {
                        // This is a trait - return all implementor types as constructors
                        let implementors = compiler_env
                            .get_trait_implementations(&StructuredType::Simple(type_id.clone()));
                        return Ok(implementors
                            .into_iter()
                            .map(|impl_type| {
                                PatternConstructor::TraitImplementor(
                                    if let StructuredType::Simple(id) = impl_type {
                                        id
                                    } else {
                                        type_id.clone()
                                    },
                                )
                            })
                            .collect());
                    }
                }

                let type_name = type_id.to_string();
                match type_name.as_str() {
                    "Boolean" | "Outrun.Core.Boolean" => Ok(vec![
                        PatternConstructor::Literal(Literal::Boolean(
                            outrun_parser::BooleanLiteral {
                                value: true,
                                span: Span::new(0, 0),
                            },
                        )),
                        PatternConstructor::Literal(Literal::Boolean(
                            outrun_parser::BooleanLiteral {
                                value: false,
                                span: Span::new(0, 0),
                            },
                        )),
                    ]),

                    // For literal types, we can check exhaustiveness by collecting all literal patterns
                    // but since they have infinite possible values, we require explicit wildcard
                    "Integer"
                    | "Outrun.Core.Integer"
                    | "Outrun.Core.Integer64"
                    | "String"
                    | "Outrun.Core.String"
                    | "Float"
                    | "Outrun.Core.Float"
                    | "Outrun.Core.Float64"
                    | "Atom"
                    | "Outrun.Core.Atom" => Ok(vec![]),

                    // For unknown simple types, assume infinite constructor space
                    _ => Ok(vec![]),
                }
            }

            // Generic types: analyze base type and args for algebraic data types
            StructuredType::Generic { base, args: _ } => {
                let base_type_name = base.to_string();

                match base_type_name.as_str() {
                    "Option" | "Outrun.Core.Option" => {
                        // Option<T> has Some and None constructors
                        Ok(vec![
                            PatternConstructor::Struct(vec![
                                "Option".to_string(),
                                "Some".to_string(),
                            ]),
                            PatternConstructor::Struct(vec![
                                "Option".to_string(),
                                "None".to_string(),
                            ]),
                        ])
                    }

                    "Result" | "Outrun.Core.Result" => {
                        // Result<T, E> has Ok and Err constructors
                        Ok(vec![
                            PatternConstructor::Struct(vec![
                                "Result".to_string(),
                                "Ok".to_string(),
                            ]),
                            PatternConstructor::Struct(vec![
                                "Result".to_string(),
                                "Err".to_string(),
                            ]),
                        ])
                    }

                    "List" | "Outrun.Core.List" => {
                        // Lists (linked lists) have three exhaustive patterns:
                        // - Empty list: []
                        // - Single element: [elem]
                        // - Multiple elements with rest: [elem, ..rest]
                        Ok(vec![
                            PatternConstructor::List {
                                min_length: 0,
                                has_rest: false,
                            }, // []
                            PatternConstructor::List {
                                min_length: 1,
                                has_rest: false,
                            }, // [elem]
                            PatternConstructor::List {
                                min_length: 1,
                                has_rest: true,
                            }, // [elem, ..rest]
                        ])
                    }

                    // For other generic types, we can't enumerate constructors
                    _ => Ok(vec![]),
                }
            }

            // Tuple types: single constructor with fixed arity (must be non-empty)
            StructuredType::Tuple(elements) => {
                if elements.is_empty() {
                    return Err(TypeError::internal(
                        "Empty tuples are not allowed in Outrun - use Unit type instead"
                            .to_string(),
                    ));
                }
                Ok(vec![PatternConstructor::Tuple(elements.len())])
            }

            // Function types: single constructor (functions can be pattern matched as values)
            StructuredType::Function { .. } => {
                // Functions have a single constructor based on their signature
                // For pattern matching, we treat them as opaque values
                Ok(vec![])
            }

            // For other structured types, we don't have enough information yet
            // TODO: Add support for user-defined structs, enums, unions
            _ => Ok(vec![]),
        }
    }

    /// Check if a required constructor is covered by any of the provided constructors
    fn constructor_is_covered(
        &self,
        required: &PatternConstructor,
        covered: &[PatternConstructor],
    ) -> bool {
        covered.iter().any(|c| self.constructors_match(required, c))
    }

    /// Check if two constructors match (represent the same value space)
    fn constructors_match(&self, c1: &PatternConstructor, c2: &PatternConstructor) -> bool {
        match (c1, c2) {
            // Exact literal matches
            (PatternConstructor::Literal(lit1), PatternConstructor::Literal(lit2)) => lit1 == lit2,

            // Struct type matches
            (PatternConstructor::Struct(path1), PatternConstructor::Struct(path2)) => {
                path1 == path2
            }

            // Tuple arity matches
            (PatternConstructor::Tuple(arity1), PatternConstructor::Tuple(arity2)) => {
                arity1 == arity2
            }

            // List pattern matching for exhaustiveness checking
            (
                PatternConstructor::List {
                    min_length: len1,
                    has_rest: rest1,
                },
                PatternConstructor::List {
                    min_length: len2,
                    has_rest: rest2,
                },
            ) => {
                match (rest1, rest2) {
                    // Both have rest patterns - they cover overlapping ranges
                    (true, true) => true,

                    // Pattern with rest covers the exact length pattern if min_length <= exact_length
                    (true, false) => len1 <= len2,
                    (false, true) => len2 <= len1,

                    // Both exact lengths must match exactly
                    (false, false) => len1 == len2,
                }
            }

            // Trait implementor matches
            (
                PatternConstructor::TraitImplementor(type1),
                PatternConstructor::TraitImplementor(type2),
            ) => type1 == type2,

            // Trait implementor matches struct pattern by type name
            (PatternConstructor::TraitImplementor(type_id), PatternConstructor::Struct(path))
            | (PatternConstructor::Struct(path), PatternConstructor::TraitImplementor(type_id)) => {
                {
                    let type_name = type_id.to_string();
                    // Check if the type name matches the struct path
                    let type_path: Vec<String> =
                        type_name.split('.').map(|s| s.to_string()).collect();
                    &type_path == path || path.len() == 1 && path[0] == type_name
                }
            }

            // Different constructor types never match
            _ => false,
        }
    }

    /// Generate a pattern for a given constructor
    fn generate_pattern_for_constructor(
        &self,
        constructor: &PatternConstructor,
        target_type: &StructuredType,
    ) -> Result<TypedPattern, TypeError> {
        let span = Span::new(0, 0); // Dummy span for generated patterns

        match constructor {
            PatternConstructor::Literal(literal) => Ok(TypedPattern::new(
                TypedPatternKind::Literal {
                    literal: TypedLiteralPattern {
                        literal: literal.clone(),
                        literal_type: Some(target_type.clone()),
                        span,
                    },
                },
                Some(target_type.clone()),
                span,
            )),

            PatternConstructor::Struct(type_path) => {
                Ok(TypedPattern::new(
                    TypedPatternKind::Struct {
                        type_path: type_path.clone(),
                        fields: vec![], // Simplified - could generate field patterns
                    },
                    Some(target_type.clone()),
                    span,
                ))
            }

            PatternConstructor::Tuple(arity) => {
                // Generate wildcard patterns for each tuple element
                let element_patterns: Vec<TypedPattern> = (0..*arity)
                    .map(|i| {
                        TypedPattern::new(
                            TypedPatternKind::Identifier {
                                name: format!("_{}", i),
                            },
                            None, // Don't know element types yet
                            span,
                        )
                    })
                    .collect();

                Ok(TypedPattern::new(
                    TypedPatternKind::Tuple {
                        elements: element_patterns,
                    },
                    Some(target_type.clone()),
                    span,
                ))
            }

            PatternConstructor::List {
                min_length,
                has_rest,
            } => {
                // Generate wildcard patterns for minimum required elements
                let element_patterns: Vec<TypedPattern> = (0..*min_length)
                    .map(|i| {
                        TypedPattern::new(
                            TypedPatternKind::Identifier {
                                name: format!("_{}", i),
                            },
                            None,
                            span,
                        )
                    })
                    .collect();

                let rest = if *has_rest {
                    Some(BoundVariable {
                        name: "_rest".to_string(),
                        variable_type: None,
                        span,
                    })
                } else {
                    None
                };

                Ok(TypedPattern::new(
                    TypedPatternKind::List {
                        elements: element_patterns,
                        rest,
                    },
                    Some(target_type.clone()),
                    span,
                ))
            }

            PatternConstructor::TraitImplementor(type_id) => {
                // Generate a struct pattern for the trait implementor type
                {
                    let type_name = type_id.to_string();
                    // Split type name into path components (e.g., "Outrun.Core.String" -> ["Outrun", "Core", "String"])
                    let type_path: Vec<String> =
                        type_name.split('.').map(|s| s.to_string()).collect();

                    Ok(TypedPattern::new(
                        TypedPatternKind::Struct {
                            type_path,
                            fields: vec![], // Simplified - could generate field patterns
                        },
                        Some(target_type.clone()),
                        span,
                    ))
                }
            }
        }
    }

    /// Check if a pattern has literal constraints that make it non-exhaustive
    #[allow(clippy::only_used_in_recursion)]
    fn pattern_has_literal_constraints(&self, pattern: &TypedPattern) -> bool {
        match &pattern.kind {
            // Literal patterns are constraints
            TypedPatternKind::Literal { .. } => true,

            // Struct patterns with literal field patterns are constraints
            TypedPatternKind::Struct { fields, .. } => {
                fields.iter().any(|field| {
                    if let Some(field_pattern) = &field.pattern {
                        self.pattern_has_literal_constraints(field_pattern)
                    } else {
                        false // Shorthand field patterns are wildcards
                    }
                })
            }

            // Tuple patterns with literal elements are constraints
            TypedPatternKind::Tuple { elements } => elements
                .iter()
                .any(|elem| self.pattern_has_literal_constraints(elem)),

            // List patterns with literal elements are constraints
            TypedPatternKind::List { elements, .. } => elements
                .iter()
                .any(|elem| self.pattern_has_literal_constraints(elem)),

            // Identifier patterns are wildcards - no constraints
            TypedPatternKind::Identifier { .. } => false,
        }
    }

    /// Check if a pattern is a struct pattern with only wildcard fields (exhaustive for user types)
    fn is_wildcard_struct_pattern(&self, pattern: &TypedPattern) -> bool {
        match &pattern.kind {
            TypedPatternKind::Struct { fields, .. } => {
                // All fields must be wildcards (either shorthand or identifier patterns)
                fields.iter().all(|field| {
                    match &field.pattern {
                        None => true, // Shorthand field is a wildcard
                        Some(field_pattern) => {
                            matches!(field_pattern.kind, TypedPatternKind::Identifier { .. })
                        }
                    }
                })
            }
            _ => false,
        }
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
    TraitImplementor(TypeNameId), // Trait implementor type for trait case exhaustiveness
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper function to create a test context
    fn create_test_context() -> UnificationContext {
        UnificationContext::new()
    }

    /// Helper function to create a test CompilerEnvironment
    fn create_test_compiler_env() -> crate::compilation::compiler_environment::CompilerEnvironment {
        crate::compilation::compiler_environment::CompilerEnvironment::new()
    }

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

        let _context = create_test_context();
        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        let patterns = vec![TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "x".to_string(),
            },
            None,
            Span::new(0, 0),
        )];

        let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
        let string_type = StructuredType::Simple(env.intern_type_name("String"));

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

    #[test]
    fn test_boolean_exhaustiveness_complete() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let boolean_type_id = compiler_env.intern_type_name("Boolean");
        let boolean_type = StructuredType::Simple(boolean_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Create patterns for true and false
        let true_pattern = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Boolean(outrun_parser::BooleanLiteral {
                        value: true,
                        span: Span::new(0, 0),
                    }),
                    literal_type: Some(boolean_type.clone()),
                    span: Span::new(0, 0),
                },
            },
            Some(boolean_type.clone()),
            Span::new(0, 0),
        );

        let false_pattern = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Boolean(outrun_parser::BooleanLiteral {
                        value: false,
                        span: Span::new(0, 0),
                    }),
                    literal_type: Some(boolean_type.clone()),
                    span: Span::new(0, 0),
                },
            },
            Some(boolean_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![true_pattern, false_pattern];

        // Should be exhaustive
        assert!(checker.is_exhaustive(&patterns, &boolean_type).unwrap());

        // Should have no missing patterns
        assert!(checker
            .find_missing_patterns(&patterns, &boolean_type)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_boolean_exhaustiveness_incomplete() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let boolean_type_id = compiler_env.intern_type_name("Boolean");
        let boolean_type = StructuredType::Simple(boolean_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Only true pattern
        let true_pattern = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Boolean(outrun_parser::BooleanLiteral {
                        value: true,
                        span: Span::new(0, 0),
                    }),
                    literal_type: Some(boolean_type.clone()),
                    span: Span::new(0, 0),
                },
            },
            Some(boolean_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![true_pattern];

        // Should not be exhaustive
        assert!(!checker.is_exhaustive(&patterns, &boolean_type).unwrap());

        // Should have missing false pattern
        let missing = checker
            .find_missing_patterns(&patterns, &boolean_type)
            .unwrap();
        assert_eq!(missing.len(), 1);

        // Check the missing pattern is false
        if let TypedPatternKind::Literal { literal } = &missing[0].kind {
            if let Literal::Boolean(bool_lit) = &literal.literal {
                assert!(!bool_lit.value);
            } else {
                panic!("Expected boolean literal");
            }
        } else {
            panic!("Expected literal pattern");
        }
    }

    #[test]
    fn test_wildcard_makes_exhaustive() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let boolean_type_id = compiler_env.intern_type_name("Boolean");
        let boolean_type = StructuredType::Simple(boolean_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Wildcard pattern (identifier)
        let wildcard_pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "_".to_string(),
            },
            Some(boolean_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![wildcard_pattern];

        // Should be exhaustive due to wildcard
        assert!(checker.is_exhaustive(&patterns, &boolean_type).unwrap());

        // Should have no missing patterns
        assert!(checker
            .find_missing_patterns(&patterns, &boolean_type)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_option_exhaustiveness() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let option_type_id = compiler_env.intern_type_name("Option");
        let string_type_id = compiler_env.intern_type_name("String");
        let option_type = StructuredType::Generic {
            base: option_type_id,
            args: vec![StructuredType::Simple(string_type_id)],
        };

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Some pattern
        let some_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["Option".to_string(), "Some".to_string()],
                fields: vec![],
            },
            Some(option_type.clone()),
            Span::new(0, 0),
        );

        // None pattern
        let none_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["Option".to_string(), "None".to_string()],
                fields: vec![],
            },
            Some(option_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![some_pattern, none_pattern];

        // Should be exhaustive
        assert!(checker.is_exhaustive(&patterns, &option_type).unwrap());
    }

    #[test]
    fn test_option_exhaustiveness_incomplete() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let option_type_id = compiler_env.intern_type_name("Option");
        let string_type_id = compiler_env.intern_type_name("String");
        let option_type = StructuredType::Generic {
            base: option_type_id,
            args: vec![StructuredType::Simple(string_type_id)],
        };

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Only Some pattern
        let some_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["Option".to_string(), "Some".to_string()],
                fields: vec![],
            },
            Some(option_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![some_pattern];

        // Should not be exhaustive
        assert!(!checker.is_exhaustive(&patterns, &option_type).unwrap());

        // Should have missing None pattern
        let missing = checker
            .find_missing_patterns(&patterns, &option_type)
            .unwrap();
        assert_eq!(missing.len(), 1);

        // Check the missing pattern is None
        if let TypedPatternKind::Struct { type_path, .. } = &missing[0].kind {
            assert_eq!(type_path, &vec!["Option".to_string(), "None".to_string()]);
        } else {
            panic!("Expected struct pattern");
        }
    }

    #[test]
    fn test_tuple_exhaustiveness() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let string_type_id = compiler_env.intern_type_name("String");
        let int_type_id = compiler_env.intern_type_name("Integer");
        let tuple_type = StructuredType::Tuple(vec![
            StructuredType::Simple(string_type_id),
            StructuredType::Simple(int_type_id),
        ]);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Tuple pattern with correct arity
        let tuple_pattern = TypedPattern::new(
            TypedPatternKind::Tuple {
                elements: vec![
                    TypedPattern::new(
                        TypedPatternKind::Identifier {
                            name: "name".to_string(),
                        },
                        None,
                        Span::new(0, 0),
                    ),
                    TypedPattern::new(
                        TypedPatternKind::Identifier {
                            name: "age".to_string(),
                        },
                        None,
                        Span::new(0, 0),
                    ),
                ],
            },
            Some(tuple_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![tuple_pattern];

        // Should be exhaustive (single constructor for tuples)
        assert!(checker.is_exhaustive(&patterns, &tuple_type).unwrap());
    }

    #[test]
    fn test_integer_requires_wildcard_for_exhaustiveness() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let int_type_id = compiler_env.intern_type_name("Integer");
        let int_type = StructuredType::Simple(int_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Integer literal pattern
        let int_pattern = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Integer(outrun_parser::IntegerLiteral {
                        value: 42,
                        format: outrun_parser::IntegerFormat::Decimal,
                        raw_text: "42".to_string(),
                        span: Span::new(0, 0),
                    }),
                    literal_type: Some(int_type.clone()),
                    span: Span::new(0, 0),
                },
            },
            Some(int_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![int_pattern];

        // Should NOT be exhaustive - integers have infinite possible values
        assert!(!checker.is_exhaustive(&patterns, &int_type).unwrap());

        // Should suggest a wildcard pattern
        let missing = checker.find_missing_patterns(&patterns, &int_type).unwrap();
        assert_eq!(missing.len(), 1);

        // Check the missing pattern is a wildcard
        if let TypedPatternKind::Identifier { name } = &missing[0].kind {
            assert_eq!(name, "_");
        } else {
            panic!("Expected identifier pattern (wildcard)");
        }
    }

    #[test]
    fn test_literal_with_wildcard_is_exhaustive() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let int_type_id = compiler_env.intern_type_name("Integer");
        let int_type = StructuredType::Simple(int_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Integer literal pattern + wildcard
        let int_pattern = TypedPattern::new(
            TypedPatternKind::Literal {
                literal: TypedLiteralPattern {
                    literal: Literal::Integer(outrun_parser::IntegerLiteral {
                        value: 42,
                        format: outrun_parser::IntegerFormat::Decimal,
                        raw_text: "42".to_string(),
                        span: Span::new(0, 0),
                    }),
                    literal_type: Some(int_type.clone()),
                    span: Span::new(0, 0),
                },
            },
            Some(int_type.clone()),
            Span::new(0, 0),
        );

        let wildcard_pattern = TypedPattern::new(
            TypedPatternKind::Identifier {
                name: "_".to_string(),
            },
            Some(int_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![int_pattern, wildcard_pattern];

        // Should be exhaustive with wildcard
        assert!(checker.is_exhaustive(&patterns, &int_type).unwrap());
    }

    #[test]
    fn test_trait_exhaustiveness_complete() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();

        // Set up trait and implementors
        let display_trait_id = compiler_env.intern_type_name("Display");
        let string_type_id = compiler_env.intern_type_name("String");
        let integer_type_id = compiler_env.intern_type_name("Integer");

        // Register trait implementations using CompilerEnvironment
        compiler_env.register_trait_implementation(
            StructuredType::Simple(string_type_id.clone()),
            StructuredType::Simple(display_trait_id.clone()),
        );
        compiler_env.register_trait_implementation(
            StructuredType::Simple(integer_type_id.clone()),
            StructuredType::Simple(display_trait_id.clone()),
        );

        let display_trait_type = StructuredType::Simple(display_trait_id);
        let checker = ExhaustivenessChecker::new(Some(compiler_env));

        // Create patterns for both implementors
        let string_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["String".to_string()],
                fields: vec![],
            },
            Some(display_trait_type.clone()),
            Span::new(0, 0),
        );

        let integer_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["Integer".to_string()],
                fields: vec![],
            },
            Some(display_trait_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![string_pattern, integer_pattern];

        // Should be exhaustive - covers all implementors
        assert!(checker
            .is_exhaustive(&patterns, &display_trait_type)
            .unwrap());

        // Should have no missing patterns
        assert!(checker
            .find_missing_patterns(&patterns, &display_trait_type)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_trait_exhaustiveness_incomplete() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();

        // Set up trait and implementors
        let display_trait_id = compiler_env.intern_type_name("Display");
        let string_type_id = compiler_env.intern_type_name("String");
        let integer_type_id = compiler_env.intern_type_name("Integer");

        // Register trait implementations using CompilerEnvironment
        compiler_env.register_trait_implementation(
            StructuredType::Simple(string_type_id.clone()),
            StructuredType::Simple(display_trait_id.clone()),
        );
        compiler_env.register_trait_implementation(
            StructuredType::Simple(integer_type_id.clone()),
            StructuredType::Simple(display_trait_id.clone()),
        );

        let display_trait_type = StructuredType::Simple(display_trait_id);
        let checker = ExhaustivenessChecker::new(Some(compiler_env));

        // Only String pattern (missing Integer)
        let string_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["String".to_string()],
                fields: vec![],
            },
            Some(display_trait_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![string_pattern];

        // Should NOT be exhaustive - missing Integer implementor
        assert!(!checker
            .is_exhaustive(&patterns, &display_trait_type)
            .unwrap());

        // Should have missing Integer pattern
        let missing = checker
            .find_missing_patterns(&patterns, &display_trait_type)
            .unwrap();
        assert_eq!(missing.len(), 1);

        // Check the missing pattern is for Integer
        if let TypedPatternKind::Struct { type_path, .. } = &missing[0].kind {
            assert_eq!(type_path, &vec!["Integer".to_string()]);
        } else {
            panic!("Expected struct pattern for missing trait implementor");
        }
    }

    #[test]
    fn test_list_exhaustiveness_complete() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let list_type_id = compiler_env.intern_type_name("List");
        let string_type_id = compiler_env.intern_type_name("String");
        let list_type = StructuredType::Generic {
            base: list_type_id,
            args: vec![StructuredType::Simple(string_type_id)],
        };

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Create patterns for all three list constructors
        // Empty list: []
        let empty_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![],
                rest: None,
            },
            Some(list_type.clone()),
            Span::new(0, 0),
        );

        // Single element: [elem]
        let single_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![TypedPattern::new(
                    TypedPatternKind::Identifier {
                        name: "elem".to_string(),
                    },
                    None,
                    Span::new(0, 0),
                )],
                rest: None,
            },
            Some(list_type.clone()),
            Span::new(0, 0),
        );

        // Multiple elements with rest: [elem, ..rest]
        let rest_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![TypedPattern::new(
                    TypedPatternKind::Identifier {
                        name: "elem".to_string(),
                    },
                    None,
                    Span::new(0, 0),
                )],
                rest: Some(BoundVariable {
                    name: "rest".to_string(),
                    variable_type: None,
                    span: Span::new(0, 0),
                }),
            },
            Some(list_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![empty_pattern, single_pattern, rest_pattern];

        // Should be exhaustive - covers all three list patterns
        assert!(checker.is_exhaustive(&patterns, &list_type).unwrap());

        // Should have no missing patterns
        assert!(checker
            .find_missing_patterns(&patterns, &list_type)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_list_exhaustiveness_incomplete() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let list_type_id = compiler_env.intern_type_name("List");
        let string_type_id = compiler_env.intern_type_name("String");
        let list_type = StructuredType::Generic {
            base: list_type_id,
            args: vec![StructuredType::Simple(string_type_id)],
        };

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Only empty list pattern (missing single element and rest patterns)
        let empty_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![],
                rest: None,
            },
            Some(list_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![empty_pattern];

        // Should NOT be exhaustive - missing [elem] and [elem, ..rest] patterns
        assert!(!checker.is_exhaustive(&patterns, &list_type).unwrap());

        // Should have missing patterns
        let missing = checker
            .find_missing_patterns(&patterns, &list_type)
            .unwrap();
        assert_eq!(missing.len(), 2); // [elem] and [elem, ..rest]
    }

    #[test]
    fn test_list_with_rest_covers_multiple_lengths() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let list_type_id = compiler_env.intern_type_name("List");
        let string_type_id = compiler_env.intern_type_name("String");
        let list_type = StructuredType::Generic {
            base: list_type_id,
            args: vec![StructuredType::Simple(string_type_id)],
        };

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Empty list: []
        let empty_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![],
                rest: None,
            },
            Some(list_type.clone()),
            Span::new(0, 0),
        );

        // Any non-empty list: [elem, ..rest] (covers [elem], [elem1, elem2], etc.)
        let rest_pattern = TypedPattern::new(
            TypedPatternKind::List {
                elements: vec![TypedPattern::new(
                    TypedPatternKind::Identifier {
                        name: "elem".to_string(),
                    },
                    None,
                    Span::new(0, 0),
                )],
                rest: Some(BoundVariable {
                    name: "rest".to_string(),
                    variable_type: None,
                    span: Span::new(0, 0),
                }),
            },
            Some(list_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![empty_pattern, rest_pattern];

        // Should be exhaustive - [] covers empty, [elem, ..rest] covers all non-empty
        assert!(checker.is_exhaustive(&patterns, &list_type).unwrap());
    }

    #[test]
    fn test_struct_pattern_with_literals_non_exhaustive() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let user_type_id = compiler_env.intern_type_name("User");
        let user_type = StructuredType::Simple(user_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Struct pattern with literal constraint: User { id: 1, name: name }
        let literal_struct_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["User".to_string()],
                fields: vec![
                    TypedStructFieldPattern {
                        name: "id".to_string(),
                        pattern: Some(TypedPattern::new(
                            TypedPatternKind::Literal {
                                literal: TypedLiteralPattern {
                                    literal: Literal::Integer(outrun_parser::IntegerLiteral {
                                        value: 1,
                                        format: outrun_parser::IntegerFormat::Decimal,
                                        raw_text: "1".to_string(),
                                        span: Span::new(0, 0),
                                    }),
                                    literal_type: None,
                                    span: Span::new(0, 0),
                                },
                            },
                            None,
                            Span::new(0, 0),
                        )),
                        field_type: None,
                        span: Span::new(0, 0),
                    },
                    TypedStructFieldPattern {
                        name: "name".to_string(),
                        pattern: Some(TypedPattern::new(
                            TypedPatternKind::Identifier {
                                name: "name".to_string(),
                            },
                            None,
                            Span::new(0, 0),
                        )),
                        field_type: None,
                        span: Span::new(0, 0),
                    },
                ],
            },
            Some(user_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![literal_struct_pattern];

        // Should NOT be exhaustive - has literal constraint on id field
        assert!(!checker.is_exhaustive(&patterns, &user_type).unwrap());

        // Should suggest wildcard pattern
        let missing = checker
            .find_missing_patterns(&patterns, &user_type)
            .unwrap();
        assert_eq!(missing.len(), 1);

        // Check the missing pattern is a wildcard
        if let TypedPatternKind::Identifier { name } = &missing[0].kind {
            assert_eq!(name, "_");
        } else {
            panic!("Expected identifier pattern (wildcard)");
        }
    }

    #[test]
    fn test_struct_pattern_with_wildcards_exhaustive() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let user_type_id = compiler_env.intern_type_name("User");
        let user_type = StructuredType::Simple(user_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Struct pattern with wildcard fields: User { id: id, name: name }
        let wildcard_struct_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["User".to_string()],
                fields: vec![
                    TypedStructFieldPattern {
                        name: "id".to_string(),
                        pattern: Some(TypedPattern::new(
                            TypedPatternKind::Identifier {
                                name: "id".to_string(),
                            },
                            None,
                            Span::new(0, 0),
                        )),
                        field_type: None,
                        span: Span::new(0, 0),
                    },
                    TypedStructFieldPattern {
                        name: "name".to_string(),
                        pattern: Some(TypedPattern::new(
                            TypedPatternKind::Identifier {
                                name: "name".to_string(),
                            },
                            None,
                            Span::new(0, 0),
                        )),
                        field_type: None,
                        span: Span::new(0, 0),
                    },
                ],
            },
            Some(user_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![wildcard_struct_pattern];

        // Should be exhaustive - all fields are wildcards
        assert!(checker.is_exhaustive(&patterns, &user_type).unwrap());

        // Should have no missing patterns
        assert!(checker
            .find_missing_patterns(&patterns, &user_type)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_struct_pattern_shorthand_exhaustive() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let user_type_id = compiler_env.intern_type_name("User");
        let user_type = StructuredType::Simple(user_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Struct pattern with shorthand fields: User { id, name }
        let shorthand_struct_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["User".to_string()],
                fields: vec![
                    TypedStructFieldPattern {
                        name: "id".to_string(),
                        pattern: None, // Shorthand - equivalent to wildcard
                        field_type: None,
                        span: Span::new(0, 0),
                    },
                    TypedStructFieldPattern {
                        name: "name".to_string(),
                        pattern: None, // Shorthand - equivalent to wildcard
                        field_type: None,
                        span: Span::new(0, 0),
                    },
                ],
            },
            Some(user_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![shorthand_struct_pattern];

        // Should be exhaustive - shorthand fields are wildcards
        assert!(checker.is_exhaustive(&patterns, &user_type).unwrap());

        // Should have no missing patterns
        assert!(checker
            .find_missing_patterns(&patterns, &user_type)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_nested_struct_pattern_literal_constraints() {
        let _context = create_test_context();
        let compiler_env = create_test_compiler_env();
        let user_type_id = compiler_env.intern_type_name("User");
        let user_type = StructuredType::Simple(user_type_id);

        let checker = ExhaustivenessChecker::new(None); // TODO: Update to pass CompilerEnvironment

        // Nested struct pattern with literal: User { address: Address { city: "NYC" } }
        let nested_literal_pattern = TypedPattern::new(
            TypedPatternKind::Struct {
                type_path: vec!["User".to_string()],
                fields: vec![TypedStructFieldPattern {
                    name: "address".to_string(),
                    pattern: Some(TypedPattern::new(
                        TypedPatternKind::Struct {
                            type_path: vec!["Address".to_string()],
                            fields: vec![TypedStructFieldPattern {
                                name: "city".to_string(),
                                pattern: Some(TypedPattern::new(
                                    TypedPatternKind::Literal {
                                        literal: TypedLiteralPattern {
                                            literal: Literal::String(
                                                outrun_parser::StringLiteral {
                                                    parts: vec![outrun_parser::StringPart::Text {
                                                        content: "NYC".to_string(),
                                                        raw_content: "NYC".to_string(),
                                                    }],
                                                    format: outrun_parser::StringFormat::Basic,
                                                    span: Span::new(0, 0),
                                                },
                                            ),
                                            literal_type: None,
                                            span: Span::new(0, 0),
                                        },
                                    },
                                    None,
                                    Span::new(0, 0),
                                )),
                                field_type: None,
                                span: Span::new(0, 0),
                            }],
                        },
                        None,
                        Span::new(0, 0),
                    )),
                    field_type: None,
                    span: Span::new(0, 0),
                }],
            },
            Some(user_type.clone()),
            Span::new(0, 0),
        );

        let patterns = vec![nested_literal_pattern];

        // Should NOT be exhaustive - has literal constraint in nested struct
        assert!(!checker.is_exhaustive(&patterns, &user_type).unwrap());

        // Should suggest wildcard pattern
        let missing = checker
            .find_missing_patterns(&patterns, &user_type)
            .unwrap();
        assert_eq!(missing.len(), 1);

        // Check the missing pattern is a wildcard
        if let TypedPatternKind::Identifier { name } = &missing[0].kind {
            assert_eq!(name, "_");
        } else {
            panic!("Expected identifier pattern (wildcard)");
        }
    }
}

/// SAT-based guard exhaustiveness analysis
pub mod guard_exhaustiveness {
    use crate::checker::TypedExpression;
    use crate::types::traits::{ExhaustivenessResult, GuardCounterExample};
    use rustsat::instances::{BasicVarManager, Cnf, ManageVars};
    use rustsat::solvers::{Solve, SolverResult};
    use rustsat::types::{Lit, Var};
    use rustsat_cadical::CaDiCaL;
    use std::collections::HashMap;

    /// Converts guard expressions to SAT clauses for exhaustiveness analysis
    pub struct GuardExpressionConverter {
        /// Variable manager for generating SAT variables
        var_manager: BasicVarManager,
        /// Maps expression names/identifiers to SAT variables
        variable_map: HashMap<String, Var>,
        /// CNF formula being built
        cnf: Cnf,
    }

    /// Result of SAT-based guard completeness checking
    #[derive(Debug, Clone, PartialEq)]
    pub enum SatResult {
        /// Guards are exhaustive - no counter-examples found
        Exhaustive,
        /// Guards are not exhaustive - contains counter-examples
        Missing(Vec<GuardCounterExample>),
    }

    impl Default for GuardExpressionConverter {
        fn default() -> Self {
            Self::new()
        }
    }

    impl GuardExpressionConverter {
        /// Create a new guard expression converter
        pub fn new() -> Self {
            Self {
                var_manager: BasicVarManager::default(),
                variable_map: HashMap::new(),
                cnf: Cnf::new(),
            }
        }

        /// Get or create a SAT variable for a given identifier
        fn get_or_create_variable(&mut self, identifier: &str) -> Var {
            if let Some(&var) = self.variable_map.get(identifier) {
                var
            } else {
                let var = self.var_manager.new_var();
                self.variable_map.insert(identifier.to_string(), var);
                var
            }
        }

        /// Convert a guard expression to SAT clauses and return the guard literal
        pub fn convert_guard_expression(
            &mut self,
            guard: &TypedExpression,
        ) -> Result<Lit, crate::error::TypeError> {
            use crate::checker::TypedExpressionKind;

            match &guard.kind {
                TypedExpressionKind::Boolean(value) => {
                    let var = self.var_manager.new_var();
                    let lit = Lit::positive(var.idx() as u32);
                    let final_lit = if *value { lit } else { !lit };
                    self.cnf.add_clause([final_lit].into());
                    Ok(final_lit)
                }

                TypedExpressionKind::Identifier(name) => {
                    let var = self.get_or_create_variable(name);
                    Ok(Lit::positive(var.idx() as u32))
                }

                TypedExpressionKind::FunctionCall { .. } => {
                    let aux_var = self.var_manager.new_var();
                    Ok(Lit::positive(aux_var.idx() as u32))
                }

                _ => {
                    let aux_var = self.var_manager.new_var();
                    Ok(Lit::positive(aux_var.idx() as u32))
                }
            }
        }

        /// Add constraints for Boolean AND operation: guard_lit <-> (left && right)
        pub fn add_and_constraint(&mut self, guard_lit: Lit, left_lit: Lit, right_lit: Lit) {
            self.cnf.add_clause([!guard_lit, left_lit].into());
            self.cnf.add_clause([!guard_lit, right_lit].into());
            self.cnf
                .add_clause([guard_lit, !left_lit, !right_lit].into());
        }

        /// Add constraints for Boolean OR operation: guard_lit <-> (left || right)
        pub fn add_or_constraint(&mut self, guard_lit: Lit, left_lit: Lit, right_lit: Lit) {
            self.cnf
                .add_clause([!guard_lit, left_lit, right_lit].into());
            self.cnf.add_clause([guard_lit, !left_lit].into());
            self.cnf.add_clause([guard_lit, !right_lit].into());
        }

        /// Add constraints for Boolean NOT operation: guard_lit <-> !operand
        pub fn add_not_constraint(&mut self, guard_lit: Lit, operand_lit: Lit) {
            self.cnf.add_clause([!guard_lit, !operand_lit].into());
            self.cnf.add_clause([guard_lit, operand_lit].into());
        }

        /// Generate CNF formula and return it for SAT solving
        pub fn finalize(self) -> (Cnf, HashMap<String, Var>) {
            (self.cnf, self.variable_map)
        }
    }

    /// Check if a set of guards provides exhaustive coverage using SAT solving
    pub fn check_guard_completeness_sat(
        guards: &[TypedExpression],
        has_default_case: bool,
    ) -> Result<SatResult, crate::error::TypeError> {
        if has_default_case {
            return Ok(SatResult::Exhaustive);
        }

        let mut converter = GuardExpressionConverter::new();
        let mut guard_literals = Vec::new();

        for guard in guards {
            let guard_lit = converter.convert_guard_expression(guard)?;
            guard_literals.push(guard_lit);
        }

        let (mut cnf, _variable_map) = converter.finalize();

        for &guard_lit in &guard_literals {
            cnf.add_clause([!guard_lit].into());
        }

        let mut solver = CaDiCaL::default();
        solver
            .add_cnf(cnf)
            .map_err(|e| crate::error::TypeError::internal(format!("SAT solver error: {:?}", e)))?;

        match solver
            .solve()
            .map_err(|e| crate::error::TypeError::internal(format!("SAT solver error: {:?}", e)))?
        {
            SolverResult::Sat => {
                let counter_examples = vec![GuardCounterExample {
                    variable_assignments: HashMap::new(),
                    description: "Missing guard pattern detected by SAT solver".to_string(),
                    suggested_guard: Some("Add additional guard condition".to_string()),
                }];
                Ok(SatResult::Missing(counter_examples))
            }
            SolverResult::Unsat => Ok(SatResult::Exhaustive),
            SolverResult::Interrupted => Err(crate::error::TypeError::internal(
                "SAT solver was interrupted before completion".to_string(),
            )),
        }
    }

    /// Main function to analyze guard exhaustiveness for function definitions
    pub fn analyze_function_guard_exhaustiveness(
        _function_name: &str,
        guards: &[TypedExpression],
        has_default_case: bool,
        _span: outrun_parser::Span,
    ) -> Result<ExhaustivenessResult, crate::error::TypeError> {
        match check_guard_completeness_sat(guards, has_default_case)? {
            SatResult::Exhaustive => Ok(ExhaustivenessResult::Exhaustive),
            SatResult::Missing(counter_examples) => {
                Ok(ExhaustivenessResult::MissingGuardPatterns(counter_examples))
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::checker::{TypedExpression, TypedExpressionKind};
        use outrun_parser::Span;

        #[test]
        fn test_guard_expression_converter_creation() {
            let converter = GuardExpressionConverter::new();
            assert!(converter.variable_map.is_empty());
        }

        #[test]
        fn test_boolean_literal_conversion() {
            let mut converter = GuardExpressionConverter::new();

            let true_expr = TypedExpression {
                kind: TypedExpressionKind::Boolean(true),
                structured_type: None,
                span: Span::new(0, 0),
                debug_info: None,
            };

            let result = converter.convert_guard_expression(&true_expr);
            assert!(result.is_ok());
        }

        #[test]
        fn test_identifier_conversion() {
            let mut converter = GuardExpressionConverter::new();

            let identifier_expr = TypedExpression {
                kind: TypedExpressionKind::Identifier("flag".to_string()),
                structured_type: None,
                span: Span::new(0, 0),
                debug_info: None,
            };

            let result = converter.convert_guard_expression(&identifier_expr);
            assert!(result.is_ok());
            assert!(converter.variable_map.contains_key("flag"));
        }

        #[test]
        fn test_exhaustive_guards_with_default() {
            let guards = vec![];
            let result = check_guard_completeness_sat(&guards, true);

            assert!(result.is_ok());
            match result.unwrap() {
                SatResult::Exhaustive => (),
                _ => panic!("Expected exhaustive result with default case"),
            }
        }

        #[test]
        fn test_analyze_function_guard_exhaustiveness_with_default() {
            let guards = vec![];
            let result = analyze_function_guard_exhaustiveness(
                "test_function",
                &guards,
                true,
                Span::new(0, 0),
            );

            assert!(result.is_ok());
            match result.unwrap() {
                ExhaustivenessResult::Exhaustive => (),
                _ => panic!("Expected exhaustive result"),
            }
        }
    }
}
