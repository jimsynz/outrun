// Type system parsing module
// Handles struct definitions, trait definitions, implementations, generics, and constraints

use crate::ast::*;
use crate::error::*;
use crate::parser::{OutrunParser, Rule};

impl OutrunParser {
    /// Parse return type annotation
    pub(crate) fn parse_return_type(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<TypeAnnotation> {
        let inner = pair.into_inner().next().unwrap();
        Self::parse_type_annotation(inner)
    }

    /// Parse type annotation (module path with optional generic args)
    pub(crate) fn parse_type_annotation(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<TypeAnnotation> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();
        let inner_pair = inner_pairs.next().unwrap();

        match inner_pair.as_rule() {
            Rule::tuple_type => {
                // Parse tuple type: (Type1, Type2, ...)
                let mut types = Vec::new();
                for type_pair in inner_pair.into_inner() {
                    if type_pair.as_rule() == Rule::type_annotation {
                        types.push(Self::parse_type_annotation(type_pair)?);
                    }
                }
                Ok(TypeAnnotation::Tuple { types, span })
            }
            Rule::module_path => {
                // Parse simple type with optional generic args
                let path = Self::parse_module_path(inner_pair)?;

                // Check if there are any more inner pairs for generic args
                let mut generic_args = None;
                if let Some(args_pair) = inner_pairs.next() {
                    if args_pair.as_rule() == Rule::generic_args {
                        generic_args = Some(Self::parse_generic_args(args_pair)?);
                    }
                }

                Ok(TypeAnnotation::Simple {
                    path,
                    generic_args,
                    span,
                })
            }
            Rule::function_type => {
                // Parse function type: Function<(param: Type) -> ReturnType>
                Self::parse_function_type(inner_pair, span)
            }
            _ => Err(Self::unexpected_token_from_pair(
                &inner_pair,
                "Expected type annotation",
            )),
        }
    }

    /// Parse a struct definition
    pub(crate) fn parse_struct_definition(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<StructDefinition> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // Parse attributes first
        let mut attributes = Vec::new();
        let mut struct_keyword_found = false;
        let mut name_pair: Option<pest::iterators::Pair<Rule>> = None;

        // Parse attributes and find the struct keyword and name
        for inner_pair in inner_pairs.by_ref() {
            match inner_pair.as_rule() {
                Rule::attribute => {
                    attributes.push(Self::parse_attribute(inner_pair)?);
                }
                Rule::keyword_struct => {
                    struct_keyword_found = true;
                }
                Rule::module_path if struct_keyword_found => {
                    name_pair = Some(inner_pair);
                    break;
                }
                _ => {}
            }
        }

        // Parse struct name
        let name = Self::parse_module_path(name_pair.unwrap())?;

        let mut generic_params = None;
        let mut fields = Vec::new();
        let mut methods = Vec::new();

        for remaining_pair in inner_pairs {
            match remaining_pair.as_rule() {
                Rule::generic_params => {
                    generic_params = Some(Self::parse_generic_params(remaining_pair)?);
                }
                Rule::struct_body => {
                    // Parse the struct body which can contain parentheses and/or braces
                    for body_part in remaining_pair.into_inner() {
                        match body_part.as_rule() {
                            Rule::struct_parentheses => {
                                // Parse struct fields from parentheses
                                for paren_part in body_part.into_inner() {
                                    if paren_part.as_rule() == Rule::struct_fields {
                                        fields = Self::parse_struct_fields(paren_part)?;
                                    }
                                }
                            }
                            Rule::struct_braces => {
                                // Parse struct methods from braces
                                for brace_part in body_part.into_inner() {
                                    if brace_part.as_rule() == Rule::struct_methods {
                                        methods = Self::parse_struct_methods(brace_part)?;
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                Rule::struct_fields => {
                    fields = Self::parse_struct_fields(remaining_pair)?;
                }
                Rule::struct_methods => {
                    methods = Self::parse_struct_methods(remaining_pair)?;
                }
                _ => {} // Skip other rules like braces
            }
        }

        Ok(StructDefinition {
            attributes,
            name,
            generic_params,
            fields,
            methods,
            span,
        })
    }

    /// Parse struct fields
    pub(crate) fn parse_struct_fields(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<StructField>> {
        let mut fields = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::struct_field {
                let field = Self::parse_struct_field(inner_pair)?;
                fields.push(field);
            }
        }

        Ok(fields)
    }

    /// Parse a single struct field
    pub(crate) fn parse_struct_field(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<StructField> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        let name_pair = inner_pairs.next().unwrap();
        let name = Self::parse_identifier(name_pair)?;

        let type_pair = inner_pairs.next().unwrap();
        let type_annotation = Self::parse_type_annotation(type_pair)?;

        Ok(StructField {
            name,
            type_annotation,
            span,
        })
    }

    /// Parse struct methods
    pub(crate) fn parse_struct_methods(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<FunctionDefinition>> {
        let mut methods = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::struct_method_item {
                for method_item_pair in inner_pair.into_inner() {
                    match method_item_pair.as_rule() {
                        Rule::function_definition => {
                            let method = Self::parse_function_definition(method_item_pair)?;
                            methods.push(method);
                        }
                        Rule::comment => {
                            // Comments are pre-collected at program level - skip
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(methods)
    }

    /// Parse a trait definition
    pub(crate) fn parse_trait_definition(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<TraitDefinition> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // Parse attributes first
        let mut attributes = Vec::new();
        let mut trait_keyword_found = false;
        let mut name_pair: Option<pest::iterators::Pair<Rule>> = None;

        // Parse attributes and find the trait keyword and name
        for inner_pair in inner_pairs.by_ref() {
            match inner_pair.as_rule() {
                Rule::attribute => {
                    attributes.push(Self::parse_attribute(inner_pair)?);
                }
                Rule::keyword_trait => {
                    trait_keyword_found = true;
                }
                Rule::module_path if trait_keyword_found => {
                    name_pair = Some(inner_pair);
                    break;
                }
                _ => {}
            }
        }

        // Parse trait name
        let name = Self::parse_module_path(name_pair.unwrap())?;

        let mut generic_params = None;
        let mut constraints = None;
        let mut functions = Vec::new();

        for remaining_pair in inner_pairs {
            match remaining_pair.as_rule() {
                Rule::generic_params => {
                    generic_params = Some(Self::parse_generic_params(remaining_pair)?);
                }
                Rule::trait_constraints => {
                    constraints = Some(Self::parse_trait_constraints(remaining_pair)?);
                }
                Rule::trait_functions => {
                    functions = crate::OutrunParser::parse_trait_functions(remaining_pair)?;
                }
                _ => {} // Skip other rules like braces
            }
        }

        Ok(TraitDefinition {
            attributes,
            name,
            generic_params,
            constraints,
            functions,
            span,
        })
    }

    /// Parse an implementation block
    pub(crate) fn parse_impl_block(pair: pest::iterators::Pair<Rule>) -> ParseResult<ImplBlock> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // Skip "impl" keyword
        let _impl_keyword = inner_pairs.next().unwrap();

        let mut generic_params = None;
        let mut trait_spec = None;
        let mut type_spec = None;
        let mut constraints = None;
        let mut methods = Vec::new();

        for remaining_pair in inner_pairs {
            match remaining_pair.as_rule() {
                Rule::generic_params => {
                    generic_params = Some(Self::parse_generic_params(remaining_pair)?);
                }
                Rule::trait_spec => {
                    trait_spec = Some(Self::parse_type_spec(remaining_pair)?);
                }
                Rule::type_spec => {
                    type_spec = Some(Self::parse_type_spec(remaining_pair)?);
                }
                Rule::impl_constraints => {
                    constraints = Some(Self::parse_impl_constraints(remaining_pair)?);
                }
                Rule::impl_methods => {
                    methods = Self::parse_impl_methods(remaining_pair)?;
                }
                _ => {} // Skip other rules like "for" keyword and braces
            }
        }

        let trait_spec = trait_spec.ok_or_else(|| {
            ParseError::unexpected_token(
                "".to_string(),
                miette::SourceSpan::from(span.start..span.end),
                "Implementation block missing trait specification".to_string(),
            )
        })?;

        let type_spec = type_spec.ok_or_else(|| {
            ParseError::unexpected_token(
                "".to_string(),
                miette::SourceSpan::from(span.start..span.end),
                "Implementation block missing type specification".to_string(),
            )
        })?;

        Ok(ImplBlock {
            generic_params,
            trait_spec,
            type_spec,
            constraints,
            methods,
            span,
        })
    }

    /// Parse implementation methods
    pub(crate) fn parse_impl_methods(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<FunctionDefinition>> {
        let mut methods = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::impl_method_item {
                for method_item_pair in inner_pair.into_inner() {
                    match method_item_pair.as_rule() {
                        Rule::function_definition => {
                            let method = Self::parse_function_definition(method_item_pair)?;
                            methods.push(method);
                        }
                        Rule::comment => {
                            // Comments are pre-collected at program level - skip
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(methods)
    }

    /// Parse generic parameters
    pub(crate) fn parse_generic_params(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<GenericParams> {
        let span = Self::span_from_pair(&pair);
        let mut params = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::generic_param_list {
                for param_pair in inner_pair.into_inner() {
                    if param_pair.as_rule() == Rule::generic_param {
                        let param = Self::parse_generic_param(param_pair)?;
                        params.push(param);
                    }
                }
            }
        }

        Ok(GenericParams { params, span })
    }

    /// Parse a generic parameter
    pub(crate) fn parse_generic_param(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<GenericParam> {
        let span = Self::span_from_pair(&pair);
        let inner = pair.into_inner().next().unwrap();
        let name = Self::parse_type_identifier(inner)?;

        Ok(GenericParam { name, span })
    }

    /// Parse generic arguments
    pub(crate) fn parse_generic_args(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<GenericArgs> {
        let span = Self::span_from_pair(&pair);
        let mut args = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::generic_arg_list {
                for arg_pair in inner_pair.into_inner() {
                    if arg_pair.as_rule() == Rule::generic_arg {
                        let arg_inner = arg_pair.into_inner().next().unwrap();
                        let arg = Self::parse_type_annotation(arg_inner)?;
                        args.push(arg);
                    }
                }
            }
        }

        Ok(GenericArgs { args, span })
    }

    /// Parse type specification
    pub(crate) fn parse_type_spec(pair: pest::iterators::Pair<Rule>) -> ParseResult<TypeSpec> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // Parse module path
        let path_pair = inner_pairs.next().unwrap();
        let path = Self::parse_module_path(path_pair)?;

        // Parse optional generic arguments
        let mut generic_args = None;
        if let Some(args_pair) = inner_pairs.next() {
            if args_pair.as_rule() == Rule::generic_args {
                generic_args = Some(Self::parse_generic_args(args_pair)?);
            }
        }

        Ok(TypeSpec {
            path,
            generic_args,
            span,
        })
    }

    /// Parse trait constraints
    pub(crate) fn parse_trait_constraints(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<ConstraintExpression> {
        let mut inner_pairs = pair.into_inner();

        // Skip "when" keyword
        let _when_keyword = inner_pairs.next().unwrap();

        // Parse constraint expression
        let constraint_pair = inner_pairs.next().unwrap();
        Self::parse_constraint_expression(constraint_pair)
    }

    /// Parse implementation constraints
    pub(crate) fn parse_impl_constraints(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<ConstraintExpression> {
        let mut inner_pairs = pair.into_inner();

        // Skip "when" keyword
        let _when_keyword = inner_pairs.next().unwrap();

        // Parse constraint expression
        let constraint_pair = inner_pairs.next().unwrap();
        Self::parse_constraint_expression(constraint_pair)
    }

    /// Parse constraint expression
    pub(crate) fn parse_constraint_expression(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<ConstraintExpression> {
        match pair.as_rule() {
            Rule::constraint_expression => {
                let inner = pair.into_inner().next().unwrap();
                Self::parse_constraint_and(inner)
            }
            _ => Self::parse_constraint_and(pair),
        }
    }

    /// Parse constraint AND expression
    pub(crate) fn parse_constraint_and(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<ConstraintExpression> {
        let mut inner_pairs = pair.into_inner();

        let mut left = Self::parse_constraint_primary(inner_pairs.next().unwrap())?;

        // Handle remaining pairs - Pest should produce: constraint_primary ("&&" constraint_primary)*
        // But due to whitespace handling, we get: [constraint_primary, constraint_primary]
        // where the && is consumed by the grammar but not exposed as a separate token
        for next_pair in inner_pairs {
            // If this is a constraint_primary, it means we have an AND situation
            if next_pair.as_rule() == Rule::constraint_primary {
                let right = Self::parse_constraint_primary(next_pair)?;

                // Create a new span covering the current left and right
                let new_span = Self::span_from_range(left.span().start, right.span().end);

                left = ConstraintExpression::And {
                    left: Box::new(left),
                    right: Box::new(right),
                    span: new_span,
                };
            }
        }

        Ok(left)
    }

    /// Parse primary constraint
    pub(crate) fn parse_constraint_primary(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<ConstraintExpression> {
        let span = Self::span_from_pair(&pair);
        let mut inner_pairs = pair.into_inner();

        // The constraint_primary rule has a choice: type_identifier ~ ":" ~ module_path | "(" ~ constraint_expression ~ ")"
        let first = inner_pairs.next().unwrap();

        match first.as_rule() {
            Rule::type_identifier => {
                // Type constraint: T: Trait
                let type_param = Self::parse_type_identifier(first)?;

                // Parse trait bound (module path) - next pair should be module_path
                let trait_bound_pair = inner_pairs.next().unwrap();
                let trait_bound = Self::parse_module_path(trait_bound_pair)?;

                Ok(ConstraintExpression::Constraint {
                    type_param,
                    trait_bound,
                    span,
                })
            }
            Rule::constraint_expression => {
                // Parenthesized expression: ( constraint_expression )
                let inner_expr = Self::parse_constraint_expression(first)?;
                Ok(ConstraintExpression::Parenthesized {
                    expression: Box::new(inner_expr),
                    span,
                })
            }
            _ => unreachable!("Invalid constraint_primary rule: {:?}", first.as_rule()),
        }
    }
}
