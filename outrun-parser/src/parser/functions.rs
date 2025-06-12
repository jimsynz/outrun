// Function parsing module
// Handles function definitions, calls, parameters, anonymous functions, and function captures

use crate::ast::*;
use crate::error::*;
use crate::parser::{OutrunParser, Rule};

impl OutrunParser {
    /// Parse a function call from a Pest pair (expands shorthand arguments)
    pub(crate) fn parse_function_call(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<FunctionCall> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        // Parse function path from function_call_prefix
        let prefix_pair = inner_pairs.next().unwrap();
        let path_pair = prefix_pair.into_inner().next().unwrap(); // Extract function_path from prefix
        let path = Self::parse_function_path(path_pair)?;

        // Parse arguments (optional)
        let mut arguments = Vec::new();
        if let Some(args_pair) = inner_pairs.next() {
            if args_pair.as_rule() == Rule::argument_list {
                for arg_pair in args_pair.into_inner() {
                    if arg_pair.as_rule() == Rule::argument {
                        let arg = Self::parse_argument(arg_pair)?;
                        arguments.push(arg);
                    }
                }
            }
        }

        Ok(FunctionCall {
            path,
            arguments,
            span,
        })
    }

    /// Parse a function path (simple or qualified)
    pub(crate) fn parse_function_path(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<FunctionPath> {
        let mut inner_pairs = pair.into_inner();

        // Check if we have a module prefix
        let first_pair = inner_pairs.next().unwrap();

        if let Some(second_pair) = inner_pairs.next() {
            // Qualified path: Module.function
            let module = Self::parse_type_identifier(first_pair)?;
            let name = Self::parse_identifier(second_pair)?;
            Ok(FunctionPath::Qualified { module, name })
        } else {
            // Simple path: just function name
            let name = Self::parse_identifier(first_pair)?;
            Ok(FunctionPath::Simple { name })
        }
    }

    /// Parse a function argument (named, shorthand, or spread)
    pub(crate) fn parse_argument(pair: pest::iterators::Pair<Rule>) -> ParseResult<Argument> {
        let span = Self::extract_span(&pair);
        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::spread_argument => {
                // Spread argument: ..expr or ..?expr
                let mut inner_pairs = inner.into_inner();
                let spread_op_pair = inner_pairs.next().unwrap();
                let expr_pair = inner_pairs.next().unwrap();

                // Determine spread kind based on operator
                let kind = match spread_op_pair.as_str() {
                    ".." => SpreadKind::Strict,
                    "..?" => SpreadKind::Lenient,
                    _ => unreachable!("Invalid spread operator"),
                };

                let expression = Self::parse_expression_from_pair(expr_pair)?;

                Ok(Argument::Spread {
                    expression,
                    kind,
                    span,
                })
            }
            Rule::named_argument => {
                // Explicit argument: name: expression
                let mut inner_pairs = inner.into_inner();
                let name_pair = inner_pairs.next().unwrap();
                let expr_pair = inner_pairs.next().unwrap();

                let name = Self::parse_identifier(name_pair)?;
                let expression = Self::parse_expression_from_pair(expr_pair)?;

                Ok(Argument::Named {
                    name,
                    expression,
                    format: ArgumentFormat::Explicit,
                    span,
                })
            }
            Rule::shorthand_argument => {
                // Shorthand argument: expand `arg` to `arg: arg`
                let name = Self::parse_identifier(inner)?;

                // Create an identifier expression with the same name
                let expression = Expression {
                    kind: ExpressionKind::Identifier(name.clone()),
                    span: name.span.clone(),
                };

                Ok(Argument::Named {
                    name,
                    expression,
                    format: ArgumentFormat::Shorthand,
                    span,
                })
            }
            _ => unreachable!("Invalid argument rule"),
        }
    }

    /// Parse a function definition from a Pest pair
    pub(crate) fn parse_function_definition(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<FunctionDefinition> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        // Parse attributes first
        let mut attributes = Vec::new();
        let mut visibility_pair: Option<pest::iterators::Pair<Rule>> = None;

        // Parse attributes and find the visibility and name
        for inner_pair in inner_pairs.by_ref() {
            match inner_pair.as_rule() {
                Rule::attribute => {
                    attributes.push(Self::parse_attribute(inner_pair)?);
                }
                Rule::function_visibility => {
                    visibility_pair = Some(inner_pair);
                    break;
                }
                _ => {}
            }
        }

        // Parse function visibility (def or defp)
        let visibility = Self::parse_function_visibility(visibility_pair.unwrap())?;

        // Parse function name
        let name_pair = inner_pairs.next().unwrap();
        let name = Self::parse_identifier(name_pair)?;

        // Parse parameters (optional)
        let mut parameters = Vec::new();
        let mut return_type = None;
        let mut guard = None;
        let mut body = None;

        for remaining_pair in inner_pairs {
            match remaining_pair.as_rule() {
                Rule::parameter_list => {
                    parameters = Self::parse_parameter_list(remaining_pair)?;
                }
                Rule::return_type => {
                    return_type = Some(Self::parse_return_type(remaining_pair)?);
                }
                Rule::guard_clause => {
                    guard = Some(Self::parse_guard_clause(remaining_pair)?);
                }
                Rule::block => {
                    body = Some(Self::parse_block(remaining_pair)?);
                    break; // Block should be last
                }
                _ => {} // Skip other rules
            }
        }

        let body = body.ok_or_else(|| {
            ParseError::unexpected_token(
                "".to_string(),
                miette::SourceSpan::from(span.start..span.end),
                "Function definition missing body block".to_string(),
            )
        })?;

        Ok(FunctionDefinition {
            attributes,
            visibility,
            name,
            parameters,
            return_type,
            guard,
            body,
            span,
        })
    }

    /// Parse function visibility (def or defp)
    pub(crate) fn parse_function_visibility(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<FunctionVisibility> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::keyword_def => Ok(FunctionVisibility::Public),
            Rule::keyword_defp => Ok(FunctionVisibility::Private),
            _ => unreachable!("Invalid function visibility rule"),
        }
    }

    /// Parse parameter list from a Pest pair
    pub(crate) fn parse_parameter_list(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<Parameter>> {
        let mut parameters = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::parameters {
                for param_pair in inner_pair.into_inner() {
                    if param_pair.as_rule() == Rule::parameter {
                        let parameter = Self::parse_parameter(param_pair)?;
                        parameters.push(parameter);
                    }
                }
            }
        }

        Ok(parameters)
    }

    /// Parse a single parameter
    pub(crate) fn parse_parameter(pair: pest::iterators::Pair<Rule>) -> ParseResult<Parameter> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        let name_pair = inner_pairs.next().unwrap();
        let name = Self::parse_identifier(name_pair)?;

        let type_pair = inner_pairs.next().unwrap();
        let type_annotation = Self::parse_type_annotation(type_pair)?;

        Ok(Parameter {
            name,
            type_annotation,
            span,
        })
    }

    /// Parse function type: Function<(param: Type) -> ReturnType>
    pub(crate) fn parse_function_type(
        pair: pest::iterators::Pair<Rule>,
        span: Span,
    ) -> ParseResult<TypeAnnotation> {
        let inner = pair.into_inner();

        // Skip "Function" keyword (already consumed by grammar)

        // Look for function_type_params and return type
        let mut params = Vec::new();
        let mut return_type = None;

        for inner_pair in inner {
            match inner_pair.as_rule() {
                Rule::function_type_params => {
                    params = Self::parse_function_type_params(inner_pair)?;
                }
                Rule::type_annotation => {
                    // This is the return type (after ->)
                    return_type = Some(Box::new(Self::parse_type_annotation(inner_pair)?));
                }
                _ => {} // Skip other tokens like "<", "(", ")", "->", ">"
            }
        }

        let return_type = return_type.ok_or_else(|| {
            ParseError::unexpected_token(
                "missing return type".to_string(),
                miette::SourceSpan::from(span.start..span.end),
                "Function type must have a return type".to_string(),
            )
        })?;

        Ok(TypeAnnotation::Function {
            params,
            return_type,
            span,
        })
    }

    /// Parse function type parameters list
    pub(crate) fn parse_function_type_params(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<FunctionTypeParam>> {
        let mut params = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::function_type_param {
                params.push(Self::parse_function_type_param(inner_pair)?);
            }
        }

        Ok(params)
    }

    /// Parse individual function type parameter: name: Type
    pub(crate) fn parse_function_type_param(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<FunctionTypeParam> {
        let span = Self::extract_span(&pair);
        let mut inner = pair.into_inner();

        let name_pair = inner.next().ok_or_else(|| {
            ParseError::unexpected_token(
                "missing parameter name".to_string(),
                miette::SourceSpan::from(span.start..span.end),
                "Function type parameter must have a name".to_string(),
            )
        })?;

        let type_pair = inner.next().ok_or_else(|| {
            ParseError::unexpected_token(
                "missing parameter type".to_string(),
                miette::SourceSpan::from(span.start..span.end),
                "Function type parameter must have a type annotation".to_string(),
            )
        })?;

        let name = Self::parse_identifier(name_pair)?;
        let type_annotation = Self::parse_type_annotation(type_pair)?;

        Ok(FunctionTypeParam {
            name,
            type_annotation,
            span,
        })
    }

    /// Parse trait functions
    pub(crate) fn parse_trait_functions(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<TraitFunction>> {
        let mut functions = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::trait_item {
                for trait_item_pair in inner_pair.into_inner() {
                    match trait_item_pair.as_rule() {
                        Rule::trait_function => {
                            let trait_function = Self::parse_trait_function(trait_item_pair)?;
                            functions.push(trait_function);
                        }
                        Rule::comment => {
                            // Comments are pre-collected at program level - skip
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(functions)
    }

    /// Parse a trait function (signature or definition)
    pub(crate) fn parse_trait_function(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<TraitFunction> {
        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::function_signature => {
                let signature = Self::parse_function_signature(inner)?;
                Ok(TraitFunction::Signature(signature))
            }
            Rule::function_definition => {
                let definition = Self::parse_function_definition(inner)?;
                Ok(TraitFunction::Definition(definition))
            }
            _ => unreachable!("Invalid trait function rule"),
        }
    }

    /// Parse a function signature
    pub(crate) fn parse_function_signature(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<FunctionSignature> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        // Parse function visibility
        let visibility_pair = inner_pairs.next().unwrap();
        let visibility = Self::parse_function_visibility(visibility_pair)?;

        // Parse function name
        let name_pair = inner_pairs.next().unwrap();
        let name = Self::parse_identifier(name_pair)?;

        let mut parameters = Vec::new();
        let mut return_type = None;
        let mut guard = None;

        for remaining_pair in inner_pairs {
            match remaining_pair.as_rule() {
                Rule::parameter_list => {
                    parameters = Self::parse_parameter_list(remaining_pair)?;
                }
                Rule::return_type => {
                    return_type = Some(Self::parse_return_type(remaining_pair)?);
                }
                Rule::guard_clause => {
                    guard = Some(Self::parse_guard_clause(remaining_pair)?);
                }
                _ => {} // Skip other rules
            }
        }

        Ok(FunctionSignature {
            visibility,
            name,
            parameters,
            return_type,
            guard,
            span,
        })
    }

    /// Parse import function list
    pub(crate) fn parse_import_function_list(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<Vec<ImportFunctionSpec>> {
        let mut functions = Vec::new();
        let mut inner_pairs = pair.into_inner();

        // Parse function list (only child of import_only_clause/import_except_clause)
        let list_pair = inner_pairs.next().unwrap();
        for inner_pair in list_pair.into_inner() {
            if inner_pair.as_rule() == Rule::import_function_spec {
                let function_spec = Self::parse_import_function_spec(inner_pair)?;
                functions.push(function_spec);
            }
        }

        Ok(functions)
    }

    /// Parse import function specification
    pub(crate) fn parse_import_function_spec(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<ImportFunctionSpec> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        // Parse function name
        let name_pair = inner_pairs.next().unwrap();
        let name = Self::parse_identifier(name_pair)?;

        // Parse arity (integer)
        let arity_pair = inner_pairs.next().unwrap();
        let arity = arity_pair.as_str().parse::<i64>().map_err(|_| {
            ParseError::invalid_integer(
                "".to_string(),
                miette::SourceSpan::from(span.start..span.end),
                arity_pair.as_str().to_string(),
            )
        })?;

        Ok(ImportFunctionSpec { name, arity, span })
    }

    /// Parse an anonymous function: fn { x: Integer -> x + 1 }
    pub(crate) fn parse_anonymous_function(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<AnonymousFunction> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        // Skip the 'fn' keyword (first token)
        let _keyword_pair = inner_pairs.next().unwrap();

        // Find and parse the clauses, skipping newlines
        let mut clauses = Vec::new();
        for pair in inner_pairs {
            match pair.as_rule() {
                Rule::anonymous_clauses => {
                    for clause_pair in pair.into_inner() {
                        if clause_pair.as_rule() == Rule::anonymous_clause {
                            let clause = Self::parse_anonymous_clause(clause_pair)?;
                            clauses.push(clause);
                        }
                    }
                }
                _ => {
                    // Ignore other tokens
                }
            }
        }

        Ok(AnonymousFunction { clauses, span })
    }

    /// Parse a single anonymous function clause
    pub(crate) fn parse_anonymous_clause(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<AnonymousClause> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        // Parse parameters
        let params_pair = inner_pairs.next().unwrap();
        let parameters = Self::parse_anonymous_parameters(params_pair)?;

        // Parse optional guard clause
        let mut guard = None;
        let mut body_pair = None;

        for inner_pair in inner_pairs {
            match inner_pair.as_rule() {
                Rule::guard_clause => {
                    let guard_clause = Self::parse_guard_clause(inner_pair)?;
                    guard = Some(guard_clause.condition);
                }
                Rule::arrow => {
                    // Skip arrow, next should be body
                }
                Rule::anonymous_body => {
                    body_pair = Some(inner_pair);
                    break;
                }
                _ => {
                    return Err(ParseError::unexpected_token(
                        "".to_string(),
                        miette::SourceSpan::from(
                            inner_pair.as_span().start()..inner_pair.as_span().end(),
                        ),
                        format!(
                            "Unexpected rule in anonymous clause: {:?}",
                            inner_pair.as_rule()
                        ),
                    ));
                }
            }
        }

        // Parse the body
        let body = if let Some(body_pair) = body_pair {
            Self::parse_anonymous_body(body_pair)?
        } else {
            return Err(ParseError::unexpected_token(
                "".to_string(),
                miette::SourceSpan::from(span.start..span.end),
                "Missing body in anonymous function clause".to_string(),
            ));
        };

        Ok(AnonymousClause {
            parameters,
            guard,
            body,
            span,
        })
    }

    /// Parse anonymous function body
    pub(crate) fn parse_anonymous_body(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<AnonymousBody> {
        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::block => {
                let block = Self::parse_block(inner)?;
                Ok(AnonymousBody::Block(block))
            }
            Rule::expression => {
                let expr = Self::parse_expression_from_pair(inner)?;
                Ok(AnonymousBody::Expression(Box::new(expr)))
            }
            _ => Err(ParseError::unexpected_token(
                "".to_string(),
                miette::SourceSpan::from(inner.as_span().start()..inner.as_span().end()),
                format!("Unexpected rule in anonymous body: {:?}", inner.as_rule()),
            )),
        }
    }

    /// Parse anonymous function parameters
    pub(crate) fn parse_anonymous_parameters(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<AnonymousParameters> {
        let span = Self::extract_span(&pair);
        let input_str = pair.as_str();

        // Handle the special case of empty parentheses
        if input_str == "()" {
            return Ok(AnonymousParameters::None { span });
        }

        let inner_pairs = pair.into_inner();

        // Check if it starts with a parenthesis (multiple parameters)
        if input_str.starts_with("(") {
            // Multiple parameters: (param1, param2, ...)
            let mut parameters = Vec::new();

            for inner_pair in inner_pairs {
                match inner_pair.as_rule() {
                    Rule::parameter => {
                        let param = Self::parse_parameter(inner_pair)?;
                        parameters.push(param);
                    }
                    _ => {
                        // Skip punctuation like "(", "," and ")"
                    }
                }
            }

            Ok(AnonymousParameters::Multiple { parameters, span })
        } else {
            // Single parameter: identifier : type_annotation
            let mut inner_pairs = inner_pairs;

            let identifier_pair = inner_pairs.next().ok_or_else(|| {
                ParseError::unexpected_token(
                    "".to_string(),
                    miette::SourceSpan::from(span.start..span.end),
                    "Expected identifier in single parameter".to_string(),
                )
            })?;
            let identifier_span = Self::extract_span(&identifier_pair);
            let identifier = Self::parse_identifier(identifier_pair)?;

            // Skip colon (it's a literal, not a pair)

            // Parse type annotation
            let type_pair = inner_pairs.next().ok_or_else(|| {
                ParseError::unexpected_token(
                    "".to_string(),
                    miette::SourceSpan::from(span.start..span.end),
                    "Expected type annotation after colon in single parameter".to_string(),
                )
            })?;
            let type_span = Self::extract_span(&type_pair);
            let type_annotation = Self::parse_type_annotation(type_pair)?;

            let parameter = Parameter {
                name: identifier,
                type_annotation,
                span: Self::span_from_range(identifier_span.start, type_span.end),
            };

            Ok(AnonymousParameters::Single { parameter, span })
        }
    }

    /// Parse a function capture: &function or &Module.function/2
    pub(crate) fn parse_function_capture(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<FunctionCapture> {
        let span = Self::extract_span(&pair);
        let mut inner_pairs = pair.into_inner();

        // Skip the '&' symbol (first token)
        let qualified_ref_pair = inner_pairs.next().unwrap();
        let (module_path, function_name, arity) =
            Self::parse_qualified_function_ref(qualified_ref_pair)?;

        Ok(FunctionCapture {
            module_path,
            function_name,
            arity,
            span,
        })
    }

    /// Parse a qualified function reference for captures
    pub(crate) fn parse_qualified_function_ref(
        pair: pest::iterators::Pair<Rule>,
    ) -> ParseResult<(Option<Vec<TypeIdentifier>>, Identifier, Option<i32>)> {
        let pair_span = Self::extract_span(&pair);
        let inner_pairs = pair.into_inner();
        let mut module_path = None;
        let mut function_name = None;
        let mut arity = None;

        for inner_pair in inner_pairs {
            match inner_pair.as_rule() {
                Rule::module_path => {
                    let path = Self::parse_module_path(inner_pair)?;
                    module_path = Some(path);
                }
                Rule::identifier => {
                    let id = Self::parse_identifier(inner_pair)?;
                    function_name = Some(id);
                }
                Rule::integer_decimal => {
                    let arity_str = inner_pair.as_str();
                    arity = Some(arity_str.parse::<i32>().map_err(|_| {
                        ParseError::unexpected_token(
                            "".to_string(),
                            miette::SourceSpan::from(
                                inner_pair.as_span().start()..inner_pair.as_span().end(),
                            ),
                            format!("Invalid arity number: {}", arity_str),
                        )
                    })?);
                }
                _ => {
                    // Skip other tokens like '/' separator
                }
            }
        }

        let function_name = function_name.ok_or_else(|| {
            ParseError::unexpected_token(
                "".to_string(),
                miette::SourceSpan::from(pair_span.start..pair_span.end),
                "Missing function name in function capture".to_string(),
            )
        })?;

        Ok((module_path, function_name, arity))
    }
}
