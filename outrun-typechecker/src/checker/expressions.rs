//! Expression type checking
//!
//! This module handles type checking for all expression types including:
//! - Literal expressions (integers, strings, booleans, etc.)
//! - Binary and unary operations with trait dispatch
//! - Function calls with parameter validation
//! - Collection literals (lists, maps, tuples)

// use crate::types::{TypeId, ConcreteType}; // TODO: Use when needed
use crate::checker::{
    TypeContext, TypedExpression, TypedExpressionKind, TypedTraitCaseClause, Variable,
};
use crate::error::{SpanExt, TypeError, TypeResult};
use crate::types::{AtomId, TypeId};
use outrun_parser::{Expression, ExpressionKind, Span};

/// Information about a function type for function value calls
#[derive(Debug, Clone)]
struct FunctionTypeInfo {
    pub parameters: Vec<(String, TypeId)>, // (param_name, param_type)
    pub return_type: TypeId,
}

/// Expression type checker
pub struct ExpressionChecker;

impl ExpressionChecker {
    /// Type check an expression and return a typed expression
    pub fn check_expression(
        context: &mut TypeContext,
        expr: &Expression,
    ) -> TypeResult<TypedExpression> {
        match &expr.kind {
            ExpressionKind::Integer(int_lit) => {
                Self::check_integer_literal(context, int_lit, expr.span)
            }
            ExpressionKind::Float(float_lit) => {
                Self::check_float_literal(context, float_lit, expr.span)
            }
            ExpressionKind::String(str_lit) => {
                Self::check_string_literal(context, str_lit, expr.span)
            }
            ExpressionKind::Boolean(bool_lit) => {
                Self::check_boolean_literal(context, bool_lit, expr.span)
            }
            ExpressionKind::Atom(atom_lit) => {
                Self::check_atom_literal(context, atom_lit, expr.span)
            }
            ExpressionKind::Identifier(ident) => Self::check_identifier(context, ident),
            ExpressionKind::BinaryOp(bin_op) => {
                Self::check_binary_operation(context, &bin_op.left, &bin_op.operator, &bin_op.right)
            }
            ExpressionKind::FunctionCall(call) => Self::check_function_call(context, call),
            ExpressionKind::FunctionCapture(capture) => {
                Self::check_function_capture(context, capture)
            }
            ExpressionKind::List(list_lit) => Self::check_list_literal(context, list_lit),
            ExpressionKind::Tuple(tuple_lit) => Self::check_tuple_literal(context, tuple_lit),
            ExpressionKind::Map(map_lit) => Self::check_map_literal(context, map_lit),
            ExpressionKind::Struct(struct_lit) => Self::check_struct_literal(context, struct_lit),
            ExpressionKind::IfExpression(if_expr) => Self::check_if_expression(context, if_expr),
            ExpressionKind::CaseExpression(case_expr) => {
                Self::check_case_expression(context, case_expr)
            }
            ExpressionKind::UnaryOp(unary_op) => Self::check_unary_operation(context, unary_op),
            ExpressionKind::FieldAccess(field_access) => {
                Self::check_field_access(context, field_access)
            }
            ExpressionKind::QualifiedIdentifier(qualified_id) => {
                Self::check_qualified_identifier(context, qualified_id)
            }
            ExpressionKind::Parenthesized(inner_expr) => {
                // Parenthesized expressions are transparent - just check the inner expression
                let typed_inner = Self::check_expression(context, inner_expr)?;
                Ok(TypedExpression {
                    kind: TypedExpressionKind::Parenthesized(Box::new(typed_inner.clone())),
                    type_id: typed_inner.type_id,
                    span: expr.span,
                })
            }
            ExpressionKind::TypeIdentifier(type_id) => {
                Self::check_type_identifier(context, type_id)
            }
            ExpressionKind::AnonymousFunction(anon_fn) => {
                Self::check_anonymous_function(context, anon_fn)
            }
            ExpressionKind::Sigil(sigil_lit) => {
                Self::check_sigil_literal(context, sigil_lit, expr.span)
            }
            ExpressionKind::MacroInjection(macro_injection) => {
                Self::check_macro_injection(context, macro_injection, expr.span)
            }
        }
    }

    /// Type check integer literal
    fn check_integer_literal(
        context: &mut TypeContext,
        int_lit: &outrun_parser::IntegerLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Integer literals default to Outrun.Core.Integer64 type
        let type_id = context.integer_type();

        Ok(TypedExpression {
            kind: TypedExpressionKind::Integer(int_lit.value),
            type_id,
            span,
        })
    }

    /// Type check float literal
    fn check_float_literal(
        context: &mut TypeContext,
        float_lit: &outrun_parser::FloatLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Float literals default to Outrun.Core.Float64 type
        let type_id = context.float_type();

        Ok(TypedExpression {
            kind: TypedExpressionKind::Float(float_lit.value),
            type_id,
            span,
        })
    }

    /// Type check string literal
    fn check_string_literal(
        context: &mut TypeContext,
        str_lit: &outrun_parser::StringLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // String literals are Outrun.Core.String type
        let type_id = context.interner.intern_type("Outrun.Core.String");

        // Process all parts, validating interpolated expressions
        let mut processed_content = String::new();
        for part in &str_lit.parts {
            match part {
                outrun_parser::StringPart::Text { content, .. } => {
                    processed_content.push_str(content);
                }
                outrun_parser::StringPart::Interpolation { expression, span } => {
                    // Type check the interpolated expression
                    let typed_expr = Self::check_expression(context, expression)?;

                    // Validate that the expression type implements Display trait
                    if !Self::implements_display_trait(context, typed_expr.type_id) {
                        let type_name = context.type_name_or_unknown(typed_expr.type_id);
                        return Err(TypeError::string_interpolation_display(
                            type_name,
                            span.to_source_span(),
                        ));
                    }

                    // For reconstruction purposes, use placeholder
                    processed_content.push_str("#{...}");
                }
            }
        }

        Ok(TypedExpression {
            kind: TypedExpressionKind::String(processed_content),
            type_id,
            span,
        })
    }

    /// Type check atom literal
    fn check_atom_literal(
        context: &mut TypeContext,
        atom_lit: &outrun_parser::AtomLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Atom literals are Outrun.Core.Atom type
        let type_id = context.interner.intern_type("Outrun.Core.Atom");

        Ok(TypedExpression {
            kind: TypedExpressionKind::Atom(atom_lit.content.clone()),
            type_id,
            span,
        })
    }

    /// Type check boolean literal
    fn check_boolean_literal(
        context: &mut TypeContext,
        bool_lit: &outrun_parser::BooleanLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Boolean literals are Outrun.Core.Boolean type
        let type_id = context.interner.intern_type("Outrun.Core.Boolean");

        Ok(TypedExpression {
            kind: TypedExpressionKind::Boolean(bool_lit.value),
            type_id,
            span,
        })
    }

    /// Type check identifier (variable reference)
    fn check_identifier(
        context: &mut TypeContext,
        ident: &outrun_parser::Identifier,
    ) -> TypeResult<TypedExpression> {
        // Look up the identifier in the current scope
        if let Some(variable) = context.lookup_variable(&ident.name) {
            Ok(TypedExpression {
                kind: TypedExpressionKind::Identifier(ident.name.clone()),
                type_id: variable.type_id,
                span: ident.span,
            })
        } else {
            Err(TypeError::undefined_variable(
                ident.name.clone(),
                crate::error::span_to_source_span(ident.span),
            ))
        }
    }

    /// Type check binary operation
    fn check_binary_operation(
        context: &mut TypeContext,
        left: &Expression,
        operator: &outrun_parser::BinaryOperator,
        right: &Expression,
    ) -> TypeResult<TypedExpression> {
        // Type check operands first
        let typed_left = Self::check_expression(context, left)?;
        let typed_right = Self::check_expression(context, right)?;

        // For now, implement basic arithmetic and comparison operations
        // TODO: Replace with proper trait dispatch system
        let result_type = match operator {
            // Arithmetic operations - for now assume same type in/out
            outrun_parser::BinaryOperator::Add
            | outrun_parser::BinaryOperator::Subtract
            | outrun_parser::BinaryOperator::Multiply
            | outrun_parser::BinaryOperator::Divide
            | outrun_parser::BinaryOperator::Modulo => {
                // Check operands are the same numeric type
                if typed_left.type_id != typed_right.type_id {
                    return Err(context.type_mismatch_error(
                        typed_left.type_id,
                        typed_right.type_id,
                        right.span,
                    ));
                }

                // TODO: Validate types support the operation via traits
                typed_left.type_id
            }

            // Comparison operations - return Boolean
            outrun_parser::BinaryOperator::Equal
            | outrun_parser::BinaryOperator::NotEqual
            | outrun_parser::BinaryOperator::Less
            | outrun_parser::BinaryOperator::LessEqual
            | outrun_parser::BinaryOperator::Greater
            | outrun_parser::BinaryOperator::GreaterEqual => {
                // TODO: Validate types support comparison via traits
                context.interner.intern_type("Outrun.Core.Boolean")
            }

            // Logical operations - require Boolean operands, return Boolean
            outrun_parser::BinaryOperator::LogicalAnd
            | outrun_parser::BinaryOperator::LogicalOr => {
                let bool_type = context.interner.intern_type("Outrun.Core.Boolean");
                if typed_left.type_id != bool_type || typed_right.type_id != bool_type {
                    return Err(TypeError::type_mismatch(
                        "Outrun.Core.Boolean".to_string(),
                        context
                            .get_type_name(if typed_left.type_id != bool_type {
                                typed_left.type_id
                            } else {
                                typed_right.type_id
                            })
                            .unwrap_or("Unknown")
                            .to_string(),
                        crate::error::span_to_source_span(if typed_left.type_id != bool_type {
                            left.span
                        } else {
                            right.span
                        }),
                    ));
                }
                bool_type
            }

            // Pipe operators - trait-based operations
            outrun_parser::BinaryOperator::Pipe => {
                // |> calls Pipe.pipe_into(left_value, right_function)
                // The right side should be a function that accepts the left side's type
                Self::check_pipe_operation(context, &typed_left, &typed_right, left, right)?
            }

            outrun_parser::BinaryOperator::PipeMaybe => {
                // |? calls Maybe.maybe_pipe(left_value, right_function)
                // The left side should be an Option type, right side a function
                Self::check_pipe_maybe_operation(context, &typed_left, &typed_right, left, right)?
            }

            _ => {
                return Err(TypeError::UnimplementedFeature {
                    feature: format!("Binary operator {:?}", operator),
                    span: crate::error::span_to_source_span(left.span),
                });
            }
        };

        Ok(TypedExpression {
            kind: TypedExpressionKind::BinaryOp {
                left: Box::new(typed_left),
                operator: operator.clone(),
                right: Box::new(typed_right),
            },
            type_id: result_type,
            span: outrun_parser::Span::new(left.span.start, right.span.end),
        })
    }

    /// Type check function call
    fn check_function_call(
        context: &mut TypeContext,
        call: &outrun_parser::FunctionCall,
    ) -> TypeResult<TypedExpression> {
        // Extract function name and span from path
        let (function_name, module_path, path_span) = match &call.path {
            outrun_parser::FunctionPath::Simple { name } => (name.name.clone(), None, name.span),
            outrun_parser::FunctionPath::Qualified { module, name } => {
                (name.name.clone(), Some(module.name.clone()), name.span)
            }
            outrun_parser::FunctionPath::Expression { expression } => {
                return Err(TypeError::UnimplementedFeature {
                    feature: "Expression function calls (captured functions)".to_string(),
                    span: crate::error::span_to_source_span(expression.span),
                });
            }
        };

        // Type check arguments first (needed for overload resolution)
        let mut typed_args = Vec::new();

        for arg in &call.arguments {
            match arg {
                outrun_parser::Argument::Named {
                    name,
                    expression,
                    format,
                    span,
                } => match format {
                    outrun_parser::ArgumentFormat::Explicit => {
                        let typed_expr = Self::check_expression(context, expression)?;
                        typed_args.push((name.name.clone(), typed_expr));
                    }
                    outrun_parser::ArgumentFormat::Shorthand => {
                        return Err(TypeError::UnimplementedFeature {
                            feature: "Shorthand function arguments".to_string(),
                            span: crate::error::span_to_source_span(*span),
                        });
                    }
                },
                outrun_parser::Argument::Spread { span, .. } => {
                    return Err(TypeError::UnimplementedFeature {
                        feature: "Spread function arguments".to_string(),
                        span: crate::error::span_to_source_span(*span),
                    });
                }
            }
        }

        // Look up the function - handle both simple and qualified calls
        let function_return_type = if let Some(module) = &module_path {
            // Qualified function call: Module.function_name()
            // This is treated as a static function call on the module/type
            Self::lookup_qualified_function(context, module, &function_name)?
        } else {
            // Simple function call: function_name()
            // First check if this is a function variable (function value call)
            if let Some(variable) = context.lookup_variable(&function_name) {
                // Check if this variable has a function type
                if let Some(function_type_info) =
                    Self::parse_function_type(context, variable.type_id)?
                {
                    return Self::validate_function_value_call(
                        context,
                        &function_type_info,
                        &typed_args,
                        call.span,
                    );
                }
            }

            // Not a function variable, try static function overload resolution
            if let Some(function) = context.resolve_function_overload(&function_name, &typed_args) {
                function.return_type
            } else {
                return Err(TypeError::undefined_function(
                    function_name,
                    crate::error::span_to_source_span(path_span),
                ));
            }
        };

        // Validate argument types match function parameters
        // Skip parameter validation for qualified function calls for now
        // TODO: Implement comprehensive parameter validation for qualified calls
        if module_path.is_none() {
            let function_def = context
                .resolve_function_overload(&function_name, &typed_args)
                .ok_or_else(|| {
                    TypeError::undefined_function(
                        function_name.clone(),
                        crate::error::span_to_source_span(path_span),
                    )
                })?;

            // Check that all required parameters are provided
            let required_param_names: std::collections::HashSet<_> = function_def
                .params
                .iter()
                .map(|(atom_id, _)| {
                    context
                        .interner
                        .resolve_atom(*atom_id)
                        .unwrap_or("<unknown>")
                })
                .collect();

            let provided_param_names: std::collections::HashSet<_> =
                typed_args.iter().map(|(name, _)| name.as_str()).collect();

            // Check for missing required parameters
            for required_param in &required_param_names {
                if !provided_param_names.contains(required_param) {
                    return Err(TypeError::MissingParameter {
                        function_name: function_name.clone(),
                        parameter_name: required_param.to_string(),
                        span: crate::error::span_to_source_span(call.span),
                    });
                }
            }

            // Check for unexpected parameters
            for provided_param in &provided_param_names {
                if !required_param_names.contains(provided_param) {
                    return Err(TypeError::UnexpectedParameter {
                        function_name: function_name.clone(),
                        parameter_name: provided_param.to_string(),
                        span: crate::error::span_to_source_span(call.span),
                    });
                }
            }

            // Validate argument types match parameter types
            for (arg_name, typed_arg) in &typed_args {
                // Find the parameter type for this argument
                let param_type = function_def
                    .params
                    .iter()
                    .find(|(atom_id, _)| {
                        context
                            .interner
                            .resolve_atom(*atom_id)
                            .unwrap_or("<unknown>")
                            == arg_name
                    })
                    .map(|(_, type_id)| *type_id);

                if let Some(expected_type) = param_type {
                    if typed_arg.type_id != expected_type {
                        return Err(TypeError::type_mismatch(
                            context
                                .get_type_name(expected_type)
                                .unwrap_or("Unknown")
                                .to_string(),
                            context
                                .get_type_name(typed_arg.type_id)
                                .unwrap_or("Unknown")
                                .to_string(),
                            crate::error::span_to_source_span(typed_arg.span),
                        ));
                    }
                }
            }
        }

        Ok(TypedExpression {
            kind: TypedExpressionKind::FunctionCall {
                name: function_name,
                args: typed_args,
            },
            type_id: function_return_type,
            span: call.span,
        })
    }

    /// Type check list literal
    fn check_list_literal(
        context: &mut TypeContext,
        list_lit: &outrun_parser::ListLiteral,
    ) -> TypeResult<TypedExpression> {
        let mut typed_elements = Vec::new();
        let mut element_type: Option<TypeId> = None;

        // Process each list element
        for list_element in &list_lit.elements {
            match list_element {
                outrun_parser::ListElement::Expression(expr) => {
                    let typed_expr = Self::check_expression(context, expr)?;

                    // For homogeneous lists, all elements must be the same type
                    match element_type {
                        None => {
                            // First element establishes the type
                            element_type = Some(typed_expr.type_id);
                        }
                        Some(expected_type) => {
                            // Subsequent elements must match
                            if typed_expr.type_id != expected_type {
                                return Err(TypeError::type_mismatch(
                                    context
                                        .get_type_name(expected_type)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    context
                                        .get_type_name(typed_expr.type_id)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    crate::error::span_to_source_span(expr.span),
                                ));
                            }
                        }
                    }

                    typed_elements.push(typed_expr);
                }
                outrun_parser::ListElement::Spread(_) => {
                    return Err(TypeError::UnimplementedFeature {
                        feature: "List spread operator".to_string(),
                        span: crate::error::span_to_source_span(list_lit.span),
                    });
                }
            }
        }

        // Determine final list type
        let final_element_type = match element_type {
            Some(element_type) => element_type,
            None => {
                // Empty list - should fail type checking unless there's type annotation from context
                return Err(TypeError::UnimplementedFeature {
                    feature: "Empty list literals require type annotation context".to_string(),
                    span: crate::error::span_to_source_span(list_lit.span),
                });
            }
        };

        // Create the list type: Outrun.Core.List<ElementType>
        // For now, we'll just intern it as a string until we have proper generic support
        let element_type_name = context
            .get_type_name(final_element_type)
            .unwrap_or("Unknown");
        let list_type_name = if element_type_name == "Unknown" {
            "Outrun.Core.List".to_string()
        } else {
            format!("Outrun.Core.List<{}>", element_type_name)
        };
        let list_type_id = context.interner.intern_type(&list_type_name);

        Ok(TypedExpression {
            kind: TypedExpressionKind::List {
                elements: typed_elements,
                element_type: final_element_type,
            },
            type_id: list_type_id,
            span: list_lit.span,
        })
    }

    /// Type check tuple literal
    fn check_tuple_literal(
        context: &mut TypeContext,
        tuple_lit: &outrun_parser::TupleLiteral,
    ) -> TypeResult<TypedExpression> {
        let mut typed_elements = Vec::new();
        let mut element_types = Vec::new();

        // Type check each tuple element
        for expr in &tuple_lit.elements {
            let typed_expr = Self::check_expression(context, expr)?;
            element_types.push(typed_expr.type_id);
            typed_elements.push(typed_expr);
        }

        // Create the tuple type: Outrun.Core.Tuple<T1, T2, ...>
        let type_names: Vec<String> = element_types
            .iter()
            .map(|&type_id| {
                context
                    .get_type_name(type_id)
                    .unwrap_or("Unknown")
                    .to_string()
            })
            .collect();
        let tuple_type_name = if type_names.is_empty() {
            "Outrun.Core.Tuple<>".to_string()
        } else {
            format!("Outrun.Core.Tuple<{}>", type_names.join(", "))
        };
        let tuple_type_id = context.interner.intern_type(&tuple_type_name);

        Ok(TypedExpression {
            kind: TypedExpressionKind::Tuple {
                elements: typed_elements,
                element_types,
            },
            type_id: tuple_type_id,
            span: tuple_lit.span,
        })
    }

    /// Type check map literal
    fn check_map_literal(
        context: &mut TypeContext,
        map_lit: &outrun_parser::MapLiteral,
    ) -> TypeResult<TypedExpression> {
        let mut typed_entries = Vec::new();
        let mut key_type: Option<TypeId> = None;
        let mut value_type: Option<TypeId> = None;

        // Process each map entry
        for entry in &map_lit.entries {
            match entry {
                outrun_parser::MapEntry::Assignment { key, value } => {
                    let typed_key = Self::check_expression(context, key)?;
                    let typed_value = Self::check_expression(context, value)?;

                    // For homogeneous maps, all keys must be the same type and all values must be the same type
                    match key_type {
                        None => {
                            key_type = Some(typed_key.type_id);
                        }
                        Some(expected_key_type) => {
                            if typed_key.type_id != expected_key_type {
                                return Err(TypeError::type_mismatch(
                                    context
                                        .get_type_name(expected_key_type)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    context
                                        .get_type_name(typed_key.type_id)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    crate::error::span_to_source_span(key.span),
                                ));
                            }
                        }
                    }

                    match value_type {
                        None => {
                            value_type = Some(typed_value.type_id);
                        }
                        Some(expected_value_type) => {
                            if typed_value.type_id != expected_value_type {
                                return Err(TypeError::type_mismatch(
                                    context
                                        .get_type_name(expected_value_type)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    context
                                        .get_type_name(typed_value.type_id)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    crate::error::span_to_source_span(value.span),
                                ));
                            }
                        }
                    }

                    typed_entries.push((typed_key, typed_value));
                }
                outrun_parser::MapEntry::Shorthand { name, value } => {
                    // Shorthand syntax {name: value} - name becomes an atom key
                    let atom_type_id = context.interner.intern_type("Outrun.Core.Atom");
                    let typed_key = TypedExpression {
                        kind: TypedExpressionKind::Atom(name.name.clone()),
                        type_id: atom_type_id,
                        span: name.span,
                    };
                    let typed_value = Self::check_expression(context, value)?;

                    // Check key type consistency
                    match key_type {
                        None => {
                            key_type = Some(atom_type_id);
                        }
                        Some(expected_key_type) => {
                            if atom_type_id != expected_key_type {
                                return Err(TypeError::type_mismatch(
                                    context
                                        .get_type_name(expected_key_type)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    "Outrun.Core.Atom".to_string(),
                                    crate::error::span_to_source_span(name.span),
                                ));
                            }
                        }
                    }

                    // Check value type consistency
                    match value_type {
                        None => {
                            value_type = Some(typed_value.type_id);
                        }
                        Some(expected_value_type) => {
                            if typed_value.type_id != expected_value_type {
                                return Err(TypeError::type_mismatch(
                                    context
                                        .get_type_name(expected_value_type)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    context
                                        .get_type_name(typed_value.type_id)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    crate::error::span_to_source_span(value.span),
                                ));
                            }
                        }
                    }

                    typed_entries.push((typed_key, typed_value));
                }
                outrun_parser::MapEntry::Spread(_) => {
                    return Err(TypeError::UnimplementedFeature {
                        feature: "Map spread operator".to_string(),
                        span: crate::error::span_to_source_span(map_lit.span),
                    });
                }
            }
        }

        // Determine final map types
        let final_key_type = key_type.unwrap_or_else(|| {
            // Empty map - use unknown types for now
            context.interner.intern_type("Unknown")
        });
        let final_value_type =
            value_type.unwrap_or_else(|| context.interner.intern_type("Unknown"));

        // Create the map type: Outrun.Core.Map<KeyType, ValueType>
        let key_type_name = context.get_type_name(final_key_type).unwrap_or("Unknown");
        let value_type_name = context.get_type_name(final_value_type).unwrap_or("Unknown");
        let map_type_name = format!("Outrun.Core.Map<{}, {}>", key_type_name, value_type_name);
        let map_type_id = context.interner.intern_type(&map_type_name);

        Ok(TypedExpression {
            kind: TypedExpressionKind::Map {
                entries: typed_entries,
                key_type: final_key_type,
                value_type: final_value_type,
            },
            type_id: map_type_id,
            span: map_lit.span,
        })
    }

    /// Type check struct literal
    fn check_struct_literal(
        context: &mut TypeContext,
        struct_lit: &outrun_parser::StructLiteral,
    ) -> TypeResult<TypedExpression> {
        // Get the struct type from the qualified type path
        let struct_type_name = struct_lit
            .type_path
            .iter()
            .map(|t| t.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        let struct_type_id = context.interner.intern_type(&struct_type_name);

        // Look up the struct definition in the context
        let struct_definition = context.get_concrete_type(struct_type_id);
        let struct_fields = match struct_definition {
            Some(crate::types::ConcreteType::Struct { fields, .. }) => fields.clone(),
            Some(_) => {
                return Err(TypeError::TypeMismatch {
                    span: crate::error::span_to_source_span(struct_lit.span),
                    expected: "struct type".to_string(),
                    found: format!("{} is not a struct", struct_type_name),
                });
            }
            None => {
                return Err(TypeError::UndefinedType {
                    name: struct_type_name.clone(),
                    span: crate::error::span_to_source_span(struct_lit.span),
                });
            }
        };

        // Process each field in the struct literal
        let mut typed_fields = Vec::new();
        let mut provided_field_names = std::collections::HashSet::new();

        for field in &struct_lit.fields {
            match field {
                outrun_parser::StructLiteralField::Assignment { name, value } => {
                    // Type check the field value
                    let typed_value = Self::check_expression(context, value)?;

                    // Intern the field name as an atom
                    let field_atom = context.interner.intern_atom(&name.name);

                    // Check if this field exists in the struct definition
                    let expected_field_type = struct_fields
                        .iter()
                        .find(|field| field.name == field_atom)
                        .map(|field| field.type_id);

                    match expected_field_type {
                        Some(expected_type) => {
                            // Validate the field value type matches the expected type
                            if typed_value.type_id != expected_type {
                                return Err(TypeError::type_mismatch(
                                    context
                                        .get_type_name(expected_type)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    context
                                        .get_type_name(typed_value.type_id)
                                        .unwrap_or("Unknown")
                                        .to_string(),
                                    crate::error::span_to_source_span(value.span),
                                ));
                            }

                            typed_fields.push((field_atom, typed_value));
                            provided_field_names.insert(field_atom);
                        }
                        None => {
                            return Err(TypeError::UnexpectedParameter {
                                function_name: struct_type_name.clone(),
                                parameter_name: name.name.clone(),
                                span: crate::error::span_to_source_span(name.span),
                            });
                        }
                    }
                }
                outrun_parser::StructLiteralField::Shorthand(name) => {
                    // First intern the field name
                    let field_atom = context.interner.intern_atom(&name.name);

                    // Then look up variable with same name in scope
                    let variable = context.lookup_variable(&name.name);
                    match variable {
                        Some(var) => {
                            // Check if this field exists in the struct definition
                            let expected_field_type = struct_fields
                                .iter()
                                .find(|field| field.name == field_atom)
                                .map(|field| field.type_id);

                            match expected_field_type {
                                Some(expected_type) => {
                                    // Validate the variable type matches the expected field type
                                    if var.type_id != expected_type {
                                        return Err(TypeError::type_mismatch(
                                            context
                                                .get_type_name(expected_type)
                                                .unwrap_or("Unknown")
                                                .to_string(),
                                            context
                                                .get_type_name(var.type_id)
                                                .unwrap_or("Unknown")
                                                .to_string(),
                                            crate::error::span_to_source_span(name.span),
                                        ));
                                    }

                                    let typed_value = TypedExpression {
                                        kind: TypedExpressionKind::Identifier(name.name.clone()),
                                        type_id: var.type_id,
                                        span: name.span,
                                    };

                                    typed_fields.push((field_atom, typed_value));
                                    provided_field_names.insert(field_atom);
                                }
                                None => {
                                    return Err(TypeError::UnexpectedParameter {
                                        function_name: struct_type_name.clone(),
                                        parameter_name: name.name.clone(),
                                        span: crate::error::span_to_source_span(name.span),
                                    });
                                }
                            }
                        }
                        None => {
                            return Err(TypeError::undefined_variable(
                                name.name.clone(),
                                crate::error::span_to_source_span(name.span),
                            ));
                        }
                    }
                }
                outrun_parser::StructLiteralField::Spread(_) => {
                    return Err(TypeError::UnimplementedFeature {
                        feature: "Struct literal spread operator".to_string(),
                        span: crate::error::span_to_source_span(struct_lit.span),
                    });
                }
            }
        }

        // Check that all required fields are provided
        for struct_field in &struct_fields {
            if !provided_field_names.contains(&struct_field.name) {
                let field_name = context
                    .interner
                    .resolve_atom(struct_field.name)
                    .unwrap_or("<unknown>");
                return Err(TypeError::MissingParameter {
                    function_name: struct_type_name.clone(),
                    parameter_name: field_name.to_string(),
                    span: crate::error::span_to_source_span(struct_lit.span),
                });
            }
        }

        Ok(TypedExpression {
            kind: TypedExpressionKind::Struct {
                type_name: struct_type_name.clone(),
                fields: typed_fields,
                struct_type: struct_type_id,
            },
            type_id: struct_type_id,
            span: struct_lit.span,
        })
    }

    /// Type check if expression
    fn check_if_expression(
        context: &mut TypeContext,
        if_expr: &outrun_parser::IfExpression,
    ) -> TypeResult<TypedExpression> {
        // Type check the condition - must be Boolean
        let typed_condition = Self::check_expression(context, &if_expr.condition)?;

        let bool_type = context.interner.intern_type("Outrun.Core.Boolean");
        if typed_condition.type_id != bool_type {
            return Err(TypeError::type_mismatch(
                "Outrun.Core.Boolean".to_string(),
                context
                    .get_type_name(typed_condition.type_id)
                    .unwrap_or("Unknown")
                    .to_string(),
                crate::error::span_to_source_span(if_expr.condition.span),
            ));
        }

        // Type check the then block
        let typed_then_block = Self::check_block(context, &if_expr.then_block)?;

        // Type check the optional else block
        let (typed_else_block, result_type) = if let Some(else_block) = &if_expr.else_block {
            let typed_else = Self::check_block(context, else_block)?;

            // Both branches must have compatible types
            if typed_then_block.result_type != typed_else.result_type {
                return Err(TypeError::type_mismatch(
                    context
                        .get_type_name(typed_then_block.result_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                    context
                        .get_type_name(typed_else.result_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                    crate::error::span_to_source_span(else_block.span),
                ));
            }

            (Some(typed_else), typed_then_block.result_type)
        } else {
            // If no else block, the expression evaluates to Unit type (for now, use a special type)
            let unit_type = context.interner.intern_type("Outrun.Core.Unit");

            // The then block must also be Unit type for consistency
            if typed_then_block.result_type != unit_type {
                return Err(TypeError::TypeMismatch {
                    span: crate::error::span_to_source_span(if_expr.then_block.span),
                    expected: "Outrun.Core.Unit".to_string(),
                    found: format!(
                        "if without else requires then block to be Unit type, but found {}",
                        context
                            .get_type_name(typed_then_block.result_type)
                            .unwrap_or("Unknown")
                    ),
                });
            }

            (None, unit_type)
        };

        Ok(TypedExpression {
            kind: TypedExpressionKind::IfExpression {
                condition: Box::new(typed_condition),
                then_block: typed_then_block,
                else_block: typed_else_block,
            },
            type_id: result_type,
            span: if_expr.span,
        })
    }

    /// Type check case expression
    pub fn check_case_expression(
        context: &mut TypeContext,
        case_expr: &outrun_parser::CaseExpression,
    ) -> TypeResult<TypedExpression> {
        match case_expr {
            outrun_parser::CaseExpression::Concrete(concrete) => {
                Self::check_concrete_case_expression(context, concrete)
            }
            outrun_parser::CaseExpression::Trait(trait_case) => {
                Self::check_trait_case_expression(context, trait_case)
            }
        }
    }

    /// Type check concrete case expression
    fn check_concrete_case_expression(
        context: &mut TypeContext,
        case_expr: &outrun_parser::ConcreteCaseExpression,
    ) -> TypeResult<TypedExpression> {
        // Type check the expression being matched
        let typed_expression = Self::check_expression(context, &case_expr.expression)?;

        // Type check all when clauses
        let mut typed_when_clauses = Vec::new();
        let mut result_types = Vec::new();

        for when_clause in &case_expr.when_clauses {
            // Type check the guard - must be Boolean
            let typed_guard = Self::check_expression(context, &when_clause.guard)?;

            let bool_type = context.interner.intern_type("Outrun.Core.Boolean");
            if typed_guard.type_id != bool_type {
                return Err(TypeError::type_mismatch(
                    "Outrun.Core.Boolean".to_string(),
                    context
                        .get_type_name(typed_guard.type_id)
                        .unwrap_or("Unknown")
                        .to_string(),
                    crate::error::span_to_source_span(when_clause.guard.span),
                ));
            }

            // Type check the result
            let typed_result = Self::check_case_result(context, &when_clause.result)?;
            result_types.push(typed_result.result_type());

            typed_when_clauses.push(crate::checker::TypedCaseWhenClause {
                guard: typed_guard,
                result: typed_result,
                span: when_clause.span,
            });
        }

        // Check that we have at least one when clause
        if result_types.is_empty() {
            return Err(TypeError::internal(
                "Case expression must have at least one when clause".to_string(),
            ));
        }

        // All result types must be compatible
        let first_result_type = result_types[0];
        for (i, &result_type) in result_types.iter().enumerate().skip(1) {
            if result_type != first_result_type {
                // Find the span for the mismatched branch
                let error_span = match &typed_when_clauses[i].result {
                    crate::checker::TypedCaseResult::Block(block) => block.span,
                    crate::checker::TypedCaseResult::Expression(expr) => expr.span,
                };

                return Err(TypeError::type_mismatch(
                    context
                        .get_type_name(first_result_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                    context
                        .get_type_name(result_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                    crate::error::span_to_source_span(error_span),
                ));
            }
        }

        // Add exhaustiveness checking for concrete case expressions
        use crate::exhaustiveness::{CaseType, ExhaustivenessAnalyzer};
        let analyzer = ExhaustivenessAnalyzer::new(context);
        let case_type = CaseType::Pattern(typed_expression.type_id);

        // Extract covered patterns from when clauses
        // For now, we can't easily extract patterns from guards since they're arbitrary expressions
        // This is a placeholder that treats concrete cases as requiring manual verification
        let covered_patterns = Vec::new(); // TODO: Extract actual patterns from guards

        let exhaustiveness_result = analyzer.analyze_case_exhaustiveness(
            case_type.clone(),
            &covered_patterns,
            case_expr.span,
        )?;

        // Convert to error if not exhaustive (but only for specific types we can analyze)
        if let Some(concrete_type) = context.get_concrete_type(typed_expression.type_id) {
            if matches!(
                concrete_type,
                crate::types::ConcreteType::Boolean
                    | crate::types::ConcreteType::Option { .. }
                    | crate::types::ConcreteType::Result { .. }
            ) {
                // For these types, we require exhaustiveness
                if let Some(error) =
                    exhaustiveness_result.to_case_error(case_type, context, case_expr.span)
                {
                    // For now, treat as warning by adding to context errors rather than failing
                    context.add_error(error);
                }
            }
            // For other types (String, Integer, etc.), exhaustiveness is not required
            // as they're infinite types that need catch-all patterns
        }

        Ok(TypedExpression {
            kind: TypedExpressionKind::CaseExpression {
                expression: Box::new(typed_expression),
                when_clauses: typed_when_clauses,
            },
            type_id: first_result_type,
            span: case_expr.span,
        })
    }

    /// Type check trait case expression
    pub fn check_trait_case_expression(
        context: &mut TypeContext,
        trait_case: &outrun_parser::TraitCaseExpression,
    ) -> TypeResult<TypedExpression> {
        // Type check the expression being matched
        let typed_expression = Self::check_expression(context, &trait_case.expression)?;

        // Resolve the trait name to a trait ID
        let trait_name = &trait_case.trait_name.name;
        let trait_id =
            context
                .interner
                .get_trait(trait_name)
                .ok_or_else(|| TypeError::UndefinedType {
                    span: crate::error::span_to_source_span(trait_case.trait_name.span),
                    name: trait_name.clone(),
                })?;

        // Verify the expression type implements the trait
        if !context.implements_trait(typed_expression.type_id, trait_id) {
            return Err(TypeError::TraitNotImplemented {
                span: crate::error::span_to_source_span(trait_case.expression.span),
                trait_name: trait_name.clone(),
                type_name: context.type_name_or_unknown(typed_expression.type_id),
            });
        }

        // Type check each trait case clause and collect covered types
        let mut typed_clauses = Vec::new();
        let mut covered_types = Vec::new();
        let mut result_type: Option<TypeId> = None;

        for clause in &trait_case.type_clauses {
            // Validate that the type exists and implements the trait
            let clause_type_name = &clause.type_name.name;
            let clause_type_id = context.interner.get_type(clause_type_name).ok_or_else(|| {
                TypeError::UndefinedType {
                    span: crate::error::span_to_source_span(clause.type_name.span),
                    name: clause_type_name.clone(),
                }
            })?;

            // Check if the type implements the required trait
            if !context
                .trait_registry
                .implements_trait(clause_type_id, trait_id)
            {
                return Err(TypeError::TraitNotImplemented {
                    span: crate::error::span_to_source_span(clause.type_name.span),
                    trait_name: trait_name.clone(),
                    type_name: clause_type_name.clone(),
                });
            }

            let typed_clause = Self::check_trait_case_clause(context, clause)?;

            // Collect the type being covered in this clause
            covered_types.push(clause_type_id);

            // Ensure all clauses return compatible types
            if let Some(existing_type) = result_type {
                if typed_clause.result_type != existing_type {
                    return Err(TypeError::TypeMismatch {
                        span: crate::error::span_to_source_span(clause.span),
                        expected: context
                            .interner
                            .type_name(existing_type)
                            .unwrap_or_else(|| "Unknown".to_string()),
                        found: context
                            .interner
                            .type_name(typed_clause.result_type)
                            .unwrap_or("Unknown".to_string()),
                    });
                }
            } else {
                result_type = Some(typed_clause.result_type);
            }

            typed_clauses.push(typed_clause);
        }

        // Check exhaustiveness using orphan rule analysis
        let exhaustiveness_result = context
            .trait_registry
            .check_trait_case_exhaustiveness(trait_id, &covered_types);

        if let crate::types::traits::ExhaustivenessResult::Missing(missing_types) =
            exhaustiveness_result
        {
            let missing_type_names: Vec<String> = missing_types
                .iter()
                .map(|&type_id| {
                    context
                        .interner
                        .type_name(type_id)
                        .unwrap_or("Unknown".to_string())
                })
                .collect();

            return Err(TypeError::CaseNotExhaustive {
                span: crate::error::span_to_source_span(trait_case.span),
                trait_name: trait_name.clone(),
                missing_types: missing_type_names.join(", "),
            });
        }

        let final_type = result_type.unwrap_or_else(|| {
            // Should not happen with valid syntax, but fallback to Unit
            context.interner.get_type("Unit").unwrap()
        });

        Ok(TypedExpression {
            kind: TypedExpressionKind::TraitCaseExpression {
                expression: Box::new(typed_expression),
                trait_name: trait_name.clone(),
                type_clauses: typed_clauses,
            },
            type_id: final_type,
            span: trait_case.span,
        })
    }

    /// Type check a trait case clause
    fn check_trait_case_clause(
        context: &mut TypeContext,
        clause: &outrun_parser::TraitCaseClause,
    ) -> TypeResult<TypedTraitCaseClause> {
        // Resolve the type name to a type ID
        let type_name = &clause.type_name.name;
        let type_id =
            context
                .interner
                .get_type(type_name)
                .ok_or_else(|| TypeError::UndefinedType {
                    span: crate::error::span_to_source_span(clause.type_name.span),
                    name: type_name.clone(),
                })?;

        // Note: Trait implementation validation is done in the main trait case expression loop

        // Create a new scope for pattern variables if there's a pattern
        context.push_scope(false);

        // Type check the pattern if present and register bound variables
        if let Some(pattern) = &clause.pattern {
            // Ensure the type being matched is actually a struct type since only structs can be destructured
            let concrete_type = context.get_concrete_type(type_id);
            match concrete_type {
                Some(crate::types::ConcreteType::Struct { .. }) => {
                    // Valid struct type, proceed with pattern checking
                    let bound_variables = crate::checker::patterns::PatternChecker::check_pattern(
                        context,
                        &outrun_parser::Pattern::Struct(pattern.clone()),
                        type_id,
                    )?;

                    // Register all bound variables in the current scope
                    for variable in bound_variables {
                        context.register_variable(variable)?;
                    }
                }
                Some(_) => {
                    // Type exists but is not a struct
                    context.pop_scope();
                    return Err(TypeError::TypeMismatch {
                        span: crate::error::span_to_source_span(pattern.span),
                        expected: "struct type for pattern destructuring".to_string(),
                        found: format!("type {}", type_name),
                    });
                }
                None => {
                    // Type not found in concrete types registry
                    context.pop_scope();
                    return Err(TypeError::TypeMismatch {
                        span: crate::error::span_to_source_span(pattern.span),
                        expected: "concrete struct type".to_string(),
                        found: format!("unknown type {}", type_name),
                    });
                }
            }
        }

        // Type check the guard if present
        if let Some(guard) = &clause.guard {
            let typed_guard = Self::check_expression(context, guard)?;
            let boolean_type = context.interner.intern_type("Outrun.Core.Boolean");

            if typed_guard.type_id != boolean_type {
                context.pop_scope();
                return Err(TypeError::TypeMismatch {
                    span: crate::error::span_to_source_span(guard.span),
                    expected: "Boolean".to_string(),
                    found: context
                        .interner
                        .type_name(typed_guard.type_id)
                        .unwrap_or("Unknown".to_string()),
                });
            }
        }

        // Type check the result
        let typed_result = Self::check_case_result(context, &clause.result)?;

        context.pop_scope();

        Ok(TypedTraitCaseClause {
            type_id,
            result_type: typed_result.result_type(),
            span: clause.span,
        })
    }

    /// Type check a case result (block or expression)
    fn check_case_result(
        context: &mut TypeContext,
        case_result: &outrun_parser::CaseResult,
    ) -> TypeResult<crate::checker::TypedCaseResult> {
        match case_result {
            outrun_parser::CaseResult::Block(block) => {
                let typed_block = Self::check_block(context, block)?;
                Ok(crate::checker::TypedCaseResult::Block(typed_block))
            }
            outrun_parser::CaseResult::Expression(expr) => {
                let typed_expr = Self::check_expression(context, expr)?;
                Ok(crate::checker::TypedCaseResult::Expression(Box::new(
                    typed_expr,
                )))
            }
        }
    }

    /// Type check pipe operation (|>)
    fn check_pipe_operation(
        context: &mut TypeContext,
        typed_left: &TypedExpression,
        typed_right: &TypedExpression,
        _left_expr: &Expression,
        right_expr: &Expression,
    ) -> TypeResult<TypeId> {
        // For now, implement basic pipe type checking
        // TODO: Enhance with proper function type analysis when function types are implemented

        // In a proper implementation:
        // 1. Right side should be a function type that accepts left's type as input
        // 2. Result type should be the function's return type
        // 3. Should validate through Pipe trait dispatch

        // For basic implementation, we'll handle simple cases:
        // - Function calls on the right side
        // - Method calls (future feature)

        match &typed_right.kind {
            TypedExpressionKind::FunctionCall { .. } => {
                // Right side is a function call - return its type
                // In the future, we should validate that the function accepts left's type
                Ok(typed_right.type_id)
            }
            TypedExpressionKind::Identifier(_) => {
                // Right side is an identifier - could be a function reference
                // For now, return the left type as a conservative approximation
                // TODO: Look up function signature and validate parameter types
                Ok(typed_left.type_id)
            }
            _ => {
                // Right side is not a valid function-like expression
                Err(TypeError::TypeMismatch {
                    span: crate::error::span_to_source_span(right_expr.span),
                    expected: "function or function call".to_string(),
                    found: format!(
                        "expression of type {}",
                        context
                            .get_type_name(typed_right.type_id)
                            .unwrap_or("Unknown")
                    ),
                })
            }
        }
    }

    /// Type check pipe maybe operation (|?)
    fn check_pipe_maybe_operation(
        context: &mut TypeContext,
        typed_left: &TypedExpression,
        typed_right: &TypedExpression,
        left_expr: &Expression,
        right_expr: &Expression,
    ) -> TypeResult<TypeId> {
        // For now, implement basic pipe maybe type checking
        // TODO: Enhance with proper Option/Maybe type analysis when implemented

        // In a proper implementation:
        // 1. Left side should be Option<T> type
        // 2. Right side should be a function T -> U
        // 3. Result should be Option<U>
        // 4. Should validate through Maybe trait dispatch

        // For basic implementation, check if left side looks like an Option type
        let left_type_name = context
            .get_type_name(typed_left.type_id)
            .unwrap_or("Unknown");

        if !left_type_name.contains("Option") && !left_type_name.contains("Maybe") {
            return Err(TypeError::TypeMismatch {
                span: crate::error::span_to_source_span(left_expr.span),
                expected: "Option or Maybe type".to_string(),
                found: left_type_name.to_string(),
            });
        }

        match &typed_right.kind {
            TypedExpressionKind::FunctionCall { .. } => {
                // Right side is a function call
                // For Option<T> |? f, if f returns U, result should be Option<U>
                // For now, return an Option type wrapping the function result
                let result_type_name = format!(
                    "Outrun.Core.Option<{}>",
                    context
                        .get_type_name(typed_right.type_id)
                        .unwrap_or("Unknown")
                );
                Ok(context.interner.intern_type(&result_type_name))
            }
            TypedExpressionKind::Identifier(_) => {
                // Right side is an identifier - could be a function reference
                // For now, return the left type (Option is preserved)
                Ok(typed_left.type_id)
            }
            _ => {
                // Right side is not a valid function-like expression
                Err(TypeError::TypeMismatch {
                    span: crate::error::span_to_source_span(right_expr.span),
                    expected: "function or function call".to_string(),
                    found: format!(
                        "expression of type {}",
                        context
                            .get_type_name(typed_right.type_id)
                            .unwrap_or("Unknown")
                    ),
                })
            }
        }
    }

    /// Type check a block
    pub fn check_block(
        context: &mut TypeContext,
        block: &outrun_parser::Block,
    ) -> TypeResult<crate::checker::TypedBlock> {
        // Create a new scope for the block
        context.push_scope(false);

        let mut typed_statements = Vec::new();
        let mut result_type = context.interner.intern_type("Outrun.Core.Unit"); // Default to Unit

        for (i, statement) in block.statements.iter().enumerate() {
            let is_last = i == block.statements.len() - 1;

            match &statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    let typed_expr = Self::check_expression(context, expr)?;

                    // The last expression determines the block's type
                    if is_last {
                        result_type = typed_expr.type_id;
                    }

                    typed_statements.push(crate::checker::TypedStatement {
                        kind: crate::checker::TypedStatementKind::Expression(Box::new(typed_expr)),
                        span: statement.span,
                    });
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    // Use the main TypeChecker's let binding method
                    // For now, we'll handle let bindings in blocks differently
                    // since we don't have access to the main TypeChecker here

                    // Type check the expression
                    let typed_expression =
                        Self::check_expression(context, &let_binding.expression)?;

                    // Determine the type for the binding (simplified version)
                    let binding_type = if let Some(type_annotation) = &let_binding.type_annotation {
                        match type_annotation {
                            outrun_parser::TypeAnnotation::Simple { path, .. } => {
                                let type_name = path
                                    .iter()
                                    .map(|part| part.name.as_str())
                                    .collect::<Vec<_>>()
                                    .join(".");
                                let annotated_type_id = context.interner.intern_type(&type_name);

                                // Check type matches
                                if typed_expression.type_id != annotated_type_id {
                                    return Err(TypeError::type_mismatch(
                                        context
                                            .get_type_name(annotated_type_id)
                                            .unwrap_or("Unknown")
                                            .to_string(),
                                        context
                                            .get_type_name(typed_expression.type_id)
                                            .unwrap_or("Unknown")
                                            .to_string(),
                                        crate::error::span_to_source_span(
                                            let_binding.expression.span,
                                        ),
                                    ));
                                }
                                annotated_type_id
                            }
                            _ => {
                                return Err(TypeError::UnimplementedFeature {
                                    feature: "Complex type annotations in block let bindings"
                                        .to_string(),
                                    span: crate::error::span_to_source_span(let_binding.span),
                                });
                            }
                        }
                    } else {
                        typed_expression.type_id
                    };

                    // Register variable in scope (simplified pattern handling)
                    match &let_binding.pattern {
                        outrun_parser::Pattern::Identifier(identifier) => {
                            let variable = crate::checker::context::Variable {
                                name: identifier.name.clone(),
                                type_id: binding_type,
                                is_mutable: false,
                                span: identifier.span,
                            };
                            context.register_variable(variable)?;
                        }
                        _ => {
                            return Err(TypeError::UnimplementedFeature {
                                feature: "Complex patterns in block let bindings".to_string(),
                                span: crate::error::span_to_source_span(let_binding.span),
                            });
                        }
                    }

                    // Convert pattern to typed pattern (currently only supports identifiers)
                    let typed_pattern = match &let_binding.pattern {
                        outrun_parser::Pattern::Identifier(identifier) => {
                            crate::checker::TypedPattern::Identifier {
                                name: identifier.name.clone(),
                                type_id: binding_type,
                            }
                        }
                        _ => {
                            return Err(TypeError::UnimplementedFeature {
                                feature: "Complex patterns in block let bindings".to_string(),
                                span: crate::error::span_to_source_span(let_binding.span),
                            });
                        }
                    };

                    let typed_let_binding = crate::checker::TypedLetBinding {
                        pattern: typed_pattern,
                        type_id: binding_type,
                        expression: typed_expression,
                        span: let_binding.span,
                    };

                    typed_statements.push(crate::checker::TypedStatement {
                        kind: crate::checker::TypedStatementKind::LetBinding(Box::new(
                            typed_let_binding,
                        )),
                        span: statement.span,
                    });

                    // Let bindings don't contribute to the block's result type
                    if is_last {
                        result_type = context.interner.intern_type("Outrun.Core.Unit");
                    }
                }
            }
        }

        // Pop the block scope
        context.pop_scope();

        Ok(crate::checker::TypedBlock {
            statements: typed_statements,
            result_type,
            span: block.span,
        })
    }

    /// Look up a qualified function (Module.function_name)
    /// This treats qualified calls as static function calls on the module/type
    fn lookup_qualified_function(
        context: &mut TypeContext,
        module_name: &str,
        function_name: &str,
    ) -> TypeResult<TypeId> {
        // Try to resolve the module as a trait first
        // This handles cases like Option.some(), Result.ok(), etc.
        if let Some(trait_id) = context.interner.get_trait(module_name) {
            if let Some(trait_def) = context.trait_registry.get_trait(trait_id) {
                // Convert function name to AtomId for lookup
                // Note: intern_atom is used instead of get_atom to ensure the atom exists for comparison
                let function_name_atom = context.interner.intern_atom(function_name);
                if let Some(function_def) = trait_def.find_function(function_name_atom) {
                    return Ok(function_def.return_type);
                }

                // Trait exists but function doesn't exist on it
                return Err(TypeError::UndefinedFunction {
                    span: crate::error::span_to_source_span(outrun_parser::Span::new(0, 0)), // TODO: Pass proper span
                    name: format!("{}.{}", module_name, function_name),
                });
            }
        }

        // Try to resolve as a type (for future struct static methods)
        if let Some(module_type_id) = context.interner.get_type(module_name) {
            // Look up the static function on this type using the dispatch table
            if let Some(_function_id) = context
                .dispatch_table
                .lookup_static_function(module_type_id, function_name)
            {
                // Function exists in dispatch table, should have been handled above if it's a trait
                // This is for future struct static methods
                // For now, return a placeholder type
                return Ok(context.interner.intern_type("String")); // TODO: Get actual return type
            }
        }

        // If not found as a static function on a type, try as a module function
        // For now, this is a simplified implementation
        // Future enhancement: proper module registry and qualified function lookup
        let qualified_name = format!("{}.{}", module_name, function_name);
        if let Some(function) = context.lookup_function(&qualified_name) {
            return Ok(function.return_type);
        }

        // Function not found in module
        Err(TypeError::UndefinedFunction {
            span: crate::error::span_to_source_span(outrun_parser::Span::new(0, 0)), // TODO: Pass proper span
            name: qualified_name,
        })
    }

    /// Check function capture expression (&function_name)
    fn check_function_capture(
        context: &mut TypeContext,
        capture: &outrun_parser::FunctionCapture,
    ) -> TypeResult<TypedExpression> {
        // Resolve the function being captured
        let function_name = &capture.function_name.name;

        // Handle both simple and qualified function captures
        let function_return_type = if let Some(module_path) = &capture.module_path {
            // Qualified capture: &Module.function_name
            let module_name = module_path
                .iter()
                .map(|id| id.name.as_str())
                .collect::<Vec<_>>()
                .join(".");
            Self::lookup_qualified_function(context, &module_name, function_name)?
        } else {
            // Simple capture: &function_name
            if let Some(function) = context.lookup_function(function_name) {
                function.return_type
            } else {
                return Err(TypeError::undefined_function(
                    function_name.clone(),
                    crate::error::span_to_source_span(capture.span),
                ));
            }
        };

        // For now, we'll create a simplified function type
        // TODO: Implement proper function types in the type system
        // For now, we'll use a placeholder type that represents "function"
        let function_type_id = context.interner.intern_type(&format!(
            "Function<{}>",
            context
                .interner
                .resolve_type(function_return_type)
                .unwrap_or("Unknown")
        ));

        Ok(TypedExpression {
            kind: crate::checker::TypedExpressionKind::FunctionCapture {
                path: if let Some(module_path) = &capture.module_path {
                    format!(
                        "{}.{}",
                        module_path
                            .iter()
                            .map(|id| id.name.as_str())
                            .collect::<Vec<_>>()
                            .join("."),
                        function_name
                    )
                } else {
                    function_name.clone()
                },
                arity: capture.arity,
            },
            type_id: function_type_id,
            span: capture.span,
        })
    }

    /// Type check unary operation
    fn check_unary_operation(
        context: &mut TypeContext,
        unary_op: &outrun_parser::UnaryOperation,
    ) -> TypeResult<TypedExpression> {
        use outrun_parser::UnaryOperator;

        // Type check the operand
        let typed_operand = Self::check_expression(context, &unary_op.operand)?;

        // Determine result type based on operator and operand type
        let result_type = match unary_op.operator {
            UnaryOperator::LogicalNot => {
                // !expr requires Boolean operand and returns Boolean
                let boolean_type = context.interner.intern_type("Outrun.Core.Boolean");
                if typed_operand.type_id != boolean_type {
                    let found_type_name = context
                        .interner
                        .resolve_type(typed_operand.type_id)
                        .unwrap_or("Unknown")
                        .to_string();
                    return Err(TypeError::type_mismatch(
                        "Outrun.Core.Boolean".to_string(),
                        found_type_name,
                        crate::error::span_to_source_span(unary_op.span),
                    ));
                }
                boolean_type
            }
            UnaryOperator::Minus => {
                // -expr requires numeric operand and returns same type
                let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
                let float_type = context.interner.intern_type("Outrun.Core.Float64");

                if typed_operand.type_id == integer_type || typed_operand.type_id == float_type {
                    typed_operand.type_id
                } else {
                    let found_type_name = context
                        .interner
                        .resolve_type(typed_operand.type_id)
                        .unwrap_or("Unknown")
                        .to_string();
                    return Err(TypeError::type_mismatch(
                        "Numeric type (Integer64 or Float64)".to_string(),
                        found_type_name,
                        crate::error::span_to_source_span(unary_op.span),
                    ));
                }
            }
            UnaryOperator::Plus => {
                // +expr requires numeric operand and returns same type
                let integer_type = context.interner.intern_type("Outrun.Core.Integer64");
                let float_type = context.interner.intern_type("Outrun.Core.Float64");

                if typed_operand.type_id == integer_type || typed_operand.type_id == float_type {
                    typed_operand.type_id
                } else {
                    let found_type_name = context
                        .interner
                        .resolve_type(typed_operand.type_id)
                        .unwrap_or("Unknown")
                        .to_string();
                    return Err(TypeError::type_mismatch(
                        "Numeric type (Integer64 or Float64)".to_string(),
                        found_type_name,
                        crate::error::span_to_source_span(unary_op.span),
                    ));
                }
            }
            UnaryOperator::BitwiseNot => {
                // ~expr requires integer operand and returns same type
                let integer_type = context.interner.intern_type("Outrun.Core.Integer64");

                if typed_operand.type_id == integer_type {
                    typed_operand.type_id
                } else {
                    let found_type_name = context
                        .interner
                        .resolve_type(typed_operand.type_id)
                        .unwrap_or("Unknown")
                        .to_string();
                    return Err(TypeError::type_mismatch(
                        "Outrun.Core.Integer64".to_string(),
                        found_type_name,
                        crate::error::span_to_source_span(unary_op.span),
                    ));
                }
            }
        };

        Ok(TypedExpression {
            kind: crate::checker::TypedExpressionKind::UnaryOp {
                operator: unary_op.operator.clone(),
                operand: Box::new(typed_operand),
            },
            type_id: result_type,
            span: unary_op.span,
        })
    }

    /// Type check field access (object.field)
    fn check_field_access(
        context: &mut TypeContext,
        field_access: &outrun_parser::FieldAccess,
    ) -> TypeResult<TypedExpression> {
        // Type check the object
        let typed_object = Self::check_expression(context, &field_access.object)?;

        // Intern the field name first (before borrowing object_type)
        let field_atom = context.interner.intern_atom(&field_access.field.name);

        // Get the object's concrete type to look up the field
        let object_type = context.get_concrete_type(typed_object.type_id);

        match object_type {
            Some(crate::types::ConcreteType::Struct { fields, .. }) => {
                // Find the field in the struct definition
                if let Some(struct_field) = fields.iter().find(|f| f.name == field_atom) {
                    // Need to perform generic type substitution
                    let resolved_field_type = Self::resolve_field_type_with_object_generics(
                        context,
                        struct_field.type_id,
                        typed_object.type_id,
                    )?;

                    Ok(TypedExpression {
                        kind: crate::checker::TypedExpressionKind::FieldAccess {
                            object: Box::new(typed_object),
                            field: field_access.field.name.clone(),
                        },
                        type_id: resolved_field_type,
                        span: field_access.span,
                    })
                } else {
                    // Field not found in struct
                    Err(TypeError::UnexpectedParameter {
                        function_name: context
                            .get_type_name(typed_object.type_id)
                            .unwrap_or("Unknown")
                            .to_string(),
                        parameter_name: field_access.field.name.clone(),
                        span: crate::error::span_to_source_span(field_access.field.span),
                    })
                }
            }
            Some(_other_type) => {
                // Object is not a struct - field access not supported
                Err(TypeError::TypeMismatch {
                    expected: "struct type".to_string(),
                    found: context
                        .get_type_name(typed_object.type_id)
                        .unwrap_or("Unknown")
                        .to_string(),
                    span: crate::error::span_to_source_span(typed_object.span),
                })
            }
            None => {
                // Object type not found in registry - this shouldn't happen
                Err(TypeError::internal(format!(
                    "Object type {} not found in registry",
                    context
                        .get_type_name(typed_object.type_id)
                        .unwrap_or("Unknown")
                )))
            }
        }
    }

    /// Type check qualified identifier (Module.function)
    fn check_qualified_identifier(
        context: &mut TypeContext,
        qualified_id: &outrun_parser::QualifiedIdentifier,
    ) -> TypeResult<TypedExpression> {
        let module_name = &qualified_id.module.name;
        let function_name = &qualified_id.name.name;

        // Try to resolve as a trait static function call
        let trait_name = format!("{}Trait", module_name); // e.g., StringTrait
        if let Some(trait_id) = context.interner.get_trait(&trait_name) {
            if let Some(trait_def) = context.trait_registry.get_trait(trait_id) {
                let function_name_atom = context.interner.intern_atom(function_name);
                if let Some(function_def) = trait_def.find_function(function_name_atom) {
                    return Ok(TypedExpression {
                        kind: crate::checker::TypedExpressionKind::QualifiedIdentifier {
                            module: module_name.clone(),
                            name: function_name.clone(),
                        },
                        type_id: function_def.return_type,
                        span: qualified_id.span,
                    });
                }
            }
        }

        // For now, return a placeholder type
        let placeholder_type = context.interner.intern_type("Outrun.Core.String");
        Ok(TypedExpression {
            kind: crate::checker::TypedExpressionKind::QualifiedIdentifier {
                module: module_name.clone(),
                name: function_name.clone(),
            },
            type_id: placeholder_type,
            span: qualified_id.span,
        })
    }

    /// Type check type identifier (type names as expressions)
    fn check_type_identifier(
        context: &mut TypeContext,
        type_id: &outrun_parser::TypeIdentifier,
    ) -> TypeResult<TypedExpression> {
        // Look up the referenced type to verify it exists
        let referenced_type =
            context
                .interner
                .get_type(&type_id.name)
                .ok_or_else(|| TypeError::UndefinedType {
                    span: crate::error::span_to_source_span(type_id.span),
                    name: type_id.name.clone(),
                })?;

        // Determine if it's a struct or trait type to decide the concrete type
        let concrete_type = if let Some(trait_id) = context.interner.get_trait(&type_id.name) {
            // It's a trait - check if it's registered in the trait registry
            if context.trait_registry.has_trait(trait_id) {
                context.interner.intern_type("Outrun.Type.TraitType")
            } else {
                context.interner.intern_type("Outrun.Type.PrimitiveType")
            }
        } else if context
            .introspection_registry
            .is_struct_type(referenced_type)
        {
            // It's a struct - create StructType
            context.interner.intern_type("Outrun.Type.StructType")
        } else {
            // For now, assume it's a primitive type and create a generic Type
            context.interner.intern_type("Outrun.Type.PrimitiveType")
        };

        Ok(TypedExpression {
            kind: TypedExpressionKind::TypeIdentifier {
                type_name: type_id.name.clone(),
                referenced_type,
            },
            type_id: concrete_type,
            span: type_id.span,
        })
    }

    /// Type check anonymous function
    fn check_anonymous_function(
        context: &mut TypeContext,
        anon_fn: &outrun_parser::AnonymousFunction,
    ) -> TypeResult<TypedExpression> {
        if anon_fn.clauses.is_empty() {
            return Err(TypeError::internal(
                "Anonymous function must have at least one clause".to_string(),
            ));
        }

        let mut typed_clauses = Vec::new();
        let mut function_signature: Option<Vec<(AtomId, TypeId)>> = None;
        let mut return_types = Vec::new();
        let mut first_clause_span = anon_fn.clauses[0].span;

        for (clause_index, clause) in anon_fn.clauses.iter().enumerate() {
            // 1. Type check and extract parameter signature
            let typed_params = Self::extract_parameter_signature(context, &clause.parameters)?;

            // 2. Validate signature consistency across clauses
            if let Some(ref expected_sig) = function_signature {
                Self::validate_signature_consistency(
                    expected_sig,
                    &typed_params,
                    first_clause_span,
                    clause.span,
                    clause_index,
                )?;
            } else {
                function_signature = Some(typed_params.clone());
                first_clause_span = clause.span;
            }

            // 3. Push new scope for parameters
            context.push_scope(false);

            // 4. Register parameters in scope
            for (atom_id, type_id) in &typed_params {
                let param_name = context
                    .interner
                    .resolve_atom(*atom_id)
                    .unwrap_or("<unknown>")
                    .to_string();
                let variable = Variable {
                    name: param_name,
                    type_id: *type_id,
                    is_mutable: false,
                    span: clause.span, // Simplified - should use actual parameter span
                };
                context.register_variable(variable)?;
            }

            // 5. Type check guard (if present)
            let typed_guard = if let Some(ref guard) = clause.guard {
                let guard_result = Self::check_expression(context, guard)?;
                Self::validate_guard_type(context, &guard_result, clause_index)?;
                Some(guard_result)
            } else {
                None
            };

            // 6. Type check body
            let typed_body = Self::check_anonymous_body(context, &clause.body)?;
            let return_type = typed_body.return_type();
            return_types.push((return_type, typed_body.span()));

            // 7. Pop parameter scope
            context.pop_scope();

            typed_clauses.push(crate::checker::TypedAnonymousClause {
                params: typed_params,
                guard: typed_guard,
                body: typed_body,
                return_type,
                span: clause.span,
            });
        }

        // 8. Validate return type consistency
        let unified_return_type =
            Self::validate_return_type_consistency(context, &return_types, first_clause_span)?;

        // 9. Create function type
        let function_type =
            Self::create_function_type(context, &function_signature.unwrap(), unified_return_type)?;

        Ok(TypedExpression {
            kind: TypedExpressionKind::AnonymousFunction {
                clauses: typed_clauses,
                function_type,
            },
            type_id: function_type,
            span: anon_fn.span,
        })
    }

    /// Extract parameter signature from anonymous function parameters
    fn extract_parameter_signature(
        context: &mut TypeContext,
        params: &outrun_parser::AnonymousParameters,
    ) -> TypeResult<Vec<(AtomId, TypeId)>> {
        match params {
            outrun_parser::AnonymousParameters::None { .. } => Ok(Vec::new()),
            outrun_parser::AnonymousParameters::Single { parameter, .. } => {
                let atom_id = context.interner.intern_atom(&parameter.name.name);
                let type_id = super::TypeChecker::resolve_type_annotation(
                    context,
                    &parameter.type_annotation,
                    &[], // No generic params in anonymous functions
                )?;
                Ok(vec![(atom_id, type_id)])
            }
            outrun_parser::AnonymousParameters::Multiple { parameters, .. } => {
                let mut typed_params = Vec::new();
                for param in parameters {
                    let atom_id = context.interner.intern_atom(&param.name.name);
                    let type_id = super::TypeChecker::resolve_type_annotation(
                        context,
                        &param.type_annotation,
                        &[], // No generic params in anonymous functions
                    )?;
                    typed_params.push((atom_id, type_id));
                }
                Ok(typed_params)
            }
        }
    }

    /// Validate parameter signature consistency between clauses
    fn validate_signature_consistency(
        expected: &[(AtomId, TypeId)],
        found: &[(AtomId, TypeId)],
        first_clause_span: Span,
        current_clause_span: Span,
        clause_index: usize,
    ) -> TypeResult<()> {
        if expected.len() != found.len() {
            return Err(TypeError::ParameterSignatureMismatch {
                span: crate::error::span_to_source_span(current_clause_span),
                first_clause_span: crate::error::span_to_source_span(first_clause_span),
                clause_index,
                expected_signature: format!("{} parameters", expected.len()),
                found_signature: format!("{} parameters", found.len()),
            });
        }

        for (i, ((exp_atom, exp_type), (found_atom, found_type))) in
            expected.iter().zip(found.iter()).enumerate()
        {
            if exp_atom != found_atom || exp_type != found_type {
                return Err(TypeError::ParameterSignatureMismatch {
                    span: crate::error::span_to_source_span(current_clause_span),
                    first_clause_span: crate::error::span_to_source_span(first_clause_span),
                    clause_index,
                    expected_signature: format!("parameter {} with different name/type", i),
                    found_signature: format!("parameter {} mismatch", i),
                });
            }
        }

        Ok(())
    }

    /// Validate guard returns Boolean type
    fn validate_guard_type(
        context: &mut TypeContext,
        guard_expr: &TypedExpression,
        clause_index: usize,
    ) -> TypeResult<()> {
        let boolean_type = context.interner.intern_type("Outrun.Core.Boolean");
        if guard_expr.type_id != boolean_type {
            return Err(TypeError::InvalidAnonymousGuard {
                span: crate::error::span_to_source_span(guard_expr.span),
                clause_index,
                found_type: context
                    .get_type_name(guard_expr.type_id)
                    .unwrap_or("Unknown")
                    .to_string(),
            });
        }
        Ok(())
    }

    /// Type check anonymous function body
    fn check_anonymous_body(
        context: &mut TypeContext,
        body: &outrun_parser::AnonymousBody,
    ) -> TypeResult<crate::checker::TypedAnonymousBody> {
        match body {
            outrun_parser::AnonymousBody::Expression(expr) => {
                let typed_expr = Self::check_expression(context, expr)?;
                Ok(crate::checker::TypedAnonymousBody::Expression(typed_expr))
            }
            outrun_parser::AnonymousBody::Block(block) => {
                let typed_block = Self::check_block(context, block)?;
                Ok(crate::checker::TypedAnonymousBody::Block(typed_block))
            }
        }
    }

    /// Validate return type consistency across all clauses
    fn validate_return_type_consistency(
        context: &TypeContext,
        return_types: &[(TypeId, Span)],
        first_clause_span: Span,
    ) -> TypeResult<TypeId> {
        if return_types.is_empty() {
            return Err(TypeError::internal(
                "No return types to validate".to_string(),
            ));
        }

        let (first_return_type, _) = return_types[0];
        for (clause_index, (return_type, return_span)) in return_types.iter().enumerate().skip(1) {
            if *return_type != first_return_type {
                return Err(TypeError::ReturnTypeMismatch {
                    span: crate::error::span_to_source_span(*return_span),
                    first_clause_span: crate::error::span_to_source_span(first_clause_span),
                    clause_index,
                    expected_type: context
                        .get_type_name(first_return_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                    found_type: context
                        .get_type_name(*return_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                });
            }
        }

        Ok(first_return_type)
    }

    /// Create function type from parameter signature and return type
    fn create_function_type(
        context: &mut TypeContext,
        params: &[(AtomId, TypeId)],
        return_type: TypeId,
    ) -> TypeResult<TypeId> {
        // Create a concrete Function type
        let function_concrete = crate::types::ConcreteType::Function {
            params: params.to_vec(),
            return_type,
        };

        // Generate a unique type name for this function signature
        let param_names: Vec<String> = params
            .iter()
            .map(|(atom_id, type_id)| {
                let param_name = context
                    .interner
                    .resolve_atom(*atom_id)
                    .unwrap_or("<unknown>");
                let type_name = context.get_type_name(*type_id).unwrap_or("Unknown");
                format!("{}: {}", param_name, type_name)
            })
            .collect();

        let return_type_name = context.get_type_name(return_type).unwrap_or("Unknown");
        let function_type_name = format!(
            "Function<({}) -> {}>",
            param_names.join(", "),
            return_type_name
        );

        let function_type_id = context.interner.intern_type(&function_type_name);
        context.register_concrete_type(function_type_id, function_concrete);

        Ok(function_type_id)
    }

    /// Check if a type implements the Display trait
    fn implements_display_trait(context: &TypeContext, type_id: TypeId) -> bool {
        // First, check if Display trait exists
        let display_trait_id = match context.interner.get_trait("Display") {
            Some(trait_id) => trait_id,
            None => return false, // Display trait not defined
        };

        // Check if the type implements the Display trait
        context
            .trait_registry
            .implements_trait(type_id, display_trait_id)
    }

    /// Type check sigil literal  
    fn check_sigil_literal(
        context: &mut TypeContext,
        sigil_lit: &outrun_parser::SigilLiteral,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // For now, we'll treat sigils as returning String type
        // In a full implementation, different sigil types (r"", json"", etc.)
        // would return different types based on their processing
        let result_type = context.interner.intern_type("Outrun.Core.String");

        // Type check any interpolated expressions within the sigil's string content
        let mut interpolated_expressions = Vec::new();

        // Process the sigil's string content for interpolations
        // Sigils contain a StringLiteral, so we process its parts
        for part in &sigil_lit.string.parts {
            match part {
                outrun_parser::StringPart::Text { .. } => {
                    // Text parts don't need type checking
                }
                outrun_parser::StringPart::Interpolation {
                    expression,
                    span: interp_span,
                } => {
                    // Type check the interpolated expression
                    let typed_expr = Self::check_expression(context, expression)?;

                    // For sigils, we might have different requirements than Display
                    // For now, we'll require Display trait like strings
                    if !Self::implements_display_trait(context, typed_expr.type_id) {
                        let type_name = context
                            .get_type_name(typed_expr.type_id)
                            .unwrap_or("Unknown")
                            .to_string();
                        return Err(TypeError::string_interpolation_display(
                            type_name,
                            crate::error::span_to_source_span(*interp_span),
                        ));
                    }

                    interpolated_expressions.push(typed_expr);
                }
            }
        }

        // Reconstruct content with interpolation placeholders
        let processed_content =
            sigil_lit
                .string
                .parts
                .iter()
                .fold(String::new(), |mut acc, part| {
                    match part {
                        outrun_parser::StringPart::Text { content, .. } => {
                            acc.push_str(content);
                        }
                        outrun_parser::StringPart::Interpolation { .. } => {
                            acc.push_str("#{...}");
                        }
                    }
                    acc
                });

        Ok(TypedExpression {
            kind: TypedExpressionKind::Sigil {
                name: sigil_lit.sigil_type.name.clone(),
                content: processed_content,
                raw_content: format!("~{}{}", sigil_lit.sigil_type.name, sigil_lit.string),
                interpolated_expressions,
                result_type,
            },
            type_id: result_type,
            span,
        })
    }

    /// Type check macro injection
    fn check_macro_injection(
        context: &mut TypeContext,
        macro_injection: &outrun_parser::MacroInjection,
        span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Macro injections reference macro parameters
        // For type checking purposes, we need to look up the parameter type
        // This would typically be resolved during macro expansion

        // For now, we'll return a generic "any" type since macro injection
        // types depend on the macro expansion context
        let injected_type = context.interner.intern_type("Outrun.Core.Any");

        // TODO: In a full implementation, this would:
        // 1. Look up the macro parameter in the current macro expansion context
        // 2. Get the actual type of the injected value
        // 3. Validate that the injection is happening within a macro definition

        Ok(TypedExpression {
            kind: TypedExpressionKind::MacroInjection {
                name: macro_injection.parameter.name.clone(),
                injected_type,
            },
            type_id: injected_type,
            span,
        })
    }

    /// Resolve a field type by substituting generic parameters from the object type
    ///
    /// This handles cases like:
    /// - Object type: Processor<T>
    /// - Field type: Outrun.Core.List<T>
    /// - Result: Outrun.Core.List<T> (with the same T as in the object)
    fn resolve_field_type_with_object_generics(
        context: &mut TypeContext,
        field_type_id: TypeId,
        object_type_id: TypeId,
    ) -> TypeResult<TypeId> {
        // Get the type names
        let field_type_name = context
            .get_type_name(field_type_id)
            .ok_or_else(|| TypeError::internal("Field type not found in interner".to_string()))?;
        let object_type_name = context
            .get_type_name(object_type_id)
            .ok_or_else(|| TypeError::internal("Object type not found in interner".to_string()))?;

        // Check if the field type is a simple generic parameter (like "T", "U", etc.)
        if field_type_name.len() == 1
            && field_type_name.chars().next().unwrap().is_ascii_uppercase()
        {
            // This is a generic parameter that needs substitution

            // Parse the object type to extract base name and generic arguments
            if let Some(generics_start) = object_type_name.find('<') {
                if let Some(generics_end) = object_type_name.rfind('>') {
                    let _base_type_name = &object_type_name[..generics_start];
                    let generics_str = &object_type_name[generics_start + 1..generics_end];

                    // Parse generic arguments
                    let generic_args: Vec<&str> =
                        generics_str.split(',').map(|s| s.trim()).collect();

                    // For now, implement a simple position-based mapping
                    // Since we know T, U, V etc. are typically the first, second, third parameters
                    let generic_param_position = match field_type_name {
                        "T" => 0,
                        "U" => 1,
                        "V" => 2,
                        "W" => 3,
                        "X" => 4,
                        "Y" => 5,
                        "Z" => 6,
                        _ => return Ok(field_type_id), // Not a standard generic parameter
                    };

                    // Get the corresponding concrete type from the object's generic arguments
                    if generic_param_position < generic_args.len() {
                        let concrete_type_name = generic_args[generic_param_position];
                        // Clone the concrete type name to avoid borrowing issues
                        let concrete_type_name_owned = concrete_type_name.to_string();
                        let concrete_type_id =
                            context.interner.intern_type(&concrete_type_name_owned);
                        return Ok(concrete_type_id);
                    }
                }
            }
        }

        // If field type contains complex generics, we need recursive substitution
        if field_type_name.contains('<') && object_type_name.contains('<') {
            // Extract generic parameters from object type
            if let Some(object_generics_start) = object_type_name.find('<') {
                if let Some(object_generics_end) = object_type_name.rfind('>') {
                    let object_generics =
                        &object_type_name[object_generics_start + 1..object_generics_end];

                    // If field type contains the same generic parameters, create a new type with substitution
                    if field_type_name.contains(object_generics) {
                        // Return the field type as-is for now, since both types should refer to the same generics
                        return Ok(field_type_id);
                    }
                }
            }
        }

        // If no generic substitution needed, return the original field type
        Ok(field_type_id)
    }

    /// Parse a function type to extract parameter and return type information
    /// Returns Some(FunctionTypeInfo) if the type is a Function<...> type, None otherwise
    fn parse_function_type(
        context: &mut TypeContext,
        type_id: TypeId,
    ) -> TypeResult<Option<FunctionTypeInfo>> {
        let type_name = context
            .get_type_name(type_id)
            .ok_or_else(|| TypeError::internal("Type not found in interner".to_string()))?
            .to_string(); // Convert to owned String to avoid borrowing issues

        // Check if this is a Function<...> type
        if !type_name.starts_with("Function<") || !type_name.ends_with(">") {
            return Ok(None);
        }

        // Extract the content inside Function<...>
        let content = &type_name[9..type_name.len() - 1]; // Remove "Function<" and ">"

        // Parse the function signature: (param1: Type1, param2: Type2) -> ReturnType
        if let Some(arrow_pos) = content.find(" -> ") {
            let params_part = &content[..arrow_pos].trim();
            let return_part = &content[arrow_pos + 4..].trim();

            // Parse parameters from (param1: Type1, param2: Type2)
            let mut parameters = Vec::new();
            if params_part.starts_with('(') && params_part.ends_with(')') {
                let params_content = &params_part[1..params_part.len() - 1]; // Remove ( and )

                if !params_content.trim().is_empty() {
                    for param in params_content.split(',') {
                        let param = param.trim();
                        if let Some(colon_pos) = param.find(':') {
                            let param_name = param[..colon_pos].trim().to_string();
                            let param_type_name = param[colon_pos + 1..].trim();
                            let param_type_id = context.interner.intern_type(param_type_name);
                            parameters.push((param_name, param_type_id));
                        }
                    }
                }
            }

            // Parse return type
            let return_type_id = context.interner.intern_type(return_part);

            return Ok(Some(FunctionTypeInfo {
                parameters,
                return_type: return_type_id,
            }));
        }

        Ok(None)
    }

    /// Validate a function value call and return the result type
    fn validate_function_value_call(
        context: &mut TypeContext,
        function_type: &FunctionTypeInfo,
        typed_args: &[(String, TypedExpression)],
        call_span: outrun_parser::Span,
    ) -> TypeResult<TypedExpression> {
        // Check argument count
        if typed_args.len() != function_type.parameters.len() {
            return Err(TypeError::SignatureMismatch {
                function_name: "function value".to_string(),
                expected: format!("{} parameters", function_type.parameters.len()),
                found: format!("{} arguments", typed_args.len()),
                span: crate::error::span_to_source_span(call_span),
            });
        }

        // Validate each argument
        for (arg_name, typed_arg) in typed_args {
            // Find the corresponding parameter
            if let Some((_, expected_type)) = function_type
                .parameters
                .iter()
                .find(|(param_name, _)| param_name == arg_name)
            {
                // Check type compatibility
                if typed_arg.type_id != *expected_type {
                    return Err(TypeError::type_mismatch(
                        context
                            .get_type_name(*expected_type)
                            .unwrap_or("Unknown")
                            .to_string(),
                        context
                            .get_type_name(typed_arg.type_id)
                            .unwrap_or("Unknown")
                            .to_string(),
                        crate::error::span_to_source_span(typed_arg.span),
                    ));
                }
            } else {
                return Err(TypeError::UnexpectedParameter {
                    function_name: "function value".to_string(),
                    parameter_name: arg_name.clone(),
                    span: crate::error::span_to_source_span(typed_arg.span),
                });
            }
        }

        // All validations passed, return the function call expression
        Ok(TypedExpression {
            kind: TypedExpressionKind::FunctionCall {
                name: "function_value".to_string(), // Placeholder name for function values
                args: typed_args.to_vec(),
            },
            type_id: function_type.return_type,
            span: call_span,
        })
    }
}
