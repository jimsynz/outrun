//! Type checking visitor for expression validation
//!
//! This module contains the TypeCheckingVisitor that performs comprehensive
//! type checking of expressions during the compilation process. It validates
//! type compatibility, manages variable scopes, and tracks type information
//! for later use in typed AST generation.

use crate::compilation::compiler_environment::{AtomId, CompilerEnvironment, TypeNameId};
use crate::compilation::FunctionType;
use crate::error::{context, SpanExt, TypeError};
use crate::unification::{StructuredType, UnificationContext};
use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{ProtocolDefinition, StructDefinition};
use std::collections::HashMap;

/// Visitor for type checking expressions (Phase 5)
pub struct TypeCheckingVisitor {
    /// Compiler environment with all necessary components
    pub compiler_environment: CompilerEnvironment,
    /// Accumulated type checking errors
    pub errors: Vec<TypeError>,
    /// Variable scope stack for tracking variable types
    variable_scopes: Vec<HashMap<String, crate::unification::StructuredType>>,
    /// Type parameter scope stack for tracking generic type parameters (Self, T, E, K, V, etc.)
    type_parameter_scopes: Vec<HashMap<String, crate::unification::StructuredType>>,
}

impl<T> Visitor<T> for TypeCheckingVisitor {
    fn visit_expression(&mut self, expr: &outrun_parser::Expression) -> VisitorResult {
        // Type check this expression using the unification system
        match self.check_expression_type(expr) {
            Ok(resolved_type) => {
                // Store the resolved type for later use by TypedASTBuilder
                // We need to get the context, modify it, and put it back since unification_context() returns a clone
                let mut context = self.compiler_environment.unification_context();
                context.add_expression_type(expr.span, resolved_type);
                self.compiler_environment.set_unification_context(context);
                // Continue traversing child expressions
                crate::visitor::walk_expression::<Self, ()>(self, expr)
            }
            Err(error) => {
                self.errors.push(error);
                // Continue checking other expressions despite this error
                let _ = crate::visitor::walk_expression::<Self, ()>(self, expr);
                Ok(())
            }
        }
    }

    fn visit_let_binding(&mut self, let_binding: &outrun_parser::LetBinding) -> VisitorResult {
        // Custom let binding handling with type hints
        // If there's a type annotation, resolve it first to use as a hint
        let type_hint = if let Some(type_annotation) = &let_binding.type_annotation {
            match self.resolve_type_annotation(type_annotation) {
                Ok(hint) => Some(hint),
                Err(error) => {
                    self.errors.push(error);
                    None
                }
            }
        } else {
            None
        };

        // Check the expression type with the hint
        let expr_type = match self
            .check_expression_type_with_hint(&let_binding.expression, type_hint.as_ref())
        {
            Ok(t) => {
                // Store the resolved type for later use by TypedASTBuilder
                let mut context = self.compiler_environment.unification_context();
                context.add_expression_type(let_binding.expression.span, t.clone());
                self.compiler_environment.set_unification_context(context);
                t
            }
            Err(error) => {
                self.errors.push(error);
                // Continue processing
                let _ = crate::visitor::walk_let_binding::<Self, ()>(self, let_binding);
                return Ok(());
            }
        };

        // If there was a type annotation, validate it matches the expression
        if let Some(expected_type) = &type_hint {
            if crate::unification::unify_structured_types(
                &expr_type,
                expected_type,
                &self.compiler_environment.unification_context(),
                &self.compiler_environment,
            )
            .unwrap_or(None)
            .is_none()
            {
                self.errors.push(TypeError::type_mismatch(
                    expected_type.to_string_representation(),
                    expr_type.to_string_representation(),
                    let_binding.expression.span.to_source_span(),
                ));
            }
        }

        // Visit the pattern (but not the expression since we already checked it with hint)
        <Self as crate::visitor::Visitor<()>>::visit_pattern(self, &let_binding.pattern)?;

        Ok(())
    }

    fn visit_function_definition(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
    ) -> VisitorResult {
        // Type check the function signature and body, collecting let-bound variables
        let let_bound_variables = match self.check_function_definition_with_variables(func) {
            Ok(variables) => variables,
            Err(error) => {
                self.errors.push(error);
                std::collections::HashMap::new()
            }
        };

        // Create a new scope for function parameters and let-bound variables
        self.push_scope();

        // Register function parameters in the scope
        for param in &func.parameters {
            match self.resolve_type_annotation(&param.type_annotation) {
                Ok(param_type) => {
                    self.register_variable(param.name.name.clone(), param_type);
                }
                Err(error) => {
                    self.errors.push(error);
                }
            }
        }

        // Register let-bound variables from type checking phase
        for (var_name, var_type) in let_bound_variables {
            self.register_variable(var_name, var_type);
        }

        // Skip traversing the function body since it was already type-checked in check_function_definition_with_variables
        // The visitor pattern is being used for AST traversal, but function bodies are already type-checked
        // Traversing them again would create duplicate type checks and expensive redundant work

        // Pop the function scope
        self.pop_scope();

        Ok(())
    }

    fn visit_struct_definition(
        &mut self,
        struct_def: &outrun_parser::StructDefinition,
    ) -> VisitorResult {
        // Establish type parameter scope for this struct definition
        self.push_type_parameter_scope();

        // Register generic parameters (T, E, K, V, etc.) but NOT Self
        if let Some(ref generic_params) = struct_def.generic_params {
            for param in &generic_params.params {
                let param_type_id = self
                    .compiler_environment
                    .intern_type_name(&param.name.name.clone());
                self.register_type_parameter(
                    param.name.name.clone(),
                    crate::unification::StructuredType::Simple(param_type_id),
                );
            }
        }

        // Validate struct field types exist (now with generic parameters in scope)
        if let Err(error) = self.check_struct_definition(struct_def) {
            self.errors.push(error);
        }

        // Restore previous type parameter scope
        self.pop_type_parameter_scope();

        Ok(())
    }

    fn visit_protocol_definition(
        &mut self,
        protocol_def: &outrun_parser::ProtocolDefinition,
    ) -> VisitorResult {
        // Establish type parameter scope for this protocol definition FIRST
        self.push_type_parameter_scope();

        // Register Self parameter
        let protocol_name = protocol_def.name_as_string();
        let protocol_type_id = self.compiler_environment.intern_type_name(&protocol_name);
        self.register_type_parameter(
            "Self".to_string(),
            crate::unification::StructuredType::Simple(protocol_type_id),
        );

        // Register generic parameters (T, E, K, V, etc.)
        if let Some(ref generic_params) = protocol_def.generic_params {
            for param in &generic_params.params {
                let param_type_id = self
                    .compiler_environment
                    .intern_type_name(&param.name.name.clone());
                self.register_type_parameter(
                    param.name.name.clone(),
                    crate::unification::StructuredType::Simple(param_type_id),
                );
            }
        }

        // Now validate protocol function signatures and constraints with Self context available
        if let Err(error) = self.check_protocol_definition(protocol_def) {
            self.errors.push(error);
        }

        // Visit protocol functions with Self context established
        // Note: protocol definitions are typically leaf nodes, so we manually traverse functions
        for protocol_function in &protocol_def.functions {
            match protocol_function {
                outrun_parser::ProtocolFunction::Definition(func) => {
                    let _ = <Self as crate::visitor::Visitor<()>>::visit_function_definition(
                        self, func,
                    );
                    // Errors are already collected in visit_function_definition
                }
                outrun_parser::ProtocolFunction::Signature(_) => {
                    // Function signatures don't have bodies to traverse
                }
                outrun_parser::ProtocolFunction::StaticDefinition(static_func) => {
                    // Convert StaticFunctionDefinition to FunctionDefinition for visitor
                    // TODO: StaticFunctionDefinition should have a guard field for consistency
                    let func_def = outrun_parser::FunctionDefinition {
                        attributes: static_func.attributes.clone(),
                        visibility: outrun_parser::FunctionVisibility::Public,
                        name: static_func.name.clone(),
                        parameters: static_func.parameters.clone(),
                        return_type: static_func.return_type.clone(),
                        guard: None, // TODO: Static functions should support guards
                        body: static_func.body.clone(),
                        span: static_func.span,
                    };
                    let _ = <Self as crate::visitor::Visitor<()>>::visit_function_definition(
                        self, &func_def,
                    );
                    // Errors are already collected in visit_function_definition
                }
            }
        }

        // Restore previous type parameter scope
        self.pop_type_parameter_scope();

        Ok(())
    }

    fn visit_impl_block(&mut self, impl_block: &outrun_parser::ImplBlock) -> VisitorResult {
        // Validate protocol implementation
        if let Err(error) = self.check_impl_block(impl_block) {
            self.errors.push(error);
        }

        // Establish type parameter scope for this impl block
        self.push_type_parameter_scope();

        // Register Self parameter with full structured type
        match self.resolve_type_spec(&impl_block.type_spec) {
            Ok(impl_type) => {
                self.register_type_parameter("Self".to_string(), impl_type);
            }
            Err(error) => {
                self.errors.push(error);
            }
        }

        // Register generic parameters from impl block (e.g., impl<T> Option<T> for Some<T>)
        if let Some(ref generic_params) = impl_block.generic_params {
            for param in &generic_params.params {
                let param_type_id = self
                    .compiler_environment
                    .intern_type_name(&param.name.name.clone());
                self.register_type_parameter(
                    param.name.name.clone(),
                    crate::unification::StructuredType::Simple(param_type_id),
                );
            }
        }

        // Continue traversing impl functions with Self context established
        let result = crate::visitor::walk_impl_block::<Self, ()>(self, impl_block);

        // Restore previous type parameter scope
        self.pop_type_parameter_scope();

        result
    }

    fn visit_binary_operation(
        &mut self,
        binary_op: &outrun_parser::BinaryOperation,
    ) -> VisitorResult {
        // Special handling for 'as' operator to avoid type checking the RHS type identifier
        match binary_op.operator {
            outrun_parser::BinaryOperator::As => {
                // For 'as' expressions like "42 as Integer", only visit the LHS expression
                // The RHS is a type identifier that should not be type-checked as an expression
                <Self as crate::visitor::Visitor<()>>::visit_expression(self, &binary_op.left)
            }
            _ => {
                // For all other binary operations, use the default visitor behavior
                crate::visitor::walk_binary_operation::<Self, ()>(self, binary_op)
            }
        }
    }
}

impl TypeCheckingVisitor {
    /// Add external variables to the global scope before type checking
    /// This is used by the REPL to provide previously bound variables
    pub fn add_external_variables(
        &mut self,
        variables: HashMap<String, crate::unification::StructuredType>,
    ) {
        // Add variables to the first (global) scope
        if let Some(global_scope) = self.variable_scopes.first_mut() {
            global_scope.extend(variables);
        }
    }
    /// Create a new TypeCheckingVisitor with the given components
    pub fn new(
        context: UnificationContext,
        structs: HashMap<TypeNameId, StructDefinition>,
        protocols: HashMap<TypeNameId, ProtocolDefinition>,
    ) -> Self {
        let mut compiler_environment = CompilerEnvironment::new();
        compiler_environment.set_unification_context(context);

        // Load structs and protocols into the environment
        for (type_id, struct_def) in structs {
            compiler_environment.add_struct(type_id, struct_def);
        }
        for (type_id, protocol_def) in protocols {
            compiler_environment.add_protocol(type_id, protocol_def);
        }

        Self {
            compiler_environment,
            errors: Vec::new(),
            variable_scopes: vec![HashMap::new()], // Start with global scope
            type_parameter_scopes: vec![HashMap::new()], // Start with global type parameter scope
        }
    }

    /// Create a new TypeCheckingVisitor from a CompilerEnvironment
    pub fn from_compiler_environment(compiler_environment: CompilerEnvironment) -> Self {
        Self {
            compiler_environment,
            errors: Vec::new(),
            variable_scopes: vec![HashMap::new()], // Start with global scope
            type_parameter_scopes: vec![HashMap::new()], // Start with global type parameter scope
        }
    }

    /// Create a new TypeCheckingVisitor from a TypeCheckingContext (for backward compatibility)
    /// This method is deprecated - use from_compiler_environment instead
    #[deprecated(note = "Use from_compiler_environment instead")]
    pub fn from_context(type_checking_context: crate::context::TypeCheckingContext) -> Self {
        let mut compiler_environment = CompilerEnvironment::new();
        compiler_environment.set_unification_context(type_checking_context.unification_context);

        // Load structs and protocols from the context
        for (type_id, struct_def) in type_checking_context.structs {
            compiler_environment.add_struct(type_id, struct_def);
        }
        for (type_id, protocol_def) in type_checking_context.protocols {
            compiler_environment.add_protocol(type_id, protocol_def);
        }

        Self {
            compiler_environment,
            errors: Vec::new(),
            variable_scopes: vec![HashMap::new()], // Start with global scope
            type_parameter_scopes: vec![HashMap::new()], // Start with global type parameter scope
        }
    }

    /// Push a new scope onto the variable scope stack
    fn push_scope(&mut self) {
        self.variable_scopes.push(HashMap::new());
    }

    /// Pop the current scope from the variable scope stack
    fn pop_scope(&mut self) {
        if self.variable_scopes.len() > 1 {
            self.variable_scopes.pop();
        }
    }

    /// Register a variable in the current scope
    fn register_variable(&mut self, name: String, var_type: crate::unification::StructuredType) {
        if let Some(current_scope) = self.variable_scopes.last_mut() {
            current_scope.insert(name, var_type);
        }
    }

    /// Check if a pattern matches the given type and extract bound variables
    fn check_pattern_type(
        &mut self,
        pattern: &outrun_parser::Pattern,
        expected_type: &crate::unification::StructuredType,
    ) -> Result<Vec<(String, crate::unification::StructuredType)>, TypeError> {
        let mut unification_context = self.compiler_environment.unification_context();
        let structs = self.compiler_environment.get_all_structs();
        let mut pattern_checker = crate::patterns::PatternChecker::new(
            &mut unification_context,
            &structs,
            &self.compiler_environment,
        );
        let typed_pattern = pattern_checker.check_pattern(pattern, &Some(expected_type.clone()))?;

        // Extract bound variables from the typed pattern and convert to the format expected by MultiProgramCompiler
        let bound_variables =
            crate::patterns::PatternChecker::collect_bound_variables(&typed_pattern);
        let result: Vec<(String, crate::unification::StructuredType)> = bound_variables
            .into_iter()
            .map(|var| {
                // Use the variable's type if available, otherwise fall back to the expected type
                let var_type = var.variable_type.unwrap_or_else(|| expected_type.clone());
                (var.name, var_type)
            })
            .collect();

        Ok(result)
    }

    /// Push a new type parameter scope onto the stack
    fn push_type_parameter_scope(&mut self) {
        self.type_parameter_scopes.push(HashMap::new());
    }

    /// Pop the current type parameter scope from the stack
    fn pop_type_parameter_scope(&mut self) {
        if self.type_parameter_scopes.len() > 1 {
            self.type_parameter_scopes.pop();
        }
        // Type parameter scope popped - no additional cleanup needed
    }

    /// Register a type parameter in the current scope
    fn register_type_parameter(
        &mut self,
        name: String,
        structured_type: crate::unification::StructuredType,
    ) {
        // No additional context updates needed - unified system handles all type parameters
        // Insert into current scope
        if let Some(current_scope) = self.type_parameter_scopes.last_mut() {
            current_scope.insert(name, structured_type);
        }
    }

    /// Resolve a type that might contain generic parameters using the current type parameter scope
    fn resolve_generic_parameter_in_current_scope(
        &self,
        structured_type: &crate::unification::StructuredType,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match structured_type {
            crate::unification::StructuredType::Simple(type_id) => {
                // Check if this is a type parameter that can be resolved
                let type_name = self
                    .compiler_environment
                    .resolve_type_name(type_id)
                    .unwrap_or_default();
                if let Some(resolved_type) = self.lookup_type_parameter(&type_name) {
                    Ok(resolved_type.clone())
                } else {
                    // Not a type parameter, return as-is
                    Ok(structured_type.clone())
                }
            }
            crate::unification::StructuredType::Generic { base, args } => {
                // Recursively resolve generic arguments
                let mut resolved_args = Vec::new();
                for arg in args {
                    resolved_args.push(self.resolve_generic_parameter_in_current_scope(arg)?);
                }
                Ok(crate::unification::StructuredType::Generic {
                    base: base.clone(),
                    args: resolved_args,
                })
            }
            // For other types (tuples, functions, etc.), return as-is for now
            // TODO: Add recursive resolution for other structured types if needed
            _ => Ok(structured_type.clone()),
        }
    }

    /// Look up a type parameter in the scope stack (from current to global)
    fn lookup_type_parameter(&self, name: &str) -> Option<&crate::unification::StructuredType> {
        for scope in self.type_parameter_scopes.iter().rev() {
            if let Some(structured_type) = scope.get(name) {
                return Some(structured_type);
            }
        }
        None
    }

    /// Look up a variable type in the scope stack (from current to global)
    fn lookup_variable(&self, name: &str) -> Option<&crate::unification::StructuredType> {
        for scope in self.variable_scopes.iter().rev() {
            if let Some(var_type) = scope.get(name) {
                return Some(var_type);
            }
        }
        None
    }

    /// Check the type of an expression using the unification system
    fn check_expression_type(
        &mut self,
        expr: &outrun_parser::Expression,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        self.check_expression_type_with_hint(expr, None)
    }

    /// Check the type of an expression with an optional type hint
    fn check_expression_type_with_hint(
        &mut self,
        expr: &outrun_parser::Expression,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        use outrun_parser::ExpressionKind;

        match &expr.kind {
            ExpressionKind::Boolean(literal) => self.check_boolean_literal_type(literal),
            ExpressionKind::Integer(literal) => self.check_integer_literal_type(literal),
            ExpressionKind::Float(literal) => self.check_float_literal_type(literal),
            ExpressionKind::String(literal) => self.check_string_literal_type(literal),
            ExpressionKind::Atom(literal) => self.check_atom_literal_type(literal),
            ExpressionKind::Sigil(_) => {
                // Sigils should be desugared before type checking
                panic!("Sigil literals should be desugared before type checking")
            }
            ExpressionKind::List(literal) => self.check_list_literal_type(literal, type_hint),
            ExpressionKind::Map(literal) => self.check_map_literal_type(literal, type_hint),
            ExpressionKind::Tuple(literal) => self.check_tuple_literal_type(literal),
            ExpressionKind::Struct(literal) => self.check_struct_literal_type(literal, type_hint),
            ExpressionKind::Identifier(var) => self.check_variable_type(var),
            ExpressionKind::QualifiedIdentifier(qual_id) => {
                self.check_qualified_identifier_type(qual_id)
            }
            ExpressionKind::FunctionCall(call) => self.check_function_call_type(call, type_hint),
            ExpressionKind::IfExpression(if_expr) => self.check_if_expression_type(if_expr),
            ExpressionKind::CaseExpression(case_expr) => self.check_case_expression_type(case_expr),
            ExpressionKind::FieldAccess(field_access) => self.check_field_access_type(field_access),
            ExpressionKind::Parenthesized(inner_expr) => self.check_expression_type(inner_expr),
            ExpressionKind::BinaryOp(binary_op) => {
                match binary_op.operator {
                    outrun_parser::BinaryOperator::As => {
                        // `as Type` expressions: verify LHS unifies with RHS type annotation
                        let lhs_type = self.check_expression_type(&binary_op.left)?;

                        // The RHS should be a type identifier
                        let rhs_type = match &binary_op.right.kind {
                            ExpressionKind::TypeIdentifier(type_id) => {
                                // Resolve the type annotation
                                crate::unification::StructuredType::Simple(
                                    self.compiler_environment.intern_type_name(&type_id.name),
                                )
                            }
                            _ => {
                                return Err(TypeError::internal_with_span(
                                    "Right-hand side of `as` must be a type".to_string(),
                                    binary_op.right.span.to_source_span(),
                                ));
                            }
                        };

                        // Verify that LHS type unifies with RHS type
                        if crate::unification::unify_structured_types(
                            &lhs_type,
                            &rhs_type,
                            &self.compiler_environment.unification_context(),
                            &self.compiler_environment,
                        )
                        .unwrap_or(None)
                        .is_some()
                        {
                            // Return the original LHS type (preserves concrete type for dispatch)
                            // The `as` operator is a type assertion, not a type conversion
                            Ok(lhs_type)
                        } else {
                            Err(TypeError::type_mismatch(
                                rhs_type.to_string_representation(),
                                lhs_type.to_string_representation(),
                                binary_op.span.to_source_span(),
                            ))
                        }
                    }
                    _ => {
                        // Other binary operations are desugared to function calls
                        // They should not appear in type checking phase
                        Err(TypeError::internal(context::messages::should_be_desugared(
                            "Binary operation",
                        )))
                    }
                }
            }
            ExpressionKind::UnaryOp(_) => {
                // Unary operations are desugared to function calls
                // They should not appear in type checking phase
                Err(TypeError::internal(context::messages::should_be_desugared(
                    "Unary operation",
                )))
            }
            ExpressionKind::TypeIdentifier(_) => {
                // Type identifiers shouldn't appear in expression context during type checking
                Err(TypeError::internal(
                    "Type identifier found in expression context during type checking".to_string(),
                ))
            }
            _ => {
                // For advanced features not yet implemented (macros, anonymous functions, etc.)
                Ok(crate::unification::StructuredType::Simple(
                    self.compiler_environment.intern_type_name("Unknown"),
                ))
            }
        }
    }

    /// Check boolean literal type
    fn check_boolean_literal_type(
        &mut self,
        _literal: &outrun_parser::BooleanLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self
            .compiler_environment
            .intern_type_name("Outrun.Core.Boolean");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check integer literal type
    fn check_integer_literal_type(
        &mut self,
        _literal: &outrun_parser::IntegerLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self
            .compiler_environment
            .intern_type_name("Outrun.Core.Integer64");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check float literal type
    fn check_float_literal_type(
        &mut self,
        _literal: &outrun_parser::FloatLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self
            .compiler_environment
            .intern_type_name("Outrun.Core.Float64");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check string literal type
    fn check_string_literal_type(
        &mut self,
        _literal: &outrun_parser::StringLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self
            .compiler_environment
            .intern_type_name("Outrun.Core.String");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check atom literal type
    fn check_atom_literal_type(
        &mut self,
        _literal: &outrun_parser::AtomLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_id = self
            .compiler_environment
            .intern_type_name("Outrun.Core.Atom");
        Ok(crate::unification::StructuredType::Simple(type_id))
    }

    /// Check list literal type with optional type hint
    fn check_list_literal_type(
        &mut self,
        literal: &outrun_parser::ListLiteral,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Case 1: Empty list without type hint - ERROR
        if literal.elements.is_empty() && type_hint.is_none() {
            return Err(TypeError::CannotInferListType {
                span: literal.span.to_source_span(),
            });
        }

        // Case 2: Empty list with type hint - extract element type from hint
        if literal.elements.is_empty() {
            if let Some(hint) = type_hint {
                // Extract element type from List<T> hint
                if let crate::unification::StructuredType::Generic { base, args } = hint {
                    let list_type_id = self.compiler_environment.intern_type_name("List");
                    if base == &list_type_id && args.len() == 1 {
                        // Convert protocol hint List<T> to concrete implementation Outrun.Core.List<T>
                        let concrete_list_type_id = self
                            .compiler_environment
                            .intern_type_name("Outrun.Core.List");
                        return Ok(crate::unification::StructuredType::Generic {
                            base: concrete_list_type_id,
                            args: args.clone(),
                        });
                    }
                }
                // If hint is not a List<T>, fall through to error
                return Err(TypeError::type_mismatch(
                    "List<T>".to_string(),
                    hint.to_string_representation(),
                    literal.span.to_source_span(),
                ));
            }
        }

        // Extract element types from all expressions
        let mut element_types = Vec::new();
        for element in &literal.elements {
            let expr = match element {
                outrun_parser::ListElement::Expression(expr) => expr.as_ref(),
                outrun_parser::ListElement::Spread(_) => {
                    return Err(TypeError::internal_with_span(
                        context::messages::not_yet_supported(
                            "Spread elements in lists in type checking",
                        ),
                        literal.span.to_source_span(),
                    ));
                }
            };
            let element_type = self.check_expression_type(expr)?;
            element_types.push(element_type);
        }

        // Case 3: Type hint provided - validate all elements match hint
        if let Some(hint) = type_hint {
            // Extract element type from List<T> hint
            let hint_element_type = match hint {
                crate::unification::StructuredType::Generic { base, args } => {
                    let list_type_id = self.compiler_environment.intern_type_name("List");
                    if base == &list_type_id && args.len() == 1 {
                        &args[0]
                    } else {
                        return Err(TypeError::type_mismatch(
                            "List<T>".to_string(),
                            hint.to_string_representation(),
                            literal.span.to_source_span(),
                        ));
                    }
                }
                _ => {
                    return Err(TypeError::type_mismatch(
                        "List<T>".to_string(),
                        hint.to_string_representation(),
                        literal.span.to_source_span(),
                    ));
                }
            };

            // Validate all elements are compatible with hint
            for (i, element_type) in element_types.iter().enumerate() {
                if crate::unification::unify_structured_types(
                    element_type,
                    hint_element_type,
                    &self.compiler_environment.unification_context(),
                    &self.compiler_environment,
                )?
                .is_none()
                {
                    let element_expr = match &literal.elements[i] {
                        outrun_parser::ListElement::Expression(expr) => expr.as_ref(),
                        _ => unreachable!(), // We already handled spreads above
                    };
                    return Err(TypeError::type_mismatch(
                        hint_element_type.to_string_representation(),
                        element_type.to_string_representation(),
                        element_expr.span.to_source_span(),
                    ));
                }
            }

            // All elements match hint, convert protocol hint to concrete implementation
            if let crate::unification::StructuredType::Generic { base, args } = hint {
                let list_type_id = self.compiler_environment.intern_type_name("List");
                if base == &list_type_id {
                    // Convert List<T> hint to Outrun.Core.List<T>
                    let concrete_list_type_id = self
                        .compiler_environment
                        .intern_type_name("Outrun.Core.List");
                    return Ok(crate::unification::StructuredType::Generic {
                        base: concrete_list_type_id,
                        args: args.clone(),
                    });
                }
            }
            // If hint is not a List<T>, return it as-is (shouldn't happen after validation above)
            return Ok(hint.clone());
        }

        // Case 4: No type hint - require homogeneous elements
        let first_element_type = &element_types[0];
        for (i, element_type) in element_types.iter().enumerate().skip(1) {
            if crate::unification::unify_structured_types(
                first_element_type,
                element_type,
                &self.compiler_environment.unification_context(),
                &self.compiler_environment,
            )?
            .is_none()
            {
                let element_expr = match &literal.elements[i] {
                    outrun_parser::ListElement::Expression(expr) => expr.as_ref(),
                    _ => unreachable!(), // We already handled spreads above
                };
                return Err(TypeError::MixedListElements {
                    span: element_expr.span.to_source_span(),
                    expected_type: first_element_type.to_string_representation(),
                    found_type: element_type.to_string_representation(),
                });
            }
        }

        // All elements are homogeneous, create concrete Outrun.Core.List<T> type
        let concrete_list_type_id = self
            .compiler_environment
            .intern_type_name("Outrun.Core.List");
        Ok(crate::unification::StructuredType::Generic {
            base: concrete_list_type_id,
            args: vec![first_element_type.clone()],
        })
    }

    /// Check map literal type
    fn check_map_literal_type(
        &mut self,
        literal: &outrun_parser::MapLiteral,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        if literal.entries.is_empty() {
            // Empty map - use type hint if available, otherwise Map<Any, Any>
            if let Some(hint) = type_hint {
                match hint {
                    crate::unification::StructuredType::Generic { base, args } => {
                        let map_type_id = self.compiler_environment.intern_type_name("Map");
                        let concrete_map_type_id = self
                            .compiler_environment
                            .intern_type_name("Outrun.Core.Map");
                        if (base.clone() == map_type_id || base.clone() == concrete_map_type_id)
                            && args.len() == 2
                        {
                            // Accept both protocol hint Map<K, V> and concrete hint Outrun.Core.Map<K, V>
                            Ok(crate::unification::StructuredType::Generic {
                                base: concrete_map_type_id,
                                args: args.clone(),
                            })
                        } else {
                            // Type hint is not a valid Map type, fall back to Outrun.Core.Map<Any, Any>
                            let any_type_id = self.compiler_environment.intern_type_name("Any");
                            let concrete_map_type_id = self
                                .compiler_environment
                                .intern_type_name("Outrun.Core.Map");
                            Ok(crate::unification::StructuredType::Generic {
                                base: concrete_map_type_id,
                                args: vec![
                                    crate::unification::StructuredType::Simple(any_type_id.clone()),
                                    crate::unification::StructuredType::Simple(any_type_id.clone()),
                                ],
                            })
                        }
                    }
                    _ => {
                        // Type hint is not a generic type, fall back to Outrun.Core.Map<Any, Any>
                        let any_type_id = self.compiler_environment.intern_type_name("Any");
                        let concrete_map_type_id = self
                            .compiler_environment
                            .intern_type_name("Outrun.Core.Map");
                        Ok(crate::unification::StructuredType::Generic {
                            base: concrete_map_type_id,
                            args: vec![
                                crate::unification::StructuredType::Simple(any_type_id.clone()),
                                crate::unification::StructuredType::Simple(any_type_id.clone()),
                            ],
                        })
                    }
                }
            } else {
                // No type hint - use Outrun.Core.Map<Any, Any>
                let any_type_id = self.compiler_environment.intern_type_name("Any");
                let concrete_map_type_id = self
                    .compiler_environment
                    .intern_type_name("Outrun.Core.Map");
                Ok(crate::unification::StructuredType::Generic {
                    base: concrete_map_type_id,
                    args: vec![
                        crate::unification::StructuredType::Simple(any_type_id.clone()),
                        crate::unification::StructuredType::Simple(any_type_id.clone()),
                    ],
                })
            }
        } else {
            // Handle MapEntry enum - for now, only support Assignment entries
            let (first_key, first_value) = match &literal.entries[0] {
                outrun_parser::MapEntry::Assignment { key, value, .. } => {
                    (key.as_ref(), value.as_ref())
                }
                outrun_parser::MapEntry::Shorthand { .. } => {
                    return Err(TypeError::internal_with_span(
                        context::messages::not_yet_supported(
                            "Shorthand map entries in type checking",
                        ),
                        literal.span.to_source_span(),
                    ));
                }
                outrun_parser::MapEntry::Spread { .. } => {
                    return Err(TypeError::internal_with_span(
                        context::messages::not_yet_supported("Spread map entries in type checking"),
                        literal.span.to_source_span(),
                    ));
                }
            };

            // Extract type hints for key and value if available
            let (key_hint, value_hint) = if let Some(hint) = type_hint {
                match hint {
                    crate::unification::StructuredType::Generic { base, args } => {
                        let map_type_id = self.compiler_environment.intern_type_name("Map");
                        if base.clone() == map_type_id && args.len() == 2 {
                            (Some(&args[0]), Some(&args[1]))
                        } else {
                            (None, None)
                        }
                    }
                    _ => (None, None),
                }
            } else {
                (None, None)
            };

            let key_type = self.check_expression_type_with_hint(first_key, key_hint)?;
            let value_type = self.check_expression_type_with_hint(first_value, value_hint)?;

            // Verify all entries have compatible key and value types
            for entry in &literal.entries[1..] {
                let (entry_key, entry_value) = match entry {
                    outrun_parser::MapEntry::Assignment { key, value, .. } => {
                        (key.as_ref(), value.as_ref())
                    }
                    outrun_parser::MapEntry::Shorthand { .. } => {
                        return Err(TypeError::internal_with_span(
                            context::messages::not_yet_supported(
                                "Shorthand map entries in type checking",
                            ),
                            literal.span.to_source_span(),
                        ));
                    }
                    outrun_parser::MapEntry::Spread { .. } => {
                        return Err(TypeError::internal_with_span(
                            context::messages::not_yet_supported(
                                "Spread map entries in type checking",
                            ),
                            literal.span.to_source_span(),
                        ));
                    }
                };

                let entry_key_type = self.check_expression_type_with_hint(entry_key, key_hint)?;
                let entry_value_type =
                    self.check_expression_type_with_hint(entry_value, value_hint)?;

                if crate::unification::unify_structured_types(
                    &key_type,
                    &entry_key_type,
                    &self.compiler_environment.unification_context(),
                    &self.compiler_environment,
                )?
                .is_none()
                {
                    return Err(TypeError::type_mismatch(
                        key_type.to_string_representation(),
                        entry_key_type.to_string_representation(),
                        entry_key.span.to_source_span(),
                    ));
                }

                if crate::unification::unify_structured_types(
                    &value_type,
                    &entry_value_type,
                    &self.compiler_environment.unification_context(),
                    &self.compiler_environment,
                )?
                .is_none()
                {
                    return Err(TypeError::type_mismatch(
                        value_type.to_string_representation(),
                        entry_value_type.to_string_representation(),
                        entry_value.span.to_source_span(),
                    ));
                }
            }

            // Create concrete implementation type Outrun.Core.Map<K, V>
            let concrete_map_type_id = self
                .compiler_environment
                .intern_type_name("Outrun.Core.Map");
            Ok(crate::unification::StructuredType::Generic {
                base: concrete_map_type_id,
                args: vec![key_type, value_type],
            })
        }
    }

    /// Check tuple literal type
    // TODO: Add type hint parameter to support specific tuple type hints like (String, Integer) for better validation and inference
    fn check_tuple_literal_type(
        &mut self,
        literal: &outrun_parser::TupleLiteral,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let mut element_types = Vec::new();
        for element in &literal.elements {
            let element_type = self.check_expression_type(element)?;
            element_types.push(element_type);
        }
        Ok(crate::unification::StructuredType::Tuple(element_types))
    }

    /// Check struct literal type
    fn check_struct_literal_type(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Create a TypeSpec from the type_path for base resolution
        let type_spec = outrun_parser::TypeSpec {
            path: literal.type_path.clone(),
            generic_args: None, // We'll resolve generics via type hint or inference
            span: literal.span,
        };

        // Resolve base struct type (without generics first)
        let base_struct_type = self.resolve_type_spec(&type_spec)?;

        // Get struct TypeId for lookup
        let base_struct_type_id = match &base_struct_type {
            crate::unification::StructuredType::Simple(type_id) => type_id.clone(),
            _ => {
                return Err(TypeError::internal_with_span(
                    "Expected simple type for struct base".to_string(),
                    literal.span.to_source_span(),
                ));
            }
        };

        // Look up struct definition
        let struct_def = self
            .compiler_environment
            .get_struct(&base_struct_type_id)
            .ok_or_else(|| {
                let struct_name = literal
                    .type_path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                TypeError::undefined_type(struct_name, literal.span.to_source_span())
            })?;

        // Handle generic struct literals
        if let Some(_generic_params) = &struct_def.generic_params {
            // Case 1: Type hint provides complete generic type information
            if let Some(hint) = type_hint {
                return self.check_struct_literal_with_hint(literal, &struct_def, hint);
            }

            // Case 2: No type hint - infer from field values
            return self.infer_struct_generic_types(literal, &struct_def);
        }

        // Non-generic struct - validate fields and return simple type
        self.validate_struct_fields(literal, &struct_def)?;
        Ok(crate::unification::StructuredType::Simple(
            base_struct_type_id,
        ))
    }

    /// Check struct literal against a type hint (e.g., Some<String>)
    fn check_struct_literal_with_hint(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        struct_def: &outrun_parser::StructDefinition,
        type_hint: &crate::unification::StructuredType,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Extract and resolve generic arguments from the hint
        let resolved_args = match type_hint {
            crate::unification::StructuredType::Generic { base, args } => {
                // Verify the base type matches our struct
                let struct_name = struct_def.name_as_string();
                let struct_type_id = self.compiler_environment.intern_type_name(&struct_name);
                if base != &struct_type_id {
                    return Err(TypeError::type_mismatch(
                        struct_name,
                        type_hint.to_string_representation(),
                        literal.span.to_source_span(),
                    ));
                }

                // Resolve any generic parameters in the args using current type parameter scope
                let mut resolved_args = Vec::new();
                for arg in args {
                    let resolved_arg = self.resolve_generic_parameter_in_current_scope(arg)?;
                    resolved_args.push(resolved_arg);
                }
                resolved_args
            }
            _ => {
                return Err(TypeError::type_mismatch(
                    format!("{}<...>", struct_def.name_as_string()),
                    type_hint.to_string_representation(),
                    literal.span.to_source_span(),
                ));
            }
        };

        // Validate that field values match the hinted generic types
        self.validate_struct_fields_with_generic_context(literal, struct_def, &resolved_args)?;

        // Return the hinted type
        Ok(type_hint.clone())
    }

    /// Infer generic types from field values when no hint is provided
    fn infer_struct_generic_types(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let generic_params = struct_def.generic_params.as_ref().unwrap();
        let mut type_bindings: std::collections::HashMap<
            String,
            crate::unification::StructuredType,
        > = std::collections::HashMap::new();

        // Check each field to extract type parameter bindings
        for field in &literal.fields {
            if let outrun_parser::StructLiteralField::Assignment { name, value } = field {
                let field_value_type = self.check_expression_type(value)?;

                // Find the corresponding field definition
                if let Some(field_def) = struct_def.fields.iter().find(|f| f.name.name == name.name)
                {
                    // Extract type parameter bindings from field type vs value type
                    self.extract_type_parameter_bindings(
                        &field_def.type_annotation,
                        &field_value_type,
                        &mut type_bindings,
                        &generic_params.params,
                    )?;
                }
            }
        }

        // Ensure all generic parameters have been inferred
        let mut inferred_args = Vec::new();
        for param in &generic_params.params {
            if let Some(inferred_type) = type_bindings.get(&param.name.name.clone()) {
                inferred_args.push(inferred_type.clone());
            } else {
                return Err(TypeError::CannotInferGenericType {
                    span: literal.span.to_source_span(),
                    type_param: param.name.name.clone(),
                });
            }
        }

        // Create the inferred generic type
        let struct_name = struct_def.name_as_string();
        let struct_type_id = self.compiler_environment.intern_type_name(&struct_name);
        Ok(crate::unification::StructuredType::Generic {
            base: struct_type_id,
            args: inferred_args,
        })
    }

    /// Extract type parameter bindings from field type annotation vs actual value type
    fn extract_type_parameter_bindings(
        &mut self,
        field_type_annotation: &outrun_parser::TypeAnnotation,
        field_value_type: &crate::unification::StructuredType,
        type_bindings: &mut std::collections::HashMap<String, crate::unification::StructuredType>,
        generic_params: &[outrun_parser::GenericParam],
    ) -> Result<(), TypeError> {
        // For now, implement simple case: T field gets bound to the value type
        // TODO: Handle more complex cases like Option<T>, List<T>, etc.
        if let outrun_parser::TypeAnnotation::Simple { path, .. } = field_type_annotation {
            // Check if this is a single identifier that matches a generic parameter
            if path.len() == 1 {
                let type_name = &path[0].name;
                if generic_params.iter().any(|p| p.name.name == *type_name) {
                    type_bindings.insert(type_name.clone(), field_value_type.clone());
                }
            }
        }
        Ok(())
    }

    /// Validate struct fields with generic context from type hint
    fn validate_struct_fields_with_generic_context(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        struct_def: &outrun_parser::StructDefinition,
        generic_args: &[crate::unification::StructuredType],
    ) -> Result<(), TypeError> {
        // Create generic substitution map
        let generic_params = struct_def.generic_params.as_ref().unwrap();
        let mut generic_substitutions = std::collections::HashMap::new();

        for (param, arg) in generic_params.params.iter().zip(generic_args.iter()) {
            generic_substitutions.insert(param.name.name.clone(), arg.clone());
        }

        // Validate each field
        for field in &literal.fields {
            if let outrun_parser::StructLiteralField::Assignment { name, value } = field {
                let field_value_type = self.check_expression_type(value)?;

                // Find field definition
                if let Some(field_def) = struct_def.fields.iter().find(|f| f.name.name == name.name)
                {
                    // Resolve field type with generic substitutions
                    let expected_field_type = self
                        .resolve_type_annotation_with_generic_substitution(
                            &field_def.type_annotation,
                            &generic_substitutions,
                        )?;

                    // Validate field type matches expected
                    if crate::unification::unify_structured_types(
                        &expected_field_type,
                        &field_value_type,
                        &self.compiler_environment.unification_context(),
                        &self.compiler_environment,
                    )?
                    .is_none()
                    {
                        return Err(TypeError::type_mismatch(
                            expected_field_type.to_string_representation(),
                            field_value_type.to_string_representation(),
                            value.span.to_source_span(),
                        ));
                    }
                } else {
                    return Err(TypeError::undefined_field(
                        name.name.clone(),
                        struct_def.name_as_string(),
                        name.span.to_source_span(),
                    ));
                }
            } else {
                return Err(TypeError::internal_with_span(
                    context::messages::not_yet_supported("Non-assignment struct fields"),
                    literal.span.to_source_span(),
                ));
            }
        }

        Ok(())
    }

    /// Validate struct fields for non-generic structs
    fn validate_struct_fields(
        &mut self,
        literal: &outrun_parser::StructLiteral,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<(), TypeError> {
        for field in &literal.fields {
            match field {
                outrun_parser::StructLiteralField::Assignment { name, value } => {
                    // Explicit assignment: { name: value }
                    let field_value_type = self.check_expression_type(value)?;

                    // Find field definition
                    if let Some(field_def) =
                        struct_def.fields.iter().find(|f| f.name.name == name.name)
                    {
                        let expected_field_type =
                            self.resolve_type_annotation(&field_def.type_annotation)?;

                        if crate::unification::unify_structured_types(
                            &expected_field_type,
                            &field_value_type,
                            &self.compiler_environment.unification_context(),
                            &self.compiler_environment,
                        )?
                        .is_none()
                        {
                            return Err(TypeError::type_mismatch(
                                expected_field_type.to_string_representation(),
                                field_value_type.to_string_representation(),
                                value.span.to_source_span(),
                            ));
                        }
                    } else {
                        return Err(TypeError::undefined_field(
                            name.name.clone(),
                            struct_def.name_as_string(),
                            name.span.to_source_span(),
                        ));
                    }
                }
                outrun_parser::StructLiteralField::Shorthand(name) => {
                    // Shorthand: { name } - field name is same as variable name
                    // Check that the field exists in the struct definition
                    if let Some(field_def) =
                        struct_def.fields.iter().find(|f| f.name.name == name.name)
                    {
                        let expected_field_type =
                            self.resolve_type_annotation(&field_def.type_annotation)?;

                        // Look up the variable in the current scope
                        if let Some(variable_type) = self.lookup_variable(&name.name) {
                            // Check that the variable type matches the field type
                            if crate::unification::unify_structured_types(
                                &expected_field_type,
                                variable_type,
                                &self.compiler_environment.unification_context(),
                                &self.compiler_environment,
                            )?
                            .is_none()
                            {
                                return Err(TypeError::type_mismatch(
                                    expected_field_type.to_string_representation(),
                                    variable_type.to_string_representation(),
                                    name.span.to_source_span(),
                                ));
                            }
                        } else {
                            return Err(TypeError::UndefinedVariable {
                                span: name.span.to_source_span(),
                                name: name.name.clone(),
                            });
                        }
                    } else {
                        return Err(TypeError::undefined_field(
                            name.name.clone(),
                            struct_def.name_as_string(),
                            name.span.to_source_span(),
                        ));
                    }
                }
                outrun_parser::StructLiteralField::Spread(_spread_identifier) => {
                    // Spread syntax: { ..other_struct }
                    // Requires struct type information and field compatibility checking
                    return Err(TypeError::internal_with_span(
                        context::messages::not_yet_implemented("Struct spread fields"),
                        literal.span.to_source_span(),
                    ));
                }
            }
        }

        Ok(())
    }

    /// Check qualified identifier type (e.g., Module.function_name)
    fn check_qualified_identifier_type(
        &mut self,
        qual_id: &outrun_parser::QualifiedIdentifier,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // For now, treat qualified identifiers as variables
        // This will need enhancement for proper module resolution
        let var_name = format!("{}.{}", qual_id.module.name, qual_id.name.name);
        self.lookup_variable(&var_name)
            .cloned()
            .ok_or_else(|| TypeError::UndefinedVariable {
                span: qual_id.span.to_source_span(),
                name: var_name,
            })
    }

    /// Check if expression type
    fn check_if_expression_type(
        &mut self,
        if_expr: &outrun_parser::IfExpression,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Check condition is Boolean
        let condition_type = self.check_expression_type(&if_expr.condition)?;
        let boolean_type_id = self.compiler_environment.intern_type_name("Boolean");
        let expected_condition_type = crate::unification::StructuredType::Simple(boolean_type_id);

        if crate::unification::unify_structured_types(
            &expected_condition_type,
            &condition_type,
            &self.compiler_environment.unification_context(),
            &self.compiler_environment,
        )?
        .is_none()
        {
            return Err(TypeError::type_mismatch(
                expected_condition_type.to_string_representation(),
                condition_type.to_string_representation(),
                if_expr.condition.span.to_source_span(),
            ));
        }

        // Check then branch type
        let then_type = self.check_block_type(&if_expr.then_block)?;

        // Check else branch type if present
        if let Some(else_block) = &if_expr.else_block {
            let else_type = self.check_block_type(else_block)?;

            // Both branches must have compatible types
            if crate::unification::unify_structured_types(
                &then_type,
                &else_type,
                &self.compiler_environment.unification_context(),
                &self.compiler_environment,
            )?
            .is_none()
            {
                return Err(TypeError::type_mismatch(
                    then_type.to_string_representation(),
                    else_type.to_string_representation(),
                    else_block.span.to_source_span(),
                ));
            }

            Ok(then_type)
        } else {
            // If no else branch, the then branch type must implement Default protocol
            let default_protocol_id = self.compiler_environment.intern_type_name("Default");

            // Check if then_type implements Default using the protocol registry
            match &then_type {
                crate::unification::StructuredType::Simple(type_id) => {
                    let type_structured =
                        crate::unification::StructuredType::Simple(type_id.clone());
                    let default_protocol_structured =
                        crate::unification::StructuredType::Simple(default_protocol_id);

                    if !self
                        .compiler_environment
                        .implements_protocol(&type_structured, &default_protocol_structured)
                    {
                        let type_name = type_id.to_string();
                        return Err(TypeError::ProtocolNotImplemented {
                            span: if_expr.then_block.span.to_source_span(),
                            protocol_name: "Default".to_string(),
                            type_name,
                        });
                    }
                }
                _ => {
                    // For generic types, tuple types, and function types, we need more sophisticated
                    // protocol implementation checking. For now, reject non-simple types.
                    return Err(TypeError::ProtocolNotImplemented {
                        span: if_expr.then_block.span.to_source_span(),
                        protocol_name: "Default".to_string(),
                        type_name: then_type.to_string_representation(),
                    });
                }
            }

            Ok(then_type)
        }
    }

    /// Check case expression type
    fn check_case_expression_type(
        &mut self,
        case_expr: &outrun_parser::CaseExpression,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Check the target expression type
        let _target_type = self.check_expression_type(&case_expr.expression)?;

        // Determine result type from clauses
        if case_expr.clauses.is_empty() {
            return Err(TypeError::internal_with_span(
                "Case expression must have at least one clause".to_string(),
                case_expr.span.to_source_span(),
            ));
        }

        // Check the guard expression of the first clause if present
        if let Some(guard) = &case_expr.clauses[0].guard {
            let guard_type = self.check_expression_type(guard)?;
            let boolean_type = crate::unification::StructuredType::Simple(
                self.compiler_environment.intern_type_name("Boolean"),
            );

            if crate::unification::unify_structured_types(
                &guard_type,
                &boolean_type,
                &self.compiler_environment.unification_context(),
                &self.compiler_environment,
            )?
            .is_none()
            {
                return Err(TypeError::type_mismatch(
                    "Boolean".to_string(),
                    guard_type.to_string_representation(),
                    guard.span.to_source_span(),
                ));
            }
        }

        // Get the result type from the first clause
        let first_result_type = match &case_expr.clauses[0].result {
            outrun_parser::CaseResult::Block(block) => self.check_block_type(block)?,
            outrun_parser::CaseResult::Expression(expr) => self.check_expression_type(expr)?,
        };

        // Verify all remaining clauses have compatible types
        for clause in case_expr.clauses.iter().skip(1) {
            // Check guard expression if present
            if let Some(guard) = &clause.guard {
                let guard_type = self.check_expression_type(guard)?;
                let boolean_type = crate::unification::StructuredType::Simple(
                    self.compiler_environment.intern_type_name("Boolean"),
                );

                if crate::unification::unify_structured_types(
                    &guard_type,
                    &boolean_type,
                    &self.compiler_environment.unification_context(),
                    &self.compiler_environment,
                )?
                .is_none()
                {
                    return Err(TypeError::type_mismatch(
                        "Boolean".to_string(),
                        guard_type.to_string_representation(),
                        guard.span.to_source_span(),
                    ));
                }
            }

            let clause_type = match &clause.result {
                outrun_parser::CaseResult::Block(block) => self.check_block_type(block)?,
                outrun_parser::CaseResult::Expression(expr) => self.check_expression_type(expr)?,
            };

            // Unify clause types
            if crate::unification::unify_structured_types(
                &first_result_type,
                &clause_type,
                &self.compiler_environment.unification_context(),
                &self.compiler_environment,
            )?
            .is_none()
            {
                return Err(TypeError::type_mismatch(
                    first_result_type.to_string_representation(),
                    clause_type.to_string_representation(),
                    clause.span.to_source_span(),
                ));
            }
        }

        Ok(first_result_type)
    }

    /// Check field access type
    fn check_field_access_type(
        &mut self,
        field_access: &outrun_parser::FieldAccess,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Check the object expression type
        let object_type = self.check_expression_type(&field_access.object)?;

        // Get the struct TypeId and generic arguments
        let (struct_type_id, generic_args) = match &object_type {
            crate::unification::StructuredType::Simple(type_id) => (type_id.clone(), None),
            crate::unification::StructuredType::Generic { base, args } => {
                (base.clone(), Some(args))
            }
            _ => {
                return Err(TypeError::internal_with_span(
                    "Field access only supported on struct types".to_string(),
                    field_access.object.span.to_source_span(),
                ));
            }
        };

        // Look up struct definition
        let struct_def = self
            .compiler_environment
            .get_struct(&struct_type_id)
            .ok_or_else(|| {
                TypeError::internal_with_span(
                    "Object type is not a struct".to_string(),
                    field_access.object.span.to_source_span(),
                )
            })?;

        // Find the field
        let field_name = &field_access.field.name;
        let field_def = struct_def
            .fields
            .iter()
            .find(|f| f.name.name == *field_name)
            .ok_or_else(|| {
                TypeError::undefined_field(
                    field_name.clone(),
                    struct_type_id.to_string(),
                    field_access.field.span.to_source_span(),
                )
            })?;

        // Return the field's type, applying generic substitutions if needed
        if let Some(args) = generic_args {
            // Build generic substitutions map
            let mut generic_substitutions = std::collections::HashMap::new();
            if let Some(ref generic_params) = struct_def.generic_params {
                for (i, generic_param) in generic_params.params.iter().enumerate() {
                    if let Some(arg_type) = args.get(i) {
                        generic_substitutions
                            .insert(generic_param.name.name.clone(), arg_type.clone());
                    }
                }
            }

            // Resolve field type with generic substitutions
            self.resolve_type_annotation_with_generic_substitution(
                &field_def.type_annotation,
                &generic_substitutions,
            )
        } else {
            // No generics, use regular resolution
            self.resolve_type_annotation(&field_def.type_annotation)
        }
    }

    /// Check function call type
    fn check_function_call_type(
        &mut self,
        call: &outrun_parser::FunctionCall,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Resolve function name to AtomId
        let func_name = match &call.path {
            outrun_parser::FunctionPath::Simple { name } => name.name.clone(),
            outrun_parser::FunctionPath::Qualified { module, name } => {
                format!("{}.{}", module.name, name.name)
            }
            outrun_parser::FunctionPath::Expression { expression: _ } => {
                // Function expressions (like captured functions) need special handling
                // For now, return a placeholder name
                "anonymous_function".to_string()
            }
        };

        let _func_atom = self.compiler_environment.intern_atom_name(&func_name);

        // Look up function definition using hierarchical registry
        match &call.path {
            outrun_parser::FunctionPath::Qualified { module, name } => {
                // Qualified call like "Option.some?" or "Outrun.Intrinsic.list_inspect"
                let module_name = &module.name;
                let function_name = &name.name;

                // Get module TypeId (base type without generics)
                let module_type_id = self.compiler_environment.intern_type_name(module_name);
                let module_type = StructuredType::Simple(module_type_id.clone());
                let function_name_atom = self.compiler_environment.intern_atom_name(function_name);

                // Check if this is a protocol that has implementations
                let module_structured =
                    crate::unification::StructuredType::Simple(module_type_id.clone());
                let is_protocol = self.compiler_environment.is_protocol(&module_structured);

                if is_protocol {
                    // For protocol calls, first infer the implementing type from arguments
                    let protocol_func_entry = self
                        .compiler_environment
                        .lookup_qualified_function(&module_type, function_name_atom.clone());

                    if let Some(protocol_func) = protocol_func_entry {
                        // Check if this is a static function (defs) - static functions don't need Self parameter resolution
                        if protocol_func.function_type() == FunctionType::ProtocolStatic {
                            // Static functions are called directly on the protocol, no implementation dispatch needed

                            // Store static dispatch strategy
                            let mut context = self.compiler_environment.unification_context();
                            context.add_dispatch_strategy(
                                call.span,
                                crate::checker::DispatchMethod::Static {
                                    function_id: protocol_func.function_id().to_string(),
                                },
                            );
                            self.compiler_environment.set_unification_context(context);

                            // First, infer generic type parameters from arguments and return type hint for static protocol functions
                            let generic_substitutions = self
                                .infer_generic_parameters_from_arguments(
                                    protocol_func.definition(),
                                    call,
                                    type_hint,
                                )?;

                            // Then validate arguments using the inferred generic types
                            self.validate_function_call_arguments_with_generic_substitution(
                                call,
                                protocol_func.definition(),
                                &generic_substitutions,
                            )?;

                            return self.resolve_type_annotation_with_generic_substitution(
                                &protocol_func.definition().return_type,
                                &generic_substitutions,
                            );
                        }

                        // Protocol functions with default implementations (ProtocolDefault) should be treated
                        // as protocol dispatch, not static dispatch - they need to find an implementation
                        // or use the default implementation

                        // For non-static protocol functions, infer the concrete implementing type from the first Self parameter
                        let implementing_structured_type = self
                            .infer_implementing_type_from_arguments(
                                module_type_id.clone(),
                                protocol_func.definition(),
                                call,
                            )?;

                        // Extract base TypeId for implementation lookup

                        // Look up the actual implementation function
                        if let Some(impl_func_def) = self.compiler_environment.lookup_impl_function(
                            &module_type,
                            &implementing_structured_type,
                            function_name_atom,
                        ) {
                            // Store protocol dispatch strategy
                            let mut context = self.compiler_environment.unification_context();
                            context.add_dispatch_strategy(
                                call.span,
                                crate::checker::DispatchMethod::Protocol {
                                    protocol_name: module_name.clone(),
                                    function_name: function_name.clone(),
                                    impl_type: Box::new(implementing_structured_type.clone()),
                                },
                            );
                            self.compiler_environment.set_unification_context(context);

                            // Use the full structured type for Self context (preserving generics)
                            let inferred_self_type = implementing_structured_type;
                            self.validate_function_call_arguments_with_self(
                                call,
                                impl_func_def.definition(),
                                &inferred_self_type,
                            )?;
                            self.resolve_type_annotation_with_self(
                                &impl_func_def.definition().return_type,
                                &inferred_self_type,
                            )
                        } else {
                            // No implementation found - check if protocol function has a default implementation
                            match protocol_func.function_type() {
                                crate::compilation::FunctionType::ProtocolSignature => {
                                    // Protocol function signature without implementation - this is an error
                                    let inferred_self_type = self
                                        .infer_protocol_self_type_from_arguments(
                                            module_type_id.clone(),
                                            protocol_func.definition(),
                                            call,
                                        )?;
                                    Err(TypeError::ProtocolNotImplemented {
                                        span: call.span.to_source_span(),
                                        protocol_name: module_name.clone(),
                                        type_name: inferred_self_type.to_string_representation(),
                                    })
                                }
                                crate::compilation::FunctionType::ProtocolDefault => {
                                    // Protocol function with default implementation - use it
                                    let inferred_self_type = self
                                        .infer_protocol_self_type_from_arguments(
                                            module_type_id.clone(),
                                            protocol_func.definition(),
                                            call,
                                        )?;

                                    // Store protocol dispatch strategy for default implementation
                                    let mut context =
                                        self.compiler_environment.unification_context();
                                    context.add_dispatch_strategy(
                                        call.span,
                                        crate::checker::DispatchMethod::Protocol {
                                            protocol_name: module_name.clone(),
                                            function_name: function_name.clone(),
                                            impl_type: Box::new(inferred_self_type.clone()),
                                        },
                                    );
                                    self.compiler_environment.set_unification_context(context);

                                    self.validate_function_call_arguments_with_self(
                                        call,
                                        protocol_func.definition(),
                                        &inferred_self_type,
                                    )?;
                                    self.resolve_type_annotation_with_self(
                                        &protocol_func.definition().return_type,
                                        &inferred_self_type,
                                    )
                                }
                                _ => {
                                    // Other function types shouldn't reach here in protocol dispatch
                                    Err(TypeError::internal_with_span(
                                        format!(
                                            "Unexpected function type {:?} in protocol dispatch",
                                            protocol_func.function_type()
                                        ),
                                        call.span.to_source_span(),
                                    ))
                                }
                            }
                        }
                    } else {
                        // No static protocol function found, try to find impl function
                        self.try_protocol_impl_function_call(
                            module_type_id,
                            function_name_atom,
                            call,
                        )
                    }
                } else if let Some(func_entry) = self
                    .compiler_environment
                    .lookup_qualified_function(&module_type, function_name_atom.clone())
                {
                    // Regular module function call - store static dispatch strategy
                    let mut context = self.compiler_environment.unification_context();
                    context.add_dispatch_strategy(
                        call.span,
                        crate::checker::DispatchMethod::Static {
                            function_id: func_entry.function_id().to_string(),
                        },
                    );
                    self.compiler_environment.set_unification_context(context);

                    self.validate_function_call_arguments(call, func_entry.definition())?;
                    self.resolve_type_annotation(&func_entry.definition().return_type)
                } else {
                    Err(TypeError::undefined_function(
                        func_name,
                        call.span.to_source_span(),
                    ))
                }
            }
            outrun_parser::FunctionPath::Simple { name } => {
                // Simple call like "some_function" - search in function registry
                let function_name = &name.name;
                let function_name_atom = self.compiler_environment.intern_atom_name(function_name);

                if let Some(func_entry) = self
                    .compiler_environment
                    .lookup_local_function(function_name_atom.clone())
                {
                    // Store static dispatch strategy for simple function call
                    let mut context = self.compiler_environment.unification_context();
                    context.add_dispatch_strategy(
                        call.span,
                        crate::checker::DispatchMethod::Static {
                            function_id: func_entry.function_id().to_string(),
                        },
                    );
                    self.compiler_environment.set_unification_context(context);

                    // Validate arguments match parameters
                    self.validate_function_call_arguments(call, func_entry.definition())?;
                    // Return the declared return type
                    self.resolve_type_annotation(&func_entry.definition().return_type)
                } else {
                    Err(TypeError::undefined_function(
                        function_name.clone(),
                        call.span.to_source_span(),
                    ))
                }
            }
            outrun_parser::FunctionPath::Expression { .. } => {
                // Function expressions need special handling
                Err(TypeError::undefined_function(
                    "anonymous_function".to_string(),
                    call.span.to_source_span(),
                ))
            }
        }
    }

    /// Validate function call arguments against function definition parameters
    fn validate_function_call_arguments(
        &mut self,
        call: &outrun_parser::FunctionCall,
        func_def: &outrun_parser::FunctionDefinition,
    ) -> Result<(), TypeError> {
        use std::collections::HashSet;

        // Track which parameters have been provided
        let mut provided_params: HashSet<String> = HashSet::new();

        // Validate each argument
        for arg in &call.arguments {
            match arg {
                outrun_parser::Argument::Named {
                    name,
                    expression,
                    format,
                    span: _,
                } => {
                    let param_name = &name.name;

                    // Check if parameter exists in function definition
                    let param_def = func_def
                        .parameters
                        .iter()
                        .find(|p| p.name.name == *param_name)
                        .ok_or_else(|| TypeError::UnknownParameter {
                            span: name.span.to_source_span(),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        })?;

                    // Check for duplicate arguments
                    if provided_params.contains(param_name) {
                        return Err(TypeError::DuplicateArgument {
                            span: name.span.to_source_span(),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        });
                    }
                    provided_params.insert(param_name.clone());

                    // Type check the argument expression
                    let arg_type = self.check_expression_type(expression)?;
                    // Store the resolved type for later use by TypedASTBuilder
                    let mut context = self.compiler_environment.unification_context();
                    context.add_expression_type(expression.span, arg_type.clone());
                    self.compiler_environment.set_unification_context(context);

                    let expected_param_type =
                        self.resolve_type_annotation(&param_def.type_annotation)?;

                    // Validate argument type matches parameter type
                    match crate::unification::unify_structured_types(
                        &arg_type,
                        &expected_param_type,
                        &self.compiler_environment.unification_context(),
                        &self.compiler_environment,
                    ) {
                        Ok(None) | Err(_) => {
                            return Err(TypeError::ArgumentTypeMismatch {
                                span: expression.span.to_source_span(),
                                function_name: func_def.name.name.clone(),
                                parameter_name: param_name.clone(),
                                expected_type: expected_param_type.to_string_representation(),
                                found_type: arg_type.to_string_representation(),
                            });
                        }
                        Ok(Some(_)) => {} // Types unify successfully
                    }

                    // For shorthand format, validate that argument name matches parameter name
                    if let outrun_parser::ArgumentFormat::Shorthand = format {
                        // Shorthand is valid since we already found the matching parameter
                    }
                }
                outrun_parser::Argument::Spread {
                    expression: _,
                    kind: _,
                    span,
                } => {
                    // TODO: Implement spread argument validation
                    // For now, return an error as spread arguments need special handling
                    return Err(TypeError::UnsupportedFeature {
                        span: span.to_source_span(),
                        feature: "Spread arguments".to_string(),
                    });
                }
            }
        }

        // Check that all required parameters have been provided
        for param in &func_def.parameters {
            if !provided_params.contains(&param.name.name.clone()) {
                // Check if parameter type implements Default protocol (making it optional)
                let param_type = self.resolve_type_annotation(&param.type_annotation)?;
                let default_protocol_id = self.compiler_environment.intern_type_name("Default");

                // Check if param_type implements Default using CompilerEnvironment
                let default_protocol_structured =
                    crate::unification::StructuredType::Simple(default_protocol_id);
                let implements_default = match &param_type {
                    crate::unification::StructuredType::Simple(_type_id) => self
                        .compiler_environment
                        .implements_protocol(&param_type, &default_protocol_structured),
                    _ => {
                        // For complex types (generics, functions, tuples), assume they don't implement Default
                        // This is a simplification - in a full implementation, we'd need to check recursively
                        false
                    }
                };

                // If the parameter type doesn't implement Default, it's required
                if !implements_default {
                    return Err(TypeError::MissingArgument {
                        span: call.span.to_source_span(),
                        function_name: func_def.name.name.clone(),
                        parameter_name: param.name.name.clone(),
                    });
                }
                // If it implements Default, the parameter is optional and can be skipped
            }
        }

        Ok(())
    }

    /// Validate function call arguments against function definition parameters with Self context
    /// This is used for protocol function calls where Self needs to be resolved to the protocol type
    fn validate_function_call_arguments_with_self(
        &mut self,
        call: &outrun_parser::FunctionCall,
        func_def: &outrun_parser::FunctionDefinition,
        self_type: &crate::unification::StructuredType,
    ) -> Result<(), TypeError> {
        use std::collections::HashSet;

        // Track which parameters have been provided
        let mut provided_params: HashSet<String> = HashSet::new();

        // Validate each argument
        for arg in &call.arguments {
            match arg {
                outrun_parser::Argument::Named {
                    name,
                    expression,
                    format: _,
                    span: _,
                } => {
                    let param_name = &name.name;

                    // Check if parameter exists in function definition
                    let param_def = func_def
                        .parameters
                        .iter()
                        .find(|p| p.name.name == *param_name)
                        .ok_or_else(|| TypeError::UnknownParameter {
                            span: name.span.to_source_span(),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        })?;

                    // Check for duplicate arguments
                    if provided_params.contains(param_name) {
                        return Err(TypeError::DuplicateArgument {
                            span: name.span.to_source_span(),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        });
                    }
                    provided_params.insert(param_name.clone());

                    // Type check the argument expression
                    let arg_type = self.check_expression_type(expression)?;
                    // Store the resolved type for later use by TypedASTBuilder
                    let mut context = self.compiler_environment.unification_context();
                    context.add_expression_type(expression.span, arg_type.clone());
                    self.compiler_environment.set_unification_context(context);

                    // Resolve parameter type with Self context
                    let expected_param_type = self
                        .resolve_type_annotation_with_self(&param_def.type_annotation, self_type)?;

                    // Validate argument type matches parameter type
                    match crate::unification::unify_structured_types(
                        &arg_type,
                        &expected_param_type,
                        &self.compiler_environment.unification_context(),
                        &self.compiler_environment,
                    ) {
                        Ok(None) | Err(_) => {
                            return Err(TypeError::ArgumentTypeMismatch {
                                span: expression.span.to_source_span(),
                                function_name: func_def.name.name.clone(),
                                parameter_name: param_name.clone(),
                                expected_type: expected_param_type.to_string_representation(),
                                found_type: arg_type.to_string_representation(),
                            });
                        }
                        Ok(Some(_)) => {} // Types unify successfully
                    }
                }
                outrun_parser::Argument::Spread { .. } => {
                    // Spread arguments not yet implemented
                    return Err(TypeError::internal_with_span(
                        context::messages::not_yet_supported("Spread arguments in function calls"),
                        call.span.to_source_span(),
                    ));
                }
            }
        }

        // Check that all required parameters have been provided
        for param in &func_def.parameters {
            if !provided_params.contains(&param.name.name.clone()) {
                // Check if parameter type implements Default protocol (making it optional)
                let param_type =
                    self.resolve_type_annotation_with_self(&param.type_annotation, self_type)?;
                let default_protocol_id = self.compiler_environment.intern_type_name("Default");

                // Check if param_type implements Default using CompilerEnvironment
                let default_protocol_structured =
                    crate::unification::StructuredType::Simple(default_protocol_id);
                let implements_default = match &param_type {
                    crate::unification::StructuredType::Simple(_type_id) => self
                        .compiler_environment
                        .implements_protocol(&param_type, &default_protocol_structured),
                    _ => {
                        // For complex types (generics, tuples, functions), conservatively assume they don't implement Default
                        false
                    }
                };

                // If the parameter type doesn't implement Default, it's required
                if !implements_default {
                    return Err(TypeError::MissingArgument {
                        span: call.span.to_source_span(),
                        function_name: func_def.name.name.clone(),
                        parameter_name: param.name.name.clone(),
                    });
                }
                // If it implements Default, the parameter is optional and can be skipped
            }
        }

        Ok(())
    }

    /// Validate function call arguments with generic type substitution
    fn validate_function_call_arguments_with_generic_substitution(
        &mut self,
        call: &outrun_parser::FunctionCall,
        func_def: &outrun_parser::FunctionDefinition,
        generic_substitutions: &std::collections::HashMap<
            String,
            crate::unification::StructuredType,
        >,
    ) -> Result<(), TypeError> {
        use std::collections::HashSet;

        // Track which parameters have been provided
        let mut provided_params: HashSet<String> = HashSet::new();

        // Validate each argument
        for arg in &call.arguments {
            match arg {
                outrun_parser::Argument::Named {
                    name,
                    expression,
                    format: _,
                    span: _,
                } => {
                    let param_name = &name.name;

                    // Check if parameter exists in function definition
                    let param_def = func_def
                        .parameters
                        .iter()
                        .find(|p| p.name.name == *param_name)
                        .ok_or_else(|| TypeError::UnknownParameter {
                            span: name.span.to_source_span(),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        })?;

                    // Check for duplicate arguments
                    if provided_params.contains(param_name) {
                        return Err(TypeError::DuplicateArgument {
                            span: name.span.to_source_span(),
                            function_name: func_def.name.name.clone(),
                            parameter_name: param_name.clone(),
                        });
                    }
                    provided_params.insert(param_name.clone());

                    // Type check the argument expression
                    let arg_type = self.check_expression_type(expression)?;
                    // Store the resolved type for later use by TypedASTBuilder
                    let mut context = self.compiler_environment.unification_context();
                    context.add_expression_type(expression.span, arg_type.clone());
                    self.compiler_environment.set_unification_context(context);

                    // Resolve parameter type with generic substitution
                    let expected_param_type = self
                        .resolve_type_annotation_with_generic_substitution(
                            &param_def.type_annotation,
                            generic_substitutions,
                        )?;

                    // Validate argument type matches parameter type
                    match crate::unification::unify_structured_types(
                        &arg_type,
                        &expected_param_type,
                        &self.compiler_environment.unification_context(),
                        &self.compiler_environment,
                    ) {
                        Ok(None) | Err(_) => {
                            return Err(TypeError::ArgumentTypeMismatch {
                                span: expression.span.to_source_span(),
                                function_name: func_def.name.name.clone(),
                                parameter_name: param_name.clone(),
                                expected_type: expected_param_type.to_string_representation(),
                                found_type: arg_type.to_string_representation(),
                            });
                        }
                        Ok(Some(_)) => {} // Types unify successfully
                    }
                }
                outrun_parser::Argument::Spread { .. } => {
                    // Spread arguments not yet implemented
                    return Err(TypeError::internal_with_span(
                        context::messages::not_yet_supported("Spread arguments in function calls"),
                        call.span.to_source_span(),
                    ));
                }
            }
        }

        // Check that all required parameters have been provided
        for param in &func_def.parameters {
            if !provided_params.contains(&param.name.name.clone()) {
                // Check if parameter type implements Default protocol (making it optional)
                let param_type = self.resolve_type_annotation_with_generic_substitution(
                    &param.type_annotation,
                    generic_substitutions,
                )?;
                let default_protocol_id = self.compiler_environment.intern_type_name("Default");

                // Check if param_type implements Default using CompilerEnvironment
                let default_protocol_structured =
                    crate::unification::StructuredType::Simple(default_protocol_id);
                let implements_default = match &param_type {
                    crate::unification::StructuredType::Simple(_type_id) => self
                        .compiler_environment
                        .implements_protocol(&param_type, &default_protocol_structured),
                    _ => {
                        // For complex types (generics, tuples, functions), conservatively assume they don't implement Default
                        false
                    }
                };

                // If the parameter type doesn't implement Default, it's required
                if !implements_default {
                    return Err(TypeError::MissingArgument {
                        span: call.span.to_source_span(),
                        function_name: func_def.name.name.clone(),
                        parameter_name: param.name.name.clone(),
                    });
                }
                // If it implements Default, the parameter is optional and can be skipped
            }
        }

        Ok(())
    }

    /// Check variable type using scope tracking
    fn check_variable_type(
        &mut self,
        var: &outrun_parser::Identifier,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        if let Some(var_type) = self.lookup_variable(&var.name) {
            Ok(var_type.clone())
        } else {
            Err(TypeError::UndefinedVariable {
                span: var.span.to_source_span(),
                name: var.name.clone(),
            })
        }
    }

    /// Check the type of a block (sequence of statements) - for simple sequential blocks
    /// Note: if/case expressions handle their own branch scoping in check_expression_type
    fn check_block_type(
        &mut self,
        block: &outrun_parser::Block,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        self.check_block_type_with_hint(block, None)
    }

    /// Check the type of a block with an optional expected type hint for the final expression
    fn check_block_type_with_hint(
        &mut self,
        block: &outrun_parser::Block,
        expected_type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let mut last_type = None;

        for (i, statement) in block.statements.iter().enumerate() {
            let is_final_statement = i == block.statements.len() - 1;

            match &statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    // For the final expression, use the expected type hint if provided
                    last_type = if is_final_statement && expected_type_hint.is_some() {
                        Some(self.check_expression_type_with_hint(expr, expected_type_hint)?)
                    } else {
                        Some(self.check_expression_type(expr)?)
                    };
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    // If there's a type annotation, resolve it first to use as a hint
                    let type_hint = if let Some(type_annotation) = &let_binding.type_annotation {
                        Some(self.resolve_type_annotation(type_annotation)?)
                    } else {
                        None
                    };

                    // Check the expression type with the hint
                    let expr_type = self.check_expression_type_with_hint(
                        &let_binding.expression,
                        type_hint.as_ref(),
                    )?;

                    // If there was a type annotation, validate it matches the expression
                    if let Some(expected_type) = &type_hint {
                        if crate::unification::unify_structured_types(
                            &expr_type,
                            expected_type,
                            &self.compiler_environment.unification_context(),
                            &self.compiler_environment,
                        )?
                        .is_none()
                        {
                            return Err(TypeError::type_mismatch(
                                expected_type.to_string_representation(),
                                expr_type.to_string_representation(),
                                let_binding.expression.span.to_source_span(),
                            ));
                        }
                    }

                    // Validate pattern against expression type and extract bound variables
                    match self.check_pattern_type(&let_binding.pattern, &expr_type) {
                        Ok(bound_variables) => {
                            // Register all pattern-bound variables in scope
                            for (var_name, var_type) in bound_variables {
                                self.register_variable(var_name, var_type);
                            }
                        }
                        Err(pattern_error) => {
                            // Pattern doesn't match expression type
                            return Err(pattern_error);
                        }
                    }

                    last_type = Some(expr_type);
                }
            }
        }

        // Return the type of the last expression, or Unit if empty block
        Ok(last_type.unwrap_or_else(|| {
            let unit_type_id = self.compiler_environment.intern_type_name("Unit");
            crate::unification::StructuredType::Simple(unit_type_id)
        }))
    }

    /// Check function definition and return let-bound variables
    fn check_function_definition_with_variables(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
    ) -> Result<std::collections::HashMap<String, crate::unification::StructuredType>, TypeError>
    {
        // Functions must always have a return type in Outrun
        let expected_return_type = self.resolve_type_annotation(&func.return_type)?;

        // Create new scope for function parameters and body
        self.push_scope();

        // Validate parameter types and register them in scope
        for param in &func.parameters {
            let param_type = self.resolve_type_annotation(&param.type_annotation)?;
            self.register_variable(param.name.name.clone(), param_type);
        }

        // Type check guard expression if present (after parameters are in scope)
        if let Some(guard_clause) = &func.guard {
            let guard_type = self.check_expression_type(&guard_clause.condition)?;
            let boolean_type = crate::unification::StructuredType::Simple(
                self.compiler_environment.intern_type_name("Boolean"),
            );

            if crate::unification::unify_structured_types(
                &guard_type,
                &boolean_type,
                &self.compiler_environment.unification_context(),
                &self.compiler_environment,
            )?
            .is_none()
            {
                return Err(TypeError::type_mismatch(
                    "Boolean".to_string(),
                    guard_type.to_string_representation(),
                    guard_clause.condition.span.to_source_span(),
                ));
            }
        }

        // Type check function body with expected return type as hint (this will register let-bound variables)
        let body_type = self.check_block_type_with_hint(&func.body, Some(&expected_return_type))?;

        // Validate body type matches return type
        match crate::unification::unify_structured_types(
            &body_type,
            &expected_return_type,
            &self.compiler_environment.unification_context(),
            &self.compiler_environment,
        ) {
            Ok(None) | Err(_) => {
                self.errors.push(TypeError::type_mismatch(
                    expected_return_type.to_string_representation(),
                    body_type.to_string_representation(),
                    func.body.span.to_source_span(),
                ));
            }
            Ok(Some(_)) => {} // Types unify successfully
        }

        // Capture let-bound variables from the current scope (excluding parameters)
        let mut let_bound_variables = std::collections::HashMap::new();
        if let Some(current_scope) = self.variable_scopes.last() {
            for (var_name, var_type) in current_scope {
                // Skip parameters, only capture let-bound variables
                let is_parameter = func.parameters.iter().any(|p| p.name.name == *var_name);
                if !is_parameter {
                    let_bound_variables.insert(var_name.clone(), var_type.clone());
                }
            }
        }

        // Pop function scope
        self.pop_scope();

        Ok(let_bound_variables)
    }

    /// Check struct definition
    fn check_struct_definition(
        &mut self,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<(), TypeError> {
        // Validate all field types exist
        for field in &struct_def.fields {
            let _field_type = self.resolve_type_annotation(&field.type_annotation)?;
        }
        Ok(())
    }

    /// Check protocol definition
    fn check_protocol_definition(
        &mut self,
        protocol_def: &outrun_parser::ProtocolDefinition,
    ) -> Result<(), TypeError> {
        // Validate all function signatures
        for protocol_func in &protocol_def.functions {
            match protocol_func {
                outrun_parser::ProtocolFunction::Signature(sig) => {
                    // Validate return type
                    let _return_type = self.resolve_type_annotation(&sig.return_type)?;

                    // Validate parameter types
                    for param in &sig.parameters {
                        let _param_type = self.resolve_type_annotation(&param.type_annotation)?;
                    }
                }
                outrun_parser::ProtocolFunction::Definition(func) => {
                    // Validate return type
                    let _return_type = self.resolve_type_annotation(&func.return_type)?;

                    // Validate parameter types
                    for param in &func.parameters {
                        let _param_type = self.resolve_type_annotation(&param.type_annotation)?;
                    }
                }
                outrun_parser::ProtocolFunction::StaticDefinition(static_func) => {
                    // Validate return type
                    let _return_type = self.resolve_type_annotation(&static_func.return_type)?;

                    // Validate parameter types
                    for param in &static_func.parameters {
                        let _param_type = self.resolve_type_annotation(&param.type_annotation)?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Check impl block
    fn check_impl_block(&mut self, impl_block: &outrun_parser::ImplBlock) -> Result<(), TypeError> {
        // Validate protocol and type exist
        let protocol_type = self.resolve_type_spec(&impl_block.protocol_spec)?;
        let impl_type = self.resolve_type_spec(&impl_block.type_spec)?;

        // Get protocol TypeId for lookup
        let protocol_id = match protocol_type {
            crate::unification::StructuredType::Simple(type_id) => type_id,
            crate::unification::StructuredType::Generic { base, .. } => {
                // For generic protocols like List<T>, use the base protocol type
                base
            }
            _ => {
                return Err(TypeError::internal_with_span(
                    "Expected simple or generic type for protocol in impl block".to_string(),
                    impl_block.protocol_spec.span.to_source_span(),
                ));
            }
        };

        // Look up protocol definition (clone to avoid borrowing issues)
        let protocol_def = self
            .compiler_environment
            .get_protocol(&protocol_id)
            .ok_or_else(|| {
                let protocol_name = impl_block
                    .protocol_spec
                    .path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");
                TypeError::UndefinedProtocol {
                    span: impl_block.protocol_spec.span.to_source_span(),
                    protocol_name,
                }
            })?;

        // Validate impl functions match protocol signatures
        self.validate_impl_functions(impl_block, &protocol_def, protocol_id, impl_type)?;

        Ok(())
    }

    /// Validate that impl block functions match protocol signatures exactly
    fn validate_impl_functions(
        &mut self,
        impl_block: &outrun_parser::ImplBlock,
        protocol_def: &outrun_parser::ProtocolDefinition,
        protocol_id: TypeNameId,
        impl_type: crate::unification::StructuredType,
    ) -> Result<(), TypeError> {
        use std::collections::{HashMap, HashSet};

        // Collect protocol function signatures by name
        let mut protocol_functions: HashMap<String, &outrun_parser::ProtocolFunction> =
            HashMap::new();
        for protocol_func in &protocol_def.functions {
            let func_name = match protocol_func {
                outrun_parser::ProtocolFunction::Signature(sig) => &sig.name.name,
                outrun_parser::ProtocolFunction::Definition(func) => &func.name.name,
                outrun_parser::ProtocolFunction::StaticDefinition(static_func) => {
                    &static_func.name.name
                }
            };
            protocol_functions.insert(func_name.clone(), protocol_func);
        }

        // Track which protocol functions have been implemented
        let mut implemented_functions: HashSet<String> = HashSet::new();

        // Validate each impl function
        for impl_func in &impl_block.methods {
            let func_name = &impl_func.name.name;
            implemented_functions.insert(func_name.clone());

            // Check if function exists in protocol
            let protocol_func = protocol_functions.get(func_name).ok_or_else(|| {
                TypeError::ExtraImplementation {
                    span: impl_func.span.to_source_span(),
                    protocol_name: protocol_id.to_string(),
                    function_name: func_name.clone(),
                }
            })?;

            // Validate function signatures match
            self.validate_function_signature_match(impl_func, protocol_func, &impl_type)?;
        }

        // Check for missing implementations - only check functions without default implementations

        for (protocol_func_name, protocol_func) in &protocol_functions {
            if !implemented_functions.contains(protocol_func_name) {
                // Only require implementation if this is a signature without a default implementation
                let requires_implementation =
                    matches!(protocol_func, outrun_parser::ProtocolFunction::Signature(_));

                if requires_implementation {
                    return Err(TypeError::MissingImplementation {
                        span: impl_block.span.to_source_span(),
                        protocol_name: protocol_id.to_string(),
                        type_name: impl_type.to_string_representation(),
                        function_name: protocol_func_name.clone(),
                    });
                }
                // Functions with default implementations are optional - no need to require them
            }
        }

        Ok(())
    }

    /// Validate that an impl function signature matches the protocol function signature exactly
    fn validate_function_signature_match(
        &mut self,
        impl_func: &outrun_parser::FunctionDefinition,
        protocol_func: &outrun_parser::ProtocolFunction,
        impl_type: &crate::unification::StructuredType,
    ) -> Result<(), TypeError> {
        // Get protocol function signature details
        let (protocol_params, protocol_return_type, protocol_visibility) = match protocol_func {
            outrun_parser::ProtocolFunction::Signature(sig) => {
                (&sig.parameters, &sig.return_type, sig.visibility.clone())
            }
            outrun_parser::ProtocolFunction::Definition(func) => {
                (&func.parameters, &func.return_type, func.visibility.clone())
            }
            outrun_parser::ProtocolFunction::StaticDefinition(_static_func) => {
                // Static functions should not be implemented in impl blocks
                return Err(TypeError::SignatureMismatch {
                    span: impl_func.span.to_source_span(),
                    function_name: impl_func.name.name.clone(),
                    expected: "Static function (should not be implemented in impl)".to_string(),
                    found: "Instance function".to_string(),
                });
            }
        };

        // Check visibility matches
        if impl_func.visibility != protocol_visibility {
            return Err(TypeError::SignatureMismatch {
                span: impl_func.span.to_source_span(),
                function_name: impl_func.name.name.clone(),
                expected: format!("Visibility: {protocol_visibility:?}"),
                found: format!("Visibility: {:?}", impl_func.visibility),
            });
        }

        // Check parameter count matches
        if impl_func.parameters.len() != protocol_params.len() {
            return Err(TypeError::SignatureMismatch {
                span: impl_func.span.to_source_span(),
                function_name: impl_func.name.name.clone(),
                expected: format!("{} parameters", protocol_params.len()),
                found: format!("{} parameters", impl_func.parameters.len()),
            });
        }

        // Check each parameter matches (name and type)
        for (impl_param, protocol_param) in impl_func.parameters.iter().zip(protocol_params.iter())
        {
            // Check parameter names match
            if impl_param.name.name.clone() != protocol_param.name.name.clone() {
                return Err(TypeError::SignatureMismatch {
                    span: impl_param.span.to_source_span(),
                    function_name: impl_func.name.name.clone(),
                    expected: format!("Parameter name: {}", protocol_param.name.name.clone()),
                    found: format!("Parameter name: {}", impl_param.name.name.clone()),
                });
            }

            // Check parameter types match (with Self substitution)
            let impl_param_type =
                self.resolve_type_annotation_with_self(&impl_param.type_annotation, impl_type)?;
            let protocol_param_type =
                self.resolve_type_annotation_with_self(&protocol_param.type_annotation, impl_type)?;

            match crate::unification::unify_structured_types(
                &impl_param_type,
                &protocol_param_type,
                &self.compiler_environment.unification_context(),
                &self.compiler_environment,
            ) {
                Ok(None) | Err(_) => {
                    return Err(TypeError::SignatureMismatch {
                        span: impl_param.span.to_source_span(),
                        function_name: impl_func.name.name.clone(),
                        expected: format!(
                            "Parameter {}: {}",
                            protocol_param.name.name.clone(),
                            protocol_param_type.to_string_representation()
                        ),
                        found: format!(
                            "Parameter {}: {}",
                            impl_param.name.name.clone(),
                            impl_param_type.to_string_representation()
                        ),
                    });
                }
                Ok(Some(_)) => {} // Types unify successfully
            }
        }

        // Check return types match (with Self substitution)
        let impl_return_type =
            self.resolve_type_annotation_with_self(&impl_func.return_type, impl_type)?;
        let protocol_return_type =
            self.resolve_type_annotation_with_self(protocol_return_type, impl_type)?;

        match crate::unification::unify_structured_types(
            &impl_return_type,
            &protocol_return_type,
            &self.compiler_environment.unification_context(),
            &self.compiler_environment,
        ) {
            Ok(None) | Err(_) => {
                return Err(TypeError::SignatureMismatch {
                    span: impl_func.span.to_source_span(),
                    function_name: impl_func.name.name.clone(),
                    expected: format!(
                        "Return type: {}",
                        protocol_return_type.to_string_representation()
                    ),
                    found: format!(
                        "Return type: {}",
                        impl_return_type.to_string_representation()
                    ),
                });
            }
            Ok(Some(_)) => {} // Types unify successfully
        }

        Ok(())
    }

    /// Resolve type annotation with Self substitution for impl blocks
    fn resolve_type_annotation_with_self(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
        self_type: &crate::unification::StructuredType,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Special handling for Self type resolution with structured types
        self.resolve_type_annotation_with_self_substitution(type_annotation, self_type)
    }

    /// Recursively resolve type annotation with Self substitution
    fn resolve_type_annotation_with_self_substitution(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
        self_type: &crate::unification::StructuredType,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path,
                generic_args,
                span: _,
            } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Handle Self substitution directly
                if type_name == "Self" {
                    return Ok(self_type.clone());
                }

                // Handle other type parameters using the existing system
                if let Some(type_param_type) = self.lookup_type_parameter(&type_name) {
                    return Ok(type_param_type.clone());
                }

                // Intern the type name using CompilerEnvironment
                {
                    let type_id = self.compiler_environment.intern_type_name(&type_name);

                    // Handle generic arguments with Self substitution
                    if let Some(ref args) = generic_args {
                        let mut arg_types = Vec::new();
                        for arg in &args.args {
                            let arg_type = self
                                .resolve_type_annotation_with_self_substitution(arg, self_type)?;
                            arg_types.push(arg_type);
                        }

                        Ok(crate::unification::StructuredType::Generic {
                            base: type_id,
                            args: arg_types,
                        })
                    } else {
                        Ok(crate::unification::StructuredType::Simple(type_id))
                    }
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                let mut element_types = Vec::new();
                for element_type in types {
                    let element_struct_type = self
                        .resolve_type_annotation_with_self_substitution(element_type, self_type)?;
                    element_types.push(element_struct_type);
                }
                Ok(crate::unification::StructuredType::Tuple(element_types))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                let mut param_types = Vec::new();
                for param in params {
                    let param_struct_type = self.resolve_type_annotation_with_self_substitution(
                        &param.type_annotation,
                        self_type,
                    )?;
                    param_types.push(crate::unification::FunctionParam {
                        name: self
                            .compiler_environment
                            .intern_atom_name(&param.name.name.clone()),
                        param_type: param_struct_type,
                    });
                }

                let return_struct_type =
                    self.resolve_type_annotation_with_self_substitution(return_type, self_type)?;

                Ok(crate::unification::StructuredType::Function {
                    params: param_types,
                    return_type: Box::new(return_struct_type),
                })
            }
        }
    }

    /// Infer protocol Self type from function call arguments
    /// This solves the "generic vs self" issue by inferring Self type from arguments
    fn infer_protocol_self_type_from_arguments(
        &mut self,
        protocol_type_id: TypeNameId,
        func_def: &outrun_parser::FunctionDefinition,
        call: &outrun_parser::FunctionCall,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Build argument type mapping
        let mut arg_type_map = std::collections::HashMap::new();
        for arg in &call.arguments {
            if let outrun_parser::Argument::Named {
                name, expression, ..
            } = arg
            {
                let arg_type = self.check_expression_type(expression)?;
                // Store the resolved type for later use by TypedASTBuilder
                let mut context = self.compiler_environment.unification_context();
                context.add_expression_type(expression.span, arg_type.clone());
                self.compiler_environment.set_unification_context(context);

                arg_type_map.insert(name.name.clone(), arg_type);
            }
        }

        // Collect ALL Self types from function parameters and arguments
        let mut self_types = Vec::new();
        for param in &func_def.parameters {
            let param_name = &param.name.name.clone();
            if let Some(arg_type) = arg_type_map.get(param_name) {
                // Check if this parameter is typed as Self
                if self.is_self_type_annotation(&param.type_annotation) {
                    self_types.push(arg_type.clone());
                }
            }
        }

        // Also check the return type for Self (needed for complete unification)
        let return_type_has_self = self.type_annotation_contains_self(&func_def.return_type);

        if !self_types.is_empty() {
            // If we only have one Self type, return it (optimization for common case)
            if self_types.len() == 1 && !return_type_has_self {
                return Ok(self_types[0].clone());
            }

            // Unify all Self types to find the most concrete compatible type
            let mut unified_type = self_types[0].clone();
            for self_type in self_types.iter().skip(1) {
                match crate::unification::unify_structured_types(
                    &unified_type,
                    self_type,
                    &self.compiler_environment.unification_context(),
                    &self.compiler_environment,
                ) {
                    Ok(Some(concrete_type)) => {
                        // Types unify - use the returned concrete type
                        unified_type = concrete_type;
                    }
                    Ok(None) => {
                        return Err(TypeError::internal_with_span(
                            format!(
                                "Incompatible Self types in protocol function call: {} vs {}",
                                unified_type.to_string_representation(),
                                self_type.to_string_representation()
                            ),
                            call.span.to_source_span(),
                        ));
                    }
                    Err(unification_error) => {
                        return Err(TypeError::internal_with_span(
                            format!(
                                "Unification error during Self type inference: {unification_error:?}"
                            ),
                            call.span.to_source_span(),
                        ));
                    }
                }
            }

            return Ok(unified_type);
        }

        // If no Self parameter found, try to infer from generic protocol structure
        let protocol_def = self
            .compiler_environment
            .get_protocol(&protocol_type_id)
            .ok_or_else(|| {
                TypeError::internal_with_span(
                    format!("Protocol not found: {protocol_type_id:?}"),
                    call.span.to_source_span(),
                )
            })?;

        // Extract generic parameter names from protocol definition (e.g., ["T"] for Option<T>)
        let generic_param_names: Vec<String> = protocol_def
            .generic_params
            .as_ref()
            .map(|params| params.params.iter().map(|p| p.name.name.clone()).collect())
            .unwrap_or_default();

        if generic_param_names.is_empty() {
            // No generic parameters, just use simple protocol type
            return Ok(crate::unification::StructuredType::Simple(protocol_type_id));
        }

        // Try to infer generic parameter types from function signature
        let mut inferred_generic_types = std::collections::HashMap::new();

        for param in &func_def.parameters {
            let param_name = &param.name.name.clone();
            if let Some(arg_type) = arg_type_map.get(param_name) {
                // Try to match parameter type annotation with argument type to infer generics
                Self::infer_generic_types_from_parameter(
                    &param.type_annotation,
                    arg_type,
                    &generic_param_names,
                    &mut inferred_generic_types,
                )?;
            }
        }

        // Construct the inferred protocol type
        if inferred_generic_types.is_empty() {
            // No type inference possible, use simple protocol type
            Ok(crate::unification::StructuredType::Simple(protocol_type_id))
        } else {
            // Build generic protocol type with inferred type arguments
            let mut generic_args = Vec::new();
            for param_name in &generic_param_names {
                if let Some(inferred_type) = inferred_generic_types.get(param_name) {
                    generic_args.push(inferred_type.clone());
                } else {
                    // Use Any for unresolved generic parameters
                    let any_type_id = self.compiler_environment.intern_type_name("Any");
                    generic_args.push(crate::unification::StructuredType::Simple(
                        any_type_id.clone(),
                    ));
                }
            }

            Ok(crate::unification::StructuredType::Generic {
                base: protocol_type_id,
                args: generic_args,
            })
        }
    }

    /// Infer the concrete implementing type from protocol function call arguments
    /// This is used for protocol dispatch to find the specific implementation to use
    fn infer_implementing_type_from_arguments(
        &mut self,
        _protocol_type_id: TypeNameId,
        protocol_func_def: &outrun_parser::FunctionDefinition,
        call: &outrun_parser::FunctionCall,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        // Build argument type mapping
        let mut arg_type_map = std::collections::HashMap::new();
        for arg in &call.arguments {
            if let outrun_parser::Argument::Named {
                name, expression, ..
            } = arg
            {
                let arg_type = self.check_expression_type(expression)?;
                arg_type_map.insert(name.name.clone(), arg_type);
            }
        }

        // Collect ALL Self types from function parameters and arguments
        let mut self_types = Vec::new();
        for param in &protocol_func_def.parameters {
            let param_name = &param.name.name.clone();
            if let Some(arg_type) = arg_type_map.get(param_name) {
                // Check if this parameter is typed as Self
                if self.is_self_type_annotation(&param.type_annotation) {
                    self_types.push(arg_type.clone());
                }
            }
        }

        // Also check the return type for Self (needed for complete unification)
        let return_type_has_self =
            self.type_annotation_contains_self(&protocol_func_def.return_type);

        if self_types.is_empty() {
            // If no Self parameter found, this is an error - protocol functions should have Self parameters
            return Err(TypeError::internal_with_span(
                format!(
                    "Protocol function {} has no Self parameter for implementation dispatch",
                    protocol_func_def.name.name
                ),
                call.span.to_source_span(),
            ));
        }

        // If we only have one Self type, return it (optimization for common case)
        if self_types.len() == 1 && !return_type_has_self {
            return Ok(self_types[0].clone());
        }

        // Unify all Self types to find the most concrete compatible type
        let mut unified_type = self_types[0].clone();
        for self_type in self_types.iter().skip(1) {
            match crate::unification::unify_structured_types(
                &unified_type,
                self_type,
                &self.compiler_environment.unification_context(),
                &self.compiler_environment,
            ) {
                Ok(Some(concrete_type)) => {
                    // Types unify - use the returned concrete type
                    unified_type = concrete_type;
                }
                Ok(None) => {
                    // Types don't unify - this is a type mismatch error
                    return Err(TypeError::type_mismatch(
                        unified_type.to_string_representation(),
                        self_type.to_string_representation(),
                        call.span.to_source_span(),
                    ));
                }
                Err(unification_error) => {
                    return Err(TypeError::internal_with_span(
                        format!(
                            "Unification error during Self type inference: {unification_error:?}"
                        ),
                        call.span.to_source_span(),
                    ));
                }
            }
        }

        Ok(unified_type)
    }

    /// Check if a type annotation is Self
    fn is_self_type_annotation(&self, type_annotation: &outrun_parser::TypeAnnotation) -> bool {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, .. } => {
                path.len() == 1 && path[0].name == "Self"
            }
            _ => false,
        }
    }

    /// Check if a type annotation contains Self anywhere (recursively)
    #[allow(clippy::only_used_in_recursion)]
    fn type_annotation_contains_self(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> bool {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                // Check if this is Self
                if path.len() == 1 && path[0].name == "Self" {
                    return true;
                }
                // Check generic arguments recursively
                if let Some(args) = generic_args {
                    for arg in &args.args {
                        if self.type_annotation_contains_self(arg) {
                            return true;
                        }
                    }
                }
                false
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                // Check all tuple elements
                types.iter().any(|t| self.type_annotation_contains_self(t))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                // Check parameter types and return type
                params
                    .iter()
                    .any(|p| self.type_annotation_contains_self(&p.type_annotation))
                    || self.type_annotation_contains_self(return_type)
            }
        }
    }

    /// Resolve type annotation with generic parameter substitutions
    fn resolve_type_annotation_with_generic_substitution(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
        generic_substitutions: &std::collections::HashMap<
            String,
            crate::unification::StructuredType,
        >,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this is a generic parameter to substitute
                if path.len() == 1 {
                    if let Some(substitution) = generic_substitutions.get(&type_name) {
                        return Ok(substitution.clone());
                    }
                }

                // Resolve base type
                let base_type_id = self.compiler_environment.intern_type_name(&type_name);
                // Type is interned through CompilerEnvironment

                // Handle generic arguments
                if let Some(args) = generic_args {
                    let mut resolved_args = Vec::new();
                    for arg in &args.args {
                        let resolved_arg = self.resolve_type_annotation_with_generic_substitution(
                            arg,
                            generic_substitutions,
                        )?;
                        resolved_args.push(resolved_arg);
                    }
                    Ok(crate::unification::StructuredType::Generic {
                        base: base_type_id,
                        args: resolved_args,
                    })
                } else {
                    Ok(crate::unification::StructuredType::Simple(base_type_id))
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                let mut resolved_types = Vec::new();
                for t in types {
                    let resolved_type = self.resolve_type_annotation_with_generic_substitution(
                        t,
                        generic_substitutions,
                    )?;
                    resolved_types.push(resolved_type);
                }
                Ok(crate::unification::StructuredType::Tuple(resolved_types))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                let mut resolved_params = Vec::new();
                for param in params {
                    let resolved_param_type = self
                        .resolve_type_annotation_with_generic_substitution(
                            &param.type_annotation,
                            generic_substitutions,
                        )?;
                    let param_name = self
                        .compiler_environment
                        .intern_atom_name(&param.name.name.clone());
                    resolved_params.push(crate::unification::FunctionParam {
                        name: param_name,
                        param_type: resolved_param_type,
                    });
                }
                let resolved_return_type = self.resolve_type_annotation_with_generic_substitution(
                    return_type,
                    generic_substitutions,
                )?;
                Ok(crate::unification::StructuredType::Function {
                    params: resolved_params,
                    return_type: Box::new(resolved_return_type),
                })
            }
        }
    }

    /// Infer generic type parameters from a function parameter type annotation and argument type
    fn infer_generic_types_from_parameter(
        param_type_annotation: &outrun_parser::TypeAnnotation,
        arg_type: &crate::unification::StructuredType,
        generic_param_names: &[String],
        inferred_types: &mut std::collections::HashMap<String, crate::unification::StructuredType>,
    ) -> Result<(), TypeError> {
        match param_type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, .. } => {
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this is a generic parameter (e.g., "T", "E", "K", "V")
                if generic_param_names.contains(&type_name) {
                    // This parameter uses a generic type, infer it from the argument
                    inferred_types.insert(type_name, arg_type.clone());
                } else if type_name == "Self" {
                    // Self parameter - this is where we need to extract the generic argument
                    // For Option.some?(value: Self), Self should be Option<T>, and we infer T from value type
                    // But we can't infer Self from Self, so we handle this differently
                    // The argument type should match what Self will be resolved to

                    // For now, we can't directly infer from Self, but we could look at other parameters
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                // Handle tuple type inference recursively
                if let crate::unification::StructuredType::Tuple(arg_elements) = arg_type {
                    if types.len() == arg_elements.len() {
                        for (param_elem, arg_elem) in types.iter().zip(arg_elements.iter()) {
                            Self::infer_generic_types_from_parameter(
                                param_elem,
                                arg_elem,
                                generic_param_names,
                                inferred_types,
                            )?;
                        }
                    }
                }
            }
            outrun_parser::TypeAnnotation::Function { .. } => {
                // Function type inference would be more complex, skip for now
            }
        }

        Ok(())
    }

    /// Infer generic type parameters from static protocol function arguments and return type hint
    fn infer_generic_parameters_from_arguments(
        &mut self,
        func_def: &outrun_parser::FunctionDefinition,
        call: &outrun_parser::FunctionCall,
        type_hint: Option<&crate::unification::StructuredType>,
    ) -> Result<std::collections::HashMap<String, crate::unification::StructuredType>, TypeError>
    {
        let mut generic_substitutions = std::collections::HashMap::new();

        // Build argument type mapping
        let mut arg_type_map = std::collections::HashMap::new();
        for arg in &call.arguments {
            if let outrun_parser::Argument::Named {
                name, expression, ..
            } = arg
            {
                let arg_type = self.check_expression_type(expression)?;
                arg_type_map.insert(name.name.clone(), arg_type);
            }
        }

        // For each parameter, check if its type annotation contains generic parameters
        // and infer them from the corresponding argument type
        for param in &func_def.parameters {
            let param_name = &param.name.name;
            if let Some(arg_type) = arg_type_map.get(param_name) {
                Self::infer_generic_parameters_from_type_annotation(
                    &param.type_annotation,
                    arg_type,
                    &mut generic_substitutions,
                )?;
            }
        }

        // If we have a type hint for the expected return type, use it to infer generic parameters
        // This is crucial for functions like Option.none() that have no parameters
        if let Some(hint) = type_hint {
            Self::infer_generic_parameters_from_type_annotation(
                &func_def.return_type,
                hint,
                &mut generic_substitutions,
            )?;
        }

        Ok(generic_substitutions)
    }

    /// Recursively infer generic parameters from a type annotation and actual type
    fn infer_generic_parameters_from_type_annotation(
        type_annotation: &outrun_parser::TypeAnnotation,
        actual_type: &crate::unification::StructuredType,
        generic_substitutions: &mut std::collections::HashMap<
            String,
            crate::unification::StructuredType,
        >,
    ) -> Result<(), TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                let _type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this is a single generic parameter like "T"
                if path.len() == 1 {
                    let param_name = &path[0].name;
                    // Generic parameters are typically single uppercase letters or start with uppercase
                    if param_name.chars().all(|c| c.is_uppercase() || c == '_') {
                        // This looks like a generic parameter - infer it from the actual type
                        generic_substitutions.insert(param_name.clone(), actual_type.clone());
                        return Ok(());
                    }
                }

                // If this type annotation has generic arguments, recursively infer from them
                if let (
                    Some(generic_args),
                    crate::unification::StructuredType::Generic {
                        args: actual_args, ..
                    },
                ) = (generic_args, actual_type)
                {
                    if generic_args.args.len() == actual_args.len() {
                        for (generic_arg, actual_arg) in
                            generic_args.args.iter().zip(actual_args.iter())
                        {
                            Self::infer_generic_parameters_from_type_annotation(
                                generic_arg,
                                actual_arg,
                                generic_substitutions,
                            )?;
                        }
                    }
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                if let crate::unification::StructuredType::Tuple(actual_elements) = actual_type {
                    if types.len() == actual_elements.len() {
                        for (type_annotation, actual_element) in
                            types.iter().zip(actual_elements.iter())
                        {
                            Self::infer_generic_parameters_from_type_annotation(
                                type_annotation,
                                actual_element,
                                generic_substitutions,
                            )?;
                        }
                    }
                }
            }
            outrun_parser::TypeAnnotation::Function { .. } => {
                // Function type inference would be more complex, skip for now
            }
        }

        Ok(())
    }

    /// Resolve type annotation to StructuredType
    fn resolve_type_annotation(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path,
                generic_args,
                span: _,
            } => {
                // Convert type path to string
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Handle type parameters (Self, T, E, K, V, etc.)
                if let Some(type_param_type) = self.lookup_type_parameter(&type_name) {
                    return Ok(type_param_type.clone());
                }

                // Check if type exists
                {
                    // Type interning always succeeds with CompilerEnvironment
                    let type_id = self.compiler_environment.intern_type_name(&type_name);

                    // Handle generic arguments if present
                    if let Some(ref args) = generic_args {
                        let mut arg_types = Vec::new();
                        for arg in &args.args {
                            let arg_type = self.resolve_type_annotation(arg)?;
                            arg_types.push(arg_type);
                        }

                        Ok(crate::unification::StructuredType::Generic {
                            base: type_id,
                            args: arg_types,
                        })
                    } else {
                        Ok(crate::unification::StructuredType::Simple(type_id))
                    }
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, span: _ } => {
                // Resolve all tuple element types
                let mut element_types = Vec::new();
                for element_type in types {
                    let element_struct_type = self.resolve_type_annotation(element_type)?;
                    element_types.push(element_struct_type);
                }

                Ok(crate::unification::StructuredType::Tuple(element_types))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                span: _,
            } => {
                // Resolve parameter types
                let mut param_types = Vec::new();
                for param in params {
                    let param_struct_type = self.resolve_type_annotation(&param.type_annotation)?;
                    param_types.push(crate::unification::FunctionParam {
                        name: self
                            .compiler_environment
                            .intern_atom_name(&param.name.name.clone()),
                        param_type: param_struct_type,
                    });
                }

                // Resolve return type
                let return_struct_type = self.resolve_type_annotation(return_type)?;

                Ok(crate::unification::StructuredType::Function {
                    params: param_types,
                    return_type: Box::new(return_struct_type),
                })
            }
        }
    }

    /// Resolve type spec to StructuredType
    fn resolve_type_spec(
        &mut self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        let type_name = type_spec
            .path
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        {
            // Type interning always succeeds with CompilerEnvironment
            let type_id = self.compiler_environment.intern_type_name(&type_name);

            // Handle generic arguments if present
            if let Some(ref generic_args) = type_spec.generic_args {
                let mut arg_types = Vec::new();
                for arg in &generic_args.args {
                    // Convert TypeAnnotation to StructuredType
                    let arg_type = self.resolve_type_annotation(arg)?;
                    arg_types.push(arg_type);
                }
                Ok(crate::unification::StructuredType::Generic {
                    base: type_id,
                    args: arg_types,
                })
            } else {
                Ok(crate::unification::StructuredType::Simple(type_id))
            }
        }
    }

    /// Try to resolve a protocol function call as an impl function when no static protocol function exists
    /// This implements the protocol dispatch lookup: static function -> impl function -> default implementation
    fn try_protocol_impl_function_call(
        &mut self,
        protocol_type_id: TypeNameId,
        function_name_atom: AtomId,
        call: &outrun_parser::FunctionCall,
    ) -> Result<crate::unification::StructuredType, TypeError> {
        use crate::unification::StructuredType;

        // For impl function lookup, we need to infer the implementing type from the arguments
        // For functions like `Comparison.greater?(left: T, right: T)`, the type T is the impl type

        // First, try to infer the implementing type from the first argument
        let impl_type = if let Some(first_arg) = call.arguments.first() {
            // Extract expression from argument and type check it
            let expression = match first_arg {
                outrun_parser::Argument::Named { expression, .. } => expression,
                outrun_parser::Argument::Spread { expression, .. } => expression,
            };
            {
                let arg_type = self.check_expression_type(expression)?;
                // Store the resolved type for later use by TypedASTBuilder
                let mut context = self.compiler_environment.unification_context();
                context.add_expression_type(expression.span, arg_type.clone());
                self.compiler_environment.set_unification_context(context);
                arg_type
            }
        } else {
            return Err(TypeError::undefined_function(
                format!("{}:{}", protocol_type_id, function_name_atom.clone()),
                call.span.to_source_span(),
            ));
        };

        // Create StructuredType for the protocol
        let protocol_type = StructuredType::Simple(protocol_type_id.clone());

        // Look up the impl function in the registry
        if let Some(func_entry) = self.compiler_environment.lookup_impl_function(
            &protocol_type,
            &impl_type,
            function_name_atom.clone(),
        ) {
            // Store protocol dispatch strategy
            let protocol_name = self
                .compiler_environment
                .resolve_type(protocol_type_id.clone())
                .unwrap_or_default();
            let function_name = self
                .compiler_environment
                .resolve_atom(function_name_atom.clone())
                .unwrap_or_default();
            let mut context = self.compiler_environment.unification_context();
            context.add_dispatch_strategy(
                call.span,
                crate::checker::DispatchMethod::Protocol {
                    protocol_name,
                    function_name,
                    impl_type: Box::new(impl_type.clone()),
                },
            );
            self.compiler_environment.set_unification_context(context);

            // Found an impl function - validate the call and return its type
            self.validate_function_call_arguments(call, func_entry.definition())?;

            // For impl functions, we may need to substitute Self type
            // For now, just return the declared return type
            self.resolve_type_annotation(&func_entry.definition().return_type)
        } else {
            // No impl function found either - this is a true undefined function error
            let protocol_name = protocol_type_id.to_string();
            let function_name = function_name_atom.to_string();

            Err(TypeError::undefined_function(
                format!("{protocol_name}.{function_name}"),
                call.span.to_source_span(),
            ))
        }
    }
}
