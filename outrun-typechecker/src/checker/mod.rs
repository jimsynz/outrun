//! Type checker core components
//!
//! This module implements the main type checking logic, including:
//! - Type context and scope management  
//! - Expression type checking with trait dispatch
//! - Function signature validation
//! - Pattern type checking for destructuring

pub mod context;
pub mod expressions;
pub mod functions;
pub mod patterns;
pub mod trait_definitions;

// Re-export core types
pub use context::{Scope, TypeContext, Variable};
pub use expressions::ExpressionChecker;
pub use functions::FunctionChecker;
pub use patterns::PatternChecker;

use crate::dispatch::DispatchTable;
use crate::error::TypeError;
use crate::types::{AtomId, TypeId};
use outrun_parser::{Program, Span};
// use std::collections::HashMap; // TODO: Use when needed

/// Main type checker that orchestrates the entire type checking process
#[derive(Debug)]
pub struct TypeChecker {
    context: TypeContext,
}

/// Typed AST program with type information and dispatch tables
#[derive(Debug, Clone)]
pub struct TypedProgram {
    pub items: Vec<TypedItem>,
    pub dispatch_table: DispatchTable,
    pub type_context: TypeContext,
}

/// Typed AST item with type information
#[derive(Debug, Clone)]
pub struct TypedItem {
    pub kind: TypedItemKind,
    pub span: Span,
}

/// Kinds of typed items
#[derive(Debug, Clone)]
pub enum TypedItemKind {
    Expression(TypedExpression),
    FunctionDefinition(TypedFunctionDefinition),
    ConstDefinition(TypedConstDefinition),
    LetBinding(Box<TypedLetBinding>),
    StructDefinition(TypedStructDefinition),
    TraitDefinition(TypedTraitDefinition),
    ImplBlock(TypedImplBlock),
}

/// Expression with type information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression {
    pub kind: TypedExpressionKind,
    pub type_id: TypeId,
    pub span: Span,
}

/// Kinds of typed expressions (stub for now)
#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpressionKind {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Atom(String),
    Identifier(String),
    BinaryOp {
        left: Box<TypedExpression>,
        operator: outrun_parser::BinaryOperator,
        right: Box<TypedExpression>,
    },
    FunctionCall {
        name: String,
        args: Vec<(String, TypedExpression)>, // Named arguments
    },
    List {
        elements: Vec<TypedExpression>,
        element_type: TypeId, // Homogeneous type for all elements
    },
    Tuple {
        elements: Vec<TypedExpression>,
        element_types: Vec<TypeId>, // Each element can have different type
    },
    Map {
        entries: Vec<(TypedExpression, TypedExpression)>, // (key, value) pairs
        key_type: TypeId,
        value_type: TypeId,
    },
    Struct {
        type_name: String,
        fields: Vec<(AtomId, TypedExpression)>, // (field_name, typed_value)
        struct_type: TypeId,
    },
    IfExpression {
        condition: Box<TypedExpression>,
        then_block: TypedBlock,
        else_block: Option<TypedBlock>,
    },
    CaseExpression {
        expression: Box<TypedExpression>,
        when_clauses: Vec<TypedCaseWhenClause>,
    },
    TraitCaseExpression {
        expression: Box<TypedExpression>,
        trait_name: String,
        type_clauses: Vec<TypedTraitCaseClause>,
    },
    FunctionCapture {
        path: String,       // Function path (e.g., "Module.function" or "function")
        arity: Option<i32>, // Optional arity specification (/2)
    },
    Block(TypedBlock), // Block expression (function bodies, etc.)
    UnaryOp {
        operator: outrun_parser::UnaryOperator,
        operand: Box<TypedExpression>,
    },
    FieldAccess {
        object: Box<TypedExpression>,
        field: String,
    },
    QualifiedIdentifier {
        module: String,
        name: String,
    },
    Parenthesized(Box<TypedExpression>),
    TypeIdentifier {
        type_name: String,
        referenced_type: TypeId, // The actual type being referenced
    },
    // TODO: Add remaining expression kinds (Sigil, MacroInjection)
}

/// Typed block with statements
#[derive(Debug, Clone, PartialEq)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub result_type: TypeId, // Type of the block's result
    pub span: Span,
}

/// Typed case when clause
#[derive(Debug, Clone, PartialEq)]
pub struct TypedCaseWhenClause {
    pub guard: TypedExpression,
    pub result: TypedCaseResult,
    pub span: Span,
}

/// Typed case result (block or expression)
#[derive(Debug, Clone, PartialEq)]
pub enum TypedCaseResult {
    Block(TypedBlock),
    Expression(Box<TypedExpression>),
}

impl TypedCaseResult {
    /// Get the result type of this case result
    pub fn result_type(&self) -> TypeId {
        match self {
            TypedCaseResult::Block(block) => block.result_type,
            TypedCaseResult::Expression(expr) => expr.type_id,
        }
    }
}

/// Typed trait case clause
#[derive(Debug, Clone, PartialEq)]
pub struct TypedTraitCaseClause {
    pub type_id: TypeId,
    pub result_type: TypeId,
    pub span: Span,
}

/// Typed statement
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStatement {
    pub kind: TypedStatementKind,
    pub span: Span,
}

/// Kinds of typed statements
#[derive(Debug, Clone, PartialEq)]
pub enum TypedStatementKind {
    Expression(Box<TypedExpression>),
    LetBinding(Box<TypedLetBinding>),
}

/// Typed function definition
#[derive(Debug, Clone)]
pub struct TypedFunctionDefinition {
    pub name: String,
    pub params: Vec<(String, TypeId)>,
    pub return_type: TypeId,
    pub body: TypedExpression,
    pub span: Span,
}

/// Typed constant definition
#[derive(Debug, Clone)]
pub struct TypedConstDefinition {
    pub name: String,
    pub type_id: TypeId,
    pub value: TypedExpression,
    pub span: Span,
}

/// Typed let binding
#[derive(Debug, Clone, PartialEq)]
pub struct TypedLetBinding {
    pub pattern: outrun_parser::Pattern, // TODO: Create typed pattern system
    pub type_id: TypeId,
    pub expression: TypedExpression,
    pub span: Span,
}

/// Typed struct definition
#[derive(Debug, Clone)]
pub struct TypedStructDefinition {
    pub name: String,
    pub fields: Vec<(String, TypeId)>,
    pub methods: Vec<TypedFunctionDefinition>,
    pub span: Span,
}

/// Typed trait definition
#[derive(Debug, Clone)]
pub struct TypedTraitDefinition {
    pub name: String,
    pub functions: Vec<TypedFunctionSignature>,
    pub span: Span,
}

/// Typed function signature
#[derive(Debug, Clone)]
pub struct TypedFunctionSignature {
    pub name: String,
    pub params: Vec<(String, TypeId)>,
    pub return_type: TypeId,
    pub span: Span,
}

/// Typed implementation block
#[derive(Debug, Clone)]
pub struct TypedImplBlock {
    pub trait_name: String,
    pub type_name: String,
    pub methods: Vec<TypedFunctionDefinition>,
    pub span: Span,
}

impl TypeChecker {
    /// Create a new type checker with default context
    pub fn new() -> Self {
        Self {
            context: TypeContext::new(),
        }
    }

    /// Get a reference to the type context (for testing)
    #[cfg(test)]
    pub fn context(&self) -> &TypeContext {
        &self.context
    }

    /// Type check a complete program
    pub fn check_program(&mut self, program: &Program) -> Result<TypedProgram, Vec<TypeError>> {
        // Phase 1: Register all type definitions and traits
        self.register_types(program)?;

        // Phase 2: Validate trait implementations
        self.validate_implementations()?;

        // Phase 3: Type check all items (this registers implementations)
        let typed_items = self.check_items(program)?;

        // Phase 4: Build dispatch tables (after implementations are registered)
        self.build_dispatch_tables()?;

        // Phase 5: Build final typed program
        Ok(TypedProgram {
            items: typed_items,
            dispatch_table: self.context.dispatch_table.clone(),
            type_context: self.context.clone(),
        })
    }

    /// Register all type definitions and traits from the program
    fn register_types(&mut self, program: &Program) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();

        // Phase 1a: Register all struct types (just the types, not full validation)
        for item in &program.items {
            if let outrun_parser::ItemKind::StructDefinition(struct_def) = &item.kind {
                match self.register_struct_type(struct_def) {
                    Ok(()) => {}
                    Err(err) => errors.push(err),
                }
            }
        }

        // Phase 1b: Register all trait definitions
        for item in &program.items {
            if let outrun_parser::ItemKind::TraitDefinition(trait_def) = &item.kind {
                match Self::check_trait_definition(&mut self.context, trait_def) {
                    Ok(()) => {}
                    Err(err) => errors.push(err),
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Register a struct type (just the type, not full validation)
    fn register_struct_type(
        &mut self,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<(), TypeError> {
        // Register the struct type in our registry
        let struct_type_id = self.context.interner.intern_type(&struct_def.name.name);

        // Process struct fields
        let mut concrete_fields = Vec::new();

        for field in &struct_def.fields {
            // Resolve field type
            let field_type_id =
                Self::resolve_type_annotation(&mut self.context, &field.type_annotation, &[])?;

            // Create concrete field for registry
            let field_atom = self.context.interner.intern_atom(&field.name.name);
            concrete_fields.push(crate::types::concrete::StructField {
                name: field_atom,
                type_id: field_type_id,
                span: field.span,
            });
        }

        // Create concrete struct type and register it
        let concrete_struct = crate::types::concrete::ConcreteType::Struct {
            name: struct_type_id,
            fields: concrete_fields,
        };

        self.context
            .register_concrete_type(struct_type_id, concrete_struct);

        Ok(())
    }

    /// Validate that all trait implementations are complete and correct
    fn validate_implementations(&mut self) -> Result<(), Vec<TypeError>> {
        // TODO: Implement implementation validation
        // - Check all required trait functions are implemented
        // - Validate function signatures match trait requirements
        // - Check trait constraints are satisfied
        Ok(())
    }

    /// Build dispatch tables for efficient runtime trait method calls
    fn build_dispatch_tables(&mut self) -> Result<(), Vec<TypeError>> {
        use crate::dispatch::lookup::DispatchTableBuilder;

        // Build complete dispatch table from trait registry
        let builder =
            DispatchTableBuilder::new(&self.context.trait_registry, &mut self.context.interner);
        match builder.build() {
            Ok(dispatch_table) => {
                self.context.dispatch_table = dispatch_table;
                Ok(())
            }
            Err(err) => Err(vec![err]),
        }
    }

    /// Type check all items in the program
    fn check_items(&mut self, program: &Program) -> Result<Vec<TypedItem>, Vec<TypeError>> {
        let mut typed_items = Vec::new();
        let mut errors = Vec::new();

        // Process each item in the program
        for item in &program.items {
            match &item.kind {
                outrun_parser::ItemKind::FunctionDefinition(func) => {
                    match self.check_function_definition(func) {
                        Ok(typed_func) => typed_items.push(typed_func),
                        Err(err) => errors.push(err),
                    }
                }
                outrun_parser::ItemKind::ConstDefinition(const_def) => {
                    match self.check_const_definition(const_def) {
                        Ok(typed_const) => typed_items.push(typed_const),
                        Err(err) => errors.push(err),
                    }
                }
                outrun_parser::ItemKind::StructDefinition(struct_def) => {
                    match self.check_struct_definition(struct_def) {
                        Ok(typed_struct) => typed_items.push(typed_struct),
                        Err(err) => errors.push(err),
                    }
                }
                outrun_parser::ItemKind::TraitDefinition(trait_def) => {
                    if let Err(err) = Self::check_trait_definition(&mut self.context, trait_def) {
                        errors.push(err);
                    }
                }
                outrun_parser::ItemKind::ImplBlock(impl_block) => {
                    match self.check_impl_block(impl_block) {
                        Ok(typed_impl) => typed_items.push(typed_impl),
                        Err(err) => errors.push(err),
                    }
                }
                outrun_parser::ItemKind::ImportDefinition(_) => {
                    // TODO: Implement import checking
                    errors.push(TypeError::UnimplementedFeature {
                        feature: "Import type checking".to_string(),
                        span: crate::error::span_to_source_span(item.span),
                    });
                }
                outrun_parser::ItemKind::AliasDefinition(_) => {
                    // TODO: Implement alias checking
                    errors.push(TypeError::UnimplementedFeature {
                        feature: "Alias type checking".to_string(),
                        span: crate::error::span_to_source_span(item.span),
                    });
                }
                outrun_parser::ItemKind::MacroDefinition(_) => {
                    // TODO: Implement macro checking
                    errors.push(TypeError::UnimplementedFeature {
                        feature: "Macro type checking".to_string(),
                        span: crate::error::span_to_source_span(item.span),
                    });
                }
                outrun_parser::ItemKind::LetBinding(let_binding) => {
                    match self.check_let_binding(let_binding) {
                        Ok(typed_let) => typed_items.push(typed_let),
                        Err(err) => errors.push(err),
                    }
                }
                outrun_parser::ItemKind::Expression(expr) => {
                    // Top-level expressions can be type checked directly
                    match ExpressionChecker::check_expression(&mut self.context, expr) {
                        Ok(typed_expr) => {
                            typed_items.push(TypedItem {
                                kind: TypedItemKind::Expression(typed_expr),
                                span: item.span,
                            });
                        }
                        Err(err) => errors.push(err),
                    }
                }

                // Handle all the literal and identifier variants that can appear at top level
                outrun_parser::ItemKind::Keyword(_)
                | outrun_parser::ItemKind::BooleanLiteral(_)
                | outrun_parser::ItemKind::IntegerLiteral(_)
                | outrun_parser::ItemKind::FloatLiteral(_)
                | outrun_parser::ItemKind::StringLiteral(_)
                | outrun_parser::ItemKind::AtomLiteral(_)
                | outrun_parser::ItemKind::SigilLiteral(_)
                | outrun_parser::ItemKind::ListLiteral(_)
                | outrun_parser::ItemKind::MapLiteral(_)
                | outrun_parser::ItemKind::TupleLiteral(_)
                | outrun_parser::ItemKind::Identifier(_)
                | outrun_parser::ItemKind::TypeIdentifier(_)
                | outrun_parser::ItemKind::Comment(_) => {
                    // These are typically not standalone items but part of expressions
                    // Skip them for now, as they should be processed as part of other constructs
                }
            }
        }

        if errors.is_empty() {
            Ok(typed_items)
        } else {
            Err(errors)
        }
    }

    /// Type check a function definition
    fn check_function_definition(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
    ) -> Result<TypedItem, TypeError> {
        // Use the comprehensive function validation from FunctionChecker
        FunctionChecker::check_function_definition(&mut self.context, func)?;
        self.create_typed_function_item(func, None)
    }

    /// Type check a function definition with implementation context (for impl blocks)
    fn check_function_definition_with_impl_context(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
        implementing_type: TypeId,
    ) -> Result<TypedItem, TypeError> {
        // Use the comprehensive function validation from FunctionChecker with impl context
        FunctionChecker::check_function_definition_with_context(
            &mut self.context,
            func,
            Some(implementing_type),
        )?;
        self.create_typed_function_item(func, Some(implementing_type))
    }

    /// Create a typed function item (shared logic for both standalone and impl functions)
    fn create_typed_function_item(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
        implementing_type: Option<TypeId>,
    ) -> Result<TypedItem, TypeError> {
        // Resolve parameter types for typed AST
        let mut typed_params = Vec::new();
        for param in &func.parameters {
            let param_type = if let Some(implementing_type_id) = implementing_type {
                // Use Self-aware type resolution for impl block functions
                Self::resolve_type_annotation_with_self(
                    &mut self.context,
                    &param.type_annotation,
                    &[], // No generic parameters in functions
                    implementing_type_id,
                )?
            } else {
                // Use regular type resolution for standalone functions
                Self::resolve_type_annotation(
                    &mut self.context,
                    &param.type_annotation,
                    &[], // No generic parameters in functions
                )?
            };
            typed_params.push((param.name.name.clone(), param_type));
        }

        // Resolve return type
        let return_type = if let Some(ret_type) = &func.return_type {
            if let Some(implementing_type_id) = implementing_type {
                // Use Self-aware type resolution for impl block functions
                Self::resolve_type_annotation_with_self(
                    &mut self.context,
                    ret_type,
                    &[], // No generic parameters in functions
                    implementing_type_id,
                )?
            } else {
                // Use regular type resolution for standalone functions
                Self::resolve_type_annotation(
                    &mut self.context,
                    ret_type,
                    &[], // No generic parameters in functions
                )?
            }
        } else {
            // This should have been caught by FunctionChecker validation
            return Err(TypeError::UnimplementedFeature {
                feature: "Functions must have explicit return type annotations".to_string(),
                span: crate::error::span_to_source_span(func.span),
            });
        };

        // The function has already been validated by FunctionChecker,
        // but we need to type-check the body with parameters in scope.
        // Recreate the parameter scope for typed AST generation
        self.context.push_scope(true); // Function scope

        // Register parameters in scope
        for (param_name, param_type) in &typed_params {
            let variable = Variable {
                name: param_name.clone(),
                type_id: *param_type,
                is_mutable: false,
                span: func.span, // Use function span for now
            };
            self.context.register_variable(variable)?;
        }

        // Type check the function body to get typed AST
        let typed_block =
            FunctionChecker::check_function_body_typed(&mut self.context, &func.body, return_type)?;
        let typed_body = TypedExpression {
            kind: TypedExpressionKind::Block(typed_block.clone()),
            type_id: typed_block.result_type,
            span: func.body.span,
        };

        // Clean up function scope
        self.context.pop_scope();

        Ok(TypedItem {
            kind: TypedItemKind::FunctionDefinition(TypedFunctionDefinition {
                name: func.name.name.clone(),
                params: typed_params,
                return_type,
                body: typed_body,
                span: func.span,
            }),
            span: func.span,
        })
    }

    /// Type check a const definition
    fn check_const_definition(
        &mut self,
        const_def: &outrun_parser::ConstDefinition,
    ) -> Result<TypedItem, TypeError> {
        // Type check the constant value
        let typed_value =
            ExpressionChecker::check_expression(&mut self.context, &const_def.expression)?;

        // TODO: Validate explicit type annotation if present

        Ok(TypedItem {
            kind: TypedItemKind::ConstDefinition(TypedConstDefinition {
                name: const_def.name.name.clone(),
                type_id: typed_value.type_id,
                value: typed_value,
                span: const_def.span,
            }),
            span: const_def.span,
        })
    }

    /// Type check a let binding
    pub fn check_let_binding(
        &mut self,
        let_binding: &outrun_parser::LetBinding,
    ) -> Result<TypedItem, TypeError> {
        // Type check the right-hand side expression first
        let typed_expression =
            ExpressionChecker::check_expression(&mut self.context, &let_binding.expression)?;

        // Determine the type for the binding
        let binding_type = if let Some(type_annotation) = &let_binding.type_annotation {
            // User provided explicit type - validate it exists and matches
            let annotated_type_id = match type_annotation {
                outrun_parser::TypeAnnotation::Simple { path, .. } => {
                    // For now, just handle simple types - join the path parts
                    let type_name = path
                        .iter()
                        .map(|part| part.name.as_str())
                        .collect::<Vec<_>>()
                        .join(".");
                    self.context.interner.intern_type(&type_name)
                }
                _ => {
                    // TODO: Handle complex type annotations (Tuple, Function, etc.)
                    return Err(TypeError::UnimplementedFeature {
                        feature: "Complex type annotations in let bindings".to_string(),
                        span: crate::error::span_to_source_span(let_binding.span),
                    });
                }
            };

            // Check that the expression type matches the annotation
            if typed_expression.type_id != annotated_type_id {
                return Err(TypeError::type_mismatch(
                    self.context
                        .get_type_name(annotated_type_id)
                        .unwrap_or("Unknown")
                        .to_string(),
                    self.context
                        .get_type_name(typed_expression.type_id)
                        .unwrap_or("Unknown")
                        .to_string(),
                    crate::error::span_to_source_span(let_binding.expression.span),
                ));
            }
            annotated_type_id
        } else {
            // Type inference - use the expression's type
            typed_expression.type_id
        };

        // Process the pattern and register variables in scope
        self.check_pattern_and_register_variables(&let_binding.pattern, binding_type)?;

        // Create a typed let binding (we'll represent it as an expression for now)
        Ok(TypedItem {
            kind: TypedItemKind::LetBinding(Box::new(TypedLetBinding {
                pattern: let_binding.pattern.clone(), // TODO: Create typed pattern
                type_id: binding_type,
                expression: typed_expression,
                span: let_binding.span,
            })),
            span: let_binding.span,
        })
    }

    /// Process a pattern and register variables in the current scope
    fn check_pattern_and_register_variables(
        &mut self,
        pattern: &outrun_parser::Pattern,
        expected_type: TypeId,
    ) -> Result<(), TypeError> {
        match pattern {
            outrun_parser::Pattern::Identifier(identifier) => {
                // Simple variable binding
                let variable = crate::checker::context::Variable {
                    name: identifier.name.clone(),
                    type_id: expected_type,
                    is_mutable: false, // Let bindings are immutable in Outrun
                    span: identifier.span,
                };
                self.context.register_variable(variable)?;
            }
            outrun_parser::Pattern::Literal(_) => {
                // Literal patterns don't bind variables, just validate the pattern matches
                // TODO: Add literal pattern validation
            }
            outrun_parser::Pattern::Tuple(tuple_pattern) => {
                // Destructuring tuple pattern - need to validate tuple type
                let tuple_type = self.context.get_concrete_type(expected_type);
                match tuple_type {
                    Some(crate::types::ConcreteType::Tuple { element_types }) => {
                        if tuple_pattern.elements.len() != element_types.len() {
                            return Err(TypeError::TypeMismatch {
                                span: crate::error::span_to_source_span(tuple_pattern.span),
                                expected: format!("tuple with {} elements", element_types.len()),
                                found: format!(
                                    "pattern with {} elements",
                                    tuple_pattern.elements.len()
                                ),
                            });
                        }

                        // Clone the element types to avoid borrowing issues
                        let element_types_clone = element_types.clone();

                        // Recursively process each pattern element
                        for (i, pattern_element) in tuple_pattern.elements.iter().enumerate() {
                            self.check_pattern_and_register_variables(
                                pattern_element,
                                element_types_clone[i],
                            )?;
                        }
                    }
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            span: crate::error::span_to_source_span(tuple_pattern.span),
                            expected: "tuple type".to_string(),
                            found: self
                                .context
                                .get_type_name(expected_type)
                                .unwrap_or("Unknown")
                                .to_string(),
                        });
                    }
                }
            }
            outrun_parser::Pattern::List(list_pattern) => {
                // Destructuring list pattern - need to validate list type
                let list_type = self.context.get_concrete_type(expected_type);
                match list_type {
                    Some(crate::types::ConcreteType::List { element_type }) => {
                        // Clone element type to avoid borrowing issues
                        let element_type_clone = *element_type;

                        // Process each pattern element with the list's element type
                        for pattern_element in &list_pattern.elements {
                            self.check_pattern_and_register_variables(
                                pattern_element,
                                element_type_clone,
                            )?;
                        }
                    }
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            span: crate::error::span_to_source_span(list_pattern.span),
                            expected: "list type".to_string(),
                            found: self
                                .context
                                .get_type_name(expected_type)
                                .unwrap_or("Unknown")
                                .to_string(),
                        });
                    }
                }
            }
            outrun_parser::Pattern::Struct(struct_pattern) => {
                // Destructuring struct pattern - need to validate struct type
                let struct_type = self.context.get_concrete_type(expected_type);
                match struct_type {
                    Some(crate::types::ConcreteType::Struct { fields, .. }) => {
                        // Clone the fields to avoid borrowing issues
                        let fields_clone = fields.clone();

                        // Process each field pattern
                        for field_pattern in &struct_pattern.fields {
                            let field_atom =
                                self.context.interner.intern_atom(&field_pattern.name.name);

                            // Find the field type in the struct definition
                            let field_type = fields_clone
                                .iter()
                                .find(|field| field.name == field_atom)
                                .map(|field| field.type_id);

                            match field_type {
                                Some(field_type_id) => {
                                    // Handle the optional pattern - if None, it's shorthand syntax (field name binding)
                                    if let Some(pattern) = &field_pattern.pattern {
                                        self.check_pattern_and_register_variables(
                                            pattern,
                                            field_type_id,
                                        )?;
                                    } else {
                                        // Shorthand syntax - register the field name as a variable
                                        let variable = crate::checker::context::Variable {
                                            name: field_pattern.name.name.clone(),
                                            type_id: field_type_id,
                                            is_mutable: false,
                                            span: field_pattern.name.span,
                                        };
                                        self.context.register_variable(variable)?;
                                    }
                                }
                                None => {
                                    return Err(TypeError::UnexpectedParameter {
                                        function_name: self
                                            .context
                                            .get_type_name(expected_type)
                                            .unwrap_or("Unknown")
                                            .to_string(),
                                        parameter_name: field_pattern.name.name.clone(),
                                        span: crate::error::span_to_source_span(
                                            field_pattern.name.span,
                                        ),
                                    });
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            span: crate::error::span_to_source_span(struct_pattern.span),
                            expected: "struct type".to_string(),
                            found: self
                                .context
                                .get_type_name(expected_type)
                                .unwrap_or("Unknown")
                                .to_string(),
                        });
                    }
                }
            } // Note: Rest patterns don't exist at the top level in the Pattern enum
              // They are handled within list/tuple patterns as part of the PatternElement enum
        }
        Ok(())
    }

    /// Type check a trait definition and register it
    fn check_trait_definition(
        context: &mut TypeContext,
        trait_def: &outrun_parser::TraitDefinition,
    ) -> Result<(), TypeError> {
        // Intern the trait name and create a TraitId
        let trait_id = context.interner.intern_trait(&trait_def.name.name);

        // Process generic parameters (register them as type parameters)
        let mut generic_params = Vec::new();
        let mut generic_param_names = Vec::new(); // For passing to function processors

        // Always add Self as a special generic parameter
        let self_type_id = context.interner.intern_type("Self");
        generic_param_names.push(("Self".to_string(), self_type_id));

        if let Some(ref generics) = trait_def.generic_params {
            for param in &generics.params {
                let param_type_id = context.interner.intern_type(&param.name.name);
                generic_params.push(param_type_id);
                generic_param_names.push((param.name.name.clone(), param_type_id));
            }
        }

        // Process trait functions (signatures and definitions)
        let mut trait_functions = Vec::new();
        for trait_func in &trait_def.functions {
            match trait_func {
                outrun_parser::TraitFunction::Signature(sig) => {
                    let func =
                        Self::process_trait_function_signature(context, sig, &generic_param_names)?;
                    trait_functions.push(func);
                }
                outrun_parser::TraitFunction::Definition(def) => {
                    let func = Self::process_trait_function_definition(
                        context,
                        def,
                        &generic_param_names,
                    )?;
                    trait_functions.push(func);
                }
                outrun_parser::TraitFunction::StaticDefinition(static_def) => {
                    let func = Self::process_static_function_definition(
                        context,
                        static_def,
                        &generic_param_names,
                    )?;
                    trait_functions.push(func);
                }
            }
        }

        // Process trait constraints
        let mut constraints = Vec::new();
        if let Some(ref constraint_expr) = trait_def.constraints {
            let processed_constraints =
                Self::process_constraint_expression(context, constraint_expr, &generic_params)?;
            constraints.extend(processed_constraints);
        }

        // Create the trait definition
        let mut trait_definition = crate::types::traits::TraitDefinition::new(
            trait_id,
            trait_def.name.name.clone(),
            trait_functions,
            trait_def.span,
        );

        // Add generic parameters and constraints
        for param in generic_params {
            trait_definition.add_generic_param(param);
        }
        for constraint in constraints {
            trait_definition.add_constraint(constraint);
        }

        // Register the trait in the registry
        context.trait_registry.register_trait(trait_definition);

        Ok(())
    }

    /// Process a trait function signature
    fn process_trait_function_signature(
        context: &mut TypeContext,
        sig: &outrun_parser::FunctionSignature,
        generic_params: &[(String, TypeId)],
    ) -> Result<crate::types::traits::TraitFunction, TypeError> {
        let func_name = context.interner.intern_atom(&sig.name.name);

        // Process parameters
        let mut params = Vec::new();
        for param in &sig.parameters {
            let param_name = context.interner.intern_atom(&param.name.name);
            let param_type =
                Self::resolve_type_annotation(context, &param.type_annotation, generic_params)?;
            params.push((param_name, param_type));
        }

        // Process return type (default to Unit if not specified)
        let return_type = if let Some(ref ret_type) = sig.return_type {
            Self::resolve_type_annotation(context, ret_type, generic_params)?
        } else {
            context.interner.intern_type("Outrun.Core.Unit")
        };

        // Check if this is a guard function (name ends with '?')
        let is_guard = sig.name.name.ends_with('?');

        // Validate guard function constraints
        if is_guard {
            let bool_type = context.interner.intern_type("Outrun.Core.Boolean");
            if return_type != bool_type {
                return Err(TypeError::InvalidGuardFunction {
                    function_name: sig.name.name.clone(),
                    actual_return_type: context
                        .get_type_name(return_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                    span: crate::error::span_to_source_span(sig.span),
                });
            }
        }

        // Validate that instance functions have at least one Self parameter
        let self_type_id = context.interner.intern_type("Self");
        let has_self_param = params
            .iter()
            .any(|(_, param_type)| *param_type == self_type_id);
        if !has_self_param {
            return Err(TypeError::MissingSelfParameter {
                function_name: sig.name.name.clone(),
                span: crate::error::span_to_source_span(sig.span),
            });
        }

        Ok(crate::types::traits::TraitFunction::new(
            func_name,
            params,
            return_type,
            is_guard,
            sig.span,
        ))
    }

    /// Process a trait function definition (full implementation within trait)
    fn process_trait_function_definition(
        context: &mut TypeContext,
        def: &outrun_parser::FunctionDefinition,
        generic_params: &[(String, TypeId)],
    ) -> Result<crate::types::traits::TraitFunction, TypeError> {
        let func_name = context.interner.intern_atom(&def.name.name);

        // Process parameters
        let mut params = Vec::new();
        for param in &def.parameters {
            let param_name = context.interner.intern_atom(&param.name.name);
            let param_type =
                Self::resolve_type_annotation(context, &param.type_annotation, generic_params)?;
            params.push((param_name, param_type));
        }

        // Process return type (default to Unit if not specified)
        let return_type = if let Some(ref ret_type) = def.return_type {
            Self::resolve_type_annotation(context, ret_type, generic_params)?
        } else {
            context.interner.intern_type("Outrun.Core.Unit")
        };

        // Check if this is a guard function (name ends with '?')
        let is_guard = def.name.name.ends_with('?');

        // Validate guard function constraints
        if is_guard {
            let bool_type = context.interner.intern_type("Outrun.Core.Boolean");
            if return_type != bool_type {
                return Err(TypeError::InvalidGuardFunction {
                    function_name: def.name.name.clone(),
                    actual_return_type: context
                        .get_type_name(return_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                    span: crate::error::span_to_source_span(def.span),
                });
            }
        }

        // Validate that instance functions have at least one Self parameter
        let self_type_id = context.interner.intern_type("Self");
        let has_self_param = params
            .iter()
            .any(|(_, param_type)| *param_type == self_type_id);
        if !has_self_param {
            return Err(TypeError::MissingSelfParameter {
                function_name: def.name.name.clone(),
                span: crate::error::span_to_source_span(def.span),
            });
        }

        // TODO: Type check the function body if present
        // For now, we just process the signature

        Ok(crate::types::traits::TraitFunction::new_with_default(
            func_name,
            params,
            return_type,
            is_guard,
            def.span,
        ))
    }

    /// Process a static trait function definition (using `defs` keyword)
    /// Note: Static functions cannot use generic parameters (including Self)
    fn process_static_function_definition(
        context: &mut TypeContext,
        static_def: &outrun_parser::StaticFunctionDefinition,
        _generic_params: &[(String, TypeId)], // Used for validation only
    ) -> Result<crate::types::traits::TraitFunction, TypeError> {
        let func_name = context.interner.intern_atom(&static_def.name.name);

        // Process parameters (static functions cannot use generic types including Self)
        let mut params = Vec::new();
        for param in &static_def.parameters {
            let param_name = context.interner.intern_atom(&param.name.name);
            let param_type = Self::resolve_type_annotation(context, &param.type_annotation, &[])?;
            params.push((param_name, param_type));
        }

        // Process return type (default to Unit if not specified, static functions cannot use generic types)
        let return_type = if let Some(ref ret_type) = static_def.return_type {
            Self::resolve_type_annotation(context, ret_type, &[])?
        } else {
            context.interner.intern_type("Outrun.Core.Unit")
        };

        // Static functions cannot be guard functions (no `?` in name validation needed since parser enforces `defs` syntax)
        // But let's validate that the name doesn't end with `?` to be safe
        if static_def.name.name.ends_with('?') {
            return Err(TypeError::InvalidGuardFunction {
                function_name: static_def.name.name.clone(),
                actual_return_type: "Static functions cannot be guard functions".to_string(),
                span: crate::error::span_to_source_span(static_def.span),
            });
        }

        // TODO: Type check the function body
        // For now, we just process the signature

        Ok(crate::types::traits::TraitFunction::new_static(
            func_name,
            params,
            return_type,
            static_def.span,
        ))
    }

    /// Process trait constraint expressions (T: Display && U: Debug)
    fn process_constraint_expression(
        context: &mut TypeContext,
        constraint_expr: &outrun_parser::ConstraintExpression,
        generic_params: &[TypeId],
    ) -> Result<Vec<crate::types::traits::TraitConstraint>, TypeError> {
        match constraint_expr {
            outrun_parser::ConstraintExpression::And { left, right, .. } => {
                let mut constraints = Vec::new();
                constraints.extend(Self::process_constraint_expression(
                    context,
                    left,
                    generic_params,
                )?);
                constraints.extend(Self::process_constraint_expression(
                    context,
                    right,
                    generic_params,
                )?);
                Ok(constraints)
            }
            outrun_parser::ConstraintExpression::Constraint {
                type_param,
                trait_bound,
                ..
            } => {
                // Find the type parameter in our generic params
                let param_type_id = context.interner.intern_type(&type_param.name);

                // Validate that this type parameter is declared in the trait's generic params
                if !generic_params.contains(&param_type_id) {
                    return Err(TypeError::UndefinedTypeParameter {
                        parameter_name: type_param.name.clone(),
                        span: crate::error::span_to_source_span(type_param.span),
                    });
                }

                // Process the required traits
                let mut required_traits = Vec::new();
                for trait_name in trait_bound {
                    let trait_id = context.interner.intern_trait(&trait_name.name);
                    required_traits.push(trait_id);
                }

                let constraint = crate::types::traits::TraitConstraint::new(
                    param_type_id,
                    required_traits,
                    *constraint_expr.span(),
                );
                Ok(vec![constraint])
            }
            outrun_parser::ConstraintExpression::Parenthesized { expression, .. } => {
                Self::process_constraint_expression(context, expression, generic_params)
            }
        }
    }

    /// Type check a struct definition
    fn check_struct_definition(
        &mut self,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<TypedItem, TypeError> {
        // Register the struct type in our registry
        let struct_type_id = self.context.interner.intern_type(&struct_def.name.name);

        // Process struct fields
        let mut typed_fields = Vec::new();
        let mut concrete_fields = Vec::new();

        for field in &struct_def.fields {
            // Resolve field type
            let field_type_id =
                Self::resolve_type_annotation(&mut self.context, &field.type_annotation, &[])?;
            typed_fields.push((field.name.name.clone(), field_type_id));

            // Create concrete field for registry
            let field_atom = self.context.interner.intern_atom(&field.name.name);
            concrete_fields.push(crate::types::concrete::StructField {
                name: field_atom,
                type_id: field_type_id,
                span: field.span,
            });
        }

        // Create concrete struct type and register it
        let concrete_struct = crate::types::concrete::ConcreteType::Struct {
            name: struct_type_id,
            fields: concrete_fields,
        };

        self.context
            .register_concrete_type(struct_type_id, concrete_struct);

        // Process struct methods (if any)
        let mut typed_methods = Vec::new();
        for method in &struct_def.methods {
            match self.check_function_definition_with_impl_context(method, struct_type_id) {
                Ok(typed_item) => {
                    if let TypedItemKind::FunctionDefinition(func_def) = typed_item.kind {
                        typed_methods.push(func_def);
                    }
                }
                Err(err) => return Err(err),
            }
        }

        Ok(TypedItem {
            kind: TypedItemKind::StructDefinition(TypedStructDefinition {
                name: struct_def.name.name.clone(),
                fields: typed_fields,
                methods: typed_methods,
                span: struct_def.span,
            }),
            span: struct_def.span,
        })
    }

    /// Type check an implementation block
    fn check_impl_block(
        &mut self,
        impl_block: &outrun_parser::ImplBlock,
    ) -> Result<TypedItem, TypeError> {
        // Resolve trait and type specifications
        let trait_id = self.resolve_type_spec_to_trait(&impl_block.trait_spec)?;
        let type_id = self.resolve_type_spec_to_type(&impl_block.type_spec)?;

        // Get the trait definition to validate against
        let trait_def = self
            .context
            .trait_registry
            .get_trait(trait_id)
            .cloned()
            .ok_or_else(|| TypeError::UndefinedTrait {
                trait_name: self.format_type_spec(&impl_block.trait_spec),
                span: crate::error::span_to_source_span(impl_block.trait_spec.span),
            })?;

        // Validate that the type exists in our registry
        let type_name = self.format_type_spec(&impl_block.type_spec);
        let type_exists = self.context.get_concrete_type(type_id).is_some();

        if !type_exists {
            return Err(TypeError::UndefinedType {
                name: type_name,
                span: crate::error::span_to_source_span(impl_block.type_spec.span),
            });
        }

        // Check if this implementation already exists
        if self
            .context
            .trait_registry
            .implements_trait(type_id, trait_id)
        {
            return Err(TypeError::DuplicateImplementation {
                trait_name: self.format_type_spec(&impl_block.trait_spec),
                type_name: self.format_type_spec(&impl_block.type_spec),
                span: crate::error::span_to_source_span(impl_block.span),
            });
        }

        // Process generic parameters and constraints
        let generic_params = self.process_impl_generic_params(&impl_block.generic_params)?;
        let constraints = if let Some(ref constraint_expr) = impl_block.constraints {
            self.process_impl_constraints(constraint_expr, &generic_params)?
        } else {
            Vec::new()
        };

        // Validate all required trait functions are implemented
        let mut function_implementations = std::collections::HashMap::new();
        for trait_func in &trait_def.functions {
            let implemented_func = impl_block.methods.iter().find(|method| {
                let trait_func_name = self
                    .context
                    .interner
                    .atom_name(trait_func.name)
                    .map(|s| s.to_string())
                    .unwrap_or_default();
                method.name.name == trait_func_name
            });

            match implemented_func {
                Some(method) => {
                    // Validate function signature compatibility
                    self.validate_function_signature_compatibility(trait_func, method, type_id)?;

                    // Generate unique function ID and register
                    let func_id = self.context.trait_registry.next_function_id();
                    function_implementations.insert(trait_func.name, func_id);
                }
                None => {
                    // Check if implementation is required
                    if !trait_func.is_static && !trait_func.has_default_impl {
                        // Function signature without default implementation - must be implemented
                        return Err(TypeError::MissingImplementation {
                            trait_name: trait_def.name.clone(),
                            type_name: self.format_type_spec(&impl_block.type_spec),
                            function_name: self
                                .context
                                .interner
                                .atom_name(trait_func.name)
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| "unknown".to_string()),
                            span: crate::error::span_to_source_span(impl_block.span),
                        });
                    }
                    // For default implementations and static functions, no implementation is required
                }
            }
        }

        // Check for extra methods not declared in trait
        for method in &impl_block.methods {
            let trait_has_method = trait_def.functions.iter().any(|f| {
                let func_name = self
                    .context
                    .interner
                    .atom_name(f.name)
                    .map(|s| s.to_string())
                    .unwrap_or_default();
                func_name == method.name.name
            });

            if !trait_has_method {
                return Err(TypeError::ExtraImplementation {
                    trait_name: trait_def.name.clone(),
                    function_name: method.name.name.clone(),
                    span: crate::error::span_to_source_span(method.span),
                });
            }
        }

        // Create and register trait implementation
        let trait_impl = crate::types::traits::TraitImplementation {
            trait_id,
            type_id,
            functions: function_implementations,
            generic_params,
            constraints,
            span: impl_block.span,
        };

        self.context
            .trait_registry
            .register_implementation(trait_impl);

        // Type check all implementation methods
        let mut typed_methods = Vec::new();
        for method in &impl_block.methods {
            match self.check_function_definition_with_impl_context(method, type_id) {
                Ok(typed_item) => {
                    if let TypedItemKind::FunctionDefinition(func_def) = typed_item.kind {
                        typed_methods.push(func_def);
                    }
                }
                Err(err) => return Err(err),
            }
        }

        Ok(TypedItem {
            kind: TypedItemKind::ImplBlock(TypedImplBlock {
                trait_name: self.format_type_spec(&impl_block.trait_spec),
                type_name: self.format_type_spec(&impl_block.type_spec),
                methods: typed_methods,
                span: impl_block.span,
            }),
            span: impl_block.span,
        })
    }

    /// Resolve a type specification to a trait ID
    fn resolve_type_spec_to_trait(
        &mut self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<crate::types::TraitId, TypeError> {
        let trait_name = self.format_type_spec(type_spec);
        Ok(self.context.interner.intern_trait(&trait_name))
    }

    /// Resolve a type specification to a type ID
    fn resolve_type_spec_to_type(
        &mut self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<TypeId, TypeError> {
        let type_name = self.format_type_spec(type_spec);
        Ok(self.context.interner.intern_type(&type_name))
    }

    /// Format a type specification as a string
    fn format_type_spec(&self, type_spec: &outrun_parser::TypeSpec) -> String {
        type_spec
            .path
            .iter()
            .map(|part| part.name.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Process generic parameters from implementation block
    fn process_impl_generic_params(
        &mut self,
        generic_params: &Option<outrun_parser::GenericParams>,
    ) -> Result<Vec<TypeId>, TypeError> {
        let mut params = Vec::new();
        if let Some(ref generics) = generic_params {
            for param in &generics.params {
                let param_type_id = self.context.interner.intern_type(&param.name.name);
                params.push(param_type_id);
            }
        }
        Ok(params)
    }

    /// Process constraint expressions from implementation block
    fn process_impl_constraints(
        &mut self,
        constraint_expr: &outrun_parser::ConstraintExpression,
        generic_params: &[TypeId],
    ) -> Result<Vec<crate::types::traits::TraitConstraint>, TypeError> {
        Self::process_constraint_expression(&mut self.context, constraint_expr, generic_params)
    }

    /// Validate that a function implementation matches the trait signature
    fn validate_function_signature_compatibility(
        &mut self,
        trait_func: &crate::types::traits::TraitFunction,
        impl_func: &outrun_parser::FunctionDefinition,
        implementing_type: TypeId,
    ) -> Result<(), TypeError> {
        // Check parameter count
        if trait_func.params.len() != impl_func.parameters.len() {
            return Err(TypeError::SignatureMismatch {
                function_name: impl_func.name.name.clone(),
                expected: format!("{} parameters", trait_func.params.len()),
                found: format!("{} parameters", impl_func.parameters.len()),
                span: crate::error::span_to_source_span(impl_func.span),
            });
        }

        // Check parameter types and names
        for (i, (trait_param, impl_param)) in trait_func
            .params
            .iter()
            .zip(&impl_func.parameters)
            .enumerate()
        {
            // Resolve implementation parameter type
            let impl_param_type = match Self::resolve_type_annotation_with_self(
                &mut self.context,
                &impl_param.type_annotation,
                &[],
                implementing_type,
            ) {
                Ok(type_id) => type_id,
                Err(_) => {
                    return Err(TypeError::SignatureMismatch {
                        function_name: impl_func.name.name.clone(),
                        expected: format!("valid type for parameter {}", i + 1),
                        found: "invalid type".to_string(),
                        span: crate::error::span_to_source_span(impl_param.span),
                    });
                }
            };

            // Check parameter type compatibility
            // Special case: if trait parameter is Self, it should match the implementing type
            let self_type_id = self.context.interner.intern_type("Self");
            let types_match = if trait_param.1 == self_type_id {
                impl_param_type == implementing_type
            } else {
                trait_param.1 == impl_param_type
            };

            if !types_match {
                return Err(TypeError::SignatureMismatch {
                    function_name: impl_func.name.name.clone(),
                    expected: if trait_param.1 == self_type_id {
                        self.context
                            .get_type_name(implementing_type)
                            .unwrap_or("Self")
                            .to_string()
                    } else {
                        self.context
                            .get_type_name(trait_param.1)
                            .unwrap_or("Unknown")
                            .to_string()
                    },
                    found: self
                        .context
                        .get_type_name(impl_param_type)
                        .unwrap_or("Unknown")
                        .to_string(),
                    span: crate::error::span_to_source_span(impl_param.span),
                });
            }

            // Check parameter name compatibility
            let trait_param_name = self
                .context
                .interner
                .atom_name(trait_param.0)
                .unwrap_or_else(|| "unknown".to_string());
            if trait_param_name != impl_param.name.name {
                return Err(TypeError::SignatureMismatch {
                    function_name: impl_func.name.name.clone(),
                    expected: format!("parameter named '{}'", trait_param_name),
                    found: format!("parameter named '{}'", impl_param.name.name),
                    span: crate::error::span_to_source_span(impl_param.name.span),
                });
            }
        }

        // Check return type compatibility
        let impl_return_type = if let Some(ref ret_type) = impl_func.return_type {
            Self::resolve_type_annotation_with_self(
                &mut self.context,
                ret_type,
                &[],
                implementing_type,
            )?
        } else {
            self.context.interner.intern_type("Outrun.Core.Unit")
        };

        // Check return type compatibility
        // Special case: if trait return type is Self, it should match the implementing type
        let self_type_id = self.context.interner.intern_type("Self");
        let return_types_match = if trait_func.return_type == self_type_id {
            impl_return_type == implementing_type
        } else {
            trait_func.return_type == impl_return_type
        };

        if !return_types_match {
            return Err(TypeError::SignatureMismatch {
                function_name: impl_func.name.name.clone(),
                expected: format!(
                    "return type {}",
                    if trait_func.return_type == self_type_id {
                        self.context
                            .get_type_name(implementing_type)
                            .unwrap_or("Self")
                    } else {
                        self.context
                            .get_type_name(trait_func.return_type)
                            .unwrap_or("Unknown")
                    }
                ),
                found: format!(
                    "return type {}",
                    self.context
                        .get_type_name(impl_return_type)
                        .unwrap_or("Unknown")
                ),
                span: crate::error::span_to_source_span(impl_func.span),
            });
        }

        Ok(())
    }

    /// Resolve a type annotation to a TypeId
    ///
    /// generic_params: Available generic type parameters in current scope
    fn resolve_type_annotation(
        context: &mut TypeContext,
        type_annotation: &outrun_parser::TypeAnnotation,
        generic_params: &[(String, TypeId)], // (name, type_id) pairs
    ) -> Result<TypeId, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, span, .. } => {
                // Join the path parts to create the type name
                let type_name = path
                    .iter()
                    .map(|part| part.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // First, check if this is a generic parameter
                for (param_name, param_type_id) in generic_params {
                    if param_name == &type_name {
                        return Ok(*param_type_id);
                    }
                }

                // Map short type names to fully qualified names for built-in types
                let qualified_type_name = match type_name.as_str() {
                    "Boolean" => "Outrun.Core.Boolean",
                    "Integer" => "Outrun.Core.Integer64",
                    "Float" => "Outrun.Core.Float64",
                    "String" => "Outrun.Core.String",
                    "Unit" => "Outrun.Core.Unit",
                    "List" => "Outrun.Core.List",
                    "Map" => "Outrun.Core.Map",
                    "Tuple" => "Outrun.Core.Tuple",
                    "Option" => "Outrun.Core.Option",
                    "Result" => "Outrun.Core.Result",
                    _ => &type_name, // Use as-is for other types
                };

                // Intern the type name to get a TypeId
                let type_id = context.interner.intern_type(qualified_type_name);

                // Check if the type has been registered in the context
                // Built-in types are always registered during context creation
                if context.get_concrete_type(type_id).is_some() {
                    Ok(type_id)
                } else {
                    // Type not found - return UndefinedType error
                    Err(TypeError::UndefinedType {
                        name: type_name,
                        span: crate::error::span_to_source_span(*span),
                    })
                }
            }
            outrun_parser::TypeAnnotation::Tuple { span, .. } => {
                // TODO: Handle tuple types properly
                Err(TypeError::UnimplementedFeature {
                    feature: "Tuple type annotations in trait definitions".to_string(),
                    span: crate::error::span_to_source_span(*span),
                })
            }
            outrun_parser::TypeAnnotation::Function { span, .. } => {
                // TODO: Handle function types properly
                Err(TypeError::UnimplementedFeature {
                    feature: "Function type annotations in trait definitions".to_string(),
                    span: crate::error::span_to_source_span(*span),
                })
            }
        }
    }

    /// Resolve type annotation with support for Self in impl blocks
    fn resolve_type_annotation_with_self(
        context: &mut TypeContext,
        type_annotation: &outrun_parser::TypeAnnotation,
        generic_params: &[(String, TypeId)], // (name, type_id) pairs
        self_type: TypeId,
    ) -> Result<TypeId, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, span, .. } => {
                // Join the path parts to create the type name
                let type_name = path
                    .iter()
                    .map(|part| part.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this is Self
                if type_name == "Self" {
                    return Ok(self_type);
                }

                // First, check if this is a generic parameter
                for (param_name, param_type_id) in generic_params {
                    if param_name == &type_name {
                        return Ok(*param_type_id);
                    }
                }

                // Map short type names to fully qualified names for built-in types
                let qualified_type_name = match type_name.as_str() {
                    "Boolean" => "Outrun.Core.Boolean",
                    "Integer" => "Outrun.Core.Integer64",
                    "Float" => "Outrun.Core.Float64",
                    "String" => "Outrun.Core.String",
                    "Unit" => "Outrun.Core.Unit",
                    "List" => "Outrun.Core.List",
                    "Map" => "Outrun.Core.Map",
                    "Tuple" => "Outrun.Core.Tuple",
                    "Option" => "Outrun.Core.Option",
                    "Result" => "Outrun.Core.Result",
                    _ => &type_name, // Use as-is for other types
                };

                // Intern the type name to get a TypeId
                let type_id = context.interner.intern_type(qualified_type_name);

                // Check if the type has been registered in the context
                // Built-in types are always registered during context creation
                if context.get_concrete_type(type_id).is_some() {
                    Ok(type_id)
                } else {
                    // Type not found - return UndefinedType error
                    Err(TypeError::UndefinedType {
                        name: type_name,
                        span: crate::error::span_to_source_span(*span),
                    })
                }
            }
            outrun_parser::TypeAnnotation::Tuple { span, .. } => {
                // TODO: Handle tuple types properly
                Err(TypeError::UnimplementedFeature {
                    feature: "Tuple type annotations in trait definitions".to_string(),
                    span: crate::error::span_to_source_span(*span),
                })
            }
            outrun_parser::TypeAnnotation::Function { span, .. } => {
                // TODO: Handle function types properly
                Err(TypeError::UnimplementedFeature {
                    feature: "Function type annotations in trait definitions".to_string(),
                    span: crate::error::span_to_source_span(*span),
                })
            }
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
