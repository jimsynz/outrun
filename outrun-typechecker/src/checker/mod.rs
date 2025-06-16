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
    AnonymousFunction {
        clauses: Vec<TypedAnonymousClause>,
        function_type: TypeId, // Function<(params...) -> ReturnType>
    },
    Sigil {
        name: String,                                   // Sigil name (e.g., "r", "json")
        content: String,                                // Processed content
        raw_content: String,                            // Original content for reconstruction
        interpolated_expressions: Vec<TypedExpression>, // Type-checked interpolated expressions
        result_type: TypeId,                            // Type returned by the sigil
    },
    MacroInjection {
        name: String, // Macro parameter name being injected (e.g., "condition" in ^condition)
        injected_type: TypeId, // Type of the injected value
    },
}

/// Typed block with statements
#[derive(Debug, Clone, PartialEq)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub result_type: TypeId, // Type of the block's result
    pub span: Span,
}

/// Typed anonymous function clause
#[derive(Debug, Clone, PartialEq)]
pub struct TypedAnonymousClause {
    pub params: Vec<(AtomId, TypeId)>,  // Parameter names and types
    pub guard: Option<TypedExpression>, // Optional guard expression (must return Boolean)
    pub body: TypedAnonymousBody,       // Function body (expression or block)
    pub return_type: TypeId,            // Return type of this clause
    pub span: Span,
}

/// Body of an anonymous function clause
#[derive(Debug, Clone, PartialEq)]
pub enum TypedAnonymousBody {
    Expression(TypedExpression),
    Block(TypedBlock),
}

impl TypedAnonymousBody {
    /// Get the return type of this body
    pub fn return_type(&self) -> TypeId {
        match self {
            TypedAnonymousBody::Expression(expr) => expr.type_id,
            TypedAnonymousBody::Block(block) => block.result_type,
        }
    }

    /// Get the span of this body
    pub fn span(&self) -> Span {
        match self {
            TypedAnonymousBody::Expression(expr) => expr.span,
            TypedAnonymousBody::Block(block) => block.span,
        }
    }
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

/// Typed pattern with complete type information
#[derive(Debug, Clone, PartialEq)]
pub enum TypedPattern {
    Identifier {
        name: String,
        type_id: TypeId,
    },
    Literal {
        literal: TypedLiteral,
        type_id: TypeId,
    },
    Tuple {
        elements: Vec<TypedPattern>,
        element_types: Vec<TypeId>, // Type of each element
        tuple_type: TypeId,         // Overall tuple type
    },
    Struct {
        type_name: String,
        type_id: TypeId,
        fields: Vec<TypedStructFieldPattern>,
    },
    List {
        elements: Vec<TypedPattern>,
        rest: Option<String>, // Rest pattern identifier if present
        element_type: TypeId, // Type of list elements
        list_type: TypeId,    // Overall list type
    },
}

/// Typed literal pattern
#[derive(Debug, Clone, PartialEq)]
pub enum TypedLiteral {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Atom(String),
}

/// Typed struct field pattern
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructFieldPattern {
    pub name: String,          // Field name
    pub pattern: TypedPattern, // Pattern for the field value
    pub field_type: TypeId,    // Type of this field
}

/// Typed let binding
#[derive(Debug, Clone, PartialEq)]
pub struct TypedLetBinding {
    pub pattern: TypedPattern,
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

    /// Convert an untyped program to a typed program
    ///
    /// This is the main entry point for AST conversion. It performs complete type checking
    /// and returns a typed AST with dispatch tables.
    pub fn convert_program(&mut self, program: &Program) -> Result<TypedProgram, Vec<TypeError>> {
        self.check_program(program)
    }

    /// Convert an untyped expression to a typed expression
    ///
    /// This performs type checking on the expression and returns a typed AST node.
    /// The expression is checked in the current type context.
    pub fn convert_expression(
        &mut self,
        expr: &outrun_parser::Expression,
    ) -> Result<TypedExpression, TypeError> {
        ExpressionChecker::check_expression(&mut self.context, expr)
    }

    /// Convert an untyped block to a typed block
    ///
    /// This performs type checking on all statements in the block and returns a typed block.
    /// The block is checked in the current type context with proper scope management.
    pub fn convert_block(&mut self, block: &outrun_parser::Block) -> Result<TypedBlock, TypeError> {
        self.context.push_scope(false); // Block scope

        let mut typed_statements = Vec::new();
        let mut block_result_type = self.context.interner.intern_type("Outrun.Core.Unit");

        for statement in &block.statements {
            match &statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    let typed_expr = self.convert_expression(expr)?;
                    // Update block result type to the last expression
                    block_result_type = typed_expr.type_id;
                    typed_statements.push(TypedStatement {
                        kind: TypedStatementKind::Expression(Box::new(typed_expr)),
                        span: statement.span,
                    });
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    let typed_let = self.convert_let_binding(let_binding)?;
                    typed_statements.push(TypedStatement {
                        kind: TypedStatementKind::LetBinding(Box::new(typed_let)),
                        span: statement.span,
                    });
                }
            }
        }

        self.context.pop_scope();

        Ok(TypedBlock {
            statements: typed_statements,
            result_type: block_result_type,
            span: block.span,
        })
    }

    /// Convert an untyped let binding to a typed let binding
    ///
    /// This performs type checking on the let binding expression and pattern,
    /// registering bound variables in the current scope.
    pub fn convert_let_binding(
        &mut self,
        let_binding: &outrun_parser::LetBinding,
    ) -> Result<TypedLetBinding, TypeError> {
        // Type check the right-hand side expression first
        let typed_expression = self.convert_expression(&let_binding.expression)?;

        // Use pattern checker to validate the pattern and register variables
        let bound_variables = PatternChecker::check_pattern(
            &mut self.context,
            &let_binding.pattern,
            typed_expression.type_id,
        )?;

        // Register all bound variables in the current scope
        for variable in bound_variables {
            self.context.register_variable(variable)?;
        }

        // Convert the pattern to a typed pattern
        let typed_pattern = self.convert_pattern(&let_binding.pattern, typed_expression.type_id)?;

        Ok(TypedLetBinding {
            pattern: typed_pattern,
            type_id: typed_expression.type_id,
            expression: typed_expression,
            span: let_binding.span,
        })
    }

    /// Convert an untyped pattern to a typed pattern
    ///
    /// This converts a parser pattern to a typed pattern with complete type information.
    /// The target_type is the expected type that this pattern should match.
    pub fn convert_pattern(
        &mut self,
        pattern: &outrun_parser::Pattern,
        target_type: TypeId,
    ) -> Result<TypedPattern, TypeError> {
        match pattern {
            outrun_parser::Pattern::Identifier(ident) => Ok(TypedPattern::Identifier {
                name: ident.name.clone(),
                type_id: target_type,
            }),
            outrun_parser::Pattern::Literal(literal_pattern) => {
                let typed_literal = self.convert_literal_pattern(&literal_pattern.literal)?;
                Ok(TypedPattern::Literal {
                    literal: typed_literal,
                    type_id: target_type,
                })
            }
            outrun_parser::Pattern::Tuple(tuple_pattern) => {
                // Get the concrete tuple type to extract element types
                let concrete_type =
                    self.context.get_concrete_type(target_type).ok_or_else(|| {
                        TypeError::internal("Tuple pattern target type not in registry".to_string())
                    })?;

                let element_types = match concrete_type {
                    crate::types::ConcreteType::Tuple { element_types } => element_types.clone(),
                    _ => {
                        return Err(TypeError::internal(
                            "Tuple pattern expected tuple type".to_string(),
                        ))
                    }
                };

                if tuple_pattern.elements.len() != element_types.len() {
                    return Err(TypeError::internal(
                        "Tuple pattern arity mismatch".to_string(),
                    ));
                }

                let mut typed_elements = Vec::new();
                for (pattern_elem, &elem_type) in
                    tuple_pattern.elements.iter().zip(element_types.iter())
                {
                    let typed_elem = self.convert_pattern(pattern_elem, elem_type)?;
                    typed_elements.push(typed_elem);
                }

                Ok(TypedPattern::Tuple {
                    elements: typed_elements,
                    element_types,
                    tuple_type: target_type,
                })
            }
            outrun_parser::Pattern::Struct(struct_pattern) => {
                // Get the struct type name
                let type_name = struct_pattern
                    .type_path
                    .iter()
                    .map(|id| &id.name)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(".");

                // Get the concrete struct type to validate fields
                let concrete_type =
                    self.context.get_concrete_type(target_type).ok_or_else(|| {
                        TypeError::internal(
                            "Struct pattern target type not in registry".to_string(),
                        )
                    })?;

                let struct_fields = match concrete_type {
                    crate::types::ConcreteType::Struct { fields, .. } => fields.clone(),
                    _ => {
                        return Err(TypeError::internal(
                            "Struct pattern expected struct type".to_string(),
                        ))
                    }
                };

                let mut typed_fields = Vec::new();
                for field_pattern in &struct_pattern.fields {
                    // Find the corresponding struct field
                    let field_atom = self.context.interner.intern_atom(&field_pattern.name.name);
                    let field_type = struct_fields
                        .iter()
                        .find(|field| field.name == field_atom)
                        .map(|field| field.type_id)
                        .ok_or_else(|| {
                            TypeError::internal(format!(
                                "Unknown field {} in struct pattern",
                                field_pattern.name.name
                            ))
                        })?;

                    // Convert the field pattern (could be shorthand or explicit)
                    let field_value_pattern = match &field_pattern.pattern {
                        Some(explicit_pattern) => {
                            self.convert_pattern(explicit_pattern, field_type)?
                        }
                        None => {
                            // Shorthand syntax: field name becomes identifier pattern
                            TypedPattern::Identifier {
                                name: field_pattern.name.name.clone(),
                                type_id: field_type,
                            }
                        }
                    };

                    typed_fields.push(TypedStructFieldPattern {
                        name: field_pattern.name.name.clone(),
                        pattern: field_value_pattern,
                        field_type,
                    });
                }

                Ok(TypedPattern::Struct {
                    type_name,
                    type_id: target_type,
                    fields: typed_fields,
                })
            }
            outrun_parser::Pattern::List(list_pattern) => {
                // Get the concrete list type to extract element type
                let concrete_type =
                    self.context.get_concrete_type(target_type).ok_or_else(|| {
                        TypeError::internal("List pattern target type not in registry".to_string())
                    })?;

                let element_type = match concrete_type {
                    crate::types::ConcreteType::List { element_type } => *element_type,
                    _ => {
                        return Err(TypeError::internal(
                            "List pattern expected list type".to_string(),
                        ))
                    }
                };

                let mut typed_elements = Vec::new();
                for pattern_elem in &list_pattern.elements {
                    let typed_elem = self.convert_pattern(pattern_elem, element_type)?;
                    typed_elements.push(typed_elem);
                }

                Ok(TypedPattern::List {
                    elements: typed_elements,
                    rest: list_pattern.rest.as_ref().map(|ident| ident.name.clone()),
                    element_type,
                    list_type: target_type,
                })
            }
        }
    }

    /// Convert an untyped literal to a typed literal for patterns
    ///
    /// This extracts the literal value and converts it to the typed representation.
    pub fn convert_literal_pattern(
        &mut self,
        literal: &outrun_parser::Literal,
    ) -> Result<TypedLiteral, TypeError> {
        match literal {
            outrun_parser::Literal::Boolean(bool_lit) => Ok(TypedLiteral::Boolean(bool_lit.value)),
            outrun_parser::Literal::Integer(int_lit) => Ok(TypedLiteral::Integer(int_lit.value)),
            outrun_parser::Literal::Float(float_lit) => Ok(TypedLiteral::Float(float_lit.value)),
            outrun_parser::Literal::String(str_lit) => {
                // For pattern strings, we need to extract the simple text content
                // String interpolation in patterns would be more complex and isn't commonly supported
                let content = str_lit
                    .parts
                    .iter()
                    .filter_map(|part| match part {
                        outrun_parser::StringPart::Text { content, .. } => Some(content.as_str()),
                        outrun_parser::StringPart::Interpolation { .. } => None, // Skip interpolations for now
                    })
                    .collect::<Vec<_>>()
                    .join("");
                Ok(TypedLiteral::String(content))
            }
            outrun_parser::Literal::Atom(atom_lit) => Ok(TypedLiteral::Atom(atom_lit.name.clone())),
        }
    }

    /// Convert an untyped function definition to a typed function definition
    ///
    /// This performs comprehensive function validation and returns a typed function definition.
    /// The function is checked in the current type context with proper parameter scoping.
    pub fn convert_function_definition(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
    ) -> Result<TypedFunctionDefinition, TypeError> {
        // Validate function signature first
        FunctionChecker::check_function_definition(&mut self.context, func)?;

        // Resolve parameter types
        let mut typed_params = Vec::new();
        for param in &func.parameters {
            let param_type = Self::resolve_type_annotation(
                &mut self.context,
                &param.type_annotation,
                &[], // No generic parameters in functions
            )?;
            typed_params.push((param.name.name.clone(), param_type));
        }

        // Resolve return type
        let return_type = if let Some(ret_type) = &func.return_type {
            Self::resolve_type_annotation(
                &mut self.context,
                ret_type,
                &[], // No generic parameters in functions
            )?
        } else {
            return Err(TypeError::UnimplementedFeature {
                feature: "Functions must have explicit return type annotations".to_string(),
                span: crate::error::span_to_source_span(func.span),
            });
        };

        // Create function scope and register parameters
        self.context.push_scope(true); // Function scope

        for (param_name, param_type) in &typed_params {
            let variable = Variable {
                name: param_name.clone(),
                type_id: *param_type,
                is_mutable: false,
                span: func.span,
            };
            self.context.register_variable(variable)?;
        }

        // Type check the function body
        let typed_block =
            FunctionChecker::check_function_body_typed(&mut self.context, &func.body, return_type)?;
        let typed_body = TypedExpression {
            kind: TypedExpressionKind::Block(typed_block.clone()),
            type_id: typed_block.result_type,
            span: func.body.span,
        };

        // Clean up function scope
        self.context.pop_scope();

        Ok(TypedFunctionDefinition {
            name: func.name.name.clone(),
            params: typed_params,
            return_type,
            body: typed_body,
            span: func.span,
        })
    }

    /// Convert an untyped const definition to a typed const definition
    ///
    /// This performs type checking on the constant expression and validates any explicit type annotation.
    pub fn convert_const_definition(
        &mut self,
        const_def: &outrun_parser::ConstDefinition,
    ) -> Result<TypedConstDefinition, TypeError> {
        // Type check the constant expression
        let typed_value = self.convert_expression(&const_def.expression)?;

        // TODO: Validate explicit type annotation if present and ensure it matches the expression type

        Ok(TypedConstDefinition {
            name: const_def.name.name.clone(),
            type_id: typed_value.type_id,
            value: typed_value,
            span: const_def.span,
        })
    }

    /// Convert an untyped struct definition to a typed struct definition
    ///
    /// This performs validation of the struct fields and creates a typed struct definition.
    pub fn convert_struct_definition(
        &mut self,
        struct_def: &outrun_parser::StructDefinition,
    ) -> Result<TypedStructDefinition, TypeError> {
        // Resolve field types
        let mut typed_fields = Vec::new();
        for field in &struct_def.fields {
            let field_type = Self::resolve_type_annotation(
                &mut self.context,
                &field.type_annotation,
                &[], // No generic parameters in structs yet
            )?;
            typed_fields.push((field.name.name.clone(), field_type));
        }

        // TODO: Handle struct methods when they are added to the parser

        Ok(TypedStructDefinition {
            name: struct_def.name.name.clone(),
            fields: typed_fields,
            methods: Vec::new(), // No methods yet in struct definitions
            span: struct_def.span,
        })
    }

    /// Convert an untyped trait definition to a typed trait definition
    ///
    /// This performs validation of trait functions and constraints.
    pub fn convert_trait_definition(
        &mut self,
        trait_def: &outrun_parser::TraitDefinition,
    ) -> Result<TypedTraitDefinition, TypeError> {
        // Process trait functions
        let mut typed_functions = Vec::new();

        for trait_func in &trait_def.functions {
            match trait_func {
                outrun_parser::TraitFunction::Signature(sig) => {
                    let typed_sig = self.convert_trait_function_signature(sig)?;
                    typed_functions.push(typed_sig);
                }
                outrun_parser::TraitFunction::Definition(_def) => {
                    // TODO: Handle trait function definitions with bodies
                    return Err(TypeError::UnimplementedFeature {
                        feature:
                            "Trait function definitions with bodies not yet supported in conversion"
                                .to_string(),
                        span: crate::error::span_to_source_span(trait_def.span),
                    });
                }
                outrun_parser::TraitFunction::StaticDefinition(_static_def) => {
                    // TODO: Handle static trait function definitions
                    return Err(TypeError::UnimplementedFeature {
                        feature:
                            "Static trait function definitions not yet supported in conversion"
                                .to_string(),
                        span: crate::error::span_to_source_span(trait_def.span),
                    });
                }
            }
        }

        Ok(TypedTraitDefinition {
            name: trait_def.name.name.clone(),
            functions: typed_functions,
            span: trait_def.span,
        })
    }

    /// Convert an untyped trait function signature to a typed function signature
    ///
    /// This performs parameter and return type validation for trait function signatures.
    pub fn convert_trait_function_signature(
        &mut self,
        sig: &outrun_parser::FunctionSignature,
    ) -> Result<TypedFunctionSignature, TypeError> {
        // Pre-intern the Self type to avoid borrow conflicts
        let self_type_id = self.context.interner.intern_type("Self");
        let generic_params = vec![("Self".to_string(), self_type_id)];

        // Resolve parameter types
        let mut typed_params = Vec::new();
        for param in &sig.parameters {
            let param_type = Self::resolve_type_annotation(
                &mut self.context,
                &param.type_annotation,
                &generic_params, // Include Self for trait functions
            )?;
            typed_params.push((param.name.name.clone(), param_type));
        }

        // Resolve return type
        let return_type = if let Some(ret_type) = &sig.return_type {
            Self::resolve_type_annotation(
                &mut self.context,
                ret_type,
                &generic_params, // Include Self for trait functions
            )?
        } else {
            self.context.interner.intern_type("Outrun.Core.Unit")
        };

        Ok(TypedFunctionSignature {
            name: sig.name.name.clone(),
            params: typed_params,
            return_type,
            span: sig.span,
        })
    }

    /// Convert an untyped impl block to a typed impl block
    ///
    /// This performs validation of impl block methods and trait implementation requirements.
    pub fn convert_impl_block(
        &mut self,
        impl_block: &outrun_parser::ImplBlock,
    ) -> Result<TypedImplBlock, TypeError> {
        // Convert impl block methods
        let mut typed_methods = Vec::new();

        for method in &impl_block.methods {
            let typed_method = self.convert_function_definition(method)?;
            typed_methods.push(typed_method);
        }

        // Extract trait and type names from TypeSpec
        let trait_name = self.extract_type_name_from_spec(&impl_block.trait_spec)?;
        let type_name = self.extract_type_name_from_spec(&impl_block.type_spec)?;

        Ok(TypedImplBlock {
            trait_name,
            type_name,
            methods: typed_methods,
            span: impl_block.span,
        })
    }

    /// Helper function to extract a type name from a TypeSpec with generic arguments
    fn extract_type_name_from_spec(
        &self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<String, TypeError> {
        // Use the improved format_type_spec method that handles generics
        Ok(self.format_type_spec(type_spec))
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

        // Create a temporary scope for generic parameters
        self.context.push_scope(false);

        // Process generic parameters
        let mut generic_params = Vec::new();
        if let Some(ref generics) = struct_def.generic_params {
            for param in &generics.params {
                let param_type_id = self.context.interner.intern_type(&param.name.name);

                // Register the generic parameter in the current scope
                self.context
                    .register_generic_param(param.name.name.clone(), param_type_id)?;

                generic_params.push((param.name.name.clone(), param_type_id));
            }
        }

        // Process struct fields with generic parameters
        let mut concrete_fields = Vec::new();

        for field in &struct_def.fields {
            // Resolve field type with generic parameters
            let field_type_id = Self::resolve_type_annotation(
                &mut self.context,
                &field.type_annotation,
                &generic_params,
            )?;

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

        // Clean up the temporary scope
        self.context.pop_scope();

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

    /// Type check a function definition with full generic context
    fn check_function_definition_with_generic_context(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
        implementing_type: TypeId,
        generic_params: &[(String, TypeId)],
    ) -> Result<TypedItem, TypeError> {
        // Use the comprehensive function validation from FunctionChecker with full generic context
        FunctionChecker::check_function_definition_with_generic_context(
            &mut self.context,
            func,
            Some(implementing_type),
            generic_params,
        )?;
        self.create_typed_function_item_with_generics(func, Some(implementing_type), generic_params)
    }

    /// Create a typed function item (shared logic for both standalone and impl functions)
    fn create_typed_function_item(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
        implementing_type: Option<TypeId>,
    ) -> Result<TypedItem, TypeError> {
        self.create_typed_function_item_with_generics(func, implementing_type, &[])
    }

    /// Create a typed function item with generic context support
    fn create_typed_function_item_with_generics(
        &mut self,
        func: &outrun_parser::FunctionDefinition,
        implementing_type: Option<TypeId>,
        generic_params: &[(String, TypeId)],
    ) -> Result<TypedItem, TypeError> {
        // Resolve parameter types for typed AST
        let mut typed_params = Vec::new();
        for param in &func.parameters {
            let param_type = if let Some(implementing_type_id) = implementing_type {
                // Use Self-aware type resolution for impl block functions
                Self::resolve_type_annotation_with_self(
                    &mut self.context,
                    &param.type_annotation,
                    generic_params, // Pass generic parameters from containing context
                    implementing_type_id,
                )?
            } else {
                // Use regular type resolution for standalone functions
                Self::resolve_type_annotation(
                    &mut self.context,
                    &param.type_annotation,
                    generic_params, // Pass generic parameters from containing context
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
                    generic_params, // Pass generic parameters from containing context
                    implementing_type_id,
                )?
            } else {
                // Use regular type resolution for standalone functions
                Self::resolve_type_annotation(
                    &mut self.context,
                    ret_type,
                    generic_params, // Pass generic parameters from containing context
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

        // Convert pattern to typed pattern
        let typed_pattern = self.convert_pattern(&let_binding.pattern, binding_type)?;

        // Create a typed let binding
        Ok(TypedItem {
            kind: TypedItemKind::LetBinding(Box::new(TypedLetBinding {
                pattern: typed_pattern,
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

                    // Validate that the trait exists
                    if context.trait_registry.get_trait(trait_id).is_none() {
                        return Err(TypeError::UndefinedTrait {
                            trait_name: trait_name.name.clone(),
                            span: crate::error::span_to_source_span(trait_name.span),
                        });
                    }

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
        // Create struct type name including generic parameters
        let struct_type_name = if let Some(ref generics) = struct_def.generic_params {
            let generic_names: Vec<&str> = generics
                .params
                .iter()
                .map(|p| p.name.name.as_str())
                .collect();
            format!("{}<{}>", struct_def.name.name, generic_names.join(", "))
        } else {
            struct_def.name.name.clone()
        };

        // Register the struct type in our registry
        let struct_type_id = self.context.interner.intern_type(&struct_type_name);

        // Register struct definition with generic parameter count
        let generic_param_count = struct_def
            .generic_params
            .as_ref()
            .map(|g| g.params.len())
            .unwrap_or(0);
        self.context.register_struct_definition(
            struct_def.name.name.clone(),
            generic_param_count,
            struct_def.span,
        );

        // Create a new scope for generic parameters
        self.context.push_scope(false);

        // Process generic parameters - reuse existing TypeIds if already interned
        let mut generic_params = Vec::new();
        if let Some(ref generics) = struct_def.generic_params {
            for param in &generics.params {
                // Reuse the existing TypeId from the first phase to maintain consistency
                let param_type_id = self
                    .context
                    .interner
                    .get_type(&param.name.name)
                    .unwrap_or_else(|| self.context.interner.intern_type(&param.name.name));

                // Register the generic parameter in the current scope
                self.context
                    .register_generic_param(param.name.name.clone(), param_type_id)?;

                generic_params.push((param.name.name.clone(), param_type_id));
            }
        }

        // Process struct fields
        let mut typed_fields = Vec::new();
        let mut concrete_fields = Vec::new();

        for field in &struct_def.fields {
            // Resolve field type with generic parameters
            let field_type_id = Self::resolve_type_annotation(
                &mut self.context,
                &field.type_annotation,
                &generic_params,
            )?;

            typed_fields.push((field.name.name.clone(), field_type_id));

            // Create concrete field for registry
            let field_atom = self.context.interner.intern_atom(&field.name.name);
            concrete_fields.push(crate::types::concrete::StructField {
                name: field_atom,
                type_id: field_type_id,
                span: field.span,
            });
        }

        // Create concrete struct type and register it (only if not already registered)
        if self.context.get_concrete_type(struct_type_id).is_none() {
            let concrete_struct = crate::types::concrete::ConcreteType::Struct {
                name: struct_type_id,
                fields: concrete_fields,
            };

            self.context
                .register_concrete_type(struct_type_id, concrete_struct);
        }

        // Process struct methods (if any)
        let mut typed_methods = Vec::new();
        for method in &struct_def.methods {
            match self.check_function_definition_with_generic_context(
                method,
                struct_type_id,
                &generic_params,
            ) {
                Ok(typed_item) => {
                    if let TypedItemKind::FunctionDefinition(func_def) = typed_item.kind {
                        typed_methods.push(func_def);
                    }
                }
                Err(err) => {
                    self.context.pop_scope();
                    return Err(err);
                }
            }
        }

        // Pop the generic parameter scope
        self.context.pop_scope();

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
        // Create a new scope for generic parameters first
        self.context.push_scope(false);

        // Process generic parameters early so they're available for type resolution
        let generic_params = self.process_impl_generic_params(&impl_block.generic_params)?;

        // Now resolve trait and type specifications with generic parameters in scope
        let trait_id = self.resolve_type_spec_to_trait(&impl_block.trait_spec)?;
        let type_id = self.resolve_type_spec_to_type(&impl_block.type_spec)?;

        // Extract trait generic arguments for signature validation (now T is in scope)
        let trait_generic_args = self.extract_trait_generic_arguments(&impl_block.trait_spec)?;

        // Get the trait definition to validate against
        let trait_def = self
            .context
            .trait_registry
            .get_trait(trait_id)
            .cloned()
            .ok_or_else(|| {
                self.context.pop_scope();
                TypeError::UndefinedTrait {
                    trait_name: self.format_type_spec(&impl_block.trait_spec),
                    span: crate::error::span_to_source_span(impl_block.trait_spec.span),
                }
            })?;

        // Validate that the type exists in our registry
        let type_name = self.format_type_spec(&impl_block.type_spec);
        let type_exists = self.context.get_concrete_type(type_id).is_some();

        if !type_exists {
            self.context.pop_scope();
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
            self.context.pop_scope();
            return Err(TypeError::DuplicateImplementation {
                trait_name: self.format_type_spec(&impl_block.trait_spec),
                type_name: self.format_type_spec(&impl_block.type_spec),
                span: crate::error::span_to_source_span(impl_block.span),
            });
        }

        // Process constraints within the scope
        let constraints = if let Some(ref constraint_expr) = impl_block.constraints {
            // Extract just the TypeIds for constraint processing
            let generic_type_ids: Vec<TypeId> =
                generic_params.iter().map(|(_, type_id)| *type_id).collect();
            self.process_impl_constraints(constraint_expr, &generic_type_ids)?
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
                    self.validate_function_signature_compatibility(
                        trait_func,
                        method,
                        type_id,
                        &trait_generic_args,
                        &impl_block.type_spec,
                    )?;

                    // Generate unique function ID and register
                    let func_id = self.context.trait_registry.next_function_id();
                    function_implementations.insert(trait_func.name, func_id);
                }
                None => {
                    // Check if implementation is required
                    if !trait_func.is_static && !trait_func.has_default_impl {
                        // Function signature without default implementation - must be implemented
                        let error = TypeError::MissingImplementation {
                            trait_name: trait_def.name.clone(),
                            type_name: self.format_type_spec(&impl_block.type_spec),
                            function_name: self
                                .context
                                .interner
                                .atom_name(trait_func.name)
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| "unknown".to_string()),
                            span: crate::error::span_to_source_span(impl_block.span),
                        };
                        self.context.pop_scope();
                        return Err(error);
                    }
                    // For default implementations and static functions, no implementation is required
                }
            }
        }

        // Check for extra methods in impl blocks that are not declared in the trait
        for impl_method in &impl_block.methods {
            let impl_method_name = &impl_method.name.name;
            let is_declared_in_trait = trait_def.functions.iter().any(|trait_func| {
                let trait_func_name = self
                    .context
                    .interner
                    .atom_name(trait_func.name)
                    .map(|s| s.to_string())
                    .unwrap_or_default();
                trait_func_name == *impl_method_name
            });

            if !is_declared_in_trait {
                self.context.pop_scope();
                return Err(TypeError::ExtraImplementation {
                    trait_name: trait_def.name.clone(),
                    function_name: impl_method_name.clone(),
                    span: crate::error::span_to_source_span(impl_method.span),
                });
            }
        }

        // Create and register trait implementation
        let trait_impl = crate::types::traits::TraitImplementation {
            trait_id,
            type_id,
            functions: function_implementations,
            generic_params: generic_params.iter().map(|(_, type_id)| *type_id).collect(),
            constraints,
            span: impl_block.span,
        };

        self.context
            .trait_registry
            .register_implementation(trait_impl);

        // Type check all implementation methods
        let mut typed_methods = Vec::new();
        for method in &impl_block.methods {
            match self.check_function_definition_with_generic_context(
                method,
                type_id,
                &generic_params,
            ) {
                Ok(typed_item) => {
                    if let TypedItemKind::FunctionDefinition(func_def) = typed_item.kind {
                        typed_methods.push(func_def);
                    }
                }
                Err(err) => {
                    self.context.pop_scope();
                    return Err(err);
                }
            }
        }

        // Pop the generic parameter scope
        self.context.pop_scope();

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
        // For trait specs, we want to resolve to the base trait, not the full generic trait
        // This is because when we check if a trait exists, we check the base trait
        let base_name = type_spec
            .path
            .iter()
            .map(|part| part.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        Ok(self.context.interner.intern_trait(&base_name))
    }

    /// Extract generic arguments from a trait specification
    /// For example, Convert<String> -> [String]
    fn extract_trait_generic_arguments(
        &mut self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<Vec<TypeId>, TypeError> {
        if let Some(ref generic_args) = type_spec.generic_args {
            let mut type_ids = Vec::new();
            for arg in &generic_args.args {
                let type_id = Self::resolve_type_annotation(&mut self.context, arg, &[])?;
                type_ids.push(type_id);
            }
            Ok(type_ids)
        } else {
            Ok(Vec::new())
        }
    }

    /// Substitute trait generic parameters with concrete types
    ///
    /// This handles cases where a trait has generic parameters that need to be
    /// substituted with concrete types from the impl block.
    ///
    /// For example: trait Convert<T> implemented as Convert<String>
    /// If trait_type_id corresponds to T, return String from trait_generic_args
    fn substitute_trait_generic_parameter(
        &mut self,
        trait_type_id: TypeId,
        trait_generic_args: &[TypeId],
    ) -> Result<TypeId, TypeError> {
        // Get the type name to check if it's a generic parameter
        if let Some(type_name) = self.context.get_type_name(trait_type_id) {
            // Check if this is a simple generic parameter name (like "T", "U", etc.)
            // Generic parameters are typically single letters or simple names
            if type_name.len() == 1 && type_name.chars().next().unwrap().is_ascii_uppercase() {
                // Map generic parameter names to positions using standard convention
                let position = match type_name {
                    "T" => 0,
                    "U" => 1,
                    "V" => 2,
                    "W" => 3,
                    "X" => 4,
                    "Y" => 5,
                    "Z" => 6,
                    _ => {
                        // If not a standard generic parameter, try the old fallback
                        if !trait_generic_args.is_empty() {
                            return Ok(trait_generic_args[0]);
                        } else {
                            return Ok(trait_type_id);
                        }
                    }
                };

                // Use position-based mapping to get the concrete type
                if position < trait_generic_args.len() {
                    return Ok(trait_generic_args[position]);
                }
            }
        }

        // If no substitution needed, return the original type
        Ok(trait_type_id)
    }

    /// Resolve a type specification to a type ID
    fn resolve_type_spec_to_type(
        &mut self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<TypeId, TypeError> {
        // For type specs, we want to resolve to the base type, not the full generic type
        // This is because when we check if a type exists, we check the base type
        let base_name = type_spec
            .path
            .iter()
            .map(|part| part.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        Ok(self.context.interner.intern_type(&base_name))
    }

    /// Create the full implementing type with generic parameters for proper Self resolution
    fn create_full_implementing_type(
        &mut self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<TypeId, TypeError> {
        // For signature validation, we need the full generic type including parameters
        let base_name = type_spec
            .path
            .iter()
            .map(|part| part.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        // If there are generic arguments, create the full type name
        if let Some(ref generic_args) = type_spec.generic_args {
            let args = generic_args
                .args
                .iter()
                .map(Self::format_type_annotation_static)
                .collect::<Vec<_>>()
                .join(", ");
            let full_type_name = format!("{}<{}>", base_name, args);
            Ok(self.context.interner.intern_type(&full_type_name))
        } else {
            // No generic arguments, use base type
            Ok(self.context.interner.intern_type(&base_name))
        }
    }

    /// Format a type specification as a string with generic arguments
    fn format_type_spec(&self, type_spec: &outrun_parser::TypeSpec) -> String {
        let base_name = type_spec
            .path
            .iter()
            .map(|part| part.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        // Add generic arguments if present
        if let Some(generic_args) = &type_spec.generic_args {
            let args = generic_args
                .args
                .iter()
                .map(Self::format_type_annotation_static)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", base_name, args)
        } else {
            base_name
        }
    }

    /// Process generic parameters from implementation block and register them in scope
    fn process_impl_generic_params(
        &mut self,
        generic_params: &Option<outrun_parser::GenericParams>,
    ) -> Result<Vec<(String, TypeId)>, TypeError> {
        let mut params = Vec::new();
        if let Some(ref generics) = generic_params {
            for param in &generics.params {
                let param_type_id = self.context.interner.intern_type(&param.name.name);

                // Register the generic parameter in the current scope
                self.context
                    .register_generic_param(param.name.name.clone(), param_type_id)?;

                params.push((param.name.name.clone(), param_type_id));
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
        trait_generic_args: &[TypeId],
        impl_type_spec: &outrun_parser::TypeSpec,
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

        // Check return type compatibility with generic parameter substitution
        let self_type_id = self.context.interner.intern_type("Self");
        let expected_return_type = if trait_func.return_type == self_type_id {
            // Special case: if trait return type is Self, it should match the full implementing type with generics
            self.create_full_implementing_type(impl_type_spec)?
        } else {
            // Check if the trait return type is a generic parameter that needs substitution
            self.substitute_trait_generic_parameter(trait_func.return_type, trait_generic_args)?
        };

        let return_types_match = impl_return_type == expected_return_type;

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
            outrun_parser::TypeAnnotation::Simple {
                path,
                generic_args,
                span,
            } => {
                // Join the path parts to create the type name
                let type_name = path
                    .iter()
                    .map(|part| part.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // First, check if this is a generic parameter in the passed parameters
                for (param_name, param_type_id) in generic_params {
                    if param_name == &type_name {
                        return Ok(*param_type_id);
                    }
                }

                // Also check if this is a generic parameter in the current scope
                if let Some(type_id) = context.lookup_generic_param(&type_name) {
                    return Ok(type_id);
                }

                // Map short type names to fully qualified names for built-in types
                let base_qualified_type_name = match type_name.as_str() {
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

                // Validate generic argument arity
                let generic_args_slice = if let Some(generics) = generic_args {
                    &generics.args[..]
                } else {
                    &[]
                };
                Self::validate_generic_arity(
                    context,
                    base_qualified_type_name,
                    generic_args_slice,
                    span,
                )?;

                // Handle generic arguments if present
                let final_type_name = if let Some(generics) = generic_args {
                    // First collect all generic TypeIds
                    let mut generic_type_ids = Vec::new();
                    for generic_arg in &generics.args {
                        let generic_type_id =
                            Self::resolve_type_annotation(context, generic_arg, generic_params)?;
                        generic_type_ids.push(generic_type_id);
                    }

                    // Then get their names
                    let mut generic_type_names = Vec::new();
                    for generic_type_id in generic_type_ids {
                        let generic_type_name =
                            context.get_type_name(generic_type_id).ok_or_else(|| {
                                TypeError::internal("Generic argument type not found".to_string())
                            })?;
                        generic_type_names.push(generic_type_name);
                    }
                    format!(
                        "{}<{}>",
                        base_qualified_type_name,
                        generic_type_names.join(", ")
                    )
                } else {
                    base_qualified_type_name.to_string()
                };

                // Intern the type name to get a TypeId
                let type_id = context.interner.intern_type(&final_type_name);

                // Check if the type has been registered in the context
                // For generic types, check if the base type is known
                let base_type_to_check = if final_type_name.contains('<') {
                    // Extract base type from generic type (e.g., "List" from "List<T>")
                    base_qualified_type_name
                } else {
                    &final_type_name
                };

                let base_type_id = context.interner.intern_type(base_type_to_check);
                if context.get_concrete_type(base_type_id).is_some()
                    || final_type_name.contains('<')
                {
                    // Base type exists or this is a generic type, so accept it
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
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                span: _,
            } => {
                // Handle function type: Function<(param1: Type1, param2: Type2) -> ReturnType>
                let mut param_type_names = Vec::new();

                // Process each parameter type
                for param in params {
                    let param_type_id = Self::resolve_type_annotation(
                        context,
                        &param.type_annotation,
                        generic_params,
                    )?;
                    let param_type_name =
                        context.get_type_name(param_type_id).ok_or_else(|| {
                            TypeError::internal("Function parameter type not found".to_string())
                        })?;
                    param_type_names.push(format!("{}: {}", param.name.name, param_type_name));
                }

                // Process return type
                let return_type_id =
                    Self::resolve_type_annotation(context, return_type, generic_params)?;
                let return_type_name = context.get_type_name(return_type_id).ok_or_else(|| {
                    TypeError::internal("Function return type not found".to_string())
                })?;

                // Create function type name: Function<(param1: Type1, param2: Type2) -> ReturnType>
                let function_type_name = format!(
                    "Function<({}) -> {}>",
                    param_type_names.join(", "),
                    return_type_name
                );

                // Intern the function type
                let function_type_id = context.interner.intern_type(&function_type_name);
                Ok(function_type_id)
            }
        }
    }

    /// Format a type annotation as a string (static version for use in static contexts)
    fn format_type_annotation_static(type_annotation: &outrun_parser::TypeAnnotation) -> String {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                let base_name = path
                    .iter()
                    .map(|part| part.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                if let Some(generic_args) = generic_args {
                    let args = generic_args
                        .args
                        .iter()
                        .map(Self::format_type_annotation_static)
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", base_name, args)
                } else {
                    base_name
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                let type_list = types
                    .iter()
                    .map(Self::format_type_annotation_static)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", type_list)
            }
            outrun_parser::TypeAnnotation::Function { .. } => {
                "Function".to_string() // Simplified for now
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
            outrun_parser::TypeAnnotation::Simple {
                path,
                generic_args,
                span,
            } => {
                // Join the path parts to create the base type name
                let base_type_name = path
                    .iter()
                    .map(|part| part.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this is Self
                if base_type_name == "Self" {
                    return Ok(self_type);
                }

                // For simple types without generic arguments, check if it's a generic parameter
                if generic_args.is_none() {
                    // First, check if this is a generic parameter in the passed parameters
                    for (param_name, param_type_id) in generic_params {
                        if param_name == &base_type_name {
                            return Ok(*param_type_id);
                        }
                    }

                    // Also check if this is a generic parameter in the current scope
                    if let Some(type_id) = context.lookup_generic_param(&base_type_name) {
                        return Ok(type_id);
                    }
                }

                // Handle generic types properly
                if let Some(generic_args) = generic_args {
                    // For generic types, first check if the base type exists
                    let qualified_base_name = match base_type_name.as_str() {
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
                        _ => &base_type_name,
                    };

                    // Check if the base type exists
                    let base_type_id = context.interner.intern_type(qualified_base_name);
                    if context.get_concrete_type(base_type_id).is_none() {
                        return Err(TypeError::UndefinedType {
                            name: base_type_name,
                            span: crate::error::span_to_source_span(*span),
                        });
                    }

                    // Validate that all generic arguments are valid types in this context
                    for arg in &generic_args.args {
                        Self::resolve_type_annotation_with_self(
                            context,
                            arg,
                            generic_params,
                            self_type,
                        )?;
                    }

                    // For generic types, we intern the full type name for tracking
                    let args = generic_args
                        .args
                        .iter()
                        .map(Self::format_type_annotation_static)
                        .collect::<Vec<_>>()
                        .join(", ");
                    let full_type_name = format!("{}<{}>", qualified_base_name, args);
                    let type_id = context.interner.intern_type(&full_type_name);
                    Ok(type_id)
                } else {
                    // Non-generic type - use original logic
                    let qualified_type_name = match base_type_name.as_str() {
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
                        _ => &base_type_name,
                    };

                    let type_id = context.interner.intern_type(qualified_type_name);

                    if context.get_concrete_type(type_id).is_some() {
                        Ok(type_id)
                    } else {
                        Err(TypeError::UndefinedType {
                            name: base_type_name,
                            span: crate::error::span_to_source_span(*span),
                        })
                    }
                }
            }
            outrun_parser::TypeAnnotation::Tuple { span, .. } => {
                // TODO: Handle tuple types properly
                Err(TypeError::UnimplementedFeature {
                    feature: "Tuple type annotations in trait definitions".to_string(),
                    span: crate::error::span_to_source_span(*span),
                })
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                span: _,
            } => {
                // Handle function type: Function<(param1: Type1, param2: Type2) -> ReturnType>
                let mut param_type_names = Vec::new();

                // Process each parameter type
                for param in params {
                    let param_type_id = Self::resolve_type_annotation_with_self(
                        context,
                        &param.type_annotation,
                        generic_params,
                        self_type,
                    )?;
                    let param_type_name =
                        context.get_type_name(param_type_id).ok_or_else(|| {
                            TypeError::internal("Function parameter type not found".to_string())
                        })?;
                    param_type_names.push(format!("{}: {}", param.name.name, param_type_name));
                }

                // Process return type
                let return_type_id = Self::resolve_type_annotation_with_self(
                    context,
                    return_type,
                    generic_params,
                    self_type,
                )?;
                let return_type_name = context.get_type_name(return_type_id).ok_or_else(|| {
                    TypeError::internal("Function return type not found".to_string())
                })?;

                // Create function type name: Function<(param1: Type1, param2: Type2) -> ReturnType>
                let function_type_name = format!(
                    "Function<({}) -> {}>",
                    param_type_names.join(", "),
                    return_type_name
                );

                // Intern the function type
                let function_type_id = context.interner.intern_type(&function_type_name);
                Ok(function_type_id)
            }
        }
    }

    /// Validate that the number of generic arguments matches the expected arity for a type
    fn validate_generic_arity(
        context: &mut TypeContext,
        type_name: &str,
        generic_args: &[outrun_parser::TypeAnnotation],
        span: &outrun_parser::Span,
    ) -> Result<(), TypeError> {
        let provided_count = generic_args.len();

        // Get expected arity for built-in types
        let expected_count = match type_name {
            // Built-in types with known arity
            "Outrun.Core.List" => 1,              // List<T>
            "Outrun.Core.Option" => 1,            // Option<T>
            "Outrun.Core.Map" => 2,               // Map<K, V>
            "Outrun.Core.Result" => 2,            // Result<T, E>
            "Outrun.Core.Tuple" => return Ok(()), // Tuple can have any arity

            // Primitive types should have no generics
            "Outrun.Core.Boolean"
            | "Outrun.Core.Integer64"
            | "Outrun.Core.Float64"
            | "Outrun.Core.String"
            | "Outrun.Core.Unit"
            | "Outrun.Core.Atom" => 0,

            // For user-defined types, try to look up trait definitions
            _ => {
                // First try to find it as a trait
                if let Some(trait_id) = context.interner.get_trait(type_name) {
                    if let Some(trait_def) = context.trait_registry.get_trait(trait_id) {
                        return Self::validate_trait_generic_arity(
                            trait_def,
                            provided_count,
                            type_name,
                            span,
                        );
                    }
                }

                // For user-defined types (structs), check the struct registry
                if let Some(struct_info) = context.get_struct_definition(type_name) {
                    let expected_count = struct_info.generic_param_count;
                    if provided_count != expected_count {
                        return Err(TypeError::GenericArityMismatch {
                            type_name: type_name.to_string(),
                            expected: expected_count,
                            found: provided_count,
                            span: crate::error::span_to_source_span(*span),
                        });
                    }
                }

                // If struct not found in registry, assume it's valid (will be caught later if undefined)
                return Ok(());
            }
        };

        if provided_count != expected_count {
            return Err(TypeError::GenericArityMismatch {
                type_name: type_name.to_string(),
                expected: expected_count,
                found: provided_count,
                span: crate::error::span_to_source_span(*span),
            });
        }

        Ok(())
    }

    /// Validate trait-specific generic arity
    fn validate_trait_generic_arity(
        trait_def: &crate::types::traits::TraitDefinition,
        provided_count: usize,
        type_name: &str,
        span: &outrun_parser::Span,
    ) -> Result<(), TypeError> {
        let expected_count = trait_def.generic_params.len();

        if provided_count != expected_count {
            return Err(TypeError::GenericArityMismatch {
                type_name: type_name.to_string(),
                expected: expected_count,
                found: provided_count,
                span: crate::error::span_to_source_span(*span),
            });
        }

        Ok(())
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
