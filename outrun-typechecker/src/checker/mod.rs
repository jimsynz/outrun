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
#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub kind: TypedExpressionKind,
    pub type_id: TypeId,
    pub span: Span,
}

/// Kinds of typed expressions (stub for now)
#[derive(Debug, Clone)]
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
    // TODO: Add all expression kinds with type information
}

/// Typed block with statements
#[derive(Debug, Clone)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub result_type: TypeId, // Type of the block's result
    pub span: Span,
}

/// Typed statement
#[derive(Debug, Clone)]
pub struct TypedStatement {
    pub kind: TypedStatementKind,
    pub span: Span,
}

/// Kinds of typed statements
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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

    /// Type check a complete program
    pub fn check_program(&mut self, program: &Program) -> Result<TypedProgram, Vec<TypeError>> {
        // Phase 1: Register all type definitions and traits
        self.register_types(program)?;

        // Phase 2: Validate trait implementations
        self.validate_implementations()?;

        // Phase 3: Build dispatch tables
        self.build_dispatch_tables()?;

        // Phase 4: Type check all items
        let typed_items = self.check_items(program)?;

        // Phase 5: Build final typed program
        Ok(TypedProgram {
            items: typed_items,
            dispatch_table: self.context.dispatch_table.clone(),
            type_context: self.context.clone(),
        })
    }

    /// Register all type definitions and traits from the program
    fn register_types(&mut self, _program: &Program) -> Result<(), Vec<TypeError>> {
        // TODO: Implement type registration
        // - Process struct definitions
        // - Process trait definitions
        // - Register built-in types
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
        // TODO: Implement dispatch table construction
        // - Build (TraitId, TypeId) -> Function mappings
        // - Build operator dispatch tables
        // - Build static function lookup tables
        Ok(())
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
                outrun_parser::ItemKind::StructDefinition(_) => {
                    // TODO: Implement struct definition checking
                    errors.push(TypeError::UnimplementedFeature {
                        feature: "Struct definition type checking".to_string(),
                        span: crate::error::span_to_source_span(item.span),
                    });
                }
                outrun_parser::ItemKind::TraitDefinition(_) => {
                    // TODO: Implement trait definition checking
                    errors.push(TypeError::UnimplementedFeature {
                        feature: "Trait definition type checking".to_string(),
                        span: crate::error::span_to_source_span(item.span),
                    });
                }
                outrun_parser::ItemKind::ImplBlock(_) => {
                    // TODO: Implement impl block checking
                    errors.push(TypeError::UnimplementedFeature {
                        feature: "Impl block type checking".to_string(),
                        span: crate::error::span_to_source_span(item.span),
                    });
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
        // Create a new scope for function parameters
        self.context.push_scope(true);

        // Register function parameters as variables in the new scope
        let mut typed_params = Vec::new();
        for param in &func.parameters {
            // TODO: Validate parameter type exists
            let param_type = self.context.interner.intern_type("Unknown"); // Stub for now
            typed_params.push((param.name.name.clone(), param_type));

            // Register parameter as variable
            let variable = crate::checker::context::Variable {
                name: param.name.name.clone(),
                type_id: param_type,
                is_mutable: false,
                span: param.span,
            };
            self.context.register_variable(variable)?;
        }

        // Type check function body (blocks are not yet supported, so we'll create a stub)
        // TODO: Implement block type checking
        let typed_body = TypedExpression {
            kind: TypedExpressionKind::Integer(0), // Stub
            type_id: self.context.interner.intern_type("Unknown"),
            span: func.body.span,
        };

        // TODO: Validate return type matches body type
        let return_type = self.context.interner.intern_type("Unknown"); // Stub for now

        // Pop function scope
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
    fn check_let_binding(
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
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::{Identifier, Pattern, Span, TypeAnnotation, TypeIdentifier};

    #[test]
    fn test_type_checker_creation() {
        let checker = TypeChecker::new();
        assert!(checker.context.interner.get_type("Integer").is_none());
    }

    #[test]
    fn test_typed_program_structure() {
        let program = TypedProgram {
            items: Vec::new(),
            dispatch_table: DispatchTable::new(),
            type_context: TypeContext::new(),
        };

        assert_eq!(program.items.len(), 0);
    }

    #[test]
    fn test_let_binding_type_inference() {
        let mut checker = TypeChecker::new();

        // Create a simple let binding: let x = 42
        let let_binding = outrun_parser::LetBinding {
            pattern: Pattern::Identifier(Identifier {
                name: "x".to_string(),
                span: Span::new(0, 1),
            }),
            type_annotation: None, // Type inference
            expression: outrun_parser::Expression {
                kind: outrun_parser::ExpressionKind::Integer(outrun_parser::IntegerLiteral {
                    value: 42,
                    format: outrun_parser::IntegerFormat::Decimal,
                    span: Span::new(6, 8),
                }),
                span: Span::new(6, 8),
            },
            span: Span::new(0, 8),
        };

        let result = checker.check_let_binding(&let_binding);
        assert!(result.is_ok());

        let typed_item = result.unwrap();
        match typed_item.kind {
            TypedItemKind::LetBinding(let_binding) => {
                // Should have inferred Integer64 type
                let type_name = checker.context.get_type_name(let_binding.type_id);
                assert_eq!(type_name, Some("Outrun.Core.Integer64"));
            }
            _ => panic!("Expected let binding"),
        }

        // Variable should be registered in scope
        let variable = checker.context.lookup_variable("x");
        assert!(variable.is_some());
        assert_eq!(variable.unwrap().name, "x");
    }

    #[test]
    fn test_let_binding_type_annotation() {
        let mut checker = TypeChecker::new();

        // Create a let binding with explicit type: let x: Outrun.Core.Integer64 = 42
        let let_binding = outrun_parser::LetBinding {
            pattern: Pattern::Identifier(Identifier {
                name: "x".to_string(),
                span: Span::new(0, 1),
            }),
            type_annotation: Some(TypeAnnotation::Simple {
                path: vec![TypeIdentifier {
                    name: "Outrun.Core.Integer64".to_string(),
                    span: Span::new(3, 25),
                }],
                generic_args: None,
                span: Span::new(3, 25),
            }),
            expression: outrun_parser::Expression {
                kind: outrun_parser::ExpressionKind::Integer(outrun_parser::IntegerLiteral {
                    value: 42,
                    format: outrun_parser::IntegerFormat::Decimal,
                    span: Span::new(28, 30),
                }),
                span: Span::new(28, 30),
            },
            span: Span::new(0, 30),
        };

        let result = checker.check_let_binding(&let_binding);
        assert!(result.is_ok());

        let typed_item = result.unwrap();
        match typed_item.kind {
            TypedItemKind::LetBinding(let_binding) => {
                // Should use the explicit type annotation
                let type_name = checker.context.get_type_name(let_binding.type_id);
                assert_eq!(type_name, Some("Outrun.Core.Integer64"));
            }
            _ => panic!("Expected let binding"),
        }
    }

    #[test]
    fn test_let_binding_type_mismatch() {
        let mut checker = TypeChecker::new();

        // Create a let binding with mismatched types: let x: Outrun.Core.String = 42
        let let_binding = outrun_parser::LetBinding {
            pattern: Pattern::Identifier(Identifier {
                name: "x".to_string(),
                span: Span::new(0, 1),
            }),
            type_annotation: Some(TypeAnnotation::Simple {
                path: vec![TypeIdentifier {
                    name: "Outrun.Core.String".to_string(),
                    span: Span::new(3, 22),
                }],
                generic_args: None,
                span: Span::new(3, 22),
            }),
            expression: outrun_parser::Expression {
                kind: outrun_parser::ExpressionKind::Integer(outrun_parser::IntegerLiteral {
                    value: 42,
                    format: outrun_parser::IntegerFormat::Decimal,
                    span: Span::new(25, 27),
                }),
                span: Span::new(25, 27),
            },
            span: Span::new(0, 27),
        };

        let result = checker.check_let_binding(&let_binding);
        assert!(result.is_err());

        // Should be a type mismatch error
        match result.unwrap_err() {
            TypeError::TypeMismatch {
                expected, found, ..
            } => {
                assert_eq!(expected, "Outrun.Core.String");
                assert_eq!(found, "Outrun.Core.Integer64");
            }
            _ => panic!("Expected TypeMismatch error"),
        }
    }
}
