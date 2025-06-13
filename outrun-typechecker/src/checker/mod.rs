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
    // TODO: Add all expression kinds with type information
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
                outrun_parser::ItemKind::LetBinding(_) => {
                    // TODO: Implement let binding checking
                    errors.push(TypeError::UnimplementedFeature {
                        feature: "Let binding type checking".to_string(),
                        span: crate::error::span_to_source_span(item.span),
                    });
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
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
