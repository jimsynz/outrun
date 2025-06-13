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
use crate::types::TypeId;
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
    String(String),
    Boolean(bool),
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
    fn check_items(&mut self, _program: &Program) -> Result<Vec<TypedItem>, Vec<TypeError>> {
        // TODO: Implement item type checking
        // - Type check function definitions
        // - Type check constant definitions
        // - Type check expressions
        Ok(Vec::new())
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
