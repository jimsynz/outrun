//! TypedASTBuilder visitor for creating comprehensive typed AST
//!
//! This visitor runs after TypeCheckingVisitor has completed type validation
//! and uses the type checking results to build a complete typed AST with
//! resolved type information for all nodes.

use crate::checker::{
    DispatchMethod, TypedArgument, TypedExpression, TypedExpressionKind, TypedFunctionPath,
    TypedItem, TypedItemKind, TypedProgram,
};
#[allow(unused_imports)]
use crate::types::TypeId;
use crate::multi_program_compiler::{FunctionRegistry, ProgramCollection};
use crate::unification::{StructuredType, UnificationContext};
use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{
    Argument, Expression, ExpressionKind, FunctionCall, FunctionPath, Item, ItemKind, Program, Span,
};
use std::collections::HashMap;

/// Visitor for building comprehensive typed AST (Phase 6)
#[derive(Debug)]
pub struct TypedASTBuilder {
    /// Type checking context with resolved types
    pub context: UnificationContext,
    /// Function registry for dispatch resolution
    pub function_registry: FunctionRegistry,
    /// Collection of errors encountered during AST building
    pub errors: Vec<crate::error::TypeError>,
    /// Cache of resolved expression types (span -> type)
    #[allow(dead_code)]
    expression_types: HashMap<Span, StructuredType>,
    /// Built typed programs (filename -> typed program)
    typed_programs: HashMap<String, TypedProgram>,
}

impl TypedASTBuilder {
    /// Create a new TypedASTBuilder with type checking results
    pub fn new(context: UnificationContext, function_registry: FunctionRegistry) -> Self {
        Self {
            context,
            function_registry,
            errors: Vec::new(),
            expression_types: HashMap::new(),
            typed_programs: HashMap::new(),
        }
    }

    /// Build typed AST for all programs in the collection
    pub fn build_typed_ast(
        &mut self,
        collection: &ProgramCollection,
        compilation_order: &[String],
    ) -> Result<HashMap<String, TypedProgram>, Vec<crate::error::TypeError>> {
        // Build typed AST for each program in dependency order
        for filename in compilation_order {
            if let Some(program) = collection.get_program(filename) {
                match self.build_typed_program(program, filename) {
                    Ok(typed_program) => {
                        self.typed_programs.insert(filename.clone(), typed_program);
                    }
                    Err(error) => {
                        self.errors.push(error);
                    }
                }
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(self.typed_programs.clone())
    }

    /// Build typed AST for a single program
    fn build_typed_program(
        &mut self,
        program: &Program,
        filename: &str,
    ) -> Result<TypedProgram, crate::error::TypeError> {
        // Convert all items to typed items
        let mut typed_items = Vec::new();
        for item in &program.items {
            if let Some(typed_item) = self.convert_item(item) {
                typed_items.push(typed_item);
            }
        }

        let summary = format!(
            "TypedAST for {}: {} items, {} functions",
            filename,
            typed_items.len(),
            self.function_registry.len()
        );

        Ok(TypedProgram {
            items: typed_items,
            type_context: self.context.clone(),
            function_registry: self.function_registry.clone(),
            compilation_order: vec![filename.to_string()], // This program only
            compilation_summary: summary,
        })
    }

    /// Convert a parser Item to TypedItem with resolved types
    fn convert_item(&mut self, item: &Item) -> Option<TypedItem> {
        let kind = match &item.kind {
            ItemKind::Expression(expr) => {
                if let Some(typed_expr) = self.convert_expression(expr) {
                    TypedItemKind::Expression(Box::new(typed_expr))
                } else {
                    TypedItemKind::Placeholder("Failed to convert expression".to_string())
                }
            }
            ItemKind::FunctionDefinition(_) => {
                TypedItemKind::Placeholder("Function definition - TODO".to_string())
            }
            ItemKind::StructDefinition(_) => {
                TypedItemKind::Placeholder("Struct definition - TODO".to_string())
            }
            ItemKind::TraitDefinition(_) => {
                TypedItemKind::Placeholder("Trait definition - TODO".to_string())
            }
            ItemKind::ImplBlock(_) => TypedItemKind::Placeholder("Impl block - TODO".to_string()),
            _ => return None, // Skip other items for now
        };

        Some(TypedItem {
            kind,
            span: item.span,
        })
    }

    /// Convert a parser Expression to TypedExpression with resolved types
    fn convert_expression(&mut self, expr: &Expression) -> Option<TypedExpression> {
        // Try to get resolved type for this expression
        let structured_type = self.get_expression_type(expr);

        let kind = match &expr.kind {
            // Basic literals
            ExpressionKind::Integer(lit) => TypedExpressionKind::Integer(lit.value),
            ExpressionKind::Float(lit) => TypedExpressionKind::Float(lit.value),
            ExpressionKind::String(lit) => {
                // Handle string interpolation
                let content = lit
                    .parts
                    .iter()
                    .map(|part| match part {
                        outrun_parser::StringPart::Text { content, .. } => content.clone(),
                        outrun_parser::StringPart::Interpolation { .. } => "{}".to_string(), // TODO: Handle interpolation
                    })
                    .collect::<String>();
                TypedExpressionKind::String(content)
            }
            ExpressionKind::Boolean(lit) => TypedExpressionKind::Boolean(lit.value),
            ExpressionKind::Atom(lit) => TypedExpressionKind::Atom(lit.name.clone()),
            ExpressionKind::Identifier(id) => TypedExpressionKind::Identifier(id.name.clone()),

            // Function calls (including desugared operations)
            ExpressionKind::FunctionCall(call) => {
                if let Some(typed_call) = self.convert_function_call(call) {
                    typed_call
                } else {
                    TypedExpressionKind::Placeholder("Failed to convert function call".to_string())
                }
            }

            // Field access
            ExpressionKind::FieldAccess(access) => {
                if let Some(typed_object) = self.convert_expression(&access.object) {
                    TypedExpressionKind::FieldAccess {
                        object: Box::new(typed_object),
                        field: access.field.name.clone(),
                        field_type: structured_type.clone(), // Field type should be resolved
                    }
                } else {
                    TypedExpressionKind::Placeholder("Failed to convert field access".to_string())
                }
            }

            // TODO: Add more expression types
            _ => TypedExpressionKind::Placeholder(format!(
                "Expression type not yet implemented: {:?}",
                std::mem::discriminant(&expr.kind)
            )),
        };

        Some(TypedExpression {
            kind,
            structured_type,
            span: expr.span,
        })
    }

    /// Convert a function call with proper dispatch resolution
    fn convert_function_call(&mut self, call: &FunctionCall) -> Option<TypedExpressionKind> {
        // Convert function path
        let function_path = match &call.path {
            FunctionPath::Simple { name } => TypedFunctionPath::Simple {
                name: name.name.clone(),
            },
            FunctionPath::Qualified { module, name } => TypedFunctionPath::Qualified {
                module: module.name.clone(),
                name: name.name.clone(),
            },
            FunctionPath::Expression { expression } => {
                if let Some(typed_expr) = self.convert_expression(expression) {
                    TypedFunctionPath::Expression {
                        expression: Box::new(typed_expr),
                    }
                } else {
                    return None;
                }
            }
        };

        // Convert arguments
        let mut typed_arguments = Vec::new();
        for arg in &call.arguments {
            match arg {
                Argument::Named {
                    name,
                    expression,
                    span,
                    ..
                } => {
                    if let Some(typed_expr) = self.convert_expression(expression) {
                        typed_arguments.push(TypedArgument {
                            name: name.name.clone(),
                            expression: typed_expr.clone(),
                            argument_type: typed_expr.structured_type.clone(),
                            span: *span,
                        });
                    } else {
                        return None;
                    }
                }
                // TODO: Handle spread arguments
                Argument::Spread { .. } => {
                    return None; // Not implemented yet
                }
            }
        }

        // Resolve dispatch method
        let dispatch_method = self.resolve_dispatch_method(&function_path, &typed_arguments);

        Some(TypedExpressionKind::FunctionCall {
            function_path,
            arguments: typed_arguments,
            dispatch_method,
        })
    }

    /// Resolve dispatch method for a function call
    fn resolve_dispatch_method(
        &self,
        function_path: &TypedFunctionPath,
        _arguments: &[TypedArgument],
    ) -> DispatchMethod {
        match function_path {
            TypedFunctionPath::Simple { name } => {
                // TODO: Look up in function registry
                DispatchMethod::Static {
                    function_id: name.clone(),
                }
            }
            TypedFunctionPath::Qualified { module, name } => {
                // Check if this is a trait method call
                if let Some(module_type_id) = self.context.type_interner.get_type(module) {
                    // Try to determine if this is trait vs static dispatch
                    // TODO: Use type checking results to determine proper dispatch
                    DispatchMethod::Trait {
                        trait_name: module.clone(),
                        method_name: name.clone(),
                        impl_type: module_type_id, // Placeholder
                    }
                } else {
                    DispatchMethod::Static {
                        function_id: format!("{}.{}", module, name),
                    }
                }
            }
            TypedFunctionPath::Expression { .. } => {
                // Dynamic function calls
                DispatchMethod::Static {
                    function_id: "dynamic".to_string(),
                }
            }
        }
    }

    /// Get resolved type for an expression (placeholder for now)
    fn get_expression_type(&self, _expr: &Expression) -> Option<StructuredType> {
        // TODO: Use type checking results to get actual resolved types
        // For now, return None since we don't have access to TypeCheckingVisitor results
        None
    }
}

impl<T> Visitor<T> for TypedASTBuilder {
    fn visit_program(&mut self, _program: &Program) -> VisitorResult {
        // Convert program to typed program
        // This is handled by build_typed_program instead
        Ok(())
    }

    fn visit_expression(&mut self, expr: &Expression) -> VisitorResult {
        // Convert expression to typed expression
        if let Some(_typed_expr) = self.convert_expression(expr) {
            // Store or process typed expression as needed
            Ok(())
        } else {
            // Log error but continue processing
            Ok(())
        }
    }
}
