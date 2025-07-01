//! Visitor trait system for traversing both parser and typed ASTs
//!
//! This module provides a unified visitor pattern that works across both
//! the parser AST (from outrun-parser) and the typed AST (from outrun-typechecker).
//! This enables generic traversal algorithms like type checking, linting,
//! and code transformation.

use crate::checker::{TypedExpression, TypedExpressionKind, TypedItem, TypedItemKind};
use crate::error::TypeError;
use outrun_parser::{
    Argument, BinaryOperation, Expression, ExpressionKind, FunctionCall, FunctionDefinition,
    FunctionPath, Identifier, ImplBlock, Item, ItemKind, LetBinding, Literal, Pattern, Program,
    Span, Statement, StatementKind, StructDefinition, TraitDefinition, TypeIdentifier,
    UnaryOperation,
};

/// Result type for visitor methods that can fail
pub type VisitorResult<T = ()> = Result<T, TypeError>;

/// Generic visitor trait that can traverse both parser and typed ASTs
///
/// This trait provides default implementations that perform depth-first traversal.
/// Implementors can override specific methods to add custom behaviour at particular
/// AST nodes while maintaining the traversal structure.
pub trait Visitor<T>: Sized {
    /// Visit a program (top-level entry point)
    fn visit_program(&mut self, program: &Program) -> VisitorResult {
        walk_program(self, program)
    }

    /// Visit an item
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        walk_item(self, item)
    }

    /// Visit an expression
    fn visit_expression(&mut self, expr: &Expression) -> VisitorResult {
        walk_expression(self, expr)
    }

    /// Visit a statement
    fn visit_statement(&mut self, stmt: &Statement) -> VisitorResult {
        walk_statement(self, stmt)
    }

    /// Visit a function definition
    fn visit_function_definition(&mut self, func: &FunctionDefinition) -> VisitorResult {
        walk_function_definition(self, func)
    }

    /// Visit a function call
    fn visit_function_call(&mut self, call: &FunctionCall) -> VisitorResult {
        walk_function_call(self, call)
    }

    /// Visit a binary operation
    fn visit_binary_operation(&mut self, op: &BinaryOperation) -> VisitorResult {
        walk_binary_operation(self, op)
    }

    /// Visit a unary operation
    fn visit_unary_operation(&mut self, op: &UnaryOperation) -> VisitorResult {
        walk_unary_operation(self, op)
    }

    /// Visit an identifier
    fn visit_identifier(&mut self, _id: &Identifier) -> VisitorResult {
        // Default: no traversal needed for leaf nodes
        Ok(())
    }

    /// Visit a type identifier
    fn visit_type_identifier(&mut self, _id: &TypeIdentifier) -> VisitorResult {
        // Default: no traversal needed for leaf nodes
        Ok(())
    }

    /// Visit a struct definition
    fn visit_struct_definition(&mut self, _struct_def: &StructDefinition) -> VisitorResult {
        // Default: no traversal needed
        Ok(())
    }

    /// Visit a trait definition
    fn visit_trait_definition(&mut self, _trait_def: &TraitDefinition) -> VisitorResult {
        // Default: no traversal needed
        Ok(())
    }

    /// Visit an impl block
    fn visit_impl_block(&mut self, impl_block: &ImplBlock) -> VisitorResult {
        walk_impl_block(self, impl_block)
    }

    /// Visit literal values (integers, strings, booleans, etc.)
    fn visit_literal(&mut self, _kind: &ExpressionKind, _span: &Span) -> VisitorResult {
        // Default: no traversal needed for leaf nodes
        Ok(())
    }

    /// Visit a let binding
    fn visit_let_binding(&mut self, let_binding: &LetBinding) -> VisitorResult {
        walk_let_binding(self, let_binding)
    }

    /// Visit a pattern
    fn visit_pattern(&mut self, pattern: &Pattern) -> VisitorResult {
        walk_pattern(self, pattern)
    }
}

/// Visitor trait specifically for typed ASTs
///
/// This trait works with the typed AST produced by the type checker.
/// It provides additional type information that can be used for
/// more sophisticated analysis.
pub trait TypedVisitor<T>: Sized {
    /// Visit a typed item
    fn visit_typed_item(&mut self, item: &TypedItem) -> VisitorResult {
        walk_typed_item(self, item)
    }

    /// Visit a typed expression
    fn visit_typed_expression(&mut self, expr: &TypedExpression) -> VisitorResult {
        walk_typed_expression(self, expr)
    }

    /// Visit typed literals with type information
    fn visit_typed_literal(
        &mut self,
        _kind: &TypedExpressionKind,
        _structured_type: &Option<crate::unification::StructuredType>,
        _span: &Span,
    ) -> VisitorResult {
        // Default: no traversal needed for leaf nodes
        Ok(())
    }
}

/// Default traversal functions for parser AST
///
/// Walk a program by visiting all items
pub fn walk_program<V: Visitor<T>, T>(visitor: &mut V, program: &Program) -> VisitorResult {
    for item in &program.items {
        visitor.visit_item(item)?;
    }
    Ok(())
}

/// Walk an item by dispatching to the appropriate visitor method
pub fn walk_item<V: Visitor<T>, T>(visitor: &mut V, item: &Item) -> VisitorResult {
    match &item.kind {
        ItemKind::Expression(expr) => visitor.visit_expression(expr),
        ItemKind::FunctionDefinition(func) => visitor.visit_function_definition(func),
        ItemKind::Identifier(id) => visitor.visit_identifier(id),
        ItemKind::TypeIdentifier(id) => visitor.visit_type_identifier(id),
        // Literals
        ItemKind::BooleanLiteral(_)
        | ItemKind::IntegerLiteral(_)
        | ItemKind::FloatLiteral(_)
        | ItemKind::StringLiteral(_)
        | ItemKind::AtomLiteral(_)
        | ItemKind::SigilLiteral(_)
        | ItemKind::ListLiteral(_)
        | ItemKind::MapLiteral(_)
        | ItemKind::TupleLiteral(_) => {
            visitor.visit_literal(&item_kind_to_expression_kind(&item.kind), &item.span)
        }
        // Complex item types
        ItemKind::ImplBlock(impl_block) => visitor.visit_impl_block(impl_block),
        ItemKind::StructDefinition(struct_def) => visitor.visit_struct_definition(struct_def),
        ItemKind::TraitDefinition(trait_def) => visitor.visit_trait_definition(trait_def),
        ItemKind::LetBinding(let_binding) => visitor.visit_let_binding(let_binding),
        // Other items that we don't handle yet
        _ => Ok(()),
    }
}

/// Walk an expression by dispatching based on expression kind
pub fn walk_expression<V: Visitor<T>, T>(visitor: &mut V, expr: &Expression) -> VisitorResult {
    match &expr.kind {
        ExpressionKind::BinaryOp(op) => visitor.visit_binary_operation(op),
        ExpressionKind::UnaryOp(op) => visitor.visit_unary_operation(op),
        ExpressionKind::FunctionCall(call) => visitor.visit_function_call(call),
        ExpressionKind::Identifier(id) => visitor.visit_identifier(id),
        ExpressionKind::TypeIdentifier(id) => visitor.visit_type_identifier(id),
        // Literals
        ExpressionKind::Boolean(_)
        | ExpressionKind::Integer(_)
        | ExpressionKind::Float(_)
        | ExpressionKind::String(_)
        | ExpressionKind::Atom(_)
        | ExpressionKind::Sigil(_)
        | ExpressionKind::List(_)
        | ExpressionKind::Map(_)
        | ExpressionKind::Tuple(_)
        | ExpressionKind::Struct(_) => visitor.visit_literal(&expr.kind, &expr.span),
        // Other expressions that we don't handle yet
        _ => Ok(()),
    }
}

/// Walk a statement by visiting its content
pub fn walk_statement<V: Visitor<T>, T>(visitor: &mut V, stmt: &Statement) -> VisitorResult {
    match &stmt.kind {
        StatementKind::Expression(expr) => visitor.visit_expression(expr),
        StatementKind::LetBinding(let_binding) => visitor.visit_let_binding(let_binding),
    }
}

/// Walk a function definition by visiting its body
pub fn walk_function_definition<V: Visitor<T>, T>(
    visitor: &mut V,
    func: &FunctionDefinition,
) -> VisitorResult {
    // Visit the function body
    for stmt in &func.body.statements {
        visitor.visit_statement(stmt)?;
    }
    Ok(())
}

/// Walk a function call by visiting all arguments
pub fn walk_function_call<V: Visitor<T>, T>(visitor: &mut V, call: &FunctionCall) -> VisitorResult {
    // Visit function path if it's an expression
    if let FunctionPath::Expression { expression } = &call.path {
        visitor.visit_expression(expression)?;
    }

    // Visit all arguments
    for arg in &call.arguments {
        match arg {
            Argument::Named { expression, .. } => visitor.visit_expression(expression)?,
            Argument::Spread { expression, .. } => visitor.visit_expression(expression)?,
        }
    }
    Ok(())
}

/// Walk a binary operation by visiting left and right operands
pub fn walk_binary_operation<V: Visitor<T>, T>(
    visitor: &mut V,
    op: &BinaryOperation,
) -> VisitorResult {
    visitor.visit_expression(&op.left)?;
    visitor.visit_expression(&op.right)?;
    Ok(())
}

/// Walk a unary operation by visiting the operand
pub fn walk_unary_operation<V: Visitor<T>, T>(
    visitor: &mut V,
    op: &UnaryOperation,
) -> VisitorResult {
    visitor.visit_expression(&op.operand)?;
    Ok(())
}

/// Walk an impl block by visiting all function definitions
pub fn walk_impl_block<V: Visitor<T>, T>(visitor: &mut V, impl_block: &ImplBlock) -> VisitorResult {
    // Visit all functions in the impl block
    for func in &impl_block.functions {
        visitor.visit_function_definition(func)?;
    }
    Ok(())
}

/// Walk a let binding by visiting the pattern and expression
pub fn walk_let_binding<V: Visitor<T>, T>(
    visitor: &mut V,
    let_binding: &LetBinding,
) -> VisitorResult {
    // Visit the pattern first (for variable declarations)
    visitor.visit_pattern(&let_binding.pattern)?;

    // Then visit the initializer expression
    visitor.visit_expression(&let_binding.expression)?;

    Ok(())
}

/// Walk a pattern by dispatching to appropriate visitor methods
pub fn walk_pattern<V: Visitor<T>, T>(visitor: &mut V, pattern: &Pattern) -> VisitorResult {
    match pattern {
        Pattern::Identifier(id) => visitor.visit_identifier(id),
        Pattern::Literal(literal_pattern) => {
            // Visit the literal within the pattern
            let expr_kind = literal_to_expression_kind(&literal_pattern.literal);
            visitor.visit_literal(&expr_kind, &literal_pattern.span)
        }
        Pattern::Tuple(tuple_pattern) => {
            // Visit all elements in the tuple pattern
            for element in &tuple_pattern.elements {
                visitor.visit_pattern(element)?;
            }
            Ok(())
        }
        Pattern::Struct(struct_pattern) => {
            // Visit the struct type path
            for type_id in &struct_pattern.type_path {
                visitor.visit_type_identifier(type_id)?;
            }

            // Visit all field patterns
            for field in &struct_pattern.fields {
                // Visit the field name
                visitor.visit_identifier(&field.name)?;

                // Visit the field pattern if present
                if let Some(pattern) = &field.pattern {
                    visitor.visit_pattern(pattern)?;
                }
            }

            Ok(())
        }
        Pattern::List(list_pattern) => {
            // Visit all elements in the list pattern
            for element in &list_pattern.elements {
                visitor.visit_pattern(element)?;
            }

            // Visit the rest pattern if present (it's an identifier, not a pattern)
            if let Some(rest) = &list_pattern.rest {
                visitor.visit_identifier(rest)?;
            }

            Ok(())
        }
    }
}

/// Default traversal functions for typed AST
///
/// Walk a typed item by dispatching to the appropriate visitor method
pub fn walk_typed_item<V: TypedVisitor<T>, T>(visitor: &mut V, item: &TypedItem) -> VisitorResult {
    match &item.kind {
        TypedItemKind::Expression(expr) => visitor.visit_typed_expression(expr),
        TypedItemKind::FunctionDefinition(_func_def) => Ok(()), // TODO: Add function definition visiting
        TypedItemKind::StructDefinition(_struct_def) => Ok(()), // TODO: Add struct definition visiting
        TypedItemKind::TraitDefinition(_trait_def) => Ok(()), // TODO: Add trait definition visiting
        TypedItemKind::ImplBlock(_impl_block) => Ok(()),      // TODO: Add impl block visiting
        TypedItemKind::ConstDefinition(_const_def) => Ok(()), // TODO: Add const definition visiting
        TypedItemKind::LetBinding(_let_binding) => Ok(()),    // TODO: Add let binding visiting
        TypedItemKind::MacroDefinition(_macro_def) => Ok(()), // TODO: Add macro definition visiting
        TypedItemKind::Placeholder(_) => Ok(()),              // No-op for placeholders
    }
}

/// Walk a typed expression by dispatching based on expression kind
pub fn walk_typed_expression<V: TypedVisitor<T>, T>(
    visitor: &mut V,
    expr: &TypedExpression,
) -> VisitorResult {
    // With our simplified typed expressions, just visit as literal
    visitor.visit_typed_literal(&expr.kind, &expr.structured_type, &expr.span)
}

/// Helper function to convert ItemKind to ExpressionKind for literal handling
fn item_kind_to_expression_kind(item_kind: &ItemKind) -> ExpressionKind {
    match item_kind {
        ItemKind::BooleanLiteral(lit) => ExpressionKind::Boolean(lit.clone()),
        ItemKind::IntegerLiteral(lit) => ExpressionKind::Integer(lit.clone()),
        ItemKind::FloatLiteral(lit) => ExpressionKind::Float(lit.clone()),
        ItemKind::StringLiteral(lit) => ExpressionKind::String(lit.clone()),
        ItemKind::AtomLiteral(lit) => ExpressionKind::Atom(lit.clone()),
        ItemKind::SigilLiteral(lit) => ExpressionKind::Sigil(lit.clone()),
        ItemKind::ListLiteral(lit) => ExpressionKind::List(lit.clone()),
        ItemKind::MapLiteral(lit) => ExpressionKind::Map(lit.clone()),
        ItemKind::TupleLiteral(lit) => ExpressionKind::Tuple(lit.clone()),
        _ => panic!("Cannot convert non-literal ItemKind to ExpressionKind"),
    }
}

/// Helper function to convert Literal to ExpressionKind for pattern literal handling
fn literal_to_expression_kind(literal: &Literal) -> ExpressionKind {
    match literal {
        Literal::Boolean(lit) => ExpressionKind::Boolean(lit.clone()),
        Literal::Integer(lit) => ExpressionKind::Integer(lit.clone()),
        Literal::Float(lit) => ExpressionKind::Float(lit.clone()),
        Literal::String(lit) => ExpressionKind::String(lit.clone()),
        Literal::Atom(lit) => ExpressionKind::Atom(lit.clone()),
    }
}

/// Example visitor implementation for collecting identifiers
#[derive(Default)]
pub struct IdentifierCollector {
    pub identifiers: Vec<String>,
}

impl<T> Visitor<T> for IdentifierCollector {
    fn visit_identifier(&mut self, id: &Identifier) -> VisitorResult {
        self.identifiers.push(id.name.clone());
        Ok(())
    }

    fn visit_type_identifier(&mut self, id: &TypeIdentifier) -> VisitorResult {
        self.identifiers.push(id.name.clone());
        Ok(())
    }
}

/// Example visitor implementation for counting expressions by type
#[derive(Default)]
pub struct ExpressionCounter {
    pub binary_ops: usize,
    pub function_calls: usize,
    pub literals: usize,
    pub let_bindings: usize,
}

impl<T> Visitor<T> for ExpressionCounter {
    fn visit_binary_operation(&mut self, op: &BinaryOperation) -> VisitorResult {
        self.binary_ops += 1;
        walk_binary_operation::<Self, ()>(self, op)
    }

    fn visit_function_call(&mut self, call: &FunctionCall) -> VisitorResult {
        self.function_calls += 1;
        walk_function_call::<Self, ()>(self, call)
    }

    fn visit_literal(&mut self, _kind: &ExpressionKind, _span: &Span) -> VisitorResult {
        self.literals += 1;
        Ok(())
    }

    fn visit_let_binding(&mut self, let_binding: &LetBinding) -> VisitorResult {
        self.let_bindings += 1;
        walk_let_binding::<Self, ()>(self, let_binding)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::{
        BooleanLiteral, Expression, ExpressionKind, Identifier, IntegerFormat, IntegerLiteral,
        Item, ItemKind, Program, Span,
    };

    fn create_test_span() -> Span {
        Span::new(0, 10)
    }

    #[test]
    fn test_identifier_collector() {
        let program = Program {
            items: vec![
                Item {
                    kind: ItemKind::Identifier(Identifier {
                        name: "test_var".to_string(),
                        span: create_test_span(),
                    }),
                    span: create_test_span(),
                },
                Item {
                    kind: ItemKind::TypeIdentifier(TypeIdentifier {
                        name: "TestType".to_string(),
                        span: create_test_span(),
                    }),
                    span: create_test_span(),
                },
            ],
            debug_info: Default::default(),
            span: create_test_span(),
        };

        let mut collector = IdentifierCollector::default();
        <IdentifierCollector as Visitor<()>>::visit_program(&mut collector, &program).unwrap();

        assert_eq!(collector.identifiers.len(), 2);
        assert!(collector.identifiers.contains(&"test_var".to_string()));
        assert!(collector.identifiers.contains(&"TestType".to_string()));
    }

    #[test]
    fn test_expression_counter() {
        let left_expr = Expression {
            kind: ExpressionKind::Integer(IntegerLiteral {
                value: 1,
                format: IntegerFormat::Decimal,
                raw_text: "1".to_string(),
                span: create_test_span(),
            }),
            span: create_test_span(),
        };

        let right_expr = Expression {
            kind: ExpressionKind::Boolean(BooleanLiteral {
                value: true,
                span: create_test_span(),
            }),
            span: create_test_span(),
        };

        let binary_op = BinaryOperation {
            left: Box::new(left_expr),
            operator: outrun_parser::BinaryOperator::Add,
            right: Box::new(right_expr),
            span: create_test_span(),
        };

        let program = Program {
            items: vec![Item {
                kind: ItemKind::Expression(Expression {
                    kind: ExpressionKind::BinaryOp(binary_op),
                    span: create_test_span(),
                }),
                span: create_test_span(),
            }],
            debug_info: Default::default(),
            span: create_test_span(),
        };

        let mut counter = ExpressionCounter::default();
        <ExpressionCounter as Visitor<()>>::visit_program(&mut counter, &program).unwrap();

        assert_eq!(counter.binary_ops, 1);
        assert_eq!(counter.literals, 2); // integer and boolean literals
        assert_eq!(counter.function_calls, 0);
    }
}
