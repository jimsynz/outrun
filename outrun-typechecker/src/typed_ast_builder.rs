//! TypedASTBuilder visitor for creating comprehensive typed AST
//!
//! This visitor runs after TypeCheckingVisitor has completed type validation
//! and uses the type checking results to build a complete typed AST with
//! resolved type information for all nodes.

use crate::checker::{
    DispatchMethod, TypedAnonymousClause, TypedAnonymousFunction, TypedArgument, TypedAsClause,
    TypedBlock, TypedCaseVariant, TypedConstDefinition, TypedExpression, TypedExpressionKind,
    TypedFunctionDefinition, TypedFunctionPath, TypedFunctionTypeParam, TypedGenericContext,
    TypedGenericParam, TypedImplBlock, TypedItem, TypedItemKind, TypedLetBinding,
    TypedMacroDefinition, TypedMapEntry, TypedParameter, TypedProgram, TypedStatement,
    TypedStructDefinition, TypedStructField, TypedStructFieldDefinition, TypedTraitDefinition,
    TypedTraitFunction, TypedTypeAnnotation, TypedTypeAnnotationKind, TypedWhenClause,
};
use crate::multi_program_compiler::{FunctionRegistry, ProgramCollection};
use crate::patterns::{PatternChecker, TypedPattern};
#[allow(unused_imports)]
use crate::types::TypeId;
use crate::unification::{StructuredType, UnificationContext};
use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{
    AnonymousClause, AnonymousFunction, Argument, Block, CaseExpression, CaseResult,
    CaseWhenClause, ConcreteCaseExpression, ConstDefinition, Expression, ExpressionKind,
    FunctionCall, FunctionDefinition, FunctionPath, GenericParam, GenericParams, IfExpression,
    ImplBlock, Item, ItemKind, LetBinding, ListElement, ListLiteral, MapEntry, MapLiteral,
    Parameter, Program, Span, Statement, StatementKind, StructDefinition, StructField,
    StructLiteral, StructLiteralField, TraitCaseClause, TraitCaseExpression, TraitDefinition,
    TraitFunction, TupleLiteral, TypeSpec,
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
    /// Current generic context for type resolution
    generic_context: Option<TypedGenericContext>,
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
            generic_context: None,
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
            ItemKind::FunctionDefinition(func_def) => {
                if let Some(typed_func) = self.convert_function_definition(func_def) {
                    TypedItemKind::FunctionDefinition(typed_func)
                } else {
                    TypedItemKind::Placeholder("Failed to convert function definition".to_string())
                }
            }
            ItemKind::StructDefinition(struct_def) => {
                if let Some(typed_struct) = self.convert_struct_definition(struct_def) {
                    TypedItemKind::StructDefinition(typed_struct)
                } else {
                    TypedItemKind::Placeholder("Failed to convert struct definition".to_string())
                }
            }
            ItemKind::TraitDefinition(trait_def) => {
                if let Some(typed_trait) = self.convert_trait_definition(trait_def) {
                    TypedItemKind::TraitDefinition(typed_trait)
                } else {
                    TypedItemKind::Placeholder("Failed to convert trait definition".to_string())
                }
            }
            ItemKind::ImplBlock(impl_block) => {
                if let Some(typed_impl) = self.convert_impl_block(impl_block) {
                    TypedItemKind::ImplBlock(typed_impl)
                } else {
                    TypedItemKind::Placeholder("Failed to convert impl block".to_string())
                }
            }
            ItemKind::ConstDefinition(const_def) => {
                if let Some(typed_const) = self.convert_const_definition(const_def) {
                    TypedItemKind::ConstDefinition(typed_const)
                } else {
                    TypedItemKind::Placeholder("Failed to convert const definition".to_string())
                }
            }
            ItemKind::LetBinding(let_binding) => {
                if let Some(typed_let) = self.convert_let_binding(let_binding) {
                    TypedItemKind::LetBinding(Box::new(typed_let))
                } else {
                    TypedItemKind::Placeholder("Failed to convert let binding".to_string())
                }
            }
            ItemKind::MacroDefinition(macro_def) => {
                if let Some(typed_macro) = self.convert_macro_definition(macro_def) {
                    TypedItemKind::MacroDefinition(typed_macro)
                } else {
                    TypedItemKind::Placeholder("Failed to convert macro definition".to_string())
                }
            }
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
                // Simple string (interpolation should be desugared in earlier phase)
                if lit
                    .parts
                    .iter()
                    .any(|part| matches!(part, outrun_parser::StringPart::Interpolation { .. }))
                {
                    // This is an error - interpolation should have been desugared
                    TypedExpressionKind::Placeholder(
                        "String interpolation not desugared - this is a compiler bug".to_string(),
                    )
                } else {
                    // Simple string - concatenate all text parts
                    let content = lit
                        .parts
                        .iter()
                        .map(|part| match part {
                            outrun_parser::StringPart::Text { content, .. } => content.clone(),
                            outrun_parser::StringPart::Interpolation { .. } => unreachable!(),
                        })
                        .collect::<String>();
                    TypedExpressionKind::String(content)
                }
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

            // Collection literals
            ExpressionKind::List(list) => {
                if let Some(typed_list) = self.convert_list_literal(list) {
                    typed_list
                } else {
                    TypedExpressionKind::Placeholder("Failed to convert list literal".to_string())
                }
            }

            ExpressionKind::Map(map) => {
                if let Some(typed_map) = self.convert_map_literal(map) {
                    typed_map
                } else {
                    TypedExpressionKind::Placeholder("Failed to convert map literal".to_string())
                }
            }

            ExpressionKind::Tuple(tuple) => {
                if let Some(typed_tuple) = self.convert_tuple_literal(tuple) {
                    typed_tuple
                } else {
                    TypedExpressionKind::Placeholder("Failed to convert tuple literal".to_string())
                }
            }

            ExpressionKind::Struct(struct_lit) => {
                if let Some(typed_struct) = self.convert_struct_literal(struct_lit) {
                    typed_struct
                } else {
                    TypedExpressionKind::Placeholder("Failed to convert struct literal".to_string())
                }
            }

            // Control flow expressions
            ExpressionKind::IfExpression(if_expr) => {
                if let Some(typed_if) = self.convert_if_expression(if_expr) {
                    typed_if
                } else {
                    TypedExpressionKind::Placeholder("Failed to convert if expression".to_string())
                }
            }

            ExpressionKind::CaseExpression(case_expr) => {
                if let Some(typed_case) = self.convert_case_expression(case_expr) {
                    typed_case
                } else {
                    TypedExpressionKind::Placeholder(
                        "Failed to convert case expression".to_string(),
                    )
                }
            }

            // Macro injection
            ExpressionKind::MacroInjection(injection) => {
                if let Some(typed_injection) = self.convert_macro_injection(injection) {
                    typed_injection
                } else {
                    TypedExpressionKind::Placeholder(
                        "Failed to convert macro injection".to_string(),
                    )
                }
            }

            // Function expressions
            ExpressionKind::AnonymousFunction(anon_func) => {
                if let Some(typed_anon) = self.convert_anonymous_function(anon_func) {
                    TypedExpressionKind::AnonymousFunction(typed_anon)
                } else {
                    TypedExpressionKind::Placeholder(
                        "Failed to convert anonymous function".to_string(),
                    )
                }
            }

            // TODO: Add more expression types (patterns, etc.)
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

    /// Convert list literal with element type validation
    fn convert_list_literal(&mut self, list: &ListLiteral) -> Option<TypedExpressionKind> {
        let mut typed_elements = Vec::new();
        let mut element_type: Option<StructuredType> = None;

        // Process all list elements
        for element in &list.elements {
            match element {
                ListElement::Expression(expr) => {
                    if let Some(typed_expr) = self.convert_expression(expr) {
                        // For homogeneous type checking, track the first element's type
                        if element_type.is_none() {
                            element_type = typed_expr.structured_type.clone();
                        }
                        typed_elements.push(typed_expr);
                    } else {
                        // Failed to convert element - create placeholder
                        typed_elements.push(TypedExpression {
                            kind: TypedExpressionKind::Placeholder(
                                "Failed to convert list element".to_string(),
                            ),
                            structured_type: None,
                            span: expr.span,
                        });
                    }
                }
                ListElement::Spread(_identifier) => {
                    // TODO: Handle spread elements in lists
                    typed_elements.push(TypedExpression {
                        kind: TypedExpressionKind::Placeholder(
                            "Spread elements not yet implemented".to_string(),
                        ),
                        structured_type: None,
                        span: list.span,
                    });
                }
            }
        }

        Some(TypedExpressionKind::List {
            elements: typed_elements,
            element_type, // Will be None if list is empty or types are mixed
        })
    }

    /// Convert map literal with key-value type validation
    fn convert_map_literal(&mut self, map: &MapLiteral) -> Option<TypedExpressionKind> {
        let mut typed_entries = Vec::new();
        let mut key_type: Option<StructuredType> = None;
        let mut value_type: Option<StructuredType> = None;

        // Process all map entries
        for entry in &map.entries {
            match entry {
                MapEntry::Assignment { key, value } => {
                    if let (Some(typed_key), Some(typed_value)) =
                        (self.convert_expression(key), self.convert_expression(value))
                    {
                        // Track key and value types for homogeneous checking
                        if key_type.is_none() {
                            key_type = typed_key.structured_type.clone();
                        }
                        if value_type.is_none() {
                            value_type = typed_value.structured_type.clone();
                        }

                        typed_entries.push(TypedMapEntry::Assignment {
                            key: Box::new(typed_key),
                            value: Box::new(typed_value),
                            key_type: key_type.clone(),
                            value_type: value_type.clone(),
                        });
                    } else {
                        return None; // Failed to convert key or value
                    }
                }
                MapEntry::Shorthand { name, value } => {
                    if let Some(typed_value) = self.convert_expression(value) {
                        // Track value type
                        if value_type.is_none() {
                            value_type = typed_value.structured_type.clone();
                        }

                        typed_entries.push(TypedMapEntry::Shorthand {
                            name: name.name.clone(),
                            value: Box::new(typed_value),
                            value_type: value_type.clone(),
                        });
                    } else {
                        return None; // Failed to convert value
                    }
                }
                MapEntry::Spread(identifier) => {
                    // TODO: Handle spread in maps - need to validate spread type
                    typed_entries.push(TypedMapEntry::Spread {
                        identifier: identifier.name.clone(),
                        spread_type: None, // TODO: Resolve spread expression type
                    });
                }
            }
        }

        Some(TypedExpressionKind::Map {
            entries: typed_entries,
            key_type,   // Will be None if map is empty or key types are mixed
            value_type, // Will be None if map is empty or value types are mixed
        })
    }

    /// Convert tuple literal with element type tracking
    fn convert_tuple_literal(&mut self, tuple: &TupleLiteral) -> Option<TypedExpressionKind> {
        let mut typed_elements = Vec::new();

        // Process all tuple elements
        for element in &tuple.elements {
            if let Some(typed_element) = self.convert_expression(element) {
                typed_elements.push(typed_element);
            } else {
                // Failed to convert element - create placeholder
                typed_elements.push(TypedExpression {
                    kind: TypedExpressionKind::Placeholder(
                        "Failed to convert tuple element".to_string(),
                    ),
                    structured_type: None,
                    span: element.span,
                });
            }
        }

        // TODO: Compute composite tuple type from element types
        let tuple_type = None; // Placeholder for now

        Some(TypedExpressionKind::Tuple {
            elements: typed_elements,
            tuple_type,
        })
    }

    /// Convert struct literal with field validation
    fn convert_struct_literal(
        &mut self,
        struct_lit: &StructLiteral,
    ) -> Option<TypedExpressionKind> {
        let mut typed_fields = Vec::new();

        // Convert type path to string representation
        let type_path: Vec<String> = struct_lit
            .type_path
            .iter()
            .map(|type_id| type_id.name.clone())
            .collect();

        // Process all struct fields
        for field in &struct_lit.fields {
            match field {
                StructLiteralField::Assignment { name, value } => {
                    if let Some(typed_value) = self.convert_expression(value) {
                        typed_fields.push(TypedStructField::Assignment {
                            name: name.name.clone(),
                            expression: Box::new(typed_value),
                            field_type: None, // TODO: Resolve from struct definition
                        });
                    } else {
                        return None; // Failed to convert field value
                    }
                }
                StructLiteralField::Shorthand(name) => {
                    typed_fields.push(TypedStructField::Shorthand {
                        name: name.name.clone(),
                        variable_type: None, // TODO: Resolve from variable context
                    });
                }
                StructLiteralField::Spread(identifier) => {
                    typed_fields.push(TypedStructField::Spread {
                        identifier: identifier.name.clone(),
                        spread_type: None, // TODO: Resolve spread expression type
                    });
                }
            }
        }

        // TODO: Validate fields against struct definition and compute struct type
        let struct_type = None; // Placeholder for now

        Some(TypedExpressionKind::StructLiteral {
            type_path,
            fields: typed_fields,
            struct_type,
        })
    }

    /// Convert a parser pattern to a typed pattern with type validation
    pub fn convert_pattern(
        &mut self,
        pattern: &outrun_parser::Pattern,
        target_type: &Option<StructuredType>,
    ) -> Option<TypedPattern> {
        let mut pattern_checker = PatternChecker::new(&mut self.context);

        match pattern_checker.check_pattern(pattern, target_type) {
            Ok(typed_pattern) => Some(typed_pattern),
            Err(error) => {
                // Store pattern conversion error
                self.errors.push(error);
                None
            }
        }
    }

    /// Get bound variables from a pattern for scope registration
    pub fn get_pattern_bound_variables(
        &mut self,
        pattern: &outrun_parser::Pattern,
        target_type: &Option<StructuredType>,
    ) -> Vec<crate::patterns::BoundVariable> {
        if let Some(typed_pattern) = self.convert_pattern(pattern, target_type) {
            typed_pattern.bound_variables
        } else {
            Vec::new() // Return empty if pattern conversion failed
        }
    }

    /// Convert if expression with branch type compatibility checking
    fn convert_if_expression(&mut self, if_expr: &IfExpression) -> Option<TypedExpressionKind> {
        // Convert condition - must be boolean
        let condition = if let Some(typed_condition) = self.convert_expression(&if_expr.condition) {
            Box::new(typed_condition)
        } else {
            return None;
        };

        // Convert then branch
        let then_branch = if let Some(typed_then) = self.convert_block(&if_expr.then_block) {
            Box::new(typed_then)
        } else {
            return None;
        };

        // Convert optional else branch
        let else_branch = if let Some(else_block) = &if_expr.else_block {
            if let Some(typed_else) = self.convert_block(else_block) {
                Some(Box::new(typed_else))
            } else {
                return None;
            }
        } else {
            None
        };

        // TODO: Unify branch types to determine result type
        let result_type = None; // Placeholder for now

        Some(TypedExpressionKind::IfExpression {
            condition,
            then_branch,
            else_branch,
            result_type,
        })
    }

    /// Convert case expression (concrete or trait variant)
    fn convert_case_expression(
        &mut self,
        case_expr: &CaseExpression,
    ) -> Option<TypedExpressionKind> {
        let variant = match case_expr {
            CaseExpression::Concrete(concrete_case) => {
                self.convert_concrete_case_expression(concrete_case)?
            }
            CaseExpression::Trait(trait_case) => self.convert_trait_case_expression(trait_case)?,
        };

        // TODO: Unify all branch types to determine result type
        let result_type = None; // Placeholder for now

        Some(TypedExpressionKind::CaseExpression {
            variant,
            result_type,
        })
    }

    /// Convert concrete case expression with pattern matching
    fn convert_concrete_case_expression(
        &mut self,
        concrete_case: &ConcreteCaseExpression,
    ) -> Option<TypedCaseVariant> {
        // Convert the expression being matched
        let expression =
            if let Some(typed_expr) = self.convert_expression(&concrete_case.expression) {
                Box::new(typed_expr)
            } else {
                return None;
            };

        // Convert all when clauses
        let mut when_clauses = Vec::new();
        for when_clause in &concrete_case.when_clauses {
            if let Some(typed_when) = self.convert_when_clause(when_clause) {
                when_clauses.push(typed_when);
            } else {
                return None; // Failed to convert a when clause
            }
        }

        Some(TypedCaseVariant::Concrete {
            expression,
            when_clauses,
        })
    }

    /// Convert trait case expression with type checking
    fn convert_trait_case_expression(
        &mut self,
        trait_case: &TraitCaseExpression,
    ) -> Option<TypedCaseVariant> {
        // Convert the expression being matched
        let expression = if let Some(typed_expr) = self.convert_expression(&trait_case.expression) {
            Box::new(typed_expr)
        } else {
            return None;
        };

        // Convert trait name
        let trait_name = trait_case.trait_name.name.clone();

        // Convert all as clauses
        let mut as_clauses = Vec::new();
        for type_clause in &trait_case.type_clauses {
            if let Some(typed_as) = self.convert_trait_case_clause(type_clause) {
                as_clauses.push(typed_as);
            } else {
                return None; // Failed to convert a type clause
            }
        }

        Some(TypedCaseVariant::Trait {
            expression,
            trait_name,
            as_clauses,
        })
    }

    /// Convert when clause for concrete case expressions
    fn convert_when_clause(&mut self, when_clause: &CaseWhenClause) -> Option<TypedWhenClause> {
        // Convert guard expression
        let guard = if let Some(typed_guard) = self.convert_expression(&when_clause.guard) {
            Box::new(typed_guard)
        } else {
            return None;
        };

        // Convert result
        let result = if let Some(typed_result) = self.convert_case_result(&when_clause.result) {
            Box::new(typed_result)
        } else {
            return None;
        };

        // TODO: Extract bound variables from guard patterns
        let bound_variables = Vec::new(); // Placeholder for now

        Some(TypedWhenClause {
            guard,
            result,
            bound_variables,
            span: when_clause.span,
        })
    }

    /// Convert trait case clause for trait case expressions
    fn convert_trait_case_clause(
        &mut self,
        type_clause: &TraitCaseClause,
    ) -> Option<TypedAsClause> {
        // Convert type path
        let type_path = vec![type_clause.type_name.name.clone()];

        // Convert optional pattern
        let pattern = if let Some(_struct_pattern) = &type_clause.pattern {
            // TODO: Convert struct pattern to TypedPattern
            // For now, we don't have struct pattern support in the pattern system
            None
        } else {
            None
        };

        // Convert result
        let result = if let Some(typed_result) = self.convert_case_result(&type_clause.result) {
            Box::new(typed_result)
        } else {
            return None;
        };

        // TODO: Verify trait implementation
        let impl_verified = false; // Placeholder for now

        Some(TypedAsClause {
            type_path,
            pattern,
            result,
            impl_verified,
            span: type_clause.span,
        })
    }

    /// Convert case result (block or expression)
    fn convert_case_result(&mut self, case_result: &CaseResult) -> Option<TypedExpression> {
        match case_result {
            CaseResult::Block(block) => self.convert_block(block),
            CaseResult::Expression(expr) => self.convert_expression(expr),
        }
    }

    /// Convert block to expression (for now, simple approach)
    fn convert_block(&mut self, block: &Block) -> Option<TypedExpression> {
        if block.statements.is_empty() {
            // Empty block - return placeholder
            return Some(TypedExpression {
                kind: TypedExpressionKind::Placeholder("Empty block".to_string()),
                structured_type: None,
                span: block.span,
            });
        }

        // For now, just convert the last statement as an expression
        // TODO: Handle multiple statements properly
        if let Some(last_statement) = block.statements.last() {
            match &last_statement.kind {
                outrun_parser::StatementKind::Expression(expr) => self.convert_expression(expr),
                _ => Some(TypedExpression {
                    kind: TypedExpressionKind::Placeholder("Non-expression statement".to_string()),
                    structured_type: None,
                    span: last_statement.span,
                }),
            }
        } else {
            None
        }
    }

    /// Convert function definition with parameter and body validation
    fn convert_function_definition(
        &mut self,
        func_def: &FunctionDefinition,
    ) -> Option<TypedFunctionDefinition> {
        // Convert parameters
        let mut typed_parameters = Vec::new();
        for param in &func_def.parameters {
            if let Some(typed_param) = self.convert_parameter(param) {
                typed_parameters.push(typed_param);
            } else {
                return None; // Failed to convert parameter
            }
        }

        // Convert return type
        let return_type = self.convert_type_annotation(&func_def.return_type);

        // Convert optional guard
        let guard = if let Some(guard_clause) = &func_def.guard {
            if let Some(typed_guard) = self.convert_expression(&guard_clause.condition) {
                Some(Box::new(typed_guard))
            } else {
                return None; // Failed to convert guard
            }
        } else {
            None
        };

        // Convert body
        let body = self.convert_block_to_typed_block(&func_def.body)?;

        // Generate function ID
        let function_id = func_def.name.name.clone();

        Some(TypedFunctionDefinition {
            name: func_def.name.name.clone(),
            parameters: typed_parameters,
            return_type,
            guard,
            body,
            function_id,
            span: func_def.span,
        })
    }

    /// Convert function parameter with type validation
    fn convert_parameter(&mut self, param: &Parameter) -> Option<TypedParameter> {
        let param_type = self.convert_type_annotation(&param.type_annotation);

        Some(TypedParameter {
            name: param.name.name.clone(),
            param_type,
            span: param.span,
        })
    }

    /// Convert type annotation to structured type
    fn convert_type_annotation(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> Option<StructuredType> {
        // For now, just create a simple placeholder
        // TODO: Implement proper type annotation resolution
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, .. } => {
                if let Some(first_type) = path.first() {
                    let type_id = self.context.type_interner.intern_type(&first_type.name);
                    Some(StructuredType::simple(type_id))
                } else {
                    None
                }
            }
            outrun_parser::TypeAnnotation::Tuple { .. } => {
                // TODO: Implement tuple type annotation conversion
                None
            }
            outrun_parser::TypeAnnotation::Function { .. } => {
                // TODO: Implement function type annotation conversion
                None
            }
        }
    }

    /// Convert parser block to typed block with statement handling
    fn convert_block_to_typed_block(&mut self, block: &Block) -> Option<TypedBlock> {
        let mut typed_statements = Vec::new();
        let mut result_type = None;

        // Convert all statements
        for statement in &block.statements {
            if let Some(typed_statement) = self.convert_statement(statement) {
                // Track the type of the last statement as the result type
                result_type = match &typed_statement {
                    TypedStatement::Expression(expr) => expr.structured_type.clone(),
                    TypedStatement::LetBinding(_) => None, // Let bindings don't have result types
                };
                typed_statements.push(typed_statement);
            } else {
                return None; // Failed to convert statement
            }
        }

        Some(TypedBlock {
            statements: typed_statements,
            result_type,
            span: block.span,
        })
    }

    /// Convert statement (expression or let binding)
    fn convert_statement(&mut self, statement: &Statement) -> Option<TypedStatement> {
        match &statement.kind {
            StatementKind::Expression(expr) => self
                .convert_expression(expr)
                .map(|typed_expr| TypedStatement::Expression(Box::new(typed_expr))),
            StatementKind::LetBinding(let_binding) => self
                .convert_let_binding(let_binding)
                .map(|typed_let| TypedStatement::LetBinding(Box::new(typed_let))),
        }
    }

    /// Convert let binding with pattern and expression validation
    fn convert_let_binding(&mut self, let_binding: &LetBinding) -> Option<TypedLetBinding> {
        // Convert expression first
        let expression = if let Some(typed_expr) = self.convert_expression(&let_binding.expression)
        {
            Box::new(typed_expr)
        } else {
            return None;
        };

        // Convert pattern with the expression's type as target
        let target_type = &expression.structured_type;
        let pattern = self.convert_pattern(&let_binding.pattern, target_type)?;

        // Binding type is the same as expression type
        let binding_type = expression.structured_type.clone();

        Some(TypedLetBinding {
            pattern,
            expression,
            binding_type,
            span: let_binding.span,
        })
    }

    /// Convert anonymous function with multiple clauses
    fn convert_anonymous_function(
        &mut self,
        anon_func: &AnonymousFunction,
    ) -> Option<TypedAnonymousFunction> {
        let mut typed_clauses = Vec::new();

        // Convert all clauses
        for clause in &anon_func.clauses {
            if let Some(typed_clause) = self.convert_anonymous_clause(clause) {
                typed_clauses.push(typed_clause);
            } else {
                return None; // Failed to convert clause
            }
        }

        // TODO: Unify all clause types to determine overall function type
        let function_type = None; // Placeholder for now

        Some(TypedAnonymousFunction {
            clauses: typed_clauses,
            function_type,
            span: anon_func.span,
        })
    }

    /// Convert single anonymous function clause
    fn convert_anonymous_clause(
        &mut self,
        clause: &AnonymousClause,
    ) -> Option<TypedAnonymousClause> {
        // Convert parameters (anonymous function parameters are simpler)
        let mut typed_parameters = Vec::new();
        match &clause.parameters {
            outrun_parser::AnonymousParameters::None { .. } => {
                // No parameters
            }
            outrun_parser::AnonymousParameters::Single { parameter, .. } => {
                typed_parameters.push(TypedParameter {
                    name: parameter.name.name.clone(),
                    param_type: self.convert_type_annotation(&parameter.type_annotation),
                    span: parameter.span,
                });
            }
            outrun_parser::AnonymousParameters::Multiple { parameters, .. } => {
                for param in parameters {
                    typed_parameters.push(TypedParameter {
                        name: param.name.name.clone(),
                        param_type: self.convert_type_annotation(&param.type_annotation),
                        span: param.span,
                    });
                }
            }
        }

        // Convert optional guard
        let guard = if let Some(guard_expr) = &clause.guard {
            if let Some(typed_guard) = self.convert_expression(guard_expr) {
                Some(Box::new(typed_guard))
            } else {
                return None;
            }
        } else {
            None
        };

        // Convert body
        let body = match &clause.body {
            outrun_parser::AnonymousBody::Expression(expr) => {
                if let Some(typed_expr) = self.convert_expression(expr) {
                    Box::new(typed_expr)
                } else {
                    return None;
                }
            }
            outrun_parser::AnonymousBody::Block(block) => {
                if let Some(typed_expr) = self.convert_block(block) {
                    Box::new(typed_expr)
                } else {
                    return None;
                }
            }
        };

        // Clause type is the same as body type
        let clause_type = body.structured_type.clone();

        Some(TypedAnonymousClause {
            parameters: typed_parameters,
            guard,
            body,
            clause_type,
            span: clause.span,
        })
    }

    /// Convert macro injection with parameter validation
    fn convert_macro_injection(
        &mut self,
        injection: &outrun_parser::MacroInjection,
    ) -> Option<TypedExpressionKind> {
        // For now, macro injection is not resolved - we just track the parameter
        // In a full implementation, we would look up the macro definition and
        // validate that the parameter exists, but that requires macro context
        Some(TypedExpressionKind::MacroInjection {
            parameter: injection.parameter.name.clone(),
            injected_expression: None, // Not resolved yet - would need macro expansion context
            original_span: injection.span,
        })
    }

    /// Convert macro definition with body validation
    fn convert_macro_definition(
        &mut self,
        macro_def: &outrun_parser::MacroDefinition,
    ) -> Option<TypedMacroDefinition> {
        // Convert parameter names
        let parameters: Vec<String> = macro_def
            .parameters
            .iter()
            .map(|param| param.name.clone())
            .collect();

        // Convert body block
        let body = self.convert_block_to_typed_block(&macro_def.body)?;

        // TODO: In a full implementation, we would:
        // 1. Validate that all macro parameters are used in the body
        // 2. Check for hygiene violations
        // 3. Ensure macro injections are valid
        // For now, we just convert the structure

        Some(TypedMacroDefinition {
            name: macro_def.name.name.clone(),
            parameters,
            body,
            hygiene_scope: None, // Not implemented yet
            span: macro_def.span,
        })
    }

    /// Convert struct definition with field validation and generic context
    fn convert_struct_definition(
        &mut self,
        struct_def: &StructDefinition,
    ) -> Option<TypedStructDefinition> {
        // Convert name path to string representation
        let name: Vec<String> = struct_def.name.iter().map(|t| t.name.clone()).collect();

        // Convert generic parameters with enhanced constraint support
        let generic_params = if let Some(params) = &struct_def.generic_params {
            self.convert_generic_params(params)
        } else {
            Vec::new()
        };

        // Create struct type for Self resolution
        let struct_type = if generic_params.is_empty() {
            Some(StructuredType::Simple(
                self.context.type_interner.intern_type(&name.join(".")),
            ))
        } else {
            // Generic struct - create generic type with parameter placeholders
            let param_types: Vec<StructuredType> = generic_params
                .iter()
                .map(|param| {
                    StructuredType::Simple(self.context.type_interner.intern_type(&param.name))
                })
                .collect();
            Some(StructuredType::Generic {
                base: self.context.type_interner.intern_type(&name.join(".")),
                args: param_types,
            })
        };

        // Push generic context for field and method resolution
        let generic_context = self.create_generic_context(&generic_params, struct_type.clone());
        self.push_generic_context(generic_context);

        // Convert struct fields with generic context
        let mut typed_fields = Vec::new();
        for field in &struct_def.fields {
            if let Some(typed_field) = self.convert_struct_field_with_generics(field) {
                typed_fields.push(typed_field);
            } else {
                self.pop_generic_context(); // Clean up on failure
                return None; // Failed to convert field
            }
        }

        // Convert methods with generic context
        let mut typed_methods = Vec::new();
        for method in &struct_def.methods {
            if let Some(typed_method) = self.convert_function_definition(method) {
                typed_methods.push(typed_method);
            } else {
                self.pop_generic_context(); // Clean up on failure
                return None; // Failed to convert method
            }
        }

        // Pop generic context
        self.pop_generic_context();

        // Generate struct ID from name path
        let struct_id = name.join(".");

        Some(TypedStructDefinition {
            name,
            generic_params,
            fields: typed_fields,
            methods: typed_methods,
            struct_id,
            span: struct_def.span,
        })
    }

    /// Convert struct field with comprehensive generic support
    fn convert_struct_field_with_generics(
        &mut self,
        field: &StructField,
    ) -> Option<TypedStructFieldDefinition> {
        // Use comprehensive type annotation conversion for better generic support
        let field_type = if let Some(typed_annotation) =
            self.convert_type_annotation_comprehensive(&field.type_annotation)
        {
            typed_annotation.resolved_type
        } else {
            // Fallback to simple conversion
            self.convert_type_annotation(&field.type_annotation)
        };

        Some(TypedStructFieldDefinition {
            name: field.name.name.clone(),
            field_type,
            span: field.span,
        })
    }

    /// Convert trait definition with function signature validation
    fn convert_trait_definition(
        &mut self,
        trait_def: &TraitDefinition,
    ) -> Option<TypedTraitDefinition> {
        // Convert name path to string representation
        let name: Vec<String> = trait_def.name.iter().map(|t| t.name.clone()).collect();

        // Convert generic parameters
        let generic_params = if let Some(params) = &trait_def.generic_params {
            self.convert_generic_params(params)
        } else {
            Vec::new()
        };

        // Convert constraints (TODO: implement constraint parsing)
        let constraints = Vec::new(); // Placeholder for now

        // Convert trait functions
        let mut typed_functions = Vec::new();
        for func in &trait_def.functions {
            if let Some(typed_func) = self.convert_trait_function(func) {
                typed_functions.push(typed_func);
            } else {
                return None; // Failed to convert function
            }
        }

        // Generate trait ID from name path
        let trait_id = name.join(".");

        Some(TypedTraitDefinition {
            name,
            generic_params,
            constraints,
            functions: typed_functions,
            trait_id,
            span: trait_def.span,
        })
    }

    /// Convert trait function (signature, definition, or static)
    fn convert_trait_function(&mut self, func: &TraitFunction) -> Option<TypedTraitFunction> {
        match func {
            TraitFunction::Signature(sig) => {
                // Convert function signature
                let mut typed_parameters = Vec::new();
                for param in &sig.parameters {
                    if let Some(typed_param) = self.convert_parameter(param) {
                        typed_parameters.push(typed_param);
                    } else {
                        return None;
                    }
                }

                let return_type = self.convert_type_annotation(&sig.return_type);

                // Convert optional guard
                let guard = if let Some(guard_clause) = &sig.guard {
                    if let Some(typed_guard) = self.convert_expression(&guard_clause.condition) {
                        Some(Box::new(typed_guard))
                    } else {
                        return None;
                    }
                } else {
                    None
                };

                Some(TypedTraitFunction::Signature {
                    name: sig.name.name.clone(),
                    parameters: typed_parameters,
                    return_type,
                    guard,
                    span: sig.span,
                })
            }
            TraitFunction::Definition(def) => {
                // Convert function definition
                self.convert_function_definition(def)
                    .map(TypedTraitFunction::Definition)
            }
            TraitFunction::StaticDefinition(static_def) => {
                // Convert static function definition
                let mut typed_parameters = Vec::new();
                for param in &static_def.parameters {
                    if let Some(typed_param) = self.convert_parameter(param) {
                        typed_parameters.push(typed_param);
                    } else {
                        return None;
                    }
                }

                let return_type = self.convert_type_annotation(&static_def.return_type);

                let body = self.convert_block_to_typed_block(&static_def.body)?;

                Some(TypedTraitFunction::StaticDefinition {
                    name: static_def.name.name.clone(),
                    parameters: typed_parameters,
                    return_type,
                    body,
                    span: static_def.span,
                })
            }
        }
    }

    /// Convert impl block with trait validation and Self type resolution
    fn convert_impl_block(&mut self, impl_block: &ImplBlock) -> Option<TypedImplBlock> {
        // Convert generic parameters
        let generic_params = if let Some(params) = &impl_block.generic_params {
            self.convert_generic_params(params)
        } else {
            Vec::new()
        };

        // Convert trait path
        let trait_path = self.convert_type_spec(&impl_block.trait_spec);

        // Convert type path
        let type_path = self.convert_type_spec(&impl_block.type_spec);

        // Resolve implementation type for Self resolution
        let impl_type = if type_path.is_empty() {
            None
        } else if let Some(generic_args) = &impl_block.type_spec.generic_args {
            // Generic implementation type
            let mut resolved_args = Vec::new();
            for arg in &generic_args.args {
                if let Some(typed_annotation) = self.convert_type_annotation_comprehensive(arg) {
                    if let Some(resolved_type) = typed_annotation.resolved_type {
                        resolved_args.push(resolved_type);
                    }
                }
            }

            if resolved_args.is_empty() {
                // Simple type
                Some(StructuredType::Simple(
                    self.context.type_interner.intern_type(&type_path.join(".")),
                ))
            } else {
                // Generic type with arguments
                Some(StructuredType::Generic {
                    base: self.context.type_interner.intern_type(&type_path.join(".")),
                    args: resolved_args,
                })
            }
        } else {
            // Simple implementation type
            Some(StructuredType::Simple(
                self.context.type_interner.intern_type(&type_path.join(".")),
            ))
        };

        // Resolve trait type
        let trait_type = if trait_path.is_empty() {
            None
        } else {
            Some(StructuredType::Simple(
                self.context
                    .type_interner
                    .intern_type(&trait_path.join(".")),
            ))
        };

        // Create generic context for method resolution with Self type
        let generic_context = self.create_generic_context(&generic_params, impl_type.clone());
        self.push_generic_context(generic_context);

        // Convert constraints (TODO: implement constraint parsing)
        let constraints = Vec::new(); // Placeholder for now

        // Convert methods with Self type resolution
        let mut typed_methods = Vec::new();
        for method in &impl_block.methods {
            if let Some(typed_method) = self.convert_function_definition(method) {
                typed_methods.push(typed_method);
            } else {
                self.pop_generic_context(); // Clean up on failure
                return None; // Failed to convert method
            }
        }

        // Pop generic context
        self.pop_generic_context();

        // Validate generic constraints
        let impl_verified = self.validate_generic_constraints();

        Some(TypedImplBlock {
            generic_params,
            trait_path,
            type_path,
            trait_type,
            impl_type,
            constraints,
            methods: typed_methods,
            impl_verified,
            span: impl_block.span,
        })
    }

    /// Convert const definition with expression validation
    fn convert_const_definition(
        &mut self,
        const_def: &ConstDefinition,
    ) -> Option<TypedConstDefinition> {
        // Convert the constant expression
        let expression = if let Some(typed_expr) = self.convert_expression(&const_def.expression) {
            Box::new(typed_expr)
        } else {
            return None;
        };

        // Convert type annotation
        let const_type = self.convert_type_annotation(&const_def.type_annotation);

        // Generate const ID
        let const_id = const_def.name.name.clone();

        Some(TypedConstDefinition {
            name: const_def.name.name.clone(),
            const_type,
            expression,
            const_id,
            span: const_def.span,
        })
    }

    /// Convert generic parameters with constraint validation
    fn convert_generic_params(&mut self, params: &GenericParams) -> Vec<TypedGenericParam> {
        let mut typed_params = Vec::new();
        for param in &params.params {
            typed_params.push(self.convert_generic_param(param));
        }
        typed_params
    }

    /// Convert single generic parameter
    fn convert_generic_param(&mut self, param: &GenericParam) -> TypedGenericParam {
        // TODO: Convert constraints when constraint parsing is implemented
        let constraints = Vec::new(); // Placeholder for now

        TypedGenericParam {
            name: param.name.name.clone(),
            constraints,
            span: param.span,
        }
    }

    /// Convert type spec to string path
    fn convert_type_spec(&self, type_spec: &TypeSpec) -> Vec<String> {
        type_spec.path.iter().map(|t| t.name.clone()).collect()
    }

    /// Comprehensive type annotation conversion with generic resolution
    fn convert_type_annotation_comprehensive(
        &mut self,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> Option<TypedTypeAnnotation> {
        use outrun_parser::TypeAnnotation;

        let annotation_kind = match type_annotation {
            TypeAnnotation::Simple {
                path,
                generic_args,
                span: _,
            } => {
                // Convert path to string representation
                let path_strings: Vec<String> = path.iter().map(|t| t.name.clone()).collect();

                // Convert generic arguments with recursive resolution
                let typed_generic_args = if let Some(args) = generic_args {
                    let mut typed_args = Vec::new();
                    for arg in &args.args {
                        if let Some(typed_arg) = self.convert_type_annotation_comprehensive(arg) {
                            typed_args.push(typed_arg);
                        } else {
                            return None; // Failed to convert generic argument
                        }
                    }
                    typed_args
                } else {
                    Vec::new()
                };

                TypedTypeAnnotationKind::Simple {
                    path: path_strings,
                    generic_args: typed_generic_args,
                }
            }
            TypeAnnotation::Tuple { types, span: _ } => {
                // Convert all tuple element types
                let mut typed_elements = Vec::new();
                for element_type in types {
                    if let Some(typed_element) =
                        self.convert_type_annotation_comprehensive(element_type)
                    {
                        typed_elements.push(typed_element);
                    } else {
                        return None; // Failed to convert tuple element
                    }
                }

                TypedTypeAnnotationKind::Tuple(typed_elements)
            }
            TypeAnnotation::Function {
                params,
                return_type,
                span: _,
            } => {
                // Convert function parameters
                let mut typed_params = Vec::new();
                for param in params {
                    if let Some(typed_param) = self.convert_function_type_param(param) {
                        typed_params.push(typed_param);
                    } else {
                        return None; // Failed to convert parameter
                    }
                }

                // Convert return type
                if let Some(typed_return_type) =
                    self.convert_type_annotation_comprehensive(return_type)
                {
                    TypedTypeAnnotationKind::Function {
                        params: typed_params,
                        return_type: Box::new(typed_return_type),
                    }
                } else {
                    return None; // Failed to convert return type
                }
            }
        };

        // Resolve the structured type with generic context
        let resolved_type = self.resolve_type_annotation_with_context(&annotation_kind);

        Some(TypedTypeAnnotation {
            annotation_kind,
            resolved_type,
            span: self.get_type_annotation_span(type_annotation),
        })
    }

    /// Convert function type parameter with comprehensive type resolution
    fn convert_function_type_param(
        &mut self,
        param: &outrun_parser::FunctionTypeParam,
    ) -> Option<TypedFunctionTypeParam> {
        self.convert_type_annotation_comprehensive(&param.type_annotation)
            .map(|typed_type| TypedFunctionTypeParam {
                name: param.name.name.clone(),
                param_type: typed_type,
                span: param.span,
            })
    }

    /// Resolve type annotation with current generic context
    fn resolve_type_annotation_with_context(
        &mut self,
        annotation_kind: &TypedTypeAnnotationKind,
    ) -> Option<StructuredType> {
        match annotation_kind {
            TypedTypeAnnotationKind::Simple { path, generic_args } => {
                // Handle Self type resolution in impl blocks
                if path.len() == 1 && path[0] == "Self" {
                    if let Some(context) = &self.generic_context {
                        return context.self_type.clone();
                    }
                }

                // Check for generic parameter substitution
                if path.len() == 1 {
                    if let Some(context) = &self.generic_context {
                        if let Some(substituted_type) = context.substitutions.get(&path[0]) {
                            return Some(substituted_type.clone());
                        }
                    }
                }

                // Resolve generic arguments recursively
                if !generic_args.is_empty() {
                    let mut resolved_args = Vec::new();
                    for arg in generic_args {
                        if let Some(resolved_arg) = arg.resolved_type.as_ref() {
                            resolved_args.push(resolved_arg.clone());
                        } else {
                            return None; // Failed to resolve generic argument
                        }
                    }

                    // Create generic type with resolved arguments
                    Some(StructuredType::Generic {
                        base: self.context.type_interner.intern_type(&path.join(".")),
                        args: resolved_args,
                    })
                } else {
                    // Simple type without generics
                    Some(StructuredType::Simple(
                        self.context.type_interner.intern_type(&path.join(".")),
                    ))
                }
            }
            TypedTypeAnnotationKind::Tuple(elements) => {
                // Resolve all tuple element types
                let mut resolved_elements = Vec::new();
                for element in elements {
                    if let Some(resolved_element) = element.resolved_type.as_ref() {
                        resolved_elements.push(resolved_element.clone());
                    } else {
                        return None; // Failed to resolve tuple element
                    }
                }

                Some(StructuredType::Tuple(resolved_elements))
            }
            TypedTypeAnnotationKind::Function {
                params,
                return_type,
            } => {
                // Resolve parameter types
                let mut resolved_params = Vec::new();
                for param in params {
                    if let Some(resolved_param_type) = param.param_type.resolved_type.as_ref() {
                        resolved_params.push(crate::unification::FunctionParam {
                            name: self.context.type_interner.intern_atom(&param.name),
                            param_type: resolved_param_type.clone(),
                        });
                    } else {
                        return None; // Failed to resolve parameter type
                    }
                }

                // Resolve return type
                return_type
                    .resolved_type
                    .as_ref()
                    .map(|resolved_return_type| StructuredType::Function {
                        params: resolved_params,
                        return_type: Box::new(resolved_return_type.clone()),
                    })
            }
        }
    }

    /// Create generic context for a scope with given parameters
    fn create_generic_context(
        &mut self,
        generic_params: &[TypedGenericParam],
        self_type: Option<StructuredType>,
    ) -> TypedGenericContext {
        // Collect all constraints from parameters
        let mut all_constraints = Vec::new();
        for param in generic_params {
            all_constraints.extend(param.constraints.clone());
        }

        TypedGenericContext {
            generic_params: generic_params.to_vec(),
            constraints: all_constraints,
            substitutions: HashMap::new(),
            self_type,
        }
    }

    /// Push generic context for nested scopes
    fn push_generic_context(&mut self, context: TypedGenericContext) {
        self.generic_context = Some(context);
    }

    /// Pop generic context when leaving scope
    fn pop_generic_context(&mut self) {
        self.generic_context = None;
    }

    /// Validate generic constraints in current context
    fn validate_generic_constraints(&self) -> bool {
        if let Some(context) = &self.generic_context {
            for constraint in &context.constraints {
                // TODO: Implement actual constraint validation
                // For now, assume all constraints are valid
                let _param_name = &constraint.param_name;
                let _trait_path = &constraint.trait_path;
                // Would check if substituted type implements required trait
            }
        }
        true // Placeholder - always valid for now
    }

    /// Get span from type annotation
    fn get_type_annotation_span(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> outrun_parser::Span {
        use outrun_parser::TypeAnnotation;
        match type_annotation {
            TypeAnnotation::Simple { span, .. } => *span,
            TypeAnnotation::Tuple { span, .. } => *span,
            TypeAnnotation::Function { span, .. } => *span,
        }
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
