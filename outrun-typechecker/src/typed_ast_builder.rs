//! TypedASTBuilder visitor for creating comprehensive typed AST
//!
//! This visitor runs after TypeCheckingVisitor has completed type validation
//! and uses the type checking results to build a complete typed AST with
//! resolved type information for all nodes.

use crate::checker::{
    DispatchMethod, TypedAnonymousClause, TypedAnonymousFunction, TypedArgument, TypedAsClause,
    TypedBlock, TypedCaseVariant, TypedExpression, TypedExpressionKind, TypedFunctionDefinition,
    TypedFunctionPath, TypedItem, TypedItemKind, TypedLetBinding, TypedMapEntry, TypedParameter,
    TypedProgram, TypedStatement, TypedStructField, TypedWhenClause,
};
use crate::multi_program_compiler::{FunctionRegistry, ProgramCollection};
use crate::patterns::{PatternChecker, TypedPattern};
#[allow(unused_imports)]
use crate::types::TypeId;
use crate::unification::{StructuredType, UnificationContext};
use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{
    AnonymousClause, AnonymousFunction, Argument, Block, CaseExpression, CaseResult,
    CaseWhenClause, ConcreteCaseExpression, Expression, ExpressionKind, FunctionCall,
    FunctionDefinition, FunctionPath, IfExpression, Item, ItemKind, LetBinding, ListElement,
    ListLiteral, MapEntry, MapLiteral, Parameter, Program, Span, Statement, StatementKind,
    StructLiteral, StructLiteralField, TraitCaseClause, TraitCaseExpression, TupleLiteral,
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
            ItemKind::FunctionDefinition(func_def) => {
                if let Some(typed_func) = self.convert_function_definition(func_def) {
                    TypedItemKind::FunctionDefinition(typed_func)
                } else {
                    TypedItemKind::Placeholder("Failed to convert function definition".to_string())
                }
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
