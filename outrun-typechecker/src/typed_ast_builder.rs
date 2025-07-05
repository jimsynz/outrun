//! TypedASTBuilder visitor for creating comprehensive typed AST
//!
//! This visitor runs after TypeCheckingVisitor has completed type validation
//! and uses the type checking results to build a complete typed AST with
//! resolved type information for all nodes.

use crate::checker::{
    AttachedComment, CommentAttachment, DispatchMethod, InterpolationPart, LiteralFormatDetails,
    LiteralFormatInfo, TypedAnonymousClause, TypedAnonymousFunction, TypedArgument, TypedBlock,
    TypedCaseVariant, TypedConstDefinition, TypedDebugInfo, TypedExpression, TypedExpressionKind,
    TypedFunctionDefinition, TypedFunctionPath, TypedFunctionTypeParam, TypedGenericContext,
    TypedGenericParam, TypedImplBlock, TypedItem, TypedItemKind, TypedLetBinding,
    TypedMacroDefinition, TypedMapEntry, TypedParameter, TypedProgram, TypedStatement,
    TypedStructDefinition, TypedStructField, TypedStructFieldDefinition, TypedTraitDefinition,
    TypedTraitFunction, TypedTypeAnnotation, TypedTypeAnnotationKind, TypedWhenClause,
};
#[allow(unused_imports)]
use crate::compilation::compiler_environment::{AtomId, TypeNameId};
use crate::compilation::program_collection::ProgramCollection;
use crate::error::{SpanExt, TypeError};
use crate::patterns::{PatternChecker, TypedPattern};
use crate::unification::{StructuredType, UnificationContext};
use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{
    AnonymousClause, AnonymousFunction, Argument, BinaryOperator, Block, CaseExpression,
    ConstDefinition, Expression, ExpressionKind, FunctionCall, FunctionDefinition, FunctionPath,
    GenericParam, GenericParams, IfExpression, ImplBlock, Item, ItemKind, LetBinding, ListElement,
    ListLiteral, MapEntry, MapLiteral, Parameter, Program, Span, Statement, StatementKind,
    StructDefinition, StructField, StructLiteral, StructLiteralField, TraitDefinition,
    TraitFunction, TupleLiteral, TypeSpec,
};
use std::collections::HashMap;

/// Comment attachment system for spatial analysis
#[derive(Debug)]
pub struct CommentAttacher {
    /// All comments from the program, sorted by position
    comments: Vec<outrun_parser::Comment>,
    /// Index of the next comment to process
    comment_index: usize,
}

impl CommentAttacher {
    /// Create a new comment attacher with sorted comments
    pub fn new(mut comments: Vec<outrun_parser::Comment>) -> Self {
        // Sort comments by their start position for efficient processing
        comments.sort_by_key(|c| c.span.start);
        Self {
            comments,
            comment_index: 0,
        }
    }

    /// Attach comments to a typed node based on spatial relationships
    pub fn attach_comments_to_node(&mut self, node_span: Span) -> Vec<AttachedComment> {
        let mut attached = Vec::new();

        // Look for comments that should be attached to this node
        while self.comment_index < self.comments.len() {
            let comment = &self.comments[self.comment_index];

            // Determine attachment relationship
            let attachment = if comment.span.end <= node_span.start {
                // Comment comes before this node
                if self.is_immediately_preceding(comment.span, node_span) {
                    Some(CommentAttachment::Preceding)
                } else {
                    // Check if this comment is close enough to attach
                    if node_span.start - comment.span.end <= 50 {
                        // Within 50 characters - attach as scope comment
                        Some(CommentAttachment::Scope)
                    } else {
                        // Too far away - skip this comment
                        self.comment_index += 1;
                        continue;
                    }
                }
            } else if comment.span.start >= node_span.start && comment.span.end <= node_span.end {
                // Comment is within this node
                Some(CommentAttachment::Internal)
            } else if comment.span.start >= node_span.end {
                // Comment comes after this node
                if self.is_immediately_following(node_span, comment.span) {
                    Some(CommentAttachment::Trailing)
                } else {
                    // Comment is too far after this node - don't process it now
                    break;
                }
            } else {
                // Comment overlaps in some weird way - skip it
                self.comment_index += 1;
                continue;
            };

            if let Some(attachment_type) = attachment {
                attached.push(AttachedComment {
                    comment: comment.clone(),
                    attachment: attachment_type,
                });
                self.comment_index += 1;
            } else {
                break;
            }
        }

        attached
    }

    /// Check if a comment immediately precedes a node (with only whitespace between)
    fn is_immediately_preceding(&self, comment_span: Span, node_span: Span) -> bool {
        // A comment is immediately preceding if there's only whitespace/newlines between
        // For now, use a simple heuristic: less than 10 characters gap
        node_span.start - comment_span.end <= 10
    }

    /// Check if a comment immediately follows a node (with only whitespace between)
    fn is_immediately_following(&self, node_span: Span, comment_span: Span) -> bool {
        // A comment is immediately following if it's on the same line or next line
        // For now, use a simple heuristic: less than 10 characters gap
        comment_span.start - node_span.end <= 10
    }
}

/// Visitor for building comprehensive typed AST (Phase 6)
#[derive(Debug)]
pub struct TypedASTBuilder {
    /// Type checking context with resolved types
    pub context: UnificationContext,
    /// Compiler environment for dispatch resolution
    pub compiler_environment: Option<crate::compilation::compiler_environment::CompilerEnvironment>,
    /// Struct definitions for pattern validation (struct name TypeNameId -> definition)
    pub struct_registry: HashMap<TypeNameId, outrun_parser::StructDefinition>,
    /// Collection of errors encountered during AST building
    pub errors: Vec<crate::error::TypeError>,
    /// Built typed programs (filename -> typed program)
    typed_programs: HashMap<String, TypedProgram>,
    /// Current generic context for type resolution
    generic_context: Option<TypedGenericContext>,
    /// Current module context for function updates
    current_module_key: Option<crate::compilation::compiler_environment::ModuleKey>,
    /// Comment attachment state
    comment_attacher: CommentAttacher,
    /// Error recovery information collected during AST building
    error_recovery_info: Vec<crate::checker::ErrorRecoveryInfo>,
    /// Performance and timing information
    compilation_start_time: std::time::Instant,
    phase_timings: std::collections::HashMap<String, u64>,
}

impl TypedASTBuilder {
    /// Create a new TypedASTBuilder with type checking results
    pub fn new(
        context: UnificationContext,
        struct_registry: HashMap<TypeNameId, outrun_parser::StructDefinition>,
        compiler_environment: Option<crate::compilation::compiler_environment::CompilerEnvironment>,
    ) -> Self {
        Self {
            context,
            compiler_environment,
            struct_registry,
            errors: Vec::new(),
            typed_programs: HashMap::new(),
            generic_context: None,
            current_module_key: None,
            comment_attacher: CommentAttacher::new(Vec::new()), // Will be initialized per program
            error_recovery_info: Vec::new(),
            compilation_start_time: std::time::Instant::now(),
            phase_timings: std::collections::HashMap::new(),
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

    /// Get the current module key
    pub fn get_current_module_key(
        &self,
    ) -> Option<crate::compilation::compiler_environment::ModuleKey> {
        self.current_module_key.clone()
    }

    /// Set the current module key
    pub fn set_current_module_key(
        &mut self,
        module_key: Option<crate::compilation::compiler_environment::ModuleKey>,
    ) {
        self.current_module_key = module_key;
    }

    /// Build typed AST for a single program with error recovery
    pub(crate) fn build_typed_program(
        &mut self,
        program: &Program,
        filename: &str,
    ) -> Result<TypedProgram, crate::error::TypeError> {
        let start_time = std::time::Instant::now();

        // Initialize comment attacher with this program's comments
        self.comment_attacher = CommentAttacher::new(program.debug_info.comments.clone());

        // Reset error recovery info for this program
        self.error_recovery_info.clear();

        // Convert all items to typed items with error recovery
        let mut typed_items = Vec::new();
        let mut successful_items = 0;
        let total_items = program.items.len();

        for item in &program.items {
            match self.convert_item_with_recovery(item) {
                Some(typed_item) => {
                    typed_items.push(typed_item);
                    successful_items += 1;
                }
                None => {
                    // Item conversion failed completely - this is rare but handle it
                    let error_context = crate::checker::ErrorContext::General {
                        description: format!("Failed to convert item: {:?}", item.kind),
                    };
                    let error = TypeError::internal_with_span(
                        "Item conversion failed".to_string(),
                        item.span.to_source_span(),
                    );
                    self.record_error_recovery(
                        error,
                        error_context,
                        item.span,
                        crate::checker::RecoveryStrategy::Skip,
                        false,
                    );
                }
            }
        }

        // Record timing for this phase
        let conversion_time = start_time.elapsed().as_millis() as u64;
        self.record_phase_timing("item_conversion", conversion_time);

        // Generate detailed compilation summary
        let detailed_summary = self.generate_compilation_summary(total_items, successful_items);

        let summary = format!(
            "TypedAST for {}: {} items, {} functions, {} errors recovered",
            filename,
            typed_items.len(),
            self.compiler_environment
                .as_ref()
                .map(|ce| ce.function_count())
                .unwrap_or(0),
            self.error_recovery_info.len()
        );

        Ok(TypedProgram {
            items: typed_items,
            type_context: self.context.clone(),
            compilation_order: vec![filename.to_string()], // This program only
            compilation_summary: summary,
            debug_info: TypedDebugInfo {
                comments: Vec::new(), // Comments are now attached to individual nodes
                source_file: program.debug_info.source_file.clone(),
                original_span: program.span,
                type_annotations: Vec::new(),
                inferred_types: HashMap::new(),
                literal_format: None,
            },
            error_recovery_info: self.error_recovery_info.clone(),
            detailed_summary: Some(detailed_summary),
        })
    }

    /// Convert a parser Item to TypedItem with error recovery
    fn convert_item_with_recovery(&mut self, item: &Item) -> Option<TypedItem> {
        // Try the normal conversion first
        if let Some(typed_item) = self.convert_item(item) {
            return Some(typed_item);
        }

        // If normal conversion fails, attempt error recovery
        let error = TypeError::internal_with_span(
            format!("Failed to convert item: {:?}", item.kind),
            item.span.to_source_span(),
        );
        let error_context = crate::checker::ErrorContext::General {
            description: format!("Item conversion: {:?}", std::mem::discriminant(&item.kind)),
        };

        // Create placeholder type first to avoid borrow conflicts
        let placeholder_type_id = self
            .compiler_environment
            .as_ref()
            .unwrap()
            .intern_type_name("Outrun.Core.PlaceholderType");
        let placeholder_type = StructuredType::Simple(placeholder_type_id);

        // Record the error recovery attempt
        self.record_error_recovery(
            error.clone(),
            error_context,
            item.span,
            crate::checker::RecoveryStrategy::PlaceholderExpression {
                placeholder_type: placeholder_type.clone(),
            },
            true,
        );

        // Create a placeholder typed item for error recovery
        let placeholder_kind = TypedItemKind::Placeholder(format!(
            "Error recovery placeholder for {:?}",
            std::mem::discriminant(&item.kind)
        ));

        Some(TypedItem {
            kind: placeholder_kind,
            span: item.span,
            debug_info: Some(TypedDebugInfo {
                comments: self.comment_attacher.attach_comments_to_node(item.span),
                source_file: None,
                original_span: item.span,
                type_annotations: Vec::new(),
                inferred_types: HashMap::new(),
                literal_format: None,
            }),
        })
    }

    /// Convert a parser Item to TypedItem with resolved types
    fn convert_item(&mut self, item: &Item) -> Option<TypedItem> {
        let kind = match &item.kind {
            ItemKind::Expression(expr) => {
                // Use error recovery conversion for robust handling
                let typed_expr = self.convert_expression_with_recovery(expr);
                TypedItemKind::Expression(Box::new(typed_expr))
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

        // Attach comments to this item
        let attached_comments = self.comment_attacher.attach_comments_to_node(item.span);

        Some(TypedItem {
            kind,
            span: item.span,
            debug_info: Some(TypedDebugInfo {
                comments: attached_comments,
                source_file: None, // Individual items don't track source file
                original_span: item.span,
                type_annotations: Vec::new(), // Will be populated if item has type annotations
                inferred_types: HashMap::new(),
                literal_format: None,
            }),
        })
    }

    /// Helper method to create TypedExpression with debug info and optional literal format
    fn create_typed_expression_with_literal_format(
        &mut self,
        kind: TypedExpressionKind,
        structured_type: Option<StructuredType>,
        span: Span,
        literal_format: Option<LiteralFormatInfo>,
    ) -> TypedExpression {
        // Attach comments to this expression
        let attached_comments = self.comment_attacher.attach_comments_to_node(span);

        // Create inferred types map for IDE support
        let mut inferred_types = HashMap::new();
        if let Some(ref stype) = structured_type {
            // Record the inferred type for this span
            inferred_types.insert((span.start, span.end), stype.clone());
        }

        TypedExpression {
            kind,
            structured_type,
            span,
            debug_info: Some(TypedDebugInfo {
                comments: attached_comments,
                source_file: None, // Individual expressions don't track source file
                original_span: span,
                type_annotations: Vec::new(),
                inferred_types,
                literal_format,
            }),
        }
    }

    /// Helper method to create TypedExpression with debug info
    fn create_typed_expression(
        &mut self,
        kind: TypedExpressionKind,
        structured_type: Option<StructuredType>,
        span: Span,
    ) -> TypedExpression {
        self.create_typed_expression_with_literal_format(kind, structured_type, span, None)
    }

    /// Extract literal format information from parser AST
    fn extract_literal_format_info(&self, expr: &Expression) -> Option<LiteralFormatInfo> {
        match &expr.kind {
            ExpressionKind::Integer(lit) => Some(LiteralFormatInfo {
                original_text: lit.raw_text.clone(),
                format_details: LiteralFormatDetails::Integer {
                    format: lit.format.clone(),
                    raw_digits: lit.raw_text.clone(),
                },
            }),
            ExpressionKind::Float(lit) => Some(LiteralFormatInfo {
                original_text: lit.raw_text.clone(),
                format_details: LiteralFormatDetails::Float {
                    format: lit.format.clone(),
                    raw_number: lit.raw_text.clone(),
                },
            }),
            ExpressionKind::String(lit) => {
                let has_interpolation = lit
                    .parts
                    .iter()
                    .any(|part| matches!(part, outrun_parser::StringPart::Interpolation { .. }));

                // Reconstruct the original string text and extract interpolation parts
                let (original_text, interpolation_parts) = if has_interpolation {
                    self.reconstruct_interpolated_string(lit)
                } else {
                    self.reconstruct_simple_string(lit)
                };

                Some(LiteralFormatInfo {
                    original_text,
                    format_details: LiteralFormatDetails::String {
                        format: lit.format.clone(),
                        delimiter_style: match lit.format {
                            outrun_parser::StringFormat::Basic => "\"".to_string(),
                            outrun_parser::StringFormat::Multiline => "\"\"\"".to_string(),
                        },
                        was_interpolated: has_interpolation,
                        interpolation_parts,
                    },
                })
            }
            _ => None,
        }
    }

    /// Reconstruct a simple (non-interpolated) string
    pub(crate) fn reconstruct_simple_string(
        &self,
        lit: &outrun_parser::StringLiteral,
    ) -> (String, Option<Vec<InterpolationPart>>) {
        // For simple strings, just reconstruct the basic form
        let delimiter = match lit.format {
            outrun_parser::StringFormat::Basic => "\"",
            outrun_parser::StringFormat::Multiline => "\"\"\"",
        };

        // Concatenate all text parts (should only be text for simple strings)
        let mut content = String::new();
        for part in &lit.parts {
            if let outrun_parser::StringPart::Text { content: text, .. } = part {
                content.push_str(text);
            }
        }

        let original_text = format!("{delimiter}{content}{delimiter}");
        (original_text, None)
    }

    /// Reconstruct an interpolated string and extract interpolation parts
    pub(crate) fn reconstruct_interpolated_string(
        &self,
        lit: &outrun_parser::StringLiteral,
    ) -> (String, Option<Vec<InterpolationPart>>) {
        let delimiter = match lit.format {
            outrun_parser::StringFormat::Basic => "\"",
            outrun_parser::StringFormat::Multiline => "\"\"\"",
        };

        let mut original_text = String::new();
        original_text.push_str(delimiter);

        let mut interpolation_parts = Vec::new();
        let mut current_pos = lit.span.start + delimiter.len(); // Start after opening delimiter

        for part in &lit.parts {
            match part {
                outrun_parser::StringPart::Text { content, .. } => {
                    // Add text content to original
                    original_text.push_str(content);

                    // Record text part for reconstruction
                    let part_span = Span::new(current_pos, current_pos + content.len());
                    interpolation_parts.push(InterpolationPart::Text {
                        content: content.clone(),
                        original_span: part_span,
                    });

                    current_pos += content.len();
                }
                outrun_parser::StringPart::Interpolation { expression, span } => {
                    // Add interpolation syntax to original
                    let interpolation_text =
                        format!("#{{{}}}", self.expression_to_text(expression));
                    original_text.push_str(&interpolation_text);

                    // Record expression part for reconstruction
                    interpolation_parts.push(InterpolationPart::Expression {
                        original_expression_text: self.expression_to_text(expression),
                        original_span: *span,
                        desugared_span: self.context.get_desugared_span(*span).unwrap_or(*span),
                    });

                    current_pos += interpolation_text.len();
                }
            }
        }

        original_text.push_str(delimiter);
        (original_text, Some(interpolation_parts))
    }

    /// Convert an expression back to source text (best effort)
    #[allow(clippy::only_used_in_recursion)]
    pub(crate) fn expression_to_text(&self, expr: &outrun_parser::Expression) -> String {
        // For now, a simple implementation - in production this would be more sophisticated
        match &expr.kind {
            outrun_parser::ExpressionKind::Identifier(id) => id.name.clone(),
            outrun_parser::ExpressionKind::Integer(lit) => lit.value.to_string(),
            outrun_parser::ExpressionKind::Float(lit) => lit.value.to_string(),
            outrun_parser::ExpressionKind::String(lit) => {
                // Simple string reconstruction for nested interpolation
                format!(
                    "\"{}\"",
                    lit.parts
                        .iter()
                        .filter_map(|p| match p {
                            outrun_parser::StringPart::Text { content, .. } =>
                                Some(content.as_str()),
                            _ => None,
                        })
                        .collect::<String>()
                )
            }
            outrun_parser::ExpressionKind::Boolean(lit) => lit.value.to_string(),
            outrun_parser::ExpressionKind::Atom(lit) => format!(":{}", lit.name),
            outrun_parser::ExpressionKind::FieldAccess(field_access) => {
                format!(
                    "{}.{}",
                    self.expression_to_text(&field_access.object),
                    field_access.field.name
                )
            }
            outrun_parser::ExpressionKind::FunctionCall(call) => {
                // Simple function call reconstruction
                let path_text = match &call.path {
                    outrun_parser::FunctionPath::Simple { name } => name.name.clone(),
                    outrun_parser::FunctionPath::Qualified { module, name } => {
                        format!("{}.{}", module.name, name.name)
                    }
                    outrun_parser::FunctionPath::Expression { expression } => {
                        format!("({})", self.expression_to_text(expression))
                    }
                };

                // Basic argument reconstruction
                let args_text = call
                    .arguments
                    .iter()
                    .map(|arg| match arg {
                        outrun_parser::Argument::Named {
                            name, expression, ..
                        } => {
                            format!("{}: {}", name.name, self.expression_to_text(expression))
                        }
                        outrun_parser::Argument::Spread { expression, .. } => {
                            format!("..{}", self.expression_to_text(expression))
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{path_text}({args_text})")
            }
            _ => {
                // For complex expressions, fall back to a placeholder
                format!("<expr:{:?}>", std::mem::discriminant(&expr.kind))
            }
        }
    }

    /// Convert a parser Expression to TypedExpression with error recovery
    fn convert_expression_with_recovery(&mut self, expr: &Expression) -> TypedExpression {
        // Try the normal conversion first
        if let Some(typed_expr) = self.convert_expression(expr) {
            return typed_expr;
        }

        // If normal conversion fails, create an error recovery expression
        let error = TypeError::internal_with_span(
            format!("Failed to convert expression: {:?}", expr.kind),
            expr.span.to_source_span(),
        );

        self.recover_expression_failure(expr, error)
    }

    /// Convert a parser Expression to TypedExpression with resolved types
    fn convert_expression(&mut self, expr: &Expression) -> Option<TypedExpression> {
        // Try to get resolved type for this expression
        let structured_type = self.get_expression_type(expr);

        // Extract literal format information if this is a literal
        let literal_format = self.extract_literal_format_info(expr);

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

            // Parenthesized expressions should have been eliminated during desugaring
            ExpressionKind::Parenthesized(_) => {
                TypedExpressionKind::Placeholder(
                    "Parenthesized expression found in TypedASTBuilder - this indicates a desugaring bug".to_string(),
                )
            }

            // Binary operations (only for operations not desugared, like type casting)
            ExpressionKind::BinaryOp(binary_op) => {
                match binary_op.operator {
                    BinaryOperator::As => {
                        // Type casting: convert left operand and return it directly
                        // The type checking phase validates the cast is valid
                        if let Some(typed_left) = self.convert_expression(&binary_op.left) {
                            // Type casting is a no-op in the interpreter - just return the left operand
                            // The type system has already validated the cast during type checking
                            typed_left.kind
                        } else {
                            TypedExpressionKind::Placeholder(
                                "Failed to convert type cast operand".to_string(),
                            )
                        }
                    }
                    _ => {
                        // This shouldn't happen - all other binary operators should be desugared
                        TypedExpressionKind::Placeholder(format!(
                            "Unexpected non-desugared binary operator: {:?}",
                            binary_op.operator
                        ))
                    }
                }
            }

            // Add more expression types as needed
            _ => TypedExpressionKind::Placeholder(format!(
                "Expression type not yet implemented: {:?}",
                std::mem::discriminant(&expr.kind)
            )),
        };

        Some(self.create_typed_expression_with_literal_format(
            kind,
            structured_type,
            expr.span,
            literal_format,
        ))
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
                // Spread arguments require function signature analysis for proper validation
                Argument::Spread { .. } => {
                    return None; // Requires function parameter type information for validation
                }
            }
        }

        // Resolve dispatch strategy
        let dispatch_strategy =
            self.resolve_dispatch_strategy(&function_path, &typed_arguments, call.span);

        Some(TypedExpressionKind::FunctionCall {
            function_path,
            arguments: typed_arguments,
            dispatch_strategy,
        })
    }

    /// Resolve dispatch strategy for a function call using CompilerEnvironment
    fn resolve_dispatch_strategy(
        &self,
        function_path: &TypedFunctionPath,
        arguments: &[TypedArgument],
        call_span: outrun_parser::Span,
    ) -> DispatchMethod {
        // First, try to retrieve the dispatch strategy computed and stored by the type checker
        if let Some(stored_strategy) = self.context.get_dispatch_strategy(&call_span) {
            println!("✅ DEBUG: Found stored dispatch strategy for span {:?}: {:?}", call_span, stored_strategy);
            return stored_strategy.clone();
        } else {
            println!("❌ DEBUG: No stored dispatch strategy found for span {:?}", call_span);
        }

        // Check if this is a desugared expression - try to get the original span
        if let Some(original_span) = self.context.get_original_span(call_span) {
            if let Some(stored_strategy) = self.context.get_dispatch_strategy(&original_span) {
                return stored_strategy.clone();
            }
        }

        // NEW: Apply SMT model results to resolve TypeVariable(Self) in arguments before failing
        if let Some(compiler_env) = &self.compiler_environment {
            if let Some(resolved_strategy) = self.resolve_dispatch_with_smt_model(
                function_path,
                arguments,
                call_span,
                compiler_env,
            ) {
                return resolved_strategy;
            }
        }

        // If no stored strategy found, this indicates a bug in the type checking process
        let function_name = match function_path {
            TypedFunctionPath::Simple { name } => name.clone(),
            TypedFunctionPath::Qualified { module, name } => format!("{module}.{name}"),
            TypedFunctionPath::Expression { .. } => "dynamic_function_expression".to_string(),
        };

        panic!(
            "CRITICAL BUG: TypedASTBuilder found no dispatch strategy for function call '{function_name}'.\n\
             This indicates that either:\n\
             1. The type checker failed to process this function call, or\n\
             2. There's a bug in storing/retrieving dispatch strategies.\n\
             \n\
             All function calls should have dispatch strategies computed during type checking.\n\
             Call span: {call_span:?}\n\
             Arguments passed: {arguments:?}"
        );
    }

    /// NEW: Resolve dispatch strategy by applying SMT model to resolve TypeVariable(Self)
    fn resolve_dispatch_with_smt_model(
        &self,
        function_path: &TypedFunctionPath,
        arguments: &[TypedArgument],
        _call_span: outrun_parser::Span,
        compiler_env: &crate::compilation::compiler_environment::CompilerEnvironment,
    ) -> Option<DispatchMethod> {
        // Check if this is a trait function call with TypeVariable(Self) arguments
        if let TypedFunctionPath::Qualified { module, name } = function_path {
            // Look for arguments that have TypeVariable types that need resolution
            let mut resolved_impl_type: Option<StructuredType> = None;

            for arg in arguments {
                if let Some(arg_type) = &arg.argument_type {
                    if matches!(arg_type, StructuredType::TypeVariable(_)) {
                        // Try to resolve this TypeVariable using the most recent SMT model
                        if let Ok(resolved_type) = compiler_env.resolve_type_variable_with_latest_smt_model(arg_type) {
                            resolved_impl_type = Some(resolved_type);
                            break;
                        }
                    }
                }
            }

            // If we found a resolved type, create a trait dispatch strategy
            if let Some(impl_type) = resolved_impl_type {
                return Some(DispatchMethod::Trait {
                    trait_name: module.clone(),
                    function_name: name.clone(),
                    impl_type: Box::new(impl_type),
                });
            }
        }

        None
    }

    /// Get resolved type for an expression using type checking results
    fn get_expression_type(&mut self, expr: &Expression) -> Option<StructuredType> {
        // First check if we have a resolved type from the type checking phase
        if let Some(resolved_type) = self.context.get_expression_type(&expr.span) {
            return Some(resolved_type.clone());
        }

        // Check if this is a desugared expression - try to get the original span
        if let Some(original_span) = self.context.get_original_span(expr.span) {
            if let Some(resolved_type) = self.context.get_expression_type(&original_span) {
                return Some(resolved_type.clone());
            }
        }

        // Fallback to basic type inference for literals and simple cases
        match &expr.kind {
            ExpressionKind::Integer(_) => {
                let integer_type_id = self
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.Integer64");
                Some(StructuredType::Simple(integer_type_id))
            }
            ExpressionKind::Float(_) => {
                let float_type_id = self
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.Float64");
                Some(StructuredType::Simple(float_type_id))
            }
            ExpressionKind::String(_) => {
                let string_type_id = self
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.String");
                Some(StructuredType::Simple(string_type_id))
            }
            ExpressionKind::Boolean(_) => {
                let boolean_type_id = self
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.Boolean");
                Some(StructuredType::Simple(boolean_type_id))
            }
            ExpressionKind::Atom(_) => {
                let atom_type_id = self
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.Atom");
                Some(StructuredType::Simple(atom_type_id))
            }
            // For other expression types, return None for now
            _ => None,
        }
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
                        typed_elements.push(self.create_typed_expression(
                            TypedExpressionKind::Placeholder(
                                "Failed to convert list element".to_string(),
                            ),
                            None,
                            expr.span,
                        ));
                    }
                }
                ListElement::Spread(_identifier) => {
                    // TODO: Handle spread elements in lists
                    typed_elements.push(self.create_typed_expression(
                        TypedExpressionKind::Placeholder(
                            "Spread elements not yet implemented".to_string(),
                        ),
                        None,
                        list.span,
                    ));
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
                    // Spread in maps requires variable type lookup and map compatibility checking
                    typed_entries.push(TypedMapEntry::Spread {
                        identifier: identifier.name.clone(),
                        spread_type: None, // Requires variable context for type resolution
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
        // Empty tuples are an error - not allowed in Outrun
        if tuple.elements.is_empty() {
            self.errors.push(TypeError::internal_with_span(
                "Empty tuples are not allowed. Use a specific type if you need a placeholder value.".to_string(),
                tuple.span.to_source_span(),
            ));
            return Some(TypedExpressionKind::Placeholder(
                "Empty tuple not allowed".to_string(),
            ));
        }

        let mut typed_elements = Vec::new();
        let mut element_types = Vec::new();
        let mut has_errors = false;

        // Process all tuple elements and collect their types
        for element in &tuple.elements {
            if let Some(typed_element) = self.convert_expression(element) {
                // Extract the concrete type from the typed element
                if let Some(element_type) = typed_element.structured_type.clone() {
                    element_types.push(element_type);
                } else {
                    // Element has no concrete type - cannot infer tuple type
                    has_errors = true;
                    element_types.push(StructuredType::TypeError {
                        error: TypeError::internal_with_span(
                            "Cannot infer type for tuple element".to_string(),
                            element.span.to_source_span(),
                        ),
                        fallback_type: None,
                        error_span: element.span,
                    });
                }
                typed_elements.push(typed_element);
            } else {
                // Failed to convert element - create placeholder
                has_errors = true;
                element_types.push(StructuredType::TypeError {
                    error: TypeError::internal_with_span(
                        "Failed to convert tuple element".to_string(),
                        element.span.to_source_span(),
                    ),
                    fallback_type: None,
                    error_span: element.span,
                });
                typed_elements.push(self.create_typed_expression(
                    TypedExpressionKind::Placeholder("Failed to convert tuple element".to_string()),
                    None,
                    element.span,
                ));
            }
        }

        // Compute composite tuple type from element types
        let tuple_type = if has_errors {
            // If any element failed, don't provide a tuple type
            None
        } else {
            // Create tuple type from concrete element types
            Some(StructuredType::Tuple(element_types))
        };

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
                            field_type: None, // Requires struct definition lookup for field types
                        });
                    } else {
                        return None; // Failed to convert field value
                    }
                }
                StructLiteralField::Shorthand(name) => {
                    typed_fields.push(TypedStructField::Shorthand {
                        name: name.name.clone(),
                        variable_type: None, // Requires variable context for type resolution
                    });
                }
                StructLiteralField::Spread(identifier) => {
                    typed_fields.push(TypedStructField::Spread {
                        identifier: identifier.name.clone(),
                        spread_type: None, // Requires variable context for type resolution
                    });
                }
            }
        }

        // Field validation requires struct definition lookup and type compatibility checking
        let struct_type = None; // Requires struct registry access for type computation

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
        let mut pattern_checker = PatternChecker::new(
            &mut self.context,
            &self.struct_registry,
            self.compiler_environment.as_ref().unwrap(),
        );

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

        // Get result type from type checking phase
        let result_type = self.context.expression_types.get(&if_expr.span).cloned();

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
        // Convert unified case expression
        let expression = if let Some(typed_expr) = self.convert_expression(&case_expr.expression) {
            Box::new(typed_expr)
        } else {
            return None;
        };

        // All case expressions use concrete pattern matching with the unified syntax
        let variant = TypedCaseVariant::Concrete {
            expression,
            when_clauses: case_expr
                .clauses
                .iter()
                .filter_map(|clause| {
                    // Convert each clause
                    let result = match &clause.result {
                        outrun_parser::CaseResult::Block(block) => self.convert_block(block)?,
                        outrun_parser::CaseResult::Expression(expr) => {
                            self.convert_expression(expr)?
                        }
                    };

                    // For unified case expressions, if there's no guard we need to create one
                    let guard = if let Some(guard_expr) = &clause.guard {
                        self.convert_expression(guard_expr)?
                    } else {
                        // Create a default "true" guard for pattern-only clauses
                        let boolean_type_id = self
                            .compiler_environment
                            .as_ref()
                            .unwrap()
                            .intern_type_name("Outrun.Core.Boolean");
                        TypedExpression {
                            kind: TypedExpressionKind::Boolean(true),
                            structured_type: Some(StructuredType::Simple(boolean_type_id)),
                            debug_info: Some(crate::checker::TypedDebugInfo {
                                comments: Vec::new(),
                                source_file: None,
                                original_span: clause.span,
                                type_annotations: Vec::new(),
                                inferred_types: std::collections::HashMap::new(),
                                literal_format: None,
                            }),
                            span: clause.span,
                        }
                    };

                    Some(TypedWhenClause {
                        guard: Box::new(guard),
                        result: Box::new(result),
                        bound_variables: self.extract_pattern_bound_variables(&clause.pattern),
                        span: clause.span,
                    })
                })
                .collect(),
        };

        // Get result type from type checking phase
        let result_type = self.context.expression_types.get(&case_expr.span).cloned();

        Some(TypedExpressionKind::CaseExpression {
            variant,
            result_type,
        })
    }

    /// Convert block to expression (for now, simple approach)
    fn convert_block(&mut self, block: &Block) -> Option<TypedExpression> {
        if block.statements.is_empty() {
            // Empty block - this should be a compile error in a functional language
            // since it has no value and cannot be typed
            self.errors.push(TypeError::EmptyBlock {
                span: block.span.to_source_span(),
                message: "Empty blocks are not allowed - every expression must have a value"
                    .to_string(),
            });
            return None;
        }

        // For now, just convert the last statement as an expression
        // TODO: Handle multiple statements properly
        if let Some(last_statement) = block.statements.last() {
            match &last_statement.kind {
                outrun_parser::StatementKind::Expression(expr) => self.convert_expression(expr),
                _ => Some(self.create_typed_expression(
                    TypedExpressionKind::Placeholder("Non-expression statement".to_string()),
                    None,
                    last_statement.span,
                )),
            }
        } else {
            None
        }
    }

    /// Convert function definition with parameter and body validation
    pub fn convert_function_definition(
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

        let typed_function = TypedFunctionDefinition {
            name: func_def.name.name.clone(),
            parameters: typed_parameters,
            return_type,
            guard,
            body,
            function_id,
            span: func_def.span,
        };

        // Update the compiler environment with the typed definition
        if let Some(compiler_env) = &self.compiler_environment {
            if let Some(module_key) = &self.current_module_key {
                // Use module-specific update for precise targeting
                compiler_env.update_function_with_typed_definition_in_module(
                    module_key,
                    &func_def.name.name,
                    typed_function.clone(),
                )
            } else {
                // Fall back to legacy behavior for standalone functions
                compiler_env.update_function_with_typed_definition(
                    &func_def.name.name,
                    typed_function.clone(),
                )
            };
        }

        Some(typed_function)
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
                    let type_id = self
                        .compiler_environment
                        .as_ref()
                        .unwrap()
                        .intern_type_name(&first_type.name);
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
                self.compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name(&name.join(".")),
            ))
        } else {
            // Generic struct - create generic type with parameter placeholders
            let param_types: Vec<StructuredType> = generic_params
                .iter()
                .map(|param| {
                    StructuredType::Simple(
                        self.compiler_environment
                            .as_ref()
                            .unwrap()
                            .intern_type_name(&param.name.clone()),
                    )
                })
                .collect();
            Some(StructuredType::Generic {
                base: self
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name(&name.join(".")),
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

        // Convert functions with generic context
        let mut typed_functions = Vec::new();
        for function in &struct_def.functions {
            if let Some(typed_function) = self.convert_function_definition(function) {
                typed_functions.push(typed_function);
            } else {
                self.pop_generic_context(); // Clean up on failure
                return None; // Failed to convert function
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
            functions: typed_functions,
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

        // Set module context for trait static functions
        let previous_module_key = self.current_module_key.clone();
        if let Some(compiler_env) = &self.compiler_environment {
            let trait_name = name.join(".");
            let trait_type_id = compiler_env.intern_type_name(&trait_name);
            self.current_module_key = Some(
                crate::compilation::compiler_environment::ModuleKey::Module(trait_type_id.hash),
            );
        }

        // Convert trait functions
        let mut typed_functions = Vec::new();
        for func in &trait_def.functions {
            if let Some(typed_func) = self.convert_trait_function(func) {
                typed_functions.push(typed_func);
            } else {
                // Restore previous module context on failure
                self.current_module_key = previous_module_key;
                return None; // Failed to convert function
            }
        }

        // Restore previous module context
        self.current_module_key = previous_module_key;

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

                let typed_trait_function = TypedTraitFunction::Signature {
                    name: sig.name.name.clone(),
                    parameters: typed_parameters.clone(),
                    return_type: return_type.clone(),
                    guard: guard.clone(),
                    span: sig.span,
                };

                // Create a typed function definition for trait signatures to register their types
                // BUT mark them as non-executable to prevent interpreter from trying to run them
                let empty_body = crate::checker::TypedBlock {
                    statements: Vec::new(),
                    result_type: None,
                    span: sig.span,
                };

                let typed_function_def = crate::checker::TypedFunctionDefinition {
                    name: sig.name.name.clone(),
                    parameters: typed_parameters
                        .iter()
                        .map(|p| crate::checker::TypedParameter {
                            name: p.name.clone(),
                            param_type: None, // TODO: Convert properly
                            span: p.span,
                        })
                        .collect(),
                    return_type: None, // TODO: Convert return type properly
                    guard: guard.clone(),
                    body: empty_body,
                    function_id: format!("trait_signature::{}", sig.name.name),
                    span: sig.span,
                };

                // Update the compiler environment with the typed definition for trait signatures
                // These are needed for the type system but should not be executed
                if let Some(compiler_env) = &self.compiler_environment {
                    if let Some(module_key) = &self.current_module_key {
                        // Use module-specific update for precise targeting
                        compiler_env.update_function_with_typed_definition_in_module(
                            module_key,
                            &sig.name.name,
                            typed_function_def,
                        )
                    } else {
                        // Fall back to legacy behavior for trait signatures
                        compiler_env.update_function_with_typed_definition(
                            &sig.name.name,
                            typed_function_def,
                        )
                    };
                }

                Some(typed_trait_function)
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

                // Create typed function definition for static functions
                let typed_function_def = crate::checker::TypedFunctionDefinition {
                    name: static_def.name.name.clone(),
                    parameters: typed_parameters.clone(),
                    return_type: return_type.clone(),
                    guard: None,
                    body: body.clone(),
                    function_id: format!("static_trait::{}", static_def.name.name),
                    span: static_def.span,
                };

                // Update the compiler environment with the typed definition for static functions
                if let Some(compiler_env) = &self.compiler_environment {
                    let function_name = &static_def.name.name;

                    let _updated = if let Some(module_key) = &self.current_module_key {
                        // Use module-specific update for precise targeting
                        compiler_env.update_function_with_typed_definition_in_module(
                            module_key,
                            function_name,
                            typed_function_def,
                        )
                    } else {
                        // Fall back to global update
                        compiler_env.update_function_with_typed_definition(
                            function_name,
                            typed_function_def,
                        )
                    };
                }

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

        // Resolve implementation type using the same method as trait registration
        let impl_type = if type_path.is_empty() {
            None
        } else {
            match self
                .compiler_environment
                .as_ref()
                .unwrap()
                .convert_type_spec_to_structured_type(&impl_block.type_spec)
            {
                Ok(structured_type) => Some(structured_type),
                Err(_) => {
                    // Fallback to simple type if conversion fails
                    Some(StructuredType::Simple(
                        self.compiler_environment
                            .as_ref()
                            .unwrap()
                            .intern_type_name(&type_path.join(".")),
                    ))
                }
            }
        };

        // Resolve trait type using the same method as trait registration
        let trait_type = if trait_path.is_empty() {
            None
        } else {
            match self
                .compiler_environment
                .as_ref()
                .unwrap()
                .convert_type_spec_to_structured_type(&impl_block.trait_spec)
            {
                Ok(structured_type) => Some(structured_type),
                Err(_) => {
                    // Fallback to simple type if conversion fails
                    Some(StructuredType::Simple(
                        self.compiler_environment
                            .as_ref()
                            .unwrap()
                            .intern_type_name(&trait_path.join(".")),
                    ))
                }
            }
        };

        // Create generic context for method resolution with Self type
        let generic_context = self.create_generic_context(&generic_params, impl_type.clone());
        self.push_generic_context(generic_context);

        // Convert constraints (TODO: implement constraint parsing)
        let constraints = Vec::new(); // Placeholder for now

        // Set module context for trait implementation functions
        let previous_module_key = self.current_module_key.clone();
        if let (Some(trait_type), Some(impl_type)) = (&trait_type, &impl_type) {
            // IMPORTANT: Use the base trait type for module key consistency
            // During registration, traits are stored with Simple types (e.g., Simple(Option))
            // but resolved type specs create Generic types (e.g., Generic { base: Option, args: [T] })
            // We need to extract the base type to match the registration module key
            let module_trait_type = match trait_type {
                StructuredType::Generic { base, .. } => StructuredType::Simple(base.clone()),
                other => other.clone(),
            };

            self.current_module_key = Some(
                crate::compilation::compiler_environment::ModuleKey::TraitImpl(
                    Box::new(module_trait_type),
                    Box::new(impl_type.clone()),
                ),
            );
        }

        // Convert functions with Self type resolution
        let mut typed_functions = Vec::new();
        for function in &impl_block.functions {
            if let Some(typed_function) = self.convert_function_definition(function) {
                typed_functions.push(typed_function);
            } else {
                self.current_module_key = previous_module_key; // Restore on failure
                self.pop_generic_context(); // Clean up on failure
                return None; // Failed to convert function
            }
        }

        // Restore previous module context
        self.current_module_key = previous_module_key;

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
            functions: typed_functions,
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
                        base: self
                            .compiler_environment
                            .as_ref()
                            .unwrap()
                            .intern_type_name(&path.join(".")),
                        args: resolved_args,
                    })
                } else {
                    // Simple type without generics
                    Some(StructuredType::Simple(
                        self.compiler_environment
                            .as_ref()
                            .unwrap()
                            .intern_type_name(&path.join(".")),
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
                            name: self
                                .compiler_environment
                                .as_ref()
                                .unwrap()
                                .intern_atom_name(&param.name.clone()),
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

    /// Error Recovery Methods for Production-Quality Error Handling
    ///
    /// Record an error recovery attempt
    pub(crate) fn record_error_recovery(
        &mut self,
        error: crate::error::TypeError,
        context: crate::checker::ErrorContext,
        recovery_span: Span,
        strategy: crate::checker::RecoveryStrategy,
        successful: bool,
    ) {
        let recovery_info = crate::checker::ErrorRecoveryInfo {
            error,
            error_context: context,
            recovery_span,
            recovery_strategy: strategy,
            recovery_successful: successful,
        };
        self.error_recovery_info.push(recovery_info);
    }

    /// Create a TypeError expression for error recovery
    pub(crate) fn create_error_expression(
        &mut self,
        error: crate::error::TypeError,
        fallback_type: Option<StructuredType>,
        recovery_expression: Option<Box<TypedExpression>>,
        span: Span,
        context: crate::checker::ErrorContext,
    ) -> TypedExpression {
        // Precompute placeholder type to avoid borrow conflicts
        let placeholder_type_id = self
            .compiler_environment
            .as_ref()
            .unwrap()
            .intern_type_name("Outrun.Core.PlaceholderType");
        let default_placeholder_type = StructuredType::Simple(placeholder_type_id);

        // Record the error recovery attempt
        let strategy = match (&fallback_type, &recovery_expression) {
            (Some(ft), None) => crate::checker::RecoveryStrategy::FallbackType {
                fallback_type: ft.clone(),
            },
            (_, Some(_)) => crate::checker::RecoveryStrategy::PlaceholderExpression {
                placeholder_type: fallback_type
                    .clone()
                    .unwrap_or_else(|| default_placeholder_type.clone()),
            },
            _ => crate::checker::RecoveryStrategy::NoRecovery,
        };

        self.record_error_recovery(error.clone(), context, span, strategy, true);

        // Create the error expression
        self.create_typed_expression(
            TypedExpressionKind::TypeError {
                error,
                fallback_type: fallback_type.clone(),
                recovery_expression,
            },
            fallback_type,
            span,
        )
    }

    /// Attempt to recover from expression conversion failure
    pub(crate) fn recover_expression_failure(
        &mut self,
        expr: &Expression,
        error: crate::error::TypeError,
    ) -> TypedExpression {
        // Try to infer a reasonable fallback type based on expression kind
        let fallback_type = self.infer_fallback_type_for_expression(expr);
        let context = crate::checker::ErrorContext::Expression {
            expression_type: format!("{:?}", std::mem::discriminant(&expr.kind)),
        };

        self.create_error_expression(error, fallback_type, None, expr.span, context)
    }

    /// Infer a reasonable fallback type for an expression based on its structure
    fn infer_fallback_type_for_expression(&mut self, expr: &Expression) -> Option<StructuredType> {
        use outrun_parser::ExpressionKind;
        match &expr.kind {
            ExpressionKind::Integer(_) => Some(StructuredType::Simple(
                self.compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.Integer64"),
            )),
            ExpressionKind::Float(_) => Some(StructuredType::Simple(
                self.compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.Float64"),
            )),
            ExpressionKind::String(_) => Some(StructuredType::Simple(
                self.compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.String"),
            )),
            ExpressionKind::Boolean(_) => Some(StructuredType::Simple(
                self.compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.Boolean"),
            )),
            ExpressionKind::Atom(_) => Some(StructuredType::Simple(
                self.compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.Atom"),
            )),
            ExpressionKind::List(_) => Some(StructuredType::Generic {
                base: self
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("List"),
                args: vec![StructuredType::Simple(
                    self.compiler_environment
                        .as_ref()
                        .unwrap()
                        .intern_type_name("Outrun.Core.PlaceholderType"),
                )],
            }),
            ExpressionKind::Map(_) => Some(StructuredType::Generic {
                base: self
                    .compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Map"),
                args: vec![
                    StructuredType::Simple(
                        self.compiler_environment
                            .as_ref()
                            .unwrap()
                            .intern_type_name("Outrun.Core.PlaceholderType"),
                    ),
                    StructuredType::Simple(
                        self.compiler_environment
                            .as_ref()
                            .unwrap()
                            .intern_type_name("Outrun.Core.PlaceholderType"),
                    ),
                ],
            }),
            ExpressionKind::Tuple(_) => {
                // For tuples, we can't easily determine the element types, so use a generic placeholder
                Some(StructuredType::Simple(
                    self.compiler_environment
                        .as_ref()
                        .unwrap()
                        .intern_type_name("Outrun.Core.PlaceholderType"),
                ))
            }
            _ => Some(StructuredType::Simple(
                self.compiler_environment
                    .as_ref()
                    .unwrap()
                    .intern_type_name("Outrun.Core.PlaceholderType"),
            )),
        }
    }

    /// Record timing information for a phase
    pub(crate) fn record_phase_timing(&mut self, phase_name: &str, duration_ms: u64) {
        self.phase_timings
            .insert(phase_name.to_string(), duration_ms);
    }

    /// Generate detailed compilation summary
    pub(crate) fn generate_compilation_summary(
        &self,
        total_items: usize,
        successful_items: usize,
    ) -> crate::checker::CompilationSummary {
        let compilation_time_ms = self.compilation_start_time.elapsed().as_millis() as u64;
        let error_items = total_items - successful_items;
        let recovered_items = self.error_recovery_info.len();

        // Estimate memory usage (simplified)
        let estimated_ast_memory = total_items * 1024; // Rough estimate: 1KB per item
        let estimated_context_memory = 1024; // FIXME: no equivalent method on CompilerEnvironment
        let estimated_registry_memory = self
            .compiler_environment
            .as_ref()
            .map(|ce| ce.function_count())
            .unwrap_or(0)
            * 512; // 512 bytes per function

        crate::checker::CompilationSummary {
            total_items,
            successful_items,
            error_items,
            recovered_items,
            compilation_time_ms,
            memory_usage: crate::checker::MemoryUsage {
                peak_memory_bytes: estimated_ast_memory
                    + estimated_context_memory
                    + estimated_registry_memory,
                typed_ast_memory_bytes: estimated_ast_memory,
                type_context_memory_bytes: estimated_context_memory,
                function_registry_memory_bytes: estimated_registry_memory,
            },
            phase_timings: self.phase_timings.clone(),
        }
    }

    /// Extract bound variables from a pattern for scope registration
    fn extract_pattern_bound_variables(
        &self,
        pattern: &outrun_parser::Pattern,
    ) -> Vec<crate::patterns::BoundVariable> {
        // Simple pattern variable extraction for typed AST building
        // This provides basic bound variable information for scope management
        let mut variables = Vec::new();
        Self::extract_pattern_variables_recursive(pattern, &mut variables);
        variables
    }

    /// Recursively extract variable names from patterns
    fn extract_pattern_variables_recursive(
        pattern: &outrun_parser::Pattern,
        variables: &mut Vec<crate::patterns::BoundVariable>,
    ) {
        match pattern {
            outrun_parser::Pattern::Identifier(ident) => {
                variables.push(crate::patterns::BoundVariable {
                    name: ident.name.clone(),
                    variable_type: None, // Type resolution would happen in full pattern checking
                    span: ident.span,
                });
            }
            outrun_parser::Pattern::Tuple(tuple_pattern) => {
                for element in &tuple_pattern.elements {
                    Self::extract_pattern_variables_recursive(element, variables);
                }
            }
            outrun_parser::Pattern::Struct(struct_pattern) => {
                for field in &struct_pattern.fields {
                    match &field.pattern {
                        Some(pattern) => {
                            Self::extract_pattern_variables_recursive(pattern, variables);
                        }
                        None => {
                            // Shorthand field - binds variable with field name
                            variables.push(crate::patterns::BoundVariable {
                                name: field.name.name.clone(),
                                variable_type: None,
                                span: field.name.span,
                            });
                        }
                    }
                }
            }
            outrun_parser::Pattern::List(list_pattern) => {
                for element in &list_pattern.elements {
                    Self::extract_pattern_variables_recursive(element, variables);
                }
                if let Some(rest) = &list_pattern.rest {
                    variables.push(crate::patterns::BoundVariable {
                        name: rest.name.clone(),
                        variable_type: None,
                        span: rest.span,
                    });
                }
            }
            outrun_parser::Pattern::Literal(_) => {
                // Literal patterns don't bind variables
            }
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
