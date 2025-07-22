//! New simplified type checker using visitor pattern and multi-program compilation

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::CompilationResult;
use crate::error::TypeError;
use crate::unification::{StructuredType, UnificationContext};
use outrun_parser::{Program, Span};
use std::collections::HashMap;

/// Function path variants for typed function calls
#[derive(Debug, Clone, PartialEq)]
pub enum TypedFunctionPath {
    Simple { name: String },
    Qualified { module: String, name: String },
    Expression { expression: Box<TypedExpression> },
}

/// Typed function argument with resolved types
#[derive(Debug, Clone, PartialEq)]
pub struct TypedArgument {
    pub name: String,
    pub expression: TypedExpression,
    pub argument_type: Option<StructuredType>, // None = type not yet resolved
    pub span: Span,
}

/// Dispatch strategy for function calls
#[derive(Debug, Clone, PartialEq)]
pub enum DispatchMethod {
    /// Static function call - resolved at compile time
    Static {
        function_id: String, // Reference to function registry
    },
    /// Protocol function call - dispatched at runtime
    Protocol {
        protocol_name: String,
        function_name: String,
        impl_type: Box<StructuredType>,
    },
}

/// Typed map entry with resolved types
#[derive(Debug, Clone, PartialEq)]
pub enum TypedMapEntry {
    /// Key-value assignment: key: value or key => value
    Assignment {
        key: Box<TypedExpression>,
        value: Box<TypedExpression>,
        key_type: Option<StructuredType>,
        value_type: Option<StructuredType>,
    },
    /// Shorthand assignment: name (implies name: name)
    Shorthand {
        name: String,
        value: Box<TypedExpression>,
        value_type: Option<StructuredType>,
    },
    /// Spread operator: ..identifier
    Spread {
        identifier: String,
        spread_type: Option<StructuredType>, // Type of the spread expression
    },
}

/// Typed struct field with resolved types
#[derive(Debug, Clone, PartialEq)]
pub enum TypedStructField {
    /// Explicit field assignment: field_name: expression
    Assignment {
        name: String,
        expression: Box<TypedExpression>,
        field_type: Option<StructuredType>,
    },
    /// Shorthand field: field_name (implies field_name: field_name)
    Shorthand {
        name: String,
        variable_type: Option<StructuredType>,
    },
    /// Spread operator: ..identifier
    Spread {
        identifier: String,
        spread_type: Option<StructuredType>, // Type of the spread expression
    },
}

/// Typed expression with structured type information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression {
    pub kind: TypedExpressionKind,
    pub structured_type: Option<StructuredType>, // None = type not yet resolved
    pub span: Span,
    pub debug_info: Option<TypedDebugInfo>, // Optional debug information
}

/// Typed expression kinds with core language support
#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpressionKind {
    // Basic literals
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Atom(String),
    Identifier(String),

    // Function calls (includes desugared binary/unary operations)
    FunctionCall {
        function_path: TypedFunctionPath,
        arguments: Vec<TypedArgument>,
        dispatch_strategy: DispatchMethod,
    },

    // Field access
    FieldAccess {
        object: Box<TypedExpression>,
        field: String,
        field_type: Option<StructuredType>, // None = type not yet resolved
    },

    // Collection literals
    List {
        elements: Vec<TypedExpression>,
        element_type: Option<StructuredType>, // Homogeneous type validation
    },
    Map {
        entries: Vec<TypedMapEntry>,
        key_type: Option<StructuredType>,
        value_type: Option<StructuredType>,
    },
    Tuple {
        elements: Vec<TypedExpression>,
        tuple_type: Option<StructuredType>, // Composite tuple type
    },
    StructLiteral {
        type_path: Vec<String>,
        fields: Vec<TypedStructField>,
        struct_type: Option<StructuredType>,
    },

    // Control flow expressions
    IfExpression {
        condition: Box<TypedExpression>,
        then_branch: Box<TypedExpression>,
        else_branch: Option<Box<TypedExpression>>,
        result_type: Option<StructuredType>, // Unified type of all branches
    },
    CaseExpression {
        variant: TypedCaseVariant,
        result_type: Option<StructuredType>, // Unified type of all branches
    },

    // Function expressions
    AnonymousFunction(TypedAnonymousFunction),

    // Macro and advanced syntax
    MacroInjection {
        parameter: String,
        injected_expression: Option<Box<TypedExpression>>, // Resolved macro content
        original_span: Span,                               // Original injection location
    },

    // Error recovery for malformed expressions
    TypeError {
        error: crate::error::TypeError,
        fallback_type: Option<StructuredType>,
        recovery_expression: Option<Box<TypedExpression>>,
    },

    // Placeholder for unsupported expressions (temporary)
    Placeholder(String),
}

/// Case expression variants for typed AST
#[derive(Debug, Clone, PartialEq)]
pub enum TypedCaseVariant {
    /// Concrete case: pattern-based matching with exhaustiveness checking
    Concrete {
        expression: Box<TypedExpression>,
        when_clauses: Vec<TypedWhenClause>,
    },
    /// Protocol case: protocol implementation dispatch
    Protocol {
        expression: Box<TypedExpression>,
        protocol_name: String,
        as_clauses: Vec<TypedAsClause>,
    },
}

/// When clause for concrete case expressions with pattern matching
#[derive(Debug, Clone, PartialEq)]
pub struct TypedWhenClause {
    pub guard: Box<TypedExpression>,
    pub result: Box<TypedExpression>,
    pub bound_variables: Vec<crate::patterns::BoundVariable>, // Variables from guard patterns
    pub span: Span,
}

/// As clause for protocol case expressions with type checking
#[derive(Debug, Clone, PartialEq)]
pub struct TypedAsClause {
    pub type_path: Vec<String>, // Type implementing the protocol
    pub pattern: Option<crate::patterns::TypedPattern>, // Optional destructuring pattern
    pub result: Box<TypedExpression>,
    pub impl_verified: bool, // Whether protocol implementation was verified
    pub span: Span,
}

/// Simplified typed item
#[derive(Debug, Clone)]
pub struct TypedItem {
    pub kind: TypedItemKind,
    pub span: Span,
    pub debug_info: Option<TypedDebugInfo>, // Optional debug information
}

/// Function definition with complete type information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedFunctionDefinition {
    pub name: String,
    pub parameters: Vec<TypedParameter>,
    pub return_type: Option<StructuredType>,
    pub guard: Option<Box<TypedExpression>>, // Guard must return Boolean
    pub body: TypedBlock,
    pub function_id: String, // Reference to function registry
    pub span: outrun_parser::Span,
}

/// Function parameter with resolved type information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedParameter {
    pub name: String,
    pub param_type: Option<StructuredType>,
    pub span: outrun_parser::Span,
}

/// Block of statements with result type
#[derive(Debug, Clone, PartialEq)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub result_type: Option<StructuredType>, // Type of the last expression/statement
    pub span: outrun_parser::Span,
}

/// Statements within blocks
#[derive(Debug, Clone, PartialEq)]
pub enum TypedStatement {
    /// Expression statement
    Expression(Box<TypedExpression>),
    /// Let binding statement
    LetBinding(Box<TypedLetBinding>),
}

/// Let binding with pattern and type information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedLetBinding {
    pub pattern: crate::patterns::TypedPattern,
    pub expression: Box<TypedExpression>,
    pub binding_type: Option<StructuredType>, // Type of the bound expression
    pub span: outrun_parser::Span,
}

/// Anonymous function with multiple clauses
#[derive(Debug, Clone, PartialEq)]
pub struct TypedAnonymousFunction {
    pub clauses: Vec<TypedAnonymousClause>,
    pub function_type: Option<StructuredType>, // Overall function type
    pub span: outrun_parser::Span,
}

/// Single clause of an anonymous function
#[derive(Debug, Clone, PartialEq)]
pub struct TypedAnonymousClause {
    pub parameters: Vec<TypedParameter>, // Anonymous parameters (may be patterns)
    pub guard: Option<Box<TypedExpression>>, // Optional guard expression
    pub body: Box<TypedExpression>,      // Body expression
    pub clause_type: Option<StructuredType>, // Type of this specific clause
    pub span: outrun_parser::Span,
}

/// Typed struct definition with field validation
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructDefinition {
    pub name: Vec<String>,                       // Module path for the struct
    pub generic_params: Vec<TypedGenericParam>,  // Generic parameters with resolved constraints
    pub fields: Vec<TypedStructFieldDefinition>, // Validated field definitions
    pub functions: Vec<TypedFunctionDefinition>, // Functions defined within the struct
    pub struct_id: String,                       // Reference to type registry
    pub span: outrun_parser::Span,
}

/// Typed struct field definition with validated type
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructFieldDefinition {
    pub name: String,
    pub field_type: Option<StructuredType>, // Resolved field type
    pub span: outrun_parser::Span,
}

/// Typed protocol definition with signature validation
#[derive(Debug, Clone, PartialEq)]
pub struct TypedProtocolDefinition {
    pub name: Vec<String>,                      // Module path for the protocol
    pub generic_params: Vec<TypedGenericParam>, // Generic parameters with constraints
    pub constraints: Vec<TypedConstraint>,      // Validated protocol constraints
    pub functions: Vec<TypedProtocolFunction>,  // Validated protocol functions
    pub protocol_id: String,                    // Reference to protocol registry
    pub span: outrun_parser::Span,
}

/// Typed protocol function variants
#[derive(Debug, Clone, PartialEq)]
pub enum TypedProtocolFunction {
    /// Function signature without implementation
    Signature {
        name: String,
        parameters: Vec<TypedParameter>,
        return_type: Option<StructuredType>,
        guard: Option<Box<TypedExpression>>,
        span: outrun_parser::Span,
    },
    /// Function with default implementation
    Definition(TypedFunctionDefinition),
    /// Static function definition
    StaticDefinition {
        name: String,
        parameters: Vec<TypedParameter>,
        return_type: Option<StructuredType>,
        body: TypedBlock,
        span: outrun_parser::Span,
    },
}

/// Typed impl block with protocol validation
#[derive(Debug, Clone, PartialEq)]
pub struct TypedImplBlock {
    pub generic_params: Vec<TypedGenericParam>, // Generic parameters
    pub protocol_path: Vec<String>,             // Protocol being implemented
    pub type_path: Vec<String>,                 // Type implementing the protocol
    pub protocol_type: Option<StructuredType>,  // Resolved protocol type
    pub impl_type: Option<StructuredType>,      // Resolved implementation type
    pub constraints: Vec<TypedConstraint>,      // Validated constraints
    pub functions: Vec<TypedFunctionDefinition>, // Implementation functions
    pub impl_verified: bool,                    // Whether protocol implementation is valid
    pub span: outrun_parser::Span,
}

/// Typed const definition with value validation
#[derive(Debug, Clone, PartialEq)]
pub struct TypedConstDefinition {
    pub name: String,
    pub const_type: Option<StructuredType>, // Resolved constant type
    pub expression: Box<TypedExpression>,   // Validated constant expression
    pub const_id: String,                   // Reference to constant registry
    pub span: outrun_parser::Span,
}

/// Generic parameter with type constraints
#[derive(Debug, Clone, PartialEq)]
pub struct TypedGenericParam {
    pub name: String,
    pub constraints: Vec<TypedConstraint>, // Protocol constraints for this parameter
    pub span: outrun_parser::Span,
}

/// Type constraint (e.g., T: Orderable)
#[derive(Debug, Clone, PartialEq)]
pub struct TypedConstraint {
    pub param_name: String,         // Generic parameter being constrained
    pub protocol_path: Vec<String>, // Protocol that must be implemented
    pub protocol_type: Option<StructuredType>, // Resolved protocol type
    pub span: outrun_parser::Span,
}

/// Generic context for type resolution and substitution
#[derive(Debug, Clone, PartialEq)]
pub struct TypedGenericContext {
    pub generic_params: Vec<TypedGenericParam>, // Available generic parameters
    pub constraints: Vec<TypedConstraint>,      // Constraints for all parameters
    pub substitutions: std::collections::HashMap<String, StructuredType>, // Type substitutions
    pub self_type: Option<StructuredType>,      // Self type for impl blocks
}

/// Comprehensive type annotation with resolved type information
#[derive(Debug, Clone, PartialEq)]
pub struct TypedTypeAnnotation {
    pub annotation_kind: TypedTypeAnnotationKind,
    pub resolved_type: Option<StructuredType>, // Resolved structured type
    pub span: outrun_parser::Span,
}

/// Type annotation variants with full language support
#[derive(Debug, Clone, PartialEq)]
pub enum TypedTypeAnnotationKind {
    /// Simple type with optional generic arguments: Module.Type<Args>
    Simple {
        path: Vec<String>,
        generic_args: Vec<TypedTypeAnnotation>,
    },
    /// Tuple type: (Type1, Type2, ...)
    Tuple(Vec<TypedTypeAnnotation>),
    /// Function type: (param1: Type1, param2: Type2) -> ReturnType
    Function {
        params: Vec<TypedFunctionTypeParam>,
        return_type: Box<TypedTypeAnnotation>,
    },
}

/// Function type parameter with name and type
#[derive(Debug, Clone, PartialEq)]
pub struct TypedFunctionTypeParam {
    pub name: String,
    pub param_type: TypedTypeAnnotation,
    pub span: outrun_parser::Span,
}

/// Typed macro definition with parameter validation
#[derive(Debug, Clone, PartialEq)]
pub struct TypedMacroDefinition {
    pub name: String,
    pub parameters: Vec<String>, // Parameter names
    pub body: TypedBlock,
    pub hygiene_scope: Option<String>, // Hygiene scope identifier (future)
    pub span: Span,
}

/// Debug information for source preservation and IDE support
#[derive(Debug, Clone, PartialEq)]
pub struct TypedDebugInfo {
    /// Comments attached to this node or its vicinity
    pub comments: Vec<AttachedComment>,
    /// Original source file this node came from
    pub source_file: Option<String>,
    /// Original span in the source code
    pub original_span: Span,
    /// Type annotations with resolution information
    pub type_annotations: Vec<TypeAnnotationInfo>,
    /// Inferred types for sub-expressions (position -> type mapping)
    /// Using (start, end) tuple instead of Span since Span doesn't implement Hash/Eq
    pub inferred_types: std::collections::HashMap<(usize, usize), StructuredType>,
    /// Original format information for literals (for formatters and IDEs)
    pub literal_format: Option<LiteralFormatInfo>,
}

impl Default for TypedDebugInfo {
    fn default() -> Self {
        Self {
            comments: Vec::new(),
            source_file: None,
            original_span: Span::new(0, 0),
            type_annotations: Vec::new(),
            inferred_types: std::collections::HashMap::new(),
            literal_format: None,
        }
    }
}

/// Comment with spatial relationship to a typed node
#[derive(Debug, Clone, PartialEq)]
pub struct AttachedComment {
    /// The comment content and metadata
    pub comment: outrun_parser::Comment,
    /// Relationship of this comment to the node
    pub attachment: CommentAttachment,
}

/// Spatial relationship between a comment and a typed node
#[derive(Debug, Clone, PartialEq)]
pub enum CommentAttachment {
    /// Comment appears immediately before the node (documentation comment)
    Preceding,
    /// Comment appears immediately after the node (trailing comment)
    Trailing,
    /// Comment appears within the node's span but not directly adjacent
    Internal,
    /// Comment is within the same scope but no clear relationship
    Scope,
}

/// Information about type annotations and their resolution
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotationInfo {
    /// Source span of the original annotation
    pub span: Span,
    /// Type as declared by the user
    pub declared_type: StructuredType,
    /// Type as inferred/resolved by the type checker
    pub inferred_type: StructuredType,
    /// Source of this annotation (explicit vs inferred)
    pub annotation_source: AnnotationSource,
}

/// Source of a type annotation
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationSource {
    /// Explicitly written by the user
    Explicit,
    /// Inferred by the type checker
    Inferred,
    /// Default from function signature
    DefaultFromSignature,
    /// Propagated from context (e.g., return type from function signature)
    Propagated,
}

/// Original format information for literals (IDE support)
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralFormatInfo {
    /// Original text representation of the literal
    pub original_text: String,
    /// Format-specific information
    pub format_details: LiteralFormatDetails,
}

/// Format details for different literal types
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralFormatDetails {
    /// Integer format information
    Integer {
        format: outrun_parser::IntegerFormat,
        raw_digits: String,
    },
    /// Float format information
    Float {
        format: outrun_parser::FloatFormat,
        raw_number: String,
    },
    /// String format information
    String {
        format: outrun_parser::StringFormat,
        delimiter_style: String, // e.g., "\"", "\"\"\"", etc.
        was_interpolated: bool,  // True if this was originally string interpolation
        interpolation_parts: Option<Vec<InterpolationPart>>, // Original interpolation structure
    },
    /// Other literal types
    Other,
}

/// Information about a part of string interpolation (for reconstruction)
#[derive(Debug, Clone, PartialEq)]
pub enum InterpolationPart {
    /// Text portion of the interpolated string
    Text {
        content: String,
        original_span: Span,
    },
    /// Expression portion that was interpolated
    Expression {
        /// The original expression before desugaring
        original_expression_text: String,
        /// Span of the expression in the original source
        original_span: Span,
        /// The desugared function call that replaced this
        desugared_span: Span,
    },
}

/// Typed item kinds supporting full language constructs
#[derive(Debug, Clone)]
pub enum TypedItemKind {
    Expression(Box<TypedExpression>),
    FunctionDefinition(TypedFunctionDefinition),
    StructDefinition(TypedStructDefinition),
    ProtocolDefinition(TypedProtocolDefinition),
    ImplBlock(TypedImplBlock),
    ConstDefinition(TypedConstDefinition),
    LetBinding(Box<TypedLetBinding>),
    MacroDefinition(TypedMacroDefinition),
    Placeholder(String), // For debugging - will be removed
}

/// Error recovery information for production-quality error handling
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorRecoveryInfo {
    /// The original error that occurred
    pub error: crate::error::TypeError,
    /// Context where the error occurred (e.g., "function parameter", "let binding")
    pub error_context: ErrorContext,
    /// Span where recovery was attempted
    pub recovery_span: Span,
    /// Strategy used for recovery
    pub recovery_strategy: RecoveryStrategy,
    /// Whether recovery was successful (partial type checking)
    pub recovery_successful: bool,
}

/// Context information for error recovery
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorContext {
    /// Error in function parameter type checking
    FunctionParameter {
        function_name: String,
        parameter_name: String,
    },
    /// Error in function body type checking
    FunctionBody { function_name: String },
    /// Error in let binding type checking
    LetBinding { variable_name: String },
    /// Error in expression type checking
    Expression { expression_type: String },
    /// Error in struct field type checking
    StructField {
        struct_name: String,
        field_name: String,
    },
    /// Error in protocol implementation checking
    ProtocolImplementation {
        protocol_name: String,
        type_name: String,
    },
    /// Error in pattern matching type checking
    PatternMatching { pattern_type: String },
    /// General context not covered by specific cases
    General { description: String },
}

/// Strategy used for error recovery
#[derive(Debug, Clone, PartialEq)]
pub enum RecoveryStrategy {
    /// Skip the problematic node and continue
    Skip,
    /// Use a fallback type and continue type checking
    FallbackType { fallback_type: StructuredType },
    /// Use a placeholder expression with known type
    PlaceholderExpression { placeholder_type: StructuredType },
    /// Partial type checking - check what we can, mark rest as errors
    PartialChecking,
    /// No recovery attempted - propagate error
    NoRecovery,
}

/// Compilation summary with detailed metrics
#[derive(Debug, Clone, PartialEq)]
pub struct CompilationSummary {
    /// Total number of items processed
    pub total_items: usize,
    /// Number of successful type checks
    pub successful_items: usize,
    /// Number of items with errors
    pub error_items: usize,
    /// Number of items that used error recovery
    pub recovered_items: usize,
    /// Total compilation time in milliseconds
    pub compilation_time_ms: u64,
    /// Memory usage statistics
    pub memory_usage: MemoryUsage,
    /// Per-phase timing information
    pub phase_timings: std::collections::HashMap<String, u64>,
}

/// Memory usage statistics for performance monitoring
#[derive(Debug, Clone, PartialEq)]
pub struct MemoryUsage {
    /// Peak memory usage during compilation (bytes)
    pub peak_memory_bytes: usize,
    /// Memory used by typed AST (bytes)
    pub typed_ast_memory_bytes: usize,
    /// Memory used by type context (bytes)
    pub type_context_memory_bytes: usize,
    /// Memory used by function registry (bytes)
    pub function_registry_memory_bytes: usize,
}

/// Simplified typed program with essential compilation artifacts
#[derive(Debug, Clone)]
pub struct TypedProgram {
    /// Top-level typed items in the program
    pub items: Vec<TypedItem>,
    /// Type checking context with resolved types
    pub type_context: UnificationContext,
    /// Compilation order for dependencies
    pub compilation_order: Vec<String>,
    /// Summary of compilation (for debugging)
    pub compilation_summary: String,
    /// Program-level debug information
    pub debug_info: TypedDebugInfo,
    /// Error recovery information for production-quality error handling
    pub error_recovery_info: Vec<ErrorRecoveryInfo>,
    /// Detailed compilation metrics and performance data
    pub detailed_summary: Option<CompilationSummary>,
}

/// Main type checker that uses the new multi-program visitor architecture
#[derive(Debug)]
pub struct TypeChecker {
    compiler_env: CompilerEnvironment,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    /// Create a new type checker
    pub fn new() -> Self {
        Self {
            compiler_env: CompilerEnvironment::new(),
        }
    }

    /// Get the compiler environment from the type checker
    /// This provides access to all loaded functions including core library
    pub fn compiler_environment(
        &self,
    ) -> &crate::compilation::compiler_environment::CompilerEnvironment {
        &self.compiler_env
    }

    /// Convert CompilationResult to TypedProgram (simplified)
    fn compilation_result_to_typed_program(
        compilation_result: CompilationResult,
        original_program: &Program,
    ) -> TypedProgram {
        // Convert top-level items from the original program (simplified)
        let typed_items: Vec<TypedItem> = original_program
            .items
            .iter()
            .filter_map(|item| Self::convert_item_simple(item, &compilation_result.type_context))
            .collect();

        let compilation_summary = format!(
            "Compiled {} items, {} protocols, {} structs, {} implementations",
            typed_items.len(),
            compilation_result.protocols.len(),
            compilation_result.structs.len(),
            compilation_result.implementations.len()
        );

        TypedProgram {
            items: typed_items,
            type_context: compilation_result.type_context,
            compilation_order: compilation_result.compilation_order,
            compilation_summary,
            debug_info: TypedDebugInfo {
                comments: Vec::new(), // Will be populated by comment attachment algorithm
                source_file: original_program.debug_info.source_file.clone(),
                original_span: original_program.span,
                type_annotations: Vec::new(),
                inferred_types: std::collections::HashMap::new(),
                literal_format: None,
            },
            error_recovery_info: Vec::new(), // Will be populated by error recovery system
            detailed_summary: None,          // Will be populated by performance monitoring
        }
    }

    /// Type check a single program using the new architecture
    pub fn check_program(&mut self, program: &Program) -> Result<TypedProgram, Vec<TypeError>> {
        // Load the core library collection
        let mut collection = crate::core_library::load_core_library_collection();

        // Add the user program to the collection
        collection.add_program(
            "main.outrun".to_string(),
            program.clone(),
            format!("{program}"), // Use Display for source reconstruction
        );

        // Compile using the compiler environment
        match self.compiler_env.compile_collection(collection) {
            Ok(result) => {
                // Use the comprehensive TypedProgram created by TypedASTBuilder
                // instead of creating a simplified one
                if let Some(typed_program) = result.typed_programs.get("main.outrun") {
                    Ok(typed_program.clone())
                } else {
                    // Fallback to simplified conversion if no typed program was created
                    let typed_program = Self::compilation_result_to_typed_program(result, program);
                    Ok(typed_program)
                }
            }
            Err(errors) => Err(errors),
        }
    }

    /// Create a type checker with core library bootstrapped
    pub fn new_bootstrapped_or_fail(
        _source: &str,
        _filename: &str,
    ) -> Result<Self, crate::error::TypeErrorReport> {
        // Use the globally cached core library compilation
        let core_compilation = crate::core_library::get_core_library_compilation();

        // Create a compiler environment with the cached compilation results
        let mut compiler_env = CompilerEnvironment::new();
        compiler_env.load_compilation_result(core_compilation.clone());

        Ok(Self { compiler_env })
    }

    /// Convert parser Item to TypedItem (simplified)
    fn convert_item_simple(
        item: &outrun_parser::Item,
        type_context: &UnificationContext,
    ) -> Option<TypedItem> {
        use outrun_parser::ItemKind;

        let kind = match &item.kind {
            ItemKind::Expression(expr) => {
                if let Some(typed_expr) = Self::convert_expression_simple(expr, type_context) {
                    TypedItemKind::Expression(Box::new(typed_expr))
                } else {
                    TypedItemKind::Placeholder("Unsupported expression".to_string())
                }
            }
            ItemKind::FunctionDefinition(_) => {
                TypedItemKind::Placeholder("Function definition".to_string())
            }
            ItemKind::StructDefinition(_) => {
                TypedItemKind::Placeholder("Struct definition".to_string())
            }
            ItemKind::ProtocolDefinition(_) => {
                TypedItemKind::Placeholder("Protocol definition".to_string())
            }
            ItemKind::ImplBlock(_) => TypedItemKind::Placeholder("Impl block".to_string()),
            _ => {
                return None; // Skip other items
            }
        };

        Some(TypedItem {
            kind,
            span: item.span,
            debug_info: None, // Will be populated by TypedASTBuilder
        })
    }

    /// Convert expression to typed expression (minimal implementation)
    ///
    /// Note: This is a placeholder implementation. Complex expressions should be handled
    /// by a TypedASTBuilder visitor that runs after TypeCheckingVisitor to use
    /// properly resolved type information.
    fn convert_expression_simple(
        expr: &outrun_parser::Expression,
        _type_context: &UnificationContext,
    ) -> Option<TypedExpression> {
        use outrun_parser::ExpressionKind;

        // Only handle basic literals here - complex expressions need proper type checking

        let kind = match &expr.kind {
            ExpressionKind::Integer(lit) => TypedExpressionKind::Integer(lit.value),
            ExpressionKind::Float(lit) => TypedExpressionKind::Float(lit.value),
            ExpressionKind::String(lit) => {
                // Simplified string handling
                let content = lit
                    .parts
                    .iter()
                    .map(|part| match part {
                        outrun_parser::StringPart::Text { content, .. } => content.clone(),
                        outrun_parser::StringPart::Interpolation { .. } => "{}".to_string(),
                    })
                    .collect::<String>();
                TypedExpressionKind::String(content)
            }
            ExpressionKind::Boolean(lit) => TypedExpressionKind::Boolean(lit.value),
            ExpressionKind::Atom(lit) => TypedExpressionKind::Atom(lit.name.clone()),
            ExpressionKind::Identifier(id) => TypedExpressionKind::Identifier(id.name.clone()),

            // Complex expressions will be handled by TypedASTBuilder visitor
            ExpressionKind::FunctionCall(_) => {
                return Some(TypedExpression {
                    kind: TypedExpressionKind::Placeholder(
                        "Function call - handled by TypedASTBuilder".to_string(),
                    ),
                    structured_type: None,
                    span: expr.span,
                    debug_info: None, // Will be populated by TypedASTBuilder
                });
            }

            ExpressionKind::FieldAccess(_) => {
                return Some(TypedExpression {
                    kind: TypedExpressionKind::Placeholder(
                        "Field access - handled by TypedASTBuilder".to_string(),
                    ),
                    structured_type: None,
                    span: expr.span,
                    debug_info: None, // Will be populated by TypedASTBuilder
                });
            }

            // Control flow expressions - create placeholders for TypedASTBuilder to handle
            ExpressionKind::IfExpression(_) => {
                return Some(TypedExpression {
                    kind: TypedExpressionKind::Placeholder(
                        "If expression - handled by TypedASTBuilder".to_string(),
                    ),
                    structured_type: None,
                    span: expr.span,
                    debug_info: None, // Will be populated by TypedASTBuilder
                });
            }

            ExpressionKind::CaseExpression(_) => {
                return Some(TypedExpression {
                    kind: TypedExpressionKind::Placeholder(
                        "Case expression - handled by TypedASTBuilder".to_string(),
                    ),
                    structured_type: None,
                    span: expr.span,
                    debug_info: None, // Will be populated by TypedASTBuilder
                });
            }

            ExpressionKind::BinaryOp(_) => {
                // Binary operators should never reach the type checker!
                // They should be desugared to function calls in the desugaring phase.
                return None; // Return None to indicate this expression type is not supported in simple conversion
            }

            // For now, return None for other complex expressions - we'll implement these incrementally
            _ => return None,
        };

        Some(TypedExpression {
            kind,
            structured_type: None, // Type will be resolved during proper type checking
            span: expr.span,
            debug_info: None, // Will be populated by TypedASTBuilder
        })
    }

    /// Type check a program with external variables (for REPL usage)
    pub fn check_program_with_external_variables(
        &mut self,
        program: &Program,
        _external_variables: HashMap<String, crate::unification::StructuredType>,
    ) -> Result<TypedProgram, Vec<TypeError>> {
        // Since we have a globally unique core library compilation, we should use it directly
        // instead of recompiling anything. We just need to type-check the user expression
        // against the existing core library context.

        let cached_core_compilation = crate::core_library::get_core_library_compilation();

        // Create a simple typed program using the cached core library data
        // For REPL usage, we typically just have a single expression to evaluate
        let typed_items: Vec<TypedItem> = program
            .items
            .iter()
            .filter_map(|item| {
                Self::convert_item_simple(item, &cached_core_compilation.type_context)
            })
            .collect();

        let compilation_summary =
            "REPL expression with core library functions available".to_string();

        Ok(TypedProgram {
            items: typed_items,
            type_context: cached_core_compilation.type_context.clone(),
            compilation_order: vec!["main.outrun".to_string()],
            compilation_summary,
            debug_info: TypedDebugInfo::default(),
            error_recovery_info: Vec::new(),
            detailed_summary: None,
        })
    }
}
