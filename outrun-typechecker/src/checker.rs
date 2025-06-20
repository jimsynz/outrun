//! New simplified type checker using visitor pattern and multi-program compilation

use crate::error::TypeError;
use crate::multi_program_compiler::{CompilationResult, FunctionRegistry, MultiProgramCompiler};
use crate::types::TypeId;
use crate::unification::{StructuredType, UnificationContext};
use outrun_parser::{Program, Span};

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

/// Dispatch method for function calls
#[derive(Debug, Clone, PartialEq)]
pub enum DispatchMethod {
    /// Static function call - resolved at compile time
    Static {
        function_id: String, // Reference to function registry
    },
    /// Trait method call - dispatched at runtime
    Trait {
        trait_name: String,
        method_name: String,
        impl_type: TypeId,
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
        dispatch_method: DispatchMethod,
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

    // Placeholder for unsupported expressions (temporary)
    Placeholder(String),
}

/// Simplified typed item
#[derive(Debug, Clone)]
pub struct TypedItem {
    pub kind: TypedItemKind,
    pub span: Span,
}

/// Simplified typed item kinds
#[derive(Debug, Clone)]
pub enum TypedItemKind {
    Expression(Box<TypedExpression>),
    Placeholder(String), // For unsupported items
}

/// Simplified typed program with essential compilation artifacts
#[derive(Debug, Clone)]
pub struct TypedProgram {
    /// Top-level typed items in the program
    pub items: Vec<TypedItem>,
    /// Type checking context with resolved types
    pub type_context: UnificationContext,
    /// Hierarchical function registry
    pub function_registry: FunctionRegistry,
    /// Compilation order for dependencies
    pub compilation_order: Vec<String>,
    /// Summary of compilation (for debugging)
    pub compilation_summary: String,
}

/// Main type checker that uses the new multi-program visitor architecture
#[derive(Debug)]
pub struct TypeChecker {
    compiler: MultiProgramCompiler,
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
            compiler: MultiProgramCompiler::new(),
        }
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
            "Compiled {} items, {} traits, {} structs, {} implementations",
            typed_items.len(),
            compilation_result.traits.len(),
            compilation_result.structs.len(),
            compilation_result.implementations.len()
        );

        TypedProgram {
            items: typed_items,
            type_context: compilation_result.type_context,
            function_registry: compilation_result.function_registry,
            compilation_order: compilation_result.compilation_order,
            compilation_summary,
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
            format!("{}", program), // Use Display for source reconstruction
        );

        // Compile using the multi-program compiler
        match self.compiler.compile(&collection) {
            Ok(result) => {
                // Convert compilation result to TypedProgram
                let typed_program = Self::compilation_result_to_typed_program(result, program);
                Ok(typed_program)
            }
            Err(errors) => Err(errors),
        }
    }

    /// Create a type checker with core library bootstrapped
    pub fn new_bootstrapped_or_fail(
        _source: &str,
        _filename: &str,
    ) -> Result<Self, crate::error::TypeErrorReport> {
        // Create a type checker with intrinsics (which includes core library)
        Ok(Self {
            compiler: MultiProgramCompiler::new_with_intrinsics(),
        })
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
            ItemKind::TraitDefinition(_) => {
                TypedItemKind::Placeholder("Trait definition".to_string())
            }
            ItemKind::ImplBlock(_) => TypedItemKind::Placeholder("Impl block".to_string()),
            _ => {
                return None; // Skip other items
            }
        };

        Some(TypedItem {
            kind,
            span: item.span,
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
                });
            }

            ExpressionKind::FieldAccess(_) => {
                return Some(TypedExpression {
                    kind: TypedExpressionKind::Placeholder(
                        "Field access - handled by TypedASTBuilder".to_string(),
                    ),
                    structured_type: None,
                    span: expr.span,
                });
            }

            // For now, return None for other complex expressions - we'll implement these incrementally
            _ => return None,
        };

        Some(TypedExpression {
            kind,
            structured_type: None, // Type will be resolved during proper type checking
            span: expr.span,
        })
    }
}

/// Placeholder type context for compatibility
#[derive(Debug, Clone)]
pub struct TypeContext {
    // TODO: Replace with unification context
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeContext {
    pub fn new() -> Self {
        Self {}
    }
}
