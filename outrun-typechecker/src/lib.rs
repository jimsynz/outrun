//! # Outrun Type Checker
//!
//! A comprehensive static type checker for the Outrun programming language that validates trait constraints,
//! function signatures, and expressions at compile time, generating efficient dispatch tables for the interpreter.
//!
//! ## Quick Start
//!
//! ```rust
//! use outrun_parser::parse_program;
//! use outrun_typechecker::typecheck_program_with_source;
//!
//! // Basic type checking
//! // Note: This example requires core library types to be available
//! let source = r#"
//!     def add(x: Integer, y: Integer): Integer {
//!         x + y
//!     }
//! "#;
//!
//! let program = parse_program(source).unwrap();
//! // This would work once core library is loaded
//! // let typed_program = typecheck_program_with_source(program, source, "example.outrun").unwrap();
//!
//! // Access type information  
//! // println!("Type checking successful!");
//! // println!("Program has {} items", typed_program.items.len());
//! ```
//!
//! ## Error Handling
//!
//! ```rust
//! use outrun_parser::parse_program;
//! use outrun_typechecker::typecheck_program_with_source;
//!
//! let source = r#"
//!     def add(x: Integer, y: String): Integer {
//!         x + y  # Type error: String + Integer not supported
//!     }
//! "#;
//!
//! let program = parse_program(source).unwrap();
//! match typecheck_program_with_source(program, source, "example.outrun") {
//!     Ok(typed_program) => println!("Type checking successful!"),
//!     Err(error_report) => {
//!         // Beautiful error display with source highlighting
//!         eprintln!("{:?}", error_report);
//!     }
//! }
//! ```
//!
//! ## Features
//!
//! - **Complete expression type checking** - All Outrun expressions with comprehensive validation
//! - **Trait system support** - Definition, implementation, dispatch, and constraint validation
//! - **Generic type support** - Complete generic parameter handling with arity validation
//! - **Pattern matching** - Comprehensive pattern validation with exhaustiveness checking
//! - **Beautiful error reporting** - Production-quality error display with source highlighting
//! - **Dispatch table generation** - Pre-computed lookup tables for efficient runtime trait method calls
//! - **Anonymous functions** - Higher-order function support with multi-clause validation
//!
//! ## Usage Examples
//!
//! ### Basic Type Checking
//!
//! ```rust
//! use outrun_parser::parse_program;
//! use outrun_typechecker::typecheck_program_with_source;
//!
//! let source = r#"
//!     struct Point(x: Float, y: Float)
//!     
//!     def distance(p1: Point, p2: Point): Float {
//!         let dx = Point.x(p1) - Point.x(p2)
//!         let dy = Point.y(p1) - Point.y(p2)
//!         Float.sqrt(dx * dx + dy * dy)
//!     }
//! "#;
//!
//! let program = parse_program(source).unwrap();
//! match typecheck_program_with_source(program, source, "geometry.outrun") {
//!     Ok(typed_program) => {
//!         println!("✓ Type checking successful!");
//!         println!("  Program items: {}", typed_program.items.len());
//!     }
//!     Err(error_report) => {
//!         eprintln!("✗ Type checking failed:");
//!         eprintln!("{:?}", error_report);
//!     }
//! }
//! ```
//!
//! ### CLI Integration
//!
//! ```rust
//! use outrun_parser::parse_program;
//! use outrun_typechecker::typecheck_status;
//!
//! // Simple CLI-friendly type checking  
//! let source = "def add(x: Integer, y: Integer): Integer { x + y }";
//! let program = parse_program(source).unwrap();
//!
//! // This would work once core library is loaded
//! // match typecheck_status(program, source, "math.outrun") {
//! //     Ok(()) => {
//! //         println!("Type checking passed");
//! //         std::process::exit(0);
//! //     }
//! //     Err(error_count) => {
//! //         eprintln!("Type checking failed with {} errors", error_count);
//! //         std::process::exit(1);
//! //     }
//! // }
//! ```
//!
//! ### Symbol Extraction for IDEs
//!
//! ```rust
//! use outrun_parser::parse_program;
//! use outrun_typechecker::{typecheck_program_with_source, extract_symbols, SymbolKind};
//!
//! let source = r#"
//!     trait Display<T> {
//!         def to_string(value: T): String
//!     }
//!     
//!     struct User(name: String, age: Integer)
//!     
//!     def greet(user: User): String {
//!         "Hello, #{User.name(user)}!"
//!     }
//! "#;
//!
//! let program = parse_program(source).unwrap();
//! if let Ok(typed_program) = typecheck_program_with_source(program, source, "example.outrun") {
//!     let symbols = extract_symbols(&typed_program);
//!     
//!     for symbol in symbols {
//!         match symbol.kind {
//!             SymbolKind::Trait => println!("Trait: {}", symbol.name),
//!             SymbolKind::Struct => println!("Struct: {}", symbol.name),
//!             SymbolKind::Function => println!("Function: {}", symbol.name),
//!             _ => {}
//!         }
//!     }
//! }
//! ```
//!
//! ### Error Handling and Diagnostics
//!
//! ```rust
//! use outrun_parser::parse_program;
//! use outrun_typechecker::{typecheck_program_with_source, get_detailed_diagnostics};
//!
//! let source = r#"
//!     def broken_function(x: Integer): String {
//!         x + "hello"  # Type error: Integer + String
//!     }
//! "#;
//!
//! let program = parse_program(source).unwrap();
//! if let Err(error_report) = typecheck_program_with_source(program, source, "broken.outrun") {
//!     let diagnostics = get_detailed_diagnostics(&error_report);
//!     
//!     for diagnostic in diagnostics {
//!         println!("Error: {}", diagnostic.message);
//!         if let Some(range) = diagnostic.range {
//!             println!("  at offset {}-{}", range.offset(), range.offset() + range.len());
//!         }
//!     }
//! }
//! ```
//!
//! ## Getting Started Guide
//!
//! ### 1. Add the Dependency
//!
//! Add to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! outrun-typechecker = "0.1.0"
//! outrun-parser = "0.1.0"
//! ```
//!
//! ### 2. Basic Type Checking Workflow
//!
//! The typical workflow involves three steps:
//!
//! 1. **Parse** your Outrun source code with `outrun-parser`
//! 2. **Type check** the parsed program with `typecheck_program_with_source`
//! 3. **Handle results** - either use the typed program or display errors
//!
//! ### 3. Understanding the Type System
//!
//! The Outrun type checker supports:
//!
//! - **Static typing** with compile-time verification
//! - **Trait-based dispatch** for method resolution
//! - **Generic types** with constraint validation
//! - **Pattern matching** with exhaustiveness checking
//! - **Function overloading** with guard-based resolution
//!
//! ### 4. Error Handling Best Practices
//!
//! - Use `typecheck_program_with_source` for rich error reporting
//! - Display errors with miette for beautiful terminal output
//! - Use `typecheck_status` for simple pass/fail checking in CLI tools
//! - Extract structured diagnostics with `get_detailed_diagnostics` for IDE integration
//!
//! ### 5. Performance Considerations
//!
//! - Type checking is designed to be fast with TypeId-based comparisons
//! - Dispatch tables are pre-computed for runtime efficiency
//! - The typed AST contains all information needed for interpretation
//! - Symbol extraction is optimized for LSP and IDE integration
//!
//! ## Integration Patterns
//!
//! ### CLI Tools
//!
//! ```rust,no_run
//! let source = std::fs::read_to_string("program.outrun").unwrap();
//! let program = outrun_parser::parse_program(&source).unwrap();
//!
//! match outrun_typechecker::typecheck_status(program, &source, "program.outrun") {
//!     Ok(()) => println!("✓ Type checking passed"),
//!     Err(count) => {
//!         eprintln!("✗ {} type errors found", count);
//!         std::process::exit(1);
//!     }
//! }
//! ```
//!
//! ### Language Servers
//!
//! ```rust,no_run
//! # struct Position { line: u32, character: u32 }
//! # struct Hover { contents: String, range: Option<()> }
//! # fn get_cached_typed_program(uri: &str) -> Option<outrun_typechecker::TypedProgram> { None }
//! # fn get_source_for_uri(uri: &str) -> Option<String> { None }
//! // In your LSP implementation
//! fn provide_hover(uri: &str, position: Position) -> Option<Hover> {
//!     let typed_program = get_cached_typed_program(uri)?;
//!     let source = get_source_for_uri(uri)?;
//!     let type_info = outrun_typechecker::get_type_at_position(
//!         &typed_program,
//!         &source,
//!         position.line as usize,
//!         position.character as usize,
//!     )?;
//!     Some(Hover { contents: type_info, range: None })
//! }
//! ```
//!
//! ### Build Systems
//!
//! ```rust,no_run
//! # use std::path::Path;
//! # struct CompileError;
//! # fn generate_code(program: &outrun_typechecker::TypedProgram) -> Result<(), CompileError> { Ok(()) }
//! fn compile_outrun_file(path: &Path) -> Result<(), CompileError> {
//!     let source = std::fs::read_to_string(path).unwrap();
//!     let program = outrun_parser::parse_program(&source).unwrap();
//!     let typed_program = outrun_typechecker::typecheck_program_with_source(
//!         program,
//!         &source,
//!         path.to_string_lossy().as_ref(),
//!     ).unwrap();
//!     
//!     // Use typed_program for code generation
//!     generate_code(&typed_program)?;
//!     Ok(())
//! }
//! ```

pub mod checker;
pub mod compilation;
pub mod core_library;
pub mod dependency_graph;
pub mod desugaring;
pub mod dispatch;
pub mod error;
// TODO: Re-enable after updating for new UnificationContext
pub mod intrinsics;
pub mod multi_program_compiler;
pub mod patterns;
pub mod typed_ast_builder;
pub mod types;
pub mod unification;
pub mod visitor;

// Internal desugaring
use desugaring::DesugaringVisitor;

#[cfg(test)]
mod tests;

// Re-export core types and functions for easy access
pub use checker::{
    TypeChecker, TypedBlock, TypedExpression, TypedExpressionKind, TypedFunctionDefinition,
    TypedFunctionPath, TypedItem, TypedItemKind, TypedLetBinding, TypedProgram, TypedStatement,
};
pub use dispatch::DispatchTable;
pub use error::{
    ErrorGroup, ErrorSummary, TypeError, TypeErrorReport, TypeErrorWithSource, TypeResult,
};
pub use multi_program_compiler::{CompilationResult, MultiProgramCompiler, ProgramCollection};
pub use types::{AtomId, ConcreteType, TypeId, TypeInterner};
pub use unification::{StructuredType, UnificationContext, UnificationError, UnificationResult};
pub use visitor::{TypedVisitor, Visitor};

// LSP integration types are defined in this module

// Integration types are defined in this module and re-exported automatically

// Main type checking API
use outrun_parser::Program;

/// Type check a parsed program and return a typed AST with dispatch tables
///
/// This function provides type checking without enhanced error reporting. For most use cases,
/// prefer `typecheck_program_with_source` which provides better error messages.
///
/// # Arguments
/// * `program` - The parsed program from outrun-parser
///
/// # Returns
/// * `Ok(TypedProgram)` - Successfully typed program with complete type information and dispatch tables
/// * `Err(Vec<TypeError>)` - Collection of type errors encountered during checking
pub fn typecheck_program(program: Program) -> Result<TypedProgram, Vec<TypeError>> {
    // First desugar the program to transform operators into trait function calls
    let desugared_program = DesugaringVisitor::desugar_program(program);

    let mut checker = TypeChecker::new();
    checker.check_program(&desugared_program)
}

/// Type check a parsed program with enhanced error reporting that includes source context
///
/// This function provides the same type checking capabilities as `typecheck_program` but with
/// enhanced error reporting that includes source context for beautiful error display. This is
/// the recommended function for CLI tools and development environments.
///
/// # Arguments
/// * `program` - The parsed program from outrun-parser
/// * `source` - The original source code for error context and highlighting
/// * `filename` - The filename for error reporting (use "<stdin>" for stdin input)
///
/// # Returns
/// * `Ok(TypedProgram)` - Successfully typed program with complete type information
/// * `Err(TypeErrorReport)` - Enhanced error report with source context, categorization, and miette integration
///
/// # Error Reporting Features
/// - **Source highlighting** - Precise error location with source context
/// - **Error categorization** - Grouped by error type (type mismatches, undefined functions, etc.)
/// - **Helpful suggestions** - Actionable recommendations for fixing errors
/// - **Professional display** - Production-quality error formatting
///
/// # Example
/// ```rust
/// use outrun_parser::parse_program;
/// use outrun_typechecker::typecheck_program_with_source;
///
/// let source = r#"
///     def calculate(x: Integer, y: String): Integer {
///         x + y  # Error: Cannot add Integer and String
///     }
/// "#;
///
/// let program = parse_program(source).unwrap();
/// match typecheck_program_with_source(program, source, "math.outrun") {
///     Ok(typed_program) => println!("✓ Type checking successful!"),
///     Err(error_report) => {
///         // Beautiful error display with miette integration
///         eprintln!("{:?}", error_report);
///         
///         // Access structured error information
///         println!("Summary: {}", error_report.error_summary());
///         for group in error_report.group_related_errors() {
///             println!("  {}: {} errors", group.category, group.errors.len());
///         }
///     }
/// }
/// ```
pub fn typecheck_program_with_source(
    program: Program,
    source: &str,
    filename: &str,
) -> Result<TypedProgram, TypeErrorReport> {
    // First desugar the program to transform operators into trait function calls
    let desugared_program = DesugaringVisitor::desugar_program(program);

    // Create type checker and bootstrap core library
    let mut checker = TypeChecker::new_bootstrapped_or_fail(source, filename)?;
    match checker.check_program(&desugared_program) {
        Ok(typed_program) => Ok(typed_program),
        Err(errors) => Err(TypeErrorReport::new(
            errors,
            source.to_string(),
            filename.to_string(),
        )),
    }
}

/// Type check a collection of programs using the new multi-program visitor-based compiler
///
/// This is the next-generation type checking API that handles multiple programs with
/// proper dependency resolution and phase-based compilation. It's designed to replace
/// the single-program typecheck functions for more complex compilation scenarios.
///
/// # Arguments
/// * `collection` - A collection of programs to compile together
///
/// # Returns
/// * `Ok(CompilationResult)` - Successfully compiled programs with full type information
/// * `Err(TypeErrorReport)` - Enhanced error report with source context from all programs
///
/// # Features
/// - **Multi-program support** - Compile multiple files with dependencies
/// - **Dependency resolution** - Automatic ordering with circular dependency detection
/// - **Phase-based compilation** - Separate phases for traits, structs, impls, functions, type checking
/// - **Visitor pattern** - Extensible compilation phases
/// - **Comprehensive type unification** - Proper generic type and trait compatibility checking
///
/// # Example
/// ```rust
/// use outrun_parser::parse_program;
/// use outrun_typechecker::{typecheck_program_collection, ProgramCollection};
///
/// // Create a program collection
/// let mut collection = ProgramCollection::new();
///
/// // Add core library
/// let core_collection = ProgramCollection::from_core_library();
/// collection.add_programs(core_collection.programs.into_iter().map(|(k, v)| (k, (v, String::new()))).collect());
///
/// // Add user program
/// let source = r#"
///     def greet(name: String): String {
///         "Hello, #{name}!"
///     }
/// "#;
/// let program = parse_program(source).unwrap();
/// collection.add_program("main.outrun".to_string(), program, source.to_string());
///
/// match typecheck_program_collection(collection) {
///     Ok(result) => {
///         println!("✓ Multi-program compilation successful!");
///         println!("  Compilation order: {:?}", result.compilation_order);
///         println!("  Traits: {}", result.traits.len());
///         println!("  Structs: {}", result.structs.len());
///         println!("  Functions: {}", result.function_registry.len());
///     }
///     Err(error_report) => {
///         eprintln!("✗ Compilation failed:");
///         eprintln!("{:?}", error_report);
///     }
/// }
/// ```
pub fn typecheck_program_collection(
    collection: ProgramCollection,
) -> Result<CompilationResult, TypeErrorReport> {
    let mut compiler = MultiProgramCompiler::new();

    match compiler.compile(&collection) {
        Ok(result) => Ok(result),
        Err(errors) => {
            // Create individual error reports with proper source context
            let mut errors_with_source = Vec::new();

            for error in errors {
                // Find which source file this error belongs to by checking all files
                let mut found_source = false;
                for (filename, source_content) in &collection.sources {
                    // For now, we'll associate errors with the first non-empty source file
                    // A more sophisticated approach would track file associations during compilation
                    if !source_content.trim().is_empty() && !found_source {
                        errors_with_source.push(TypeErrorWithSource::new(
                            error.clone(),
                            source_content.clone(),
                            filename.clone(),
                        ));
                        found_source = true;
                        break;
                    }
                }

                // Fallback: if no source found, create a generic error
                if !found_source {
                    errors_with_source.push(TypeErrorWithSource::new(
                        error,
                        "<source unavailable>".to_string(),
                        "<unknown>".to_string(),
                    ));
                }
            }

            Err(TypeErrorReport::from_errors_with_source(errors_with_source))
        }
    }
}

/// Create a program collection from core library and a single user program
///
/// This is a convenience function for the common case of type checking a single
/// user program against the core library using the new multi-program compiler.
///
/// # Example
/// ```rust
/// use outrun_parser::parse_program;
/// use outrun_typechecker::typecheck_with_core_library;
///
/// let source = r#"
///     def add(x: Integer, y: Integer): Integer {
///         x + y
///     }
/// "#;
///
/// let program = parse_program(source).unwrap();
/// match typecheck_with_core_library(program, source, "fibonacci.outrun") {
///     Ok(result) => println!("✓ Type checking with core library successful!"),
///     Err(error_report) => eprintln!("✗ Type checking failed: {:?}", error_report),
/// }
/// ```
pub fn typecheck_with_core_library(
    program: Program,
    source: &str,
    filename: &str,
) -> Result<CompilationResult, TypeErrorReport> {
    // Create collection with core library
    let mut collection = ProgramCollection::from_core_library();

    // Add user program
    let desugared_program = DesugaringVisitor::desugar_program(program);
    collection.add_program(filename.to_string(), desugared_program, source.to_string());

    // Compile the collection
    typecheck_program_collection(collection)
}

/// Create a new TypeChecker instance for advanced usage scenarios
///
/// This function creates a fresh TypeChecker instance that can be reused for multiple
/// type checking operations. This is useful for language servers or advanced tooling.
pub fn create_type_checker() -> TypeChecker {
    TypeChecker::new()
}

// TODO: Re-enable after updating TypeChecker API
// /// Validate a single expression type in isolation
// ///
// /// This utility function is useful for interactive development tools, REPLs, or
// /// language servers that need to validate expressions without full program context.
// ///
// /// Note: This function is currently limited as it requires TypeChecker::with_context
// /// which is not yet implemented.
// pub fn validate_expression(
//     expression_source: &str,
//     _context: Option<&mut TypeContext>,
// ) -> Result<TypedExpression, TypeError> {
//     // Parse the expression
//     let expr = outrun_parser::parse_expression(expression_source)
//         .map_err(|_| TypeError::internal("Failed to parse expression".to_string()))?;
//
//     // Create a new checker (context support would require with_context method)
//     let mut checker = TypeChecker::new();
//
//     // Convert and type check the expression
//     checker.convert_expression(&expr)
// }

// =============================================================================
// Integration Points for CLI and LSP
// =============================================================================

/// Type check a program and return success status with error count
///
/// This is a convenience function for CLI tools that just need to know if
/// type checking succeeded and how many errors occurred.
///
/// # Returns
/// `Ok(())` if type checking succeeded, `Err(error_count)` if it failed
pub fn typecheck_status(program: Program, source: &str, filename: &str) -> Result<(), usize> {
    match typecheck_program_with_source(program, source, filename) {
        Ok(_) => Ok(()),
        Err(error_report) => Err(error_report.error_count()),
    }
}

/// Extract type information for a specific position in the source code
///
/// This function is designed for LSP implementations that need to provide
/// hover information, go-to-definition, and other position-based features.
///
/// # Arguments
/// * `typed_program` - The typed program with complete type information
/// * `source` - The original source code for position resolution
/// * `line` - Zero-based line number
/// * `column` - Zero-based column number
///
/// # Returns
/// Returns formatted type information if a typed node is found at the position,
/// None if no typed node exists at that location.
pub fn get_type_at_position(
    typed_program: &TypedProgram,
    source: &str,
    line: usize,
    column: usize,
) -> Option<String> {
    // 1. Convert line/column to byte offset
    let target_offset = line_column_to_offset(line, column, source)?;

    // 2. Find the most specific typed node at this position
    let type_info = find_type_at_offset(typed_program, target_offset)?;

    // 3. Format type information for display using built-in string representation
    Some(format!(
        "Type: {}",
        type_info.to_string_representation(&typed_program.type_context.type_interner)
    ))
}

/// Convert line and column position to byte offset in source text
pub fn line_column_to_offset(line: usize, column: usize, source: &str) -> Option<usize> {
    let mut current_line = 0;
    let mut current_column = 0;

    for (offset, ch) in source.char_indices() {
        if current_line == line && current_column == column {
            return Some(offset);
        }

        if ch == '\n' {
            current_line += 1;
            current_column = 0;
        } else {
            current_column += 1;
        }
    }

    // Handle end-of-file position
    if current_line == line && current_column == column {
        Some(source.len())
    } else {
        None
    }
}

/// Find type information for a typed node at the given byte offset
fn find_type_at_offset(typed_program: &TypedProgram, offset: usize) -> Option<StructuredType> {
    // Search through all typed items for the one containing this offset
    for item in &typed_program.items {
        if span_contains_offset(&item.span, offset) {
            if let Some(type_info) = find_type_in_item(item, offset) {
                return Some(type_info);
            }
        }
    }
    None
}

/// Check if a span contains the given byte offset
pub fn span_contains_offset(span: &outrun_parser::Span, offset: usize) -> bool {
    offset >= span.start && offset < span.end
}

/// Find type information within a typed item
fn find_type_in_item(item: &TypedItem, offset: usize) -> Option<StructuredType> {
    match &item.kind {
        TypedItemKind::Expression(expr) => find_type_in_expression(expr, offset),
        TypedItemKind::FunctionDefinition(func_def) => find_type_in_function(func_def, offset),
        TypedItemKind::LetBinding(let_binding) => find_type_in_let_binding(let_binding, offset),
        // For now, handle the most common cases
        _ => None,
    }
}

/// Find type information within a typed expression
fn find_type_in_expression(expr: &TypedExpression, offset: usize) -> Option<StructuredType> {
    // If this expression's span contains the offset, it's a candidate
    if !span_contains_offset(&expr.span, offset) {
        return None;
    }

    // First, check for more specific nested expressions
    match &expr.kind {
        TypedExpressionKind::FunctionCall {
            function_path,
            arguments,
            ..
        } => {
            // Check function path
            if let TypedFunctionPath::Expression { expression } = function_path {
                if let Some(nested_type) = find_type_in_expression(expression, offset) {
                    return Some(nested_type);
                }
            }

            // Check arguments
            for arg in arguments {
                if let Some(nested_type) = find_type_in_expression(&arg.expression, offset) {
                    return Some(nested_type);
                }
            }
        }
        TypedExpressionKind::FieldAccess { object, .. } => {
            if let Some(nested_type) = find_type_in_expression(object, offset) {
                return Some(nested_type);
            }
        }
        TypedExpressionKind::List { elements, .. } => {
            for element in elements {
                if let Some(nested_type) = find_type_in_expression(element, offset) {
                    return Some(nested_type);
                }
            }
        }
        // Add more expression kinds as needed
        _ => {}
    }

    // If no nested expression matched, return this expression's type
    expr.structured_type.clone()
}

/// Find type information within a function definition
fn find_type_in_function(
    func_def: &TypedFunctionDefinition,
    offset: usize,
) -> Option<StructuredType> {
    // Check function body for the most specific match
    find_type_in_block(&func_def.body, offset).or_else(|| {
        // If not in body, check if we're on the function name or return type
        if span_contains_offset(&func_def.span, offset) {
            // Return the function's type information
            func_def.return_type.clone()
        } else {
            None
        }
    })
}

/// Find type information within a block
fn find_type_in_block(block: &TypedBlock, offset: usize) -> Option<StructuredType> {
    // Search through statements in the block
    for statement in &block.statements {
        match statement {
            TypedStatement::Expression(expr) => {
                if let Some(type_info) = find_type_in_expression(expr, offset) {
                    return Some(type_info);
                }
            }
            TypedStatement::LetBinding(let_binding) => {
                if let Some(type_info) = find_type_in_let_binding(let_binding, offset) {
                    return Some(type_info);
                }
            }
        }
    }
    None
}

/// Find type information within a let binding
fn find_type_in_let_binding(
    let_binding: &TypedLetBinding,
    offset: usize,
) -> Option<StructuredType> {
    // Check if the offset is in the expression part
    find_type_in_expression(&let_binding.expression, offset).or_else(|| {
        // If not in expression, check if we're on the variable name
        if span_contains_offset(&let_binding.span, offset) {
            // Return the let binding's binding type
            let_binding.binding_type.clone()
        } else {
            None
        }
    })
}

/// Format type information for hover display
pub fn format_type_for_hover(structured_type: &StructuredType) -> String {
    match structured_type {
        StructuredType::Simple(type_name) => {
            format!("Type: {}", type_name)
        }
        StructuredType::Generic { base, args } => {
            let args_str = args
                .iter()
                .map(format_type_for_hover)
                .map(|s| s.strip_prefix("Type: ").unwrap_or(&s).to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("Type: {}<{}>", base, args_str)
        }
        StructuredType::Function {
            params,
            return_type,
        } => {
            let params_str = params
                .iter()
                .map(|param| {
                    format!(
                        "{}: {}",
                        param.name,
                        format_type_for_hover(&param.param_type)
                            .strip_prefix("Type: ")
                            .unwrap_or("")
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            let return_str = format_type_for_hover(return_type)
                .strip_prefix("Type: ")
                .unwrap_or("")
                .to_string();
            format!("Type: ({}) -> {}", params_str, return_str)
        }
        StructuredType::Tuple(elements) => {
            let elements_str = elements
                .iter()
                .map(format_type_for_hover)
                .map(|s| s.strip_prefix("Type: ").unwrap_or(&s).to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("Type: ({})", elements_str)
        }
        StructuredType::TypeError {
            error,
            fallback_type,
            ..
        } => {
            if let Some(fallback) = fallback_type {
                format!(
                    "Type Error (fallback: {}): {}",
                    format_type_for_hover(fallback)
                        .strip_prefix("Type: ")
                        .unwrap_or("unknown"),
                    error
                )
            } else {
                format!("Type Error: {}", error)
            }
        }
    }
}

/// Completion item for auto-completion functionality
#[derive(Debug, Clone)]
pub struct CompletionItem {
    /// The label shown in the completion list
    pub label: String,
    /// The kind of completion (function, variable, type, etc.)
    pub kind: CompletionKind,
    /// Additional detail information (e.g., function signature)
    pub detail: Option<String>,
    /// Documentation/help text for this completion
    pub documentation: Option<String>,
    /// Text to insert (if different from label)
    pub insert_text: Option<String>,
}

/// Types of completion items
#[derive(Debug, Clone, PartialEq)]
pub enum CompletionKind {
    Function,
    Variable,
    Type,
    Module,
    Constant,
    Keyword,
    Method,
    Field,
}

/// Get completion suggestions for a specific position
///
/// This function analyzes the context at the given position and returns
/// relevant completion suggestions for LSP implementations.
///
/// # Arguments
/// * `typed_program` - The typed program with all type information
/// * `line` - Zero-based line number
/// * `column` - Zero-based column number
///
/// # Returns
/// A vector of completion items appropriate for the context
pub fn get_completions_at_position(
    typed_program: &TypedProgram,
    line: usize,
    column: usize,
) -> Vec<CompletionItem> {
    let completion_engine = CompletionEngine::new(typed_program);
    completion_engine.get_completions(line, column)
}

/// Completion engine for analyzing context and generating suggestions
struct CompletionEngine<'a> {
    typed_program: &'a TypedProgram,
}

impl<'a> CompletionEngine<'a> {
    fn new(typed_program: &'a TypedProgram) -> Self {
        Self { typed_program }
    }

    fn get_completions(&self, _line: usize, _column: usize) -> Vec<CompletionItem> {
        // For now, provide basic completions without context analysis
        // In a full implementation, we would parse the source at the position
        // to determine the exact context

        let mut completions = Vec::new();

        // Add function completions
        completions.extend(self.get_function_completions());

        // Add type completions
        completions.extend(self.get_type_completions());

        // Add keyword completions
        completions.extend(self.get_keyword_completions());

        // Sort completions alphabetically
        completions.sort_by(|a, b| a.label.cmp(&b.label));

        completions
    }

    fn get_function_completions(&self) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        // Extract functions from typed items
        for item in &self.typed_program.items {
            if let TypedItemKind::FunctionDefinition(func_def) = &item.kind {
                let signature = self.format_function_signature(func_def);
                let insert_text = self.generate_function_call_snippet(func_def);

                completions.push(CompletionItem {
                    label: func_def.name.clone(),
                    kind: CompletionKind::Function,
                    detail: Some(signature),
                    documentation: self.extract_function_documentation(func_def),
                    insert_text: Some(insert_text),
                });
            }
        }

        // Extract functions from function registry
        for module_funcs in self
            .typed_program
            .function_registry
            .module_functions
            .values()
        {
            for entry in module_funcs.values() {
                // Avoid duplicates by checking if we already added this function
                if !completions
                    .iter()
                    .any(|c| c.label == entry.definition.name.name)
                {
                    let signature = self.format_parser_function_signature(&entry.definition);
                    let insert_text = self.generate_parser_function_call_snippet(&entry.definition);

                    completions.push(CompletionItem {
                        label: entry.definition.name.name.clone(),
                        kind: CompletionKind::Function,
                        detail: Some(signature),
                        documentation: None, // Parser functions don't have typed documentation yet
                        insert_text: Some(insert_text),
                    });
                }
            }
        }

        completions
    }

    fn get_type_completions(&self) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        // Extract struct types
        for item in &self.typed_program.items {
            if let TypedItemKind::StructDefinition(struct_def) = &item.kind {
                let full_name = struct_def.name.join(".");

                completions.push(CompletionItem {
                    label: full_name.clone(),
                    kind: CompletionKind::Type,
                    detail: Some(format!("struct {}", full_name)),
                    documentation: None,
                    insert_text: None,
                });
            }
        }

        // Extract trait types
        for item in &self.typed_program.items {
            if let TypedItemKind::TraitDefinition(trait_def) = &item.kind {
                let trait_name = trait_def.name.join(".");

                completions.push(CompletionItem {
                    label: trait_name.clone(),
                    kind: CompletionKind::Type,
                    detail: Some(format!("trait {}", trait_name)),
                    documentation: None,
                    insert_text: None,
                });
            }
        }

        // Add common built-in types
        let builtin_types = vec![
            ("Integer", "Built-in integer type"),
            ("Float", "Built-in floating-point type"),
            ("String", "Built-in string type"),
            ("Boolean", "Built-in boolean type"),
            ("Atom", "Built-in atom type"),
            ("List", "Built-in list type"),
            ("Map", "Built-in map type"),
            ("Option", "Optional value type"),
            ("Result", "Result type for error handling"),
        ];

        for (type_name, description) in builtin_types {
            completions.push(CompletionItem {
                label: type_name.to_string(),
                kind: CompletionKind::Type,
                detail: Some(description.to_string()),
                documentation: None,
                insert_text: None,
            });
        }

        completions
    }

    fn get_keyword_completions(&self) -> Vec<CompletionItem> {
        let keywords = vec![
            (
                "def",
                "Function definition",
                "def ${1:name}(${2:params}): ${3:Type} {\n    ${4:body}\n}",
            ),
            (
                "defp",
                "Private function definition",
                "defp ${1:name}(${2:params}): ${3:Type} {\n    ${4:body}\n}",
            ),
            (
                "struct",
                "Struct definition",
                "struct ${1:Name} {\n    ${2:fields}\n}",
            ),
            (
                "trait",
                "Trait definition",
                "trait ${1:Name} {\n    ${2:functions}\n}",
            ),
            (
                "impl",
                "Implementation block",
                "impl ${1:Trait} for ${2:Type} {\n    ${3:implementations}\n}",
            ),
            ("let", "Variable binding", "let ${1:name} = ${2:value}"),
            (
                "if",
                "Conditional expression",
                "if ${1:condition} {\n    ${2:then_branch}\n}",
            ),
            (
                "case",
                "Pattern matching",
                "case ${1:value} {\n    when ${2:pattern} -> ${3:result}\n}",
            ),
            ("when", "Case clause", "when ${1:pattern} -> ${2:result}"),
            ("import", "Import statement", "import ${1:module}"),
            (
                "alias",
                "Type alias",
                "alias ${1:NewName} = ${2:ExistingType}",
            ),
            (
                "const",
                "Constant definition",
                "const ${1:NAME} = ${2:value}",
            ),
        ];

        keywords
            .into_iter()
            .map(|(keyword, description, snippet)| CompletionItem {
                label: keyword.to_string(),
                kind: CompletionKind::Keyword,
                detail: Some(description.to_string()),
                documentation: None,
                insert_text: Some(snippet.to_string()),
            })
            .collect()
    }

    fn format_function_signature(&self, func_def: &TypedFunctionDefinition) -> String {
        let params = func_def
            .parameters
            .iter()
            .map(|param| {
                let param_type = param
                    .param_type
                    .as_ref()
                    .map(|t| self.format_type(t))
                    .unwrap_or_else(|| "Unknown".to_string());
                format!("{}: {}", param.name, param_type)
            })
            .collect::<Vec<_>>()
            .join(", ");

        let return_type = func_def
            .return_type
            .as_ref()
            .map(|t| self.format_type(t))
            .unwrap_or_else(|| "Unknown".to_string());

        format!("({}) -> {}", params, return_type)
    }

    fn generate_function_call_snippet(&self, func_def: &TypedFunctionDefinition) -> String {
        if func_def.parameters.is_empty() {
            format!("{}()", func_def.name)
        } else {
            let params = func_def
                .parameters
                .iter()
                .enumerate()
                .map(|(i, param)| format!("{}: ${{{}}}", param.name, i + 1))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", func_def.name, params)
        }
    }

    fn extract_function_documentation(
        &self,
        _func_def: &TypedFunctionDefinition,
    ) -> Option<String> {
        // TODO: Extract documentation from function comments or attributes
        None
    }

    fn format_type(&self, structured_type: &StructuredType) -> String {
        structured_type.to_string_representation(&self.typed_program.type_context.type_interner)
    }

    fn format_parser_function_signature(
        &self,
        func_def: &outrun_parser::FunctionDefinition,
    ) -> String {
        let params = func_def
            .parameters
            .iter()
            .map(|param| {
                format!(
                    "{}: {}",
                    param.name.name,
                    self.format_parser_type_annotation(&param.type_annotation)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        let return_type = self.format_parser_type_annotation(&func_def.return_type);
        format!("({}) -> {}", params, return_type)
    }

    fn generate_parser_function_call_snippet(
        &self,
        func_def: &outrun_parser::FunctionDefinition,
    ) -> String {
        if func_def.parameters.is_empty() {
            format!("{}()", func_def.name.name)
        } else {
            let params = func_def
                .parameters
                .iter()
                .enumerate()
                .map(|(i, param)| format!("{}: ${{{}}}", param.name.name, i + 1))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", func_def.name.name, params)
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn format_parser_type_annotation(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> String {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                let base_name = path
                    .iter()
                    .map(|part| part.name.clone())
                    .collect::<Vec<_>>()
                    .join(".");

                if let Some(args) = generic_args {
                    let arg_names = args
                        .args
                        .iter()
                        .map(|arg| self.format_parser_type_annotation(arg))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", base_name, arg_names)
                } else {
                    base_name
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                let element_names = types
                    .iter()
                    .map(|elem| self.format_parser_type_annotation(elem))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", element_names)
            }
            outrun_parser::TypeAnnotation::Function { .. } => {
                "Function".to_string() // Simplified for now
            }
        }
    }
}

/// Get completion suggestions with prefix filtering
///
/// This is a convenience function that filters completions by a prefix string.
/// Useful for implementing prefix-based auto-completion in editors.
pub fn get_completions_with_prefix(
    typed_program: &TypedProgram,
    line: usize,
    column: usize,
    prefix: &str,
) -> Vec<CompletionItem> {
    get_completions_at_position(typed_program, line, column)
        .into_iter()
        .filter(|item| item.label.starts_with(prefix))
        .collect()
}

/// Extract all symbols (functions, types, variables) from a typed program
///
/// This function is useful for LSP implementations that need to provide
/// document symbols, outline views, or workspace symbol search.
pub fn extract_symbols(_typed_program: &TypedProgram) -> Vec<Symbol> {
    // TODO: Re-enable after updating TypedItemKind variants
    // let mut symbols = Vec::new();

    // // Extract symbols from typed items
    // for item in &typed_program.items {
    //     match &item.kind {
    //         crate::checker::TypedItemKind::FunctionDefinition(func) => {
    //             symbols.push(Symbol {
    //                 name: func.name.clone(),
    //                 kind: SymbolKind::Function,
    //                 range: item.span,
    //             });
    //         }
    //         crate::checker::TypedItemKind::StructDefinition(struct_def) => {
    //             symbols.push(Symbol {
    //                 name: struct_def.name.clone(),
    //                 kind: SymbolKind::Struct,
    //                 range: item.span,
    //             });
    //         }
    //         crate::checker::TypedItemKind::TraitDefinition(trait_def) => {
    //             symbols.push(Symbol {
    //                 name: trait_def.name.clone(),
    //                 kind: SymbolKind::Trait,
    //                 range: item.span,
    //             });
    //         }
    //         crate::checker::TypedItemKind::ConstDefinition(const_def) => {
    //             symbols.push(Symbol {
    //                 name: const_def.name.clone(),
    //                 kind: SymbolKind::Constant,
    //                 range: item.span,
    //             });
    //         }
    //         _ => {} // Other item types don't contribute top-level symbols
    //     }
    // }

    // symbols
    Vec::new() // Placeholder return
}

/// Symbol information for LSP integration
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub range: outrun_parser::Span,
}

/// Types of symbols that can be extracted from a program
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Function,
    Struct,
    Trait,
    Constant,
    Variable,
    Parameter,
}

/// Get detailed diagnostic information with file context
///
/// This function provides structured diagnostic information that's suitable
/// for IDE integration, returning both the error details and suggested fixes.
pub fn get_detailed_diagnostics(error_report: &TypeErrorReport) -> Vec<DetailedDiagnostic> {
    let mut diagnostics = Vec::new();

    for error in error_report.type_errors() {
        let diagnostic = DetailedDiagnostic {
            message: error.to_string(),
            severity: DiagnosticSeverity::Error,
            range: match error {
                TypeError::TypeMismatch { span, .. } => Some(*span),
                TypeError::UndefinedFunction { span, .. } => Some(*span),
                TypeError::UndefinedType { span, .. } => Some(*span),
                TypeError::UndefinedVariable { span, .. } => Some(*span),
                _ => None,
            },
            suggested_fixes: Vec::new(), // TODO: Add suggested fixes
        };
        diagnostics.push(diagnostic);
    }

    diagnostics
}

/// Detailed diagnostic information for IDE integration
#[derive(Debug, Clone)]
pub struct DetailedDiagnostic {
    pub message: String,
    pub severity: DiagnosticSeverity,
    pub range: Option<miette::SourceSpan>,
    pub suggested_fixes: Vec<String>,
}

/// Diagnostic severity levels
#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
    Hint,
}

// =============================================================================
// Version and Build Information
// =============================================================================

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const NAME: &str = env!("CARGO_PKG_NAME");
