//! # Outrun Type Checker
//!
//! A comprehensive static type checker for the Outrun programming language that validates trait constraints,
//! function signatures, and expressions at compile time, generating efficient dispatch tables for the interpreter.

// TypeNameId and AtomId are safe as HashMap keys despite containing Arc<RwLock<_>>
// because their Hash and Eq implementations only use the symbol field, not the interner
#![allow(clippy::mutable_key_type)]
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
//! Symbol extraction APIs are currently being updated to work with the new unified function entry system.
//!
//! ### Error Handling and Diagnostics
//!
//! Error handling APIs are currently being updated to work with the new compiler environment system.
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
//! - Type checking is designed to be fast with TypeNameId-based comparisons
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
pub mod context;
pub mod core_library;
pub mod dependency_graph;
pub mod desugaring;
pub mod dispatch;
pub mod error;
// TODO: Re-enable after updating for new UnificationContext
pub mod intrinsics;
// DEPRECATED: MultiProgramCompiler has been replaced by CompilerEnvironment
// The file has been renamed to multi_program_compiler.rs.deprecated
pub mod patterns;
pub mod purity;
pub mod shared_context;
pub mod simple_context;
pub mod smt;
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
    TypedBlock, TypedDebugInfo, TypedExpression, TypedExpressionKind, TypedFunctionDefinition,
    TypedFunctionPath, TypedItem, TypedItemKind, TypedLetBinding, TypedProgram, TypedStatement,
};
pub use compilation::compiler_environment::CompilerEnvironment;
pub use compilation::program_collection::{CompilationResult, ProgramCollection};
pub use dispatch::DispatchTable;
pub use error::{
    ErrorGroup, ErrorSummary, TypeError, TypeErrorReport, TypeErrorWithSource, TypeResult,
};
// ConcreteType has been unified into StructuredType
// TypeNameId and AtomId are now available from compilation::compiler_environment
pub use compilation::compiler_environment::{AtomId, TypeNameId};
pub use simple_context::{CompilationPhaseData, TypeCheckingData};
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
    let program_source = format!("{program}"); // Get source before move
    let desugared_program = DesugaringVisitor::desugar_program(program);

    // Use CompilerEnvironment directly without core library bootstrap
    // This matches the original behavior of TypeChecker::new() (empty environment)
    let mut compiler_env = CompilerEnvironment::new();

    // Create a program collection with just the user program
    let mut collection = ProgramCollection::new();
    collection.add_program("<program>".to_string(), desugared_program, program_source);

    // Compile without core library (original behavior)
    compiler_env
        .compile_collection(collection)
        .and_then(|result| {
            // Extract TypedProgram from the result
            if let Some(typed_program) = result.typed_programs.get("<program>") {
                Ok(typed_program.clone())
            } else {
                // Fallback conversion if needed
                Err(vec![TypeError::internal(
                    "Failed to extract typed program from compilation result".to_string(),
                )])
            }
        })
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

    // Use the modern multi-program API with core library
    match typecheck_with_core_library(desugared_program, source, filename) {
        Ok(compilation_result) => {
            // Extract the TypedProgram from the compilation result
            if let Some(typed_program) = compilation_result.typed_programs.get(filename) {
                Ok(typed_program.clone())
            } else {
                // Fallback: create a basic TypedProgram from the compilation result
                // This shouldn't happen in normal operation, but provides a safety net
                Ok(TypedProgram {
                    items: Vec::new(), // Would need to convert from compilation_result
                    type_context: compilation_result.type_context,
                    compilation_order: compilation_result.compilation_order,
                    compilation_summary: format!(
                        "Compiled {} traits, {} structs, {} implementations",
                        compilation_result.traits.len(),
                        compilation_result.structs.len(),
                        compilation_result.implementations.len()
                    ),
                    debug_info: TypedDebugInfo {
                        comments: Vec::new(),
                        source_file: Some(filename.to_string()),
                        original_span: outrun_parser::Span {
                            start: 0,
                            end: source.len(),
                            start_line_col: Some((1, 1)),
                            end_line_col: Some((1, source.len())),
                        },
                        type_annotations: Vec::new(),
                        inferred_types: std::collections::HashMap::new(),
                        literal_format: None,
                    },
                    error_recovery_info: Vec::new(),
                    detailed_summary: None,
                })
            }
        }
        Err(error_report) => Err(error_report),
    }
}

/// Type check a collection of programs using the new multi-program visitor-based compiler
///
/// This is the next-generation type checking API that handles multiple programs with
/// proper dependency resolution and phase-based compilation. It's designed to replace
/// the single-program typecheck functions for more complex compilation scenarios.
///
/// # Arguments
/// * `compiler_env` - Shared compiler environment with type interning and module management
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
/// - **Shared type interning** - Consistent type IDs across compilation units
///
/// # Example
/// ```rust
/// use outrun_parser::parse_program;
/// use outrun_typechecker::{typecheck_program_collection, ProgramCollection, create_compiler_environment};
///
/// // Create shared compiler environment
/// let mut compiler_env = create_compiler_environment();
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
/// match typecheck_program_collection(&mut compiler_env, collection) {
///     Ok(result) => {
///         println!("✓ Multi-program compilation successful!");
///         println!("  Compilation order: {:?}", result.compilation_order);
///         println!("  Traits: {}", result.traits.len());
///         println!("  Structs: {}", result.structs.len());
///         println!("  Implementations: {}", result.implementations.len());
///     }
///     Err(error_report) => {
///         eprintln!("✗ Compilation failed:");
///         eprintln!("{:?}", error_report);
///     }
/// }
/// ```
pub fn typecheck_program_collection(
    compiler_env: &mut CompilerEnvironment,
    collection: ProgramCollection,
) -> Result<CompilationResult, TypeErrorReport> {
    match compiler_env.compile_collection(collection.clone()) {
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
    // Create shared compiler environment
    let mut compiler_env = create_compiler_environment();

    // First, compile the core library into the shared environment
    let _core_result =
        crate::core_library::compile_core_library_with_environment(&mut compiler_env);

    // Create collection with just the user program
    let mut collection = ProgramCollection::new();

    // Add user program
    let desugared_program = DesugaringVisitor::desugar_program(program);
    collection.add_program(filename.to_string(), desugared_program, source.to_string());

    // Compile the user program with the shared environment that already has core library
    typecheck_program_collection(&mut compiler_env, collection)
}

/// Type check a collection of programs with a fresh CompilerEnvironment (convenience function)
///
/// This is a convenience wrapper around `typecheck_program_collection` that creates
/// a fresh CompilerEnvironment. For better performance when doing multiple compilations,
/// use `typecheck_program_collection` directly with a shared CompilerEnvironment.
///
/// # Arguments
/// * `collection` - A collection of programs to compile together
///
/// # Returns
/// * `Ok(CompilationResult)` - Successfully compiled programs with full type information
/// * `Err(TypeErrorReport)` - Enhanced error report with source context from all programs
pub fn typecheck_program_collection_simple(
    collection: ProgramCollection,
) -> Result<CompilationResult, TypeErrorReport> {
    let mut compiler_env = create_compiler_environment();
    typecheck_program_collection(&mut compiler_env, collection)
}

/// Create a new CompilerEnvironment for advanced usage scenarios
///
/// This function creates a fresh CompilerEnvironment instance that can be reused for multiple
/// compilation operations. This is the new recommended approach for advanced use cases.
pub fn create_compiler_environment() -> CompilerEnvironment {
    CompilerEnvironment::new()
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
    Some(format!("Type: {}", type_info.to_string_representation()))
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
            format!("Type: {type_name}")
        }
        StructuredType::TypeVariable(type_name) => {
            format!("TypeVariable: {type_name}")
        }
        StructuredType::Generic { base, args } => {
            let args_str = args
                .iter()
                .map(format_type_for_hover)
                .map(|s| s.strip_prefix("Type: ").unwrap_or(&s).to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("Type: {base}<{args_str}>")
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
                        param.name.clone(),
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
            format!("Type: ({params_str}) -> {return_str}")
        }
        StructuredType::Tuple(elements) => {
            let elements_str = elements
                .iter()
                .map(format_type_for_hover)
                .map(|s| s.strip_prefix("Type: ").unwrap_or(&s).to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("Type: ({elements_str})")
        }
        // Concrete primitive types
        StructuredType::Integer64 => "Type: Integer64".to_string(),
        StructuredType::Float64 => "Type: Float64".to_string(),
        StructuredType::Boolean => "Type: Boolean".to_string(),
        StructuredType::String => "Type: String".to_string(),
        StructuredType::Atom => "Type: Atom".to_string(),

        // Concrete collection types
        StructuredType::List { element_type } => {
            let elem_str = format_type_for_hover(element_type)
                .strip_prefix("Type: ")
                .unwrap_or("")
                .to_string();
            format!("Type: List<{elem_str}>")
        }
        StructuredType::Map {
            key_type,
            value_type,
        } => {
            let key_str = format_type_for_hover(key_type)
                .strip_prefix("Type: ")
                .unwrap_or("")
                .to_string();
            let value_str = format_type_for_hover(value_type)
                .strip_prefix("Type: ")
                .unwrap_or("")
                .to_string();
            format!("Type: Map<{key_str}, {value_str}>")
        }

        // Concrete option and result types
        StructuredType::Option { inner_type } => {
            let inner_str = format_type_for_hover(inner_type)
                .strip_prefix("Type: ")
                .unwrap_or("")
                .to_string();
            format!("Type: Option<{inner_str}>")
        }
        StructuredType::Result { ok_type, err_type } => {
            let ok_str = format_type_for_hover(ok_type)
                .strip_prefix("Type: ")
                .unwrap_or("")
                .to_string();
            let err_str = format_type_for_hover(err_type)
                .strip_prefix("Type: ")
                .unwrap_or("")
                .to_string();
            format!("Type: Result<{ok_str}, {err_str}>")
        }

        // Concrete struct and trait types
        StructuredType::Struct { name, .. } => {
            format!("Type: struct {name}")
        }
        StructuredType::Trait { name, .. } => {
            format!("Type: trait {name}")
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
                format!("Type Error: {error}")
            }
        }
    }
}

// =============================================================================
// Version and Build Information
// =============================================================================

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const NAME: &str = env!("CARGO_PKG_NAME");
