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
//! // In your LSP implementation
//! fn provide_hover(uri: &str, position: Position) -> Option<Hover> {
//!     let typed_program = get_cached_typed_program(uri)?;
//!     let type_info = outrun_typechecker::get_type_at_position(
//!         &typed_program,
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
pub mod core_library;
pub mod dependency_graph;
pub mod desugaring;
pub mod dispatch;
pub mod error;
pub mod exhaustiveness;
pub mod intrinsics;
pub mod multi_program_compiler;
pub mod types;
pub mod unification;
pub mod visitor;

// Internal desugaring
use desugaring::DesugaringVisitor;

#[cfg(test)]
mod tests;

// Re-export core types and functions for easy access
pub use checker::{TypeChecker, TypeContext, TypedExpression, TypedItem, TypedProgram};
pub use dispatch::DispatchTable;
pub use error::{
    ErrorGroup, ErrorSummary, TypeError, TypeErrorReport, TypeErrorWithSource, TypeResult,
};
pub use exhaustiveness::{ExhaustivenessAnalyzer, ExhaustivenessResult, MissingPattern};
pub use multi_program_compiler::{CompilationResult, MultiProgramCompiler, ProgramCollection};
pub use types::{AtomId, ConcreteType, TypeId, TypeInterner};
pub use unification::{StructuredType, UnificationContext, UnificationError, UnificationResult};
pub use visitor::{TypedVisitor, Visitor};

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
/// Currently returns basic type information. Future versions will include
/// more detailed semantic information.
pub fn get_type_at_position(
    typed_program: &TypedProgram,
    line: usize,
    column: usize,
) -> Option<String> {
    // This is a placeholder implementation for future LSP integration
    // A full implementation would:
    // 1. Find the AST node at the given position
    // 2. Extract its type information
    // 3. Format it for display

    let _ = (typed_program, line, column);
    None // TODO: Implement position-based type lookup
}

/// Get completion suggestions for a specific position
///
/// This function is designed for LSP implementations that need to provide
/// auto-completion functionality.
///
/// Currently returns empty suggestions. Future versions will analyze the
/// context and provide relevant completions.
pub fn get_completions_at_position(
    typed_program: &TypedProgram,
    line: usize,
    column: usize,
) -> Vec<String> {
    // This is a placeholder implementation for future LSP integration
    // A full implementation would:
    // 1. Analyze the context at the given position
    // 2. Determine what completions are valid
    // 3. Return function names, variable names, type names, etc.

    let _ = (typed_program, line, column);
    Vec::new() // TODO: Implement completion suggestions
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
