use clap::{Parser, Subcommand};
use miette::{Diagnostic, IntoDiagnostic, MietteHandlerOpts, NamedSource, Result, SourceSpan};
use outrun_parser::parse_program_with_diagnostics_and_source;
use outrun_typechecker::{CompilationResult, core_library, package};
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;
use std::process;
use thiserror::Error;

mod sexpr;
mod simple_repl; // New simplified REPL with new interpreter

use simple_repl::{SimpleReplConfig, SimpleReplSession};

#[derive(Parser)]
#[command(
    name = "outrun",
    version,
    about = "The Outrun programming language toolchain",
    long_about = "Outrun is a statically-typed, functional programming language built around protocols.",
    before_help = format!("🌆 OUTRUN Programming Language v{} 🌃\n🟣 A statically-typed, functional language built around protocols 🔮\n", env!("CARGO_PKG_VERSION"))
)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse Outrun source files and display their AST (debug only)
    Parse {
        /// Outrun source files to parse (use '-' to read from stdin)
        #[arg(required = true, value_name = "FILE")]
        files: Vec<PathBuf>,
    },
    /// Type check Outrun source files and display parsing and type checking results
    Typecheck {
        /// Outrun source files to type check (use '-' to read from stdin)
        #[arg(value_name = "FILE")]
        files: Vec<PathBuf>,

        /// Type check only the core library (ignore files)
        #[arg(long)]
        core_lib: bool,
    },
    /// Start an interactive REPL session for evaluating Outrun expressions
    Repl {
        /// Show type information with results (note: no type checking yet)
        #[arg(long, short = 't')]
        show_types: bool,

        /// Enable verbose error messages
        #[arg(long, short = 'v')]
        verbose: bool,

        /// Load program context from file (for variables and functions) - not yet implemented
        #[arg(long, value_name = "FILE")]
        context: Option<PathBuf>,
    },
}

fn main() {
    // Set up beautiful miette error reporting with syntax highlighting
    setup_miette_handler();

    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Parse { files }) => {
            handle_parse_command(files);
        }
        Some(Commands::Typecheck { files, core_lib }) => {
            // Debug span corruption issue
            if files.len() == 1 && files[0].to_string_lossy() == "debug-spans" {
                outrun_typechecker::debug_spans::debug_minimal_typecheck();
                return;
            }
            
            handle_typecheck_command(files, core_lib);
        }
        Some(Commands::Repl {
            show_types,
            verbose,
            context,
        }) => {
            handle_repl_command(show_types, verbose, context);
        }
        None => {
            // No subcommand provided, show help
            Cli::parse_from(["outrun", "--help"]);
        }
    }
}

/// Configure miette for beautiful error reporting with syntax highlighting
fn setup_miette_handler() {
    use miette::highlighters::SyntectHighlighter;
    use syntect::highlighting::ThemeSet;

    // Load pre-compiled syntax set (fast!)
    let syntax_set = load_precompiled_syntax_set();
    let theme_set = ThemeSet::load_defaults();

    // Create custom syntect highlighter with our syntax set
    // Use the "base16-ocean.dark" theme which is a nice default
    let theme = &theme_set.themes["base16-ocean.dark"];
    let highlighter = SyntectHighlighter::new(syntax_set, theme.clone(), true);

    miette::set_hook(Box::new(move |_| {
        Box::new(
            MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(true)
                .color(true)
                .tab_width(4)
                .with_cause_chain()
                .with_syntax_highlighting(highlighter.clone())
                .build(),
        )
    }))
    .ok();
}

/// Load pre-compiled syntax set for instant startup
fn load_precompiled_syntax_set() -> syntect::parsing::SyntaxSet {
    // Option 1: Embedded binary data (fastest - no file I/O)
    #[cfg(feature = "embedded-syntax")]
    {
        const SYNTAX_DATA: &[u8] = include_bytes!("../outrun_syntax.dump");
        match syntect::dumps::from_uncompressed_data(SYNTAX_DATA) {
            Ok(syntax_set) => return syntax_set,
            Err(e) => eprintln!("Warning: Failed to load embedded syntax: {e}"),
        }
    }

    // Option 2: Load from file (fast - for development)
    #[cfg(any(feature = "file-syntax", not(feature = "embedded-syntax")))]
    {
        use std::path::Path;
        let dump_path = Path::new("outrun_syntax.dump");
        if dump_path.exists() {
            match std::fs::read(dump_path) {
                Ok(data) => match syntect::dumps::from_uncompressed_data(&data) {
                    Ok(syntax_set) => return syntax_set,
                    Err(e) => eprintln!("Warning: Failed to parse pre-compiled syntax: {e}"),
                },
                Err(e) => eprintln!("Warning: Failed to read pre-compiled syntax file: {e}"),
            }
        }
    }

    // Fallback to default syntaxes if embedded version not available
    eprintln!("Warning: Using default syntax highlighting (no Outrun support)");
    eprintln!("Note: Outrun syntax should be embedded by default. This suggests a build issue.");
    syntect::parsing::SyntaxSet::load_defaults_newlines()
}

fn handle_parse_command(files: Vec<PathBuf>) {
    let mut success = true;
    let multiple_files = files.len() > 1;

    for file_path in files {
        let display_name = if file_path.to_str() == Some("-") {
            "<stdin>".to_string()
        } else {
            file_path.display().to_string()
        };

        match parse_single_file(&file_path) {
            Ok(()) => {
                if multiple_files {
                    println!("✅ {display_name}");
                }
            }
            Err(e) => {
                // Use miette's beautiful error reporting
                eprintln!("{e:?}");
                success = false;
            }
        }
    }

    if !success {
        process::exit(1);
    }
}

fn parse_single_file(file_path: &PathBuf) -> Result<()> {
    let (source, source_name) = if file_path.to_str() == Some("-") {
        // Read from stdin
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer).into_diagnostic()?;
        (buffer, "<stdin>".to_string())
    } else {
        // Validate file exists and has .outrun extension
        if !file_path.exists() {
            return Err(miette::miette!("File not found: {}", file_path.display()));
        }

        if file_path.extension().and_then(|s| s.to_str()) != Some("outrun") {
            return Err(miette::miette!(
                "Expected .outrun file, got: {}",
                file_path.display()
            ));
        }

        // Read file contents
        let source = fs::read_to_string(file_path).into_diagnostic()?;
        (source, file_path.display().to_string())
    };

    // Parse with outrun-parser using comprehensive diagnostics and source file tracking
    let (maybe_ast, diagnostics) =
        parse_program_with_diagnostics_and_source(&source, Some(source_name.clone()));

    // Print any diagnostics (errors, warnings, info) with beautiful formatting
    if diagnostics.has_diagnostics() {
        for report in diagnostics.create_reports_with_filename(&source_name) {
            eprintln!("{report:?}");
        }

        // Print summary
        let summary = diagnostics.summary();
        if summary.total > 0 {
            eprintln!("\n📊 Diagnostics Summary: {summary}");
        }

        // Return error if there were actual errors (not just warnings/info)
        if diagnostics.has_errors() {
            return Err(miette::miette!(
                "Parsing failed with {} errors",
                diagnostics.error_count()
            ));
        }
    }

    // If we have a successfully parsed AST, print it
    if let Some(ast) = maybe_ast {
        print_ast(&ast);
        Ok(())
    } else {
        // This case should not happen since we checked has_errors above
        Err(miette::miette!("Unknown parsing failure"))
    }
}

fn print_ast(ast: &outrun_parser::Program) {
    // Clean debug output without spans for readability
    println!("{}", format_ast_clean(ast));
}

fn format_ast_clean(ast: &outrun_parser::Program) -> String {
    sexpr::format_program_as_sexpr(ast)
}

fn typecheck_core_library() -> Result<()> {
    println!("🔬 TYPE CHECKING CORE LIBRARY:");
    println!("{}", "=".repeat(60));

    // Use the new typechecker API to precompile core library
    match CompilationResult::precompile_core_library() {
        Ok(compilation_result) => {
            println!("✅ Core library type checking successful!");

            // Print compilation summary
            println!("\n📊 CORE LIBRARY COMPILATION SUMMARY:");
            println!("{}", "-".repeat(40));

            // Access the compilation result details
            println!(
                "• Function Registry: {} entries",
                compilation_result.function_registry.function_count()
            );
            println!(
                "• Protocol Registry: {} entries",
                compilation_result.protocol_registry.implementation_count()
            );
            println!(
                "• Dispatch Table: {} entries",
                compilation_result.dispatch_table.len()
            );
            println!(
                "• Universal Dispatch: {} entries",
                compilation_result
                    .universal_dispatch
                    .get_all_function_signatures()
                    .len()
            );
            println!("• Programs: {} compiled", compilation_result.programs.len());

            Ok(())
        }
        Err(error) => {
            println!("❌ Core library type checking failed!");
            println!("{}", "-".repeat(40));

            // Try to create a beautiful miette report with source context
            if let Err(report_error) = create_miette_report_with_source_context(&error) {
                // Fallback to basic error display if we can't create a rich report
                eprintln!("Error creating detailed report: {report_error}");
                eprintln!("{error:?}");
            }

            Err(miette::miette!(
                "Core library type checking failed: {}",
                error
            ))
        }
    }
}

/// A custom diagnostic error for beautiful source context display
#[derive(Error, Diagnostic, Debug)]
#[error("{message}")]
#[diagnostic(code(outrun::cli::compilation_error))]
struct OutrunDiagnostic {
    message: String,

    #[source_code]
    src: NamedSource<String>,

    #[label("error occurred here")]
    error_span: SourceSpan,
}

/// Extract span and message information from any CompilerError type generically
fn extract_error_info(error: &outrun_typechecker::CompilerError) -> (Option<&SourceSpan>, String) {
    use outrun_typechecker::{
        CompilerError, ConstraintError, DispatchError, InferenceError, TypecheckError,
        UnificationError,
    };

    match error {
        CompilerError::Parse(parse_error) => {
            // Parse errors already have miette diagnostics, extract what we can
            (None, format!("Parse error: {}", parse_error))
        }
        CompilerError::Typecheck(boxed_error) => match boxed_error.as_ref() {
            TypecheckError::InferenceError(inference_error) => match inference_error {
                InferenceError::AmbiguousType { span, suggestions } => (
                    span.as_ref(),
                    format!(
                        "Type inference failed: ambiguous expression. Suggestions: {}",
                        suggestions.join(", ")
                    ),
                ),
                InferenceError::UndefinedVariable {
                    span,
                    variable_name,
                    ..
                } => (
                    span.as_ref(),
                    format!("Undefined variable: {}", variable_name),
                ),
                InferenceError::UndefinedType {
                    span, type_name, ..
                } => (span.as_ref(), format!("Undefined type: {}", type_name)),
                InferenceError::FunctionCallError { span, message, .. } => {
                    (span.as_ref(), format!("Function call error: {}", message))
                }
                InferenceError::CollectionMismatch { span, message, .. } => {
                    (span.as_ref(), format!("Collection type error: {}", message))
                }
                InferenceError::EmptyCollectionNeedsAnnotation {
                    span,
                    collection_type,
                    ..
                } => (
                    span.as_ref(),
                    format!("Empty {} needs type annotation", collection_type),
                ),
                InferenceError::InvalidConstraintVariable {
                    span,
                    variable_name,
                    ..
                } => (
                    span.as_ref(),
                    format!("Invalid constraint variable: {}", variable_name),
                ),
                _ => (None, "Type inference error".to_string()),
            },
            TypecheckError::DispatchError(dispatch_error) => match dispatch_error {
                DispatchError::NoImplementation {
                    file_span,
                    protocol_name,
                    type_name,
                    suggestions,
                    ..
                } => {
                    let suggestion_text = if suggestions.is_empty() {
                        String::new()
                    } else {
                        format!(" Try: {}", suggestions.join(", "))
                    };
                    
                    // Include filename in error message if available
                    let filename_info = if let Some(file_span) = file_span {
                        format!(" [File: {}]", file_span.filename())
                    } else {
                        String::new()
                    };
                    
                    (
                        None, // We'll let the old span-guessing logic handle the span for now
                        format!(
                            "No implementation found: type {} does not implement protocol {}.{}{}",
                            type_name, protocol_name, suggestion_text, filename_info
                        ),
                    )
                }
                DispatchError::AmbiguousDispatch {
                    span,
                    protocol_name,
                    candidates,
                } => (
                    span.as_ref(),
                    format!(
                        "Ambiguous dispatch: multiple implementations found for {}. Candidates: {}",
                        protocol_name,
                        candidates.join(", ")
                    ),
                ),
                DispatchError::UnresolvedTypeVariable {
                    span,
                    protocol_name,
                } => (
                    span.as_ref(),
                    format!(
                        "Unresolved type variable: cannot dispatch on unknown type for protocol {}",
                        protocol_name
                    ),
                ),
                DispatchError::UnboundSelfType {
                    span,
                    protocol_name,
                } => (
                    span.as_ref(),
                    format!(
                        "Unbound Self type: cannot dispatch on unresolved Self for protocol {}",
                        protocol_name
                    ),
                ),
                DispatchError::InvalidTarget {
                    span,
                    protocol_name,
                    target_description,
                } => (
                    span.as_ref(),
                    format!(
                        "Invalid dispatch target: {} cannot be called on {}",
                        protocol_name, target_description
                    ),
                ),
            },
            TypecheckError::UnificationError(unification_error) => match unification_error {
                UnificationError::TypeMismatch {
                    span,
                    expected,
                    found,
                    ..
                } => (
                    span.as_ref(),
                    format!("Type mismatch: expected {}, found {}", expected, found),
                ),
                UnificationError::OccursCheckViolation {
                    span,
                    var_name,
                    containing_type,
                    ..
                } => (
                    span.as_ref(),
                    format!(
                        "Occurs check violation: variable {} occurs in {}",
                        var_name, containing_type
                    ),
                ),
                _ => (None, "Type unification error".to_string()),
            },
            TypecheckError::ConstraintError(constraint_error) => match constraint_error {
                ConstraintError::Unsatisfiable { span, constraint } => (
                    span.as_ref(),
                    format!("Unsatisfiable constraint: {}", constraint),
                ),
                ConstraintError::ConflictingConstraints {
                    span1,
                    constraint1,
                    constraint2,
                    ..
                } => (
                    span1.as_ref(),
                    format!(
                        "Conflicting constraints: {} conflicts with {}",
                        constraint1, constraint2
                    ),
                ),
                _ => (None, "Constraint solving error".to_string()),
            },
            TypecheckError::ImplementationError(_) => {
                (None, "Protocol implementation error".to_string())
            }
            TypecheckError::ExhaustivenessError(_) => {
                (None, "Exhaustiveness check failed".to_string())
            }
            TypecheckError::CoreLibraryError(message) => {
                (None, format!("Core library error: {}", message))
            }
            TypecheckError::Generic { message, span } => {
                (span.as_ref(), format!("Type checking error: {}", message))
            }
        },
        CompilerError::ModuleRedefinition { span, module_name } => (
            span.as_ref(),
            format!(
                "Module redefinition: module '{}' is already defined by a dependency package",
                module_name
            ),
        ),
    }
}

/// Extract FileSpan from errors that support it (new approach)
fn extract_file_span_from_error(error: &outrun_typechecker::CompilerError) -> Option<&outrun_typechecker::error::FileSpan> {
    use outrun_typechecker::{CompilerError, DispatchError, TypecheckError};

    match error {
        CompilerError::Typecheck(boxed_error) => match boxed_error.as_ref() {
            TypecheckError::DispatchError(dispatch_error) => match dispatch_error {
                DispatchError::NoImplementation { file_span, .. } => file_span.as_ref(),
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

/// Extract error message from any error type
fn extract_error_message(error: &outrun_typechecker::CompilerError) -> String {
    use outrun_typechecker::{CompilerError, DispatchError, TypecheckError};

    match error {
        CompilerError::Typecheck(boxed_error) => match boxed_error.as_ref() {
            TypecheckError::DispatchError(dispatch_error) => match dispatch_error {
                DispatchError::NoImplementation {
                    protocol_name,
                    type_name,
                    suggestions,
                    ..
                } => {
                    let suggestion_text = if suggestions.is_empty() {
                        String::new()
                    } else {
                        format!(" Try: {}", suggestions.join(", "))
                    };
                    format!(
                        "No implementation found: type {} does not implement protocol {}{}",
                        type_name, protocol_name, suggestion_text
                    )
                }
                _ => format!("Dispatch error: {}", dispatch_error),
            },
            _ => format!("Type checking error: {}", boxed_error),
        },
        _ => format!("Compiler error: {}", error),
    }
}

/// Create a miette report directly from a FileSpan (bypasses the guessing logic)
fn create_miette_report_with_file_span(file_span: &outrun_typechecker::error::FileSpan, message: &str) -> Result<()> {
    // Try to read the source file
    if let Ok(source_content) = std::fs::read_to_string(&file_span.source_file) {
        let filename = std::path::Path::new(&file_span.source_file)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");

        let named_source = NamedSource::new(filename, source_content);
        let diagnostic = OutrunDiagnostic {
            message: message.to_string(),
            src: named_source.clone(),
            error_span: file_span.to_source_span(),
        };

        let report = miette::Report::new(diagnostic).with_source_code(named_source);
        println!("\n💎 Beautiful miette error output:");
        println!("{:?}", report);
        Ok(())
    } else {
        Err(miette::miette!("Could not read source file: {}", file_span.source_file))
    }
}

/// Create a beautiful miette report with source context for compilation errors using span-to-program mapping
fn create_miette_report_with_source_context(
    error: &outrun_typechecker::CompilerError,
) -> Result<()> {
    // Check if this error has FileSpan information (new approach)
    if let Some(file_span) = extract_file_span_from_error(error) {
        return create_miette_report_with_file_span(file_span, &extract_error_message(error));
    }

    // Fallback to old approach for errors without FileSpan
    let (span, error_message) = extract_error_info(error);

    // If we have span information, try to create a report with source context
    if let Some(source_span) = span {
        // First, try to load the core library package to get the programs with source file info
        if let Ok(Some(loaded_package)) = outrun_typechecker::package::load_core_library_package() {
            // CORRECT APPROACH: Use span semantics to find the correct program
            // Each program corresponds to one source file, and spans within that program
            // reference positions within that specific file

            let span_offset = source_span.offset();
            let span_len = source_span.len();

            println!(
                "🔍 Looking for span [{}..{}] in {} core library programs",
                span_offset,
                span_offset + span_len,
                loaded_package.programs.len()
            );

            // The correct approach: for each program, check if this error could have originated
            // from that program by looking at span bounds and validating against file content
            for (i, program) in loaded_package.programs.iter().enumerate() {
                if let Some(source_file) = &program.debug_info.source_file {
                    // Try to read the source file content
                    if let Ok(source_content) = std::fs::read_to_string(source_file) {
                        let filename = std::path::Path::new(source_file)
                            .file_name()
                            .and_then(|n| n.to_str())
                            .unwrap_or("unknown");

                        // Check if this span is valid within this file
                        if span_offset < source_content.len()
                            && (span_offset + span_len) <= source_content.len()
                        {
                            // Extract the error text to verify it's sensible
                            let end_offset = (span_offset + span_len).min(source_content.len());
                            let error_text = &source_content[span_offset..end_offset];

                            // Only proceed if the error text looks reasonable
                            if error_text.trim().len() > 0 {
                                println!(
                                    "✅ Program {}: Found valid span in {} [{}..{}] -> {:?}",
                                    i, filename, span_offset, end_offset, error_text
                                );

                                // Create a miette report with the correct file context
                                let named_source = NamedSource::new(filename, source_content);
                                let diagnostic = OutrunDiagnostic {
                                    message: error_message,
                                    src: named_source.clone(),
                                    error_span: *source_span,
                                };

                                let report =
                                    miette::Report::new(diagnostic).with_source_code(named_source);

                                println!("\n💎 Beautiful miette error output:");
                                eprintln!("{:?}", report);
                                return Ok(());
                            } else {
                                println!(
                                    "❌ Program {}: {} contains span but points to whitespace: {:?}",
                                    i, filename, error_text
                                );
                            }
                        } else {
                            // Don't spam logs for programs that are clearly too small
                            if source_content.len() > span_offset {
                                println!(
                                    "⚠️  Program {}: {} has span bounds issue [{}..{}] vs file size {}",
                                    i,
                                    filename,
                                    span_offset,
                                    span_offset + span_len,
                                    source_content.len()
                                );
                            }
                        }
                    }
                }
            }

            println!(
                "❌ No program found containing valid span [{}..{}]",
                span_offset,
                span_offset + span_len
            );
        }

        // Fallback: try the old approach of scanning the directory
        if let Ok(core_library_path) = core_library::default_core_library_path() {
            for entry in std::fs::read_dir(&core_library_path).into_diagnostic()? {
                let entry = entry.into_diagnostic()?;
                let path = entry.path();

                if path.extension().and_then(|s| s.to_str()) == Some("outrun") {
                    let source_content = std::fs::read_to_string(&path).into_diagnostic()?;
                    let filename = path
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("unknown");

                    if source_span.offset() < source_content.len() {
                        let diagnostic = OutrunDiagnostic {
                            message: error_message,
                            src: NamedSource::new(filename, source_content),
                            error_span: *source_span,
                        };

                        eprintln!("{diagnostic:?}");
                        return Ok(());
                    }
                }
            }
        }
    }

    // Fallback to basic error display
    eprintln!("{error:?}");
    Ok(())
}

// TODO: Re-implement with new typechecker API
// fn typecheck_core_library() -> Result<()> {
//     println!("🔬 TYPE CHECKING CORE LIBRARY:");
//     println!("{}", "=".repeat(60));

//     // Load and compile just the core library
//     let collection = outrun_typechecker::core_library::load_core_library_collection();
//     let mut compiler_env = outrun_typechecker::CompilerEnvironment::new();

//     match compiler_env.compile_collection(collection) {
//         Ok(result) => {
//             println!("✅ Core library type checking successful!");

//             // Debug print the typed AST
//             if !result.typed_programs.is_empty() {
//                 println!("\n📋 CORE LIBRARY TYPED AST DEBUG:");
//                 println!("{}", "-".repeat(40));
//                 for (filename, typed_program) in &result.typed_programs {
//                     println!("\n🗂️ File: {filename}");
//                     println!("{typed_program:#?}");
//                 }
//             }

//             println!("\n📊 COMPILATION SUMMARY:");
//             println!("{}", "-".repeat(40));
//             println!("• Protocols: {}", result.protocols.len());
//             println!("• Structs: {}", result.structs.len());
//             println!("• Implementations: {}", result.implementations.len());
//             println!("• Functions: {}", compiler_env.function_count());
//             println!("• Typed Programs: {}", result.typed_programs.len());

//             Ok(())
//         }
//         Err(errors) => {
//             println!("❌ Core library type checking failed!");
//             println!("{}", "-".repeat(40));

//             for error in &errors {
//                 eprintln!("{error:?}");
//                 eprintln!();
//             }

//             Err(miette::miette!(
//                 "Core library type checking failed with {} errors",
//                 errors.len()
//             ))
//         }
//     }
// }

fn handle_typecheck_command(files: Vec<PathBuf>, core_lib: bool) {
    if core_lib {
        // Type check the core library using new typechecker API
        match typecheck_core_library() {
            Ok(()) => {
                println!("✅ Core library type checking completed successfully");
            }
            Err(e) => {
                eprintln!("{e:?}");
                process::exit(1);
            }
        }
        return;
    }

    if files.is_empty() {
        eprintln!("Error: Must provide files to type check or use --core-lib");
        process::exit(1);
    }

    let mut success = true;
    let multiple_files = files.len() > 1;

    for file_path in files {
        let display_name = if file_path.to_str() == Some("-") {
            "<stdin>".to_string()
        } else {
            file_path.display().to_string()
        };

        match typecheck_single_file(&file_path) {
            Ok(()) => {
                if multiple_files {
                    println!("✅ {display_name}");
                }
            }
            Err(e) => {
                // Use miette's beautiful error reporting
                eprintln!("{e:?}");
                success = false;
            }
        }
    }

    if !success {
        process::exit(1);
    }
}

fn typecheck_single_file(file_path: &PathBuf) -> Result<()> {
    let (source, source_name) = if file_path.to_str() == Some("-") {
        // Read from stdin
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer).into_diagnostic()?;
        (buffer, "<stdin>".to_string())
    } else {
        // Validate file exists and has .outrun extension
        if !file_path.exists() {
            return Err(miette::miette!("File not found: {}", file_path.display()));
        }

        if file_path.extension().and_then(|s| s.to_str()) != Some("outrun") {
            return Err(miette::miette!(
                "Expected .outrun file, got: {}",
                file_path.display()
            ));
        }

        // Read file contents
        let source = fs::read_to_string(file_path).into_diagnostic()?;
        (source, file_path.display().to_string())
    };

    // Parse with outrun-parser using comprehensive diagnostics and source file tracking
    let (maybe_ast, diagnostics) =
        parse_program_with_diagnostics_and_source(&source, Some(source_name.clone()));

    // Print parsing results
    println!("🔍 PARSING RESULTS for {source_name}");
    println!("{}", "=".repeat(60));

    // Print any diagnostics (errors, warnings, info) with beautiful formatting
    if diagnostics.has_diagnostics() {
        for report in diagnostics.create_reports_with_filename(&source_name) {
            eprintln!("{report:?}");
        }

        // Print summary
        let summary = diagnostics.summary();
        if summary.total > 0 {
            eprintln!("\n📊 Parsing Diagnostics Summary: {summary}");
        }

        // Return error if there were actual errors (not just warnings/info)
        if diagnostics.has_errors() {
            return Err(miette::miette!(
                "Parsing failed with {} errors",
                diagnostics.error_count()
            ));
        }
    } else {
        println!("✅ No parsing errors");
    }

    // If we have a successfully parsed AST, print it and try to type check it
    if let Some(ast) = maybe_ast {
        // Print AST
        println!("\n📄 PARSED AST:");
        println!("{}", "-".repeat(40));
        print_ast(&ast);

        // TODO: Re-implement with new typechecker API
        // Now try to type check it
        println!("\n🔬 TYPE CHECKING RESULTS:");
        println!("{}", "=".repeat(60));

        // Use new typechecker API
        use outrun_typechecker::typecheck_program;
        let mut ast = ast; // Make mutable for new API
        match typecheck_program(&mut ast) {
            Ok(()) => {
                println!("✅ Type checking successful!");
                println!("\n📋 TYPED AST DEBUG:");
                println!("{}", "-".repeat(40));
                println!("{ast:#?}");
            }
            Err(error) => {
                println!("❌ Type checking failed: {error}");
                println!("{}", "-".repeat(40));

                eprintln!("{error:?}");

                return Err(miette::miette!("Type checking failed: {error}"));
            }
        }

        Ok(())
    } else {
        // This case should not happen since we checked has_errors above
        Err(miette::miette!("Unknown parsing failure"))
    }
}

fn handle_repl_command(show_types: bool, verbose: bool, context: Option<PathBuf>) {
    // Create REPL configuration based on command line options
    let config = SimpleReplConfig {
        show_types,
        verbose_errors: verbose,
        ..Default::default()
    };

    // TODO: Handle context file loading in future versions
    if let Some(_context_file) = context {
        eprintln!("Warning: --context option not yet implemented");
        eprintln!(
            "Note: Variables from previous sessions are not yet persistent across REPL restarts"
        );
    }

    // Create and start the REPL session
    match SimpleReplSession::with_config(config) {
        Ok(mut session) => {
            if let Err(e) = session.run() {
                eprintln!("REPL error: {e:?}");
                process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Failed to start REPL: {e:?}");
            process::exit(1);
        }
    }
}
