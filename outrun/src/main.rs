use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, MietteHandlerOpts, Result};
use outrun_parser::parse_program_with_diagnostics_and_source;
// use outrun_typechecker::typecheck_program_with_source;  // TODO: Fix with new typechecker API
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;
use std::process;

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
        // TODO: Re-implement with new typechecker API
        // Type check only the core library
        // match typecheck_core_library() {
        //     Ok(()) => {
        //         println!("✅ Core library type checking completed successfully");
        //     }
        //     Err(e) => {
        //         eprintln!("{e:?}");
        //         process::exit(1);
        //     }
        // }
        eprintln!("TODO: Core library type checking not yet implemented with new typechecker API");
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
