use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, Result};
use outrun_parser::parse_program;
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;
use std::process;

mod sexpr;

#[derive(Parser)]
#[command(
    name = "outrun",
    version,
    about = "The Outrun programming language toolchain",
    long_about = "Outrun is a statically-typed, functional programming language built around traits.",
    before_help = format!("🌆 OUTRUN Programming Language v{} 🌃\n🟣 A statically-typed, functional language built around traits 🔮\n", env!("CARGO_PKG_VERSION"))
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
        
        /// Show detailed span information in output
        #[arg(short, long)]
        spans: bool,
    },
}

fn main() {
    let cli = Cli::parse();
    
    match cli.command {
        Some(Commands::Parse { files, spans }) => {
            handle_parse_command(files, spans);
        }
        None => {
            // No subcommand provided, show help
            Cli::parse_from(["outrun", "--help"]);
        }
    }
}


fn handle_parse_command(files: Vec<PathBuf>, spans: bool) {
    let mut success = true;
    let multiple_files = files.len() > 1;
    
    for file_path in files {
        let display_name = if file_path.to_str() == Some("-") {
            "<stdin>".to_string()
        } else {
            file_path.display().to_string()
        };
        
        match parse_single_file(&file_path, spans) {
            Ok(()) => {
                if multiple_files {
                    println!("✅ {}", display_name);
                }
            }
            Err(e) => {
                // Use miette's beautiful error reporting
                eprintln!("{:?}", e);
                success = false;
            }
        }
    }
    
    if !success {
        process::exit(1);
    }
}

fn parse_single_file(
    file_path: &PathBuf, 
    spans: bool
) -> Result<()> {
    let (source, _source_name) = if file_path.to_str() == Some("-") {
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
            return Err(miette::miette!("Expected .outrun file, got: {}", file_path.display()));
        }
        
        // Read file contents
        let source = fs::read_to_string(file_path).into_diagnostic()?;
        (source, file_path.display().to_string())
    };
    
    // Parse with outrun-parser - this will use miette's beautiful error reporting
    match parse_program(&source) {
        Ok(ast) => {
            print_ast(&ast, spans);
            Ok(())
        }
        Err(parse_error) => {
            // The ParseError already has miette integration with source code
            Err(parse_error.into())
        }
    }
}

fn print_ast(ast: &outrun_parser::Program, spans: bool) {
    if spans {
        // Show full AST with span information for debugging
        println!("{:#?}", ast);
    } else {
        // Clean debug output without spans for readability
        println!("{}", format_ast_clean(ast));
    }
}

fn format_ast_clean(ast: &outrun_parser::Program) -> String {
    sexpr::format_program_as_sexpr(ast)
}

