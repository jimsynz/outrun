// Outrun Parser Library
// Pest-based parser for the Outrun programming language

pub mod ast;
pub mod error;
pub mod parser;

pub use ast::*;
pub use error::*;
pub use parser::*;

// Re-export parser rule for manual testing
pub use parser::Rule;

// Main parsing functions
pub fn parse_program(input: &str) -> Result<Program, ParseError> {
    parser::OutrunParser::parse_program(input)
}

pub fn parse_program_with_source(
    input: &str,
    source_file: Option<String>,
) -> Result<Program, ParseError> {
    parser::OutrunParser::parse_program_with_source(input, source_file)
}

pub fn parse_expression(input: &str) -> Result<Expression, ParseError> {
    parser::OutrunParser::parse_expression(input)
}

// Version and metadata
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const NAME: &str = env!("CARGO_PKG_NAME");
