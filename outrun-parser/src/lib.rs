// Outrun Parser Library
// Pest-based parser for the Outrun programming language

pub mod ast;
pub mod diagnostics;
pub mod error;
pub mod parser;

#[cfg(test)]
mod tests;

pub use ast::*;
pub use diagnostics::*;
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

/// Parse program with comprehensive diagnostics collection
/// Returns the parsed program (if successful) and a diagnostic collector with all errors/warnings
pub fn parse_program_with_diagnostics(input: &str) -> (Option<Program>, DiagnosticCollector) {
    parse_program_with_diagnostics_and_source(input, None)
}

/// Parse program with comprehensive diagnostics collection and source file tracking
/// Returns the parsed program (if successful) and a diagnostic collector with all errors/warnings
pub fn parse_program_with_diagnostics_and_source(
    input: &str,
    source_file: Option<String>,
) -> (Option<Program>, DiagnosticCollector) {
    let mut collector = DiagnosticCollector::new(input.to_string());

    match parse_program_with_source(input, source_file) {
        Ok(program) => {
            // Could add any post-parse diagnostics here (unused variables, etc.)
            (Some(program), collector)
        }
        Err(parse_error) => {
            // Convert ParseError to DiagnosticError and add to collector
            let diagnostic = convert_parse_error_to_diagnostic(parse_error);
            collector.add_diagnostic(diagnostic);
            (None, collector)
        }
    }
}

/// Convert ParseError to DiagnosticError for better error reporting
fn convert_parse_error_to_diagnostic(parse_error: ParseError) -> DiagnosticError {
    match parse_error {
        ParseError::PestError { span, message, .. } => {
            let ast_span = Span::new(span.offset(), span.offset() + span.len());
            DiagnosticError::syntax_error(message, ast_span)
        }
        ParseError::UnexpectedToken { span, expected, .. } => {
            let ast_span = Span::new(span.offset(), span.offset() + span.len());
            DiagnosticError::unexpected_token("unknown".to_string(), expected, ast_span)
        }
        ParseError::InvalidKeyword { span, found, .. } => {
            let ast_span = Span::new(span.offset(), span.offset() + span.len());
            DiagnosticError::syntax_error(format!("Invalid keyword: {}", found), ast_span)
        }
        ParseError::InvalidBoolean { span, found, .. } => {
            let ast_span = Span::new(span.offset(), span.offset() + span.len());
            DiagnosticError::invalid_literal(
                format!("Invalid boolean literal: {}", found),
                "Use 'true' or 'false'".to_string(),
                ast_span,
            )
        }
        ParseError::InvalidInteger { span, found, .. } => {
            let ast_span = Span::new(span.offset(), span.offset() + span.len());
            DiagnosticError::invalid_literal(
                format!("Invalid integer literal: {}", found),
                "Use decimal, binary (0b), octal (0o), or hexadecimal (0x) format".to_string(),
                ast_span,
            )
        }
        ParseError::InvalidFloat { span, found, .. } => {
            let ast_span = Span::new(span.offset(), span.offset() + span.len());
            DiagnosticError::invalid_literal(
                format!("Invalid float literal: {}", found),
                "Use decimal format like 3.14 or scientific notation like 1.23e-4".to_string(),
                ast_span,
            )
        }
        ParseError::InvalidStringEscape { span, found, .. } => {
            let ast_span = Span::new(span.offset(), span.offset() + span.len());
            DiagnosticError::invalid_literal(
                format!("Invalid string escape sequence: {}", found),
                "Valid escapes: \\n, \\t, \\r, \\\\, \\\", \\uXXXX".to_string(),
                ast_span,
            )
        }
        ParseError::UnexpectedRule { span, .. } => {
            DiagnosticError::syntax_error("Unexpected grammar rule".to_string(), span)
        }
        ParseError::InvalidSpreadElement { span } => {
            DiagnosticError::syntax_error("Invalid spread element".to_string(), span)
        }
    }
}

// Version and metadata
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const NAME: &str = env!("CARGO_PKG_NAME");
