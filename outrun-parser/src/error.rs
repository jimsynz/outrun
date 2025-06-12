// Outrun Parser Error Handling
// Beautiful error reporting with miette integration

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Main parse error type with miette integration
#[derive(Error, Diagnostic, Debug)]
pub enum ParseError {
    #[error("Parse error")]
    #[diagnostic(
        code(outrun::parse::pest_error),
        help("Check the syntax near the highlighted location")
    )]
    PestError {
        #[source_code]
        src: String,
        #[label("error occurred here")]
        span: SourceSpan,
        message: String,
    },

    #[error("Unexpected token")]
    #[diagnostic(
        code(outrun::parse::unexpected_token),
        help("Expected one of: {expected}")
    )]
    UnexpectedToken {
        #[source_code]
        src: String,
        #[label("found this")]
        span: SourceSpan,
        expected: String,
    },

    #[error("Invalid keyword")]
    #[diagnostic(
        code(outrun::parse::invalid_keyword),
        help("Keywords must be one of the reserved words")
    )]
    InvalidKeyword {
        #[source_code]
        src: String,
        #[label("invalid keyword")]
        span: SourceSpan,
        found: String,
    },

    #[error("Invalid boolean literal")]
    #[diagnostic(
        code(outrun::parse::invalid_boolean),
        help("Boolean literals must be 'true' or 'false'")
    )]
    InvalidBoolean {
        #[source_code]
        src: String,
        #[label("invalid boolean")]
        span: SourceSpan,
        found: String,
    },

    #[error("Invalid integer literal")]
    #[diagnostic(
        code(outrun::parse::invalid_integer),
        help("Integer literals must be valid decimal numbers")
    )]
    InvalidInteger {
        #[source_code]
        src: String,
        #[label("invalid integer")]
        span: SourceSpan,
        found: String,
    },

    #[error("Invalid float literal")]
    #[diagnostic(
        code(outrun::parse::invalid_float),
        help("Float literals must be valid decimal numbers with decimal points (e.g., 3.14, 1.23e-4)")
    )]
    InvalidFloat {
        #[source_code]
        src: String,
        #[label("invalid float")]
        span: SourceSpan,
        found: String,
    },

    #[error("Invalid string escape sequence")]
    #[diagnostic(
        code(outrun::parse::invalid_string_escape),
        help("Valid escape sequences: \\n, \\t, \\r, \\\\, \\\", \\uXXXX")
    )]
    InvalidStringEscape {
        #[source_code]
        src: String,
        #[label("invalid escape sequence")]
        span: SourceSpan,
        found: String,
    },

    #[error("Unexpected grammar rule")]
    #[diagnostic(
        code(outrun::parse::unexpected_rule),
        help("Expected rule: {expected}")
    )]
    UnexpectedRule {
        expected: String,
        found: crate::parser::Rule,
        span: crate::ast::Span,
    },

    #[error("Invalid spread element")]
    #[diagnostic(
        code(outrun::parse::invalid_spread),
        help("Spread elements must be '..identifier'")
    )]
    InvalidSpreadElement { span: crate::ast::Span },
}

impl ParseError {
    /// Create a parse error from a Pest parsing error
    pub fn from_pest_error(error: pest::error::Error<crate::parser::Rule>, src: String) -> Self {
        let span = match error.location {
            pest::error::InputLocation::Pos(pos) => SourceSpan::new(pos.into(), 1),
            pest::error::InputLocation::Span((start, end)) => {
                SourceSpan::new(start.into(), end - start)
            }
        };

        // Enhanced error message with debugging details
        let detailed_message = format!(
            "{}\n\nDEBUG INFO:\n- Error variant: {:?}\n- Line/col: {:?}",
            error, error.variant, error.line_col
        );

        ParseError::PestError {
            src,
            span,
            message: detailed_message,
        }
    }

    /// Create an unexpected token error
    pub fn unexpected_token(src: String, span: SourceSpan, expected: String) -> Self {
        ParseError::UnexpectedToken {
            src,
            span,
            expected,
        }
    }

    /// Create an invalid keyword error
    pub fn invalid_keyword(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidKeyword { src, span, found }
    }

    /// Create an invalid boolean error
    pub fn invalid_boolean(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidBoolean { src, span, found }
    }

    /// Create an invalid integer error
    pub fn invalid_integer(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidInteger { src, span, found }
    }

    /// Create an invalid float error
    pub fn invalid_float(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidFloat { src, span, found }
    }

    /// Create an invalid string escape error
    pub fn invalid_string_escape(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidStringEscape { src, span, found }
    }
}

/// Result type for parsing operations
pub type ParseResult<T> = Result<T, ParseError>;
