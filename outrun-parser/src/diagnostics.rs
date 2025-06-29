// Outrun Parser Diagnostics
// Diagnostic error types, severity levels, and error collection for parsing

use crate::ast::Span;
use miette::{Diagnostic, SourceSpan};
use std::fmt;
use thiserror::Error;

/// Severity level for diagnostic messages
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    /// Informational message (lowest priority)
    Info,
    /// Warning message - code will work but may have issues
    Warning,
    /// Error message - code cannot be processed
    Error,
    /// Fatal error - parsing must stop immediately
    Fatal,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Info => write!(f, "info"),
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
            Severity::Fatal => write!(f, "fatal"),
        }
    }
}

/// Diagnostic error with severity and source location
#[derive(Error, Diagnostic, Debug, Clone)]
pub enum DiagnosticError {
    #[error("Syntax error: {message}")]
    #[diagnostic(code(outrun::diagnostic::syntax_error), severity(Error))]
    SyntaxError {
        message: String,
        #[label("syntax error here")]
        span: SourceSpan,
    },

    #[error("Type error: {message}")]
    #[diagnostic(code(outrun::diagnostic::type_error), severity(Error))]
    TypeError {
        message: String,
        #[label("type error here")]
        span: SourceSpan,
    },

    #[error("Unused variable: {name}")]
    #[diagnostic(
        code(outrun::diagnostic::unused_variable),
        severity(Warning),
        help("Consider using underscore prefix to indicate intentionally unused variable")
    )]
    UnusedVariable {
        name: String,
        #[label("unused variable")]
        span: SourceSpan,
    },

    #[error("Deprecated syntax: {message}")]
    #[diagnostic(
        code(outrun::diagnostic::deprecated_syntax),
        severity(Warning),
        help("{suggestion}")
    )]
    DeprecatedSyntax {
        message: String,
        suggestion: String,
        #[label("deprecated syntax")]
        span: SourceSpan,
    },

    #[error("Missing semicolon")]
    #[diagnostic(
        code(outrun::diagnostic::missing_semicolon),
        severity(Error),
        help("Add a semicolon to end the statement")
    )]
    MissingSemicolon {
        #[label("expected semicolon here")]
        span: SourceSpan,
    },

    #[error("Unexpected token: found {found}, expected {expected}")]
    #[diagnostic(
        code(outrun::diagnostic::unexpected_token),
        severity(Error),
        help("Try using one of the expected tokens: {expected}")
    )]
    UnexpectedToken {
        found: String,
        expected: String,
        #[label("unexpected token")]
        span: SourceSpan,
    },

    #[error("Invalid literal: {message}")]
    #[diagnostic(
        code(outrun::diagnostic::invalid_literal),
        severity(Error),
        help("{suggestion}")
    )]
    InvalidLiteral {
        message: String,
        suggestion: String,
        #[label("invalid literal")]
        span: SourceSpan,
    },

    #[error("Performance warning: {message}")]
    #[diagnostic(
        code(outrun::diagnostic::performance_warning),
        severity(Info),
        help("{suggestion}")
    )]
    PerformanceWarning {
        message: String,
        suggestion: String,
        #[label("performance concern")]
        span: SourceSpan,
    },

    #[error("Fatal parsing error: {message}")]
    #[diagnostic(code(outrun::diagnostic::fatal_error), severity(Error))]
    FatalError {
        message: String,
        #[label("fatal error")]
        span: SourceSpan,
    },
}

impl DiagnosticError {
    /// Get the severity level of this diagnostic
    pub fn severity(&self) -> Severity {
        match self {
            DiagnosticError::SyntaxError { .. } => Severity::Error,
            DiagnosticError::TypeError { .. } => Severity::Error,
            DiagnosticError::UnusedVariable { .. } => Severity::Warning,
            DiagnosticError::DeprecatedSyntax { .. } => Severity::Warning,
            DiagnosticError::MissingSemicolon { .. } => Severity::Error,
            DiagnosticError::UnexpectedToken { .. } => Severity::Error,
            DiagnosticError::InvalidLiteral { .. } => Severity::Error,
            DiagnosticError::PerformanceWarning { .. } => Severity::Info,
            DiagnosticError::FatalError { .. } => Severity::Fatal,
        }
    }

    /// Get the source span of this diagnostic
    pub fn span(&self) -> SourceSpan {
        match self {
            DiagnosticError::SyntaxError { span, .. } => *span,
            DiagnosticError::TypeError { span, .. } => *span,
            DiagnosticError::UnusedVariable { span, .. } => *span,
            DiagnosticError::DeprecatedSyntax { span, .. } => *span,
            DiagnosticError::MissingSemicolon { span } => *span,
            DiagnosticError::UnexpectedToken { span, .. } => *span,
            DiagnosticError::InvalidLiteral { span, .. } => *span,
            DiagnosticError::PerformanceWarning { span, .. } => *span,
            DiagnosticError::FatalError { span, .. } => *span,
        }
    }

    /// Create a syntax error diagnostic
    pub fn syntax_error(message: String, span: Span) -> Self {
        DiagnosticError::SyntaxError {
            message,
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }

    /// Create a type error diagnostic
    pub fn type_error(message: String, span: Span) -> Self {
        DiagnosticError::TypeError {
            message,
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }

    /// Create an unused variable warning
    pub fn unused_variable(name: String, span: Span) -> Self {
        DiagnosticError::UnusedVariable {
            name,
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }

    /// Create a deprecated syntax warning
    pub fn deprecated_syntax(message: String, suggestion: String, span: Span) -> Self {
        DiagnosticError::DeprecatedSyntax {
            message,
            suggestion,
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }

    /// Create a missing semicolon error
    pub fn missing_semicolon(span: Span) -> Self {
        DiagnosticError::MissingSemicolon {
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }

    /// Create an unexpected token error
    pub fn unexpected_token(found: String, expected: String, span: Span) -> Self {
        DiagnosticError::UnexpectedToken {
            found,
            expected,
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }

    /// Create an invalid literal error
    pub fn invalid_literal(message: String, suggestion: String, span: Span) -> Self {
        DiagnosticError::InvalidLiteral {
            message,
            suggestion,
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }

    /// Create a performance warning
    pub fn performance_warning(message: String, suggestion: String, span: Span) -> Self {
        DiagnosticError::PerformanceWarning {
            message,
            suggestion,
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }

    /// Create a fatal error
    pub fn fatal_error(message: String, span: Span) -> Self {
        DiagnosticError::FatalError {
            message,
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        }
    }
}

/// Diagnostic collector that accumulates multiple errors during parsing
#[derive(Debug, Clone)]
pub struct DiagnosticCollector {
    /// Source code being parsed
    source: String,
    /// Collected diagnostics with their severity levels
    diagnostics: Vec<DiagnosticError>,
    /// Maximum number of errors to collect before stopping
    max_errors: usize,
    /// Whether to continue parsing after fatal errors
    continue_on_fatal: bool,
}

impl DiagnosticCollector {
    /// Create a new diagnostic collector
    pub fn new(source: String) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
            max_errors: 100, // Default maximum
            continue_on_fatal: false,
        }
    }

    /// Create a diagnostic collector with custom settings
    pub fn with_settings(source: String, max_errors: usize, continue_on_fatal: bool) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
            max_errors,
            continue_on_fatal,
        }
    }

    /// Add a diagnostic to the collection
    pub fn add_diagnostic(&mut self, diagnostic: DiagnosticError) {
        // Add the diagnostic if we haven't reached the limit
        if self.diagnostics.len() < self.max_errors {
            self.diagnostics.push(diagnostic);
        }
    }

    /// Add a diagnostic and return whether to stop processing (for batch operations)
    fn add_diagnostic_with_stop_check(&mut self, diagnostic: DiagnosticError) -> bool {
        // Add the diagnostic if we haven't reached the limit
        if self.diagnostics.len() < self.max_errors {
            let is_fatal = diagnostic.severity() == Severity::Fatal;
            self.diagnostics.push(diagnostic);

            // Return whether we should stop processing
            return is_fatal && !self.continue_on_fatal;
        }

        false
    }

    /// Add multiple diagnostics at once
    pub fn add_diagnostics(&mut self, diagnostics: Vec<DiagnosticError>) {
        for diagnostic in diagnostics {
            if self.add_diagnostic_with_stop_check(diagnostic) {
                // Stop if add_diagnostic_with_stop_check returned true (fatal error and shouldn't continue)
                break;
            }
        }
    }

    /// Check if there are any diagnostics
    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    /// Check if there are any errors (Error or Fatal severity)
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| matches!(d.severity(), Severity::Error | Severity::Fatal))
    }

    /// Check if there are any warnings
    pub fn has_warnings(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity() == Severity::Warning)
    }

    /// Check if there are any fatal errors
    pub fn has_fatal_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity() == Severity::Fatal)
    }

    /// Get all diagnostics
    pub fn diagnostics(&self) -> &[DiagnosticError] {
        &self.diagnostics
    }

    /// Get diagnostics filtered by severity
    pub fn diagnostics_by_severity(&self, severity: Severity) -> Vec<&DiagnosticError> {
        self.diagnostics
            .iter()
            .filter(|d| d.severity() == severity)
            .collect()
    }

    /// Get error count by severity
    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| matches!(d.severity(), Severity::Error | Severity::Fatal))
            .count()
    }

    /// Get warning count
    pub fn warning_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity() == Severity::Warning)
            .count()
    }

    /// Get info count
    pub fn info_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity() == Severity::Info)
            .count()
    }

    /// Get total diagnostic count
    pub fn total_count(&self) -> usize {
        self.diagnostics.len()
    }

    /// Clear all diagnostics
    pub fn clear(&mut self) {
        self.diagnostics.clear();
    }

    /// Get the source code
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Set maximum number of errors to collect
    pub fn set_max_errors(&mut self, max_errors: usize) {
        self.max_errors = max_errors;
    }

    /// Set whether to continue on fatal errors
    pub fn set_continue_on_fatal(&mut self, continue_on_fatal: bool) {
        self.continue_on_fatal = continue_on_fatal;
    }

    /// Sort diagnostics by span location
    pub fn sort_by_location(&mut self) {
        self.diagnostics.sort_by_key(|d| d.span().offset());
    }

    /// Create miette reports for all diagnostics with syntax highlighting support
    pub fn create_reports(&self) -> Vec<miette::Report> {
        self.create_reports_with_filename("input")
    }

    /// Create miette reports with a specific filename for better syntax highlighting
    pub fn create_reports_with_filename(&self, filename: &str) -> Vec<miette::Report> {
        use miette::NamedSource;

        // Create a named source with .outrun extension for syntax highlighting
        let source_name = if filename.ends_with(".outrun") {
            filename.to_string()
        } else {
            format!("{filename}.outrun")
        };

        let named_source = NamedSource::new(source_name, self.source.clone());

        self.diagnostics
            .iter()
            .map(|diagnostic| {
                miette::Report::new(diagnostic.clone()).with_source_code(named_source.clone())
            })
            .collect()
    }

    /// Print all diagnostics to stderr
    pub fn print_diagnostics(&self) {
        for report in self.create_reports() {
            eprintln!("{report:?}");
        }
    }

    /// Get a summary of diagnostic counts
    pub fn summary(&self) -> DiagnosticSummary {
        DiagnosticSummary {
            total: self.total_count(),
            errors: self.error_count(),
            warnings: self.warning_count(),
            info: self.info_count(),
        }
    }
}

/// Summary of diagnostic counts
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticSummary {
    pub total: usize,
    pub errors: usize,
    pub warnings: usize,
    pub info: usize,
}

impl fmt::Display for DiagnosticSummary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.total == 0 {
            write!(f, "No diagnostics")
        } else {
            write!(
                f,
                "{} total ({} errors, {} warnings, {} info)",
                self.total, self.errors, self.warnings, self.info
            )
        }
    }
}

/// Result type for operations that may produce diagnostics
pub type DiagnosticResult<T> = Result<T, Vec<DiagnosticError>>;
