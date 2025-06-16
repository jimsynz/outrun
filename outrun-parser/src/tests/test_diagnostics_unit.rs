//! Unit tests for the diagnostics system
//!
//! Tests core functionality of DiagnosticCollector, Severity, and DiagnosticError

use crate::ast::Span;
use crate::diagnostics::*;

#[test]
fn test_severity_ordering() {
    assert!(Severity::Info < Severity::Warning);
    assert!(Severity::Warning < Severity::Error);
    assert!(Severity::Error < Severity::Fatal);
}

#[test]
fn test_diagnostic_collector_basic() {
    let mut collector = DiagnosticCollector::new("test source".to_string());

    assert!(!collector.has_diagnostics());
    assert_eq!(collector.total_count(), 0);

    let span = Span::new(0, 4);
    collector.add_diagnostic(DiagnosticError::syntax_error(
        "Test error".to_string(),
        span,
    ));

    assert!(collector.has_diagnostics());
    assert!(collector.has_errors());
    assert_eq!(collector.total_count(), 1);
    assert_eq!(collector.error_count(), 1);
}

#[test]
fn test_diagnostic_collector_max_errors() {
    let mut collector = DiagnosticCollector::with_settings("test".to_string(), 2, false);
    let span = Span::new(0, 1);

    // Add 3 errors, should only keep first 2
    for i in 0..3 {
        collector.add_diagnostic(DiagnosticError::syntax_error(format!("Error {}", i), span));
    }

    assert_eq!(collector.total_count(), 2);
}

#[test]
fn test_diagnostic_collector_fatal_stops() {
    let mut collector = DiagnosticCollector::with_settings("test".to_string(), 10, false);
    let span = Span::new(0, 1);

    // Test with batch operations - fatal errors should stop processing in batch mode
    let diagnostics = vec![
        DiagnosticError::syntax_error("Error 1".to_string(), span),
        DiagnosticError::fatal_error("Fatal error".to_string(), span),
        DiagnosticError::syntax_error("Error 2".to_string(), span), // Should not be added
    ];

    collector.add_diagnostics(diagnostics);

    // Should stop after fatal error when continue_on_fatal is false
    assert_eq!(collector.total_count(), 2);
    assert!(collector.has_fatal_errors());
}
