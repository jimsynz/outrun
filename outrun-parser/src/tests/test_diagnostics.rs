use crate::{ast::Span, DiagnosticCollector, DiagnosticError, Severity};

#[test]
fn test_severity_levels() {
    assert!(Severity::Info < Severity::Warning);
    assert!(Severity::Warning < Severity::Error);
    assert!(Severity::Error < Severity::Fatal);

    assert_eq!(Severity::Info.to_string(), "info");
    assert_eq!(Severity::Warning.to_string(), "warning");
    assert_eq!(Severity::Error.to_string(), "error");
    assert_eq!(Severity::Fatal.to_string(), "fatal");
}

#[test]
fn test_diagnostic_error_creation() {
    let span = Span::new(10, 20);

    let error = DiagnosticError::syntax_error("Invalid syntax".to_string(), span);
    assert_eq!(error.severity(), Severity::Error);
    assert_eq!(error.span().offset(), 10);
    assert_eq!(error.span().len(), 10);

    let error = DiagnosticError::type_error("Type mismatch".to_string(), span);
    assert_eq!(error.severity(), Severity::Error);

    let error = DiagnosticError::unused_variable("unused_var".to_string(), span);
    assert_eq!(error.severity(), Severity::Warning);

    let error = DiagnosticError::fatal_error("Fatal parsing error".to_string(), span);
    assert_eq!(error.severity(), Severity::Fatal);
}

#[test]
fn test_diagnostic_collector_empty() {
    let collector = DiagnosticCollector::new("test source".to_string());

    assert!(!collector.has_diagnostics());
    assert!(!collector.has_errors());
    assert!(!collector.has_warnings());
    assert!(!collector.has_fatal_errors());
    assert_eq!(collector.total_count(), 0);
    assert_eq!(collector.error_count(), 0);
    assert_eq!(collector.warning_count(), 0);
    assert_eq!(collector.info_count(), 0);
    assert_eq!(collector.source(), "test source");
}

#[test]
fn test_diagnostic_collector_add_single() {
    let mut collector = DiagnosticCollector::new("test source".to_string());
    let span = Span::new(0, 5);

    collector.add_diagnostic(DiagnosticError::syntax_error(
        "Test error".to_string(),
        span,
    ));

    assert!(collector.has_diagnostics());
    assert!(collector.has_errors());
    assert!(!collector.has_warnings());
    assert!(!collector.has_fatal_errors());
    assert_eq!(collector.total_count(), 1);
    assert_eq!(collector.error_count(), 1);
    assert_eq!(collector.warning_count(), 0);
    assert_eq!(collector.info_count(), 0);
}

#[test]
fn test_diagnostic_collector_multiple_severities() {
    let mut collector = DiagnosticCollector::new("test source".to_string());
    let span = Span::new(0, 5);

    collector.add_diagnostic(DiagnosticError::syntax_error("Error 1".to_string(), span));
    collector.add_diagnostic(DiagnosticError::unused_variable("var1".to_string(), span));
    collector.add_diagnostic(DiagnosticError::performance_warning(
        "Slow operation".to_string(),
        "Use faster method".to_string(),
        span,
    ));
    collector.add_diagnostic(DiagnosticError::type_error("Type error".to_string(), span));

    assert!(collector.has_diagnostics());
    assert!(collector.has_errors());
    assert!(collector.has_warnings());
    assert!(!collector.has_fatal_errors());
    assert_eq!(collector.total_count(), 4);
    assert_eq!(collector.error_count(), 2); // syntax_error + type_error
    assert_eq!(collector.warning_count(), 1); // unused_variable
    assert_eq!(collector.info_count(), 1); // performance_warning
}

#[test]
fn test_diagnostic_collector_filter_by_severity() {
    let mut collector = DiagnosticCollector::new("test source".to_string());
    let span = Span::new(0, 5);

    collector.add_diagnostic(DiagnosticError::syntax_error("Error 1".to_string(), span));
    collector.add_diagnostic(DiagnosticError::unused_variable("var1".to_string(), span));
    collector.add_diagnostic(DiagnosticError::syntax_error("Error 2".to_string(), span));

    let errors = collector.diagnostics_by_severity(Severity::Error);
    let warnings = collector.diagnostics_by_severity(Severity::Warning);

    assert_eq!(errors.len(), 2);
    assert_eq!(warnings.len(), 1);
}

#[test]
fn test_diagnostic_collector_max_errors() {
    let mut collector = DiagnosticCollector::with_settings("test".to_string(), 3, false);
    let span = Span::new(0, 1);

    for i in 0..5 {
        collector.add_diagnostic(DiagnosticError::syntax_error(format!("Error {}", i), span));
    }

    assert_eq!(collector.total_count(), 3);
    assert_eq!(collector.error_count(), 3);
}

#[test]
fn test_diagnostic_collector_fatal_stops_parsing() {
    let mut collector = DiagnosticCollector::with_settings("test".to_string(), 10, false);
    let span = Span::new(0, 1);

    let diagnostics = vec![
        DiagnosticError::syntax_error("Error 1".to_string(), span),
        DiagnosticError::fatal_error("Fatal error".to_string(), span),
        DiagnosticError::syntax_error("Error 2".to_string(), span), // Should not be added
    ];

    collector.add_diagnostics(diagnostics);

    assert_eq!(collector.total_count(), 2);
    assert!(collector.has_fatal_errors());
    assert_eq!(collector.error_count(), 2); // 1 syntax + 1 fatal
}

#[test]
fn test_diagnostic_collector_continue_on_fatal() {
    let mut collector = DiagnosticCollector::with_settings("test".to_string(), 10, true);
    let span = Span::new(0, 1);

    let diagnostics = vec![
        DiagnosticError::syntax_error("Error 1".to_string(), span),
        DiagnosticError::fatal_error("Fatal error".to_string(), span),
        DiagnosticError::syntax_error("Error 2".to_string(), span), // Should be added
    ];

    collector.add_diagnostics(diagnostics);

    assert_eq!(collector.total_count(), 3);
    assert!(collector.has_fatal_errors());
    assert_eq!(collector.error_count(), 3);
}

#[test]
fn test_diagnostic_collector_add_multiple() {
    let mut collector = DiagnosticCollector::new("test source".to_string());
    let span = Span::new(0, 5);

    let diagnostics = vec![
        DiagnosticError::syntax_error("Error 1".to_string(), span),
        DiagnosticError::unused_variable("var1".to_string(), span),
        DiagnosticError::type_error("Type error".to_string(), span),
    ];

    collector.add_diagnostics(diagnostics);

    assert_eq!(collector.total_count(), 3);
    assert_eq!(collector.error_count(), 2);
    assert_eq!(collector.warning_count(), 1);
}

#[test]
fn test_diagnostic_collector_clear() {
    let mut collector = DiagnosticCollector::new("test source".to_string());
    let span = Span::new(0, 5);

    collector.add_diagnostic(DiagnosticError::syntax_error("Error".to_string(), span));
    assert!(collector.has_diagnostics());

    collector.clear();
    assert!(!collector.has_diagnostics());
    assert_eq!(collector.total_count(), 0);
}

#[test]
fn test_diagnostic_collector_sort_by_location() {
    let mut collector = DiagnosticCollector::new("test source".to_string());

    collector.add_diagnostic(DiagnosticError::syntax_error(
        "Error 3".to_string(),
        Span::new(20, 25),
    ));
    collector.add_diagnostic(DiagnosticError::syntax_error(
        "Error 1".to_string(),
        Span::new(0, 5),
    ));
    collector.add_diagnostic(DiagnosticError::syntax_error(
        "Error 2".to_string(),
        Span::new(10, 15),
    ));

    collector.sort_by_location();

    let diagnostics = collector.diagnostics();
    assert_eq!(diagnostics[0].span().offset(), 0);
    assert_eq!(diagnostics[1].span().offset(), 10);
    assert_eq!(diagnostics[2].span().offset(), 20);
}

#[test]
fn test_diagnostic_collector_settings() {
    let mut collector = DiagnosticCollector::new("test".to_string());

    collector.set_max_errors(5);
    collector.set_continue_on_fatal(true);

    let span = Span::new(0, 1);

    for i in 0..6 {
        collector.add_diagnostic(DiagnosticError::syntax_error(format!("Error {}", i), span));
    }

    assert_eq!(collector.total_count(), 5);

    collector.clear();
    collector.add_diagnostic(DiagnosticError::fatal_error("Fatal".to_string(), span));
    collector.add_diagnostic(DiagnosticError::syntax_error(
        "After fatal".to_string(),
        span,
    ));

    assert_eq!(collector.total_count(), 2); // Both should be added
}

#[test]
fn test_diagnostic_summary() {
    let collector = DiagnosticCollector::new("test".to_string());
    let empty_summary = collector.summary();

    assert_eq!(empty_summary.total, 0);
    assert_eq!(empty_summary.errors, 0);
    assert_eq!(empty_summary.warnings, 0);
    assert_eq!(empty_summary.info, 0);
    assert_eq!(empty_summary.to_string(), "No diagnostics");

    let mut collector = DiagnosticCollector::new("test".to_string());
    let span = Span::new(0, 5);

    collector.add_diagnostic(DiagnosticError::syntax_error("Error".to_string(), span));
    collector.add_diagnostic(DiagnosticError::unused_variable("var".to_string(), span));
    collector.add_diagnostic(DiagnosticError::performance_warning(
        "Slow".to_string(),
        "Fast".to_string(),
        span,
    ));

    let summary = collector.summary();
    assert_eq!(summary.total, 3);
    assert_eq!(summary.errors, 1);
    assert_eq!(summary.warnings, 1);
    assert_eq!(summary.info, 1);
    assert_eq!(
        summary.to_string(),
        "3 total (1 errors, 1 warnings, 1 info)"
    );
}

#[test]
fn test_diagnostic_create_reports() {
    let mut collector = DiagnosticCollector::new("let x = 42".to_string());
    let span = Span::new(4, 5);

    collector.add_diagnostic(DiagnosticError::syntax_error(
        "Invalid identifier".to_string(),
        span,
    ));

    let reports = collector.create_reports();
    assert_eq!(reports.len(), 1);

    let report_string = format!("{}", reports[0]);
    assert!(report_string.contains("Syntax error"));
}

#[test]
fn test_all_diagnostic_error_types() {
    let span = Span::new(0, 5);

    let syntax_error = DiagnosticError::syntax_error("Syntax error".to_string(), span);
    assert_eq!(syntax_error.severity(), Severity::Error);

    let type_error = DiagnosticError::type_error("Type error".to_string(), span);
    assert_eq!(type_error.severity(), Severity::Error);

    let unused_var = DiagnosticError::unused_variable("unused".to_string(), span);
    assert_eq!(unused_var.severity(), Severity::Warning);

    let deprecated = DiagnosticError::deprecated_syntax(
        "Old syntax".to_string(),
        "Use new syntax".to_string(),
        span,
    );
    assert_eq!(deprecated.severity(), Severity::Warning);

    let missing_semi = DiagnosticError::missing_semicolon(span);
    assert_eq!(missing_semi.severity(), Severity::Error);

    let unexpected_token =
        DiagnosticError::unexpected_token("found".to_string(), "expected".to_string(), span);
    assert_eq!(unexpected_token.severity(), Severity::Error);

    let invalid_literal = DiagnosticError::invalid_literal(
        "Invalid number".to_string(),
        "Use valid number".to_string(),
        span,
    );
    assert_eq!(invalid_literal.severity(), Severity::Error);

    let performance = DiagnosticError::performance_warning(
        "Slow code".to_string(),
        "Optimise this".to_string(),
        span,
    );
    assert_eq!(performance.severity(), Severity::Info);

    let fatal = DiagnosticError::fatal_error("Fatal error".to_string(), span);
    assert_eq!(fatal.severity(), Severity::Fatal);

    for error in [
        &syntax_error,
        &type_error,
        &unused_var,
        &deprecated,
        &missing_semi,
        &unexpected_token,
        &invalid_literal,
        &performance,
        &fatal,
    ] {
        assert_eq!(error.span().offset(), 0);
        assert_eq!(error.span().len(), 5);
    }
}
