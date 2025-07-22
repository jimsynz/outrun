//! Tests for error recovery and production polish features

use crate::checker::{
    CompilationSummary, ErrorContext, ErrorRecoveryInfo, MemoryUsage, RecoveryStrategy,
    TypedDebugInfo, TypedExpressionKind, TypedProgram,
};
use crate::error::{SpanExt, TypeError};
use crate::typed_ast_builder::TypedASTBuilder;
use crate::unification::UnificationContext;
use outrun_parser::{parse_program, Span};

#[test]
fn test_error_recovery_info_creation() {
    let recovery_info = ErrorRecoveryInfo {
        error: TypeError::InternalError {
            span: Span::new(0, 10).to_source_span(),
            message: "Test error".to_string(),
        },
        error_context: ErrorContext::Expression {
            expression_type: "FunctionCall".to_string(),
        },
        recovery_span: Span::new(0, 10),
        recovery_strategy: RecoveryStrategy::FallbackType {
            fallback_type: {
                let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
                let type_id = env.intern_type_name("TestType");
                crate::unification::StructuredType::Simple(type_id)
            },
        },
        recovery_successful: true,
    };

    assert!(recovery_info.recovery_successful);
    assert!(matches!(
        recovery_info.error_context,
        ErrorContext::Expression { .. }
    ));
    assert!(matches!(
        recovery_info.recovery_strategy,
        RecoveryStrategy::FallbackType { .. }
    ));
}

#[test]
fn test_error_context_variants() {
    let contexts = vec![
        ErrorContext::FunctionParameter {
            function_name: "test_function".to_string(),
            parameter_name: "param".to_string(),
        },
        ErrorContext::FunctionBody {
            function_name: "test_function".to_string(),
        },
        ErrorContext::LetBinding {
            variable_name: "x".to_string(),
        },
        ErrorContext::Expression {
            expression_type: "FunctionCall".to_string(),
        },
        ErrorContext::StructField {
            struct_name: "User".to_string(),
            field_name: "name".to_string(),
        },
        ErrorContext::ProtocolImplementation {
            protocol_name: "Display".to_string(),
            type_name: "String".to_string(),
        },
        ErrorContext::PatternMatching {
            pattern_type: "StructPattern".to_string(),
        },
        ErrorContext::General {
            description: "Generic error context".to_string(),
        },
    ];

    for context in contexts {
        // All contexts should be constructible and have proper variants
        match context {
            ErrorContext::FunctionParameter { .. } => (),
            ErrorContext::FunctionBody { .. } => (),
            ErrorContext::LetBinding { .. } => (),
            ErrorContext::Expression { .. } => (),
            ErrorContext::StructField { .. } => (),
            ErrorContext::ProtocolImplementation { .. } => (),
            ErrorContext::PatternMatching { .. } => (),
            ErrorContext::General { .. } => (),
        }
    }
}

#[test]
fn test_recovery_strategy_variants() {
    let strategies = vec![
        RecoveryStrategy::Skip,
        RecoveryStrategy::FallbackType {
            fallback_type: {
                let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
                let type_id = env.intern_type_name("FallbackType");
                crate::unification::StructuredType::Simple(type_id)
            },
        },
        RecoveryStrategy::PlaceholderExpression {
            placeholder_type: {
                let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
                let type_id = env.intern_type_name("PlaceholderType");
                crate::unification::StructuredType::Simple(type_id)
            },
        },
        RecoveryStrategy::PartialChecking,
        RecoveryStrategy::NoRecovery,
    ];

    for strategy in strategies {
        // All strategies should be constructible and have proper variants
        match strategy {
            RecoveryStrategy::Skip => (),
            RecoveryStrategy::FallbackType { .. } => (),
            RecoveryStrategy::PlaceholderExpression { .. } => (),
            RecoveryStrategy::PartialChecking => (),
            RecoveryStrategy::NoRecovery => (),
        }
    }
}

#[test]
fn test_compilation_summary_creation() {
    let mut phase_timings = std::collections::HashMap::new();
    phase_timings.insert("parsing".to_string(), 100);
    phase_timings.insert("type_checking".to_string(), 250);
    phase_timings.insert("ast_building".to_string(), 75);

    let summary = CompilationSummary {
        total_items: 10,
        successful_items: 8,
        error_items: 2,
        recovered_items: 1,
        compilation_time_ms: 425,
        memory_usage: MemoryUsage {
            peak_memory_bytes: 1024 * 1024,             // 1MB
            typed_ast_memory_bytes: 512 * 1024,         // 512KB
            type_context_memory_bytes: 256 * 1024,      // 256KB
            function_registry_memory_bytes: 128 * 1024, // 128KB
        },
        phase_timings,
    };

    assert_eq!(summary.total_items, 10);
    assert_eq!(summary.successful_items, 8);
    assert_eq!(summary.error_items, 2);
    assert_eq!(summary.recovered_items, 1);
    assert_eq!(summary.compilation_time_ms, 425);
    assert_eq!(summary.memory_usage.peak_memory_bytes, 1024 * 1024);
    assert_eq!(summary.phase_timings.len(), 3);
    assert_eq!(summary.phase_timings["parsing"], 100);
}

#[test]
fn test_memory_usage_calculation() {
    let memory_usage = MemoryUsage {
        peak_memory_bytes: 2048,
        typed_ast_memory_bytes: 1024,
        type_context_memory_bytes: 512,
        function_registry_memory_bytes: 256,
    };

    // Check that component memory adds up to something reasonable
    let component_total = memory_usage.typed_ast_memory_bytes
        + memory_usage.type_context_memory_bytes
        + memory_usage.function_registry_memory_bytes;

    assert!(component_total <= memory_usage.peak_memory_bytes);
    assert_eq!(component_total, 1792);
    assert_eq!(memory_usage.peak_memory_bytes, 2048);
}

#[test]
fn test_typed_program_with_error_recovery() {
    let context = UnificationContext::default();
    let typed_program = TypedProgram {
        items: Vec::new(),
        type_context: context,
        compilation_order: vec!["test.outrun".to_string()],
        compilation_summary: "Test program with error recovery".to_string(),
        debug_info: TypedDebugInfo::default(),
        error_recovery_info: vec![ErrorRecoveryInfo {
            error: TypeError::InternalError {
                span: Span::new(0, 5).to_source_span(),
                message: "Test recovery".to_string(),
            },
            error_context: ErrorContext::General {
                description: "Test context".to_string(),
            },
            recovery_span: Span::new(0, 5),
            recovery_strategy: RecoveryStrategy::Skip,
            recovery_successful: true,
        }],
        detailed_summary: Some(CompilationSummary {
            total_items: 1,
            successful_items: 0,
            error_items: 1,
            recovered_items: 1,
            compilation_time_ms: 50,
            memory_usage: MemoryUsage {
                peak_memory_bytes: 1024,
                typed_ast_memory_bytes: 512,
                type_context_memory_bytes: 256,
                function_registry_memory_bytes: 128,
            },
            phase_timings: std::collections::HashMap::new(),
        }),
    };

    // Verify error recovery info is properly stored
    assert_eq!(typed_program.error_recovery_info.len(), 1);
    assert!(typed_program.error_recovery_info[0].recovery_successful);

    // Verify detailed summary is present
    assert!(typed_program.detailed_summary.is_some());
    let summary = typed_program.detailed_summary.unwrap();
    assert_eq!(summary.total_items, 1);
    assert_eq!(summary.recovered_items, 1);
}

#[test]
fn test_typed_ast_builder_error_recovery_init() {
    let context = UnificationContext::default();
    let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
    let builder = TypedASTBuilder::new(context, std::collections::HashMap::new(), Some(env));

    // Verify builder initializes with empty error recovery info
    // Note: We can't directly access private fields, but we can test through public interfaces

    // This should not panic and should create a valid builder
    assert_eq!(builder.errors.len(), 0);
}

#[test]
fn test_type_error_expression_creation() {
    // Test that TypeError expressions can be created
    let error_expr_kind = TypedExpressionKind::TypeError {
        error: TypeError::InternalError {
            span: Span::new(0, 10).to_source_span(),
            message: "Test error for recovery".to_string(),
        },
        fallback_type: Some({
            let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
            let type_id = env.intern_type_name("TestType");
            crate::unification::StructuredType::Simple(type_id)
        }),
        recovery_expression: None,
    };

    // Verify the error expression has the correct structure
    if let TypedExpressionKind::TypeError {
        error,
        fallback_type,
        recovery_expression,
    } = error_expr_kind
    {
        assert!(matches!(error, TypeError::InternalError { .. }));
        assert!(fallback_type.is_some());
        assert!(recovery_expression.is_none());
    } else {
        panic!("Expected TypeError expression kind");
    }
}

#[test]
fn test_error_recovery_integration_with_simple_program() {
    // Test with a simple valid program to ensure normal operation isn't affected
    let source = r#"
    def add(a: Integer, b: Integer): Integer {
        a + b
    }
    "#;

    let program = parse_program(source).expect("Failed to parse test program");

    let context = UnificationContext::default();
    let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
    let mut builder = TypedASTBuilder::new(context, std::collections::HashMap::new(), Some(env));

    // This should succeed without any error recovery needed
    let result = builder.build_typed_program(&program, "test.outrun");

    // For a well-formed program, we shouldn't need error recovery
    assert!(result.is_ok());
    let typed_program = result.unwrap();

    // Should have no error recovery info for a valid program
    assert_eq!(typed_program.error_recovery_info.len(), 0);

    // Should have detailed summary
    assert!(typed_program.detailed_summary.is_some());
    let summary = typed_program.detailed_summary.unwrap();
    assert_eq!(summary.error_items, 0);
    assert_eq!(summary.recovered_items, 0);
}

#[test]
fn test_structured_type_error_variant() {
    use crate::unification::StructuredType;

    // Create a TypeError StructuredType
    let error_type = StructuredType::type_error(
        TypeError::InternalError {
            span: outrun_parser::Span::new(0, 10).to_source_span(),
            message: "Failed to resolve type".to_string(),
        },
        Some({
            let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
            let type_id = env.intern_type_name("TestType");
            StructuredType::Simple(type_id)
        }),
        outrun_parser::Span::new(0, 10),
    );

    // Verify the error type has the correct structure
    if let StructuredType::TypeError {
        error,
        fallback_type,
        error_span,
    } = error_type
    {
        assert!(matches!(error, TypeError::InternalError { .. }));
        assert!(fallback_type.is_some());
        assert_eq!(error_span, outrun_parser::Span::new(0, 10));
    } else {
        panic!("Expected TypeError structured type");
    }
}

#[test]
fn test_structured_type_error_unification() {
    use crate::unification::{unify_structured_types, StructuredType, UnificationContext};

    let context = UnificationContext::default();

    // Create an error type with a fallback
    let error_type = StructuredType::type_error(
        TypeError::InternalError {
            span: outrun_parser::Span::new(0, 5).to_source_span(),
            message: "Type error".to_string(),
        },
        Some({
            let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
            let type_id = env.intern_type_name("TestType");
            StructuredType::Simple(type_id)
        }),
        outrun_parser::Span::new(0, 5),
    );

    // Create a normal type
    let normal_type = {
        let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
        let type_id = env.intern_type_name("TestType");
        StructuredType::Simple(type_id)
    };

    // Error type with fallback should unify with the fallback type
    let compiler_env = crate::compilation::compiler_environment::CompilerEnvironment::new();
    let result = unify_structured_types(&error_type, &normal_type, &context, &compiler_env);
    assert!(result.is_ok());
    // Note: The actual unification result depends on whether the fallback types match
    // This is testing that the unification doesn't panic and handles the error type
}

#[test]
fn test_structured_type_error_string_representation() {
    use crate::unification::StructuredType;

    let env = crate::compilation::compiler_environment::CompilerEnvironment::new();
    let string_type_id = env.intern_type_name("String");

    // Create an error type with a fallback
    let error_type = StructuredType::type_error(
        TypeError::InternalError {
            span: outrun_parser::Span::new(0, 5).to_source_span(),
            message: "Type error".to_string(),
        },
        Some(StructuredType::Simple(string_type_id)),
        outrun_parser::Span::new(0, 5),
    );

    // Error type should display with error marker and fallback type
    let representation = error_type.to_string_representation();
    assert_eq!(representation, "<ERROR: String>");

    // Error type without fallback
    let error_type_no_fallback = StructuredType::type_error(
        TypeError::InternalError {
            span: outrun_parser::Span::new(0, 5).to_source_span(),
            message: "Type error".to_string(),
        },
        None,
        outrun_parser::Span::new(0, 5),
    );

    let representation_no_fallback = error_type_no_fallback.to_string_representation();
    assert_eq!(representation_no_fallback, "<ERROR>");
}
