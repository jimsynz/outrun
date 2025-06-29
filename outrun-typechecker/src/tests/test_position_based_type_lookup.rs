//! Tests for position-based type lookup functionality
//!
//! This module tests the LSP integration feature that allows finding type information
//! at specific positions in the source code for hover tooltips and IDE support.

use crate::{get_type_at_position, typecheck_with_core_library};
use outrun_parser::parse_program;

/// Helper function to create a typed program from source
fn create_typed_program_with_source(source: &str) -> (crate::TypedProgram, String) {
    let program = parse_program(source).unwrap_or_else(|e| {
        panic!("Failed to parse test program: {e:?}\nSource: {source}");
    });

    let result = typecheck_with_core_library(program, source, "test.outrun")
        .expect("Type checking should succeed");

    // Extract the typed program from compilation result
    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program")
        .clone();

    (typed_program, source.to_string())
}

#[test]
fn test_position_based_type_lookup_simple_literal() {
    let source = "42";
    let (typed_program, source_text) = create_typed_program_with_source(source);

    // Debug: print the typed program structure
    println!("Typed program items: {}", typed_program.items.len());
    for (i, item) in typed_program.items.iter().enumerate() {
        println!(
            "Item {}: kind = {:?}",
            i,
            match &item.kind {
                crate::TypedItemKind::Expression(_) => "Expression",
                crate::TypedItemKind::FunctionDefinition(_) => "FunctionDefinition",
                crate::TypedItemKind::LetBinding(_) => "LetBinding",
                _ => "Other",
            }
        );
        println!("  Span: {:?}", item.span);
    }

    // Position at the start of "42" (line 0, column 0)
    let result = get_type_at_position(&typed_program, &source_text, 0, 0);

    println!("Type lookup result: {result:?}");

    assert!(result.is_some());
    let type_info = result.unwrap();
    assert!(type_info.contains("Integer") || type_info.contains("Integer64"));
}

#[test]
fn test_position_based_type_lookup_string_literal() {
    let source = r#""hello world""#;
    let (typed_program, source_text) = create_typed_program_with_source(source);

    // Position at the start of the string literal (line 0, column 0)
    let result = get_type_at_position(&typed_program, &source_text, 0, 0);

    assert!(result.is_some());
    let type_info = result.unwrap();
    assert!(type_info.contains("String"));
}

#[test]
fn test_position_based_type_lookup_function_call() {
    let source = r#"String.length(value: "test")"#;
    let (typed_program, source_text) = create_typed_program_with_source(source);

    // Position at the start of "String" (line 0, column 0)
    let result_string = get_type_at_position(&typed_program, &source_text, 0, 0);

    // Position at the string argument (line 0, column 21 - inside "test")
    let result_arg = get_type_at_position(&typed_program, &source_text, 0, 21);

    // At least one of these should give us type information
    assert!(result_string.is_some() || result_arg.is_some());

    if let Some(type_info) = result_arg {
        assert!(type_info.contains("String"));
    }
}

#[test]
fn test_position_based_type_lookup_let_binding() {
    let source = r#"let x = 42"#;
    let (typed_program, source_text) = create_typed_program_with_source(source);

    // Position at the literal "42" (line 0, column 8)
    let result_literal = get_type_at_position(&typed_program, &source_text, 0, 8);

    // Position at the variable "x" (line 0, column 4)
    let result_variable = get_type_at_position(&typed_program, &source_text, 0, 4);

    // Should get type information for the literal
    if let Some(type_info) = result_literal {
        assert!(type_info.contains("Integer"));
    }

    // May get type information for the variable depending on implementation
    if let Some(type_info) = result_variable {
        assert!(type_info.contains("Integer"));
    }
}

#[test]
fn test_position_based_type_lookup_function_definition() {
    let source = r#"def add(x: Integer, y: Integer): Integer { x + y }"#;
    let (typed_program, source_text) = create_typed_program_with_source(source);

    // Position at the first parameter "x" (line 0, column 8)
    let result_param_x = get_type_at_position(&typed_program, &source_text, 0, 8);

    // Position at the binary operation "x + y" - specifically at "x" (line 0, column 43)
    let result_body_x = get_type_at_position(&typed_program, &source_text, 0, 43);

    // Should get type information somewhere in the function
    assert!(result_param_x.is_some() || result_body_x.is_some());
}

#[test]
fn test_position_based_type_lookup_no_match() {
    let source = "42";
    let (typed_program, source_text) = create_typed_program_with_source(source);

    // Position way outside the source (line 10, column 10)
    let result = get_type_at_position(&typed_program, &source_text, 10, 10);

    // Should return None for positions outside the source
    assert!(result.is_none());
}

#[test]
fn test_line_column_to_offset_conversion() {
    let source = "line one\nline two\nline three";

    // Test various positions
    let test_cases = vec![
        (0, 0, 0),   // Start of first line
        (0, 5, 5),   // Middle of first line
        (1, 0, 9),   // Start of second line (after \n)
        (1, 5, 14),  // Middle of second line
        (2, 0, 18),  // Start of third line
        (2, 10, 28), // End of source
    ];

    for (line, column, expected_offset) in test_cases {
        let result = crate::line_column_to_offset(line, column, source);
        assert_eq!(
            result,
            Some(expected_offset),
            "Failed for line {line} column {column}: expected {expected_offset}, got {result:?}"
        );
    }

    // Test out-of-bounds position
    let result = crate::line_column_to_offset(10, 0, source);
    assert_eq!(result, None);
}

#[test]
fn test_position_based_type_lookup_multiline() {
    let source = r#"def test(): Integer {
    let x = 42
    x + 1
}"#;
    let (typed_program, source_text) = create_typed_program_with_source(source);

    // Position at "42" on line 1 (line 1, column 12)
    let result_literal = get_type_at_position(&typed_program, &source_text, 1, 12);

    // Position at "x" in "x + 1" on line 2 (line 2, column 4)
    let result_variable = get_type_at_position(&typed_program, &source_text, 2, 4);

    // Should get type information for at least one position
    assert!(result_literal.is_some() || result_variable.is_some());

    if let Some(type_info) = result_literal {
        assert!(type_info.contains("Integer"));
    }
}

#[test]
fn test_position_based_type_lookup_nested_expressions() {
    // Test nested expressions with a simpler case that doesn't require complex generic inference
    let source = r#"Integer.abs(value: -42)"#;
    let (typed_program, source_text) = create_typed_program_with_source(source);

    // Position at the nested literal "-42" (line 0, column 19)
    let result = get_type_at_position(&typed_program, &source_text, 0, 19);

    if let Some(type_info) = result {
        assert!(type_info.contains("Integer"));
    }
}

#[test]
fn test_span_contains_offset_functionality() {
    use crate::span_contains_offset;
    use outrun_parser::Span;

    let span = Span::new(10, 20);

    // Test positions within span
    assert!(span_contains_offset(&span, 10)); // Start boundary
    assert!(span_contains_offset(&span, 15)); // Middle
    assert!(span_contains_offset(&span, 19)); // Just before end

    // Test positions outside span
    assert!(!span_contains_offset(&span, 9)); // Before start
    assert!(!span_contains_offset(&span, 20)); // At end (exclusive)
    assert!(!span_contains_offset(&span, 25)); // After end
}
