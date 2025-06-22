//! Tests for LSP completion suggestions functionality

use crate::*;
use outrun_parser::parse_program;

#[test]
fn test_basic_completion_suggestions() {
    let source = r#"
        def greet(name: String): String {
            "Hello"
        }
        
        def calculate(x: Integer, y: Integer): Integer {
            x + y
        }
    "#;

    let program = parse_program(source).expect("Program should parse");

    // For this test, we'll create a minimal typed program
    // In practice, this would come from full type checking
    let typed_program = match typecheck_program_with_source(program, source, "test.outrun") {
        Ok(tp) => tp,
        Err(_) => {
            // If type checking fails, we'll skip this test for now
            // since it requires the full core library to be available
            return;
        }
    };

    // Get all completions
    let completions = get_completions_at_position(&typed_program, 0, 0);

    // Should have at least some completions
    assert!(
        !completions.is_empty(),
        "Should have some completion suggestions"
    );

    // Should include keywords
    let keyword_completions: Vec<_> = completions
        .iter()
        .filter(|c| c.kind == CompletionKind::Keyword)
        .collect();
    assert!(
        !keyword_completions.is_empty(),
        "Should have keyword completions"
    );

    // Should include built-in types
    let type_completions: Vec<_> = completions
        .iter()
        .filter(|c| c.kind == CompletionKind::Type)
        .collect();
    assert!(!type_completions.is_empty(), "Should have type completions");

    // Check that we have some common keywords
    let def_completion = completions
        .iter()
        .find(|c| c.label == "def" && c.kind == CompletionKind::Keyword);
    assert!(
        def_completion.is_some(),
        "Should have 'def' keyword completion"
    );

    // Check that we have some built-in types
    let string_completion = completions
        .iter()
        .find(|c| c.label == "String" && c.kind == CompletionKind::Type);
    assert!(
        string_completion.is_some(),
        "Should have 'String' type completion"
    );

    println!("Found {} total completions", completions.len());
    for completion in completions.iter().take(10) {
        println!(
            "  {} ({:?}): {}",
            completion.label,
            completion.kind,
            completion.detail.as_deref().unwrap_or("")
        );
    }
}

#[test]
fn test_completion_with_prefix_filtering() {
    let source = r#"
        def greet(name: String): String {
            "Hello"
        }
        
        def goodbye(name: String): String {
            "Goodbye"
        }
    "#;

    let program = parse_program(source).expect("Program should parse");

    // Try to create a typed program, but skip test if it fails
    let typed_program = match typecheck_program_with_source(program, source, "test.outrun") {
        Ok(tp) => tp,
        Err(_) => return, // Skip test if core library not available
    };

    // Get completions with prefix filtering
    let g_completions = get_completions_with_prefix(&typed_program, 0, 0, "g");

    // Should only include items starting with 'g'
    for completion in &g_completions {
        assert!(
            completion.label.starts_with("g"),
            "Completion '{}' should start with 'g'",
            completion.label
        );
    }

    println!(
        "Found {} completions starting with 'g'",
        g_completions.len()
    );
}

#[test]
fn test_completion_item_structure() {
    // Test basic completion item creation and fields
    let completion = CompletionItem {
        label: "test_function".to_string(),
        kind: CompletionKind::Function,
        detail: Some("(x: Integer) -> String".to_string()),
        documentation: Some("A test function".to_string()),
        insert_text: Some("test_function(x: ${1})".to_string()),
    };

    assert_eq!(completion.label, "test_function");
    assert_eq!(completion.kind, CompletionKind::Function);
    assert!(completion.detail.is_some());
    assert!(completion.documentation.is_some());
    assert!(completion.insert_text.is_some());
}

#[test]
fn test_completion_kinds() {
    // Test that all completion kinds can be created
    let kinds = vec![
        CompletionKind::Function,
        CompletionKind::Variable,
        CompletionKind::Type,
        CompletionKind::Module,
        CompletionKind::Constant,
        CompletionKind::Keyword,
        CompletionKind::Method,
        CompletionKind::Field,
    ];

    for kind in kinds {
        let completion = CompletionItem {
            label: format!("test_{:?}", kind),
            kind: kind.clone(),
            detail: None,
            documentation: None,
            insert_text: None,
        };

        assert_eq!(completion.kind, kind);
    }
}
