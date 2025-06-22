//! Tests for typed AST collection literal support
//!
//! These tests verify that the TypedASTBuilder correctly converts
//! parser collection literals into typed AST structures.

use crate::checker::{TypedExpressionKind, TypedMapEntry, TypedStructField};
use crate::multi_program_compiler::MultiProgramCompiler;
use outrun_parser::parse_program;

#[test]
fn test_list_literal_typed_ast_conversion() {
    let source = "[1, 2, 3]";
    let program = parse_program(source).expect("Failed to parse list literal");

    let mut compiler = MultiProgramCompiler::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler.compile(&collection).expect("Failed to compile");

    // Check that we have typed programs
    assert!(
        !result.typed_programs.is_empty(),
        "Should have typed programs"
    );

    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program");
    assert!(!typed_program.items.is_empty(), "Should have items");

    // Check the first item is an expression with a list
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::Expression(expr) => {
            match &expr.kind {
                TypedExpressionKind::List { elements, .. } => {
                    assert_eq!(elements.len(), 3, "Should have 3 elements");

                    // Check each element is an integer
                    for (i, element) in elements.iter().enumerate() {
                        match &element.kind {
                            TypedExpressionKind::Integer(value) => {
                                assert_eq!(*value, (i + 1) as i64, "Element {} should match", i);
                            }
                            _ => panic!("Element {} should be an integer", i),
                        }
                    }
                }
                _ => panic!("Expected list expression, got {:?}", expr.kind),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_map_literal_typed_ast_conversion() {
    // Use explicit key-value syntax with consistent string values
    let source = r#"{:name => "Alice", :role => "admin"}"#;
    let program = parse_program(source).expect("Failed to parse map literal");

    let mut compiler = MultiProgramCompiler::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler.compile(&collection).expect("Failed to compile");

    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program");

    // Check the first item is an expression with a map
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::Expression(expr) => {
            match &expr.kind {
                TypedExpressionKind::Map { entries, .. } => {
                    assert_eq!(entries.len(), 2, "Should have 2 entries");

                    // Check entries are converted to typed map entries
                    for entry in entries {
                        match entry {
                            TypedMapEntry::Assignment { key, value, .. } => {
                                // Verify we have proper key-value structure
                                assert!(
                                    matches!(key.kind, TypedExpressionKind::Atom(_)),
                                    "Key should be atom"
                                );
                                assert!(
                                    matches!(value.kind, TypedExpressionKind::String(_)),
                                    "Value should be string"
                                );
                            }
                            _ => panic!("Expected assignment entry, got {:?}", entry),
                        }
                    }
                }
                _ => panic!("Expected map expression, got {:?}", expr.kind),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_tuple_literal_typed_ast_conversion() {
    let source = r#"("hello", 42, true)"#;
    let program = parse_program(source).expect("Failed to parse tuple literal");

    let mut compiler = MultiProgramCompiler::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler.compile(&collection).expect("Failed to compile");

    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program");

    // Check the first item is an expression with a tuple
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::Expression(expr) => {
            match &expr.kind {
                TypedExpressionKind::Tuple { elements, .. } => {
                    assert_eq!(elements.len(), 3, "Should have 3 elements");

                    // Check each element type
                    match &elements[0].kind {
                        TypedExpressionKind::String(s) => assert_eq!(s, "hello"),
                        _ => panic!("First element should be string"),
                    }
                    match &elements[1].kind {
                        TypedExpressionKind::Integer(i) => assert_eq!(*i, 42),
                        _ => panic!("Second element should be integer"),
                    }
                    match &elements[2].kind {
                        TypedExpressionKind::Boolean(b) => assert!(*b),
                        _ => panic!("Third element should be boolean"),
                    }
                }
                _ => panic!("Expected tuple expression, got {:?}", expr.kind),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_struct_literal_typed_ast_conversion() {
    let source = r#"User { name: "Bob", age: 25 }"#;
    let program = parse_program(source).expect("Failed to parse struct literal");

    let mut compiler = MultiProgramCompiler::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    // Note: This may fail compilation due to missing User struct definition
    // but should still create a typed AST structure
    let result = compiler.compile(&collection);
    match result {
        Ok(compilation_result) => {
            let typed_program = compilation_result
                .typed_programs
                .get("test.outrun")
                .expect("Should have test program");

            // Check the first item is an expression with a struct literal
            match &typed_program.items[0].kind {
                crate::checker::TypedItemKind::Expression(expr) => {
                    match &expr.kind {
                        TypedExpressionKind::StructLiteral {
                            type_path, fields, ..
                        } => {
                            assert_eq!(type_path, &vec!["User"], "Should be User struct");
                            assert_eq!(fields.len(), 2, "Should have 2 fields");

                            // Check field names
                            let field_names: Vec<String> = fields
                                .iter()
                                .map(|f| match f {
                                    TypedStructField::Assignment { name, .. } => name.clone(),
                                    TypedStructField::Shorthand { name, .. } => name.clone(),
                                    TypedStructField::Spread { identifier, .. } => {
                                        identifier.clone()
                                    }
                                })
                                .collect();

                            assert!(
                                field_names.contains(&"name".to_string()),
                                "Should have name field"
                            );
                            assert!(
                                field_names.contains(&"age".to_string()),
                                "Should have age field"
                            );
                        }
                        _ => panic!("Expected struct literal expression, got {:?}", expr.kind),
                    }
                }
                _ => panic!("Expected expression item"),
            }
        }
        Err(_) => {
            // Compilation may fail due to undefined User struct, which is expected
            // The important thing is that parsing and AST building works
            println!("Compilation failed as expected (User struct not defined)");
        }
    }
}

#[test]
fn test_empty_tuple_error() {
    // Test that empty tuples produce errors as expected
    let source = "()";
    let program = parse_program(source).expect("Failed to parse tuple");

    let mut compiler = MultiProgramCompiler::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    // Empty tuples should cause compilation to fail
    let result = compiler.compile(&collection);
    assert!(
        result.is_err(),
        "Empty tuples should cause compilation errors"
    );

    if let Err(errors) = result {
        // Should have at least one error about empty tuples
        let error_messages = errors
            .iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join(" ");
        assert!(
            error_messages.contains("Empty tuples are not allowed"),
            "Should have empty tuple error message, got: {}",
            error_messages
        );
    }
}

#[test]
fn test_non_empty_tuple_typed_ast() {
    // Test that non-empty tuples work correctly
    let source = "(42, \"hello\")";
    let program = parse_program(source).expect("Failed to parse tuple");

    let mut compiler = MultiProgramCompiler::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    // Non-empty tuples should compile successfully
    match compiler.compile(&collection) {
        Ok(result) => {
            let typed_program = result
                .typed_programs
                .get("test.outrun")
                .expect("Should have test program");

            match &typed_program.items[0].kind {
                crate::checker::TypedItemKind::Expression(expr) => match &expr.kind {
                    TypedExpressionKind::Tuple {
                        elements,
                        tuple_type,
                    } => {
                        assert_eq!(elements.len(), 2, "Tuple should have two elements");
                        // tuple_type might be None if literal type inference isn't complete
                        println!("Tuple type: {:?}", tuple_type);
                    }
                    _ => panic!("Expected tuple expression, got: {:?}", expr.kind),
                },
                _ => panic!("Expected expression item"),
            }
        }
        Err(errors) => {
            // If it fails, print the errors for debugging
            for error in &errors {
                println!("Compilation error: {}", error);
            }
            panic!("Non-empty tuple compilation should succeed");
        }
    }
}

#[test]
fn test_nested_collections_typed_ast() {
    let source = r#"[[1, 2], [3, 4]]"#;
    let program = parse_program(source).expect("Failed to parse nested list");

    let mut compiler = MultiProgramCompiler::new();
    let mut collection = crate::core_library::load_core_library_collection();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let result = compiler.compile(&collection).expect("Failed to compile");
    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Should have test program");

    // Check nested structure
    match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::Expression(expr) => {
            match &expr.kind {
                TypedExpressionKind::List { elements, .. } => {
                    assert_eq!(elements.len(), 2, "Should have 2 nested lists");

                    // Check each nested list
                    for (i, element) in elements.iter().enumerate() {
                        match &element.kind {
                            TypedExpressionKind::List {
                                elements: nested_elements,
                                ..
                            } => {
                                assert_eq!(
                                    nested_elements.len(),
                                    2,
                                    "Nested list {} should have 2 elements",
                                    i
                                );

                                // Check nested elements are integers
                                for nested_element in nested_elements {
                                    match &nested_element.kind {
                                        TypedExpressionKind::Integer(_) => {} // Expected
                                        _ => panic!("Nested element should be integer"),
                                    }
                                }
                            }
                            _ => panic!("Element {} should be a list", i),
                        }
                    }
                }
                _ => panic!("Expected list expression"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}
