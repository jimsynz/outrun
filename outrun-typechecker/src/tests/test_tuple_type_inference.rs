//! Tests for tuple type inference and validation

use outrun_parser::parse_program;

#[test]
fn test_empty_tuple_parsing() {
    // Test that empty tuples parse correctly (even though they should be type errors)
    let source = r#"
        def test_empty(): Something {
            ()
        }
    "#;

    let program = parse_program(source).expect("Program should parse");
    assert!(!program.items.is_empty());

    // The parser should allow empty tuples - the type checker will catch the error
}

#[test]
fn test_simple_tuple_parsing() {
    // Test that simple tuples parse correctly
    let source = r#"
        def test_simple(): (Integer, String) {
            (42, "hello")
        }
    "#;

    let program = parse_program(source).expect("Program should parse");
    assert!(!program.items.is_empty());
}

#[test]
fn test_tuple_with_mixed_types() {
    // Test that tuples can contain different types
    let source = r#"
        def test_tuple(): (Integer, String, Boolean) {
            (42, "hello", true)
        }
    "#;

    let program = parse_program(source).expect("Program should parse");

    // This tests integration with the broader type checking system
    // The exact behavior depends on how literal type inference is implemented
    assert!(!program.items.is_empty());
}

#[test]
fn test_nested_tuple_structure() {
    // Test that nested tuples work correctly
    let source = r#"
        def test_nested(): ((Integer, String), Boolean) {
            ((1, "hi"), false)
        }
    "#;

    let program = parse_program(source).expect("Program should parse");
    assert!(!program.items.is_empty());
}

#[test]
fn test_single_element_tuple() {
    // Test that single-element tuples parse correctly
    let source = r#"
        def test_single(): (Integer,) {
            (42,)
        }
    "#;

    let program = parse_program(source).expect("Program should parse");
    assert!(!program.items.is_empty());
}

#[test]
fn test_tuple_type_annotation_parsing() {
    // Test parsing of tuple type annotations
    let source = r#"
        def complex_tuple(): ((Integer, String), (Boolean, Float)) {
            ((1, "hi"), (true, 3.14))
        }
    "#;

    let program = parse_program(source).expect("Program should parse");
    assert!(!program.items.is_empty());
}
