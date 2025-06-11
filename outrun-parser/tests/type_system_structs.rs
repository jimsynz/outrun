// Test struct definitions parsing
// Comprehensive tests for struct definition syntax

use outrun_parser::ast::*;
use outrun_parser::parser::OutrunParser;

#[test]
fn test_basic_struct_definition() {
    let input = r#"struct User(name: String, email: String) {
        def greet(self: Self): String {
            "Hello, #{self.name}!"
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            assert_eq!(struct_def.name.name, "User");
            assert!(struct_def.generic_params.is_none());
            assert_eq!(struct_def.fields.len(), 2);
            assert_eq!(struct_def.methods.len(), 1);

            // Check fields
            assert_eq!(struct_def.fields[0].name.name, "name");
            assert_eq!(struct_def.fields[1].name.name, "email");

            // Check method
            assert_eq!(struct_def.methods[0].name.name, "greet");
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_struct_with_generics() {
    let input = r#"struct Container<T>(value: T) {
        def get(self: Self): T {
            self.value
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            assert_eq!(struct_def.name.name, "Container");

            // Check generics
            assert!(struct_def.generic_params.is_some());
            let generics = struct_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");

            // Check field with generic type
            assert_eq!(struct_def.fields.len(), 1);
            assert_eq!(struct_def.fields[0].name.name, "value");
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_struct_with_multiple_generics() {
    let input = r#"struct Pair<T, U>(first: T, second: U) {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            assert_eq!(struct_def.name.name, "Pair");

            // Check multiple generics
            let generics = struct_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 2);
            assert_eq!(generics.params[0].name.name, "T");
            assert_eq!(generics.params[1].name.name, "U");

            // Check fields
            assert_eq!(struct_def.fields.len(), 2);
            assert_eq!(struct_def.fields[0].name.name, "first");
            assert_eq!(struct_def.fields[1].name.name, "second");
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_empty_struct() {
    let input = r#"struct Empty() {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            assert_eq!(struct_def.name.name, "Empty");
            assert!(struct_def.generic_params.is_none());
            assert_eq!(struct_def.fields.len(), 0);
            assert_eq!(struct_def.methods.len(), 0);
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_struct_with_complex_types() {
    let input = r#"struct Repository(users: List<User>, config: Database) {
        def find_user(self: Self, id: Integer): Option<User> {
            List.find(users: self.users, id: id)
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            assert_eq!(struct_def.name.name, "Repository");
            assert_eq!(struct_def.fields.len(), 2);
            assert_eq!(struct_def.methods.len(), 1);

            // Check field names
            assert_eq!(struct_def.fields[0].name.name, "users");
            assert_eq!(struct_def.fields[1].name.name, "config");
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_struct_display_formatting() {
    let inputs_and_patterns = [
        (
            r#"struct User(name: String) {}"#,
            vec!["struct User", "(name: String)"],
        ),
        (
            r#"struct Container<T>(value: T) {}"#,
            vec!["struct Container<T>", "(value: T)"],
        ),
    ];

    for (input, expected_patterns) in inputs_and_patterns.iter() {
        let program = OutrunParser::parse_program(input).unwrap();
        let formatted = format!("{}", program);

        for pattern in expected_patterns {
            assert!(
                formatted.contains(pattern),
                "Display format failed for: {}. Expected pattern '{}' not found in: {}",
                input,
                pattern,
                formatted
            );
        }
    }
}
