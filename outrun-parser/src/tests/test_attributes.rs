use crate::ast::*;
use crate::parser::OutrunParser;

// Test basic attribute parsing

#[test]
fn test_struct_with_basic_attribute() {
    let input = r#"@Derive(traits: [Debug])
struct User(name: String) {
}"#;

    let result = OutrunParser::parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            // Check attributes
            assert_eq!(struct_def.attributes.len(), 1);
            assert_eq!(struct_def.attributes[0].name.name, "Derive");
            assert!(struct_def.attributes[0].args.is_some());

            // Check struct properties
            assert_eq!(struct_def.name.name, "User");
            assert_eq!(struct_def.fields.len(), 1);
            assert_eq!(struct_def.fields[0].name.name, "name");
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_struct_with_no_args_attribute() {
    let input = r#"@Serializable
struct Config(value: String) {
}"#;

    let result = OutrunParser::parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            // Check attributes
            assert_eq!(struct_def.attributes.len(), 1);
            assert_eq!(struct_def.attributes[0].name.name, "Serializable");
            assert!(struct_def.attributes[0].args.is_none());

            // Check struct properties
            assert_eq!(struct_def.name.name, "Config");
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_struct_with_complex_attribute() {
    let input = r#"@Config(database: {host: "localhost", port: 5432}, retry: true)
struct DatabaseConnection(url: String) {
}"#;

    let result = OutrunParser::parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            // Check attributes
            assert_eq!(struct_def.attributes.len(), 1);
            assert_eq!(struct_def.attributes[0].name.name, "Config");
            assert!(struct_def.attributes[0].args.is_some());

            let args = struct_def.attributes[0].args.as_ref().unwrap();
            assert_eq!(args.arguments.len(), 2);

            // Check struct properties
            assert_eq!(struct_def.name.name, "DatabaseConnection");
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_function_with_attribute() {
    let input = r#"@Deprecated(since: "1.0.0")
def old_function(): String {
    "deprecated"
}"#;

    let result = OutrunParser::parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::FunctionDefinition(func_def) => {
            // Check attributes
            assert_eq!(func_def.attributes.len(), 1);
            assert_eq!(func_def.attributes[0].name.name, "Deprecated");
            assert!(func_def.attributes[0].args.is_some());

            // Check function properties
            assert_eq!(func_def.name.name, "old_function");
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_struct_with_multiple_attributes() {
    let input = r#"@Derive(traits: [Debug, Clone])
@Serializable
@CustomAttribute(key: "value")
struct User(name: String, age: Integer) {
}"#;

    let result = OutrunParser::parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            // Check attributes
            assert_eq!(struct_def.attributes.len(), 3);

            assert_eq!(struct_def.attributes[0].name.name, "Derive");
            assert!(struct_def.attributes[0].args.is_some());

            assert_eq!(struct_def.attributes[1].name.name, "Serializable");
            assert!(struct_def.attributes[1].args.is_none());

            assert_eq!(struct_def.attributes[2].name.name, "CustomAttribute");
            assert!(struct_def.attributes[2].args.is_some());

            // Check struct properties
            assert_eq!(struct_def.name.name, "User");
            assert_eq!(struct_def.fields.len(), 2);
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_trait_with_attribute() {
    let input = r#"@Exportable
trait Drawable {
    def draw(self: Self): String
}"#;

    let result = OutrunParser::parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);

    match &result.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            // Check attributes
            assert_eq!(trait_def.attributes.len(), 1);
            assert_eq!(trait_def.attributes[0].name.name, "Exportable");
            assert!(trait_def.attributes[0].args.is_none());

            // Check trait properties
            assert_eq!(trait_def.name.name, "Drawable");
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_attribute_display_formatting() {
    let input = r#"@Derive(traits: [Debug])
struct User(name: String) {
}"#;

    let result = OutrunParser::parse_program(input).unwrap();

    match &result.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            let formatted = format!("{}", struct_def);

            // Check that the formatted output includes the attribute
            assert!(formatted.contains("@Derive"));
            assert!(formatted.contains("traits:"));
            assert!(formatted.contains("struct User"));
        }
        _ => panic!("Expected struct definition"),
    }
}
