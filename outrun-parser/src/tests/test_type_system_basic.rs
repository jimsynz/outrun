use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_simple_struct_definition() {
    let input = r#"struct User(name: String, email: String) {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            assert_eq!(struct_def.name[0].name, "User");
            assert!(struct_def.generic_params.is_none());
            assert_eq!(struct_def.fields.len(), 2);
            assert_eq!(struct_def.functions.len(), 0);

            assert_eq!(struct_def.fields[0].name.name, "name");
            assert_eq!(struct_def.fields[1].name.name, "email");
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_struct_with_single_generic() {
    let input = r#"struct Container<T>(value: T) {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            assert_eq!(struct_def.name[0].name, "Container");

            assert!(struct_def.generic_params.is_some());
            let generics = struct_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");

            assert_eq!(struct_def.fields.len(), 1);
            assert_eq!(struct_def.fields[0].name.name, "value");
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
            assert_eq!(struct_def.name[0].name, "Empty");
            assert!(struct_def.generic_params.is_none());
            assert_eq!(struct_def.fields.len(), 0);
            assert_eq!(struct_def.functions.len(), 0);
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_simple_protocol_definition() {
    let input = r#"protocol Drawable {
        def draw(self: Self): String
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Drawable");
            assert!(protocol_def.generic_params.is_none());
            assert!(protocol_def.constraints.is_none());
            assert_eq!(protocol_def.functions.len(), 1);

            match &protocol_def.functions[0] {
                ProtocolFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "draw");
                    assert_eq!(sig.parameters.len(), 1);
                }
                _ => panic!("Expected function signature"),
            }
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_protocol_with_generics() {
    let input = r#"protocol Serializable<T> {
        def serialize(self: Self): T
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Serializable");

            assert!(protocol_def.generic_params.is_some());
            let generics = protocol_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_empty_protocol() {
    let input = r#"protocol Marker {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ProtocolDefinition(protocol_def) => {
            assert_eq!(protocol_def.name_as_string(), "Marker");
            assert!(protocol_def.generic_params.is_none());
            assert!(protocol_def.constraints.is_none());
            assert_eq!(protocol_def.functions.len(), 0);
        }
        _ => panic!("Expected protocol definition"),
    }
}

#[test]
fn test_simple_impl_block() {
    let input = r#"impl Drawable for User {
        def draw(self: Self): String {
            "simple implementation"
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ImplBlock(impl_block) => {
            assert!(impl_block.constraints.is_none());
            assert_eq!(impl_block.functions.len(), 1);

            assert_eq!(impl_block.protocol_spec.path.len(), 1);
            assert_eq!(impl_block.protocol_spec.path[0].name, "Drawable");

            assert_eq!(impl_block.type_spec.path.len(), 1);
            assert_eq!(impl_block.type_spec.path[0].name, "User");

            assert_eq!(impl_block.functions[0].name.name, "draw");
        }
        _ => panic!("Expected impl block"),
    }
}

#[test]
fn test_struct_display_formatting() {
    let input = r#"struct User(name: String) {}"#;
    let program = OutrunParser::parse_program(input).unwrap();
    let formatted = format!("{program}");

    assert!(formatted.contains("struct User"));
    assert!(formatted.contains("name: String"));
}

#[test]
fn test_protocol_display_formatting() {
    let input = r#"protocol Drawable {}"#;
    let program = OutrunParser::parse_program(input).unwrap();
    let formatted = format!("{program}");

    assert!(formatted.contains("protocol Drawable"));
}

#[test]
fn test_impl_display_formatting() {
    let input = r#"impl Drawable for User {}"#;
    let program = OutrunParser::parse_program(input).unwrap();
    let formatted = format!("{program}");

    assert!(formatted.contains("impl Drawable for User"));
}
