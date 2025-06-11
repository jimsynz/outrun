// Test basic type system parsing without complex features
// Tests for struct, trait, and impl syntax without field access

use outrun_parser::ast::*;
use outrun_parser::parser::OutrunParser;

#[test]
fn test_simple_struct_definition() {
    let input = r#"struct User(name: String, email: String) {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::StructDefinition(struct_def) => {
            assert_eq!(struct_def.name.name, "User");
            assert!(struct_def.generic_params.is_none());
            assert_eq!(struct_def.fields.len(), 2);
            assert_eq!(struct_def.methods.len(), 0);

            // Check fields
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
            assert_eq!(struct_def.name.name, "Container");

            // Check generics
            assert!(struct_def.generic_params.is_some());
            let generics = struct_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");

            // Check field
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
            assert_eq!(struct_def.name.name, "Empty");
            assert!(struct_def.generic_params.is_none());
            assert_eq!(struct_def.fields.len(), 0);
            assert_eq!(struct_def.methods.len(), 0);
        }
        _ => panic!("Expected struct definition"),
    }
}

#[test]
fn test_simple_trait_definition() {
    let input = r#"trait Drawable {
        def draw(self: Self): String
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Drawable");
            assert!(trait_def.generic_params.is_none());
            assert!(trait_def.constraints.is_none());
            assert_eq!(trait_def.functions.len(), 1);

            // Check function signature
            match &trait_def.functions[0] {
                TraitFunction::Signature(sig) => {
                    assert_eq!(sig.name.name, "draw");
                    assert_eq!(sig.parameters.len(), 1);
                    assert!(sig.return_type.is_some());
                }
                _ => panic!("Expected function signature"),
            }
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_trait_with_generics() {
    let input = r#"trait Serializable<T> {
        def serialize(self: Self): T
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Serializable");

            // Check generics
            assert!(trait_def.generic_params.is_some());
            let generics = trait_def.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");
        }
        _ => panic!("Expected trait definition"),
    }
}

#[test]
fn test_empty_trait() {
    let input = r#"trait Marker {}"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::TraitDefinition(trait_def) => {
            assert_eq!(trait_def.name.name, "Marker");
            assert!(trait_def.generic_params.is_none());
            assert!(trait_def.constraints.is_none());
            assert_eq!(trait_def.functions.len(), 0);
        }
        _ => panic!("Expected trait definition"),
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
            assert!(impl_block.generic_params.is_none());
            assert!(impl_block.constraints.is_none());
            assert_eq!(impl_block.methods.len(), 1);

            // Check trait spec
            assert_eq!(impl_block.trait_spec.path.len(), 1);
            assert_eq!(impl_block.trait_spec.path[0].name, "Drawable");

            // Check type spec
            assert_eq!(impl_block.type_spec.path.len(), 1);
            assert_eq!(impl_block.type_spec.path[0].name, "User");

            // Check method
            assert_eq!(impl_block.methods[0].name.name, "draw");
        }
        _ => panic!("Expected impl block"),
    }
}

#[test]
fn test_struct_display_formatting() {
    let input = r#"struct User(name: String) {}"#;
    let program = OutrunParser::parse_program(input).unwrap();
    let formatted = format!("{}", program);

    assert!(formatted.contains("struct User"));
    assert!(formatted.contains("name: String"));
}

#[test]
fn test_trait_display_formatting() {
    let input = r#"trait Drawable {}"#;
    let program = OutrunParser::parse_program(input).unwrap();
    let formatted = format!("{}", program);

    assert!(formatted.contains("trait Drawable"));
}

#[test]
fn test_impl_display_formatting() {
    let input = r#"impl Drawable for User {}"#;
    let program = OutrunParser::parse_program(input).unwrap();
    let formatted = format!("{}", program);

    assert!(formatted.contains("impl Drawable for User"));
}
