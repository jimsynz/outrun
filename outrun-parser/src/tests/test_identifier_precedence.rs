use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_capitalized_identifiers_in_expressions() {
    let result = OutrunParser::parse_expression("String");

    assert!(result.is_ok(), "Should parse 'String' as expression");
    let expr = result.unwrap();

    match expr.kind {
        ExpressionKind::TypeIdentifier(tid) => {
            assert_eq!(tid.name, "String");
            println!("✓ 'String' parsed as TypeIdentifier: '{}'", tid.name);
        }
        ExpressionKind::Identifier(id) => {
            panic!("Expected TypeIdentifier but got Identifier: '{}'", id.name);
        }
        _ => {
            panic!("Expected TypeIdentifier but got: {:?}", expr.kind);
        }
    }
}

#[test]
fn test_type_identifier_in_struct_literal_context() {
    let result = OutrunParser::parse_expression("String { value: \"hello\" }");

    assert!(result.is_ok(), "Should parse struct literal");
    let expr = result.unwrap();

    match expr.kind {
        ExpressionKind::Struct(struct_lit) => {
            assert_eq!(struct_lit.type_path.len(), 1);
            assert_eq!(struct_lit.type_path[0].name, "String");
            println!(
                "✓ 'String' in struct literal parsed as TypeIdentifier: '{}'",
                struct_lit.type_path[0].name
            );
        }
        _ => {
            panic!("Expected Struct literal but got: {:?}", expr.kind);
        }
    }
}

#[test]
fn test_qualified_identifier_precedence() {
    let result = OutrunParser::parse_expression("String.length");

    assert!(result.is_ok(), "Should parse qualified identifier");
    let expr = result.unwrap();

    match expr.kind {
        ExpressionKind::QualifiedIdentifier(qualified) => {
            assert_eq!(qualified.module.name, "String");
            assert_eq!(qualified.name.name, "length");
            println!(
                "✓ 'String.length' parsed as QualifiedIdentifier: '{}.{}'",
                qualified.module.name, qualified.name.name
            );
        }
        _ => {
            panic!("Expected QualifiedIdentifier but got: {:?}", expr.kind);
        }
    }
}

#[test]
fn test_lowercase_identifier() {
    let result = OutrunParser::parse_expression("string");

    assert!(result.is_ok(), "Should parse 'string' as expression");
    let expr = result.unwrap();

    match expr.kind {
        ExpressionKind::Identifier(id) => {
            assert_eq!(id.name, "string");
            println!("✓ 'string' parsed as Identifier: '{}'", id.name);
        }
        _ => {
            panic!("Expected Identifier but got: {:?}", expr.kind);
        }
    }
}

#[test]
fn test_contexts_where_type_identifier_appears() {
    let struct_result = OutrunParser::parse_expression("String { value: \"hello\" }");
    assert!(struct_result.is_ok());
    match struct_result.unwrap().kind {
        ExpressionKind::Struct(struct_lit) => {
            assert_eq!(struct_lit.type_path.len(), 1);
            assert_eq!(struct_lit.type_path[0].name, "String");
        }
        _ => panic!("Expected struct literal"),
    }

    let qualified_result = OutrunParser::parse_expression("String.upcase");
    assert!(qualified_result.is_ok());
    match qualified_result.unwrap().kind {
        ExpressionKind::QualifiedIdentifier(qualified) => {
            assert_eq!(qualified.module.name, "String");
            assert_eq!(qualified.name.name, "upcase");
        }
        _ => panic!("Expected qualified identifier"),
    }

    let standalone_result = OutrunParser::parse_expression("String");
    assert!(standalone_result.is_ok());
    match standalone_result.unwrap().kind {
        ExpressionKind::TypeIdentifier(tid) => {
            assert_eq!(tid.name, "String");
            println!("✓ Standalone 'String' in expression context is TypeIdentifier");
        }
        _ => panic!("Expected TypeIdentifier"),
    }
}
