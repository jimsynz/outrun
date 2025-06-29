use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_simple_field_access() {
    let input = "self.name";

    let expr = OutrunParser::parse_expression(input).unwrap();

    match expr.kind {
        ExpressionKind::FieldAccess(field_access) => {
            match &field_access.object.kind {
                ExpressionKind::Identifier(id) => {
                    assert_eq!(id.name, "self");
                }
                _ => panic!("Expected identifier for object"),
            }

            assert_eq!(field_access.field.name, "name");
        }
        _ => panic!("Expected field access expression, got: {:?}", expr.kind),
    }
}

#[test]
fn test_chained_field_access() {
    let input = "user.profile.name";

    let expr = OutrunParser::parse_expression(input).unwrap();

    match expr.kind {
        ExpressionKind::FieldAccess(outer) => {
            assert_eq!(outer.field.name, "name");

            match &outer.object.kind {
                ExpressionKind::FieldAccess(inner) => {
                    assert_eq!(inner.field.name, "profile");

                    match &inner.object.kind {
                        ExpressionKind::Identifier(id) => {
                            assert_eq!(id.name, "user");
                        }
                        _ => panic!("Expected identifier for innermost object"),
                    }
                }
                _ => panic!("Expected field access for object"),
            }
        }
        _ => panic!("Expected field access expression"),
    }
}

#[test]
fn test_field_access_display() {
    let input = "self.name";
    let expr = OutrunParser::parse_expression(input).unwrap();
    let formatted = format!("{expr}");
    assert_eq!(formatted, "self.name");
}
