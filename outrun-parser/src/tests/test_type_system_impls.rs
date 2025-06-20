use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_basic_impl_block() {
    let input = r#"impl Drawable for User {
        def draw(self: Self): String {
            "User(#{self.name})"
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ImplBlock(impl_block) => {
            assert!(impl_block.generic_params.is_none());
            assert!(impl_block.constraints.is_none());
            assert_eq!(impl_block.methods.len(), 1);

            assert_eq!(impl_block.trait_spec.path.len(), 1);
            assert_eq!(impl_block.trait_spec.path[0].name, "Drawable");

            assert_eq!(impl_block.type_spec.path.len(), 1);
            assert_eq!(impl_block.type_spec.path[0].name, "User");

            assert_eq!(impl_block.methods[0].name.name, "draw");
        }
        _ => panic!("Expected impl block"),
    }
}

#[test]
fn test_impl_with_generics() {
    let input = r#"impl<T> Serializable<T> for Container<T> {
        def serialize(self: Self): T {
            self.value
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ImplBlock(impl_block) => {
            assert!(impl_block.generic_params.is_some());
            let generics = impl_block.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");

            assert_eq!(impl_block.trait_spec.path[0].name, "Serializable");
            assert!(impl_block.trait_spec.generic_args.is_some());

            assert_eq!(impl_block.type_spec.path[0].name, "Container");
            assert!(impl_block.type_spec.generic_args.is_some());
        }
        _ => panic!("Expected impl block"),
    }
}

#[test]
fn test_impl_with_constraints() {
    let input = r#"impl<T> Comparable<T> for List<T> when T: Orderable {
        def compare(self: Self, other: List<T>): Integer {
            List.compare(self, other)
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ImplBlock(impl_block) => {
            assert!(impl_block.constraints.is_some());
            match impl_block.constraints.as_ref().unwrap() {
                ConstraintExpression::Constraint {
                    type_param,
                    trait_bound,
                    ..
                } => {
                    assert_eq!(type_param.name, "T");
                    assert_eq!(trait_bound.len(), 1);
                    assert_eq!(trait_bound[0].name, "Orderable");
                }
                _ => panic!("Expected simple constraint"),
            }
        }
        _ => panic!("Expected impl block"),
    }
}

#[test]
fn test_impl_with_complex_constraints() {
    let input = r#"impl<T, U> Converter<T, U> for Adapter<T, U> when T: Serializable && U: Deserializable {
        def convert(input: T): U {
            U.deserialize(data: T.serialize(value: input))
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ImplBlock(impl_block) => {
            let generics = impl_block.generic_params.as_ref().unwrap();
            assert_eq!(generics.params.len(), 2);
            assert_eq!(generics.params[0].name.name, "T");
            assert_eq!(generics.params[1].name.name, "U");

            match impl_block.constraints.as_ref().unwrap() {
                ConstraintExpression::And { left, right, .. } => {
                    match left.as_ref() {
                        ConstraintExpression::Constraint {
                            type_param,
                            trait_bound,
                            ..
                        } => {
                            assert_eq!(type_param.name, "T");
                            assert_eq!(trait_bound[0].name, "Serializable");
                        }
                        _ => panic!("Expected T: Serializable constraint"),
                    }
                    match right.as_ref() {
                        ConstraintExpression::Constraint {
                            type_param,
                            trait_bound,
                            ..
                        } => {
                            assert_eq!(type_param.name, "U");
                            assert_eq!(trait_bound[0].name, "Deserializable");
                        }
                        _ => panic!("Expected U: Deserializable constraint"),
                    }
                }
                _ => panic!("Expected AND constraint"),
            }
        }
        _ => panic!("Expected impl block"),
    }
}

#[test]
fn test_impl_with_qualified_types() {
    let input = r#"impl Http.Client for Http.TcpClient {
        def request(self: Self, url: String): Http.Response {
            Http.get(url)
        }
    }"#;

    let program = OutrunParser::parse_program(input).unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        ItemKind::ImplBlock(impl_block) => {
            assert_eq!(impl_block.trait_spec.path.len(), 2);
            assert_eq!(impl_block.trait_spec.path[0].name, "Http");
            assert_eq!(impl_block.trait_spec.path[1].name, "Client");

            assert_eq!(impl_block.type_spec.path.len(), 2);
            assert_eq!(impl_block.type_spec.path[0].name, "Http");
            assert_eq!(impl_block.type_spec.path[1].name, "TcpClient");
        }
        _ => panic!("Expected impl block"),
    }
}

#[test]
fn test_impl_display_formatting() {
    let inputs_and_patterns = [
        (
            r#"impl Drawable for User {}"#,
            vec!["impl Drawable for User"],
        ),
        (
            r#"impl<T> Serializable<T> for Container<T> {}"#,
            vec!["impl<T>", "Serializable", "for Container"],
        ),
        (
            r#"impl<T> Comparable<T> for List<T> when T: Orderable {}"#,
            vec!["impl<T>", "when T: Orderable"],
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
