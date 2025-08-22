// Tests for struct patterns with qualified type paths
// This tests the fix for allowing Outrun.Option.Some { value } patterns

use outrun_parser::parse_program;

#[test]
fn test_simple_qualified_struct_pattern() {
    let input = r#"
        case option {
            Outrun.Option.Some { value } -> value
            Outrun.Option.None {} -> 0
        }
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse qualified struct pattern: {:?}",
        result.err()
    );

    let program = result.unwrap();
    assert_eq!(program.items.len(), 1);

    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.clauses.len(), 2);

                    // Check first clause pattern
                    match &case_expr.clauses[0].pattern {
                        outrun_parser::Pattern::Struct(struct_pattern) => {
                            // The type_path should be a qualified path
                            assert_eq!(struct_pattern.type_path.len(), 3);
                            assert_eq!(struct_pattern.type_path[0].name, "Outrun");
                            assert_eq!(struct_pattern.type_path[1].name, "Option");
                            assert_eq!(struct_pattern.type_path[2].name, "Some");
                            assert_eq!(struct_pattern.fields.len(), 1);
                        }
                        _ => panic!("Expected struct pattern"),
                    }

                    // Check second clause pattern
                    match &case_expr.clauses[1].pattern {
                        outrun_parser::Pattern::Struct(struct_pattern) => {
                            assert_eq!(struct_pattern.type_path.len(), 3);
                            assert_eq!(struct_pattern.type_path[0].name, "Outrun");
                            assert_eq!(struct_pattern.type_path[1].name, "Option");
                            assert_eq!(struct_pattern.type_path[2].name, "None");
                            assert_eq!(struct_pattern.fields.len(), 0);
                        }
                        _ => panic!("Expected struct pattern"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_nested_qualified_struct_patterns() {
    let input = r#"
        case result {
            Outrun.Result.Ok { value: Outrun.Option.Some { value: x } } -> x
            Outrun.Result.Ok { value: Outrun.Option.None {} } -> 0
            Outrun.Result.Err { error } -> -1
        }
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse nested qualified struct patterns: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.clauses.len(), 3);

                    // First clause should have nested patterns
                    match &case_expr.clauses[0].pattern {
                        outrun_parser::Pattern::Struct(outer_pattern) => {
                            assert_eq!(outer_pattern.type_path.len(), 3);
                            assert_eq!(outer_pattern.type_path[0].name, "Outrun");
                            assert_eq!(outer_pattern.type_path[1].name, "Result");
                            assert_eq!(outer_pattern.type_path[2].name, "Ok");
                            
                            // Check the nested pattern
                            assert_eq!(outer_pattern.fields.len(), 1);
                            let field = &outer_pattern.fields[0];
                            assert_eq!(field.name.name, "value");
                            
                            // The nested pattern should be another struct pattern
                            match &field.pattern {
                                Some(outrun_parser::Pattern::Struct(inner_pattern)) => {
                                    assert_eq!(inner_pattern.type_path.len(), 3);
                                    assert_eq!(inner_pattern.type_path[0].name, "Outrun");
                                    assert_eq!(inner_pattern.type_path[1].name, "Option");
                                    assert_eq!(inner_pattern.type_path[2].name, "Some");
                                }
                                _ => panic!("Expected nested struct pattern"),
                            }
                        }
                        _ => panic!("Expected struct pattern"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_mixed_qualified_and_simple_patterns() {
    let input = r#"
        case value {
            Outrun.Core.User { name, age } -> name
            SimpleType { id } -> "simple"
            Http.Response.Success { status } -> "http"
        }
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse mixed qualified and simple patterns: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::Expression(expr) => {
            match &expr.kind {
                outrun_parser::ExpressionKind::CaseExpression(case_expr) => {
                    assert_eq!(case_expr.clauses.len(), 3);

                    // First clause - qualified path
                    match &case_expr.clauses[0].pattern {
                        outrun_parser::Pattern::Struct(pattern) => {
                            assert_eq!(pattern.type_path.len(), 3);
                            assert_eq!(pattern.type_path[0].name, "Outrun");
                        }
                        _ => panic!("Expected struct pattern"),
                    }

                    // Second clause - simple type
                    match &case_expr.clauses[1].pattern {
                        outrun_parser::Pattern::Struct(pattern) => {
                            assert_eq!(pattern.type_path.len(), 1);
                            assert_eq!(pattern.type_path[0].name, "SimpleType");
                        }
                        _ => panic!("Expected struct pattern"),
                    }

                    // Third clause - different qualified path
                    match &case_expr.clauses[2].pattern {
                        outrun_parser::Pattern::Struct(pattern) => {
                            assert_eq!(pattern.type_path.len(), 3);
                            assert_eq!(pattern.type_path[0].name, "Http");
                        }
                        _ => panic!("Expected struct pattern"),
                    }
                }
                _ => panic!("Expected case expression"),
            }
        }
        _ => panic!("Expected expression item"),
    }
}

#[test]
fn test_let_binding_with_qualified_struct_pattern() {
    let input = r#"
        let Outrun.Option.Some { value } = get_option()
    "#;

    let result = parse_program(input);
    assert!(
        result.is_ok(),
        "Failed to parse let binding with qualified struct pattern: {:?}",
        result.err()
    );

    let program = result.unwrap();
    match &program.items[0].kind {
        outrun_parser::ItemKind::LetBinding(let_binding) => {
            match &let_binding.pattern {
                outrun_parser::Pattern::Struct(struct_pattern) => {
                    assert_eq!(struct_pattern.type_path.len(), 3);
                    assert_eq!(struct_pattern.type_path[0].name, "Outrun");
                    assert_eq!(struct_pattern.type_path[1].name, "Option");
                    assert_eq!(struct_pattern.type_path[2].name, "Some");
                }
                _ => panic!("Expected struct pattern"),
            }
        }
        _ => panic!("Expected let binding"),
    }
}