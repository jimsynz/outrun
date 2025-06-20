use crate::ast::*;
use crate::{parse_program, ExpressionKind, ItemKind};

#[test]
fn test_function_type_single_parameter() {
    let input =
        r#"def process(callback: Function<(x: Integer) -> String>): String { callback(x: 42) }"#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    if let ItemKind::FunctionDefinition(func_def) = &result.items[0].kind {
        assert_eq!(func_def.name.name, "process");
        assert_eq!(func_def.parameters.len(), 1);

        let param = &func_def.parameters[0];
        assert_eq!(param.name.name, "callback");

        match &param.type_annotation {
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name.name, "x");

                match &params[0].type_annotation {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "Integer");
                    }
                    _ => panic!("Expected simple type annotation"),
                }

                match return_type.as_ref() {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "String");
                    }
                    _ => panic!("Expected simple type annotation for return type"),
                }
            }
            _ => panic!("Expected function type annotation"),
        }
    } else {
        panic!("Expected function definition");
    }
}

#[test]
fn test_function_type_no_parameters() {
    let input = r#"def run(task: Function<() -> Unit>): Unit { task() }"#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    if let ItemKind::FunctionDefinition(func_def) = &result.items[0].kind {
        assert_eq!(func_def.name.name, "run");
        assert_eq!(func_def.parameters.len(), 1);

        let param = &func_def.parameters[0];
        assert_eq!(param.name.name, "task");

        match &param.type_annotation {
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                assert_eq!(params.len(), 0); // No parameters

                match return_type.as_ref() {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path.len(), 1);
                        assert_eq!(path[0].name, "Unit");
                    }
                    _ => panic!("Expected simple type annotation for return type"),
                }
            }
            _ => panic!("Expected function type annotation"),
        }
    } else {
        panic!("Expected function definition");
    }
}

#[test]
fn test_function_type_multiple_parameters() {
    let input = r#"def combine(merger: Function<(x: Integer, y: String) -> Boolean>): Boolean { merger(x: 1, y: "test") }"#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    if let ItemKind::FunctionDefinition(func_def) = &result.items[0].kind {
        assert_eq!(func_def.name.name, "combine");
        assert_eq!(func_def.parameters.len(), 1);

        let param = &func_def.parameters[0];
        assert_eq!(param.name.name, "merger");

        match &param.type_annotation {
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                assert_eq!(params.len(), 2);

                assert_eq!(params[0].name.name, "x");
                match &params[0].type_annotation {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "Integer");
                    }
                    _ => panic!("Expected simple type annotation"),
                }

                assert_eq!(params[1].name.name, "y");
                match &params[1].type_annotation {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "String");
                    }
                    _ => panic!("Expected simple type annotation"),
                }

                match return_type.as_ref() {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "Boolean");
                    }
                    _ => panic!("Expected simple type annotation for return type"),
                }
            }
            _ => panic!("Expected function type annotation"),
        }
    } else {
        panic!("Expected function definition");
    }
}

#[test]
fn test_function_type_in_struct_field() {
    let input = r#"struct EventHandler(on_click: Function<(event: Event) -> Unit>, validator: Function<(input: String) -> Boolean>) {}"#;

    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    if let ItemKind::StructDefinition(struct_def) = &result.items[0].kind {
        assert_eq!(struct_def.name[0].name, "EventHandler");
        assert_eq!(struct_def.fields.len(), 2);

        let field1 = &struct_def.fields[0];
        assert_eq!(field1.name.name, "on_click");
        match &field1.type_annotation {
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name.name, "event");
                match &params[0].type_annotation {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "Event");
                    }
                    _ => panic!("Expected simple type annotation"),
                }

                match return_type.as_ref() {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "Unit");
                    }
                    _ => panic!("Expected simple type annotation for return type"),
                }
            }
            _ => panic!("Expected function type annotation"),
        }

        let field2 = &struct_def.fields[1];
        assert_eq!(field2.name.name, "validator");
        match &field2.type_annotation {
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name.name, "input");
                match &params[0].type_annotation {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "String");
                    }
                    _ => panic!("Expected simple type annotation"),
                }

                match return_type.as_ref() {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "Boolean");
                    }
                    _ => panic!("Expected simple type annotation for return type"),
                }
            }
            _ => panic!("Expected function type annotation"),
        }
    } else {
        panic!("Expected struct definition");
    }
}

#[test]
fn test_function_type_with_generic_return_type() {
    let input =
        r#"def map_list(mapper: Function<(item: T) -> U>): List<U> { List.map(list, mapper) }"#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    if let ItemKind::FunctionDefinition(func_def) = &result.items[0].kind {
        assert_eq!(func_def.name.name, "map_list");
        assert_eq!(func_def.parameters.len(), 1);

        let param = &func_def.parameters[0];
        assert_eq!(param.name.name, "mapper");

        match &param.type_annotation {
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name.name, "item");

                match &params[0].type_annotation {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "T");
                    }
                    _ => panic!("Expected simple type annotation"),
                }

                match return_type.as_ref() {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "U");
                    }
                    _ => panic!("Expected simple type annotation for return type"),
                }
            }
            _ => panic!("Expected function type annotation"),
        }
    } else {
        panic!("Expected function definition");
    }
}

#[test]
fn test_function_type_with_complex_generic_return_type() {
    let input = r#"def process_data(handler: Function<(data: String) -> Result<User, DatabaseError>>): Unit { handler(data: "user_data") }"#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    if let ItemKind::FunctionDefinition(func_def) = &result.items[0].kind {
        assert_eq!(func_def.name.name, "process_data");
        assert_eq!(func_def.parameters.len(), 1);

        let param = &func_def.parameters[0];
        assert_eq!(param.name.name, "handler");

        match &param.type_annotation {
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name.name, "data");

                match &params[0].type_annotation {
                    TypeAnnotation::Simple { path, .. } => {
                        assert_eq!(path[0].name, "String");
                    }
                    _ => panic!("Expected simple type annotation"),
                }

                match return_type.as_ref() {
                    TypeAnnotation::Simple {
                        path, generic_args, ..
                    } => {
                        assert_eq!(path[0].name, "Result");
                        assert!(generic_args.is_some());

                        if let Some(args) = generic_args {
                            assert_eq!(args.args.len(), 2);

                            match &args.args[0] {
                                TypeAnnotation::Simple { path, .. } => {
                                    assert_eq!(path[0].name, "User");
                                }
                                _ => panic!("Expected simple type annotation for User"),
                            }

                            match &args.args[1] {
                                TypeAnnotation::Simple { path, .. } => {
                                    assert_eq!(path[0].name, "DatabaseError");
                                }
                                _ => panic!("Expected simple type annotation for DatabaseError"),
                            }
                        }
                    }
                    _ => panic!("Expected simple type annotation with generics for return type"),
                }
            }
            _ => panic!("Expected function type annotation"),
        }
    } else {
        panic!("Expected function definition");
    }
}

#[test]
fn test_function_type_in_variable_binding() {
    let input = r#"let validator: Function<(email: String) -> Boolean> = &Email.valid?"#;
    let result = parse_program(input).unwrap();

    assert_eq!(result.items.len(), 1);
    if let ItemKind::LetBinding(let_binding) = &result.items[0].kind {
        match &let_binding.pattern {
            Pattern::Identifier(id) => {
                assert_eq!(id.name, "validator");
            }
            _ => panic!("Expected identifier pattern"),
        }

        assert!(let_binding.type_annotation.is_some());
        if let Some(type_annotation) = &let_binding.type_annotation {
            match type_annotation {
                TypeAnnotation::Function {
                    params,
                    return_type,
                    ..
                } => {
                    assert_eq!(params.len(), 1);
                    assert_eq!(params[0].name.name, "email");

                    match &params[0].type_annotation {
                        TypeAnnotation::Simple { path, .. } => {
                            assert_eq!(path[0].name, "String");
                        }
                        _ => panic!("Expected simple type annotation"),
                    }

                    match return_type.as_ref() {
                        TypeAnnotation::Simple { path, .. } => {
                            assert_eq!(path[0].name, "Boolean");
                        }
                        _ => panic!("Expected simple type annotation for return type"),
                    }
                }
                _ => panic!("Expected function type annotation"),
            }
        }

        match &let_binding.expression.kind {
            ExpressionKind::FunctionCapture(capture) => {
                assert_eq!(capture.function_name.name, "valid?");
                assert!(capture.module_path.is_some());
                if let Some(path) = &capture.module_path {
                    assert_eq!(path[0].name, "Email");
                }
            }
            _ => panic!("Expected function capture expression"),
        }
    } else {
        panic!("Expected let binding");
    }
}

#[test]
fn test_function_type_display_formatting() {
    let input =
        r#"def process(callback: Function<(x: Integer) -> String>): String { callback(x: 42) }"#;
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);

    assert!(formatted.contains("Function<(x: Integer) -> String>"));
}

#[test]
fn test_function_type_no_parameters_display() {
    let input = r#"def run(task: Function<() -> Unit>): Unit { task() }"#;
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);

    assert!(formatted.contains("Function<() -> Unit>"));
}

#[test]
fn test_function_type_multiple_parameters_display() {
    let input =
        r#"def combine(merger: Function<(x: Integer, y: String) -> Boolean>): Boolean { true }"#;
    let result = parse_program(input).unwrap();

    let formatted = format!("{}", result);

    assert!(formatted.contains("Function<(x: Integer, y: String) -> Boolean>"));
}
