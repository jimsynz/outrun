use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_function_with_parameters() {
    let input = "def add(a: Integer, b: Integer): Integer { a + b }";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name.name, "add");
            assert_eq!(func.parameters.len(), 2);

            assert_eq!(func.parameters[0].name.name, "a");
            match &func.parameters[0].type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 1);
                    assert_eq!(path[0].name, "Integer");
                }
                _ => panic!("Expected simple type annotation"),
            }

            assert_eq!(func.parameters[1].name.name, "b");
            match &func.parameters[1].type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 1);
                    assert_eq!(path[0].name, "Integer");
                }
                _ => panic!("Expected simple type annotation"),
            }
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_function_with_return_type() {
    let input = "def calculate(): Float { 3.14 }";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name.name, "calculate");
            let return_type = &func.return_type;
            match return_type {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 1);
                    assert_eq!(path[0].name, "Float");
                }
                _ => panic!("Expected simple type annotation"),
            }
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_function_with_module_type() {
    let input = "def process(data: Http.Request): Http.Response { data }";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name.name, "process");
            assert_eq!(func.parameters.len(), 1);

            let param = &func.parameters[0];
            assert_eq!(param.name.name, "data");
            match &param.type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 2);
                    assert_eq!(path[0].name, "Http");
                    assert_eq!(path[1].name, "Request");
                }
                _ => panic!("Expected simple type annotation"),
            }

            let return_type = &func.return_type;
            match return_type {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path.len(), 2);
                    assert_eq!(path[0].name, "Http");
                    assert_eq!(path[1].name, "Response");
                }
                _ => panic!("Expected simple type annotation"),
            }
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_function_with_guard() {
    let input = "def divide(a: Integer, b: Integer): Float when Integer.non_zero?(b) { Float.from_integer(a) / Float.from_integer(b) }";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name.name, "divide");
            assert_eq!(func.parameters.len(), 2);
            assert!(func.guard.is_some());

            let guard = func.guard.as_ref().unwrap();
            match &guard.condition.kind {
                ExpressionKind::FunctionCall(call) => match &call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "Integer");
                        assert_eq!(name.name, "non_zero?");
                    }
                    _ => panic!("Expected qualified function call"),
                },
                _ => panic!("Expected function call in guard"),
            }
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_function_with_complex_body() {
    let input = "def complex_function(x: Integer): String { x * 2 }";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name.name, "complex_function");
            assert_eq!(func.body.statements.len(), 1);
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_function_with_all_features() {
    let input = "defp advanced_calc(x: Math.Number, y: Math.Number): Math.Result when Math.valid?(x) { Math.calculate(x: x, y: y) }";
    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.items.len(), 1);
    match &program.items[0].kind {
        ItemKind::FunctionDefinition(func) => {
            assert!(matches!(func.visibility, FunctionVisibility::Private));

            assert_eq!(func.name.name, "advanced_calc");

            assert_eq!(func.parameters.len(), 2);
            assert_eq!(func.parameters[0].name.name, "x");
            match &func.parameters[0].type_annotation {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path[0].name, "Math");
                    assert_eq!(path[1].name, "Number");
                }
                _ => panic!("Expected simple type annotation"),
            }

            let return_type = &func.return_type;
            match return_type {
                TypeAnnotation::Simple { path, .. } => {
                    assert_eq!(path[0].name, "Math");
                    assert_eq!(path[1].name, "Result");
                }
                _ => panic!("Expected simple type annotation"),
            }

            assert!(func.guard.is_some());

            assert_eq!(func.body.statements.len(), 1);
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_multiple_function_definitions() {
    let input = r#"
def first(): Integer { 1 }
defp second(x: String): Boolean { String.empty?(x) }
def third(a: Integer, b: Integer): Integer when Integer.positive?(a) { a + b }
"#;
    let program = OutrunParser::parse_program(input).unwrap();

    let functions: Vec<_> = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::FunctionDefinition(func) => Some(func),
            _ => None,
        })
        .collect();

    assert_eq!(functions.len(), 3);

    assert_eq!(functions[0].name.name, "first");
    assert!(matches!(
        functions[0].visibility,
        FunctionVisibility::Public
    ));
    assert!(functions[0].parameters.is_empty());

    assert_eq!(functions[1].name.name, "second");
    assert!(matches!(
        functions[1].visibility,
        FunctionVisibility::Private
    ));
    assert_eq!(functions[1].parameters.len(), 1);

    assert_eq!(functions[2].name.name, "third");
    assert_eq!(functions[2].parameters.len(), 2);
    assert!(functions[2].guard.is_some());
}

#[test]
fn test_function_definition_display() {
    let input =
        "def example(name: String): String when String.non_empty?(name) { \"Hello, \" + name }";
    let program = OutrunParser::parse_program(input).unwrap();

    let reconstructed = format!("{}", program);

    assert!(reconstructed.contains("def example"));
    assert!(reconstructed.contains("name: String"));
    assert!(reconstructed.contains(": String"));
    assert!(reconstructed.contains("when"));
    assert!(reconstructed.contains("String.non_empty?"));
    assert!(reconstructed.contains("Hello"));
}
