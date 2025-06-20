use crate::*;

#[test]
fn test_function_call_with_positional_string_should_fail() {
    let input = r#"func("hello")"#;

    let result = parse_program(input);

    if result.is_ok() {
        let program = result.unwrap();

        assert_eq!(
            program.items.len(),
            1,
            "Expected either parse failure or single function call, got {} items",
            program.items.len()
        );

        match &program.items[0].kind {
            ItemKind::Expression(expr) => {
                match &expr.kind {
                    ExpressionKind::FunctionCall(_) => {
                        panic!("Function call with string literal should fail, not parse as function call");
                    }
                    _ => {
                        panic!("Function call with invalid arguments parsed as separate expressions instead of failing. \
                               Expected parse error, got: {:?}", expr.kind);
                    }
                }
            }
            _ => {
                panic!("Expected expression item, got: {:?}", program.items[0].kind);
            }
        }
    }
}

#[test]
fn test_function_call_with_positional_number_should_fail() {
    let input = r#"calculate(42)"#;

    let result = parse_program(input);

    if result.is_ok() {
        let program = result.unwrap();

        assert_eq!(program.items.len(), 2, "Current bug: should parse as 2 separate expressions (function name + parenthesized number)");

        match &program.items[0].kind {
            ItemKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Identifier(id) => {
                    assert_eq!(id.name, "calculate");
                }
                _ => panic!("Expected first item to be identifier 'calculate'"),
            },
            _ => panic!("Expected first item to be expression"),
        }

        match &program.items[1].kind {
            ItemKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Parenthesized(_) => {}
                _ => panic!("Expected second item to be parenthesized expression"),
            },
            _ => panic!("Expected second item to be expression"),
        }

        panic!(
            "Function call with invalid positional argument should fail to parse, \
               but currently parses as {} separate expressions",
            program.items.len()
        );
    } else {
        println!("✓ Parse correctly failed as expected");
    }
}

#[test]
fn test_function_call_with_complex_positional_args_should_fail() {
    let input = r#"process(123, "string", [1, 2, 3])"#;

    let result = parse_program(input);

    if result.is_ok() {
        let program = result.unwrap();

        assert_eq!(program.items.len(), 2);
        panic!(
            "Complex positional arguments should cause parse failure, \
               got {} items instead",
            program.items.len()
        );
    }
}

#[test]
fn test_valid_function_calls_should_succeed() {
    let input1 = r#"func(message: "hello")"#;
    let result1 = parse_program(input1).expect("Named argument should parse successfully");
    assert_eq!(result1.items.len(), 1);

    let input2 = r#"func(message)"#;
    let result2 = parse_program(input2).expect("Shorthand argument should parse successfully");
    assert_eq!(result2.items.len(), 1);

    let input3 = r#"func()"#;
    let result3 = parse_program(input3).expect("No arguments should parse successfully");
    assert_eq!(result3.items.len(), 1);

    let input4 = r#"Module.func(arg: value)"#;
    let result4 = parse_program(input4).expect("Qualified function call should parse successfully");
    assert_eq!(result4.items.len(), 1);
}
