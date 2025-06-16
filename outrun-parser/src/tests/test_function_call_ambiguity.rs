// Function call ambiguity tests
// These tests exercise the parser bug where invalid function calls
// are parsed as separate expressions instead of failing

use crate::*;

#[test]
fn test_function_call_with_positional_string_should_fail() {
    // This should FAIL to parse, not succeed as separate expressions
    let input = r#"func("hello")"#;

    // Currently this incorrectly parses as two separate expressions:
    // 1. `func` (identifier)
    // 2. `("hello")` (parenthesized string)

    let result = parse_program(input);

    // EXPECTED BEHAVIOR: This should fail because "hello" cannot be a shorthand argument
    // (only identifiers can be shorthand arguments in Outrun)
    if result.is_ok() {
        let program = result.unwrap();

        // If it parses successfully, it should be a single function call, not separate expressions
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
                        // This is the correct behavior - a single function call
                        // But it should actually FAIL because "hello" is not a valid shorthand argument
                        panic!("Function call with string literal should fail, not parse as function call");
                    }
                    _ => {
                        // This is the current buggy behavior - parsing as separate expressions
                        panic!("Function call with invalid arguments parsed as separate expressions instead of failing. \
                               Expected parse error, got: {:?}", expr.kind);
                    }
                }
            }
            _ => {
                panic!("Expected expression item, got: {:?}", program.items[0].kind);
            }
        }
    } else {
        // This is the DESIRED behavior - the parse should fail
        println!("✓ Parse correctly failed as expected");
    }
}

#[test]
fn test_function_call_with_positional_number_should_fail() {
    // This should FAIL to parse, not succeed as separate expressions
    let input = r#"calculate(42)"#;

    let result = parse_program(input);

    // EXPECTED BEHAVIOR: This should fail because 42 cannot be a shorthand argument
    if result.is_ok() {
        let program = result.unwrap();

        // Current buggy behavior: parses as separate expressions
        assert_eq!(program.items.len(), 2, "Current bug: should parse as 2 separate expressions (function name + parenthesized number)");

        // Verify the buggy parsing structure
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
            ItemKind::Expression(expr) => {
                match &expr.kind {
                    ExpressionKind::Parenthesized(_) => {
                        // This confirms the buggy behavior
                    }
                    _ => panic!("Expected second item to be parenthesized expression"),
                }
            }
            _ => panic!("Expected second item to be expression"),
        }

        panic!(
            "Function call with invalid positional argument should fail to parse, \
               but currently parses as {} separate expressions",
            program.items.len()
        );
    } else {
        // This is the DESIRED behavior
        println!("✓ Parse correctly failed as expected");
    }
}

#[test]
fn test_function_call_with_complex_positional_args_should_fail() {
    // Complex case that should definitely fail
    let input = r#"process(123, "string", [1, 2, 3])"#;

    let result = parse_program(input);

    if result.is_ok() {
        let program = result.unwrap();

        // This currently parses as:
        // 1. `process` (identifier)
        // 2. `(123, "string", [1, 2, 3])` (tuple literal)

        assert_eq!(program.items.len(), 2);
        panic!(
            "Complex positional arguments should cause parse failure, \
               got {} items instead",
            program.items.len()
        );
    } else {
        println!("✓ Complex positional args correctly failed to parse");
    }
}

#[test]
fn test_valid_function_calls_should_succeed() {
    // These should continue to work correctly

    // Valid: named argument
    let input1 = r#"func(message: "hello")"#;
    let result1 = parse_program(input1).expect("Named argument should parse successfully");
    assert_eq!(result1.items.len(), 1);

    // Valid: shorthand argument (identifier)
    let input2 = r#"func(message)"#;
    let result2 = parse_program(input2).expect("Shorthand argument should parse successfully");
    assert_eq!(result2.items.len(), 1);

    // Valid: no arguments
    let input3 = r#"func()"#;
    let result3 = parse_program(input3).expect("No arguments should parse successfully");
    assert_eq!(result3.items.len(), 1);

    // Valid: qualified function call
    let input4 = r#"Module.func(arg: value)"#;
    let result4 = parse_program(input4).expect("Qualified function call should parse successfully");
    assert_eq!(result4.items.len(), 1);

    println!("✓ All valid function calls parsed correctly");
}
