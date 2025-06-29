// Minimal test for case expressions to debug parsing issues

use outrun_parser::parse_program;

#[test]
fn test_minimal_case_expression() {
    let input = r#"
        case value {
            42 -> "answer"
        }
    "#;

    let result = parse_program(input);
    if let Err(ref e) = result {
        println!("Parse error: {e:?}");
    }
    assert!(result.is_ok(), "Failed to parse minimal case expression");
}

#[test]
fn test_case_with_simple_guard() {
    let input = r#"
        case value {
            x when true -> "found"
        }
    "#;

    let result = parse_program(input);
    if let Err(ref e) = result {
        println!("Parse error: {e:?}");
    }
    assert!(result.is_ok(), "Failed to parse case with simple guard");
}

#[test]
fn test_case_with_identifier_pattern() {
    let input = r#"
        case value {
            x -> "any value"
        }
    "#;

    let result = parse_program(input);
    if let Err(ref e) = result {
        println!("Parse error: {e:?}");
    }
    assert!(
        result.is_ok(),
        "Failed to parse case with identifier pattern"
    );
}
