use crate::*;

#[test]
fn test_source_reconstruction_integers() {
    let inputs = ["42", "0b1010", "0o755", "0xFF", "0xabc"];

    for input in &inputs {
        let result = parse_program(input).unwrap();
        let reconstructed = format!("{}", result);

        let expected = if input == &"0xFF" { "0xff" } else { *input };

        assert_eq!(
            reconstructed, expected,
            "Source reconstruction failed for '{}': expected '{}', got '{}'",
            input, expected, reconstructed
        );
    }
}

#[test]
fn test_source_reconstruction_mixed() {
    let input = "struct true 42 0b1010 MyType false 0o755 my_var 0xFF";
    let result = parse_program(input).unwrap();
    let reconstructed = format!("{}", result);

    let expected = "structtrue420b1010MyTypefalse0o755my_var0xff";

    assert_eq!(reconstructed, expected);
}

#[test]
fn test_integer_value_vs_format_preservation() {
    let inputs = ["255", "0b11111111", "0o377", "0xFF"];
    let expected_value = 255;

    for input in &inputs {
        let result = parse_program(input).unwrap();

        assert_eq!(result.items.len(), 1);
        match &result.items[0].kind {
            ItemKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Integer(integer) => {
                    assert_eq!(
                        integer.value, expected_value,
                        "Value should be {} for input '{}'",
                        expected_value, input
                    );

                    let display = format!("{}", integer);
                    let expected_display = if input == &"0xFF" { "0xff" } else { *input };
                    assert_eq!(
                        display, expected_display,
                        "Display format should be preserved for input '{}'",
                        input
                    );
                }
                _ => panic!("Expected integer in expression for input '{}'", input),
            },
            _ => panic!("Expected expression for input '{}'", input),
        }
    }
}
