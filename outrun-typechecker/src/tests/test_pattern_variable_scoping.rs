use crate::typecheck_program;
use outrun_parser::parse_program;

// Simple test without core library to isolate the variable scoping issue
#[test]
fn test_let_binding_identifier_pattern_simple() {
    let source = r#"
protocol MyProtocol {
    def test(): MyProtocol
}

struct MyStruct {}

impl MyProtocol for MyStruct {
    def test(): MyProtocol {
        let x = MyStruct {}
        x
    }
}
"#;

    let program = parse_program(source).unwrap();
    let result = typecheck_program(program);

    // This test should pass if variable scoping works correctly
    // If it fails with "UndefinedVariable: x", then we know the pattern extraction is broken
    match result {
        Ok(_) => {
            println!("✓ Pattern variable extraction works correctly");
        }
        Err(errors) => {
            let error_messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
            let has_undefined_variable_x = error_messages
                .iter()
                .any(|msg| msg.contains("Undefined variable") && msg.contains("x"));

            if has_undefined_variable_x {
                panic!("Variable 'x' from let pattern not registered in scope: {errors:?}");
            } else {
                // Other errors are OK for this test - we just care about variable scoping
                println!("Other errors found (not variable scoping): {errors:?}");
            }
        }
    }
}
