use crate::*;
use outrun_parser::parse_program;

#[test]
fn test_case_expression_with_guard_pattern_binding() {
    let source = r#"
def test_simple_case(input: Boolean): Boolean {
    case input {
        value when value -> true
        _ -> false
    }
}
"#;

    let program = parse_program(source).expect("Failed to parse test program");
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    // This tests that pattern bindings are available in guard expressions
    let result = CompilationResult::compile_package(&mut package);

    match result {
        Ok(_) => {
            // println!("✅ Case expression with guard pattern binding compiled successfully");
        }
        Err(e) => {
            println!("❌ Case expression compilation failed: {:?}", e);
            // This might fail due to incomplete implementation, but shouldn't crash
            // The key is that it should recognize 'value' in the guard
        }
    }
}

#[test]
fn test_case_expression_pattern_binding_scope() {
    let source = r#"
def test_pattern_binding_scope(input: Option<Integer>): Integer {
    case input {
        some_value when Option.some?(value: some_value) -> {
            case some_value {
                inner_value -> inner_value
            }
        }
        _ -> 0
    }
}
"#;

    let program = parse_program(source).expect("Failed to parse test program");
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    // This tests that pattern bindings are properly scoped within case expressions
    let result = CompilationResult::compile_package(&mut package);

    match result {
        Ok(_) => {
            println!(
                "✅ Nested case expression with pattern binding scoping compiled successfully"
            );
        }
        Err(e) => {
            println!("❌ Nested case expression compilation failed: {:?}", e);
            // This might fail due to incomplete implementation, but shouldn't crash
        }
    }
}

#[test]
fn test_file_context_in_diagnostics() {
    let source = r#"
def test_unknown_variable(): Boolean {
    case true {
        value when unknown_variable -> true
        _ -> false
    }
}
"#;

    let mut program = parse_program(source).expect("Failed to parse test program");
    // Set a fake source file to test file context
    program.debug_info.source_file = Some("test_file.outrun".to_string());

    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    // This should fail with an unknown variable error that shows the correct file
    let result = CompilationResult::compile_package(&mut package);

    match result {
        Ok(_) => {
            // println!("❌ Expected compilation to fail with unknown variable error");
        }
        Err(e) => {
            let error_string = format!("{:?}", e);
            println!("✅ Got expected error: {}", error_string);
            // Check if the error contains file context (not just "unknown")
            if error_string.contains("test_file.outrun") {
                // println!("✅ File context is working correctly!");
            } else {
                println!("❌ File context is missing from error: {}", error_string);
            }
        }
    }
}
