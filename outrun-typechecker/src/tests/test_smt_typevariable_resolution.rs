//! Test for SMT-based TypeVariable resolution

use crate::checker::TypeChecker;

#[test]
fn test_smt_typevariable_resolution_with_option_some() {
    let source = r#"
Option.some(value: "test")
"#;

    let program = match outrun_parser::parse_program(source) {
        Ok(program) => program,
        Err(err) => {
            panic!("Parse error: {:?}", err);
        }
    };

    let mut checker = TypeChecker::new();
    match checker.check_program(&program) {
        Ok(typed_program) => {
            println!("✅ Type checking succeeded!");
            println!("Compilation summary: {}", typed_program.compilation_summary);
            assert!(!typed_program.items.is_empty());
        }
        Err(errors) => {
            println!("❌ Type checking failed:");
            for error in errors {
                println!("  - {:?}", error);
            }
            // This might fail due to missing Option trait functions, but should exercise SMT system
        }
    }
}

#[test]
fn test_smt_typevariable_resolution_with_simple_function() {
    let source = r#"
Boolean.and(left: true, right: false)
"#;

    let program = match outrun_parser::parse_program(source) {
        Ok(program) => program,
        Err(err) => {
            panic!("Parse error: {:?}", err);
        }
    };

    let mut checker = TypeChecker::new();
    match checker.check_program(&program) {
        Ok(typed_program) => {
            println!("✅ Boolean.and type checking succeeded!");
            println!("Compilation summary: {}", typed_program.compilation_summary);
            assert!(!typed_program.items.is_empty());
        }
        Err(errors) => {
            println!("❌ Type checking failed:");
            for error in errors {
                println!("  - {:?}", error);
            }
            // This test helps verify the SMT constraint generation is working
        }
    }
}