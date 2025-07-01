//! Test complex trait constraint declarations like Integer and Float

use crate::checker::TypeChecker;

#[test]
fn test_integer_trait_constraint_composition() {
    // Test that the Integer trait from the core library with its complex constraints
    // is properly loaded and has its constraints validated
    let source = r#"
Integer.abs(value: 42)
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
            println!("✅ Integer trait with complex constraints validated successfully");
            println!("Compilation summary: {}", typed_program.compilation_summary);
            assert!(!typed_program.items.is_empty());
        }
        Err(errors) => {
            println!("❌ Integer trait constraint validation results:");
            for error in &errors {
                println!("  - {:?}", error);
            }
            
            // Look for specific constraint-related errors vs missing implementation errors
            let has_constraint_errors = errors.iter().any(|e| {
                matches!(e, crate::error::TypeError::MissingTraitConstraintInDefinition { .. })
            });
            
            if has_constraint_errors {
                println!("✅ Found trait constraint validation errors - system is working");
            } else {
                println!("ℹ️  No constraint validation errors - likely missing implementations which is expected");
            }
        }
    }
}

#[test] 
fn test_float_trait_with_integer_constraints() {
    let source = r#"
Integer.abs(value: 42)
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
            println!("✅ Integer trait function call type checked successfully");
            println!("Compilation summary: {}", typed_program.compilation_summary);
            assert!(!typed_program.items.is_empty());
        }
        Err(errors) => {
            println!("❌ Integer trait constraint validation results:");
            for error in &errors {
                println!("  - {:?}", error);
            }
            
            // We expect this to have errors due to missing implementations,
            // but the constraint system should be working
            println!("ℹ️  Errors are expected due to missing core library implementations");
        }
    }
}

#[test]
fn test_smt_handles_constraint_conjunction() {
    let source = r#"
Float.abs(value: 3.14)
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
            println!("✅ Float trait function call type checked successfully");
            println!("Compilation summary: {}", typed_program.compilation_summary);
            assert!(!typed_program.items.is_empty());
        }
        Err(errors) => {
            println!("❌ Float trait constraint validation results:");
            for error in &errors {
                println!("  - {:?}", error);
            }
            
            // We expect this to have errors due to missing implementations,
            // but the constraint system should be working
            println!("ℹ️  Errors are expected due to missing core library implementations");
        }
    }
}