//! Test constraint validation for impl block constraint expressions
//!
//! Tests that constrained type variables must appear in the impl type specifications
//! and that Self constraints are properly supported.

use crate::typecheck_program;
use outrun_parser::parse_program;

#[test]
fn test_valid_constraint_on_protocol_type_variable() {
    // Valid: T appears in Display<T> 
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        struct Wrapper<U>(value: U) {}
        
        impl Display<T> for Wrapper<U> when T: Debug {
            def display(value: T): String {
                "debug representation"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should succeed - T appears in Display<T>
    let result = typecheck_program(&mut program);
    assert!(result.is_ok(), "Constraint validation should succeed when T appears in Display<T>: {:?}", result.err());
}

#[test]
fn test_valid_constraint_on_implementing_type_variable() {
    
    // Valid: U appears in Wrapper<U>
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        struct Wrapper<U>(value: U) {}
        
        impl Display<T> for Wrapper<U> when U: Debug {
            def display(value: T): String {
                "display"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should succeed - U appears in Wrapper<U>
    let result = typecheck_program(&mut program);
    assert!(result.is_ok(), "Constraint validation should succeed when U appears in Wrapper<U>: {:?}", result.err());
}

#[test]
fn test_valid_constraint_on_both_type_variables() {
    
    // Valid: T appears in Display<T>, U appears in Wrapper<U>
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        protocol Clone {
            def clone(value: Self): Boolean {
                true
            }
        }
        
        struct Wrapper<U>(value: U) {}
        
        impl Display<T> for Wrapper<U> when T: Debug && U: Clone {
            def display(value: T): String {
                "display"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should succeed - both T and U appear in type specifications
    let result = typecheck_program(&mut program);
    assert!(result.is_ok(), "Constraint validation should succeed when both T and U appear in type specs: {:?}", result.err());
}

#[test]
fn test_valid_self_constraint() {
    
    // Valid: Self constraints are always allowed
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        struct Wrapper<U>(value: U) {}
        
        impl Display<T> for Wrapper<U> when Self: Debug {
            def display(value: T): String {
                "display"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should succeed - Self constraints are always valid
    let result = typecheck_program(&mut program);
    assert!(result.is_ok(), "Constraint validation should succeed for Self constraints: {:?}", result.err());
}

#[test]
fn test_invalid_constraint_variable_not_in_types() {
    
    // Invalid: V doesn't appear in Display<T> or Wrapper<U>
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        struct Wrapper<U>(value: U) {}
        
        impl Display<T> for Wrapper<U> when V: Debug {
            def display(value: T): String {
                "display"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should fail - V doesn't appear in type specifications
    let result = typecheck_program(&mut program);
    assert!(result.is_err(), "Constraint validation should fail when V doesn't appear in type specifications");
    
    let error_message = format!("{:?}", result.err().unwrap());
    assert!(error_message.contains("V"), "Error should mention the invalid variable V");
    assert!(error_message.contains("InvalidConstraintVariable"), "Error should be InvalidConstraintVariable type");
    assert!(error_message.contains("Constrained type variables must appear in the impl type specifications"), "Error should explain the constraint validation rule");
}

#[test]
fn test_invalid_constraint_variable_in_complex_expression() {
    
    // Invalid: X doesn't appear in Display<T> or Wrapper<U>
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        protocol Clone {
            def clone(value: Self): Boolean {
                true
            }
        }
        
        struct Wrapper<U>(value: U) {}
        
        impl Display<T> for Wrapper<U> when T: Debug && U: Clone && X: Debug {
            def display(value: T): String {
                "display"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should fail - X doesn't appear in type specifications
    let result = typecheck_program(&mut program);
    assert!(result.is_err(), "Constraint validation should fail when X doesn't appear in type specifications");
    
    let error_message = format!("{:?}", result.err().unwrap());
    assert!(error_message.contains("X"), "Error should mention the invalid variable X");
}

#[test]
fn test_invalid_constraint_variable_with_no_type_parameters() {
    
    // Invalid: No type variables in impl, but Z is constrained
    let program_text = r#"
        protocol Display {
            def display(value: Self): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        struct Wrapper(value: String) {}
        
        impl Display for Wrapper when Z: Debug {
            def display(value: Self): String {
                "display"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should fail - no type variables available, but Z is constrained
    let result = typecheck_program(&mut program);
    assert!(result.is_err(), "Constraint validation should fail when no type variables are available");
    
    let error_message = format!("{:?}", result.err().unwrap());
    assert!(error_message.contains("Z"), "Error should mention the invalid variable Z");
    assert!(error_message.contains("none"), "Error should indicate no type variables are available");
}

#[test]
fn test_valid_mixed_constraints_with_self() {
    
    // Valid: Mix of valid type variables and Self
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        protocol Clone {
            def clone(value: Self): Boolean {
                true
            }
        }
        
        struct Wrapper<U>(value: U) {}
        
        impl Display<T> for Wrapper<U> when T: Debug && Self: Clone && U: Debug {
            def display(value: T): String {
                "display"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should succeed - T and U appear in types, Self is always valid
    let result = typecheck_program(&mut program);
    assert!(result.is_ok(), "Constraint validation should succeed with mixed valid constraints: {:?}", result.err());
}

#[test]
fn test_parenthesized_constraint_validation() {
    
    // Invalid: Y in parentheses doesn't appear in type specifications
    let program_text = r#"
        protocol Display<T> {
            def display(value: T): String {
                "display"
            }
        }
        
        protocol Debug {
            def debug(value: Self): String {
                "debug"
            }
        }
        
        struct Wrapper<U>(value: U) {}
        
        impl Display<T> for Wrapper<U> when (T: Debug && Y: Debug) {
            def display(value: T): String {
                "display"
            }
        }
    "#;
    
    let mut program = parse_program(program_text).expect("Parse should succeed");
    
    // Should fail - Y doesn't appear in type specifications (even in parentheses)
    let result = typecheck_program(&mut program);
    assert!(result.is_err(), "Constraint validation should fail for invalid variables in parentheses");
    
    let error_message = format!("{:?}", result.err().unwrap());
    assert!(error_message.contains("Y"), "Error should mention the invalid variable Y");
}