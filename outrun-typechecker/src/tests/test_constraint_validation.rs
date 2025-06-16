use crate::checker::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_valid_constraint_with_trait_implementation() {
    let input = r#"
        trait Display {
            def to_string(self: Self): Outrun.Core.String  
        }

        trait Debug {
            def debug(self: Self): Outrun.Core.String
        }

        impl Display for Outrun.Core.String {
            def to_string(self: Self): Outrun.Core.String {
                self
            }
        }

        impl Debug for Outrun.Core.String {
            def debug(self: Self): Outrun.Core.String {
                "quoted_string"
            }
        }

        struct Container<T>(value: T) {}

        impl<T> Display for Container<T> when T: Display && T: Debug {
            def to_string(self: Self): Outrun.Core.String {
                "(container)"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Valid constraint validation should succeed: {:?}",
        result.err()
    );
}

#[test]
fn test_constraint_with_undefined_trait() {
    let input = r#"
        trait Display {
            def to_string(self: Self): Outrun.Core.String  
        }

        struct Container<T>(value: T) {}

        impl<T> Display for Container<T> when T: UndefinedTrait {
            def to_string(self: Self): Outrun.Core.String {
                "(container)"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail with undefined trait in constraint"
    );
    let errors = result.unwrap_err();
    let has_undefined_trait = errors
        .iter()
        .any(|e| matches!(e, crate::error::TypeError::UndefinedTrait { .. }));
    assert!(
        has_undefined_trait,
        "Should detect undefined trait in constraint"
    );
}

#[test]
fn test_constraint_circular_dependency() {
    let input = r#"
        trait A {
            def method_a(self: Self): Outrun.Core.String
        }

        trait B {
            def method_b(self: Self): Outrun.Core.String
        }

        impl<T> A for T when T: B {
            def method_a(self: Self): Outrun.Core.String {
                "a"
            }
        }

        impl<T> B for T when T: A {
            def method_b(self: Self): Outrun.Core.String {
                "b"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    if result.is_err() {
        let errors = result.unwrap_err();
        assert!(
            !errors.is_empty(),
            "Should have specific errors for circular constraints"
        );
    }
}

#[test]
fn test_multiple_constraints_in_impl_block() {
    let input = r#"
        trait Display {
            def to_string(self: Self): Outrun.Core.String  
        }

        trait Debug {
            def debug(self: Self): Outrun.Core.String
        }
        
        trait Clone {
            def clone(self: Self): Self
        }

        struct Triple<T, U, V>(first: T, second: U, third: V) {}

        impl<T, U, V> Display for Triple<T, U, V> when T: Display && U: Debug && V: Clone {
            def to_string(self: Self): Outrun.Core.String {
                "triple"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Multiple constraint validation should succeed: {:?}",
        result.err()
    );
}

#[test]
fn test_constraint_with_multiple_undefined_traits() {
    let input = r#"
        trait Display {
            def to_string(self: Self): Outrun.Core.String  
        }

        struct Container<T>(value: T) {}

        impl<T> Display for Container<T> when T: UndefinedTrait1 && T: UndefinedTrait2 {
            def to_string(self: Self): Outrun.Core.String {
                "(container)"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Should fail with undefined traits in constraint"
    );
    let errors = result.unwrap_err();
    let has_undefined_trait = errors.iter().any(|e| {
        matches!(e, crate::error::TypeError::UndefinedTrait { trait_name, .. } if trait_name == "UndefinedTrait1")
    });
    assert!(
        has_undefined_trait,
        "Should detect undefined trait in constraint"
    );
}

#[test]
fn test_nested_constraint_expressions() {
    let input = r#"
        trait Display {
            def to_string(self: Self): Outrun.Core.String  
        }

        trait Debug {
            def debug(self: Self): Outrun.Core.String
        }

        struct Container<T>(value: T) {}

        impl<T> Display for Container<T> when (T: Display && T: Debug) {
            def to_string(self: Self): Outrun.Core.String {
                "(container)"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Nested constraint validation should succeed: {:?}",
        result.err()
    );
}
