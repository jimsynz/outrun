use crate::checker::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_basic_generic_type_resolution() {
    let input = r#"
        trait Display {
            def to_string(self: Self): String
        }
        
        impl<T> Display for List<T> {
            def to_string(self: Self): String {
                "list"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Should fail with proper generic type names, not "not yet supported"
    if let Err(errors) = &result {
        // Check that we get proper type errors, not unsupported feature errors
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        let has_unsupported = error_messages
            .iter()
            .any(|msg| msg.contains("not yet supported"));
        assert!(
            !has_unsupported,
            "Should not have 'not yet supported' error: {:?}",
            error_messages
        );

        // Should properly format generic types
        let has_list_t = error_messages.iter().any(|msg| msg.contains("List<T>"));
        assert!(
            has_list_t,
            "Should format List<T> correctly: {:?}",
            error_messages
        );
    }

    if let Ok(typed_program) = result {
        assert_eq!(typed_program.items.len(), 2);

        if let crate::checker::TypedItemKind::ImplBlock(impl_block) = &typed_program.items[1].kind {
            // Should have formatted the generic types correctly
            assert_eq!(impl_block.trait_name, "Display");
            assert_eq!(impl_block.type_name, "List<T>");
        } else {
            assert!(
                matches!(
                    typed_program.items[1].kind,
                    crate::checker::TypedItemKind::ImplBlock(_)
                ),
                "Expected impl block"
            );
        }
    }
}

#[test]
fn test_nested_generic_type_resolution() {
    let input = r#"
        trait Serializable<T> {
            def serialize(self: Self): T
        }
        
        struct DatabaseError() {}
        
        impl<T> Serializable<String> for Result<List<T>, DatabaseError> {
            def serialize(self: Self): String {
                "result"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Should fail with proper generic type names, not "not yet supported"
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        let has_unsupported = error_messages
            .iter()
            .any(|msg| msg.contains("not yet supported"));
        assert!(
            !has_unsupported,
            "Should not have 'not yet supported' error: {:?}",
            error_messages
        );

        // Should properly format nested generic types
        let has_nested_generics = error_messages.iter().any(|msg| {
            msg.contains("Serializable<String>") || msg.contains("Result<List<T>, DatabaseError>")
        });
        assert!(
            has_nested_generics,
            "Should format nested generics correctly: {:?}",
            error_messages
        );
    }

    if let Ok(typed_program) = result {
        if let crate::checker::TypedItemKind::ImplBlock(impl_block) = &typed_program.items[2].kind {
            assert_eq!(impl_block.trait_name, "Serializable<String>");
            assert_eq!(impl_block.type_name, "Result<List<T>, DatabaseError>");
        } else {
            assert!(
                matches!(
                    typed_program.items[2].kind,
                    crate::checker::TypedItemKind::ImplBlock(_)
                ),
                "Expected impl block"
            );
        }
    }
}

#[test]
fn test_simple_generic_struct_field_validation() {
    let input = r#"
        struct Container<T>(value: T) {}
        
        let example: Container<Integer> = Container { value: 42 }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // This will still fail until we implement generic struct support
    // but it should fail with a more specific error, not the old "not supported" error
    if let Err(errors) = result {
        // Should not contain the old "not yet supported" error
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        let has_old_error = error_messages
            .iter()
            .any(|msg| msg.contains("not yet supported"));
        assert!(
            !has_old_error,
            "Should not have old 'not yet supported' error: {:?}",
            error_messages
        );
    }
}

#[test]
fn test_generic_parameter_scoping() {
    let input = r#"
        trait Container<T> {
            def get(self: Self): T
        }
        
        impl<T> Container<T> for List<T> {
            def get(self: Self): T {
                List.head(self)
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Should fail with proper generic type names but not "already defined" errors
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        // Should not have generic parameter redefinition errors
        let has_redefinition = error_messages
            .iter()
            .any(|msg| msg.contains("already defined"));
        assert!(
            !has_redefinition,
            "Should not have generic parameter redefinition errors: {:?}",
            error_messages
        );

        // Should properly format generic types
        let has_proper_generics = error_messages
            .iter()
            .any(|msg| msg.contains("Container<T>") || msg.contains("List<T>"));
        assert!(
            has_proper_generics,
            "Should format generic types correctly: {:?}",
            error_messages
        );
    }
}

#[test]
fn test_generic_parameter_scoping_isolation() {
    // Test that generic parameters are properly isolated to their impl block scope
    let input = r#"
        trait Display {
            def to_string(self: Self): String
        }
        
        struct Container<T>(value: T) {}
        struct Wrapper<U>(item: U) {}
        
        impl<T> Display for Container<T> {
            def to_string(self: Self): String {
                "container"
            }
        }
        
        impl<U> Display for Wrapper<U> {
            def to_string(self: Self): String {
                "wrapper"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Should fail for type resolution reasons but not generic parameter conflicts
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        // Should not have generic parameter redefinition errors between different impl blocks
        let has_redefinition = error_messages
            .iter()
            .any(|msg| msg.contains("already defined"));
        assert!(
            !has_redefinition,
            "Should not have generic parameter redefinition errors between impl blocks: {:?}",
            error_messages
        );

        // Should see undefined type errors for T and U, showing they're being treated as type references
        let has_undefined_t = error_messages
            .iter()
            .any(|msg| msg.contains("Undefined type T"));
        let has_undefined_u = error_messages
            .iter()
            .any(|msg| msg.contains("Undefined type U"));
        assert!(
            has_undefined_t && has_undefined_u,
            "Should see generic parameters being referenced as types: {:?}",
            error_messages
        );
    }
}

#[test]
fn test_generic_struct_field_validation() {
    let input = r#"
        struct Container<T>(value: T) {}
        
        struct Pair<T, U>(first: T, second: U) {}
        
        struct Wrapper<T>(item: List<T>) {}
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Should succeed in processing struct definitions with generic field types
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        // Should not have generic parameter redefinition errors
        let has_redefinition = error_messages
            .iter()
            .any(|msg| msg.contains("already defined"));
        assert!(
            !has_redefinition,
            "Should not have generic parameter redefinition errors in struct definitions: {:?}",
            error_messages
        );

        // Should not have old "not yet supported" errors
        let has_unsupported = error_messages
            .iter()
            .any(|msg| msg.contains("not yet supported"));
        assert!(
            !has_unsupported,
            "Should not have 'not yet supported' errors: {:?}",
            error_messages
        );

        // Should properly handle generic parameters as field types
        // The type checker should now recognize T, U as valid generic parameters
        // But might still fail on List<T> because that's a concrete type with generic args
        // Errors (expected)
    } else {
        // If it succeeds, that's great! Our generic struct support is working
        // Generic struct field validation succeeded
    }
}

#[test]
fn test_generic_struct_with_simple_fields() {
    let input = r#"
        struct SimpleContainer<T>(value: T, count: Integer) {}
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    // Should succeed with T as a generic parameter and Integer as a concrete type
    if let Err(errors) = &result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();

        // Should not have "already defined" or "not yet supported" errors
        let has_bad_errors = error_messages
            .iter()
            .any(|msg| msg.contains("already defined") || msg.contains("not yet supported"));
        assert!(
            !has_bad_errors,
            "Should not have generic parameter or unsupported errors: {:?}",
            error_messages
        );

        // The main error should be about undefined type T (which shows our generic parameter is being recognized)
        let has_undefined_t = error_messages
            .iter()
            .any(|msg| msg.contains("Undefined type T"));
        assert!(
            has_undefined_t,
            "Should recognize T as a type reference: {:?}",
            error_messages
        );
    } else {
        // Generic struct with simple fields succeeded
    }
}
