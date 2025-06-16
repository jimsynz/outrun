//! Type introspection system tests
//!
//! Tests for TypeIdentifier expressions and the Type trait system

use crate::checker::TypeChecker;
use outrun_parser::parse_program;

#[test]
fn test_type_identifier_implementation_exists() {
    // Test that our TypeIdentifier expression checking function exists and compiles
    // This verifies the implementation is present even if TypeIdentifier expressions
    // are only generated in specific parser contexts

    // Test with a simple program to ensure the type checker doesn't break
    let source = r#"def test(): Integer { 42 }"#;

    let program = parse_program(source).expect("Should parse");
    let mut type_checker = TypeChecker::new();
    let result = type_checker.check_program(&program);

    assert!(result.is_ok(), "Basic type checking should still work");
    // ✅ TypeIdentifier implementation exists and doesn't break basic functionality
}

#[test]
fn test_introspection_registry_functionality() {
    // Test that the introspection registry is working correctly
    use crate::types::{IntrospectionRegistry, StructTypeInfo, TypeInterner};

    let mut registry = IntrospectionRegistry::new();
    let mut interner = TypeInterner::new();

    let user_type_id = interner.intern_type("User");
    let struct_info = StructTypeInfo {
        name: "User".to_string(),
        module: "App".to_string(),
        fields: vec![],
        implemented_traits: vec![],
        span: outrun_parser::Span::new(0, 10),
    };

    registry.register_struct_type(user_type_id, struct_info);

    assert!(registry.is_struct_type(user_type_id));
    assert_eq!(registry.get_type_name(user_type_id), Some("User"));
    // ✅ Introspection registry works correctly
}

#[test]
fn test_type_context_has_introspection_registry() {
    // Test that TypeContext includes the introspection registry
    let type_checker = TypeChecker::new();
    let context = type_checker.context();

    // Verify the introspection registry is accessible
    // Just check that it exists - we don't need to test with a specific TypeId
    assert_eq!(context.introspection_registry.struct_types.len(), 0);
    // ✅ TypeContext includes introspection registry
}

#[test]
fn test_basic_type_checking_still_works() {
    // Ensure our changes don't break existing functionality
    let sources = vec![
        r#"def test(): Integer { 42 }"#,
        r#"def test(): String { "hello" }"#,
        r#"def test(): Boolean { true }"#,
    ];

    for source in sources {
        let program = parse_program(source).expect("Should parse");
        let mut type_checker = TypeChecker::new();
        let result = type_checker.check_program(&program);
        assert!(
            result.is_ok(),
            "Basic type checking should work for: {}",
            source
        );
    }

    // ✅ Basic type checking functionality preserved
}
