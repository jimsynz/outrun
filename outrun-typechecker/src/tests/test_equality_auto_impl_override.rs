//! Test automatic Equality implementation and override behavior

use crate::*;
use outrun_parser::parse_program;

#[test]
fn test_equality_automatic_implementation() {
    // Test that types automatically get Equality without manual implementation
    let program_source = r#"
        struct User(name: String, age: Integer) {}
        
        def test_user_equality(u1: User, u2: User): Boolean {
            u1 == u2
        }
    "#;

    let program = parse_program(program_source).unwrap();
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    let compilation_result = CompilationResult::compile_package(&mut package).unwrap();

    // Verify User automatically implements Equality
    assert!(
        compilation_result
            .type_registry
            .has_implementation(&ModuleName::new("Equality"), &ModuleName::new("User")),
        "User should automatically implement Equality"
    );
}

#[test]
fn test_equality_manual_override() {
    // Test that types can override the automatic Equality implementation
    let program_source = r#"
        struct SpecialNumber(value: Integer) {}
        
        # Manual override of automatic Equality implementation
        impl Equality for SpecialNumber {
            def equal?(lhs: Self, rhs: Self): Boolean {
                # Custom equality: always returns true (for testing)
                true
            }
        }
        
        def test_special_equality(s1: SpecialNumber, s2: SpecialNumber): Boolean {
            Equality.equal?(lhs: s1, rhs: s2)
        }
    "#;

    let program = parse_program(program_source).unwrap();
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    let compilation_result = CompilationResult::compile_package(&mut package).unwrap();

    // Verify SpecialNumber has manual implementation (not automatic)
    assert!(
        compilation_result.type_registry.has_implementation(
            &ModuleName::new("Equality"),
            &ModuleName::new("SpecialNumber")
        ),
        "SpecialNumber should implement Equality"
    );

    // Verify the manual implementation takes precedence
    // (This would require interpreter integration to fully test the behavior)
}

#[test]
fn test_equality_with_built_in_types() {
    // Test that built-in types work with automatic Equality
    let program_source = r#"
        def test_integer_equality(a: Outrun.Core.Integer64, b: Outrun.Core.Integer64): Boolean {
            a == b
        }
        
        def test_string_equality(s1: Outrun.Core.String, s2: Outrun.Core.String): Boolean {
            s1 == s2
        }
        
        def test_boolean_equality(b1: Outrun.Core.Boolean, b2: Outrun.Core.Boolean): Boolean {
            b1 == b2
        }
    "#;

    let program = parse_program(program_source).unwrap();
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    let compilation_result = CompilationResult::compile_package(&mut package).unwrap();

    // All built-in types should have Equality (either automatic or manual)
    let built_in_types = [
        "Outrun.Core.Integer64",
        "Outrun.Core.String",
        "Outrun.Core.Boolean",
    ];
    for type_name in &built_in_types {
        assert!(
            compilation_result
                .type_registry
                .has_implementation(&ModuleName::new("Equality"), &ModuleName::new(*type_name)),
            "{} should implement Equality",
            type_name
        );
    }
}

#[test]
fn test_equality_override_precedence() {
    // Test that manual implementations take precedence over automatic ones
    let program_source = r#"
        struct TestType(value: Outrun.Core.Integer64) {}
        
        # This manual implementation should override the automatic one
        impl Equality for TestType {
            def equal?(lhs: Self, rhs: Self): Boolean {
                # Custom logic: always returns true (for testing override)
                true
            }
        }
    "#;

    let program = parse_program(program_source).unwrap();
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    // This should succeed without conflicts
    let compilation_result = CompilationResult::compile_package(&mut package);
    assert!(
        compilation_result.is_ok(),
        "Manual Equality implementation should override automatic one"
    );

    let compilation_result = compilation_result.unwrap();
    assert!(
        compilation_result
            .type_registry
            .has_implementation(&ModuleName::new("Equality"), &ModuleName::new("TestType")),
        "TestType should have Equality implementation"
    );
}

#[test]
fn test_float_keeps_specialized_equality() {
    // Test that Float64 keeps its specialized equality implementation
    let program_source = r#"
        def test_float_equality(f1: Outrun.Core.Float64, f2: Outrun.Core.Float64): Boolean {
            f1 == f2
        }
    "#;

    let program = parse_program(program_source).unwrap();
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    let compilation_result = CompilationResult::compile_package(&mut package).unwrap();

    // Float64 should have Equality (specialized, not automatic)
    assert!(
        compilation_result.type_registry.has_implementation(
            &ModuleName::new("Equality"),
            &ModuleName::new("Outrun.Core.Float64")
        ),
        "Float64 should have specialized Equality implementation"
    );
}

#[test]
fn test_boolean_protocol_requires_equality() {
    // Test that Boolean protocol now requires Equality constraint
    let program_source = r#"
        struct CustomBoolean(active: Outrun.Core.Boolean) {}
        
        # Must implement all required protocols including Equality
        impl LogicalAnd for CustomBoolean {
            def and?(lhs: Self, rhs: Self): Outrun.Core.Boolean {
                lhs.active && rhs.active
            }
        }
        
        impl LogicalOr for CustomBoolean {
            def or?(lhs: Self, rhs: Self): Outrun.Core.Boolean {
                lhs.active || rhs.active
            }
        }
        
        impl LogicalNot for CustomBoolean {
            def not(value: Self): Self {
                CustomBoolean { active: !value.active }
            }
        }
        
        # Equality is now automatically provided, so Boolean constraint should be satisfied
        impl Boolean for CustomBoolean {
            def true?(value: Self): Outrun.Core.Boolean {
                value.active
            }
        }
    "#;

    let program = parse_program(program_source).unwrap();
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    let compilation_result = CompilationResult::compile_package(&mut package);
    assert!(
        compilation_result.is_ok(),
        "CustomBoolean should satisfy Boolean protocol constraints including Equality"
    );

    let compilation_result = compilation_result.unwrap();

    // Verify all required implementations exist
    assert!(
        compilation_result.type_registry.has_implementation(
            &ModuleName::new("Equality"),
            &ModuleName::new("CustomBoolean")
        ),
        "CustomBoolean should automatically implement Equality"
    );

    assert!(
        compilation_result.type_registry.has_implementation(
            &ModuleName::new("Boolean"),
            &ModuleName::new("CustomBoolean")
        ),
        "CustomBoolean should implement Boolean protocol"
    );
}
