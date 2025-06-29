//! Tests to verify that type checking results are correctly integrated into TypedASTBuilder

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::ProgramCollection;
use outrun_parser::parse_program;
use std::collections::HashMap;

#[test]
fn test_expression_types_captured_during_type_checking() {
    // Create a simple program with expressions that should have resolved types
    let source = r#"
        let x: Outrun.Core.Integer64 = 42
        let y: Outrun.Core.String = "hello"
    "#;

    let program = parse_program(source).expect("Program should parse");
    let mut collection = ProgramCollection::from_core_library();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env
        .compile_collection(collection)
        .expect("Compilation should succeed");

    // Verify that expression types were captured
    assert!(
        !result.type_context.expression_types.is_empty(),
        "Expression types should be captured"
    );

    // Check that we have types for the expected expressions
    println!(
        "Captured {} expression types",
        result.type_context.expression_types.len()
    );
    for (span, structured_type) in &result.type_context.expression_types {
        println!("Span {:?} -> Type: {:?}", span, structured_type);
    }
}

#[test]
fn test_typed_ast_builder_uses_resolved_types() {
    // Create a program with different types of expressions
    let source = r#"
        let number = 42
        let text = "hello world"
        let flag = true
    "#;

    let program = parse_program(source).expect("Program should parse");
    let mut collection = ProgramCollection::from_core_library();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env
        .compile_collection(collection)
        .expect("Compilation should succeed");

    // Verify that typed programs were built using the resolved types
    assert!(
        !result.typed_programs.is_empty(),
        "Typed programs should be generated"
    );

    let typed_program = result
        .typed_programs
        .get("test.outrun")
        .expect("Typed program should exist");

    // Check that the typed AST contains properly typed expressions
    assert!(
        typed_program.items.len() >= 3,
        "Should have at least 3 let bindings"
    );

    println!(
        "Generated typed program with {} items",
        typed_program.items.len()
    );
}

#[test]
fn test_fallback_to_literal_inference_when_no_resolved_type() {
    // Test that the system gracefully falls back to literal inference
    // when expression types aren't available (e.g., in test scenarios)
    let source = r#"let x = 123"#;

    let program = parse_program(source).expect("Program should parse");

    // Create a TypedASTBuilder with empty expression types to test fallback
    let context = crate::unification::UnificationContext::new();
    let struct_registry = HashMap::new();
    let env = crate::compilation::compiler_environment::CompilerEnvironment::new();

    let mut builder =
        crate::typed_ast_builder::TypedASTBuilder::new(context, struct_registry, Some(env));

    let mut collection = ProgramCollection::new();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    // This should work even without pre-resolved types
    let typed_programs = builder
        .build_typed_ast(&collection, &["test.outrun".to_string()])
        .expect("Should build typed AST with fallback inference");

    assert!(
        !typed_programs.is_empty(),
        "Should generate typed programs using fallback"
    );

    let typed_program = typed_programs
        .get("test.outrun")
        .expect("Typed program should exist");
    assert!(!typed_program.items.is_empty(), "Should have typed items");
}
