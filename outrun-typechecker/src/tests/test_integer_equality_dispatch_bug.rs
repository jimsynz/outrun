//! Test for integer equality dispatch bug
//!
//! This test reproduces the specific bug where `1 == 1` evaluation
//! incorrectly resolves to Map equality implementation instead of
//! Integer64 equality implementation during runtime.

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::ProgramCollection;
use crate::core_library;
use outrun_parser::parse_program;

#[test]
fn test_integer_equality_dispatch_bug() {
    // Create a minimal program that should evaluate `1 == 1`
    let source = r#"
        def test(): Boolean {
            1 == 1
        }
    "#;

    // Parse the program
    let program = parse_program(source).expect("Failed to parse test program");

    // Create compiler environment and load core library
    let mut compiler_env = CompilerEnvironment::new();
    let _core_result = core_library::compile_core_library_with_environment(&mut compiler_env);

    // Create program collection and compile
    let mut collection = ProgramCollection::new();
    collection.add_program("test.outrun".to_string(), program, source.to_string());

    let compilation_result = compiler_env
        .compile_collection(collection)
        .expect("Failed to compile test program");

    // Verify the typed program compiled correctly
    let typed_program = compilation_result
        .typed_programs
        .get("test.outrun")
        .expect("No typed program found");

    assert_eq!(typed_program.items.len(), 1, "Should have one function");

    // Extract the function and its body
    let test_function = match &typed_program.items[0].kind {
        crate::checker::TypedItemKind::FunctionDefinition(func) => func,
        _ => panic!("Expected function definition"),
    };

    assert_eq!(test_function.name, "test");
    assert_eq!(test_function.body.statements.len(), 1);

    // Extract the equality expression
    let equality_expr = match &test_function.body.statements[0] {
        crate::checker::TypedStatement::Expression(expr) => expr,
        _ => panic!("Expected expression statement"),
    };

    // Verify it's a function call to Equality.equal?
    let (trait_name, function_name, dispatch_strategy, arguments) = match &equality_expr.kind {
        crate::checker::TypedExpressionKind::FunctionCall {
            function_path,
            arguments,
            dispatch_strategy,
            ..
        } => {
            let (trait_name, function_name) = match function_path {
                crate::checker::TypedFunctionPath::Qualified { module, name } => {
                    (module.clone(), name.clone())
                }
                _ => panic!("Expected qualified function path"),
            };
            (trait_name, function_name, dispatch_strategy, arguments)
        }
        _ => panic!("Expected function call expression"),
    };

    assert_eq!(trait_name, "Equality");
    assert_eq!(function_name, "equal?");
    assert_eq!(arguments.len(), 2);

    // Verify dispatch strategy points to Integer64 implementation
    match dispatch_strategy {
        crate::checker::DispatchMethod::Trait { impl_type, .. } => {
            // This should be Outrun.Core.Integer64, not Map
            let type_name = impl_type.to_string_representation();
            assert_eq!(
                type_name, "Outrun.Core.Integer64",
                "Dispatch should resolve to Integer64, not Map"
            );
        }
        _ => panic!("Expected trait dispatch method"),
    }

    // The typechecker part is working correctly - this verifies that the dispatch
    // strategy is set to Integer64, so the bug must be in the interpreter.
    println!("✅ Typechecker correctly resolved dispatch to Integer64 implementation");
    println!("📝 This test validates that the typechecker phase is working correctly.");
    println!("🐛 The actual bug occurs during interpreter evaluation phase.");
}
