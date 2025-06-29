//! Test for integer equality runtime dispatch bug
//!
//! This test reproduces the runtime bug where `1 == 1` evaluation
//! incorrectly dispatches to Map equality implementation instead of
//! Integer64 equality implementation.

use crate::{ExpressionEvaluator, InterpreterContext, Value};
use outrun_parser::parse_program;
use outrun_typechecker::{
    compilation::compiler_environment::CompilerEnvironment,
    compilation::program_collection::ProgramCollection, context::FunctionDispatchContext,
    core_library,
};

#[test]
fn test_integer_equality_runtime_dispatch_bug() {
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

    // Extract the equality expression from the compiled program
    let typed_program = compilation_result
        .typed_programs
        .get("test.outrun")
        .expect("No typed program found");

    let test_function = match &typed_program.items[0].kind {
        outrun_typechecker::checker::TypedItemKind::FunctionDefinition(func) => func,
        _ => panic!("Expected function definition"),
    };

    let equality_expr = match &test_function.body.statements[0] {
        outrun_typechecker::checker::TypedStatement::Expression(expr) => expr,
        _ => panic!("Expected expression statement"),
    };

    // Verify the dispatch strategy is correct (typechecker part works)
    match &equality_expr.kind {
        outrun_typechecker::checker::TypedExpressionKind::FunctionCall {
            dispatch_strategy,
            ..
        } => match dispatch_strategy {
            outrun_typechecker::checker::DispatchMethod::Trait { impl_type, .. } => {
                let type_name = impl_type.to_string_representation();
                assert_eq!(type_name, "Outrun.Core.Integer64");
                println!("✅ Typechecker dispatch strategy: {}", type_name);
            }
            _ => panic!("Expected trait dispatch method"),
        },
        _ => panic!("Expected function call expression"),
    }

    // Extract dispatch details for debugging
    let (trait_name, function_name, impl_type) = match &equality_expr.kind {
        outrun_typechecker::checker::TypedExpressionKind::FunctionCall {
            dispatch_strategy,
            ..
        } => match dispatch_strategy {
            outrun_typechecker::checker::DispatchMethod::Trait {
                trait_name,
                function_name,
                impl_type,
            } => (trait_name.clone(), function_name.clone(), impl_type.clone()),
            _ => panic!("Expected trait dispatch method"),
        },
        _ => panic!("Expected function call expression"),
    };

    println!("🔍 Debug dispatch details:");
    println!("  trait_name: {}", trait_name);
    println!("  function_name: {}", function_name);
    println!("  impl_type: {}", impl_type.to_string_representation());

    // Now test the interpreter evaluation (this is where the bug occurs)
    let dispatch_context = FunctionDispatchContext::new(Some(compiler_env.clone()));
    let mut evaluator = ExpressionEvaluator::from_dispatch_context(dispatch_context);
    let mut interpreter_context = InterpreterContext::new(
        compilation_result.type_context.clone(),
        compiler_env,
        Some(100),
    );

    // This should evaluate to Boolean(true) but currently fails with Map.size error
    let result = evaluator.evaluate(&mut interpreter_context, equality_expr);

    match result {
        Ok(Value::Boolean(true)) => {
            println!("✅ SUCCESS: Integer equality evaluated correctly: 1 == 1 → true");
            println!("🎉 The bug has been fixed!");
        }
        Ok(value) => {
            panic!(
                "❌ WRONG RESULT: 1 == 1 should return Boolean(true), got {:?}",
                value
            );
        }
        Err(error) => {
            // This is the bug we're reproducing
            let error_msg = format!("{:?}", error);
            if error_msg.contains("Map.size") {
                println!(
                    "🐛 BUG REPRODUCED: Integer equality incorrectly dispatched to Map implementation"
                );
                println!("📋 Error details: {}", error_msg);

                // This is expected for now - the test documents the bug
                // Once fixed, this should become a successful evaluation
                panic!("BUG: Integer equality evaluation dispatched to wrong implementation");
            } else {
                panic!(
                    "❌ UNEXPECTED ERROR: Different error than expected Map.size bug: {}",
                    error_msg
                );
            }
        }
    }
}
