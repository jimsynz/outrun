//! Test for not_equal runtime dispatch fix
//!
//! This test verifies that the `!=` operator works correctly after fixing
//! the issue where function calls within default implementations were
//! incorrectly dispatched as static functions instead of trait functions.

use crate::{ExpressionEvaluator, InterpreterContext, Value};
use outrun_parser::parse_program;
use outrun_typechecker::{
    compilation::compiler_environment::CompilerEnvironment,
    compilation::program_collection::ProgramCollection, context::FunctionDispatchContext,
    core_library,
};

#[test]
fn test_not_equal_runtime_dispatch_fix() {
    // Create a program that uses the != operator
    let source = r#"
        def test(): Boolean {
            1 != 2
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

    // Extract the not_equal expression from the compiled program
    let typed_program = compilation_result
        .typed_programs
        .get("test.outrun")
        .expect("No typed program found");

    let test_function = match &typed_program.items[0].kind {
        outrun_typechecker::checker::TypedItemKind::FunctionDefinition(func) => func,
        _ => panic!("Expected function definition"),
    };

    let not_equal_expr = match &test_function.body.statements[0] {
        outrun_typechecker::checker::TypedStatement::Expression(expr) => expr,
        _ => panic!("Expected expression statement"),
    };

    // Verify the dispatch strategy is correct (trait dispatch, not static)
    match &not_equal_expr.kind {
        outrun_typechecker::checker::TypedExpressionKind::FunctionCall {
            dispatch_strategy,
            ..
        } => match dispatch_strategy {
            outrun_typechecker::checker::DispatchMethod::Trait { impl_type, .. } => {
                let type_name = impl_type.to_string_representation();
                assert_eq!(type_name, "Outrun.Core.Integer64");
                println!("✅ Typechecker dispatch strategy: {type_name}");
            }
            _ => panic!("Expected trait dispatch method"),
        },
        _ => panic!("Expected function call expression"),
    }

    // Now test the interpreter evaluation
    let dispatch_context = FunctionDispatchContext::new(Some(compiler_env.clone()));
    let mut evaluator = ExpressionEvaluator::from_dispatch_context(dispatch_context);
    let mut interpreter_context = InterpreterContext::new(
        compilation_result.type_context.clone(),
        compiler_env,
        Some(100),
    );

    // This should evaluate to Boolean(true) since 1 != 2
    let result = evaluator.evaluate(&mut interpreter_context, not_equal_expr);

    match result {
        Ok(Value::Boolean(true)) => {
            println!("✅ SUCCESS: Not-equal evaluation works correctly: 1 != 2 → true");
            println!("🎉 The dispatch fix is working properly!");
        }
        Ok(value) => {
            panic!("❌ WRONG RESULT: 1 != 2 should return Boolean(true), got {value:?}");
        }
        Err(error) => {
            panic!("❌ RUNTIME ERROR: {error:?}");
        }
    }
}