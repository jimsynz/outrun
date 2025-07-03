//! Test to debug guard evaluation issues

use outrun_parser::parse_program;
use outrun_typechecker::compilation::{
    compiler_environment::CompilerEnvironment, program_collection::ProgramCollection,
};
use crate::{ExpressionEvaluator, InterpreterContext};
use outrun_typechecker::unification::UnificationContext;

#[test]
fn test_simple_guard_evaluation() {
    println!("=== SIMPLE GUARD EVALUATION TEST ===");
    
    // Test code with guarded function
    let test_code = r#"
def simple_divide(rhs: Integer): Integer
when rhs == 0 {
    42
}

def simple_divide(rhs: Integer): Integer {
    rhs
}

def test(): Integer {
    simple_divide(rhs: 0)
}
"#;

    println!("Test code:\n{}", test_code);

    // Parse the program
    let parsed = parse_program(test_code).expect("Parsing should succeed");
    println!("✅ Parsed successfully");

    // Create compiler environment and compile with core library
    let mut env = CompilerEnvironment::new();
    
    // Load core library first
    let core_collection = outrun_typechecker::core_library::load_core_library_collection();
    let _core_result = outrun_typechecker::core_library::load_and_compile_core_library_with_environment(&mut env, core_collection);
    println!("✅ Core library loaded");
    
    let mut collection = ProgramCollection::new();
    collection.add_program("main".to_string(), parsed, "".to_string());
    
    let _compilation_result = env.compile_collection_with_external_variables(collection, std::collections::HashMap::new());
    
    match _compilation_result {
        Ok(_) => {
            println!("✅ Compiled successfully");
            
            // Create interpreter context
            let unification_context = UnificationContext::new();
            let mut interpreter_context = InterpreterContext::new(
                unification_context,
                env.clone(),
                Some(1000),
            );
            
            // Create evaluator
            let mut evaluator = ExpressionEvaluator::new(env.clone());
            
            // Try to call the test function
            let test_atom = env.intern_atom_name("test");
            
            if let Some(function_entry) = env.lookup_local_function(test_atom) {
                if let Some(typed_function) = function_entry.typed_definition() {
                    println!("✅ Found test function");
                    
                    let function_executor = crate::function_executor::FunctionExecutor::new(env.clone());
                    let arguments = std::collections::HashMap::new();
                    
                    // Execute the function which should trigger guard evaluation
                    let result = function_executor.execute_typed_function(
                        &mut interpreter_context,
                        &mut evaluator,
                        typed_function,
                        arguments,
                        outrun_parser::Span::new(0, 0),
                    );
                    
                    match result {
                        Ok(value) => {
                            println!("✅ Function executed successfully: {:?}", value);
                        }
                        Err(e) => {
                            println!("❌ Function execution failed: {:?}", e);
                            
                            // Check if this is the type mismatch error we're looking for
                            let error_string = format!("{:?}", e);
                            if error_string.contains("TypeMismatch") && 
                               error_string.contains("expected") && 
                               error_string.contains("Atom") && 
                               error_string.contains("Integer64") {
                                println!("🎯 FOUND THE GUARD EVALUATION TYPE MISMATCH ERROR!");
                                println!("Error details: {}", e);
                            } else {
                                println!("Different error type: {}", error_string);
                            }
                        }
                    }
                } else {
                    println!("❌ No typed definition found for test function");
                }
            } else {
                println!("❌ Test function not found");
            }
            
        },
        Err(errors) => {
            println!("❌ Compilation failed");
            for error in &errors {
                println!("Error: {}", error);
            }
        }
    }
}