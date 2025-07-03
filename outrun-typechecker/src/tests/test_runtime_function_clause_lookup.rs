//! Test that function clauses are accessible at runtime for dispatch

use crate::compilation::compiler_environment::CompilerEnvironment;

#[test]
fn test_runtime_function_clause_lookup() {
    // Test code with guarded divide function
    let test_code = r#"
trait DivideTrait {
    def divide(a: Self, b: Self): Self
}

struct Integer() {}

impl DivideTrait for Integer {
    def divide(a: Self, b: Self): Self {
        a
    }
    
    def divide(a: Self, b: Self): Self 
    when true {
        a
    }
}
"#;

    println!("=== RUNTIME FUNCTION CLAUSE LOOKUP TEST ===");

    // Parse and compile
    let parsed = outrun_parser::parse_program(test_code).expect("Parsing should succeed");
    let mut env = CompilerEnvironment::new();
    let compilation_result = env.compile_program(parsed).expect("Compilation should succeed");

    println!("✅ Compilation successful");

    // Check that function clauses are accessible in the compiler environment
    println!("\n=== CHECKING COMPILER ENVIRONMENT ===");
    let modules = env.modules().read().unwrap();
    println!("Module count: {}", modules.len());

    // Look for our test module
    let mut found_divide_clauses = false;
    for (module_key, module) in modules.iter() {
        let module_name = format!("{:?}", module_key);
        println!("Module: {}", module_name);
        
        if module_name.contains("DivideTrait") && module_name.contains("Integer") {
            println!("  Found target module!");
            println!("  Function clauses: {}", module.function_clauses.len());
            
            // Look for divide function clauses
            for (function_atom_id, clause_set) in &module.function_clauses {
                let function_name = env.resolve_atom_name(function_atom_id).unwrap_or("<unknown>".to_string());
                println!("    Function: {}", function_name);
                println!("    Clauses: {}", clause_set.clauses.len());
                
                if function_name == "divide" && clause_set.clauses.len() >= 2 {
                    found_divide_clauses = true;
                    println!("    ✅ Found divide function with {} clauses", clause_set.clauses.len());
                    
                    for (i, clause) in clause_set.clauses.iter().enumerate() {
                        println!("      Clause {}: {} (from_guard: {}, priority: {})", 
                            i, clause.clause_id, clause.from_guard, clause.priority);
                    }
                }
            }
        }
    }

    if found_divide_clauses {
        println!("\n✅ SUCCESS: Function clauses are accessible in compilation result!");
    } else {
        println!("\n❌ FAILURE: Function clauses not found in compilation result");
        
        // Debug output
        println!("\n=== DEBUG: All modules and their function clauses ===");
        for (module_key, module) in modules.iter() {
            if !module.function_clauses.is_empty() {
                println!("Module: {:?}", module_key);
                for (function_atom_id, clause_set) in &module.function_clauses {
                    let function_name = env.resolve_atom_name(function_atom_id).unwrap_or("<unknown>".to_string());
                    println!("  Function: {} ({} clauses)", function_name, clause_set.clauses.len());
                }
            }
        }
        
        panic!("Function clauses not accessible at runtime");
    }
}