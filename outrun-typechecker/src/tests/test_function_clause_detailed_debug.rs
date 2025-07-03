//! Detailed debugging of function clause creation at each compilation phase

use crate::compilation::compiler_environment::CompilerEnvironment;

#[test]
fn debug_function_clause_creation_phases() {
    // Test code with two divide functions (one with guard, one without)
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

    println!("=== DETAILED FUNCTION CLAUSE DEBUGGING ===");
    println!("Test code:\n{}", test_code);

    // Parse the code
    let parsed = outrun_parser::parse_program(test_code).expect("Parsing should succeed");
    println!("\n=== PARSED SUCCESSFULLY ===");

    // Create compiler environment
    let mut env = CompilerEnvironment::new();
    
    println!("\n=== STARTING COMPILATION ===");
    
    // Let's manually debug the compilation phases by calling compile_collection_with_external_variables
    // and tracing the function registration
    let mut collection = crate::compilation::program_collection::ProgramCollection::new();
    collection.add_program("main".to_string(), parsed, "".to_string());
    
    let result = env.compile_collection_with_external_variables(collection, std::collections::HashMap::new());
    
    match result {
        Ok(_) => {
            println!("=== COMPILATION SUCCESSFUL ===");
            
            // Check the final state
            let modules = env.modules().read().unwrap();
            println!("\n=== FINAL MODULE STATE ===");
            
            for (module_key, module) in modules.iter() {
                if matches!(module_key, crate::compilation::compiler_environment::ModuleKey::TraitImpl(_, _)) {
                    println!("TraitImpl Module: {:?}", module_key);
                    println!("  Functions by name: {}", module.functions_by_name.len());
                    println!("  Function clauses: {}", module.function_clauses.len());
                    
                    // List functions
                    for (atom_id, entry) in &module.functions_by_name {
                        let function_name = env.resolve_atom_name(atom_id).unwrap_or("<unknown>".to_string());
                        println!("    Function: {}", function_name);
                        
                        if let crate::compilation::compiler_environment::UnifiedFunctionEntry::ImplFunction { 
                            definition, typed_definition, function_id, is_guard 
                        } = entry {
                            println!("      Function ID: {}", function_id);
                            println!("      Has guard in definition: {}", definition.guard.is_some());
                            println!("      Is guard marked: {}", is_guard);
                            println!("      Has typed definition: {}", typed_definition.is_some());
                        }
                    }
                    
                    // List clause sets
                    for (atom_id, clause_set) in &module.function_clauses {
                        let function_name = env.resolve_atom_name(atom_id).unwrap_or("<unknown>".to_string());
                        println!("    Clause set for: {}", function_name);
                        println!("      Total clauses: {}", clause_set.clauses.len());
                        
                        for (i, clause) in clause_set.clauses.iter().enumerate() {
                            println!("        Clause {}: {}", i, clause.clause_id);
                            println!("          Priority: {}", clause.priority);
                            println!("          From guard: {}", clause.from_guard);
                            println!("          Has guard in base function: {}", clause.base_function.guard.is_some());
                        }
                    }
                }
            }
            
            // Specific analysis for "divide" function
            println!("\n=== DIVIDE FUNCTION DETAILED ANALYSIS ===");
            let divide_atom = env.intern_atom_name("divide");
            
            for (module_key, module) in modules.iter() {
                if matches!(module_key, crate::compilation::compiler_environment::ModuleKey::TraitImpl(_, _)) {
                    if let Some(clause_set) = module.function_clauses.get(&divide_atom) {
                        println!("Found divide clause set in module: {:?}", module_key);
                        println!("  Clauses: {}", clause_set.clauses.len());
                        
                        // Check if we have both a guarded and non-guarded version
                        let guarded_clauses = clause_set.clauses.iter().filter(|c| c.from_guard).count();
                        let non_guarded_clauses = clause_set.clauses.iter().filter(|c| !c.from_guard).count();
                        
                        println!("  Guarded clauses: {}", guarded_clauses);
                        println!("  Non-guarded clauses: {}", non_guarded_clauses);
                        
                        if clause_set.clauses.len() < 2 {
                            println!("  ❌ MISSING CLAUSE! Expected 2 clauses for 'divide' function");
                        } else {
                            println!("  ✅ Both clauses found");
                        }
                    }
                }
            }
        },
        Err(errors) => {
            println!("=== COMPILATION FAILED ===");
            for error in &errors {
                println!("Error: {}", error);
            }
            panic!("Compilation failed");
        }
    }
}