//! Debug tests for function clause set creation during compilation

use crate::compilation::compiler_environment::CompilerEnvironment;

#[test]
fn debug_function_clause_creation_during_compilation() {
    // Test code with guarded divide function
    let test_code = r#"
trait DivideTrait {
    def divide(a: Self, b: Self): Self
    def non_zero?(value: Self): Boolean
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
    
    def non_zero?(value: Self): Boolean {
        true
    }
}
"#;

    println!("=== DEBUGGING FUNCTION CLAUSE CREATION ===");
    println!("Test code:\n{}", test_code);

    // Parse the code
    let parsed = outrun_parser::parse_program(test_code).expect("Parsing should succeed");
    println!("\n=== PARSED SUCCESSFULLY ===");

    // Create compiler environment
    let mut env = CompilerEnvironment::new();
    
    // Compile the program
    println!("\n=== STARTING COMPILATION ===");
    let compilation_result = env.compile_program(parsed);
    
    match compilation_result {
        Ok(result) => {
            println!("=== COMPILATION SUCCESSFUL ===");
            
            // Debug: Print all modules and their function clause sets
            let modules = env.modules().read().unwrap();
            println!("\n=== MODULE ANALYSIS ===");
            println!("Total modules: {}", modules.len());
            
            for (module_key, module) in modules.iter() {
                println!("\nModule: {:?}", module_key);
                println!("  Functions by name: {}", module.functions_by_name.len());
                println!("  Function clauses: {}", module.function_clauses.len());
                
                // List all functions by name
                for (atom_id, entry) in &module.functions_by_name {
                    let function_name = env.resolve_atom_name(atom_id).unwrap_or("<unknown>".to_string());
                    println!("    Function: {} (hash: {})", function_name, atom_id.hash);
                    
                    match entry {
                        crate::compilation::compiler_environment::UnifiedFunctionEntry::ImplFunction { 
                            definition, typed_definition, function_id, is_guard 
                        } => {
                            println!("      Type: ImplFunction");
                            println!("      Function ID: {}", function_id);
                            println!("      Is guard: {}", is_guard);
                            println!("      Has guard clause: {}", definition.guard.is_some());
                            println!("      Typed definition: {}", typed_definition.is_some());
                        },
                        _ => {
                            println!("      Type: Other");
                        }
                    }
                }
                
                // List all function clause sets
                for (atom_id, clause_set) in &module.function_clauses {
                    let function_name = env.resolve_atom_name(atom_id).unwrap_or("<unknown>".to_string());
                    
                    println!("    Function clause set: {}", function_name);
                    println!("      Clauses: {}", clause_set.clauses.len());
                    
                    for (i, clause) in clause_set.clauses.iter().enumerate() {
                        println!("        Clause {}: {}", i, clause.clause_id);
                        println!("          Priority: {}", clause.priority);
                        println!("          From guard: {}", clause.from_guard);
                        println!("          Has guard in def: {}", clause.base_function.guard.is_some());
                    }
                }
            }
            
            // Specific check for "divide" function
            println!("\n=== DIVIDE FUNCTION ANALYSIS ===");
            let divide_atom = env.intern_atom_name("divide");
            println!("Divide atom hash: {}", divide_atom.hash);
            
            let mut found_divide_clauses = false;
            for (module_key, module) in modules.iter() {
                if let Some(clause_set) = module.function_clauses.get(&divide_atom) {
                    found_divide_clauses = true;
                    println!("Found divide clause set in module: {:?}", module_key);
                    println!("  Number of clauses: {}", clause_set.clauses.len());
                    
                    for (i, clause) in clause_set.clauses.iter().enumerate() {
                        println!("    Clause {}: {}", i, clause.clause_id);
                        println!("      From guard: {}", clause.from_guard);
                        println!("      Priority: {}", clause.priority);
                    }
                }
                
                if let Some(function_entry) = module.functions_by_name.get(&divide_atom) {
                    println!("Found divide function entry in module: {:?}", module_key);
                }
            }
            
            if !found_divide_clauses {
                println!("❌ NO DIVIDE FUNCTION CLAUSE SETS FOUND!");
                
                // Debug: Let's see what functions DO exist
                println!("\n=== ALL FUNCTION NAMES IN ALL MODULES ===");
                for (module_key, module) in modules.iter() {
                    println!("Module: {:?}", module_key);
                    for (atom_id, _) in &module.functions_by_name {
                        let name = env.resolve_atom_name(atom_id).unwrap_or("<unknown>".to_string());
                        println!("  Function: {} (hash: {})", name, atom_id.hash);
                    }
                }
            } else {
                println!("✅ DIVIDE FUNCTION CLAUSE SETS FOUND!");
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

#[test]
fn debug_function_registration_process() {
    // Minimal test to see the function registration process
    let test_code = r#"
trait TestTrait {
    def divide(a: Self, b: Self): Self
}

struct Integer() {}

impl TestTrait for Integer {
    def divide(a: Self, b: Self): Self {
        a
    }
}
"#;

    println!("=== DEBUGGING SINGLE FUNCTION REGISTRATION ===");
    
    let parsed = outrun_parser::parse_program(test_code).expect("Parsing should succeed");
    let mut env = CompilerEnvironment::new();
    
    // Step by step compilation debugging
    println!("Creating compiler environment...");
    
    println!("Starting compilation...");
    let result = env.compile_program(parsed);
    
    match result {
        Ok(_) => {
            println!("Single function compilation successful");
            
            let modules = env.modules().read().unwrap();
            println!("Total modules: {}", modules.len());
            
            for (module_key, module) in modules.iter() {
                println!("Module: {:?}", module_key);
                println!("  Functions by name: {}", module.functions_by_name.len());
                for (atom_id, _) in &module.functions_by_name {
                    let name = env.resolve_atom_name(atom_id).unwrap_or("<unknown>".to_string());
                    println!("    Function: {}", name);
                }
            }
        },
        Err(errors) => {
            println!("Single function compilation failed:");
            for error in &errors {
                println!("  Error: {}", error);
            }
        }
    }
}