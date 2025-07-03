//! Debug the parsing of impl blocks with multiple functions

#[test]
fn debug_impl_block_parsing() {
    // Test code with two divide functions
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

    println!("=== PARSING DEBUG ===");
    println!("Test code:\n{}", test_code);

    // Parse the code
    let parsed = outrun_parser::parse_program(test_code).expect("Parsing should succeed");
    println!("\n=== PARSING SUCCESSFUL ===");

    // Debug the parsed AST
    println!("\n=== PARSED AST ANALYSIS ===");
    println!("Total items in program: {}", parsed.items.len());
    
    for (i, item) in parsed.items.iter().enumerate() {
        println!("\nItem {}: {:?}", i, item.kind);
        
        match &item.kind {
            outrun_parser::ItemKind::ImplBlock(impl_block) => {
                println!("  Impl block found!");
                println!("  Trait: {:?}", impl_block.trait_spec);
                println!("  Type: {:?}", impl_block.type_spec);
                println!("  Number of functions: {}", impl_block.functions.len());
                
                for (j, func) in impl_block.functions.iter().enumerate() {
                    println!("    Function {}: {}", j, func.name.name);
                    println!("      Has guard: {}", func.guard.is_some());
                    if let Some(ref guard) = func.guard {
                        println!("      Guard: {:?}", guard);
                    }
                    println!("      Parameters: {}", func.parameters.len());
                    for param in &func.parameters {
                        println!("        Param: {}", param.name.name);
                    }
                }
            },
            outrun_parser::ItemKind::TraitDefinition(trait_def) => {
                println!("  Trait definition: {:?}", trait_def.name);
                println!("  Functions: {}", trait_def.functions.len());
            },
            outrun_parser::ItemKind::StructDefinition(struct_def) => {
                println!("  Struct definition: {:?}", struct_def.name);
            },
            _ => {
                println!("  Other item type");
            }
        }
    }
}