//! Debug test to investigate literal AST transformation issues

use outrun_parser::parse_program;
use outrun_typechecker::{CompilationResult, Package};

#[test]
fn test_debug_literal_ast_transformation() {
    // Parse a simple integer literal
    let program = parse_program("42").expect("Failed to parse");
    
    println!("Original parsed program:");
    println!("{:#?}", program);
    
    // Run through typechecker
    let mut package = Package::new("debug_test".to_string());
    package.add_program(program);
    
    let compilation_result = CompilationResult::compile_package(&mut package).expect("Failed to typecheck");
    
    println!("\nTypechecked program:");  
    for program in &package.programs {
        println!("{:#?}", program);
    }
    
    // Check what kind of expression we have after typechecking
    if let Some(first_program) = package.programs.first() {
        if let Some(first_item) = first_program.items.first() {
            match &first_item.kind {
                outrun_parser::ItemKind::Expression(expr) => {
                    println!("\nFirst expression after typechecking:");
                    println!("Kind: {:?}", expr.kind);
                    println!("Span: {:?}", expr.span);
                    println!("Type info: {:?}", expr.type_info);
                }
                other => {
                    println!("\nUnexpected item kind: {:?}", other);
                }
            }
        }
    }
    
    println!("\nDispatch table has {} entries", compilation_result.dispatch_table.dispatch_map.len());
    println!("Universal dispatch has {} clauses", compilation_result.universal_dispatch.clauses.len());
}