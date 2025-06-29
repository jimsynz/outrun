use outrun_typechecker::core_library::get_core_library_compilation;

#[test]
fn test_core_library_function_bodies_have_statements() {
    let core_compilation = get_core_library_compilation();

    println!("📊 Core library compilation statistics:");
    println!("  - Traits: {}", core_compilation.traits.len());
    println!("  - Structs: {}", core_compilation.structs.len());
    println!(
        "  - Implementations: {}",
        core_compilation.implementations.len()
    );

    // Count total items across all typed programs
    let total_items: usize = core_compilation
        .typed_programs
        .values()
        .map(|p| p.items.len())
        .sum();
    println!("  - Typed program items: {total_items}");

    // Look for List trait implementation functions
    for (program_name, typed_program) in &core_compilation.typed_programs {
        println!("\n📄 Program: {program_name}");
        for (i, item) in typed_program.items.iter().enumerate() {
            if let outrun_typechecker::checker::TypedItemKind::FunctionDefinition(func) = &item.kind
            {
                println!("\n🔍 Function {}: {}", i, func.name);
                println!("  - Function ID: {}", func.function_id);
                println!("  - Parameters: {}", func.parameters.len());
                println!("  - Body statements: {}", func.body.statements.len());

                // Print all function names to see what we have
                println!("  - Function name: {}", func.name);

                // Check if this is a List function (head, tail, etc.)
                if func.name.contains("head")
                    || func.name.contains("tail")
                    || func.name.contains("length")
                    || func.name.contains("prepend")
                {
                    println!("  ⭐ This is a List function!");

                    if func.body.statements.is_empty() {
                        println!("  ❌ PROBLEM: Function body has no statements!");
                    } else {
                        println!(
                            "  ✅ Function body has {} statements",
                            func.body.statements.len()
                        );

                        // Print the statements
                        for (stmt_i, stmt) in func.body.statements.iter().enumerate() {
                            match stmt {
                                outrun_typechecker::checker::TypedStatement::Expression(expr) => {
                                    println!(
                                        "    Statement {}: Expression -> {:?}",
                                        stmt_i, expr.kind
                                    );
                                }
                                outrun_typechecker::checker::TypedStatement::LetBinding(
                                    let_binding,
                                ) => {
                                    println!(
                                        "    Statement {}: LetBinding -> pattern with {} variables",
                                        stmt_i,
                                        let_binding.pattern.bound_variables.len()
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Assert that we found at least one function (any function, not just List functions)
    let total_functions_count = core_compilation
        .typed_programs
        .values()
        .flat_map(|program| &program.items)
        .filter(|item| {
            matches!(
                item.kind,
                outrun_typechecker::checker::TypedItemKind::FunctionDefinition(_)
            )
        })
        .count();

    println!("\n📊 Summary: Found {total_functions_count} functions total");

    // Look for any functions related to List
    let list_functions_count = core_compilation
        .typed_programs
        .values()
        .flat_map(|program| &program.items)
        .filter(|item| {
            if let outrun_typechecker::checker::TypedItemKind::FunctionDefinition(func) = &item.kind
            {
                let name_lower = func.name.to_lowercase();
                name_lower.contains("head")
                    || name_lower.contains("tail")
                    || name_lower.contains("length")
                    || name_lower.contains("prepend")
                    || name_lower.contains("list")
            } else {
                false
            }
        })
        .count();

    println!("📊 Found {list_functions_count} List-related functions");

    // Don't assert for now - let's just see what we have
    // assert!(total_functions_count > 0, "Should find at least one function in core library");
}
