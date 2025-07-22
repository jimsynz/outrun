use outrun_typechecker::core_library::get_core_library_compilation;
use outrun_typechecker::unification::StructuredType;

#[test]
fn test_protocol_implementation_lookup_investigation() {
    let core_compilation = get_core_library_compilation();

    println!("📊 Investigating protocol implementation storage and lookup");

    // Create a temporary compiler environment to test lookup
    let mut compiler_env =
        outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();

    // Load the core library compilation into the compiler environment
    compiler_env.load_compilation_result(core_compilation.clone());

    println!("✅ Successfully loaded core library into compiler environment");

    // Debug: List all modules and their contents to see what's actually stored
    println!("\n🔍 Debugging module storage:");
    // Note: This would require access to internal module structure

    // Test specific List protocol implementation lookup
    let list_protocol_name = "List";
    let list_type_name = "Outrun.Core.List";
    let head_function_name = "head";

    // Intern the necessary identifiers
    let protocol_type_id = compiler_env.intern_type_name(list_protocol_name);
    let impl_type_id = compiler_env.intern_type_name(list_type_name);
    let function_name_atom = compiler_env.intern_atom_name(head_function_name);

    println!("\n🔍 Testing protocol implementation lookup:");
    println!("  - Protocol: {list_protocol_name} (id: {protocol_type_id})");
    println!("  - Implementation type: {list_type_name} (id: {impl_type_id})");
    println!("  - Function: {head_function_name} (atom: {function_name_atom})");

    // Create protocol and impl types for lookup
    let protocol_type = StructuredType::Simple(protocol_type_id);
    let impl_type = StructuredType::Generic {
        base: impl_type_id.clone(),
        args: vec![
            // Use a generic parameter for T
            StructuredType::Simple(compiler_env.intern_type_name("T")),
        ],
    };

    println!("\n  - Protocol type for lookup: {protocol_type:?}");
    println!("  - Impl type for lookup: {impl_type:?}");

    // Attempt the lookup
    let lookup_result =
        compiler_env.lookup_impl_function(&protocol_type, &impl_type, function_name_atom.clone());

    match lookup_result {
        Some(function_entry) => {
            println!("\n✅ Found function entry!");
            println!(
                "  - Function entry type: {:?}",
                std::mem::discriminant(&function_entry)
            );

            // Check what type of entry it is
            match &function_entry {
                outrun_typechecker::compilation::compiler_environment::UnifiedFunctionEntry::ProtocolSignature {
                    definition, typed_definition, function_id, ..
                } => {
                    println!("  - Entry type: ProtocolSignature");
                    println!("  - Function ID: {function_id}");
                    println!("  - Has typed definition: {}", typed_definition.is_some());
                    if let Some(typed_def) = typed_definition {
                        println!("  - Typed definition body statements: {}", typed_def.body.statements.len());
                    }
                    println!("  - Original definition name: {}", definition.name.name);
                }
                outrun_typechecker::compilation::compiler_environment::UnifiedFunctionEntry::ImplFunction {
                    definition, typed_definition, function_id, ..
                } => {
                    println!("  - Entry type: ImplFunction");
                    println!("  - Function ID: {function_id}");
                    println!("  - Has typed definition: {}", typed_definition.is_some());
                    if let Some(typed_def) = typed_definition {
                        println!("  - Typed definition body statements: {}", typed_def.body.statements.len());
                    }
                    println!("  - Original definition name: {}", definition.name.name);
                }
                outrun_typechecker::compilation::compiler_environment::UnifiedFunctionEntry::ProtocolDefault {
                    definition, typed_definition, function_id, ..
                } => {
                    println!("  - Entry type: ProtocolDefault");
                    println!("  - Function ID: {function_id}");
                    println!("  - Has typed definition: {}", typed_definition.is_some());
                    if let Some(typed_def) = typed_definition {
                        println!("  - Typed definition body statements: {}", typed_def.body.statements.len());
                    }
                    println!("  - Original definition name: {}", definition.name.name);
                }
                other => {
                    println!("  - Entry type: {other:?}");
                }
            }

            // Try to get the typed definition
            if let Some(typed_def) = function_entry.typed_definition() {
                println!("\n📋 Typed definition details:");
                println!("  - Function name: {}", typed_def.name);
                println!("  - Function ID: {}", typed_def.function_id);
                println!("  - Parameters: {}", typed_def.parameters.len());
                println!("  - Body statements: {}", typed_def.body.statements.len());

                // Print statement details
                for (i, stmt) in typed_def.body.statements.iter().enumerate() {
                    match stmt {
                        outrun_typechecker::checker::TypedStatement::Expression(expr) => {
                            println!("    Statement {}: Expression -> {:?}", i, expr.kind);
                        }
                        outrun_typechecker::checker::TypedStatement::LetBinding(let_binding) => {
                            println!(
                                "    Statement {}: LetBinding -> {} variables",
                                i,
                                let_binding.pattern.bound_variables.len()
                            );
                        }
                    }
                }
            } else {
                println!("\n❌ NO TYPED DEFINITION AVAILABLE");
                println!("  This is why the interpreter gets EmptyFunctionBody errors!");
            }
        }
        None => {
            println!("\n❌ Function entry not found");

            // Try to debug what's available
            println!("\n🔍 Debugging what functions are available...");

            // Try with a simpler impl type (just base, no generics)
            let simple_impl_type = StructuredType::Simple(impl_type_id);
            let simple_lookup_result = compiler_env.lookup_impl_function(
                &protocol_type,
                &simple_impl_type,
                function_name_atom,
            );

            match simple_lookup_result {
                Some(entry) => {
                    println!(
                        "  ✅ Found with simple impl type: {:?}",
                        std::mem::discriminant(&entry)
                    );
                }
                None => {
                    println!("  ❌ Not found with simple impl type either");
                }
            }
        }
    }
}
