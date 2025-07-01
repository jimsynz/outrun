use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::core_library;
use crate::unification::StructuredType;

#[allow(clippy::single_match, clippy::for_kv_map, clippy::uninlined_format_args)]
#[test]
fn test_option_trait_implementation_registration() {
    println!("🧪 Testing Option trait implementation registration");

    // Load core library
    let mut compiler_env = CompilerEnvironment::new();
    let compilation = core_library::compile_core_library_with_environment(&mut compiler_env);

    println!("✅ Core library compiled with {} traits, {} structs, {} implementations", 
        compilation.traits.len(),
        compilation.structs.len(), 
        compilation.implementations.len()
    );

    // Find Option trait
    let option_trait_id = compiler_env.intern_type_name("Option");
    let option_trait_type = StructuredType::Simple(option_trait_id.clone());
    
    // Find Option<T> implementations
    let some_type_id = compiler_env.intern_type_name("Outrun.Option.Some");
    let none_type_id = compiler_env.intern_type_name("Outrun.Option.None");
    
    let some_simple_type = StructuredType::Simple(some_type_id.clone());
    let none_simple_type = StructuredType::Simple(none_type_id.clone());

    println!("\n🔍 Checking trait implementations:");
    println!("  - Option trait ID: {}", option_trait_id);
    println!("  - Some type ID: {}", some_type_id);
    println!("  - None type ID: {}", none_type_id);

    // Check if trait implementations are registered
    let some_function_name = compiler_env.intern_atom_name("some?");
    let none_function_name = compiler_env.intern_atom_name("none?");

    println!("\n🔍 Looking up trait functions:");
    
    // Try looking up some? on Some type
    let some_lookup = compiler_env.lookup_impl_function(
        &option_trait_type,
        &some_simple_type,
        some_function_name.clone()
    );
    
    match some_lookup {
        Some(entry) => {
            println!("  ✅ Found some? on Outrun.Option.Some: {:?}", 
                std::mem::discriminant(&entry));
        }
        None => {
            println!("  ❌ Could not find some? on Outrun.Option.Some");
        }
    }

    // Try looking up none? on None type
    let none_lookup = compiler_env.lookup_impl_function(
        &option_trait_type,
        &none_simple_type,
        none_function_name.clone()
    );
    
    match none_lookup {
        Some(entry) => {
            println!("  ✅ Found none? on Outrun.Option.None: {:?}", 
                std::mem::discriminant(&entry));
        }
        None => {
            println!("  ❌ Could not find none? on Outrun.Option.None");
        }
    }

    // Try looking up some? on None type (should work since both implement Option)
    let cross_lookup = compiler_env.lookup_impl_function(
        &option_trait_type,
        &none_simple_type,
        some_function_name
    );
    
    match cross_lookup {
        Some(entry) => {
            println!("  ✅ Found some? on Outrun.Option.None: {:?}", 
                std::mem::discriminant(&entry));
        }
        None => {
            println!("  ❌ Could not find some? on Outrun.Option.None");
        }
    }

    // Debug: List all modules to see what's registered
    println!("\n🔍 Debugging all registered modules:");
    let modules = compiler_env.modules().read().unwrap();
    let mut trait_impl_count = 0;
    
    for (module_key, module) in modules.iter() {
        match module_key {
            crate::compilation::compiler_environment::ModuleKey::TraitImpl(trait_type, impl_type) => {
                trait_impl_count += 1;
                println!("  - TraitImpl: {} for {}", trait_type.to_string_representation(), impl_type.to_string_representation());
                
                // If this is Option-related, show functions
                if trait_type.to_string_representation().contains("Option") || impl_type.to_string_representation().contains("Option") {
                    println!("    Functions: {}", module.functions_by_name.len());
                    for (function_atom, _) in &module.functions_by_name {
                        let function_name = compiler_env.resolve_atom_name(function_atom)
                            .unwrap_or_else(|| "unknown".to_string());
                        println!("      - {}", function_name);
                    }
                }
            }
            _ => {}
        }
    }
    
    println!("\n📊 Total trait implementations found: {}", trait_impl_count);

    // Test the SMT-enhanced lookup as well
    println!("\n🧠 Testing SMT-enhanced lookup:");
    let smt_lookup = compiler_env.lookup_impl_function_with_smt(
        &option_trait_type,
        &some_simple_type,
        compiler_env.intern_atom_name("some?")
    );
    
    match smt_lookup {
        Some(entry) => {
            println!("  ✅ SMT found some? on Outrun.Option.Some: {:?}", 
                std::mem::discriminant(&entry));
        }
        None => {
            println!("  ❌ SMT could not find some? on Outrun.Option.Some");
        }
    }
}