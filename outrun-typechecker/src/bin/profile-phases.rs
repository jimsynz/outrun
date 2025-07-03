// Phase-by-phase profiling binary for core library compilation
use std::time::Instant;
use std::path::PathBuf;

// We'll temporarily patch the core library loading to add timing
fn main() {
    println!("🔬 Starting phase-by-phase core library compilation profiling...");
    
    // Let's load the core library directly with timing
    let total_start = Instant::now();
    
    // Load the collection (parsing phase)
    println!("📖 Phase 1: Loading and parsing core library files...");
    let parse_start = Instant::now();
    let collection = load_core_library_collection_timed();
    let parse_duration = parse_start.elapsed();
    println!("  ✅ Parsing took: {:.2}s", parse_duration.as_secs_f64());
    
    // Compilation phase
    println!("🔄 Phase 2: Starting compilation pipeline...");
    let compile_start = Instant::now();
    let mut compiler_env = outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
    let _result = outrun_typechecker::core_library::load_and_compile_core_library_with_environment(&mut compiler_env, collection);
    let compile_duration = compile_start.elapsed();
    println!("  ✅ Compilation took: {:.2}s", compile_duration.as_secs_f64());
    
    let total_duration = total_start.elapsed();
    
    println!("📊 Phase Breakdown:");
    println!("  Parsing: {:.2}s ({:.1}%)", parse_duration.as_secs_f64(), (parse_duration.as_secs_f64() / total_duration.as_secs_f64()) * 100.0);
    println!("  Compilation: {:.2}s ({:.1}%)", compile_duration.as_secs_f64(), (compile_duration.as_secs_f64() / total_duration.as_secs_f64()) * 100.0);
    println!("  Total: {:.2}s", total_duration.as_secs_f64());
    
    // Cache statistics
    let cache_stats = outrun_typechecker::smt::solver_pool::get_cache_stats();
    println!("  SMT Cache: {}", cache_stats);
    
    if cache_stats.total_queries == 0 {
        println!("\n🎯 Key Insight: No SMT queries during compilation!");
        println!("   This explains why async SMT didn't improve performance.");
        println!("   Bottlenecks are likely in:");
        println!("   - File I/O and parsing ({:.1}% of time)", (parse_duration.as_secs_f64() / total_duration.as_secs_f64()) * 100.0);
        println!("   - AST construction and type system registration");
        println!("   - Trait/struct/function extraction");
        println!("   - Module creation and organization");
    }
}

// Copy of the core library loading function with timing
fn load_core_library_collection_timed() -> outrun_typechecker::compilation::program_collection::ProgramCollection {
    use outrun_typechecker::compilation::program_collection::ProgramCollection;
    use std::fs;
    use std::path::{Path, PathBuf};
    
    // Find core library directory
    let core_lib_dir = find_core_library_directory();
    
    println!("  📁 Loading from: {}", core_lib_dir.display());
    
    // Load all .outrun files
    let mut collection = ProgramCollection::new();
    let file_start = Instant::now();
    
    if let Ok(entries) = fs::read_dir(&core_lib_dir) {
        let mut file_count = 0;
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("outrun") {
                let file_load_start = Instant::now();
                if let Ok(source) = fs::read_to_string(&path) {
                    match outrun_parser::parse_program(&source) {
                        Ok(program) => {
                            let filename = path.file_name()
                                .and_then(|n| n.to_str())
                                .unwrap_or("unknown")
                                .to_string();
                            
                            collection.add_program(filename, program, source.clone());
                            file_count += 1;
                            
                            let file_duration = file_load_start.elapsed();
                            if file_duration.as_millis() > 100 { // Only log slow files
                                println!("    📄 {} took {:.2}s", path.file_name().unwrap().to_str().unwrap(), file_duration.as_secs_f64());
                            }
                        }
                        Err(e) => {
                            eprintln!("    ❌ Failed to parse {}: {:?}", path.display(), e);
                        }
                    }
                } else {
                    eprintln!("    ❌ Failed to read {}", path.display());
                }
            }
        }
        let file_duration = file_start.elapsed();
        println!("  📊 Loaded {} files in {:.2}s", file_count, file_duration.as_secs_f64());
    }
    
    collection
}

fn find_core_library_directory() -> PathBuf {
    use std::env;
    use std::path::Path;
    
    // Try different possible locations
    let possible_paths = [
        "core-library",
        "../core-library", 
        "../../core-library",
        "../../../core-library",
        "./outrun/core-library",
        "../outrun/core-library",
        "../../outrun/core-library",
    ];
    
    for path_str in &possible_paths {
        let path = Path::new(path_str);
        if path.exists() && path.is_dir() {
            return path.to_path_buf();
        }
    }
    
    // Fallback: use current directory or panic
    env::current_dir().unwrap_or_else(|_| {
        panic!("Could not find core library directory in any of: {:?}", possible_paths)
    })
}