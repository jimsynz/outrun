// Simple standalone profiling binary for core library compilation
use outrun_typechecker::core_library;

fn main() {
    println!("Starting core library compilation profiling...");
    let start = std::time::Instant::now();
    
    // This will trigger the lazy_static compilation
    let _compilation = core_library::get_core_library_compilation();
    
    let duration = start.elapsed();
    println!("Core library compilation took: {:.2}s", duration.as_secs_f64());
}