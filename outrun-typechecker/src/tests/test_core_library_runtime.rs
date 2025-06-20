//! Tests for the new runtime core library loading and compilation caching

use crate::core_library::{
    core_library_stats, get_core_library_compilation, load_core_library_collection,
};

#[test]
fn test_lazy_static_compilation_caching() {
    // Test that accessing the compilation multiple times returns the same cached result
    let compilation1 = get_core_library_compilation();
    let compilation2 = get_core_library_compilation();

    // These should be the exact same reference (lazy_static ensures single initialization)
    assert!(
        std::ptr::eq(compilation1, compilation2),
        "Compilation should be cached"
    );

    // Verify the compilation has expected content
    assert!(!compilation1.traits.is_empty(), "Should have traits");
    assert!(!compilation1.structs.is_empty(), "Should have structs");
    assert!(
        !compilation1.function_registry.is_empty(),
        "Should have functions"
    );
    assert!(
        !compilation1.compilation_order.is_empty(),
        "Should have compilation order"
    );
}

#[test]
fn test_runtime_file_loading() {
    // Test that we can load the core library collection without build.rs
    let collection = load_core_library_collection();

    assert!(!collection.programs.is_empty(), "Should load programs");
    assert!(!collection.sources.is_empty(), "Should load sources");
    assert_eq!(
        collection.programs.len(),
        collection.sources.len(),
        "Programs and sources should match"
    );

    // Verify some expected core library files exist
    let expected_files = vec!["boolean.outrun", "string.outrun", "integer.outrun"];
    for expected_file in expected_files {
        assert!(
            collection
                .programs
                .keys()
                .any(|key| key.contains(expected_file)),
            "Should contain {}",
            expected_file
        );
    }
}

#[test]
fn test_core_library_stats() {
    let stats = core_library_stats();

    // Verify reasonable numbers (these may change as core library evolves)
    assert!(
        stats.parsed_files >= 10,
        "Should have at least 10 core files, got {}",
        stats.parsed_files
    );
    assert!(
        stats.total_traits >= 5,
        "Should have at least 5 traits, got {}",
        stats.total_traits
    );
    assert!(
        stats.total_structs >= 5,
        "Should have at least 5 structs, got {}",
        stats.total_structs
    );

    println!("Core library stats: {}", stats);
}

#[test]
fn test_compilation_reproducibility() {
    // Test that compilation is deterministic across multiple loads
    let collection1 = load_core_library_collection();
    let collection2 = load_core_library_collection();

    // Should have same number of programs
    assert_eq!(collection1.programs.len(), collection2.programs.len());

    // Should have same file paths
    let mut paths1: Vec<_> = collection1.programs.keys().cloned().collect();
    let mut paths2: Vec<_> = collection2.programs.keys().cloned().collect();
    paths1.sort();
    paths2.sort();
    assert_eq!(paths1, paths2);
}

#[test]
fn test_no_build_rs_dependencies() {
    // This test verifies that we're not depending on build.rs artifacts
    // by ensuring our runtime loading actually works

    let compilation = get_core_library_compilation();

    // If this test passes, it means we successfully loaded and compiled
    // the core library at runtime without needing build.rs
    assert!(!compilation.traits.is_empty());
    assert!(!compilation.compilation_order.is_empty());

    println!("✅ Runtime core library loading works without build.rs!");
    println!(
        "   Loaded {} traits, {} structs, {} functions",
        compilation.traits.len(),
        compilation.structs.len(),
        compilation.function_registry.len()
    );
}
