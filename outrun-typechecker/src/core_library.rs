//! Core library loader with lazy compilation caching
//!
//! This module provides runtime file loading and caches the compiled results
//! using our multi-program compiler.

use crate::multi_program_compiler::{CompilationResult, MultiProgramCompiler, ProgramCollection};
use lazy_static::lazy_static;
use miette::{NamedSource, Report};
use std::fs;
use std::path::{Path, PathBuf};

lazy_static! {
    /// Core library compilation results cached at runtime
    pub static ref CORE_LIBRARY_COMPILATION: CompilationResult = {
        load_and_compile_core_library()
    };
}

/// Load and compile the entire core library using the multi-program compiler
fn load_and_compile_core_library() -> CompilationResult {
    let collection = load_core_library_collection();

    let mut compiler = MultiProgramCompiler::new_with_intrinsics();
    match compiler.compile(&collection) {
        Ok(result) => result,
        Err(errors) => {
            eprintln!("🚨 Core Library Compilation Errors:");
            eprintln!("===================================");
            eprintln!(
                "Found {} compilation errors in core library.\n",
                errors.len()
            );

            // Create miette reports with source context
            for (i, error) in errors.iter().enumerate() {
                // Find the source file for this error
                if let Some(source_info) = find_source_for_error(error, &collection) {
                    let report = Report::new(error.clone()).with_source_code(NamedSource::new(
                        source_info.filename,
                        source_info.content,
                    ));
                    eprintln!("Error {}:\n{:?}\n", i + 1, report);
                } else {
                    eprintln!("Error {}: {}", i + 1, error);
                }

                if i >= 9 {
                    // Only show first 10 errors for readability
                    if errors.len() > 10 {
                        eprintln!("... and {} more errors", errors.len() - 10);
                    }
                    break;
                }
            }

            eprintln!("\n💡 This indicates a problem with the core library implementation.");
            eprintln!("The core library must compile successfully for the type checker to work.");

            std::process::exit(1);
        }
    }
}

/// Source information for error reporting
struct SourceInfo {
    filename: String,
    content: String,
}

/// Find the source file and content for a given error
fn find_source_for_error(
    error: &crate::error::TypeError,
    collection: &ProgramCollection,
) -> Option<SourceInfo> {
    use crate::error::TypeError;

    // Extract span from the error
    let span = match error {
        TypeError::TypeMismatch { span, .. } => Some(span),
        TypeError::UndefinedFunction { span, .. } => Some(span),
        TypeError::ArgumentTypeMismatch { span, .. } => Some(span),
        TypeError::UnknownParameter { span, .. } => Some(span),
        _ => None,
    }?;

    let _span_offset = span.offset();

    // The compilation process may create cumulative spans, so we need to find
    // which file contains this offset. For now, let's try to map to the first file
    // that has content, as a simple heuristic.
    for (filename, source_content) in &collection.sources {
        if !source_content.trim().is_empty() {
            return Some(SourceInfo {
                filename: filename.clone(),
                content: source_content.clone(),
            });
        }
    }

    None
}

/// Load core library programs into a ProgramCollection
pub fn load_core_library_collection() -> ProgramCollection {
    let mut collection = ProgramCollection::new();
    let core_lib_path = get_core_library_path();

    let outrun_files = find_outrun_files(&core_lib_path);
    let mut parse_errors = Vec::new();

    for (relative_path, absolute_path) in outrun_files {
        match fs::read_to_string(&absolute_path) {
            Ok(source_code) => match outrun_parser::parse_program(&source_code) {
                Ok(program) => {
                    collection.add_program(relative_path, program, source_code);
                }
                Err(parse_error) => {
                    parse_errors.push((relative_path, parse_error, source_code));
                }
            },
            Err(io_error) => {
                eprintln!(
                    "❌ Failed to read core library file {}: {}",
                    relative_path, io_error
                );
                std::process::exit(1);
            }
        }
    }

    // Handle parse errors
    if !parse_errors.is_empty() {
        eprintln!("🚨 Core Library Parse Errors:");
        eprintln!("============================");
        eprintln!(
            "Found {} parse errors in core library files.\n",
            parse_errors.len()
        );

        for (file_path, parse_error, source_code) in parse_errors {
            let report = Report::from(parse_error)
                .with_source_code(NamedSource::new(file_path.clone(), source_code));
            eprintln!("📄 {}:\n{:?}\n", file_path, report);
        }

        eprintln!("💡 Fix the parse errors above and try again.");
        eprintln!("The core library must parse successfully for the type checker to work.");
        std::process::exit(1);
    }

    collection
}

/// Get the path to the core library directory
fn get_core_library_path() -> PathBuf {
    // Get the path relative to the typechecker crate
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    Path::new(manifest_dir).join("../outrun-core/lib")
}

/// Find all .outrun files in the core library directory
fn find_outrun_files(dir: &Path) -> Vec<(String, PathBuf)> {
    let mut files = Vec::new();
    find_outrun_files_recursive(dir, dir, &mut files);
    files.sort_by(|a, b| a.0.cmp(&b.0)); // Sort by relative path for deterministic order
    files
}

/// Recursively find .outrun files
fn find_outrun_files_recursive(
    root_dir: &Path,
    current_dir: &Path,
    files: &mut Vec<(String, PathBuf)>,
) {
    if let Ok(entries) = fs::read_dir(current_dir) {
        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_dir() {
                find_outrun_files_recursive(root_dir, &path, files);
            } else if path.extension().is_some_and(|ext| ext == "outrun") {
                // Calculate relative path from the root core library directory
                let relative_path = path
                    .strip_prefix(root_dir)
                    .unwrap()
                    .to_string_lossy()
                    .to_string();

                files.push((relative_path, path));
            }
        }
    }
}

/// Get the compiled core library (trait definitions, structs, functions, etc.)
pub fn get_core_library_compilation() -> &'static CompilationResult {
    &CORE_LIBRARY_COMPILATION
}

/// Get source code for a specific core library file
pub fn get_core_source(file_path: &str) -> Option<String> {
    let collection = load_core_library_collection();
    collection.get_source(file_path).cloned()
}

/// Core library statistics for diagnostics
#[derive(Debug)]
pub struct CoreLibraryStats {
    pub parsed_files: usize,
    pub total_traits: usize,
    pub total_structs: usize,
    pub total_functions: usize,
    pub total_implementations: usize,
}

/// Get statistics about the loaded core library
pub fn core_library_stats() -> CoreLibraryStats {
    let compilation = get_core_library_compilation();
    let collection = load_core_library_collection();

    CoreLibraryStats {
        parsed_files: collection.programs.len(),
        total_traits: compilation.traits.len(),
        total_structs: compilation.structs.len(),
        total_functions: compilation.function_registry.len(),
        total_implementations: compilation.implementations.len(),
    }
}

impl std::fmt::Display for CoreLibraryStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Core Library Stats: {} files, {} traits, {} structs, {} functions, {} implementations",
            self.parsed_files,
            self.total_traits,
            self.total_structs,
            self.total_functions,
            self.total_implementations
        )
    }
}

/// Load and compile core library for testing, returning errors instead of exiting
/// This function is used by tests to examine compilation errors in detail
pub fn load_and_compile_core_library_for_test(
    core_lib_path: &Path,
) -> Result<CompilationResult, Vec<crate::error::TypeError>> {
    let mut collection = ProgramCollection::new();

    let outrun_files = find_outrun_files(core_lib_path);
    let mut parse_errors = Vec::new();

    for (relative_path, absolute_path) in outrun_files {
        match fs::read_to_string(&absolute_path) {
            Ok(source_code) => match outrun_parser::parse_program(&source_code) {
                Ok(program) => {
                    collection.add_program(relative_path, program, source_code);
                }
                Err(parse_error) => {
                    parse_errors.push((relative_path, parse_error, source_code));
                }
            },
            Err(io_error) => {
                return Err(vec![crate::error::TypeError::internal_with_span(
                    format!("Failed to read core library file: {}", io_error),
                    miette::SourceSpan::new(0.into(), 0),
                )]);
            }
        }
    }

    // Handle parse errors by converting them to TypeError
    if !parse_errors.is_empty() {
        let mut errors = Vec::new();
        for (file_path, _parse_error, _source_code) in parse_errors {
            errors.push(crate::error::TypeError::internal_with_span(
                format!("Parse error in core library file: {}", file_path),
                miette::SourceSpan::new(0.into(), 0),
            ));
        }
        return Err(errors);
    }

    let mut compiler = MultiProgramCompiler::new_with_intrinsics();
    compiler.compile(&collection)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_core_library_loads() {
        let stats = core_library_stats();
        println!("{}", stats);

        // Verify core library loaded successfully
        assert!(stats.parsed_files > 0, "No core library files were parsed");
        assert!(
            stats.total_traits > 0,
            "No traits were found in core library"
        );
        assert!(
            stats.total_structs > 0,
            "No structs were found in core library"
        );
    }

    #[test]
    fn test_core_library_compilation() {
        let compilation = get_core_library_compilation();

        // Verify compilation succeeded
        assert!(!compilation.traits.is_empty(), "No traits compiled");
        assert!(!compilation.structs.is_empty(), "No structs compiled");

        // Verify compilation order is valid
        assert!(
            !compilation.compilation_order.is_empty(),
            "No compilation order"
        );
    }

    #[test]
    fn test_core_source_access() {
        // Test that we can access source code for core files
        let collection = load_core_library_collection();
        assert!(
            !collection.programs.is_empty(),
            "Core library should not be empty"
        );

        // Try to get source for first file
        if let Some(first_file) = collection.programs.keys().next() {
            let source = get_core_source(first_file);
            assert!(
                source.is_some(),
                "Should be able to get source for core file"
            );
            assert!(!source.unwrap().is_empty(), "Source should not be empty");
        }
    }
}
