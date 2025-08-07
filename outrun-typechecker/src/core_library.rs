//! Core library path utilities
//!
//! This module provides utilities for locating the outrun-core library.
//! The actual loading is now handled by the unified package system in package.rs.

use crate::error::TypecheckError;
use std::fs;
use std::path::Path;

/// Recursively collect all .outrun files from a directory
/// Files are processed in deterministic alphabetical order to ensure consistent compilation
pub fn collect_outrun_files(
    dir: &Path,
    programs: &mut Vec<(String, String)>,
) -> Result<(), TypecheckError> {
    let entries = fs::read_dir(dir).map_err(|e| {
        TypecheckError::CoreLibraryError(format!(
            "Failed to read directory {}: {}",
            dir.display(),
            e
        ))
    })?;

    // Collect all entries and sort them for deterministic processing order
    let mut sorted_entries: Vec<_> = entries.collect::<Result<Vec<_>, _>>().map_err(|e| {
        TypecheckError::CoreLibraryError(format!("Failed to read directory entries: {}", e))
    })?;

    // Sort by path for deterministic order
    sorted_entries.sort_by_key(|a| a.path());

    for entry in sorted_entries {
        let path = entry.path();

        if path.is_dir() {
            // Recursively collect from subdirectories
            collect_outrun_files(&path, programs)?;
        } else if path.extension().and_then(|s| s.to_str()) == Some("outrun") {
            // Read .outrun file
            let file_path = path.to_string_lossy().to_string();
            let content = fs::read_to_string(&path).map_err(|e| {
                TypecheckError::CoreLibraryError(format!(
                    "Failed to read file {}: {}",
                    file_path, e
                ))
            })?;

            programs.push((file_path, content));
        }
    }

    Ok(())
}

/// Get the default core library path relative to the current project
pub fn default_core_library_path() -> Result<std::path::PathBuf, TypecheckError> {
    // Try to find outrun-core relative to the current working directory
    let current_dir = std::env::current_dir().map_err(|e| {
        TypecheckError::CoreLibraryError(format!("Failed to get current directory: {}", e))
    })?;

    // Look for outrun-core in the project root
    let mut search_path = current_dir;
    loop {
        let core_path = search_path.join("outrun-core");
        if core_path.exists() {
            return Ok(core_path);
        }

        // Move up one directory
        if let Some(parent) = search_path.parent() {
            search_path = parent.to_path_buf();
        } else {
            break;
        }
    }

    Err(TypecheckError::CoreLibraryError(
        "Could not find outrun-core directory in project hierarchy".to_string(),
    ))
}
