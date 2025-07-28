//! Package loading and manifest parsing for Outrun cassettes
//!
//! This module handles loading Outrun packages (cassettes) by reading outrun.toml
//! manifests and collecting source files according to the package specification.

use crate::error::TypecheckError;
use outrun_parser::{parse_program, parse_program_with_source, Program};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Outrun package manifest (outrun.toml)
#[derive(Debug, Deserialize)]
pub struct PackageManifest {
    pub package: PackageInfo,
    pub lib: Option<LibConfig>,
    pub dependencies: Option<HashMap<String, String>>,
}

/// Package metadata from [package] section
#[derive(Debug, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    pub authors: Option<Vec<String>>,
    pub description: Option<String>,
    pub license: Option<String>,
}

/// Library configuration from [lib] section
#[derive(Debug, Deserialize)]
pub struct LibConfig {
    pub path: String,
}

/// A loaded Outrun package with all source files parsed
pub struct LoadedPackage {
    pub manifest: PackageManifest,
    pub programs: Vec<Program>,
    pub package_root: PathBuf,
}

impl LoadedPackage {
    /// Load a package from a directory containing outrun.toml
    pub fn load_from_directory(package_dir: &Path) -> Result<Self, TypecheckError> {
        let manifest_path = package_dir.join("outrun.toml");

        if !manifest_path.exists() {
            return Err(TypecheckError::CoreLibraryError(format!(
                "No outrun.toml found in {}",
                package_dir.display()
            )));
        }

        // Parse the manifest
        let manifest_content = fs::read_to_string(&manifest_path).map_err(|e| {
            TypecheckError::CoreLibraryError(format!(
                "Failed to read {}: {}",
                manifest_path.display(),
                e
            ))
        })?;

        let manifest: PackageManifest = toml::from_str(&manifest_content).map_err(|e| {
            TypecheckError::CoreLibraryError(format!(
                "Failed to parse {}: {}",
                manifest_path.display(),
                e
            ))
        })?;

        // Determine source directory
        let source_path = if let Some(lib_config) = &manifest.lib {
            package_dir.join(&lib_config.path)
        } else {
            // Default to lib/ if no [lib] section
            package_dir.join("lib")
        };

        if !source_path.exists() {
            return Err(TypecheckError::CoreLibraryError(format!(
                "Source directory {} does not exist",
                source_path.display()
            )));
        }

        // Collect and parse all .outrun files
        let mut programs = Vec::new();
        collect_and_parse_outrun_files(&source_path, &mut programs)?;

        // Sort for deterministic processing order
        programs.sort_by(|a, b| {
            // Compare by the file path that would be stored in debug info
            // For now, just use a simple comparison - this could be enhanced
            // to prioritize protocol definitions, etc.
            format!("{:?}", a).cmp(&format!("{:?}", b))
        });

        println!(
            "📦 Loaded package '{}' with {} source files",
            manifest.package.name,
            programs.len()
        );

        Ok(LoadedPackage {
            manifest,
            programs,
            package_root: package_dir.to_path_buf(),
        })
    }

    /// Get the package name
    pub fn name(&self) -> &str {
        &self.manifest.package.name
    }

    /// Check if this is the core library package
    pub fn is_core_library(&self) -> bool {
        self.manifest.package.name == "outrun-core"
    }
}

/// Recursively collect and parse all .outrun files from a directory
fn collect_and_parse_outrun_files(
    dir: &Path,
    programs: &mut Vec<Program>,
) -> Result<(), TypecheckError> {
    let entries = fs::read_dir(dir).map_err(|e| {
        TypecheckError::CoreLibraryError(format!(
            "Failed to read directory {}: {}",
            dir.display(),
            e
        ))
    })?;

    for entry in entries {
        let entry = entry.map_err(|e| {
            TypecheckError::CoreLibraryError(format!("Failed to read directory entry: {}", e))
        })?;

        let path = entry.path();

        if path.is_dir() {
            // Recursively collect from subdirectories
            collect_and_parse_outrun_files(&path, programs)?;
        } else if path.extension().and_then(|s| s.to_str()) == Some("outrun") {
            // Parse .outrun file
            let file_path = path.to_string_lossy().to_string();
            let content = fs::read_to_string(&path).map_err(|e| {
                TypecheckError::CoreLibraryError(format!(
                    "Failed to read file {}: {}",
                    file_path, e
                ))
            })?;

            match parse_program_with_source(&content, Some(file_path.clone())) {
                Ok(program) => {
                    programs.push(program);
                }
                Err(parse_error) => {
                    return Err(TypecheckError::CoreLibraryError(format!(
                        "Failed to parse {}: {:?}",
                        file_path, parse_error
                    )));
                }
            }
        }
    }

    Ok(())
}

/// Load the core library package from the default location
pub fn load_core_library_package() -> Result<Option<LoadedPackage>, TypecheckError> {
    if let Ok(core_path) = crate::core_library::default_core_library_path() {
        let package = LoadedPackage::load_from_directory(&core_path)?;
        Ok(Some(package))
    } else {
        Ok(None)
    }
}
