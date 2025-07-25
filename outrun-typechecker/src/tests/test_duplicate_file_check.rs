//! Check for duplicate files in core library collection

use crate::core_library::{collect_outrun_files, default_core_library_path};
use std::collections::HashSet;

#[test]
fn test_duplicate_file_paths_detection() {
    let core_path = default_core_library_path().expect("Should find core library");
    let lib_path = core_path.join("lib");

    let mut programs = Vec::new();
    collect_outrun_files(&lib_path, &mut programs).expect("Should collect files");

    println!("🔍 Checking for duplicate file paths in core library collection...");

    let mut seen_paths = HashSet::new();
    let mut duplicates = Vec::new();

    for (file_path, _content) in &programs {
        if seen_paths.contains(file_path) {
            duplicates.push(file_path.clone());
            println!("🚨 DUPLICATE: {}", file_path);
        } else {
            seen_paths.insert(file_path.clone());
        }
    }

    println!("📊 Total files collected: {}", programs.len());
    println!("📊 Unique file paths: {}", seen_paths.len());
    println!("📊 Duplicate file paths: {}", duplicates.len());

    if !duplicates.is_empty() {
        println!("❌ Found {} duplicate file paths:", duplicates.len());
        for duplicate in &duplicates {
            println!("  - {}", duplicate);
        }
    } else {
        println!("✅ No duplicate file paths found");
    }

    // Also check for files with similar content that might have duplicate impl blocks
    println!("\n🔍 Analyzing content for potential duplicate impl blocks...");

    let mut impl_block_counts = std::collections::HashMap::new();

    for (file_path, content) in &programs {
        // Count impl blocks by looking for "impl ProtocolName for TypeName" patterns
        let impl_blocks: Vec<&str> = content
            .lines()
            .filter(|line| line.trim().starts_with("impl ") && line.contains(" for "))
            .collect();

        if !impl_blocks.is_empty() {
            println!("📄 {}: {} impl blocks", file_path, impl_blocks.len());

            for impl_block in impl_blocks {
                let key = impl_block.trim().to_string();
                *impl_block_counts.entry(key.clone()).or_insert(0) += 1;

                if impl_block_counts[&key] > 1 {
                    println!(
                        "🚨 DUPLICATE IMPL: {} (appears {} times)",
                        key, impl_block_counts[&key]
                    );
                }
            }
        }
    }

    // Check for specific problematic impl blocks mentioned in the error
    let problematic_impls = [
        "impl BinaryAddition for Outrun.Core.Integer64",
        "impl Display for Outrun.Core.String",
        "impl Equality for Outrun.Core.List",
        "impl Option for Outrun.Option.None",
    ];

    println!("\n🎯 Checking for specific problematic impl blocks...");
    for target_impl in &problematic_impls {
        let count = impl_block_counts.get(*target_impl).unwrap_or(&0);
        if *count > 1 {
            println!("🚨 {} appears {} times!", target_impl, count);
        } else if *count == 1 {
            println!("✅ {} appears once (normal)", target_impl);
        } else {
            println!("❓ {} not found", target_impl);
        }
    }
}
