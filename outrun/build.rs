// Build script to generate outrun_syntax.dump from the YAML source
// This ensures the binary syntax file exists for embedded mode

use std::path::Path;

fn main() {
    let dump_path = Path::new("outrun_syntax.dump");
    let _yaml_path = Path::new("../outrun.sublime-syntax");

    // Only generate if the dump file doesn't exist
    if !dump_path.exists() {
        println!("cargo:warning=Generating outrun_syntax.dump from YAML source...");

        if let Err(e) = generate_syntax_dump_from_yaml() {
            panic!("Failed to generate syntax dump: {e}");
        }

        println!("cargo:warning=Generated outrun_syntax.dump successfully");
    }

    // Tell cargo to rerun if the YAML source changes
    println!("cargo:rerun-if-changed=../outrun.sublime-syntax");
    // Also rerun if the dump file is deleted
    println!("cargo:rerun-if-changed=outrun_syntax.dump");
}

fn generate_syntax_dump_from_yaml() -> Result<(), Box<dyn std::error::Error>> {
    // Load default syntaxes for broad language support
    let syntax_set = syntect::parsing::SyntaxSet::load_defaults_newlines();
    let mut builder = syntax_set.into_builder();

    // Add our Outrun syntax from the parent directory
    builder.add_from_folder("..", true)?;
    let final_syntax_set = builder.build();

    // Serialize to binary format
    syntect::dumps::dump_to_uncompressed_file(&final_syntax_set, "outrun_syntax.dump")?;

    Ok(())
}
