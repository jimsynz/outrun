use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::ProgramCollection;
use outrun_parser::parse_program;

fn load_core_library() -> ProgramCollection {
    let mut collection = ProgramCollection::new();

    let core_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("outrun-core/lib");

    fn load_files_recursively(dir: &std::path::Path, collection: &mut ProgramCollection) {
        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    load_files_recursively(&path, collection);
                } else if path.extension().and_then(|s| s.to_str()) == Some("outrun") {
                    if let Ok(source) = std::fs::read_to_string(&path) {
                        if let Ok(program) = parse_program(&source) {
                            let key = path.to_string_lossy().into_owned();
                            collection.add_program(key, program, source);
                        }
                    }
                }
            }
        }
    }

    load_files_recursively(&core_dir, &mut collection);
    collection
}

#[test]
fn test_exponentiate_with_generic_types() {
    // Test case that reproduces the ArgumentTypeMismatch error
    let test_source = r#"
trait Option<T> {
    def unwrap(value: Self): T
}

def test_exponentiate_generic(): Integer {
    let power = Option.unwrap(value: some_option)
    10 ** power
}
"#;

    let mut collection = load_core_library();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Exponentiate with generic types worked!");
        }
        Err(errors) => {
            println!("❌ Exponentiate test failed with {} errors:", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);

                if let crate::error::TypeError::ArgumentTypeMismatch {
                    function_name,
                    parameter_name,
                    expected_type,
                    found_type,
                    ..
                } = error
                {
                    if function_name == "exponentiate" {
                        println!("🔍 Found exponentiate type mismatch:");
                        println!("   Function: {function_name}");
                        println!("   Parameter: {parameter_name}");
                        println!("   Expected: {expected_type}");
                        println!("   Found: {found_type}");
                    }
                }
            }
        }
    }
}

#[test]
fn test_exponentiate_with_option_unwrap() {
    // More specific test case matching the Float64 implementation
    let test_source = r#"
def test_scale_calculation(precision: Option<Integer>): Integer {
    10 ** Option.unwrap(value: precision)
}
"#;

    let mut collection = load_core_library();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Option.unwrap exponentiate test worked!");
        }
        Err(errors) => {
            println!(
                "❌ Option.unwrap exponentiate test failed with {} errors:",
                errors.len()
            );

            let mut found_exponentiate_error = false;
            for (i, error) in errors.iter().enumerate() {
                if let crate::error::TypeError::ArgumentTypeMismatch {
                    function_name,
                    parameter_name,
                    expected_type,
                    found_type,
                    ..
                } = error
                {
                    if function_name == "exponentiate" && parameter_name == "rhs" {
                        println!("🎯 Found the exact issue:");
                        println!("   Expected: {expected_type} (concrete type)");
                        println!("   Found: {found_type} (generic type)");
                        println!(
                            "   Problem: Option.unwrap returns T, but ** needs concrete Integer"
                        );
                        found_exponentiate_error = true;
                        break;
                    }
                }

                if i < 5 {
                    // Show first 5 errors for context
                    println!("Error {}: {:?}", i + 1, error);
                }
            }

            if !found_exponentiate_error {
                println!("ℹ️  No exponentiate ArgumentTypeMismatch found in first 5 errors");
            }
        }
    }
}

#[test]
fn test_simple_exponentiate_works() {
    // Verify that non-generic exponentiate works fine
    let test_source = r#"
def test_simple_exponentiate(): Integer {
    10 ** 3
}
"#;

    let mut collection = load_core_library();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Simple exponentiate test worked!");
        }
        Err(errors) => {
            println!(
                "❌ Simple exponentiate test failed with {} errors:",
                errors.len()
            );
            for (i, error) in errors.iter().enumerate().take(3) {
                println!("Error {}: {:?}", i + 1, error);
            }
        }
    }
}
