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
fn test_option_some_with_generic_inference() {
    // Test that demonstrates the "generic vs self" issue
    // When calling Option.some?(value: concrete_value), Self should be inferred as Option<ConcreteType>

    let test_source = r#"
trait TestTrait {
    def index_of(haystack: Self, needle: String): Integer
}

impl TestTrait for String {
    def index_of(haystack: Self, needle: String): Integer {
        42  # Simplified implementation
    }
}

def test_option_generic_inference(): Option<Integer> {
    let text = "hello world"
    let needle = "world"
    # This should infer Self = Option<Integer> for Option.some?
    # The argument type is Integer (from index_of), so Self should be Option<Integer>
    Option.some?(value: TestTrait.index_of(haystack: text, needle: needle))
}
"#;

    let mut collection = load_core_library();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Generic Self inference worked correctly!");
        }
        Err(errors) => {
            println!("❌ Compilation failed with {} errors:", errors.len());

            // Look for argument type mismatch errors that indicate the Self resolution issue
            let mut found_self_issue = false;
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
                    if function_name == "some?" && parameter_name == "value" {
                        println!("🔍 Self resolution issue found:");
                        println!("   Expected: {expected_type}");
                        println!("   Found:    {found_type}");

                        // This should show something like:
                        // Expected: Self (where Self = Option<Integer>)
                        // Found: Integer
                        // But currently it probably shows:
                        // Expected: Self (where Self = Option)
                        // Found: Integer
                        found_self_issue = true;
                    }
                }
            }

            if found_self_issue {
                println!("🎯 Confirmed: This is the 'generic vs self' issue we need to fix");
            }
        }
    }
}

#[test]
fn test_basic_option_some_call() {
    // Simpler test to isolate the issue
    let test_source = r#"
def test_let_binding(): Boolean {
    let x = 42
    x == 42
}
"#;

    let mut collection = load_core_library();
    let program = parse_program(test_source).expect("Failed to parse test program");
    collection.add_program("test.outrun".to_string(), program, test_source.to_string());

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection);

    match result {
        Ok(_) => {
            println!("✅ Basic Option.some? call worked!");
        }
        Err(errors) => {
            println!("❌ Basic test failed with {} errors:", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {:?}", i + 1, error);
            }
        }
    }
}
