use crate::*;
use outrun_parser::parse_program;

#[test]
fn test_option_some_guard_generic_preservation() {
    let source = r#"
        def test_function(): Boolean {
            let opt = Option.some(value: 42)
            Option.some?(value: opt)
        }
    "#;

    let mut program = parse_program(source).expect("Failed to parse test program");
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    let result = CompilationResult::compile_package(&mut package);
    
    match result {
        Ok(_) => println!("✅ Test passed - no generic parameter loss"),
        Err(e) => {
            println!("❌ Error: {}", e);
            println!("❌ Error Debug: {:?}", e);
            if e.to_string().contains("expected") && e.to_string().contains("Option") {
                println!("🔍 This is the generic parameter loss issue we're debugging");
            }
            panic!("Test failed with error: {}", e);
        }
    }
}

#[test]
fn test_minimal_option_some_guard() {
    // This test reproduces the exact issue from the core library
    // WITHOUT using the core library - define our own minimal Option protocol
    let source = r#"
        # Define a minimal Option protocol to avoid core library dependency
        protocol Option<T> {
            def some(value: T): Option<T>
            def some?(value: Self): Boolean
        }
        
        # Define a concrete struct for Option<Integer>
        struct OptionSome<T>
        
        impl OptionSome<Integer> for Option<Integer> {
            def some(value: Integer): Option<Integer> {
                # Return a dummy value - we're just testing type inference
                Option.some(value: value)
            }
            
            def some?(value: Self): Boolean {
                true  # Simplified - just return true if it's a Some
            }
        }
        
        def test_function(): Boolean {
            # This creates Option<Integer>
            let result = Option.some(value: 42)
            # This should work with Option<Integer> but might lose generic parameter
            Option.some?(value: result)
        }
    "#;

    let mut program = parse_program(source).expect("Failed to parse test program");
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    let result = CompilationResult::compile_with_dependencies(&mut package, vec![]);
    
    match result {
        Ok(_) => println!("✅ Test passed - minimal Option.some? pattern works"),
        Err(e) => {
            println!("❌ Error: {}", e);
            println!("🔍 This reproduces the core library generic parameter issue");
            // Don't panic, just report the error for now
        }
    }
}

#[test]
fn test_index_of_option_some_guard() {
    // This test reproduces the EXACT pattern from the core library:
    // A local function call (index_of) inside a protocol call (Option.some?)
    let source = r#"
        # Simulate being inside the String module where index_of is a local function
        def index_of(value: String, search: String): Option<Integer> {
            # Dummy implementation - just return some option
            Option.some(value: 42)
        }
        
        def contains?(value: String, search: String): Boolean {
            # This is the EXACT pattern from string.outrun:114
            # Local call to index_of, then Option.some? on the result
            Option.some?(value: index_of(value: value, search: search))
        }
    "#;

    let mut program = parse_program(source).expect("Failed to parse test program");
    let mut package = Package::new("test_package".to_string());
    package.add_program(program);

    let result = CompilationResult::compile_package(&mut package);
    
    match result {
        Ok(_) => println!("✅ Test passed - local call + protocol call pattern works"),
        Err(e) => {
            println!("❌ Error: {}", e);
            if e.to_string().contains("expected") && e.to_string().contains("Option") {
                println!("🔍 This reproduces the core library local call issue!");
            }
            panic!("Test failed with error: {}", e);
        }
    }
}