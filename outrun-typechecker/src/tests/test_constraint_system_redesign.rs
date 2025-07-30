use crate::types::Type;
use crate::Package;
use outrun_parser::parse_program;

#[test]
fn test_unary_minus_parameter_name_based_constraint() {
    // Test code that uses UnaryMinus protocol
    let code = r#"
        -1
    "#;

    let program = parse_program(code).expect("Parse should succeed");
    let mut package = Package::new("test".to_string());
    package.add_program(program);

    // Process through the full pipeline using the new CompilationResult system
    let result = crate::CompilationResult::compile_package(&mut package);

    match result {
        Ok(_compilation_result) => {
            println!(
                "✅ UnaryMinus constraint resolution succeeded with parameter-name-based system"
            );
        }
        Err(e) => {
            eprintln!("❌ UnaryMinus constraint resolution failed: {:?}", e);
            // This should now work with the redesigned constraint system
            panic!(
                "Constraint system should handle UnaryMinus with parameter names, not positions"
            );
        }
    }
}

#[test]
fn test_parameter_name_extraction() {
    use crate::inference::FunctionSignatureAnalyzer;

    // Test parameter name-based path generation
    let params = vec![
        ("value".to_string(), Type::concrete("Integer64")),
        ("other".to_string(), Type::concrete("String")),
    ];
    let return_type = Type::concrete("Integer64");

    let analysis = FunctionSignatureAnalyzer::analyze_signature(&params, &return_type);

    // Check that Self positions contain parameter names
    for self_pos in &analysis.self_positions {
        println!("Self position path: {:?}", self_pos.path);

        // The path should now use "param_<name>" format instead of "arg_<index>"
        if !self_pos.path.is_empty() {
            let first_segment = &self_pos.path[0];
            if first_segment.starts_with("param_") {
                println!("✅ Found parameter-name-based path: {}", first_segment);
            } else if first_segment.starts_with("arg_") {
                panic!("❌ Still using positional format: {}", first_segment);
            }
        }
    }
}
