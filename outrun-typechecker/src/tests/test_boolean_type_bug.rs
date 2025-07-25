//! Test to reproduce the Boolean type name bug in core library

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_boolean_type_bug_reproduction() {
    let mut engine = TypeInferenceEngine::new();

    // Simulate the exact scenario from string.outrun
    let test_code = r#"
protocol Option<T> {
    def some?(value: Self): Boolean
}

protocol Boolean {
    def to_string(value: Self): String
}

struct Outrun.Core.Boolean() {}

impl Boolean for Outrun.Core.Boolean {
    def to_string(value: Self): String {
        "boolean"
    }
}

# Mock implementation of Option.some? that returns a boolean literal
impl<T> Option<T> for Outrun.Option.Some<T> {
    def some?(value: Self): Boolean {
        true  # This should infer as Outrun.Core.Boolean (concrete)
    }
}

# The failing function from string.outrun
def test_function(): Boolean {
    Option.some?(value: some_option_value)
}
"#;

    let mut program = parse_program(test_code).expect("Parse should succeed");
    let result = engine.typecheck_program(&mut program);

    match result {
        Ok(()) => {
            println!("✅ Boolean type bug test passed - this means the bug is fixed!");
        }
        Err(e) => {
            println!("❌ Boolean type bug reproduced: {:?}", e);

            // Check if we get the specific error about Boolean concrete type
            let error_str = format!("{:?}", e);
            if error_str.contains("Concrete { id: TypeId(\"Boolean\")") {
                println!("🐛 CONFIRMED: Bug reproduced - typechecker thinks there's a concrete 'Boolean' type");
                println!("The bug is in how we resolve function call return types");
            } else {
                println!("❓ Different error - this might be a different issue");
            }
        }
    }
}

#[test]
fn test_boolean_literal_inference() {
    let mut engine = TypeInferenceEngine::new();

    // Test that boolean literals correctly infer as Outrun.Core.Boolean
    let test_code = r#"
def test_literal(): Boolean {
    true
}
"#;

    let mut program = parse_program(test_code).expect("Parse should succeed");
    let result = engine.typecheck_program(&mut program);

    match result {
        Ok(()) => {
            println!("✅ Boolean literal test passed - literals infer correctly");
        }
        Err(e) => {
            println!("❌ Boolean literal test failed: {:?}", e);
            let error_str = format!("{:?}", e);
            if error_str.contains("Concrete { id: TypeId(\"Boolean\")") {
                println!("🐛 CONFIRMED: Boolean literals are incorrectly inferring as 'Boolean' concrete type");
            }
        }
    }
}
