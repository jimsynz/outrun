use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_debug_boolean_protocol_to_concrete() {
    let input = r#"
# This reproduces the core library issue
protocol Boolean {
    def true?(value: Self): Boolean
}

struct Outrun.Core.Boolean() {}

# Outrun.Core.Boolean should implement Boolean
impl Boolean for Outrun.Core.Boolean {
    def true?(value: Self): Boolean {
        true  # This should be compatible
    }
}

# Function declaring Boolean protocol return type but returning boolean literal
def test_function(): Boolean {
    true  # This should be Outrun.Core.Boolean -> Boolean protocol (should work)
}
"#;

    let mut program = parse_program(input).expect("Parse should succeed");
    let mut engine = TypeInferenceEngine::new();

    println!("🔍 Testing boolean protocol-to-concrete compatibility...");

    // Test the type checking and see what type 'true' gets inferred as
    engine
        .register_protocols_and_structs(&program)
        .expect("Phase 2 should succeed");
    engine
        .register_implementations(&program)
        .expect("Phase 3 should succeed");
    engine
        .register_functions(&program)
        .expect("Phase 4 should succeed");
    // All phases completed successfully

    // All phases completed, check for errors in type checking
    {
        let registry_result = engine.register_implementations(&program);
        println!("Implementation registration result: {:?}", registry_result);

        if registry_result.is_ok() {
            let typecheck_result = engine.typecheck_program_items_only(&mut program);
            println!("Type checking result: {:?}", typecheck_result);

            // If type checking failed, show detailed error
            if let Err(error) = &typecheck_result {
                println!("Detailed error: {:#?}", error);
            }
        }
    }
}

#[test]
fn test_self_type_resolution_in_impl() {
    let input = r#"
struct Outrun.Core.Integer64() {}

impl Integer for Outrun.Core.Integer64 {
    def abs(value: Self): Self {
        value
    }
}
"#;

    let mut program = parse_program(input).expect("Parse should succeed");
    let mut engine = TypeInferenceEngine::new();

    println!("🔍 Testing Self type resolution...");

    let _ = engine
        .register_protocols_and_structs(&program)
        .expect("Phase 2 should succeed");
    engine
        .register_automatic_implementations(&program)
        .expect("Phase 2.5 should succeed");
    engine
        .register_implementations(&program)
        .expect("Phase 3 should succeed");
    engine
        .register_functions(&program)
        .expect("Phase 4 should succeed");
    let _ = engine.register_implementations(&program);
    let result = engine.typecheck_program_items_only(&mut program);

    println!("Self resolution result: {:?}", result);
}

#[test]
fn test_boolean_type_consistency() {
    let input = r#"
struct Outrun.Core.Boolean() {}

impl Integer for Outrun.Core.Integer64 {
    def zero?(value: Self): Boolean {
        true
    }
}
"#;

    let mut program = parse_program(input).expect("Parse should succeed");
    let mut engine = TypeInferenceEngine::new();

    println!("🔍 Testing Boolean type consistency...");

    let _ = engine
        .register_protocols_and_structs(&program)
        .expect("Phase 2 should succeed");
    engine
        .register_automatic_implementations(&program)
        .expect("Phase 2.5 should succeed");
    engine
        .register_implementations(&program)
        .expect("Phase 3 should succeed");
    engine
        .register_functions(&program)
        .expect("Phase 4 should succeed");
    let _ = engine.register_implementations(&program);
    let result = engine.typecheck_program_items_only(&mut program);

    println!("Boolean type result: {:?}", result);
}

#[test]
fn test_protocol_parameter_types() {
    let input = r#"
struct Outrun.Core.Integer64() {}

impl Integer for Outrun.Core.Integer64 {
    def to_string_radix(value: Self, radix: Integer): Outrun.Core.String {
        "test"
    }
}
"#;

    let mut program = parse_program(input).expect("Parse should succeed");
    let mut engine = TypeInferenceEngine::new();

    println!("🔍 Testing protocol parameter types...");

    let _ = engine
        .register_protocols_and_structs(&program)
        .expect("Phase 2 should succeed");
    engine
        .register_automatic_implementations(&program)
        .expect("Phase 2.5 should succeed");
    engine
        .register_implementations(&program)
        .expect("Phase 3 should succeed");
    engine
        .register_functions(&program)
        .expect("Phase 4 should succeed");
    let _ = engine.register_implementations(&program);
    let result = engine.typecheck_program_items_only(&mut program);

    println!("Protocol parameter result: {:?}", result);
}
