//! Test isolated core library files to identify the exact failure

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_single_core_file_isolated() {
    let mut engine = TypeInferenceEngine::new();

    println!("=== Testing single core file in isolation ===");

    // Create a minimal engine with just the protocols needed
    let protocol_deps = r#"
protocol Boolean when Self: LogicalAnd && Self: LogicalOr && Self: LogicalNot {
    def true?(value: Self): Outrun.Core.Boolean
}

protocol LogicalAnd {
    def and(lhs: Self, rhs: Self): Self
}

protocol LogicalOr {
    def or(lhs: Self, rhs: Self): Self
}

protocol LogicalNot {
    def not?(value: Self): Self
}

protocol Display {
    def to_string(value: Self): Outrun.Core.String
}

protocol Equality {
    def equal?(lhs: Self, rhs: Self): Outrun.Core.Boolean
}

protocol Default {
    def default(): Self
}

struct Outrun.Core.String() {}
"#;

    let boolean_file_content =
        std::fs::read_to_string("../outrun-core/lib/outrun/core/boolean.outrun")
            .expect("Should be able to read boolean.outrun");

    println!(
        "Boolean file content length: {}",
        boolean_file_content.len()
    );

    // Process protocol dependencies
    let mut deps_program = parse_program(protocol_deps).expect("Parse deps should succeed");

    engine
        .register_protocols_and_structs(&deps_program)
        .expect("Phase 2 deps should succeed");
    engine
        .register_automatic_implementations(&deps_program)
        .expect("Phase 2.5 deps should succeed");
    engine
        .register_implementations(&deps_program)
        .expect("Phase 3 deps should succeed");
    engine
        .register_functions(&deps_program)
        .expect("Phase 4 deps should succeed");

    // Process boolean file
    let mut boolean_program =
        parse_program(&boolean_file_content).expect("Parse boolean should succeed");

    println!("=== Phase 2: Register protocols and structs ===");
    match engine.register_protocols_and_structs(&boolean_program) {
        Ok(()) => println!("✅ Phase 2 succeeded"),
        Err(e) => {
            println!("❌ Phase 2 failed: {:?}", e);
            return;
        }
    }

    println!("=== Phase 2.5: Register automatic implementations ===");
    match engine.register_automatic_implementations(&boolean_program) {
        Ok(()) => println!("✅ Phase 2.5 succeeded"),
        Err(e) => {
            println!("❌ Phase 2.5 failed: {:?}", e);
            println!("This is likely the source of the ConflictingImplementation error");
            return;
        }
    }

    println!("=== Phase 3: Register implementations ===");
    match engine.register_implementations(&boolean_program) {
        Ok(()) => println!("✅ Phase 3 succeeded"),
        Err(e) => {
            println!("❌ Phase 3 failed: {:?}", e);
            return;
        }
    }

    println!("=== Phase 4: Register functions ===");
    match engine.register_functions(&boolean_program) {
        Ok(()) => println!("✅ Phase 4 succeeded"),
        Err(e) => {
            println!("❌ Phase 4 failed: {:?}", e);
            return;
        }
    }

    println!("=== Phase 6: Type check function bodies ===");
    match engine.typecheck_function_bodies(&mut boolean_program) {
        Ok(()) => println!("✅ Phase 6 succeeded - file type checks correctly!"),
        Err(e) => {
            println!("❌ Phase 6 failed: {:?}", e);
            println!("Error: {}", e);
        }
    }
}
