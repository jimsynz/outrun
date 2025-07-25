//! Debug core boolean.outrun type checking failure

use crate::inference::TypeInferenceEngine;
use outrun_parser::parse_program;

#[test]
fn test_core_boolean_type_checking() {
    let mut engine = TypeInferenceEngine::new();

    // First, register the dependencies (protocols only)
    let boolean_protocol = r#"
protocol Boolean when Self: LogicalAnd && Self: LogicalOr && Self: LogicalNot {
    def true?(value: Self): Outrun.Core.Boolean
    def false?(value: Self): Outrun.Core.Boolean {
        !Boolean.true?(value: value)
    }
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

    let core_boolean_file = r#"
struct Outrun.Core.Boolean() {}

# Display protocol implementation
impl Display for Outrun.Core.Boolean {
    def to_string(value: Self): Outrun.Core.String {
        if value {
            "true"
        } else {
            "false"
        }
    }
}

# Equality protocol implementation
impl Equality for Outrun.Core.Boolean {
    def equal?(lhs: Self, rhs: Self): Outrun.Core.Boolean {
        Outrun.Intrinsic.bool_eq(lhs: lhs, rhs: rhs)
    }
}

# Logical operator protocol implementations
impl LogicalAnd for Outrun.Core.Boolean {
    def and(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.bool_and(lhs: lhs, rhs: rhs)
    }
}

impl LogicalOr for Outrun.Core.Boolean {
    def or(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.bool_or(lhs: lhs, rhs: rhs)
    }
}

impl LogicalNot for Outrun.Core.Boolean {
    def not?(value: Self): Self {
        Outrun.Intrinsic.bool_not(value: value)
    }
}

# Boolean protocol implementation
impl Boolean for Outrun.Core.Boolean {
    def true?(value: Self): Outrun.Core.Boolean {
        value
    }
}

# Default protocol implementation
impl Default for Outrun.Core.Boolean {
    def default(): Self {
        false
    }
}
"#;

    println!("=== Setting up dependencies ===");
    let mut deps_program =
        parse_program(boolean_protocol).expect("Parse dependencies should succeed");

    // Process through all phases for dependencies
    engine
        .register_protocols_and_structs(&deps_program)
        .expect("Phase 2 should succeed");
    engine
        .register_automatic_implementations(&deps_program)
        .expect("Phase 2.5 should succeed");
    engine
        .register_implementations(&deps_program)
        .expect("Phase 3 should succeed");
    engine
        .register_functions(&deps_program)
        .expect("Phase 4 should succeed");

    println!("=== Processing core boolean file ===");
    let mut boolean_program = parse_program(core_boolean_file).expect("Parse should succeed");

    // Process through all phases
    engine
        .register_protocols_and_structs(&boolean_program)
        .expect("Phase 2 should succeed");
    engine
        .register_automatic_implementations(&boolean_program)
        .expect("Phase 2.5 should succeed");
    engine
        .register_implementations(&boolean_program)
        .expect("Phase 3 should succeed");
    engine
        .register_functions(&boolean_program)
        .expect("Phase 4 should succeed");

    // This should reproduce the exact error from core boolean.outrun
    let result = engine.typecheck_function_bodies(&mut boolean_program);

    match result {
        Ok(()) => {
            println!("✅ Type checking succeeded");
        }
        Err(e) => {
            println!("❌ Type checking failed: {:?}", e);
            println!("Error details: {}", e);
        }
    }
}
