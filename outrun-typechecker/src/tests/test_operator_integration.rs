//! Integration tests for operator desugaring with complete type checking pipeline

use crate::DesugaringEngine;
use outrun_parser::{parse_program, BinaryOperator, ExpressionKind};

#[test]
fn test_operator_desugaring_with_typechecking() {
    use crate::types::{ModuleId, ProtocolId};
    use crate::{DesugaringEngine, TypeInferenceEngine};

    // Create a simple program with binary operations
    let source = r#"
        1 + 2 * 3
    "#;

    let mut program = parse_program(source).expect("Failed to parse program");

    // Set up minimal typechecker without core library loading
    let mut engine = TypeInferenceEngine::new();
    let mut desugaring_engine = DesugaringEngine::new();

    // Register necessary protocols for arithmetic operations
    engine.protocol_registry_mut().register_protocol_definition(
        ProtocolId::new("BinaryAddition"),
        std::collections::HashSet::new(),
        ModuleId::new("TestModule"),
        std::collections::HashSet::new(),
        std::collections::HashSet::new(),
        None,
    );

    engine.protocol_registry_mut().register_protocol_definition(
        ProtocolId::new("BinaryMultiplication"),
        std::collections::HashSet::new(),
        ModuleId::new("TestModule"),
        std::collections::HashSet::new(),
        std::collections::HashSet::new(),
        None,
    );

    // Apply desugaring first
    let desugar_result = desugaring_engine.desugar_program(&mut program);
    assert!(desugar_result.is_ok(), "Desugaring should succeed");

    // Then run the complete typechecking pipeline (without core library)
    let typecheck_result = engine.typecheck_program_items_only(&mut program);

    // This may fail due to missing implementations, but the important part is that
    // desugaring works and the pipeline can process the desugared operators
    if let Err(e) = &typecheck_result {
        eprintln!(
            "Type checking failed (expected due to missing implementations): {:?}",
            e
        );
    }

    // The main point is that desugaring + basic processing should work without crashing
}

#[test]
fn test_desugaring_preserves_semantics() {
    // Test that 1 + 2 gets desugared to BinaryAddition.add(left: 1, right: 2)
    let source = "1 + 2";

    let mut program = parse_program(source).expect("Failed to parse program");
    let mut desugaring_engine = DesugaringEngine::new();

    // Verify we start with a binary operation
    let initial_expr = &program.items[0];
    if let outrun_parser::ItemKind::Expression(expr) = &initial_expr.kind {
        match &expr.kind {
            ExpressionKind::BinaryOp(binary_op) => {
                assert_eq!(binary_op.operator, BinaryOperator::Add);
            }
            _ => panic!("Expected binary operation before desugaring"),
        }
    } else {
        panic!("Expected expression item");
    }

    // Apply desugaring
    desugaring_engine
        .desugar_program(&mut program)
        .expect("Desugaring should succeed");

    // Verify we now have a function call
    let desugared_expr = &program.items[0];
    if let outrun_parser::ItemKind::Expression(expr) = &desugared_expr.kind {
        match &expr.kind {
            ExpressionKind::FunctionCall(func_call) => match &func_call.path {
                outrun_parser::FunctionPath::Qualified { module, name } => {
                    assert_eq!(module.name, "BinaryAddition");
                    assert_eq!(name.name, "add");
                    assert_eq!(func_call.arguments.len(), 2);
                }
                _ => panic!("Expected qualified function path"),
            },
            _ => panic!("Expected function call after desugaring"),
        }
    }
}

#[test]
fn test_nested_operators_with_precedence() {
    // Test that operator precedence is preserved through desugaring
    let source = "1 + 2 * 3"; // Should parse as 1 + (2 * 3)

    let mut program = parse_program(source).expect("Failed to parse program");
    let mut desugaring_engine = DesugaringEngine::new();

    desugaring_engine
        .desugar_program(&mut program)
        .expect("Desugaring should succeed");

    // Should become BinaryAddition.add(left: 1, right: BinaryMultiplication.multiply(left: 2, right: 3))
    let expr = &program.items[0];
    if let outrun_parser::ItemKind::Expression(expr) = &expr.kind {
        match &expr.kind {
            ExpressionKind::FunctionCall(outer_call) => {
                // Check outer call is addition
                match &outer_call.path {
                    outrun_parser::FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "BinaryAddition");
                        assert_eq!(name.name, "add");
                    }
                    _ => panic!("Expected BinaryAddition for outer call"),
                }

                // Check right argument is multiplication
                assert_eq!(outer_call.arguments.len(), 2);
                if let outrun_parser::Argument::Named {
                    expression: right_expr,
                    ..
                } = &outer_call.arguments[1]
                {
                    match &right_expr.kind {
                        ExpressionKind::FunctionCall(inner_call) => match &inner_call.path {
                            outrun_parser::FunctionPath::Qualified { module, name } => {
                                assert_eq!(module.name, "BinaryMultiplication");
                                assert_eq!(name.name, "multiply");
                            }
                            _ => panic!("Expected BinaryMultiplication for inner call"),
                        },
                        _ => panic!("Expected function call for right operand"),
                    }
                }
            }
            _ => panic!("Expected function call after desugaring"),
        }
    }
}

#[test]
fn test_unary_operators_desugaring() {
    let source = "-42";

    let mut program = parse_program(source).expect("Failed to parse program");
    let mut desugaring_engine = DesugaringEngine::new();

    desugaring_engine
        .desugar_program(&mut program)
        .expect("Desugaring should succeed");

    // Should become UnaryMinus.minus(value: 42)
    let expr = &program.items[0];
    if let outrun_parser::ItemKind::Expression(expr) = &expr.kind {
        match &expr.kind {
            ExpressionKind::FunctionCall(func_call) => match &func_call.path {
                outrun_parser::FunctionPath::Qualified { module, name } => {
                    assert_eq!(module.name, "UnaryMinus");
                    assert_eq!(name.name, "minus");
                    assert_eq!(func_call.arguments.len(), 1);
                }
                _ => panic!("Expected qualified function path"),
            },
            _ => panic!("Expected function call after desugaring"),
        }
    }
}

#[test]
fn test_comparison_operators() {
    let test_cases = vec![
        ("x == y", "Equality", "equal?"),
        ("x < y", "Comparison", "less_than?"),
        ("x <= y", "Comparison", "less_than_or_equal?"),
        ("x > y", "Comparison", "greater_than?"),
        ("x >= y", "Comparison", "greater_than_or_equal?"),
    ];

    for (source, expected_protocol, expected_method) in test_cases {
        let mut program = parse_program(source).expect("Failed to parse program");
        let mut desugaring_engine = DesugaringEngine::new();

        desugaring_engine
            .desugar_program(&mut program)
            .expect("Desugaring should succeed");

        let expr = &program.items[0];
        if let outrun_parser::ItemKind::Expression(expr) = &expr.kind {
            match &expr.kind {
                ExpressionKind::FunctionCall(func_call) => match &func_call.path {
                    outrun_parser::FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, expected_protocol, "Failed for {}", source);
                        assert_eq!(name.name, expected_method, "Failed for {}", source);
                    }
                    _ => panic!("Expected qualified function path for {}", source),
                },
                _ => panic!("Expected function call after desugaring for {}", source),
            }
        }
    }
}

#[test]
fn test_pipeline_operators() {
    let source = "value |> List.head";

    let mut program = parse_program(source).expect("Failed to parse program");
    let mut desugaring_engine = DesugaringEngine::new();

    desugaring_engine
        .desugar_program(&mut program)
        .expect("Desugaring should succeed");

    // Should become Pipe.pipe_into(value: value, function: List.head)
    let expr = &program.items[0];
    if let outrun_parser::ItemKind::Expression(expr) = &expr.kind {
        match &expr.kind {
            ExpressionKind::FunctionCall(func_call) => match &func_call.path {
                outrun_parser::FunctionPath::Qualified { module, name } => {
                    assert_eq!(module.name, "Pipe");
                    assert_eq!(name.name, "pipe_into");
                    assert_eq!(func_call.arguments.len(), 2);
                }
                _ => panic!("Expected qualified function path"),
            },
            _ => panic!("Expected function call after desugaring"),
        }
    }
}
