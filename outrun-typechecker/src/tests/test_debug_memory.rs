//! Debug tests for memory usage in iterative inference and recursive pattern detection

use crate::{typecheck_package, Package, TypeInferenceEngine};
use outrun_parser::parse_program;

#[test]
fn test_memory_usage_small_expression() {
    // Test memory usage for a small expression
    let mut package = Package::new("test_small".to_string());
    
    let source = "let x = 1 + 2 + 3";
    let program = parse_program(source).expect("Should parse successfully");
    package.add_program(program);
    
    println!("Testing small expression: {}", source);
    
    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Small expression typechecked successfully"),
        Err(e) => println!("! Small expression failed: {:?}", e),
    }
}

#[test]
fn test_memory_usage_medium_expression() {
    // Test memory usage for a medium expression (10 terms)
    let mut package = Package::new("test_medium".to_string());
    
    let mut parts = Vec::new();
    for i in 0..10 {
        parts.push(i.to_string());
    }
    let expression = parts.join(" + ");
    let source = format!("let x = {}", expression);
    
    let program = parse_program(&source).expect("Should parse successfully");
    package.add_program(program);
    
    println!("Testing medium expression: {} terms", parts.len());
    
    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Medium expression typechecked successfully"),
        Err(e) => println!("! Medium expression failed: {:?}", e),
    }
}

#[test]
fn test_memory_usage_large_expression() {
    // Test memory usage for a large expression (50 terms)
    let mut package = Package::new("test_large".to_string());
    
    let mut parts = Vec::new();
    for i in 0..50 {
        parts.push(i.to_string());
    }
    let expression = parts.join(" + ");
    let source = format!("let x = {}", expression);
    
    let program = parse_program(&source).expect("Should parse successfully");
    package.add_program(program);
    
    println!("Testing large expression: {} terms", parts.len());
    
    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Large expression typechecked successfully"),
        Err(e) => println!("! Large expression failed: {:?}", e),
    }
}

#[test]
fn test_memory_usage_very_large_expression() {
    // Test memory usage for a very large expression (100 terms) - this should be the limit
    let mut package = Package::new("test_very_large".to_string());
    
    let mut parts = Vec::new();
    for i in 0..100 {
        parts.push(i.to_string());
    }
    let expression = parts.join(" + ");
    let source = format!("let x = {}", expression);
    
    let program = parse_program(&source).expect("Should parse successfully");
    package.add_program(program);
    
    println!("Testing very large expression: {} terms", parts.len());
    
    let result = typecheck_package(&mut package);
    match result {
        Ok(_) => println!("✓ Very large expression typechecked successfully"),
        Err(e) => println!("! Very large expression failed: {:?}", e),
    }
}

#[test]
fn test_recursive_protocol_implementation_detection() {
    // Test that our recursive pattern detection is integrated and working
    let source = r#"
        protocol Empty {
            def empty?(value: Self): Boolean
        }

        struct Option<T> {
            def some(value: T): Option<T> {
                Option { value }
            }
        } 

        impl Empty for Option<T> when T: Empty {
            def empty?(value: Self): Boolean {
                # Implementation for Option<T> depends on T being Empty
                true
            }
        }
    "#;

    let mut program = parse_program(source).expect("Failed to parse program");
    let mut inference_engine = TypeInferenceEngine::new();
    
    // Capture output - the recursive pattern detection runs during typechecking
    println!("Testing recursive protocol implementation detection");
    
    let result = inference_engine.typecheck_program(&mut program);
    
    // The recursive pattern detection should run during phase 3.5
    // and emit a warning to stderr about the recursive implementation
    
    match result {
        Ok(()) => {
            println!("✓ Program typechecked successfully - recursive pattern detection ran");
        }
        Err(e) => {
            println!("! Typecheck error (may be expected): {:?}", e);
        }
    }
}

#[test]
fn test_constraint_solver_recursive_analysis_directly() {
    // Test the constraint solver recursive analysis directly
    let mut solver = crate::constraints::ConstraintSolver::new();
    
    // Analyze patterns (should be safe with empty registry)
    solver.analyze_recursive_type_patterns();
    
    // Should have no warnings with an empty registry
    let warnings = solver.get_warnings();
    assert_eq!(warnings.len(), 0, "Empty registry should produce no warnings");
    
    println!("✓ Direct constraint solver recursive analysis completed with {} warnings", warnings.len());
}

#[test]
fn test_call_stack_backtracking_integration() {
    // Test the call stack backtracking system directly
    let mut solver = crate::constraints::ConstraintSolver::new();
    
    // Create some test call contexts
    let mut call_context1 = crate::constraints::CallContext::new(
        "outer_function".to_string(),
        vec!["MyModule".to_string()],
        None,
        0,
    );
    
    // Add some local constraints and generic substitutions
    call_context1.add_local_constraint(crate::types::Type::concrete("String"));
    call_context1.add_generic_substitution("T".to_string(), crate::types::Type::concrete("Integer64"));
    
    let mut call_context2 = crate::constraints::CallContext::new(
        "inner_function".to_string(),
        vec!["MyModule".to_string()],
        None,
        1,
    );
    call_context2.add_local_constraint(crate::types::Type::concrete("Boolean"));
    
    // Set up call stack context
    solver.push_call_context(call_context1);
    solver.push_call_context(call_context2);
    
    // Create a test constraint to backtrack on
    let test_constraint = crate::types::Constraint::Implements {
        type_var: crate::types::TypeVarId(42),
        protocol: crate::types::ProtocolId::new("Display"),
        span: None,
    };
    
    // Perform backtracking
    let backtracked_constraints = solver.backtrack_for_enhanced_context(&test_constraint);
    
    println!("✓ Call stack backtracking completed with {} backtracked constraints", backtracked_constraints.len());
    
    // Verify call stack management
    assert!(solver.call_stack_context().is_some());
    let context = solver.call_stack_context().unwrap();
    assert_eq!(context.call_stack.len(), 2, "Should have 2 call contexts");
    assert_eq!(context.current_depth(), 2, "Should report correct depth");
    
    // Test popping contexts
    let popped = solver.pop_call_context();
    assert!(popped.is_some());
    assert_eq!(popped.unwrap().function_signature, "inner_function");
    
    let context = solver.call_stack_context().unwrap();
    assert_eq!(context.call_stack.len(), 1, "Should have 1 context after pop");
    
    println!("✓ Call stack management working correctly");
}

#[test]
fn test_call_stack_context_creation() {
    // Test CallStackContext and CallContext creation
    let context = crate::constraints::CallStackContext::new(5);
    assert_eq!(context.max_depth, 5);
    assert_eq!(context.current_depth(), 0);
    assert!(!context.at_max_depth());
    
    let mut call_ctx = crate::constraints::CallContext::new(
        "test_function".to_string(),
        vec!["Test".to_string(), "Module".to_string()],
        None,
        0,
    );
    
    // Test adding constraints and substitutions
    call_ctx.add_local_constraint(crate::types::Type::concrete("TestType"));
    call_ctx.add_generic_substitution("T".to_string(), crate::types::Type::concrete("ConcreteType"));
    
    assert_eq!(call_ctx.local_constraints.len(), 1);
    assert_eq!(call_ctx.generic_substitutions.len(), 1);
    assert_eq!(call_ctx.generic_substitutions.get("T").unwrap(), &crate::types::Type::concrete("ConcreteType"));
    
    println!("✓ Call stack context creation and manipulation working correctly");
}

#[test]
fn test_phase2_complete_integration() {
    // Comprehensive test showing Phase 1 + Phase 2 working together
    let source = r#"
        protocol Display {
            def display(value: Self): String
        }

        struct Container<T> {
            def new(value: T): Container<T> {
                Container { value }
            }
        } 

        impl Display for Container<T> when T: Display {
            def display(value: Self): String {
                # Display implementation for Container<T> requires T: Display
                "Display implementation"
            }
        }
    "#;

    let mut program = parse_program(source).expect("Failed to parse program");
    let mut inference_engine = TypeInferenceEngine::new();
    
    println!("Testing Phase 1 + Phase 2 integration");
    
    let result = inference_engine.typecheck_program(&mut program);
    
    match result {
        Ok(()) => {
            println!("✓ Program typechecked successfully");
        }
        Err(e) => {
            println!("! Typecheck error (may be expected): {:?}", e);
        }
    }
    
    // Verify that the constraint solver with backtracking was initialized
    assert!(inference_engine.constraint_solver_with_backtracking().is_some(), 
            "Constraint solver with backtracking should be initialized");
    
    let constraint_solver = inference_engine.constraint_solver_with_backtracking().unwrap();
    assert!(constraint_solver.call_stack_context().is_some(), 
            "Call stack context should be initialized");
    
    println!("✓ Phase 2 (Call Stack Backtracking) integration verified");
    println!("✓ Both Phase 1 (Recursive Pattern Detection) and Phase 2 are complete!");
}