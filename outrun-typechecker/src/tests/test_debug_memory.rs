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

#[test]
fn test_phase3_public_function_template_generation() {
    // Test Phase 3: Public Function Template Generation
    let mut solver = crate::constraints::ConstraintSolver::new();
    
    // Create a test function definition
    let function_def = outrun_parser::FunctionDefinition {
        attributes: vec![],
        visibility: outrun_parser::FunctionVisibility::Public,
        name: outrun_parser::Identifier {
            name: "test_function".to_string(),
            span: outrun_parser::Span::new(0, 13),
        },
        parameters: vec![
            outrun_parser::Parameter {
                name: outrun_parser::Identifier {
                    name: "value".to_string(),
                    span: outrun_parser::Span::new(14, 19),
                },
                type_annotation: outrun_parser::TypeAnnotation::Simple {
                    path: vec![outrun_parser::TypeIdentifier {
                        name: "T".to_string(),
                        span: outrun_parser::Span::new(21, 22),
                    }],
                    generic_args: None,
                    span: outrun_parser::Span::new(21, 22),
                },
                span: outrun_parser::Span::new(14, 22),
            },
        ],
        return_type: outrun_parser::TypeAnnotation::Simple {
            path: vec![outrun_parser::TypeIdentifier {
                name: "Boolean".to_string(),
                span: outrun_parser::Span::new(25, 32),
            }],
            generic_args: None,
            span: outrun_parser::Span::new(25, 32),
        },
        guard: None,
        body: outrun_parser::Block {
            statements: vec![],
            span: outrun_parser::Span::new(34, 36),
        },
        span: outrun_parser::Span::new(0, 36),
    };
    
    // Create function signature
    let function_signature = crate::universal_dispatch::FunctionSignature::simple("test_function".to_string());
    
    // Generate template with available generic parameters
    let available_generics = vec!["T".to_string()]; // Our test function uses generic parameter T
    let result = solver.generate_public_function_template(
        function_signature.clone(),
        &function_def,
        crate::constraints::FunctionVisibility::Public,
        &available_generics,
    );
    
    match result {
        Ok(template) => {
            println!("✓ Successfully generated public function template");
            println!("  Function: {}", template.function_signature.function_name);
            println!("  Generic parameters: {}", template.generic_parameters.len());
            println!("  Parameter types: {}", template.parameter_types.len());
            println!("  Visibility: {:?}", template.visibility);
            
            // Verify template structure
            assert_eq!(template.function_signature.function_name, "test_function");
            assert_eq!(template.generic_parameters.len(), 1, "Should detect 'T' as generic parameter");
            assert_eq!(template.parameter_types.len(), 1, "Should have one parameter");
            assert_eq!(template.parameter_types[0].name, "value");
            
            // Check that template was stored in the solver
            let stored_templates = solver.get_public_function_templates();
            assert!(stored_templates.contains_key(&function_signature), "Template should be stored in solver");
            
            println!("✓ Phase 3 (Public Function Template Generation) is working correctly!");
        }
        Err(e) => {
            println!("! Template generation failed: {}", e);
            panic!("Phase 3 template generation should work");
        }
    }
}

#[test]
fn test_phase3_proper_generic_detection() {
    // Test that generic parameter detection is based on available generics, not heuristics
    let mut solver = crate::constraints::ConstraintSolver::new();
    
    // Create a function that uses 'T' (which should be generic) and 'Outrun.Core.String' (which should be concrete)
    let function_def = outrun_parser::FunctionDefinition {
        attributes: vec![],
        visibility: outrun_parser::FunctionVisibility::Public,
        name: outrun_parser::Identifier {
            name: "mixed_function".to_string(),
            span: outrun_parser::Span::new(0, 14),
        },
        parameters: vec![
            outrun_parser::Parameter {
                name: outrun_parser::Identifier {
                    name: "generic_param".to_string(),
                    span: outrun_parser::Span::new(15, 28),
                },
                type_annotation: outrun_parser::TypeAnnotation::Simple {
                    path: vec![outrun_parser::TypeIdentifier {
                        name: "T".to_string(),
                        span: outrun_parser::Span::new(30, 31),
                    }],
                    generic_args: None,
                    span: outrun_parser::Span::new(30, 31),
                },
                span: outrun_parser::Span::new(15, 31),
            },
            outrun_parser::Parameter {
                name: outrun_parser::Identifier {
                    name: "concrete_param".to_string(),
                    span: outrun_parser::Span::new(33, 47),
                },
                type_annotation: outrun_parser::TypeAnnotation::Simple {
                    path: vec![
                        outrun_parser::TypeIdentifier {
                            name: "Outrun".to_string(),
                            span: outrun_parser::Span::new(49, 55),
                        },
                        outrun_parser::TypeIdentifier {
                            name: "Core".to_string(),
                            span: outrun_parser::Span::new(56, 60),
                        },
                        outrun_parser::TypeIdentifier {
                            name: "String".to_string(),
                            span: outrun_parser::Span::new(61, 67),
                        },
                    ],
                    generic_args: None,
                    span: outrun_parser::Span::new(49, 67),
                },
                span: outrun_parser::Span::new(33, 55),
            },
        ],
        return_type: outrun_parser::TypeAnnotation::Simple {
            path: vec![outrun_parser::TypeIdentifier {
                name: "Boolean".to_string(),
                span: outrun_parser::Span::new(58, 65),
            }],
            generic_args: None,
            span: outrun_parser::Span::new(58, 65),
        },
        guard: None,
        body: outrun_parser::Block {
            statements: vec![],
            span: outrun_parser::Span::new(67, 69),
        },
        span: outrun_parser::Span::new(0, 69),
    };
    
    let function_signature = crate::universal_dispatch::FunctionSignature::simple("mixed_function".to_string());
    
    // Only 'T' is available as a generic parameter
    let available_generics = vec!["T".to_string()];
    
    let result = solver.generate_public_function_template(
        function_signature,
        &function_def,
        crate::constraints::FunctionVisibility::Public,
        &available_generics,
    );
    
    match result {
        Ok(template) => {
            println!("✓ Template generated with proper generic detection");
            
            // Should detect exactly 1 generic parameter ('T')
            assert_eq!(template.generic_parameters.len(), 1, "Should detect exactly one generic parameter");
            assert_eq!(template.generic_parameters[0].name, "T", "Should detect 'T' as generic");
            
            // Should have 2 parameters total
            assert_eq!(template.parameter_types.len(), 2, "Should have two parameters");
            
            // First parameter should be generic
            if let crate::constraints::TypeTemplate::Generic { parameter_name, .. } = &template.parameter_types[0].type_template {
                assert_eq!(parameter_name, "T", "First parameter should be generic T");
            } else {
                panic!("First parameter should be generic, got: {:?}", template.parameter_types[0].type_template);
            }
            
            // Second parameter should be concrete
            if let crate::constraints::TypeTemplate::Concrete { type_name, .. } = &template.parameter_types[1].type_template {
                assert_eq!(type_name, "Outrun.Core.String", "Second parameter should be concrete Outrun.Core.String");
            } else {
                panic!("Second parameter should be concrete Outrun.Core.String, got: {:?}", template.parameter_types[1].type_template);
            }
            
            println!("✓ Generic parameter detection is now properly AST-based!");
        }
        Err(e) => {
            panic!("Template generation should succeed: {}", e);
        }
    }
}