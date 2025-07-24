//! Debug tests for expression inference to understand what's happening

#[cfg(test)]
mod expression_inference_debug_tests {
    use crate::inference::TypeInferenceEngine;
    use outrun_parser::parse_program;

    /// Helper to create a type inference engine for testing with basic protocol setup
    fn create_test_engine() -> TypeInferenceEngine {
        let mut engine = TypeInferenceEngine::new();
        
        // Set up basic protocol requirements for testing
        use std::collections::HashSet;
        use crate::types::{ProtocolId, ModuleId, TypeId};
        
        let registry = engine.protocol_registry_mut();
        
        // Add local modules for orphan rule compliance
        registry.add_local_module(ModuleId::new("Integer"));
        registry.add_local_module(ModuleId::new("BinaryAddition"));
        registry.add_local_module(ModuleId::new("Equality"));
        registry.add_local_module(ModuleId::new("Outrun.Core.Integer64"));
        
        // Register Integer protocol with BinaryAddition requirement
        let mut integer_requirements = HashSet::new();
        integer_requirements.insert(ProtocolId::new("BinaryAddition"));
        integer_requirements.insert(ProtocolId::new("Equality"));
        
        registry.register_protocol_definition(
            ProtocolId::new("Integer"),
            integer_requirements,
            ModuleId::new("Integer"),
            HashSet::new(), // default_implementations
            HashSet::new(), // required_functions  
            None,
        );
        
        // Register a concrete type that implements Integer and its requirements
        let integer64_type = TypeId::new("Outrun.Core.Integer64");
        
        registry.register_implementation(
            integer64_type.clone(),
            vec![],
            ProtocolId::new("Integer"),
            vec![],
            ModuleId::new("Integer"),
            None,
        ).ok(); // Ignore errors for test setup
        
        registry.register_implementation(
            integer64_type.clone(),
            vec![],
            ProtocolId::new("BinaryAddition"),
            vec![],
            ModuleId::new("BinaryAddition"),
            None,
        ).ok();
        
        registry.register_implementation(
            integer64_type,
            vec![],
            ProtocolId::new("Equality"),
            vec![],
            ModuleId::new("Equality"),
            None,
        ).ok();
        
        engine
    }

    #[test]
    fn test_debug_simple_binary_operation() {
        let source = r#"
            def add(a: Integer, b: Integer): Integer {
                a + b
            }
        "#;

        let mut program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // First collect definitions
        engine.register_protocols_and_structs(&program).expect("Phase 2 should succeed");
        engine.register_implementations(&program).expect("Phase 3 should succeed");
        engine.register_functions(&program).expect("Phase 4 should succeed");
        // All phases completed successfully
        // All phases completed successfully
        
        // Parse what the binary operation `a + b` should look like after desugaring
        let binary_expr_source = "a + b";
        let binary_expr = outrun_parser::parse_expression(binary_expr_source);
        println!("Parsed binary expression: {:?}", binary_expr);
        
        // Check if desugaring is working
        let desugared_source = "BinaryAddition.add(left: a, right: b)";
        let desugared_expr = outrun_parser::parse_expression(desugared_source);
        println!("Parsed desugared expression: {:?}", desugared_expr);
        
        // Then try to typecheck the function body
        if let Some(item) = program.items.first_mut() {
            let result = engine.typecheck_item(item);
            println!("Function body typecheck result: {:?}", result);
            
            // For now, let's see what kind of error we get
            match result {
                Ok(_) => println!("✅ Type checking succeeded!"),
                Err(e) => println!("❌ Type checking failed: {:?}", e),
            }
        }
    }

    #[test]
    fn test_debug_desugared_protocol_call() {
        let source = r#"
            def add(a: Integer, b: Integer): Integer {
                BinaryAddition.add(left: a, right: b)
            }
        "#;

        let mut program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // Register the BinaryAddition.add function that protocol calls should resolve to
        use crate::dispatch::{FunctionInfo, FunctionVisibility};
        use crate::types::Type;
        
        let integer_type = Type::Protocol { 
            id: crate::types::ProtocolId::new("Integer"),
            args: vec![],
            span: None,
        };
        let function_info = FunctionInfo {
            defining_scope: "BinaryAddition".to_string(),
            function_name: "add".to_string(),
            visibility: FunctionVisibility::Public,
            parameters: vec![
                ("left".to_string(), integer_type.clone()),
                ("right".to_string(), integer_type.clone()),
            ],
            return_type: integer_type.clone(),
            span: None,
        };
        
        engine.function_registry_mut().register_function(
            "BinaryAddition".to_string(),
            "add".to_string(),
            function_info,
        );
        
        // First collect definitions
        engine.register_protocols_and_structs(&program).expect("Phase 2 should succeed");
        engine.register_implementations(&program).expect("Phase 3 should succeed");
        engine.register_functions(&program).expect("Phase 4 should succeed");
        // All phases completed successfully
        
        // Then try to typecheck the function body with explicit protocol call
        if let Some(item) = program.items.first_mut() {
            let result = engine.typecheck_item(item);
            println!("Explicit protocol call result: {:?}", result);
            
            match result {
                Ok(_) => println!("✅ Explicit protocol call succeeded!"),
                Err(e) => println!("❌ Explicit protocol call failed: {:?}", e),
            }
        }
    }
    
    #[test]
    fn test_debug_what_happens_to_binary_operation() {
        // Let's trace exactly what happens when we have a + b in a function body
        let source = r#"
            def add(a: Integer, b: Integer): Integer {
                a + b
            }
        "#;

        let mut program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // First collect definitions
        engine.register_protocols_and_structs(&program).expect("Phase 2 should succeed");
        engine.register_implementations(&program).expect("Phase 3 should succeed");
        engine.register_functions(&program).expect("Phase 4 should succeed");
        // All phases completed successfully
        
        // Let's see what the AST looks like for the function body before and after desugaring
        if let Some(item) = program.items.first_mut() {
            if let outrun_parser::ItemKind::FunctionDefinition(func_def) = &mut item.kind {
                let body = &mut func_def.body;
                if let Some(statement) = body.statements.first_mut() {
                    if let outrun_parser::StatementKind::Expression(expr) = &mut statement.kind {
                        println!("Original function body AST: {:#?}", expr);
                        
                        // Let's try to desugar this expression to see what it becomes
                        use crate::desugaring::DesugaringEngine;
                        let mut desugaring_engine = DesugaringEngine::new();
                        match desugaring_engine.desugar_expression(expr) {
                            Ok(_) => {
                                println!("After desugaring AST: {:#?}", expr);
                            }
                            Err(e) => {
                                println!("Desugaring failed: {:?}", e);
                            }
                        }
                    }
                }
            }
        }
        
        // Then try to typecheck the function body
        if let Some(item) = program.items.first_mut() {
            let result = engine.typecheck_item(item);
            println!("Final typecheck result: {:?}", result);
            
            match result {
                Ok(_) => println!("✅ Binary operation type checking succeeded!"),
                Err(e) => println!("❌ Binary operation type checking failed: {:?}", e),
            }
        }
    }
}