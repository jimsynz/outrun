//! Test generic type parameter resolution in struct and protocol definition contexts

#[cfg(test)]
mod generic_parameter_resolution_tests {
    use crate::inference::TypeInferenceEngine;
    use crate::types::{Level, ModuleId, ProtocolId, Type, TypeVarId};
    use outrun_parser::parse_program;

    #[test]
    fn test_impl_block_generic_parameter_context() {
        let mut engine = TypeInferenceEngine::new();

        // Test that generic parameters from impl syntax are properly extracted
        let source = r#"
            protocol TestProtocol<T> {
                def test_method(value: T): T
            }
            
            struct TestStruct<T>()
            
            impl TestProtocol<T> for TestStruct<T> {
                def test_method(value: T): T {
                    value
                }
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");

        // Phase 2: Register protocols and structs (should set up generic context correctly)
        engine
            .register_protocols_and_structs(&program)
            .expect("Phase 2 should succeed");

        // Phase 2.5: Register automatic implementations
        engine
            .register_automatic_implementations(&program)
            .expect("Phase 2.5 should succeed");

        // Phase 3: Register implementations (should handle generic contexts)
        let result = engine.register_implementations(&program);
        assert!(
            result.is_ok(),
            "Implementation registration should succeed with generic parameters"
        );
    }

    #[test]
    fn test_generic_parameter_type_variable_creation() {
        let mut engine = TypeInferenceEngine::new();

        // Test that generic parameters create proper type variables
        let param_names = vec!["T".to_string(), "U".to_string(), "V".to_string()];
        let context = engine.create_generic_context_from_names(&param_names);

        assert_eq!(context.len(), 3, "Should create context for all parameters");

        // Check that T, U, V are all type variables
        for param_name in &param_names {
            let param_type = context
                .get(param_name)
                .expect("Parameter should exist in context");
            match param_type {
                Type::Variable {
                    var_id: _,
                    level: Level(0),
                    name: Some(name),
                    span: None,
                } => {
                    assert_eq!(
                        name, param_name,
                        "Variable name should match parameter name"
                    );
                }
                _ => panic!(
                    "Generic parameter should be a type variable, got: {:?}",
                    param_type
                ),
            }
        }
    }

    #[test]
    fn test_convert_type_annotation_with_generics() {
        let mut engine = TypeInferenceEngine::new();

        // Set up a generic parameter context
        engine.add_generic_parameter(
            "T".to_string(),
            Type::Variable {
                var_id: TypeVarId(42),
                level: Level(0),
                name: Some("T".to_string()),
                span: None,
            },
        );

        // Test parsing a simple type annotation "T"
        let source = r#"def test_func(param: T): T { param }"#;
        let program = parse_program(source).expect("Parse should succeed");

        if let Some(item) = program.items.first() {
            if let outrun_parser::ItemKind::FunctionDefinition(func_def) = &item.kind {
                // Convert the parameter type annotation
                let param_type_result =
                    engine.convert_type_annotation(&func_def.parameters[0].type_annotation);
                assert!(
                    param_type_result.is_ok(),
                    "Should convert T to type variable"
                );

                let param_type = param_type_result.unwrap();
                match param_type {
                    Type::Variable {
                        var_id: TypeVarId(42),
                        ..
                    } => {
                        // Success - T was resolved to the type variable we set up
                    }
                    _ => panic!(
                        "T should resolve to the type variable in context, got: {:?}",
                        param_type
                    ),
                }

                // Convert the return type annotation
                let return_type_result = engine.convert_type_annotation(&func_def.return_type);
                assert!(
                    return_type_result.is_ok(),
                    "Should convert return T to type variable"
                );

                let return_type = return_type_result.unwrap();
                match return_type {
                    Type::Variable {
                        var_id: TypeVarId(42),
                        ..
                    } => {
                        // Success - return T was also resolved to the same type variable
                    }
                    _ => panic!(
                        "Return T should resolve to the same type variable, got: {:?}",
                        return_type
                    ),
                }
            }
        }
    }

    #[test]
    fn test_generic_type_with_arguments() {
        let mut engine = TypeInferenceEngine::new();

        // Register the Option protocol so it's recognized as a protocol, not concrete type
        let option_protocol_id = ProtocolId::new("Option");
        engine.protocol_registry_mut().register_protocol_definition(
            option_protocol_id.clone(),
            std::collections::HashSet::new(), // No required protocols
            ModuleId::new("TestModule"),
            std::collections::HashSet::new(), // No default implementations
            std::collections::HashSet::new(), // No required functions
            None,                             // No span
        );

        // Set up a generic parameter context
        engine.add_generic_parameter(
            "T".to_string(),
            Type::Variable {
                var_id: TypeVarId(99),
                level: Level(0),
                name: Some("T".to_string()),
                span: None,
            },
        );

        // Test parsing "Option<T>" where T is a generic parameter
        let source = r#"def test_func(param: Option<T>): Option<T> { param }"#;
        let program = parse_program(source).expect("Parse should succeed");

        if let Some(item) = program.items.first() {
            if let outrun_parser::ItemKind::FunctionDefinition(func_def) = &item.kind {
                // Convert the parameter type annotation "Option<T>"
                let param_type_result =
                    engine.convert_type_annotation(&func_def.parameters[0].type_annotation);
                assert!(
                    param_type_result.is_ok(),
                    "Should convert Option<T> successfully"
                );

                let param_type = param_type_result.unwrap();
                match param_type {
                    Type::Protocol {
                        id: ProtocolId(ref protocol_name),
                        ref args,
                        ..
                    } => {
                        assert_eq!(protocol_name, "Option", "Should be Option protocol");
                        assert_eq!(args.len(), 1, "Should have one type argument");

                        // Check that the argument is the T type variable
                        match &args[0] {
                            Type::Variable {
                                var_id: TypeVarId(99),
                                ..
                            } => {
                                // Success - T was resolved correctly within Option<T>
                            }
                            _ => panic!(
                                "Option<T> argument should be the T type variable, got: {:?}",
                                args[0]
                            ),
                        }
                    }
                    _ => panic!("Option<T> should be a Protocol type, got: {:?}", param_type),
                }
            }
        }
    }
}
