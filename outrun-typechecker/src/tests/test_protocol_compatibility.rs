//! Test enhanced protocol compatibility checking

#[cfg(test)]
mod protocol_compatibility_tests {
    use crate::inference::TypeInferenceEngine;
    use crate::types::{ProtocolId, Type};

    #[test]
    fn test_generic_protocol_compatibility() {
        let engine = TypeInferenceEngine::new();

        // Test: Option<Any> should be compatible with Option<Any>
        let option_any1 = Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![Type::Protocol {
                id: ProtocolId::new("Any"),
                args: vec![],
                span: None,
            }],
            span: None,
        };
        let option_any2 = Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![Type::Protocol {
                id: ProtocolId::new("Any"),
                args: vec![],
                span: None,
            }],
            span: None,
        };

        assert!(
            engine.test_types_are_compatible(&option_any1, &option_any2),
            "Option<Any> should be compatible with Option<Any>"
        );
    }

    #[test]
    fn test_protocol_subtyping_compatibility() {
        let mut engine = TypeInferenceEngine::new();

        // Create a struct that should automatically implement Any
        let source = r#"struct User(name: String)"#;
        let program = outrun_parser::parse_program(source).expect("Parse should succeed");
        engine
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
        engine
            .register_implementations(&program)
            .expect("Implementation registration should succeed");

        // Test: User should be compatible with Any
        let user_type = Type::Concrete {
            id: crate::types::TypeId::new("User"),
            args: vec![],
            span: None,
        };
        let any_type = Type::Protocol {
            id: ProtocolId::new("Any"),
            args: vec![],
            span: None,
        };

        assert!(
            engine.test_types_are_compatible(&user_type, &any_type),
            "User (concrete type) should be compatible with Any (protocol type) via automatic implementation"
        );

        // Test: Option<User> should be compatible with Option<Any>
        let option_user = Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![user_type],
            span: None,
        };
        let option_any = Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![any_type],
            span: None,
        };

        assert!(
            engine.test_types_are_compatible(&option_user, &option_any),
            "Option<User> should be compatible with Option<Any> via generic protocol subtyping"
        );
    }

    #[test]
    fn test_protocol_argument_count_mismatch() {
        let engine = TypeInferenceEngine::new();

        // Test: Option<Any> should NOT be compatible with Option (no args)
        let option_any = Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![Type::Protocol {
                id: ProtocolId::new("Any"),
                args: vec![],
                span: None,
            }],
            span: None,
        };
        let option_no_args = Type::Protocol {
            id: ProtocolId::new("Option"),
            args: vec![],
            span: None,
        };

        assert!(
            !engine.test_types_are_compatible(&option_any, &option_no_args),
            "Option<Any> should NOT be compatible with Option (no type arguments)"
        );
    }
}
