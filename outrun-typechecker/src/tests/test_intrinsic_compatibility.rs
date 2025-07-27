//! Test intrinsic function compatibility with Self types

#[cfg(test)]
mod intrinsic_compatibility_tests {
    use crate::inference::TypeInferenceEngine;
    use crate::types::{ProtocolId, SelfBindingContext, Type, TypeId};
    use outrun_parser::parse_program;

    #[test]
    fn test_self_type_resolution() {

        // Create a SelfType with Implementation context like in the error
        let self_type = Type::SelfType {
            binding_context: SelfBindingContext::Implementation {
                implementing_type: TypeId::new("Outrun.Core.List"),
                implementing_args: vec![],
                protocol_id: ProtocolId::new("List"),
                protocol_args: vec![],
            },
            span: None,
        };

        // Check if resolve_self works
        let resolved = self_type.resolve_self();
        assert!(
            resolved.is_some(),
            "SelfType should resolve in Implementation context"
        );

        let resolved_type = resolved.unwrap();
        match resolved_type {
            Type::Concrete { id, .. } => {
                assert_eq!(
                    id.name(),
                    "Outrun.Core.List",
                    "Should resolve to Outrun.Core.List"
                );
            }
            _ => panic!("SelfType should resolve to Concrete type"),
        }
    }

    #[test]
    fn test_any_protocol_compatibility() {
        let mut engine = TypeInferenceEngine::new();

        // Set up a simple struct that should automatically implement Any
        let source = r#"struct TestStruct()"#;
        let program = parse_program(source).expect("Parse should succeed");
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

        // Create types for the compatibility test
        let concrete_type = Type::Concrete {
            id: TypeId::new("TestStruct"),
            args: vec![],
            span: None,
        };
        let any_protocol = Type::Protocol {
            id: ProtocolId::new("Any"),
            args: vec![],
            span: None,
        };

        // Test if concrete type is compatible with Any protocol
        let is_compatible = engine.test_types_are_compatible(&concrete_type, &any_protocol);
        assert!(
            is_compatible,
            "TestStruct should be compatible with Any protocol via automatic implementation"
        );
    }

    #[test]
    fn test_self_to_any_compatibility() {
        let mut engine = TypeInferenceEngine::new();

        // Set up List type
        let source = r#"struct Outrun.Core.List<T>()"#;
        let program = parse_program(source).expect("Parse should succeed");
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

        // Create the exact types from the error
        let self_type = Type::SelfType {
            binding_context: SelfBindingContext::Implementation {
                implementing_type: TypeId::new("Outrun.Core.List"),
                implementing_args: vec![],
                protocol_id: ProtocolId::new("List"),
                protocol_args: vec![],
            },
            span: None,
        };

        let any_protocol = Type::Protocol {
            id: ProtocolId::new("Any"),
            args: vec![],
            span: None,
        };

        // Test compatibility in both directions to understand the issue
        let is_compatible_1 = engine.test_types_are_compatible(&any_protocol, &self_type);
        println!(
            "Compatibility result: any_protocol -> self_type = {}",
            is_compatible_1
        );

        let is_compatible_2 = engine.test_types_are_compatible(&self_type, &any_protocol);
        println!(
            "Compatibility result: self_type -> any_protocol = {}",
            is_compatible_2
        );

        // Also test the reverse (which is what should happen after self resolution)
        if let Some(resolved_self) = self_type.resolve_self() {
            let forward_compatible =
                engine.test_types_are_compatible(&any_protocol, &resolved_self);
            println!(
                "Compatibility result: any_protocol -> resolved_self = {}",
                forward_compatible
            );

            let reverse_compatible =
                engine.test_types_are_compatible(&resolved_self, &any_protocol);
            println!(
                "Compatibility result: resolved_self -> any_protocol = {}",
                reverse_compatible
            );

            // Check if Outrun.Core.List implements Any
            if let Type::Concrete { id, .. } = &resolved_self {
                let implements_any = engine
                    .get_protocol_registry()
                    .type_satisfies_protocol(id, &ProtocolId::new("Any"));
                println!("Outrun.Core.List implements Any: {}", implements_any);
            }
        }

        assert!(
            is_compatible_2,
            "Self type should be compatible with Any protocol (Outrun.Core.List)"
        );
    }
}
