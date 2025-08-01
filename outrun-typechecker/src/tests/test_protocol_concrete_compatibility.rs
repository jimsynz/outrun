//! Test protocol-to-concrete type compatibility

use crate::{
    types::{ModuleName, ProtocolDefinition, Type, TypeModule},
    TypeInferenceEngine,
};
use outrun_parser::Span;

#[test]
fn test_boolean_protocol_concrete_compatibility() {
    let mut engine = TypeInferenceEngine::new();

    // Add modules as local to avoid orphan rule violations
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Boolean"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Outrun.Core.Boolean"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("TestModule"));

    // Register Boolean protocol
    engine
        .type_registry_mut()
        .register_module(TypeModule::Protocol {
            name: ModuleName::new("Boolean"),
            definition: ProtocolDefinition {
                protocol_name: ModuleName::new("Boolean"),
                required_protocols: std::collections::HashSet::new(),
                defining_module: ModuleName::new("TestModule"),
                default_implementations: std::collections::HashSet::new(),
                required_functions: std::collections::HashSet::new(),
                span: None,
            },
            source_location: Span::new(0, 0),
            generic_arity: 0,
        })
        .unwrap();

    // Register the implementation: Outrun.Core.Boolean implements Boolean
    engine
        .type_registry_mut()
        .register_implementation(
            ModuleName::new("Outrun.Core.Boolean"),
            vec![],
            ModuleName::new("Boolean"),
            vec![],
            ModuleName::new("Outrun.Core.Boolean"),
            None,
        )
        .expect("Should register Boolean implementation");

    // Test types_are_compatible
    let concrete_type = Type::concrete("Outrun.Core.Boolean");
    let protocol_type = Type::protocol("Boolean");

    // This should return true: Outrun.Core.Boolean implements Boolean
    let result = engine.test_types_are_compatible(&concrete_type, &protocol_type);

    println!("Concrete type: {:?}", concrete_type);
    println!("Protocol type: {:?}", protocol_type);
    println!("Compatible? {}", result);

    assert!(
        result,
        "Outrun.Core.Boolean should be compatible with Boolean protocol"
    );
}

#[test]
fn test_string_contains_scenario() {
    let mut engine = TypeInferenceEngine::new();

    // Set up the exact scenario from String.contains?

    // Add modules as local to avoid orphan rule violations
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Boolean"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Option"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("Outrun.Core.Boolean"));
    engine
        .type_registry_mut()
        .add_local_module(ModuleName::new("TestModule"));

    // 1. Register Boolean protocol
    engine
        .type_registry_mut()
        .register_module(TypeModule::Protocol {
            name: ModuleName::new("Boolean"),
            definition: ProtocolDefinition {
                protocol_name: ModuleName::new("Boolean"),
                required_protocols: std::collections::HashSet::new(),
                defining_module: ModuleName::new("TestModule"),
                default_implementations: std::collections::HashSet::new(),
                required_functions: std::collections::HashSet::new(),
                span: None,
            },
            source_location: Span::new(0, 0),
            generic_arity: 0,
        })
        .unwrap();

    // 2. Register Option protocol
    engine
        .type_registry_mut()
        .register_module(TypeModule::Protocol {
            name: ModuleName::new("Option"),
            definition: ProtocolDefinition {
                protocol_name: ModuleName::new("Option"),
                required_protocols: std::collections::HashSet::new(),
                defining_module: ModuleName::new("TestModule"),
                default_implementations: std::collections::HashSet::new(),
                required_functions: std::collections::HashSet::new(),
                span: None,
            },
            source_location: Span::new(0, 0),
            generic_arity: 1,
        })
        .unwrap();

    // 3. Register the implementation: Outrun.Core.Boolean implements Boolean
    engine
        .type_registry_mut()
        .register_implementation(
            ModuleName::new("Outrun.Core.Boolean"),
            vec![],
            ModuleName::new("Boolean"),
            vec![],
            ModuleName::new("Outrun.Core.Boolean"),
            None,
        )
        .expect("Should register Boolean implementation");

    // Test the exact compatibility issue:
    // Function declares return type Boolean (protocol)
    // But body returns true literal which infers as Outrun.Core.Boolean (concrete)
    let declared_return_type = Type::protocol("Boolean");
    let body_inferred_type = Type::concrete("Outrun.Core.Boolean");

    let compatible = engine.test_types_are_compatible(&body_inferred_type, &declared_return_type);

    println!("Declared return type: {:?}", declared_return_type);
    println!("Body inferred type: {:?}", body_inferred_type);
    println!("Compatible? {}", compatible);

    assert!(compatible, "Function body type Outrun.Core.Boolean should be compatible with declared return type Boolean");
}
