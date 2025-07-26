//! Tests for static function dispatch resolution in Outrun's functional model

use crate::dispatch::{
    build_dispatch_table, DispatchResult, FunctionContext, FunctionDispatcher, FunctionInfo,
    FunctionRegistry, FunctionVisibility,
};
use crate::registry::ProtocolRegistry;
use crate::types::{ModuleId, ProtocolId, Type, TypeId};

fn create_test_protocol_registry() -> ProtocolRegistry {
    let mut registry = ProtocolRegistry::new();

    // Set up local modules (simulating current package)
    // Include Outrun.Core modules as local for testing
    registry.add_local_module(ModuleId::new("String"));
    registry.add_local_module(ModuleId::new("Integer"));
    registry.add_local_module(ModuleId::new("List"));
    registry.add_local_module(ModuleId::new("User"));
    registry.add_local_module(ModuleId::new("Equality"));
    registry.add_local_module(ModuleId::new("Display"));
    registry.add_local_module(ModuleId::new("Debug"));

    // Add Outrun.Core modules as local to avoid orphan rule violations
    registry.add_local_module(ModuleId::new("Outrun.Core.String"));
    registry.add_local_module(ModuleId::new("Outrun.Core.Integer64"));

    registry.set_current_module(ModuleId::new("TestModule"));

    registry
}

fn create_test_function_registry() -> FunctionRegistry {
    let mut registry = FunctionRegistry::new();

    // Register protocol functions
    registry.register_function(
        "Equality".to_string(),
        "equal?".to_string(),
        FunctionInfo {
            defining_scope: "Equality".to_string(),
            function_name: "equal?".to_string(),
            visibility: FunctionVisibility::Public,
            parameters: vec![
                ("self".to_string(), Type::concrete("Self")),
                ("other".to_string(), Type::concrete("Self")),
            ],
            return_type: Type::concrete("Outrun.Core.Boolean"),
            body: None, // Test function without body
            span: None,
            generic_parameters: vec![],
            is_generic: false,
        },
    );

    registry.register_function(
        "Equality".to_string(),
        "not_equal?".to_string(),
        FunctionInfo {
            defining_scope: "Equality".to_string(),
            function_name: "not_equal?".to_string(),
            visibility: FunctionVisibility::Public,
            parameters: vec![
                ("self".to_string(), Type::concrete("Self")),
                ("other".to_string(), Type::concrete("Self")),
            ],
            return_type: Type::concrete("Outrun.Core.Boolean"),
            body: None, // Test function without body
            span: None,
            generic_parameters: vec![],
            is_generic: false,
        },
    );

    // Register String implementation functions
    registry.register_function(
        "impl Equality for Outrun.Core.String".to_string(),
        "equal?".to_string(),
        FunctionInfo {
            defining_scope: "impl Equality for Outrun.Core.String".to_string(),
            function_name: "equal?".to_string(),
            visibility: FunctionVisibility::Public,
            parameters: vec![
                ("self".to_string(), Type::concrete("Outrun.Core.String")),
                ("other".to_string(), Type::concrete("Outrun.Core.String")),
            ],
            return_type: Type::concrete("Outrun.Core.Boolean"),
            body: None, // Test function without body
            span: None,
            generic_parameters: vec![],
            is_generic: false,
        },
    );

    // Register module functions
    registry.register_function(
        "User".to_string(),
        "create".to_string(),
        FunctionInfo {
            defining_scope: "User".to_string(),
            function_name: "create".to_string(),
            visibility: FunctionVisibility::Public,
            parameters: vec![
                ("name".to_string(), Type::concrete("Outrun.Core.String")),
                ("email".to_string(), Type::concrete("Outrun.Core.String")),
            ],
            return_type: Type::generic_concrete(
                "Result",
                vec![Type::concrete("User"), Type::concrete("Outrun.Core.String")],
            ),
            body: None, // Test function without body
            span: None,
            generic_parameters: vec![],
            is_generic: false,
        },
    );

    // Register private function in module
    registry.register_function(
        "User".to_string(),
        "validate_email?".to_string(),
        FunctionInfo {
            defining_scope: "User".to_string(),
            function_name: "validate_email?".to_string(),
            visibility: FunctionVisibility::Private,
            parameters: vec![("email".to_string(), Type::concrete("Outrun.Core.String"))],
            return_type: Type::concrete("Outrun.Core.Boolean"),
            body: None, // Test function without body
            span: None,
            generic_parameters: vec![],
            is_generic: false,
        },
    );

    // Register private protocol function
    registry.register_function(
        "Equality".to_string(),
        "helper?".to_string(),
        FunctionInfo {
            defining_scope: "Equality".to_string(),
            function_name: "helper?".to_string(),
            visibility: FunctionVisibility::Private,
            parameters: vec![("value".to_string(), Type::concrete("Self"))],
            return_type: Type::concrete("Outrun.Core.Boolean"),
            body: None, // Test function without body
            span: None,
            generic_parameters: vec![],
            is_generic: false,
        },
    );

    registry
}

fn register_basic_implementations(protocol_registry: &mut ProtocolRegistry) {
    // String implements Equality
    protocol_registry
        .register_implementation(
            TypeId::new("Outrun.Core.String"),
            vec![],
            ProtocolId::new("Equality"),
            vec![],
            ModuleId::new("Outrun.Core.String"),
            None,
        )
        .unwrap();

    // Integer implements Equality
    protocol_registry
        .register_implementation(
            TypeId::new("Outrun.Core.Integer64"),
            vec![],
            ProtocolId::new("Equality"),
            vec![],
            ModuleId::new("Outrun.Core.Integer64"),
            None,
        )
        .unwrap();
}

#[test]
fn test_protocol_function_call_resolution() {
    let mut protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();
    register_basic_implementations(&mut protocol_registry);

    let dispatcher = FunctionDispatcher::new(&protocol_registry, &function_registry, None, None);

    // Test Equality.equal?(value: "hello", other: "world")
    let string_type = Type::concrete("Outrun.Core.String");
    let result = dispatcher
        .resolve_qualified_call("Equality.equal?", Some(&string_type), None)
        .unwrap();

    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "Equality.equal?:Outrun.Core.String");
            assert_eq!(
                resolved_func.implementing_type.as_ref().unwrap().name(),
                "Outrun.Core.String"
            );
            assert_eq!(resolved_func.function_info.function_name, "equal?");
        }
        _ => panic!("Expected resolved dispatch"),
    }
}

#[test]
fn test_static_module_call_resolution() {
    let protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();

    let dispatcher = FunctionDispatcher::new(&protocol_registry, &function_registry, None, None);

    // Test User.create(name: "john", email: "john@example.com")
    let result = dispatcher
        .resolve_qualified_call(
            "User.create",
            None, // No target type - static call
            None,
        )
        .unwrap();

    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "User.create");
            assert!(resolved_func.implementing_type.is_none()); // Static call
            assert_eq!(resolved_func.function_info.function_name, "create");
        }
        _ => panic!("Expected resolved static call"),
    }
}

#[test]
fn test_local_call_in_protocol_context() {
    let protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();

    let context = FunctionContext::Protocol {
        protocol_id: ProtocolId::new("Equality"),
        protocol_args: vec![],
    };

    let dispatcher =
        FunctionDispatcher::new(&protocol_registry, &function_registry, None, None).with_context(context);

    // Local call: equal?(self, other) inside Equality.not_equal?
    // Should resolve to Equality.equal?
    let result = dispatcher.resolve_local_call("equal?", None).unwrap();

    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "Equality.equal?");
            assert_eq!(resolved_func.function_info.function_name, "equal?");
        }
        _ => panic!("Expected resolved local protocol call"),
    }
}

#[test]
fn test_local_call_in_module_context() {
    let protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();

    let context = FunctionContext::Module {
        module_id: TypeId::new("User"),
        module_args: vec![],
    };

    let dispatcher =
        FunctionDispatcher::new(&protocol_registry, &function_registry, None, None).with_context(context);

    // Local call: validate_email?(email) inside User.create
    // Should resolve to User.validate_email?
    let result = dispatcher
        .resolve_local_call("validate_email?", None)
        .unwrap();

    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "User.validate_email?");
            assert_eq!(resolved_func.function_info.function_name, "validate_email?");
            assert_eq!(
                resolved_func.function_info.visibility,
                FunctionVisibility::Private
            );
        }
        _ => panic!("Expected resolved local module call"),
    }
}

#[test]
fn test_no_implementation_error() {
    let protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();

    let dispatcher = FunctionDispatcher::new(&protocol_registry, &function_registry, None, None);

    // Try to call a non-existent protocol function
    let string_type = Type::concrete("Outrun.Core.String");
    let result =
        dispatcher.resolve_qualified_call("NonExistent.function", Some(&string_type), None);

    assert!(result.is_err());
}

#[test]
fn test_dispatch_table_creation() {
    let mut protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();
    register_basic_implementations(&mut protocol_registry);

    let table = build_dispatch_table(&protocol_registry, &function_registry, None);

    // Table should contain implementation entries using monomorphised key format
    let entry = table.lookup("Equality.equal?:Outrun.Core.String");
    assert!(entry.is_some());

    let entry = entry.unwrap();
    assert_eq!(entry.qualified_name, "Equality.equal?");
}

#[test]
fn test_malformed_qualified_name_error() {
    let protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();

    let dispatcher = FunctionDispatcher::new(&protocol_registry, &function_registry, None, None);

    // Test malformed qualified name (no dot)
    let result = dispatcher.resolve_qualified_call("NotQualified", None, None);
    assert!(result.is_err());

    // Test too many parts
    let result = dispatcher.resolve_qualified_call("Too.Many.Parts", None, None);
    assert!(result.is_err());
}

#[test]
fn test_private_protocol_function_access() {
    let protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();

    let context = FunctionContext::Protocol {
        protocol_id: ProtocolId::new("Equality"),
        protocol_args: vec![],
    };

    let dispatcher =
        FunctionDispatcher::new(&protocol_registry, &function_registry, None, None).with_context(context);

    // Private protocol function should be accessible within protocol context
    let result = dispatcher.resolve_local_call("helper?", None).unwrap();

    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "Equality.helper?");
            assert_eq!(resolved_func.function_info.function_name, "helper?");
            assert_eq!(
                resolved_func.function_info.visibility,
                FunctionVisibility::Private
            );
        }
        _ => panic!("Expected resolved local protocol private function call"),
    }
}

#[test]
fn test_private_protocol_function_blocked_externally() {
    let protocol_registry = create_test_protocol_registry();
    let function_registry = create_test_function_registry();

    // Top-level context - no local access to private protocol functions
    let dispatcher = FunctionDispatcher::new(&protocol_registry, &function_registry, None, None);

    // Private protocol function should NOT be accessible from external context
    let result = dispatcher.resolve_qualified_call("Equality.helper?", None, None);
    assert!(result.is_err());
}

#[test]
fn test_protocol_default_implementation_with_private_helper() {
    let protocol_registry = create_test_protocol_registry();
    let mut function_registry = create_test_function_registry();

    // Register a protocol default implementation that calls a private helper
    function_registry.register_function(
        "Equality".to_string(),
        "detailed_equal?".to_string(),
        FunctionInfo {
            defining_scope: "Equality".to_string(),
            function_name: "detailed_equal?".to_string(),
            visibility: FunctionVisibility::Public,
            parameters: vec![
                ("self".to_string(), Type::concrete("Self")),
                ("other".to_string(), Type::concrete("Self")),
            ],
            return_type: Type::concrete("Outrun.Core.Boolean"),
            body: None, // Test function without body
            span: None,
            generic_parameters: vec![],
            is_generic: false,
        },
    );

    let context = FunctionContext::Protocol {
        protocol_id: ProtocolId::new("Equality"),
        protocol_args: vec![],
    };

    let dispatcher =
        FunctionDispatcher::new(&protocol_registry, &function_registry, None, None).with_context(context);

    // Within the protocol context, the default implementation should be able to call:
    // 1. Other public protocol functions
    let result = dispatcher.resolve_local_call("equal?", None).unwrap();
    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "Equality.equal?");
            assert_eq!(
                resolved_func.function_info.visibility,
                FunctionVisibility::Public
            );
        }
        _ => panic!("Expected resolved public protocol call"),
    }

    // 2. Private protocol helper functions
    let result = dispatcher.resolve_local_call("helper?", None).unwrap();
    match result {
        DispatchResult::Resolved(resolved_func) => {
            assert_eq!(resolved_func.qualified_name, "Equality.helper?");
            assert_eq!(
                resolved_func.function_info.visibility,
                FunctionVisibility::Private
            );
        }
        _ => panic!("Expected resolved private protocol helper call"),
    }
}
