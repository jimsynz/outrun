//! Test function body type checking implementation

#[cfg(test)]
mod function_body_typechecking_tests {
    use crate::inference::TypeInferenceEngine;
    use outrun_parser::parse_program;

    /// Helper to create a type inference engine for testing with basic protocol setup
    fn create_test_engine() -> TypeInferenceEngine {
        let mut engine = TypeInferenceEngine::new();
        
        // Set up basic protocol requirements for testing
        // This simulates what would happen during real protocol definition collection
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
    fn test_function_body_typecheck_exists() {
        let source = r#"
            def add(a: Integer, b: Integer): Integer {
                a + b
            }
        "#;

        let mut program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // First collect definitions
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Definition collection should succeed: {:?}", result);
        
        // Then try to typecheck the function body
        if let Some(item) = program.items.first_mut() {
            let result = engine.typecheck_item(item);
            // Should not panic - this validates our implementation works
            assert!(result.is_ok(), "Function body typechecking should succeed: {:?}", result);
        }
    }

    #[test]  
    fn test_struct_function_body_typecheck() {
        let source = r#"
            struct Calculator {
                def add(a: Integer, b: Integer): Integer {
                    a + b
                }
            }
        "#;

        let mut program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // First collect definitions
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Definition collection should succeed: {:?}", result);
        
        // Then try to typecheck the struct function bodies
        if let Some(item) = program.items.first_mut() {
            let result = engine.typecheck_item(item);
            // Should not panic - this validates our implementation works
            assert!(result.is_ok(), "Struct function body typechecking should succeed: {:?}", result);
        }
    }

    #[test]
    fn test_function_body_typecheck_integration() {
        let source = r#"
            struct User(name: String, age: Integer) {
                def new(name: String, age: Integer): Self {
                    User(name: name, age: age)
                }
                
                def greet(user: Self): String {
                    "Hello, " + User.name(user: user)
                }
            }
            
            def main(): String {
                let user = User.new(name: "Alice", age: 30)
                User.greet(user: user)
            }
        "#;

        let mut program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // Test the complete workflow
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Definition collection should succeed: {:?}", result);
        
        // Type check all items
        for item in &mut program.items {
            let result = engine.typecheck_item(item);
            assert!(result.is_ok(), "Function body typechecking should succeed: {:?}", result);
        }
    }
}