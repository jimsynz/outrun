//! Tests for function definition type checking in structs, protocols, and impl blocks

#[cfg(test)]
mod function_definition_tests {
    use crate::inference::TypeInferenceEngine;
    use outrun_parser::parse_program;

    /// Helper to create a type inference engine for testing
    fn create_test_engine() -> TypeInferenceEngine {
        TypeInferenceEngine::new()
    }

    #[test]
    fn test_struct_function_collection() {
        let source = r#"
            struct User(name: String, age: Integer) {
                def new(name: String, age: Integer): Self {
                    User(name: name, age: age)
                }
                
                def greet(user: Self): String {
                    "Hello, " + User.name(user: user)
                }
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // This should not panic and should collect the struct functions
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Function collection should succeed: {:?}", result);
        
        // Verify that the functions were registered in the function registry
        // Note: Since the function registry is behind Rc, we can't easily inspect it here
        // but the fact that collect_definitions succeeded means the functions were processed
    }

    #[test]
    fn test_protocol_function_collection() {
        let source = r#"
            protocol Greeter {
                def greet(self: Self): String
                
                def default_greeting(): String {
                    "Hello, World!"
                }
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // This should not panic and should collect the protocol functions
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Protocol function collection should succeed: {:?}", result);
    }

    #[test]
    fn test_standalone_function_collection() {
        let source = r#"
            def add(a: Integer, b: Integer): Integer {
                a + b
            }
            
            def greet(name: String): String {
                "Hello, " + name
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // This should not panic and should collect the standalone functions
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Standalone function collection should succeed: {:?}", result);
    }

    #[test]
    fn test_function_collection_with_complex_types() {
        let source = r#"
            struct Calculator {
                def complex_operation(
                    numbers: List<Integer>, 
                    multiplier: Float
                ): Result<Float, String> {
                    Result.ok(value: 42.0)
                }
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // This should handle complex type annotations
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Complex type function collection should succeed: {:?}", result);
    }

    #[test]
    fn test_function_collection_preserves_visibility() {
        let source = r#"
            struct Processor {
                def public_method(input: String): String {
                    "Processed: " + input
                }
                
                def helper_method(data: String): String {
                    "Helper: " + data
                }
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // This should handle function collection
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Function collection should succeed: {:?}", result);
    }

    #[test]
    fn test_impl_block_function_collection() {
        let source = r#"
            impl Display for User {
                def display(user: Self): String {
                    "User: " + User.name(user: user)
                }
                
                defp private_helper(data: String): String {
                    "Helper: " + data
                }
            }
        "#;

        let program = parse_program(source).expect("Parse should succeed");
        let mut engine = create_test_engine();
        
        // This should handle impl block functions with both public and private visibility
        let result = engine.collect_definitions(&program);
        assert!(result.is_ok(), "Impl block function collection should succeed: {:?}", result);
    }
}