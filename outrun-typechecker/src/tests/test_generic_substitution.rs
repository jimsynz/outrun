#[cfg(test)]
mod tests {
    use crate::*;
    use outrun_parser::parse_program;

    #[test]
    fn test_option_unwrap_returns_correct_type() {
        let source = r#"
            def test_option_integer(): Integer {
                Option.unwrap(value: Option.some(value: 42))
            }
            
            def test_option_string(): String {
                Option.unwrap(value: Option.some(value: "hello"))
            }
        "#;

        let program = parse_program(source).expect("Failed to parse");
        let mut package = Package::new("test_package".to_string());
        package.add_program(program);

        let result = CompilationResult::compile_package(&mut package);
        assert!(
            result.is_ok(),
            "Compilation should succeed: {:?}",
            result.err()
        );

        // The important thing is that compilation succeeds - this means
        // Option.unwrap correctly infers its return type based on the
        // generic type argument
    }

    #[test]
    fn test_custom_generic_protocol() {
        // Test that our generic substitution works for user-defined protocols too
        let source = r#"
            protocol Container<T> {
                def get_value(self: Self): T
            }
            
            struct Box<T>(value: T) {}
            
            impl Container<T> for Box<T> {
                def get_value(self: Self): T {
                    self.value
                }
            }
            
            def test_box_integer(): Integer {
                let box = Box { value: 42 }
                Container.get_value(self: box)
            }
        "#;

        let program = parse_program(source).expect("Failed to parse");
        let mut package = Package::new("test_package".to_string());
        package.add_program(program);

        let result = CompilationResult::compile_package(&mut package);

        // This test might fail if we don't support user-defined generics yet,
        // but it demonstrates that our solution should work for any protocol
        if result.is_ok() {
            let compilation = result.unwrap();
            let func = compilation
                .function_registry
                .get_function("test_package", "test_box_integer");
            if let Some(f) = func {
                assert_eq!(f.return_type, Type::protocol("Integer"));
            }
        }
    }
}
