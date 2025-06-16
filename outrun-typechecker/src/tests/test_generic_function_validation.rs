use crate::checker::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_struct_method_with_generic_parameters() {
    let input = r#"
        struct Container<T>(value: T) {
            def get_value(self: Self): T {
                self.value
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Struct method with generic parameters should type check successfully: {:?}",
        result.err()
    );
}

#[test]
fn test_struct_method_with_multiple_generic_parameters() {
    let input = r#"
        trait PairOperations {
            def get_first_value(self: Self): String
            def get_second_value(self: Self): String
        }
        
        struct Pair<T, U>(first: T, second: U) {}
        
        impl<T, U> PairOperations for Pair<T, U> {
            def get_first_value(self: Self): String {
                "first"
            }
            
            def get_second_value(self: Self): String {
                "second"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Trait impl functions with multiple generic parameters should type check successfully: {:?}",
        result.err()
    );
}

#[test]
fn test_impl_block_method_with_generic_parameters() {
    let input = r#"
        trait SimpleDisplay {
            def to_string(self: Self): String
        }
        
        struct Container<T>(value: T) {}
        
        impl<T> SimpleDisplay for Container<T> {
            def to_string(self: Self): String {
                "container"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Impl block methods with generic parameters should type check successfully: {:?}",
        result.err()
    );
}

#[test]
fn test_struct_with_unused_generic_parameter() {
    let input = r#"
        struct Unused<T>() {}
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Struct with unused generic parameter should type check: {:?}",
        result.err()
    );
}

#[test]
fn test_struct_with_undeclared_generic_in_field() {
    let input = r#"
        struct Invalid(value: T) {}
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Struct using undeclared generic parameter should fail"
    );

    if let Err(errors) = result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        let has_undefined_type = error_messages
            .iter()
            .any(|msg| msg.contains("Undefined type T"));
        assert!(
            has_undefined_type,
            "Should report undefined type T: {:?}",
            error_messages
        );
    }
}

#[test]
fn test_struct_method_using_undeclared_generic() {
    let input = r#"
        struct Container<T>(value: T) {
            def invalid_method(self: Self): U {
                U.default()
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Struct method using undeclared generic parameter should fail"
    );

    if let Err(errors) = result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        let has_undefined_type = error_messages
            .iter()
            .any(|msg| msg.contains("Undefined type U"));
        assert!(
            has_undefined_type,
            "Should report undefined type U: {:?}",
            error_messages
        );
    }
}

#[test]
fn test_impl_block_with_different_generic_parameters() {
    let input = r#"
        trait Convert<T> {
            def convert(self: Self): T
        }
        
        struct Value<T>(data: T) {}
        
        impl<U> Convert<String> for Value<U> {
            def convert(self: Self): String {
                "converted"
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Impl block with different generic parameters should type check successfully: {:?}",
        result.err()
    );
}

#[test]
fn test_function_using_generic_list_types() {
    let input = r#"
        struct Processor<T>(items: Outrun.Core.List<T>) {
            def process_all(self: Self): Outrun.Core.List<T> {
                self.items
            }
            
            def add_item(self: Self, item: T): Outrun.Core.List<T> {
                [item]
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Function using generic parameters in complex types should type check successfully: {:?}",
        result.err()
    );
}

#[test]
fn test_function_parameter_using_generic_from_wrong_scope() {
    let input = r#"
        struct Outer<T>(value: T) {}
        
        struct Inner<U>(data: U) {
            def invalid_method(self: Self, param: T): U {
                param
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Function using generic from wrong scope should fail"
    );

    if let Err(errors) = result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        let has_undefined_type = error_messages
            .iter()
            .any(|msg| msg.contains("Undefined type T"));
        assert!(
            has_undefined_type,
            "Should report undefined type T: {:?}",
            error_messages
        );
    }
}

#[test]
fn test_standalone_function_cannot_use_undeclared_generics() {
    let input = r#"
        def invalid_function(param: T): T {
            param
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_err(),
        "Standalone function using undeclared generic should fail"
    );

    if let Err(errors) = result {
        let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
        let has_undefined_type = error_messages
            .iter()
            .any(|msg| msg.contains("Undefined type T"));
        assert!(
            has_undefined_type,
            "Should report undefined type T: {:?}",
            error_messages
        );
    }
}

#[test]
fn test_generic_parameter_scoping_isolation_between_structs() {
    let input = r#"
        struct First<T>(value: T) {
            def get_value(self: Self): T {
                self.value
            }
        }
        
        struct Second<T>(data: T) {
            def get_data(self: Self): T {
                self.data
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Generic parameter scoping should be isolated between structs: {:?}",
        result.err()
    );
}

#[test]
fn test_complex_generic_function_signatures() {
    let input = r#"
        struct Converter<Input, Output>(
            input: Input,
            output: Output
        ) {
            def transform(self: Self, new_input: Input): Output {
                self.output
            }
            
            def create_pair(self: Self): Tuple<Input, Output> {
                (self.input, self.output)
            }
            
            def map_input(self: Self, mapper: Function<(value: Input) -> Input>): Input {
                self.input
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Complex generic function signatures should type check successfully: {:?}",
        result.err()
    );
}
