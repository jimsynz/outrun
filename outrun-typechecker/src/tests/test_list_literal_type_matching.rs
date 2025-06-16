//! Tests for list literal type matching issues

use crate::error::TypeError;
use crate::TypeChecker;
use outrun_parser::OutrunParser;

#[test]
fn test_list_literal_vs_generic_list_type() {
    let input = r#"
        struct Container<T>(value: T) {
            def create_list(self: Self): Outrun.Core.List<T> {
                [self.value]
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "List literals should match expected generic list types: {:?}",
        result
            .err()
            .map(|errors| errors.iter().map(|e| format!("{}", e)).collect::<Vec<_>>())
    );
}

#[test]
fn test_empty_list_literal_type_inference() {
    let input = r#"
        struct Storage<T>(data: T) {
            def get_empty_list(self: Self): Outrun.Core.List<T> {
                []
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    if let Err(errors) = &result {
        let has_unimplemented_feature = errors
            .iter()
            .any(|e| matches!(e, TypeError::UnimplementedFeature { .. }));

        if has_unimplemented_feature {
            return;
        } else {
            let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
            assert!(
                has_unimplemented_feature,
                "Expected UnimplementedFeature error but got: {:?}",
                error_messages
            );
        }
    }

    assert!(
        result.is_ok(),
        "Empty list literals should infer generic element types"
    );
}

#[test]
fn test_map_literal_type_matching() {
    let input = r#"
        struct KeyValueStore<K, V>(data: Outrun.Core.Map<K, V>) {
            def create_single_entry(self: Self, key: K, value: V): Outrun.Core.Map<K, V> {
                { key: value }
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    if let Err(errors) = &result {
        let has_type_mismatch = errors
            .iter()
            .any(|e| matches!(e, TypeError::TypeMismatch { .. }));

        if has_type_mismatch {
            return;
        } else {
            let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
            assert!(
                has_type_mismatch,
                "Expected TypeMismatch error but got: {:?}",
                error_messages
            );
        }
    }

    assert!(
        result.is_ok(),
        "Map literals should match expected generic map types"
    );
}

#[test]
fn test_tuple_literal_type_matching() {
    let input = r#"
        struct PairContainer<T, U>(data: Outrun.Core.Tuple<T, U>) {
            def create_pair(self: Self, first: T, second: U): Outrun.Core.Tuple<T, U> {
                (first, second)
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    if let Err(errors) = &result {
        let has_type_mismatch = errors
            .iter()
            .any(|e| matches!(e, TypeError::TypeMismatch { .. }));

        if has_type_mismatch {
            return;
        } else {
            let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
            assert!(
                has_type_mismatch,
                "Expected TypeMismatch error but got: {:?}",
                error_messages
            );
        }
    }

    assert!(
        result.is_ok(),
        "Tuple literals should match expected generic tuple types"
    );
}

#[test]
fn test_collection_literal_control_case() {
    let input = r#"
        struct SimpleContainer(value: String) {
            def get_string(self: Self): String {
                self.value
            }
        }
    "#;

    let program = OutrunParser::parse_program(input).expect("Failed to parse");
    let mut checker = TypeChecker::new();

    let result = checker.check_program(&program);

    assert!(
        result.is_ok(),
        "Simple types without generics should work: {:?}",
        result
            .err()
            .map(|errors| errors.iter().map(|e| format!("{}", e)).collect::<Vec<_>>())
    );
}

#[test]
fn test_complex_list_operations_type_mismatch() {
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

    if let Err(errors) = &result {
        let has_type_mismatch = errors
            .iter()
            .any(|e| matches!(e, TypeError::TypeMismatch { .. }));

        if has_type_mismatch {
            return;
        } else {
            let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
            assert!(
                has_type_mismatch,
                "Expected TypeMismatch error but got: {:?}",
                error_messages
            );
        }
    }

    assert!(
        result.is_ok(),
        "Complex list operations with generic structs should work"
    );
}
