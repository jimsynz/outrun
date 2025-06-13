//! Test cases for trait definition type checking
//!
//! Tests trait definition processing, validation, and registration.

#[cfg(test)]
mod tests {
    use crate::checker::TypeChecker;
    use crate::error::TypeError;
    use outrun_parser::parse_program;

    #[test]
    fn test_simple_trait_definition() {
        let mut checker = TypeChecker::new();
        
        let source = r#"
            trait Display {
                def to_string(): String
            }
        "#;
        
        let program = parse_program(source).expect("Should parse trait definition");
        let result = checker.check_program(&program);
        
        assert!(result.is_ok(), "Simple trait definition should type check: {:?}", result);
        
        // Verify the trait was registered
        let display_trait_id = checker.context.interner.intern_trait("Display");
        assert!(checker.context.trait_registry.get_trait(display_trait_id).is_some());
    }
    
    #[test]
    fn test_trait_with_generic_parameters() {
        let mut checker = TypeChecker::new();
        
        let source = r#"
            trait Comparable<T> {
                def compare(other: T): Integer
            }
        "#;
        
        let program = parse_program(source).expect("Should parse generic trait");
        let result = checker.check_program(&program);
        
        assert!(result.is_ok(), "Generic trait definition should type check: {:?}", result);
        
        // Verify the trait was registered with generics
        let comparable_trait_id = checker.context.interner.intern_trait("Comparable");
        let trait_def = checker.context.trait_registry.get_trait(comparable_trait_id).unwrap();
        assert!(trait_def.is_generic());
        assert_eq!(trait_def.generic_params.len(), 1);
    }
    
    #[test]
    fn test_trait_with_constraints() {
        let mut checker = TypeChecker::new();
        
        let source = r#"
            trait Serializable<T> when T: Display && T: Debug {
                def serialize(): String
                def deserialize(data: String): T
            }
        "#;
        
        let program = parse_program(source).expect("Should parse trait with constraints");
        let result = checker.check_program(&program);
        
        assert!(result.is_ok(), "Trait with constraints should type check: {:?}", result);
        
        // Verify the trait was registered with constraints
        let trait_id = checker.context.interner.intern_trait("Serializable");
        let trait_def = checker.context.trait_registry.get_trait(trait_id).unwrap();
        assert!(!trait_def.constraints.is_empty());
    }
    
    #[test] 
    fn test_trait_with_guard_function() {
        let mut checker = TypeChecker::new();
        
        let source = r#"
            trait Nullable<T> {
                def is_null?(): Boolean
                def unwrap(): T
            }
        "#;
        
        let program = parse_program(source).expect("Should parse trait with guard function");
        let result = checker.check_program(&program);
        
        assert!(result.is_ok(), "Trait with guard function should type check: {:?}", result);
        
        // Verify the guard function is properly marked
        let trait_id = checker.context.interner.intern_trait("Nullable");
        let trait_def = checker.context.trait_registry.get_trait(trait_id).unwrap();
        let is_null_atom = checker.context.interner.intern_atom("is_null?");
        let func = trait_def.find_function(is_null_atom).unwrap();
        assert!(func.is_guard_function());
    }
    
    #[test]
    fn test_invalid_guard_function_return_type() {
        let mut checker = TypeChecker::new();
        
        let source = r#"
            trait Invalid {
                def is_valid?(): String
            }
        "#;
        
        let program = parse_program(source).expect("Should parse invalid guard function");
        let result = checker.check_program(&program);
        
        assert!(result.is_err(), "Invalid guard function should fail type checking");
        
        let errors = result.unwrap_err();
        assert!(!errors.is_empty(), "Expected at least one error");
        
        match &errors[0] {
            TypeError::InvalidGuardFunction { function_name, actual_return_type, .. } => {
                assert_eq!(function_name, "is_valid?");
                assert_eq!(actual_return_type, "Outrun.Core.String");
            }
            _ => panic!("Expected InvalidGuardFunction error, got: {:?}", errors[0]),
        }
    }
    
    #[test]
    fn test_undefined_type_parameter_in_constraint() {
        let mut checker = TypeChecker::new();
        
        let source = r#"
            trait Invalid<T> when U: Display {
                def process(value: T): String
            }
        "#;
        
        let program = parse_program(source).expect("Should parse trait with undefined type parameter");
        let result = checker.check_program(&program);
        
        assert!(result.is_err(), "Undefined type parameter should fail type checking");
        
        let errors = result.unwrap_err();
        assert!(!errors.is_empty(), "Expected at least one error");
        
        match &errors[0] {
            TypeError::UndefinedTypeParameter { parameter_name, .. } => {
                assert_eq!(parameter_name, "U");
            }
            _ => panic!("Expected UndefinedTypeParameter error, got: {:?}", errors[0]),
        }
    }
    
    #[test]
    fn test_trait_with_multiple_functions() {
        let mut checker = TypeChecker::new();
        
        let source = r#"
            trait Collection<T> {
                def size(): Integer
                def is_empty?(): Boolean
                def add(item: T): Collection<T>
                def remove(item: T): Collection<T>
                def contains?(item: T): Boolean
            }
        "#;
        
        let program = parse_program(source).expect("Should parse trait with multiple functions");
        let result = checker.check_program(&program);
        
        assert!(result.is_ok(), "Trait with multiple functions should type check: {:?}", result);
        
        // Verify all functions were registered
        let trait_id = checker.context.interner.intern_trait("Collection");
        let trait_def = checker.context.trait_registry.get_trait(trait_id).unwrap();
        assert_eq!(trait_def.functions.len(), 5);
        
        // Check that guard functions are properly identified
        let is_empty_atom = checker.context.interner.intern_atom("is_empty?");
        let contains_atom = checker.context.interner.intern_atom("contains?");
        
        assert!(trait_def.find_function(is_empty_atom).unwrap().is_guard_function());
        assert!(trait_def.find_function(contains_atom).unwrap().is_guard_function());
    }
    
    #[test]
    fn test_trait_with_complex_constraints() {
        let mut checker = TypeChecker::new();
        
        let source = r#"
            trait Advanced<T, U> when T: Display && T: Debug && U: Clone {
                def transform(input: T): U
                def combine(first: T, second: U): String
            }
        "#;
        
        let program = parse_program(source).expect("Should parse trait with complex constraints");
        let result = checker.check_program(&program);
        
        assert!(result.is_ok(), "Trait with complex constraints should type check: {:?}", result);
        
        // Verify multiple generic parameters and constraints
        let trait_id = checker.context.interner.intern_trait("Advanced");
        let trait_def = checker.context.trait_registry.get_trait(trait_id).unwrap();
        assert_eq!(trait_def.generic_params.len(), 2);
        assert!(!trait_def.constraints.is_empty());
    }
}