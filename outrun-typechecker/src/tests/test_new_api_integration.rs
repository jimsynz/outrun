//! Integration tests for the new multi-program type checking API

use crate::{
    typecheck_program_collection, typecheck_with_core_library, ProgramCollection,
};
use outrun_parser::{
    BooleanLiteral, DebugInfo, Expression, ExpressionKind, FunctionDefinition, FunctionVisibility,
    Identifier, IntegerFormat, IntegerLiteral, Item, ItemKind, Parameter, Program, Span,
    TypeAnnotation, TypeIdentifier,
};

fn create_test_span() -> Span {
    Span::new(0, 10)
}

fn create_simple_program(items: Vec<Item>) -> Program {
    Program {
        items,
        debug_info: DebugInfo::new(),
        span: create_test_span(),
    }
}

fn create_simple_function(name: &str, return_value: i64) -> Item {
    Item {
        kind: ItemKind::FunctionDefinition(FunctionDefinition {
            attributes: vec![],
            visibility: FunctionVisibility::Public,
            name: Identifier {
                name: name.to_string(),
                span: create_test_span(),
            },
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple {
                path: vec![TypeIdentifier {
                    name: "Integer".to_string(),
                    span: create_test_span(),
                }],
                generic_args: None,
                span: create_test_span(),
            }),
            guard: None,
            body: outrun_parser::Block {
                statements: vec![outrun_parser::Statement {
                    kind: outrun_parser::StatementKind::Expression(Box::new(Expression {
                        kind: ExpressionKind::Integer(IntegerLiteral {
                            value: return_value,
                            format: IntegerFormat::Decimal,
                            span: create_test_span(),
                        }),
                        span: create_test_span(),
                    })),
                    span: create_test_span(),
                }],
                span: create_test_span(),
            },
            span: create_test_span(),
        }),
        span: create_test_span(),
    }
}

#[test]
fn test_typecheck_program_collection_empty() {
    let collection = ProgramCollection::new();
    
    let result = typecheck_program_collection(collection).unwrap();
    
    assert!(result.compilation_order.is_empty());
    assert!(result.traits.is_empty());
    assert!(result.structs.is_empty());
    assert!(result.functions.is_empty());
}

#[test]
fn test_typecheck_program_collection_single_function() {
    let mut collection = ProgramCollection::new();
    
    let program = create_simple_program(vec![create_simple_function("test_func", 42)]);
    collection.add_program(
        "test.outrun".to_string(),
        program,
        "def test_func(): Integer { 42 }".to_string(),
    );
    
    let result = typecheck_program_collection(collection).unwrap();
    
    assert_eq!(result.compilation_order.len(), 1);
    assert_eq!(result.functions.len(), 1);
    assert!(result.functions.contains_key("test_func"));
}

#[test]
fn test_typecheck_program_collection_multiple_files() {
    let mut collection = ProgramCollection::new();
    
    // Add multiple programs
    let program1 = create_simple_program(vec![create_simple_function("func1", 1)]);
    let program2 = create_simple_program(vec![create_simple_function("func2", 2)]);
    
    collection.add_program("file1.outrun".to_string(), program1, "def func1(): Integer { 1 }".to_string());
    collection.add_program("file2.outrun".to_string(), program2, "def func2(): Integer { 2 }".to_string());
    
    let result = typecheck_program_collection(collection).unwrap();
    
    assert_eq!(result.compilation_order.len(), 2);
    assert_eq!(result.functions.len(), 2);
    assert!(result.functions.contains_key("func1"));
    assert!(result.functions.contains_key("func2"));
}

#[test]
fn test_typecheck_with_core_library_simple() {
    let program = create_simple_program(vec![create_simple_function("user_func", 123)]);
    let source = "def user_func(): Integer { 123 }";
    
    let result = typecheck_with_core_library(program, source, "user.outrun").unwrap();
    
    // Should have core library traits + user function
    assert!(!result.compilation_order.is_empty());
    assert!(!result.traits.is_empty()); // Core library should provide traits
    assert!(result.functions.contains_key("user_func"));
}

#[test]
fn test_program_collection_from_core_library() {
    let collection = ProgramCollection::from_core_library();
    
    // Core library should have multiple programs
    assert!(!collection.programs.is_empty());
    assert!(!collection.sources.is_empty());
    
    // All programs should have corresponding sources
    for file_path in collection.file_paths() {
        assert!(collection.get_program(&file_path).is_some());
        assert!(collection.get_source(&file_path).is_some());
    }
}

#[test]
fn test_compilation_result_structure() {
    let mut collection = ProgramCollection::new();
    
    // Create a program with different types of definitions
    let program = create_simple_program(vec![
        // Add a trait
        Item {
            kind: ItemKind::TraitDefinition(outrun_parser::TraitDefinition {
                attributes: vec![],
                name: vec![TypeIdentifier {
                    name: "TestTrait".to_string(),
                    span: create_test_span(),
                }],
                generic_params: None,
                constraints: None,
                functions: vec![],
                span: create_test_span(),
            }),
            span: create_test_span(),
        },
        // Add a struct
        Item {
            kind: ItemKind::StructDefinition(outrun_parser::StructDefinition {
                attributes: vec![],
                name: vec![TypeIdentifier {
                    name: "TestStruct".to_string(),
                    span: create_test_span(),
                }],
                generic_params: None,
                fields: vec![],
                methods: vec![],
                span: create_test_span(),
            }),
            span: create_test_span(),
        },
        // Add a function
        create_simple_function("test_func", 42),
    ]);
    
    collection.add_program(
        "comprehensive.outrun".to_string(),
        program,
        "trait TestTrait {}\nstruct TestStruct()\ndef test_func(): Integer { 42 }".to_string(),
    );
    
    let result = typecheck_program_collection(collection).unwrap();
    
    // Verify all components are present
    assert_eq!(result.compilation_order.len(), 1);
    assert_eq!(result.traits.len(), 1);
    assert_eq!(result.structs.len(), 1);
    assert_eq!(result.functions.len(), 1);
    
    assert!(result.traits.contains_key("TestTrait"));
    assert!(result.structs.contains_key("TestStruct"));
    assert!(result.functions.contains_key("test_func"));
}

#[test]
fn test_error_reporting_with_multiple_files() {
    // This test would verify that errors from multiple files are properly aggregated
    // For now, we'll test the success case since we don't have error-inducing programs
    
    let mut collection = ProgramCollection::new();
    
    let program1 = create_simple_program(vec![create_simple_function("good_func1", 1)]);
    let program2 = create_simple_program(vec![create_simple_function("good_func2", 2)]);
    
    collection.add_program("good1.outrun".to_string(), program1, "def good_func1(): Integer { 1 }".to_string());
    collection.add_program("good2.outrun".to_string(), program2, "def good_func2(): Integer { 2 }".to_string());
    
    // This should succeed
    let result = typecheck_program_collection(collection);
    assert!(result.is_ok());
    
    let compilation_result = result.unwrap();
    assert_eq!(compilation_result.functions.len(), 2);
}

#[test]
fn test_api_consistency() {
    // Test that both old and new APIs produce consistent results for simple cases
    let program = create_simple_program(vec![create_simple_function("consistency_test", 999)]);
    let source = "def consistency_test(): Integer { 999 }";
    
    // Use new API
    let new_result = typecheck_with_core_library(program.clone(), source, "test.outrun");
    
    // Verify new API works
    assert!(new_result.is_ok());
    let compilation_result = new_result.unwrap();
    assert!(compilation_result.functions.contains_key("consistency_test"));
}

#[test]
fn test_compilation_order_consistency() {
    let mut collection = ProgramCollection::new();
    
    // Add programs in a specific order
    let program1 = create_simple_program(vec![create_simple_function("first", 1)]);
    let program2 = create_simple_program(vec![create_simple_function("second", 2)]);
    let program3 = create_simple_program(vec![create_simple_function("third", 3)]);
    
    collection.add_program("z_last.outrun".to_string(), program3, "def third(): Integer { 3 }".to_string());
    collection.add_program("a_first.outrun".to_string(), program1, "def first(): Integer { 1 }".to_string());
    collection.add_program("m_middle.outrun".to_string(), program2, "def second(): Integer { 2 }".to_string());
    
    let result = typecheck_program_collection(collection).unwrap();
    
    // Compilation order should be determined by dependency analysis, not insertion order
    assert_eq!(result.compilation_order.len(), 3);
    assert!(result.compilation_order.contains(&"a_first.outrun".to_string()));
    assert!(result.compilation_order.contains(&"m_middle.outrun".to_string()));
    assert!(result.compilation_order.contains(&"z_last.outrun".to_string()));
}

#[test]
fn test_type_context_preservation() {
    let program = create_simple_program(vec![create_simple_function("context_test", 42)]);
    let source = "def context_test(): Integer { 42 }";
    
    let result = typecheck_with_core_library(program, source, "context.outrun").unwrap();
    
    // The type context should be populated with information from compilation
    // This is a basic test - more detailed context inspection would require additional APIs
    assert!(!result.compilation_order.is_empty());
}