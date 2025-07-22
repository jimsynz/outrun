//! Tests for the multi-program compiler with visitor-based type checking

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::compilation::program_collection::ProgramCollection;
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
fn test_empty_program_collection() {
    let collection = ProgramCollection::new();
    let mut compiler_env = CompilerEnvironment::new();

    let result = compiler_env.compile_collection(collection).unwrap();

    assert!(result.compilation_order.is_empty());
    assert!(result.protocols.is_empty());
    assert!(result.structs.is_empty());
    assert!(result.implementations.is_empty());
    // Note: Functions are now stored in the CompilerEnvironment's module system
}

#[test]
fn test_single_program_compilation() {
    let mut collection = ProgramCollection::new();

    // Create a simple program with one function
    let program = create_simple_program(vec![create_simple_function("test_func", 42)]);

    collection.add_program(
        "test.outrun".to_string(),
        program,
        "def test_func(): Integer { 42 }".to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection).unwrap();

    assert_eq!(result.compilation_order, vec!["test.outrun"]);
    // Functions are now stored in the CompilerEnvironment's module system
    assert!(result.typed_programs.contains_key("test.outrun"));
}

#[test]
fn test_multiple_program_compilation() {
    let mut collection = ProgramCollection::new();

    // Create two simple programs
    let program1 = create_simple_program(vec![create_simple_function("func1", 1)]);
    let program2 = create_simple_program(vec![create_simple_function("func2", 2)]);

    collection.add_program(
        "program1.outrun".to_string(),
        program1,
        "def func1(): Integer { 1 }".to_string(),
    );

    collection.add_program(
        "program2.outrun".to_string(),
        program2,
        "def func2(): Integer { 2 }".to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection).unwrap();

    assert_eq!(result.compilation_order.len(), 2);
    assert!(result
        .compilation_order
        .contains(&"program1.outrun".to_string()));
    assert!(result
        .compilation_order
        .contains(&"program2.outrun".to_string()));
    // Functions are now stored in the module system via CompilerEnvironment
    assert_eq!(result.typed_programs.len(), 2);
    assert!(result.typed_programs.contains_key("program1.outrun"));
    assert!(result.typed_programs.contains_key("program2.outrun"));
}

#[test]
fn test_program_collection_from_single_program() {
    let program = create_simple_program(vec![create_simple_function("single_func", 100)]);
    let source = "def single_func(): Integer { 100 }".to_string();

    let collection = ProgramCollection::from_single_program(
        "single.outrun".to_string(),
        program,
        source.clone(),
    );

    assert_eq!(collection.programs.len(), 1);
    assert!(collection.get_program("single.outrun").is_some());
    assert_eq!(collection.get_source("single.outrun"), Some(&source));
}

#[test]
fn test_program_collection_file_paths() {
    let mut collection = ProgramCollection::new();

    let program1 = create_simple_program(vec![]);
    let program2 = create_simple_program(vec![]);

    collection.add_program("file1.outrun".to_string(), program1, "".to_string());
    collection.add_program("file2.outrun".to_string(), program2, "".to_string());

    let mut file_paths = collection.file_paths();
    file_paths.sort(); // Order is not guaranteed

    assert_eq!(file_paths, vec!["file1.outrun", "file2.outrun"]);
}

#[test]
fn test_compilation_phases() {
    let mut collection = ProgramCollection::new();

    // Create a program with a protocol definition (this would be extracted in phase 1)
    let protocol_program = create_simple_program(vec![Item {
        kind: ItemKind::ProtocolDefinition(outrun_parser::ProtocolDefinition {
            attributes: vec![],
            name: vec![TypeIdentifier {
                name: "TestProtocol".to_string(),
                span: create_test_span(),
            }],
            generic_params: None,
            constraints: None,
            functions: vec![],
            span: create_test_span(),
        }),
        span: create_test_span(),
    }]);

    collection.add_program(
        "protocols.outrun".to_string(),
        protocol_program,
        "protocol TestProtocol {}".to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection).unwrap();

    // Verify that the protocol was extracted in phase 1
    assert_eq!(result.protocols.len(), 1);
    assert!(result.protocols.contains_key("TestProtocol"));
}

#[test]
fn test_compiler_error_accumulation() {
    let mut collection = ProgramCollection::new();

    // Create a program that might cause dependency resolution errors
    // (This is a simplified test - real dependency errors would be more complex)
    let empty_program = create_simple_program(vec![]);
    collection.add_program("empty.outrun".to_string(), empty_program, "".to_string());

    let mut compiler_env = CompilerEnvironment::new();

    // This should succeed with an empty program
    let result = compiler_env.compile_collection(collection);
    assert!(result.is_ok());
}

#[test]
fn test_context_preservation_across_phases() {
    let mut collection = ProgramCollection::new();

    // Create programs that define and use types across phases
    let protocol_program = create_simple_program(vec![Item {
        kind: ItemKind::ProtocolDefinition(outrun_parser::ProtocolDefinition {
            attributes: vec![],
            name: vec![TypeIdentifier {
                name: "Addable".to_string(),
                span: create_test_span(),
            }],
            generic_params: None,
            constraints: None,
            functions: vec![],
            span: create_test_span(),
        }),
        span: create_test_span(),
    }]);

    let struct_program = create_simple_program(vec![Item {
        kind: ItemKind::StructDefinition(outrun_parser::StructDefinition {
            attributes: vec![],
            name: vec![TypeIdentifier {
                name: "Number".to_string(),
                span: create_test_span(),
            }],
            generic_params: None,
            fields: vec![],
            methods: vec![],
            span: create_test_span(),
        }),
        span: create_test_span(),
    }]);

    collection.add_program(
        "protocols.outrun".to_string(),
        protocol_program,
        "protocol Addable {}".to_string(),
    );
    collection.add_program(
        "structs.outrun".to_string(),
        struct_program,
        "struct Number()".to_string(),
    );

    let mut compiler_env = CompilerEnvironment::new();
    let result = compiler_env.compile_collection(collection).unwrap();

    // Verify that context is preserved across phases
    assert_eq!(result.protocols.len(), 1);
    assert_eq!(result.structs.len(), 1);
    assert!(result.protocols.contains_key("Addable"));
    assert!(result.structs.contains_key("Number"));

    // The type context should have both protocol and struct registered
    // (This would require implementing more detailed context inspection)
}
