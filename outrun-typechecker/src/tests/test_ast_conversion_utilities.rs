//! AST conversion utilities tests
//!
//! Tests for converting untyped parser AST to typed AST using dedicated conversion methods

use crate::checker::{TypeChecker, TypedExpressionKind, TypedItemKind};
use outrun_parser::{parse_expression, parse_program};

#[test]
fn test_convert_expression_integer_literal() {
    let mut checker = TypeChecker::new();

    let expr = parse_expression("42").unwrap();
    let typed_expr = checker.convert_expression(&expr).unwrap();

    assert!(matches!(typed_expr.kind, TypedExpressionKind::Integer(42)));

    let type_name = checker
        .context()
        .interner
        .type_name(typed_expr.type_id)
        .unwrap();
    assert_eq!(type_name, "Outrun.Core.Integer64");
}

#[test]
fn test_convert_expression_string_literal() {
    let mut checker = TypeChecker::new();

    let expr = parse_expression(r#""Hello World""#).unwrap();
    let typed_expr = checker.convert_expression(&expr).unwrap();

    match typed_expr.kind {
        TypedExpressionKind::String(content) => {
            assert_eq!(content, "Hello World");
        }
        _ => panic!("Expected string expression, got {:?}", typed_expr.kind),
    }

    let type_name = checker
        .context()
        .interner
        .type_name(typed_expr.type_id)
        .unwrap();
    assert_eq!(type_name, "Outrun.Core.String");
}

#[test]
fn test_convert_expression_binary_operation() {
    let mut checker = TypeChecker::new();

    let expr = parse_expression("40 + 2").unwrap();
    let typed_expr = checker.convert_expression(&expr).unwrap();

    match &typed_expr.kind {
        TypedExpressionKind::BinaryOp {
            left,
            operator,
            right,
        } => {
            assert!(matches!(left.kind, TypedExpressionKind::Integer(40)));
            assert_eq!(*operator, outrun_parser::BinaryOperator::Add);
            assert!(matches!(right.kind, TypedExpressionKind::Integer(2)));
        }
        _ => panic!("Expected binary operation, got {:?}", typed_expr.kind),
    }
}

#[test]
fn test_convert_let_binding() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"let x = 42"#).unwrap();

    let let_binding = match &program.items[0].kind {
        outrun_parser::ItemKind::LetBinding(let_binding) => let_binding,
        _ => {
            panic!("Expected let binding item");
        }
    };

    let typed_let = checker.convert_let_binding(let_binding).unwrap();

    assert!(matches!(
        typed_let.expression.kind,
        TypedExpressionKind::Integer(42)
    ));

    let type_name = checker
        .context()
        .interner
        .type_name(typed_let.type_id)
        .unwrap();
    assert_eq!(type_name, "Outrun.Core.Integer64");
}

#[test]
fn test_convert_function_definition() {
    let mut checker = TypeChecker::new();

    let program = parse_program(
        r#"
    def add(x: Integer, y: Integer): Integer {
        x + y
    }
    "#,
    )
    .unwrap();

    let function_def = match &program.items[0].kind {
        outrun_parser::ItemKind::FunctionDefinition(func) => func,
        _ => {
            panic!("Expected function definition");
        }
    };

    let typed_func = checker.convert_function_definition(function_def).unwrap();

    assert_eq!(typed_func.name, "add");
    assert_eq!(typed_func.params.len(), 2);
    assert_eq!(typed_func.params[0].0, "x");
    assert_eq!(typed_func.params[1].0, "y");

    let x_type_name = checker
        .context()
        .interner
        .type_name(typed_func.params[0].1)
        .unwrap();
    assert_eq!(x_type_name, "Outrun.Core.Integer64");

    let y_type_name = checker
        .context()
        .interner
        .type_name(typed_func.params[1].1)
        .unwrap();
    assert_eq!(y_type_name, "Outrun.Core.Integer64");

    let return_type_name = checker
        .context()
        .interner
        .type_name(typed_func.return_type)
        .unwrap();
    assert_eq!(return_type_name, "Outrun.Core.Integer64");

    match &typed_func.body.kind {
        TypedExpressionKind::Block(block) => {
            assert_eq!(block.statements.len(), 1);
        }
        _ => panic!(
            "Expected block expression body, got {:?}",
            typed_func.body.kind
        ),
    }
}

#[test]
fn test_convert_const_definition() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"const ANSWER: Integer = 42"#).unwrap();

    let const_def = match &program.items[0].kind {
        outrun_parser::ItemKind::ConstDefinition(const_def) => const_def,
        _ => {
            panic!("Expected const definition");
        }
    };

    let typed_const = checker.convert_const_definition(const_def).unwrap();

    assert_eq!(typed_const.name, "ANSWER");
    assert!(matches!(
        typed_const.value.kind,
        TypedExpressionKind::Integer(42)
    ));

    let type_name = checker
        .context()
        .interner
        .type_name(typed_const.type_id)
        .unwrap();
    assert_eq!(type_name, "Outrun.Core.Integer64");
}

#[test]
fn test_convert_struct_definition() {
    let mut checker = TypeChecker::new();

    let program = parse_program(r#"struct User(name: String, age: Integer) {}"#).unwrap();

    let struct_def = match &program.items[0].kind {
        outrun_parser::ItemKind::StructDefinition(struct_def) => struct_def,
        _ => {
            panic!("Expected struct definition");
        }
    };

    let typed_struct = checker.convert_struct_definition(struct_def).unwrap();

    assert_eq!(typed_struct.name, "User");
    assert_eq!(typed_struct.fields.len(), 2);

    assert_eq!(typed_struct.fields[0].0, "name");
    assert_eq!(typed_struct.fields[1].0, "age");

    let name_type_name = checker
        .context()
        .interner
        .type_name(typed_struct.fields[0].1)
        .unwrap();
    assert_eq!(name_type_name, "Outrun.Core.String");

    let age_type_name = checker
        .context()
        .interner
        .type_name(typed_struct.fields[1].1)
        .unwrap();
    assert_eq!(age_type_name, "Outrun.Core.Integer64");
}

#[test]
fn test_convert_trait_definition_with_signature() {
    let mut checker = TypeChecker::new();

    let program = parse_program(
        r#"
    trait Display {
        def display(self: Self): String
    }
    "#,
    )
    .unwrap();

    let trait_def = match &program.items[0].kind {
        outrun_parser::ItemKind::TraitDefinition(trait_def) => trait_def,
        _ => {
            panic!("Expected trait definition");
        }
    };

    let typed_trait = checker.convert_trait_definition(trait_def).unwrap();

    assert_eq!(typed_trait.name, "Display");
    assert_eq!(typed_trait.functions.len(), 1);

    let func_sig = &typed_trait.functions[0];
    assert_eq!(func_sig.name, "display");
    assert_eq!(func_sig.params.len(), 1);
    assert_eq!(func_sig.params[0].0, "self");

    let self_type_name = checker
        .context()
        .interner
        .type_name(func_sig.params[0].1)
        .unwrap();
    assert_eq!(self_type_name, "Self");

    let return_type_name = checker
        .context()
        .interner
        .type_name(func_sig.return_type)
        .unwrap();
    assert_eq!(return_type_name, "Outrun.Core.String");
}

#[test]
fn test_convert_program_integration() {
    let mut checker = TypeChecker::new();

    let program = parse_program(
        r#"
    def add(x: Integer, y: Integer): Integer {
        x + y
    }
    
    const RESULT: Integer = 42
    
    add(x: 10, y: 20)
    "#,
    )
    .unwrap();

    let typed_program = checker.convert_program(&program).unwrap();

    assert_eq!(typed_program.items.len(), 3);

    match &typed_program.items[0].kind {
        TypedItemKind::FunctionDefinition(func) => {
            assert_eq!(func.name, "add");
        }
        _ => assert!(
            matches!(
                typed_program.items[0].kind,
                TypedItemKind::FunctionDefinition(_)
            ),
            "Expected function definition, got {:?}",
            typed_program.items[0].kind
        ),
    }

    match &typed_program.items[1].kind {
        TypedItemKind::ConstDefinition(const_def) => {
            assert_eq!(const_def.name, "RESULT");
        }
        _ => assert!(
            matches!(
                typed_program.items[1].kind,
                TypedItemKind::ConstDefinition(_)
            ),
            "Expected const definition, got {:?}",
            typed_program.items[1].kind
        ),
    }

    match &typed_program.items[2].kind {
        TypedItemKind::Expression(expr) => match &expr.kind {
            TypedExpressionKind::FunctionCall { name, args } => {
                assert_eq!(name, "add");
                assert_eq!(args.len(), 2);
            }
            _ => assert!(
                matches!(expr.kind, TypedExpressionKind::FunctionCall { .. }),
                "Expected function call, got {:?}",
                expr.kind
            ),
        },
        _ => panic!("Expected expression, got {:?}", typed_program.items[2].kind),
    }

    let stats = typed_program.dispatch_table.stats();
    assert_eq!(stats.trait_implementations, 0);
    assert_eq!(stats.static_functions, 0);
}

#[test]
fn test_convert_expression_error_handling() {
    let mut checker = TypeChecker::new();

    let expr = parse_expression("undefined_variable").unwrap();
    let result = checker.convert_expression(&expr);

    assert!(result.is_err());
}

#[test]
fn test_convert_function_with_missing_return_type() {
    let mut checker = TypeChecker::new();

    let program = parse_program(
        r#"
    def broken_func(x: Integer) {
        x + 1
    }
    "#,
    )
    .unwrap();

    let function_def = match &program.items[0].kind {
        outrun_parser::ItemKind::FunctionDefinition(func) => func,
        _ => {
            panic!("Expected function definition");
        }
    };

    let result = checker.convert_function_definition(function_def);

    assert!(result.is_err());
}
