//! Integration tests for pattern matching in let bindings and case expressions

use crate::{ExpressionEvaluator, InterpreterContext, Value};
use outrun_parser::{parse_expression, parse_program};

#[test]
fn test_simple_let_binding_pattern() {
    let mut context = InterpreterContext::new();
    let evaluator = ExpressionEvaluator::new();

    // Parse "let x = 42"
    let program = parse_program("let x = 42").unwrap();
    assert_eq!(program.items.len(), 1);

    // Evaluate the let binding
    let result = evaluator.evaluate_item(&program.items[0], &mut context);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), None); // Let bindings don't return values

    // Check that x was bound correctly
    assert_eq!(context.get_variable("x").unwrap(), &Value::integer(42));
}

#[test]
fn test_tuple_destructuring_let_binding() {
    let mut context = InterpreterContext::new();
    let evaluator = ExpressionEvaluator::new();

    // First create a tuple value (represented as a list)
    context
        .define_variable(
            "tuple_val".to_string(),
            Value::single_list(Value::integer(1))
                .into_list()
                .unwrap()
                .prepend_to_value(Value::integer(2)),
        )
        .unwrap();

    // Parse "let (x, y) = tuple_val"
    let program = parse_program("let (x, y) = tuple_val").unwrap();

    // Evaluate the destructuring let binding
    let result = evaluator.evaluate_item(&program.items[0], &mut context);
    assert!(result.is_ok());

    // Check that both variables were bound
    assert_eq!(context.get_variable("x").unwrap(), &Value::integer(2));
    assert_eq!(context.get_variable("y").unwrap(), &Value::integer(1));
}

#[test]
fn test_simple_case_expression() {
    let mut context = InterpreterContext::new();
    let evaluator = ExpressionEvaluator::new();

    // Parse a simple case expression
    let expr = parse_expression(
        r#"case 42 {
        42 -> "matched"
        _ -> "not matched"
    }"#,
    )
    .unwrap();

    let result = evaluator.evaluate(&expr, &mut context).unwrap();
    assert_eq!(result, Value::string("matched".to_string()));
}

#[test]
fn test_case_expression_with_pattern_binding() {
    let mut context = InterpreterContext::new();
    let evaluator = ExpressionEvaluator::new();

    // Create a list value first
    let list_val = Value::single_list(Value::integer(1))
        .into_list()
        .unwrap()
        .prepend_to_value(Value::integer(2));
    context
        .define_variable("my_list".to_string(), list_val)
        .unwrap();

    // Parse case expression with list pattern
    let expr = parse_expression(
        r#"case my_list {
        [x, y] -> x
        [] -> 0
    }"#,
    )
    .unwrap();

    let result = evaluator.evaluate(&expr, &mut context).unwrap();
    assert_eq!(result, Value::integer(2)); // First element of [2, 1]
}

// Helper method to make it easier to create lists for testing
impl Value {
    fn into_list(self) -> Option<crate::value::List> {
        match self {
            Value::List { list, .. } => Some((*list).clone()),
            _ => None,
        }
    }
}

// Helper method to prepend to a List and create a new Value::List
impl crate::value::List {
    fn prepend_to_value(self, element: Value) -> Value {
        use std::rc::Rc;
        Value::List {
            list: Rc::new(crate::value::List::Cons {
                head: element,
                tail: Rc::new(self),
            }),
            element_type_info: None,
        }
    }
}
