//! Tests for spread operator functionality in lists, function arguments, and struct literals

use crate::context::InterpreterContext;
use crate::evaluator::ExpressionEvaluator;
use crate::value::Value;
use outrun_parser::parse_expression;

#[test]
fn test_list_spread_basic() {
    let mut context = InterpreterContext::new();

    // Set up a list to spread
    let original_list = Value::List {
        list: std::rc::Rc::new(crate::value::List::Cons {
            head: Value::integer(2),
            tail: std::rc::Rc::new(crate::value::List::Cons {
                head: Value::integer(3),
                tail: std::rc::Rc::new(crate::value::List::Empty),
            }),
        }),
        element_type_info: None,
    };
    context
        .define_variable("middle".to_string(), original_list)
        .unwrap();

    // Parse expression with spread: [1, ..middle, 4]
    let expr = parse_expression("[1, ..middle, 4]").unwrap();

    let evaluator = ExpressionEvaluator::new();
    let result = evaluator.evaluate(&expr, &mut context).unwrap();

    // Verify the result is [1, 2, 3, 4]
    match result {
        Value::List { list, .. } => {
            let elements = collect_list_elements(&list);
            assert_eq!(elements.len(), 4);
            assert_eq!(elements[0], Value::integer(1));
            assert_eq!(elements[1], Value::integer(2));
            assert_eq!(elements[2], Value::integer(3));
            assert_eq!(elements[3], Value::integer(4));
        }
        _ => panic!("Expected List, got {:?}", result),
    }
}

#[test]
fn test_list_spread_empty() {
    let mut context = InterpreterContext::new();

    // Set up an empty list to spread
    context
        .define_variable("empty".to_string(), Value::empty_list())
        .unwrap();

    // Parse expression with spread: [1, ..empty, 2]
    let expr = parse_expression("[1, ..empty, 2]").unwrap();

    let evaluator = ExpressionEvaluator::new();
    let result = evaluator.evaluate(&expr, &mut context).unwrap();

    // Verify the result is [1, 2]
    match result {
        Value::List { list, .. } => {
            let elements = collect_list_elements(&list);
            assert_eq!(elements.len(), 2);
            assert_eq!(elements[0], Value::integer(1));
            assert_eq!(elements[1], Value::integer(2));
        }
        _ => panic!("Expected List, got {:?}", result),
    }
}

#[test]
fn test_struct_spread_basic() {
    let mut context = InterpreterContext::new();

    // Set up a struct to spread
    let mut base_fields = std::collections::HashMap::new();
    base_fields.insert("x".to_string(), Value::integer(10));
    base_fields.insert("y".to_string(), Value::integer(20));

    let base_struct = Value::Struct {
        type_name: "Point".to_string(),
        fields: base_fields,
        type_info: None,
    };
    context
        .define_variable("base".to_string(), base_struct)
        .unwrap();

    // Parse expression with spread: Point { z: 30, ..base }
    let expr = parse_expression("Point { z: 30, ..base }").unwrap();

    let evaluator = ExpressionEvaluator::new();
    let result = evaluator.evaluate(&expr, &mut context).unwrap();

    // Verify the result has all three fields
    match result {
        Value::Struct { fields, .. } => {
            assert_eq!(fields.len(), 3);
            assert_eq!(fields.get("x"), Some(&Value::integer(10)));
            assert_eq!(fields.get("y"), Some(&Value::integer(20)));
            assert_eq!(fields.get("z"), Some(&Value::integer(30)));
        }
        _ => panic!("Expected Struct, got {:?}", result),
    }
}

// Helper function to collect list elements
fn collect_list_elements(list: &crate::value::List) -> Vec<Value> {
    let mut elements = Vec::new();
    let mut current = list;

    loop {
        match current {
            crate::value::List::Empty => break,
            crate::value::List::Cons { head, tail } => {
                elements.push(head.clone());
                current = tail;
            }
        }
    }

    elements
}
