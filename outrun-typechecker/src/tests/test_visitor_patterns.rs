//! Tests for the visitor pattern implementations

use crate::visitor::{Visitor, walk_expression, walk_program, VisitorResult, ExpressionCounter, IdentifierCollector};
use outrun_parser::{
    BinaryOperation, BinaryOperator, BooleanLiteral, DebugInfo, Expression, ExpressionKind,
    FunctionCall, FunctionPath, Identifier, IntegerFormat, IntegerLiteral, Item, ItemKind,
    Program, Span, TypeIdentifier, Argument,
};

fn create_test_span() -> Span {
    Span::new(0, 10)
}

fn create_test_program(items: Vec<Item>) -> Program {
    Program {
        items,
        debug_info: DebugInfo::new(),
        span: create_test_span(),
    }
}

fn create_identifier_item(name: &str) -> Item {
    Item {
        kind: ItemKind::Identifier(Identifier {
            name: name.to_string(),
            span: create_test_span(),
        }),
        span: create_test_span(),
    }
}

fn create_type_identifier_item(name: &str) -> Item {
    Item {
        kind: ItemKind::TypeIdentifier(TypeIdentifier {
            name: name.to_string(),
            span: create_test_span(),
        }),
        span: create_test_span(),
    }
}

fn create_integer_expression(value: i64) -> Expression {
    Expression {
        kind: ExpressionKind::Integer(IntegerLiteral {
            value,
            format: IntegerFormat::Decimal,
            span: create_test_span(),
        }),
        span: create_test_span(),
    }
}

fn create_boolean_expression(value: bool) -> Expression {
    Expression {
        kind: ExpressionKind::Boolean(BooleanLiteral {
            value,
            span: create_test_span(),
        }),
        span: create_test_span(),
    }
}

fn create_binary_operation(left: Expression, operator: BinaryOperator, right: Expression) -> Expression {
    Expression {
        kind: ExpressionKind::BinaryOp(BinaryOperation {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            span: create_test_span(),
        }),
        span: create_test_span(),
    }
}

#[test]
fn test_identifier_collector_basic() {
    let program = create_test_program(vec![
        create_identifier_item("variable1"),
        create_identifier_item("variable2"),
        create_type_identifier_item("TypeName"),
    ]);

    let mut collector = IdentifierCollector::default();
    collector.visit_program(&program).unwrap();

    assert_eq!(collector.identifiers.len(), 3);
    assert!(collector.identifiers.contains(&"variable1".to_string()));
    assert!(collector.identifiers.contains(&"variable2".to_string()));
    assert!(collector.identifiers.contains(&"TypeName".to_string()));
}

#[test]
fn test_identifier_collector_empty_program() {
    let program = create_test_program(vec![]);

    let mut collector = IdentifierCollector::default();
    collector.visit_program(&program).unwrap();

    assert!(collector.identifiers.is_empty());
}

#[test]
fn test_expression_counter_basic() {
    // Create: 1 + true (binary op with two literals)
    let binary_expr = create_binary_operation(
        create_integer_expression(1),
        BinaryOperator::Add,
        create_boolean_expression(true),
    );

    let program = create_test_program(vec![Item {
        kind: ItemKind::Expression(binary_expr),
        span: create_test_span(),
    }]);

    let mut counter = ExpressionCounter::default();
    counter.visit_program(&program).unwrap();

    assert_eq!(counter.binary_ops, 1);
    assert_eq!(counter.literals, 2); // integer and boolean literals
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_expression_counter_nested_operations() {
    // Create: (1 + 2) * (3 + 4) - nested binary operations with literals
    let left_side = create_binary_operation(
        create_integer_expression(1),
        BinaryOperator::Add,
        create_integer_expression(2),
    );
    
    let right_side = create_binary_operation(
        create_integer_expression(3),
        BinaryOperator::Add,
        create_integer_expression(4),
    );

    let outer_expr = create_binary_operation(left_side, BinaryOperator::Multiply, right_side);

    let program = create_test_program(vec![Item {
        kind: ItemKind::Expression(outer_expr),
        span: create_test_span(),
    }]);

    let mut counter = ExpressionCounter::default();
    counter.visit_program(&program).unwrap();

    assert_eq!(counter.binary_ops, 3); // Three binary operations total
    assert_eq!(counter.literals, 4); // Four integer literals
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_expression_counter_function_calls() {
    // Create a function call expression
    let function_call = Expression {
        kind: ExpressionKind::FunctionCall(FunctionCall {
            path: FunctionPath::Simple {
                name: Identifier {
                    name: "test_function".to_string(),
                    span: create_test_span(),
                },
            },
            arguments: vec![
                Argument::Named {
                    name: Identifier {
                        name: "arg1".to_string(),
                        span: create_test_span(),
                    },
                    expression: create_integer_expression(42),
                    format: outrun_parser::ArgumentFormat::Explicit,
                    span: create_test_span(),
                }
            ],
            span: create_test_span(),
        }),
        span: create_test_span(),
    };

    let program = create_test_program(vec![Item {
        kind: ItemKind::Expression(function_call),
        span: create_test_span(),
    }]);

    let mut counter = ExpressionCounter::default();
    counter.visit_program(&program).unwrap();

    assert_eq!(counter.function_calls, 1);
    assert_eq!(counter.literals, 1); // The argument literal
    assert_eq!(counter.binary_ops, 0);
}

#[test]
fn test_walk_expression_basic() {
    let expr = create_integer_expression(42);
    let mut counter = ExpressionCounter::default();
    
    counter.visit_expression(&expr).unwrap();
    
    assert_eq!(counter.literals, 1);
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_walk_expression_binary_operation() {
    let binary_expr = create_binary_operation(
        create_integer_expression(1),
        BinaryOperator::Add,
        create_integer_expression(2),
    );
    
    let mut counter = ExpressionCounter::default();
    counter.visit_expression(&binary_expr).unwrap();
    
    assert_eq!(counter.binary_ops, 1);
    assert_eq!(counter.literals, 2);
}

#[test]
fn test_walk_program_empty() {
    let program = create_test_program(vec![]);
    let mut counter = ExpressionCounter::default();
    
    counter.visit_program(&program).unwrap();
    
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.literals, 0);
    assert_eq!(counter.function_calls, 0);
}

#[test]
fn test_walk_program_multiple_items() {
    let program = create_test_program(vec![
        Item {
            kind: ItemKind::Expression(create_integer_expression(1)),
            span: create_test_span(),
        },
        Item {
            kind: ItemKind::Expression(create_boolean_expression(true)),
            span: create_test_span(),
        },
        create_identifier_item("test_var"),
    ]);

    let mut counter = ExpressionCounter::default();
    counter.visit_program(&program).unwrap();

    assert_eq!(counter.literals, 2); // integer and boolean
    assert_eq!(counter.binary_ops, 0);
    assert_eq!(counter.function_calls, 0);
}

// Custom visitor for testing
#[derive(Default)]
struct BinaryOperatorCollector {
    operators: Vec<BinaryOperator>,
}

impl<T> Visitor<T> for BinaryOperatorCollector {
    fn visit_binary_operation(&mut self, op: &BinaryOperation) -> VisitorResult {
        self.operators.push(op.operator.clone());
        // Continue traversing
        walk_expression(self, &op.left)?;
        walk_expression(self, &op.right)?;
        Ok(())
    }
}

#[test]
fn test_custom_visitor() {
    // Create: (1 + 2) * 3 - should collect Add and Multiply operators
    let inner_add = create_binary_operation(
        create_integer_expression(1),
        BinaryOperator::Add,
        create_integer_expression(2),
    );
    
    let outer_multiply = create_binary_operation(
        inner_add,
        BinaryOperator::Multiply,
        create_integer_expression(3),
    );

    let program = create_test_program(vec![Item {
        kind: ItemKind::Expression(outer_multiply),
        span: create_test_span(),
    }]);

    let mut collector = BinaryOperatorCollector::default();
    collector.visit_program(&program).unwrap();

    assert_eq!(collector.operators.len(), 2);
    assert!(collector.operators.contains(&BinaryOperator::Add));
    assert!(collector.operators.contains(&BinaryOperator::Multiply));
}

#[test]
fn test_visitor_error_propagation() {
    // Test that visitor errors are properly propagated
    // Since our basic visitors don't return errors, we'll test with a successful case
    let program = create_test_program(vec![create_identifier_item("test")]);
    
    let mut collector = IdentifierCollector::default();
    let result = collector.visit_program(&program);
    
    assert!(result.is_ok());
    assert_eq!(collector.identifiers.len(), 1);
}

#[test]
fn test_visitor_pattern_extensibility() {
    // Test that the visitor pattern allows for easy extension
    // by creating a visitor that only counts specific types of expressions
    
    #[derive(Default)]
    struct IntegerLiteralCounter {
        count: usize,
    }
    
    impl<T> Visitor<T> for IntegerLiteralCounter {
        fn visit_literal(&mut self, kind: &ExpressionKind, _span: &Span) -> VisitorResult {
            if matches!(kind, ExpressionKind::Integer(_)) {
                self.count += 1;
            }
            Ok(())
        }
    }
    
    let program = create_test_program(vec![
        Item {
            kind: ItemKind::Expression(create_integer_expression(1)),
            span: create_test_span(),
        },
        Item {
            kind: ItemKind::Expression(create_boolean_expression(true)),
            span: create_test_span(),
        },
        Item {
            kind: ItemKind::Expression(create_integer_expression(2)),
            span: create_test_span(),
        },
    ]);
    
    let mut counter = IntegerLiteralCounter::default();
    counter.visit_program(&program).unwrap();
    
    assert_eq!(counter.count, 2); // Only counts integer literals, not boolean
}