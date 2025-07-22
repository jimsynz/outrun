//! Operator Desugaring Engine
//!
//! Transforms binary and unary operators into their corresponding protocol function calls,
//! implementing Outrun's "everything is protocols" philosophy.
//!
//! ## Desugaring Rules
//!
//! **Binary Operators → Protocol Calls:**
//! - `a + b` → `BinaryAddition.add(left: a, right: b)`
//! - `a - b` → `BinarySubtraction.subtract(left: a, right: b)`
//! - `a == b` → `Equality.equal?(left: a, right: b)`
//! - `a != b` → `!Equality.equal?(left: a, right: b)` (two-step transformation)
//!
//! **Unary Operators → Protocol Calls:**
//! - `+a` → `UnaryPlus.plus(value: a)`
//! - `-a` → `UnaryMinus.minus(value: a)`
//! - `!a` → `LogicalNot.not?(value: a)`
//! - `~a` → `BitwiseNot.bitwise_not(value: a)`

use outrun_parser::{
    BinaryOperator, BinaryOperation, UnaryOperator, UnaryOperation,
    Expression, ExpressionKind, FunctionCall, FunctionPath, Argument, ArgumentFormat,
    Identifier, TypeIdentifier,
    Program, Item, ItemKind
};

use crate::error::TypecheckError;

/// Engine responsible for desugaring operators into protocol function calls
pub struct DesugaringEngine {
    /// Track transformations for debugging and error reporting
    pub transformations: Vec<String>,
}

impl DesugaringEngine {
    /// Create a new desugaring engine
    pub fn new() -> Self {
        Self {
            transformations: Vec::new(),
        }
    }

    /// Desugar all operators in a program, transforming them into protocol function calls
    #[allow(clippy::result_large_err)]
    pub fn desugar_program(&mut self, program: &mut Program) -> Result<(), TypecheckError> {
        for item in &mut program.items {
            self.desugar_item(item)?;
        }
        Ok(())
    }

    /// Desugar operators in a single item
    #[allow(clippy::result_large_err)]
    fn desugar_item(&mut self, item: &mut Item) -> Result<(), TypecheckError> {
        match &mut item.kind {
            ItemKind::Expression(expr) => {
                self.desugar_expression(expr)?;
            }
            ItemKind::FunctionDefinition(func_def) => {
                // FunctionDefinition.body is a Block, not Option<Block>
                // For now, we don't desugar function bodies at the item level
                // They'll be desugared when type checking the function body expressions
                // TODO: Add block expression desugaring when implementing control flow
                let _ = &func_def.body; // Acknowledge the field exists
            }
            ItemKind::LetBinding(let_binding) => {
                self.desugar_expression(&mut let_binding.expression)?;
            }
            ItemKind::ConstDefinition(const_def) => {
                self.desugar_expression(&mut const_def.expression)?;
            }
            // Other item types don't contain expressions that need desugaring
            _ => {}
        }
        Ok(())
    }

    /// Iteratively desugar all operators in an expression tree to prevent stack overflow
    #[allow(clippy::result_large_err)]
    pub fn desugar_expression(&mut self, expr: &mut Expression) -> Result<(), TypecheckError> {
        // Use iterative approach to handle deep expression trees
        let mut work_stack: Vec<&mut Expression> = vec![expr];
        let mut transform_stack: Vec<*mut Expression> = Vec::new();
        
        // Phase 1: Collect all expressions that need desugaring (post-order traversal)
        while let Some(current_expr) = work_stack.pop() {
            let expr_ptr = current_expr as *mut Expression;
            transform_stack.push(expr_ptr);
            
            // Add child expressions to work stack
            match &mut current_expr.kind {
                ExpressionKind::BinaryOp(binary_op) => {
                    work_stack.push(&mut binary_op.left);
                    work_stack.push(&mut binary_op.right);
                }
                ExpressionKind::UnaryOp(unary_op) => {
                    work_stack.push(&mut unary_op.operand);
                }
                ExpressionKind::FunctionCall(func_call) => {
                    for arg in &mut func_call.arguments {
                        match arg {
                            Argument::Named { expression, .. } => work_stack.push(expression),
                            Argument::Spread { expression, .. } => work_stack.push(expression),
                        }
                    }
                }
                ExpressionKind::List(list_literal) => {
                    for element in &mut list_literal.elements {
                        if let outrun_parser::ListElement::Expression(expr) = element {
                            work_stack.push(expr);
                        }
                    }
                }
                ExpressionKind::Tuple(tuple_literal) => {
                    for element in &mut tuple_literal.elements {
                        work_stack.push(element);
                    }
                }
                ExpressionKind::Map(map_literal) => {
                    for entry in &mut map_literal.entries {
                        match entry {
                            outrun_parser::MapEntry::Assignment { key, value } => {
                                work_stack.push(key);
                                work_stack.push(value);
                            }
                            outrun_parser::MapEntry::Shorthand { value, .. } => {
                                work_stack.push(value);
                            }
                            outrun_parser::MapEntry::Spread(_) => {
                                // Spread entries just reference an identifier, no expression to desugar
                            }
                        }
                    }
                }
                ExpressionKind::IfExpression(if_expr) => {
                    work_stack.push(&mut if_expr.condition);
                    // if_expr.then_block and else_block are Block types, not Expression
                    // For now, we don't desugar block expressions
                    // TODO: Add block expression desugaring when implementing control flow
                }
                ExpressionKind::CaseExpression(case_expr) => {
                    work_stack.push(&mut case_expr.expression);
                    for clause in &mut case_expr.clauses {
                        if let Some(ref mut guard) = clause.guard {
                            work_stack.push(guard);
                        }
                        // clause.result is a CaseResult enum (Block or Expression)
                        // For now, we don't desugar case results
                        // TODO: Add case result desugaring when implementing control flow
                    }
                }
                ExpressionKind::Parenthesized(inner_expr) => {
                    work_stack.push(inner_expr);
                }
                // Literals and identifiers don't contain nested expressions
                _ => {}
            }
        }
        
        // Phase 2: Transform expressions in post-order (children first)
        for expr_ptr in transform_stack.into_iter().rev() {
            unsafe {
                let current_expr = &mut *expr_ptr;
                match &mut current_expr.kind {
                    ExpressionKind::BinaryOp(binary_op) => {
                        // Transform this binary operation
                        let desugared_call = self.desugar_binary_operation(binary_op)?;
                        current_expr.kind = ExpressionKind::FunctionCall(desugared_call);
                    }
                    ExpressionKind::UnaryOp(unary_op) => {
                        // Transform this unary operation
                        let desugared_call = self.desugar_unary_operation(unary_op)?;
                        current_expr.kind = ExpressionKind::FunctionCall(desugared_call);
                    }
                    // Other expressions don't need transformation, just traversal
                    _ => {}
                }
            }
        }
        
        Ok(())
    }

    /// Transform a binary operation into a protocol function call
    #[allow(clippy::result_large_err)]
    fn desugar_binary_operation(&mut self, binary_op: &BinaryOperation) -> Result<FunctionCall, TypecheckError> {
        let (protocol_name, function_name, param_names) = match binary_op.operator {
            // Arithmetic operators
            BinaryOperator::Add => ("BinaryAddition", "add", ("left", "right")),
            BinaryOperator::Subtract => ("BinarySubtraction", "subtract", ("left", "right")),
            BinaryOperator::Multiply => ("BinaryMultiplication", "multiply", ("left", "right")),
            BinaryOperator::Divide => ("BinaryDivision", "divide", ("left", "right")),
            BinaryOperator::Modulo => ("BinaryModulo", "modulo", ("left", "right")),
            BinaryOperator::Exponent => ("BinaryExponentiation", "power", ("base", "exponent")),

            // Comparison operators
            BinaryOperator::Equal => ("Equality", "equal?", ("left", "right")),
            BinaryOperator::Less => ("Comparison", "less_than?", ("left", "right")),
            BinaryOperator::LessEqual => ("Comparison", "less_than_or_equal?", ("left", "right")),
            BinaryOperator::Greater => ("Comparison", "greater_than?", ("left", "right")),
            BinaryOperator::GreaterEqual => ("Comparison", "greater_than_or_equal?", ("left", "right")),

            // Logical operators
            BinaryOperator::LogicalAnd => ("LogicalAnd", "and?", ("left", "right")),
            BinaryOperator::LogicalOr => ("LogicalOr", "or?", ("left", "right")),

            // Bitwise operators
            BinaryOperator::BitwiseAnd => ("BitwiseAnd", "bitwise_and", ("left", "right")),
            BinaryOperator::BitwiseOr => ("BitwiseOr", "bitwise_or", ("left", "right")),
            BinaryOperator::BitwiseXor => ("BitwiseXor", "bitwise_xor", ("left", "right")),
            BinaryOperator::ShiftLeft => ("BitShift", "shift_left", ("value", "positions")),
            BinaryOperator::ShiftRight => ("BitShift", "shift_right", ("value", "positions")),

            // Pipe operators
            BinaryOperator::Pipe => ("Pipe", "pipe_into", ("value", "function")),
            BinaryOperator::PipeMaybe => ("Maybe", "maybe_pipe", ("value", "function")),

            // Special case: != desugars to negated ==
            BinaryOperator::NotEqual => {
                return self.desugar_not_equal_operation(binary_op);
            }

            // Type annotation operator (not a protocol call)
            BinaryOperator::As => {
                use crate::error::to_source_span;
                return Err(TypecheckError::Generic {
                    message: "Type annotation operator 'as' should not be desugared".to_string(),
                    span: to_source_span(Some(binary_op.span)),
                });
            }
        };

        // Record the transformation for debugging
        self.transformations.push(format!("Binary {} → {}.{}", 
            format!("{:?}", binary_op.operator).to_lowercase(),
            protocol_name, 
            function_name
        ));

        // Create the function call with named parameters
        Ok(FunctionCall {
            path: FunctionPath::Qualified {
                module: TypeIdentifier {
                    name: protocol_name.to_string(),
                    span: binary_op.span,
                },
                name: Identifier {
                    name: function_name.to_string(),
                    span: binary_op.span,
                },
            },
            arguments: vec![
                Argument::Named {
                    name: Identifier {
                        name: param_names.0.to_string(),
                        span: binary_op.left.span,
                    },
                    expression: *binary_op.left.clone(),
                    format: ArgumentFormat::Explicit,
                    span: binary_op.left.span,
                },
                Argument::Named {
                    name: Identifier {
                        name: param_names.1.to_string(),
                        span: binary_op.right.span,
                    },
                    expression: *binary_op.right.clone(),
                    format: ArgumentFormat::Explicit,
                    span: binary_op.right.span,
                },
            ],
            span: binary_op.span,
        })
    }

    /// Special case: Transform `a != b` into `!Equality.equal?(left: a, right: b)`
    #[allow(clippy::result_large_err)]
    fn desugar_not_equal_operation(&mut self, binary_op: &BinaryOperation) -> Result<FunctionCall, TypecheckError> {
        // Record the transformation
        self.transformations.push("Binary not_equal → !Equality.equal?".to_string());

        // First create the equality call
        let equality_function_call = FunctionCall {
            path: FunctionPath::Qualified {
                module: TypeIdentifier {
                    name: "Equality".to_string(),
                    span: binary_op.span,
                },
                name: Identifier {
                    name: "equal?".to_string(),
                    span: binary_op.span,
                },
            },
            arguments: vec![
                Argument::Named {
                    name: Identifier {
                        name: "left".to_string(),
                        span: binary_op.left.span,
                    },
                    expression: *binary_op.left.clone(),
                    format: ArgumentFormat::Explicit,
                    span: binary_op.left.span,
                },
                Argument::Named {
                    name: Identifier {
                        name: "right".to_string(),
                        span: binary_op.right.span,
                    },
                    expression: *binary_op.right.clone(),
                    format: ArgumentFormat::Explicit,
                    span: binary_op.right.span,
                },
            ],
            span: binary_op.span,
        };

        // Then wrap it in a LogicalNot call
        Ok(FunctionCall {
            path: FunctionPath::Qualified {
                module: TypeIdentifier {
                    name: "LogicalNot".to_string(),
                    span: binary_op.span,
                },
                name: Identifier {
                    name: "not?".to_string(),
                    span: binary_op.span,
                },
            },
            arguments: vec![
                Argument::Named {
                    name: Identifier {
                        name: "value".to_string(),
                        span: binary_op.span,
                    },
                    expression: Expression {
                        kind: ExpressionKind::FunctionCall(equality_function_call),
                        span: binary_op.span,
                        type_info: None,
                    },
                    format: ArgumentFormat::Explicit,
                    span: binary_op.span,
                },
            ],
            span: binary_op.span,
        })
    }

    /// Transform a unary operation into a protocol function call
    #[allow(clippy::result_large_err)]
    fn desugar_unary_operation(&mut self, unary_op: &UnaryOperation) -> Result<FunctionCall, TypecheckError> {
        let (protocol_name, function_name) = match unary_op.operator {
            UnaryOperator::Plus => ("UnaryPlus", "plus"),
            UnaryOperator::Minus => ("UnaryMinus", "minus"),
            UnaryOperator::LogicalNot => ("LogicalNot", "not?"),
            UnaryOperator::BitwiseNot => ("BitwiseNot", "bitwise_not"),
        };

        // Record the transformation for debugging
        self.transformations.push(format!("Unary {} → {}.{}", 
            format!("{:?}", unary_op.operator).to_lowercase(),
            protocol_name, 
            function_name
        ));

        // Create the function call with the operand as the 'value' parameter
        Ok(FunctionCall {
            path: FunctionPath::Qualified {
                module: TypeIdentifier {
                    name: protocol_name.to_string(),
                    span: unary_op.span,
                },
                name: Identifier {
                    name: function_name.to_string(),
                    span: unary_op.span,
                },
            },
            arguments: vec![
                Argument::Named {
                    name: Identifier {
                        name: "value".to_string(),
                        span: unary_op.operand.span,
                    },
                    expression: *unary_op.operand.clone(),
                    format: ArgumentFormat::Explicit,
                    span: unary_op.operand.span,
                },
            ],
            span: unary_op.span,
        })
    }
}

impl Default for DesugaringEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::parse_expression;

    #[test]
    fn test_binary_addition_desugaring() {
        let mut engine = DesugaringEngine::new();
        
        // Parse "1 + 2"
        let mut expr = parse_expression("1 + 2").unwrap();
        
        // Desugar the expression
        engine.desugar_expression(&mut expr).unwrap();
        
        // Should become BinaryAddition.add(left: 1, right: 2)
        match expr.kind {
            ExpressionKind::FunctionCall(func_call) => {
                // Check function is BinaryAddition.add
                match &func_call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "BinaryAddition");
                        assert_eq!(name.name, "add");
                    }
                    _ => panic!("Expected qualified function path for desugared function"),
                }
                
                // Check arguments
                assert_eq!(func_call.arguments.len(), 2);
                
                match &func_call.arguments[0] {
                    Argument::Named { name, expression, .. } => {
                        assert_eq!(name.name, "left");
                        match &expression.kind {
                            ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 1),
                            _ => panic!("Expected integer literal for left operand"),
                        }
                    }
                    _ => panic!("Expected named argument for left operand"),
                }
                
                match &func_call.arguments[1] {
                    Argument::Named { name, expression, .. } => {
                        assert_eq!(name.name, "right");
                        match &expression.kind {
                            ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 2),
                            _ => panic!("Expected integer literal for right operand"),
                        }
                    }
                    _ => panic!("Expected named argument for right operand"),
                }
            }
            _ => panic!("Expected function call after desugaring"),
        }
        
        // Check transformation was recorded
        assert_eq!(engine.transformations.len(), 1);
        assert!(engine.transformations[0].contains("Binary add → BinaryAddition.add"));
    }

    #[test]
    fn test_unary_minus_desugaring() {
        let mut engine = DesugaringEngine::new();
        
        // Parse "-42"
        let mut expr = parse_expression("-42").unwrap();
        
        // Desugar the expression
        engine.desugar_expression(&mut expr).unwrap();
        
        // Should become UnaryMinus.minus(value: 42)
        match expr.kind {
            ExpressionKind::FunctionCall(func_call) => {
                // Check function is UnaryMinus.minus
                match &func_call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "UnaryMinus");
                        assert_eq!(name.name, "minus");
                    }
                    _ => panic!("Expected qualified function path for desugared function"),
                }
                
                // Check argument
                assert_eq!(func_call.arguments.len(), 1);
                match &func_call.arguments[0] {
                    Argument::Named { name, expression, .. } => {
                        assert_eq!(name.name, "value");
                        match &expression.kind {
                            ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 42),
                            _ => panic!("Expected integer literal for operand"),
                        }
                    }
                    _ => panic!("Expected named argument for operand"),
                }
            }
            _ => panic!("Expected function call after desugaring"),
        }
        
        // Check transformation was recorded
        assert_eq!(engine.transformations.len(), 1);
        assert!(engine.transformations[0].contains("Unary minus → UnaryMinus.minus"));
    }

    #[test]
    fn test_not_equal_special_case_desugaring() {
        let mut engine = DesugaringEngine::new();
        
        // Parse "a != b"  
        let mut expr = parse_expression("a != b").unwrap();
        
        // Desugar the expression
        engine.desugar_expression(&mut expr).unwrap();
        
        // Should become LogicalNot.not?(value: Equality.equal?(left: a, right: b))
        match expr.kind {
            ExpressionKind::FunctionCall(outer_call) => {
                // Check outer function is LogicalNot.not?
                match &outer_call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "LogicalNot");
                        assert_eq!(name.name, "not?");
                    }
                    _ => panic!("Expected LogicalNot for outer function"),
                }
                
                // Check inner function is Equality.equal?
                assert_eq!(outer_call.arguments.len(), 1);
                match &outer_call.arguments[0] {
                    Argument::Named { name, expression, .. } => {
                        assert_eq!(name.name, "value");
                        match &expression.kind {
                            ExpressionKind::FunctionCall(inner_call) => {
                                match &inner_call.path {
                                    FunctionPath::Qualified { module, name } => {
                                        assert_eq!(module.name, "Equality");
                                        assert_eq!(name.name, "equal?");
                                    }
                                    _ => panic!("Expected Equality.equal? for inner function"),
                                }
                            }
                            _ => panic!("Expected function call as inner expression"),
                        }
                    }
                    _ => panic!("Expected named argument for LogicalNot"),
                }
            }
            _ => panic!("Expected function call after desugaring"),
        }
    }

    #[test]
    fn test_nested_operations_desugaring() {
        let mut engine = DesugaringEngine::new();
        
        // Parse "1 + 2 * 3" (should parse as 1 + (2 * 3) due to precedence)
        let mut expr = parse_expression("1 + 2 * 3").unwrap();
        
        // Desugar the expression  
        engine.desugar_expression(&mut expr).unwrap();
        
        // Should become BinaryAddition.add(left: 1, right: BinaryMultiplication.multiply(left: 2, right: 3))
        match expr.kind {
            ExpressionKind::FunctionCall(outer_call) => {
                // Check outer operation is addition
                match &outer_call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "BinaryAddition");
                        assert_eq!(name.name, "add");
                    }
                    _ => panic!("Expected BinaryAddition for outer operation"),
                }
                
                // Check right operand is a multiplication function call
                match &outer_call.arguments[1] {
                    Argument::Named { expression, .. } => {
                        match &expression.kind {
                            ExpressionKind::FunctionCall(inner_call) => {
                                match &inner_call.path {
                                    FunctionPath::Qualified { module, name } => {
                                        assert_eq!(module.name, "BinaryMultiplication");
                                        assert_eq!(name.name, "multiply");
                                    }
                                    _ => panic!("Expected BinaryMultiplication for inner operation"),
                                }
                            }
                            _ => panic!("Expected function call for right operand"),
                        }
                    }
                    _ => panic!("Expected named argument"),
                }
            }
            _ => panic!("Expected function call after desugaring"),
        }
        
        // Should have recorded multiple transformations
        assert_eq!(engine.transformations.len(), 2);
    }
}