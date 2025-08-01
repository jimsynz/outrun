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
//! - `a != b` → `Equality.not_equal?(left: a, right: b)`
//!
//! **Unary Operators → Protocol Calls:**
//! - `+a` → `UnaryPlus.plus(value: a)`
//! - `-a` → `UnaryMinus.minus(value: a)`
//! - `!a` → `LogicalNot.not?(value: a)`
//! - `~a` → `BitwiseNot.bitwise_not(value: a)`

use outrun_parser::{
    Argument, ArgumentFormat, BinaryOperation, BinaryOperator, Expression, ExpressionKind,
    FunctionCall, FunctionPath, Identifier, Item, ItemKind, Program, TypeIdentifier,
    UnaryOperation, UnaryOperator,
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
    pub fn desugar_program(&mut self, program: &mut Program) -> Result<(), TypecheckError> {
        eprintln!(
            "🔧 Starting program desugaring with {} items",
            program.items.len()
        );
        for (i, item) in program.items.iter_mut().enumerate() {
            eprintln!("🔧 Desugaring item {}", i);
            self.desugar_item(item)?;
        }
        eprintln!(
            "🔧 Program desugaring complete. Total transformations: {}",
            self.transformations.len()
        );
        Ok(())
    }

    /// Desugar operators in a single item
    fn desugar_item(&mut self, item: &mut Item) -> Result<(), TypecheckError> {
        match &mut item.kind {
            ItemKind::Expression(expr) => {
                self.desugar_expression(expr)?;
            }
            ItemKind::FunctionDefinition(func_def) => {
                // Desugar guard expressions in function definitions
                if let Some(ref mut guard) = func_def.guard {
                    self.desugar_expression(&mut guard.condition)?;
                }
                // Desugar expressions in function body
                self.desugar_block(&mut func_def.body)?;
            }
            ItemKind::LetBinding(let_binding) => {
                self.desugar_expression(&mut let_binding.expression)?;
            }
            ItemKind::ConstDefinition(const_def) => {
                self.desugar_expression(&mut const_def.expression)?;
            }
            ItemKind::StructDefinition(struct_def) => {
                // Desugar expressions in struct function bodies
                for function in &mut struct_def.functions {
                    if let Some(ref mut guard) = function.guard {
                        self.desugar_expression(&mut guard.condition)?;
                    }
                    self.desugar_block(&mut function.body)?;
                }
            }
            ItemKind::ProtocolDefinition(protocol_def) => {
                // Desugar expressions in protocol function bodies
                for protocol_function in &mut protocol_def.functions {
                    match protocol_function {
                        outrun_parser::ProtocolFunction::Definition(function_def) => {
                            if let Some(ref mut guard) = function_def.guard {
                                self.desugar_expression(&mut guard.condition)?;
                            }
                            self.desugar_block(&mut function_def.body)?;
                        }
                        outrun_parser::ProtocolFunction::StaticDefinition(static_def) => {
                            self.desugar_block(&mut static_def.body)?;
                        }
                        // Signatures don't have bodies to desugar
                        outrun_parser::ProtocolFunction::Signature(_) => {}
                    }
                }
            }
            ItemKind::ImplBlock(impl_block) => {
                // Desugar expressions in impl block function bodies
                for function in &mut impl_block.functions {
                    if let Some(ref mut guard) = function.guard {
                        self.desugar_expression(&mut guard.condition)?;
                    }
                    self.desugar_block(&mut function.body)?;
                }
            }
            ItemKind::MacroDefinition(macro_def) => {
                // Desugar expressions in macro body
                self.desugar_block(&mut macro_def.body)?;
            }
            // Other item types don't contain expressions that need desugaring
            _ => {}
        }
        Ok(())
    }

    /// Desugar all expressions within a block
    fn desugar_block(&mut self, block: &mut outrun_parser::Block) -> Result<(), TypecheckError> {
        for statement in &mut block.statements {
            match &mut statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    self.desugar_expression(&mut **expr)?;
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    // CRITICAL FIX: Desugar expressions in let binding statements
                    // This was the root cause of the binary operations not being desugared
                    // in function bodies that contain let bindings
                    self.desugar_expression(&mut let_binding.expression)?;
                }
            }
        }
        Ok(())
    }

    /// Iteratively desugar all operators in an expression tree to prevent stack overflow
    pub fn desugar_expression(&mut self, expr: &mut Expression) -> Result<(), TypecheckError> {
        eprintln!("🔍 Starting expression desugaring at span {:?}", expr.span);

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
                    eprintln!(
                        "🎯 Found binary operation {:?} at span {:?} during traversal",
                        binary_op.operator, current_expr.span
                    );
                    work_stack.push(&mut binary_op.left);
                    work_stack.push(&mut binary_op.right);
                }
                ExpressionKind::UnaryOp(unary_op) => {
                    eprintln!(
                        "🎯 Found unary operation {:?} at span {:?} during traversal",
                        unary_op.operator, current_expr.span
                    );
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
                    // Process expressions in then block
                    for statement in &mut if_expr.then_block.statements {
                        if let outrun_parser::StatementKind::Expression(expr) = &mut statement.kind
                        {
                            work_stack.push(&mut **expr);
                        }
                    }
                    // Process expressions in optional else block
                    if let Some(ref mut else_block) = if_expr.else_block {
                        for statement in &mut else_block.statements {
                            if let outrun_parser::StatementKind::Expression(expr) =
                                &mut statement.kind
                            {
                                work_stack.push(&mut **expr);
                            }
                        }
                    }
                }
                ExpressionKind::CaseExpression(case_expr) => {
                    work_stack.push(&mut case_expr.expression);
                    for clause in &mut case_expr.clauses {
                        if let Some(ref mut guard) = clause.guard {
                            work_stack.push(guard);
                        }
                        // Process expressions in case result bodies
                        match &mut clause.result {
                            outrun_parser::CaseResult::Block(block) => {
                                for statement in &mut block.statements {
                                    if let outrun_parser::StatementKind::Expression(expr) =
                                        &mut statement.kind
                                    {
                                        work_stack.push(&mut **expr);
                                    }
                                }
                            }
                            outrun_parser::CaseResult::Expression(expr) => {
                                work_stack.push(&mut **expr);
                            }
                        }
                    }
                }
                ExpressionKind::Parenthesized(inner_expr) => {
                    work_stack.push(inner_expr);
                }
                ExpressionKind::FieldAccess(field_access) => {
                    // Process the object being accessed
                    work_stack.push(&mut field_access.object);
                }
                ExpressionKind::Struct(struct_literal) => {
                    // Process expressions in struct field values
                    for field in &mut struct_literal.fields {
                        match field {
                            outrun_parser::StructLiteralField::Assignment { value, .. } => {
                                work_stack.push(&mut **value);
                            }
                            outrun_parser::StructLiteralField::Shorthand(_) => {
                                // Shorthand just references identifiers, no expressions
                            }
                            outrun_parser::StructLiteralField::Spread(_) => {
                                // Spread fields just reference identifiers, no expressions
                            }
                        }
                    }
                }
                ExpressionKind::AnonymousFunction(anon_fn) => {
                    // Process expressions in function clauses
                    for clause in &mut anon_fn.clauses {
                        if let Some(ref mut guard) = clause.guard {
                            work_stack.push(guard);
                        }
                        // Process body based on its type
                        match &mut clause.body {
                            outrun_parser::AnonymousBody::Expression(expr) => {
                                work_stack.push(&mut **expr);
                            }
                            outrun_parser::AnonymousBody::Block(block) => {
                                // Process expressions within the block
                                for statement in &mut block.statements {
                                    if let outrun_parser::StatementKind::Expression(expr) =
                                        &mut statement.kind
                                    {
                                        work_stack.push(&mut **expr);
                                    }
                                }
                            }
                        }
                    }
                }
                ExpressionKind::MacroInjection(_) => {
                    // MacroInjection only contains parameter identifier, no expressions to desugar
                }
                ExpressionKind::String(string_literal) => {
                    // Process expressions in string interpolations
                    for part in &mut string_literal.parts {
                        if let outrun_parser::StringPart::Interpolation { expression, .. } = part {
                            work_stack.push(&mut **expression);
                        }
                    }
                }
                ExpressionKind::Sigil(sigil_literal) => {
                    // Process expressions in sigil string interpolations
                    for part in &mut sigil_literal.string.parts {
                        if let outrun_parser::StringPart::Interpolation { expression, .. } = part {
                            work_stack.push(&mut **expression);
                        }
                    }
                }
                // Literals and simple identifiers don't contain nested expressions
                ExpressionKind::Identifier(_)
                | ExpressionKind::TypeIdentifier(_)
                | ExpressionKind::QualifiedIdentifier(_)
                | ExpressionKind::Integer(_)
                | ExpressionKind::Float(_)
                | ExpressionKind::Boolean(_)
                | ExpressionKind::Atom(_)
                | ExpressionKind::FunctionCapture(_) => {
                    // These don't contain nested expressions that need desugaring
                }
            }
        }

        // Phase 2: Transform expressions in post-order (children first)
        for expr_ptr in transform_stack.into_iter().rev() {
            unsafe {
                let current_expr = &mut *expr_ptr;
                match &mut current_expr.kind {
                    ExpressionKind::BinaryOp(binary_op) => {
                        // Transform this binary operation
                        match self.desugar_binary_operation(binary_op) {
                            Ok(desugared_call) => {
                                // DEBUG: Commented out for performance
                                // println!(
                                //     "✅ Successfully desugared binary op {:?} at span {:?}",
                                //     binary_op.operator, current_expr.span
                                // );
                                current_expr.kind = ExpressionKind::FunctionCall(desugared_call);
                            }
                            Err(e) => {
                                println!(
                                    "❌ Failed to desugar binary op {:?} at span {:?}: {}",
                                    binary_op.operator, current_expr.span, e
                                );
                                return Err(e);
                            }
                        }
                    }
                    ExpressionKind::UnaryOp(unary_op) => {
                        // Transform this unary operation
                        match self.desugar_unary_operation(unary_op) {
                            Ok(desugared_call) => {
                                // DEBUG: Commented out for performance
                                // println!(
                                //     "✅ Successfully desugared unary op {:?} at span {:?}",
                                //     unary_op.operator, current_expr.span
                                // );
                                current_expr.kind = ExpressionKind::FunctionCall(desugared_call);
                            }
                            Err(e) => {
                                println!(
                                    "❌ Failed to desugar unary op {:?} at span {:?}: {}",
                                    unary_op.operator, current_expr.span, e
                                );
                                return Err(e);
                            }
                        }
                    }
                    // Other expressions don't need transformation, just traversal
                    _ => {}
                }
            }
        }

        eprintln!(
            "🔍 Expression desugaring complete at span {:?}. Transformations made: {}",
            expr.span,
            self.transformations.len()
        );
        Ok(())
    }

    /// Transform a binary operation into a protocol function call
    fn desugar_binary_operation(
        &mut self,
        binary_op: &BinaryOperation,
    ) -> Result<FunctionCall, TypecheckError> {
        let (protocol_name, function_name, param_names) = match binary_op.operator {
            // Arithmetic operators
            BinaryOperator::Add => ("BinaryAddition", "add", ("lhs", "rhs")),
            BinaryOperator::Subtract => ("BinarySubtraction", "subtract", ("lhs", "rhs")),
            BinaryOperator::Multiply => ("BinaryMultiplication", "multiply", ("lhs", "rhs")),
            BinaryOperator::Divide => ("BinaryDivision", "divide", ("lhs", "rhs")),
            BinaryOperator::Modulo => ("BinaryModulo", "modulo", ("left", "right")),
            BinaryOperator::Exponent => ("BinaryExponentiation", "power", ("base", "exponent")),

            // Comparison operators
            BinaryOperator::Equal => ("Equality", "equal?", ("left", "right")),
            BinaryOperator::Less => ("Comparison", "less_than?", ("left", "right")),
            BinaryOperator::LessEqual => ("Comparison", "less_than_or_equal?", ("left", "right")),
            BinaryOperator::Greater => ("Comparison", "greater_than?", ("left", "right")),
            BinaryOperator::GreaterEqual => {
                ("Comparison", "greater_than_or_equal?", ("left", "right"))
            }

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

            BinaryOperator::NotEqual => ("Equality", "not_equal?", ("left", "right")),

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
        self.transformations.push(format!(
            "Binary {} → {}.{}",
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
            resolved_function_key: None,
            universal_clause_ids: None,
        })
    }

    /// Transform a unary operation into a protocol function call
    fn desugar_unary_operation(
        &mut self,
        unary_op: &UnaryOperation,
    ) -> Result<FunctionCall, TypecheckError> {
        let (protocol_name, function_name) = match unary_op.operator {
            UnaryOperator::Plus => ("UnaryPlus", "plus"),
            UnaryOperator::Minus => ("UnaryMinus", "minus"),
            UnaryOperator::LogicalNot => ("LogicalNot", "not?"),
            UnaryOperator::BitwiseNot => ("BitwiseNot", "bitwise_not"),
        };

        // Record the transformation for debugging
        self.transformations.push(format!(
            "Unary {} → {}.{}",
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
            arguments: vec![Argument::Named {
                name: Identifier {
                    name: "value".to_string(),
                    span: unary_op.operand.span,
                },
                expression: *unary_op.operand.clone(),
                format: ArgumentFormat::Explicit,
                span: unary_op.operand.span,
            }],
            span: unary_op.span,
            resolved_function_key: None,
            universal_clause_ids: None,
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
                    Argument::Named {
                        name, expression, ..
                    } => {
                        assert_eq!(name.name, "lhs");
                        match &expression.kind {
                            ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 1),
                            _ => panic!("Expected integer literal for lhs operand"),
                        }
                    }
                    _ => panic!("Expected named argument for lhs operand"),
                }

                match &func_call.arguments[1] {
                    Argument::Named {
                        name, expression, ..
                    } => {
                        assert_eq!(name.name, "rhs");
                        match &expression.kind {
                            ExpressionKind::Integer(int_lit) => assert_eq!(int_lit.value, 2),
                            _ => panic!("Expected integer literal for rhs operand"),
                        }
                    }
                    _ => panic!("Expected named argument for rhs operand"),
                }
            }
            _ => panic!("Expected function call after desugaring"),
        }

        // Check transformation was recorded
        assert_eq!(engine.transformations.len(), 1);
        assert!(engine.transformations[0].contains("Binary add → BinaryAddition.add"));
    }

    #[test]
    fn test_function_guard_desugaring() {
        use outrun_parser::parse_program;

        let source = r#"
            def divide(a: Integer, b: Integer): Float
            when b != 0 {
                Float.from_integer(a) / Float.from_integer(b)
            }
        "#;

        let mut program = parse_program(source).unwrap();
        let mut engine = DesugaringEngine::new();

        // Desugar the program (should desugar the guard expression)
        engine.desugar_program(&mut program).unwrap();

        // Check that the guard expression was desugared
        if let Some(item) = program.items.first() {
            if let outrun_parser::ItemKind::FunctionDefinition(func_def) = &item.kind {
                if let Some(guard) = &func_def.guard {
                    // The guard "b != 0" should be desugared to "Equality.not_equal?(left: b, right: 0)"
                    match &guard.condition.kind {
                        outrun_parser::ExpressionKind::FunctionCall(func_call) => {
                            match &func_call.path {
                                outrun_parser::FunctionPath::Qualified { module, name } => {
                                    assert_eq!(module.name, "Equality");
                                    assert_eq!(name.name, "not_equal?");
                                }
                                _ => panic!("Expected qualified function call for !="),
                            }
                        }
                        _ => panic!("Expected function call after desugaring !="),
                    }
                } else {
                    panic!("Expected guard clause in function definition");
                }
            } else {
                panic!("Expected function definition");
            }
        } else {
            panic!("Expected at least one item in program");
        }
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
                    Argument::Named {
                        name, expression, ..
                    } => {
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
    fn test_not_equal_desugaring() {
        let mut engine = DesugaringEngine::new();

        // Parse "a != b"
        let mut expr = parse_expression("a != b").unwrap();

        // Desugar the expression
        engine.desugar_expression(&mut expr).unwrap();

        // Should become Equality.not_equal?(left: a, right: b)
        match expr.kind {
            ExpressionKind::FunctionCall(func_call) => {
                // Check function is Equality.not_equal?
                match &func_call.path {
                    FunctionPath::Qualified { module, name } => {
                        assert_eq!(module.name, "Equality");
                        assert_eq!(name.name, "not_equal?");
                    }
                    _ => panic!("Expected Equality.not_equal? function"),
                }

                // Check arguments
                assert_eq!(func_call.arguments.len(), 2);
                match &func_call.arguments[0] {
                    Argument::Named { name, .. } => {
                        assert_eq!(name.name, "left");
                    }
                    _ => panic!("Expected named argument 'left'"),
                }

                match &func_call.arguments[1] {
                    Argument::Named { name, .. } => {
                        assert_eq!(name.name, "right");
                    }
                    _ => panic!("Expected named argument 'right'"),
                }
            }
            _ => panic!("Expected function call after desugaring"),
        }

        // Check transformation was recorded
        assert_eq!(engine.transformations.len(), 1);
        assert!(engine.transformations[0].contains("Binary notequal → Equality.not_equal?"));
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
                    Argument::Named { expression, .. } => match &expression.kind {
                        ExpressionKind::FunctionCall(inner_call) => match &inner_call.path {
                            FunctionPath::Qualified { module, name } => {
                                assert_eq!(module.name, "BinaryMultiplication");
                                assert_eq!(name.name, "multiply");
                            }
                            _ => panic!("Expected BinaryMultiplication for inner operation"),
                        },
                        _ => panic!("Expected function call for right operand"),
                    },
                    _ => panic!("Expected named argument"),
                }
            }
            _ => panic!("Expected function call after desugaring"),
        }

        // Should have recorded multiple transformations
        assert_eq!(engine.transformations.len(), 2);
    }
}
