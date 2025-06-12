//! Core interpreter implementation for evaluating Outrun AST nodes.

use crate::environment::Environment;
use crate::error::{Result, RuntimeError};
use crate::value::Value;
use outrun_parser::{Expression, Program};

/// The main interpreter for evaluating Outrun programs
#[derive(Debug)]
pub struct Interpreter {
    /// Variable environment and scope management
    environment: Environment,
}

impl Interpreter {
    /// Create a new interpreter with default environment
    pub fn new() -> Self {
        let mut interpreter = Self {
            environment: Environment::new(),
        };

        // Register built-in functions
        crate::builtins::register_builtins(&mut interpreter.environment);

        interpreter
    }

    /// Evaluate a complete program
    pub fn eval_program(&mut self, program: &Program) -> Result<Value> {
        let mut last_value = Value::Unit;

        for item in &program.items {
            // For now, just evaluate each item as an expression
            // TODO: Handle different item types properly
            match &item.kind {
                outrun_parser::ItemKind::Expression(expr) => {
                    last_value = self.eval_expression(expr)?;
                }
                _ => {
                    // TODO: Handle other item types (functions, structs, etc.)
                    return Err(RuntimeError::custom(format!(
                        "Item type {:?} not yet supported",
                        item.kind
                    )));
                }
            }
        }

        Ok(last_value)
    }

    /// Evaluate an expression
    pub fn eval_expression(&mut self, expr: &Expression) -> Result<Value> {
        match &expr.kind {
            // Literals
            outrun_parser::ExpressionKind::Boolean(lit) => Ok(Value::Boolean(lit.value)),
            outrun_parser::ExpressionKind::Integer(lit) => Ok(Value::Integer(lit.value)),
            outrun_parser::ExpressionKind::Float(lit) => Ok(Value::Float(lit.value)),
            outrun_parser::ExpressionKind::String(lit) => self.eval_string_literal(lit),
            outrun_parser::ExpressionKind::Atom(lit) => Ok(Value::Atom(lit.name.clone())),

            // Variables
            outrun_parser::ExpressionKind::Identifier(id) => self.environment.get(&id.name),

            // TODO: Implement other expression types
            _ => Err(RuntimeError::custom(format!(
                "Expression type {:?} not yet implemented",
                expr.kind
            ))),
        }
    }

    /// Evaluate a string literal with potential interpolation
    fn eval_string_literal(&mut self, string: &outrun_parser::StringLiteral) -> Result<Value> {
        let mut result = String::new();

        for part in &string.parts {
            match part {
                outrun_parser::StringPart::Text { content, .. } => {
                    result.push_str(content);
                }
                outrun_parser::StringPart::Interpolation { expression, .. } => {
                    let value = self.eval_expression(expression)?;
                    result.push_str(&value.to_string_repr());
                }
            }
        }

        Ok(Value::String(result))
    }

    /// Get reference to the environment (for testing and REPL)
    pub fn environment(&self) -> &Environment {
        &self.environment
    }

    /// Get mutable reference to the environment (for REPL commands)
    pub fn environment_mut(&mut self) -> &mut Environment {
        &mut self.environment
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::parse_expression;

    #[test]
    fn test_literal_evaluation() {
        let mut interpreter = Interpreter::new();

        // Integer literal
        let expr = parse_expression("42").unwrap();
        assert_eq!(
            interpreter.eval_expression(&expr).unwrap(),
            Value::Integer(42)
        );

        // Float literal
        let expr = parse_expression("1.5").unwrap();
        assert_eq!(
            interpreter.eval_expression(&expr).unwrap(),
            Value::Float(1.5)
        );

        // Boolean literals
        let expr = parse_expression("true").unwrap();
        assert_eq!(
            interpreter.eval_expression(&expr).unwrap(),
            Value::Boolean(true)
        );

        let expr = parse_expression("false").unwrap();
        assert_eq!(
            interpreter.eval_expression(&expr).unwrap(),
            Value::Boolean(false)
        );

        // String literal
        let expr = parse_expression("\"hello\"").unwrap();
        assert_eq!(
            interpreter.eval_expression(&expr).unwrap(),
            Value::String("hello".to_string())
        );

        // Atom literal
        let expr = parse_expression(":test").unwrap();
        assert_eq!(
            interpreter.eval_expression(&expr).unwrap(),
            Value::Atom("test".to_string())
        );
    }

    #[test]
    fn test_string_interpolation() {
        let mut interpreter = Interpreter::new();

        // Define a variable for interpolation
        interpreter
            .environment
            .define("name".to_string(), Value::String("World".to_string()));

        // String with interpolation
        let expr = parse_expression(r#""Hello #{name}!""#).unwrap();
        assert_eq!(
            interpreter.eval_expression(&expr).unwrap(),
            Value::String("Hello World!".to_string())
        );
    }

    #[test]
    fn test_variable_lookup() {
        let mut interpreter = Interpreter::new();

        // Define variable
        interpreter
            .environment
            .define("x".to_string(), Value::Integer(42));

        // Look up variable
        let expr = parse_expression("x").unwrap();
        assert_eq!(
            interpreter.eval_expression(&expr).unwrap(),
            Value::Integer(42)
        );

        // Undefined variable should error
        let expr = parse_expression("undefined").unwrap();
        assert!(interpreter.eval_expression(&expr).is_err());
    }
}
