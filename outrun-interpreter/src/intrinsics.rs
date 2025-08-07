//! Intrinsics implementation for the new Outrun interpreter
//!
//! This module implements all the `Outrun.Intrinsic.*` functions that provide
//! the core functionality for Outrun programs. This is a minimal implementation
//! that will be expanded as we integrate with the typechecker v3 system.

use crate::value::Value;
use outrun_parser::Span;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

/// Type alias for intrinsic function signatures
type IntrinsicFunction = fn(&[Value], Span) -> Result<Value, IntrinsicError>;

/// Errors that can occur during intrinsic function execution
#[derive(Debug, Error)]
pub enum IntrinsicError {
    #[error("Intrinsic function '{name}' not found")]
    NotFound { name: String, span: Span },

    #[error("Missing required argument '{arg}' for intrinsic '{function}'")]
    MissingArgument {
        arg: String,
        function: String,
        span: Span,
    },

    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Division by zero")]
    DivisionByZero { span: Span },

    #[error("Invalid operation: {message}")]
    InvalidOperation { message: String, span: Span },
}

/// Handler for intrinsic function execution
pub struct IntrinsicsHandler {
    /// Registry of available intrinsic functions mapped to their execution functions
    intrinsics: HashMap<String, IntrinsicFunction>,
}

impl IntrinsicsHandler {
    /// Create a new intrinsics handler with all built-in functions registered
    pub fn new() -> Self {
        let mut handler = Self {
            intrinsics: HashMap::new(),
        };

        handler.register_all_intrinsics();
        handler
    }

    /// Execute an intrinsic function by name with proper argument evaluation
    pub fn execute_intrinsic(
        &self,
        name: &str,
        arguments: &[Value],
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        if let Some(intrinsic_fn) = self.intrinsics.get(name) {
            intrinsic_fn(arguments, span)
        } else {
            Err(IntrinsicError::NotFound {
                name: name.to_string(),
                span,
            })
        }
    }

    /// Register all intrinsic functions with their implementations
    fn register_all_intrinsics(&mut self) {
        // Integer arithmetic - using actual core library names
        self.register("Outrun.Intrinsic.i64_add", intrinsic_add_integer64);
        self.register("Outrun.Intrinsic.i64_sub", intrinsic_subtract_integer64);
        self.register("Outrun.Intrinsic.i64_mul", intrinsic_multiply_integer64);
        self.register("Outrun.Intrinsic.i64_div", intrinsic_divide_integer64);
        self.register("Outrun.Intrinsic.i64_mod", intrinsic_modulo_integer64);

        // Integer comparison operations
        self.register("Outrun.Intrinsic.i64_eq", intrinsic_equal);
        self.register("Outrun.Intrinsic.i64_gt", intrinsic_greater_than);
        self.register("Outrun.Intrinsic.i64_ge", intrinsic_greater_equal);
        self.register("Outrun.Intrinsic.i64_lt", intrinsic_less_than);
        self.register("Outrun.Intrinsic.i64_le", intrinsic_less_equal);

        // Integer unary operations
        self.register("Outrun.Intrinsic.i64_pos", intrinsic_unary_plus);
        self.register("Outrun.Intrinsic.i64_neg", intrinsic_unary_minus);

        // Boolean logical operations
        self.register("Outrun.Intrinsic.bool_and", intrinsic_bool_and);
        self.register("Outrun.Intrinsic.bool_or", intrinsic_bool_or);
        self.register("Outrun.Intrinsic.bool_not", intrinsic_bool_not);

        // TODO: Remove these protocol bridges once proper dispatch is implemented
        // These are architectural hacks that bypass the protocol system
        // self.register("BinaryAddition.add", intrinsic_add_integer64);
        // Protocol functions should use proper dispatch, not direct intrinsic mapping

        // Float arithmetic - using actual core library names
        self.register("Outrun.Intrinsic.f64_add", intrinsic_add_float64);
        self.register("Outrun.Intrinsic.f64_sub", intrinsic_subtract_float64);
        self.register("Outrun.Intrinsic.f64_mul", intrinsic_multiply_float64);
        self.register("Outrun.Intrinsic.f64_div", intrinsic_divide_float64);

        // List operations
        self.register("Outrun.Intrinsic.list_head", intrinsic_list_head);
        self.register("Outrun.Intrinsic.list_tail", intrinsic_list_tail);
        self.register("Outrun.Intrinsic.list_prepend", intrinsic_list_prepend);
        self.register("Outrun.Intrinsic.list_is_empty", intrinsic_list_is_empty);

        // Comparison operations
        self.register("Outrun.Intrinsic.equal", intrinsic_equal);
        self.register("Outrun.Intrinsic.not_equal", intrinsic_not_equal);
        self.register("Outrun.Intrinsic.less_than", intrinsic_less_than);
        self.register("Outrun.Intrinsic.greater_than", intrinsic_greater_than);

        // Boolean operations
        self.register("Outrun.Intrinsic.logical_and", intrinsic_logical_and);
        self.register("Outrun.Intrinsic.logical_or", intrinsic_logical_or);
        self.register("Outrun.Intrinsic.logical_not", intrinsic_logical_not);

        // String operations
        self.register("Outrun.Intrinsic.string_concat", intrinsic_string_concat);
        self.register("Outrun.Intrinsic.string_length", intrinsic_string_length);
    }

    /// Register a single intrinsic function with its implementation
    fn register(&mut self, name: &str, func: fn(&[Value], Span) -> Result<Value, IntrinsicError>) {
        self.intrinsics.insert(name.to_string(), func);
    }
}

impl Default for IntrinsicsHandler {
    fn default() -> Self {
        Self::new()
    }
}

// Integer arithmetic intrinsics
fn intrinsic_add_integer64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("add_integer64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => Ok(Value::integer(a + b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64, Integer64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_subtract_integer64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("subtract_integer64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => Ok(Value::integer(a - b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64, Integer64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_multiply_integer64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("multiply_integer64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => Ok(Value::integer(a * b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64, Integer64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_divide_integer64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("divide_integer64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => {
            if *b == 0 {
                Err(IntrinsicError::DivisionByZero { span })
            } else {
                Ok(Value::integer(a / b))
            }
        }
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64, Integer64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_modulo_integer64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("modulo_integer64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => {
            if *b == 0 {
                Err(IntrinsicError::DivisionByZero { span })
            } else {
                Ok(Value::integer(a % b))
            }
        }
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64, Integer64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

// Float arithmetic intrinsics
fn intrinsic_add_float64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("add_float64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Float64(a), Value::Float64(b)) => Ok(Value::float(a + b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Float64, Float64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_subtract_float64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("subtract_float64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Float64(a), Value::Float64(b)) => Ok(Value::float(a - b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Float64, Float64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_multiply_float64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("multiply_float64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Float64(a), Value::Float64(b)) => Ok(Value::float(a * b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Float64, Float64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_divide_float64(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("divide_float64 expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Float64(a), Value::Float64(b)) => {
            if *b == 0.0 {
                Err(IntrinsicError::DivisionByZero { span })
            } else {
                Ok(Value::float(a / b))
            }
        }
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Float64, Float64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

// List operation intrinsics
fn intrinsic_list_head(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 1 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("list_head expects 1 argument, got {}", args.len()),
            span,
        });
    }

    match &args[0] {
        Value::List { list, .. } => match list.head() {
            Some(head) => Ok(head.clone()),
            None => Err(IntrinsicError::InvalidOperation {
                message: "Cannot get head of empty list".to_string(),
                span,
            }),
        },
        v => Err(IntrinsicError::TypeMismatch {
            expected: "List".to_string(),
            found: v.type_name().to_string(),
            span,
        }),
    }
}

fn intrinsic_list_tail(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 1 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("list_tail expects 1 argument, got {}", args.len()),
            span,
        });
    }

    match &args[0] {
        Value::List {
            list,
            element_type_info,
        } => match list.tail() {
            Some(tail) => Ok(Value::List {
                list: Rc::new(tail.clone()),
                element_type_info: element_type_info.clone(),
            }),
            None => Err(IntrinsicError::InvalidOperation {
                message: "Cannot get tail of empty list".to_string(),
                span,
            }),
        },
        v => Err(IntrinsicError::TypeMismatch {
            expected: "List".to_string(),
            found: v.type_name().to_string(),
            span,
        }),
    }
}

fn intrinsic_list_prepend(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("list_prepend expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    let element = &args[0];
    match &args[1] {
        Value::List {
            list,
            element_type_info,
        } => {
            let new_list = list.clone().prepend(element.clone());
            Ok(Value::List {
                list: Rc::new(new_list),
                element_type_info: element_type_info.clone(),
            })
        }
        v => Err(IntrinsicError::TypeMismatch {
            expected: "any, List".to_string(),
            found: format!("any, {}", v.type_name()),
            span,
        }),
    }
}

fn intrinsic_list_is_empty(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 1 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("list_is_empty expects 1 argument, got {}", args.len()),
            span,
        });
    }

    match &args[0] {
        Value::List { list, .. } => Ok(Value::boolean(list.is_empty())),
        v => Err(IntrinsicError::TypeMismatch {
            expected: "List".to_string(),
            found: v.type_name().to_string(),
            span,
        }),
    }
}

// Comparison intrinsics
fn intrinsic_equal(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("equal expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    Ok(Value::boolean(args[0] == args[1]))
}

fn intrinsic_not_equal(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("not_equal expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    Ok(Value::boolean(args[0] != args[1]))
}

fn intrinsic_less_than(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("less_than expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => Ok(Value::boolean(a < b)),
        (Value::Float64(a), Value::Float64(b)) => Ok(Value::boolean(a < b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "comparable types".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_greater_than(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("greater_than expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => Ok(Value::boolean(a > b)),
        (Value::Float64(a), Value::Float64(b)) => Ok(Value::boolean(a > b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "comparable types".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

// Boolean operation intrinsics
fn intrinsic_logical_and(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("logical_and expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::boolean(*a && *b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Boolean, Boolean".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_logical_or(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("logical_or expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::boolean(*a || *b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Boolean, Boolean".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_logical_not(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 1 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("logical_not expects 1 argument, got {}", args.len()),
            span,
        });
    }

    match &args[0] {
        Value::Boolean(b) => Ok(Value::boolean(!b)),
        v => Err(IntrinsicError::TypeMismatch {
            expected: "Boolean".to_string(),
            found: v.type_name().to_string(),
            span,
        }),
    }
}

// String operation intrinsics
fn intrinsic_string_concat(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("string_concat expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::String(a), Value::String(b)) => Ok(Value::string(format!("{}{}", a, b))),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "String, String".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_string_length(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 1 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("string_length expects 1 argument, got {}", args.len()),
            span,
        });
    }

    match &args[0] {
        Value::String(s) => Ok(Value::integer(s.len() as i64)),
        v => Err(IntrinsicError::TypeMismatch {
            expected: "String".to_string(),
            found: v.type_name().to_string(),
            span,
        }),
    }
}

/// Greater than or equal comparison for integers (>=)
fn intrinsic_greater_equal(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("greater_equal expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => Ok(Value::boolean(a >= b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

/// Less than or equal comparison for integers (<=)
fn intrinsic_less_equal(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("less_equal expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Integer64(a), Value::Integer64(b)) => Ok(Value::boolean(a <= b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

/// Unary plus operation for integers (+x)
fn intrinsic_unary_plus(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 1 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("unary_plus expects 1 argument, got {}", args.len()),
            span,
        });
    }

    match &args[0] {
        Value::Integer64(n) => Ok(Value::integer(*n)), // Unary plus is identity
        v => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64".to_string(),
            found: v.type_name().to_string(),
            span,
        }),
    }
}

/// Unary minus operation for integers (-x)
fn intrinsic_unary_minus(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 1 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("unary_minus expects 1 argument, got {}", args.len()),
            span,
        });
    }

    match &args[0] {
        Value::Integer64(n) => Ok(Value::integer(-n)),
        v => Err(IntrinsicError::TypeMismatch {
            expected: "Integer64".to_string(),
            found: v.type_name().to_string(),
            span,
        }),
    }
}

// Boolean logical operations
fn intrinsic_bool_and(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("bool_and expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::boolean(*a && *b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Boolean, Boolean".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_bool_or(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 2 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("bool_or expects 2 arguments, got {}", args.len()),
            span,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::boolean(*a || *b)),
        (a, b) => Err(IntrinsicError::TypeMismatch {
            expected: "Boolean, Boolean".to_string(),
            found: format!("{}, {}", a.type_name(), b.type_name()),
            span,
        }),
    }
}

fn intrinsic_bool_not(args: &[Value], span: Span) -> Result<Value, IntrinsicError> {
    if args.len() != 1 {
        return Err(IntrinsicError::InvalidOperation {
            message: format!("bool_not expects 1 argument, got {}", args.len()),
            span,
        });
    }

    match &args[0] {
        Value::Boolean(b) => Ok(Value::boolean(!b)),
        v => Err(IntrinsicError::TypeMismatch {
            expected: "Boolean".to_string(),
            found: v.type_name().to_string(),
            span,
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intrinsics_creation() {
        let handler = IntrinsicsHandler::new();
        // Verify that intrinsics are registered with core library names
        assert!(handler.intrinsics.contains_key("Outrun.Intrinsic.i64_add"));
        assert!(
            handler
                .intrinsics
                .contains_key("Outrun.Intrinsic.list_head")
        );
        assert!(handler.intrinsics.contains_key("Outrun.Intrinsic.i64_eq"));
    }

    #[test]
    fn test_integer_arithmetic() {
        let handler = IntrinsicsHandler::new();
        let span = Span::new(0, 0);

        // Test addition
        let args = vec![Value::integer(5), Value::integer(3)];
        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.i64_add", &args, span)
            .unwrap();
        assert_eq!(result, Value::integer(8));

        // Test division by zero
        let args = vec![Value::integer(5), Value::integer(0)];
        let result = handler.execute_intrinsic("Outrun.Intrinsic.i64_div", &args, span);
        assert!(matches!(result, Err(IntrinsicError::DivisionByZero { .. })));
    }

    #[test]
    fn test_list_operations() {
        let handler = IntrinsicsHandler::new();
        let span = Span::new(0, 0);

        // Test list prepend
        let empty_list = Value::empty_list();
        let args = vec![Value::integer(42), empty_list];
        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.list_prepend", &args, span)
            .unwrap();

        // Test list head
        let args = vec![result.clone()];
        let head = handler
            .execute_intrinsic("Outrun.Intrinsic.list_head", &args, span)
            .unwrap();
        assert_eq!(head, Value::integer(42));

        // Test list is_empty
        let args = vec![result];
        let is_empty = handler
            .execute_intrinsic("Outrun.Intrinsic.list_is_empty", &args, span)
            .unwrap();
        assert_eq!(is_empty, Value::boolean(false));
    }

    #[test]
    fn test_comparison_operations() {
        let handler = IntrinsicsHandler::new();
        let span = Span::new(0, 0);

        // Test equality
        let args = vec![Value::integer(5), Value::integer(5)];
        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.equal", &args, span)
            .unwrap();
        assert_eq!(result, Value::boolean(true));

        // Test less than
        let args = vec![Value::integer(3), Value::integer(5)];
        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.less_than", &args, span)
            .unwrap();
        assert_eq!(result, Value::boolean(true));
    }

    #[test]
    fn test_intrinsic_lookup() {
        let handler = IntrinsicsHandler::new();
        let span = Span::new(0, 0);

        // Test that a known intrinsic can be found and executed
        let args = vec![Value::integer(5), Value::integer(3)];
        let result = handler.execute_intrinsic("Outrun.Intrinsic.i64_add", &args, span);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::integer(8));

        // Test that an unknown intrinsic returns error
        let result = handler.execute_intrinsic("Outrun.Intrinsic.nonexistent", &args, span);
        assert!(result.is_err());
    }
}
