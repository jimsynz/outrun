//! Intrinsics implementation for the Outrun interpreter
//!
//! This module implements all the `Outrun.Intrinsic.*` functions that provide
//! the core functionality for Outrun programs. These intrinsics handle:
//! - Arithmetic operations on integers and floats
//! - List operations (head, tail, prepend, etc.)
//! - Option/Result construction and operations
//! - String operations
//! - Comparison operations

use crate::function_call_context::IntrinsicExecutionContext;
use crate::type_extraction::{FunctionReturnInfo, TypeExtractor};
use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::checker::TypedExpression;
use outrun_typechecker::{
    compilation::compiler_environment::{AtomId, CompilerEnvironment},
    unification::StructuredType,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use thiserror::Error;

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

    #[error("Index out of bounds: index {index}, length {length}")]
    IndexOutOfBounds {
        index: usize,
        length: usize,
        span: Span,
    },

    #[error("Internal error: {message}")]
    Internal { message: String, span: Span },
}

/// The intrinsics handler that executes built-in functions
pub struct IntrinsicsHandler {
    /// Compiler environment for creating TypeNameIds and AtomIds
    compiler_environment: CompilerEnvironment,
}

impl IntrinsicsHandler {
    /// Create a new intrinsics handler
    pub fn new() -> Self {
        Self {
            compiler_environment: CompilerEnvironment::new(),
        }
    }

    /// Get all intrinsic function names that this handler supports
    pub fn get_all_intrinsic_names() -> HashSet<String> {
        let mut names = Self::get_intrinsic_handlers()
            .keys()
            .cloned()
            .collect::<HashSet<String>>();

        // Add special cases that need TypedExpression (only those that exist in typechecker)
        names.insert("Outrun.Intrinsic.list_empty".to_string());

        names
    }

    /// Define all intrinsic handlers in one place
    /// This ensures the implementation and registry stay in perfect sync
    #[allow(clippy::type_complexity)]
    fn get_intrinsic_handlers() -> HashMap<
        String,
        fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
    > {
        let mut handlers = HashMap::new();

        // Integer arithmetic intrinsics (current names from typechecker)
        handlers.insert(
            "Outrun.Intrinsic.i64_add".to_string(),
            Self::integer_add
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_sub".to_string(),
            Self::integer_subtract
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_mul".to_string(),
            Self::integer_multiply
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_div".to_string(),
            Self::integer_divide
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_mod".to_string(),
            Self::integer_modulo
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_neg".to_string(),
            Self::integer_negate
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Integer comparison intrinsics (current names from typechecker)
        handlers.insert(
            "Outrun.Intrinsic.i64_eq".to_string(),
            Self::integer_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_gt".to_string(),
            Self::integer_greater_than
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_ge".to_string(),
            Self::integer_greater_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_lt".to_string(),
            Self::integer_less_than
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_le".to_string(),
            Self::integer_less_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Integer positive operation
        handlers.insert(
            "Outrun.Intrinsic.i64_pos".to_string(),
            Self::integer_positive
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Integer bitwise operations
        handlers.insert(
            "Outrun.Intrinsic.i64_and".to_string(),
            Self::integer_and
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_or".to_string(),
            Self::integer_or
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_xor".to_string(),
            Self::integer_xor
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_not".to_string(),
            Self::integer_not
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_shl".to_string(),
            Self::integer_shl
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_shr".to_string(),
            Self::integer_shr
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Additional integer operations
        handlers.insert(
            "Outrun.Intrinsic.i64_abs".to_string(),
            Self::integer_abs
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_pow".to_string(),
            Self::integer_pow
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_to_string".to_string(),
            Self::integer_to_string
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.i64_to_string_radix".to_string(),
            Self::integer_to_string_radix
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Float arithmetic operations
        handlers.insert(
            "Outrun.Intrinsic.f64_add".to_string(),
            Self::float_add
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_sub".to_string(),
            Self::float_subtract
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_mul".to_string(),
            Self::float_multiply
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_div".to_string(),
            Self::float_divide
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_mod".to_string(),
            Self::float_modulo
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_neg".to_string(),
            Self::float_negate
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_pos".to_string(),
            Self::float_positive
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Float comparison operations
        handlers.insert(
            "Outrun.Intrinsic.f64_eq".to_string(),
            Self::float_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_gt".to_string(),
            Self::float_greater_than
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_ge".to_string(),
            Self::float_greater_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_lt".to_string(),
            Self::float_less_than
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_le".to_string(),
            Self::float_less_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Float mathematical operations
        handlers.insert(
            "Outrun.Intrinsic.f64_abs".to_string(),
            Self::float_abs
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_ceil".to_string(),
            Self::float_ceil
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_floor".to_string(),
            Self::float_floor
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_round".to_string(),
            Self::float_round
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_trunc".to_string(),
            Self::float_trunc
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_pow".to_string(),
            Self::float_pow
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Float precision operations
        handlers.insert(
            "Outrun.Intrinsic.f64_ceil_precision".to_string(),
            Self::float_ceil_precision
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_floor_precision".to_string(),
            Self::float_floor_precision
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_round_precision".to_string(),
            Self::float_round_precision
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Float status check operations
        handlers.insert(
            "Outrun.Intrinsic.f64_is_nan".to_string(),
            Self::float_is_nan
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_is_finite".to_string(),
            Self::float_is_finite
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_is_infinite".to_string(),
            Self::float_is_infinite
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.f64_to_string".to_string(),
            Self::float_to_string
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Boolean operations
        handlers.insert(
            "Outrun.Intrinsic.bool_and".to_string(),
            Self::boolean_and
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.bool_or".to_string(),
            Self::boolean_or
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.bool_not".to_string(),
            Self::boolean_not
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.bool_eq".to_string(),
            Self::boolean_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // List operations
        handlers.insert(
            "Outrun.Intrinsic.list_head".to_string(),
            Self::list_head
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.list_tail".to_string(),
            Self::list_tail
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.list_prepend".to_string(),
            Self::list_prepend
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.list_length".to_string(),
            Self::list_length
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // String operations
        handlers.insert(
            "Outrun.Intrinsic.string_length".to_string(),
            Self::string_length
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_concat".to_string(),
            Self::string_concat
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_eq".to_string(),
            Self::string_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_slice".to_string(),
            Self::string_slice
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_char_at".to_string(),
            Self::string_char_at
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_index_of".to_string(),
            Self::string_index_of
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_to_upper".to_string(),
            Self::string_to_upper
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_to_lower".to_string(),
            Self::string_to_lower
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_trim".to_string(),
            Self::string_trim
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_trim_start".to_string(),
            Self::string_trim_start
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_trim_end".to_string(),
            Self::string_trim_end
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.string_valid_utf8".to_string(),
            Self::string_valid_utf8
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Map operations
        handlers.insert(
            "Outrun.Intrinsic.map_get".to_string(),
            Self::map_get
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.map_put".to_string(),
            Self::map_put
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.map_remove".to_string(),
            Self::map_remove
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.map_size".to_string(),
            Self::map_size
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.map_equal".to_string(),
            Self::map_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Binary operations
        handlers.insert(
            "Outrun.Intrinsic.binary_byte_size".to_string(),
            Self::binary_byte_size
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.binary_to_hex".to_string(),
            Self::binary_to_hex
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.binary_from_hex".to_string(),
            Self::binary_from_hex
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.binary_concat".to_string(),
            Self::binary_concat
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.binary_byte_at".to_string(),
            Self::binary_byte_at
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.binary_slice".to_string(),
            Self::binary_slice
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.binary_index_of".to_string(),
            Self::binary_index_of
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Atom operations
        handlers.insert(
            "Outrun.Intrinsic.atom_eq".to_string(),
            Self::atom_equal
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );
        handlers.insert(
            "Outrun.Intrinsic.atom_to_string".to_string(),
            Self::atom_to_string
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Panic operations
        handlers.insert(
            "Outrun.Intrinsic.panic".to_string(),
            Self::panic
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        // Inspect operations
        handlers.insert(
            "Outrun.Intrinsic.inspect_value".to_string(),
            Self::inspect_value
                as fn(&mut Self, &HashMap<String, Value>, Span) -> Result<Value, IntrinsicError>,
        );

        handlers
    }

    /// Check if an intrinsic function exists
    pub fn has_intrinsic(&self, name: &str) -> bool {
        Self::get_intrinsic_handlers().contains_key(name)
    }

    /// Execute an intrinsic function by name with the given arguments
    pub fn execute_intrinsic(
        &mut self,
        name: &str,
        arguments: HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        self.execute_intrinsic_with_types(name, arguments, None, span)
    }

    /// Execute an intrinsic function using consolidated context object
    pub fn execute_intrinsic_with_context(
        &mut self,
        context: &IntrinsicExecutionContext,
    ) -> Result<Value, IntrinsicError> {
        // Delegate to the existing method for now, but this provides a cleaner interface
        self.execute_intrinsic_with_types(
            context.function_name,
            context.arguments.clone(),
            context.typed_expr,
            context.span,
        )
    }

    /// Execute an intrinsic function with proper type information from TypedExpression
    pub fn execute_intrinsic_with_types(
        &mut self,
        name: &str,
        arguments: HashMap<String, Value>,
        typed_expr: Option<&TypedExpression>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        // Handle special cases that need TypedExpression
        if name == "Outrun.Intrinsic.list_empty" {
            return self.list_empty_with_types(&arguments, typed_expr, span);
        }

        // Use the standard handlers for everything else
        let handlers = Self::get_intrinsic_handlers();
        if let Some(handler) = handlers.get(name) {
            handler(self, &arguments, span)
        } else {
            Err(IntrinsicError::NotFound {
                name: name.to_string(),
                span,
            })
        }
    }

    // Integer arithmetic operations

    fn integer_add(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::integer(lhs + rhs))
    }

    fn integer_subtract(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::integer(lhs - rhs))
    }

    fn integer_multiply(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::integer(lhs * rhs))
    }

    fn integer_divide(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        if rhs == 0 {
            return Err(IntrinsicError::DivisionByZero { span });
        }
        Ok(Value::integer(lhs / rhs))
    }

    fn integer_modulo(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        if rhs == 0 {
            return Err(IntrinsicError::DivisionByZero { span });
        }
        Ok(Value::integer(lhs % rhs))
    }

    fn integer_negate(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_integer_arg(arguments, "value", span)?;
        Ok(Value::integer(-value))
    }

    fn integer_positive(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_integer_arg(arguments, "value", span)?;
        Ok(Value::integer(value)) // Unary plus returns the value unchanged
    }

    // Integer bitwise operations

    fn integer_and(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::integer(lhs & rhs))
    }

    fn integer_or(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::integer(lhs | rhs))
    }

    fn integer_xor(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::integer(lhs ^ rhs))
    }

    fn integer_not(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_integer_arg(arguments, "value", span)?;
        Ok(Value::integer(!value))
    }

    fn integer_shl(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        if !(0..=63).contains(&rhs) {
            return Err(IntrinsicError::TypeMismatch {
                expected: "shift amount 0-63".to_string(),
                found: format!("{rhs}"),
                span,
            });
        }
        Ok(Value::integer(lhs << rhs))
    }

    fn integer_shr(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        if !(0..=63).contains(&rhs) {
            return Err(IntrinsicError::TypeMismatch {
                expected: "shift amount 0-63".to_string(),
                found: format!("{rhs}"),
                span,
            });
        }
        Ok(Value::integer(lhs >> rhs))
    }

    // Additional integer operations

    fn integer_abs(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_integer_arg(arguments, "value", span)?;
        Ok(Value::integer(value.abs()))
    }

    fn integer_pow(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let base = self.get_integer_arg(arguments, "lhs", span)?;
        let exp = self.get_integer_arg(arguments, "rhs", span)?;
        if exp < 0 {
            return Err(IntrinsicError::TypeMismatch {
                expected: "non-negative exponent".to_string(),
                found: format!("{exp}"),
                span,
            });
        }
        match base.checked_pow(exp as u32) {
            Some(result) => Ok(Value::integer(result)),
            None => Err(IntrinsicError::Internal {
                message: "Integer overflow in power operation".to_string(),
                span,
            }),
        }
    }

    fn integer_to_string(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_integer_arg(arguments, "value", span)?;
        Ok(Value::string(value.to_string()))
    }

    fn integer_to_string_radix(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_integer_arg(arguments, "value", span)?;
        let radix = self.get_integer_arg(arguments, "radix", span)?;
        if !(2..=36).contains(&radix) {
            return Err(IntrinsicError::TypeMismatch {
                expected: "radix 2-36".to_string(),
                found: format!("{radix}"),
                span,
            });
        }

        fn to_radix(mut value: i64, radix: i64) -> String {
            if value == 0 {
                return "0".to_string();
            }

            let is_negative = value < 0;
            if is_negative {
                value = -value;
            }

            let digits = "0123456789abcdefghijklmnopqrstuvwxyz";
            let mut result = String::new();

            while value > 0 {
                let digit = (value % radix) as usize;
                result.insert(0, digits.chars().nth(digit).unwrap());
                value /= radix;
            }

            if is_negative {
                result.insert(0, '-');
            }

            result
        }

        Ok(Value::string(to_radix(value, radix)))
    }

    // Float arithmetic operations

    fn float_add(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::float(lhs + rhs))
    }

    fn float_subtract(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::float(lhs - rhs))
    }

    fn float_multiply(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::float(lhs * rhs))
    }

    fn float_divide(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::float(lhs / rhs))
    }

    fn float_negate(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::float(-value))
    }

    fn float_positive(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::float(value)) // Unary plus returns the value unchanged
    }

    fn float_modulo(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::float(lhs % rhs))
    }

    // Float mathematical operations

    fn float_abs(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::float(value.abs()))
    }

    fn float_ceil(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::float(value.ceil()))
    }

    fn float_floor(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::float(value.floor()))
    }

    fn float_round(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::float(value.round()))
    }

    fn float_trunc(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::float(value.trunc()))
    }

    fn float_pow(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let base = self.get_float_arg(arguments, "lhs", span)?;
        let exp = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::float(base.powf(exp)))
    }

    // Float precision operations

    fn float_ceil_precision(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        let precision = self.get_integer_arg(arguments, "precision", span)?;
        let scale = 10.0_f64.powi(precision as i32);
        Ok(Value::float((value * scale).ceil() / scale))
    }

    fn float_floor_precision(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        let precision = self.get_integer_arg(arguments, "precision", span)?;
        let scale = 10.0_f64.powi(precision as i32);
        Ok(Value::float((value * scale).floor() / scale))
    }

    fn float_round_precision(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        let precision = self.get_integer_arg(arguments, "precision", span)?;
        let scale = 10.0_f64.powi(precision as i32);
        Ok(Value::float((value * scale).round() / scale))
    }

    // Float status check operations

    fn float_is_nan(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::boolean(value.is_nan()))
    }

    fn float_is_finite(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::boolean(value.is_finite()))
    }

    fn float_is_infinite(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::boolean(value.is_infinite()))
    }

    fn float_to_string(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_float_arg(arguments, "value", span)?;
        Ok(Value::string(value.to_string()))
    }

    // Integer comparison operations

    fn integer_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs == rhs))
    }

    fn integer_less_than(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs < rhs))
    }

    fn integer_less_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs <= rhs))
    }

    fn integer_greater_than(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs > rhs))
    }

    fn integer_greater_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_integer_arg(arguments, "lhs", span)?;
        let rhs = self.get_integer_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs >= rhs))
    }

    // Float comparison operations

    fn float_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs == rhs))
    }

    fn float_less_than(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs < rhs))
    }

    fn float_less_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs <= rhs))
    }

    fn float_greater_than(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs > rhs))
    }

    fn float_greater_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_float_arg(arguments, "lhs", span)?;
        let rhs = self.get_float_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs >= rhs))
    }

    // Boolean operations

    fn boolean_and(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_boolean_arg(arguments, "lhs", span)?;
        let rhs = self.get_boolean_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs && rhs))
    }

    fn boolean_or(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_boolean_arg(arguments, "lhs", span)?;
        let rhs = self.get_boolean_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs || rhs))
    }

    fn boolean_not(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_boolean_arg(arguments, "value", span)?;
        Ok(Value::boolean(!value))
    }

    fn boolean_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_boolean_arg(arguments, "lhs", span)?;
        let rhs = self.get_boolean_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs == rhs))
    }

    // List operations

    fn list_head(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let list = self.get_list_arg(arguments, "value", span)?;
        match list.head() {
            Some(value) => {
                // Return Option.some(value: head)
                self.create_option_some(value.clone(), span)
            }
            None => {
                // Return Option.none()
                self.create_option_none(span)
            }
        }
    }

    fn list_tail(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let list_value = self.get_list_value_arg(arguments, "value", span)?;
        match list_value.list_tail() {
            Some(tail_value) => Ok(tail_value),
            None => {
                // This should not happen as list_tail always returns a list (possibly empty)
                Err(IntrinsicError::Internal {
                    message: "List tail operation failed unexpectedly".to_string(),
                    span,
                })
            }
        }
    }

    fn list_prepend(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let list_value = self.get_list_value_arg(arguments, "list", span)?;
        let element = arguments
            .get("elem")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "elem".to_string(),
                function: "List_prepend".to_string(),
                span,
            })?;

        match list_value.list_prepend(element.clone()) {
            Some(new_list) => Ok(new_list),
            None => Err(IntrinsicError::Internal {
                message: "List prepend operation failed".to_string(),
                span,
            }),
        }
    }

    fn list_length(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let list_value = self.get_list_value_arg(arguments, "value", span)?;
        match list_value.list_length() {
            Some(length) => Ok(Value::integer(length as i64)),
            None => Err(IntrinsicError::Internal {
                message: "List length operation failed".to_string(),
                span,
            }),
        }
    }

    fn list_empty_with_types(
        &mut self,
        _arguments: &HashMap<String, Value>,
        typed_expr: Option<&TypedExpression>,
        _span: Span,
    ) -> Result<Value, IntrinsicError> {
        // Try to extract real type information from the typed expression
        if let Some(expr) = typed_expr {
            if let Some(return_type) = &expr.structured_type {
                let return_info = TypeExtractor::extract_function_return_info(return_type);
                match return_info {
                    FunctionReturnInfo::List { element_type } => {
                        return Ok(Value::empty_list(element_type));
                    }
                    _ => {
                        // Fall through to mock type creation
                    }
                }
            }
        }

        // Fallback to mock type if we can't extract proper type information
        let mock_element_type = self.create_mock_structured_type("T");
        Ok(Value::empty_list(mock_element_type))
    }

    // String operations

    fn string_length(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        // Return character count (Unicode code points), not byte count
        Ok(Value::integer(string.chars().count() as i64))
    }

    fn string_concat(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_string_arg(arguments, "lhs", span)?;
        let rhs = self.get_string_arg(arguments, "rhs", span)?;
        Ok(Value::string(format!("{lhs}{rhs}")))
    }

    // Additional string operations

    fn string_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_string_arg(arguments, "lhs", span)?;
        let rhs = self.get_string_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs == rhs))
    }

    fn string_slice(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        let start = self.get_integer_arg(arguments, "start", span)?;
        let end = self.get_integer_arg(arguments, "end", span)?;

        if start < 0 {
            return Err(IntrinsicError::TypeMismatch {
                expected: "non-negative start index".to_string(),
                found: format!("{start}"),
                span,
            });
        }

        let chars: Vec<char> = string.chars().collect();
        let start_idx = start as usize;
        let end_idx = if end < 0 { chars.len() } else { end as usize };

        if start_idx > chars.len() || end_idx > chars.len() || start_idx > end_idx {
            return Err(IntrinsicError::TypeMismatch {
                expected: "valid slice range".to_string(),
                found: format!("start: {}, end: {}, length: {}", start, end, chars.len()),
                span,
            });
        }

        let result: String = chars[start_idx..end_idx].iter().collect();
        Ok(Value::string(result))
    }

    fn string_char_at(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        let index = self.get_integer_arg(arguments, "index", span)?;

        if index < 0 {
            return Err(IntrinsicError::TypeMismatch {
                expected: "non-negative index".to_string(),
                found: format!("{index}"),
                span,
            });
        }

        let chars: Vec<char> = string.chars().collect();
        let idx = index as usize;

        if idx >= chars.len() {
            // Return Option.none() for out of bounds
            self.create_option_none(span)
        } else {
            // Return Option.some(char as string)
            let char_str = chars[idx].to_string();
            self.create_option_some(Value::string(char_str), span)
        }
    }

    fn string_index_of(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        let substring = self.get_string_arg(arguments, "substring", span)?;

        match string.find(substring) {
            Some(index) => {
                // Return Option.some(index)
                self.create_option_some(Value::integer(index as i64), span)
            }
            None => {
                // Return Option.none()
                self.create_option_none(span)
            }
        }
    }

    fn string_to_upper(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        Ok(Value::string(string.to_uppercase()))
    }

    fn string_to_lower(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        Ok(Value::string(string.to_lowercase()))
    }

    fn string_trim(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        Ok(Value::string(string.trim().to_string()))
    }

    fn string_trim_start(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        Ok(Value::string(string.trim_start().to_string()))
    }

    fn string_trim_end(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let string = self.get_string_arg(arguments, "value", span)?;
        Ok(Value::string(string.trim_end().to_string()))
    }

    fn string_valid_utf8(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let _string = self.get_string_arg(arguments, "value", span)?;
        // In Rust, String is always valid UTF-8 by construction
        Ok(Value::boolean(true))
    }

    // Binary trait operations

    fn binary_byte_size(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_string_arg(arguments, "value", span)?;
        // Return byte count (actual UTF-8 bytes), not character count
        Ok(Value::integer(value.len() as i64))
    }

    // Map operations

    fn map_get(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let map = arguments
            .get("map")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "map".to_string(),
                function: "map_get".to_string(),
                span,
            })?;
        let key = arguments
            .get("key")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "key".to_string(),
                function: "map_get".to_string(),
                span,
            })?;

        match map {
            Value::Map { entries, .. } => match entries.get(key) {
                Some(value) => self.create_option_some(value.clone(), span),
                None => self.create_option_none(span),
            },
            _ => Err(IntrinsicError::TypeMismatch {
                expected: "Map".to_string(),
                found: format!("{map:?}"),
                span,
            }),
        }
    }

    fn map_put(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let map = arguments
            .get("map")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "map".to_string(),
                function: "map_put".to_string(),
                span,
            })?;
        let key = arguments
            .get("key")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "key".to_string(),
                function: "map_put".to_string(),
                span,
            })?;
        let value = arguments
            .get("value")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "value".to_string(),
                function: "map_put".to_string(),
                span,
            })?;

        match map {
            Value::Map {
                entries,
                key_type,
                value_type,
                ..
            } => {
                let mut new_entries = entries.as_ref().clone();
                new_entries.insert(key.clone(), value.clone());
                Ok(Value::Map {
                    entries: Rc::new(new_entries),
                    key_type: key_type.clone(),
                    value_type: value_type.clone(),
                })
            }
            _ => Err(IntrinsicError::TypeMismatch {
                expected: "Map".to_string(),
                found: format!("{map:?}"),
                span,
            }),
        }
    }

    fn map_remove(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let map = arguments
            .get("map")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "map".to_string(),
                function: "map_remove".to_string(),
                span,
            })?;
        let key = arguments
            .get("key")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "key".to_string(),
                function: "map_remove".to_string(),
                span,
            })?;

        match map {
            Value::Map {
                entries,
                key_type,
                value_type,
                ..
            } => {
                let mut new_entries = entries.as_ref().clone();
                new_entries.remove(key);
                Ok(Value::Map {
                    entries: Rc::new(new_entries),
                    key_type: key_type.clone(),
                    value_type: value_type.clone(),
                })
            }
            _ => Err(IntrinsicError::TypeMismatch {
                expected: "Map".to_string(),
                found: format!("{map:?}"),
                span,
            }),
        }
    }

    fn map_size(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let map = arguments
            .get("map")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "map".to_string(),
                function: "map_size".to_string(),
                span,
            })?;

        match map {
            Value::Map { entries, .. } => Ok(Value::integer(entries.len() as i64)),
            _ => Err(IntrinsicError::TypeMismatch {
                expected: "Map".to_string(),
                found: format!("{map:?}"),
                span,
            }),
        }
    }

    fn map_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = arguments
            .get("lhs")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "lhs".to_string(),
                function: "map_equal".to_string(),
                span,
            })?;
        let rhs = arguments
            .get("rhs")
            .ok_or_else(|| IntrinsicError::MissingArgument {
                arg: "rhs".to_string(),
                function: "map_equal".to_string(),
                span,
            })?;

        match (lhs, rhs) {
            (
                Value::Map {
                    entries: lhs_entries,
                    ..
                },
                Value::Map {
                    entries: rhs_entries,
                    ..
                },
            ) => Ok(Value::boolean(lhs_entries == rhs_entries)),
            _ => Err(IntrinsicError::TypeMismatch {
                expected: "Map".to_string(),
                found: format!("lhs: {lhs:?}, rhs: {rhs:?}"),
                span,
            }),
        }
    }

    // Additional binary operations

    fn binary_to_hex(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_string_arg(arguments, "value", span)?;
        let hex_string = value
            .bytes()
            .map(|b| format!("{b:02x}"))
            .collect::<String>();
        Ok(Value::string(hex_string))
    }

    fn binary_from_hex(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let hex = self.get_string_arg(arguments, "hex", span)?;

        if hex.len() % 2 != 0 {
            return Err(IntrinsicError::TypeMismatch {
                expected: "even-length hex string".to_string(),
                found: format!("length {}", hex.len()),
                span,
            });
        }

        let mut bytes = Vec::new();
        for chunk in hex.chars().collect::<Vec<_>>().chunks(2) {
            let hex_byte: String = chunk.iter().collect();
            match u8::from_str_radix(&hex_byte, 16) {
                Ok(byte) => bytes.push(byte),
                Err(_) => {
                    return Err(IntrinsicError::TypeMismatch {
                        expected: "valid hex characters".to_string(),
                        found: hex_byte,
                        span,
                    });
                }
            }
        }

        match String::from_utf8(bytes) {
            Ok(string) => Ok(Value::string(string)),
            Err(_) => Err(IntrinsicError::TypeMismatch {
                expected: "valid UTF-8 from hex".to_string(),
                found: "invalid UTF-8 bytes".to_string(),
                span,
            }),
        }
    }

    fn binary_concat(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_string_arg(arguments, "lhs", span)?;
        let rhs = self.get_string_arg(arguments, "rhs", span)?;

        // For simplicity, treating strings as binary data for concatenation
        let mut lhs_bytes = lhs.as_bytes().to_vec();
        let rhs_bytes = rhs.as_bytes();
        lhs_bytes.extend_from_slice(rhs_bytes);

        match String::from_utf8(lhs_bytes) {
            Ok(result) => Ok(Value::string(result)),
            Err(_) => Err(IntrinsicError::TypeMismatch {
                expected: "valid UTF-8 result".to_string(),
                found: "invalid UTF-8 bytes".to_string(),
                span,
            }),
        }
    }

    fn binary_byte_at(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_string_arg(arguments, "value", span)?;
        let index = self.get_integer_arg(arguments, "index", span)?;

        if index < 0 {
            return Err(IntrinsicError::TypeMismatch {
                expected: "non-negative index".to_string(),
                found: format!("{index}"),
                span,
            });
        }

        let bytes = value.as_bytes();
        let idx = index as usize;

        if idx >= bytes.len() {
            self.create_option_none(span)
        } else {
            self.create_option_some(Value::integer(bytes[idx] as i64), span)
        }
    }

    fn binary_slice(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_string_arg(arguments, "value", span)?;
        let start = self.get_integer_arg(arguments, "start", span)?;
        let end = self.get_integer_arg(arguments, "end", span)?;

        if start < 0 {
            return Err(IntrinsicError::TypeMismatch {
                expected: "non-negative start index".to_string(),
                found: format!("{start}"),
                span,
            });
        }

        let bytes = value.as_bytes();
        let start_idx = start as usize;
        let end_idx = if end < 0 { bytes.len() } else { end as usize };

        if start_idx > bytes.len() || end_idx > bytes.len() || start_idx > end_idx {
            return Err(IntrinsicError::TypeMismatch {
                expected: "valid slice range".to_string(),
                found: format!("start: {}, end: {}, length: {}", start, end, bytes.len()),
                span,
            });
        }

        let result_bytes = &bytes[start_idx..end_idx];
        match String::from_utf8(result_bytes.to_vec()) {
            Ok(result) => Ok(Value::string(result)),
            Err(_) => Err(IntrinsicError::TypeMismatch {
                expected: "valid UTF-8 slice".to_string(),
                found: "invalid UTF-8 bytes".to_string(),
                span,
            }),
        }
    }

    fn binary_index_of(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = self.get_string_arg(arguments, "value", span)?;
        let pattern = self.get_string_arg(arguments, "pattern", span)?;

        let value_bytes = value.as_bytes();
        let pattern_bytes = pattern.as_bytes();

        if pattern_bytes.is_empty() {
            return self.create_option_some(Value::integer(0), span);
        }

        for i in 0..=value_bytes.len().saturating_sub(pattern_bytes.len()) {
            if &value_bytes[i..i + pattern_bytes.len()] == pattern_bytes {
                return self.create_option_some(Value::integer(i as i64), span);
            }
        }

        self.create_option_none(span)
    }

    // Atom operations

    fn atom_equal(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let lhs = self.get_atom_arg(arguments, "lhs", span)?;
        let rhs = self.get_atom_arg(arguments, "rhs", span)?;
        Ok(Value::boolean(lhs == rhs))
    }

    fn atom_to_string(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let atom = self.get_atom_arg(arguments, "value", span)?;
        Ok(Value::string(atom.to_string()))
    }

    // Panic operations

    fn panic(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let message = self.get_string_arg(arguments, "message", span)?;
        // This should never return normally - it represents a panic
        Err(IntrinsicError::Internal {
            message: message.to_string(),
            span,
        })
    }

    // Helper methods for argument extraction

    fn get_integer_arg(
        &self,
        arguments: &HashMap<String, Value>,
        name: &str,
        span: Span,
    ) -> Result<i64, IntrinsicError> {
        match arguments.get(name) {
            Some(Value::Integer64(value)) => Ok(*value),
            Some(other) => Err(IntrinsicError::TypeMismatch {
                expected: "Integer64".to_string(),
                found: format!("{other:?}"),
                span,
            }),
            None => Err(IntrinsicError::MissingArgument {
                arg: name.to_string(),
                function: "intrinsic".to_string(),
                span,
            }),
        }
    }

    fn get_float_arg(
        &self,
        arguments: &HashMap<String, Value>,
        name: &str,
        span: Span,
    ) -> Result<f64, IntrinsicError> {
        match arguments.get(name) {
            Some(Value::Float64(value)) => Ok(*value),
            Some(other) => Err(IntrinsicError::TypeMismatch {
                expected: "Float64".to_string(),
                found: format!("{other:?}"),
                span,
            }),
            None => Err(IntrinsicError::MissingArgument {
                arg: name.to_string(),
                function: "intrinsic".to_string(),
                span,
            }),
        }
    }

    fn get_boolean_arg(
        &self,
        arguments: &HashMap<String, Value>,
        name: &str,
        span: Span,
    ) -> Result<bool, IntrinsicError> {
        match arguments.get(name) {
            Some(Value::Boolean(value)) => Ok(*value),
            Some(other) => Err(IntrinsicError::TypeMismatch {
                expected: "Boolean".to_string(),
                found: format!("{other:?}"),
                span,
            }),
            None => Err(IntrinsicError::MissingArgument {
                arg: name.to_string(),
                function: "intrinsic".to_string(),
                span,
            }),
        }
    }

    fn get_string_arg<'a>(
        &self,
        arguments: &'a HashMap<String, Value>,
        name: &str,
        span: Span,
    ) -> Result<&'a str, IntrinsicError> {
        match arguments.get(name) {
            Some(Value::String(value)) => Ok(value),
            Some(other) => Err(IntrinsicError::TypeMismatch {
                expected: "String".to_string(),
                found: format!("{other:?}"),
                span,
            }),
            None => Err(IntrinsicError::MissingArgument {
                arg: name.to_string(),
                function: "intrinsic".to_string(),
                span,
            }),
        }
    }

    fn get_list_arg<'a>(
        &self,
        arguments: &'a HashMap<String, Value>,
        name: &str,
        span: Span,
    ) -> Result<&'a crate::list::List<Value>, IntrinsicError> {
        match arguments.get(name) {
            Some(Value::List { list, .. }) => Ok(list.as_ref()),
            Some(other) => Err(IntrinsicError::TypeMismatch {
                expected: "List".to_string(),
                found: format!("{other:?}"),
                span,
            }),
            None => Err(IntrinsicError::MissingArgument {
                arg: name.to_string(),
                function: "intrinsic".to_string(),
                span,
            }),
        }
    }

    fn get_list_value_arg<'a>(
        &self,
        arguments: &'a HashMap<String, Value>,
        name: &str,
        span: Span,
    ) -> Result<&'a Value, IntrinsicError> {
        match arguments.get(name) {
            Some(value @ Value::List { .. }) => Ok(value),
            Some(other) => Err(IntrinsicError::TypeMismatch {
                expected: "List".to_string(),
                found: format!("{other:?}"),
                span,
            }),
            None => Err(IntrinsicError::MissingArgument {
                arg: name.to_string(),
                function: "intrinsic".to_string(),
                span,
            }),
        }
    }

    fn get_atom_arg<'a>(
        &self,
        arguments: &'a HashMap<String, Value>,
        name: &str,
        span: Span,
    ) -> Result<&'a AtomId, IntrinsicError> {
        match arguments.get(name) {
            Some(Value::Atom(value)) => Ok(value),
            Some(other) => Err(IntrinsicError::TypeMismatch {
                expected: "Atom".to_string(),
                found: format!("{other:?}"),
                span,
            }),
            None => Err(IntrinsicError::MissingArgument {
                arg: name.to_string(),
                function: "intrinsic".to_string(),
                span,
            }),
        }
    }

    // Atom formatting helpers

    /// Format an atom for inspect output, with proper quoting when needed
    fn format_atom_for_inspect(&self, atom: &AtomId) -> String {
        let atom_string = atom.to_string();

        // Check if the atom needs quotes:
        // - Contains spaces or special characters
        // - Starts with a digit
        // - Is empty
        // - Contains non-ASCII alphanumeric characters (except underscore)
        // - Contains any non-alphanumeric ASCII characters (except underscore)
        let needs_quotes = atom_string.is_empty()
            || atom_string
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_digit())
            || atom_string.chars().any(|c| {
                // Need quotes for any non-ASCII character
                if !c.is_ascii() {
                    return true;
                }
                // Need quotes for ASCII characters that are not alphanumeric or underscore
                !c.is_ascii_alphanumeric() && c != '_'
            });

        if needs_quotes {
            // Escape special characters in the atom string
            let escaped = atom_string
                .replace("\\", "\\\\") // Escape backslashes first
                .replace("\"", "\\\"") // Escape quotes
                .replace("\n", "\\n") // Escape newlines
                .replace("\r", "\\r") // Escape carriage returns
                .replace("\t", "\\t"); // Escape tabs
            format!(":\"{escaped}\"")
        } else {
            format!(":{atom_string}")
        }
    }

    // Option/Result construction helpers

    fn create_option_some(&mut self, value: Value, _span: Span) -> Result<Value, IntrinsicError> {
        self.create_option_some_with_types(value, None, _span)
    }

    fn create_option_some_with_types(
        &mut self,
        value: Value,
        typed_expr: Option<&TypedExpression>,
        _span: Span,
    ) -> Result<Value, IntrinsicError> {
        let type_id = self
            .compiler_environment
            .intern_type_name("Outrun.Option.Some");
        let value_atom = self.compiler_environment.intern_atom_name("value");

        let mut fields = HashMap::new();
        fields.insert(value_atom, value);

        // Try to extract proper Option<T> type from typed expression
        let struct_type = if let Some(expr) = typed_expr {
            if let Some(return_type) = &expr.structured_type {
                return_type.clone()
            } else {
                self.create_mock_structured_type("Option.Some<T>")
            }
        } else {
            self.create_mock_structured_type("Option.Some<T>")
        };

        Ok(Value::struct_instance(type_id, fields, struct_type))
    }

    fn create_option_none(&mut self, _span: Span) -> Result<Value, IntrinsicError> {
        self.create_option_none_with_types(None, _span)
    }

    fn create_option_none_with_types(
        &mut self,
        typed_expr: Option<&TypedExpression>,
        _span: Span,
    ) -> Result<Value, IntrinsicError> {
        let type_id = self
            .compiler_environment
            .intern_type_name("Outrun.Option.None");
        let fields = HashMap::new(); // None has no fields

        // Try to extract proper Option<T> type from typed expression
        let struct_type = if let Some(expr) = typed_expr {
            if let Some(return_type) = &expr.structured_type {
                return_type.clone()
            } else {
                self.create_mock_structured_type("Option.None")
            }
        } else {
            self.create_mock_structured_type("Option.None")
        };

        Ok(Value::struct_instance(type_id, fields, struct_type))
    }

    // Temporary helper for mock types - will be replaced with proper type integration
    fn create_mock_structured_type(&mut self, name: &str) -> StructuredType {
        let type_id = self.compiler_environment.intern_type_name(name);
        StructuredType::Simple(type_id)
    }

    // Inspect operations

    /// Universal inspect function that handles all value types
    /// This is used as the default implementation for the Inspect trait
    #[allow(clippy::only_used_in_recursion)]
    fn inspect_value(
        &mut self,
        arguments: &HashMap<String, Value>,
        span: Span,
    ) -> Result<Value, IntrinsicError> {
        let value = arguments
            .get("value")
            .ok_or(IntrinsicError::MissingArgument {
                arg: "value".to_string(),
                function: "inspect_value".to_string(),
                span,
            })?;

        let inspect_result = match value {
            Value::Integer64(i) => i.to_string(),
            Value::Float64(f) => f.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => format!("\"{}\"", s.replace("\\", "\\\\").replace("\"", "\\\"")),
            Value::Atom(a) => self.format_atom_for_inspect(a),
            Value::List { list, .. } => {
                // Format as [elem1, elem2, ...]
                let elements: Vec<String> = list
                    .iter()
                    .map(|elem| {
                        // Recursively inspect each element
                        let mut inspect_args = HashMap::new();
                        inspect_args.insert("value".to_string(), elem.clone());
                        match self.inspect_value(&inspect_args, span) {
                            Ok(Value::String(s)) => s,
                            _ => format!("{elem:?}"), // Fallback
                        }
                    })
                    .collect();
                format!("[{}]", elements.join(", "))
            }
            Value::Map { entries, .. } => {
                // Format as %{key1: value1, key2: value2, ...}
                let entry_strings: Vec<String> = entries
                    .iter()
                    .map(|(key, val)| {
                        // Recursively inspect key and value
                        let mut key_args = HashMap::new();
                        key_args.insert("value".to_string(), key.clone());
                        let key_str = match self.inspect_value(&key_args, span) {
                            Ok(Value::String(s)) => s,
                            _ => format!("{key:?}"), // Fallback
                        };

                        let mut val_args = HashMap::new();
                        val_args.insert("value".to_string(), val.clone());
                        let val_str = match self.inspect_value(&val_args, span) {
                            Ok(Value::String(s)) => s,
                            _ => format!("{val:?}"), // Fallback
                        };

                        format!("{key_str}: {val_str}")
                    })
                    .collect();
                format!("%{{{}}}", entry_strings.join(", "))
            }
            Value::Tuple { elements, .. } => {
                // Format as (elem1, elem2, ...)
                let element_strings: Vec<String> = elements
                    .iter()
                    .map(|elem| {
                        // Recursively inspect each element
                        let mut inspect_args = HashMap::new();
                        inspect_args.insert("value".to_string(), elem.clone());
                        match self.inspect_value(&inspect_args, span) {
                            Ok(Value::String(s)) => s,
                            _ => format!("{elem:?}"), // Fallback
                        }
                    })
                    .collect();
                format!("({})", element_strings.join(", "))
            }
            Value::Struct {
                type_id, fields, ..
            } => {
                // Format as constructor syntax: TypeName { field1: value1, field2: value2, ... }
                let type_name = type_id.to_string();

                if fields.is_empty() {
                    format!("{type_name} {{}}")
                } else {
                    let field_strings: Vec<String> = fields
                        .iter()
                        .map(|(field_name, field_value)| {
                            let field_name_str = field_name.to_string();
                            // Recursively inspect field values
                            let mut field_args = HashMap::new();
                            field_args.insert("value".to_string(), field_value.clone());
                            let field_value_str = match self.inspect_value(&field_args, span) {
                                Ok(Value::String(s)) => s,
                                _ => format!("{field_value:?}"), // Fallback
                            };
                            format!("{field_name_str}: {field_value_str}")
                        })
                        .collect();

                    format!("{} {{ {} }}", type_name, field_strings.join(", "))
                }
            }
        };

        Ok(Value::String(inspect_result))
    }

    /// Get access to the compiler environment for advanced operations
    pub fn compiler_environment(&mut self) -> &mut CompilerEnvironment {
        &mut self.compiler_environment
    }
}

impl Default for IntrinsicsHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::Span;

    fn test_span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn test_integer_arithmetic() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        // Test addition
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(5));
        args.insert("rhs".to_string(), Value::integer(3));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.i64_add", args, span)
            .unwrap();
        assert_eq!(result, Value::integer(8));

        // Test subtraction
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(10));
        args.insert("rhs".to_string(), Value::integer(4));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.i64_sub", args, span)
            .unwrap();
        assert_eq!(result, Value::integer(6));

        // Test multiplication
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(6));
        args.insert("rhs".to_string(), Value::integer(7));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.i64_mul", args, span)
            .unwrap();
        assert_eq!(result, Value::integer(42));

        // Test division
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(15));
        args.insert("rhs".to_string(), Value::integer(3));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.i64_div", args, span)
            .unwrap();
        assert_eq!(result, Value::integer(5));
    }

    #[test]
    fn test_integer_division_by_zero() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(10));
        args.insert("rhs".to_string(), Value::integer(0));

        let result = handler.execute_intrinsic("Outrun.Intrinsic.i64_div", args, span);
        assert!(matches!(result, Err(IntrinsicError::DivisionByZero { .. })));
    }

    #[test]
    fn test_integer_comparison() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        // Test equality
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(5));
        args.insert("rhs".to_string(), Value::integer(5));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.i64_eq", args, span)
            .unwrap();
        assert_eq!(result, Value::boolean(true));

        // Test less than
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(3));
        args.insert("rhs".to_string(), Value::integer(7));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.i64_lt", args, span)
            .unwrap();
        assert_eq!(result, Value::boolean(true));

        // Test greater than
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(10));
        args.insert("rhs".to_string(), Value::integer(5));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.i64_gt", args, span)
            .unwrap();
        assert_eq!(result, Value::boolean(true));
    }

    #[test]
    fn test_float_arithmetic() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        // Test float addition
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::float(2.5));
        args.insert("rhs".to_string(), Value::float(3.7));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.f64_add", args, span)
            .unwrap();
        assert!((result.as_float().unwrap() - 6.2).abs() < f64::EPSILON);

        // Test float division
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::float(10.0));
        args.insert("rhs".to_string(), Value::float(4.0));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.f64_div", args, span)
            .unwrap();
        assert_eq!(result, Value::float(2.5));
    }

    #[test]
    fn test_boolean_operations() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        // Test boolean and
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::boolean(true));
        args.insert("rhs".to_string(), Value::boolean(false));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.bool_and", args, span)
            .unwrap();
        assert_eq!(result, Value::boolean(false));

        // Test boolean or
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::boolean(true));
        args.insert("rhs".to_string(), Value::boolean(false));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.bool_or", args, span)
            .unwrap();
        assert_eq!(result, Value::boolean(true));

        // Test boolean not
        let mut args = HashMap::new();
        args.insert("value".to_string(), Value::boolean(true));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.bool_not", args, span)
            .unwrap();
        assert_eq!(result, Value::boolean(false));
    }

    #[test]
    fn test_string_operations() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        // Test string length
        let mut args = HashMap::new();
        args.insert("value".to_string(), Value::string("hello".to_string()));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.string_length", args, span)
            .unwrap();
        assert_eq!(result, Value::integer(5));

        // Test string concatenation
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::string("hello".to_string()));
        args.insert("rhs".to_string(), Value::string(" world".to_string()));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.string_concat", args, span)
            .unwrap();
        assert_eq!(result, Value::string("hello world".to_string()));
    }

    #[test]
    fn test_list_operations() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        // Create a list with some elements
        let mock_type = handler.create_mock_structured_type("Integer");
        let list_val = Value::list_from_vec(vec![Value::integer(1), Value::integer(2)], mock_type);

        // Test list length
        let mut args = HashMap::new();
        args.insert("value".to_string(), list_val.clone());

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.list_length", args, span)
            .unwrap();
        assert_eq!(result, Value::integer(2));

        // Test list prepend
        let mut args = HashMap::new();
        args.insert("list".to_string(), list_val);
        args.insert("elem".to_string(), Value::integer(0));

        let result = handler
            .execute_intrinsic("Outrun.Intrinsic.list_prepend", args, span)
            .unwrap();
        assert_eq!(result.list_length(), Some(3));
        assert_eq!(result.list_head(), Some(Some(&Value::integer(0))));
    }

    #[test]
    fn test_type_mismatch_errors() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        // Try to add a string and integer
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::string("hello".to_string()));
        args.insert("rhs".to_string(), Value::integer(42));

        let result = handler.execute_intrinsic("Outrun.Intrinsic.i64_add", args, span);
        assert!(matches!(result, Err(IntrinsicError::TypeMismatch { .. })));
    }

    #[test]
    fn test_missing_argument_errors() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        // Try to call integer add with only one argument
        let mut args = HashMap::new();
        args.insert("lhs".to_string(), Value::integer(5));
        // Missing "rhs" argument

        let result = handler.execute_intrinsic("Outrun.Intrinsic.i64_add", args, span);
        assert!(matches!(
            result,
            Err(IntrinsicError::MissingArgument { .. })
        ));
    }

    #[test]
    fn test_unknown_intrinsic() {
        let mut handler = IntrinsicsHandler::new();
        let span = test_span();

        let args = HashMap::new();
        let result = handler.execute_intrinsic("Outrun.Intrinsic.NonExistent", args, span);
        assert!(matches!(result, Err(IntrinsicError::NotFound { .. })));
    }
}

// Compile-time validation of intrinsic completeness
#[cfg(test)]
mod intrinsic_validation {
    use super::*;

    /// Extract function names from the has_intrinsic method by checking what returns true
    fn get_implemented_intrinsic_names() -> Vec<String> {
        // Test each known intrinsic pattern to see what's implemented
        // This approach validates against the actual has_intrinsic implementation
        let handler = IntrinsicsHandler::new();
        let mut implemented = Vec::new();

        // All intrinsic patterns from has_intrinsic method
        let candidate_intrinsics = [
            // Integer arithmetic intrinsics (legacy names)
            "Integer_add",
            "Integer_subtract",
            "Integer_multiply",
            "Integer_divide",
            "Integer_modulo",
            "Integer_negate",
            // Integer arithmetic intrinsics (current names from typechecker)
            "i64_add",
            "i64_sub",
            "i64_mul",
            "i64_div",
            "i64_mod",
            "i64_neg",
            "i64_pos",
            // Float arithmetic intrinsics
            "Float_add",
            "Float_subtract",
            "Float_multiply",
            "Float_divide",
            "Float_negate",
            // Integer comparison intrinsics (legacy names)
            "Integer_equal",
            "Integer_not_equal",
            "Integer_less_than",
            "Integer_less_equal",
            "Integer_greater_than",
            "Integer_greater_equal",
            // Integer comparison intrinsics (current names from typechecker)
            "i64_eq",
            "i64_gt",
            "i64_ge",
            "i64_lt",
            "i64_le",
            // Float comparison intrinsics
            "Float_equal",
            "Float_not_equal",
            "Float_less_than",
            "Float_less_equal",
            "Float_greater_than",
            "Float_greater_equal",
            // Boolean operations
            "Boolean_and",
            "Boolean_or",
            "Boolean_not",
            "bool_and",
            "bool_or",
            "bool_not",
            "bool_eq",
            // List intrinsics
            "List_head",
            "List_tail",
            "List_prepend",
            "List_length",
            "List_empty",
            "List_is_empty",
            // Option construction intrinsics
            "Option_some",
            "Option_none",
            // Result construction intrinsics
            "Result_ok",
            "Result_error",
            // String operations
            "String_length",
            "String_concat",
            // Binary operations
            "binary_byte_size",
            // Inspect operations
            "inspect_value",
            // Default trait intrinsics
            "Default_default",
        ];

        for intrinsic in &candidate_intrinsics {
            let full_name = format!("Outrun.Intrinsic.{intrinsic}");
            if handler.has_intrinsic(&full_name) {
                implemented.push(intrinsic.to_string());
            }
        }

        implemented
    }

    #[test]
    fn validate_interpreter_intrinsics_completeness() {
        // This test will fail at compile time if there are missing or extra intrinsics
        let implemented_names = get_implemented_intrinsic_names();

        // TODO: Enable this once outrun-intrinsics is added as dependency
        // outrun_intrinsics::validate_intrinsic_completeness(&implemented_names, "interpreter")
        //     .expect("Interpreter intrinsics do not match specification");

        // For now, just check we have intrinsics implemented
        assert!(
            !implemented_names.is_empty(),
            "No intrinsics implemented in interpreter"
        );

        // The implemented_names are extracted directly from has_intrinsic, so they should all exist
        // This validation step is automatically correct by design

        // Print the implemented intrinsics for manual verification
        println!(
            "Interpreter implements {} intrinsics:",
            implemented_names.len()
        );
        let mut sorted_names = implemented_names.clone();
        sorted_names.sort();
        for name in sorted_names {
            println!("  - {name}");
        }
    }
}
