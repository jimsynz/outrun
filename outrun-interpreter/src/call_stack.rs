//! Call stack management for function call tracking and error context
//!
//! This module provides call stack management for tracking function calls,
//! maintaining parameter bindings, and preserving error context with source spans.
//! It supports recursive calls, tail call optimization, and provides detailed
//! stack traces for debugging.
//!
//! ## Tail Call Optimization
//!
//! Since Outrun is a functional programming language with heavy recursion usage,
//! this implementation includes tail call optimization to prevent stack overflow
//! in recursive functions. When a function call is detected to be in tail position,
//! the current frame is reused instead of creating a new one.

use crate::value::Value;
use outrun_parser::Span;
use outrun_typechecker::checker::TypedFunctionPath;
use outrun_typechecker::compilation::compiler_environment::TypeNameId;
use std::collections::HashMap;
use thiserror::Error;

/// Maximum call stack depth to prevent stack overflow
const MAX_CALL_STACK_DEPTH: usize = 1000;

/// Errors that can occur during call stack operations
#[derive(Debug, Error)]
pub enum CallStackError {
    #[error("Stack overflow: maximum call depth of {max_depth} exceeded")]
    StackOverflow { max_depth: usize, span: Span },

    #[error("Stack underflow: attempted to pop from empty stack")]
    StackUnderflow { span: Span },

    #[error("Variable '{name}' not found in current frame")]
    VariableNotFound { name: String, span: Span },

    #[error("Invalid frame operation: {message}")]
    InvalidFrameOperation { message: String, span: Span },

    #[error("Tail call optimization failed: {message}")]
    TailCallOptimizationFailed { message: String, span: Span },
}

/// A single call frame on the stack
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// The function that was called
    pub function_path: TypedFunctionPath,

    /// Parameter bindings for this function call
    pub parameters: HashMap<String, Value>,

    /// Local variable bindings within this function
    pub locals: HashMap<String, Value>,

    /// Return type expected from this function
    pub return_type: Option<TypeNameId>,

    /// Source span where this function was called
    pub call_site: Span,

    /// Source span of the function definition (if available)
    pub function_span: Option<Span>,

    /// Number of tail calls optimized in this frame
    pub tail_call_count: usize,

    /// Self type context for trait default implementations
    pub self_type: Option<outrun_typechecker::unification::StructuredType>,
}

impl CallFrame {
    /// Create a new call frame for a function call
    pub fn new(
        function_path: TypedFunctionPath,
        parameters: HashMap<String, Value>,
        return_type: Option<TypeNameId>,
        call_site: Span,
        function_span: Option<Span>,
        self_type: Option<outrun_typechecker::unification::StructuredType>,
    ) -> Self {
        Self {
            function_path,
            parameters,
            locals: HashMap::new(),
            return_type,
            call_site,
            function_span,
            tail_call_count: 0,
            self_type,
        }
    }

    /// Reuse this frame for a tail call optimization
    pub fn reuse_for_tail_call(
        &mut self,
        new_parameters: HashMap<String, Value>,
        new_call_site: Span,
    ) {
        // Keep the same function_path, return_type, and function_span
        self.parameters = new_parameters;
        self.locals.clear(); // Clear locals for fresh execution
        self.call_site = new_call_site; // Update to new call site
        self.tail_call_count += 1; // Track optimization
    }

    /// Look up a variable (parameter or local) in this frame
    pub fn lookup_variable(&self, name: &str) -> Option<&Value> {
        // Check locals first, then parameters
        self.locals.get(name).or_else(|| self.parameters.get(name))
    }

    /// Set a local variable in this frame
    pub fn set_local_variable(&mut self, name: String, value: Value) {
        self.locals.insert(name, value);
    }

    /// Get the function name for display purposes
    pub fn function_name(&self) -> String {
        match &self.function_path {
            TypedFunctionPath::Simple { name } => name.clone(),
            TypedFunctionPath::Qualified { module, name } => format!("{module}.{name}"),
            TypedFunctionPath::Expression { .. } => "<dynamic function>".to_string(),
        }
    }

    /// Check if this frame represents a recursive call to the same function
    pub fn is_recursive_call(&self, other_function_path: &TypedFunctionPath) -> bool {
        match (&self.function_path, other_function_path) {
            (
                TypedFunctionPath::Simple { name: name1 },
                TypedFunctionPath::Simple { name: name2 },
            ) => name1 == name2,
            (
                TypedFunctionPath::Qualified {
                    module: mod1,
                    name: name1,
                },
                TypedFunctionPath::Qualified {
                    module: mod2,
                    name: name2,
                },
            ) => mod1 == mod2 && name1 == name2,
            _ => false, // Expression-based calls are never considered recursive for simplicity
        }
    }
}

/// Call stack manager for tracking function calls
#[derive(Debug, Clone)]
pub struct CallStack {
    /// Stack of call frames (bottom to top)
    frames: Vec<CallFrame>,

    /// Maximum allowed stack depth
    max_depth: usize,
}

impl CallStack {
    /// Create a new empty call stack
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            max_depth: MAX_CALL_STACK_DEPTH,
        }
    }

    /// Create a new call stack with custom maximum depth
    pub fn with_max_depth(max_depth: usize) -> Self {
        Self {
            frames: Vec::new(),
            max_depth,
        }
    }

    /// Push a new call frame onto the stack
    pub fn push_frame(&mut self, frame: CallFrame) -> Result<(), CallStackError> {
        if self.frames.len() >= self.max_depth {
            return Err(CallStackError::StackOverflow {
                max_depth: self.max_depth,
                span: frame.call_site,
            });
        }

        self.frames.push(frame);
        Ok(())
    }

    /// Pop the top call frame from the stack
    pub fn pop_frame(&mut self, span: Span) -> Result<CallFrame, CallStackError> {
        self.frames
            .pop()
            .ok_or(CallStackError::StackUnderflow { span })
    }

    /// Get the current (top) call frame
    pub fn current_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    /// Get a mutable reference to the current call frame
    pub fn current_frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.frames.last_mut()
    }

    /// Get the current Self type context from the top call frame
    pub fn current_self_type(&self) -> Option<&outrun_typechecker::unification::StructuredType> {
        self.current_frame()?.self_type.as_ref()
    }

    /// Get the call stack depth
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    /// Check if the stack is empty
    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Look up a variable in the current frame
    pub fn lookup_variable(&self, name: &str, span: Span) -> Result<&Value, CallStackError> {
        match self.current_frame() {
            Some(frame) => {
                frame
                    .lookup_variable(name)
                    .ok_or_else(|| CallStackError::VariableNotFound {
                        name: name.to_string(),
                        span,
                    })
            }
            None => Err(CallStackError::VariableNotFound {
                name: name.to_string(),
                span,
            }),
        }
    }

    /// Set a local variable in the current frame
    pub fn set_local_variable(
        &mut self,
        name: String,
        value: Value,
        span: Span,
    ) -> Result<(), CallStackError> {
        match self.current_frame_mut() {
            Some(frame) => {
                frame.set_local_variable(name, value);
                Ok(())
            }
            None => Err(CallStackError::InvalidFrameOperation {
                message: "No current frame to set variable in".to_string(),
                span,
            }),
        }
    }

    /// Check if the next function call would be recursive
    pub fn would_be_recursive(&self, function_path: &TypedFunctionPath) -> bool {
        self.frames
            .iter()
            .any(|frame| frame.is_recursive_call(function_path))
    }

    /// Check if a function call can be tail-call optimized
    /// This returns true if the current frame is calling the same function
    pub fn can_tail_call_optimize(&self, function_path: &TypedFunctionPath) -> bool {
        match self.current_frame() {
            Some(current_frame) => current_frame.is_recursive_call(function_path),
            None => false, // No current frame, can't optimize
        }
    }

    /// Push a new frame, but use tail call optimization if possible
    pub fn push_frame_or_tail_call(
        &mut self,
        frame: CallFrame,
        is_tail_call: bool,
    ) -> Result<(), CallStackError> {
        if is_tail_call && self.can_tail_call_optimize(&frame.function_path) {
            self.tail_call_optimize(frame.parameters, frame.call_site)
        } else {
            self.push_frame(frame)
        }
    }

    /// Optimize a tail call by reusing the current frame
    pub fn tail_call_optimize(
        &mut self,
        new_parameters: HashMap<String, Value>,
        new_call_site: Span,
    ) -> Result<(), CallStackError> {
        match self.current_frame_mut() {
            Some(frame) => {
                frame.reuse_for_tail_call(new_parameters, new_call_site);
                Ok(())
            }
            None => Err(CallStackError::TailCallOptimizationFailed {
                message: "No current frame to optimize".to_string(),
                span: new_call_site,
            }),
        }
    }

    /// Get total number of tail calls optimized across all frames
    pub fn total_tail_calls_optimized(&self) -> usize {
        self.frames.iter().map(|frame| frame.tail_call_count).sum()
    }

    /// Get a stack trace for error reporting
    pub fn stack_trace(&self) -> Vec<StackTraceEntry> {
        self.frames
            .iter()
            .enumerate()
            .map(|(depth, frame)| StackTraceEntry {
                depth,
                function_name: frame.function_name(),
                call_site: frame.call_site,
                function_span: frame.function_span,
                parameter_count: frame.parameters.len(),
                local_count: frame.locals.len(),
                tail_call_count: frame.tail_call_count,
            })
            .collect()
    }

    /// Get a formatted stack trace string
    pub fn format_stack_trace(&self, title: &str) -> String {
        let mut result = format!("{title}\n");

        if self.frames.is_empty() {
            result.push_str("  (no call stack)\n");
            return result;
        }

        let total_tail_calls = self.total_tail_calls_optimized();
        if total_tail_calls > 0 {
            result.push_str(&format!(
                "  (tail call optimization: {total_tail_calls} calls optimized)\n"
            ));
        }

        for entry in self.stack_trace().iter().rev() {
            let tail_info = if entry.tail_call_count > 0 {
                format!(" [+{} tail calls]", entry.tail_call_count)
            } else {
                String::new()
            };

            result.push_str(&format!(
                "  {}: {}{} (called at line {})\n",
                entry.depth,
                entry.function_name,
                tail_info,
                entry.call_site.start // Simplified span display
            ));
        }

        result
    }

    /// Clear the entire call stack (for REPL reset)
    pub fn clear(&mut self) {
        self.frames.clear();
    }

    /// Get all frames (for debugging/inspection)
    pub fn frames(&self) -> &[CallFrame] {
        &self.frames
    }
}

impl Default for CallStack {
    fn default() -> Self {
        Self::new()
    }
}

/// A single entry in a stack trace
#[derive(Debug, Clone)]
pub struct StackTraceEntry {
    /// Depth in the call stack (0 = bottom, higher = deeper)
    pub depth: usize,

    /// Name of the function
    pub function_name: String,

    /// Where this function was called from
    pub call_site: Span,

    /// Where the function is defined (if available)
    pub function_span: Option<Span>,

    /// Number of parameters passed to this function
    pub parameter_count: usize,

    /// Number of local variables in this frame
    pub local_count: usize,

    /// Number of tail calls optimized in this frame
    pub tail_call_count: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::Span;
    use outrun_typechecker::checker::TypedFunctionPath;

    fn test_span() -> Span {
        Span::new(0, 10)
    }

    fn create_test_frame(name: &str) -> CallFrame {
        let function_path = TypedFunctionPath::Simple {
            name: name.to_string(),
        };
        let mut parameters = HashMap::new();
        parameters.insert("x".to_string(), Value::integer(42));

        CallFrame::new(function_path, parameters, None, test_span(), None, None)
    }

    #[test]
    fn test_call_stack_creation() {
        let stack = CallStack::new();
        assert!(stack.is_empty());
        assert_eq!(stack.depth(), 0);
        assert_eq!(stack.max_depth, MAX_CALL_STACK_DEPTH);
    }

    #[test]
    fn test_call_stack_with_custom_depth() {
        let stack = CallStack::with_max_depth(50);
        assert_eq!(stack.max_depth, 50);
    }

    #[test]
    fn test_push_and_pop_frames() {
        let mut stack = CallStack::new();
        let frame1 = create_test_frame("function1");
        let frame2 = create_test_frame("function2");

        // Push frames
        stack.push_frame(frame1.clone()).unwrap();
        assert_eq!(stack.depth(), 1);
        assert_eq!(stack.current_frame().unwrap().function_name(), "function1");

        stack.push_frame(frame2.clone()).unwrap();
        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.current_frame().unwrap().function_name(), "function2");

        // Pop frames
        let popped = stack.pop_frame(test_span()).unwrap();
        assert_eq!(popped.function_name(), "function2");
        assert_eq!(stack.depth(), 1);

        let popped = stack.pop_frame(test_span()).unwrap();
        assert_eq!(popped.function_name(), "function1");
        assert!(stack.is_empty());
    }

    #[test]
    fn test_stack_overflow_protection() {
        let mut stack = CallStack::with_max_depth(2);

        let frame1 = create_test_frame("func1");
        let frame2 = create_test_frame("func2");
        let frame3 = create_test_frame("func3");

        // First two frames should succeed
        stack.push_frame(frame1).unwrap();
        stack.push_frame(frame2).unwrap();

        // Third frame should fail with stack overflow
        let result = stack.push_frame(frame3);
        assert!(matches!(result, Err(CallStackError::StackOverflow { .. })));
    }

    #[test]
    fn test_stack_underflow_protection() {
        let mut stack = CallStack::new();

        let result = stack.pop_frame(test_span());
        assert!(matches!(result, Err(CallStackError::StackUnderflow { .. })));
    }

    #[test]
    fn test_variable_lookup() {
        let mut stack = CallStack::new();
        let frame = create_test_frame("test_func");

        stack.push_frame(frame).unwrap();

        // Look up parameter
        let value = stack.lookup_variable("x", test_span()).unwrap();
        assert_eq!(value, &Value::integer(42));

        // Look up non-existent variable
        let result = stack.lookup_variable("nonexistent", test_span());
        assert!(matches!(
            result,
            Err(CallStackError::VariableNotFound { .. })
        ));
    }

    #[test]
    fn test_local_variable_management() {
        let mut stack = CallStack::new();
        let frame = create_test_frame("test_func");

        stack.push_frame(frame).unwrap();

        // Set a local variable
        stack
            .set_local_variable(
                "local_var".to_string(),
                Value::string("hello".to_string()),
                test_span(),
            )
            .unwrap();

        // Look up the local variable
        let value = stack.lookup_variable("local_var", test_span()).unwrap();
        assert_eq!(value, &Value::string("hello".to_string()));

        // Parameters should still be accessible
        let param_value = stack.lookup_variable("x", test_span()).unwrap();
        assert_eq!(param_value, &Value::integer(42));
    }

    #[test]
    fn test_recursive_call_detection() {
        let stack = CallStack::new();

        let func_path1 = TypedFunctionPath::Simple {
            name: "factorial".to_string(),
        };
        let func_path2 = TypedFunctionPath::Simple {
            name: "fibonacci".to_string(),
        };
        let func_path3 = TypedFunctionPath::Simple {
            name: "factorial".to_string(),
        }; // Same as func_path1

        // Empty stack - no recursion
        assert!(!stack.would_be_recursive(&func_path1));

        // Test with frames in stack
        let mut stack_with_frames = CallStack::new();
        let frame1 = CallFrame::new(
            func_path1.clone(),
            HashMap::new(),
            None,
            test_span(),
            None,
            None,
        );
        stack_with_frames.push_frame(frame1).unwrap();

        // Different function - no recursion
        assert!(!stack_with_frames.would_be_recursive(&func_path2));

        // Same function - recursion detected
        assert!(stack_with_frames.would_be_recursive(&func_path3));
    }

    #[test]
    fn test_qualified_function_recursion() {
        let mut stack = CallStack::new();

        let func_path1 = TypedFunctionPath::Qualified {
            module: "Math".to_string(),
            name: "factorial".to_string(),
        };
        let func_path2 = TypedFunctionPath::Qualified {
            module: "Math".to_string(),
            name: "factorial".to_string(),
        };
        let func_path3 = TypedFunctionPath::Qualified {
            module: "Utils".to_string(),
            name: "factorial".to_string(),
        };

        let frame = CallFrame::new(func_path1, HashMap::new(), None, test_span(), None, None);
        stack.push_frame(frame).unwrap();

        // Same module and function - recursion
        assert!(stack.would_be_recursive(&func_path2));

        // Different module, same function name - no recursion
        assert!(!stack.would_be_recursive(&func_path3));
    }

    #[test]
    fn test_stack_trace_generation() {
        let mut stack = CallStack::new();

        let frame1 = create_test_frame("main");
        let frame2 = create_test_frame("helper");
        let frame3 = create_test_frame("utility");

        stack.push_frame(frame1).unwrap();
        stack.push_frame(frame2).unwrap();
        stack.push_frame(frame3).unwrap();

        let trace = stack.stack_trace();
        assert_eq!(trace.len(), 3);

        // Check frames are in correct order (bottom to top)
        assert_eq!(trace[0].function_name, "main");
        assert_eq!(trace[0].depth, 0);
        assert_eq!(trace[1].function_name, "helper");
        assert_eq!(trace[1].depth, 1);
        assert_eq!(trace[2].function_name, "utility");
        assert_eq!(trace[2].depth, 2);
    }

    #[test]
    fn test_formatted_stack_trace() {
        let mut stack = CallStack::new();
        let frame = create_test_frame("test_function");
        stack.push_frame(frame).unwrap();

        let formatted = stack.format_stack_trace("Test Error");
        assert!(formatted.contains("Test Error"));
        assert!(formatted.contains("test_function"));
        assert!(formatted.contains("0:"));
    }

    #[test]
    fn test_empty_stack_trace() {
        let stack = CallStack::new();
        let formatted = stack.format_stack_trace("Empty Stack");
        assert!(formatted.contains("Empty Stack"));
        assert!(formatted.contains("(no call stack)"));
    }

    #[test]
    fn test_stack_clear() {
        let mut stack = CallStack::new();
        let frame = create_test_frame("test");

        stack.push_frame(frame).unwrap();
        assert!(!stack.is_empty());

        stack.clear();
        assert!(stack.is_empty());
        assert_eq!(stack.depth(), 0);
    }

    #[test]
    fn test_call_frame_variable_precedence() {
        let mut frame = create_test_frame("test");

        // Parameter "x" should be accessible
        assert_eq!(frame.lookup_variable("x"), Some(&Value::integer(42)));

        // Set a local variable with the same name
        frame.set_local_variable("x".to_string(), Value::string("local".to_string()));

        // Local should take precedence over parameter
        assert_eq!(
            frame.lookup_variable("x"),
            Some(&Value::string("local".to_string()))
        );
    }

    #[test]
    fn test_call_frame_function_names() {
        let simple_path = TypedFunctionPath::Simple {
            name: "simple".to_string(),
        };
        let qualified_path = TypedFunctionPath::Qualified {
            module: "Module".to_string(),
            name: "qualified".to_string(),
        };
        let expression_path = TypedFunctionPath::Expression {
            expression: Box::new(outrun_typechecker::checker::TypedExpression {
                kind: outrun_typechecker::checker::TypedExpressionKind::Integer(42),
                structured_type: None,
                span: test_span(),
                debug_info: None,
            }),
        };

        let frame1 = CallFrame::new(simple_path, HashMap::new(), None, test_span(), None, None);
        let frame2 = CallFrame::new(
            qualified_path,
            HashMap::new(),
            None,
            test_span(),
            None,
            None,
        );
        let frame3 = CallFrame::new(
            expression_path,
            HashMap::new(),
            None,
            test_span(),
            None,
            None,
        );

        assert_eq!(frame1.function_name(), "simple");
        assert_eq!(frame2.function_name(), "Module.qualified");
        assert_eq!(frame3.function_name(), "<dynamic function>");
    }

    #[test]
    fn test_tail_call_optimization_detection() {
        let mut stack = CallStack::new();
        let factorial_path = TypedFunctionPath::Simple {
            name: "factorial".to_string(),
        };
        let fibonacci_path = TypedFunctionPath::Simple {
            name: "fibonacci".to_string(),
        };

        // Empty stack - can't optimize
        assert!(!stack.can_tail_call_optimize(&factorial_path));

        // Push factorial frame
        let frame = CallFrame::new(
            factorial_path.clone(),
            HashMap::new(),
            None,
            test_span(),
            None,
            None,
        );
        stack.push_frame(frame).unwrap();

        // Same function - can optimize
        assert!(stack.can_tail_call_optimize(&factorial_path));

        // Different function - can't optimize
        assert!(!stack.can_tail_call_optimize(&fibonacci_path));
    }

    #[test]
    fn test_tail_call_optimization_execution() {
        let mut stack = CallStack::new();
        let factorial_path = TypedFunctionPath::Simple {
            name: "factorial".to_string(),
        };

        // Create initial frame
        let mut initial_params = HashMap::new();
        initial_params.insert("n".to_string(), Value::integer(5));
        let frame = CallFrame::new(
            factorial_path.clone(),
            initial_params,
            None,
            test_span(),
            None,
            None,
        );
        stack.push_frame(frame).unwrap();

        // Add some local variables
        stack
            .set_local_variable("acc".to_string(), Value::integer(1), test_span())
            .unwrap();

        // Verify initial state
        assert_eq!(stack.depth(), 1);
        assert_eq!(stack.current_frame().unwrap().tail_call_count, 0);
        assert_eq!(
            stack.lookup_variable("n", test_span()).unwrap(),
            &Value::integer(5)
        );
        assert_eq!(
            stack.lookup_variable("acc", test_span()).unwrap(),
            &Value::integer(1)
        );

        // Perform tail call optimization
        let mut new_params = HashMap::new();
        new_params.insert("n".to_string(), Value::integer(4));
        stack.tail_call_optimize(new_params, test_span()).unwrap();

        // Verify optimization worked
        assert_eq!(stack.depth(), 1); // Same depth
        assert_eq!(stack.current_frame().unwrap().tail_call_count, 1); // Optimization counter incremented
        assert_eq!(
            stack.lookup_variable("n", test_span()).unwrap(),
            &Value::integer(4)
        ); // New parameter

        // Local variables should be cleared
        let acc_lookup = stack.lookup_variable("acc", test_span());
        assert!(acc_lookup.is_err());
    }

    #[test]
    fn test_push_frame_or_tail_call() {
        let mut stack = CallStack::new();
        let factorial_path = TypedFunctionPath::Simple {
            name: "factorial".to_string(),
        };

        // Initial call - should push normally
        let mut params1 = HashMap::new();
        params1.insert("n".to_string(), Value::integer(5));
        let frame1 = CallFrame::new(
            factorial_path.clone(),
            params1,
            None,
            test_span(),
            None,
            None,
        );
        stack.push_frame_or_tail_call(frame1, false).unwrap();

        assert_eq!(stack.depth(), 1);
        assert_eq!(stack.current_frame().unwrap().tail_call_count, 0);

        // Recursive call, not in tail position - should push normally
        let mut params2 = HashMap::new();
        params2.insert("n".to_string(), Value::integer(4));
        let frame2 = CallFrame::new(
            factorial_path.clone(),
            params2,
            None,
            test_span(),
            None,
            None,
        );
        stack.push_frame_or_tail_call(frame2, false).unwrap();

        assert_eq!(stack.depth(), 2);

        // Recursive call in tail position - should optimize
        let mut params3 = HashMap::new();
        params3.insert("n".to_string(), Value::integer(3));
        let frame3 = CallFrame::new(
            factorial_path.clone(),
            params3,
            None,
            test_span(),
            None,
            None,
        );
        stack.push_frame_or_tail_call(frame3, true).unwrap();

        assert_eq!(stack.depth(), 2); // Same depth due to optimization
        assert_eq!(stack.current_frame().unwrap().tail_call_count, 1);
        assert_eq!(
            stack.lookup_variable("n", test_span()).unwrap(),
            &Value::integer(3)
        );
    }

    #[test]
    fn test_multiple_tail_call_optimizations() {
        let mut stack = CallStack::new();
        let factorial_path = TypedFunctionPath::Simple {
            name: "factorial".to_string(),
        };

        // Initial frame
        let mut params = HashMap::new();
        params.insert("n".to_string(), Value::integer(10));
        let frame = CallFrame::new(factorial_path, params, None, test_span(), None, None);
        stack.push_frame(frame).unwrap();

        // Perform multiple tail call optimizations
        for i in (1..10).rev() {
            let mut new_params = HashMap::new();
            new_params.insert("n".to_string(), Value::integer(i));
            stack.tail_call_optimize(new_params, test_span()).unwrap();
        }

        // Verify multiple optimizations
        assert_eq!(stack.depth(), 1); // Still only one frame
        assert_eq!(stack.current_frame().unwrap().tail_call_count, 9); // 9 optimizations
        assert_eq!(stack.total_tail_calls_optimized(), 9);
        assert_eq!(
            stack.lookup_variable("n", test_span()).unwrap(),
            &Value::integer(1)
        );
    }

    #[test]
    fn test_tail_call_stack_trace() {
        let mut stack = CallStack::new();
        let factorial_path = TypedFunctionPath::Simple {
            name: "factorial".to_string(),
        };

        // Create frame with tail calls
        let frame = CallFrame::new(
            factorial_path,
            HashMap::new(),
            None,
            test_span(),
            None,
            None,
        );
        stack.push_frame(frame).unwrap();

        // Perform some tail call optimizations
        for _ in 0..5 {
            stack
                .tail_call_optimize(HashMap::new(), test_span())
                .unwrap();
        }

        // Check stack trace includes tail call information
        let trace = stack.stack_trace();
        assert_eq!(trace.len(), 1);
        assert_eq!(trace[0].tail_call_count, 5);

        // Check formatted stack trace
        let formatted = stack.format_stack_trace("Recursive Function");
        assert!(formatted.contains("tail call optimization"));
        assert!(formatted.contains("5 calls optimized"));
        assert!(formatted.contains("[+5 tail calls]"));
    }

    #[test]
    fn test_tail_call_error_cases() {
        let mut stack = CallStack::new();

        // Try to optimize with no current frame
        let result = stack.tail_call_optimize(HashMap::new(), test_span());
        assert!(matches!(
            result,
            Err(CallStackError::TailCallOptimizationFailed { .. })
        ));
    }

    #[test]
    fn test_call_frame_reuse_for_tail_call() {
        let factorial_path = TypedFunctionPath::Simple {
            name: "factorial".to_string(),
        };
        let mut frame = CallFrame::new(
            factorial_path,
            HashMap::new(),
            None,
            test_span(),
            None,
            None,
        );

        // Add some locals
        frame.set_local_variable("temp".to_string(), Value::string("test".to_string()));
        assert_eq!(frame.locals.len(), 1);
        assert_eq!(frame.tail_call_count, 0);

        // Reuse for tail call
        let mut new_params = HashMap::new();
        new_params.insert("n".to_string(), Value::integer(42));
        frame.reuse_for_tail_call(new_params, Span::new(10, 20));

        // Verify reuse worked correctly
        assert_eq!(frame.parameters.len(), 1);
        assert_eq!(frame.parameters.get("n"), Some(&Value::integer(42)));
        assert_eq!(frame.locals.len(), 0); // Locals cleared
        assert_eq!(frame.tail_call_count, 1); // Counter incremented
        assert_eq!(frame.call_site.start, 10); // Call site updated
    }
}
