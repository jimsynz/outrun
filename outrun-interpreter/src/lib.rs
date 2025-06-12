//! # Outrun Interpreter
//!
//! Tree-walking interpreter for the Outrun programming language.
//!
//! This crate provides:
//! - Runtime value representation for all Outrun types
//! - Variable environment and scope management  
//! - Expression evaluation engine
//! - Built-in function registry
//! - Runtime error handling with beautiful diagnostics
//!
//! ## Example
//!
//! ```rust
//! use outrun_interpreter::Interpreter;
//! use outrun_parser::parse_expression;
//!
//! let mut interpreter = Interpreter::new();
//! let expr = parse_expression("42").unwrap();
//! let result = interpreter.eval_expression(&expr).unwrap();
//!
//! println!("Result: {}", result); // Result: 42
//! ```

pub mod builtins;
pub mod environment;
pub mod error;
pub mod interpreter;
pub mod value;

// Re-export core types for convenience
pub use builtins::*;
pub use environment::*;
pub use error::*;
pub use interpreter::*;
pub use value::*;

// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const NAME: &str = env!("CARGO_PKG_NAME");
