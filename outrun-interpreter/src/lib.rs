// AtomId and TypeNameId are safe as HashMap keys despite containing Arc<RwLock<_>>
// because their Hash and Eq implementations only use the symbol field, not the interner
#![allow(clippy::mutable_key_type)]

pub mod call_stack;
pub mod context;
pub mod evaluator;
pub mod function_call_context;
pub mod function_dispatch;
pub mod function_executor;
pub mod intrinsics;
pub mod list;
pub mod patterns;
pub mod runtime_assertions;
pub mod test_harness;
pub mod type_extraction;
pub mod types;
pub mod value;

#[cfg(test)]
mod tests;

pub use call_stack::{CallFrame, CallStack, CallStackError, StackTraceEntry};
pub use context::{ContextError, InterpreterContext, ScopeType};
pub use evaluator::{EvaluationError, ExpressionEvaluator};
pub use function_call_context::{FunctionCallContext, IntrinsicExecutionContext};
pub use function_dispatch::{DispatchError, FunctionDispatcher};
pub use function_executor::{FunctionExecutionError, FunctionExecutor};
pub use intrinsics::{IntrinsicError, IntrinsicsHandler};
pub use list::List;
pub use patterns::{MatchResult, PatternMatchError, PatternMatcher, PatternMatchingUtils};
pub use runtime_assertions::{AssertionContext, AssertionError, RuntimeAssertions};
pub use test_harness::{OutrunTestHarness, TestHarnessError};
pub use type_extraction::{FunctionReturnInfo, TypeExtractor};
pub use types::{TypeIntegration, TypeIntegrationError};
pub use value::Value;
