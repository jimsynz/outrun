//! Type inference context and engine
//!
//! TODO: Implement in future tasks

use crate::error::TypecheckError;
use outrun_parser::{Item, Program};

/// Placeholder for inference context
pub struct InferenceContext;

impl InferenceContext {
    pub fn new() -> Self {
        Self
    }
}

impl Default for InferenceContext {
    fn default() -> Self {
        Self::new()
    }
}

impl InferenceContext {
    #[allow(clippy::result_large_err)]
    pub fn collect_definitions(&mut self, _program: &Program) -> Result<(), TypecheckError> {
        todo!("Implement in task #1328")
    }

    #[allow(clippy::result_large_err)]
    pub fn register_implementations(&mut self, _program: &Program) -> Result<(), TypecheckError> {
        todo!("Implement in task #1325")
    }

    #[allow(clippy::result_large_err)]
    pub fn typecheck_program(&mut self, _program: &mut Program) -> Result<(), TypecheckError> {
        todo!("Implement in task #1322")
    }

    #[allow(clippy::result_large_err)]
    pub fn typecheck_item(&mut self, _item: &mut Item) -> Result<(), TypecheckError> {
        todo!("Implement in task #1322")
    }
}
