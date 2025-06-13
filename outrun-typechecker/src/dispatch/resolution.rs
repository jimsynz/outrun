//! Function resolution logic
//!
//! This module handles resolving function calls to their correct implementations,
//! whether they are static module functions or trait method calls.

use crate::dispatch::DispatchTable;
use crate::error::{TypeError, TypeResult};
use crate::types::traits::FunctionId;
use crate::types::{AtomId, TraitId, TypeId};
use outrun_parser::Span;

/// Resolve a trait function call to its implementation
pub fn resolve_trait_function(
    _table: &DispatchTable,
    _trait_id: TraitId,
    _type_id: TypeId,
    _function_name: AtomId,
    span: Span,
) -> TypeResult<FunctionId> {
    // TODO: Implement trait function resolution
    // - Look up (trait_id, type_id) in dispatch table
    // - Find function within the trait implementation
    // - Return function ID for execution
    Err(TypeError::UnimplementedFeature {
        feature: "Trait function resolution".to_string(),
        span: crate::error::span_to_source_span(span),
    })
}

/// Resolve a static function call to its implementation
pub fn resolve_static_function(
    _table: &DispatchTable,
    _type_id: TypeId,
    _function_name: String,
    span: Span,
) -> TypeResult<FunctionId> {
    // TODO: Implement static function resolution
    // - Look up type_id in static function table
    // - Find function by name
    // - Return function ID for execution
    Err(TypeError::UnimplementedFeature {
        feature: "Static function resolution".to_string(),
        span: crate::error::span_to_source_span(span),
    })
}

/// Function resolution context for complex lookups
pub struct ResolutionContext<'a> {
    pub dispatch_table: &'a DispatchTable,
    pub current_scope_types: Vec<TypeId>, // For generic resolution
}

impl<'a> ResolutionContext<'a> {
    /// Create a new resolution context
    pub fn new(dispatch_table: &'a DispatchTable) -> Self {
        Self {
            dispatch_table,
            current_scope_types: Vec::new(),
        }
    }

    /// Resolve function call with full context
    pub fn resolve_function_call(
        &self,
        _call: &outrun_parser::FunctionCall,
    ) -> TypeResult<FunctionId> {
        // TODO: Implement context-aware function resolution
        // - Determine if static or trait call
        // - Handle generic type resolution
        // - Apply overload resolution if needed
        Err(TypeError::UnimplementedFeature {
            feature: "Context-aware function resolution".to_string(),
            span: crate::error::span_to_source_span(outrun_parser::Span::new(0, 0)),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolution_context_creation() {
        let table = DispatchTable::new();
        let context = ResolutionContext::new(&table);

        assert_eq!(context.current_scope_types.len(), 0);
    }
}
