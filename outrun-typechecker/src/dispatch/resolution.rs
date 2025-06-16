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
    table: &DispatchTable,
    trait_id: TraitId,
    type_id: TypeId,
    _function_name: AtomId,
    span: Span,
) -> TypeResult<FunctionId> {
    // Look up (trait_id, type_id) in dispatch table
    match table.lookup_trait_impl(trait_id, type_id) {
        Some(module_id) => {
            // Found the implementation module
            // TODO: The interpreter will use module_id to look up the actual function
            // For now, we return a synthetic function ID based on the module
            Ok(FunctionId(module_id.0))
        }
        None => Err(TypeError::TraitNotImplemented {
            trait_name: format!("trait_{:?}", trait_id),
            type_name: format!("type_{:?}", type_id),
            span: crate::error::span_to_source_span(span),
        }),
    }
}

/// Resolve a static function call to its implementation
pub fn resolve_static_function(
    table: &DispatchTable,
    type_id: TypeId,
    function_name: String,
    span: Span,
) -> TypeResult<FunctionId> {
    // Look up type_id in static function table
    match table.lookup_static_function(type_id, &function_name) {
        Some(function_id) => {
            // Found the static function implementation
            Ok(function_id)
        }
        None => Err(TypeError::UndefinedFunction {
            name: function_name,
            span: crate::error::span_to_source_span(span),
        }),
    }
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
