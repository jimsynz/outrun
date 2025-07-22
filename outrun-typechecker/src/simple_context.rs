//! Simplified context structures to replace complex nested contexts
//!
//! This module provides simplified data structures that can be passed alongside
//! CompilerEnvironment to replace the complex nested context hierarchies.

use crate::compilation::compiler_environment::TypeNameId;
use crate::unification::StructuredType;
use outrun_parser::{ProtocolDefinition, StructDefinition};
use std::collections::HashMap;

/// Simplified type checking data that complements CompilerEnvironment
///
/// Instead of complex nested contexts, this simple struct contains additional
/// data needed for type checking operations alongside the core CompilerEnvironment.
#[derive(Debug, Clone)]
pub struct TypeCheckingData {
    /// Struct definitions indexed by TypeNameId for type checking
    pub structs: HashMap<TypeNameId, StructDefinition>,

    /// Protocol definitions indexed by TypeNameId for protocol resolution
    pub protocols: HashMap<TypeNameId, ProtocolDefinition>,

    /// Generic parameter substitutions for the current context
    pub generic_substitutions: HashMap<TypeNameId, StructuredType>,

    /// Resolved expression types from type checking phase
    pub expression_types: HashMap<outrun_parser::Span, StructuredType>,

    /// Span mapping from desugaring phase for tracking original to desugared spans
    pub span_mapping: crate::desugaring::SpanMapping,
}

impl TypeCheckingData {
    /// Create a new empty type checking data structure
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            protocols: HashMap::new(),
            generic_substitutions: HashMap::new(),
            expression_types: HashMap::new(),
            span_mapping: crate::desugaring::SpanMapping::new(),
        }
    }

    /// Add a struct definition to the context
    pub fn add_struct(&mut self, type_id: TypeNameId, struct_def: StructDefinition) {
        self.structs.insert(type_id, struct_def);
    }

    /// Add a protocol definition to the context
    pub fn add_protocol(&mut self, type_id: TypeNameId, protocol_def: ProtocolDefinition) {
        self.protocols.insert(type_id, protocol_def);
    }

    /// Look up a struct definition by TypeNameId
    pub fn get_struct(&self, type_id: &TypeNameId) -> Option<&StructDefinition> {
        self.structs.get(type_id)
    }

    /// Look up a protocol definition by TypeNameId
    pub fn get_protocol(&self, type_id: &TypeNameId) -> Option<&ProtocolDefinition> {
        self.protocols.get(type_id)
    }

    /// Add a generic parameter substitution
    pub fn add_generic_substitution(&mut self, param: TypeNameId, substitution: StructuredType) {
        self.generic_substitutions.insert(param, substitution);
    }

    /// Set expression type for a span
    pub fn set_expression_type(&mut self, span: outrun_parser::Span, expr_type: StructuredType) {
        self.expression_types.insert(span, expr_type);
    }

    /// Get expression type for a span
    pub fn get_expression_type(&self, span: &outrun_parser::Span) -> Option<&StructuredType> {
        self.expression_types.get(span)
    }
}

impl Default for TypeCheckingData {
    fn default() -> Self {
        Self::new()
    }
}

/// Compilation phase data for multi-program compilation
///
/// This contains additional information needed during compilation phases
/// alongside CompilerEnvironment and TypeCheckingData.
#[derive(Debug, Clone)]
pub struct CompilationPhaseData {
    /// Compilation order for dependency resolution
    pub compilation_order: Vec<String>,

    /// External variables available in this compilation phase
    pub external_variables: HashMap<String, StructuredType>,

    /// Implementation blocks extracted during compilation
    pub implementations: Vec<outrun_parser::ImplBlock>,
}

impl CompilationPhaseData {
    /// Create new compilation phase data
    pub fn new(
        compilation_order: Vec<String>,
        external_variables: HashMap<String, StructuredType>,
        implementations: Vec<outrun_parser::ImplBlock>,
    ) -> Self {
        Self {
            compilation_order,
            external_variables,
            implementations,
        }
    }

    /// Create empty compilation phase data
    pub fn empty() -> Self {
        Self {
            compilation_order: Vec::new(),
            external_variables: HashMap::new(),
            implementations: Vec::new(),
        }
    }
}

impl Default for CompilationPhaseData {
    fn default() -> Self {
        Self::empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_checking_data_creation() {
        let data = TypeCheckingData::new();

        // Verify data is properly initialized
        assert!(data.structs.is_empty());
        assert!(data.protocols.is_empty());
        assert!(data.generic_substitutions.is_empty());
        assert!(data.expression_types.is_empty());
    }

    #[test]
    fn test_compilation_phase_data_creation() {
        let data = CompilationPhaseData::new(vec!["program1".to_string()], HashMap::new(), vec![]);

        assert_eq!(data.compilation_order.len(), 1);
        assert!(data.external_variables.is_empty());
        assert!(data.implementations.is_empty());
    }
}
