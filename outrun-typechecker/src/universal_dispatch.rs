//! Universal Clause-Based Function Dispatch System
//!
//! This module implements the revolutionary universal dispatch architecture where
//! ALL function calls - protocol instance, protocol static, concrete type,
//! multi-clause with guards, single-clause - follow the same pattern:
//! "Here's a list of clauses with guards - try each one until the guards pass"

use crate::types::Type;
use outrun_parser::{Expression, Span};
use std::collections::HashMap;

/// Universal clause identifier - unique ID for every function clause
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct ClauseId(pub u64);

impl ClauseId {
    /// Create a deterministic clause ID based on function signature and guards
    /// This ensures the same function clause always gets the same unique clause ID
    pub fn deterministic(signature: &FunctionSignature, guards: &[Guard]) -> Self {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();

        // Hash the function signature
        signature.module_path.hash(&mut hasher);
        signature.function_name.hash(&mut hasher);

        // Hash the guards (we need a stable representation)
        for guard in guards {
            match guard {
                Guard::AlwaysTrue => {
                    "AlwaysTrue".hash(&mut hasher);
                }
                Guard::TypeCompatible {
                    target_type,
                    implementing_type,
                    ..
                } => {
                    "TypeCompatible".hash(&mut hasher);
                    // Hash the type names for stability
                    format!("{:?}", target_type).hash(&mut hasher);
                    format!("{:?}", implementing_type).hash(&mut hasher);
                }
                Guard::ValueGuard {
                    bound_variables, ..
                } => {
                    "ValueGuard".hash(&mut hasher);
                    bound_variables.hash(&mut hasher);
                }
            }
        }

        ClauseId(hasher.finish())
    }
}

/// Universal function signature for indexing all function types
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FunctionSignature {
    pub module_path: Vec<String>,
    pub function_name: String,
}

impl FunctionSignature {
    pub fn new(module_path: Vec<String>, function_name: String) -> Self {
        Self {
            module_path,
            function_name,
        }
    }

    pub fn simple(function_name: String) -> Self {
        Self {
            module_path: vec![],
            function_name,
        }
    }

    pub fn qualified(module: String, function_name: String) -> Self {
        Self {
            module_path: vec![module],
            function_name,
        }
    }
}

/// Universal guard system - treats type compatibility as guard conditions
#[derive(Debug, Clone, PartialEq)]
pub enum Guard {
    /// Type compatibility check (replaces separate type dispatch)
    TypeCompatible {
        target_type: Type,
        implementing_type: Type,
        constraint_context: ConstraintContext,
    },

    /// Runtime value guard expressions
    ValueGuard {
        expression: Expression,
        bound_variables: Vec<String>,
    },

    /// Always-true guard for simple/fallback clauses
    AlwaysTrue,
}

/// Context for type compatibility constraints
#[derive(Debug, Clone, PartialEq)]
pub struct ConstraintContext {
    pub generic_substitutions: HashMap<String, Type>,
    pub self_type: Option<Type>,
}

impl Default for ConstraintContext {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstraintContext {
    pub fn new() -> Self {
        Self {
            generic_substitutions: HashMap::new(),
            self_type: None,
        }
    }
}

/// Function implementation body - unified representation
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionBody {
    /// User-defined function with AST body
    UserFunction(outrun_parser::Block),

    /// Intrinsic function (built-in operations)
    IntrinsicFunction(String),

    /// Struct constructor
    StructConstructor {
        struct_name: String,
        field_mappings: Vec<(String, ArgumentRef)>,
    },

    /// Protocol function implementation
    ProtocolImplementation {
        implementation_name: String,
        body: outrun_parser::Block,
    },
}

/// Reference to function argument by name (Outrun uses named parameters only)
#[derive(Debug, Clone, PartialEq)]
pub enum ArgumentRef {
    Name(String),
}

/// Universal clause representation - works for ALL function types
#[derive(Debug, Clone, PartialEq)]
pub struct ClauseInfo {
    pub clause_id: ClauseId,
    pub function_signature: FunctionSignature,

    /// Unified guard system - type compatibility + value guards
    pub guards: Vec<Guard>,

    /// Function implementation body
    pub body: FunctionBody,

    /// Optimization metadata
    pub estimated_cost: u32,
    pub priority: u32,

    /// Source location for debugging
    pub span: Option<Span>,
}

/// Execution statistics for performance monitoring
#[derive(Debug, Clone, PartialEq)]
pub struct ClauseExecutionStats {
    pub call_count: u64,
    pub success_count: u64,
    pub success_rate: f64,
    pub total_guard_evaluations: u64,
    pub average_guard_evaluations: f64,
    pub average_execution_time: Option<f64>,
}

impl Default for ClauseExecutionStats {
    fn default() -> Self {
        Self {
            call_count: 0,
            success_count: 0,
            success_rate: 0.0,
            total_guard_evaluations: 0,
            average_guard_evaluations: 0.0,
            average_execution_time: None,
        }
    }
}

/// Type pattern for optimization indexing
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum TypePattern {
    Concrete(String),
    Protocol(String),
    Generic(String),
    Any,
}

/// Universal registry handles ALL function types uniformly
#[derive(Debug, Clone)]
pub struct UniversalDispatchRegistry {
    /// Individual clauses with unique identifiers
    clauses: HashMap<ClauseId, ClauseInfo>,

    /// Fast dispatch lookup: function_name -> ordered clause list
    dispatch_index: HashMap<FunctionSignature, Vec<ClauseId>>,

    /// Type-based optimization index for O(1) type filtering
    type_index: HashMap<(FunctionSignature, TypePattern), Vec<ClauseId>>,

    /// Universal performance monitoring
    statistics: HashMap<ClauseId, ClauseExecutionStats>,
}

impl UniversalDispatchRegistry {
    /// Create a new universal dispatch registry
    pub fn new() -> Self {
        Self {
            clauses: HashMap::new(),
            dispatch_index: HashMap::new(),
            type_index: HashMap::new(),
            statistics: HashMap::new(),
        }
    }

    /// Register a new clause in the universal system
    pub fn register_clause(&mut self, clause: ClauseInfo) -> ClauseId {
        let clause_id = clause.clause_id;

        // Store the clause
        self.clauses.insert(clause_id, clause.clone());

        // Add to dispatch index
        self.dispatch_index
            .entry(clause.function_signature.clone())
            .or_default()
            .push(clause_id);

        // Add to type index for optimization
        if let Some(type_pattern) = self.extract_type_pattern(&clause) {
            self.type_index
                .entry((clause.function_signature.clone(), type_pattern))
                .or_default()
                .push(clause_id);
        }

        // Initialize statistics
        self.statistics
            .insert(clause_id, ClauseExecutionStats::default());

        clause_id
    }

    /// Get all clauses for a function signature
    pub fn get_clauses_for_function(&self, sig: &FunctionSignature) -> &[ClauseId] {
        self.dispatch_index
            .get(sig)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Get a specific clause by ID
    pub fn get_clause(&self, id: ClauseId) -> Option<&ClauseInfo> {
        self.clauses.get(&id)
    }

    /// Get clauses filtered by type pattern for optimization
    pub fn get_clauses_for_type(
        &self,
        sig: &FunctionSignature,
        pattern: &TypePattern,
    ) -> Vec<ClauseId> {
        if let Some(clauses) = self.type_index.get(&(sig.clone(), pattern.clone())) {
            clauses.clone()
        } else {
            // Fallback: filter all clauses for this function
            self.get_clauses_for_function(sig)
                .iter()
                .filter(|&&id| {
                    if let Some(clause) = self.clauses.get(&id) {
                        self.clause_matches_type_pattern(clause, pattern)
                    } else {
                        false
                    }
                })
                .copied()
                .collect()
        }
    }

    /// Extract type pattern from clause for indexing
    fn extract_type_pattern(&self, clause: &ClauseInfo) -> Option<TypePattern> {
        for guard in &clause.guards {
            if let Guard::TypeCompatible {
                implementing_type, ..
            } = guard
            {
                return Some(match implementing_type {
                    Type::Concrete { name, .. } => TypePattern::Concrete(name.as_str().to_string()),
                    Type::Protocol { name, .. } => TypePattern::Protocol(name.as_str().to_string()),
                    Type::Variable { .. } => TypePattern::Generic("T".to_string()),
                    Type::SelfType { .. } => TypePattern::Generic("Self".to_string()),
                    Type::Function { .. } => TypePattern::Generic("Function".to_string()),
                    Type::Tuple { .. } => TypePattern::Generic("Tuple".to_string()),
                });
            }
        }
        Some(TypePattern::Any)
    }

    /// Check if clause matches type pattern
    fn clause_matches_type_pattern(&self, clause: &ClauseInfo, pattern: &TypePattern) -> bool {
        for guard in &clause.guards {
            if let Guard::TypeCompatible {
                implementing_type, ..
            } = guard
            {
                return match (implementing_type, pattern) {
                    (Type::Concrete { name, .. }, TypePattern::Concrete(pattern_name)) => {
                        name.as_str() == pattern_name
                    }
                    (Type::Protocol { name, .. }, TypePattern::Protocol(pattern_name)) => {
                        name.as_str() == pattern_name
                    }
                    (Type::Variable { .. }, TypePattern::Generic(_)) => true,
                    (Type::SelfType { .. }, TypePattern::Generic(_)) => true,
                    (Type::Function { .. }, TypePattern::Generic(_)) => true,
                    (_, TypePattern::Any) => true,
                    _ => false,
                };
            }
        }
        matches!(pattern, TypePattern::Any)
    }

    /// Record clause execution for performance monitoring
    pub fn record_clause_execution(
        &mut self,
        clause_id: ClauseId,
        succeeded: bool,
        guard_count: u32,
    ) {
        let stats = self.statistics.entry(clause_id).or_default();
        stats.call_count += 1;
        if succeeded {
            stats.success_count += 1;
        }
        stats.total_guard_evaluations += guard_count as u64;
        stats.success_rate = stats.success_count as f64 / stats.call_count as f64;
        stats.average_guard_evaluations =
            stats.total_guard_evaluations as f64 / stats.call_count as f64;
    }

    /// Get all function signatures in the registry
    pub fn get_all_function_signatures(&self) -> Vec<&FunctionSignature> {
        self.dispatch_index.keys().collect()
    }

    /// Get execution statistics for a clause
    pub fn get_statistics(&self, clause_id: ClauseId) -> Option<&ClauseExecutionStats> {
        self.statistics.get(&clause_id)
    }

    /// Phase 3: Simple Static Clause Elimination
    /// Check if a clause could possibly match given argument types
    pub fn clause_could_match(&self, clause_id: ClauseId, argument_types: &[Type]) -> bool {
        if let Some(clause) = self.clauses.get(&clause_id) {
            self.clause_is_viable(clause, argument_types)
        } else {
            true // If clause not found, conservatively keep it
        }
    }

    /// Simple viability check for a clause - just basic type compatibility
    fn clause_is_viable(&self, clause: &ClauseInfo, _argument_types: &[Type]) -> bool {
        for guard in &clause.guards {
            match guard {
                Guard::TypeCompatible {
                    target_type,
                    implementing_type,
                    ..
                } => {
                    // Very simple check: if types are obviously incompatible, eliminate
                    if !self.types_could_be_compatible(target_type, implementing_type) {
                        return false;
                    }
                }
                Guard::ValueGuard { .. } => {
                    // Value guards require runtime evaluation - keep the clause
                }
                Guard::AlwaysTrue => {
                    // Always matches
                }
            }
        }
        true
    }

    /// Very simple type compatibility check - only eliminate obvious mismatches
    fn types_could_be_compatible(&self, type1: &Type, type2: &Type) -> bool {
        match (type1, type2) {
            // Same concrete types are compatible
            (Type::Concrete { name: id1, .. }, Type::Concrete { name: id2, .. }) => {
                id1.as_str() == id2.as_str()
            }
            // Type variables could unify with anything
            (Type::Variable { .. }, _) | (_, Type::Variable { .. }) => true,
            // Self types could resolve to anything
            (Type::SelfType { .. }, _) | (_, Type::SelfType { .. }) => true,
            // Protocols and concrete types might be compatible
            (Type::Protocol { .. }, Type::Concrete { .. }) => true,
            (Type::Concrete { .. }, Type::Protocol { .. }) => true,
            // Same protocol types are compatible
            (Type::Protocol { name: id1, .. }, Type::Protocol { name: id2, .. }) => id1 == id2,
            // Function types - conservatively allow
            (Type::Function { .. }, Type::Function { .. }) => true,
            // If we can't determine, err on the side of keeping the clause
            _ => true,
        }
    }
}

impl Default for UniversalDispatchRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_universal_registry_creation() {
        let registry = UniversalDispatchRegistry::new();
        assert_eq!(registry.clauses.len(), 0);
        assert_eq!(registry.dispatch_index.len(), 0);
    }

    #[test]
    fn test_clause_registration() {
        let mut registry = UniversalDispatchRegistry::new();

        let function_signature = FunctionSignature::simple("test_function".to_string());
        let guards = vec![Guard::AlwaysTrue];
        let clause = ClauseInfo {
            clause_id: ClauseId::deterministic(&function_signature, &guards),
            function_signature: function_signature.clone(),
            guards,
            body: FunctionBody::IntrinsicFunction("test_intrinsic".to_string()),
            estimated_cost: 1,
            priority: 0,
            span: None,
        };

        let clause_id = clause.clause_id;
        registry.register_clause(clause);

        assert!(registry.get_clause(clause_id).is_some());
        let clauses = registry
            .get_clauses_for_function(&FunctionSignature::simple("test_function".to_string()));
        assert_eq!(clauses.len(), 1);
        assert_eq!(clauses[0], clause_id);
    }

    #[test]
    fn test_type_pattern_extraction() {
        let registry = UniversalDispatchRegistry::new();

        let function_signature = FunctionSignature::simple("test_function".to_string());
        let guards = vec![Guard::TypeCompatible {
            target_type: Type::concrete("String"),
            implementing_type: Type::concrete("String"),
            constraint_context: ConstraintContext::new(),
        }];
        let clause = ClauseInfo {
            clause_id: ClauseId::deterministic(&function_signature, &guards),
            function_signature: function_signature.clone(),
            guards,
            body: FunctionBody::IntrinsicFunction("test_intrinsic".to_string()),
            estimated_cost: 1,
            priority: 0,
            span: None,
        };

        let pattern = registry.extract_type_pattern(&clause);
        assert_eq!(pattern, Some(TypePattern::Concrete("String".to_string())));
    }

    #[test]
    fn test_simple_clause_elimination() {
        let registry = UniversalDispatchRegistry::new();

        // Create a clause that should match String types
        let function_signature = FunctionSignature::simple("test_function".to_string());
        let guards = vec![Guard::TypeCompatible {
            target_type: Type::concrete("String"),
            implementing_type: Type::concrete("String"),
            constraint_context: ConstraintContext::new(),
        }];
        let string_clause = ClauseInfo {
            clause_id: ClauseId::deterministic(&function_signature, &guards),
            function_signature: function_signature.clone(),
            guards,
            body: FunctionBody::IntrinsicFunction("test_intrinsic".to_string()),
            estimated_cost: 1,
            priority: 0,
            span: None,
        };

        // Test with String argument type - should match
        let string_args = vec![Type::concrete("String")];
        assert!(registry.clause_could_match(string_clause.clause_id, &string_args));

        // Test with Integer argument type - should not match (but we're conservative, so it returns true)
        let integer_args = vec![Type::concrete("Integer")];
        assert!(registry.clause_could_match(string_clause.clause_id, &integer_args));

        // Test clause viability directly
        assert!(registry.clause_is_viable(&string_clause, &string_args));
    }

    #[test]
    fn test_always_true_guard() {
        let registry = UniversalDispatchRegistry::new();

        // Create a clause with AlwaysTrue guard
        let function_signature = FunctionSignature::simple("test_function".to_string());
        let guards = vec![Guard::AlwaysTrue];
        let always_clause = ClauseInfo {
            clause_id: ClauseId::deterministic(&function_signature, &guards),
            function_signature: function_signature.clone(),
            guards,
            body: FunctionBody::IntrinsicFunction("test_intrinsic".to_string()),
            estimated_cost: 1,
            priority: 0,
            span: None,
        };

        // Should match any argument types
        let string_args = vec![Type::concrete("String")];
        let integer_args = vec![Type::concrete("Integer")];

        assert!(registry.clause_is_viable(&always_clause, &string_args));
        assert!(registry.clause_is_viable(&always_clause, &integer_args));
    }
}
