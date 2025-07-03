//! Constraint solving result cache
//!
//! This module implements caching for SMT constraint solving results to improve
//! performance by avoiding redundant solver invocations.

use crate::compilation::compiler_environment::TypeNameId;
use crate::smt::constraints::SMTConstraint;
use crate::smt::solver::SolverResult;
use crate::unification::StructuredType;
use lru::LruCache;
use std::hash::{Hash, Hasher};
use std::num::NonZeroUsize;

/// Cache for SMT constraint solving results
pub struct ConstraintCache {
    /// LRU cache for solved constraint sets
    solved_constraints: LruCache<ConstraintSetHash, SolverResult>,

    /// LRU cache for type hierarchies (which types implement which traits)
    type_hierarchies: LruCache<TypeNameId, Vec<TypeNameId>>,

    /// LRU cache for trait implementation lookups
    implementation_cache: LruCache<(TypeNameId, TypeNameId), bool>,

    /// LRU cache for Self type resolution based on function signatures
    self_resolution_cache: LruCache<SelfResolutionKey, StructuredType>,

    /// Statistics for cache performance
    stats: CacheStats,
}

/// Hash wrapper for constraint sets to enable caching
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstraintSetHash {
    /// Hash of the constraint contents
    content_hash: u64,
    /// Number of constraints (for quick differentiation)
    constraint_count: usize,
}

/// Key for Self type resolution caching
/// This normalizes function calls to avoid unique Self_call_X variables
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SelfResolutionKey {
    /// The trait being called
    pub trait_type: TypeNameId,
    /// The function name being called
    pub function_name: String,
    /// The argument types (normalized, no Self variables)
    pub argument_types: Vec<StructuredType>,
}

/// Cache performance statistics
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    pub cache_hits: usize,
    pub cache_misses: usize,
    pub evictions: usize,
    pub total_queries: usize,
}

impl ConstraintCache {
    /// Create a new constraint cache with default settings
    pub fn new() -> Self {
        Self::with_capacity(1000) // Default capacity
    }

    /// Create a new constraint cache with specified capacity
    pub fn with_capacity(max_size: usize) -> Self {
        let capacity = NonZeroUsize::new(max_size).unwrap_or(NonZeroUsize::new(1000).unwrap());
        Self {
            solved_constraints: LruCache::new(capacity),
            type_hierarchies: LruCache::new(capacity),
            implementation_cache: LruCache::new(capacity),
            self_resolution_cache: LruCache::new(capacity),
            stats: CacheStats::default(),
        }
    }

    /// Get a cached solution for a set of constraints
    pub fn get_cached_solution(&mut self, constraints: &[SMTConstraint]) -> Option<&SolverResult> {
        let hash = self.compute_constraint_hash(constraints);
        self.stats.total_queries += 1;

        if let Some(result) = self.solved_constraints.get(&hash) {
            self.stats.cache_hits += 1;
            Some(result)
        } else {
            self.stats.cache_misses += 1;
            None
        }
    }

    /// Get a cached solution without updating statistics (async-safe)
    /// Note: This doesn't update LRU order to maintain immutability
    pub fn get_cached_solution_readonly(&self, constraints: &[SMTConstraint]) -> Option<SolverResult> {
        let hash = self.compute_constraint_hash(constraints);
        // Use peek instead of get to avoid LRU updates (immutable access)
        self.solved_constraints.peek(&hash).cloned()
    }

    /// Cache a constraint solving result
    pub fn cache_solution(&mut self, constraints: Vec<SMTConstraint>, result: SolverResult) {
        let hash = self.compute_constraint_hash(&constraints);

        // LRU cache handles eviction automatically
        if let Some(_evicted) = self.solved_constraints.push(hash, result) {
            self.stats.evictions += 1;
        }
    }

    /// Get cached Self type resolution result
    pub fn get_cached_self_resolution(&mut self, key: &SelfResolutionKey) -> Option<&StructuredType> {
        self.stats.total_queries += 1;
        
        if let Some(result) = self.self_resolution_cache.get(key) {
            self.stats.cache_hits += 1;
            Some(result)
        } else {
            self.stats.cache_misses += 1;
            None
        }
    }

    /// Get cached Self type resolution result without updating statistics (async-safe)
    /// Note: This doesn't update LRU order to maintain immutability
    pub fn get_cached_self_resolution_readonly(&self, key: &SelfResolutionKey) -> Option<StructuredType> {
        // Use peek instead of get to avoid LRU updates (immutable access)
        self.self_resolution_cache.peek(key).cloned()
    }

    /// Cache a Self type resolution result
    pub fn cache_self_resolution(&mut self, key: SelfResolutionKey, resolved_type: StructuredType) {
        if let Some(_evicted) = self.self_resolution_cache.push(key, resolved_type) {
            self.stats.evictions += 1;
        }
    }

    /// Invalidate cache entries related to a specific type
    pub fn invalidate_type_cache(&mut self, type_id: TypeNameId) {
        // Remove type hierarchy entries
        self.type_hierarchies.pop(&type_id);

        // Remove implementation cache entries involving this type
        let keys_to_remove: Vec<_> = self
            .implementation_cache
            .iter()
            .filter(|(key, _)| key.0 == type_id || key.1 == type_id)
            .map(|(key, _)| key.clone())
            .collect();

        for key in keys_to_remove {
            self.implementation_cache.pop(&key);
        }

        // For constraint cache, we could be more aggressive and clear all entries
        // since type relationships might have changed, but for now we'll leave them
        // TODO: Implement more fine-grained constraint cache invalidation
    }

    /// Cache a type hierarchy (which types implement a trait)
    pub fn cache_type_hierarchy(
        &mut self,
        trait_id: TypeNameId,
        implementing_types: Vec<TypeNameId>,
    ) {
        self.type_hierarchies.push(trait_id, implementing_types);
    }

    /// Get cached type hierarchy
    pub fn get_type_hierarchy(&mut self, trait_id: &TypeNameId) -> Option<&Vec<TypeNameId>> {
        self.type_hierarchies.get(trait_id)
    }

    /// Cache a trait implementation check result
    pub fn cache_implementation(
        &mut self,
        trait_id: TypeNameId,
        impl_id: TypeNameId,
        implements: bool,
    ) {
        self.implementation_cache
            .push((trait_id, impl_id), implements);
    }

    /// Get cached implementation check result
    pub fn get_implementation(
        &mut self,
        trait_id: &TypeNameId,
        impl_id: &TypeNameId,
    ) -> Option<bool> {
        self.implementation_cache
            .get(&(trait_id.clone(), impl_id.clone()))
            .copied()
    }

    /// Clear all cached entries
    pub fn clear(&mut self) {
        self.solved_constraints.clear();
        self.type_hierarchies.clear();
        self.implementation_cache.clear();
        self.self_resolution_cache.clear();
        self.stats = CacheStats::default();
    }

    /// Get cache statistics
    pub fn get_stats(&self) -> &CacheStats {
        &self.stats
    }

    /// Get cache hit ratio
    pub fn get_hit_ratio(&self) -> f64 {
        if self.stats.total_queries == 0 {
            0.0
        } else {
            self.stats.cache_hits as f64 / self.stats.total_queries as f64
        }
    }

    /// Get current cache size
    pub fn size(&self) -> usize {
        self.solved_constraints.len()
    }

    /// Check if cache is empty
    pub fn is_empty(&self) -> bool {
        self.solved_constraints.is_empty()
            && self.type_hierarchies.is_empty()
            && self.implementation_cache.is_empty()
            && self.self_resolution_cache.is_empty()
    }

    /// Compute hash for a set of constraints
    fn compute_constraint_hash(&self, constraints: &[SMTConstraint]) -> ConstraintSetHash {
        // TODO: Re-enable constraint normalization after async implementation
        // For now, use simple hashing to avoid semantic interference
        
        let mut hasher = std::collections::hash_map::DefaultHasher::new();

        // Sort constraints by their hash to ensure consistent ordering
        let mut constraint_hashes: Vec<u64> = constraints
            .iter()
            .map(|constraint| {
                let mut constraint_hasher = std::collections::hash_map::DefaultHasher::new();
                constraint.hash(&mut constraint_hasher);
                constraint_hasher.finish()
            })
            .collect();
        constraint_hashes.sort();

        // Hash the sorted constraint hashes
        for constraint_hash in constraint_hashes {
            constraint_hash.hash(&mut hasher);
        }

        ConstraintSetHash {
            content_hash: hasher.finish(),
            constraint_count: constraints.len(),
        }
    }
    
    /// Normalize and deduplicate constraints to improve cache hit ratios
    /// This removes duplicate constraints and normalizes variable names for better caching
    fn normalize_and_deduplicate_constraints(&self, constraints: &[SMTConstraint]) -> Vec<SMTConstraint> {
        use std::collections::HashSet;
        
        // Step 1: Deduplicate identical constraints
        let mut unique_constraints = HashSet::new();
        let mut deduplicated = Vec::new();
        
        for constraint in constraints {
            // Normalize Self variable names to improve cache hits
            let normalized_constraint = self.normalize_constraint_variables(constraint);
            
            if unique_constraints.insert(normalized_constraint.clone()) {
                deduplicated.push(normalized_constraint);
            }
        }
        
        // Step 2: Sort for consistent ordering (improves cache hit ratio)
        deduplicated.sort_by_key(|constraint| {
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            constraint.hash(&mut hasher);
            hasher.finish()
        });
        
        deduplicated
    }
    
    /// Normalize constraint variable names to improve cache hit ratios
    /// This converts unique Self_call_X variables to canonical names where possible
    fn normalize_constraint_variables(&self, constraint: &SMTConstraint) -> SMTConstraint {
        // For now, implement basic normalization - this can be extended
        match constraint {
            SMTConstraint::SelfTypeInference { 
                self_variable_id: _, 
                inferred_type, 
                call_site_context: _, 
                confidence 
            } => {
                // Normalize Self variable names based on the inferred type
                // This allows multiple calls with the same inferred type to share cache entries
                let normalized_var_name = format!("Self_normalized_{}", 
                    self.normalize_type_for_caching(inferred_type));
                let normalized_var_id = self.create_normalized_type_id(&normalized_var_name);
                
                SMTConstraint::SelfTypeInference {
                    self_variable_id: normalized_var_id,
                    inferred_type: inferred_type.clone(),
                    call_site_context: "normalized_call_site".to_string(), // Normalize context for caching
                    confidence: confidence.clone(),
                }
            }
            SMTConstraint::ConcreteSelfBinding {
                self_variable_id: _,
                concrete_type,
                context: _
            } => {
                // Normalize concrete Self bindings
                let normalized_var_name = format!("Self_concrete_{}", 
                    self.normalize_type_for_caching(concrete_type));
                let normalized_var_id = self.create_normalized_type_id(&normalized_var_name);
                
                SMTConstraint::ConcreteSelfBinding {
                    self_variable_id: normalized_var_id,
                    concrete_type: concrete_type.clone(),
                    context: "normalized_context".to_string(),
                }
            }
            // For other constraint types, return as-is for now
            // This can be extended to normalize more constraint types
            _ => constraint.clone(),
        }
    }
    
    /// Create a normalized type string for caching purposes
    fn normalize_type_for_caching(&self, structured_type: &StructuredType) -> String {
        match structured_type {
            StructuredType::Simple(type_id) => format!("Simple_{}", type_id.hash),
            StructuredType::Generic { base, args } => {
                let arg_strs: Vec<String> = args.iter()
                    .map(|arg| self.normalize_type_for_caching(arg))
                    .collect();
                format!("Generic_{}_{}", base.hash, arg_strs.join("_"))
            }
            StructuredType::Tuple(types) => {
                let type_strs: Vec<String> = types.iter()
                    .map(|t| self.normalize_type_for_caching(t))
                    .collect();
                format!("Tuple_{}", type_strs.join("_"))
            }
            StructuredType::Function { params, return_type } => {
                let param_strs: Vec<String> = params.iter()
                    .map(|p| self.normalize_type_for_caching(&p.param_type))
                    .collect();
                format!("Function_{}_to_{}", 
                    param_strs.join("_"), 
                    self.normalize_type_for_caching(return_type))
            }
            StructuredType::TypeVariable(var_id) => format!("TypeVar_{}", var_id.hash),
            StructuredType::Integer64 => "Integer64".to_string(),
            StructuredType::Float64 => "Float64".to_string(),
            StructuredType::Boolean => "Boolean".to_string(),
            StructuredType::String => "String".to_string(),
            StructuredType::Atom => "Atom".to_string(),
            StructuredType::List { element_type } => {
                format!("List_{}", self.normalize_type_for_caching(element_type))
            }
            StructuredType::Map { key_type, value_type } => {
                format!("Map_{}_{}", 
                    self.normalize_type_for_caching(key_type),
                    self.normalize_type_for_caching(value_type))
            }
            StructuredType::Option { inner_type } => {
                format!("Option_{}", self.normalize_type_for_caching(inner_type))
            }
            StructuredType::Result { ok_type, err_type } => {
                format!("Result_{}_{}", 
                    self.normalize_type_for_caching(ok_type),
                    self.normalize_type_for_caching(err_type))
            }
            StructuredType::Struct { name, .. } => {
                format!("Struct_{}", name.hash)
            }
            StructuredType::Trait { name, .. } => {
                format!("Trait_{}", name.hash)
            }
            StructuredType::TypeError { .. } => "TypeError".to_string(),
        }
    }
    
    /// Create a normalized TypeNameId for caching
    fn create_normalized_type_id(&self, name: &str) -> TypeNameId {
        // Create a hash-based ID for the normalized name
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        name.hash(&mut hasher);
        let hash = hasher.finish();
        
        // Use a dummy storage since this is just for caching normalization
        use std::sync::{Arc, RwLock};
        let dummy_storage = Arc::new(RwLock::new(std::collections::HashMap::new()));
        TypeNameId::new(hash, dummy_storage)
    }
}

impl Default for ConstraintCache {
    fn default() -> Self {
        Self::new()
    }
}

impl CacheStats {
    /// Create new empty cache statistics
    pub fn new() -> Self {
        Self::default()
    }

    /// Reset all statistics to zero
    pub fn reset(&mut self) {
        *self = Self::default();
    }

    /// Get total number of cache operations
    pub fn total_operations(&self) -> usize {
        self.cache_hits + self.cache_misses
    }

    /// Check if any operations have been performed
    pub fn has_activity(&self) -> bool {
        self.total_queries > 0
    }
}

impl std::fmt::Display for CacheStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cache Stats: {} hits, {} misses, {} evictions, {:.2}% hit ratio",
            self.cache_hits,
            self.cache_misses,
            self.evictions,
            if self.total_queries > 0 {
                (self.cache_hits as f64 / self.total_queries as f64) * 100.0
            } else {
                0.0
            }
        )
    }
}
