//! Constraint solving result cache
//!
//! This module implements caching for SMT constraint solving results to improve
//! performance by avoiding redundant solver invocations.

use crate::compilation::compiler_environment::TypeNameId;
use crate::smt::constraints::SMTConstraint;
use crate::smt::solver::SolverResult;
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

    /// Cache a constraint solving result
    pub fn cache_solution(&mut self, constraints: Vec<SMTConstraint>, result: SolverResult) {
        let hash = self.compute_constraint_hash(&constraints);

        // LRU cache handles eviction automatically
        if let Some(_evicted) = self.solved_constraints.push(hash, result) {
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
    pub fn get_implementation(&mut self, trait_id: &TypeNameId, impl_id: &TypeNameId) -> Option<bool> {
        self.implementation_cache
            .get(&(trait_id.clone(), impl_id.clone()))
            .copied()
    }

    /// Clear all cached entries
    pub fn clear(&mut self) {
        self.solved_constraints.clear();
        self.type_hierarchies.clear();
        self.implementation_cache.clear();
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
    }

    /// Compute hash for a set of constraints
    fn compute_constraint_hash(&self, constraints: &[SMTConstraint]) -> ConstraintSetHash {
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
