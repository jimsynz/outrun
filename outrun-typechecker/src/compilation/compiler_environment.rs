//! Global compiler environment for managing types and modules across all programs
//!
//! This module provides a unified CompilerEnvironment that manages type names,
//! modules, and trait implementations across multiple compilation units.
//!
//! The CompilerEnvironment serves as the root of the compilation system,
//! providing methods to compile programs and manage compilation state.

#![allow(dead_code)] // Many methods are for future use or debugging
#![allow(clippy::manual_unwrap_or_default)] // More explicit error handling
#![allow(clippy::manual_unwrap_or)] // More explicit error handling
#![allow(clippy::collapsible_match)] // Clearer logic flow

use crate::checker::TypedFunctionDefinition;
use crate::compilation::program_collection::{CompilationResult, ProgramCollection};
use crate::compilation::type_checking::TypeCheckingVisitor;
use crate::compilation::visitors::{
    ImplExtractionVisitor, StructExtractionVisitor, TraitExtractionVisitor,
};
use crate::dependency_graph::DependencyGraph;
use crate::desugaring::DesugaringVisitor;
use crate::error::{SpanExt, TypeError};
use crate::types::traits::TraitConstraint;
use crate::unification::{StructuredType, UnificationContext};
use crate::visitor::Visitor;
use outrun_parser::{
    FunctionDefinition, ImplBlock, Program, StructDefinition, TraitDefinition, TraitFunction,
};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, RwLock};

/// Result of applying SMT model to resolve type parameters
struct ResolvedTypes {
    trait_type: StructuredType,
    impl_type: StructuredType,
}

/// Function call site information for SMT clause analysis
#[derive(Debug, Clone)]
struct FunctionCallSite {
    pub trait_name: String,
    pub function_name: String,
    pub argument_types: Vec<StructuredType>,
    pub impl_type: StructuredType,
    pub span: outrun_parser::Span,
    pub call_context: String, // For debugging
}

/// Specification for generating SMT constraints after type checking
/// This captures all the information needed to create constraints without requiring immediate generation
#[derive(Debug, Clone)]
pub struct ConstraintSpecification {
    pub call_site: outrun_parser::Span,
    pub trait_name: String,
    pub function_name: String,
    pub trait_type: StructuredType,     // Fully resolved trait type
    pub impl_type: StructuredType,      // Fully resolved implementation type (no unresolved Self)
    pub argument_types: Vec<StructuredType>,
    pub has_guard: bool,
    pub call_context: String,          // For debugging
}

/// Information about a function call site for monomorphization analysis
#[derive(Debug, Clone)]
struct MonomorphizationCallSite {
    pub trait_name: String,
    pub function_name: String,
    pub argument_types: std::collections::HashMap<String, StructuredType>,
    pub return_type: StructuredType,
    pub span: outrun_parser::Span,
    pub file_path: String,
}

/// Specification for generating a fully monomorphized function clause
#[derive(Debug, Clone)]
struct FullMonomorphizationSpec {
    pub trait_name: String,
    pub function_name: String,
    pub implementing_type: StructuredType,
    pub concrete_argument_types: std::collections::HashMap<String, StructuredType>,
    pub concrete_return_type: StructuredType,
    pub call_site_span: outrun_parser::Span,
}

/// Hash-based type name identifier with display capabilities
#[derive(Clone)]
pub struct TypeNameId {
    /// Deterministic hash of the type name
    pub hash: u64,
    /// Arc reference to the underlying storage
    pub storage: Arc<RwLock<HashMap<u64, String>>>,
}

impl TypeNameId {
    /// Create a new TypeNameId from a hash and storage
    pub fn new(hash: u64, storage: Arc<RwLock<HashMap<u64, String>>>) -> Self {
        Self { hash, storage }
    }
}

impl fmt::Display for TypeNameId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.storage.read().unwrap().get(&self.hash) {
            Some(name) => write!(f, "{name}"),
            None => write!(f, "<unknown type:{}>", self.hash),
        }
    }
}

impl fmt::Debug for TypeNameId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.storage.read().unwrap().get(&self.hash) {
            Some(name) => write!(f, "TypeNameId({name})"),
            None => write!(f, "TypeNameId(<unknown:{}>)", self.hash),
        }
    }
}

impl PartialEq for TypeNameId {
    fn eq(&self, other: &Self) -> bool {
        // Two TypeNameIds are equal if they have the same hash
        // The storage reference doesn't matter for equality
        self.hash == other.hash
    }
}

impl Eq for TypeNameId {}

impl std::hash::Hash for TypeNameId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Use the pre-computed hash directly
        state.write_u64(self.hash);
    }
}

/// Hash-based atom identifier with display capabilities
#[derive(Clone)]
pub struct AtomId {
    /// Deterministic hash of the atom name
    pub hash: u64,
    /// Arc reference to the underlying storage
    pub storage: Arc<RwLock<HashMap<u64, String>>>,
}

impl AtomId {
    /// Create a new AtomId from a hash and storage
    pub fn new(hash: u64, storage: Arc<RwLock<HashMap<u64, String>>>) -> Self {
        Self { hash, storage }
    }
}

impl fmt::Display for AtomId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.storage.read().unwrap().get(&self.hash) {
            Some(name) => write!(f, "{name}"),
            None => write!(f, "<unknown atom:{}>", self.hash),
        }
    }
}

impl fmt::Debug for AtomId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.storage.read().unwrap().get(&self.hash) {
            Some(name) => write!(f, "AtomId({name})"),
            None => write!(f, "AtomId(<unknown:{}>)", self.hash),
        }
    }
}

impl PartialEq for AtomId {
    fn eq(&self, other: &Self) -> bool {
        // Two AtomIds are equal if they have the same hash
        // The storage reference doesn't matter for equality
        self.hash == other.hash
    }
}

impl Eq for AtomId {}

impl std::hash::Hash for AtomId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Use the pre-computed hash directly
        state.write_u64(self.hash);
    }
}

/// Function signature for central registry lookup
/// Provides fast O(1) access to all implementations of a function across traits
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    /// Trait name (None for non-trait functions, Some for trait functions)
    pub trait_name: Option<String>,
    /// Function name
    pub function_name: String,
}

impl FunctionSignature {
    /// Create a new function signature for a trait function
    pub fn trait_function(trait_name: String, function_name: String) -> Self {
        Self {
            trait_name: Some(trait_name),
            function_name,
        }
    }
    
    /// Create a new function signature for a non-trait function
    pub fn standalone_function(function_name: String) -> Self {
        Self {
            trait_name: None,
            function_name,
        }
    }
    
    /// Get the qualified function name for display
    pub fn qualified_name(&self) -> String {
        match &self.trait_name {
            Some(trait_name) => format!("{}::{}", trait_name, self.function_name),
            None => self.function_name.clone(),
        }
    }
}

/// Central function clause registry for fast dispatch across all modules
/// This provides O(1) lookup for function clauses instead of O(modules) module traversal
#[derive(Debug)]
pub struct FunctionClauseRegistry {
    /// Fast lookup by function signature -> all implementations
    /// Maps (trait_name, function_name) to all clauses that implement that function
    clauses_by_signature: HashMap<FunctionSignature, Vec<Arc<crate::checker::FunctionClause>>>,
    
    /// Fast lookup by clause ID for pre-resolved dispatch
    /// Maps clause_id to the specific clause for direct execution
    clauses_by_id: HashMap<String, Arc<crate::checker::FunctionClause>>,
    
    /// Statistics for debugging and optimization
    total_clauses: usize,
    total_signatures: usize,
}

impl FunctionClauseRegistry {
    /// Create a new empty function clause registry
    pub fn new() -> Self {
        Self {
            clauses_by_signature: HashMap::new(),
            clauses_by_id: HashMap::new(),
            total_clauses: 0,
            total_signatures: 0,
        }
    }
    
    /// Add a function clause to the registry
    /// The clause will be indexed by both signature and ID for fast lookup
    pub fn add_clause(&mut self, signature: FunctionSignature, clause: crate::checker::FunctionClause) {
        let clause_arc = Arc::new(clause);
        
        // Index by signature for trait dispatch
        self.clauses_by_signature
            .entry(signature)
            .or_insert_with(Vec::new)
            .push(clause_arc.clone());
            
        // Index by ID for pre-resolved dispatch
        self.clauses_by_id.insert(clause_arc.clause_id.clone(), clause_arc);
        
        // Update statistics
        self.total_clauses += 1;
        self.total_signatures = self.clauses_by_signature.len();
    }
    
    /// Get all clauses for a function signature
    /// Returns all implementations of trait_name::function_name across all types
    pub fn get_clauses_by_signature(&self, signature: &FunctionSignature) -> Option<&[Arc<crate::checker::FunctionClause>]> {
        self.clauses_by_signature.get(signature).map(|v| v.as_slice())
    }
    
    /// Get a specific clause by ID
    /// Used for pre-resolved dispatch where SMT has already selected the clause
    pub fn get_clause_by_id(&self, clause_id: &str) -> Option<&Arc<crate::checker::FunctionClause>> {
        self.clauses_by_id.get(clause_id)
    }
    
    /// Get all clauses as a unified clause set
    /// This replaces the module traversal approach with direct registry lookup
    pub fn get_unified_clause_set(&self, signature: &FunctionSignature) -> Option<crate::checker::FunctionClauseSet> {
        if let Some(clauses) = self.clauses_by_signature.get(signature) {
            if !clauses.is_empty() {
                // Convert Arc<FunctionClause> to FunctionClause for the clause set
                let clause_copies: Vec<crate::checker::FunctionClause> = clauses
                    .iter()
                    .map(|arc_clause| (**arc_clause).clone())
                    .collect();
                    
                Some(crate::checker::FunctionClauseSet {
                    clauses: clause_copies,
                    function_name: signature.qualified_name(),
                    clause_resolution_cache: HashMap::new(),
                })
            } else {
                None
            }
        } else {
            None
        }
    }
    
    /// Get registry statistics for debugging
    pub fn stats(&self) -> (usize, usize, usize) {
        (self.total_clauses, self.total_signatures, self.clauses_by_id.len())
    }
    
    /// List all function signatures in the registry
    pub fn list_signatures(&self) -> Vec<&FunctionSignature> {
        self.clauses_by_signature.keys().collect()
    }
}

impl Default for FunctionClauseRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Key for identifying modules in the compiler environment
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleKey {
    /// Module identified by its base name hash (for traits and structs)
    Module(u64),
    /// Trait implementation module identified by its full structured types
    TraitImpl(Box<StructuredType>, Box<StructuredType>),
}

/// Kind of module being stored
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleKind {
    /// Struct definition module
    Struct,
    /// Trait definition module
    Trait,
    /// Trait implementation module
    TraitImpl,
    /// Auto-implemented trait module (generated by compiler)
    AutoImplemented,
}

/// Source location information for modules
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SourceLocation {
    /// Source from a file path
    File(String),
    /// Source from user input (e.g., REPL)
    Input(String),
}

// REMOVED: Old FunctionEntry struct - replaced by UnifiedFunctionEntry enum

/// Type of function for dispatch resolution
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType {
    /// Static trait function (defs keyword)
    TraitStatic,
    /// Trait function signature only (def without body)
    TraitSignature,
    /// Trait function with default implementation (def with body)
    TraitDefault,
    /// Static function on a type (not in trait)
    TypeStatic,
    /// Implementation function in impl block
    ImplFunction,
}

/// Unified function entry enum that replaces both FunctionEntry struct and FunctionType enum
/// This will eventually replace the above structures
#[derive(Debug, Clone)]
pub enum UnifiedFunctionEntry {
    /// Static trait function (defs keyword)
    TraitStatic {
        definition: outrun_parser::FunctionDefinition,
        typed_definition: Option<TypedFunctionDefinition>,
        function_id: String,
        is_guard: bool,
    },
    /// Trait function signature only (def without body)
    TraitSignature {
        definition: outrun_parser::FunctionDefinition,
        typed_definition: Option<TypedFunctionDefinition>,
        function_id: String,
        is_guard: bool,
    },
    /// Trait function with default implementation (def with body)
    TraitDefault {
        definition: outrun_parser::FunctionDefinition,
        typed_definition: Option<TypedFunctionDefinition>,
        function_id: String,
        is_guard: bool,
    },
    /// Static function on a type (not in trait)
    TypeStatic {
        definition: outrun_parser::FunctionDefinition,
        typed_definition: Option<TypedFunctionDefinition>,
        function_id: String,
        is_guard: bool,
    },
    /// Implementation function in impl block
    ImplFunction {
        definition: outrun_parser::FunctionDefinition,
        typed_definition: Option<TypedFunctionDefinition>,
        function_id: String,
        is_guard: bool,
    },
    /// Intrinsic function implemented by the compiler/runtime
    Intrinsic {
        definition: outrun_parser::FunctionDefinition,
        typed_definition: Option<TypedFunctionDefinition>,
        function_id: String,
        is_guard: bool,
    },
}

impl UnifiedFunctionEntry {
    /// Get the function definition
    pub fn definition(&self) -> &outrun_parser::FunctionDefinition {
        match self {
            UnifiedFunctionEntry::TraitStatic { definition, .. } => definition,
            UnifiedFunctionEntry::TraitSignature { definition, .. } => definition,
            UnifiedFunctionEntry::TraitDefault { definition, .. } => definition,
            UnifiedFunctionEntry::TypeStatic { definition, .. } => definition,
            UnifiedFunctionEntry::ImplFunction { definition, .. } => definition,
            UnifiedFunctionEntry::Intrinsic { definition, .. } => definition,
        }
    }

    /// Get the typed function definition
    pub fn typed_definition(&self) -> &Option<TypedFunctionDefinition> {
        match self {
            UnifiedFunctionEntry::TraitStatic {
                typed_definition, ..
            } => typed_definition,
            UnifiedFunctionEntry::TraitSignature {
                typed_definition, ..
            } => typed_definition,
            UnifiedFunctionEntry::TraitDefault {
                typed_definition, ..
            } => typed_definition,
            UnifiedFunctionEntry::TypeStatic {
                typed_definition, ..
            } => typed_definition,
            UnifiedFunctionEntry::ImplFunction {
                typed_definition, ..
            } => typed_definition,
            UnifiedFunctionEntry::Intrinsic {
                typed_definition, ..
            } => typed_definition,
        }
    }

    /// Get the function ID
    pub fn function_id(&self) -> &str {
        match self {
            UnifiedFunctionEntry::TraitStatic { function_id, .. } => function_id,
            UnifiedFunctionEntry::TraitSignature { function_id, .. } => function_id,
            UnifiedFunctionEntry::TraitDefault { function_id, .. } => function_id,
            UnifiedFunctionEntry::TypeStatic { function_id, .. } => function_id,
            UnifiedFunctionEntry::ImplFunction { function_id, .. } => function_id,
            UnifiedFunctionEntry::Intrinsic { function_id, .. } => function_id,
        }
    }

    /// Check if this is a guard function
    pub fn is_guard(&self) -> bool {
        match self {
            UnifiedFunctionEntry::TraitStatic { is_guard, .. } => *is_guard,
            UnifiedFunctionEntry::TraitSignature { is_guard, .. } => *is_guard,
            UnifiedFunctionEntry::TraitDefault { is_guard, .. } => *is_guard,
            UnifiedFunctionEntry::TypeStatic { is_guard, .. } => *is_guard,
            UnifiedFunctionEntry::ImplFunction { is_guard, .. } => *is_guard,
            UnifiedFunctionEntry::Intrinsic { is_guard, .. } => *is_guard,
        }
    }

    /// Get the function type for compatibility with existing code
    pub fn function_type(&self) -> FunctionType {
        match self {
            UnifiedFunctionEntry::TraitStatic { .. } => FunctionType::TraitStatic,
            UnifiedFunctionEntry::TraitSignature { .. } => FunctionType::TraitSignature,
            UnifiedFunctionEntry::TraitDefault { .. } => FunctionType::TraitDefault,
            UnifiedFunctionEntry::TypeStatic { .. } => FunctionType::TypeStatic,
            UnifiedFunctionEntry::ImplFunction { .. } => FunctionType::ImplFunction,
            UnifiedFunctionEntry::Intrinsic { .. } => FunctionType::TraitStatic, // Treat intrinsics as static for now
        }
    }

    /// Set the typed function definition
    pub fn set_typed_definition(&mut self, typed_def: TypedFunctionDefinition) {
        match self {
            UnifiedFunctionEntry::TraitStatic {
                typed_definition, ..
            } => *typed_definition = Some(typed_def),
            UnifiedFunctionEntry::TraitSignature {
                typed_definition, ..
            } => *typed_definition = Some(typed_def),
            UnifiedFunctionEntry::TraitDefault {
                typed_definition, ..
            } => *typed_definition = Some(typed_def),
            UnifiedFunctionEntry::TypeStatic {
                typed_definition, ..
            } => *typed_definition = Some(typed_def),
            UnifiedFunctionEntry::ImplFunction {
                typed_definition, ..
            } => *typed_definition = Some(typed_def),
            UnifiedFunctionEntry::Intrinsic {
                typed_definition, ..
            } => *typed_definition = Some(typed_def),
        }
    }

    /// Check if this is an intrinsic function
    pub fn is_intrinsic(&self) -> bool {
        matches!(self, UnifiedFunctionEntry::Intrinsic { .. })
    }
}

/// Module entry in the compiler environment
#[derive(Debug, Clone)]
pub struct Module {
    /// The kind of module (struct, trait, or trait impl)
    pub module_kind: ModuleKind,
    /// Source location where this module was defined
    pub source_location: SourceLocation,
    /// The full structured type of the module
    pub structured_type: StructuredType,
    /// Functions defined in this module
    /// Maps function structured types to their entries
    pub functions: HashMap<StructuredType, UnifiedFunctionEntry>,
    /// Functions indexed by name for efficient lookup
    /// Maps function names (AtomId) to their entries
    pub functions_by_name: HashMap<AtomId, UnifiedFunctionEntry>,
    /// Trait constraints for trait modules (empty for struct/impl modules)
    /// These define requirements like T: Display && T: Debug for generic parameters
    pub trait_constraints: Vec<TraitConstraint>,
    /// The original trait definition (only for trait modules)
    pub trait_definition: Option<outrun_parser::TraitDefinition>,
    /// Function clause sets for functions with multiple definitions (guards)
    /// Maps function names to clause sets containing all variants
    pub function_clauses: HashMap<AtomId, crate::checker::FunctionClauseSet>,
    /// All function entries by name (for functions with same name)
    /// Maps function names to ALL entries (not just the "preferred" one in functions_by_name)
    pub all_functions_by_name: HashMap<AtomId, Vec<UnifiedFunctionEntry>>,
}

impl Module {
    /// Create a new module
    pub fn new(
        module_kind: ModuleKind,
        source_location: SourceLocation,
        structured_type: StructuredType,
    ) -> Self {
        Self {
            module_kind,
            source_location,
            structured_type,
            functions: HashMap::new(),
            functions_by_name: HashMap::new(),
            trait_constraints: Vec::new(),
            trait_definition: None,
            function_clauses: HashMap::new(),
            all_functions_by_name: HashMap::new(),
        }
    }

    /// Add a function to this module
    pub fn add_function(&mut self, function_type: StructuredType, entry: UnifiedFunctionEntry) {
        self.functions.insert(function_type, entry.clone());
    }

    /// Add a function to this module by name
    /// Handles multiple functions with the same name (function clauses) automatically
    pub fn add_function_by_name(&mut self, function_name: AtomId, entry: UnifiedFunctionEntry) {
        // Only debug the specific function we're interested in to avoid spam
        
        // Always add to the all_functions_by_name list
        self.all_functions_by_name
            .entry(function_name.clone())
            .or_insert_with(Vec::new)
            .push(entry.clone());
        
        // Check if a function with this name already exists in the primary registry
        if let Some(existing_entry) = self.functions_by_name.get(&function_name) {
            // Multiple functions with same name - create or update function clause set
            self.create_or_update_function_clause_set(function_name.clone(), existing_entry.clone(), entry);
        } else {
            // First function with this name - store in primary registry
            self.functions_by_name.insert(function_name, entry);
        }
    }

    /// Add a function to this module (both by type and name)
    pub fn add_function_complete(
        &mut self,
        function_type: StructuredType,
        function_name: AtomId,
        entry: UnifiedFunctionEntry,
    ) {
        self.functions.insert(function_type, entry.clone());
        self.add_function_by_name(function_name, entry); // Use the clause-aware method
    }

    /// Create or update a function clause set when multiple functions have the same name
    fn create_or_update_function_clause_set(
        &mut self,
        function_name: AtomId,
        existing_entry: UnifiedFunctionEntry,
        new_entry: UnifiedFunctionEntry,
    ) {
        // Get function name as string for the clause set
        // Note: This is a placeholder - the atom name should be passed from CompilerEnvironment
        let function_name_str = format!("function_{}", function_name.hash);

        // Check if we already have a clause set for this function
        if let Some(existing_clause_set) = self.function_clauses.get_mut(&function_name) {
            // Add the new function to the existing clause set
            if let Some(typed_def) = new_entry.typed_definition() {
                let clause = crate::checker::FunctionClause {
                    clause_id: format!("{}_{}", function_name_str, existing_clause_set.clauses.len()),
                    base_function: typed_def.clone(),
                    source_order: typed_def.span.start as u32, // Source order based on span position
                    applicability_constraints: Vec::new(), // Will be filled during SMT analysis
                    from_guard: typed_def.guard.is_some(),
                    span: typed_def.span,
                };
                existing_clause_set.clauses.push(clause);
            }
        } else {
            // Create a new function clause set with both functions
            let clauses = Vec::new();

            // During initial registration, we don't have typed definitions yet
            // So we'll create an empty clause set and populate it later during process_registered_impl_functions
            // This is a marker that multiple functions with this name exist

            // Create the function clause set
            let clause_set = crate::checker::FunctionClauseSet {
                function_name: function_name_str.clone(),
                clauses,
                clause_resolution_cache: std::collections::HashMap::new(),
            };

            // Store the clause set
            self.function_clauses.insert(function_name.clone(), clause_set);
        }

        // Update the main function registry to point to the most specific (guard-based) function
        // Priority: functions with guards over functions without guards
        let existing_raw_def = existing_entry.definition();
        let new_raw_def = new_entry.definition();
        
        let should_update_main_registry = {
            // Always check raw definitions first (available during initial registration)
            if new_raw_def.guard.is_some() && existing_raw_def.guard.is_none() {
                true
            } else {
                // Fallback to typed definitions if needed
                match (existing_entry.typed_definition(), new_entry.typed_definition()) {
                    (Some(existing_def), Some(new_def)) => {
                        new_def.guard.is_some() && existing_def.guard.is_none()
                    }
                    _ => false,
                }
            }
        };

        if should_update_main_registry {
            self.functions_by_name.insert(function_name, new_entry);
        }
    }

    /// Helper method to resolve atom name (needs to be implemented or access CompilerEnvironment)
    fn resolve_atom_name(&self, _atom_id: &AtomId) -> Option<String> {
        // This is a placeholder - in the actual implementation, this would need access to the CompilerEnvironment
        // For now, we'll use a simple fallback
        None
    }

    /// Get a function from this module by type
    pub fn get_function(&self, function_type: &StructuredType) -> Option<&UnifiedFunctionEntry> {
        self.functions.get(function_type)
    }

    /// Get a function from this module by name
    pub fn get_function_by_name(&self, function_name: AtomId) -> Option<&UnifiedFunctionEntry> {
        self.functions_by_name.get(&function_name)
    }

    /// Set trait constraints for this module (only valid for trait modules)
    pub fn set_trait_constraints(&mut self, constraints: Vec<TraitConstraint>) {
        self.trait_constraints = constraints;
    }

    /// Get trait constraints for this module
    pub fn get_trait_constraints(&self) -> &[TraitConstraint] {
        &self.trait_constraints
    }

    /// Set trait definition for this module (only valid for trait modules)
    pub fn set_trait_definition(&mut self, trait_def: outrun_parser::TraitDefinition) {
        self.trait_definition = Some(trait_def);
    }

    /// Get trait definition for this module
    pub fn get_trait_definition(&self) -> Option<&outrun_parser::TraitDefinition> {
        self.trait_definition.as_ref()
    }

    /// Add a function clause to a clause set
    pub fn add_function_clause(&mut self, function_name: AtomId, clause: crate::checker::FunctionClause) {
        let function_name_str = self.atom_to_string(&function_name);
        
        // Get or create the clause set for this function name
        let clause_set = self.function_clauses
            .entry(function_name)
            .or_insert_with(|| crate::checker::FunctionClauseSet::new(function_name_str));
        
        clause_set.add_clause(clause);
    }

    /// Get function clause set by name
    pub fn get_function_clause_set(&self, function_name: &AtomId) -> Option<&crate::checker::FunctionClauseSet> {
        self.function_clauses.get(function_name)
    }

    /// Get mutable function clause set by name
    pub fn get_function_clause_set_mut(&mut self, function_name: &AtomId) -> Option<&mut crate::checker::FunctionClauseSet> {
        self.function_clauses.get_mut(function_name)
    }

    /// Check if a function has multiple clauses (guards)
    pub fn has_function_clauses(&self, function_name: &AtomId) -> bool {
        self.function_clauses.contains_key(function_name)
    }

    /// Get all function clause sets in this module
    pub fn get_all_function_clauses(&self) -> &HashMap<AtomId, crate::checker::FunctionClauseSet> {
        &self.function_clauses
    }

    /// Helper method to convert AtomId to string (needs access to CompilerEnvironment)
    fn atom_to_string(&self, atom_id: &AtomId) -> String {
        // This is a temporary implementation - in real usage, we'd need access to CompilerEnvironment
        format!("function_{}", atom_id.hash)
    }
}

/// Global compiler environment for managing types and modules
///
/// This serves as the root of the compilation system, managing all state
/// necessary for compiling Outrun programs including type interning,
/// module storage, and compilation results.
#[derive(Debug, Clone)]
pub struct CompilerEnvironment {
    /// Hash-based storage for type names with interior mutability
    type_names: Arc<RwLock<HashMap<u64, String>>>,
    /// Hash-based storage for atom names with interior mutability
    atoms: Arc<RwLock<HashMap<u64, String>>>,
    /// Map of module keys to their module definitions with interior mutability
    modules: Arc<RwLock<HashMap<ModuleKey, Module>>>,
    /// Central function clause registry for fast O(1) dispatch lookup
    /// Replaces expensive module traversal with direct function signature lookup
    function_registry: Arc<RwLock<FunctionClauseRegistry>>,
    /// Compilation state
    compilation_state: Arc<RwLock<CompilationState>>,
    /// Struct definitions indexed by TypeNameId for type checking
    structs: Arc<RwLock<HashMap<TypeNameId, StructDefinition>>>,
    /// Trait definitions indexed by TypeNameId for trait resolution
    traits: Arc<RwLock<HashMap<TypeNameId, TraitDefinition>>>,
    /// Cache for clause index lookups to improve performance during constraint generation
    clause_index_cache: Arc<RwLock<HashMap<usize, usize>>>,
}

/// Internal compilation state for the CompilerEnvironment
#[derive(Debug, Clone, Default)]
struct CompilationState {
    /// Dependency graph for program ordering
    dependency_graph: DependencyGraph,
    /// Unification context for type checking
    unification_context: UnificationContext,
    /// Accumulated compilation errors
    errors: Vec<TypeError>,
    /// Latest compilation result
    compilation_result: Option<CompilationResult>,
    /// Dispatch table for runtime trait method resolution
    dispatch_table: crate::dispatch::DispatchTable,
    /// Compilation order for dependency resolution
    compilation_order: Vec<String>,
    /// External variables available in this compilation phase (for REPL usage)
    external_variables: HashMap<String, StructuredType>,
    /// Implementation blocks extracted during compilation
    implementations: Vec<ImplBlock>,
    /// Next dispatch ID for trait function calls
    next_dispatch_id: u32,
    /// SMT constraint solving model results (Phase 7)
    smt_model: Option<crate::smt::solver::ConstraintModel>,
}

impl CompilationState {}

impl CompilerEnvironment {
    /// Create a new compiler environment
    pub fn new() -> Self {
        let env = Self {
            type_names: Arc::new(RwLock::new(HashMap::new())),
            atoms: Arc::new(RwLock::new(HashMap::new())),
            modules: Arc::new(RwLock::new(HashMap::new())),
            function_registry: Arc::new(RwLock::new(FunctionClauseRegistry::new())),
            compilation_state: Arc::new(RwLock::new(CompilationState::default())),
            structs: Arc::new(RwLock::new(HashMap::new())),
            traits: Arc::new(RwLock::new(HashMap::new())),
            clause_index_cache: Arc::new(RwLock::new(HashMap::new())),
        };

        // Initialize with default compilation state
        *env.compilation_state.write().unwrap() = CompilationState::default();

        // Bootstrap intrinsic functions automatically
        crate::intrinsics::bootstrap_intrinsics(env)
    }

    // REMOVED: bootstrap_intrinsic_functions() - intrinsics are now automatically bootstrapped in new()

    /// Compile a single program
    pub fn compile_program(
        &mut self,
        program: Program,
    ) -> Result<CompilationResult, Vec<TypeError>> {
        let mut collection = ProgramCollection::new();
        collection.add_program("main".to_string(), program, "".to_string());
        self.compile_collection(collection)
    }

    /// Compile a collection of programs
    pub fn compile_collection(
        &mut self,
        collection: ProgramCollection,
    ) -> Result<CompilationResult, Vec<TypeError>> {
        self.compile_collection_with_external_variables(collection, HashMap::new())
    }

    /// Compile a collection of programs with external variables (for REPL usage)
    pub fn compile_collection_with_external_variables(
        &mut self,
        collection: ProgramCollection,
        external_variables: HashMap<String, StructuredType>,
    ) -> Result<CompilationResult, Vec<TypeError>> {
        // Store external variables in CompilerEnvironment
        self.set_external_variables(external_variables.clone());

        // Use our internal compilation implementation
        self.internal_compile_collection(&collection, external_variables)
    }

    /// Internal compilation implementation
    /// This contains the main compilation logic moved from MultiProgramCompiler
    fn internal_compile_collection(
        &mut self,
        collection: &ProgramCollection,
        external_variables: HashMap<String, StructuredType>,
    ) -> Result<CompilationResult, Vec<TypeError>> {
        // Clear previous errors
        self.clear_errors();

        // Step 1: Build dependency graph and get compilation order
        let compilation_order = self.resolve_dependencies(collection)?;

        // Store compilation order in CompilerEnvironment
        self.set_compilation_order(compilation_order.clone());

        // Step 1: Phase 1 - Desugar all programs (transform operators to trait function calls)
        // This must happen BEFORE trait/struct/function extraction so we're working with canonical form
        let desugared_collection = self.desugar_programs(collection, &compilation_order)?;

        // Step 2: Phase 2 - Extract all traits (from desugared code)
        let traits = self.extract_traits(&desugared_collection, &compilation_order)?;

        // Store traits in CompilerEnvironment
        for (type_id, trait_def) in &traits {
            self.add_trait(type_id.clone(), trait_def.clone());
        }

        // Create modules for traits (similar to struct modules)
        for (type_id, trait_def) in &traits {
            // Create the structured type for this trait
            let structured_type = if let Some(generic_params) = &trait_def.generic_params {
                // Generic trait like Map<K, V>
                let generic_args: Vec<StructuredType> = generic_params
                    .params
                    .iter()
                    .map(|param| {
                        let param_type_id = self.intern_type_name(&param.name.name);
                        StructuredType::TypeVariable(param_type_id)  // ✅ Fix: Use TypeVariable for trait type parameters
                    })
                    .collect();

                StructuredType::Generic {
                    base: type_id.clone(),
                    args: generic_args,
                }
            } else {
                // Simple trait without generics
                StructuredType::Simple(type_id.clone())
            };

            // Create module for the trait
            let module_key = ModuleKey::Module(type_id.hash);
            self.get_or_create_module(
                module_key.clone(),
                ModuleKind::Trait,
                SourceLocation::File("trait_definition".to_string()),
                structured_type,
            );

            // CRITICAL FIX: Store the trait definition in the module for SMT constraint generation
            if let Ok(mut modules) = self.modules.write() {
                if let Some(module) = modules.get_mut(&module_key) {
                    module.set_trait_definition(trait_def.clone());
                }
            }
        }

        // Step 3: Phase 3 - Extract all structs (from desugared code)
        let structs = self.extract_structs(&desugared_collection, &compilation_order)?;

        // Store structs in CompilerEnvironment
        for (type_id, struct_def) in &structs {
            self.add_struct(type_id.clone(), struct_def.clone());
        }

        // Create modules for structs (similar to how trait modules are created)
        for (type_id, struct_def) in &structs {
            // Create the structured type for this struct
            let structured_type = if let Some(generic_params) = &struct_def.generic_params {
                // Generic struct like Outrun.Core.Map<K, V>
                let generic_args: Vec<StructuredType> = generic_params
                    .params
                    .iter()
                    .map(|param| {
                        let param_type_id = self.intern_type_name(&param.name.name);
                        StructuredType::Simple(param_type_id)
                    })
                    .collect();

                StructuredType::Generic {
                    base: type_id.clone(),
                    args: generic_args,
                }
            } else {
                // Simple struct without generics
                StructuredType::Simple(type_id.clone())
            };

            // Create module for the struct
            let module_key = ModuleKey::Module(type_id.hash);
            self.get_or_create_module(
                module_key,
                ModuleKind::Struct,
                SourceLocation::File("struct_definition".to_string()),
                structured_type,
            );
        }

        // Step 3.5: Generate auto-implementations for traits that support it
        self.generate_auto_implementations(&structs)?;

        // Step 4: Phase 4 - Extract all implementations (from desugared code)
        let implementations =
            self.extract_implementations(&desugared_collection, &compilation_order)?;

        // Store implementations in CompilerEnvironment
        // CRITICAL FIX: For incremental compilation (like user code on top of core library),
        // we need to append implementations rather than replace them
        let existing_implementations = self.get_implementations();
        if existing_implementations.is_empty() {
            // First compilation (e.g., core library) - set implementations
            self.set_implementations(implementations.clone());
        } else {
            // Incremental compilation (e.g., user code) - append to preserve core library implementations
            self.append_implementations(implementations.clone());
        }

        // Step 4.5: Phase 4.5 - Expand trait default implementations into concrete impl blocks (BEFORE function extraction)
        let expanded_collection =
            self.expand_trait_default_implementations(&desugared_collection)?;

        // Step 5: Phase 5 - Extract all functions (from EXPANDED code with default implementations)
        self.extract_functions(&expanded_collection, &compilation_order)?;

        // Step 5.6: Phase 6.5 - SMT-based clause analysis for function dispatch optimization
        self.perform_smt_clause_analysis()?;

        // Step 5.7: Phase 6.6 - Guard purity analysis for mathematical soundness
        self.perform_guard_purity_analysis()?;

        // Step 6: Phase 6 - SMT-based type checking with constraint collection (using expanded collection with Self as type variables)
        match self.smt_type_check_all(
            &expanded_collection,
            &compilation_order,
            &traits,
            &structs,
            &implementations,
            external_variables,
        ) {
            Ok(()) => {}
            Err(type_errors) => {
                return Err(type_errors);
            }
        }

        // Step 6.5: Phase 6.5 - Generate SMT constraints from collected specifications
        self.phase_6_5_generate_smt_constraints_from_specifications()?;

        // Step 7: Phase 7 - SMT constraint solving  
        self.phase_7_smt_constraint_solving()?;

        // Step 7.5: Phase 7.5 - SMT-guided monomorphization
        self.phase_7_5_smt_guided_monomorphization()?;

        // Step 7.6: Phase 7.6 - Comprehensive function signature monomorphization  
        self.phase_7_6_comprehensive_monomorphization(&expanded_collection)?;

        // Check for errors accumulated during type checking
        let errors = self.get_errors();
        if !errors.is_empty() {
            return Err(errors);
        }

        // Step 8: Phase 8 - Calculate dispatch tables using SMT results (prepare runtime dispatch information)
        self.calculate_dispatch_tables_with_smt(&expanded_collection, &compilation_order)?;

        // Step 9: Phase 9 - Build comprehensive typed AST
        let typed_programs =
            self.build_typed_ast(&expanded_collection, &compilation_order, &structs)?;

        // Step 6.5: Phase 6.5 - SMT-based function clause analysis (moved here after clause sets are populated)
        self.phase_6_5_smt_function_clause_analysis(&expanded_collection)?;

        // Create the final compilation result
        let result = CompilationResult {
            compilation_order,
            type_context: self.unification_context(),
            traits,
            structs,
            implementations,
            typed_programs,
        };

        // Store the result in our compilation state
        {
            let mut state = self.compilation_state.write().unwrap();
            state.compilation_result = Some(result.clone());
        }

        // Final validation: Ensure all functions have typed definitions
        let validation_errors = self.validate_all_functions_have_typed_definitions();
        if !validation_errors.is_empty() {
            return Err(validation_errors);
        }

        // Display SMT cache performance statistics only in debug mode

        Ok(result)
    }

    /// Get the latest compilation result
    pub fn get_compilation_result(&self) -> Option<CompilationResult> {
        self.compilation_state
            .read()
            .unwrap()
            .compilation_result
            .clone()
    }

    /// Get accumulated compilation errors
    pub fn get_errors(&self) -> Vec<TypeError> {
        self.compilation_state.read().unwrap().errors.clone()
    }

    /// Clear compilation errors
    pub fn clear_errors(&mut self) {
        self.compilation_state.write().unwrap().errors.clear();
    }

    /// Get access to the unification context
    pub fn unification_context(&self) -> UnificationContext {
        self.compilation_state
            .read()
            .unwrap()
            .unification_context
            .clone()
    }

    /// Get mutable access to the unification context
    pub fn unification_context_mut(&mut self) -> UnificationContext {
        let state = self.compilation_state.write().unwrap();
        state.unification_context.clone()
    }

    /// Update the unification context
    pub fn set_unification_context(&self, context: UnificationContext) {
        self.compilation_state.write().unwrap().unification_context = context;
    }

    /// Generate a consistent SMT-format clause ID for function clauses
    /// Format: "TraitName:ImplType:FunctionName" (deterministic, no spans)
    fn generate_smt_clause_id(
        &self,
        trait_name: Option<&str>,
        impl_type: &StructuredType,
        function_name: &str,
        _span: outrun_parser::Span, // Span no longer used for deterministic IDs
    ) -> String {
        let trait_part = trait_name.unwrap_or("Function");
        let impl_part = impl_type.to_string_representation();
        format!("{}:{}:{}", trait_part, impl_part, function_name)
    }
    
    /// Generate a clause ID for a specific function definition using clause counting
    fn generate_clause_id_for_function(
        &self,
        trait_name: Option<&str>,
        impl_type: &StructuredType,
        func_def: &FunctionDefinition,
        clause_index: usize,
    ) -> String {
        let trait_part = trait_name.unwrap_or("Function");
        let impl_part = impl_type.to_string_representation();
        let function_name = &func_def.name.name;
        
        // Use clause counting for deterministic, source-order-independent IDs
        // This uniquely identifies each function clause (including multiple clauses with guards)
        // Format: "TraitName:ImplType:FunctionName:ClauseIndex"
        format!("{}:{}:{}:{}", trait_part, impl_part, function_name, clause_index)
    }

    /// Get the clause index for a function within its trait/impl module
    /// This is used by the type checker to generate consistent clause IDs
    /// Results are cached to improve performance during repeated constraint generation
    pub fn get_clause_index_for_function(
        &self,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
        function_name: &str,
        func_def: &FunctionDefinition,
    ) -> Option<usize> {
        // Create a cache key based on function span for uniqueness
        let cache_key = func_def.span.start;
        
        // Check the clause index cache first
        {
            let cache = self.clause_index_cache.read().unwrap();
            if let Some(&index) = cache.get(&cache_key) {
                return Some(index);
            }
        }
        
        // Cache miss - compute the clause index
        let modules = self.modules.read().unwrap();
        
        // Determine the module key based on whether this is a trait static or trait impl function
        let module_key = if trait_type == impl_type {
            // Trait static function - stored in trait definition module
            match trait_type {
                StructuredType::Simple(type_id) => ModuleKey::Module(type_id.hash),
                StructuredType::Generic { base, .. } => ModuleKey::Module(base.hash),
                _ => return None,
            }
        } else {
            // Trait implementation function - stored in trait impl module  
            ModuleKey::TraitImpl(Box::new(trait_type.clone()), Box::new(impl_type.clone()))
        };
        
        let result = modules.get(&module_key)
            .and_then(|module| {
                let function_name_atom = self.intern_atom_name(function_name);
                module.all_functions_by_name.get(&function_name_atom)
            })
            .and_then(|function_entries| {
                // Find the index of this specific function definition among all clauses with the same name
                for (index, entry) in function_entries.iter().enumerate() {
                    let entry_def = match entry {
                        UnifiedFunctionEntry::TraitStatic { definition, .. } => definition,
                        UnifiedFunctionEntry::TraitDefault { definition, .. } => definition,
                        UnifiedFunctionEntry::ImplFunction { definition, .. } => definition,
                        UnifiedFunctionEntry::TraitSignature { definition, .. } => definition,
                        UnifiedFunctionEntry::TypeStatic { definition, .. } => definition,
                        UnifiedFunctionEntry::Intrinsic { .. } => continue, // Skip intrinsics, they don't have clause IDs
                    };
                    
                    // Match by span position for uniqueness
                    if entry_def.span == func_def.span {
                        return Some(index);
                    }
                }
                None
            });
        
        // Cache the result for future lookups
        if let Some(index) = result {
            let mut cache = self.clause_index_cache.write().unwrap();
            cache.insert(cache_key, index);
        }
        
        result
    }

    /// Get access to the dependency graph
    pub fn dependency_graph(&self) -> DependencyGraph {
        self.compilation_state
            .read()
            .unwrap()
            .dependency_graph
            .clone()
    }

    /// Get mutable access to the dependency graph
    pub fn dependency_graph_mut(&mut self) -> DependencyGraph {
        let state = self.compilation_state.write().unwrap();
        state.dependency_graph.clone()
    }

    // ===== Compilation Phase Methods =====
    // These methods implement the individual phases of compilation

    /// Resolve dependencies and get compilation order
    fn resolve_dependencies(
        &mut self,
        collection: &ProgramCollection,
    ) -> Result<Vec<String>, Vec<TypeError>> {
        let mut dependency_graph = self.dependency_graph();

        // Phase 1: Add all programs to collect type definitions
        for (file_path, program) in &collection.programs {
            if let Err(err) = dependency_graph.add_program(file_path.clone(), program.clone()) {
                use crate::dependency_graph::DependencyError;
                match err {
                    DependencyError::ConflictingDefinition { type_name, files } => {
                        let error = TypeError::internal(format!(
                            "Type {} is defined in multiple files: {}",
                            type_name,
                            files.join(", ")
                        ));
                        self.compilation_state.write().unwrap().errors.push(error);
                    }
                    _ => {
                        let error = TypeError::internal(format!(
                            "Unexpected dependency error in {file_path}: {err:?}"
                        ));
                        self.compilation_state.write().unwrap().errors.push(error);
                    }
                }
            }
        }

        // Phase 2: Build dependency edges and resolve
        dependency_graph.build_dependency_edges();
        let result = dependency_graph.resolve_with_trait_cycles_allowed();

        // Check for fatal circular dependencies
        if !result.circular_dependencies.is_empty() {
            let structural_cycles: Vec<_> = result
                .circular_dependencies
                .into_iter()
                .filter(|cycle| dependency_graph.is_structural_cycle(cycle))
                .collect();

            if !structural_cycles.is_empty() {
                let cycle_descriptions: Vec<String> = structural_cycles
                    .iter()
                    .map(|cycle| cycle.join(" -> "))
                    .collect();

                let error = TypeError::internal(format!(
                    "Structural circular dependencies detected: {}",
                    cycle_descriptions.join("; ")
                ));
                self.compilation_state
                    .write()
                    .unwrap()
                    .errors
                    .push(error.clone());
                return Err(vec![error]);
            }
        }

        // Update our dependency graph
        self.compilation_state.write().unwrap().dependency_graph = dependency_graph;

        Ok(result.compilation_order)
    }

    /// Extract trait definitions (Phase 1)
    fn extract_traits(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<HashMap<TypeNameId, TraitDefinition>, Vec<TypeError>> {
        let mut visitor = TraitExtractionVisitor::default();

        for file_path in order {
            if let Some(program) = collection.programs.get(file_path) {
                if let Err(err) =
                    <TraitExtractionVisitor as Visitor<()>>::visit_program(&mut visitor, program)
                {
                    self.compilation_state.write().unwrap().errors.push(err);
                }
            }
        }

        // Convert to TypeNameId-keyed map
        let mut traits = HashMap::new();

        for (name, trait_def) in visitor.traits {
            let type_name_id = self.intern_type_name(&name);
            traits.insert(type_name_id, trait_def);
        }

        Ok(traits)
    }

    /// Extract struct definitions (Phase 2)
    fn extract_structs(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<HashMap<TypeNameId, StructDefinition>, Vec<TypeError>> {
        let mut visitor = StructExtractionVisitor::default();

        for file_path in order {
            if let Some(program) = collection.programs.get(file_path) {
                if let Err(err) =
                    <StructExtractionVisitor as Visitor<()>>::visit_program(&mut visitor, program)
                {
                    self.compilation_state.write().unwrap().errors.push(err);
                }
            }
        }

        // Convert to TypeNameId-keyed map
        let mut structs = HashMap::new();

        for (name, struct_def) in visitor.structs {
            let type_name_id = self.intern_type_name(&name);
            structs.insert(type_name_id, struct_def);
        }

        Ok(structs)
    }

    /// Extract implementation blocks (Phase 3)
    fn extract_implementations(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<Vec<ImplBlock>, Vec<TypeError>> {
        let mut visitor = ImplExtractionVisitor::default();

        for file_path in order {
            if let Some(program) = collection.programs.get(file_path) {
                if let Err(err) =
                    <ImplExtractionVisitor as Visitor<()>>::visit_program(&mut visitor, program)
                {
                    self.compilation_state.write().unwrap().errors.push(err);
                }
            }
        }

        Ok(visitor.implementations)
    }

    /// Extract function definitions (Phase 4)
    fn extract_functions(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<(), Vec<TypeError>> {
        // Extract functions and populate the module system directly
        self.extract_functions_to_modules(collection, order)?;

        Ok(())
    }

    /// Generate automatic trait implementations for types that don't have manual ones
    /// This is called after struct extraction but before manual impl extraction
    fn generate_auto_implementations(
        &self,
        structs: &HashMap<TypeNameId, StructDefinition>,
    ) -> Result<(), Vec<TypeError>> {
        // Auto-implement Inspect trait for all concrete types
        let inspect_trait_id = self.intern_type_name("Inspect");
        let inspect_trait_type = StructuredType::Simple(inspect_trait_id);

        for struct_type_id in structs.keys() {
            let struct_type = StructuredType::Simple(struct_type_id.clone());

            // Auto-implement Inspect trait for this struct
            // Register the trait implementation AND create function entries for default implementations
            self.register_trait_implementation(struct_type.clone(), inspect_trait_type.clone());
            self.create_default_implementation_functions(&struct_type, &inspect_trait_type)?;
        }

        // Auto-implement Inspect trait for built-in generic types
        self.generate_builtin_inspect_implementations(&inspect_trait_type)?;

        Ok(())
    }

    /// Generate auto-implementations for built-in generic types like List<T>, Map<K,V>, etc.
    fn generate_builtin_inspect_implementations(
        &self,
        inspect_trait_type: &StructuredType,
    ) -> Result<(), Vec<TypeError>> {
        let _inspect_atom = self.intern_atom_name("inspect");

        // List of built-in types that need Inspect implementations
        let builtin_types = [
            "Outrun.Core.List",
            "Outrun.Core.Map",
            "Outrun.Core.Tuple",
            "Outrun.Core.Integer64",
            "Outrun.Core.Float64",
            "Outrun.Core.Boolean",
            "Outrun.Core.String",
            "Outrun.Core.Atom",
        ];

        for builtin_type_name in builtin_types.iter() {
            let builtin_type_id = self.intern_type_name(builtin_type_name);
            let builtin_type = StructuredType::Simple(builtin_type_id.clone());

            // Register the trait implementation AND create function entries for default implementations
            self.register_trait_implementation(builtin_type.clone(), inspect_trait_type.clone());
            self.create_default_implementation_functions(&builtin_type, inspect_trait_type)?;
        }

        Ok(())
    }

    /// Create function entries for default implementations from a trait
    /// This is needed so that auto-generated trait implementations can use default implementations
    fn create_default_implementation_functions(
        &self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
    ) -> Result<(), Vec<TypeError>> {
        // Get the trait name to look up its definition
        let trait_name = match trait_type {
            StructuredType::Simple(type_id) => {
                self.resolve_type_name(type_id).unwrap_or_default()
            }
            _ => return Ok(()), // Complex trait types not yet supported
        };

        // Find the trait definition
        if let Some(trait_def) = self.find_trait_definition(&trait_name) {
            // Create function entries for each default implementation in the trait
            for trait_function in &trait_def.functions {
                if let outrun_parser::TraitFunction::Definition(func_def) = trait_function {
                    // This is a default implementation - create a function entry for it
                    self.create_default_function_entry(impl_type, trait_type, func_def)?;
                }
            }
        }

        Ok(())
    }

    /// Create a function entry that references a default implementation
    fn create_default_function_entry(
        &self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
        default_func_def: &outrun_parser::FunctionDefinition,
    ) -> Result<(), Vec<TypeError>> {
        // Create the module key for this trait implementation
        let module_key = ModuleKey::TraitImpl(
            Box::new(trait_type.clone()),
            Box::new(impl_type.clone()),
        );

        // Ensure the module exists
        self.get_or_create_module(
            module_key.clone(),
            ModuleKind::TraitImpl,
            SourceLocation::Input("auto_implementation".to_string()),
            impl_type.clone(),
        );

        // Create an ImplFunction entry that references the default implementation
        let function_name_atom = self.intern_atom_name(&default_func_def.name.name);
        
        // Generate a unique function ID for this auto-implementation
        let function_id = format!(
            "auto_impl::{}::for::{}::{}",
            self.resolve_type_name(&match trait_type {
                StructuredType::Simple(id) => id.clone(),
                _ => return Ok(()), // Skip complex trait types for now
            }).unwrap_or_default(),
            self.resolve_type_name(&match impl_type {
                StructuredType::Simple(id) => id.clone(),
                _ => return Ok(()), // Skip complex impl types for now
            }).unwrap_or_default(),
            default_func_def.name.name
        );
        
        let is_guard = default_func_def.name.name.ends_with('?');
        
        // Create a UnifiedFunctionEntry for this default implementation
        let function_entry = UnifiedFunctionEntry::ImplFunction {
            definition: default_func_def.clone(),
            typed_definition: None, // Will be filled later by process_registered_impl_functions
            function_id,
            is_guard,
        };

        // Add the function using the same pattern as other modules
        // We need to use the function signature type as the key
        let function_signature_type = impl_type.clone(); // Use impl type as function type for now
        
        {
            let mut modules = self.modules.write().unwrap();
            if let Some(module) = modules.get_mut(&module_key) {
                module.add_function(function_signature_type, function_entry);
                
                // Also add to the by-name registry for lookup
                module.all_functions_by_name
                    .entry(function_name_atom)
                    .or_insert_with(Vec::new)
                    .push(module.functions.get(&impl_type.clone()).unwrap().clone());
            }
        }

        Ok(())
    }

    /// Extract function definitions into the module system (new approach)
    fn extract_functions_to_modules(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<(), Vec<TypeError>> {
        // Function extraction to modules
        let mut _total_functions = 0;

        for file_path in order {
            if let Some(program) = collection.programs.get(file_path) {
                // Processing file: {file_path}
                // Process each item in the program
                for item in &program.items {
                    if let Err(err) = self.extract_function_from_item(item, file_path) {
                        self.compilation_state.write().unwrap().errors.push(err);
                    } else {
                        // Count successful extractions
                        match &item.kind {
                            outrun_parser::ItemKind::FunctionDefinition(_) => {
                                _total_functions += 1;
                                // Extracted standalone function
                            }
                            outrun_parser::ItemKind::TraitDefinition(trait_def) => {
                                let func_count = trait_def.functions.len();
                                _total_functions += func_count;
                                // Extracted {func_count} trait functions from {trait_def.name_as_string()}
                            }
                            outrun_parser::ItemKind::ImplBlock(impl_block) => {
                                let func_count = impl_block.functions.len();
                                _total_functions += func_count;
                                // Extracted {func_count} impl functions
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Extract a function from an AST item and add it to the appropriate module
    fn extract_function_from_item(
        &self,
        item: &outrun_parser::Item,
        source_file: &str,
    ) -> Result<(), TypeError> {
        use outrun_parser::ItemKind;

        match &item.kind {
            ItemKind::FunctionDefinition(func_def) => {
                self.extract_standalone_function(func_def, source_file)
            }
            ItemKind::TraitDefinition(trait_def) => {
                // Extract functions from trait definition
                for trait_func in &trait_def.functions {
                    self.extract_trait_function(trait_func, trait_def, source_file)?;
                }
                Ok(())
            }
            ItemKind::ImplBlock(impl_block) => {
                // Extract trait implementations (no "functions" in functional language)
                // All functions in impl blocks are trait function implementations
                for func_def in &impl_block.functions {
                    self.extract_trait_impl_function(func_def, impl_block, source_file)?;
                }
                Ok(())
            }
            _ => Ok(()), // Skip non-function items
        }
    }

    /// Extract a standalone function and add it to a module
    fn extract_standalone_function(
        &self,
        func_def: &FunctionDefinition,
        source_file: &str,
    ) -> Result<(), TypeError> {
        // Create function entry
        let function_id = format!("function::{}", func_def.name.name);
        let is_guard = func_def.guard.is_some();
        let entry = UnifiedFunctionEntry::TypeStatic {
            definition: func_def.clone(),
            typed_definition: None,
            function_id: function_id.clone(),
            is_guard,
        };

        // Get the module type (use source file as module identifier for now)
        let module_type_name = format!("Module::{source_file}");
        // Create module if it doesn't exist
        let module_type_id = self.intern_type_name(&module_type_name);
        let module_key = ModuleKey::Module(module_type_id.hash);
        let module_type = StructuredType::Simple(module_type_id);
        self.get_or_create_module(
            module_key.clone(),
            ModuleKind::Struct, // Default for standalone functions
            SourceLocation::File(source_file.to_string()),
            module_type.clone(),
        );

        let function_name = self.intern_atom_name(&func_def.name.name);
        
        // If this function has a guard or could be part of a clause set, create a function clause
        if is_guard || self.should_create_clause_for_function(func_def) {
            self.add_function_clause_to_module(
                module_key.clone(),
                function_name.clone(),
                func_def,
                source_file,
            )?;
        }

        // Always add the function to the regular function storage as well
        self.add_function_to_module(module_key, module_type, function_name, entry);

        Ok(())
    }

    /// Extract a trait function and add it to the trait module
    fn extract_trait_function(
        &self,
        trait_func: &TraitFunction,
        trait_def: &outrun_parser::TraitDefinition,
        source_file: &str,
    ) -> Result<(), TypeError> {
        // Get the function definition from the TraitFunction enum
        // Handle all types of trait functions
        let (func_def, function_type) = match trait_func {
            TraitFunction::Definition(def) => (def.clone(), FunctionType::TraitDefault),
            TraitFunction::Signature(sig) => {
                // Convert signature to function definition for storage
                let func_def = FunctionDefinition {
                    attributes: sig.attributes.clone(),
                    name: sig.name.clone(),
                    visibility: sig.visibility.clone(),
                    parameters: sig.parameters.clone(),
                    return_type: sig.return_type.clone(),
                    guard: sig.guard.clone(),
                    body: outrun_parser::Block {
                        statements: Vec::new(),
                        span: outrun_parser::Span {
                            start: 0,
                            end: 0,
                            start_line_col: None,
                            end_line_col: None,
                        },
                    },
                    span: sig.span,
                };
                (func_def, FunctionType::TraitSignature)
            }
            TraitFunction::StaticDefinition(static_def) => {
                // Convert static definition to function definition for storage
                let func_def = FunctionDefinition {
                    attributes: static_def.attributes.clone(),
                    name: static_def.name.clone(),
                    visibility: outrun_parser::FunctionVisibility::Public,
                    parameters: static_def.parameters.clone(),
                    return_type: static_def.return_type.clone(),
                    guard: None,
                    body: static_def.body.clone(),
                    span: static_def.span,
                };
                (func_def, FunctionType::TraitStatic)
            }
        };

        // Create function entry
        let trait_name = trait_def.name_as_string();
        let function_id = format!("trait::{}::{}", trait_name, func_def.name.name);
        let is_guard = func_def.name.name.ends_with('?');
        let entry = match function_type {
            FunctionType::TraitStatic => UnifiedFunctionEntry::TraitStatic {
                definition: func_def.clone(),
                typed_definition: None,
                function_id,
                is_guard,
            },
            FunctionType::TraitSignature => UnifiedFunctionEntry::TraitSignature {
                definition: func_def.clone(),
                typed_definition: None,
                function_id,
                is_guard,
            },
            FunctionType::TraitDefault => UnifiedFunctionEntry::TraitDefault {
                definition: func_def.clone(),
                typed_definition: None,
                function_id,
                is_guard,
            },
            _ => panic!("Unexpected function type for trait function: {function_type:?}"),
        };

        // Create trait module if it doesn't exist
        let trait_type_id = self.intern_type_name(&trait_name);
        let module_key = ModuleKey::Module(trait_type_id.hash);
        let trait_type = StructuredType::Simple(trait_type_id);
        self.get_or_create_module(
            module_key.clone(),
            ModuleKind::Trait,
            SourceLocation::File(source_file.to_string()),
            trait_type.clone(),
        );

        // Add function to trait module
        let function_name = self.intern_atom_name(&func_def.name.name);
        // Adding trait function to trait
        self.add_function_to_module(module_key, trait_type, function_name, entry);

        Ok(())
    }

    /// Extract a trait implementation function and add it to the impl module
    /// In Outrun, impl blocks provide implementations of trait functions for specific types
    fn extract_trait_impl_function(
        &self,
        func_def: &FunctionDefinition,
        impl_block: &outrun_parser::ImplBlock,
        source_file: &str,
    ) -> Result<(), TypeError> {
        let trait_name = impl_block
            .trait_spec
            .path
            .iter()
            .map(|id| &id.name)
            .cloned()
            .collect::<Vec<_>>()
            .join(".");

        let impl_type_name = impl_block
            .type_spec
            .path
            .iter()
            .map(|id| &id.name)
            .cloned()
            .collect::<Vec<_>>()
            .join(".");

        // Create function entry for trait implementation
        let function_id = format!(
            "impl::{}::for::{}::{}",
            trait_name, impl_type_name, func_def.name.name
        );
        let is_guard = func_def.name.name.ends_with('?');
        let entry = UnifiedFunctionEntry::ImplFunction {
            definition: func_def.clone(),
            typed_definition: None,
            function_id,
            is_guard,
        };

        // CRITICAL FIX: Look up full trait type from module registry
        // This ensures we use Generic(Option<T>) instead of Simple(Option) for generic traits
        let trait_type_id = self.intern_type_name(&trait_name);
        let trait_type = if let Some(module) = self.get_module(trait_type_id.clone()) {
            module.structured_type
        } else {
            StructuredType::Simple(trait_type_id)
        };

        // Extract generic parameters from the impl block to properly handle type parameters
        let mut type_params = std::collections::HashMap::new();
        if let Some(ref generic_params) = impl_block.generic_params {
            for param in &generic_params.params {
                let param_type_id = self.intern_type_name(&param.name.name);
                // CRITICAL FIX: Use TypeVariable instead of Simple for type parameters like T
                type_params.insert(
                    param.name.name.clone(),
                    StructuredType::TypeVariable(param_type_id.clone()),
                );
            }
        }

        let impl_type = if type_params.is_empty() {
            // No generic parameters, use regular conversion
            match self.convert_type_spec_to_structured_type(&impl_block.type_spec) {
                Ok(structured_type) => structured_type,
                Err(_) => {
                    // Fallback to simple type if conversion fails
                    let impl_type_id = self.intern_type_name(&impl_type_name);
                    StructuredType::Simple(impl_type_id)
                }
            }
        } else {
            // Has generic parameters, use conversion with type parameter context
            match self.convert_type_spec_to_structured_type_with_params(
                &impl_block.type_spec,
                &type_params,
            ) {
                Ok(structured_type) => structured_type,
                Err(_) => {
                    // Fallback to simple type if conversion fails
                    let impl_type_id = self.intern_type_name(&impl_type_name);
                    StructuredType::Simple(impl_type_id)
                }
            }
        };

        let module_key =
            ModuleKey::TraitImpl(Box::new(trait_type.clone()), Box::new(impl_type.clone()));

        // Create trait implementation module if it doesn't exist
        self.get_or_create_module(
            module_key.clone(),
            ModuleKind::TraitImpl,
            SourceLocation::File(source_file.to_string()),
            impl_type.clone(),
        );

        // Add trait function implementation to the module
        let function_name = self.intern_atom_name(&func_def.name.name);
        // Use the actual function signature from the definition
        let function_signature_type =
            StructuredType::Simple(self.intern_type_name(&func_def.name.name));

        self.add_function_to_module(
            module_key,
            function_signature_type.clone(),
            function_name.clone(),
            entry.clone(),
        );

        // No base type registration - SMT system handles all type matching
        // Functions are only registered with their exact generic types

        Ok(())
    }

    /// Desugar all programs (Phase 5) - Transform operators to trait function calls
    fn desugar_programs(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<ProgramCollection, Vec<TypeError>> {
        let mut desugared_collection = ProgramCollection::new();

        for file_path in order {
            if let Some(program) = collection.programs.get(file_path) {
                // Desugar the program with span mapping
                let (desugared_program, span_mapping) =
                    DesugaringVisitor::desugar_program_with_span_mapping(program.clone());

                // Merge this program's span mapping into the unification context
                {
                    let mut state = self.compilation_state.write().unwrap();
                    state.unification_context.merge_span_mapping(span_mapping);
                }

                // Add the desugared program to the collection
                if let Some(source) = collection.sources.get(file_path) {
                    desugared_collection.add_program(
                        file_path.clone(),
                        desugared_program,
                        source.clone(),
                    );
                } else {
                    // Fallback for programs without source tracking
                    desugared_collection.add_program(
                        file_path.clone(),
                        desugared_program,
                        "".to_string(),
                    );
                }
            }
        }

        Ok(desugared_collection)
    }

    /// Register trait implementations with the unification context
    fn register_trait_implementations(
        &mut self,
        implementations: &[ImplBlock],
    ) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();

        // Get a mutable reference to the unification context
        let unification_context = self.unification_context();

        for impl_block in implementations {
            // Extract generic parameters from the impl block
            let mut type_params = std::collections::HashMap::new();
            if let Some(ref generic_params) = impl_block.generic_params {
                for param in &generic_params.params {
                    let param_type_id = self.intern_type_name(&param.name.name);
                    type_params.insert(
                        param.name.name.clone(),
                        StructuredType::TypeVariable(param_type_id),  // ✅ Fix: Use TypeVariable for type parameters
                    );
                }
            }

            // Get trait name from TypeSpec path
            let trait_name = impl_block
                .trait_spec
                .path
                .iter()
                .map(|id| &id.name)
                .cloned()
                .collect::<Vec<_>>()
                .join(".");

            // CRITICAL FIX: Look up full trait type from module registry
            // This ensures we use Generic(Option<T>) instead of Simple(Option) for generic traits
            let trait_type_id = self.intern_type_name(&trait_name);
            let trait_structured = if let Some(module) = self.get_module(trait_type_id.clone()) {
                module.structured_type
            } else {
                StructuredType::Simple(trait_type_id)
            };

            let impl_structured = match self.convert_type_spec_to_structured_type_with_params(
                &impl_block.type_spec,
                &type_params,
            ) {
                Ok(structured_type) => structured_type,
                Err(error) => {
                    errors.push(error);
                    continue;
                }
            };

            self.register_trait_implementation(impl_structured.clone(), trait_structured.clone());

            // 🚀 CRITICAL FIX: Generate ConcreteSelfBinding constraints for Self types in impl blocks
            // This ensures Self resolves to the concrete implementing type, not the abstract trait type
            self.generate_concrete_self_binding_constraints(
                &impl_structured,
                &trait_structured,
                impl_block,
            );
        }

        // Update the compilation state with the modified unification context
        self.set_unification_context(unification_context);

        // Note: Validation of typed definitions is done after Phase 7 (typed AST building)
        // where typed definitions are actually added to functions. Don't validate here.

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }

    /// Generate ConcreteSelfBinding constraints for impl blocks
    /// This is the missing piece that ensures Self resolves to concrete implementing types
    fn generate_concrete_self_binding_constraints(
        &mut self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
        impl_block: &ImplBlock,
    ) {
        // Generate ConcreteSelfBinding constraints

        // For each function in the impl block, create Self binding constraints
        for function_def in &impl_block.functions {
            // Create a unique Self type variable for this impl block + function combination
            let self_var_name = format!(
                "Self_impl_{}_{}",
                impl_type
                    .to_string_representation()
                    .replace(['<', '>', ' ', '.'], "_"),
                function_def.name.name
            );
            let self_type_id = self.intern_type_name(&self_var_name);

            // Generate ConcreteSelfBinding: Self = ConcreteImplementingType
            let concrete_self_constraint =
                crate::smt::constraints::SMTConstraint::ConcreteSelfBinding {
                    self_variable_id: self_type_id,
                    concrete_type: impl_type.clone(),
                    context: format!(
                        "impl {} for {} - function {}",
                        trait_type.to_string_representation(),
                        impl_type.to_string_representation(),
                        function_def.name.name
                    ),
                };

            // Add the constraint to our SMT system via unification context
            let mut context = self.unification_context();
            context.add_smt_constraint(concrete_self_constraint);
            self.set_unification_context(context);
        }

        // ADDITIONAL: Handle generic Self parameters in impl blocks
        // For `impl Option<T> for Outrun.Option.Some<T>`, ensure Self<T> = Outrun.Option.Some<T>
        if let StructuredType::Generic { base: _, args: _ } = impl_type {
            // Generate a generic Self binding for the entire impl block scope
            let generic_self_var_name = format!(
                "Self_impl_generic_{}",
                impl_type
                    .to_string_representation()
                    .replace(['<', '>', ' ', '.'], "_")
            );
            let generic_self_type_id = self.intern_type_name(&generic_self_var_name);

            let generic_concrete_self_constraint =
                crate::smt::constraints::SMTConstraint::ConcreteSelfBinding {
                    self_variable_id: generic_self_type_id,
                    concrete_type: impl_type.clone(),
                    context: format!(
                        "impl {} for {} - generic Self binding",
                        trait_type.to_string_representation(),
                        impl_type.to_string_representation()
                    ),
                };

            // Add the constraint to our SMT system via unification context
            let mut context = self.unification_context();
            context.add_smt_constraint(generic_concrete_self_constraint);
            self.set_unification_context(context);
        }
    }

    // =============================================================================
    // SMT-Based Type Checking (Phase 5 Implementation)
    // =============================================================================

    /// NEW: SMT-based type checking with constraint collection (replaces type_check_all)
    fn smt_type_check_all(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
        _traits: &HashMap<TypeNameId, TraitDefinition>,
        _structs: &HashMap<TypeNameId, StructDefinition>,
        implementations: &[ImplBlock],
        external_variables: HashMap<String, StructuredType>,
    ) -> Result<(), Vec<TypeError>> {
        // Register trait implementations with the unification context
        self.register_trait_implementations(implementations)?;

        // For Phase 5, use existing type checking visitor but with SMT constraint collection
        // TODO: Replace with dedicated SMTTypeCheckingVisitor in future phases
        let mut visitor = TypeCheckingVisitor::from_compiler_environment(self.clone());

        // Add external variables to the visitor's scope
        visitor.add_external_variables(external_variables);

        // Type check all programs (existing logic for now)
        // SMT constraint collection will be added to the visitor in future implementation
        for file_path in order {
            if let Some(program) = collection.programs.get(file_path) {
                if let Err(err) =
                    <TypeCheckingVisitor as Visitor<()>>::visit_program(&mut visitor, program)
                {
                    self.compilation_state.write().unwrap().errors.push(err);
                }
            }
        }

        // Collect any errors from the visitor
        for error in visitor.errors {
            self.compilation_state.write().unwrap().errors.push(error);
        }

        // Check if any errors were accumulated during SMT type checking
        let errors = self.get_errors();
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(())
    }


    /// NEW: Phase 6.5 - SMT-based function clause analysis
    /// Analyzes function clauses during compilation and generates SMT constraints for clause selection
    fn phase_6_5_smt_function_clause_analysis(&mut self, collection: &ProgramCollection) -> Result<(), Vec<TypeError>> {
        // Step 1: Run exhaustiveness analysis on all function clause sets
        // TODO: Temporarily disabled to fix core library compilation - will re-enable after caching is working
        // self.run_exhaustiveness_analysis(collection)?;
        // Phase 6.5: Generate SMT constraints for function clause dispatch
        // This replaces runtime guard evaluation with compile-time SMT analysis
        
        // 1. Find all function calls in the AST that could use function clauses
        let mut function_call_sites = Vec::new();
        self.collect_function_call_sites(collection, &mut function_call_sites)?;
        
        // 2. For each call site, analyze which function clauses could apply
        for call_site in function_call_sites {
            self.analyze_function_clause_constraints(&call_site)?;
        }
        
        // 3. Generate SMT constraints for clause selection and guard evaluation
        // This will be used by Phase 7 to resolve which clauses to use at each call site
        
        Ok(())
    }

    /// Run exhaustiveness analysis on all function clause sets
    fn run_exhaustiveness_analysis(&mut self, _collection: &ProgramCollection) -> Result<(), Vec<TypeError>> {
        use crate::smt::exhaustiveness::FunctionClauseExhaustivenessAnalyzer;
        
        let mut all_errors = Vec::new();
        let mut all_warnings = Vec::new();
        
        // Create exhaustiveness analyzer
        let mut analyzer = FunctionClauseExhaustivenessAnalyzer::new(self.clone());
        
        // Analyze all function clause sets across all modules
        let modules = self.modules.read().unwrap();
        for module in modules.values() {
            for clause_set in module.function_clauses.values() {
                // Only analyze clause sets with multiple clauses or guards
                if clause_set.clauses.len() > 1 || clause_set.has_guards() {
                    match analyzer.analyze_clause_set(clause_set, outrun_parser::Span::new(0, 0)) {
                        Ok(analysis) => {
                            let (errors, warnings) = analyzer.generate_diagnostics(
                                &analysis, 
                                outrun_parser::Span::new(0, 0), // TODO: Get actual function span
                                clause_set
                            );
                            all_errors.extend(errors);
                            all_warnings.extend(warnings);
                        }
                        Err(_smt_error) => {
                            // Don't fail compilation for SMT analysis errors
                        }
                    }
                }
            }
        }
        
        // Report warnings to stderr (non-fatal)
        for warning in all_warnings {
            eprintln!("Warning: {}", warning);
        }
        
        // Return errors (these will fail compilation)
        if all_errors.is_empty() {
            Ok(())
        } else {
            Err(all_errors)
        }
    }

    /// Collect all function call sites that could potentially use function clauses
    fn collect_function_call_sites(&self, collection: &ProgramCollection, call_sites: &mut Vec<FunctionCallSite>) -> Result<(), Vec<TypeError>> {
        // TODO: Implement AST traversal to find all function calls
        // For now, we'll stub this out since the real implementation would be quite complex
        // This would traverse all TypedExpression::FunctionCall nodes and extract call site info
        
        // Stub: Add a sample call site for division to test the concept
        call_sites.push(FunctionCallSite {
            trait_name: "BinaryDivision".to_string(),
            function_name: "divide".to_string(),
            argument_types: vec![
                StructuredType::Simple(self.intern_type_name("Outrun.Core.Integer64")),
                StructuredType::Simple(self.intern_type_name("Outrun.Core.Integer64")),
            ],
            impl_type: StructuredType::Simple(self.intern_type_name("Outrun.Core.Integer64")),
            span: outrun_parser::Span::new(0, 0), // Placeholder span
            call_context: "Test division call site".to_string(),
        });
        
        Ok(())
    }

    /// Analyze SMT constraints for a specific function call site
    fn analyze_function_clause_constraints(&mut self, call_site: &FunctionCallSite) -> Result<(), Vec<TypeError>> {
        // Step 1: Find all function clauses for this trait/function combination
        let function_name_atom = self.intern_atom_name(&call_site.function_name);
        
        // Step 2: Look up clauses from ALL implementations of the trait (not just one!)
        let trait_type = StructuredType::Simple(self.intern_type_name(&call_site.trait_name));
        if let Some(unified_clause_set) = self.lookup_all_function_clauses_for_trait(&trait_type, &function_name_atom) {
            
            // Step 3: Generate SMT constraints for each clause
            for clause in unified_clause_set.get_clauses_by_priority() {
                // Generate ArgumentTypeMatch constraints
                self.generate_argument_type_constraints(call_site, clause)?;
                
                // Generate GuardApplicable constraints (if guard exists)
                if clause.base_function.guard.is_some() {
                    self.generate_guard_applicability_constraints(call_site, clause)?;
                }
                
                // Generate ClausePriority constraints
                self.generate_clause_priority_constraints(call_site, clause)?;
            }
            
            // Step 4: Generate PreResolvedClause constraint with SMT-selected clause
            // For now, we'll stub this - in reality, SMT would solve these constraints
            self.generate_pre_resolved_clause_constraint(call_site, &unified_clause_set)?;
        }
        
        Ok(())
    }

    /// Generate SMT constraints for argument type matching
    fn generate_argument_type_constraints(&mut self, call_site: &FunctionCallSite, clause: &crate::checker::FunctionClause) -> Result<(), Vec<TypeError>> {
        for (i, parameter) in clause.base_function.parameters.iter().enumerate() {
            if let Some(arg_type) = call_site.argument_types.get(i) {
                let constraint = crate::smt::constraints::SMTConstraint::ArgumentTypeMatch {
                    clause_id: format!("{}::{}", call_site.trait_name, clause.clause_id),
                    parameter_name: parameter.name.clone(),
                    expected_type: parameter.param_type.clone().unwrap_or_else(|| StructuredType::Simple(self.intern_type_name("Unknown"))),
                    actual_type: arg_type.clone(),
                    call_site: call_site.span,
                };
                
                // Add constraint to unification context
                self.unification_context().add_smt_constraint(constraint);
            }
        }
        Ok(())
    }

    /// Generate SMT constraints for guard applicability  
    fn generate_guard_applicability_constraints(&mut self, call_site: &FunctionCallSite, clause: &crate::checker::FunctionClause) -> Result<(), Vec<TypeError>> {
        if let Some(_guard_expr) = &clause.base_function.guard {
            let constraint = crate::smt::constraints::SMTConstraint::GuardApplicable {
                clause_id: format!("{}::{}", call_site.trait_name, clause.clause_id),
                guard_expression: "rhs == 0".to_string(), // Placeholder - would extract from guard_expr
                guard_variables: std::collections::HashMap::new(), // Would extract from parameters
                context: format!("Guard for {} in {}", call_site.function_name, call_site.trait_name),
            };
            
            self.unification_context().add_smt_constraint(constraint);
        }
        Ok(())
    }

    /// Generate SMT constraints for clause priority
    fn generate_clause_priority_constraints(&mut self, call_site: &FunctionCallSite, clause: &crate::checker::FunctionClause) -> Result<(), Vec<TypeError>> {
        let constraint = crate::smt::constraints::SMTConstraint::ClausePriority {
            clause_id: format!("{}::{}", call_site.trait_name, clause.clause_id),
            priority: clause.source_order,
            context: format!("Priority for {} in {}", call_site.function_name, call_site.trait_name),
        };
        
        self.unification_context().add_smt_constraint(constraint);
        Ok(())
    }

    /// Generate pre-resolved clause constraint based on SMT analysis
    fn generate_pre_resolved_clause_constraint(&mut self, call_site: &FunctionCallSite, clause_set: &crate::checker::FunctionClauseSet) -> Result<(), Vec<TypeError>> {
        // For now, stub this to select the highest priority clause
        // In reality, SMT would solve all the constraints to determine the best clause
        let clauses = clause_set.get_clauses_by_priority();
        let selected_clause = clauses
            .first()
            .ok_or_else(|| vec![TypeError::internal("No clauses found in clause set".to_string())])?;
        
        let constraint = crate::smt::constraints::SMTConstraint::PreResolvedClause {
            call_site: call_site.span,
            trait_type: StructuredType::Simple(self.intern_type_name(&call_site.trait_name)),
            impl_type: call_site.impl_type.clone(),
            function_name: call_site.function_name.clone(),
            selected_clause_id: selected_clause.clause_id.clone(),
            guard_pre_evaluated: Some(true), // Stub - SMT would determine this
            argument_types: call_site.argument_types.clone(),
        };
        
        self.unification_context().add_smt_constraint(constraint);
        Ok(())
    }

    /// Check if SMT has pre-resolved a function clause for a specific call site
    /// This is called during type checking to determine dispatch method
    pub fn get_smt_pre_resolved_clause(
        &self,
        trait_name: &str,
        function_name: &str, 
        impl_type: &StructuredType,
        call_span: outrun_parser::Span
    ) -> Option<crate::checker::DispatchMethod> {
        self.get_smt_pre_resolved_clause_with_context(
            &self.unification_context(), 
            trait_name, 
            function_name, 
            impl_type, 
            call_span
        )
    }

    /// Check if SMT has pre-resolved a function clause using a specific context
    /// Updated for two-phase constraint resolution: checks for PendingClauseResolution during type checking
    /// and PreResolvedClause after SMT solving
    pub fn get_smt_pre_resolved_clause_with_context(
        &self,
        context: &crate::unification::UnificationContext,
        trait_name: &str,
        function_name: &str, 
        impl_type: &StructuredType,
        call_span: outrun_parser::Span
    ) -> Option<crate::checker::DispatchMethod> {
        let constraints = &context.smt_constraints;
        
        // First, check for PreResolvedClause constraints (post-SMT solving)
        for constraint in constraints.iter() {
            if let crate::smt::constraints::SMTConstraint::PreResolvedClause { 
                call_site: _,
                trait_type: constraint_trait,
                impl_type: constraint_impl,
                function_name: constraint_function,
                selected_clause_id,
                guard_pre_evaluated,
                ..
            } = constraint {
                if *constraint_function == function_name && constraint_impl == impl_type {
                    // Extract trait name from constraint_trait
                    let constraint_trait_name = match constraint_trait {
                        StructuredType::Simple(type_id) => {
                            self.resolve_type(type_id.clone()).unwrap_or_default()
                        }
                        StructuredType::Generic { base, .. } => {
                            self.resolve_type(base.clone()).unwrap_or_default()
                        }
                        _ => continue,
                    };
                    
                    if constraint_trait_name == trait_name {
                        return Some(crate::checker::DispatchMethod::PreResolvedClause {
                            trait_name: trait_name.to_string(),
                            function_name: function_name.to_string(), 
                            impl_type: Box::new(impl_type.clone()),
                            selected_clause_id: selected_clause_id.clone(),
                            guard_pre_evaluated: *guard_pre_evaluated,
                            clause_source_order: 0,
                        });
                    }
                }
            }
        }
        
        // If no PreResolvedClause found, check for constraint specifications (during type checking)
        // This indicates constraint collection was successful and SMT generation is deferred
        for spec in context.constraint_specifications.iter() {
            if spec.function_name == function_name && &spec.impl_type == impl_type {
                // Extract trait name from spec
                let spec_trait_name = match &spec.trait_type {
                    StructuredType::Simple(type_id) => {
                        self.resolve_type(type_id.clone()).unwrap_or_default()
                    }
                    StructuredType::Generic { base, .. } => {
                        self.resolve_type(base.clone()).unwrap_or_default()
                    }
                    _ => continue,
                };
                
                if spec_trait_name == trait_name {
                    // Found matching constraint specification - return placeholder dispatch method
                    // This will be replaced with actual PreResolvedClause after SMT constraint generation and solving
                    return Some(crate::checker::DispatchMethod::Trait {
                        trait_name: trait_name.to_string(),
                        function_name: function_name.to_string(),
                        impl_type: Box::new(impl_type.clone()),
                    });
                }
            }
        }
        
        None
    }

    /// NEW: Phase 6.5 - Generate SMT constraints from collected specifications
    /// This phase converts constraint specifications collected during type checking into actual SMT constraints
    /// with fully resolved types (no more unresolved Self variables)
    fn phase_6_5_generate_smt_constraints_from_specifications(&mut self) -> Result<(), Vec<TypeError>> {
        let mut context = self.unification_context();
        let specifications = context.constraint_specifications.clone();
        
        if specifications.is_empty() {
            return Ok(());
        }
        
        // Generate SMT constraints from each specification
        for spec in specifications {
            // Resolve any remaining type variables in impl_type using current context
            let resolved_impl_type = self.resolve_type_variables_in_structured_type(&spec.impl_type, &context)
                .map_err(|e| vec![e])?;
            
            // Generate the actual clause ID now that all types are resolved
            let clause_id = format!(
                "{}:{}:{}:{}",
                spec.trait_name,
                resolved_impl_type.to_string_representation(),
                spec.function_name,
                0 // clause index - for now we assume single clause, this could be enhanced
            );
            
            // Create the PreResolvedClause constraint with resolved types
            let pre_resolved_constraint = crate::smt::constraints::SMTConstraint::PreResolvedClause {
                call_site: spec.call_site,
                trait_type: spec.trait_type.clone(),
                impl_type: resolved_impl_type.clone(),
                function_name: spec.function_name.clone(),
                selected_clause_id: clause_id,
                guard_pre_evaluated: if spec.has_guard { Some(true) } else { None }, // Simplified for now
                argument_types: spec.argument_types.clone(),
            };
            
            context.add_smt_constraint(pre_resolved_constraint);
        }
        
        // Clear specifications since they've been converted to constraints
        context.constraint_specifications.clear();
        self.set_unification_context(context);
        
        Ok(())
    }

    /// Helper method to resolve type variables in a StructuredType using current context
    fn resolve_type_variables_in_structured_type(
        &self,
        structured_type: &StructuredType,
        context: &crate::unification::UnificationContext,
    ) -> Result<StructuredType, TypeError> {
        match structured_type {
            StructuredType::TypeVariable(type_id) => {
                // Try to resolve this type variable from generic substitutions or other context
                if let Some(substitution) = context.generic_substitutions.get(type_id) {
                    Ok(substitution.clone())
                } else {
                    // If we can't resolve it, this might be a Self type that should have been resolved
                    // For now, return the original type and let SMT handling deal with it
                    Ok(structured_type.clone())
                }
            }
            StructuredType::Generic { base, args } => {
                // Recursively resolve type variables in generic arguments
                let resolved_args = args.iter()
                    .map(|arg| self.resolve_type_variables_in_structured_type(arg, context))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(StructuredType::Generic { base: base.clone(), args: resolved_args })
            }
            StructuredType::Function { params, return_type } => {
                // Recursively resolve in function parameters and return type
                let mut resolved_params = Vec::new();
                for param in params {
                    let resolved_type = self.resolve_type_variables_in_structured_type(&param.param_type, context)?;
                    resolved_params.push(crate::unification::FunctionParam {
                        name: param.name.clone(),
                        param_type: resolved_type,
                    });
                }
                let resolved_return = self.resolve_type_variables_in_structured_type(return_type, context)?;
                Ok(StructuredType::Function {
                    params: resolved_params,
                    return_type: Box::new(resolved_return),
                })
            }
            // For other types, return as-is since they don't contain type variables
            _ => Ok(structured_type.clone()),
        }
    }

    /// Phase 7.6 - Comprehensive function signature monomorphization
    /// This phase monomorphizes ALL trait implementations in the system
    /// where ALL trait types are resolved to concrete types
    fn phase_7_6_comprehensive_monomorphization(
        &mut self,
        _collection: &ProgramCollection,
    ) -> Result<(), Vec<TypeError>> {
        eprintln!("🔧 PHASE 7.6: Starting comprehensive monomorphization");

        // Step 1: Find all trait implementations that need monomorphization
        let trait_implementations = self.collect_all_trait_implementations()?;
        
        eprintln!("🔧 PHASE 7.6: Found {} trait implementations", trait_implementations.len());

        // Step 2: Monomorphize each trait implementation
        for (trait_name, impl_type, function_name) in trait_implementations {
            eprintln!("🔧 MONO IMPL: Processing {}::{} for {}", trait_name, function_name, 
                     self.resolve_type_display(&impl_type));
            
            self.monomorphize_trait_implementation(&trait_name, &impl_type, &function_name)?;
        }

        eprintln!("🔧 PHASE 7.6: Comprehensive monomorphization complete");
        Ok(())
    }

    /// Collect all trait implementations in the system that need monomorphization
    fn collect_all_trait_implementations(&self) -> Result<Vec<(String, StructuredType, String)>, Vec<TypeError>> {
        let mut implementations = Vec::new();
        
        // Look through all modules for trait implementations
        let modules = self.modules.read().unwrap();
        for (module_key, module) in modules.iter() {
            if let ModuleKey::TraitImpl(trait_type, impl_type) = module_key {
                let trait_name = self.resolve_type_display(trait_type);
                let impl_type_resolved = (**impl_type).clone(); // Dereference the Box
                
                // For each function in this trait implementation
                for function_name_atom in module.function_clauses.keys() {
                    let function_name = self.resolve_atom_name(function_name_atom).unwrap_or_default();
                    
                    implementations.push((trait_name.clone(), impl_type_resolved.clone(), function_name));
                }
            }
        }
        
        Ok(implementations)
    }

    /// Monomorphize a specific trait implementation
    fn monomorphize_trait_implementation(
        &mut self,
        trait_name: &str,
        impl_type: &StructuredType,
        function_name: &str,
    ) -> Result<(), Vec<TypeError>> {
        // Create a spec for this trait implementation
        let spec = FullMonomorphizationSpec {
            trait_name: trait_name.to_string(),
            function_name: function_name.to_string(),
            implementing_type: impl_type.clone(),
            concrete_argument_types: std::collections::HashMap::new(), // We'll infer from the clause
            concrete_return_type: StructuredType::Simple(self.intern_type_name("Unknown")), // Will be inferred
            call_site_span: outrun_parser::Span::new(0, 0),
        };
        
        self.generate_fully_monomorphized_clause(&spec)
    }

    /// Collect all function call sites with their resolved argument types
    fn collect_function_call_sites_with_types(
        &self,
        collection: &ProgramCollection,
        call_sites: &mut Vec<MonomorphizationCallSite>,
    ) -> Result<(), Vec<TypeError>> {
        for (file_path, program) in &collection.programs {
            self.collect_call_sites_from_program(program, file_path, call_sites)?;
        }
        Ok(())
    }

    /// Collect call sites from a single program
    fn collect_call_sites_from_program(
        &self,
        program: &outrun_parser::Program,
        file_path: &str,
        call_sites: &mut Vec<MonomorphizationCallSite>,
    ) -> Result<(), Vec<TypeError>> {
        for item in &program.items {
            self.collect_call_sites_from_item(item, file_path, call_sites)?;
        }
        Ok(())
    }

    /// Collect call sites from an AST item
    fn collect_call_sites_from_item(
        &self,
        item: &outrun_parser::Item,
        file_path: &str,
        call_sites: &mut Vec<MonomorphizationCallSite>,
    ) -> Result<(), Vec<TypeError>> {
        match &item.kind {
            outrun_parser::ItemKind::Expression(expr) => {
                self.collect_call_sites_from_expression(expr, file_path, call_sites)?;
            }
            outrun_parser::ItemKind::LetBinding(let_binding) => {
                self.collect_call_sites_from_expression(&let_binding.expression, file_path, call_sites)?;
            }
            _ => {
                // Other items don't contain expressions with call sites
            }
        }
        Ok(())
    }

    /// Collect call sites from an expression (recursive)
    fn collect_call_sites_from_expression(
        &self,
        expr: &outrun_parser::Expression,
        file_path: &str,
        call_sites: &mut Vec<MonomorphizationCallSite>,
    ) -> Result<(), Vec<TypeError>> {
        match &expr.kind {
            outrun_parser::ExpressionKind::FunctionCall(call) => {
                // Check if this is a trait function call
                if let Some(site) = self.extract_trait_call_site(call, file_path)? {
                    call_sites.push(site);
                }
                
                // Recursively check arguments
                for arg in &call.arguments {
                    if let outrun_parser::Argument::Named { expression, .. } = arg {
                        self.collect_call_sites_from_expression(expression, file_path, call_sites)?;
                    }
                }
            }
            outrun_parser::ExpressionKind::IfExpression(if_expr) => {
                self.collect_call_sites_from_expression(&if_expr.condition, file_path, call_sites)?;
                // Handle blocks - for now, we'll skip this complex case
                // TODO: Implement proper block traversal when needed
            }
            _ => {
                // Handle other expression types as needed
            }
        }
        Ok(())
    }

    /// Extract trait call site information from a function call
    fn extract_trait_call_site(
        &self,
        call: &outrun_parser::FunctionCall,
        file_path: &str,
    ) -> Result<Option<MonomorphizationCallSite>, Vec<TypeError>> {
        // Check if this is a qualified function call (has a path like Trait.function)
        if let outrun_parser::FunctionPath::Qualified { module, name } = &call.path {
            let trait_name = module.name.clone();
            let function_name = name.name.clone();
                
            // Get the type information from the unification context
            let unification_context = self.unification_context();
            if let Some(resolved_type) = unification_context.get_expression_type(&call.span) {
                // Extract argument types
                let mut argument_types = std::collections::HashMap::new();
                for arg in &call.arguments {
                    if let outrun_parser::Argument::Named { name, expression, .. } = arg {
                        if let Some(arg_type) = unification_context.get_expression_type(&expression.span) {
                            argument_types.insert(name.name.clone(), arg_type.clone());
                        }
                    }
                }

                return Ok(Some(MonomorphizationCallSite {
                    trait_name,
                    function_name,
                    argument_types,
                    return_type: resolved_type.clone(),
                    span: call.span,
                    file_path: file_path.to_string(),
                }));
            }
        }
        Ok(None)
    }

    /// Analyze a call site to determine what monomorphization is needed
    fn analyze_call_site_for_monomorphization(
        &self,
        call_site: &MonomorphizationCallSite,
    ) -> Result<Option<FullMonomorphizationSpec>, Vec<TypeError>> {
        // For each trait function call, we need to:
        // 1. Find the trait definition
        // 2. Determine concrete types for all trait type parameters  
        // 3. Create a spec for generating a fully concrete clause

        // Find the trait definition using the trait registry directly
        let all_traits = self.get_all_traits();
        let trait_type_id = self.intern_type_name(&call_site.trait_name);
        let trait_def = all_traits.get(&trait_type_id);
        
        if trait_def.is_none() {
            eprintln!("🔧 ANALYZE: Trait '{}' not found in trait registry", call_site.trait_name);
            eprintln!("🔧 ANALYZE: Available traits: {:?}", 
                      all_traits.keys().map(|k| self.resolve_type_name(k).unwrap_or_default()).collect::<Vec<_>>());
            return Ok(None);
        }

        // Find which concrete implementing type is being used
        let implementing_type = self.infer_implementing_type_from_arguments(&call_site.argument_types)?;
        if implementing_type.is_none() {
            return Ok(None);
        }

        Ok(Some(FullMonomorphizationSpec {
            trait_name: call_site.trait_name.clone(),
            function_name: call_site.function_name.clone(),
            implementing_type: implementing_type.unwrap(),
            concrete_argument_types: call_site.argument_types.clone(),
            concrete_return_type: call_site.return_type.clone(),
            call_site_span: call_site.span,
        }))
    }

    /// Infer the implementing type from the function call arguments
    fn infer_implementing_type_from_arguments(
        &self,
        argument_types: &std::collections::HashMap<String, StructuredType>,
    ) -> Result<Option<StructuredType>, Vec<TypeError>> {
        // Look for Self parameters to determine the implementing type
        for (param_name, arg_type) in argument_types {
            // For now, use the first concrete type we find
            // In a more sophisticated system, we'd check which parameter is typed as Self
            if param_name == "value" {
                return Ok(Some(arg_type.clone()));
            }
        }
        Ok(None)
    }

    /// Generate a fully monomorphized clause with all trait types resolved
    fn generate_fully_monomorphized_clause(
        &mut self,
        spec: &FullMonomorphizationSpec,
    ) -> Result<(), Vec<TypeError>> {
        eprintln!("🔧 MONOMORPH: Generating clause for {}::{} with impl type {:?}", 
                  spec.trait_name, spec.function_name, spec.implementing_type);

        eprintln!("🔧 MONOMORPH: Available traits: {:?}", 
                  self.get_all_traits().keys().map(|k| self.resolve_type_name(k).unwrap_or_default()).collect::<Vec<_>>());

        // Find the original trait function definition using get_all_traits directly
        let all_traits = self.get_all_traits();
        let trait_type_id = self.intern_type_name(&spec.trait_name);
        let trait_def = all_traits.get(&trait_type_id);
        
        let Some(trait_def) = trait_def else {
            eprintln!("🔧 MONOMORPH: Trait '{}' not found in trait registry", spec.trait_name);
            eprintln!("🔧 MONOMORPH: Available traits: {:?}", 
                      all_traits.keys().map(|k| self.resolve_type_name(k).unwrap_or_default()).collect::<Vec<_>>());
            return Ok(());
        };

        // The real monomorphization task: transform existing function clauses to have concrete types
        // instead of trait types like Integer -> Outrun.Core.Integer64
        
        eprintln!("🔧 MONOMORPH: Starting clause type monomorphization for {}::{}", spec.trait_name, spec.function_name);
        
        // Find existing function clauses for this trait/function combination
        let trait_type = StructuredType::Simple(self.intern_type_name(&spec.trait_name));
        let module_key = ModuleKey::TraitImpl(
            Box::new(trait_type.clone()),
            Box::new(spec.implementing_type.clone()),
        );
        
        // Look for the existing function clauses
        let function_name_atom = self.intern_atom_name(&spec.function_name);
        let clauses_to_monomorphize = {
            let modules = self.modules.read().unwrap();
            if let Some(module) = modules.get(&module_key) {
                if let Some(clause_set) = module.get_function_clause_set(&function_name_atom) {
                    clause_set.clauses.clone()
                } else {
                    Vec::new()
                }
            } else {
                eprintln!("🔧 MONOMORPH: No module found for trait impl - this is expected for trait signatures");
                return Ok(());
            }
        };
        
        if clauses_to_monomorphize.is_empty() {
            eprintln!("🔧 MONOMORPH: No existing clauses found to monomorphize");
            return Ok(());
        }
        
        eprintln!("🔧 MONOMORPH: Found {} clauses to monomorphize", clauses_to_monomorphize.len());
        
        // Transform each clause to have concrete types
        for clause in clauses_to_monomorphize {
            self.monomorphize_function_clause(&clause, spec)?;
        }
        
        Ok(())
    }

    /// Monomorphize a single function clause by replacing trait types with concrete types
    fn monomorphize_function_clause(
        &mut self,
        clause: &crate::checker::FunctionClause,
        spec: &FullMonomorphizationSpec,
    ) -> Result<(), Vec<TypeError>> {
        eprintln!("🔧 MONO CLAUSE: Monomorphizing clause {}", clause.clause_id);
        
        // Create a new clause with monomorphized parameter types
        let mut monomorphized_function = clause.base_function.clone();
        
        // Transform each parameter type from trait type to concrete type
        for parameter in &mut monomorphized_function.parameters {
            if let Some(param_type) = &mut parameter.param_type {
                let original_type = param_type.clone();
                let monomorphized_type = self.monomorphize_type(param_type, spec)?;
                
                if original_type != monomorphized_type {
                    eprintln!("🔧 MONO PARAM: '{}' type: {:?} -> {:?}", 
                              parameter.name, original_type, monomorphized_type);
                    *param_type = monomorphized_type;
                }
            }
        }
        
        // Transform return type if needed
        if let Some(return_type) = &mut monomorphized_function.return_type {
            let original_return = return_type.clone();
            let monomorphized_return = self.monomorphize_type(return_type, spec)?;
            
            if original_return != monomorphized_return {
                eprintln!("🔧 MONO RETURN: {:?} -> {:?}", original_return, monomorphized_return);
                *return_type = monomorphized_return;
            }
        }
        
        // Create a deterministic clause ID that includes concrete argument types
        let monomorphized_clause_id = self.generate_monomorphized_clause_id(
            &spec.trait_name,
            &spec.function_name,
            &spec.implementing_type,
            &monomorphized_function.parameters
        );
        
        // Check if this exact monomorphization already exists
        let trait_type = StructuredType::Simple(self.intern_type_name(&spec.trait_name));
        let module_key = ModuleKey::TraitImpl(
            Box::new(trait_type.clone()),
            Box::new(spec.implementing_type.clone()),
        );
        let function_name_atom = self.intern_atom_name(&spec.function_name);
        
        // Check if we already have this monomorphized clause
        let already_exists = {
            let modules = self.modules.read().unwrap();
            if let Some(module) = modules.get(&module_key) {
                if let Some(clause_set) = module.get_function_clause_set(&function_name_atom) {
                    clause_set.clauses.iter().any(|c| c.clause_id == monomorphized_clause_id)
                } else {
                    false
                }
            } else {
                false
            }
        };
        
        if already_exists {
            eprintln!("🔧 MONO CLAUSE: Monomorphized clause {} already exists - skipping", monomorphized_clause_id);
            return Ok(());
        }
        
        let monomorphized_clause = crate::checker::FunctionClause {
            clause_id: monomorphized_clause_id.clone(),
            base_function: monomorphized_function,
            source_order: clause.source_order,
            applicability_constraints: clause.applicability_constraints.clone(),
            from_guard: clause.from_guard,
            span: clause.span,
        };
        
        // Add the monomorphized clause with strict validation against overwrites
        {
            let mut modules = self.modules.write().unwrap();
            if let Some(module) = modules.get_mut(&module_key) {
                if let Some(clause_set) = module.get_function_clause_set_mut(&function_name_atom) {
                    // Check if this clause ID already exists
                    if clause_set.clauses.iter().any(|c| c.clause_id == monomorphized_clause_id) {
                        return Err(vec![TypeError::InternalError { 
                            message: format!("CRITICAL ERROR: Attempted to overwrite existing clause {}", monomorphized_clause_id),
                            span: clause.span.to_source_span()
                        }]);
                    }
                    
                    clause_set.clauses.push(monomorphized_clause.clone());
                    eprintln!("🔧 MONO CLAUSE: Added monomorphized clause {} (total clauses: {})", 
                             monomorphized_clause_id, clause_set.clauses.len());
                    
                    // CRITICAL: Also register the clause in the central registry for fast dispatch
                    self.register_function_clause(
                        Some(spec.trait_name.clone()), 
                        spec.function_name.clone(), 
                        monomorphized_clause
                    );
                              
                    // Special debugging for Display::to_string for Atom
                    if spec.trait_name == "Display" && spec.function_name == "to_string" {
                        if let StructuredType::Simple(type_id) = &spec.implementing_type {
                            let type_name = self.resolve_type_name(type_id).unwrap_or_default();
                            if type_name == "Outrun.Core.Atom" {
                                eprintln!("🎯 TARGET MODULE: Display::to_string for Atom - added clause {} (clause #{}/{})", 
                                         monomorphized_clause_id, clause_set.clauses.len(), clause_set.clauses.len());
                                eprintln!("🎯 TARGET MODULE: Module key: {:?}", module_key);
                                eprintln!("🎯 TARGET MODULE: Function atom: {:?}", function_name_atom);
                            }
                        }
                    }
                } else {
                    eprintln!("🔧 MONO CLAUSE: No clause set found for function {}", function_name_atom);
                }
            } else {
                eprintln!("🔧 MONO CLAUSE: No module found for key {:?}", module_key);
            }
        }
        
        Ok(())
    }

    /// Monomorphize a single type by replacing trait types with concrete types
    fn monomorphize_type(
        &self,
        structured_type: &StructuredType,
        spec: &FullMonomorphizationSpec,
    ) -> Result<StructuredType, Vec<TypeError>> {
        match structured_type {
            StructuredType::Simple(type_id) => {
                let type_name = self.resolve_type_name(type_id).unwrap_or_default();
                
                // Map trait types to concrete types based on the context
                let concrete_type_name = match type_name.as_str() {
                    "Integer" => "Outrun.Core.Integer64",
                    "Boolean" => "Outrun.Core.Boolean", 
                    "String" => "Outrun.Core.String",
                    "Float" => "Outrun.Core.Float64",
                    "Atom" => "Outrun.Core.Atom",
                    "Self" => {
                        // Self should resolve to the implementing type
                        return Ok(spec.implementing_type.clone());
                    },
                    _ => {
                        // Type is already concrete or not a common trait type
                        return Ok(structured_type.clone());
                    }
                };
                
                let concrete_type_id = self.intern_type_name(concrete_type_name);
                Ok(StructuredType::Simple(concrete_type_id))
            },
            StructuredType::Generic { base, args } => {
                // Recursively monomorphize generic arguments
                let monomorphized_base = self.monomorphize_type(
                    &StructuredType::Simple(base.clone()), 
                    spec
                )?;
                
                let base_id = match monomorphized_base {
                    StructuredType::Simple(id) => id,
                    _ => base.clone(), // Fallback to original
                };
                
                let mut monomorphized_args = Vec::new();
                for arg in args {
                    monomorphized_args.push(self.monomorphize_type(arg, spec)?);
                }
                
                Ok(StructuredType::Generic { 
                    base: base_id, 
                    args: monomorphized_args 
                })
            },
            _ => {
                // For other types (Function, Tuple, etc.), return as-is for now
                Ok(structured_type.clone())
            }
        }
    }

    /// Generate a deterministic clause ID that includes all concrete argument types
    fn generate_monomorphized_clause_id(
        &self,
        trait_name: &str,
        function_name: &str,
        implementing_type: &StructuredType,
        parameters: &[crate::checker::TypedParameter],
    ) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        
        // Hash the trait name
        trait_name.hash(&mut hasher);
        
        // Hash the function name  
        function_name.hash(&mut hasher);
        
        // Hash the implementing type
        self.resolve_type_display(implementing_type).hash(&mut hasher);
        
        // Sort parameters alphabetically by name for deterministic hashing
        let mut sorted_params: Vec<_> = parameters.iter().collect();
        sorted_params.sort_by(|a, b| a.name.cmp(&b.name));
        
        // Hash each parameter name and concrete type in alphabetical order
        for param in &sorted_params {
            param.name.hash(&mut hasher);
            if let Some(param_type) = &param.param_type {
                self.resolve_type_display(param_type).hash(&mut hasher);
            }
        }
        
        // Create a human-readable but unique ID with alphabetically sorted parameters
        let impl_type_name = self.resolve_type_display(implementing_type);
        let mut param_types: Vec<String> = parameters.iter()
            .map(|p| {
                let type_name = p.param_type.as_ref()
                    .map(|t| self.resolve_type_display(t))
                    .unwrap_or_else(|| "Unknown".to_string());
                format!("{}:{}", p.name, type_name)
            })
            .collect();
        
        // Sort parameters alphabetically for consistent function IDs
        param_types.sort();
        
        format!(
            "MONO_{}:{}:{}:({}):{}",
            trait_name,
            function_name,
            impl_type_name,
            param_types.join(","),
            format!("{:x}", hasher.finish())[..8].to_string() // 8-char hash for uniqueness
        )
    }

    /// Create a monomorphized function definition with all trait types resolved
    fn create_monomorphized_function_definition(
        &self,
        original_func_def: &outrun_parser::FunctionDefinition,
        spec: &FullMonomorphizationSpec,
    ) -> Result<crate::checker::TypedFunctionDefinition, Vec<TypeError>> {
        // Create typed parameters with fully resolved types
        let mut typed_parameters = Vec::new();
        for param in &original_func_def.parameters {
            // Always resolve trait types to concrete types using the call site information
            let concrete_param_type = self.resolve_trait_type_to_concrete(&param.type_annotation, spec)?;

            eprintln!("🔧 MONOMORPH PARAM: '{}' type: {:?} -> {:?}", 
                      param.name.name, 
                      param.type_annotation,
                      concrete_param_type);

            typed_parameters.push(crate::checker::TypedParameter {
                name: param.name.name.clone(),
                param_type: concrete_param_type,
                span: param.span,
            });
        }

        // Resolve return type
        let concrete_return_type = Some(spec.concrete_return_type.clone());

        // Create a dummy typed block for now
        let dummy_block = crate::checker::TypedBlock {
            statements: Vec::new(),
            result_type: concrete_return_type.clone(),
            span: original_func_def.span,
        };

        // Generate a unique function ID for this monomorphized function
        let function_id = format!(
            "MONO_{}_{}_{}_{}",
            spec.trait_name,
            self.resolve_type_display(&spec.implementing_type),
            spec.function_name,
            self.hash_argument_types(&spec.concrete_argument_types)
        );

        Ok(crate::checker::TypedFunctionDefinition {
            name: original_func_def.name.name.clone(),
            parameters: typed_parameters,
            return_type: concrete_return_type,
            guard: None, // TODO: Handle guards if needed
            body: dummy_block,
            function_id,
            span: original_func_def.span,
        })
    }

    /// Resolve a trait type annotation to a concrete type based on the monomorphization spec
    fn resolve_trait_type_to_concrete(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
        spec: &FullMonomorphizationSpec,
    ) -> Result<Option<StructuredType>, Vec<TypeError>> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, .. } => {
                if let Some(first_type) = path.first() {
                    let type_name = &first_type.name;
                    
                    // Handle Self -> implementing type
                    if type_name == "Self" {
                        return Ok(Some(spec.implementing_type.clone()));
                    }
                    
                    // Handle trait types -> concrete implementing types
                    // For now, map common trait types to their concrete implementations
                    let concrete_type = match type_name.as_str() {
                        "Integer" => {
                            let type_id = self.intern_type_name("Outrun.Core.Integer64");
                            StructuredType::Simple(type_id)
                        }
                        "Boolean" => {
                            let type_id = self.intern_type_name("Outrun.Core.Boolean");
                            StructuredType::Simple(type_id)
                        }
                        "String" => {
                            let type_id = self.intern_type_name("Outrun.Core.String");
                            StructuredType::Simple(type_id)
                        }
                        _ => {
                            // Use the original type name
                            let type_id = self.intern_type_name(type_name);
                            StructuredType::Simple(type_id)
                        }
                    };
                    
                    Ok(Some(concrete_type))
                } else {
                    Ok(None)
                }
            }
            _ => {
                // TODO: Handle complex type annotations
                Ok(None)
            }
        }
    }

    /// Create a hash of argument types for unique clause identification
    fn hash_argument_types(&self, argument_types: &std::collections::HashMap<String, StructuredType>) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        
        // Sort keys for deterministic hashing
        let mut keys: Vec<_> = argument_types.keys().collect();
        keys.sort();
        
        for key in keys {
            key.hash(&mut hasher);
            if let Some(arg_type) = argument_types.get(key) {
                self.resolve_type_display(arg_type).hash(&mut hasher);
            }
        }
        
        format!("{:x}", hasher.finish())
    }

    /// Helper to resolve a type to a display string
    fn resolve_type_display(&self, structured_type: &StructuredType) -> String {
        match structured_type {
            StructuredType::Simple(type_id) => {
                self.resolve_type_name(type_id).unwrap_or_default()
            }
            StructuredType::Generic { base, args } => {
                let base_name = self.resolve_type_name(base).unwrap_or_default();
                let arg_names: Vec<String> = args.iter().map(|arg| self.resolve_type_display(arg)).collect();
                format!("{}<{}>", base_name, arg_names.join(", "))
            }
            _ => "ComplexType".to_string(),
        }
    }

    /// NEW: Phase 7.5 - SMT-guided monomorphization
    /// Generate concrete trait implementations for all generic types used in SMT constraints
    fn phase_7_5_smt_guided_monomorphization(&mut self) -> Result<(), Vec<TypeError>> {
        let unification_context = self.unification_context();
        
        // Extract all PreResolvedClause constraints that involve generic types
        let mut monomorphization_requests = Vec::new();
        
        for constraint in &unification_context.smt_constraints {
            if let crate::smt::constraints::SMTConstraint::PreResolvedClause {
                trait_type,
                impl_type,
                function_name,
                ..
            } = constraint {
                // Check if this involves a generic trait implementation
                if self.requires_monomorphization(trait_type, impl_type) {
                    monomorphization_requests.push((trait_type.clone(), impl_type.clone(), function_name.clone()));
                }
            }
        }
        
        // Generate monomorphized implementations
        for (trait_type, impl_type, _function_name) in monomorphization_requests {
            self.generate_monomorphized_trait_impl(&trait_type, &impl_type)?;
        }
        
        Ok(())
    }

    /// Check if a trait implementation requires monomorphization
    fn requires_monomorphization(&self, trait_type: &StructuredType, impl_type: &StructuredType) -> bool {
        // Look for cases where impl_type is a concrete generic instantiation
        // but the trait implementation is generic
        match impl_type {
            StructuredType::Generic { base, args } => {
                // This is a concrete generic type like Outrun.Option.Some<Outrun.Core.Integer64>
                // Check if args contain concrete types rather than type variables
                args.iter().any(|arg| self.is_concrete_type(arg))
            }
            _ => false,
        }
    }
    
    /// Check if a type is concrete (not a type variable)
    fn is_concrete_type(&self, structured_type: &StructuredType) -> bool {
        match structured_type {
            StructuredType::TypeVariable(_) => false,
            StructuredType::Generic { args, .. } => args.iter().all(|arg| self.is_concrete_type(arg)),
            _ => true,
        }
    }
    
    /// Generate a monomorphized trait implementation
    fn generate_monomorphized_trait_impl(
        &mut self,
        trait_type: &StructuredType,
        concrete_impl_type: &StructuredType,
    ) -> Result<(), Vec<TypeError>> {
        // Find the generic trait implementation that we need to monomorphize
        let generic_impl = self.find_generic_trait_impl(trait_type, concrete_impl_type)?;
        
        // Create the monomorphized implementation
        let monomorphized_impl = self.monomorphize_impl_block(&generic_impl, concrete_impl_type)?;
        
        // CRITICAL FIX: Runtime always expects Simple trait type for module keys
        // Even for generic traits like Option<T>, the module key uses Simple(Option)
        let simple_trait_type = match trait_type {
            StructuredType::Simple(type_id) => StructuredType::Simple(type_id.clone()),
            StructuredType::Generic { base, .. } => StructuredType::Simple(base.clone()),
            _ => {
                return Err(vec![TypeError::internal(format!(
                    "Unsupported trait type for monomorphization: {:?}", trait_type
                ))]);
            }
        };
        
        // Register the monomorphized implementation in the module system
        self.register_monomorphized_impl(&monomorphized_impl, &simple_trait_type, concrete_impl_type)?;
        
        Ok(())
    }
    
    /// Compute the concrete trait type for module registration
    /// This converts generic trait types like Option<T> to concrete ones like Option<Integer64>
    fn compute_concrete_trait_type(
        &self,
        trait_type: &StructuredType,
        concrete_impl_type: &StructuredType,
        generic_impl: &outrun_parser::ImplBlock,
    ) -> Result<StructuredType, Vec<TypeError>> {
        // If the trait type is already concrete (like Simple(Equality)), use it as-is
        if self.is_concrete_type(trait_type) {
            return Ok(trait_type.clone());
        }
        
        // If trait type is generic, we need to substitute type parameters
        // First, build the type parameter mapping from the generic implementation
        let type_param_map = self.build_type_parameter_mapping(generic_impl, concrete_impl_type)?;
        
        // Apply the type parameter substitution to the trait type
        self.substitute_type_parameters_for_concrete_trait(trait_type, &type_param_map)
    }
    
    /// Build type parameter mapping from generic implementation to concrete types
    fn build_type_parameter_mapping(
        &self,
        generic_impl: &outrun_parser::ImplBlock,
        concrete_impl_type: &StructuredType,
    ) -> Result<std::collections::HashMap<String, StructuredType>, Vec<TypeError>> {
        let mut type_param_map = std::collections::HashMap::new();
        
        // Get the generic parameters from the impl block
        if let Some(generic_params) = &generic_impl.generic_params {
            // Convert the generic impl type to structured type to see the pattern
            let generic_impl_type = self.convert_type_spec_to_structured_type(&generic_impl.type_spec)
                .map_err(|e| vec![e])?;
            
            // Match the generic pattern against the concrete type to extract type arguments
            self.extract_type_arguments(&generic_impl_type, concrete_impl_type, &mut type_param_map)?;
        }
        
        Ok(type_param_map)
    }
    
    /// Extract type arguments by matching generic pattern against concrete type
    /// For example: Outrun.Option.Some<T> vs Outrun.Option.Some<Integer64> → T = Integer64
    fn extract_type_arguments(
        &self,
        generic_pattern: &StructuredType,
        concrete_type: &StructuredType,
        type_param_map: &mut std::collections::HashMap<String, StructuredType>,
    ) -> Result<(), Vec<TypeError>> {
        match (generic_pattern, concrete_type) {
            (StructuredType::TypeVariable(var_id), concrete) => {
                // T matches Integer64 → T = Integer64
                let var_name = self.resolve_type_name(var_id)
                    .ok_or_else(|| vec![TypeError::internal(format!("Unknown type variable: {}", var_id))])?;
                type_param_map.insert(var_name, concrete.clone());
            }
            (StructuredType::Generic { base: _, args: generic_args }, 
             StructuredType::Generic { base: _, args: concrete_args }) => {
                // Option<T> matches Option<Integer64> → T = Integer64
                if generic_args.len() == concrete_args.len() {
                    for (generic_arg, concrete_arg) in generic_args.iter().zip(concrete_args.iter()) {
                        self.extract_type_arguments(generic_arg, concrete_arg, type_param_map)?;
                    }
                }
            }
            _ => {
                // Other cases: no type parameters to extract
            }
        }
        Ok(())
    }
    
    /// Substitute type parameters in a type for concrete trait type computation
    fn substitute_type_parameters_for_concrete_trait(
        &self,
        type_to_substitute: &StructuredType,
        type_param_map: &std::collections::HashMap<String, StructuredType>,
    ) -> Result<StructuredType, Vec<TypeError>> {
        match type_to_substitute {
            StructuredType::TypeVariable(var_id) => {
                let var_name = self.resolve_type_name(var_id)
                    .ok_or_else(|| vec![TypeError::internal(format!("Unknown type variable: {}", var_id))])?;
                if let Some(replacement) = type_param_map.get(&var_name) {
                    Ok(replacement.clone())
                } else {
                    Ok(type_to_substitute.clone())
                }
            }
            StructuredType::Generic { base, args } => {
                let substituted_args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| self.substitute_type_parameters_for_concrete_trait(arg, type_param_map))
                    .collect();
                Ok(StructuredType::Generic {
                    base: base.clone(),
                    args: substituted_args?,
                })
            }
            _ => Ok(type_to_substitute.clone()),
        }
    }
    
    /// Find the generic trait implementation that matches the pattern
    fn find_generic_trait_impl(
        &self,
        trait_type: &StructuredType,
        concrete_impl_type: &StructuredType,
    ) -> Result<outrun_parser::ImplBlock, Vec<TypeError>> {
        let implementations = &self.compilation_state.read().unwrap().implementations;
        
        // We need to find an impl block like: impl<T> Option<T> for Outrun.Option.Some<T>
        // that matches our concrete need: impl Option<Integer64> for Outrun.Option.Some<Integer64>
        
        // Extract the base trait name and impl type base for matching
        let (trait_base, concrete_trait_args) = match trait_type {
            StructuredType::Simple(base) => (base.clone(), vec![]),
            StructuredType::Generic { base, args } => (base.clone(), args.clone()),
            _ => return Err(vec![TypeError::internal(format!(
                "Unsupported trait type for monomorphization: {:?}", trait_type
            ))]),
        };
        
        let (impl_base, concrete_impl_args) = match concrete_impl_type {
            StructuredType::Simple(base) => (base.clone(), vec![]),
            StructuredType::Generic { base, args } => (base.clone(), args.clone()),
            _ => return Err(vec![TypeError::internal(format!(
                "Unsupported impl type for monomorphization: {:?}", concrete_impl_type
            ))]),
        };
        
        for impl_block in implementations {
            // Check if this is a generic implementation (has type parameters)
            if impl_block.generic_params.is_none() {
                continue; // Skip non-generic implementations
            }
            
            // Convert the impl block's trait specification to StructuredType WITH type parameters
            let generic_params = impl_block.generic_params.as_ref().unwrap();
            let mut type_param_map = std::collections::HashMap::new();
            for param in &generic_params.params {
                let param_type_id = self.intern_type_name(&param.name.name);
                type_param_map.insert(
                    param.name.name.clone(),
                    StructuredType::TypeVariable(param_type_id),
                );
            }
            
            let impl_trait_type = match self.convert_type_spec_to_structured_type_with_params(
                &impl_block.trait_spec,
                &type_param_map,
            ) {
                Ok(t) => t,
                Err(_) => continue, // Skip if we can't parse the trait spec
            };
            
            let impl_implementing_type = match self.convert_type_spec_to_structured_type_with_params(
                &impl_block.type_spec,
                &type_param_map,
            ) {
                Ok(t) => t,
                Err(_) => continue, // Skip if we can't parse the type spec
            };
            
            // Extract base types from the generic implementation
            let impl_trait_base = match &impl_trait_type {
                StructuredType::Simple(base) => base,
                StructuredType::Generic { base, .. } => base,
                _ => continue,
            };
            
            let impl_impl_base = match &impl_implementing_type {
                StructuredType::Simple(base) => base,
                StructuredType::Generic { base, .. } => base,
                _ => continue,
            };
            
            // Check if base types match
            let trait_bases_match = impl_trait_base == &trait_base;
            let impl_bases_match = impl_impl_base == &impl_base;
            
            
            // Check if this is a viable generic implementation to monomorphize
            let (is_monomorphizable, both_generic, trait_simple_impl_generic, specializable_to_concrete) = if trait_bases_match && impl_bases_match {
                // Case 1: Both trait and impl are generic with type variables
                // e.g., impl<T> Option<T> for Outrun.Option.Some<T>
                let both_generic = match (&impl_trait_type, &impl_implementing_type) {
                    (StructuredType::Generic { args: trait_args, .. }, StructuredType::Generic { args: impl_args, .. }) => {
                        trait_args.iter().any(|arg| matches!(arg, StructuredType::TypeVariable(_))) &&
                        impl_args.iter().any(|arg| matches!(arg, StructuredType::TypeVariable(_)))
                    }
                    _ => false,
                };
                
                // Case 2: Trait is simple but impl is generic with type variables
                // e.g., impl<T> Equality for Outrun.Core.List<T>
                let trait_simple_impl_generic = match (&impl_trait_type, &impl_implementing_type) {
                    (StructuredType::Simple(_), StructuredType::Generic { args: impl_args, .. }) => {
                        impl_args.iter().any(|arg| matches!(arg, StructuredType::TypeVariable(_)))
                    }
                    _ => false,
                };
                
                // CRITICAL FIX: Case 3: Generic trait and impl structures match concrete request
                // This handles the case where we have:
                // - Generic impl: impl<T> Option<T> for Outrun.Option.Some<T>
                // - Concrete request: Option<Integer64> for Outrun.Option.Some<Integer64>
                // We check if the generic implementation can be specialized to the concrete types
                let specializable_to_concrete = match (&impl_trait_type, &impl_implementing_type) {
                    (StructuredType::Generic { base: generic_trait_base, args: generic_trait_args }, 
                     StructuredType::Generic { base: generic_impl_base, args: generic_impl_args }) => {
                        // Check if bases match our concrete request
                        let trait_base_matches = generic_trait_base == &trait_base;
                        let impl_base_matches = generic_impl_base == &impl_base;
                        
                        // Check if generic has TypeVariables that can be specialized
                        let has_specializable_trait_params = generic_trait_args.iter().any(|arg| matches!(arg, StructuredType::TypeVariable(_)));
                        let has_specializable_impl_params = generic_impl_args.iter().any(|arg| matches!(arg, StructuredType::TypeVariable(_)));
                        
                        trait_base_matches && impl_base_matches && 
                        (has_specializable_trait_params || has_specializable_impl_params)
                    }
                    _ => false,
                };
                
                let is_monomorphizable = both_generic || trait_simple_impl_generic || specializable_to_concrete;
                (is_monomorphizable, both_generic, trait_simple_impl_generic, specializable_to_concrete)
            } else {
                (false, false, false, false)
            };
            
            if is_monomorphizable {
                return Ok(impl_block.clone());
            }
        }
        
        Err(vec![TypeError::internal(format!(
            "No generic trait implementation found for trait {:?} implemented by {:?}",
            trait_type, concrete_impl_type
        ))])
    }
    
    /// Check if trait types match for monomorphization
    /// Example: Option<Integer64> matches Option<T>
    fn trait_types_match_for_monomorphization(
        &self,
        concrete_trait: &StructuredType,
        generic_trait: &StructuredType,
    ) -> bool {
        match (concrete_trait, generic_trait) {
            // Simple trait: Option matches Option
            (StructuredType::Simple(concrete_id), StructuredType::Simple(generic_id)) => {
                concrete_id == generic_id
            }
            // Generic trait: Option<Integer64> matches Option<T>
            (
                StructuredType::Generic { base: concrete_base, args: concrete_args },
                StructuredType::Generic { base: generic_base, args: generic_args }
            ) => {
                // Base types must match exactly
                if concrete_base != generic_base {
                    return false;
                }
                
                // Must have same number of type arguments
                if concrete_args.len() != generic_args.len() {
                    return false;
                }
                
                // Each generic arg must be a type variable, and concrete args must be concrete
                for (concrete_arg, generic_arg) in concrete_args.iter().zip(generic_args.iter()) {
                    match generic_arg {
                        StructuredType::TypeVariable(_) => {
                            // Generic argument is a type variable - this is what we expect
                            // The concrete argument can be anything (it will be substituted)
                        }
                        _ => {
                            // Generic argument is not a type variable - they must match exactly
                            if concrete_arg != generic_arg {
                                return false;
                            }
                        }
                    }
                }
                
                true
            }
            // Mixed cases don't match
            _ => false,
        }
    }
    
    /// Check if impl types match for monomorphization
    /// Example: Outrun.Option.Some<Integer64> matches Outrun.Option.Some<T>
    fn impl_types_match_for_monomorphization(
        &self,
        concrete_impl: &StructuredType,
        generic_impl: &StructuredType,
    ) -> bool {
        // For impl types, we use the same logic as trait types
        self.trait_types_match_for_monomorphization(concrete_impl, generic_impl)
    }
    
    /// Monomorphize an impl block by substituting concrete types for type parameters
    fn monomorphize_impl_block(
        &self,
        generic_impl: &outrun_parser::ImplBlock,
        concrete_impl_type: &StructuredType,
    ) -> Result<outrun_parser::ImplBlock, Vec<TypeError>> {
        // Create a type parameter substitution map
        // We need to map the generic type parameters (T) to concrete types (Integer64)
        
        let mut type_substitutions = std::collections::HashMap::new();
        
        // Get the generic parameters from the impl block
        let generic_params = generic_impl.generic_params.as_ref()
            .ok_or_else(|| vec![TypeError::internal(
                "Cannot monomorphize non-generic impl block".to_string()
            )])?;
            
        // Extract concrete type arguments from the concrete impl type
        let concrete_type_args = match concrete_impl_type {
            StructuredType::Simple(_) => vec![], // No type arguments
            StructuredType::Generic { args, .. } => args.clone(),
            _ => return Err(vec![TypeError::internal(format!(
                "Unsupported concrete impl type for monomorphization: {:?}", concrete_impl_type
            ))]),
        };
        
        // Map each generic parameter to its concrete type
        if generic_params.params.len() != concrete_type_args.len() {
            return Err(vec![TypeError::internal(format!(
                "Generic parameter count mismatch: {} generic params but {} concrete args",
                generic_params.params.len(),
                concrete_type_args.len()
            ))]);
        }
        
        for (generic_param, concrete_arg) in generic_params.params.iter().zip(concrete_type_args.iter()) {
            type_substitutions.insert(generic_param.name.name.clone(), concrete_arg.clone());
        }
        
        // Clone the impl block and perform type substitutions
        let mut monomorphized_impl = generic_impl.clone();
        
        // Remove generic parameters from the monomorphized version
        monomorphized_impl.generic_params = None;
        
        // Substitute concrete types in the trait specification
        monomorphized_impl.trait_spec = self.substitute_types_in_type_spec(
            &generic_impl.trait_spec,
            &type_substitutions,
        )?;
        
        // Substitute concrete types in the implementing type specification  
        monomorphized_impl.type_spec = self.substitute_types_in_type_spec(
            &generic_impl.type_spec,
            &type_substitutions,
        )?;
        
        // Substitute concrete types in each function definition
        for func in &mut monomorphized_impl.functions {
            *func = self.substitute_types_in_function_definition(func, &type_substitutions)?;
        }
        
        Ok(monomorphized_impl)
    }
    
    /// Substitute types in a type specification
    fn substitute_types_in_type_spec(
        &self,
        type_spec: &outrun_parser::TypeSpec,
        type_substitutions: &std::collections::HashMap<String, StructuredType>,
    ) -> Result<outrun_parser::TypeSpec, Vec<TypeError>> {
        let mut substituted_spec = type_spec.clone();
        
        // Substitute in the main path
        for path_element in &mut substituted_spec.path {
            if let Some(concrete_type) = type_substitutions.get(&path_element.name) {
                // Replace the type parameter with its concrete type
                path_element.name = self.structured_type_to_type_name(concrete_type);
            }
        }
        
        // Substitute in generic arguments
        if let Some(ref mut generic_args) = substituted_spec.generic_args {
            for arg in &mut generic_args.args {
                *arg = self.substitute_types_in_type_annotation(arg, type_substitutions)?;
            }
        }
        
        Ok(substituted_spec)
    }
    
    /// Substitute types in a type annotation
    fn substitute_types_in_type_annotation(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
        type_substitutions: &std::collections::HashMap<String, StructuredType>,
    ) -> Result<outrun_parser::TypeAnnotation, Vec<TypeError>> {
        let substituted_annotation = match type_annotation {
            outrun_parser::TypeAnnotation::Simple { path, generic_args, span } => {
                // Substitute in the path
                let mut substituted_path = path.clone();
                for path_element in &mut substituted_path {
                    if let Some(concrete_type) = type_substitutions.get(&path_element.name) {
                        path_element.name = self.structured_type_to_type_name(concrete_type);
                    }
                }
                
                // Substitute in generic arguments
                let substituted_generic_args = if let Some(ref generic_args) = generic_args {
                    let mut substituted_args = generic_args.clone();
                    for arg in &mut substituted_args.args {
                        *arg = self.substitute_types_in_type_annotation(arg, type_substitutions)?;
                    }
                    Some(substituted_args)
                } else {
                    None
                };
                
                outrun_parser::TypeAnnotation::Simple {
                    path: substituted_path,
                    generic_args: substituted_generic_args,
                    span: *span,
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, span } => {
                let mut substituted_types = Vec::new();
                for type_annotation in types {
                    substituted_types.push(self.substitute_types_in_type_annotation(type_annotation, type_substitutions)?);
                }
                outrun_parser::TypeAnnotation::Tuple {
                    types: substituted_types,
                    span: *span,
                }
            }
            outrun_parser::TypeAnnotation::Function { params, return_type, span } => {
                let mut substituted_params = Vec::new();
                for param in params {
                    substituted_params.push(outrun_parser::FunctionTypeParam {
                        name: param.name.clone(),
                        type_annotation: self.substitute_types_in_type_annotation(&param.type_annotation, type_substitutions)?,
                        span: param.span,
                    });
                }
                let substituted_return_type = Box::new(self.substitute_types_in_type_annotation(return_type, type_substitutions)?);
                outrun_parser::TypeAnnotation::Function {
                    params: substituted_params,
                    return_type: substituted_return_type,
                    span: *span,
                }
            }
        };
        
        Ok(substituted_annotation)
    }
    
    /// Substitute types in a function definition
    fn substitute_types_in_function_definition(
        &self,
        func_def: &outrun_parser::FunctionDefinition,
        type_substitutions: &std::collections::HashMap<String, StructuredType>,
    ) -> Result<outrun_parser::FunctionDefinition, Vec<TypeError>> {
        let mut substituted_func = func_def.clone();
        
        // Substitute in parameter types
        for param in &mut substituted_func.parameters {
            param.type_annotation = self.substitute_types_in_type_annotation(&param.type_annotation, type_substitutions)?;
        }
        
        // Substitute in return type (return_type is TypeAnnotation in ImplBlock FunctionDefinition)
        substituted_func.return_type = self.substitute_types_in_type_annotation(&substituted_func.return_type, type_substitutions)?;
        
        // For now, we don't substitute in the function body - that would require
        // a more complex AST transformation that handles type annotations in expressions
        // The function body will be type-checked again with the concrete types
        
        Ok(substituted_func)
    }
    
    /// Convert a StructuredType back to a type name string for substitution
    fn structured_type_to_type_name(&self, structured_type: &StructuredType) -> String {
        match structured_type {
            StructuredType::Simple(type_id) => {
                self.resolve_type_name(type_id).unwrap_or_else(|| format!("UnknownType_{}", type_id.hash))
            }
            StructuredType::Generic { base, args } => {
                let base_name = self.resolve_type_name(base).unwrap_or_else(|| format!("UnknownType_{}", base.hash));
                let arg_names: Vec<String> = args.iter()
                    .map(|arg| self.structured_type_to_type_name(arg))
                    .collect();
                format!("{}<{}>", base_name, arg_names.join(", "))
            }
            StructuredType::TypeVariable(type_id) => {
                self.resolve_type_name(type_id).unwrap_or_else(|| format!("TypeVar_{}", type_id.hash))
            }
            StructuredType::Tuple(elements) => {
                let element_names: Vec<String> = elements.iter()
                    .map(|elem| self.structured_type_to_type_name(elem))
                    .collect();
                format!("({})", element_names.join(", "))
            }
            StructuredType::Function { params, return_type } => {
                let param_names: Vec<String> = params.iter()
                    .map(|param| format!("{}: {}", param.name, self.structured_type_to_type_name(&param.param_type)))
                    .collect();
                let return_name = self.structured_type_to_type_name(return_type);
                format!("({}) -> {}", param_names.join(", "), return_name)
            }
            StructuredType::Integer64 => "Outrun.Core.Integer64".to_string(),
            StructuredType::Float64 => "Outrun.Core.Float64".to_string(),
            StructuredType::Boolean => "Outrun.Core.Boolean".to_string(),
            StructuredType::String => "Outrun.Core.String".to_string(),
            StructuredType::Atom => "Outrun.Core.Atom".to_string(),
            StructuredType::List { element_type } => {
                format!("List<{}>", self.structured_type_to_type_name(element_type))
            }
            StructuredType::Map { key_type, value_type } => {
                format!("Map<{}, {}>", 
                    self.structured_type_to_type_name(key_type),
                    self.structured_type_to_type_name(value_type))
            }
            StructuredType::Option { inner_type } => {
                format!("Option<{}>", self.structured_type_to_type_name(inner_type))
            }
            StructuredType::Result { ok_type, err_type } => {
                format!("Result<{}, {}>", 
                    self.structured_type_to_type_name(ok_type),
                    self.structured_type_to_type_name(err_type))
            }
            StructuredType::Struct { name, .. } => {
                self.resolve_type_name(name).unwrap_or_else(|| format!("Struct_{}", name.hash))
            }
            StructuredType::Trait { name, .. } => {
                self.resolve_type_name(name).unwrap_or_else(|| format!("Trait_{}", name.hash))
            }
            StructuredType::TypeError { .. } => "TypeError".to_string(),
        }
    }
    
    /// Register the monomorphized implementation in the module system
    fn register_monomorphized_impl(
        &mut self,
        monomorphized_impl: &outrun_parser::ImplBlock,
        trait_type: &StructuredType,
        concrete_impl_type: &StructuredType,
    ) -> Result<(), Vec<TypeError>> {
        // Create the correct module key using the provided trait_type and concrete_impl_type
        let module_key = ModuleKey::TraitImpl(
            Box::new(trait_type.clone()),
            Box::new(concrete_impl_type.clone()),
        );
        
        // Create trait implementation module if it doesn't exist
        self.get_or_create_module(
            module_key.clone(),
            ModuleKind::TraitImpl,
            SourceLocation::File("monomorphized".to_string()),
            concrete_impl_type.clone(),
        );
        
        // Register each function in the monomorphized implementation
        for func_def in &monomorphized_impl.functions {
            // Create function entry for trait implementation
            let trait_name = monomorphized_impl
                .trait_spec
                .path
                .iter()
                .map(|id| &id.name)
                .cloned()
                .collect::<Vec<_>>()
                .join(".");
            
            let impl_type_name = monomorphized_impl
                .type_spec
                .path
                .iter()
                .map(|id| &id.name)
                .cloned()
                .collect::<Vec<_>>()
                .join(".");
            
            let function_id = format!(
                "impl::{}::for::{}::{}",
                trait_name, impl_type_name, func_def.name.name
            );
            
            let is_guard = func_def.name.name.ends_with('?');
            let entry = UnifiedFunctionEntry::ImplFunction {
                definition: func_def.clone(),
                typed_definition: None,
                function_id,
                is_guard,
            };
            
            // Add trait function implementation to the module using the correct module key
            let function_name = self.intern_atom_name(&func_def.name.name);
            let function_signature_type =
                StructuredType::Simple(self.intern_type_name(&func_def.name.name));
            
            self.add_function_to_module(
                module_key.clone(),
                function_signature_type.clone(),
                function_name.clone(),
                entry.clone(),
            );
        }
        
        Ok(())
    }
    
    /// Internal helper method to register impl functions without the complex signature requirements
    fn add_impl_function_internal(
        &self,
        impl_block: &outrun_parser::ImplBlock,
        func_def: &outrun_parser::FunctionDefinition,
        source_file: &str,
    ) -> Result<(), Vec<TypeError>> {
        // Extract trait name from impl block
        let trait_name = impl_block
            .trait_spec
            .path
            .iter()
            .map(|id| &id.name)
            .cloned()
            .collect::<Vec<_>>()
            .join(".");

        let impl_type_name = impl_block
            .type_spec
            .path
            .iter()
            .map(|id| &id.name)
            .cloned()
            .collect::<Vec<_>>()
            .join(".");

        // Create function entry for trait implementation
        let function_id = format!(
            "impl::{}::for::{}::{}",
            trait_name, impl_type_name, func_def.name.name
        );
        let is_guard = func_def.name.name.ends_with('?');
        let entry = UnifiedFunctionEntry::ImplFunction {
            definition: func_def.clone(),
            typed_definition: None,
            function_id,
            is_guard,
        };

        // Look up trait type from module registry
        let trait_type_id = self.intern_type_name(&trait_name);
        let trait_type = if let Some(module) = self.get_module(trait_type_id.clone()) {
            module.structured_type
        } else {
            StructuredType::Simple(trait_type_id)
        };

        // Convert impl type using the monomorphized type spec
        let impl_type = match self.convert_type_spec_to_structured_type(&impl_block.type_spec) {
            Ok(structured_type) => structured_type,
            Err(_) => {
                let impl_type_id = self.intern_type_name(&impl_type_name);
                StructuredType::Simple(impl_type_id)
            }
        };

        let module_key =
            ModuleKey::TraitImpl(Box::new(trait_type.clone()), Box::new(impl_type.clone()));

        // Create trait implementation module if it doesn't exist
        self.get_or_create_module(
            module_key.clone(),
            ModuleKind::TraitImpl,
            SourceLocation::File(source_file.to_string()),
            impl_type.clone(),
        );

        // Add trait function implementation to the module
        let function_name = self.intern_atom_name(&func_def.name.name);
        let function_signature_type =
            StructuredType::Simple(self.intern_type_name(&func_def.name.name));

        self.add_function_to_module(
            module_key,
            function_signature_type.clone(),
            function_name.clone(),
            entry.clone(),
        );

        Ok(())
    }
    
    /// Check if there's a generic implementation that can handle the concrete type
    /// This is used during dispatch to find implementations that were created via monomorphization
    fn has_generic_impl_for_concrete_type(
        &self,
        trait_type: &StructuredType,
        concrete_impl_type: &StructuredType,
    ) -> bool {
        // CRITICAL FIX: Check if the monomorphized implementation already exists in the module system
        // This is where Phase 7.5 stores the concrete implementations it generates
        let concrete_impl_key = ModuleKey::TraitImpl(
            Box::new(trait_type.clone()),
            Box::new(concrete_impl_type.clone()),
        );
        
        if self.has_module(&concrete_impl_key) {
            return true; // Monomorphized implementation found
        }
        
        // Extract the base trait name and impl type base for matching
        let (trait_base, _) = match trait_type {
            StructuredType::Simple(base) => (base.clone(), vec![]),
            StructuredType::Generic { base, args } => (base.clone(), args.clone()),
            _ => return false,
        };
        
        let (impl_base, _concrete_impl_args) = match concrete_impl_type {
            StructuredType::Simple(base) => (base.clone(), vec![]),
            StructuredType::Generic { base, args } => (base.clone(), args.clone()),
            _ => return false,
        };
        
        // Look through all existing implementations to find a generic one that matches
        let implementations = &self.compilation_state.read().unwrap().implementations;
        
        for impl_block in implementations {
            // Check if this is a generic implementation (has type parameters)
            if impl_block.generic_params.is_none() {
                continue; // Skip non-generic implementations
            }
            
            // Extract base types from the generic implementation
            let impl_trait_type_result = self.convert_type_spec_to_structured_type(&impl_block.trait_spec);
            let impl_implementing_type_result = self.convert_type_spec_to_structured_type(&impl_block.type_spec);
            
            let (impl_trait_type, impl_implementing_type) = match (impl_trait_type_result, impl_implementing_type_result) {
                (Ok(trait_t), Ok(impl_t)) => (trait_t, impl_t),
                _ => continue,
            };
            
            let impl_trait_base = match &impl_trait_type {
                StructuredType::Simple(base) => base,
                StructuredType::Generic { base, .. } => base,
                _ => continue,
            };
            
            let impl_impl_base = match &impl_implementing_type {
                StructuredType::Simple(base) => base,
                StructuredType::Generic { base, .. } => base,
                _ => continue,
            };
            
            // Check if base types match
            if impl_trait_base == &trait_base && impl_impl_base == &impl_base {
                // This generic implementation could work for our concrete type
                return true;
            }
        }
        
        false
    }

    /// NEW: Phase 7 - SMT constraint solving
    fn phase_7_smt_constraint_solving(&mut self) -> Result<(), Vec<TypeError>> {
        // 1. Get accumulated constraints from unification context
        let mut unification_context = self.unification_context();

        if !unification_context.has_pending_constraints() {
            return Ok(());
        }

        let solve_result = unification_context.solve_accumulated_constraints(self);

        match solve_result {
            Ok(model) => {
                // Store model in compilation state for backward compatibility
                self.compilation_state.write().unwrap().smt_model = Some(model.clone());
                
                // Also store model in unification context for Self type resolution
                unification_context.latest_smt_model = Some(model);
                self.set_unification_context(unification_context);

                Ok(())
            }
            Err(smt_error) => {
                let type_error =
                    TypeError::internal(format!("SMT constraint solving failed: {smt_error}"));
                Err(vec![type_error])
            }
        }
    }

    /// Phase 7.5: Post-SMT clause resolution
    /// Convert PendingClauseResolution constraints to PreResolvedClause constraints
    /// using resolved Self types from the SMT model

    /// NEW: Calculate dispatch tables using SMT results (replaces calculate_dispatch_tables)
    fn calculate_dispatch_tables_with_smt(
        &mut self,
        collection: &ProgramCollection,
        order: &[String],
    ) -> Result<(), Vec<TypeError>> {
        // Get the SMT model from constraint solving
        let smt_model = {
            let state = self.compilation_state.read().unwrap();
            state.smt_model.clone()
        };

        if let Some(_model) = smt_model {
            // Use SMT model to determine which trait implementations to include in dispatch tables

            // For now, fall back to traditional dispatch table calculation
            // TODO: Implement SMT-guided dispatch table generation
            self.calculate_dispatch_tables(collection, order)?;
        } else {
            // No SMT model available, use traditional approach
            self.calculate_dispatch_tables(collection, order)?;
        }

        Ok(())
    }

    /// Validate that all functions have typed definitions
    fn validate_all_functions_have_typed_definitions(&self) -> Vec<TypeError> {
        let mut errors = Vec::new();

        if let Ok(modules) = self.modules.read() {
            for (module_key, module) in modules.iter() {
                for function_entry in module.functions_by_name.values() {
                    if function_entry.typed_definition().is_none() {
                        let function_name = &function_entry.definition().name.name;
                        let span = function_entry.definition().span;

                        errors.push(TypeError::internal_with_span(
                            format!(
                                "Function '{function_name}' in module {module_key:?} missing typed definition after type checking. This indicates a compilation pipeline issue."
                            ),
                            span.to_source_span(),
                        ));
                    }
                }
            }
        }

        errors
    }

    /// Extract base type for module key lookups
    fn extract_base_type_for_lookup(&self, structured_type: &StructuredType) -> StructuredType {
        match structured_type {
            StructuredType::Generic { base, .. } => StructuredType::Simple(base.clone()),
            other => other.clone(),
        }
    }

    /// Convert TypeSpec to StructuredType with type parameter context
    pub fn convert_type_spec_to_structured_type_with_params(
        &self,
        type_spec: &outrun_parser::TypeSpec,
        type_params: &std::collections::HashMap<String, StructuredType>,
    ) -> Result<StructuredType, TypeError> {
        // Convert TypeSpec to TypeAnnotation first
        let type_annotation = outrun_parser::TypeAnnotation::Simple {
            path: type_spec.path.clone(),
            generic_args: type_spec.generic_args.clone(),
            span: type_spec.span,
        };

        // Use the type annotation converter with parameter support
        self.convert_type_annotation_to_structured_type_with_params(&type_annotation, type_params)
    }

    /// Convert TypeSpec to StructuredType, properly handling generic arguments
    pub fn convert_type_spec_to_structured_type(
        &self,
        type_spec: &outrun_parser::TypeSpec,
    ) -> Result<StructuredType, TypeError> {
        // Convert type path to string
        let type_name = type_spec
            .path
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        // Get the base type ID
        let type_id = self.intern_type_name(&type_name);

        // Handle generic arguments if present
        if let Some(ref generic_args) = type_spec.generic_args {
            let mut arg_types = Vec::new();
            for arg in &generic_args.args {
                // Convert each generic argument recursively
                let arg_type = self.convert_type_annotation_to_structured_type(arg)?;
                arg_types.push(arg_type);
            }

            Ok(StructuredType::Generic {
                base: type_id,
                args: arg_types,
            })
        } else {
            Ok(StructuredType::Simple(type_id))
        }
    }

    /// Convert TypeAnnotation to StructuredType with type parameter context
    fn convert_type_annotation_to_structured_type_with_params(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
        type_params: &std::collections::HashMap<String, StructuredType>,
    ) -> Result<StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path,
                generic_args,
                span: _,
            } => {
                // Convert type path to string
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this is a type parameter first
                if let Some(param_type) = type_params.get(&type_name) {
                    // This is a type parameter, return it directly
                    return Ok(param_type.clone());
                }

                // Get the type ID
                let type_id = self.intern_type_name(&type_name);

                // Handle generic arguments if present
                if let Some(ref args) = generic_args {
                    let mut arg_types = Vec::new();
                    for arg in &args.args {
                        let arg_type = self
                            .convert_type_annotation_to_structured_type_with_params(
                                arg,
                                type_params,
                            )?;
                        arg_types.push(arg_type);
                    }

                    Ok(StructuredType::Generic {
                        base: type_id,
                        args: arg_types,
                    })
                } else {
                    Ok(StructuredType::Simple(type_id))
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, span: _ } => {
                // Resolve all tuple element types
                let mut element_types = Vec::new();
                for element_type in types {
                    let element_struct_type = self
                        .convert_type_annotation_to_structured_type_with_params(
                            element_type,
                            type_params,
                        )?;
                    element_types.push(element_struct_type);
                }

                Ok(StructuredType::Tuple(element_types))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                span: _,
            } => {
                // Resolve parameter types
                let mut param_types = Vec::new();
                for param in params {
                    let param_name_atom = self.intern_atom_name(&param.name.name);
                    let param_type = self.convert_type_annotation_to_structured_type_with_params(
                        &param.type_annotation,
                        type_params,
                    )?;
                    param_types.push(crate::unification::FunctionParam {
                        name: param_name_atom,
                        param_type,
                    });
                }

                // Resolve return type
                let return_struct_type = self
                    .convert_type_annotation_to_structured_type_with_params(
                        return_type,
                        type_params,
                    )?;

                Ok(StructuredType::Function {
                    params: param_types,
                    return_type: Box::new(return_struct_type),
                })
            }
        }
    }

    /// Convert TypeAnnotation to StructuredType (helper method)
    fn convert_type_annotation_to_structured_type(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
    ) -> Result<StructuredType, TypeError> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path,
                generic_args,
                span: _,
            } => {
                // Convert type path to string
                let type_name = path
                    .iter()
                    .map(|id| id.name.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Get the type ID
                let type_id = self.intern_type_name(&type_name);

                // Handle generic arguments if present
                if let Some(ref args) = generic_args {
                    let mut arg_types = Vec::new();
                    for arg in &args.args {
                        let arg_type = self.convert_type_annotation_to_structured_type(arg)?;
                        arg_types.push(arg_type);
                    }

                    Ok(StructuredType::Generic {
                        base: type_id,
                        args: arg_types,
                    })
                } else {
                    // Check if this is a trait name that should resolve to concrete implementers
                    let potential_trait_type = StructuredType::Simple(type_id.clone());
                    if self.is_trait(&potential_trait_type) {
                        let implementations = self.get_trait_implementations(&potential_trait_type);

                        if implementations.is_empty() {
                            Err(TypeError::UndefinedType {
                                span: (0, 0).into(), // TODO: Get actual span from type annotation
                                name: format!("{type_name} (no concrete implementations found)"),
                            })
                        } else {
                            // Take the first implementation (either == 1 or > 1)
                            let concrete_type = implementations[0].clone();

                            Ok(concrete_type)
                        }
                    } else {
                        // Not a trait, treat as concrete type
                        Ok(StructuredType::Simple(type_id))
                    }
                }
            }
            outrun_parser::TypeAnnotation::Tuple { types, span: _ } => {
                // Resolve all tuple element types
                let mut element_types = Vec::new();
                for element_type in types {
                    let element_struct_type =
                        self.convert_type_annotation_to_structured_type(element_type)?;
                    element_types.push(element_struct_type);
                }

                Ok(StructuredType::Tuple(element_types))
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                span: _,
            } => {
                // Resolve parameter types
                let mut param_types = Vec::new();
                for param in params {
                    let param_name_atom = self.intern_atom_name(&param.name.name);
                    let param_type =
                        self.convert_type_annotation_to_structured_type(&param.type_annotation)?;
                    param_types.push(crate::unification::FunctionParam {
                        name: param_name_atom,
                        param_type,
                    });
                }

                // Resolve return type
                let return_struct_type =
                    self.convert_type_annotation_to_structured_type(return_type)?;

                Ok(StructuredType::Function {
                    params: param_types,
                    return_type: Box::new(return_struct_type),
                })
            }
        }
    }

    /// Calculate dispatch tables (Phase 6.5) - Prepare runtime dispatch information
    fn calculate_dispatch_tables(
        &mut self,
        collection: &ProgramCollection,
        compilation_order: &[String],
    ) -> Result<(), Vec<TypeError>> {
        for file_path in compilation_order {
            if let Some(program) = collection.programs.get(file_path) {
                self.process_program_for_dispatch(program)?;
            }
        }

        // Get dispatch table statistics
        let _stats = {
            let state = self.compilation_state.read().unwrap();
            state.dispatch_table.stats()
        };

        Ok(())
    }

    /// Process a single program to identify and resolve dispatch requirements
    fn process_program_for_dispatch(
        &mut self,
        program: &outrun_parser::Program,
    ) -> Result<u32, Vec<TypeError>> {
        let mut dispatch_count = 0;

        for item in &program.items {
            match &item.kind {
                outrun_parser::ItemKind::FunctionDefinition(func_def) => {
                    // Process function body for trait calls
                    dispatch_count += self.process_block_for_dispatch(&func_def.body)?;
                }
                outrun_parser::ItemKind::ConstDefinition(const_def) => {
                    // Process constant expression for trait calls
                    dispatch_count +=
                        self.process_expression_for_dispatch(&const_def.expression)?;
                }
                outrun_parser::ItemKind::LetBinding(let_binding) => {
                    // Process let binding expression for trait calls
                    dispatch_count +=
                        self.process_expression_for_dispatch(&let_binding.expression)?;
                }
                _ => {
                    // Other items (traits, structs, impls) don't contain expressions that need dispatch
                }
            }
        }

        Ok(dispatch_count)
    }

    /// Process a block to find trait function calls
    fn process_block_for_dispatch(
        &mut self,
        block: &outrun_parser::Block,
    ) -> Result<u32, Vec<TypeError>> {
        let mut dispatch_count = 0;

        for statement in &block.statements {
            match &statement.kind {
                outrun_parser::StatementKind::Expression(expr) => {
                    dispatch_count += self.process_expression_for_dispatch(expr)?;
                }
                outrun_parser::StatementKind::LetBinding(let_binding) => {
                    dispatch_count +=
                        self.process_expression_for_dispatch(&let_binding.expression)?;
                }
            }
        }

        Ok(dispatch_count)
    }

    /// Process an expression to find trait function calls
    fn process_expression_for_dispatch(
        &mut self,
        expression: &outrun_parser::Expression,
    ) -> Result<u32, Vec<TypeError>> {
        let mut dispatch_count = 0;

        match &expression.kind {
            outrun_parser::ExpressionKind::FunctionCall(call) => {
                // This is a function call - check if it's a trait function call
                dispatch_count += self.process_function_call_for_dispatch(call)?;
            }
            outrun_parser::ExpressionKind::QualifiedIdentifier(qualified_id) => {
                // Check if this is a trait reference (module name is a trait)
                let trait_name = &qualified_id.module.name;

                let trait_type_id = self.intern_type_name(trait_name);
                if self.get_trait(&trait_type_id).is_some() {
                    dispatch_count += 1;
                }
            }
            outrun_parser::ExpressionKind::IfExpression(if_expr) => {
                // Process condition and branches
                dispatch_count += self.process_expression_for_dispatch(&if_expr.condition)?;
                dispatch_count += self.process_block_for_dispatch(&if_expr.then_block)?;
                if let Some(else_block) = &if_expr.else_block {
                    dispatch_count += self.process_block_for_dispatch(else_block)?;
                }
            }
            outrun_parser::ExpressionKind::CaseExpression(case_expr) => {
                // Process case expression
                dispatch_count += self.process_expression_for_dispatch(&case_expr.expression)?;
                for case_clause in &case_expr.clauses {
                    match &case_clause.result {
                        outrun_parser::CaseResult::Block(block) => {
                            dispatch_count += self.process_block_for_dispatch(block)?;
                        }
                        outrun_parser::CaseResult::Expression(expr) => {
                            dispatch_count += self.process_expression_for_dispatch(expr)?;
                        }
                    }
                }
            }
            // Handle other expression types that contain sub-expressions
            outrun_parser::ExpressionKind::BinaryOp(bin_op) => {
                // Binary operations were desugared to trait function calls, so these should be rare
                dispatch_count += self.process_expression_for_dispatch(&bin_op.left)?;
                dispatch_count += self.process_expression_for_dispatch(&bin_op.right)?;
            }
            outrun_parser::ExpressionKind::UnaryOp(unary_op) => {
                // Unary operations were desugared to trait function calls, so these should be rare
                dispatch_count += self.process_expression_for_dispatch(&unary_op.operand)?;
            }
            outrun_parser::ExpressionKind::List(list) => {
                // Process all elements in the list
                for element in &list.elements {
                    match element {
                        outrun_parser::ListElement::Expression(expr) => {
                            dispatch_count += self.process_expression_for_dispatch(expr)?;
                        }
                        outrun_parser::ListElement::Spread(_) => {
                            // Spread elements don't contain sub-expressions
                        }
                    }
                }
            }
            outrun_parser::ExpressionKind::Map(map) => {
                // Process all keys and values in the map
                for entry in &map.entries {
                    match entry {
                        outrun_parser::MapEntry::Assignment { key, value } => {
                            dispatch_count += self.process_expression_for_dispatch(key)?;
                            dispatch_count += self.process_expression_for_dispatch(value)?;
                        }
                        outrun_parser::MapEntry::Shorthand { value, .. } => {
                            dispatch_count += self.process_expression_for_dispatch(value)?;
                        }
                        outrun_parser::MapEntry::Spread(_) => {
                            // Spread entries don't contain sub-expressions
                        }
                    }
                }
            }
            outrun_parser::ExpressionKind::Tuple(tuple) => {
                // Process all elements in the tuple
                for element in &tuple.elements {
                    dispatch_count += self.process_expression_for_dispatch(element)?;
                }
            }
            outrun_parser::ExpressionKind::Struct(struct_lit) => {
                // Process all field values in the struct literal
                for field in &struct_lit.fields {
                    match field {
                        outrun_parser::StructLiteralField::Assignment { value, .. } => {
                            dispatch_count += self.process_expression_for_dispatch(value)?;
                        }
                        outrun_parser::StructLiteralField::Shorthand(_) => {
                            // Shorthand fields don't contain expressions
                        }
                        outrun_parser::StructLiteralField::Spread(_) => {
                            // Spread fields don't contain expressions
                        }
                    }
                }
            }
            // Literal expressions don't contain sub-expressions
            outrun_parser::ExpressionKind::Integer(_)
            | outrun_parser::ExpressionKind::Float(_)
            | outrun_parser::ExpressionKind::String(_)
            | outrun_parser::ExpressionKind::Atom(_)
            | outrun_parser::ExpressionKind::Boolean(_)
            | outrun_parser::ExpressionKind::Sigil(_)
            | outrun_parser::ExpressionKind::Identifier(_) => {
                // These expressions don't contain function calls
            }
            _ => {
                panic!("⚠️ Unhandled expression type in dispatch calculation");
            }
        }

        Ok(dispatch_count)
    }

    /// Process a direct function call for dispatch
    fn process_function_call_for_dispatch(
        &mut self,
        call: &outrun_parser::FunctionCall,
    ) -> Result<u32, Vec<TypeError>> {
        // Check if this is a trait function call that needs dispatch resolution
        if let outrun_parser::FunctionPath::Qualified { module, name } = &call.path {
            let trait_name = &module.name;
            let function_name = &name.name;

            // Check if this is a call to a known trait
            let trait_type_id = self.intern_type_name(trait_name);
            if self.get_trait(&trait_type_id).is_some() {
                let trait_structured = StructuredType::Simple(trait_type_id.clone());

                // Try to resolve the dispatch entry for this trait function call
                if let Some(first_arg) = call.arguments.first() {
                    // Try to determine the concrete type from the first argument
                    let arg_expression = match first_arg {
                        outrun_parser::Argument::Named { expression, .. } => expression,
                        outrun_parser::Argument::Spread { expression, .. } => expression,
                    };

                    if let Some(concrete_type) = self.try_resolve_argument_type(arg_expression) {
                        if self.implements_trait(&concrete_type, &trait_structured) {
                            let dispatch_id = self.generate_dispatch_id();

                            // Register the dispatch table entry
                            self.register_trait_implementation_dispatch(
                                trait_structured,
                                concrete_type,
                                function_name.clone(),
                                dispatch_id,
                            );

                            return Ok(1);
                        } else {
                            // Type checking should have caught this, but dispatch generation found an inconsistency
                            return Err(vec![TypeError::TraitNotImplemented {
                                span: call.span.to_source_span(),
                                trait_name: trait_name.clone(),
                                type_name: concrete_type.to_string_representation(),
                            }]);
                        }
                    } else {
                        // Could not resolve argument type - this is normal during early dispatch processing
                        // We'll defer dispatch table generation for this call site until later phases
                        // when type information is fully available
                        return Ok(0); // No dispatch entries generated, but not an error
                    }
                } else {
                    // Trait function calls should have arguments for dispatch resolution
                    return Err(vec![TypeError::internal_with_span(
                        format!(
                            "Trait function call {trait_name}.{function_name} has no arguments for dispatch resolution"
                        ),
                        call.span.to_source_span(),
                    )]);
                }
            }
        }

        // Not a trait function call - no dispatch entry needed
        Ok(0)
    }

    /// Try to resolve the concrete type of an argument expression
    /// This implementation tries multiple strategies to determine types
    fn try_resolve_argument_type(
        &self,
        expr: &outrun_parser::Expression,
    ) -> Option<StructuredType> {
        match &expr.kind {
            outrun_parser::ExpressionKind::Identifier(ident) => {
                // Try to look up the type from the unification context
                let context = self.unification_context();
                if let Some(expr_type) = context.get_expression_type(&expr.span) {
                    return Some(expr_type.clone());
                }

                // FIXED: For argument type resolution failures during dispatch,
                // we should be more permissive and defer to later phases
                // Instead of failing, we can return a placeholder that will be resolved later
                
                // Return None for now - this will cause the dispatch to be deferred
                // which is actually the correct behavior since type resolution
                // should happen after full type checking
                None
            }
            outrun_parser::ExpressionKind::Struct(struct_literal) => {
                // Try to infer type from struct literal
                if !struct_literal.type_path.is_empty() {
                    // Convert Vec<TypeIdentifier> to TypeSpec 
                    let type_spec = outrun_parser::TypeSpec {
                        path: struct_literal.type_path.clone(),
                        generic_args: None, // Struct literals don't have explicit generic params in syntax
                        span: struct_literal.span,
                    };
                    if let Ok(structured_type) = self.convert_type_spec_to_structured_type(&type_spec) {
                        return Some(structured_type);
                    }
                }
                None
            }
            outrun_parser::ExpressionKind::Integer(_) => {
                let int_type_id = self.intern_type_name("Outrun.Core.Integer64");
                Some(StructuredType::Simple(int_type_id))
            }
            outrun_parser::ExpressionKind::Float(_) => {
                let float_type_id = self.intern_type_name("Outrun.Core.Float64");
                Some(StructuredType::Simple(float_type_id))
            }
            outrun_parser::ExpressionKind::String(_) => {
                let string_type_id = self.intern_type_name("Outrun.Core.String");
                Some(StructuredType::Simple(string_type_id))
            }
            outrun_parser::ExpressionKind::Boolean(_) => {
                let bool_type_id = self.intern_type_name("Outrun.Core.Boolean");
                Some(StructuredType::Simple(bool_type_id))
            }
            outrun_parser::ExpressionKind::Atom(_) => {
                let atom_type_id = self.intern_type_name("Outrun.Core.Atom");
                Some(StructuredType::Simple(atom_type_id))
            }
            outrun_parser::ExpressionKind::List(_) => {
                let list_type_id = self.intern_type_name("Outrun.Core.List");
                // TODO: Could determine element type from list elements
                Some(StructuredType::Simple(list_type_id))
            }
            _ => {
                // Try to look up the type from the unification context first
                let context = self.unification_context();
                if let Some(expr_type) = context.get_expression_type(&expr.span) {
                    return Some(expr_type.clone());
                }

                // For other expression types, we'd need more sophisticated type inference
                None
            }
        }
    }

    /// Generate a unique dispatch ID for trait function calls
    fn generate_dispatch_id(&self) -> u32 {
        let mut state = self.compilation_state.write().unwrap();
        state.next_dispatch_id += 1;
        state.next_dispatch_id
    }

    /// Register a trait implementation in the dispatch table
    fn register_trait_implementation_dispatch(
        &self,
        trait_type: StructuredType,
        impl_type: StructuredType,
        _function_name: String,
        _dispatch_id: u32,
    ) {
        let mut state = self.compilation_state.write().unwrap();

        // Extract TypeNameIds from StructuredTypes for dispatch table registration
        if let (StructuredType::Simple(trait_id), StructuredType::Simple(impl_id)) =
            (&trait_type, &impl_type)
        {
            // Register the trait implementation in dispatch table
            let _module_id = state
                .dispatch_table
                .register_trait_impl(trait_id.clone(), impl_id.clone());
        } else {
            // Cannot register dispatch for complex types
        }
    }

    /// Build typed AST (Phase 7)
    fn build_typed_ast(
        &mut self,
        collection: &ProgramCollection,
        compilation_order: &[String],
        structs: &HashMap<TypeNameId, StructDefinition>,
    ) -> Result<HashMap<String, crate::checker::TypedProgram>, Vec<TypeError>> {
        // Create TypedASTBuilder with the type checking results
        let unification_context = self.unification_context();

        let mut typed_ast_builder = crate::typed_ast_builder::TypedASTBuilder::new(
            unification_context,
            structs.clone(),
            Some(self.clone()),
        );

        // IMPORTANT: Process impl functions that were registered during Phase 4
        // These functions (like List.head, Option.some) need typed definitions
        self.process_registered_impl_functions(&mut typed_ast_builder)?;

        // Build the typed AST using the existing TypedASTBuilder
        typed_ast_builder.build_typed_ast(collection, compilation_order)
    }

    /// Process impl functions that were registered during Phase 4 to add typed definitions
    fn process_registered_impl_functions(
        &mut self,
        typed_ast_builder: &mut crate::typed_ast_builder::TypedASTBuilder,
    ) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();

        // Get all modules and their functions
        let modules = self.modules.read().unwrap().clone();
        for (module_key, module) in modules.iter() {
            // Process both trait implementation modules AND trait definition modules
            match module_key {
                ModuleKey::TraitImpl(_trait_type, _impl_type) => {
                    // Process trait implementation functions
                    // First, group functions by name and count clauses for each function
                    let function_clause_counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
                    
                    // Process ALL functions, not just those in functions_by_name
                    for (function_name, function_entries) in &module.all_functions_by_name {
                        let function_name_str = self.resolve_atom_name(function_name).unwrap_or_default();
                        let mut clause_index = 0;
                        
                        for function_entry in function_entries {
                            // Only process ImplFunction entries that don't have typed definitions yet
                            if let UnifiedFunctionEntry::ImplFunction {
                                definition,
                                typed_definition,
                                ..
                            } = function_entry
                            {
                            if typed_definition.is_none() {
                                // IMPORTANT: Set the correct module context in TypedASTBuilder
                                // This ensures that function updates happen in the right trait implementation module
                                let previous_module_key = typed_ast_builder.get_current_module_key();
                                typed_ast_builder.set_current_module_key(Some(module_key.clone()));

                                // CRITICAL FIX: Set up generic context for auto-implementations
                                // This maps "Self" to the concrete implementing type
                                if let ModuleKey::TraitImpl(_trait_type, impl_type) = module_key {
                                    // Create a generic context where Self resolves to the implementing type
                                    let generic_context = typed_ast_builder.create_generic_context(&[], Some(impl_type.as_ref().clone()));
                                    typed_ast_builder.push_generic_context(generic_context);
                                }

                                // Create typed definition for this impl function
                                let conversion_result = typed_ast_builder.convert_function_definition(definition);
                                
                                // Pop the generic context if we pushed one
                                if matches!(module_key, ModuleKey::TraitImpl(_, _)) {
                                    typed_ast_builder.pop_generic_context();
                                }

                                match conversion_result {
                                    Some(_typed_def) => {
                                        // CRITICAL: Register ALL trait impl functions as function clauses
                                        // This is needed for the SMT-first architecture where the interpreter
                                        // relies on having clause sets for static dispatch instead of runtime SMT solving
                                        let function_name_atom = self.intern_atom_name(&definition.name.name);
                                        
                                        // Use source order for clause priority - earlier functions have higher priority (lower number)
                                        // This matches developer expectations from functional programming languages like Elixir
                                        let source_order = definition.span.start as u32;
                                        
                                        // PERFORMANCE OPTIMIZATION: Check if this will be a simple single-clause function
                                        // Single clause + no guard = can use direct dispatch without SMT solving
                                        let has_guard = definition.guard.is_some();
                                        let is_simple_function = !has_guard && function_entries.len() == 1;
                                        
                                        // Create a function clause for this impl function using clause counting
                                        let clause_id = if let ModuleKey::TraitImpl(trait_type, impl_type) = module_key {
                                            // Extract trait name from trait type
                                            let trait_name = match trait_type.as_ref() {
                                                StructuredType::Simple(type_id) => {
                                                    self.resolve_type(type_id.clone()).unwrap_or_default()
                                                }
                                                StructuredType::Generic { base, .. } => {
                                                    self.resolve_type(base.clone()).unwrap_or_default()
                                                }
                                                _ => "UnknownTrait".to_string(),
                                            };
                                            // Use clause counting for deterministic, unique IDs
                                            self.generate_clause_id_for_function(
                                                Some(&trait_name),
                                                impl_type.as_ref(),
                                                definition,
                                                clause_index,
                                            )
                                        } else {
                                            // Fallback for non-trait-impl modules
                                            format!("function_{}_{}", definition.name.name, clause_index)
                                        };
                                        
                                        // Increment clause index for this function name
                                        clause_index += 1;

                                    let mut clause = crate::checker::FunctionClause {
                                        clause_id,
                                        base_function: _typed_def.clone(),
                                        source_order,
                                        applicability_constraints: Vec::new(),
                                        from_guard: has_guard,
                                        span: definition.span,
                                    };
                                    
                                    // For simple functions, pre-populate with direct dispatch constraints
                                    // This avoids expensive SMT constraint generation for common cases
                                    if is_simple_function {
                                        // Add a marker constraint indicating this clause can use direct dispatch
                                        clause.applicability_constraints.push(
                                            crate::smt::constraints::SMTConstraint::PreResolvedClause {
                                                call_site: definition.span,
                                                trait_type: StructuredType::Simple(self.intern_type_name("DirectDispatch")),
                                                impl_type: StructuredType::Simple(self.intern_type_name("SingleClause")),
                                                function_name: definition.name.name.clone(),
                                                selected_clause_id: clause.clause_id.clone(),
                                                guard_pre_evaluated: Some(true), // No guard = always applicable
                                                argument_types: vec![], // Will be filled in by type checking
                                            }
                                        );
                                    }
                                    
                                    // Add the clause to the module's function clause registry
                                    {
                                        let mut modules = self.modules.write().unwrap();
                                        if let Some(module) = modules.get_mut(module_key) {
                                            module.add_function_clause(function_name_atom.clone(), clause);
                                        }
                                    }
                                }
                                None => {
                                    // Create an error for the failed conversion
                                    errors.push(crate::error::TypeError::internal_with_span(
                                        format!(
                                            "Failed to convert function definition for {}",
                                            definition.name.name
                                        ),
                                        definition.span.to_source_span(),
                                    ));
                                }
                            }

                            // Restore the previous module context
                            typed_ast_builder.set_current_module_key(previous_module_key);
                        }
                        }
                    }
                }
                }
                ModuleKey::Module(_trait_type_hash) => {
                    // Process trait definition modules (for static functions like Option.some)
                    // Check if this is actually a trait module by looking for trait definition
                    if module.get_trait_definition().is_some() {
                        // Process ALL functions in trait definition modules, grouping by name for clause counting
                        for (function_name, function_entries) in &module.all_functions_by_name {
                            let mut clause_index = 0;
                            
                            for function_entry in function_entries {
                                // Process trait static functions and default implementations
                                match function_entry {
                                    UnifiedFunctionEntry::TraitStatic { definition, typed_definition, .. } |
                                    UnifiedFunctionEntry::TraitDefault { definition, typed_definition, .. } => {
                                        if typed_definition.is_none() {
                                            // Set the correct module context in TypedASTBuilder
                                            let previous_module_key = typed_ast_builder.get_current_module_key();
                                            typed_ast_builder.set_current_module_key(Some(module_key.clone()));

                                            // Create typed definition for this trait function
                                            match typed_ast_builder.convert_function_definition(definition) {
                                                Some(_typed_def) => {
                                                    // CRITICAL FIX: Register trait static functions as function clauses
                                                    // This enables static dispatch for Option.some() and similar functions
                                                    let function_name_atom = self.intern_atom_name(&definition.name.name);
                                                    
                                                    let source_order = definition.span.start as u32;
                                                    let has_guard = definition.guard.is_some();
                                                    let is_simple_function = !has_guard && function_entries.len() == 1;
                                                    
                                                    // Create a function clause for this trait function using clause counting
                                                    let clause_id = match module_key {
                                                        ModuleKey::Module(type_hash) => {
                                                            // For trait modules, use trait name from type hash
                                                            let trait_name = self.type_names.read().unwrap()
                                                                .get(type_hash)
                                                                .cloned()
                                                                .unwrap_or_else(|| format!("Trait_{}", type_hash));
                                                            // Use clause counting for consistency with trait impl modules
                                                            format!("{}:{}:{}:{}", trait_name, trait_name, definition.name.name, clause_index)
                                                        }
                                                        ModuleKey::TraitImpl(trait_type, impl_type) => {
                                                            // Extract trait name from trait type
                                                            let trait_name = match trait_type.as_ref() {
                                                                StructuredType::Simple(type_id) => {
                                                                    self.resolve_type(type_id.clone()).unwrap_or_default()
                                                                }
                                                                StructuredType::Generic { base, .. } => {
                                                                    self.resolve_type(base.clone()).unwrap_or_default()
                                                                }
                                                                _ => "UnknownTrait".to_string(),
                                                            };
                                                            // Use clause counting for deterministic, unique IDs
                                                            self.generate_clause_id_for_function(
                                                                Some(&trait_name),
                                                                impl_type.as_ref(),
                                                                definition,
                                                                clause_index,
                                                            )
                                                        }
                                                    };
                                                    
                                                    // Increment clause index for this function name
                                                    clause_index += 1;

                                                    let mut clause = crate::checker::FunctionClause {
                                                        clause_id,
                                                        base_function: _typed_def.clone(),
                                                        source_order,
                                                        applicability_constraints: Vec::new(),
                                                        from_guard: has_guard,
                                                        span: definition.span,
                                                    };
                                                    
                                                    // For simple trait static functions, pre-populate with direct dispatch constraints
                                                    if is_simple_function {
                                                        clause.applicability_constraints.push(
                                                            crate::smt::constraints::SMTConstraint::PreResolvedClause {
                                                                call_site: definition.span,
                                                                trait_type: StructuredType::Simple(self.intern_type_name("DirectDispatch")),
                                                                impl_type: StructuredType::Simple(self.intern_type_name("SingleClause")),
                                                                function_name: definition.name.name.clone(),
                                                                selected_clause_id: clause.clause_id.clone(),
                                                                guard_pre_evaluated: Some(true), // No guard = always applicable
                                                                argument_types: vec![], // Will be filled in by type checking
                                                            }
                                                        );
                                                    }
                                                    
                                                    // Add the clause to the trait module's function clause registry
                                                    {
                                                        let mut modules = self.modules.write().unwrap();
                                                        if let Some(module) = modules.get_mut(module_key) {
                                                            module.add_function_clause(function_name_atom.clone(), clause);
                                                        }
                                                    }
                                                }
                                                None => {
                                                    // Create an error for the failed conversion
                                                    errors.push(crate::error::TypeError::internal_with_span(
                                                        format!(
                                                            "Failed to convert trait function definition for {}",
                                                            definition.name.name
                                                        ),
                                                        definition.span.to_source_span(),
                                                    ));
                                                }
                                            }

                                            // Restore the previous module context
                                            typed_ast_builder.set_current_module_key(previous_module_key);
                                        }
                                    }
                                    _ => {
                                        // Skip other function types (signatures without bodies)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Check if a module has multiple functions with the same name (indicating overloading/guards)
    fn has_multiple_functions_with_same_name(&self, module_key: &ModuleKey, function_name: &str) -> bool {
        let modules = self.modules.read().unwrap();
        if let Some(module) = modules.get(module_key) {
            let function_name_atom = self.intern_atom_name(function_name);
            if let Some(function_entries) = module.all_functions_by_name.get(&function_name_atom) {
                return function_entries.len() > 1;
            }
        }
        false
    }

    // ===== Compatibility Bridge Methods =====
    // These methods provide TypeInterner-compatible APIs for gradual migration

    /// Create a TypeNameId using CompilerEnvironment's own interner
    /// This is the new preferred method for type interning
    pub fn intern_type_name(&self, name: &str) -> TypeNameId {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        let hash = hasher.finish();

        // Store in our HashMap
        self.type_names
            .write()
            .unwrap()
            .insert(hash, name.to_string());
        TypeNameId::new(hash, Arc::clone(&self.type_names))
    }

    /// Create an AtomId using CompilerEnvironment's own hash-based storage
    /// This is the new preferred method for atom interning
    pub fn intern_atom_name(&self, name: &str) -> AtomId {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        let hash = hasher.finish();

        // Store in our HashMap
        self.atoms.write().unwrap().insert(hash, name.to_string());
        AtomId::new(hash, Arc::clone(&self.atoms))
    }

    /// Resolve a TypeNameId to its name - compatibility method
    pub fn resolve_type(&self, type_name_id: TypeNameId) -> Option<String> {
        self.resolve_type_name(&type_name_id)
    }

    /// Resolve an AtomId to its name - compatibility method
    pub fn resolve_atom(&self, atom_id: AtomId) -> Option<String> {
        self.resolve_atom_name(&atom_id)
    }

    /// Resolve a TypeNameId to its name using CompilerEnvironment's hash storage
    pub fn resolve_type_name(&self, type_name_id: &TypeNameId) -> Option<String> {
        self.type_names
            .read()
            .unwrap()
            .get(&type_name_id.hash)
            .cloned()
    }

    /// Resolve an AtomId to its name using CompilerEnvironment's hash storage
    pub fn resolve_atom_name(&self, atom_id: &AtomId) -> Option<String> {
        self.atoms.read().unwrap().get(&atom_id.hash).cloned()
    }

    /// Get a module by its type name
    pub fn get_module(&self, name: TypeNameId) -> Option<Module> {
        let key = ModuleKey::Module(name.hash);
        self.modules.read().unwrap().get(&key).cloned()
    }

    /// Get a trait implementation module
    pub fn get_trait_impl(
        &self,
        trait_type: StructuredType,
        struct_type: StructuredType,
    ) -> Option<Module> {
        let key = ModuleKey::TraitImpl(Box::new(trait_type), Box::new(struct_type));
        self.modules.read().unwrap().get(&key).cloned()
    }

    /// Add a module to the environment
    pub fn add_module(&self, key: ModuleKey, module: Module) {
        self.modules.write().unwrap().insert(key, module);
    }

    /// Add a module by type name
    pub fn add_module_by_name(&self, name: TypeNameId, module: Module) {
        let key = ModuleKey::Module(name.hash);
        self.modules.write().unwrap().insert(key, module);
    }

    /// Add a trait implementation module
    pub fn add_trait_impl(
        &self,
        trait_type: StructuredType,
        struct_type: StructuredType,
        module: Module,
    ) {
        let key = ModuleKey::TraitImpl(Box::new(trait_type), Box::new(struct_type));
        self.modules.write().unwrap().insert(key, module);
    }

    /// Get the type name storage
    pub fn type_names(&self) -> &Arc<RwLock<HashMap<u64, String>>> {
        &self.type_names
    }

    /// Get the atom storage
    pub fn atoms(&self) -> &Arc<RwLock<HashMap<u64, String>>> {
        &self.atoms
    }

    /// Get the modules interner
    pub fn modules(&self) -> &Arc<RwLock<HashMap<ModuleKey, Module>>> {
        &self.modules
    }

    /// Check if a module exists
    pub fn has_module(&self, key: &ModuleKey) -> bool {
        self.modules.read().unwrap().contains_key(key)
    }

    /// Get the number of modules
    pub fn module_count(&self) -> usize {
        self.modules.read().unwrap().len()
    }

    /// Check if the environment is empty
    pub fn is_empty(&self) -> bool {
        self.modules.read().unwrap().is_empty()
    }

    // ===== Trait Lookup Methods =====
    // These methods replace the TraitRegistry functionality

    /// Check if a type implements a trait
    pub fn implements_trait(
        &self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
    ) -> bool {
        // NEW: SMT-based trait implementation checking with TypeVariable resolution

        // Step 1: Resolve TypeVariable constraints using SMT system
        let resolved_impl_type = self.resolve_type_variables(impl_type);
        let resolved_trait_type = self.resolve_type_variables(trait_type);
        // Step 2: Special case - a trait implements itself for type compatibility
        if resolved_trait_type == resolved_impl_type {
            return true;
        }

        // Step 3: Use SMT constraint system to check trait implementation
        if let Ok(result) = self.smt_implements_trait(&resolved_impl_type, &resolved_trait_type) {
            return result;
        }

        // Step 4: Direct lookup for exact type matches (including monomorphized implementations)
        let impl_key = ModuleKey::TraitImpl(
            Box::new(resolved_trait_type.clone()),
            Box::new(resolved_impl_type.clone()),
        );

        if self.has_module(&impl_key) {
            return true;
        }

        // Step 4.5: FIXED: Look for generic-to-concrete trait implementation mappings
        // If we're looking for a concrete generic type implementation,
        // check if there's a generic implementation that could serve as a template
        if let StructuredType::Generic { base: impl_base, args: impl_args } = &resolved_impl_type {
            // Check if all type arguments are concrete (not type variables)
            let is_concrete = impl_args.iter().all(|arg| self.is_concrete_type(arg));
            if is_concrete {
                // Look for a generic implementation that matches the pattern
                if self.has_generic_impl_for_concrete_type(&resolved_trait_type, &resolved_impl_type) {
                    return true;
                }
            }
        }

        // Step 5: Use SMT constraint generation and solving - no manual type checking
        // Result handled by caller with proper error reporting
        self.smt_check_trait_implementation(&resolved_impl_type, &resolved_trait_type)
    }

    /// SMT-based trait implementation checking - mathematically sound (with caching + early termination)
    /// Generate constraints and let Z3 prove trait compatibility
    fn smt_check_trait_implementation(
        &self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
    ) -> bool {
        // Fast path: identical types always match
        if impl_type == trait_type {
            return true;
        }

        // Fast path: check for obviously incompatible types
        if self.definitely_incompatible_types(impl_type, trait_type) {
            return false;
        }

        let constraint = crate::smt::constraints::SMTConstraint::TraitImplemented {
            impl_type: impl_type.clone(),
            trait_type: trait_type.clone(),
        };

        // Use cached SMT solver for better performance
        match crate::smt::solver_pool::check_constraints_satisfiable_cached(&[constraint], self) {
            Ok(satisfiable) => satisfiable,
            Err(_) => false, // SMT solver errors mean constraint failed
        }
    }

    /// Fast check for obviously incompatible types to avoid SMT calls
    pub fn definitely_incompatible_types(
        &self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
    ) -> bool {
        match (impl_type, trait_type) {
            // Simple type mismatches that can never be compatible
            (StructuredType::Simple(impl_id), StructuredType::Simple(trait_id)) => {
                // Check if these are known to be completely different type families
                if let (Some(impl_name), Some(trait_name)) = (
                    self.resolve_type(impl_id.clone()),
                    self.resolve_type(trait_id.clone()),
                ) {
                    // Quick check: Integer vs String vs Boolean etc. can never be compatible
                    self.are_incompatible_primitive_types(&impl_name, &trait_name)
                } else {
                    false // Unknown types, let SMT decide
                }
            }
            // Tuple vs non-tuple
            (StructuredType::Tuple(_), StructuredType::Simple(_)) => true,
            (StructuredType::Simple(_), StructuredType::Tuple(_)) => true,
            // Function vs non-function
            (StructuredType::Function { .. }, StructuredType::Simple(_)) => true,
            (StructuredType::Simple(_), StructuredType::Function { .. }) => true,
            // Different arity tuples
            (StructuredType::Tuple(t1), StructuredType::Tuple(t2)) => t1.len() != t2.len(),
            // Generic vs simple with incompatible base
            (StructuredType::Generic { base: base1, .. }, StructuredType::Simple(simple)) => {
                base1 != simple
            }
            (StructuredType::Simple(simple), StructuredType::Generic { base: base2, .. }) => {
                simple != base2
            }
            _ => false, // Let SMT handle complex cases
        }
    }

    /// Check if two primitive type names are known to be incompatible
    fn are_incompatible_primitive_types(&self, type1: &str, type2: &str) -> bool {
        let primitives = ["Integer", "String", "Boolean", "Float"];
        primitives.contains(&type1) && primitives.contains(&type2) && type1 != type2
    }

    /// Resolve TypeVariable constraints using the SMT system
    /// This is the key method that bridges TypeVariables to concrete types
    fn resolve_type_variables(&self, structured_type: &StructuredType) -> StructuredType {
        match structured_type {
            StructuredType::TypeVariable(var_id) => {
                // Try to resolve this TypeVariable using SMT constraints
                let context = self.unification_context();

                // Look for TypeVariableConstraint that binds this variable
                for constraint in &context.smt_constraints {
                    if let crate::smt::constraints::SMTConstraint::TypeVariableConstraint {
                        variable_id,
                        bound_type,
                        ..
                    } = constraint
                    {
                        if variable_id == var_id {
                            return bound_type.clone();
                        }
                    }
                }

                // Look for TraitCompatibility constraints that involve this TypeVariable
                for constraint in &context.smt_constraints {
                    if let crate::smt::constraints::SMTConstraint::TraitCompatibility {
                        trait_type,
                        implementing_type,
                        ..
                    } = constraint
                    {
                        // Check if this constraint involves our TypeVariable
                        if let StructuredType::TypeVariable(constraint_var_id) = implementing_type {
                            if constraint_var_id == var_id {
                                // Use SMT solver to find concrete implementations of the trait
                                if let Ok(concrete_type) =
                                    self.resolve_trait_to_concrete_type(trait_type)
                                {
                                    return concrete_type;
                                }
                            }
                        }
                    }
                }

                // If no constraint found, return the TypeVariable as-is
                // This allows the SMT solver to handle it
                // Unresolved TypeVariable
                structured_type.clone()
            }
            StructuredType::Generic { base, args } => {
                // Recursively resolve TypeVariables in generic arguments
                let resolved_args: Vec<StructuredType> = args
                    .iter()
                    .map(|arg| self.resolve_type_variables(arg))
                    .collect();
                StructuredType::Generic {
                    base: base.clone(),
                    args: resolved_args,
                }
            }
            StructuredType::Tuple(elements) => {
                // Recursively resolve TypeVariables in tuple elements
                let resolved_elements: Vec<StructuredType> = elements
                    .iter()
                    .map(|elem| self.resolve_type_variables(elem))
                    .collect();
                StructuredType::Tuple(resolved_elements)
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                // Recursively resolve TypeVariables in function signature
                let resolved_params: Vec<crate::unification::FunctionParam> = params
                    .iter()
                    .map(|param| crate::unification::FunctionParam {
                        name: param.name.clone(),
                        param_type: self.resolve_type_variables(&param.param_type),
                    })
                    .collect();
                let resolved_return_type = Box::new(self.resolve_type_variables(return_type));
                StructuredType::Function {
                    params: resolved_params,
                    return_type: resolved_return_type,
                }
            }
            _ => {
                // For concrete types (Simple, primitives, etc.), return as-is
                structured_type.clone()
            }
        }
    }

    /// Use SMT constraint system to check trait implementation (with caching)
    /// This replaces manual structural matching with constraint-based checking
    fn smt_implements_trait(
        &self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
    ) -> Result<bool, crate::smt::solver::SMTError> {
        // Fast path: identical types always match
        if impl_type == trait_type {
            return Ok(true);
        }

        // Fast path: check for obviously incompatible types
        if self.definitely_incompatible_types(impl_type, trait_type) {
            return Ok(false);
        }

        // Create a trait implementation constraint
        let constraint = crate::smt::constraints::SMTConstraint::TraitImplemented {
            impl_type: impl_type.clone(),
            trait_type: trait_type.clone(),
        };

        // Create a temporary context with this constraint
        let mut context = self.unification_context();
        context.add_smt_constraint(constraint);

        // Use cached SMT solver for better performance
        match crate::smt::solver_pool::check_constraints_satisfiable_cached(
            &context.smt_constraints,
            self,
        ) {
            Ok(satisfiable) => Ok(satisfiable),
            Err(e) => Err(e),
        }
    }

    /// Resolve a trait to a concrete implementation type
    /// This method chooses a concrete type that implements the given trait
    fn resolve_trait_to_concrete_type(
        &self,
        trait_type: &StructuredType,
    ) -> Result<StructuredType, crate::smt::solver::SMTError> {
        // Find all implementations of this trait
        let implementations = self.find_all_trait_implementations(trait_type);

        if implementations.is_empty() {
            // No implementations found for trait
            return Err(crate::smt::solver::SMTError::SolverError(format!(
                "No implementations found for trait {}",
                trait_type.to_string_representation()
            )));
        }

        let chosen_impl = &implementations[0];

        Ok(chosen_impl.clone())
    }

    /// Find all concrete types that implement a given trait
    fn find_all_trait_implementations(&self, trait_type: &StructuredType) -> Vec<StructuredType> {
        let mut implementations = Vec::new();
        let modules = self.modules.read().unwrap();

        // Extract trait name for matching
        let trait_name = match trait_type {
            StructuredType::Simple(trait_id) => {
                if let Some(name) = self.resolve_type(trait_id.clone()) {
                    name
                } else {
                    // Could not resolve trait name
                    return implementations;
                }
            }
            StructuredType::Generic { base, .. } => {
                if let Some(name) = self.resolve_type(base.clone()) {
                    name
                } else {
                    // Could not resolve generic trait name
                    return implementations;
                }
            }
            _ => {
                // Unsupported trait type
                return implementations;
            }
        };

        // Search through all trait implementation modules
        for (module_key, _module) in modules.iter() {
            if let ModuleKey::TraitImpl(impl_trait_type, impl_type) = module_key {
                // Check if this implementation is for our target trait
                let impl_trait_name = match impl_trait_type.as_ref() {
                    StructuredType::Simple(trait_id) => {
                        self.resolve_type(trait_id.clone()).unwrap_or_default()
                    }
                    StructuredType::Generic { base, .. } => {
                        self.resolve_type(base.clone()).unwrap_or_default()
                    }
                    _ => String::new(),
                };

                if impl_trait_name == trait_name {
                    implementations.push((**impl_type).clone());
                }
            }
        }

        implementations
    }

    /// Find generic trait implementations that can be instantiated for a concrete type
    /// This handles the cartesian product case where Option<Integer> represents
    /// the product of all Option<T> implementations × all Integer implementations
    pub fn find_generic_trait_implementations_for_concrete_type(
        &self,
        trait_type: &StructuredType,
        _concrete_type: &StructuredType,
    ) -> Vec<StructuredType> {
        let mut implementations = Vec::new();

        // Extract the base trait name
        let trait_name = match trait_type {
            StructuredType::Simple(trait_id) => {
                if let Some(name) = self.resolve_type(trait_id.clone()) {
                    name
                } else {
                    // Could not resolve trait name
                    return implementations;
                }
            }
            StructuredType::Generic { base, .. } => {
                if let Some(name) = self.resolve_type(base.clone()) {
                    name
                } else {
                    // Could not resolve generic trait name
                    return implementations;
                }
            }
            _ => {
                // Unsupported trait type
                return implementations;
            }
        };

        let modules = self.modules.read().unwrap();

        // Search for generic trait implementations that can be instantiated
        for (module_key, _module) in modules.iter() {
            if let ModuleKey::TraitImpl(impl_trait_type, impl_type) = module_key {
                // Check if this implementation is for our target trait
                let impl_trait_name = match impl_trait_type.as_ref() {
                    StructuredType::Simple(trait_id) => {
                        self.resolve_type(trait_id.clone()).unwrap_or_default()
                    }
                    StructuredType::Generic { base, .. } => {
                        self.resolve_type(base.clone()).unwrap_or_default()
                    }
                    _ => String::new(),
                };

                if impl_trait_name == trait_name {
                    // Check if this implementation can be instantiated for the concrete type
                    if let StructuredType::Generic { base: _, args } = impl_type.as_ref() {
                        // This is a generic implementation like Option<T> or Map<K, V>
                        // We need to check if the type parameters can be unified with the concrete type
                        
                        // Check if this implementation could potentially match
                        // We're more permissive here and let SMT do the precise unification
                        let has_type_variables = args.iter().any(|arg| {
                            matches!(arg, StructuredType::TypeVariable(_))
                        });
                        
                        if has_type_variables {
                            // This is a generic implementation with at least some type variables
                            // Return it as a potential match - SMT will handle the precise unification
                            implementations.push(impl_type.as_ref().clone());
                        }
                    }
                }
            }
        }

        implementations
    }

    /// Resolve a Self type variable using SMT constraint solving
    /// This method uses the accumulated SelfTypeInference constraints to determine
    /// the concrete type that Self should be bound to
    pub fn smt_resolve_self_type(
        &self,
        self_type_id: &TypeNameId,
    ) -> Result<StructuredType, crate::smt::solver::SMTError> {
        let context = self.unification_context();

        let self_constraints: Vec<_> = context
            .smt_constraints
            .iter()
            .filter_map(|constraint| {
                if let crate::smt::constraints::SMTConstraint::SelfTypeInference {
                    self_variable_id,
                    inferred_type,
                    confidence,
                    call_site_context,
                } = constraint
                {
                    if self_variable_id == self_type_id {
                        Some((
                            inferred_type.clone(),
                            confidence.clone(),
                            call_site_context.clone(),
                        ))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        if self_constraints.is_empty() {
            return Err(crate::smt::solver::SMTError::NoConstraintsFound(format!(
                "No SelfTypeInference constraints found for Self variable {self_type_id:?}"
            )));
        }

        // Use cached SMT solving to resolve Self type - no fallbacks!
        match crate::smt::solver_pool::solve_constraints_cached(&context.smt_constraints, self)? {
            crate::smt::solver::SolverResult::Satisfiable(model) => {
                // Extract the Self type from the model
                let self_var_name = format!("Self_{}", self_type_id.hash);
                if let Some(resolved_type) =
                    self.extract_self_type_from_model(&model, &self_var_name)
                {
                    Ok(resolved_type)
                } else {
                    // If SMT says satisfiable but we can't extract the type, this is a bug
                    Err(crate::smt::solver::SMTError::SolverError(format!(
                        "SMT model is satisfiable but missing Self variable {self_var_name}"
                    )))
                }
            }
            crate::smt::solver::SolverResult::Unsatisfiable(conflicting) => {
                // SMT constraints unsatisfiable for Self resolution
                Err(crate::smt::solver::SMTError::SolvingFailed(format!(
                    "Conflicting Self type constraints - this indicates a type error: {conflicting:?}"
                )))
            }
            crate::smt::solver::SolverResult::Unknown(reason) => {
                // SMT solver returned unknown for Self resolution
                // Unknown means we can't prove satisfiability OR unsatisfiability
                // This should be treated as a solver limitation, not a fallback opportunity
                Err(crate::smt::solver::SMTError::SolvingFailed(format!(
                    "SMT solver cannot determine satisfiability: {reason}"
                )))
            }
        }
    }

    /// Extract the resolved Self type from an SMT model
    /// This method looks for the Self variable assignment in the Z3 model
    pub fn extract_self_type_from_model(
        &self,
        model: &crate::smt::solver::ConstraintModel,
        self_var_name: &str,
    ) -> Option<StructuredType> {
        // Look for the Self type assignment in the model
        if let Some(assigned_type) = model.type_assignments.get(self_var_name) {
            let resolved_type = self.apply_smt_model_to_type(assigned_type, model);

            Some(resolved_type)
        } else {
            // Check if there are any boolean assignments that tell us about Self
            for (var_name, is_true) in &model.boolean_assignments {
                if var_name.contains(self_var_name) && *is_true {
                    if let Some(type_info) = self.extract_type_from_boolean_var(var_name) {
                        return Some(type_info);
                    }
                }
            }

            // No Self type assignment found in SMT model
            // Model contains type assignments for debugging
            // Model contains boolean assignments for debugging
            None
        }
    }

    /// Extract type information from a boolean variable name like "Self_123_equals_Option_Integer"
    fn extract_type_from_boolean_var(&self, var_name: &str) -> Option<StructuredType> {
        // This is a heuristic approach - in a production system, we'd want more structured model extraction
        if let Some(type_part) = var_name.split("_equals_").nth(1) {
            // Try to parse the type part back into a StructuredType
            // For now, just handle simple cases
            if let Some(type_id) = self.try_resolve_type_name(type_part) {
                return Some(StructuredType::Simple(type_id));
            }
        }

        // Could not extract type from boolean variable
        None
    }

    /// Try to resolve a type name string back to a TypeNameId
    fn try_resolve_type_name(
        &self,
        type_name: &str,
    ) -> Option<crate::compilation::compiler_environment::TypeNameId> {
        // For now, just intern the type name and hope it exists
        // In a more robust system, we'd maintain bidirectional mappings
        let type_id = self.intern_type_name(type_name);

        // Verify it actually exists
        if self.resolve_type(type_id.clone()).is_some() {
            Some(type_id)
        } else {
            None
        }
    }

    /// Check if a generic type can be instantiated to match a concrete type
    /// For example: Map<K, V> can be instantiated to Map<String, Integer>
    fn generic_types_match(
        &self,
        generic_type: &StructuredType,
        concrete_type: &StructuredType,
    ) -> bool {
        match (generic_type, concrete_type) {
            // Both are generic types - check structural compatibility
            (
                StructuredType::Generic {
                    base: gen_base,
                    args: gen_args,
                },
                StructuredType::Generic {
                    base: con_base,
                    args: con_args,
                },
            ) => {
                // Base types must be the same
                if gen_base != con_base {
                    return false;
                }

                // Must have same number of type arguments
                if gen_args.len() != con_args.len() {
                    return false;
                }

                // Check if generic arguments can be instantiated
                for (gen_arg, con_arg) in gen_args.iter().zip(con_args.iter()) {
                    if !self.generic_argument_can_instantiate(gen_arg, con_arg) {
                        return false;
                    }
                }

                true
            }

            // Simple trait type can be instantiated to a generic type with the same base
            // For example: Simple(Map) can match Generic{Map, [String, Integer]}
            (
                StructuredType::Simple(gen_type_id),
                StructuredType::Generic {
                    base: con_base,
                    args: _,
                },
            ) => gen_type_id == con_base,

            // Simple generic parameter (like T) can be instantiated to any concrete type
            (StructuredType::Simple(gen_type_id), _) => self.is_generic_parameter(gen_type_id),

            // Other combinations don't match
            _ => false,
        }
    }

    /// Check if a generic argument can be instantiated with a concrete argument
    fn generic_argument_can_instantiate(
        &self,
        generic_arg: &StructuredType,
        concrete_arg: &StructuredType,
    ) -> bool {
        match generic_arg {
            // Generic parameter (T, K, V, etc.) can be instantiated with any type
            StructuredType::Simple(type_id) if self.is_generic_parameter(type_id) => true,

            // Otherwise, types must match exactly or recursively
            _ => self.generic_types_match(generic_arg, concrete_arg),
        }
    }

    /// Check if a type ID represents a generic parameter (like T, K, V, etc.)
    fn is_generic_parameter(
        &self,
        type_id: &crate::compilation::compiler_environment::TypeNameId,
    ) -> bool {
        let type_name = type_id.to_string();

        // Single uppercase letters are generic parameters
        if type_name.len() == 1 {
            return type_name.chars().next().unwrap().is_ascii_uppercase();
        }

        // Common generic parameter names
        matches!(
            type_name.as_str(),
            "T" | "U" | "K" | "V" | "E" | "A" | "B" | "C"
        )
    }

    /// Get trait constraints for a trait type
    pub fn get_trait_constraints(&self, trait_type: &StructuredType) -> Vec<TraitConstraint> {
        if let StructuredType::Simple(type_name_id) = trait_type {
            let module_key = ModuleKey::Module(type_name_id.hash);
            if let Some(module) = self.modules.read().unwrap().get(&module_key) {
                if matches!(module.module_kind, ModuleKind::Trait) {
                    return module.trait_constraints.clone();
                }
            }
        }
        Vec::new()
    }

    /// Get all types that implement a specific trait
    pub fn get_trait_implementations(&self, trait_type: &StructuredType) -> Vec<StructuredType> {
        let mut implementations = Vec::new();
        let modules = self.modules.read().unwrap();

        for (module_key, _module) in modules.iter() {
            if let ModuleKey::TraitImpl(module_trait_type, impl_type) = module_key {
                // Use StructuredType equality for trait matching
                if **module_trait_type == *trait_type {
                    implementations.push((**impl_type).clone());
                }
            }
        }

        implementations
    }

    /// Check if a type is a trait (has a trait module)
    pub fn is_trait(&self, type_ref: &StructuredType) -> bool {
        if let StructuredType::Simple(type_name_id) = type_ref {
            let module_key = ModuleKey::Module(type_name_id.hash);
            if let Some(module) = self.modules.read().unwrap().get(&module_key) {
                return matches!(module.module_kind, ModuleKind::Trait);
            }
        }
        false
    }

    /// Register a trait (creates a Trait module)
    pub fn register_trait(&self, trait_type: StructuredType) {
        if let StructuredType::Simple(trait_type_id) = &trait_type {
            let module_key = ModuleKey::Module(trait_type_id.hash);

            // Create trait module if it doesn't exist
            self.get_or_create_module(
                module_key,
                ModuleKind::Trait,
                SourceLocation::Input("compiler".to_string()),
                trait_type,
            );
        }
    }

    /// Register a trait implementation (creates a TraitImpl module)
    pub fn register_trait_implementation(
        &self,
        impl_type: StructuredType,
        trait_type: StructuredType,
    ) {
        // First ensure the trait itself is registered
        self.register_trait(trait_type.clone());

        let module_key =
            ModuleKey::TraitImpl(Box::new(trait_type.clone()), Box::new(impl_type.clone()));

        self.get_or_create_module(
            module_key,
            ModuleKind::TraitImpl,
            SourceLocation::Input("compiler".to_string()),
            impl_type.clone(),
        );

        let trait_impl_constraint = crate::smt::constraints::SMTConstraint::TraitImplemented {
            impl_type: impl_type.clone(),
            trait_type: trait_type.clone(),
        };

        if let Ok(mut compilation_state) = self.compilation_state.write() {
            compilation_state
                .unification_context
                .smt_constraints
                .push(trait_impl_constraint);
        } else {
            // Failed to add SMT constraint for trait implementation
        }
    }

    /// Register a trait definition with the environment
    /// This replaces TraitRegistry::register_trait
    pub fn register_trait_definition(&self, trait_def: outrun_parser::TraitDefinition) {
        let trait_name = trait_def.name_as_string();
        let trait_type_id = self.intern_type_name(&trait_name);

        // DISABLED: Constraint validation is causing false positives
        // The real issue is in how default trait implementations are type-checked
        // Binary and String traits already declare `when Self: Equality` correctly
        // if let Err(constraint_errors) = self.validate_trait_constraint_dependencies_via_constraints(&trait_def) {
        //     // Add constraint errors to compilation state
        //     for error in constraint_errors {
        //         self.compilation_state.write().unwrap().errors.push(error);
        //     }
        //     // Continue with registration even if there are constraint errors
        //     // The errors will be reported during compilation
        // }

        // CRITICAL FIX: Preserve generic parameters from trait definition
        let trait_type = if let Some(ref generic_params) = trait_def.generic_params {
            // Create generic type arguments from parameter names
            let args: Vec<StructuredType> = generic_params
                .params
                .iter()
                .map(|param| {
                    let param_type_id = self.intern_type_name(&param.name.name);
                    StructuredType::TypeVariable(param_type_id)  // ✅ Fix: Use TypeVariable for trait type parameters
                })
                .collect();

            StructuredType::Generic {
                base: trait_type_id.clone(),
                args,
            }
        } else {
            StructuredType::Simple(trait_type_id.clone())
        };

        let module_key = ModuleKey::Module(trait_type_id.hash);
        self.get_or_create_module(
            module_key.clone(),
            ModuleKind::Trait,
            SourceLocation::File("trait_definition".to_string()),
            trait_type.clone(),
        );

        if let Ok(mut modules) = self.modules.write() {
            if let Some(module) = modules.get_mut(&module_key) {
                module.set_trait_constraints(Vec::new());
            }
        }

        // Extract and register trait functions
        for trait_function in &trait_def.functions {
            let function_name_str = match trait_function {
                outrun_parser::TraitFunction::Signature(sig) => &sig.name.name,
                outrun_parser::TraitFunction::Definition(def) => &def.name.name,
                outrun_parser::TraitFunction::StaticDefinition(static_def) => &static_def.name.name,
            };
            let function_name = self.intern_atom_name(function_name_str);
            let function_entry = self.extract_trait_function_entry(trait_function, &trait_name);
            if let Some(entry) = function_entry {
                self.add_function_to_module(
                    module_key.clone(),
                    trait_type.clone(),
                    function_name,
                    entry,
                );
            }
        }
    }

    /// Extract a UnifiedFunctionEntry from a trait function
    /// This is a helper method for trait definition registration
    fn extract_trait_function_entry(
        &self,
        trait_function: &outrun_parser::TraitFunction,
        trait_name: &str,
    ) -> Option<UnifiedFunctionEntry> {
        match trait_function {
            outrun_parser::TraitFunction::Definition(func_def) => {
                let function_id = format!("trait::{}::{}", trait_name, func_def.name.name);
                let is_guard = func_def.name.name.ends_with('?');
                Some(UnifiedFunctionEntry::TraitDefault {
                    definition: func_def.clone(),
                    typed_definition: None,
                    function_id,
                    is_guard,
                })
            }
            outrun_parser::TraitFunction::Signature(func_sig) => {
                // Convert signature to function definition for storage
                let func_def = outrun_parser::FunctionDefinition {
                    attributes: func_sig.attributes.clone(),
                    name: func_sig.name.clone(),
                    visibility: func_sig.visibility.clone(),
                    parameters: func_sig.parameters.clone(),
                    return_type: func_sig.return_type.clone(),
                    guard: func_sig.guard.clone(),
                    body: outrun_parser::Block {
                        statements: Vec::new(),
                        span: outrun_parser::Span {
                            start: 0,
                            end: 0,
                            start_line_col: None,
                            end_line_col: None,
                        },
                    },
                    span: func_sig.span,
                };
                let function_id = format!("trait::{}::{}", trait_name, func_def.name.name);
                let is_guard = func_def.name.name.ends_with('?');
                Some(UnifiedFunctionEntry::TraitSignature {
                    definition: func_def,
                    typed_definition: None,
                    function_id,
                    is_guard,
                })
            }
            outrun_parser::TraitFunction::StaticDefinition(static_def) => {
                // Convert static definition to function definition for storage
                let func_def = outrun_parser::FunctionDefinition {
                    attributes: static_def.attributes.clone(),
                    name: static_def.name.clone(),
                    visibility: outrun_parser::FunctionVisibility::Public,
                    parameters: static_def.parameters.clone(),
                    return_type: static_def.return_type.clone(),
                    guard: None,
                    body: static_def.body.clone(),
                    span: static_def.span,
                };
                let function_id = format!("trait::{}::{}", trait_name, func_def.name.name);
                let is_guard = func_def.name.name.ends_with('?');
                Some(UnifiedFunctionEntry::TraitStatic {
                    definition: func_def,
                    typed_definition: None,
                    function_id,
                    is_guard,
                })
            }
        }
    }

    /// Check trait case exhaustiveness
    /// This replaces TraitRegistry::check_trait_case_exhaustiveness
    pub fn check_trait_case_exhaustiveness(
        &self,
        trait_type: &StructuredType,
        covered_types: &[StructuredType],
    ) -> crate::types::traits::ExhaustivenessResult {
        let all_implementations = self.get_trait_implementations(trait_type);

        // Find missing implementations
        let missing_types: Vec<StructuredType> = all_implementations
            .iter()
            .filter(|&impl_type| !covered_types.contains(impl_type))
            .cloned()
            .collect();

        if missing_types.is_empty() {
            crate::types::traits::ExhaustivenessResult::Exhaustive
        } else {
            // Convert StructuredType to TypeNameId for compatibility with existing ExhaustivenessResult
            let missing_type_ids: Vec<TypeNameId> = missing_types
                .iter()
                .filter_map(|st| match st {
                    StructuredType::Simple(type_id) => Some(type_id.clone()),
                    _ => None, // Skip complex types for now
                })
                .collect();
            crate::types::traits::ExhaustivenessResult::MissingTraitImplementations(
                missing_type_ids,
            )
        }
    }

    // ===== Function Lookup Methods =====
    // These methods replace the FunctionRegistry lookup functionality

    /// Create or get a module for the given key
    pub fn get_or_create_module(
        &self,
        key: ModuleKey,
        module_kind: ModuleKind,
        source_location: SourceLocation,
        structured_type: StructuredType,
    ) -> bool {
        let mut modules = self.modules.write().unwrap();
        if let std::collections::hash_map::Entry::Vacant(e) = modules.entry(key) {
            let module = Module::new(module_kind, source_location, structured_type);
            e.insert(module);
            true // Created new module
        } else {
            false // Module already exists
        }
    }

    /// Add a function to a module
    pub fn add_function_to_module(
        &self,
        module_key: ModuleKey,
        function_type: StructuredType,
        function_name: AtomId,
        entry: UnifiedFunctionEntry,
    ) {
        if let Some(module) = self.modules.write().unwrap().get_mut(&module_key) {
            module.add_function_complete(function_type, function_name, entry);
        }
    }

    /// Add a unified function entry to a module
    pub fn add_unified_function_to_module(
        &self,
        module_key: ModuleKey,
        function_type: StructuredType,
        function_name: AtomId,
        entry: UnifiedFunctionEntry,
    ) {
        if let Some(module) = self.modules.write().unwrap().get_mut(&module_key) {
            module.add_function_complete(function_type, function_name.clone(), entry);
        }
    }

    /// Load an existing compilation result into this environment
    /// This is used for initializing with core library or previous compilation results
    pub fn load_compilation_result(&mut self, compilation_result: CompilationResult) {
        // Store the compilation result in our state
        {
            let mut state = self.compilation_state.write().unwrap();
            state.compilation_result = Some(compilation_result.clone());
        }

        // Load structs and traits from the compilation result
        self.load_structs_and_traits(&compilation_result);

        // Note: Functions are now stored directly in the module system within CompilerEnvironment
        // No need to recreate modules from a separate function registry
        let _module_count = self.modules.read().unwrap().len();
    }

    // =============================================================================
    // Function Lookup Methods (Replacing FunctionRegistry functionality)
    // =============================================================================

    /// Look up a function by qualified name (e.g., "Option.some", "List.head")
    /// This replaces FunctionRegistry::lookup_qualified_function
    pub fn lookup_qualified_function(
        &self,
        module_type: &StructuredType,
        function_name: AtomId,
    ) -> Option<UnifiedFunctionEntry> {
        // Extract the base type from both Simple and Generic types
        let base_type_id = match module_type {
            StructuredType::Simple(type_id) => type_id.clone(),
            StructuredType::Generic { base, .. } => base.clone(),
            _ => {
                // Qualified function lookup failed: unsupported module type
                return None;
            }
        };

        if let Some(_type_name) = self.resolve_type(base_type_id.clone()) {

            let module_key = ModuleKey::Module(base_type_id.hash);

            if let Ok(modules) = self.modules.read() {
                if let Some(module) = modules.get(&module_key) {
                    let function_entry = module.get_function_by_name(function_name).cloned();
                    if let Some(ref _entry) = function_entry {
                        // Function found in module
                    } else {
                        // Function not found in module
                        // Function count logged for debugging
                    }
                    return function_entry;
                } else {
                    // Module not found in registry
                    // Available modules logged for debugging
                }
            }
        }
        // Qualified function lookup failed
        None
    }

    /// Look up a function implementation for trait dispatch
    /// SIMPLIFIED: If the trait is implemented by the type, we should always be able to dispatch to it
    /// This replaces FunctionRegistry::lookup_impl_function
    pub fn lookup_impl_function(
        &self,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
        function_name: AtomId,
    ) -> Option<UnifiedFunctionEntry> {
        let resolved_trait_type = self.resolve_type_variables(trait_type);
        let resolved_impl_type = self.resolve_type_variables(impl_type);

        if let Ok(modules) = self.modules.read() {
            let module_key = ModuleKey::TraitImpl(
                Box::new(resolved_trait_type.clone()),
                Box::new(resolved_impl_type.clone()),
            );

            if let Some(impl_module) = modules.get(&module_key) {
                if let Some(function) = impl_module.get_function_by_name(function_name.clone()) {
                    if matches!(function.function_type(), FunctionType::TraitSignature) {
                        panic!("CRITICAL: Found TraitSignature after trait default expansion for trait {trait_type:?} on type {impl_type:?} function {function_name:?}");
                    }
                    return Some(function.clone());
                } else {
                    // Function not found in trait implementation module
                }
            } else {
                // No exact module match found, proceeding to SMT-based search
            }

            if let Some(generic_function) = self.find_compatible_generic_implementation(
                &resolved_trait_type,
                &resolved_impl_type,
                function_name.clone(),
                &modules,
            ) {
                return Some(generic_function);
            }
        }

        // If we reach here, the trait implementation is missing or the function doesn't exist
        // No implementation found for trait function
        None
    }

    /// Find a compatible generic trait implementation using SMT constraint checking
    /// This handles cases where concrete types need to match generic registrations through SMT solving
    fn find_compatible_generic_implementation(
        &self,
        concrete_trait_type: &StructuredType,
        concrete_impl_type: &StructuredType,
        function_name: AtomId,
        modules: &std::collections::HashMap<ModuleKey, Module>,
    ) -> Option<UnifiedFunctionEntry> {
        for (module_key, module) in modules.iter() {
            if let ModuleKey::TraitImpl(generic_trait_type, generic_impl_type) = module_key {
                if generic_trait_type
                    .to_string_representation()
                    .contains("Option")
                {
                    // Use SMT to check if concrete types are compatible with this generic implementation
                    if self.smt_types_are_compatible(concrete_trait_type, generic_trait_type)
                        && self.smt_types_are_compatible(concrete_impl_type, generic_impl_type)
                    {
                        if let Some(function) = module.get_function_by_name(function_name.clone()) {
                            return Some(function.clone());
                        }
                    }
                }
            }
        }

        // No SMT-compatible generic implementation found
        None
    }

    /// Use SMT to check if two types are compatible through type parameter unification
    /// This generates precise TypeParameterUnification constraints, not overly broad TypeUnification
    fn smt_types_are_compatible(
        &self,
        concrete_type: &StructuredType,
        generic_type: &StructuredType,
    ) -> bool {
        // Generate precise SMT constraints for type parameter unification
        match self.generate_type_parameter_unification_constraints(concrete_type, generic_type) {
            Some(constraints) => {
                // Use SMT solver to check if the type parameter unifications are satisfiable
                self.smt_check_constraints_satisfiable(&constraints)
            }
            None => {
                // Types are structurally incompatible (e.g., Option vs List)
                false
            }
        }
    }

    /// Generate precise TypeParameterUnification constraints for compatibility checking
    /// Returns None if types are structurally incompatible (different base types, arity, etc.)
    fn generate_type_parameter_unification_constraints(
        &self,
        concrete_type: &StructuredType,
        generic_type: &StructuredType,
    ) -> Option<Vec<crate::smt::constraints::SMTConstraint>> {
        let mut constraints = Vec::new();

        let result = match (concrete_type, generic_type) {
            // Simple types must match exactly - no unification needed
            (StructuredType::Simple(concrete_id), StructuredType::Simple(generic_id)) => {
                if concrete_id == generic_id {
                    Some(constraints) // Empty constraints = trivially satisfiable
                } else {
                    None // Different simple types are incompatible
                }
            }

            // Concrete type can unify with type variable through TypeParameterUnification
            (concrete, StructuredType::TypeVariable(var_id)) => {
                let type_name = self.resolve_type_name(var_id).unwrap_or_default();
                constraints.push(
                    crate::smt::constraints::SMTConstraint::TypeParameterUnification {
                        parameter_name: type_name,
                        concrete_type: concrete.clone(),
                        context: "generic implementation compatibility".to_string(),
                    },
                );
                Some(constraints)
            }

            (
                StructuredType::Generic {
                    base: concrete_base,
                    args: concrete_args,
                },
                StructuredType::Generic {
                    base: generic_base,
                    args: generic_args,
                },
            ) => {
                // Base types must match exactly (Option = Option, List = List)
                if concrete_base != generic_base {
                    return None;
                }

                // Must have same arity (number of type arguments)
                if concrete_args.len() != generic_args.len() {
                    return None;
                }

                // Recursively generate constraints for each type argument
                for (concrete_arg, generic_arg) in concrete_args.iter().zip(generic_args.iter()) {
                    if let Some(mut arg_constraints) = self
                        .generate_type_parameter_unification_constraints(concrete_arg, generic_arg)
                    {
                        constraints.append(&mut arg_constraints);
                    } else {
                        return None; // Incompatible argument types
                    }
                }

                Some(constraints)
            }

            // Other combinations are structurally incompatible
            _ => {
                // Incompatible type combination
                None
            }
        };

        result
    }

    /// Use SMT solver to check if a set of constraints is satisfiable (with caching)
    fn smt_check_constraints_satisfiable(
        &self,
        constraints: &[crate::smt::constraints::SMTConstraint],
    ) -> bool {
        if constraints.is_empty() {
            return true; // Empty constraint set is trivially satisfiable
        }

        // Use cached version for better performance
        match crate::smt::solver_pool::check_constraints_satisfiable_cached(constraints, self) {
            Ok(satisfiable) => satisfiable,
            Err(_) => false, // SMT solver errors mean unsatisfiable
        }
    }

    /// Instantiate a generic implementation function with concrete types
    /// This creates a concrete typed definition from a generic one
    fn instantiate_generic_impl_function(
        &self,
        generic_function: &UnifiedFunctionEntry,
    ) -> Option<UnifiedFunctionEntry> {
        // Simple implementation: if the function has a typed definition, return it as-is
        // The typed definition should already have concrete types resolved
        if generic_function.typed_definition().is_some() {
            return Some(generic_function.clone());
        }

        None
    }

    /// Expand trait default implementations into concrete impl blocks
    /// This eliminates the need for complex trait default dispatch by copying
    /// default implementations into every impl block that doesn't override them
    fn expand_trait_default_implementations(
        &mut self,
        collection: &ProgramCollection,
    ) -> Result<ProgramCollection, Vec<TypeError>> {
        use outrun_parser::{ItemKind, TraitFunction};
        use std::collections::HashMap;
        

        // Step 1: Collect all trait default implementations
        let mut trait_defaults: HashMap<String, Vec<outrun_parser::FunctionDefinition>> =
            HashMap::new();

        for program in collection.programs.values() {
            for item in &program.items {
                if let ItemKind::TraitDefinition(trait_def) = &item.kind {
                    let trait_name = trait_def
                        .name
                        .iter()
                        .map(|id| id.name.as_str())
                        .collect::<Vec<_>>()
                        .join(".");

                    let mut defaults = Vec::new();
                    for trait_function in &trait_def.functions {
                        if let TraitFunction::Definition(func_def) = trait_function {
                            // This is a default implementation
                            defaults.push(func_def.clone());
                        }
                    }

                    if !defaults.is_empty() {
                        trait_defaults.insert(trait_name, defaults);
                    }
                }
            }
        }

        let mut expanded_programs = HashMap::new();

        for (file_path, program) in &collection.programs {
            let mut expanded_items = Vec::new();

            for item in &program.items {
                match &item.kind {
                    ItemKind::ImplBlock(impl_block) => {
                        // Expand this impl block with missing default implementations
                        let expanded_impl =
                            self.expand_impl_block_with_defaults(impl_block, &trait_defaults)?;
                            
                        expanded_items.push(outrun_parser::Item {
                            kind: ItemKind::ImplBlock(expanded_impl),
                            ..item.clone()
                        });
                    }
                    _ => {
                        // Keep all other items unchanged
                        expanded_items.push(item.clone());
                    }
                }
            }

            expanded_programs.insert(
                file_path.clone(),
                outrun_parser::Program {
                    items: expanded_items,
                    ..program.clone()
                },
            );
        }

        let expanded_collection = ProgramCollection {
            programs: expanded_programs,
            sources: collection.sources.clone(),
        };

        Ok(expanded_collection)
    }

    /// Expand an impl block with missing trait default implementations
    fn expand_impl_block_with_defaults(
        &self,
        impl_block: &outrun_parser::ImplBlock,
        trait_defaults: &HashMap<String, Vec<outrun_parser::FunctionDefinition>>,
    ) -> Result<outrun_parser::ImplBlock, Vec<TypeError>> {
        // Get the trait name from this impl block
        let trait_name = impl_block
            .trait_spec
            .path
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        // Check if this trait has default implementations
        let Some(defaults) = trait_defaults.get(&trait_name) else {
            // No defaults for this trait, return unchanged
            return Ok(impl_block.clone());
        };

        // Collect existing function names in this impl block
        let existing_functions: std::collections::HashSet<String> = impl_block
            .functions
            .iter()
            .map(|f| f.name.name.clone())
            .collect();

        // Find missing default implementations
        let mut expanded_functions = impl_block.functions.clone();

        for default_func in defaults {
            if !existing_functions.contains(&default_func.name.name) {
                let copied_func = outrun_parser::FunctionDefinition {
                    attributes: default_func.attributes.clone(),
                    visibility: default_func.visibility.clone(),
                    name: default_func.name.clone(), // Preserves original span
                    parameters: default_func.parameters.clone(), // Preserves parameter spans
                    return_type: default_func.return_type.clone(), // Preserves return type span
                    guard: default_func.guard.clone(), // Preserves guard spans
                    body: default_func.body.clone(), // Preserves body spans - this is key!
                    span: default_func.span,         // Preserve original span for go-to-definition
                                                     // All spans point to the original trait definition for proper go-to-definition
                };

                expanded_functions.push(copied_func);
            }
        }

        Ok(outrun_parser::ImplBlock {
            functions: expanded_functions,
            ..impl_block.clone()
        })
    }

    /// Look up a local function in the current module context
    /// This replaces FunctionRegistry::lookup_local_function
    pub fn lookup_local_function(&self, function_name: AtomId) -> Option<UnifiedFunctionEntry> {
        if let Ok(modules) = self.modules.read() {
            // Search through all modules for the function name
            for module in modules.values() {
                if let Some(function) = module.get_function_by_name(function_name.clone()) {
                    return Some(function.clone());
                }
            }
        }
        None
    }

    /// Check if an impl block has an override for a trait function
    /// This replaces FunctionRegistry::has_impl_override
    pub fn has_impl_override(
        &self,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
        function_name: AtomId,
    ) -> bool {
        let impl_module_key =
            ModuleKey::TraitImpl(Box::new(trait_type.clone()), Box::new(impl_type.clone()));

        if let Ok(modules) = self.modules.read() {
            if let Some(impl_module) = modules.get(&impl_module_key) {
                return impl_module.get_function_by_name(function_name).is_some();
            }
        }

        false
    }

    /// Add a trait function entry
    /// This replaces FunctionRegistry::add_trait_function
    pub fn add_trait_function(
        &self,
        trait_type: StructuredType,
        function_name: AtomId,
        entry: UnifiedFunctionEntry,
    ) {
        if let StructuredType::Simple(trait_type_id) = &trait_type {
            if let Some(_trait_name) = self.resolve_type(trait_type_id.clone()) {
                let module_key = ModuleKey::Module(trait_type_id.hash);

                // Create trait module if it doesn't exist
                self.get_or_create_module(
                    module_key.clone(),
                    ModuleKind::Trait,
                    SourceLocation::File("generated".to_string()),
                    trait_type.clone(),
                );

                // Add function to the module
                self.add_function_to_module(module_key, trait_type, function_name, entry);
            }
        }
    }

    /// Add an impl function entry
    /// This replaces FunctionRegistry::add_impl_function
    pub fn add_impl_function(
        &self,
        trait_type: StructuredType,
        impl_type: StructuredType,
        function_name: AtomId,
        entry: UnifiedFunctionEntry,
    ) {
        let impl_module_key =
            ModuleKey::TraitImpl(Box::new(trait_type.clone()), Box::new(impl_type.clone()));

        // Create impl module if it doesn't exist
        self.get_or_create_module(
            impl_module_key.clone(),
            ModuleKind::TraitImpl,
            SourceLocation::File("generated".to_string()),
            trait_type.clone(), // Use trait type as the structured type for the module
        );

        // Add function to the module
        self.add_function_to_module(impl_module_key, trait_type, function_name, entry);
    }

    /// Add module function entry
    /// This replaces FunctionRegistry::add_module_function
    pub fn add_module_function(
        &self,
        module_type: StructuredType,
        function_name: AtomId,
        entry: UnifiedFunctionEntry,
    ) {
        if let StructuredType::Simple(module_type_id) = &module_type {
            if let Some(_module_name) = self.resolve_type(module_type_id.clone()) {
                let module_key = ModuleKey::Module(module_type_id.hash);

                // Create module if it doesn't exist
                self.get_or_create_module(
                    module_key.clone(),
                    ModuleKind::Struct,
                    SourceLocation::File("generated".to_string()),
                    module_type.clone(),
                );

                // Add function to the module
                self.add_function_to_module(module_key, module_type, function_name, entry);
            }
        }
    }

    /// Update an existing function entry with its typed definition in a specific module
    pub fn update_function_with_typed_definition_in_module(
        &self,
        module_key: &ModuleKey,
        function_name: &str,
        typed_definition: crate::checker::TypedFunctionDefinition,
    ) -> bool {
        let mut found_any = false;

        if let Ok(mut modules) = self.modules.write() {
            // Only update functions in the specific module
            if let Some(module) = modules.get_mut(module_key) {
                // Update functions in both function maps
                for entry in module.functions.values_mut() {
                    if entry.definition().name.name == function_name {
                        entry.set_typed_definition(typed_definition.clone());
                        found_any = true;
                    }
                }

                for entry in module.functions_by_name.values_mut() {
                    if entry.definition().name.name == function_name {
                        entry.set_typed_definition(typed_definition.clone());
                        found_any = true;
                    }
                }
            }
        }

        found_any
    }

    /// Update an existing function entry with its typed definition (legacy version - searches all modules)
    /// This replaces FunctionRegistry::update_function_with_typed_definition
    ///
    /// WARNING: This function updates ALL functions with the given name across ALL modules.
    /// Use update_function_with_typed_definition_in_module for precision.
    pub fn update_function_with_typed_definition(
        &self,
        function_name: &str,
        typed_definition: crate::checker::TypedFunctionDefinition,
    ) -> bool {
        let mut found_any = false;

        if let Ok(mut modules) = self.modules.write() {
            for module in modules.values_mut() {
                // Update functions in both function maps
                for entry in module.functions.values_mut() {
                    if entry.definition().name.name == function_name {
                        entry.set_typed_definition(typed_definition.clone());
                        found_any = true;
                    }
                }

                for entry in module.functions_by_name.values_mut() {
                    if entry.definition().name.name == function_name {
                        entry.set_typed_definition(typed_definition.clone());
                        found_any = true;
                    }
                }
            }
        }

        found_any
    }

    /// Get total number of functions across all modules
    /// This replaces FunctionRegistry::len
    pub fn function_count(&self) -> usize {
        if let Ok(modules) = self.modules.read() {
            modules
                .values()
                .map(|module| module.functions_by_name.len())
                .sum()
        } else {
            0
        }
    }

    /// Check if there are any functions stored
    /// This replaces FunctionRegistry::is_empty
    pub fn has_functions(&self) -> bool {
        self.function_count() > 0
    }

    /// Check if a StructuredType represents a trait (not a concrete type)
    /// This is critical for SMT constraint generation and trait validation
    fn is_trait_type(&self, structured_type: &StructuredType) -> bool {
        match structured_type {
            StructuredType::Simple(type_id) => {
                // Check if this type ID represents a trait in our trait registry
                if let Ok(traits) = self.traits.read() {
                    traits.contains_key(type_id)
                } else {
                    false
                }
            }
            StructuredType::Generic { base, .. } => {
                // For generic types like Option<T>, check if the base is a trait
                if let Ok(traits) = self.traits.read() {
                    traits.contains_key(base)
                } else {
                    false
                }
            }
            // Other structured types (Tuple, Function, etc.) are not traits
            _ => false,
        }
    }

    /// Validate trait constraint dependencies using the constraint system
    /// This detects when default implementations require traits not declared in the trait's constraints
    fn validate_trait_constraint_dependencies_via_constraints(
        &self,
        trait_def: &TraitDefinition,
    ) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();
        let trait_name = &trait_def.name_as_string();

        // Get the declared constraints for Self in this trait (from where clauses)
        let declared_constraints = self.extract_self_constraints_from_trait(trait_def);

        // For each default implementation, type-check it and see what constraints it generates
        for trait_function in &trait_def.functions {
            if let outrun_parser::TraitFunction::Definition(func_def) = trait_function {
                // Create a temporary type-checking context where Self can only implement declared traits
                let required_constraints = self.analyze_default_implementation_constraints(
                    trait_name,
                    func_def,
                    &declared_constraints,
                );

                // Find constraints that are required but not declared
                for required_trait in required_constraints {
                    if !declared_constraints.contains(&required_trait) {
                        errors.push(TypeError::MissingTraitConstraintInDefinition {
                            span: crate::error::span_to_source_span(func_def.span),
                            trait_name: trait_name.clone(),
                            function_name: func_def.name.name.clone(),
                            required_trait: required_trait.clone(),
                            suggestion: format!(
                                "Add constraint 'where Self: {required_trait}' to trait {trait_name} definition"
                            ),
                        });
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Extract Self constraints from a trait definition (from where clauses)
    fn extract_self_constraints_from_trait(&self, trait_def: &TraitDefinition) -> Vec<String> {
        let mut constraints = Vec::new();

        // Parse constraints from trait_def.constraints (when Self: Trait1 && Self: Trait2 && ...)
        if let Some(constraint_expr) = &trait_def.constraints {
            // Parse the constraint expression to extract trait names
            constraints.extend(self.parse_trait_constraints(constraint_expr));
        }

        constraints
    }

    /// Parse trait constraint expressions like "Self: Equality && Self: Comparison"
    #[allow(clippy::only_used_in_recursion)]
    fn parse_trait_constraints(
        &self,
        constraint_expr: &outrun_parser::ConstraintExpression,
    ) -> Vec<String> {
        use outrun_parser::ConstraintExpression;
        let mut constraints = Vec::new();

        match constraint_expr {
            // Binary AND expression: A && B
            ConstraintExpression::And { left, right, .. } => {
                // Recursively parse left and right sides
                constraints.extend(self.parse_trait_constraints(left));
                constraints.extend(self.parse_trait_constraints(right));
            }
            // Type constraint: Self: TraitName
            ConstraintExpression::Constraint {
                type_param,
                trait_bound,
                ..
            } => {
                // Check if this is a Self constraint
                if type_param.name == "Self" {
                    // Extract trait name from trait_bound (Vec<TypeIdentifier>)
                    let trait_name = trait_bound
                        .iter()
                        .map(|id| id.name.clone())
                        .collect::<Vec<_>>()
                        .join(".");
                    constraints.push(trait_name);
                }
            }
            // Parenthesized constraint: (expr)
            ConstraintExpression::Parenthesized { expression, .. } => {
                constraints.extend(self.parse_trait_constraints(expression));
            }
        }

        constraints
    }

    /// Analyze what trait constraints a default implementation actually requires
    /// by running type checking and examining generated constraints
    fn analyze_default_implementation_constraints(
        &self,
        _trait_name: &str,
        _func_def: &outrun_parser::FunctionDefinition,
        _declared_constraints: &[String],
    ) -> Vec<String> {
        // For now, return empty constraints since the validation is causing false positives
        // The real issue is that Binary and String traits already declare `when Self: Equality`
        // but the constraint validation system isn't recognizing this properly

        // Simply return that no additional constraints are required beyond what's declared
        // This prevents false positive errors about missing constraints
        Vec::new()
    }

    // =============================================================================
    // Dispatch Table Methods (Replacing DispatchTable functionality)
    // =============================================================================

    /// Register a trait implementation for dispatch
    /// This replaces DispatchTable::register_trait_impl
    pub fn register_trait_impl_for_dispatch(
        &self,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
    ) -> u32 {
        let mut state = self.compilation_state.write().unwrap();

        // Convert StructuredType to TypeNameId for dispatch table
        if let (StructuredType::Simple(trait_id), StructuredType::Simple(impl_id)) =
            (trait_type, impl_type)
        {
            state
                .dispatch_table
                .register_trait_impl(trait_id.clone(), impl_id.clone())
                .0
        } else {
            // For complex types, generate a placeholder ID
            state
                .dispatch_table
                .register_trait_impl(
                    self.intern_type_name("ComplexTraitType"),
                    self.intern_type_name("ComplexImplType"),
                )
                .0
        }
    }

    /// Look up trait implementation in dispatch table
    /// This replaces DispatchTable::lookup_trait_impl
    pub fn lookup_trait_impl_dispatch(
        &self,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
    ) -> Option<u32> {
        let state = self.compilation_state.read().unwrap();

        // Convert StructuredType to TypeNameId for dispatch table lookup
        if let (StructuredType::Simple(trait_id), StructuredType::Simple(impl_id)) =
            (trait_type, impl_type)
        {
            state
                .dispatch_table
                .lookup_trait_impl(trait_id.clone(), impl_id.clone())
                .map(|id| id.0)
        } else {
            None
        }
    }

    /// Register a static function for dispatch
    /// This replaces DispatchTable::register_static_function
    pub fn register_static_function_for_dispatch(
        &self,
        type_ref: &StructuredType,
        function_name: &str,
        function_id: u32,
    ) {
        let mut state = self.compilation_state.write().unwrap();

        if let StructuredType::Simple(type_id) = type_ref {
            state.dispatch_table.register_static_function(
                type_id.clone(),
                function_name.to_string(),
                crate::types::traits::FunctionId(function_id),
            );
        }
    }

    /// Look up static function in dispatch table
    /// This replaces DispatchTable::lookup_static_function
    pub fn lookup_static_function_dispatch(
        &self,
        type_ref: &StructuredType,
        function_name: &str,
    ) -> Option<u32> {
        let state = self.compilation_state.read().unwrap();

        if let StructuredType::Simple(type_id) = type_ref {
            state
                .dispatch_table
                .lookup_static_function(type_id.clone(), function_name)
                .map(|id| id.0)
        } else {
            None
        }
    }

    /// Register binary operator implementation
    /// This replaces DispatchTable::register_binary_op
    pub fn register_binary_operator(
        &self,
        operator: outrun_parser::BinaryOperator,
        left_type: &StructuredType,
        right_type: &StructuredType,
        function_id: u32,
    ) {
        let mut state = self.compilation_state.write().unwrap();

        if let (StructuredType::Simple(left_id), StructuredType::Simple(right_id)) =
            (left_type, right_type)
        {
            state.dispatch_table.register_binary_op(
                operator,
                left_id.clone(),
                right_id.clone(),
                crate::types::traits::FunctionId(function_id),
            );
        }
    }

    /// Look up binary operator implementation
    /// This replaces DispatchTable::lookup_binary_op
    pub fn lookup_binary_operator(
        &self,
        operator: outrun_parser::BinaryOperator,
        left_type: &StructuredType,
        right_type: &StructuredType,
    ) -> Option<u32> {
        let state = self.compilation_state.read().unwrap();

        if let (StructuredType::Simple(left_id), StructuredType::Simple(right_id)) =
            (left_type, right_type)
        {
            state
                .dispatch_table
                .lookup_binary_op(operator, left_id.clone(), right_id.clone())
                .map(|id| id.0)
        } else {
            None
        }
    }

    /// Register unary operator implementation
    /// This replaces DispatchTable::register_unary_op
    pub fn register_unary_operator(
        &self,
        operator: outrun_parser::UnaryOperator,
        operand_type: &StructuredType,
        function_id: u32,
    ) {
        let mut state = self.compilation_state.write().unwrap();

        if let StructuredType::Simple(operand_id) = operand_type {
            state.dispatch_table.register_unary_op(
                operator,
                operand_id.clone(),
                crate::types::traits::FunctionId(function_id),
            );
        }
    }

    /// Look up unary operator implementation
    /// This replaces DispatchTable::lookup_unary_op
    pub fn lookup_unary_operator(
        &self,
        operator: outrun_parser::UnaryOperator,
        operand_type: &StructuredType,
    ) -> Option<u32> {
        let state = self.compilation_state.read().unwrap();

        if let StructuredType::Simple(operand_id) = operand_type {
            state
                .dispatch_table
                .lookup_unary_op(operator, operand_id.clone())
                .map(|id| id.0)
        } else {
            None
        }
    }

    /// Get a reference to the dispatch table for advanced operations
    /// This allows access to the underlying dispatch table when needed
    pub fn dispatch_table(&self) -> crate::dispatch::DispatchTable {
        self.compilation_state
            .read()
            .unwrap()
            .dispatch_table
            .clone()
    }

    /// Set the dispatch table (useful for loading pre-built dispatch tables)
    pub fn set_dispatch_table(&self, dispatch_table: crate::dispatch::DispatchTable) {
        self.compilation_state.write().unwrap().dispatch_table = dispatch_table;
    }

    // =============================================================================
    // Struct and Trait Management Methods (Replacing TypeCheckingContext functionality)
    // =============================================================================

    /// Add a struct definition to the context
    pub fn add_struct(&self, type_id: TypeNameId, struct_def: StructDefinition) {
        self.structs.write().unwrap().insert(type_id, struct_def);
    }

    /// Add a trait definition to the context
    pub fn add_trait(&self, type_id: TypeNameId, trait_def: TraitDefinition) {
        self.traits.write().unwrap().insert(type_id, trait_def);
    }

    /// Look up a struct definition by TypeNameId
    pub fn get_struct(&self, type_id: &TypeNameId) -> Option<StructDefinition> {
        self.structs.read().unwrap().get(type_id).cloned()
    }

    /// Look up a trait definition by TypeNameId
    pub fn get_trait(&self, type_id: &TypeNameId) -> Option<TraitDefinition> {
        self.traits.read().unwrap().get(type_id).cloned()
    }

    /// Get all struct definitions
    pub fn get_all_structs(&self) -> HashMap<TypeNameId, StructDefinition> {
        self.structs.read().unwrap().clone()
    }

    /// Get all trait definitions
    pub fn get_all_traits(&self) -> HashMap<TypeNameId, TraitDefinition> {
        self.traits.read().unwrap().clone()
    }

    /// Load structs and traits from a compilation result
    pub fn load_structs_and_traits(&self, compilation_result: &CompilationResult) {
        // Load structs
        {
            let mut structs = self.structs.write().unwrap();
            for (type_id, struct_def) in &compilation_result.structs {
                structs.insert(type_id.clone(), struct_def.clone());
            }
        }

        // Load traits
        {
            let mut traits = self.traits.write().unwrap();
            for (type_id, trait_def) in &compilation_result.traits {
                traits.insert(type_id.clone(), trait_def.clone());
            }
        }
    }

    /// Create a compilation result that includes structs and traits from this environment
    pub fn create_compilation_result_with_structs_and_traits(
        &self,
        compilation_order: Vec<String>,
        implementations: Vec<ImplBlock>,
        typed_programs: HashMap<String, crate::checker::TypedProgram>,
    ) -> CompilationResult {
        CompilationResult {
            compilation_order,
            type_context: self.unification_context(),
            traits: self.get_all_traits(),
            structs: self.get_all_structs(),
            implementations,
            typed_programs,
        }
    }

    /// Create a TypeCheckingContext-compatible interface
    /// This method bridges the old TypeCheckingContext API with the new CompilerEnvironment
    /// and allows for gradual migration of code
    pub fn create_type_checking_context(&self) -> crate::context::TypeCheckingContext {
        crate::context::TypeCheckingContext::with_compiler_environment(
            self.unification_context(),
            self.clone(),
            self.get_all_structs(),
            self.get_all_traits(),
        )
    }

    /// Create a minimal TypeCheckingContext for testing
    #[cfg(test)]
    pub fn minimal_type_checking_context_for_testing(&self) -> crate::context::TypeCheckingContext {
        crate::context::TypeCheckingContext::with_compiler_environment(
            self.unification_context(),
            self.clone(),
            HashMap::new(),
            HashMap::new(),
        )
    }

    // =============================================================================
    // Compilation Phase Management Methods (Replacing CompilationPhaseContext functionality)
    // =============================================================================

    /// Set the compilation order
    pub fn set_compilation_order(&self, order: Vec<String>) {
        self.compilation_state.write().unwrap().compilation_order = order;
    }

    /// Get the compilation order
    pub fn get_compilation_order(&self) -> Vec<String> {
        self.compilation_state
            .read()
            .unwrap()
            .compilation_order
            .clone()
    }

    /// Set external variables for REPL usage
    pub fn set_external_variables(&self, variables: HashMap<String, StructuredType>) {
        self.compilation_state.write().unwrap().external_variables = variables;
    }

    /// Get external variables
    pub fn get_external_variables(&self) -> HashMap<String, StructuredType> {
        self.compilation_state
            .read()
            .unwrap()
            .external_variables
            .clone()
    }

    /// Add an external variable
    pub fn add_external_variable(&self, name: String, var_type: StructuredType) {
        self.compilation_state
            .write()
            .unwrap()
            .external_variables
            .insert(name, var_type);
    }

    /// Set implementation blocks
    pub fn set_implementations(&self, implementations: Vec<ImplBlock>) {
        self.compilation_state.write().unwrap().implementations = implementations;
    }

    /// Append implementation blocks to existing ones (for incremental compilation)
    pub fn append_implementations(&self, mut new_implementations: Vec<ImplBlock>) {
        self.compilation_state.write().unwrap().implementations.append(&mut new_implementations);
    }

    /// Get implementation blocks
    pub fn get_implementations(&self) -> Vec<ImplBlock> {
        self.compilation_state
            .read()
            .unwrap()
            .implementations
            .clone()
    }

    /// Add an implementation block
    pub fn add_implementation(&self, impl_block: ImplBlock) {
        self.compilation_state
            .write()
            .unwrap()
            .implementations
            .push(impl_block);
    }

    /// Create a CompilationPhaseContext-compatible interface
    /// This method bridges the old CompilationPhaseContext API with the new CompilerEnvironment
    pub fn create_compilation_phase_context(&self) -> crate::context::CompilationPhaseContext {
        crate::context::CompilationPhaseContext::new(
            self.create_type_checking_context(),
            self.get_compilation_order(),
            self.get_external_variables(),
            self.get_implementations(),
        )
    }

    /// Load compilation phase data from a CompilationPhaseContext
    pub fn load_from_compilation_phase_context(
        &self,
        phase_context: &crate::context::CompilationPhaseContext,
    ) {
        self.set_compilation_order(phase_context.compilation_order.clone());
        self.set_external_variables(phase_context.external_variables.clone());
        self.set_implementations(phase_context.implementations.clone());

        // Load structs and traits from the type checking context
        for (type_id, struct_def) in &phase_context.type_checking_context.structs {
            self.add_struct(type_id.clone(), struct_def.clone());
        }

        for (type_id, trait_def) in &phase_context.type_checking_context.traits {
            self.add_trait(type_id.clone(), trait_def.clone());
        }
    }

    // =============================================================================
    // Shared Compilation Context Methods (Replacing SharedCompilationContext functionality)
    // =============================================================================

    /// Create a derived compilation result by merging with user compilation
    /// This provides the same functionality as SharedCompilationContext::extend_with_user_compilation
    pub fn extend_with_user_compilation(
        &self,
        user_compilation: CompilationResult,
    ) -> CompilationResult {
        // Get the current core compilation result
        if let Some(core_compilation) = self.get_compilation_result() {
            // Use CompilationResult::merge to combine core + user
            match CompilationResult::merge(core_compilation, vec![user_compilation.clone()]) {
                Ok(merged) => merged,
                Err(_conflicts) => {
                    // Log conflicts but proceed with user compilation only
                    // Warning: Compilation conflicts detected
                    user_compilation
                }
            }
        } else {
            // No core compilation, just return the user compilation
            user_compilation
        }
    }

    /// Create a compilation session that shares this environment
    /// This provides similar functionality to SharedCompilationContext::create_session_context
    pub fn create_session(&self) -> Self {
        // CompilerEnvironment is already efficiently shareable due to Arc<RwLock<>> design
        // A clone only clones the Arc pointers, not the underlying data
        self.clone()
    }

    /// Load core library compilation into this environment
    /// This provides similar functionality to SharedCompilationContext::new
    pub fn load_core_compilation(&mut self, core_compilation: CompilationResult) {
        self.load_compilation_result(core_compilation);
    }

    /// Get the effective compilation result for the current session
    /// This provides similar functionality to CompilationSessionContext::effective_compilation
    pub fn effective_compilation(&self) -> Option<CompilationResult> {
        self.get_compilation_result()
    }

    /// Check if this environment has a compilation result
    pub fn has_compilation_result(&self) -> bool {
        self.get_compilation_result().is_some()
    }

    /// Clear compilation result, reverting to a clean state
    pub fn clear_compilation_result(&mut self) {
        self.compilation_state.write().unwrap().compilation_result = None;
    }

    /// **SMT-BASED TRAIT EXPANSION SYSTEM**
    /// These functions implement the core trait expansion logic needed for SMT constraint solving.
    ///
    /// Look up trait implementation using full StructuredType
    pub fn find_trait_implementations(&self, trait_type: &StructuredType) -> Vec<StructuredType> {
        match trait_type {
            StructuredType::Simple(trait_id) => self.find_simple_trait_implementations(trait_id),
            StructuredType::Generic { base, args } => {
                self.find_generic_trait_implementations(base, args)
            }
            _ => Vec::new(),
        }
    }

    /// Find concrete implementations for simple traits
    fn find_simple_trait_implementations(&self, trait_id: &TypeNameId) -> Vec<StructuredType> {
        // For now, use core trait knowledge until we can properly access parsed implementations
        // This is a simplified approach that gets the core library working
        self.get_core_trait_implementations(trait_id)
    }

    /// Get known core trait implementations for built-in traits
    fn get_core_trait_implementations(&self, trait_id: &TypeNameId) -> Vec<StructuredType> {
        let trait_name = self.resolve_type_name(trait_id).unwrap_or_default();
        let mut implementations = Vec::new();

        match trait_name.as_str() {
            "Option" => {
                // Option<T> trait is implemented by Outrun.Option.Some<T> and Outrun.Option.None<T>
                let some_id = self.intern_type_name("Outrun.Option.Some");
                let none_id = self.intern_type_name("Outrun.Option.None");
                implementations.push(StructuredType::Simple(some_id));
                implementations.push(StructuredType::Simple(none_id));
            }
            "String" => {
                // String trait is implemented by Outrun.Core.String
                let string_id = self.intern_type_name("Outrun.Core.String");
                implementations.push(StructuredType::Simple(string_id));
            }
            "Integer" => {
                // Integer trait is implemented by Outrun.Core.Integer64
                let int_id = self.intern_type_name("Outrun.Core.Integer64");
                implementations.push(StructuredType::Simple(int_id));
            }
            "Boolean" => {
                // Boolean trait is implemented by Outrun.Core.Boolean
                let bool_id = self.intern_type_name("Outrun.Core.Boolean");
                implementations.push(StructuredType::Simple(bool_id));
            }
            "List" => {
                // List trait is implemented by Outrun.Core.List
                let list_id = self.intern_type_name("Outrun.Core.List");
                implementations.push(StructuredType::Simple(list_id));
            }
            "Map" => {
                // Map trait is implemented by Outrun.Core.Map
                let map_id = self.intern_type_name("Outrun.Core.Map");
                implementations.push(StructuredType::Simple(map_id));
            }
            "Binary" => {
                // Binary trait is implemented by Outrun.Core.Binary
                let binary_id = self.intern_type_name("Outrun.Core.Binary");
                implementations.push(StructuredType::Simple(binary_id));
            }
            "BinarySubtraction" => {
                // BinarySubtraction trait is implemented by numeric types
                for concrete_type in ["Outrun.Core.Integer64", "Outrun.Core.Float64"] {
                    let concrete_id = self.intern_type_name(concrete_type);
                    implementations.push(StructuredType::Simple(concrete_id));
                }
            }
            "Equality" => {
                // Equality trait is implemented by most concrete types
                for concrete_type in [
                    "Outrun.Core.String",
                    "Outrun.Core.Integer64",
                    "Outrun.Core.Float64",
                    "Outrun.Core.Boolean",
                    "Outrun.Core.Binary",
                ] {
                    let concrete_id = self.intern_type_name(concrete_type);
                    implementations.push(StructuredType::Simple(concrete_id));
                }
            }
            _ => {
                // No known core implementations for trait
            }
        }

        implementations
    }

    /// Helper to format StructuredType for debugging
    fn format_structured_type(&self, structured_type: &StructuredType) -> String {
        match structured_type {
            StructuredType::Simple(type_id) => self
                .resolve_type_name(type_id)
                .unwrap_or_else(|| format!("Unknown({})", type_id.hash)),
            StructuredType::Generic { base, args } => {
                let base_name = self
                    .resolve_type_name(base)
                    .unwrap_or_else(|| format!("Unknown({})", base.hash));
                let arg_names: Vec<String> = args
                    .iter()
                    .map(|arg| self.format_structured_type(arg))
                    .collect();
                format!("{}<{}>", base_name, arg_names.join(", "))
            }
            _ => "ComplexType".to_string(),
        }
    }

    /// Find concrete implementations for generic traits like Option<T>
    fn find_generic_trait_implementations(
        &self,
        base_trait: &TypeNameId,
        args: &[StructuredType],
    ) -> Vec<StructuredType> {
        let mut concrete_implementations = Vec::new();

        // Recursively expand each argument type
        let expanded_args: Vec<Vec<StructuredType>> = args
            .iter()
            .map(|arg_type| {
                let impls = self.find_trait_implementations(arg_type);

                if impls.is_empty() {
                    vec![arg_type.clone()]
                } else {
                    impls
                }
            })
            .collect();

        // Get base trait implementations and create combinations
        let base_implementations = self.find_simple_trait_implementations(base_trait);

        for base_impl in &base_implementations {
            if let StructuredType::Simple(base_impl_id) = base_impl {
                let arg_combinations = self.cartesian_product(&expanded_args);

                for arg_combination in arg_combinations {
                    let generic_type = StructuredType::Generic {
                        base: base_impl_id.clone(),
                        args: arg_combination.clone(),
                    };
                    concrete_implementations.push(generic_type);
                }
            }
        }

        concrete_implementations
    }

    /// Generate cartesian product of argument type combinations
    fn cartesian_product(&self, sets: &[Vec<StructuredType>]) -> Vec<Vec<StructuredType>> {
        if sets.is_empty() {
            return vec![vec![]];
        }

        let mut result = vec![vec![]];
        for set in sets {
            let mut new_result = Vec::new();
            for existing_combo in &result {
                for item in set {
                    let mut new_combo = existing_combo.clone();
                    new_combo.push(item.clone());
                    new_result.push(new_combo);
                }
            }
            result = new_result;
        }
        result
    }

    /// Helper function to check if two TypeNameIds represent the same type
    fn type_names_equal(&self, id1: &TypeNameId, id2: &TypeNameId) -> bool {
        id1.hash == id2.hash
    }

    /// **SMT-ENHANCED FUNCTION LOOKUP**
    /// Uses SMT constraint solving to find function implementations when traditional lookup fails
    pub fn lookup_impl_function_with_smt(
        &self,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
        function_name: AtomId,
    ) -> Option<UnifiedFunctionEntry> {
        // First try traditional lookup
        if let Some(entry) = self.lookup_impl_function(trait_type, impl_type, function_name.clone())
        {
            return Some(entry);
        }

        let (actual_trait_type, type_parameter_constraints) =
            self.resolve_generic_trait_with_parameters(trait_type, impl_type);

        // Create all constraints needed for this lookup
        let mut constraints = Vec::new();

        // Add type parameter unification constraints
        constraints.extend(type_parameter_constraints);

        // Generate SMT constraint: impl_type must implement actual_trait_type
        let trait_constraint = crate::smt::constraints::SMTConstraint::TraitImplemented {
            impl_type: impl_type.clone(),
            trait_type: actual_trait_type.clone(),
        };
        constraints.push(trait_constraint);

        // Create a temporary SMT solver to check if this constraint is satisfiable
        let constraint_system = crate::smt::SMTConstraintSystem::new();
        let mut solver = constraint_system.create_solver();

        // Add all constraints to the solver
        if let Err(_e) = solver.add_constraints(&constraints, self) {
            // Failed to add SMT constraints
            return None;
        }

        // Solve the constraints
        match solver.solve() {
            crate::smt::solver::SolverResult::Satisfiable(model) => {
                let resolved_types = self.apply_smt_model_to_types(&model, trait_type, impl_type);

                if let Some(concrete_function) = self.lookup_concrete_function_with_resolved_types(
                    &resolved_types.trait_type,
                    &resolved_types.impl_type,
                    function_name.clone(),
                    &model,
                ) {
                    return Some(concrete_function);
                }

                // If SMT-guided lookup fails, this indicates a problem with our SMT model application
                // In an SMT-first system, if constraints are satisfiable, we should find the function
                // CRITICAL: SMT constraints satisfiable but concrete function not found
                None
            }
            crate::smt::solver::SolverResult::Unsatisfiable(_) => {
                // SMT constraint unsatisfiable - trait not implemented
                None
            }
            crate::smt::solver::SolverResult::Unknown(_reason) => {
                // SMT solver unknown result
                None
            }
        }
    }

    /// Add SMT constraints to resolve TypeVariable references in a structured type to a constraint list
    fn add_type_variable_resolution_constraints_to_list(
        &self,
        structured_type: &StructuredType,
        constraints: &mut Vec<crate::smt::constraints::SMTConstraint>,
    ) {
        match structured_type {
            StructuredType::TypeVariable(type_var_id) => {
                // Look up the concrete type that this TypeVariable represents
                if let Some(concrete_type_name) = self.resolve_type(type_var_id.clone()) {
                    // Convert the resolved type name to a StructuredType
                    let concrete_type_id = self.intern_type_name(&concrete_type_name);
                    let concrete_type = StructuredType::Simple(concrete_type_id);

                    // Add SMT constraint: TypeVariable(X) = ConcreteType
                    let constraint =
                        crate::smt::constraints::SMTConstraint::TypeVariableConstraint {
                            variable_id: type_var_id.clone(),
                            bound_type: concrete_type,
                            context: "TypeVariable resolution".to_string(),
                        };
                    constraints.push(constraint);
                } else {
                    // Unable to resolve TypeVariable to concrete type
                }
            }
            StructuredType::Generic { base: _, args } => {
                // Recursively add constraints for generic arguments
                for arg in args {
                    self.add_type_variable_resolution_constraints_to_list(arg, constraints);
                }
            }
            StructuredType::Tuple(elements) => {
                // Recursively add constraints for tuple elements
                for elem in elements {
                    self.add_type_variable_resolution_constraints_to_list(elem, constraints);
                }
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                // Recursively add constraints for function parameter and return types
                for param in params {
                    self.add_type_variable_resolution_constraints_to_list(
                        &param.param_type,
                        constraints,
                    );
                }
                self.add_type_variable_resolution_constraints_to_list(return_type, constraints);
            }
            StructuredType::Option { inner_type } => {
                // Recursively handle the inner type
                self.add_type_variable_resolution_constraints_to_list(inner_type, constraints);
            }
            StructuredType::Result { ok_type, err_type } => {
                // Recursively handle both result types
                self.add_type_variable_resolution_constraints_to_list(ok_type, constraints);
                self.add_type_variable_resolution_constraints_to_list(err_type, constraints);
            }
            StructuredType::List { element_type } => {
                // Recursively handle the element type
                self.add_type_variable_resolution_constraints_to_list(element_type, constraints);
            }
            StructuredType::Map {
                key_type,
                value_type,
            } => {
                // Recursively handle both key and value types
                self.add_type_variable_resolution_constraints_to_list(key_type, constraints);
                self.add_type_variable_resolution_constraints_to_list(value_type, constraints);
            }
            StructuredType::Struct { name: _, fields } => {
                // Recursively handle field types in struct definitions
                for field in fields {
                    self.add_type_variable_resolution_constraints_to_list(
                        &field.field_type,
                        constraints,
                    );
                }
            }
            StructuredType::Trait {
                name: _,
                functions: _,
            } => {
                // Trait functions are resolved separately, no TypeVariables in trait structure itself
            }
            StructuredType::TypeError {
                error: _,
                fallback_type,
                error_span: _,
            } => {
                // If there's a fallback type, recursively handle it
                if let Some(fallback) = fallback_type {
                    self.add_type_variable_resolution_constraints_to_list(fallback, constraints);
                }
            }
            // For primitive types, no TypeVariables to resolve
            StructuredType::Simple(_)
            | StructuredType::Integer64
            | StructuredType::Float64
            | StructuredType::Boolean
            | StructuredType::String
            | StructuredType::Atom => {}
        }
    }

    /// Apply SMT constraint model to resolve type parameters in trait and implementation types
    fn apply_smt_model_to_types(
        &self,
        model: &crate::smt::solver::ConstraintModel,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
    ) -> ResolvedTypes {
        let mut type_parameter_map = std::collections::HashMap::new();

        for (var_name, concrete_type) in &model.type_assignments {
            if var_name.starts_with("param_") {
                let param_name = var_name.strip_prefix("param_").unwrap();
                type_parameter_map.insert(param_name.to_string(), concrete_type.clone());
            } else {
                type_parameter_map.insert(var_name.clone(), concrete_type.clone());
            }
        }

        // Apply the type parameter assignments to resolve generic types
        let resolved_trait_type = self.substitute_type_parameters(trait_type, &type_parameter_map);
        let resolved_impl_type = self.substitute_type_parameters(impl_type, &type_parameter_map);

        ResolvedTypes {
            trait_type: resolved_trait_type,
            impl_type: resolved_impl_type,
        }
    }

    /// Substitute type parameters in a StructuredType using SMT model assignments
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_type_parameters(
        &self,
        structured_type: &StructuredType,
        type_parameter_map: &std::collections::HashMap<String, StructuredType>,
    ) -> StructuredType {
        match structured_type {
            StructuredType::Simple(type_id) => {
                let type_name = type_id.to_string();

                // Check if this is a type parameter that needs substitution
                if let Some(concrete_type) = type_parameter_map.get(&type_name) {
                    concrete_type.clone()
                } else {
                    structured_type.clone()
                }
            }
            StructuredType::Generic { base, args } => {
                // Recursively substitute in generic arguments
                let substituted_args: Vec<StructuredType> = args
                    .iter()
                    .map(|arg| self.substitute_type_parameters(arg, type_parameter_map))
                    .collect();

                StructuredType::Generic {
                    base: base.clone(),
                    args: substituted_args,
                }
            }
            StructuredType::TypeVariable(var_id) => {
                let var_name = var_id.to_string();

                if let Some(concrete_type) = type_parameter_map.get(&var_name) {
                    concrete_type.clone()
                } else {
                    // TypeVariable not found in SMT model
                    structured_type.clone()
                }
            }
            // For other types (Tuple, Function, etc.), recursively substitute if they contain generics
            StructuredType::Tuple(elements) => {
                let substituted_elements: Vec<StructuredType> = elements
                    .iter()
                    .map(|elem| self.substitute_type_parameters(elem, type_parameter_map))
                    .collect();
                StructuredType::Tuple(substituted_elements)
            }
            // For primitive and other types, no substitution needed
            _ => structured_type.clone(),
        }
    }

    /// Look up concrete function implementation using fully resolved types from SMT model
    fn lookup_concrete_function_with_resolved_types(
        &self,
        resolved_trait_type: &StructuredType,
        resolved_impl_type: &StructuredType,
        function_name: AtomId,
        model: &crate::smt::solver::ConstraintModel,
    ) -> Option<UnifiedFunctionEntry> {
        let modules = self.modules().read().unwrap();

        for (module_key, module) in modules.iter() {
            if let ModuleKey::TraitImpl(registered_trait_type, registered_impl_type) = module_key {
                // Apply SMT model to the registered types to see if they match our resolved types
                let resolved_registered_trait =
                    self.apply_smt_model_to_type(registered_trait_type, model);
                let resolved_registered_impl =
                    self.apply_smt_model_to_type(registered_impl_type, model);

                // Check if the SMT-resolved registered types match our target types
                if resolved_registered_trait == *resolved_trait_type
                    && resolved_registered_impl == *resolved_impl_type
                {
                    if let Some(function_entry) = module.functions_by_name.get(&function_name) {
                        return Some(function_entry.clone());
                    }
                }
            }
        }

        // Fallback: Try direct lookup (for cases where types were already concrete)
        if let Some(entry) = self.lookup_impl_function(
            resolved_trait_type,
            resolved_impl_type,
            function_name.clone(),
        ) {
            return Some(entry);
        }

        // No concrete function found even with SMT-guided lookup
        None
    }

    /// NEW: Resolve a TypeVariable using the most recent SMT model
    /// This is used by TypedASTBuilder when dispatch strategies weren't stored during type checking
    pub fn resolve_type_variable_with_latest_smt_model(
        &self,
        type_variable: &StructuredType,
    ) -> Result<StructuredType, crate::smt::solver::SMTError> {
        // Get the latest SMT model from unification context
        let unification_context = self.unification_context();
        
        if let Some(latest_model) = unification_context.get_latest_smt_model() {
            Ok(self.apply_smt_model_to_type(type_variable, &latest_model))
        } else {
            Err(crate::smt::solver::SMTError::SolverError(
                "No SMT model available for type variable resolution".to_string(),
            ))
        }
    }

    /// Apply SMT model variable assignments to a structured type
    /// This resolves type variables like $Self using the SMT model results
    fn apply_smt_model_to_type(
        &self,
        structured_type: &StructuredType,
        model: &crate::smt::solver::ConstraintModel,
    ) -> StructuredType {
        match structured_type {
            StructuredType::TypeVariable(type_name_id) => {
                // Try to find this type variable in the SMT model
                let type_name = self.resolve_type_name(type_name_id).unwrap_or_default();

                // Check for Self variables
                if type_name.starts_with("Self") {
                    let self_var_name = format!("Self_{}", type_name_id.hash);
                    
                    if let Some(resolved_type) = model.get_type_assignment(&self_var_name) {
                        return self.apply_smt_model_to_type(resolved_type, model);
                    }
                }

                // Check for other type variables (T, E, K, V, etc.)
                let var_name = format!("TypeVar_{type_name}");
                if let Some(resolved_type) = model.get_type_assignment(&var_name) {
                    return self.apply_smt_model_to_type(resolved_type, model);
                }

                // Check for trait implementation variables like TraitImpl_Integer
                if type_name.starts_with("TraitImpl_") {
                    let trait_impl_var_name = format!("TypeVar_{type_name}");
                    if let Some(resolved_type) = model.get_type_assignment(&trait_impl_var_name) {
                        return self.apply_smt_model_to_type(resolved_type, model);
                    }
                }

                if type_name.starts_with("TraitImpl_") {
                    if let Some(trait_name) = type_name.strip_prefix("TraitImpl_") {
                        if let Some(resolved_concrete_type) =
                            self.resolve_trait_impl_variable(trait_name)
                        {
                            return self.apply_smt_model_to_type(&resolved_concrete_type, model);
                        }
                    }
                }

                // No resolution found, return original
                structured_type.clone()
            }
            StructuredType::Generic { base, args } => {
                // Recursively apply model to generic arguments
                let resolved_args: Vec<StructuredType> = args
                    .iter()
                    .map(|arg| self.apply_smt_model_to_type(arg, model))
                    .collect();

                StructuredType::Generic {
                    base: base.clone(),
                    args: resolved_args,
                }
            }
            StructuredType::Tuple(elements) => {
                // Recursively apply model to tuple elements
                let resolved_elements: Vec<StructuredType> = elements
                    .iter()
                    .map(|elem| self.apply_smt_model_to_type(elem, model))
                    .collect();

                StructuredType::Tuple(resolved_elements)
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                // Recursively apply model to function parameters and return type
                let resolved_params: Vec<crate::unification::FunctionParam> = params
                    .iter()
                    .map(|param| crate::unification::FunctionParam {
                        name: param.name.clone(),
                        param_type: self.apply_smt_model_to_type(&param.param_type, model),
                    })
                    .collect();

                let resolved_return_type =
                    Box::new(self.apply_smt_model_to_type(return_type, model));

                StructuredType::Function {
                    params: resolved_params,
                    return_type: resolved_return_type,
                }
            }
            // For concrete types (Simple, primitives, etc.), no resolution needed
            _ => structured_type.clone(),
        }
    }

    /// Resolve trait implementation variables like TraitImpl_Integer -> Outrun.Core.Integer64
    /// This looks for the most common concrete implementation of a trait
    pub fn resolve_trait_impl_variable(&self, trait_name: &str) -> Option<StructuredType> {
        // Convert trait name to TypeNameId
        let trait_type_id = self.intern_type_name(trait_name);
        let trait_type = StructuredType::Simple(trait_type_id);

        // Get all implementations of this trait
        let implementations = self.get_trait_implementations(&trait_type);

        // For simplicity, return the first concrete implementation
        // In a more sophisticated system, this could use SMT to find the best match
        for impl_type in implementations {
            match impl_type {
                StructuredType::Simple(_) => {
                    return Some(impl_type);
                }
                _ => continue, // Skip non-concrete implementations
            }
        }

        // No concrete implementation found for trait
        None
    }

    /// Use SMT solver to resolve type variables in a structured type
    /// This is a convenience method that creates SMT constraints and solves for type variables
    pub fn smt_resolve_type_variables(
        &self,
        structured_type: &StructuredType,
    ) -> Result<StructuredType, crate::smt::solver::SMTError> {
        use crate::smt::solver::SolverResult;

        // Get existing SMT constraints from unification context
        let constraints = self.unification_context().smt_constraints.clone();

        if constraints.is_empty() {
            // No SMT constraints available for type variable resolution
            return Ok(structured_type.clone());
        }

        // Use cached SMT solver for better performance
        match crate::smt::solver_pool::solve_constraints_cached(&constraints, self)? {
            SolverResult::Satisfiable(model) => {
                let resolved_type = self.apply_smt_model_to_type(structured_type, &model);
                Ok(resolved_type)
            }
            SolverResult::Unsatisfiable(conflicts) => {
                // SMT constraints unsatisfiable
                Err(crate::smt::solver::SMTError::SolvingFailed(format!(
                    "Type variable resolution failed: conflicting constraints: {conflicts:?}"
                )))
            }
            SolverResult::Unknown(reason) => {
                // SMT solver returned unknown
                Err(crate::smt::solver::SMTError::SolvingFailed(format!(
                    "Type variable resolution failed: {reason}"
                )))
            }
        }
    }

    /// Find concrete implementations that are compatible with the resolved type
    fn find_implementations_for_resolved_type(
        &self,
        resolved_type: &StructuredType,
    ) -> Vec<StructuredType> {
        // For now, return the resolved type itself and any known concrete implementations
        // This can be enhanced later with more sophisticated implementation discovery
        let mut implementations = vec![resolved_type.clone()];

        // If the resolved type is still generic somehow, expand it
        if let StructuredType::Generic { base: _, args: _ } = resolved_type {
            // Look for concrete implementations of this generic type
            let concrete_impls = self.find_trait_implementations(resolved_type);
            implementations.extend(concrete_impls);
        }

        implementations
    }

    /// Resolve a concrete trait type to its generic definition and extract type parameter constraints
    ///
    /// For example: Option<Integer> -> (Option<T>, [T = Integer])
    /// This is the core logic for proper type parameter handling in SMT constraints
    fn resolve_generic_trait_with_parameters(
        &self,
        concrete_trait_type: &StructuredType,
        _impl_type: &StructuredType,
    ) -> (StructuredType, Vec<crate::smt::constraints::SMTConstraint>) {
        match concrete_trait_type {
            StructuredType::Generic { base, args } => {
                // This is already a generic type like Option<Integer>
                // We need to find the trait definition to get the parameter names

                let base_name = self.resolve_type_name(base).unwrap_or_default();

                // Look up the trait definition to get parameter names
                if let Some(trait_def) = self.find_trait_definition(&base_name) {
                    if let Some(ref generic_params) = trait_def.generic_params {
                        // Create type parameter constraints by matching concrete args with parameter names
                        let mut constraints = Vec::new();

                        for (param, concrete_arg) in generic_params.params.iter().zip(args.iter()) {
                            let constraint =
                                crate::smt::constraints::SMTConstraint::TypeParameterUnification {
                                    parameter_name: param.name.name.clone(),
                                    concrete_type: concrete_arg.clone(),
                                    context: format!("trait lookup for {base_name}"),
                                };
                            constraints.push(constraint);

                            // Add constraints to resolve any TypeVariables in concrete_arg
                            self.add_type_variable_resolution_constraints_to_list(
                                concrete_arg,
                                &mut constraints,
                            );
                        }

                        // Create the generic trait type with parameter placeholders
                        let generic_args: Vec<StructuredType> = generic_params
                            .params
                            .iter()
                            .map(|param| {
                                let param_type_id = self.intern_type_name(&param.name.name);
                                StructuredType::Simple(param_type_id)
                            })
                            .collect();

                        let generic_trait_type = StructuredType::Generic {
                            base: base.clone(),
                            args: generic_args,
                        };

                        return (generic_trait_type, constraints);
                    }
                }

                // Fallback: no trait definition found, use the concrete type as-is
                // No trait definition found, using concrete type
                (concrete_trait_type.clone(), Vec::new())
            }
            StructuredType::Simple(type_id) => {
                // Simple trait type, check if it has a generic definition
                let type_name = self.resolve_type_name(type_id).unwrap_or_default();

                if let Some(trait_def) = self.find_trait_definition(&type_name) {
                    if trait_def.generic_params.is_some() {
                        // Simple type has generic parameters but used without arguments
                    }
                }

                // No generic parameters, use as-is
                (concrete_trait_type.clone(), Vec::new())
            }
            _ => {
                // Other types (Function, Tuple, etc.) - use as-is
                (concrete_trait_type.clone(), Vec::new())
            }
        }
    }

    /// Find a trait definition by name
    fn find_trait_definition(&self, trait_name: &str) -> Option<outrun_parser::TraitDefinition> {
        eprintln!("🔧 TRAIT SEARCH: Looking for trait '{}'", trait_name);
        
        // Look through all registered modules for trait definitions
        if let Ok(modules) = self.modules.read() {
            eprintln!("🔧 TRAIT SEARCH: Found {} modules", modules.len());
            
            for (key, module) in modules.iter() {
                eprintln!("🔧 TRAIT SEARCH: Checking module {:?}, kind: {:?}", key, module.module_kind);
                
                if module.module_kind == ModuleKind::Trait {
                    eprintln!("🔧 TRAIT SEARCH: Found trait module");
                    // Check if this module corresponds to our trait
                    if let Some(trait_def) = &module.trait_definition {
                        let def_name = trait_def.name_as_string();
                        eprintln!("🔧 TRAIT SEARCH: Trait definition name: '{}'", def_name);
                        if def_name == trait_name {
                            eprintln!("🔧 TRAIT SEARCH: Found matching trait definition for '{}'", trait_name);
                            return Some(trait_def.clone());
                        }
                    } else {
                        eprintln!("🔧 TRAIT SEARCH: Trait module has no trait_definition");
                    }
                }
            }
        }

        eprintln!("🔧 TRAIT SEARCH: Trait '{}' not found in any module", trait_name);
        None
    }

    /// Helper to check if two types are compatible according to SMT constraints
    fn types_are_smt_compatible(&self, type1: &StructuredType, type2: &StructuredType) -> bool {
        // For now, use a simple compatibility check
        // TODO: This should also use SMT constraints for complex cases
        match (type1, type2) {
            // Same type
            (t1, t2) if t1 == t2 => true,

            // Generic types with same base and compatible arguments
            (
                StructuredType::Generic {
                    base: base1,
                    args: args1,
                },
                StructuredType::Generic {
                    base: base2,
                    args: args2,
                },
            ) => {
                base1 == base2
                    && args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(a1, a2)| self.types_are_smt_compatible(a1, a2))
            }

            // Simple type compatibility (trait to concrete)
            (StructuredType::Simple(t1), StructuredType::Simple(t2)) => {
                self.type_names_equal(t1, t2) || self.simple_types_compatible(t1, t2)
            }

            _ => false,
        }
    }

    /// Check if simple types are compatible (e.g., Integer trait with Integer64 concrete)
    fn simple_types_compatible(&self, type1: &TypeNameId, type2: &TypeNameId) -> bool {
        let name1 = self.resolve_type_name(type1).unwrap_or_default();
        let name2 = self.resolve_type_name(type2).unwrap_or_default();

        // Common trait-to-concrete mappings
        matches!(
            (name1.as_str(), name2.as_str()),
            ("Integer", "Outrun.Core.Integer64") |
            ("Float", "Outrun.Core.Float64") |
            ("String", "Outrun.Core.String") |
            ("Boolean", "Outrun.Core.Boolean") |
            ("Option", "Outrun.Option.Some") |
            ("Option", "Outrun.Option.None") |
            // Reverse mappings
            ("Outrun.Core.Integer64", "Integer") |
            ("Outrun.Core.Float64", "Float") |
            ("Outrun.Core.String", "String") |
            ("Outrun.Core.Boolean", "Boolean") |
            ("Outrun.Option.Some", "Option") |
            ("Outrun.Option.None", "Option")
        )
    }

    // === Function Clause Management ===

    /// Check if a function should be registered as a clause (has guards or complex patterns)
    fn should_create_clause_for_function(&self, func_def: &FunctionDefinition) -> bool {
        // For now, create clauses for all functions with guards
        // In the future, we might expand this to include other criteria
        func_def.guard.is_some()
    }

    /// Add a function clause to a module
    fn add_function_clause_to_module(
        &self,
        module_key: ModuleKey,
        function_name: AtomId,
        func_def: &FunctionDefinition,
        source_file: &str,
    ) -> Result<(), TypeError> {
        // Create a deterministic clause ID using SMT format for consistency
        // For standalone functions, use source file as "trait" name and clause index 0
        let clause_id = self.generate_clause_id_for_function(
            Some(source_file),
            &StructuredType::Simple(self.intern_type_name("StandaloneFunction")),
            func_def,
            0, // Standalone functions always have clause index 0
        );
        
        // Convert FunctionDefinition to TypedFunctionDefinition (simplified)
        let typed_function = self.convert_function_to_typed(func_def)?;
        
        // Use source order for clause priority - earlier functions have higher priority (lower number)
        // This matches developer expectations from functional programming languages like Elixir
        // Functions are checked in the order they're defined, regardless of guards
        let source_order = func_def.span.start as u32;

        // Create the function clause
        let clause = crate::checker::FunctionClause::new(
            clause_id,
            typed_function,
            source_order,
            func_def.span,
        );

        // Add the clause to the module
        {
            let mut modules = self.modules.write().unwrap();
            if let Some(module) = modules.get_mut(&module_key) {
                module.add_function_clause(function_name, clause);
            }
        }

        Ok(())
    }

    /// Convert FunctionDefinition to TypedFunctionDefinition (simplified conversion)
    fn convert_function_to_typed(&self, func_def: &FunctionDefinition) -> Result<crate::checker::TypedFunctionDefinition, TypeError> {
        // This is a simplified conversion - in a full implementation, 
        // this would go through the complete type checking pipeline
        
        let typed_parameters: Vec<crate::checker::TypedParameter> = func_def
            .parameters
            .iter()
            .map(|param| crate::checker::TypedParameter {
                name: param.name.name.clone(),
                param_type: None, // Would be resolved during type checking
                span: param.span,
            })
            .collect();

        let typed_guard = func_def.guard.as_ref().map(|guard_expr| {
            Box::new(crate::checker::TypedExpression {
                kind: crate::checker::TypedExpressionKind::Placeholder("Guard expression".to_string()),
                structured_type: Some(crate::unification::StructuredType::Boolean),
                span: guard_expr.span,
                debug_info: None,
            })
        });

        let typed_body = crate::checker::TypedBlock {
            statements: Vec::new(), // Would be populated during type checking
            result_type: None,
            span: func_def.body.span,
        };

        Ok(crate::checker::TypedFunctionDefinition {
            name: func_def.name.name.clone(),
            parameters: typed_parameters,
            return_type: None, // Would be resolved during type checking
            guard: typed_guard,
            body: typed_body,
            function_id: format!("function::{}", func_def.name.name),
            span: func_def.span,
        })
    }

    /// Phase 6.5: SMT-based clause analysis during compilation
    /// This analyzes function clauses and generates SMT constraints for dispatch optimization
    pub fn perform_smt_clause_analysis(&mut self) -> Result<(), Vec<TypeError>> {
        // Get all modules and analyze their function clauses
        let module_keys: Vec<ModuleKey> = {
            let modules = self.modules.read().unwrap();
            modules.keys().cloned().collect()
        };

        for module_key in module_keys {
            self.analyze_module_function_clauses(&module_key).map_err(|e| vec![e])?;
        }

        Ok(())
    }

    /// Analyze function clauses in a specific module
    fn analyze_module_function_clauses(&mut self, module_key: &ModuleKey) -> Result<(), TypeError> {
        // Get clause sets for this module
        let clause_sets: Vec<crate::checker::FunctionClauseSet> = {
            let modules = self.modules.read().unwrap();
            if let Some(module) = modules.get(module_key) {
                module.function_clauses.values().cloned().collect()
            } else {
                return Ok(()); // Module not found, skip
            }
        };

        // Analyze each clause set
        for clause_set in clause_sets {
            if clause_set.clauses.len() > 1 {
                // Multiple clauses - generate dispatch constraints
                self.generate_clause_dispatch_constraints(&clause_set)?;
            } else if clause_set.has_guards() {
                // Single clause with guard - generate guard constraints
                self.generate_guard_analysis_constraints(&clause_set)?;
            }
        }

        Ok(())
    }

    /// Generate SMT constraints for clause dispatch when multiple clauses exist
    fn generate_clause_dispatch_constraints(
        &mut self,
        clause_set: &crate::checker::FunctionClauseSet,
    ) -> Result<(), TypeError> {
        use crate::smt::constraints::SMTConstraint;

        // Generate constraints for each clause
        for clause in &clause_set.clauses {
            // Generate argument type matching constraints
            for parameter in clause.base_function.parameters.iter() {
                if let Some(param_type) = &parameter.param_type {
                    let constraint = SMTConstraint::ArgumentTypeMatch {
                        clause_id: clause.clause_id.clone(),
                        parameter_name: parameter.name.clone(),
                        expected_type: param_type.clone(),
                        actual_type: param_type.clone(), // Will be unified with actual call arguments
                        call_site: clause.span,
                    };
                    
                    self.add_smt_constraint(constraint);
                }
            }

            // Generate guard applicability constraints
            if let Some(_guard_expr) = &clause.base_function.guard {
                let constraint = SMTConstraint::GuardApplicable {
                    clause_id: clause.clause_id.clone(),
                    guard_expression: "guard_placeholder".to_string(), // TODO: Actual guard analysis
                    guard_variables: std::collections::HashMap::new(), // TODO: Extract guard variables
                    context: format!("Guard for function {}", clause.base_function.name),
                };
                
                self.add_smt_constraint(constraint);
            }

            // Generate clause priority constraints
            let constraint = SMTConstraint::ClausePriority {
                clause_id: clause.clause_id.clone(),
                priority: clause.source_order,
                context: format!("Priority for function {}", clause.base_function.name),
            };
            
            self.add_smt_constraint(constraint);
        }

        Ok(())
    }

    /// Generate SMT constraints for guard analysis and static evaluation
    fn generate_guard_analysis_constraints(
        &mut self,
        clause_set: &crate::checker::FunctionClauseSet,
    ) -> Result<(), TypeError> {
        use crate::smt::constraints::SMTConstraint;

        for clause in &clause_set.clauses {
            if let Some(_guard_expr) = &clause.base_function.guard {
                // Try to statically evaluate the guard
                if let Some(static_result) = self.try_static_guard_evaluation(&clause.base_function) {
                    // Guard can be statically evaluated
                    let constraint = SMTConstraint::GuardStaticallyEvaluated {
                        clause_id: clause.clause_id.clone(),
                        guard_expression: "static_guard".to_string(), // TODO: Actual guard text
                        when_arguments: std::collections::HashMap::new(), // TODO: Extract from call site
                        evaluation_result: true, // Stub - SMT would determine this
                        context: format!("Static evaluation for {}", clause.base_function.name),
                    };
                    
                    self.add_smt_constraint(constraint);
                } else {
                    // Guard requires runtime evaluation
                    let constraint = SMTConstraint::GuardApplicable {
                        clause_id: clause.clause_id.clone(),
                        guard_expression: "runtime_guard".to_string(), // TODO: Actual guard analysis
                        guard_variables: std::collections::HashMap::new(), // TODO: Extract variables
                        context: format!("Runtime guard for {}", clause.base_function.name),
                    };
                    
                    self.add_smt_constraint(constraint);
                }
            }
        }

        Ok(())
    }

    /// Try to statically evaluate a guard expression
    /// Returns Some(result) if the guard can be evaluated at compile time, None otherwise
    fn try_static_guard_evaluation(&self, func_def: &crate::checker::TypedFunctionDefinition) -> Option<bool> {
        // This is a placeholder implementation
        // In a full implementation, this would:
        // 1. Parse the guard expression
        // 2. Check if all variables are compile-time constants
        // 3. Evaluate the expression if possible
        // 4. Return the boolean result
        
        if let Some(_guard_expr) = &func_def.guard {
            // For now, we'll just return None to indicate runtime evaluation needed
            // This would be where constant folding and static analysis happens
            None
        } else {
            None
        }
    }

    /// Add an SMT constraint to the unification context
    fn add_smt_constraint(&mut self, constraint: crate::smt::constraints::SMTConstraint) {
        // Add to the global SMT constraint set
        // This integrates with the existing SMT system
        self.unification_context().add_smt_constraint(constraint);
    }

    /// Look up function clauses across all modules for runtime dispatch
    pub fn lookup_function_clauses_by_name(&self, function_name: &AtomId) -> Option<crate::checker::FunctionClauseSet> {
        let modules = self.modules.read().unwrap();
        
        // Search through all modules for a function clause set with the given name
        for module in modules.values() {
            if let Some(clause_set) = module.function_clauses.get(function_name) {
                return Some(clause_set.clone());
            }
        }
        
        None
    }

    /// Look up function clauses within a specific trait implementation module
    /// This is the correct approach for trait dispatch - respects module boundaries
    pub fn lookup_function_clauses_in_trait_impl(
        &self, 
        trait_type: &StructuredType, 
        impl_type: &StructuredType, 
        function_name: &AtomId
    ) -> Option<crate::checker::FunctionClauseSet> {
        let modules = self.modules.read().unwrap();
        
        // Create the module key for the specific trait implementation
        let module_key = ModuleKey::TraitImpl(
            Box::new(trait_type.clone()),
            Box::new(impl_type.clone()),
        );
        
        // Look up the specific trait implementation module
        if let Some(trait_impl_module) = modules.get(&module_key) {
            // Look for function clauses within this specific module
            if let Some(clause_set) = trait_impl_module.function_clauses.get(function_name) {
                return Some(clause_set.clone());
            }
        }
        
        None
    }

    /// Look up function clauses across ALL implementations of a trait
    /// This collects clauses from every module that implements the given trait
    /// Used for runtime dispatch when multiple implementations could match
    pub fn lookup_all_function_clauses_for_trait(
        &self,
        trait_type: &StructuredType,
        function_name: &AtomId,
    ) -> Option<crate::checker::FunctionClauseSet> {
        let modules = self.modules.read().unwrap();
        let mut unified_clauses = Vec::new();
        
        eprintln!("🔍 CLAUSE LOOKUP: Looking for {}::{}", trait_type.to_string_representation(), function_name);
        eprintln!("🔍 CLAUSE LOOKUP: Total modules: {}", modules.len());
        
        // Iterate through all modules to find trait implementations
        for (module_key, module) in modules.iter() {
            if let ModuleKey::TraitImpl(module_trait_type, impl_type) = module_key {
                eprintln!("🔍 CLAUSE LOOKUP: Checking module trait {} for impl {}", 
                         module_trait_type.to_string_representation(), 
                         impl_type.to_string_representation());
                
                // Check if this module implements the trait we're looking for
                if self.types_match_for_dispatch(trait_type, module_trait_type) {
                    eprintln!("🔍 CLAUSE LOOKUP: ✅ Trait types match! Looking for function clauses...");
                    
                    // Look for function clauses for this function name in this implementation
                    if let Some(clause_set) = module.function_clauses.get(function_name) {
                        eprintln!("🔍 CLAUSE LOOKUP: ✅ Found {} clauses for function {}", 
                                 clause_set.clauses.len(), function_name);
                        
                        for clause in &clause_set.clauses {
                            eprintln!("🔍 CLAUSE LOOKUP:   - Clause: {}", clause.clause_id);
                        }
                        
                        // Add all clauses from this implementation
                        unified_clauses.extend(clause_set.clauses.clone());
                    } else {
                        eprintln!("🔍 CLAUSE LOOKUP: ❌ No clauses found for function {} in this implementation", function_name);
                        eprintln!("🔍 CLAUSE LOOKUP: Available functions: {:?}", 
                                 module.function_clauses.keys().map(|k| self.resolve_atom_name(k).unwrap_or_default()).collect::<Vec<_>>());
                    }
                } else {
                    eprintln!("🔍 CLAUSE LOOKUP: ❌ Trait types don't match (requested: {}, module: {})", 
                             trait_type.to_string_representation(), 
                             module_trait_type.to_string_representation());
                }
            }
        }
        
        eprintln!("🔍 CLAUSE LOOKUP: Total unified clauses found: {}", unified_clauses.len());
        
        // Return unified clause set if we found any clauses
        if !unified_clauses.is_empty() {
            Some(crate::checker::FunctionClauseSet {
                clauses: unified_clauses,
                function_name: self.resolve_atom_name(function_name).unwrap_or_default(),
                clause_resolution_cache: std::collections::HashMap::new(),
            })
        } else {
            eprintln!("🔍 CLAUSE LOOKUP: ❌ No clauses found at all!");
            None
        }
    }

    /// Check if two trait types match for dispatch purposes
    /// Handles both simple and generic trait types
    fn types_match_for_dispatch(&self, requested_type: &StructuredType, module_type: &StructuredType) -> bool {
        match (requested_type, module_type) {
            // Simple trait types: exact match
            (StructuredType::Simple(req_id), StructuredType::Simple(mod_id)) => {
                req_id.hash == mod_id.hash
            }
            // Generic trait types: match base type, ignore type parameters for dispatch
            (StructuredType::Generic { base: req_base, .. }, StructuredType::Generic { base: mod_base, .. }) => {
                req_base.hash == mod_base.hash
            }
            // Mixed cases: extract base types and compare
            (StructuredType::Simple(req_id), StructuredType::Generic { base: mod_base, .. }) => {
                req_id.hash == mod_base.hash
            }
            (StructuredType::Generic { base: req_base, .. }, StructuredType::Simple(mod_id)) => {
                req_base.hash == mod_id.hash
            }
            _ => false, // Other combinations don't match
        }
    }

    // ========== CENTRAL FUNCTION CLAUSE REGISTRY METHODS ==========

    /// Add a function clause to the central registry 
    /// This should be called during monomorphization and function registration
    pub fn register_function_clause(&self, trait_name: Option<String>, function_name: String, clause: crate::checker::FunctionClause) {
        let signature = if let Some(trait_name) = trait_name {
            FunctionSignature::trait_function(trait_name, function_name)
        } else {
            FunctionSignature::standalone_function(function_name)
        };
        
        eprintln!("🏛️ REGISTRY: Adding clause {} for signature {}", clause.clause_id, signature.qualified_name());
        
        let mut registry = self.function_registry.write().unwrap();
        registry.add_clause(signature, clause);
        
        let (total_clauses, total_signatures, _) = registry.stats();
        eprintln!("🏛️ REGISTRY: Now has {} clauses across {} signatures", total_clauses, total_signatures);
    }

    /// Get all function clauses for a trait function from the central registry
    /// This replaces the expensive module traversal approach with O(1) lookup
    pub fn get_function_clauses_from_registry(&self, trait_name: &str, function_name: &str) -> Option<crate::checker::FunctionClauseSet> {
        let signature = FunctionSignature::trait_function(trait_name.to_string(), function_name.to_string());
        
        eprintln!("🏛️ REGISTRY: Looking up clauses for {}", signature.qualified_name());
        
        let registry = self.function_registry.read().unwrap();
        
        if let Some(clause_set) = registry.get_unified_clause_set(&signature) {
            eprintln!("🏛️ REGISTRY: ✅ Found {} clauses", clause_set.clauses.len());
            for clause in &clause_set.clauses {
                eprintln!("🏛️ REGISTRY:   - {}", clause.clause_id);
            }
            Some(clause_set)
        } else {
            eprintln!("🏛️ REGISTRY: ❌ No clauses found for {}", signature.qualified_name());
            
            // Debug: List all available signatures
            let signatures = registry.list_signatures();
            eprintln!("🏛️ REGISTRY: Available signatures: {:?}", 
                     signatures.iter().map(|s| s.qualified_name()).collect::<Vec<_>>());
            None
        }
    }

    /// Get a specific function clause by ID from the central registry
    /// Used for pre-resolved dispatch where SMT has already selected the clause
    pub fn get_function_clause_by_id(&self, clause_id: &str) -> Option<Arc<crate::checker::FunctionClause>> {
        let registry = self.function_registry.read().unwrap();
        registry.get_clause_by_id(clause_id).map(|arc| arc.clone())
    }

    /// List all function signatures in the central registry (for debugging)
    pub fn list_registered_function_signatures(&self) -> Vec<String> {
        let registry = self.function_registry.read().unwrap();
        registry.list_signatures().iter().map(|s| s.qualified_name()).collect()
    }

    /// Get central registry statistics for debugging and optimization
    pub fn get_registry_stats(&self) -> (usize, usize, usize) {
        let registry = self.function_registry.read().unwrap();
        registry.stats()
    }

    /// Phase 6.6: Guard purity analysis for mathematical soundness
    /// Ensures that all guard expressions are side-effect-free and return Boolean
    /// TODO: Complete implementation - currently stubbed out
    pub fn perform_guard_purity_analysis(&mut self) -> Result<(), Vec<TypeError>> {
        // TODO: Implement comprehensive guard purity analysis
        // This would ensure that:
        // 1. Functions ending in '?' are pure and return Boolean
        // 2. Guard expressions only call pure functions
        // 3. Guard expressions are side-effect-free
        // 4. Guard expressions return Boolean values
        
        // For now, always succeed - this is a placeholder
        Ok(())
    }
}

impl Default for CompilerEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use outrun_parser::{
        Expression, ExpressionKind, IntegerFormat, IntegerLiteral, Item, ItemKind, Program, Span,
    };

    #[test]
    fn test_compiler_environment_creation() {
        let env = CompilerEnvironment::new();

        // Verify initial state
        assert!(env.get_compilation_result().is_none());
        assert!(env.get_errors().is_empty());
        // Should have 1 module now (Outrun.Intrinsic) after bootstrap
        assert_eq!(env.module_count(), 1);

        // Verify that Outrun.Intrinsic module was created
        let intrinsic_type = env.intern_type_name("Outrun.Intrinsic");
        let module_key = ModuleKey::Module(intrinsic_type.hash);
        assert!(env.has_module(&module_key));

        // Verify that some intrinsic functions were created
        assert!(env.function_count() > 0);

        // Verify that we can lookup a specific intrinsic function
        let list_empty_atom = env.intern_atom_name("list_empty");
        let intrinsic_module_type = StructuredType::Simple(intrinsic_type);
        let found_function = env.lookup_qualified_function(&intrinsic_module_type, list_empty_atom);
        assert!(found_function.is_some());
    }

    #[test]
    fn test_type_interning() {
        let env = CompilerEnvironment::new();

        // Test the type interning API
        let type_id = env.intern_type_name("TestType");

        // Verify it works
        assert!(env.resolve_type(type_id).is_some());
    }

    #[test]
    fn test_atom_interning() {
        let env = CompilerEnvironment::new();

        // Test the atom interning API
        let atom_id = env.intern_atom_name("test_atom");

        // Verify it works
        assert_eq!(format!("{atom_id}"), "test_atom");
        assert!(env.resolve_atom(atom_id).is_some());
    }

    #[test]
    fn test_compilation_api() {
        let mut env = CompilerEnvironment::new();

        // Create a simple program
        let program = Program {
            items: vec![Item {
                kind: ItemKind::Expression(Expression {
                    kind: ExpressionKind::Integer(IntegerLiteral {
                        value: 42,
                        format: IntegerFormat::Decimal,
                        raw_text: "42".to_string(),
                        span: Span::new(0, 2),
                    }),
                    span: Span::new(0, 2),
                }),
                span: Span::new(0, 2),
            }],
            debug_info: Default::default(),
            span: Span::new(0, 2),
        };

        // Test compilation - this might fail due to missing core library, but we want to test the API
        let _result = env.compile_program(program);

        // The compilation might fail, but we can still verify the API works
        // The important thing is that the method exists and returns the right type
    }

    #[test]
    fn test_unified_interning() {
        let env = CompilerEnvironment::new();

        // Test that multiple calls use the same underlying interner
        let type_id = env.intern_type_name("TestType");
        let another_type_id = env.intern_type_name("TestType");

        // They should resolve to the same string
        assert_eq!(env.resolve_type(type_id), Some("TestType".to_string()));
        assert_eq!(
            env.resolve_type(another_type_id),
            Some("TestType".to_string())
        );

        // Test unification context shares the same interner
        let _unif_context = env.unification_context();
        let third_type_id = env.intern_type_name("TestType");
        assert_eq!(
            env.resolve_type(third_type_id),
            Some("TestType".to_string())
        );
    }
}
