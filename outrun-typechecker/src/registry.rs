//! Unified module registry for Outrun's "everything is a module" system
//!
//! Manages all modules (protocols, structs, implementations) in a single namespace,
//! ensuring coherence and preventing conflicts.

use crate::error::ImplementationError;
use crate::types::{ModuleName, Type, TypeModule, ProtocolDefinition, ConcreteTypeDefinition, FunctionDefinition};
use outrun_parser::Span;
use std::collections::{HashMap, HashSet};

/// Information about a protocol implementation (legacy compatibility)
#[derive(Debug, Clone, PartialEq)]
pub struct ImplementationInfo {
    /// The type implementing the protocol
    pub implementing_type: ModuleName,
    /// Generic arguments for the implementing type (e.g., List<T> -> [T])
    pub implementing_args: Vec<Type>,
    /// The protocol being implemented
    pub protocol_name: ModuleName,
    /// Generic arguments for the protocol in this implementation
    pub protocol_args: Vec<Type>,
    /// Module where this implementation is defined
    pub defining_module: ModuleName,
    /// Source location of the implementation for error reporting
    pub span: Option<Span>,
}

/// Registry key uniquely identifying a protocol implementation (legacy compatibility)
/// Maps (Protocol, ConcreteType) -> Implementation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementationKey {
    pub protocol_name: ModuleName,
    pub implementing_type: ModuleName,
    /// Canonical string representation of generic args for hashing
    /// e.g., List<String> -> "String", Map<K,V> -> "K,V"
    pub type_args_signature: String,
}

impl ImplementationKey {
    /// Create a new implementation key
    pub fn new(
        protocol_name: ModuleName,
        implementing_type: ModuleName,
        implementing_args: &[Type],
    ) -> Self {
        let type_args_signature = if implementing_args.is_empty() {
            String::new()
        } else {
            implementing_args
                .iter()
                .map(|arg| format!("{arg}"))
                .collect::<Vec<_>>()
                .join(",")
        };

        Self {
            protocol_name,
            implementing_type,
            type_args_signature,
        }
    }
}

/// Unified type registry for all modules in Outrun's system
#[derive(Debug, Clone)]
pub struct TypeRegistry {
    /// Map from module name to module definition
    modules: HashMap<ModuleName, TypeModule>,
    /// Current module being processed (for orphan rule checking)
    current_module: Option<ModuleName>,
    /// Set of modules that are considered "local" (part of current package)
    local_modules: HashSet<ModuleName>,
}

impl TypeRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            current_module: None,
            local_modules: HashSet::new(),
        }
    }

    /// Set the current module being processed (for orphan rule enforcement)
    pub fn set_current_module(&mut self, module: ModuleName) {
        self.current_module = Some(module);
    }

    /// Add a module to the set of local modules (part of current package)
    pub fn add_local_module(&mut self, module: ModuleName) {
        self.local_modules.insert(module);
    }

    /// Get a module by name
    pub fn get_module(&self, name: &str) -> Option<&TypeModule> {
        self.modules.get(&ModuleName::new(name))
    }

    /// Register a module in the registry
    pub fn register_module(&mut self, module: TypeModule) -> Result<(), crate::error::TypeError> {
        let module_name = match &module {
            TypeModule::Protocol { name, .. } => name.clone(),
            TypeModule::Struct { name, .. } => name.clone(),
            TypeModule::Implementation { name, .. } => name.clone(),
            TypeModule::ForwardBinding { name, .. } => name.clone(),
        };

        // Check for conflicts
        if let Some(existing) = self.modules.get(&module_name) {
            // Allow forward bindings to be replaced by actual definitions
            if matches!(existing, TypeModule::ForwardBinding { .. }) {
                // Replace forward binding with actual definition
                self.modules.insert(module_name, module);
                return Ok(());
            }
            
            return Err(crate::error::TypeError::ModuleRedefinition {
                module_name: module_name.as_str().to_string(),
                span: None,
            });
        }

        self.modules.insert(module_name, module);
        Ok(())
    }

    /// Check if a name refers to a protocol
    pub fn is_protocol(&self, name: &str) -> bool {
        if let Some(module) = self.get_module(name) {
            matches!(module, TypeModule::Protocol { .. })
        } else {
            false
        }
    }

    /// Check if a name refers to a struct
    pub fn is_struct(&self, name: &str) -> bool {
        if let Some(module) = self.get_module(name) {
            matches!(module, TypeModule::Struct { .. })
        } else {
            false
        }
    }

    /// Get an implementation module by type and protocol names
    pub fn get_implementation(&self, impl_type: &str, protocol: &str) -> Option<&TypeModule> {
        let impl_name = ModuleName::implementation(impl_type, protocol);
        self.modules.get(&impl_name)
    }

    /// Legacy compatibility: Get protocol definition
    pub fn get_protocol_definition(&self, protocol_name: &ModuleName) -> Option<ProtocolDefinition> {
        if let Some(TypeModule::Protocol { definition, .. }) = self.modules.get(protocol_name) {
            Some(definition.clone())
        } else {
            None
        }
    }

    /// Legacy compatibility: Check if protocol exists
    pub fn has_protocol(&self, protocol_name: &ModuleName) -> bool {
        matches!(self.modules.get(protocol_name), Some(TypeModule::Protocol { .. }))
    }

    /// Legacy compatibility: Register protocol implementation
    pub fn register_implementation(
        &mut self,
        implementing_type: ModuleName,
        implementing_args: Vec<Type>,
        protocol_name: ModuleName,
        protocol_args: Vec<Type>,
        defining_module: ModuleName,
        span: Option<Span>,
    ) -> Result<(), ImplementationError> {
        let impl_name = ModuleName::implementation(implementing_type.as_str(), protocol_name.as_str());
        
        // Check for existing implementation
        if self.modules.contains_key(&impl_name) {
            return Err(ImplementationError::ConflictingImplementation {
                protocol_name: protocol_name.as_str().to_string(),
                type_name: implementing_type.as_str().to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                previous_span: None,
            });
        }

        // Validate orphan rule
        self.validate_orphan_rule(&protocol_name, &implementing_type, &defining_module, span)?;

        // Create implementation module
        let impl_module = TypeModule::Implementation {
            name: impl_name,
            implementing_type,
            protocol: protocol_name,
            generic_bindings: implementing_args,
            functions: Vec::new(), // Will be populated later
            source_location: span.unwrap_or_else(|| Span::new(0, 0)),
            defining_module,
        };

        self.register_module(impl_module).map_err(|_| {
            ImplementationError::ConflictingImplementation {
                protocol_name: protocol_name.as_str().to_string(),
                type_name: implementing_type.as_str().to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                previous_span: None,
            }
        })?;

        Ok(())
    }

    /// Validate orphan rule for implementations
    fn validate_orphan_rule(
        &self,
        protocol_name: &ModuleName,
        implementing_type: &ModuleName,
        defining_module: &ModuleName,
        span: Option<Span>,
    ) -> Result<(), ImplementationError> {
        // Check if either protocol or type module is local
        let protocol_is_local = self.local_modules.contains(protocol_name);
        let type_is_local = self.local_modules.contains(implementing_type);
        let defining_is_local = self.local_modules.contains(defining_module);

        // Orphan rule: at least one of protocol or type must be defined in a local module
        if !protocol_is_local && !type_is_local {
            return Err(ImplementationError::OrphanRuleViolation {
                protocol_name: protocol_name.as_str().to_string(),
                type_name: implementing_type.as_str().to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }

        // Additional check: implementation must be defined in a local module
        if !defining_is_local {
            return Err(ImplementationError::OrphanRuleViolation {
                protocol_name: protocol_name.as_str().to_string(),
                type_name: implementing_type.as_str().to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }

        Ok(())
    }

    /// Legacy compatibility: Check if implementation exists
    pub fn has_implementation(&self, protocol_name: &ModuleName, type_name: &ModuleName) -> bool {
        let impl_name = ModuleName::implementation(type_name.as_str(), protocol_name.as_str());
        matches!(self.modules.get(&impl_name), Some(TypeModule::Implementation { .. }))
    }

    /// Legacy compatibility: Get implementation count
    pub fn implementation_count(&self) -> usize {
        self.modules.values()
            .filter(|module| matches!(module, TypeModule::Implementation { .. }))
            .count()
    }

    /// Legacy compatibility: Check if registry is empty
    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }

    /// Legacy compatibility: Clear all modules
    pub fn clear(&mut self) {
        self.modules.clear();
    }

    /// Initialize with core types that are always available
    pub fn with_core_types() -> Self {
        let mut registry = Self::new();
        registry.register_core_types();
        registry
    }

    /// Register all core types (Integer64, String, Float64, etc.)
    fn register_core_types(&mut self) {
        let core_module = ModuleName::new("Outrun.Core");

        // Register core concrete types
        let core_types = [
            ("Outrun.Core.Integer64", false),
            ("Outrun.Core.String", false),
            ("Outrun.Core.Float64", false),
            ("Outrun.Core.Boolean", false),
            ("Outrun.Core.Atom", false),
            ("Outrun.Core.List", true),
            ("Outrun.Core.Map", true),
            ("Outrun.Core.Tuple", true),
            ("Outrun.Core.Option", true),
            ("Outrun.Core.Result", true),
        ];

        for (type_name, is_generic) in &core_types {
            let type_def = ConcreteTypeDefinition {
                type_name: ModuleName::new(*type_name),
                defining_module: core_module.clone(),
                is_generic: *is_generic,
                span: None,
                never_info: None,
            };

            let module = TypeModule::Struct {
                name: ModuleName::new(*type_name),
                definition: type_def,
                source_location: Span::new(0, 0),
                generic_arity: if *is_generic { 1 } else { 0 },
            };

            let _ = self.register_module(module);
        }
    }
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::with_core_types()
    }
}

/// Legacy type alias for backward compatibility during transition
pub type ProtocolRegistry = TypeRegistry;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Type;

    fn create_test_registry() -> TypeRegistry {
        let mut registry = TypeRegistry::new();

        // Set up local modules (simulating current package)
        registry.add_local_module(ModuleName::new("String"));
        registry.add_local_module(ModuleName::new("Integer"));
        registry.add_local_module(ModuleName::new("List"));
        registry.add_local_module(ModuleName::new("Display"));
        registry.add_local_module(ModuleName::new("Debug"));
        registry.add_local_module(ModuleName::new("Clone"));
        registry.add_local_module(ModuleName::new("BinaryAddition"));
        registry.add_local_module(ModuleName::new("Equality"));
        registry.add_local_module(ModuleName::new("Outrun.Core.Integer64"));

        registry.set_current_module(ModuleName::new("TestModule"));

        registry
    }

    #[test]
    fn test_unified_module_registration() {
        let mut registry = TypeRegistry::new();

        // Register a protocol module
        let protocol_def = ProtocolDefinition {
            protocol_name: ModuleName::new("Display"),
            required_protocols: HashSet::new(),
            defining_module: ModuleName::new("Display"),
            default_implementations: HashSet::new(),
            required_functions: HashSet::new(),
            span: None,
        };

        let protocol_module = TypeModule::Protocol {
            name: ModuleName::new("Display"),
            definition: protocol_def,
            source_location: Span::new(0, 0),
            generic_arity: 0,
        };

        assert!(registry.register_module(protocol_module).is_ok());
        assert!(registry.is_protocol("Display"));
        assert!(!registry.is_struct("Display"));
    }

    #[test]
    fn test_implementation_module_naming() {
        let impl_name = ModuleName::implementation("List", "Display");
        assert_eq!(impl_name.as_str(), "List:Display");
        assert!(impl_name.is_implementation());

        let (type_part, protocol_part) = impl_name.split_implementation().unwrap();
        assert_eq!(type_part, "List");
        assert_eq!(protocol_part, "Display");
    }

    #[test]
    fn test_module_conflict_detection() {
        let mut registry = TypeRegistry::new();

        // Register a protocol
        let protocol_def = ProtocolDefinition {
            protocol_name: ModuleName::new("Display"),
            required_protocols: HashSet::new(),
            defining_module: ModuleName::new("Display"),
            default_implementations: HashSet::new(),
            required_functions: HashSet::new(),
            span: None,
        };

        let protocol_module = TypeModule::Protocol {
            name: ModuleName::new("Display"),
            definition: protocol_def,
            source_location: Span::new(0, 0),
            generic_arity: 0,
        };

        assert!(registry.register_module(protocol_module).is_ok());

        // Try to register a struct with the same name - should fail
        let struct_def = ConcreteTypeDefinition {
            type_name: ModuleName::new("Display"),
            defining_module: ModuleName::new("Display"),
            is_generic: false,
            span: None,
            never_info: None,
        };

        let struct_module = TypeModule::Struct {
            name: ModuleName::new("Display"),
            definition: struct_def,
            source_location: Span::new(0, 0),
            generic_arity: 0,
        };

        assert!(registry.register_module(struct_module).is_err());
    }

    #[test]
    fn test_forward_binding_replacement() {
        let mut registry = TypeRegistry::new();

        // Register a forward binding
        let forward_binding = TypeModule::ForwardBinding {
            name: ModuleName::new("User"),
            expected_arity: Some(0),
            source_location: Span::new(0, 0),
            references: vec![],
        };

        assert!(registry.register_module(forward_binding).is_ok());

        // Now register the actual struct - should replace the forward binding
        let struct_def = ConcreteTypeDefinition {
            type_name: ModuleName::new("User"),
            defining_module: ModuleName::new("User"),
            is_generic: false,
            span: None,
            never_info: None,
        };

        let struct_module = TypeModule::Struct {
            name: ModuleName::new("User"),
            definition: struct_def,
            source_location: Span::new(0, 0),
            generic_arity: 0,
        };

        assert!(registry.register_module(struct_module).is_ok());
        assert!(registry.is_struct("User"));
    }

    #[test]
    fn test_legacy_compatibility() {
        let mut registry = create_test_registry();

        // Test legacy implementation registration
        let result = registry.register_implementation(
            ModuleName::new("String"),
            vec![],
            ModuleName::new("Display"),
            vec![],
            ModuleName::new("String"),
            None,
        );

        assert!(result.is_ok());
        assert_eq!(registry.implementation_count(), 1);
        assert!(!registry.is_empty());

        // Test legacy has_implementation
        assert!(registry.has_implementation(&ModuleName::new("Display"), &ModuleName::new("String")));
    }

    #[test]
    fn test_core_types_registration() {
        let registry = TypeRegistry::with_core_types();

        // Check that core types are registered
        assert!(registry.is_struct("Outrun.Core.Integer64"));
        assert!(registry.is_struct("Outrun.Core.String"));
        assert!(registry.is_struct("Outrun.Core.List"));
        assert!(registry.is_struct("Outrun.Core.Map"));

        // Check that non-existent types are not registered
        assert!(!registry.is_struct("NonExistent"));
        assert!(!registry.is_protocol("NonExistent"));
    }
}


