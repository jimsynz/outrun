//! Unified module registry for Outrun's "everything is a module" system
//!
//! Manages all modules (protocols, structs, implementations) in a single namespace,
//! ensuring coherence and preventing conflicts.

use crate::error::ImplementationError;
use crate::types::{
    ConcreteTypeDefinition, ModuleName, ProtocolDefinition, Type, TypeModule,
};
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
            if let TypeModule::ForwardBinding { expected_arity, .. } = existing {
                // Check arity compatibility when replacing forward binding
                let actual_arity = match &module {
                    TypeModule::Protocol { generic_arity, .. } => *generic_arity,
                    TypeModule::Struct { generic_arity, .. } => *generic_arity,
                    TypeModule::Implementation { .. } => 0, // Implementations don't have their own arity
                    TypeModule::ForwardBinding { .. } => 0, // Shouldn't happen
                };

                if let Some(expected) = expected_arity {
                    if *expected != actual_arity {
                        return Err(crate::error::TypeError::ArityConflict {
                            type_name: module_name.as_str().to_string(),
                            expected_arity: *expected,
                            found_arity: actual_arity,
                            span: crate::error::to_source_span(None)
                                .unwrap_or_else(|| miette::SourceSpan::from(0..0)),
                        });
                    }
                }

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
    pub fn get_protocol_definition(
        &self,
        protocol_name: &ModuleName,
    ) -> Option<ProtocolDefinition> {
        if let Some(TypeModule::Protocol { definition, .. }) = self.modules.get(protocol_name) {
            Some(definition.clone())
        } else {
            None
        }
    }

    /// Legacy compatibility: Check if protocol exists
    pub fn has_protocol(&self, protocol_name: &ModuleName) -> bool {
        matches!(
            self.modules.get(protocol_name),
            Some(TypeModule::Protocol { .. })
        )
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
        let impl_name =
            ModuleName::implementation(implementing_type.as_str(), protocol_name.as_str());

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
            implementing_type: implementing_type.clone(),
            protocol: protocol_name.clone(),
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
        matches!(
            self.modules.get(&impl_name),
            Some(TypeModule::Implementation { .. })
        )
    }

    /// Legacy compatibility: Get implementation count
    pub fn implementation_count(&self) -> usize {
        self.modules
            .values()
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

        // Register core concrete types with proper arity
        let core_types = [
            ("Outrun.Core.Integer64", 0),
            ("Outrun.Core.String", 0),
            ("Outrun.Core.Float64", 0),
            ("Outrun.Core.Boolean", 0),
            ("Outrun.Core.Atom", 0),
            ("Outrun.Core.List", 1),   // List<T>
            ("Outrun.Core.Map", 2),    // Map<K, V>
            ("Outrun.Core.Tuple", 0),  // Tuple is variadic, but we'll treat as 0 for now
            ("Outrun.Core.Option", 1), // Option<T>
            ("Outrun.Core.Result", 2), // Result<T, E>
        ];

        for (type_name, arity) in &core_types {
            let module_name = ModuleName::new(*type_name);
            
            // Skip registration if module already exists (idempotent registration)
            if self.modules.contains_key(&module_name) {
                continue;
            }
            
            let type_def = ConcreteTypeDefinition {
                type_name: module_name.clone(),
                defining_module: core_module.clone(),
                is_generic: *arity > 0,
                span: None,
                never_info: None,
            };

            let module = TypeModule::Struct {
                name: module_name,
                definition: type_def,
                source_location: Span::new(0, 0),
                generic_arity: *arity,
            };

            let _ = self.register_module(module);
        }
    }

    /// Legacy compatibility: Check if implementation exists with specific type arguments
    pub fn has_implementation_with_args(
        &self,
        protocol_name: &ModuleName,
        implementing_type: &ModuleName,
        _type_args: &[Type],
    ) -> bool {
        // For now, ignore type arguments and just check basic implementation existence
        // TODO: Add proper generic type argument matching when needed
        self.has_implementation(protocol_name, implementing_type)
    }

    /// Legacy compatibility: Get all implementations as an iterator
    pub fn all_implementations(&self) -> impl Iterator<Item = ImplementationInfo> + '_ {
        self.modules.values().filter_map(|module| {
            if let TypeModule::Implementation {
                implementing_type,
                protocol,
                generic_bindings,
                defining_module,
                source_location,
                ..
            } = module
            {
                Some(ImplementationInfo {
                    implementing_type: implementing_type.clone(),
                    implementing_args: generic_bindings.clone(),
                    protocol_name: protocol.clone(),
                    protocol_args: Vec::new(), // TODO: Extract from generic bindings if needed
                    defining_module: defining_module.clone(),
                    span: Some(*source_location),
                })
            } else {
                None
            }
        })
    }

    /// Legacy compatibility: Check if a protocol requires another protocol
    pub fn protocol_requires(
        &self,
        protocol_name: &ModuleName,
        required_protocol: &ModuleName,
    ) -> bool {
        if let Some(TypeModule::Protocol { definition, .. }) = self.modules.get(protocol_name) {
            definition.required_protocols.contains(required_protocol)
        } else {
            false
        }
    }

    /// Get all concrete type names (legacy compatibility)
    pub fn get_concrete_type_names(&self) -> Vec<String> {
        self.modules
            .iter()
            .filter_map(|(name, module)| match module {
                TypeModule::Struct { .. } => Some(name.as_str().to_string()),
                _ => None,
            })
            .collect()
    }

    /// Check if a type is a never type (legacy compatibility)
    pub fn is_never_type(&self, _type_name: &str) -> bool {
        // TODO: Implement proper never type detection
        false
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
        assert!(
            registry.has_implementation(&ModuleName::new("Display"), &ModuleName::new("String"))
        );
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
