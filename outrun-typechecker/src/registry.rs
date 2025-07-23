//! Protocol implementation registry with orphan rule checking
//!
//! Manages all protocol implementations across the package, ensuring coherence
//! and preventing orphan implementations that could cause conflicts.

use crate::error::ImplementationError;
use crate::types::{ModuleId, ProtocolId, Type, TypeId};
use outrun_parser::Span;
use std::collections::{HashMap, HashSet};

/// Information about a protocol implementation
#[derive(Debug, Clone, PartialEq)]
pub struct ImplementationInfo {
    /// The type implementing the protocol
    pub implementing_type: TypeId,
    /// Generic arguments for the implementing type (e.g., List<T> -> [T])
    pub implementing_args: Vec<Type>,
    /// The protocol being implemented
    pub protocol_id: ProtocolId,
    /// Generic arguments for the protocol in this implementation
    pub protocol_args: Vec<Type>,
    /// Module where this implementation is defined
    pub defining_module: ModuleId,
    /// Source location of the implementation for error reporting
    pub span: Option<Span>,
}

/// Registry key uniquely identifying a protocol implementation
/// Maps (Protocol, ConcreteType) -> Implementation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementationKey {
    pub protocol_id: ProtocolId,
    pub implementing_type: TypeId,
    /// Canonical string representation of generic args for hashing
    /// e.g., List<String> -> "String", Map<K,V> -> "K,V"
    pub type_args_signature: String,
}

impl ImplementationKey {
    /// Create a new implementation key
    pub fn new(
        protocol_id: ProtocolId,
        implementing_type: TypeId,
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
            protocol_id,
            implementing_type,
            type_args_signature,
        }
    }
}

/// Information about a protocol definition and its requirements
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolDefinition {
    /// The protocol itself
    pub protocol_id: ProtocolId,
    /// Protocols required by this protocol (e.g., Integer requires BinaryAddition)
    pub required_protocols: HashSet<ProtocolId>,
    /// Module where this protocol is defined
    pub defining_module: ModuleId,
    /// Source location for error reporting
    pub span: Option<Span>,
}

/// Comprehensive protocol implementation registry
#[derive(Debug, Clone)]
pub struct ProtocolRegistry {
    /// Map from implementation key to implementation info
    implementations: HashMap<ImplementationKey, ImplementationInfo>,
    /// Map from protocol ID to protocol definition and requirements
    protocol_definitions: HashMap<ProtocolId, ProtocolDefinition>,
    /// Current module being processed (for orphan rule checking)
    current_module: Option<ModuleId>,
    /// Set of modules that are considered "local" (part of current package)
    local_modules: HashSet<ModuleId>,
}

impl ProtocolRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            implementations: HashMap::new(),
            protocol_definitions: HashMap::new(),
            current_module: None,
            local_modules: HashSet::new(),
        }
    }

    /// Set the current module being processed (for orphan rule enforcement)
    pub fn set_current_module(&mut self, module: ModuleId) {
        self.current_module = Some(module);
    }

    /// Add a module to the set of local modules (part of current package)
    pub fn add_local_module(&mut self, module: ModuleId) {
        self.local_modules.insert(module);
    }

    /// Register a protocol definition with its requirements
    pub fn register_protocol_definition(
        &mut self,
        protocol_id: ProtocolId,
        required_protocols: HashSet<ProtocolId>,
        defining_module: ModuleId,
        span: Option<Span>,
    ) {
        let protocol_def = ProtocolDefinition {
            protocol_id: protocol_id.clone(),
            required_protocols,
            defining_module,
            span,
        };
        
        self.protocol_definitions.insert(protocol_id, protocol_def);
    }

    /// Check if a protocol requires another protocol
    /// E.g., does Integer require BinaryAddition?
    pub fn protocol_requires(&self, protocol: &ProtocolId, required: &ProtocolId) -> bool {
        if let Some(definition) = self.protocol_definitions.get(protocol) {
            definition.required_protocols.contains(required)
        } else {
            // If protocol not registered, assume no requirements
            false
        }
    }

    /// Get all protocols required by a given protocol
    pub fn get_protocol_requirements(&self, protocol: &ProtocolId) -> HashSet<ProtocolId> {
        if let Some(definition) = self.protocol_definitions.get(protocol) {
            definition.required_protocols.clone()
        } else {
            HashSet::new()
        }
    }

    /// Check if a type satisfies all requirements for a protocol
    /// This checks both direct implementation and transitive requirements
    pub fn type_satisfies_protocol(&self, type_id: &TypeId, protocol: &ProtocolId) -> bool {
        // Check direct implementation
        if !self.has_implementation(protocol, type_id) {
            return false;
        }

        // Check all required protocols recursively
        let requirements = self.get_protocol_requirements(protocol);
        for required_protocol in requirements {
            if !self.type_satisfies_protocol(type_id, &required_protocol) {
                return false;
            }
        }

        true
    }

    /// Register a new protocol implementation with full validation
    pub fn register_implementation(
        &mut self,
        implementing_type: TypeId,
        implementing_args: Vec<Type>,
        protocol_id: ProtocolId,
        protocol_args: Vec<Type>,
        defining_module: ModuleId,
        span: Option<Span>,
    ) -> Result<(), ImplementationError> {
        // Create implementation key for uniqueness checking
        let key = ImplementationKey::new(
            protocol_id.clone(),
            implementing_type.clone(),
            &implementing_args,
        );

        // Check for conflicting implementations
        if let Some(existing) = self.implementations.get(&key) {
            return Err(ImplementationError::ConflictingImplementation {
                protocol_name: protocol_id.0,
                type_name: implementing_type.name().to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                previous_span: existing
                    .span
                    .and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }

        // Perform orphan rule validation
        self.validate_orphan_rule(&protocol_id, &implementing_type, &defining_module, span)?;

        // Create and store implementation info
        let impl_info = ImplementationInfo {
            implementing_type,
            implementing_args,
            protocol_id,
            protocol_args,
            defining_module,
            span,
        };

        self.implementations.insert(key, impl_info);
        Ok(())
    }

    /// Validate orphan rule: at least one of protocol or implementing type must be local
    fn validate_orphan_rule(
        &self,
        protocol_id: &ProtocolId,
        implementing_type: &TypeId,
        defining_module: &ModuleId,
        span: Option<Span>,
    ) -> Result<(), ImplementationError> {
        // Extract module names from protocol and type
        let protocol_module = ModuleId::from_protocol_id(protocol_id);
        let type_module = ModuleId::from_type_id(implementing_type);

        // Check if either protocol or type module is local
        let protocol_is_local = self.local_modules.contains(&protocol_module);
        let type_is_local = self.local_modules.contains(&type_module);
        let defining_is_local = self.local_modules.contains(defining_module);

        // Orphan rule: at least one of protocol or type must be defined in a local module
        if !protocol_is_local && !type_is_local {
            return Err(ImplementationError::OrphanRuleViolation {
                protocol_name: protocol_id.0.clone(),
                type_name: implementing_type.name().to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }

        // Additional check: implementation must be defined in a local module
        if !defining_is_local {
            return Err(ImplementationError::OrphanRuleViolation {
                protocol_name: protocol_id.0.clone(),
                type_name: implementing_type.name().to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }

        Ok(())
    }

    /// Check if a type implements a protocol
    pub fn has_implementation(&self, protocol_id: &ProtocolId, type_id: &TypeId) -> bool {
        let key = ImplementationKey::new(protocol_id.clone(), type_id.clone(), &[]);
        self.implementations.contains_key(&key)
    }

    /// Check if a generic type implements a protocol
    pub fn has_implementation_with_args(
        &self,
        protocol_id: &ProtocolId,
        type_id: &TypeId,
        type_args: &[Type],
    ) -> bool {
        let key = ImplementationKey::new(protocol_id.clone(), type_id.clone(), type_args);
        self.implementations.contains_key(&key)
    }

    /// Get implementation info for a specific protocol-type pair
    pub fn get_implementation(
        &self,
        protocol_id: &ProtocolId,
        type_id: &TypeId,
        type_args: &[Type],
    ) -> Option<&ImplementationInfo> {
        let key = ImplementationKey::new(protocol_id.clone(), type_id.clone(), type_args);
        self.implementations.get(&key)
    }

    /// Get all implementations of a protocol
    pub fn get_protocol_implementations(
        &self,
        protocol_id: &ProtocolId,
    ) -> Vec<&ImplementationInfo> {
        self.implementations
            .values()
            .filter(|info| info.protocol_id == *protocol_id)
            .collect()
    }

    /// Get all implementations for a type
    pub fn get_type_implementations(&self, type_id: &TypeId) -> Vec<&ImplementationInfo> {
        self.implementations
            .values()
            .filter(|info| info.implementing_type == *type_id)
            .collect()
    }

    /// Get total number of registered implementations
    pub fn implementation_count(&self) -> usize {
        self.implementations.len()
    }

    /// Check if registry is empty
    pub fn is_empty(&self) -> bool {
        self.implementations.is_empty()
    }

    /// Clear all implementations and protocol definitions (useful for testing)
    pub fn clear(&mut self) {
        self.implementations.clear();
        self.protocol_definitions.clear();
    }

    /// Get an iterator over all implementation info (for dispatch table building)
    pub fn all_implementations(&self) -> impl Iterator<Item = &ImplementationInfo> {
        self.implementations.values()
    }
}

impl Default for ProtocolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Legacy implementation registry (for backward compatibility during transition)
pub type ImplementationRegistry = ProtocolRegistry;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Type;

    fn create_test_registry() -> ProtocolRegistry {
        let mut registry = ProtocolRegistry::new();

        // Set up local modules (simulating current package)
        registry.add_local_module(ModuleId::new("String"));
        registry.add_local_module(ModuleId::new("Integer"));
        registry.add_local_module(ModuleId::new("List"));
        registry.add_local_module(ModuleId::new("Display"));
        registry.add_local_module(ModuleId::new("Debug"));
        registry.add_local_module(ModuleId::new("Clone"));
        registry.add_local_module(ModuleId::new("BinaryAddition"));
        registry.add_local_module(ModuleId::new("Equality"));
        registry.add_local_module(ModuleId::new("Outrun.Core.Integer64"));

        registry.set_current_module(ModuleId::new("TestModule"));

        registry
    }

    #[test]
    fn test_module_id_creation_from_type_id() {
        let type_id = TypeId::new("Http.Client.Connection");
        let module_id = ModuleId::from_type_id(&type_id);
        assert_eq!(module_id.name(), "Http.Client.Connection");
    }

    #[test]
    fn test_module_id_creation_from_protocol_id() {
        let protocol_id = ProtocolId::new("Display");
        let module_id = ModuleId::from_protocol_id(&protocol_id);
        assert_eq!(module_id.name(), "Display");
    }

    #[test]
    fn test_implementation_key_creation() {
        let protocol_id = ProtocolId::new("Display");
        let type_id = TypeId::new("String");
        let args = vec![Type::concrete("Integer")];

        let key = ImplementationKey::new(protocol_id.clone(), type_id.clone(), &args);

        assert_eq!(key.protocol_id, protocol_id);
        assert_eq!(key.implementing_type, type_id);
        assert_eq!(key.type_args_signature, "Integer");
    }

    #[test]
    fn test_implementation_key_with_no_args() {
        let protocol_id = ProtocolId::new("Display");
        let type_id = TypeId::new("String");

        let key = ImplementationKey::new(protocol_id, type_id, &[]);

        assert_eq!(key.type_args_signature, "");
    }

    #[test]
    fn test_implementation_key_with_multiple_args() {
        let protocol_id = ProtocolId::new("Display");
        let type_id = TypeId::new("Map");
        let args = vec![Type::concrete("String"), Type::concrete("Integer")];

        let key = ImplementationKey::new(protocol_id, type_id, &args);

        assert_eq!(key.type_args_signature, "String,Integer");
    }

    #[test]
    fn test_successful_implementation_registration() {
        let mut registry = create_test_registry();

        let result = registry.register_implementation(
            TypeId::new("String"),
            vec![],
            ProtocolId::new("Display"),
            vec![],
            ModuleId::new("String"), // String is local, so this should succeed
            None,
        );

        assert!(result.is_ok());
        assert_eq!(registry.implementation_count(), 1);
        assert!(!registry.is_empty());
    }

    #[test]
    fn test_conflicting_implementation_error() {
        let mut registry = create_test_registry();

        // Register first implementation
        let result1 = registry.register_implementation(
            TypeId::new("String"),
            vec![],
            ProtocolId::new("Display"),
            vec![],
            ModuleId::new("String"),
            None,
        );
        assert!(result1.is_ok());

        // Try to register conflicting implementation
        let result2 = registry.register_implementation(
            TypeId::new("String"),
            vec![],
            ProtocolId::new("Display"),
            vec![],
            ModuleId::new("String"),
            None,
        );

        assert!(result2.is_err());
        match result2.unwrap_err() {
            ImplementationError::ConflictingImplementation {
                protocol_name,
                type_name,
                ..
            } => {
                assert_eq!(protocol_name, "Display");
                assert_eq!(type_name, "String");
            }
            _ => panic!("Expected ConflictingImplementation error"),
        }
    }

    #[test]
    fn test_orphan_rule_violation_both_foreign() {
        let mut registry = ProtocolRegistry::new();

        // Don't add any local modules - both protocol and type will be foreign
        registry.set_current_module(ModuleId::new("TestModule"));

        let result = registry.register_implementation(
            TypeId::new("ForeignType"),
            vec![],
            ProtocolId::new("ForeignProtocol"),
            vec![],
            ModuleId::new("TestModule"), // Even though defining module is "local",
            // both protocol and type are foreign
            None,
        );

        assert!(result.is_err());
        match result.unwrap_err() {
            ImplementationError::OrphanRuleViolation {
                protocol_name,
                type_name,
                ..
            } => {
                assert_eq!(protocol_name, "ForeignProtocol");
                assert_eq!(type_name, "ForeignType");
            }
            _ => panic!("Expected OrphanRuleViolation error"),
        }
    }

    #[test]
    fn test_orphan_rule_success_local_protocol() {
        let mut registry = ProtocolRegistry::new();

        // Add only protocol as local
        registry.add_local_module(ModuleId::new("Display"));
        registry.add_local_module(ModuleId::new("TestModule"));
        registry.set_current_module(ModuleId::new("TestModule"));

        let result = registry.register_implementation(
            TypeId::new("ForeignType"),
            vec![],
            ProtocolId::new("Display"), // Local protocol
            vec![],
            ModuleId::new("TestModule"),
            None,
        );

        assert!(result.is_ok());
    }

    #[test]
    fn test_orphan_rule_success_local_type() {
        let mut registry = ProtocolRegistry::new();

        // Add only type as local
        registry.add_local_module(ModuleId::new("String"));
        registry.add_local_module(ModuleId::new("TestModule"));
        registry.set_current_module(ModuleId::new("TestModule"));

        let result = registry.register_implementation(
            TypeId::new("String"), // Local type
            vec![],
            ProtocolId::new("ForeignProtocol"),
            vec![],
            ModuleId::new("TestModule"),
            None,
        );

        assert!(result.is_ok());
    }

    #[test]
    fn test_orphan_rule_violation_non_local_defining_module() {
        let mut registry = ProtocolRegistry::new();

        // Add protocol and type as local, but not the defining module
        registry.add_local_module(ModuleId::new("Display"));
        registry.add_local_module(ModuleId::new("String"));
        registry.set_current_module(ModuleId::new("TestModule"));

        let result = registry.register_implementation(
            TypeId::new("String"),
            vec![],
            ProtocolId::new("Display"),
            vec![],
            ModuleId::new("ForeignModule"), // Non-local defining module
            None,
        );

        assert!(result.is_err());
        match result.unwrap_err() {
            ImplementationError::OrphanRuleViolation { .. } => {
                // Expected - defining module must be local
            }
            _ => panic!("Expected OrphanRuleViolation error"),
        }
    }

    #[test]
    fn test_has_implementation_simple() {
        let mut registry = create_test_registry();

        registry
            .register_implementation(
                TypeId::new("String"),
                vec![],
                ProtocolId::new("Display"),
                vec![],
                ModuleId::new("String"),
                None,
            )
            .unwrap();

        assert!(registry.has_implementation(&ProtocolId::new("Display"), &TypeId::new("String")));
        assert!(!registry.has_implementation(&ProtocolId::new("Debug"), &TypeId::new("String")));
        assert!(!registry.has_implementation(&ProtocolId::new("Display"), &TypeId::new("Integer")));
    }

    #[test]
    fn test_has_implementation_with_generic_args() {
        let mut registry = create_test_registry();

        let generic_args = vec![Type::concrete("String")];
        registry
            .register_implementation(
                TypeId::new("List"),
                generic_args.clone(),
                ProtocolId::new("Display"),
                vec![],
                ModuleId::new("List"),
                None,
            )
            .unwrap();

        assert!(registry.has_implementation_with_args(
            &ProtocolId::new("Display"),
            &TypeId::new("List"),
            &generic_args
        ));

        // Different generic args should not match
        let different_args = vec![Type::concrete("Integer")];
        assert!(!registry.has_implementation_with_args(
            &ProtocolId::new("Display"),
            &TypeId::new("List"),
            &different_args
        ));
    }

    #[test]
    fn test_get_implementation_info() {
        let mut registry = create_test_registry();

        let implementing_type = TypeId::new("String");
        let protocol_id = ProtocolId::new("Display");
        let defining_module = ModuleId::new("String");

        registry
            .register_implementation(
                implementing_type.clone(),
                vec![],
                protocol_id.clone(),
                vec![],
                defining_module.clone(),
                None,
            )
            .unwrap();

        let info = registry.get_implementation(&protocol_id, &implementing_type, &[]);
        assert!(info.is_some());

        let info = info.unwrap();
        assert_eq!(info.implementing_type, implementing_type);
        assert_eq!(info.protocol_id, protocol_id);
        assert_eq!(info.defining_module, defining_module);
        assert!(info.implementing_args.is_empty());
    }

    #[test]
    fn test_get_protocol_implementations() {
        let mut registry = create_test_registry();

        let protocol_id = ProtocolId::new("Display");

        // Register multiple implementations of Display
        registry
            .register_implementation(
                TypeId::new("String"),
                vec![],
                protocol_id.clone(),
                vec![],
                ModuleId::new("String"),
                None,
            )
            .unwrap();

        registry
            .register_implementation(
                TypeId::new("Integer"),
                vec![],
                protocol_id.clone(),
                vec![],
                ModuleId::new("Integer"),
                None,
            )
            .unwrap();

        let implementations = registry.get_protocol_implementations(&protocol_id);
        assert_eq!(implementations.len(), 2);

        let type_names: std::collections::HashSet<_> = implementations
            .iter()
            .map(|info| info.implementing_type.name())
            .collect();
        assert!(type_names.contains("String"));
        assert!(type_names.contains("Integer"));
    }

    #[test]
    fn test_get_type_implementations() {
        let mut registry = create_test_registry();

        let type_id = TypeId::new("String");

        // Register multiple protocols for String
        registry
            .register_implementation(
                type_id.clone(),
                vec![],
                ProtocolId::new("Display"),
                vec![],
                ModuleId::new("String"),
                None,
            )
            .unwrap();

        registry
            .register_implementation(
                type_id.clone(),
                vec![],
                ProtocolId::new("Debug"),
                vec![],
                ModuleId::new("String"),
                None,
            )
            .unwrap();

        let implementations = registry.get_type_implementations(&type_id);
        assert_eq!(implementations.len(), 2);

        let protocol_names: std::collections::HashSet<_> = implementations
            .iter()
            .map(|info| info.protocol_id.0.clone())
            .collect();
        assert!(protocol_names.contains("Display"));
        assert!(protocol_names.contains("Debug"));
    }

    #[test]
    fn test_protocol_requirement_registration() {
        let mut registry = create_test_registry();
        
        // Register Integer protocol with BinaryAddition requirement
        let integer_protocol = ProtocolId::new("Integer");
        let binary_addition_protocol = ProtocolId::new("BinaryAddition");
        let mut requirements = HashSet::new();
        requirements.insert(binary_addition_protocol.clone());
        
        registry.register_protocol_definition(
            integer_protocol.clone(),
            requirements,
            ModuleId::new("Integer"),
            None,
        );
        
        // Test requirement lookup
        assert!(registry.protocol_requires(&integer_protocol, &binary_addition_protocol));
        assert!(!registry.protocol_requires(&integer_protocol, &ProtocolId::new("Display")));
    }

    #[test]
    fn test_protocol_requirement_lookup() {
        let mut registry = create_test_registry();
        
        // Register protocols with requirements
        let integer_protocol = ProtocolId::new("Integer");
        let binary_addition = ProtocolId::new("BinaryAddition");
        let equality = ProtocolId::new("Equality");
        
        let mut integer_requirements = HashSet::new();
        integer_requirements.insert(binary_addition.clone());
        integer_requirements.insert(equality.clone());
        
        registry.register_protocol_definition(
            integer_protocol.clone(),
            integer_requirements.clone(),
            ModuleId::new("Integer"),
            None,
        );
        
        // Test getting all requirements
        let requirements = registry.get_protocol_requirements(&integer_protocol);
        assert_eq!(requirements, integer_requirements);
        
        // Test non-existent protocol
        let empty_requirements = registry.get_protocol_requirements(&ProtocolId::new("NonExistent"));
        assert!(empty_requirements.is_empty());
    }

    #[test]
    fn test_type_satisfies_protocol_simple() {
        let mut registry = create_test_registry();
        
        // Register Integer64 type implementing Integer protocol
        let integer64_type = TypeId::new("Outrun.Core.Integer64");
        let integer_protocol = ProtocolId::new("Integer");
        
        registry.register_implementation(
            integer64_type.clone(),
            vec![],
            integer_protocol.clone(),
            vec![],
            ModuleId::new("Integer"),
            None,
        ).expect("Implementation should succeed");
        
        // Test simple satisfaction (no requirements)
        assert!(registry.type_satisfies_protocol(&integer64_type, &integer_protocol));
        
        // Test non-existent implementation
        let string_type = TypeId::new("String");
        assert!(!registry.type_satisfies_protocol(&string_type, &integer_protocol));
    }

    #[test]
    fn test_type_satisfies_protocol_with_requirements() {
        let mut registry = create_test_registry();
        
        // Register protocols with requirements
        let integer_protocol = ProtocolId::new("Integer");
        let binary_addition = ProtocolId::new("BinaryAddition");
        let equality = ProtocolId::new("Equality");
        
        let mut integer_requirements = HashSet::new();
        integer_requirements.insert(binary_addition.clone());
        integer_requirements.insert(equality.clone());
        
        registry.register_protocol_definition(
            integer_protocol.clone(),
            integer_requirements,
            ModuleId::new("Integer"),
            None,
        );
        
        // Register type implementations
        let integer64_type = TypeId::new("Outrun.Core.Integer64");
        
        // Register Integer implementation
        registry.register_implementation(
            integer64_type.clone(),
            vec![],
            integer_protocol.clone(),
            vec![],
            ModuleId::new("Integer"),
            None,
        ).expect("Integer implementation should succeed");
        
        // Register BinaryAddition implementation
        registry.register_implementation(
            integer64_type.clone(),
            vec![],
            binary_addition.clone(),
            vec![],
            ModuleId::new("BinaryAddition"),
            None,
        ).expect("BinaryAddition implementation should succeed");
        
        // Register Equality implementation
        registry.register_implementation(
            integer64_type.clone(),
            vec![],
            equality.clone(),
            vec![],
            ModuleId::new("Equality"),
            None,
        ).expect("Equality implementation should succeed");
        
        // Test that type satisfies protocol with all requirements
        assert!(registry.type_satisfies_protocol(&integer64_type, &integer_protocol));
        
        // Test with missing requirement
        let another_type = TypeId::new("AnotherType");
        registry.register_implementation(
            another_type.clone(),
            vec![],
            integer_protocol.clone(),
            vec![],
            ModuleId::new("Integer"),
            None,
        ).expect("Integer implementation should succeed");
        
        // Missing BinaryAddition and Equality implementations
        assert!(!registry.type_satisfies_protocol(&another_type, &integer_protocol));
    }

    #[test]
    fn test_registry_clear() {
        let mut registry = create_test_registry();

        registry
            .register_implementation(
                TypeId::new("String"),
                vec![],
                ProtocolId::new("Display"),
                vec![],
                ModuleId::new("String"),
                None,
            )
            .unwrap();

        assert!(!registry.is_empty());
        assert_eq!(registry.implementation_count(), 1);

        registry.clear();

        assert!(registry.is_empty());
        assert_eq!(registry.implementation_count(), 0);
    }

    #[test]
    fn test_complex_generic_type_implementation() {
        let mut registry = create_test_registry();

        // Register List<T> implementing Display where T: Display
        let generic_args = vec![Type::variable(
            crate::types::TypeVarId(0),
            crate::types::Level(0),
        )];
        registry
            .register_implementation(
                TypeId::new("List"),
                generic_args.clone(),
                ProtocolId::new("Display"),
                vec![],
                ModuleId::new("List"),
                None,
            )
            .unwrap();

        assert!(registry.has_implementation_with_args(
            &ProtocolId::new("Display"),
            &TypeId::new("List"),
            &generic_args
        ));
    }

    #[test]
    fn test_current_module_setting() {
        let mut registry = ProtocolRegistry::new();

        registry.set_current_module(ModuleId::new("TestModule"));

        // The current module tracking is internal, but we can test it indirectly
        // by checking that local modules work correctly
        registry.add_local_module(ModuleId::new("TestModule"));
        registry.add_local_module(ModuleId::new("Display"));

        let result = registry.register_implementation(
            TypeId::new("ForeignType"),
            vec![],
            ProtocolId::new("Display"),
            vec![],
            ModuleId::new("TestModule"),
            None,
        );

        assert!(result.is_ok());
    }
}
