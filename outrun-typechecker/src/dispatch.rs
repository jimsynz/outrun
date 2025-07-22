//! Static function dispatch resolution for Outrun
//!
//! Handles all forms of function dispatch in Outrun's purely functional model:
//! - Static protocol calls: Protocol.function() → concrete implementation
//! - Local scope resolution: unqualified calls → qualified calls based on context
//! - Private function calls: defp functions within modules/implementations

use crate::error::DispatchError;
use crate::registry::ProtocolRegistry;
use crate::types::{ProtocolId, Substitution, Type, TypeId};
use outrun_parser::Span;
use std::collections::HashMap;

/// Function visibility in Outrun
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionVisibility {
    /// Public function (def)
    Public,
    /// Private function (defp)
    Private,
}

/// Context where a function call is being resolved
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionContext {
    /// Inside a protocol definition
    Protocol {
        protocol_id: ProtocolId,
        protocol_args: Vec<Type>,
    },
    /// Inside a struct/module definition
    Module {
        module_id: TypeId,
        module_args: Vec<Type>,
    },
    /// Inside a protocol implementation
    Implementation {
        implementing_type: TypeId,
        implementing_args: Vec<Type>,
        protocol_id: ProtocolId,
        protocol_args: Vec<Type>,
    },
    /// Top-level context (no local scope)
    TopLevel,
}

/// Information about a function that can be called
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    /// Module/protocol where function is defined
    pub defining_scope: String,
    /// Function name
    pub function_name: String,
    /// Function visibility
    pub visibility: FunctionVisibility,
    /// Function signature
    pub parameters: Vec<(String, Type)>,
    /// Return type
    pub return_type: Type,
    /// Source location
    pub span: Option<Span>,
}

/// Result of function resolution
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedFunction {
    /// Fully qualified function name (e.g., "Equality.equal?", "User.validate_email?")
    pub qualified_name: String,
    /// Concrete implementing type for protocol calls
    pub implementing_type: Option<TypeId>,
    /// Function information
    pub function_info: FunctionInfo,
}

/// Function dispatch resolution result
#[derive(Debug, Clone, PartialEq)]
pub enum DispatchResult {
    /// Successfully resolved to a specific function
    Resolved(Box<ResolvedFunction>),
    /// Multiple implementations found (disambiguation needed)
    Ambiguous(Vec<ResolvedFunction>),
    /// No function found
    NotFound,
}

/// Registry for function information across modules
#[derive(Debug, Clone)]
pub struct FunctionRegistry {
    /// Map from (module_name, function_name) to function info
    functions: HashMap<(String, String), FunctionInfo>,
}

impl FunctionRegistry {
    /// Create a new function registry
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Register a function definition
    pub fn register_function(
        &mut self,
        module_name: String,
        function_name: String,
        info: FunctionInfo,
    ) {
        self.functions.insert((module_name, function_name), info);
    }

    /// Get function info by module and name
    pub fn get_function(&self, module_name: &str, function_name: &str) -> Option<&FunctionInfo> {
        self.functions
            .get(&(module_name.to_string(), function_name.to_string()))
    }

    /// Get all functions in a module
    pub fn get_module_functions(&self, module_name: &str) -> Vec<(&String, &FunctionInfo)> {
        self.functions
            .iter()
            .filter_map(|((mod_name, func_name), info)| {
                if mod_name == module_name {
                    Some((func_name, info))
                } else {
                    None
                }
            })
            .collect()
    }
}

impl Default for FunctionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Static function dispatch resolver
#[derive(Debug)]
pub struct FunctionDispatcher<'a> {
    /// Protocol implementation registry
    protocol_registry: &'a ProtocolRegistry,
    /// Function registry
    function_registry: &'a FunctionRegistry,
    /// Current substitution context for type variable resolution
    substitution: Option<&'a Substitution>,
    /// Current function context for local resolution
    context: FunctionContext,
}

impl<'a> FunctionDispatcher<'a> {
    /// Create a new function dispatcher
    pub fn new(
        protocol_registry: &'a ProtocolRegistry,
        function_registry: &'a FunctionRegistry,
    ) -> Self {
        Self {
            protocol_registry,
            function_registry,
            substitution: None,
            context: FunctionContext::TopLevel,
        }
    }

    /// Create dispatcher with substitution context
    pub fn with_substitution(
        protocol_registry: &'a ProtocolRegistry,
        function_registry: &'a FunctionRegistry,
        substitution: &'a Substitution,
    ) -> Self {
        Self {
            protocol_registry,
            function_registry,
            substitution: Some(substitution),
            context: FunctionContext::TopLevel,
        }
    }

    /// Set the current function context for local resolution
    pub fn with_context(mut self, context: FunctionContext) -> Self {
        self.context = context;
        self
    }

    /// Resolve a fully qualified function call (e.g., Protocol.function(), Module.function())
    pub fn resolve_qualified_call(
        &self,
        qualified_name: &str,
        target_type: Option<&Type>,
        span: Option<Span>,
    ) -> Result<DispatchResult, DispatchError> {
        let parts: Vec<&str> = qualified_name.split('.').collect();
        if parts.len() != 2 {
            return Err(DispatchError::InvalidTarget {
                protocol_name: qualified_name.to_string(),
                target_description: "malformed qualified name".to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }

        let module_name = parts[0];
        let function_name = parts[1];

        // Check if this is a protocol call (has target type)
        if let Some(target) = target_type {
            self.resolve_protocol_call(module_name, function_name, target, span)
        } else {
            // Static module function call
            self.resolve_static_call(module_name, function_name, span)
        }
    }

    /// Resolve a local (unqualified) function call based on current context
    pub fn resolve_local_call(
        &self,
        function_name: &str,
        span: Option<Span>,
    ) -> Result<DispatchResult, DispatchError> {
        match &self.context {
            FunctionContext::Protocol { protocol_id, .. } => {
                // Local call in protocol context -> resolve to protocol method
                self.resolve_static_call(&protocol_id.0, function_name, span)
            }
            FunctionContext::Module { module_id, .. } => {
                // Local call in module context -> resolve to module function
                self.resolve_static_call(module_id.name(), function_name, span)
            }
            FunctionContext::Implementation {
                implementing_type,
                protocol_id,
                ..
            } => {
                // Local call in impl context -> try impl functions first, then protocol defaults
                if let Some(func_info) = self.function_registry.get_function(
                    &format!("impl {} for {}", protocol_id.0, implementing_type.name()),
                    function_name,
                ) {
                    // Found in implementation
                    Ok(DispatchResult::Resolved(Box::new(ResolvedFunction {
                        qualified_name: format!("{}.{}", protocol_id.0, function_name),
                        implementing_type: Some(implementing_type.clone()),
                        function_info: func_info.clone(),
                    })))
                } else {
                    // Try protocol default
                    self.resolve_static_call(&protocol_id.0, function_name, span)
                }
            }
            FunctionContext::TopLevel => {
                // No local context - this is an error
                Err(DispatchError::NoImplementation {
                    protocol_name: "unknown".to_string(),
                    type_name: function_name.to_string(),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                })
            }
        }
    }

    /// Resolve a protocol call with type-based dispatch
    fn resolve_protocol_call(
        &self,
        protocol_name: &str,
        function_name: &str,
        target_type: &Type,
        span: Option<Span>,
    ) -> Result<DispatchResult, DispatchError> {
        // Apply substitutions to resolve type variables
        let resolved_type = if let Some(subst) = self.substitution {
            subst.apply(target_type)
        } else {
            target_type.clone()
        };

        match resolved_type {
            Type::Concrete { id, ref args, .. } => {
                // Look up protocol implementation for this concrete type
                let protocol_id = ProtocolId::new(protocol_name);
                if let Some(_impl_info) =
                    self.protocol_registry
                        .get_implementation(&protocol_id, &id, args)
                {
                    // Found implementation - look up the specific function
                    let impl_scope = format!("impl {} for {}", protocol_name, id.name());
                    if let Some(func_info) = self
                        .function_registry
                        .get_function(&impl_scope, function_name)
                    {
                        Ok(DispatchResult::Resolved(Box::new(ResolvedFunction {
                            qualified_name: format!("{protocol_name}.{function_name}"),
                            implementing_type: Some(id),
                            function_info: func_info.clone(),
                        })))
                    } else {
                        // Try protocol default
                        self.resolve_static_call(protocol_name, function_name, span)
                    }
                } else {
                    Err(DispatchError::NoImplementation {
                        protocol_name: protocol_name.to_string(),
                        type_name: id.name().to_string(),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    })
                }
            }
            Type::Variable { .. } => Err(DispatchError::UnresolvedTypeVariable {
                protocol_name: protocol_name.to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            }),
            Type::SelfType { .. } => match resolved_type.resolve_self() {
                Some(concrete_type) => {
                    self.resolve_protocol_call(protocol_name, function_name, &concrete_type, span)
                }
                None => Err(DispatchError::UnboundSelfType {
                    protocol_name: protocol_name.to_string(),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                }),
            },
            _ => {
                // Other type categories (Protocol, Function) handled similarly
                Err(DispatchError::InvalidTarget {
                    protocol_name: protocol_name.to_string(),
                    target_description: format!("{resolved_type}"),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                })
            }
        }
    }

    /// Resolve a static function call (no type-based dispatch)
    fn resolve_static_call(
        &self,
        module_name: &str,
        function_name: &str,
        span: Option<Span>,
    ) -> Result<DispatchResult, DispatchError> {
        if let Some(func_info) = self
            .function_registry
            .get_function(module_name, function_name)
        {
            // Check visibility - private functions only accessible within same module
            if func_info.visibility == FunctionVisibility::Private
                && !self.is_local_access(module_name)
            {
                return Err(DispatchError::NoImplementation {
                    protocol_name: module_name.to_string(),
                    type_name: function_name.to_string(),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                });
            }

            Ok(DispatchResult::Resolved(Box::new(ResolvedFunction {
                qualified_name: format!("{module_name}.{function_name}"),
                implementing_type: None,
                function_info: func_info.clone(),
            })))
        } else {
            Err(DispatchError::NoImplementation {
                protocol_name: module_name.to_string(),
                type_name: function_name.to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            })
        }
    }

    /// Check if we have local access to a module (for private function visibility)
    fn is_local_access(&self, module_name: &str) -> bool {
        match &self.context {
            FunctionContext::Protocol { protocol_id, .. } => protocol_id.0 == module_name,
            FunctionContext::Module { module_id, .. } => module_id.name() == module_name,
            FunctionContext::Implementation {
                implementing_type,
                protocol_id,
                ..
            } => module_name == protocol_id.0 || module_name == implementing_type.name(),
            FunctionContext::TopLevel => false,
        }
    }
}

/// Dispatch table for efficient runtime dispatch
#[derive(Debug, Clone)]
pub struct DispatchTable {
    /// Map from (protocol, type) to resolved function
    entries: HashMap<(String, String), ResolvedFunction>,
}

impl DispatchTable {
    /// Create a new empty dispatch table
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    /// Add a dispatch entry
    pub fn add_entry(
        &mut self,
        protocol_name: String,
        type_name: String,
        resolved_func: ResolvedFunction,
    ) {
        self.entries
            .insert((protocol_name, type_name), resolved_func);
    }

    /// Look up a dispatch target
    pub fn lookup(&self, protocol_name: &str, type_name: &str) -> Option<&ResolvedFunction> {
        self.entries
            .get(&(protocol_name.to_string(), type_name.to_string()))
    }

    /// Get all entries in the dispatch table
    pub fn entries(&self) -> &HashMap<(String, String), ResolvedFunction> {
        &self.entries
    }

    /// Check if table is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Get number of entries
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Clear all entries
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

impl Default for DispatchTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Build a complete dispatch table from protocol and function registries
pub fn build_dispatch_table(
    protocol_registry: &ProtocolRegistry,
    function_registry: &FunctionRegistry,
) -> DispatchTable {
    let mut table = DispatchTable::new();

    // Process all registered implementations
    for impl_info in protocol_registry.all_implementations() {
        let impl_scope = format!(
            "impl {} for {}",
            impl_info.protocol_id.0,
            impl_info.implementing_type.name()
        );

        // Get all functions in this implementation
        for (func_name, func_info) in function_registry.get_module_functions(&impl_scope) {
            let resolved_func = ResolvedFunction {
                qualified_name: format!("{}.{}", impl_info.protocol_id.0, func_name),
                implementing_type: Some(impl_info.implementing_type.clone()),
                function_info: func_info.clone(),
            };

            table.add_entry(
                impl_info.protocol_id.0.clone(),
                impl_info.implementing_type.name().to_string(),
                resolved_func,
            );
        }
    }

    table
}

// Legacy type aliases for backward compatibility during transition
pub type DispatchResolver<'a> = FunctionDispatcher<'a>;
pub type DispatchTarget = ResolvedFunction;
