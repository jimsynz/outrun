//! Static function dispatch resolution for Outrun
//!
//! Handles all forms of function dispatch in Outrun's purely functional model:
//! - Static protocol calls: Protocol.function() → concrete implementation
//! - Local scope resolution: unqualified calls → qualified calls based on context
//! - Private function calls: defp functions within modules/implementations

use crate::error::DispatchError;
use crate::registry::TypeRegistry;
use crate::types::{ModuleName, Substitution, Type};
use outrun_parser::Span;
use std::collections::HashMap;

/// Result of merging function registries with conflict information
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionMergeResult {
    pub added_functions: usize,
    pub identical_functions: usize,
    pub conflicts: Vec<FunctionConflict>,
}

impl FunctionMergeResult {
    pub fn new() -> Self {
        Self {
            added_functions: 0,
            identical_functions: 0,
            conflicts: Vec::new(),
        }
    }

    pub fn has_conflicts(&self) -> bool {
        !self.conflicts.is_empty()
    }
}

impl Default for FunctionMergeResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Information about a function conflict during merging
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionConflict {
    pub module_name: String,
    pub function_name: String,
    pub resolution: ConflictResolution,
}

/// How a conflict was resolved
#[derive(Debug, Clone, PartialEq)]
pub enum ConflictResolution {
    LocalTakesPrecedence,
    DependencyTakesPrecedence,
    Error,
}

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
        protocol_name: ModuleName,
        protocol_args: Vec<Type>,
    },
    /// Inside a struct/module definition
    Module {
        module_name: ModuleName,
        module_args: Vec<Type>,
    },
    /// Inside a protocol implementation
    Implementation {
        implementing_type: ModuleName,
        implementing_args: Vec<Type>,
        protocol_name: ModuleName,
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
    /// Function body block (for user-defined functions)
    pub body: Option<outrun_parser::Block>,
    /// Source location
    pub span: Option<Span>,
    /// Generic parameters for this function (e.g., ["T", "U"] for generic functions)
    pub generic_parameters: Vec<String>,
    /// Whether this function is generic (has type parameters)
    pub is_generic: bool,
}

/// Result of function resolution
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedFunction {
    /// Fully qualified function name (e.g., "Equality.equal?", "User.validate_email?")
    pub qualified_name: String,
    /// Concrete implementing type for protocol calls
    pub implementing_type: Option<ModuleName>,
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

    /// Merge another function registry into this one with proper precedence
    /// Used for composing packages with pre-compiled dependencies
    ///
    /// Merging Strategy:
    /// - Current package functions take precedence over dependencies
    /// - Dependencies only add functions that don't exist locally
    /// - This preserves local overrides and customizations
    pub fn merge_from_dependency(&mut self, dependency_registry: &FunctionRegistry) {
        for ((module_name, function_name), function_info) in &dependency_registry.functions {
            let key = (module_name.clone(), function_name.clone());

            // Only add if we don't already have this function
            // This gives current package precedence over dependencies
            self.functions
                .entry(key)
                .or_insert_with(|| function_info.clone());
            // If we already have the function, keep our version (local precedence)
        }
    }

    /// Merge with intelligent conflict detection and reporting
    /// Returns information about conflicts and merging results
    pub fn merge_with_conflict_detection(
        &mut self,
        dependency_registry: &FunctionRegistry,
    ) -> FunctionMergeResult {
        let mut result = FunctionMergeResult::new();

        for ((module_name, function_name), function_info) in &dependency_registry.functions {
            let key = (module_name.clone(), function_name.clone());

            match self.functions.get(&key) {
                Some(existing_info) => {
                    if self.functions_are_identical(existing_info, function_info) {
                        // Identical functions - no conflict
                        result.identical_functions += 1;
                    } else {
                        // Different implementations - local takes precedence
                        result.conflicts.push(FunctionConflict {
                            module_name: module_name.clone(),
                            function_name: function_name.clone(),
                            resolution: ConflictResolution::LocalTakesPrecedence,
                        });
                    }
                }
                None => {
                    // New function from dependency
                    self.functions.insert(key, function_info.clone());
                    result.added_functions += 1;
                }
            }
        }

        result
    }

    /// Check if two function infos are identical
    fn functions_are_identical(&self, func1: &FunctionInfo, func2: &FunctionInfo) -> bool {
        // For now, use structural equality
        // In the future, we might want more sophisticated comparison
        func1 == func2
    }

    /// Get total number of registered functions
    pub fn function_count(&self) -> usize {
        self.functions.len()
    }

    /// Check if registry is empty
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }

    /// Get an iterator over all functions (for debug and inspection)
    pub fn all_functions(&self) -> impl Iterator<Item = (&(String, String), &FunctionInfo)> {
        self.functions.iter()
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
    /// Unified type registry (protocols, structs, implementations)
    type_registry: &'a TypeRegistry,
    /// Function registry
    function_registry: &'a FunctionRegistry,
    /// Monomorphisation table for generic function instantiations
    monomorphisation_table: Option<&'a MonomorphisationTable>,
    /// Current substitution context for type variable resolution
    substitution: Option<&'a Substitution>,
    /// Current function context for local resolution
    context: FunctionContext,
    /// Current file context for error reporting
    current_file: Option<String>,
}

impl<'a> FunctionDispatcher<'a> {
    /// Create a new function dispatcher
    pub fn new(
        type_registry: &'a TypeRegistry,
        function_registry: &'a FunctionRegistry,
        monomorphisation_table: Option<&'a MonomorphisationTable>,
        substitution: Option<&'a Substitution>,
    ) -> Self {
        Self {
            type_registry,
            function_registry,
            monomorphisation_table,
            substitution,
            context: FunctionContext::TopLevel,
            current_file: None,
        }
    }

    /// Set the current function context for local resolution
    pub fn with_context(mut self, context: FunctionContext) -> Self {
        self.context = context;
        self
    }

    /// Set the current file context for error reporting
    pub fn with_file_context(mut self, file: Option<String>) -> Self {
        self.current_file = file;
        self
    }

    /// Create a FileSpan using current file context
    fn create_file_span(&self, span: Option<Span>) -> crate::error::FileSpan {
        crate::error::FileSpan {
            span: span.unwrap_or(Span {
                start: 0,
                end: 0,
                start_line_col: None,
                end_line_col: None,
            }),
            source_file: self
                .current_file
                .clone()
                .unwrap_or_else(|| "unknown".to_string()),
        }
    }

    /// Resolve a fully qualified function call (e.g., Protocol.function(), Module.function())
    pub fn resolve_qualified_call(
        &self,
        qualified_name: &str,
        target_type: Option<&Type>,
        span: Option<Span>,
    ) -> Result<DispatchResult, DispatchError> {
        // Split qualified name: everything before last dot is module, everything after is function
        let last_dot_pos = qualified_name.rfind('.');
        if let Some(pos) = last_dot_pos {
            let module_name = &qualified_name[..pos];
            let function_name = &qualified_name[pos + 1..];

            // Ensure we have both parts
            if module_name.is_empty() || function_name.is_empty() {
                return Err(DispatchError::InvalidTarget {
                    protocol_name: qualified_name.to_string(),
                    target_description: "malformed qualified name".to_string(),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                });
            }

            // Check if this is a protocol call (has target type)
            if let Some(target) = target_type {
                self.resolve_protocol_call(module_name, function_name, target, span)
            } else {
                self.resolve_static_call(module_name, function_name, span)
            }
        } else {
            // No dot found - invalid qualified name
            return Err(DispatchError::InvalidTarget {
                protocol_name: qualified_name.to_string(),
                target_description: "malformed qualified name".to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }
    }

    /// Resolve a local (unqualified) function call based on current context
    pub fn resolve_local_call(
        &self,
        function_name: &str,
        span: Option<Span>,
    ) -> Result<DispatchResult, DispatchError> {
        match &self.context {
            FunctionContext::Protocol { protocol_name, .. } => {
                // Local call in protocol context -> resolve to protocol method
                self.resolve_static_call(protocol_name.as_str(), function_name, span)
            }
            FunctionContext::Module { module_name, .. } => {
                // Local call in module context -> resolve to module function
                self.resolve_static_call(module_name.as_str(), function_name, span)
            }
            FunctionContext::Implementation {
                implementing_type,
                protocol_name,
                ..
            } => {
                // Local call in impl context -> try impl functions first, then protocol defaults
                if let Some(func_info) = self.function_registry.get_function(
                    &format!(
                        "impl {} for {}",
                        protocol_name.as_str(),
                        implementing_type.as_str()
                    ),
                    function_name,
                ) {
                    // Found in implementation
                    Ok(DispatchResult::Resolved(Box::new(ResolvedFunction {
                        qualified_name: format!(
                            "{}.{}:{}",
                            protocol_name.as_str(),
                            function_name,
                            implementing_type.as_str()
                        ),
                        implementing_type: Some(implementing_type.clone()),
                        function_info: func_info.clone(),
                    })))
                } else {
                    // Try protocol default
                    self.resolve_static_call(protocol_name.as_str(), function_name, span)
                }
            }
            FunctionContext::TopLevel => {
                // No local context - this is an error
                Err(DispatchError::NoImplementation {
                    protocol_name: "unknown".to_string(),
                    type_name: function_name.to_string(),
                    file_span: Some(self.create_file_span(span)),
                    similar_implementations: Vec::new(),
                    suggestions: Vec::new(),
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
            Type::Concrete { name, ref args, .. } => {
                // Look up protocol implementation for this concrete type
                // For generic types, we need to try both generic and non-generic protocol names

                // First try exact match with current protocol name
                let mut impl_info = self
                    .type_registry
                    .get_implementation(name.as_str(), protocol_name);

                // If not found and we have args, also try with empty args (for generic implementations registered without args)
                if impl_info.is_none() && !args.is_empty() {
                    impl_info = self
                        .type_registry
                        .get_implementation(name.as_str(), protocol_name);
                }

                // If not found and we have generic args, try constructing generic protocol name
                if impl_info.is_none() && !args.is_empty() {
                    let generic_args_str = args
                        .iter()
                        .map(|arg| match arg {
                            Type::Variable {
                                name: Some(name), ..
                            } => name.clone(),
                            Type::Variable { var_id, .. } => format!("T{}", var_id.0),
                            other => format!("{}", other),
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    let generic_protocol_name = format!("{}<{}>", protocol_name, generic_args_str);
                    let generic_protocol_id = ModuleName::new(&generic_protocol_name);

                    // Debug: Show the exact key we're looking for with generic protocol name
                    // Implementation lookup: protocol="{}", type="{}", args={:?}
                    let _debug_info = (generic_protocol_id.clone(), name.clone(), args);

                    impl_info = self
                        .type_registry
                        .get_implementation(name.as_str(), protocol_name);
                }

                if let Some(_impl_info) = impl_info {
                    // Found implementation - look up the specific function using impl scope format
                    let impl_scope = format!("impl {} for {}", protocol_name, name.as_str());
                    if let Some(func_info) = self
                        .function_registry
                        .get_function(&impl_scope, function_name)
                    {
                        Ok(DispatchResult::Resolved(Box::new(ResolvedFunction {
                            qualified_name: format!(
                                "{protocol_name}.{function_name}:{}",
                                name.as_str()
                            ),
                            implementing_type: Some(name.clone()),
                            function_info: func_info.clone(),
                        })))
                    } else {
                        // Try protocol default
                        self.resolve_static_call(protocol_name, function_name, span)
                    }
                } else {
                    // Check if this is a type variable that looks like a concrete type
                    // Type variables like "T", "U", "K", "V", etc. can be constrained
                    if self.is_type_variable_name(name.as_str()) {
                        // This is likely a constrained type variable
                        // For now, allow the call and assume constraints will be validated elsewhere
                        // TODO: Implement proper constraint checking here
                        return self.resolve_static_call(protocol_name, function_name, span);
                    }

                    // Debug: List all available implementations
                    for impl_info in self.type_registry.all_implementations() {
                        // Show details for List implementations
                        if impl_info.protocol_name.as_str().contains("List") {}
                    }

                    // Debug: Show the exact key we're looking for
                    // Implementation lookup: protocol="{}", type="{}", args={:?}
                    let _debug_info = (protocol_name, name.clone(), args);
                    // Add debug info about what implementations are actually available
                    Err(DispatchError::NoImplementation {
                        protocol_name: protocol_name.to_string(),
                        type_name: name.as_str().to_string(),
                        file_span: Some(self.create_file_span(span)),
                        similar_implementations: Vec::new(),
                        suggestions: Vec::new(),
                    })
                }
            }
            Type::Variable { .. } => Err(DispatchError::UnresolvedTypeVariable {
                protocol_name: protocol_name.to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            }),
            Type::SelfType { .. } => {
                // For protocol functions with Self parameters, try to resolve Self first
                match resolved_type.resolve_self() {
                    Some(concrete_type) => {
                        // Self resolved to a concrete type - proceed with normal resolution
                        self.resolve_protocol_call(
                            protocol_name,
                            function_name,
                            &concrete_type,
                            span,
                        )
                    }
                    None => {
                        // Self is unbound (protocol definition context) - this is valid for protocol functions
                        // Return an abstract protocol result that will be resolved later when called
                        self.resolve_static_call(protocol_name, function_name, span)
                    }
                }
            }
            Type::Protocol {
                name: source_protocol_name,
                args: _,
                ..
            } => {
                // Protocol type - find ALL concrete implementations that satisfy both constraints
                let target_protocol_id = ModuleName::new(protocol_name);

                // Check if the source protocol requires the target protocol
                if self
                    .type_registry
                    .protocol_requires(&source_protocol_name, &target_protocol_id)
                {
                    // Find all concrete types that implement both protocols
                    let mut matching_implementations = Vec::new();

                    // Get all implementations of the source protocol
                    for impl_info in self.type_registry.all_implementations() {
                        // Check if this implementation is for our source protocol
                        if impl_info.protocol_name == source_protocol_name {
                            let implementing_type = &impl_info.implementing_type;

                            // Check if this type also implements the target protocol
                            if self
                                .type_registry
                                .get_implementation(implementing_type.as_str(), protocol_name)
                                .is_some()
                            {
                                // Try to resolve the function for this concrete type
                                let concrete_type = Type::concrete(implementing_type.as_str());

                                // Use a nested dispatcher to avoid recursion issues
                                let nested_dispatcher = FunctionDispatcher::new(
                                    self.type_registry,
                                    self.function_registry,
                                    self.monomorphisation_table,
                                    self.substitution,
                                );

                                if let Ok(DispatchResult::Resolved(resolved_func)) =
                                    nested_dispatcher.resolve_protocol_call(
                                        protocol_name,
                                        function_name,
                                        &concrete_type,
                                        span,
                                    )
                                {
                                    matching_implementations.push(*resolved_func);
                                }
                            }
                        }
                    }

                    if !matching_implementations.is_empty() {
                        if matching_implementations.len() == 1 {
                            return Ok(DispatchResult::Resolved(Box::new(
                                matching_implementations.into_iter().next().unwrap(),
                            )));
                        } else {
                            return Ok(DispatchResult::Ambiguous(matching_implementations));
                        }
                    }
                }

                // Direct match: the protocol itself (e.g., Integer calling Integer.method)
                if source_protocol_name.as_str() == protocol_name {
                    self.resolve_static_call(protocol_name, function_name, span)
                }
                // Protocol requirement match but no implementations found
                else if self
                    .type_registry
                    .protocol_requires(&source_protocol_name, &target_protocol_id)
                {
                    // The protocol type satisfies the requirement but we found no implementations
                    self.resolve_static_call(protocol_name, function_name, span)
                } else {
                    Err(DispatchError::InvalidTarget {
                        protocol_name: protocol_name.to_string(),
                        target_description: format!(
                            "Protocol mismatch: {} does not require {}",
                            source_protocol_name.as_str(),
                            protocol_name
                        ),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    })
                }
            }
            _ => {
                // Other type categories (Function) handled similarly
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
                    file_span: Some(self.create_file_span(span)),
                    similar_implementations: Vec::new(),
                    suggestions: Vec::new(),
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
                file_span: Some(self.create_file_span(span)),
                similar_implementations: Vec::new(),
                suggestions: Vec::new(),
            })
        }
    }

    /// Resolve a generic function call with concrete type arguments
    pub fn resolve_generic_call(
        &self,
        module_name: &str,
        function_name: &str,
        type_args: &[&Type],
        span: Option<Span>,
    ) -> Result<DispatchResult, DispatchError> {
        // Check if we have a monomorphisation table available
        if let Some(mono_table) = self.monomorphisation_table {
            // Generate the monomorphised key for this function call
            let mono_key =
                MonomorphisationTable::generate_key(module_name, function_name, type_args);

            // Try to find an existing monomorphisation
            if let Some(mono_entry) = mono_table.get_instantiation(&mono_key) {
                // Found a monomorphised version - return it
                return Ok(DispatchResult::Resolved(Box::new(ResolvedFunction {
                    qualified_name: format!("{}.{}", module_name, function_name),
                    implementing_type: None,
                    function_info: mono_entry.monomorphised_function.clone(),
                })));
            }
        }

        // Fall back to generic function resolution if no monomorphisation found
        // First, try to find the generic function
        if let Some(func_info) = self
            .function_registry
            .get_function(module_name, function_name)
        {
            if func_info.is_generic {
                // This is a generic function but we don't have a monomorphised version
                // This could happen during type inference when we haven't generated the instantiation yet
                // Return the generic function info with a note that it needs monomorphisation
                return Ok(DispatchResult::Resolved(Box::new(ResolvedFunction {
                    qualified_name: format!("{}.{}", module_name, function_name),
                    implementing_type: None,
                    function_info: func_info.clone(),
                })));
            }
        }

        // If we get here, either the function doesn't exist or it's not generic
        // Fall back to standard static call resolution
        self.resolve_static_call(module_name, function_name, span)
    }

    /// Resolve a qualified call with potential generic type arguments
    /// This is an enhanced version that can handle both generic and non-generic functions
    pub fn resolve_qualified_call_with_types(
        &self,
        qualified_name: &str,
        type_args: &[&Type],
        target_type: Option<&Type>,
        span: Option<Span>,
    ) -> Result<DispatchResult, DispatchError> {
        // Split qualified name: everything before last dot is module, everything after is function
        let last_dot_pos = qualified_name.rfind('.');
        if let Some(pos) = last_dot_pos {
            let module_name = &qualified_name[..pos];
            let function_name = &qualified_name[pos + 1..];

            // Ensure we have both parts
            if module_name.is_empty() || function_name.is_empty() {
                return Err(DispatchError::InvalidTarget {
                    protocol_name: qualified_name.to_string(),
                    target_description: "malformed qualified name".to_string(),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                });
            }

            // If we have type arguments, try generic resolution first
            if !type_args.is_empty() {
                match self.resolve_generic_call(module_name, function_name, type_args, span) {
                    Ok(result) => return Ok(result),
                    Err(_) => {
                        // Fall through to normal resolution if generic resolution fails
                    }
                }
            }

            // Check if this is a protocol call (has target type)
            if let Some(target) = target_type {
                self.resolve_protocol_call(module_name, function_name, target, span)
            } else {
                self.resolve_static_call(module_name, function_name, span)
            }
        } else {
            // No dot found - invalid qualified name
            return Err(DispatchError::InvalidTarget {
                protocol_name: qualified_name.to_string(),
                target_description: "malformed qualified name".to_string(),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }
    }

    /// Check if we have local access to a module (for private function visibility)
    fn is_local_access(&self, module_name: &str) -> bool {
        match &self.context {
            FunctionContext::Protocol { protocol_name, .. } => {
                protocol_name.as_str() == module_name
            }
            FunctionContext::Module {
                module_name: ctx_module_name,
                ..
            } => ctx_module_name.as_str() == module_name,
            FunctionContext::Implementation {
                implementing_type,
                protocol_name,
                ..
            } => module_name == protocol_name.as_str() || module_name == implementing_type.as_str(),
            FunctionContext::TopLevel => false,
        }
    }

    /// Check if a name looks like a type variable name
    /// Type variables: T, U, A, B, TKey, TValue, etc.
    fn is_type_variable_name(&self, name: &str) -> bool {
        // Single uppercase letters (T, U, K, V, etc.)
        if name.len() == 1 && name.chars().next().unwrap().is_uppercase() {
            return true;
        }

        // T-prefixed names (T1, TKey, TValue, etc.)
        if name.starts_with('T') && name.len() > 1 && name.chars().nth(1).unwrap().is_uppercase() {
            return true;
        }

        // Common type parameter conventions
        matches!(
            name,
            "Key" | "Value" | "Input" | "Output" | "Result" | "Error" | "Element"
        )
    }
}

/// Dispatch table for efficient runtime dispatch
#[derive(Debug, Clone)]
pub struct DispatchTable {
    /// Map from "Protocol.function:TargetType" to resolved function
    entries: HashMap<String, ResolvedFunction>,
}

impl DispatchTable {
    /// Create a new empty dispatch table
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    /// Add a dispatch entry with monomorphised key format
    pub fn add_entry(&mut self, key: String, resolved_func: ResolvedFunction) {
        self.entries.insert(key, resolved_func);
    }

    /// Look up a dispatch target using monomorphised key
    pub fn lookup(&self, key: &str) -> Option<&ResolvedFunction> {
        self.entries.get(key)
    }

    /// Get all entries in the dispatch table
    pub fn entries(&self) -> &HashMap<String, ResolvedFunction> {
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

/// Monomorphisation entry for tracking generic function instantiations
#[derive(Debug, Clone, PartialEq)]
pub struct MonomorphisationEntry {
    /// Original generic function info
    pub generic_function: FunctionInfo,
    /// Concrete type substitutions for each generic parameter
    /// Maps generic parameter name (e.g., "T") to concrete type (e.g., "Integer64")
    pub type_substitutions: HashMap<String, Type>,
    /// Monomorphised key for dispatch table lookup
    /// Format: "ModuleName.function_name:Type1:Type2" for multi-parameter generics
    pub monomorphised_key: String,
    /// Monomorphised function info with concrete types substituted
    pub monomorphised_function: FunctionInfo,
}

/// Registry for tracking generic function instantiations and monomorphisation
#[derive(Debug, Clone)]
pub struct MonomorphisationTable {
    /// Map from monomorphised key to instantiation entry
    entries: HashMap<String, MonomorphisationEntry>,
    /// Map from original function signature to all instantiations
    instantiations_by_function: HashMap<(String, String), Vec<String>>, // (module, function) -> keys
}

impl MonomorphisationTable {
    /// Create a new empty monomorphisation table
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            instantiations_by_function: HashMap::new(),
        }
    }

    /// Add a monomorphisation entry for a generic function instantiation
    pub fn add_instantiation(&mut self, entry: MonomorphisationEntry) {
        let key = entry.monomorphised_key.clone();
        let function_key = (
            entry.generic_function.defining_scope.clone(),
            entry.generic_function.function_name.clone(),
        );

        // Add to main entries table
        self.entries.insert(key.clone(), entry);

        // Track instantiation by original function
        self.instantiations_by_function
            .entry(function_key)
            .or_default()
            .push(key);
    }

    /// Look up a monomorphised function by key
    pub fn get_instantiation(&self, key: &str) -> Option<&MonomorphisationEntry> {
        self.entries.get(key)
    }

    /// Get all instantiations for a specific function
    pub fn get_function_instantiations(
        &self,
        module: &str,
        function: &str,
    ) -> Vec<&MonomorphisationEntry> {
        if let Some(keys) = self
            .instantiations_by_function
            .get(&(module.to_string(), function.to_string()))
        {
            keys.iter()
                .filter_map(|key| self.entries.get(key))
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Check if a specific instantiation exists
    pub fn has_instantiation(&self, key: &str) -> bool {
        self.entries.contains_key(key)
    }

    /// Get total number of instantiations
    pub fn instantiation_count(&self) -> usize {
        self.entries.len()
    }

    /// Check if table is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Get all entries (for debug and inspection)
    pub fn all_entries(&self) -> impl Iterator<Item = (&String, &MonomorphisationEntry)> {
        self.entries.iter()
    }

    /// Generate monomorphised key for generic function call
    pub fn generate_key(module: &str, function: &str, type_args: &[&Type]) -> String {
        if type_args.is_empty() {
            format!("{}.{}", module, function)
        } else {
            let type_names: Vec<String> = type_args
                .iter()
                .map(|t| Self::type_to_key_component(t))
                .collect();
            format!("{}.{}:{}", module, function, type_names.join(":"))
        }
    }

    /// Convert a type to a string component for monomorphised keys
    fn type_to_key_component(ty: &Type) -> String {
        match ty {
            Type::Concrete { name, args, .. } => {
                if args.is_empty() {
                    name.as_str().to_string()
                } else {
                    let arg_names: Vec<String> =
                        args.iter().map(Self::type_to_key_component).collect();
                    format!("{}_{}", name.as_str(), arg_names.join("_"))
                }
            }
            Type::Protocol { name, args, .. } => {
                if args.is_empty() {
                    name.as_str().to_string()
                } else {
                    let arg_names: Vec<String> =
                        args.iter().map(Self::type_to_key_component).collect();
                    format!("{}_{}", name.as_str(), arg_names.join("_"))
                }
            }
            Type::Variable { .. } => "Var".to_string(), // Should not appear in monomorphised keys
            Type::SelfType { .. } => "Self".to_string(), // Should be resolved before monomorphisation
            Type::Function { .. } => "Function".to_string(), // Function types in generic args
            Type::Tuple { element_types, .. } => {
                let element_keys: Vec<String> = element_types
                    .iter()
                    .map(Self::type_to_key_component)
                    .collect();
                format!("Tuple_{}", element_keys.join("_"))
            }
        }
    }

    /// Create monomorphised function info by substituting generic parameters
    pub fn create_monomorphised_function(
        generic_function: &FunctionInfo,
        type_substitutions: &HashMap<String, Type>,
    ) -> Result<FunctionInfo, crate::error::TypecheckError> {
        let mut substituted_parameters = Vec::new();
        for (name, ty) in &generic_function.parameters {
            substituted_parameters.push((
                name.clone(),
                ty.substitute_type_parameters(type_substitutions, false)?,
            ));
        }

        Ok(FunctionInfo {
            defining_scope: generic_function.defining_scope.clone(),
            function_name: generic_function.function_name.clone(),
            visibility: generic_function.visibility,
            parameters: substituted_parameters,
            return_type: generic_function
                .return_type
                .substitute_type_parameters(type_substitutions, false)?,
            body: generic_function.body.clone(),
            span: generic_function.span,
            generic_parameters: Vec::new(), // Monomorphised functions are not generic
            is_generic: false,
        })
    }

    /// Create monomorphised protocol function info by substituting Self with concrete type
    pub fn create_monomorphised_protocol_function(
        protocol_function: &FunctionInfo,
        self_type: &Type,
    ) -> Result<FunctionInfo, crate::error::TypecheckError> {
        let mut substitutions = HashMap::new();
        substitutions.insert("Self".to_string(), self_type.clone());

        let mut substituted_parameters = Vec::new();
        for (name, ty) in &protocol_function.parameters {
            substituted_parameters.push((
                name.clone(),
                ty.substitute_type_parameters(&substitutions, true)?,
            ));
        }

        Ok(FunctionInfo {
            defining_scope: protocol_function.defining_scope.clone(),
            function_name: protocol_function.function_name.clone(),
            visibility: protocol_function.visibility,
            parameters: substituted_parameters,
            return_type: protocol_function
                .return_type
                .substitute_type_parameters(&substitutions, true)?,
            body: protocol_function.body.clone(),
            span: protocol_function.span,
            generic_parameters: Vec::new(), // Monomorphised functions are not generic
            is_generic: false,
        })
    }
}

impl Default for MonomorphisationTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Build a complete dispatch table from protocol and function registries with monomorphisation support
pub fn build_dispatch_table(
    type_registry: &TypeRegistry,
    function_registry: &FunctionRegistry,
    monomorphisation_table: Option<&MonomorphisationTable>,
) -> DispatchTable {
    let mut table = DispatchTable::new();

    // Debug: Check what implementations we have (can be removed in production)
    // println!("🔧 Type registry has {} implementations", type_registry.all_implementations().count());

    // Process all registered implementations
    for impl_info in type_registry.all_implementations() {
        let impl_scope = format!(
            "impl {} for {}",
            impl_info.protocol_name.as_str(),
            impl_info.implementing_type.as_str()
        );

        // Debug output (can be removed in production)
        // println!("🔧 Processing implementation: {}", impl_scope);

        // Get all functions in this implementation (used in the loop below)
        // let functions = function_registry.get_module_functions(&impl_scope);
        // println!("🔧 Found {} functions in implementation", functions.len());

        for (func_name, func_info) in function_registry.get_module_functions(&impl_scope) {
            let resolved_func = ResolvedFunction {
                qualified_name: format!("{}.{}", impl_info.protocol_name.as_str(), func_name),
                implementing_type: Some(impl_info.implementing_type.clone()),
                function_info: func_info.clone(),
            };

            // Generate monomorphised key: "Protocol.function:TargetType"
            let monomorphised_key = format!(
                "{}.{}:{}",
                impl_info.protocol_name.as_str(),
                func_name,
                impl_info.implementing_type.as_str()
            );

            // Debug output (can be removed in production)
            // println!("🔧 Adding dispatch entry: {} -> {} (has_body: {})",
            //     monomorphised_key,
            //     resolved_func.qualified_name,
            //     resolved_func.function_info.body.is_some()
            // );

            table.add_entry(monomorphised_key, resolved_func);
        }
    }

    // Add monomorphised function entries if available
    if let Some(mono_table) = monomorphisation_table {
        for (mono_key, mono_entry) in mono_table.all_entries() {
            let resolved_func = ResolvedFunction {
                qualified_name: format!(
                    "{}.{}",
                    mono_entry.generic_function.defining_scope,
                    mono_entry.generic_function.function_name
                ),
                implementing_type: None, // Monomorphised struct functions don't have implementing types
                function_info: mono_entry.monomorphised_function.clone(),
            };

            // Add the monomorphised function to the dispatch table
            table.add_entry(mono_key.clone(), resolved_func);
        }
    }

    // Debug output (can be removed in production)
    // println!("🔧 Final dispatch table has {} entries", table.entries().len());
    table
}
