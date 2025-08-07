//! Type System for Outrun Typechecker v3
//!
//! Following the minimalist philosophy, this module extends existing parser patterns
//! rather than creating parallel structures.

use outrun_parser::Span;
use std::collections::HashMap;
use std::fmt;

/// Type variable identifier for Hindley-Milner inference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVarId(pub u32);

/// Level for let-polymorphism (prevents premature generalization)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(pub u32);

/// Unified module name for all types, protocols, and implementations
/// In Outrun's unified module system, everything is a module:
/// - Types: "List", "Http.Client.Connection"
/// - Protocols: "Display", "BinaryAddition"
/// - Implementations: "List:Display", "Map:Iterator"
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName(String);

impl ModuleName {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Create implementation module name from type and protocol
    /// e.g., "List" + "Display" -> "List:Display"
    pub fn implementation(type_name: &str, protocol_name: &str) -> Self {
        Self(format!("{}:{}", type_name, protocol_name))
    }

    /// Check if this is an implementation module name (contains colon)
    pub fn is_implementation(&self) -> bool {
        self.0.contains(':')
    }

    /// Split implementation module name into type and protocol parts
    /// e.g., "List:Display" -> Some(("List", "Display"))
    pub fn split_implementation(&self) -> Option<(&str, &str)> {
        if let Some(colon_pos) = self.0.find(':') {
            let (type_part, protocol_part) = self.0.split_at(colon_pos);
            Some((type_part, &protocol_part[1..])) // Skip the colon
        } else {
            None
        }
    }
}

impl From<Vec<outrun_parser::TypeIdentifier>> for ModuleName {
    fn from(type_identifiers: Vec<outrun_parser::TypeIdentifier>) -> Self {
        let qualified_name = type_identifiers
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        ModuleName::new(qualified_name)
    }
}

impl From<&Vec<outrun_parser::TypeIdentifier>> for ModuleName {
    fn from(type_identifiers: &Vec<outrun_parser::TypeIdentifier>) -> Self {
        let qualified_name = type_identifiers
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        ModuleName::new(qualified_name)
    }
}

/// Unified type representation extending the existing parser patterns
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Concrete types: Outrun.Core.Integer64, User, List<String>
    Concrete {
        name: ModuleName,
        args: Vec<Type>,
        span: Option<Span>, // Preserve source location when available
    },

    /// Protocol constraints: Integer, Display, Result<T, E>
    Protocol {
        name: ModuleName,
        args: Vec<Type>,
        span: Option<Span>,
    },

    /// Type variables for inference: T, U, V
    Variable {
        var_id: TypeVarId,
        level: Level,         // For let-polymorphism
        name: Option<String>, // For debugging/error messages
        span: Option<Span>,
    },

    /// Self type (special case of type variable with binding semantics)
    SelfType {
        binding_context: SelfBindingContext,
        span: Option<Span>,
    },

    /// Function types: Function<(x: Integer) -> String>
    Function {
        params: Vec<(String, Type)>, // (param_name, param_type)
        return_type: Box<Type>,
        span: Option<Span>,
    },

    /// Tuple types: (Integer64, String, Boolean)
    /// Structural types with fixed arity and element types
    Tuple {
        element_types: Vec<Type>,
        span: Option<Span>,
    },
}

/// Position tracking for type variables and Self within complex type structures
#[derive(Debug, Clone, PartialEq)]
pub enum TypePosition {
    /// Type variable position with path from root type
    VariablePosition { path: Vec<String> },
    /// Self position with path from root type  
    SelfPosition { path: Vec<String> },
    /// Argument position in function signature
    ArgumentPosition {
        index: usize,
        path: Vec<String>,
        param_name: Option<String>, // Parameter name for keyword argument matching
    },
    /// Return type position in function signature
    ReturnPosition { path: Vec<String> },
    /// Generic argument position within container type
    GenericArgument {
        container: String,
        arg_index: usize,
        path: Vec<String>,
    },
}

/// Self type position with binding context
#[derive(Debug, Clone, PartialEq)]
pub struct SelfPosition {
    /// Path from root type to this Self occurrence
    pub path: Vec<String>,
    /// Binding context for this Self occurrence
    pub binding_context: SelfBindingContext,
}

/// Context for Self type binding
#[derive(Debug, Clone, PartialEq)]
pub enum SelfBindingContext {
    /// Self in protocol definition (unbound - could be any implementer)
    ProtocolDefinition {
        protocol_name: ModuleName,
        /// Generic parameters of the protocol (e.g., Container<T> where Self is Container)
        protocol_args: Vec<Type>,
    },
    /// Self in implementation (bound to specific implementing type)
    Implementation {
        implementing_type: ModuleName,
        /// Generic arguments of the implementing type (e.g., List<String> implementing Display)
        implementing_args: Vec<Type>,
        /// Protocol being implemented
        protocol_name: ModuleName,
        /// Generic arguments for the protocol in this implementation
        protocol_args: Vec<Type>,
    },
    /// Self in function signature within protocol/implementation context
    FunctionContext {
        /// The binding context this function is within
        parent_context: Box<SelfBindingContext>,
        /// Function name for error reporting
        function_name: Option<String>,
    },
}

impl Type {
    /// Create a concrete type with no generic arguments
    pub fn concrete(name: impl Into<String>) -> Self {
        Self::Concrete {
            name: ModuleName::new(name),
            args: vec![],
            span: None,
        }
    }

    /// Create a concrete type with span information
    pub fn concrete_with_span(name: impl Into<String>, span: Span) -> Self {
        Self::Concrete {
            name: ModuleName::new(name),
            args: vec![],
            span: Some(span),
        }
    }

    /// Create a protocol type without arguments
    pub fn protocol(name: impl Into<String>) -> Self {
        Self::Protocol {
            name: ModuleName::new(name),
            args: vec![],
            span: None,
        }
    }

    /// Create a protocol type with generic arguments
    pub fn protocol_with_args(name: impl Into<String>, args: Vec<Type>) -> Self {
        Self::Protocol {
            name: ModuleName::new(name),
            args,
            span: None,
        }
    }

    /// Create a protocol type with span information
    pub fn protocol_with_span(name: impl Into<String>, span: Span) -> Self {
        Self::Protocol {
            name: ModuleName::new(name),
            args: vec![],
            span: Some(span),
        }
    }

    /// Create a protocol type with arguments and span information
    pub fn protocol_with_args_and_span(
        name: impl Into<String>,
        args: Vec<Type>,
        span: Span,
    ) -> Self {
        Self::Protocol {
            name: ModuleName::new(name),
            args,
            span: Some(span),
        }
    }

    /// Create a generic concrete type (e.g., List<T>)
    pub fn generic_concrete(name: impl Into<String>, args: Vec<Type>) -> Self {
        Self::Concrete {
            name: ModuleName::new(name),
            args,
            span: None,
        }
    }

    /// Create a type variable
    pub fn variable(var_id: TypeVarId, level: Level) -> Self {
        Self::Variable {
            var_id,
            level,
            name: None,
            span: None,
        }
    }

    /// Create a named type variable (for debugging)
    pub fn named_variable(var_id: TypeVarId, level: Level, name: String) -> Self {
        Self::Variable {
            var_id,
            level,
            name: Some(name),
            span: None,
        }
    }

    /// Create a Self type in protocol definition context
    pub fn self_in_protocol(protocol_name: ModuleName, protocol_args: Vec<Type>) -> Self {
        Self::SelfType {
            binding_context: SelfBindingContext::ProtocolDefinition {
                protocol_name,
                protocol_args,
            },
            span: None,
        }
    }

    /// Create a Self type in implementation context
    pub fn self_in_implementation(
        implementing_type: ModuleName,
        implementing_args: Vec<Type>,
        protocol_name: ModuleName,
        protocol_args: Vec<Type>,
    ) -> Self {
        Self::SelfType {
            binding_context: SelfBindingContext::Implementation {
                implementing_type,
                implementing_args,
                protocol_name,
                protocol_args,
            },
            span: None,
        }
    }

    /// Create a Self type with span information
    pub fn self_with_span(context: SelfBindingContext, span: Span) -> Self {
        Self::SelfType {
            binding_context: context,
            span: Some(span),
        }
    }

    /// Get the span of this type if available
    pub fn span(&self) -> Option<&Span> {
        match self {
            Self::Concrete { span, .. }
            | Self::Protocol { span, .. }
            | Self::Variable { span, .. }
            | Self::SelfType { span, .. }
            | Self::Function { span, .. }
            | Self::Tuple { span, .. } => span.as_ref(),
        }
    }

    /// Check if this type contains the given type variable (for occurs check)
    /// Uses iterative traversal to prevent stack overflow on deep type hierarchies
    pub fn contains_var(&self, target_var: TypeVarId) -> bool {
        let mut work_stack = vec![self];
        let mut visited = std::collections::HashSet::new();

        while let Some(current_type) = work_stack.pop() {
            // Prevent infinite loops on recursive type structures
            let type_ptr = current_type as *const Type;
            if !visited.insert(type_ptr) {
                continue;
            }

            match current_type {
                Self::Variable { var_id, .. } => {
                    if *var_id == target_var {
                        return true;
                    }
                }
                Self::Concrete { args, .. } | Self::Protocol { args, .. } => {
                    // Add all args to work stack
                    work_stack.extend(args.iter());
                }
                Self::Function {
                    params,
                    return_type,
                    ..
                } => {
                    // Add parameter types and return type to work stack
                    work_stack.extend(params.iter().map(|(_, param_type)| param_type));
                    work_stack.push(return_type.as_ref());
                }
                Self::SelfType {
                    binding_context, ..
                } => {
                    // Add types from binding context to work stack
                    self.collect_binding_context_types(binding_context, &mut work_stack);
                }
                Self::Tuple { element_types, .. } => {
                    // Add all element types to work stack
                    work_stack.extend(element_types.iter());
                }
            }
        }

        false
    }

    /// Helper to collect types from SelfBindingContext for iterative traversal
    fn collect_binding_context_types<'a>(
        &self,
        context: &'a SelfBindingContext,
        work_stack: &mut Vec<&'a Type>,
    ) {
        // Use iterative approach to avoid stack overflow on deep context chains
        let mut context_stack = vec![context];

        while let Some(current_context) = context_stack.pop() {
            match current_context {
                SelfBindingContext::ProtocolDefinition { protocol_args, .. } => {
                    work_stack.extend(protocol_args.iter());
                }
                SelfBindingContext::Implementation {
                    implementing_args,
                    protocol_args,
                    ..
                } => {
                    work_stack.extend(implementing_args.iter());
                    work_stack.extend(protocol_args.iter());
                }
                SelfBindingContext::FunctionContext { parent_context, .. } => {
                    // Add parent context to stack for processing
                    context_stack.push(parent_context.as_ref());
                }
            }
        }
    }

    /// Check if this is a Self type
    pub fn is_self_type(&self) -> bool {
        matches!(self, Self::SelfType { .. })
    }

    /// Get the binding context if this is a Self type
    pub fn self_binding_context(&self) -> Option<&SelfBindingContext> {
        match self {
            Self::SelfType {
                binding_context, ..
            } => Some(binding_context),
            _ => None,
        }
    }

    /// Check if this type represents a protocol constraint (e.g., Option<Integer>)
    /// rather than a concrete monomorphizable type
    pub fn is_protocol_constraint(&self) -> bool {
        match self {
            Self::Protocol { .. } => true,
            Self::Concrete { args, .. } => {
                // Concrete type with protocol arguments is a constraint
                args.iter().any(|arg| arg.is_protocol_constraint())
            }
            Self::Variable { .. } => false, // Type variables are not constraints
            Self::SelfType { .. } => false, // Self will be resolved to concrete type
            Self::Function {
                params,
                return_type,
                ..
            } => {
                // Function with protocol constraints in signature
                params
                    .iter()
                    .any(|(_, param_type)| param_type.is_protocol_constraint())
                    || return_type.is_protocol_constraint()
            }
            Self::Tuple { element_types, .. } => {
                // Tuple is a protocol constraint if any element is
                element_types
                    .iter()
                    .any(|elem_type| elem_type.is_protocol_constraint())
            }
        }
    }

    /// Check if this type can be monomorphized to concrete implementations
    pub fn is_concrete_monomorphizable(&self) -> bool {
        match self {
            Self::Concrete { args, .. } => {
                // Concrete type is monomorphizable if all args are monomorphizable
                args.iter().all(|arg| arg.is_concrete_monomorphizable())
            }
            Self::Variable { .. } => true, // Type variables can be substituted
            Self::SelfType { .. } => true, // Self can be resolved
            Self::Protocol { .. } => false, // Protocol constraints need implementation resolution
            Self::Function {
                params,
                return_type,
                ..
            } => {
                // Function is monomorphizable if all types in signature are
                params
                    .iter()
                    .all(|(_, param_type)| param_type.is_concrete_monomorphizable())
                    && return_type.is_concrete_monomorphizable()
            }
            Self::Tuple { element_types, .. } => {
                // Tuple is monomorphizable if all elements are
                element_types
                    .iter()
                    .all(|elem_type| elem_type.is_concrete_monomorphizable())
            }
        }
    }

    /// Collect all positions where type variables (including Self) appear in this type
    pub fn collect_type_variable_positions(&self) -> HashMap<String, Vec<TypePosition>> {
        let mut positions = HashMap::new();
        self.collect_type_variable_positions_recursive(&mut positions, &[]);
        positions
    }

    /// Recursive helper for collecting type variable positions with path tracking
    fn collect_type_variable_positions_recursive(
        &self,
        positions: &mut HashMap<String, Vec<TypePosition>>,
        path: &[String],
    ) {
        match self {
            Self::Variable {
                name: Some(var_name),
                ..
            } => {
                positions.entry(var_name.clone()).or_default().push(
                    TypePosition::VariablePosition {
                        path: path.to_vec(),
                    },
                );
            }
            Self::SelfType { .. } => {
                positions
                    .entry("Self".to_string())
                    .or_default()
                    .push(TypePosition::SelfPosition {
                        path: path.to_vec(),
                    });
            }
            Self::Concrete { args, .. } | Self::Protocol { args, .. } => {
                for (i, arg) in args.iter().enumerate() {
                    let mut new_path = path.to_vec();
                    new_path.push(i.to_string());
                    arg.collect_type_variable_positions_recursive(positions, &new_path);
                }
            }
            Self::Function {
                params,
                return_type,
                ..
            } => {
                for (i, (_, param_type)) in params.iter().enumerate() {
                    let mut new_path = path.to_vec();
                    new_path.push(format!("param_{}", i));
                    param_type.collect_type_variable_positions_recursive(positions, &new_path);
                }
                let mut return_path = path.to_vec();
                return_path.push("return".to_string());
                return_type.collect_type_variable_positions_recursive(positions, &return_path);
            }
            _ => {} // Variables without names, nothing to track
        }
    }

    /// Extract all positions where Self appears in this type
    pub fn extract_self_positions(&self) -> Vec<SelfPosition> {
        let mut positions = Vec::new();
        self.extract_self_positions_recursive(&mut positions, &[]);
        positions
    }

    /// Recursive helper for extracting Self positions
    fn extract_self_positions_recursive(&self, positions: &mut Vec<SelfPosition>, path: &[String]) {
        match self {
            Self::SelfType {
                binding_context, ..
            } => {
                positions.push(SelfPosition {
                    path: path.to_vec(),
                    binding_context: binding_context.clone(),
                });
            }
            Self::Concrete { args, .. } | Self::Protocol { args, .. } => {
                for (i, arg) in args.iter().enumerate() {
                    let mut new_path = path.to_vec();
                    new_path.push(i.to_string());
                    arg.extract_self_positions_recursive(positions, &new_path);
                }
            }
            Self::Function {
                params,
                return_type,
                ..
            } => {
                for (i, (_, param_type)) in params.iter().enumerate() {
                    let mut new_path = path.to_vec();
                    new_path.push(format!("param_{}", i));
                    param_type.extract_self_positions_recursive(positions, &new_path);
                }
                let mut return_path = path.to_vec();
                return_path.push("return".to_string());
                return_type.extract_self_positions_recursive(positions, &return_path);
            }
            _ => {} // Other types don't contain Self
        }
    }

    /// Substitute type variables with concrete types throughout this type
    pub fn substitute_type_variables(&self, substitutions: &HashMap<String, Type>) -> Type {
        match self {
            Self::Variable {
                name: Some(var_name),
                ..
            } => substitutions
                .get(var_name)
                .cloned()
                .unwrap_or_else(|| self.clone()),
            Self::SelfType { .. } => substitutions
                .get("Self")
                .cloned()
                .unwrap_or_else(|| self.clone()),
            Self::Concrete { name, args, span } => {
                let substituted_args = args
                    .iter()
                    .map(|arg| arg.substitute_type_variables(substitutions))
                    .collect();
                Self::Concrete {
                    name: name.clone(),
                    args: substituted_args,
                    span: *span,
                }
            }
            Self::Protocol { name, args, span } => {
                let substituted_args = args
                    .iter()
                    .map(|arg| arg.substitute_type_variables(substitutions))
                    .collect();
                Self::Protocol {
                    name: name.clone(),
                    args: substituted_args,
                    span: *span,
                }
            }
            Self::Function {
                params,
                return_type,
                span,
            } => {
                let substituted_params = params
                    .iter()
                    .map(|(name, param_type)| {
                        (
                            name.clone(),
                            param_type.substitute_type_variables(substitutions),
                        )
                    })
                    .collect();
                let substituted_return = return_type.substitute_type_variables(substitutions);
                Self::Function {
                    params: substituted_params,
                    return_type: Box::new(substituted_return),
                    span: *span,
                }
            }
            _ => self.clone(), // Variables without names, no substitution needed
        }
    }

    /// Temporary compatibility method for resolve_self (will be removed in Phase 2)
    /// This provides a basic Self resolution to prevent compilation errors
    /// during the Phase 1 type system rewrite
    pub fn resolve_self(&self) -> Option<Type> {
        match self {
            Self::SelfType {
                binding_context:
                    SelfBindingContext::Implementation {
                        implementing_type,
                        implementing_args,
                        ..
                    },
                span,
            } => {
                let resolved = Type::Concrete {
                    name: implementing_type.clone(),
                    args: implementing_args.clone(),
                    span: *span,
                };
                Some(resolved)
            }
            Self::SelfType {..} => {
                None // Self is unbound in protocol definitions
            }
            _ => None,
        }
    }

    /// Unified type parameter substitution with comprehensive Self handling
    ///
    /// This is the canonical implementation that handles:
    /// - Regular generic type parameter substitution (T -> ConcreteType)
    /// - Self type substitution for protocol function monomorphization
    /// - Recursive substitution in nested generic types
    /// - Proper error handling for edge cases
    pub fn substitute_type_parameters(
        &self,
        substitutions: &std::collections::HashMap<String, Type>,
        allow_self_substitution: bool,
    ) -> Result<Type, crate::error::TypecheckError> {
        match self {
            Type::Concrete { name, args, span } => {
                // Check if this is a generic parameter to substitute
                if let Some(substitution) = substitutions.get(name.as_str()) {
                    Ok(substitution.clone())
                } else {
                    // Recursively substitute in generic arguments
                    let mut substituted_args = Vec::new();
                    for arg in args {
                        substituted_args.push(
                            arg.substitute_type_parameters(substitutions, allow_self_substitution)?,
                        );
                    }
                    Ok(Type::Concrete {
                        name: name.clone(),
                        args: substituted_args,
                        span: *span,
                    })
                }
            }
            Type::Protocol { name, args, span } => {
                // Check if this is a generic parameter to substitute
                if let Some(substitution) = substitutions.get(name.as_str()) {
                    Ok(substitution.clone())
                } else {
                    // Recursively substitute in generic arguments
                    let mut substituted_args = Vec::new();
                    for arg in args {
                        substituted_args.push(
                            arg.substitute_type_parameters(substitutions, allow_self_substitution)?,
                        );
                    }
                    Ok(Type::Protocol {
                        name: name.clone(),
                        args: substituted_args,
                        span: *span,
                    })
                }
            }
            Type::Variable { .. } => Ok(self.clone()), // Type variables remain unchanged
            Type::SelfType { .. } => {
                if allow_self_substitution {
                    // For protocol function monomorphization, substitute Self with concrete type
                    if let Some(substitution) = substitutions.get("Self") {
                        Ok(substitution.clone())
                    } else {
                        // Self not found in substitutions - this could be an error in some contexts
                        Ok(self.clone())
                    }
                } else {
                    // For regular monomorphization, Self types should be resolved before substitution
                    Ok(self.clone())
                }
            }
            Type::Function {
                params,
                return_type,
                span,
            } => {
                let mut substituted_params = Vec::new();
                for (name, param_type) in params {
                    substituted_params.push((
                        name.clone(),
                        param_type
                            .substitute_type_parameters(substitutions, allow_self_substitution)?,
                    ));
                }

                Ok(Type::Function {
                    params: substituted_params,
                    return_type: Box::new(
                        return_type
                            .substitute_type_parameters(substitutions, allow_self_substitution)?,
                    ),
                    span: *span,
                })
            }
            Type::Tuple { element_types, span } => {
                // Recursively substitute in element types
                let mut substituted_elements = Vec::new();
                for elem_type in element_types {
                    substituted_elements.push(
                        elem_type.substitute_type_parameters(substitutions, allow_self_substitution)?,
                    );
                }

                Ok(Type::Tuple {
                    element_types: substituted_elements,
                    span: *span,
                })
            }
        }
    }
}

/// Type constraints for protocol bounds
#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    /// T: Display
    Implements {
        type_var: TypeVarId,
        protocol: ModuleName,
        span: Option<Span>,
    },

    /// Self: Display (Self type constraint)
    SelfImplements {
        self_context: SelfBindingContext,
        protocol: ModuleName,
        span: Option<Span>,
    },

    /// T: Display || T: Debug
    LogicalOr {
        left: Box<Constraint>,
        right: Box<Constraint>,
        span: Option<Span>,
    },

    /// T: Display && T: Clone
    LogicalAnd {
        left: Box<Constraint>,
        right: Box<Constraint>,
        span: Option<Span>,
    },

    /// Equality constraint for unification: T = List<U>
    Equality {
        left: Box<Type>,
        right: Box<Type>,
        span: Option<Span>,
    },

    /// Self = SomeType (Self type binding constraint)
    SelfBinding {
        self_context: SelfBindingContext,
        bound_type: Box<Type>,
        span: Option<Span>,
    },

    /// Type variable must resolve to a specific named type (for deferred type resolution)
    /// Used when type annotation references unknown type name that may be registered later
    NameBinding {
        type_var: TypeVarId,
        expected_name: String,
        expected_args: Vec<Type>, // Generic arguments for the expected type
        span: Option<Span>,
    },
}

/// Substitution mapping type variables to types
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Substitution {
    mappings: HashMap<TypeVarId, Type>,
}

impl Substitution {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a mapping from type variable to type
    pub fn insert(&mut self, var_id: TypeVarId, ty: Type) {
        self.mappings.insert(var_id, ty);
    }

    /// Get the type that a variable maps to
    pub fn get(&self, var_id: TypeVarId) -> Option<&Type> {
        self.mappings.get(&var_id)
    }

    /// Check if the substitution is empty
    pub fn is_empty(&self) -> bool {
        self.mappings.is_empty()
    }

    /// Get the number of mappings
    pub fn len(&self) -> usize {
        self.mappings.len()
    }

    /// Apply this substitution to a type, iteratively resolving variables to prevent stack overflow
    pub fn apply(&self, ty: &Type) -> Type {
        self.apply_iterative(ty)
    }

    /// Iterative implementation to prevent stack overflow on deep type chains
    fn apply_iterative(&self, ty: &Type) -> Type {
        // Handle variable chains iteratively to prevent stack overflow
        if let Type::Variable { var_id, .. } = ty {
            let mut current_var = *var_id;
            let mut visited = std::collections::HashSet::new();

            // Follow the substitution chain iteratively
            while let Some(substituted_type) = self.get(current_var) {
                if !visited.insert(current_var) {
                    // Circular substitution detected - return original type
                    return ty.clone();
                }

                if let Type::Variable {
                    var_id: next_var, ..
                } = substituted_type
                {
                    current_var = *next_var;
                } else {
                    // Found non-variable type, apply substitution to it and return
                    return self.apply_to_structure(substituted_type);
                }
            }

            // No substitution found for this variable
            return ty.clone();
        }

        // For non-variable types, apply to structure
        self.apply_to_structure(ty)
    }

    /// Apply substitution to the structure of compound types
    fn apply_to_structure(&self, ty: &Type) -> Type {
        match ty {
            Type::Variable { .. } => {
                // This should be handled by apply_iterative, but handle just in case
                self.apply_iterative(ty)
            }
            Type::Concrete { name, args, span } => Type::Concrete {
                name: name.clone(),
                args: args.iter().map(|arg| self.apply(arg)).collect(),
                span: *span,
            },
            Type::Protocol { name, args, span } => Type::Protocol {
                name: name.clone(),
                args: args.iter().map(|arg| self.apply(arg)).collect(),
                span: *span,
            },
            Type::Function {
                params,
                return_type,
                span,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|(name, param_type)| (name.clone(), self.apply(param_type)))
                    .collect(),
                return_type: Box::new(self.apply(return_type)),
                span: *span,
            },
            Type::SelfType { .. } => ty.clone(), // Self is handled separately
            Type::Tuple { element_types, span } => Type::Tuple {
                element_types: element_types.iter().map(|elem| self.apply(elem)).collect(),
                span: *span,
            },
        }
    }

    /// Compose this substitution with another (self applied first)
    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = Substitution::new();

        // Apply other to our mappings
        for (&var_id, ty) in &self.mappings {
            result.insert(var_id, other.apply(ty));
        }

        // Add mappings from other that we don't have
        for (&var_id, ty) in &other.mappings {
            if !result.mappings.contains_key(&var_id) {
                result.insert(var_id, ty.clone());
            }
        }

        result
    }
}

/// Type information attached to expressions after type checking
#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    /// The inferred or annotated type
    pub resolved_type: Type,
    /// Constraints that were satisfied during inference
    pub constraints: Vec<Constraint>,
    /// Substitution used to resolve this type
    pub substitution: Option<Substitution>,
}

impl TypeInfo {
    pub fn new(resolved_type: Type) -> Self {
        Self {
            resolved_type,
            constraints: Vec::new(),
            substitution: None,
        }
    }

    pub fn with_constraints(resolved_type: Type, constraints: Vec<Constraint>) -> Self {
        Self {
            resolved_type,
            constraints,
            substitution: None,
        }
    }

    /// Convert to parser's lightweight ParsedTypeInfo for AST integration
    pub fn to_parsed_type_info(&self) -> outrun_parser::ParsedTypeInfo {
        outrun_parser::ParsedTypeInfo::new(format!("{}", self.resolved_type))
    }

    /// Convert to parser's ParsedTypeInfo with span information
    pub fn to_parsed_type_info_with_span(
        &self,
        span: outrun_parser::Span,
    ) -> outrun_parser::ParsedTypeInfo {
        outrun_parser::ParsedTypeInfo::with_span(format!("{}", self.resolved_type), span)
    }
}

/// Type variable generator for creating fresh variables
#[derive(Debug, Clone)]
pub struct TypeVarGenerator {
    next_id: u32,
}

impl TypeVarGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    /// Generate a fresh type variable
    pub fn fresh(&mut self, level: Level) -> Type {
        let var_id = TypeVarId(self.next_id);
        self.next_id += 1;
        Type::variable(var_id, level)
    }

    /// Generate a fresh named type variable (for debugging)
    pub fn fresh_named(&mut self, level: Level, name: String) -> Type {
        let var_id = TypeVarId(self.next_id);
        self.next_id += 1;
        Type::named_variable(var_id, level, name)
    }
}

impl Default for TypeVarGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Complete unified module representation for Outrun's "everything is a module" philosophy
#[derive(Debug, Clone, PartialEq)]
pub enum TypeModule {
    /// Protocol definition module (e.g., "Display", "BinaryAddition")
    Protocol {
        name: ModuleName,
        definition: ProtocolDefinition,
        source_location: Span,
        generic_arity: usize,
    },
    /// Struct definition module (e.g., "List", "User", "Http.Client")
    Struct {
        name: ModuleName,
        definition: ConcreteTypeDefinition,
        source_location: Span,
        generic_arity: usize,
    },
    /// Implementation module (e.g., "List:Display", "Map:Iterator")
    Implementation {
        name: ModuleName,                   // "List:Display"
        implementing_type: ModuleName,      // "List"
        protocol: ModuleName,               // "Display"
        generic_bindings: Vec<Type>,        // T -> String for this impl
        functions: Vec<FunctionDefinition>, // The actual impl functions
        source_location: Span,
        defining_module: ModuleName, // Where this impl lives
    },
    /// Forward binding for types referenced before definition
    ForwardBinding {
        name: ModuleName,
        expected_arity: Option<usize>,
        source_location: Span,
        references: Vec<Span>,
    },
}

/// Function definition within an implementation
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Option<outrun_parser::Expression>, // None for signatures only
    pub is_static: bool,
    pub span: Option<Span>,
}

/// Protocol definition information
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolDefinition {
    /// The protocol itself
    pub protocol_name: ModuleName,
    /// Protocols required by this protocol (e.g., Integer requires BinaryAddition)
    pub required_protocols: std::collections::HashSet<ModuleName>,
    /// Module where this protocol is defined
    pub defining_module: ModuleName,
    /// Functions that have default implementations (function_name -> true)
    pub default_implementations: std::collections::HashSet<String>,
    /// All function signatures required by this protocol (function_name -> signature info)
    pub required_functions: std::collections::HashSet<String>,
    /// Source location for error reporting
    pub span: Option<Span>,
}

/// Information about a concrete type definition
#[derive(Debug, Clone, PartialEq)]
pub struct ConcreteTypeDefinition {
    /// The concrete type itself
    pub type_name: ModuleName,
    /// Module where this type is defined
    pub defining_module: ModuleName,
    /// Whether this type is generic (has type parameters)
    pub is_generic: bool,
    /// Source location for error reporting
    pub span: Option<Span>,
    /// WORKAROUND: Never type info for @Never() attribute support
    /// TODO: Replace with proper attribute system when macro system is implemented
    pub never_info: Option<NeverTypeInfo>,
}

/// WORKAROUND: Information about never types marked with @Never() attribute
/// TODO: Replace with proper attribute system when macro system is implemented
#[derive(Debug, Clone, PartialEq)]
pub struct NeverTypeInfo {
    pub message: String,
}

/// Display implementation for types (for error messages)
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Concrete { name, args, .. } => {
                write!(f, "{}", name.as_str())?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Self::Protocol { name, args, .. } => {
                write!(f, "{}", name.as_str())?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Self::Variable {
                name: Some(name), ..
            } => write!(f, "{name}"),
            Self::Variable { var_id, .. } => write!(f, "T{}", var_id.0),
            Self::SelfType { .. } => write!(f, "Self"),
            Self::Function {
                params,
                return_type,
                ..
            } => {
                write!(f, "Function<(")?;
                for (i, (name, param_type)) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name}: {param_type}")?;
                }
                write!(f, ") -> {return_type}>")
            }
            Self::Tuple { element_types, .. } => {
                write!(f, "(")?;
                for (i, elem_type) in element_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem_type}")?;
                }
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implements {
                type_var, protocol, ..
            } => {
                write!(f, "T{}: {}", type_var.0, protocol.as_str())
            }
            Self::SelfImplements { protocol, .. } => {
                write!(f, "Self: {}", protocol.as_str())
            }
            Self::LogicalOr { left, right, .. } => {
                write!(f, "({left} || {right})")
            }
            Self::LogicalAnd { left, right, .. } => {
                write!(f, "({left} && {right})")
            }
            Self::Equality { left, right, .. } => {
                write!(f, "{left} = {right}")
            }
            Self::SelfBinding { bound_type, .. } => {
                write!(f, "Self = {bound_type}")
            }
            Self::NameBinding {
                type_var,
                expected_name,
                expected_args,
                ..
            } => {
                if expected_args.is_empty() {
                    write!(f, "T{} must resolve to {}", type_var.0, expected_name)
                } else {
                    let args_str = expected_args
                        .iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(
                        f,
                        "T{} must resolve to {}<{}>",
                        type_var.0, expected_name, args_str
                    )
                }
            }
        }
    }
}
