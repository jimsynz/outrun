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

/// Unique identifier for types to enable fast equality checks
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(pub String);

impl TypeId {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub fn name(&self) -> &str {
        &self.0
    }
}

/// Protocol identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProtocolId(pub String);

impl ProtocolId {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

/// Module identifier for tracking implementation sources (orphan rule enforcement)
/// In Outrun, modules are defined by type names minus generic arguments
/// e.g., Http.Client.Connection<T> -> module Http.Client.Connection, List<String> -> module List
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(pub String);

impl ModuleId {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
    
    /// Create ModuleId from a TypeId (module is the type name itself)
    /// e.g., "Http.Client.Connection" -> module "Http.Client.Connection"
    pub fn from_type_id(type_id: &TypeId) -> Self {
        Self(type_id.name().to_string())
    }
    
    /// Create ModuleId from a ProtocolId (module is the protocol name itself)
    /// e.g., "Display" -> module "Display"
    pub fn from_protocol_id(protocol_id: &ProtocolId) -> Self {
        Self(protocol_id.0.clone())
    }
    
    pub fn name(&self) -> &str {
        &self.0
    }
}

/// Unified type representation extending the existing parser patterns
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Concrete types: Outrun.Core.Integer64, User, List<String>
    Concrete {
        id: TypeId,
        args: Vec<Type>,
        span: Option<Span>, // Preserve source location when available
    },

    /// Protocol constraints: Integer, Display, Result<T, E>
    Protocol {
        id: ProtocolId,
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
}

/// Context for Self type binding
#[derive(Debug, Clone, PartialEq)]
pub enum SelfBindingContext {
    /// Self in protocol definition (unbound - could be any implementer)
    ProtocolDefinition { 
        protocol_id: ProtocolId,
        /// Generic parameters of the protocol (e.g., Container<T> where Self is Container)
        protocol_args: Vec<Type>,
    },
    /// Self in implementation (bound to specific implementing type)
    Implementation { 
        implementing_type: TypeId,
        /// Generic arguments of the implementing type (e.g., List<String> implementing Display)
        implementing_args: Vec<Type>,
        /// Protocol being implemented
        protocol_id: ProtocolId,
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
            id: TypeId::new(name),
            args: vec![],
            span: None,
        }
    }

    /// Create a concrete type with span information
    pub fn concrete_with_span(name: impl Into<String>, span: Span) -> Self {
        Self::Concrete {
            id: TypeId::new(name),
            args: vec![],
            span: Some(span),
        }
    }

    /// Create a generic concrete type (e.g., List<T>)
    pub fn generic_concrete(name: impl Into<String>, args: Vec<Type>) -> Self {
        Self::Concrete {
            id: TypeId::new(name),
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
    pub fn self_in_protocol(protocol_id: ProtocolId, protocol_args: Vec<Type>) -> Self {
        Self::SelfType {
            binding_context: SelfBindingContext::ProtocolDefinition {
                protocol_id,
                protocol_args,
            },
            span: None,
        }
    }

    /// Create a Self type in implementation context
    pub fn self_in_implementation(
        implementing_type: TypeId,
        implementing_args: Vec<Type>,
        protocol_id: ProtocolId,
        protocol_args: Vec<Type>,
    ) -> Self {
        Self::SelfType {
            binding_context: SelfBindingContext::Implementation {
                implementing_type,
                implementing_args,
                protocol_id,
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
            | Self::Function { span, .. } => span.as_ref(),
        }
    }

    /// Check if this type contains the given type variable (for occurs check)
    pub fn contains_var(&self, target_var: TypeVarId) -> bool {
        match self {
            Self::Variable { var_id, .. } => *var_id == target_var,
            Self::Concrete { args, .. } | Self::Protocol { args, .. } => {
                args.iter().any(|arg| arg.contains_var(target_var))
            }
            Self::Function {
                params,
                return_type,
                ..
            } => {
                params
                    .iter()
                    .any(|(_, param_type)| param_type.contains_var(target_var))
                    || return_type.contains_var(target_var)
            }
            Self::SelfType { binding_context, .. } => {
                // Self types can contain variables in their binding context arguments
                match binding_context {
                    SelfBindingContext::ProtocolDefinition { protocol_args, .. } => {
                        protocol_args.iter().any(|arg| arg.contains_var(target_var))
                    }
                    SelfBindingContext::Implementation { 
                        implementing_args, protocol_args, .. 
                    } => {
                        implementing_args.iter().any(|arg| arg.contains_var(target_var))
                            || protocol_args.iter().any(|arg| arg.contains_var(target_var))
                    }
                    SelfBindingContext::FunctionContext { parent_context, .. } => {
                        // Recursively check parent context
                        match parent_context.as_ref() {
                            SelfBindingContext::ProtocolDefinition { protocol_args, .. } => {
                                protocol_args.iter().any(|arg| arg.contains_var(target_var))
                            }
                            SelfBindingContext::Implementation { 
                                implementing_args, protocol_args, .. 
                            } => {
                                implementing_args.iter().any(|arg| arg.contains_var(target_var))
                                    || protocol_args.iter().any(|arg| arg.contains_var(target_var))
                            }
                            SelfBindingContext::FunctionContext { .. } => false, // Avoid infinite recursion
                        }
                    }
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
            Self::SelfType { binding_context, .. } => Some(binding_context),
            _ => None,
        }
    }

    /// Resolve Self to its concrete type if in implementation context
    pub fn resolve_self(&self) -> Option<Type> {
        match self {
            Self::SelfType { 
                binding_context: SelfBindingContext::Implementation { 
                    implementing_type, implementing_args, .. 
                }, 
                span 
            } => {
                Some(Type::Concrete {
                    id: implementing_type.clone(),
                    args: implementing_args.clone(),
                    span: *span,
                })
            }
            Self::SelfType { .. } => None, // Self is unbound in protocol definitions
            _ => None,
        }
    }
}

/// Type constraints for protocol bounds
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum Constraint {
    /// T: Display
    Implements {
        type_var: TypeVarId,
        protocol: ProtocolId,
        span: Option<Span>,
    },

    /// Self: Display (Self type constraint)
    SelfImplements {
        self_context: SelfBindingContext,
        protocol: ProtocolId,
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

    /// Apply this substitution to a type, recursively resolving variables
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Variable { var_id, .. } => {
                match self.get(*var_id) {
                    Some(substituted) => self.apply(substituted), // Recursive resolution
                    None => ty.clone(),                           // Unbound variable
                }
            }
            Type::Concrete { id, args, span } => Type::Concrete {
                id: id.clone(),
                args: args.iter().map(|arg| self.apply(arg)).collect(),
                span: *span,
            },
            Type::Protocol { id, args, span } => Type::Protocol {
                id: id.clone(),
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

/// Display implementation for types (for error messages)
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Concrete { id, args, .. } => {
                write!(f, "{}", id.name())?;
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
            Self::Protocol { id, args, .. } => {
                write!(f, "{}", id.0)?;
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
        }
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implements {
                type_var, protocol, ..
            } => {
                write!(f, "T{}: {}", type_var.0, protocol.0)
            }
            Self::SelfImplements { protocol, .. } => {
                write!(f, "Self: {}", protocol.0)
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
        }
    }
}
