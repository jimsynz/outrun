//! Constraint solving for protocol bounds
//!
//! Implements logical constraint satisfaction for Outrun's protocol system.
//! Handles protocol implementation verification, logical operators, and orphan rules.

use crate::error::ConstraintError;
use crate::registry::TypeRegistry;
use crate::types::{Constraint, ModuleName, SelfBindingContext, Substitution, Type, TypeVarId};
use outrun_parser::Span;
use std::collections::HashMap;

/// Compiler warning system for type complexity and recursive patterns
#[derive(Debug, Clone, PartialEq)]
pub enum CompilerWarning {
    /// Warning about recursive protocol implementation pattern
    RecursiveProtocolImplementation {
        /// Location of the implementation that creates recursion
        implementation_span: Option<Span>,
        /// Protocol name involved in recursion
        protocol_name: String,
        /// Type implementing the protocol
        implementing_type: String,
        /// Explanation of the recursive pattern
        explanation: String,
        /// Impact description
        impact: String,
        /// Suggested improvements
        suggestions: Vec<String>,
    },
    /// Warning about high complexity dispatch with many concrete combinations
    HighConcreteCombinationCount {
        /// Location of the call site generating many combinations
        call_site: Option<Span>,
        /// Number of concrete combinations generated
        combination_count: usize,
        /// Sample of the generated combinations (first few)
        sample_combinations: Vec<Type>,
        /// Explanation of why this generates many combinations
        explanation: String,
        /// Suggested improvements
        suggestions: Vec<String>,
    },
    /// Warning about complex generic constraints
    ComplexGenericConstraints {
        /// Implementation that has complex constraints
        implementation: String,
        /// Number of concrete combinations this generates
        concrete_combinations: usize,
        /// Sample combinations
        sample_combinations: Vec<Type>,
        /// Explanation of complexity
        explanation: String,
        /// Suggested improvements
        suggestions: Vec<String>,
    },
}

/// Information about a detected recursive pattern in protocol implementations
#[derive(Debug, Clone, PartialEq)]
pub struct RecursivePatternInfo {
    /// Unique identifier for this pattern
    pub pattern_key: String,
    /// Type implementing the protocol (e.g., Option<T>)
    pub implementing_type: Type,
    /// Protocol being implemented (e.g., Empty)
    pub protocol: ModuleName,
    /// Detected recursion depth (None = unbounded)
    pub recursion_depth: Option<usize>,
    /// Whether warning has been emitted for this pattern
    pub warning_emitted: bool,
    /// Source location of the implementation
    pub implementation_span: Option<Span>,
}

/// Call stack context for enhanced constraint resolution through backtracking
#[derive(Debug, Clone, PartialEq)]
pub struct CallStackContext {
    /// Stack of function call contexts leading to current constraint
    pub call_stack: Vec<CallContext>,

    /// Additional type constraints discovered through backtracking  
    pub backtracked_constraints: Vec<BacktrackedConstraint>,

    /// Call depth limit to prevent infinite backtracking
    pub max_depth: usize,
}

/// Context information for a single function call in the call stack
#[derive(Debug, Clone, PartialEq)]
pub struct CallContext {
    /// Function signature being called
    pub function_signature: String,

    /// Module path where the call originates
    pub module_path: Vec<String>,

    /// Type constraints at this call site
    pub local_constraints: Vec<Type>,

    /// Generic type substitutions active at this call site
    pub generic_substitutions: HashMap<String, Type>,

    /// Source span of the function call
    pub span: Option<Span>,

    /// Call depth in the stack
    pub depth: usize,
}

/// A constraint discovered through call stack backtracking
#[derive(Debug, Clone, PartialEq)]
pub struct BacktrackedConstraint {
    /// The original constraint that triggered backtracking
    pub original_constraint: Constraint,

    /// Additional type information found by backtracking
    pub discovered_types: Vec<Type>,

    /// The call context where this information was found
    pub source_context: CallContext,

    /// Confidence level in this constraint (0.0 to 1.0)
    pub confidence: f64,
}

/// Template for a public function that dependent packages can use to generate clauses
#[derive(Debug, Clone, PartialEq)]
pub struct PublicFunctionTemplate {
    /// Function signature for identification
    pub function_signature: crate::universal_dispatch::FunctionSignature,

    /// Generic parameters in this function (e.g., T, U, Self)
    pub generic_parameters: Vec<GenericParameter>,

    /// Parameter types with their names and constraints
    pub parameter_types: Vec<ParameterTemplate>,

    /// Return type template
    pub return_type: TypeTemplate,

    /// Protocol constraints that must be satisfied (e.g., T: Display)
    pub protocol_constraints: Vec<ProtocolConstraintTemplate>,

    /// Function visibility (public/private)
    pub visibility: FunctionVisibility,

    /// Source span for debugging
    pub span: Option<Span>,

    /// Package name this template comes from
    pub source_package: String,

    /// Template generation timestamp for cache management
    pub generated_at: std::time::SystemTime,
}

/// A generic parameter in a function template
#[derive(Debug, Clone, PartialEq)]
pub struct GenericParameter {
    /// Parameter name (e.g., "T", "U", "Self")
    pub name: String,

    /// Direct constraints on this parameter (e.g., T: Display)
    pub direct_constraints: Vec<crate::types::ModuleName>,

    /// Whether this parameter appears in multiple positions
    pub multiple_occurrences: bool,

    /// Variance information for advanced constraint solving
    pub variance: ParameterVariance,
}

/// Parameter template with constraint information
#[derive(Debug, Clone, PartialEq)]
pub struct ParameterTemplate {
    /// Parameter name
    pub name: String,

    /// Parameter type template
    pub type_template: TypeTemplate,

    /// Whether this parameter is required or optional
    pub required: bool,
}

/// Template for types that can be instantiated with concrete types
#[derive(Debug, Clone, PartialEq)]
pub enum TypeTemplate {
    /// Concrete type that requires no substitution
    Concrete {
        type_name: String,
        generic_args: Vec<TypeTemplate>,
    },

    /// Generic parameter that needs substitution
    Generic {
        parameter_name: String,
        constraints: Vec<crate::types::ModuleName>,
    },

    /// Protocol reference
    Protocol {
        protocol_name: String,
        generic_args: Vec<TypeTemplate>,
    },

    /// Self type reference
    SelfType { context: SelfTypeContext },

    /// Function type template
    Function {
        parameter_templates: Vec<TypeTemplate>,
        return_template: Box<TypeTemplate>,
    },
}

/// Context information for Self type resolution
#[derive(Debug, Clone, PartialEq)]
pub enum SelfTypeContext {
    /// Self in protocol definition
    ProtocolDefinition { protocol_name: String },

    /// Self in impl block
    ImplementationBlock {
        protocol_name: String,
        implementing_type: String,
    },

    /// Self in struct method
    StructMethod { struct_name: String },
}

/// Protocol constraint in a function template
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolConstraintTemplate {
    /// Generic parameter being constrained
    pub parameter_name: String,

    /// Protocol that must be implemented
    pub protocol_id: crate::types::ModuleName,

    /// Additional generic arguments to the protocol
    pub protocol_args: Vec<TypeTemplate>,

    /// Whether this constraint is required or optional
    pub required: bool,
}

/// Function visibility for template generation
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionVisibility {
    /// Public function - generate template
    Public,

    /// Private function - don't generate template
    Private,
}

/// Parameter variance for advanced constraint solving
#[derive(Debug, Clone, PartialEq)]
pub enum ParameterVariance {
    /// Parameter appears in covariant position (return types, read-only)
    Covariant,

    /// Parameter appears in contravariant position (parameter types, write-only)
    Contravariant,

    /// Parameter appears in both positions (invariant)
    Invariant,

    /// Variance not yet determined
    Unknown,
}

impl CallStackContext {
    /// Create a new empty call stack context
    pub fn new(max_depth: usize) -> Self {
        Self {
            call_stack: Vec::new(),
            backtracked_constraints: Vec::new(),
            max_depth,
        }
    }

    /// Get the current call depth
    pub fn current_depth(&self) -> usize {
        self.call_stack.len()
    }

    /// Check if we're at maximum backtracking depth
    pub fn at_max_depth(&self) -> bool {
        self.current_depth() >= self.max_depth
    }
}

impl CallContext {
    /// Create a new call context
    pub fn new(
        function_signature: String,
        module_path: Vec<String>,
        span: Option<Span>,
        depth: usize,
    ) -> Self {
        Self {
            function_signature,
            module_path,
            local_constraints: Vec::new(),
            generic_substitutions: HashMap::new(),
            span,
            depth,
        }
    }

    /// Add a local type constraint to this call context
    pub fn add_local_constraint(&mut self, constraint: Type) {
        self.local_constraints.push(constraint);
    }

    /// Add a generic type substitution to this call context
    pub fn add_generic_substitution(&mut self, param_name: String, concrete_type: Type) {
        self.generic_substitutions.insert(param_name, concrete_type);
    }
}

/// Constraint solver implementing logical constraint satisfaction
#[derive(Debug)]
pub struct ConstraintSolver {
    /// Unified type registry (protocols, structs, implementations)
    type_registry: TypeRegistry,
    /// Cache of solved constraint results
    solution_cache: HashMap<String, bool>,
    /// Detected recursive patterns in protocol implementations
    recursive_patterns: HashMap<String, RecursivePatternInfo>,
    /// Collected warnings during constraint solving
    warnings: Vec<CompilerWarning>,
    /// Call stack context for backtracking support
    call_stack_context: Option<CallStackContext>,
    /// Maximum backtracking depth to prevent infinite recursion
    max_backtrack_depth: usize,
    /// Generated public function templates for dependent packages
    public_function_templates:
        HashMap<crate::universal_dispatch::FunctionSignature, PublicFunctionTemplate>,
    /// Package name for template generation
    current_package_name: String,
}

impl ConstraintSolver {
    pub fn new() -> Self {
        Self {
            type_registry: TypeRegistry::new(),
            solution_cache: HashMap::new(),
            recursive_patterns: HashMap::new(),
            warnings: Vec::new(),
            call_stack_context: None,
            max_backtrack_depth: 10, // Default backtracking depth limit
            public_function_templates: HashMap::new(),
            current_package_name: "unknown".to_string(),
        }
    }

    /// Create solver with existing type registry
    pub fn with_registry(registry: TypeRegistry) -> Self {
        Self {
            type_registry: registry,
            solution_cache: HashMap::new(),
            recursive_patterns: HashMap::new(),
            warnings: Vec::new(),
            call_stack_context: None,
            max_backtrack_depth: 10, // Default backtracking depth limit
            public_function_templates: HashMap::new(),
            current_package_name: "unknown".to_string(),
        }
    }

    /// Get mutable access to protocol registry
    pub fn type_registry_mut(&mut self) -> &mut TypeRegistry {
        &mut self.type_registry
    }

    /// Get immutable access to type registry
    pub fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    /// Get all collected warnings
    pub fn warnings(&self) -> &[CompilerWarning] {
        &self.warnings
    }

    /// Take all warnings (consuming them)
    pub fn take_warnings(&mut self) -> Vec<CompilerWarning> {
        std::mem::take(&mut self.warnings)
    }

    /// Get recursive patterns detected
    pub fn recursive_patterns(&self) -> &HashMap<String, RecursivePatternInfo> {
        &self.recursive_patterns
    }

    /// Solve a set of constraints with a given substitution
    pub fn solve(
        &mut self,
        constraints: &[Constraint],
        substitution: &Substitution,
    ) -> Result<(), ConstraintError> {
        for constraint in constraints {
            self.solve_constraint(constraint, substitution)?;
        }
        Ok(())
    }

    /// Solve a single constraint
    fn solve_constraint(
        &mut self,
        constraint: &Constraint,
        substitution: &Substitution,
    ) -> Result<(), ConstraintError> {
        match constraint {
            Constraint::Implements {
                type_var,
                protocol,
                span,
            } => self.solve_implements_constraint(*type_var, protocol, substitution, *span),

            Constraint::SelfImplements {
                self_context,
                protocol,
                span,
            } => self.solve_self_implements_constraint(self_context, protocol, *span),

            Constraint::LogicalOr { left, right, span } => {
                // T: Display || T: Debug - either constraint must be satisfiable
                let left_result = self.solve_constraint(left, substitution);
                if left_result.is_ok() {
                    return Ok(());
                }

                let right_result = self.solve_constraint(right, substitution);
                if right_result.is_ok() {
                    return Ok(());
                }

                // Both sides failed - report the constraint as unsatisfiable
                Err(ConstraintError::Unsatisfiable {
                    constraint: constraint.to_string(),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                })
            }

            Constraint::LogicalAnd { left, right, .. } => {
                // T: Display && T: Clone - both constraints must be satisfied
                self.solve_constraint(left, substitution)?;
                self.solve_constraint(right, substitution)?;
                Ok(())
            }

            Constraint::Equality { left, right, span } => {
                self.solve_equality_constraint(left, right, substitution, *span)
            }

            Constraint::SelfBinding {
                self_context,
                bound_type,
                span,
            } => self.solve_self_binding_constraint(self_context, bound_type, substitution, *span),

            Constraint::NameBinding {
                type_var,
                expected_name,
                expected_args,
                span,
            } => self.solve_name_binding_constraint(
                *type_var,
                expected_name,
                expected_args,
                substitution,
                *span,
            ),
        }
    }

    /// Solve an implements constraint: T: Protocol
    fn solve_implements_constraint(
        &mut self,
        type_var: TypeVarId,
        protocol: &ModuleName,
        substitution: &Substitution,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), ConstraintError> {
        // Apply substitution to resolve the type variable
        let resolved_type = self.resolve_type_var(type_var, substitution)?;

        match resolved_type {
            Type::Concrete { name, args, .. } => {
                // Check if this concrete type implements the protocol using the type registry
                if !self
                    .type_registry
                    .has_implementation_with_args(protocol, name, &args)
                {
                    let type_name = if args.is_empty() {
                        name.as_str().to_string()
                    } else {
                        // For generic types like List<String>, create a canonical representation
                        format!(
                            "{}[{}]",
                            name.as_str(),
                            args.iter()
                                .map(Self::type_to_string)
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    };

                    return Err(ConstraintError::MissingImplementation {
                        type_name,
                        protocol_name: protocol.as_str().to_string(),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    });
                }

                Ok(())
            }

            Type::Variable { .. } => {
                // Type variable still unbound - constraint cannot be verified yet
                // This is not necessarily an error; it may be resolved later
                Ok(())
            }

            Type::Protocol { name, .. } => {
                // Protocol types automatically implement themselves
                if name == protocol {
                    Ok(())
                } else {
                    // Check if there's a relationship between protocols
                    Err(ConstraintError::MissingImplementation {
                        type_name: format!("protocol {}", name.as_str()),
                        protocol_name: protocol.as_str().to_string(),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    })
                }
            }

            Type::SelfType {
                binding_context, ..
            } => {
                // Self types are handled by resolving them based on their binding context
                self.solve_self_implements_constraint(&binding_context, protocol, span)
            }

            Type::Function { .. } => {
                // Function types could potentially implement protocols
                // For now, treat as missing implementation
                Err(ConstraintError::MissingImplementation {
                    type_name: "function".to_string(),
                    protocol_name: protocol.0.clone(),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                })
            }
        }
    }

    /// Solve equality constraint: T = SomeType
    fn solve_equality_constraint(
        &mut self,
        left: &Type,
        right: &Type,
        _substitution: &Substitution,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), ConstraintError> {
        // For equality constraints, we would typically delegate to the unifier
        // Since we don't have access to the unifier here, we do a simple structural check
        if std::mem::discriminant(left) != std::mem::discriminant(right) {
            return Err(ConstraintError::Unsatisfiable {
                constraint: format!("{left} = {right}"),
                span: span.and_then(|s| crate::error::to_source_span(Some(s))),
            });
        }

        // More detailed equality checking would happen in the unifier
        // This is a simplified implementation
        Ok(())
    }

    /// Resolve a type variable using the substitution
    fn resolve_type_var(
        &self,
        type_var: TypeVarId,
        substitution: &Substitution,
    ) -> Result<Type, ConstraintError> {
        if let Some(resolved) = substitution.get(type_var) {
            Ok(resolved.clone())
        } else {
            // Type variable is unbound - create a placeholder variable type
            Ok(Type::variable(type_var, crate::types::Level(0)))
        }
    }

    /// Convert a type to a canonical string representation
    fn type_to_string(ty: &Type) -> String {
        match ty {
            Type::Concrete { name, args, .. } => {
                if args.is_empty() {
                    name.as_str().to_string()
                } else {
                    format!(
                        "{}[{}]",
                        name.as_str(),
                        args.iter()
                            .map(Self::type_to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Type::Variable { var_id, .. } => format!("T{}", var_id.0),
            Type::Protocol { name, .. } => format!("protocol {}", name.as_str()),
            Type::SelfType { .. } => "Self".to_string(),
            Type::Function { .. } => "function".to_string(),
        }
    }

    /// Solve a Self implements constraint: Self: Protocol
    fn solve_self_implements_constraint(
        &mut self,
        self_context: &SelfBindingContext,
        protocol: &ModuleName,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), ConstraintError> {
        match self_context {
            SelfBindingContext::Implementation {
                implementing_type,
                implementing_args,
                ..
            } => {
                // In implementation context, Self is bound to the concrete implementing type
                if !self.type_registry.has_implementation_with_args(
                    protocol,
                    implementing_type,
                    implementing_args,
                ) {
                    let type_name = if implementing_args.is_empty() {
                        implementing_type.name().to_string()
                    } else {
                        // For generic implementations like impl Display for List<T>
                        format!(
                            "{}[{}]",
                            implementing_type.name(),
                            implementing_args
                                .iter()
                                .map(Self::type_to_string)
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    };

                    return Err(ConstraintError::MissingImplementation {
                        type_name,
                        protocol_name: protocol.as_str().to_string(),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    });
                }

                Ok(())
            }

            SelfBindingContext::ProtocolDefinition { protocol_name, .. } => {
                // In protocol definition context, Self: Protocol is valid if:
                // 1. The protocol is the same as the defining protocol (trivially true)
                // 2. The protocol is a super-protocol of the defining protocol
                if protocol_name == protocol {
                    // Self: SameProtocol is trivially satisfied
                    Ok(())
                } else {
                    // Check if there's a protocol hierarchy relationship
                    // For now, assume no super-protocols (this would be extended in a full implementation)
                    Err(ConstraintError::MissingImplementation {
                        type_name: "Self".to_string(),
                        protocol_name: protocol.as_str().to_string(),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    })
                }
            }

            SelfBindingContext::FunctionContext { parent_context, .. } => {
                // Delegate to parent context
                self.solve_self_implements_constraint(parent_context, protocol, span)
            }
        }
    }

    /// Solve a Self binding constraint: Self = SomeType
    fn solve_self_binding_constraint(
        &mut self,
        mut self_context: &SelfBindingContext,
        bound_type: &Type,
        substitution: &Substitution,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), ConstraintError> {
        // Apply substitution to the bound type
        let resolved_bound_type = substitution.apply(bound_type);

        // Iteratively follow parent contexts to avoid recursion
        loop {
            match self_context {
                SelfBindingContext::Implementation {
                    implementing_type,
                    implementing_args,
                    ..
                } => {
                    // Self is already bound to the implementing type - check consistency
                    let expected_self = Type::Concrete {
                        id: implementing_type.clone(),
                        args: implementing_args.clone(),
                        span: None,
                    };

                    if expected_self != resolved_bound_type {
                        return Err(ConstraintError::Unsatisfiable {
                            constraint: format!("Self = {resolved_bound_type}"),
                            span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                        });
                    }

                    return Ok(());
                }

                SelfBindingContext::ProtocolDefinition { .. } => {
                    // In protocol definition, Self binding constraints establish requirements
                    // for implementers. For now, accept any binding (would need more sophisticated
                    // validation in a full implementation)
                    return Ok(());
                }

                SelfBindingContext::FunctionContext { parent_context, .. } => {
                    // Continue with parent context
                    self_context = parent_context;
                }
            }
        }
    }

    /// Check for constraint conflicts (currently minimal - no negative constraints in Outrun)
    pub fn check_conflicts(&mut self, _constraints: &[Constraint]) -> Result<(), ConstraintError> {
        // In Outrun's constraint system, we only have positive protocol constraints
        // (T: Display, T: Debug, etc.) with no negation operators, so direct conflicts
        // between constraints are not possible at the constraint level.
        //
        // Conflicts arise only when constraints cannot be satisfied by available
        // implementations, which is handled during constraint solving.
        //
        // Future: Could detect impossible constraint combinations like requiring
        // mutually exclusive protocols, but this would need semantic analysis
        // of protocol relationships.

        Ok(())
    }

    /// Solve a name binding constraint: TypeVar must resolve to specific named type
    fn solve_name_binding_constraint(
        &mut self,
        type_var: TypeVarId,
        expected_name: &str,
        _expected_args: &[Type],
        substitution: &Substitution,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), ConstraintError> {
        // Apply substitution to resolve the type variable
        let resolved_type = self.resolve_type_var(type_var, substitution)?;

        match resolved_type {
            Type::Variable { .. } => {
                // Type variable still unbound - constraint cannot be verified yet
                // This is not necessarily an error; it may be resolved later
                Ok(())
            }

            Type::Concrete { name, .. } => {
                // Check if the concrete type name matches the expected name
                if name.as_str() == expected_name {
                    Ok(())
                } else {
                    Err(ConstraintError::Unsatisfiable {
                        constraint: format!(
                            "type variable must resolve to {}, but got {}",
                            expected_name,
                            name.as_str()
                        ),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    })
                }
            }

            Type::Protocol { name, .. } => {
                // Check if the protocol name matches the expected name
                if name.as_str() == expected_name {
                    Ok(())
                } else {
                    Err(ConstraintError::Unsatisfiable {
                        constraint: format!(
                            "type variable must resolve to {}, but got protocol {}",
                            expected_name, name.as_str()
                        ),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    })
                }
            }

            Type::SelfType { .. } => {
                // Self types would need to be resolved first
                Err(ConstraintError::Unsatisfiable {
                    constraint: format!(
                        "type variable must resolve to {}, but got unresolved Self type",
                        expected_name
                    ),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                })
            }

            Type::Function { .. } => {
                // Function types don't match simple names
                Err(ConstraintError::Unsatisfiable {
                    constraint: format!(
                        "type variable must resolve to {}, but got function type",
                        expected_name
                    ),
                    span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                })
            }
        }
    }

    /// Clear the solution cache (useful when implementation context changes)
    pub fn clear_cache(&mut self) {
        self.solution_cache.clear();
    }

    /// Analyze protocol implementations to detect recursive patterns
    /// Should be called after all protocol implementations are registered
    pub fn analyze_recursive_type_patterns(&mut self) {
        let mut recursive_patterns = HashMap::new();
        let mut patterns_to_warn = Vec::new();

        // First pass: collect all recursive patterns (immutable borrow)
        for impl_info in self.type_registry.all_implementations() {
            if let Some(recursive_info) = self.detect_recursive_pattern(&impl_info) {
                recursive_patterns
                    .insert(recursive_info.pattern_key.clone(), recursive_info.clone());
                patterns_to_warn.push(recursive_info);
            }
        }

        // Second pass: emit warnings (mutable borrow)
        for pattern in patterns_to_warn {
            self.emit_recursive_implementation_warning(&pattern);
        }

        self.recursive_patterns = recursive_patterns;
    }

    /// Detect if an implementation creates a recursive pattern
    /// Example: impl Empty for Option<T> when T: Empty
    /// Pattern: Option<T> implements Empty, constraint requires T: Empty
    /// This creates: Option<X> -> Option<Option<X>> -> Option<Option<Option<X>>> -> ...
    fn detect_recursive_pattern(
        &self,
        impl_info: &crate::registry::ImplementationInfo,
    ) -> Option<RecursivePatternInfo> {
        // We need to check if this implementation has constraints that could create recursion
        // Since we don't have direct access to constraints in ImplementationInfo,
        // we'll need to infer recursion from the type structure

        // For now, detect simple cases where a generic type implements a protocol
        // and the same generic type could appear in constraints

        // Check if implementing type is generic and could recurse
        if !impl_info.implementing_args.is_empty() {
            // This is a generic implementation like Option<T>
            // Check if the protocol could be implemented by the same generic pattern

            // Simple heuristic: if we have Option<T> implementing Protocol,
            // and Protocol could theoretically be implemented by Option<U>,
            // then this could create recursion

            let pattern_key = format!(
                "{}:{}",
                impl_info.protocol_name.as_str(),
                impl_info.implementing_type.as_str()
            );

            // Check if this protocol has other implementations that could chain
            let mut could_recurse = false;
            for other_impl in self.type_registry.all_implementations() {
                if other_impl.protocol_id == impl_info.protocol_id
                    && other_impl.implementing_type.name() != impl_info.implementing_type.name()
                {
                    // Different type implementing same protocol - potential for chaining
                    could_recurse = true;
                    break;
                }
            }

            if could_recurse {
                return Some(RecursivePatternInfo {
                    pattern_key,
                    implementing_type: Type::Concrete {
                        id: impl_info.implementing_type.clone(),
                        args: impl_info.implementing_args.clone(),
                        span: impl_info.span,
                    },
                    protocol: impl_info.protocol_id.clone(),
                    recursion_depth: None, // Will be calculated during resolution
                    warning_emitted: false,
                    implementation_span: impl_info.span,
                });
            }
        }

        None
    }

    /// Generate and store a warning about recursive implementation patterns
    fn emit_recursive_implementation_warning(&mut self, pattern: &RecursivePatternInfo) {
        let type_name = self.get_type_name(&pattern.implementing_type);
        let explanation = format!(
            "Implementation creates recursive pattern: {} -> {} -> {} -> ...",
            type_name,
            self.expand_recursive_pattern(pattern, 1),
            self.expand_recursive_pattern(pattern, 2)
        );

        let warning = CompilerWarning::RecursiveProtocolImplementation {
            implementation_span: pattern.implementation_span,
            protocol_name: pattern.protocol.as_str().to_string(),
            implementing_type: type_name,
            explanation,
            impact: "This may generate many concrete type combinations during clause generation"
                .to_string(),
            suggestions: vec![
                "Consider adding explicit depth bounds in constraints".to_string(),
                "Consider using sealed trait pattern to limit recursion".to_string(),
                "Monitor compilation warnings for excessive clause generation".to_string(),
            ],
        };

        self.warnings.push(warning);
    }

    /// Generate an example of how the recursive pattern expands at given depth
    fn expand_recursive_pattern(&self, pattern: &RecursivePatternInfo, depth: usize) -> String {
        // Simple expansion for demonstration
        // In real implementation, this would be more sophisticated
        let base_name = self.get_type_name(&pattern.implementing_type);

        let mut result = base_name.clone();
        for _ in 0..depth {
            result = format!("{}[{}]", base_name, result);
        }
        result
    }

    /// Extract a name from a Type for display purposes
    fn get_type_name(&self, ty: &Type) -> String {
        match ty {
            Type::Concrete { name, .. } => name.as_str().to_string(),
            Type::Protocol { name, .. } => name.as_str().to_string(),
            Type::Variable {
                name: Some(name), ..
            } => name.clone(),
            Type::Variable { var_id, .. } => format!("T{}", var_id.0),
            Type::SelfType { .. } => "Self".to_string(),
            Type::Function { .. } => "Function".to_string(),
        }
    }

    /// Add a warning to the collection
    pub fn add_warning(&mut self, warning: CompilerWarning) {
        self.warnings.push(warning);
    }

    /// Get all compiler warnings collected during constraint solving
    pub fn get_warnings(&self) -> &[CompilerWarning] {
        &self.warnings
    }

    /// Set call stack context for backtracking support
    pub fn set_call_stack_context(&mut self, context: CallStackContext) {
        self.call_stack_context = Some(context);
    }

    /// Get current call stack context
    pub fn call_stack_context(&self) -> Option<&CallStackContext> {
        self.call_stack_context.as_ref()
    }

    /// Push a new call context onto the call stack
    pub fn push_call_context(&mut self, context: CallContext) {
        if let Some(ref mut stack_context) = self.call_stack_context {
            stack_context.call_stack.push(context);
        } else {
            // Initialize call stack if it doesn't exist
            self.call_stack_context = Some(CallStackContext {
                call_stack: vec![context],
                backtracked_constraints: Vec::new(),
                max_depth: self.max_backtrack_depth,
            });
        }
    }

    /// Pop the most recent call context from the call stack
    pub fn pop_call_context(&mut self) -> Option<CallContext> {
        self.call_stack_context
            .as_mut()
            .and_then(|ctx| ctx.call_stack.pop())
    }

    /// Perform call stack backtracking to find additional type constraints
    pub fn backtrack_for_enhanced_context(
        &mut self,
        constraint: &Constraint,
    ) -> Vec<BacktrackedConstraint> {
        let mut backtracked_constraints = Vec::new();

        if let Some(stack_context) = &self.call_stack_context {
            // Walk backwards through the call stack to find additional constraints
            for (depth, call_context) in stack_context.call_stack.iter().rev().enumerate() {
                if depth >= self.max_backtrack_depth {
                    break; // Prevent infinite backtracking
                }

                // Analyze this call context for additional type information
                let discovered_types =
                    self.analyze_call_context_for_types(call_context, constraint);

                if !discovered_types.is_empty() {
                    let backtracked_constraint = BacktrackedConstraint {
                        original_constraint: constraint.clone(),
                        discovered_types,
                        source_context: call_context.clone(),
                        confidence: self.calculate_constraint_confidence(call_context, depth),
                    };

                    backtracked_constraints.push(backtracked_constraint);
                }
            }
        }

        // Store the backtracked constraints for future use
        if let Some(ref mut stack_context) = self.call_stack_context {
            stack_context
                .backtracked_constraints
                .extend(backtracked_constraints.clone());
        }

        backtracked_constraints
    }

    /// Analyze a call context to extract additional type information
    fn analyze_call_context_for_types(
        &self,
        call_context: &CallContext,
        constraint: &Constraint,
    ) -> Vec<Type> {
        let mut discovered_types = Vec::new();

        // Extract type information from local constraints at this call site
        for local_constraint in &call_context.local_constraints {
            if self.constraint_is_relevant(constraint, local_constraint) {
                discovered_types.push(local_constraint.clone());
            }
        }

        // Apply generic substitutions to discover concrete types
        for (param_name, concrete_type) in &call_context.generic_substitutions {
            if self.generic_parameter_is_relevant(constraint, param_name) {
                discovered_types.push(concrete_type.clone());
            }
        }

        discovered_types
    }

    /// Check if a local constraint is relevant to the target constraint
    fn constraint_is_relevant(
        &self,
        target_constraint: &Constraint,
        local_constraint: &Type,
    ) -> bool {
        match target_constraint {
            Constraint::Implements {
                type_var, protocol, ..
            } => {
                // Check if local constraint provides information about this type variable or protocol
                self.type_involves_variable(local_constraint, *type_var)
                    || self.type_involves_protocol(local_constraint, protocol)
            }
            Constraint::SelfImplements { protocol, .. } => {
                // Check if local constraint provides information about Self or this protocol
                self.type_involves_self(local_constraint)
                    || self.type_involves_protocol(local_constraint, protocol)
            }
            Constraint::Equality { left, right, .. } => {
                // Check if local constraint involves either side of the equality
                self.types_overlap(local_constraint, left)
                    || self.types_overlap(local_constraint, right)
            }
            _ => false, // Other constraint types don't benefit from backtracking yet
        }
    }

    /// Check if a generic parameter is relevant to the target constraint
    fn generic_parameter_is_relevant(
        &self,
        target_constraint: &Constraint,
        _param_name: &str,
    ) -> bool {
        match target_constraint {
            Constraint::Implements { .. } => {
                // Generic parameters could provide concrete type information
                true // Conservative approach - consider all generic parameters relevant
            }
            _ => false,
        }
    }

    /// Calculate confidence level for a backtracked constraint
    fn calculate_constraint_confidence(&self, call_context: &CallContext, depth: usize) -> f64 {
        // Higher confidence for shallower call stack depths
        let depth_penalty = depth as f64 * 0.1;

        // Higher confidence for contexts with more local constraints
        let constraint_bonus = (call_context.local_constraints.len() as f64 * 0.1).min(0.3);

        // Higher confidence for contexts with generic substitutions
        let substitution_bonus = if call_context.generic_substitutions.is_empty() {
            0.0
        } else {
            0.2
        };

        // Base confidence starts at 0.8, adjusted by factors
        (0.8 - depth_penalty + constraint_bonus + substitution_bonus).clamp(0.0, 1.0)
    }

    /// Helper methods for type analysis
    fn type_involves_variable(&self, ty: &Type, var_id: TypeVarId) -> bool {
        match ty {
            Type::Variable {
                var_id: ty_var_id, ..
            } => *ty_var_id == var_id,
            Type::Concrete { args, .. } => args
                .iter()
                .any(|arg| self.type_involves_variable(arg, var_id)),
            Type::Protocol { args, .. } => args
                .iter()
                .any(|arg| self.type_involves_variable(arg, var_id)),
            Type::Function {
                params,
                return_type,
                ..
            } => {
                params
                    .iter()
                    .any(|(_, param_type)| self.type_involves_variable(param_type, var_id))
                    || self.type_involves_variable(return_type, var_id)
            }
            _ => false,
        }
    }

    fn type_involves_protocol(&self, ty: &Type, protocol_id: &crate::types::ModuleName) -> bool {
        match ty {
            Type::Protocol { name, args, .. } => {
                // Check both if this is the target protocol and if args contain the protocol
                name == protocol_id
                    || args
                        .iter()
                        .any(|arg| self.type_involves_protocol(arg, protocol_id))
            }
            Type::Concrete { args, .. } => args
                .iter()
                .any(|arg| self.type_involves_protocol(arg, protocol_id)),
            Type::Function {
                params,
                return_type,
                ..
            } => {
                params
                    .iter()
                    .any(|(_, param_type)| self.type_involves_protocol(param_type, protocol_id))
                    || self.type_involves_protocol(return_type, protocol_id)
            }
            _ => false,
        }
    }

    fn type_involves_self(&self, ty: &Type) -> bool {
        match ty {
            Type::SelfType { .. } => true,
            Type::Concrete { args, .. } => args.iter().any(|arg| self.type_involves_self(arg)),
            Type::Protocol { args, .. } => args.iter().any(|arg| self.type_involves_self(arg)),
            Type::Function {
                params,
                return_type,
                ..
            } => {
                params
                    .iter()
                    .any(|(_, param_type)| self.type_involves_self(param_type))
                    || self.type_involves_self(return_type)
            }
            _ => false,
        }
    }

    fn types_overlap(&self, type1: &Type, type2: &Type) -> bool {
        // Simple overlap check - in a full implementation this would be more sophisticated
        match (type1, type2) {
            (Type::Concrete { name: name1, .. }, Type::Concrete { name: name2, .. }) => name1 == name2,
            (Type::Protocol { name: name1, .. }, Type::Protocol { name: name2, .. }) => name1 == name2,
            (Type::Variable { var_id: id1, .. }, Type::Variable { var_id: id2, .. }) => id1 == id2,
            _ => false, // Conservative approach
        }
    }

    /// Set the current package name for template generation
    pub fn set_package_name(&mut self, package_name: String) {
        self.current_package_name = package_name;
    }

    /// Generate a public function template from a function definition
    pub fn generate_public_function_template(
        &mut self,
        function_signature: crate::universal_dispatch::FunctionSignature,
        function_def: &outrun_parser::FunctionDefinition,
        visibility: FunctionVisibility,
        available_generic_params: &[String], // Generic parameters from containing struct/protocol/impl
    ) -> Result<PublicFunctionTemplate, String> {
        // Extract generic parameters from the function definition using available generics
        let generic_parameters =
            self.extract_generic_parameters_from_function(function_def, available_generic_params)?;

        // Convert parameter types to templates
        let parameter_types = self
            .convert_parameters_to_templates(&function_def.parameters, available_generic_params)?;

        // Convert return type to template
        let return_type =
            self.convert_type_to_template(&function_def.return_type, available_generic_params)?;

        // Extract protocol constraints (simplified for now)
        let protocol_constraints = Vec::new(); // TODO: Extract protocol constraints from function_def

        let template = PublicFunctionTemplate {
            function_signature: function_signature.clone(),
            generic_parameters,
            parameter_types,
            return_type,
            protocol_constraints,
            visibility,
            span: Some(function_def.span),
            source_package: self.current_package_name.clone(),
            generated_at: std::time::SystemTime::now(),
        };

        // Store the template
        self.public_function_templates
            .insert(function_signature, template.clone());

        Ok(template)
    }

    /// Extract generic parameters from a function definition
    fn extract_generic_parameters_from_function(
        &self,
        function_def: &outrun_parser::FunctionDefinition,
        available_generic_params: &[String],
    ) -> Result<Vec<GenericParameter>, String> {
        let mut parameters = Vec::new();
        let mut seen_names = std::collections::HashSet::new();

        // Collect generic parameters from parameter types
        for param in &function_def.parameters {
            self.collect_generic_parameters_from_type(
                &param.type_annotation,
                &mut parameters,
                &mut seen_names,
                available_generic_params,
            )?;
        }

        // Collect generic parameters from return type
        self.collect_generic_parameters_from_type(
            &function_def.return_type,
            &mut parameters,
            &mut seen_names,
            available_generic_params,
        )?;

        Ok(parameters)
    }

    /// Recursively collect generic parameters from a type annotation
    fn collect_generic_parameters_from_type(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
        parameters: &mut Vec<GenericParameter>,
        seen_names: &mut std::collections::HashSet<String>,
        available_generic_params: &[String],
    ) -> Result<(), String> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                // Check the last component of the path for generic parameter names
                if let Some(last_component) = path.last() {
                    let name = &last_component.name;
                    if available_generic_params.contains(name) && !seen_names.contains(name) {
                        seen_names.insert(name.clone());
                        parameters.push(GenericParameter {
                            name: name.clone(),
                            direct_constraints: Vec::new(), // Will be filled by constraint extraction
                            multiple_occurrences: false,    // Will be updated if we see it again
                            variance: ParameterVariance::Unknown,
                        });
                    } else if seen_names.contains(name) {
                        // Mark as multiple occurrences
                        for param in parameters.iter_mut() {
                            if param.name == *name {
                                param.multiple_occurrences = true;
                                break;
                            }
                        }
                    }
                }

                // Recursively process generic arguments
                if let Some(args) = generic_args {
                    for arg in &args.args {
                        self.collect_generic_parameters_from_type(
                            arg,
                            parameters,
                            seen_names,
                            available_generic_params,
                        )?;
                    }
                }
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                // Process function parameter types
                for param in params {
                    self.collect_generic_parameters_from_type(
                        &param.type_annotation,
                        parameters,
                        seen_names,
                        available_generic_params,
                    )?;
                }
                // Process return type
                self.collect_generic_parameters_from_type(
                    return_type,
                    parameters,
                    seen_names,
                    available_generic_params,
                )?;
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                // Process tuple element types
                for element_type in types {
                    self.collect_generic_parameters_from_type(
                        element_type,
                        parameters,
                        seen_names,
                        available_generic_params,
                    )?;
                }
            }
        }

        Ok(())
    }

    /// Convert function parameters to parameter templates
    fn convert_parameters_to_templates(
        &self,
        params: &[outrun_parser::Parameter],
        available_generic_params: &[String],
    ) -> Result<Vec<ParameterTemplate>, String> {
        let mut templates = Vec::new();

        for param in params {
            let type_template =
                self.convert_type_to_template(&param.type_annotation, available_generic_params)?;
            templates.push(ParameterTemplate {
                name: param.name.name.clone(),
                type_template,
                required: true, // All parameters are required in Outrun
            });
        }

        Ok(templates)
    }

    /// Convert a type annotation to a type template
    fn convert_type_to_template(
        &self,
        type_annotation: &outrun_parser::TypeAnnotation,
        available_generic_params: &[String],
    ) -> Result<TypeTemplate, String> {
        match type_annotation {
            outrun_parser::TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                if let Some(last_component) = path.last() {
                    let name = &last_component.name;
                    if available_generic_params.contains(name) {
                        Ok(TypeTemplate::Generic {
                            parameter_name: name.clone(),
                            constraints: Vec::new(), // Will be filled by constraint extraction
                        })
                    } else {
                        // Convert generic arguments
                        let generic_args = if let Some(args) = generic_args {
                            let mut templates = Vec::new();
                            for arg in &args.args {
                                templates.push(
                                    self.convert_type_to_template(arg, available_generic_params)?,
                                );
                            }
                            templates
                        } else {
                            Vec::new()
                        };

                        // Use the full qualified path for concrete types
                        let full_type_name = path
                            .iter()
                            .map(|component| component.name.clone())
                            .collect::<Vec<_>>()
                            .join(".");

                        Ok(TypeTemplate::Concrete {
                            type_name: full_type_name,
                            generic_args,
                        })
                    }
                } else {
                    Err("Empty type path".to_string())
                }
            }
            outrun_parser::TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                let mut parameter_templates = Vec::new();
                for param in params {
                    parameter_templates.push(self.convert_type_to_template(
                        &param.type_annotation,
                        available_generic_params,
                    )?);
                }

                let return_template =
                    Box::new(self.convert_type_to_template(return_type, available_generic_params)?);

                Ok(TypeTemplate::Function {
                    parameter_templates,
                    return_template,
                })
            }
            outrun_parser::TypeAnnotation::Tuple { types, .. } => {
                let mut element_templates = Vec::new();
                for element_type in types {
                    element_templates.push(
                        self.convert_type_to_template(element_type, available_generic_params)?,
                    );
                }

                Ok(TypeTemplate::Concrete {
                    type_name: "Tuple".to_string(),
                    generic_args: element_templates,
                })
            }
        }
    }

    /// Extract protocol constraints from a function signature
    fn extract_protocol_constraints(
        &self,
        _signature: &outrun_parser::FunctionSignature,
    ) -> Result<Vec<ProtocolConstraintTemplate>, String> {
        let constraints = Vec::new();

        // For now, this is a simplified implementation
        // In a full implementation, this would parse constraint clauses
        // like "where T: Display + Clone"

        // TODO: Parse constraint expressions from function signature
        // This would involve extending the parser to capture constraint clauses

        Ok(constraints)
    }

    /// Get all generated public function templates
    pub fn get_public_function_templates(
        &self,
    ) -> &HashMap<crate::universal_dispatch::FunctionSignature, PublicFunctionTemplate> {
        &self.public_function_templates
    }

    /// Get a specific public function template
    pub fn get_public_function_template(
        &self,
        signature: &crate::universal_dispatch::FunctionSignature,
    ) -> Option<&PublicFunctionTemplate> {
        self.public_function_templates.get(signature)
    }

    /// Import public function templates from a dependency package
    pub fn import_dependency_templates(
        &mut self,
        dependency_templates: &std::collections::HashMap<
            crate::universal_dispatch::FunctionSignature,
            PublicFunctionTemplate,
        >,
        dependency_package: &str,
    ) -> Result<usize, String> {
        let mut imported_count = 0;

        for (signature, template) in dependency_templates {
            // CRITICAL: Only import PUBLIC function templates - private functions must not be accessible
            if template.visibility != FunctionVisibility::Public {
                continue; // Skip private functions - they should never be accessible to dependent packages
            }

            // Only import templates that we don't already have
            if !self.public_function_templates.contains_key(signature) {
                // Create a copy of the template with dependency package info
                let mut imported_template = template.clone();
                imported_template.source_package = dependency_package.to_string();

                self.public_function_templates
                    .insert(signature.clone(), imported_template);
                imported_count += 1;
            }
        }

        Ok(imported_count)
    }

    /// Generate a function clause from a public function template with concrete type substitutions
    pub fn generate_clause_from_template(
        &self,
        template: &PublicFunctionTemplate,
        concrete_type_substitutions: &std::collections::HashMap<String, String>, // generic param -> concrete type
    ) -> Result<crate::universal_dispatch::ClauseInfo, String> {
        // Substitute generic parameters with concrete types in all templates
        let substituted_parameter_types = self.substitute_types_in_parameters(
            &template.parameter_types,
            concrete_type_substitutions,
        )?;

        let substituted_return_type = self.substitute_types_in_type_template(
            &template.return_type,
            concrete_type_substitutions,
        )?;

        // Generate type compatibility guards for the substituted types
        let mut guards = Vec::new();
        for (i, param_template) in substituted_parameter_types.iter().enumerate() {
            if let TypeTemplate::Concrete { type_name, .. } = &param_template.type_template {
                guards.push(crate::universal_dispatch::Guard::TypeCompatible {
                    target_type: self.parse_concrete_type_name(type_name)?,
                    implementing_type: self.parse_concrete_type_name(type_name)?,
                    constraint_context: crate::universal_dispatch::ConstraintContext::new(),
                });
            }
        }

        // If no specific guards were needed, add AlwaysTrue
        if guards.is_empty() {
            guards.push(crate::universal_dispatch::Guard::AlwaysTrue);
        }

        // Create the clause with intrinsic function body (simplified for now)
        let clause = crate::universal_dispatch::ClauseInfo {
            clause_id: crate::universal_dispatch::ClauseId::new(),
            function_signature: template.function_signature.clone(),
            guards,
            body: crate::universal_dispatch::FunctionBody::IntrinsicFunction(format!(
                "{}.{}",
                template.source_package, template.function_signature.function_name
            )),
            estimated_cost: 1,
            priority: 0,
            span: template.span,
        };

        Ok(clause)
    }

    /// Substitute generic types with concrete types in parameter templates
    fn substitute_types_in_parameters(
        &self,
        parameters: &[ParameterTemplate],
        substitutions: &std::collections::HashMap<String, String>,
    ) -> Result<Vec<ParameterTemplate>, String> {
        let mut substituted_params = Vec::new();

        for param in parameters {
            let substituted_type =
                self.substitute_types_in_type_template(&param.type_template, substitutions)?;

            substituted_params.push(ParameterTemplate {
                name: param.name.clone(),
                type_template: substituted_type,
                required: param.required,
            });
        }

        Ok(substituted_params)
    }

    /// Substitute generic types with concrete types in a type template
    fn substitute_types_in_type_template(
        &self,
        type_template: &TypeTemplate,
        substitutions: &std::collections::HashMap<String, String>,
    ) -> Result<TypeTemplate, String> {
        match type_template {
            TypeTemplate::Generic { parameter_name, .. } => {
                if let Some(concrete_type) = substitutions.get(parameter_name) {
                    Ok(TypeTemplate::Concrete {
                        type_name: concrete_type.clone(),
                        generic_args: Vec::new(),
                    })
                } else {
                    Err(format!(
                        "No concrete type provided for generic parameter '{}'",
                        parameter_name
                    ))
                }
            }
            TypeTemplate::Concrete {
                type_name,
                generic_args,
            } => {
                // Recursively substitute in generic arguments
                let mut substituted_args = Vec::new();
                for arg in generic_args {
                    substituted_args
                        .push(self.substitute_types_in_type_template(arg, substitutions)?);
                }

                Ok(TypeTemplate::Concrete {
                    type_name: type_name.clone(),
                    generic_args: substituted_args,
                })
            }
            TypeTemplate::Protocol {
                protocol_name,
                generic_args,
            } => {
                // Recursively substitute in generic arguments
                let mut substituted_args = Vec::new();
                for arg in generic_args {
                    substituted_args
                        .push(self.substitute_types_in_type_template(arg, substitutions)?);
                }

                Ok(TypeTemplate::Protocol {
                    protocol_name: protocol_name.clone(),
                    generic_args: substituted_args,
                })
            }
            TypeTemplate::SelfType { context } => {
                // Self types need special handling - for now, keep as-is
                Ok(TypeTemplate::SelfType {
                    context: context.clone(),
                })
            }
            TypeTemplate::Function {
                parameter_templates,
                return_template,
            } => {
                // Recursively substitute in function parameters and return type
                let mut substituted_params = Vec::new();
                for param in parameter_templates {
                    substituted_params
                        .push(self.substitute_types_in_type_template(param, substitutions)?);
                }

                let substituted_return = Box::new(
                    self.substitute_types_in_type_template(return_template, substitutions)?,
                );

                Ok(TypeTemplate::Function {
                    parameter_templates: substituted_params,
                    return_template: substituted_return,
                })
            }
        }
    }

    /// Parse a concrete type name into a Type (simplified implementation)
    fn parse_concrete_type_name(&self, type_name: &str) -> Result<crate::types::Type, String> {
        // For now, create simple concrete types
        // In a full implementation, this would parse the qualified name properly
        Ok(crate::types::Type::concrete(type_name))
    }

    /// Clear all generated templates (for testing)
    pub fn clear_templates(&mut self) {
        self.public_function_templates.clear();
    }
}

impl Default for ConstraintSolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Level, TypeId, TypeVarGenerator};

    fn create_test_registry() -> ProtocolRegistry {
        let mut registry = ProtocolRegistry::new();

        // Set up local modules for testing
        registry.add_local_module(crate::types::ModuleId::new("Integer"));
        registry.add_local_module(crate::types::ModuleId::new("String"));
        registry.add_local_module(crate::types::ModuleId::new("Display"));
        registry.add_local_module(crate::types::ModuleId::new("Debug"));
        registry.add_local_module(crate::types::ModuleId::new("Clone"));
        registry.add_local_module(crate::types::ModuleId::new("TestModule"));
        registry.set_current_module(crate::types::ModuleId::new("TestModule"));

        // Register some basic implementations for testing
        registry
            .register_implementation(
                TypeId::new("Integer"),
                vec![],
                ModuleName::new("Display"),
                vec![],
                crate::types::ModuleId::new("Integer"),
                None,
            )
            .unwrap();

        registry
            .register_implementation(
                TypeId::new("String"),
                vec![],
                ModuleName::new("Display"),
                vec![],
                crate::types::ModuleId::new("String"),
                None,
            )
            .unwrap();

        registry
            .register_implementation(
                TypeId::new("Integer"),
                vec![],
                ModuleName::new("Debug"),
                vec![],
                crate::types::ModuleId::new("Integer"),
                None,
            )
            .unwrap();

        registry
            .register_implementation(
                TypeId::new("String"),
                vec![],
                ModuleName::new("Debug"),
                vec![],
                crate::types::ModuleId::new("String"),
                None,
            )
            .unwrap();

        registry
            .register_implementation(
                TypeId::new("String"),
                vec![],
                ModuleName::new("Clone"),
                vec![],
                crate::types::ModuleId::new("String"),
                None,
            )
            .unwrap();

        registry
    }

    #[test]
    fn test_simple_implements_constraint_satisfied() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("Display"),
                span: None,
            };

            // Create substitution T -> String
            let mut substitution = Substitution::new();
            substitution.insert(var_id, Type::concrete("String"));

            let result = solver.solve(&[constraint], &substitution);
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_simple_implements_constraint_unsatisfied() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: NonExistentProtocol
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("NonExistentProtocol"),
                span: None,
            };

            // Create substitution T -> String
            let mut substitution = Substitution::new();
            substitution.insert(var_id, Type::concrete("String"));

            let result = solver.solve(&[constraint], &substitution);
            assert!(result.is_err());

            if let Err(ConstraintError::MissingImplementation { .. }) = result {
                // Expected error
            } else {
                panic!("Expected MissingImplementation error");
            }
        }
    }

    #[test]
    fn test_logical_or_constraint_satisfied() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display || T: NonExistentProtocol
            let left = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("Display"),
                span: None,
            };
            let right = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("NonExistentProtocol"),
                span: None,
            };
            let constraint = Constraint::LogicalOr {
                left: Box::new(left),
                right: Box::new(right),
                span: None,
            };

            // Create substitution T -> String (String implements Display)
            let mut substitution = Substitution::new();
            substitution.insert(var_id, Type::concrete("String"));

            let result = solver.solve(&[constraint], &substitution);
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_logical_or_constraint_unsatisfied() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: NonExistent1 || T: NonExistent2
            let left = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("NonExistent1"),
                span: None,
            };
            let right = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("NonExistent2"),
                span: None,
            };
            let constraint = Constraint::LogicalOr {
                left: Box::new(left),
                right: Box::new(right),
                span: None,
            };

            // Create substitution T -> String
            let mut substitution = Substitution::new();
            substitution.insert(var_id, Type::concrete("String"));

            let result = solver.solve(&[constraint], &substitution);
            assert!(result.is_err());

            if let Err(ConstraintError::Unsatisfiable { .. }) = result {
                // Expected error
            } else {
                panic!("Expected Unsatisfiable error");
            }
        }
    }

    #[test]
    fn test_logical_and_constraint_satisfied() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display && T: Debug
            let left = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("Display"),
                span: None,
            };
            let right = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("Debug"),
                span: None,
            };
            let constraint = Constraint::LogicalAnd {
                left: Box::new(left),
                right: Box::new(right),
                span: None,
            };

            // Create substitution T -> String (String implements both Display and Debug)
            let mut substitution = Substitution::new();
            substitution.insert(var_id, Type::concrete("String"));

            let result = solver.solve(&[constraint], &substitution);
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_logical_and_constraint_unsatisfied() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display && T: NonExistent
            let left = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("Display"),
                span: None,
            };
            let right = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("NonExistent"),
                span: None,
            };
            let constraint = Constraint::LogicalAnd {
                left: Box::new(left),
                right: Box::new(right),
                span: None,
            };

            // Create substitution T -> String
            let mut substitution = Substitution::new();
            substitution.insert(var_id, Type::concrete("String"));

            let result = solver.solve(&[constraint], &substitution);
            assert!(result.is_err());

            if let Err(ConstraintError::MissingImplementation { .. }) = result {
                // Expected error
            } else {
                panic!("Expected MissingImplementation error");
            }
        }
    }

    #[test]
    fn test_unbound_type_variable() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("Display"),
                span: None,
            };

            // No substitution - type variable remains unbound
            let substitution = Substitution::new();

            let result = solver.solve(&[constraint], &substitution);
            // Unbound variables are not considered errors at constraint solving time
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_generic_type_constraint() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("Display"),
                span: None,
            };

            // Create substitution T -> List<String>
            let generic_type = Type::generic_concrete("List", vec![Type::concrete("String")]);
            let mut substitution = Substitution::new();
            substitution.insert(var_id, generic_type);

            let result = solver.solve(&[constraint], &substitution);
            // This should fail because List<String> is not registered as implementing Display
            assert!(result.is_err());
        }
    }

    #[test]
    fn test_protocol_registry_integration() {
        let mut registry = ProtocolRegistry::new();
        registry.add_local_module(crate::types::ModuleId::new("TestModule"));
        registry.add_local_module(crate::types::ModuleId::new("TestType"));
        registry.add_local_module(crate::types::ModuleId::new("TestProtocol"));
        registry.set_current_module(crate::types::ModuleId::new("TestModule"));

        let protocol = ModuleName::new("TestProtocol");
        let type_id = TypeId::new("TestType");

        // Initially no implementation
        assert!(!registry.has_implementation(&protocol, &type_id));

        // Register implementation
        registry
            .register_implementation(
                type_id.clone(),
                vec![],
                protocol.clone(),
                vec![],
                crate::types::ModuleId::new("TestType"),
                None,
            )
            .unwrap();

        // Now has implementation
        assert!(registry.has_implementation(&protocol, &type_id));

        // Create constraint solver with this registry
        let solver = ConstraintSolver::with_registry(registry);

        // Verify the solver can access the registry
        assert!(solver
            .protocol_registry()
            .has_implementation(&protocol, &type_id));
    }

    #[test]
    fn test_conflict_detection() {
        let mut solver = ConstraintSolver::new();
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create conflicting constraints - this is a simplified test
            // In practice, conflicts would be more complex
            let constraint1 = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("TestProtocol"),
                span: None,
            };
            let constraint2 = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("TestProtocol"),
                span: None,
            };

            let result = solver.check_conflicts(&[constraint1, constraint2]);
            // This shouldn't conflict - same constraint repeated
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_protocol_self_implementation() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ModuleName::new("Display"),
                span: None,
            };

            // Create substitution T -> Display protocol type
            let protocol_type = Type::Protocol {
                id: ModuleName::new("Display"),
                args: vec![],
                span: None,
            };
            let mut substitution = Substitution::new();
            substitution.insert(var_id, protocol_type);

            let result = solver.solve(&[constraint], &substitution);
            // Protocol types implement themselves
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_self_implements_in_implementation_context() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());

        // Create Self: Display constraint in implementation context
        let self_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("String"),
            implementing_args: vec![],
            protocol_id: ModuleName::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ModuleName::new("Display"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // String implements Display, so Self: Display should succeed
        assert!(result.is_ok());
    }

    #[test]
    fn test_self_implements_in_implementation_context_missing() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());

        // Create Self: NonExistentProtocol constraint
        let self_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("String"),
            implementing_args: vec![],
            protocol_id: ModuleName::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ModuleName::new("NonExistentProtocol"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // String doesn't implement NonExistentProtocol
        assert!(result.is_err());

        if let Err(ConstraintError::MissingImplementation { .. }) = result {
            // Expected error type
        } else {
            panic!("Expected MissingImplementation error");
        }
    }

    #[test]
    fn test_self_implements_in_protocol_context_same_protocol() {
        let mut solver = ConstraintSolver::new();

        // Create Self: Display constraint in Display protocol definition
        let self_context = SelfBindingContext::ProtocolDefinition {
            protocol_id: ModuleName::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ModuleName::new("Display"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // Self: SameProtocol is trivially satisfied
        assert!(result.is_ok());
    }

    #[test]
    fn test_self_implements_in_protocol_context_different_protocol() {
        let mut solver = ConstraintSolver::new();

        // Create Self: Debug constraint in Display protocol definition
        let self_context = SelfBindingContext::ProtocolDefinition {
            protocol_id: ModuleName::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ModuleName::new("Debug"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // Self: DifferentProtocol requires protocol hierarchy (not implemented yet)
        assert!(result.is_err());

        if let Err(ConstraintError::MissingImplementation { .. }) = result {
            // Expected error type
        } else {
            panic!("Expected MissingImplementation error");
        }
    }

    #[test]
    fn test_self_binding_constraint_consistent() {
        let mut solver = ConstraintSolver::new();

        // Create Self = String constraint in String implementation context
        let self_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("String"),
            implementing_args: vec![],
            protocol_id: ModuleName::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfBinding {
            self_context,
            bound_type: Box::new(Type::concrete("String")),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // Self = String is consistent with String implementation context
        assert!(result.is_ok());
    }

    #[test]
    fn test_self_binding_constraint_inconsistent() {
        let mut solver = ConstraintSolver::new();

        // Create Self = Integer constraint in String implementation context
        let self_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("String"),
            implementing_args: vec![],
            protocol_id: ModuleName::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfBinding {
            self_context,
            bound_type: Box::new(Type::concrete("Integer")),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // Self = Integer is inconsistent with String implementation context
        assert!(result.is_err());

        if let Err(ConstraintError::Unsatisfiable { .. }) = result {
            // Expected error type
        } else {
            panic!("Expected Unsatisfiable error");
        }
    }

    #[test]
    fn test_self_with_generic_types() {
        let mut registry = ProtocolRegistry::new();
        registry.add_local_module(crate::types::ModuleId::new("List"));
        registry.add_local_module(crate::types::ModuleId::new("Display"));
        registry.add_local_module(crate::types::ModuleId::new("TestModule"));
        registry.set_current_module(crate::types::ModuleId::new("TestModule"));
        registry
            .register_implementation(
                TypeId::new("List"),
                vec![Type::concrete("String")],
                ModuleName::new("Display"),
                vec![],
                crate::types::ModuleId::new("List"),
                None,
            )
            .unwrap();
        let mut solver = ConstraintSolver::with_registry(registry);

        // Create Self: Display constraint in List<String> implementation context
        let self_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("List"),
            implementing_args: vec![Type::concrete("String")],
            protocol_id: ModuleName::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ModuleName::new("Display"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // List<String> implements Display, so Self: Display should succeed
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_context_self_constraint() {
        let mut solver = ConstraintSolver::with_registry(create_test_registry());

        // Create Self: Display constraint in function context within String implementation
        let parent_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("String"),
            implementing_args: vec![],
            protocol_id: ModuleName::new("Display"),
            protocol_args: vec![],
        };
        let self_context = SelfBindingContext::FunctionContext {
            parent_context: Box::new(parent_context),
            function_name: Some("to_string".to_string()),
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ModuleName::new("Display"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // Should delegate to parent context and succeed
        assert!(result.is_ok());
    }
}
