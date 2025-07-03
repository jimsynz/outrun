//! SMT constraint definitions for type inference
//!
//! This module defines the constraint types that represent type relationships
//! and requirements that need to be solved by the SMT solver.

use crate::unification::StructuredType;
use outrun_parser::Span;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

/// SMT constraint representing a type relationship that must be satisfied
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SMTConstraint {
    /// Type A must implement trait B
    /// CRITICAL: Always uses StructuredType to preserve generic information
    TraitImplemented {
        impl_type: StructuredType, // ✅ Preserves generic info like Option<Integer>
        trait_type: StructuredType, // ✅ Preserves generic info like Option<T>
    },

    /// Type A must unify with type B
    TypeUnification {
        type1: StructuredType,
        type2: StructuredType,
        context: String, // For error reporting (e.g., "function call argument")
    },

    /// Generic type must be instantiated with specific concrete types
    GenericInstantiation {
        generic_type: StructuredType,             // e.g., Option<T>
        concrete_candidates: Vec<StructuredType>, // e.g., [Option<Int64>, Option<String>]
    },

    /// Function signature must match expected parameters/return type
    FunctionSignatureMatch {
        expected: FunctionSignature, // Uses StructuredType internally
        actual: FunctionSignature,   // Uses StructuredType internally
        call_site: Span,
    },

    /// Guard condition must be satisfiable
    GuardCondition {
        condition: String, // For now, use string representation until we define BooleanExpression
        variables: HashMap<String, StructuredType>,
    },

    /// Type parameter must be unified to a concrete type
    /// This handles cases like Option<T> matching Option<Integer> where T = Integer
    TypeParameterUnification {
        parameter_name: String,        // e.g., "T"
        concrete_type: StructuredType, // e.g., Integer
        context: String,               // For error reporting
    },

    /// Type variable constraint for generic parameters and Self types
    /// This replaces manual Self substitution with SMT-based constraint solving
    TypeVariableConstraint {
        variable_id: crate::compilation::compiler_environment::TypeNameId,
        bound_type: StructuredType,
        context: String, // For error reporting
    },

    /// Trait compatibility constraint - any type that implements the trait
    /// This represents "T where T: TraitName" semantics
    /// When we see a trait name in a type position, this constraint is generated
    TraitCompatibility {
        trait_type: StructuredType,        // e.g., Boolean trait
        implementing_type: StructuredType, // e.g., Outrun.Core.Boolean (what actually implements it)
        context: String,                   // For error reporting (e.g., "parameter type check")
    },

    /// Universal Self constraint for trait definitions
    /// This represents "∀ Self. (implements(Self, TraitBeingDefined) ∧ implements(Self, BoundTrait))"
    /// Used for "when Self: Equality" constraints in trait definitions
    UniversalSelfConstraint {
        self_variable_id: crate::compilation::compiler_environment::TypeNameId, // The Self type variable
        trait_being_defined: StructuredType, // The trait being defined (e.g., Binary)
        bound_traits: Vec<StructuredType>,   // Required trait implementations (e.g., [Equality])
        context: String,                     // For error reporting
    },

    /// Concrete Self binding for trait implementations
    /// This represents "Self = ConcreteType" in impl blocks
    /// Used to bind Self to the specific implementing type
    ConcreteSelfBinding {
        self_variable_id: crate::compilation::compiler_environment::TypeNameId, // The Self type variable
        concrete_type: StructuredType, // The concrete implementing type (e.g., String)
        context: String,               // For error reporting
    },

    /// Self type inference constraint for function calls
    /// This represents bidirectional inference where call arguments constrain Self
    /// Used for calls like "Option.some?(value: Option<Integer>)" to infer Self = Option<Integer>
    SelfTypeInference {
        self_variable_id: crate::compilation::compiler_environment::TypeNameId, // The Self type variable
        inferred_type: StructuredType, // The type inferred from arguments
        call_site_context: String,     // Description of the function call
        confidence: InferenceConfidence, // How confident we are in this inference
    },

    // === Function Clause Dispatch Constraints ===

    /// Argument type must match function clause parameter
    /// Used for function clause selection based on argument types
    ArgumentTypeMatch {
        clause_id: String,              // Unique identifier for the function clause
        parameter_name: String,         // Name of the parameter being matched
        expected_type: StructuredType,  // Expected parameter type from clause definition
        actual_type: StructuredType,    // Actual argument type from call site
        call_site: Span,               // Location of the function call
    },

    /// Guard expression must be applicable (side-effect free and Boolean-valued)
    /// Guards in Outrun must be pure functions that return Boolean
    GuardApplicable {
        clause_id: String,              // Unique identifier for the function clause
        guard_expression: String,       // String representation of guard (for now)
        guard_variables: std::collections::HashMap<String, StructuredType>, // Variables in scope
        context: String,                // For error reporting
    },

    /// Priority ordering between function clauses
    /// Lower priority number = higher precedence
    ClausePriority {
        clause_id: String,              // Unique identifier for the function clause  
        priority: u32,                  // Numeric priority (0 = highest)
        context: String,                // Function name and location context
    },

    /// Pre-resolved function clause selection
    /// This represents the SMT solver's decision about which clause to use for a specific call
    PreResolvedClause {
        call_site: Span,                // Location of the function call
        trait_type: StructuredType,     // Trait being called (e.g., BinaryDivision)
        impl_type: StructuredType,      // Implementation type (e.g., Integer64)
        function_name: String,          // Function name (e.g., "divide")
        selected_clause_id: String,     // SMT-selected clause ID
        guard_pre_evaluated: Option<bool>, // SMT pre-computed guard result (if applicable)
        argument_types: Vec<StructuredType>, // Call site argument types
    },

    /// Guard static evaluation result
    /// SMT pre-computed guard expression results for specific argument patterns
    GuardStaticallyEvaluated {
        clause_id: String,              // Function clause containing the guard
        guard_expression: String,       // Guard expression (e.g., "rhs == 0")
        when_arguments: std::collections::HashMap<String, String>, // Argument patterns (e.g., "rhs" -> "0")
        evaluation_result: bool,        // SMT-computed result (true/false)
        context: String,                // For debugging
    },
}

/// Confidence level for Self type inference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferenceConfidence {
    High,   // Direct parameter match (e.g., parameter is Self, argument is concrete)
    Medium, // Indirect inference (e.g., parameter is Option<Self>, argument is Option<Concrete>)
    Low,    // Complex inference with multiple steps
}

/// Function signature with named parameters (Outrun requirement)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    pub params: Vec<FunctionParam>, // FunctionParam.param_type is StructuredType
    pub return_type: StructuredType, // ✅ Not TypeNameId - preserves generics
}

/// Function parameter with name and type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionParam {
    pub name: String,
    pub param_type: StructuredType,
}

/// Set of constraints with metadata
#[derive(Debug, Clone)]
pub struct ConstraintSet {
    pub constraints: Vec<SMTConstraint>,
    pub context: ConstraintContext,
    pub priority: ConstraintPriority,
}

/// Context information for where constraints were generated
#[derive(Debug, Clone, PartialEq)]
pub struct ConstraintContext {
    pub location: String, // e.g., "function call at line 42"
    pub function_name: Option<String>,
    pub module_name: Option<String>,
}

/// Priority level for constraint solving
#[derive(Debug, Clone, PartialEq)]
pub enum ConstraintPriority {
    Critical,  // Must solve for compilation to succeed
    Important, // Affects dispatch optimization
    Optional,  // For enhanced error messages
}

/// Hash implementation for constraint sets (for caching)
impl Hash for ConstraintSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash based on constraint contents, not metadata
        for constraint in &self.constraints {
            constraint.hash(state);
        }
    }
}

/// Hash implementation for SMT constraints
impl Hash for SMTConstraint {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            SMTConstraint::TraitImplemented {
                impl_type,
                trait_type,
            } => {
                0u8.hash(state);
                impl_type.hash(state);
                trait_type.hash(state);
            }
            SMTConstraint::TypeUnification {
                type1,
                type2,
                context,
            } => {
                1u8.hash(state);
                type1.hash(state);
                type2.hash(state);
                context.hash(state);
            }
            SMTConstraint::GenericInstantiation {
                generic_type,
                concrete_candidates,
            } => {
                2u8.hash(state);
                generic_type.hash(state);
                concrete_candidates.hash(state);
            }
            SMTConstraint::FunctionSignatureMatch {
                expected,
                actual,
                call_site,
            } => {
                3u8.hash(state);
                expected.hash(state);
                actual.hash(state);
                call_site.start.hash(state);
            }
            SMTConstraint::GuardCondition {
                condition,
                variables,
            } => {
                4u8.hash(state);
                // Note: BooleanExpression and HashMap don't implement Hash
                // For now, we'll use a simplified hash
                format!("{condition:?}").hash(state);
                variables.len().hash(state);
            }
            SMTConstraint::TypeParameterUnification {
                parameter_name,
                concrete_type,
                context,
            } => {
                5u8.hash(state);
                parameter_name.hash(state);
                concrete_type.hash(state);
                context.hash(state);
            }
            SMTConstraint::TypeVariableConstraint {
                variable_id,
                bound_type,
                context,
            } => {
                6u8.hash(state);
                variable_id.hash(state);
                bound_type.hash(state);
                context.hash(state);
            }
            SMTConstraint::TraitCompatibility {
                trait_type,
                implementing_type,
                context,
            } => {
                7u8.hash(state);
                trait_type.hash(state);
                implementing_type.hash(state);
                context.hash(state);
            }
            SMTConstraint::UniversalSelfConstraint {
                self_variable_id,
                trait_being_defined,
                bound_traits,
                context,
            } => {
                8u8.hash(state);
                self_variable_id.hash(state);
                trait_being_defined.hash(state);
                bound_traits.hash(state);
                context.hash(state);
            }
            SMTConstraint::ConcreteSelfBinding {
                self_variable_id,
                concrete_type,
                context,
            } => {
                9u8.hash(state);
                self_variable_id.hash(state);
                concrete_type.hash(state);
                context.hash(state);
            }
            SMTConstraint::SelfTypeInference {
                self_variable_id,
                inferred_type,
                call_site_context,
                confidence,
            } => {
                10u8.hash(state);
                self_variable_id.hash(state);
                inferred_type.hash(state);
                call_site_context.hash(state);
                confidence.hash(state);
            }
            SMTConstraint::ArgumentTypeMatch {
                clause_id,
                parameter_name,
                expected_type,
                actual_type,
                call_site,
            } => {
                11u8.hash(state);
                clause_id.hash(state);
                parameter_name.hash(state);
                expected_type.hash(state);
                actual_type.hash(state);
                call_site.start.hash(state);
            }
            SMTConstraint::GuardApplicable {
                clause_id,
                guard_expression,
                guard_variables,
                context,
            } => {
                12u8.hash(state);
                clause_id.hash(state);
                guard_expression.hash(state);
                guard_variables.len().hash(state); // HashMap doesn't implement Hash
                context.hash(state);
            }
            SMTConstraint::ClausePriority {
                clause_id,
                priority,
                context,
            } => {
                13u8.hash(state);
                clause_id.hash(state);
                priority.hash(state);
                context.hash(state);
            }
            SMTConstraint::GuardStaticallyEvaluated {
                clause_id,
                guard_expression,
                when_arguments,
                evaluation_result,
                context,
            } => {
                14u8.hash(state);
                clause_id.hash(state);
                guard_expression.hash(state);
                // HashMap doesn't implement Hash, so hash the sorted entries
                let mut sorted_args: Vec<_> = when_arguments.iter().collect();
                sorted_args.sort_by_key(|(k, _)| *k);
                for (key, value) in sorted_args {
                    key.hash(state);
                    value.hash(state);
                }
                evaluation_result.hash(state);
                context.hash(state);
            }
            SMTConstraint::PreResolvedClause {
                call_site,
                trait_type,
                impl_type,
                function_name,
                selected_clause_id,
                guard_pre_evaluated,
                argument_types,
            } => {
                15u8.hash(state);
                call_site.hash(state);
                trait_type.hash(state);
                impl_type.hash(state);
                function_name.hash(state);
                selected_clause_id.hash(state);
                guard_pre_evaluated.hash(state);
                argument_types.hash(state);
            }
        }
    }
}

impl ConstraintSet {
    /// Create a new constraint set with critical priority
    pub fn critical(constraints: Vec<SMTConstraint>, context: ConstraintContext) -> Self {
        Self {
            constraints,
            context,
            priority: ConstraintPriority::Critical,
        }
    }

    /// Create a new constraint set with important priority
    pub fn important(constraints: Vec<SMTConstraint>, context: ConstraintContext) -> Self {
        Self {
            constraints,
            context,
            priority: ConstraintPriority::Important,
        }
    }

    /// Create a new constraint set with optional priority
    pub fn optional(constraints: Vec<SMTConstraint>, context: ConstraintContext) -> Self {
        Self {
            constraints,
            context,
            priority: ConstraintPriority::Optional,
        }
    }

    /// Check if this constraint set is critical
    pub fn is_critical(&self) -> bool {
        matches!(self.priority, ConstraintPriority::Critical)
    }

    /// Check if this constraint set is empty
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    /// Get the number of constraints in this set
    pub fn len(&self) -> usize {
        self.constraints.len()
    }
}

impl ConstraintContext {
    /// Create a simple context with just a location
    pub fn simple(location: &str) -> Self {
        Self {
            location: location.to_string(),
            function_name: None,
            module_name: None,
        }
    }

    /// Create a context for a function call
    pub fn function_call(location: &str, function_name: &str) -> Self {
        Self {
            location: location.to_string(),
            function_name: Some(function_name.to_string()),
            module_name: None,
        }
    }

    /// Create a context for a module
    pub fn module(location: &str, module_name: &str) -> Self {
        Self {
            location: location.to_string(),
            function_name: None,
            module_name: Some(module_name.to_string()),
        }
    }
}
