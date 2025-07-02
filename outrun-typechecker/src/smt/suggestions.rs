//! SMT-based error suggestions and constraint relaxation
//!
//! This module provides intelligent error suggestions based on SMT solver
//! results and constraint analysis.

use crate::error::TypeError;
use crate::smt::constraints::SMTConstraint;
use crate::smt::solver::{ConstraintModel, SolverResult};
use crate::unification::StructuredType;

/// Generator for SMT-based error suggestions  
pub struct ErrorSuggestionGenerator {
    z3_context: crate::smt::solver::Z3Context,
}

/// Constraint relaxation suggestion
#[derive(Debug, Clone)]
pub struct ConstraintRelaxation {
    pub description: String,
    pub relaxed_constraints: Vec<SMTConstraint>,
    pub confidence: f32, // 0.0 to 1.0
}

/// Type of suggestion that can be made
#[derive(Debug, Clone)]
pub enum SuggestionType {
    /// Suggest adding a trait implementation
    AddTraitImplementation {
        trait_type: StructuredType,
        impl_type: StructuredType,
        reasoning: String,
    },
    /// Suggest changing a type annotation
    ChangeTypeAnnotation {
        current_type: StructuredType,
        suggested_type: StructuredType,
        location: String,
    },
    /// Suggest adding a function overload
    AddFunctionOverload {
        function_name: String,
        suggested_signature: String,
        reasoning: String,
    },
    /// Suggest relaxing a constraint
    RelaxConstraint {
        original_constraint: SMTConstraint,
        relaxed_constraint: SMTConstraint,
        reasoning: String,
    },
}

impl ErrorSuggestionGenerator {
    /// Create a new error suggestion generator
    pub fn new() -> Self {
        Self {
            z3_context: crate::smt::solver::Z3Context::new(),
        }
    }

    /// Suggest trait implementations that could resolve an error
    pub fn suggest_trait_implementations(&mut self, error: &TypeError) -> Vec<String> {
        match error {
            TypeError::TraitNotImplemented {
                trait_name,
                type_name,
                ..
            } => {
                vec![
                    format!(
                        "Consider implementing trait {} for type {}",
                        trait_name, type_name
                    ),
                    format!(
                        "Check if {} already implements {} in a different module",
                        type_name, trait_name
                    ),
                    format!(
                        "Verify that the trait constraint {} is necessary",
                        trait_name
                    ),
                ]
            }
            _ => Vec::new(),
        }
    }

    /// Suggest type annotations that could resolve an error
    pub fn suggest_type_annotations(&mut self, error: &TypeError) -> Vec<String> {
        match error {
            TypeError::TypeMismatch {
                expected, found, ..
            } => {
                vec![
                    format!(
                        "Expected type {}, but found {}. Consider adding explicit type annotation.",
                        expected, found
                    ),
                    format!("Try converting {} to {} if possible", found, expected),
                ]
            }
            _ => Vec::new(),
        }
    }

    /// Suggest function overloads that could resolve an error
    pub fn suggest_function_overloads(&mut self, error: &TypeError) -> Vec<String> {
        match error {
            TypeError::UndefinedFunction { name, .. } => {
                vec![
                    format!(
                        "Function {} is not defined. Check the function name and module path.",
                        name
                    ),
                    format!("Consider importing the module that contains {}", name),
                    format!("Check if {} requires specific trait implementations", name),
                ]
            }
            _ => Vec::new(),
        }
    }

    /// Generate constraint relaxations for unsatisfiable constraints
    pub fn generate_constraint_relaxations(
        &mut self,
        constraints: &[SMTConstraint],
        compiler_env: &crate::compilation::compiler_environment::CompilerEnvironment,
    ) -> Vec<ConstraintRelaxation> {
        let mut relaxations = Vec::new();

        // Try removing each constraint individually to see which ones are problematic
        for (i, constraint) in constraints.iter().enumerate() {
            let mut relaxed_constraints = constraints.to_vec();
            relaxed_constraints.remove(i);

            // Test if removing this constraint makes the set satisfiable
            let mut solver = self.z3_context.create_solver();
            if solver
                .add_constraints(&relaxed_constraints, compiler_env)
                .is_ok()
            {
                match solver.solve() {
                    SolverResult::Satisfiable(_) => {
                        let description = match constraint {
                            SMTConstraint::TraitImplemented {
                                impl_type,
                                trait_type,
                            } => {
                                format!(
                                    "Remove trait requirement: {} implementing {}",
                                    self.type_to_string(impl_type),
                                    self.type_to_string(trait_type)
                                )
                            }
                            SMTConstraint::TypeUnification {
                                type1,
                                type2,
                                context,
                            } => {
                                format!(
                                    "Remove type unification: {} = {} ({})",
                                    self.type_to_string(type1),
                                    self.type_to_string(type2),
                                    context
                                )
                            }
                            SMTConstraint::GenericInstantiation { generic_type, .. } => {
                                format!(
                                    "Remove generic instantiation requirement for {}",
                                    self.type_to_string(generic_type)
                                )
                            }
                            SMTConstraint::FunctionSignatureMatch { .. } => {
                                "Remove function signature matching requirement".to_string()
                            }
                            SMTConstraint::GuardCondition { .. } => {
                                "Remove guard condition requirement".to_string()
                            }
                            SMTConstraint::TypeParameterUnification { 
                                parameter_name, 
                                concrete_type, 
                                .. 
                            } => {
                                format!(
                                    "Remove type parameter constraint: {} = {}",
                                    parameter_name,
                                    self.type_to_string(concrete_type)
                                )
                            }
                            SMTConstraint::TypeVariableConstraint {
                                variable_id,
                                bound_type,
                                ..
                            } => {
                                let var_name = if let Some(name) = compiler_env.resolve_type(variable_id.clone()) {
                                    name
                                } else {
                                    format!("TypeVar_{}", variable_id.hash)
                                };
                                format!(
                                    "Remove type variable constraint: {} = {}",
                                    var_name,
                                    self.type_to_string(bound_type)
                                )
                            }
                            SMTConstraint::TraitCompatibility {
                                trait_type,
                                implementing_type,
                                ..
                            } => {
                                format!(
                                    "Remove trait compatibility constraint: {} must be implemented by {}",
                                    self.type_to_string(trait_type),
                                    self.type_to_string(implementing_type)
                                )
                            }
                            SMTConstraint::UniversalSelfConstraint {
                                trait_being_defined,
                                bound_traits,
                                ..
                            } => {
                                let bound_names = bound_traits.iter()
                                    .map(|t| self.type_to_string(t))
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                format!(
                                    "Remove universal Self constraint: Self must implement {} and {}",
                                    self.type_to_string(trait_being_defined),
                                    bound_names
                                )
                            }
                            SMTConstraint::ConcreteSelfBinding {
                                concrete_type,
                                ..
                            } => {
                                format!(
                                    "Remove concrete Self binding: Self = {}",
                                    self.type_to_string(concrete_type)
                                )
                            }
                            SMTConstraint::SelfTypeInference {
                                inferred_type,
                                call_site_context,
                                ..
                            } => {
                                format!(
                                    "Remove Self type inference: Self = {} (from {})",
                                    self.type_to_string(inferred_type),
                                    call_site_context
                                )
                            }
                        };

                        relaxations.push(ConstraintRelaxation {
                            description,
                            relaxed_constraints: relaxed_constraints.clone(),
                            confidence: 0.7, // Medium confidence
                        });
                    }
                    _ => {
                        // This constraint isn't the only problem
                    }
                }
            }
        }

        // Try relaxing pairs of constraints
        if relaxations.is_empty() && constraints.len() > 1 {
            relaxations.extend(self.try_pairwise_relaxation(constraints, compiler_env));
        }

        relaxations
    }

    /// Generate specific suggestions based on SMT model analysis
    pub fn analyze_model_for_suggestions(
        &mut self,
        model: &ConstraintModel,
        _original_constraints: &[SMTConstraint],
    ) -> Vec<SuggestionType> {
        let mut suggestions = Vec::new();

        // Analyze type assignments for potential improvements
        for (var_name, assigned_type) in &model.type_assignments {
            if var_name.starts_with("trait_") {
                // This might suggest a missing trait implementation
                suggestions.push(SuggestionType::AddTraitImplementation {
                    trait_type: assigned_type.clone(),
                    impl_type: assigned_type.clone(), // Placeholder
                    reasoning: format!("SMT model suggests {var_name} trait implementation"),
                });
            }
        }

        // Analyze function selections for overload suggestions
        for (constraint_id, selected_function) in &model.function_selections {
            suggestions.push(SuggestionType::AddFunctionOverload {
                function_name: selected_function.clone(),
                suggested_signature: format!("Based on constraint {constraint_id}"),
                reasoning: "SMT solver identified this as a viable function signature".to_string(),
            });
        }

        suggestions
    }

    /// Try relaxing pairs of constraints to find conflicts
    fn try_pairwise_relaxation(
        &mut self,
        constraints: &[SMTConstraint],
        compiler_env: &crate::compilation::compiler_environment::CompilerEnvironment,
    ) -> Vec<ConstraintRelaxation> {
        let mut relaxations = Vec::new();

        for i in 0..constraints.len() {
            for j in (i + 1)..constraints.len() {
                let mut relaxed_constraints = constraints.to_vec();
                relaxed_constraints.remove(j); // Remove j first (higher index)
                relaxed_constraints.remove(i); // Then remove i

                let mut solver = self.z3_context.create_solver();
                if solver
                    .add_constraints(&relaxed_constraints, compiler_env)
                    .is_ok()
                {
                    if let SolverResult::Satisfiable(_) = solver.solve() {
                        relaxations.push(ConstraintRelaxation {
                            description: format!("Remove conflicting constraints {i} and {j}"),
                            relaxed_constraints: relaxed_constraints.clone(),
                            confidence: 0.5, // Lower confidence for pairwise relaxation
                        });
                    }
                }
            }
        }

        relaxations
    }

    /// Helper function to convert StructuredType to string for display
    fn type_to_string(&self, structured_type: &StructuredType) -> String {
        // TODO: Implement proper StructuredType to string conversion
        // For now, use debug formatting
        format!("{structured_type:?}")
    }

    /// Reset the suggestion generator state
    pub fn reset(&mut self) {
        // Create a new context to reset state
        self.z3_context = crate::smt::solver::Z3Context::new();
    }
}

impl Default for ErrorSuggestionGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstraintRelaxation {
    /// Create a new constraint relaxation
    pub fn new(
        description: String,
        relaxed_constraints: Vec<SMTConstraint>,
        confidence: f32,
    ) -> Self {
        Self {
            description,
            relaxed_constraints,
            confidence: confidence.clamp(0.0, 1.0),
        }
    }

    /// Check if this relaxation has high confidence
    pub fn is_high_confidence(&self) -> bool {
        self.confidence >= 0.8
    }

    /// Check if this relaxation has medium confidence
    pub fn is_medium_confidence(&self) -> bool {
        self.confidence >= 0.5 && self.confidence < 0.8
    }

    /// Check if this relaxation has low confidence
    pub fn is_low_confidence(&self) -> bool {
        self.confidence < 0.5
    }

    /// Get the number of constraints that were removed
    pub fn constraints_removed(&self, original_count: usize) -> usize {
        original_count.saturating_sub(self.relaxed_constraints.len())
    }
}

impl SuggestionType {
    /// Get a human-readable description of this suggestion
    pub fn description(&self) -> String {
        match self {
            SuggestionType::AddTraitImplementation {
                trait_type,
                impl_type,
                reasoning,
            } => {
                format!(
                    "Add trait implementation: implement {trait_type:?} for {impl_type:?}. {reasoning}"
                )
            }
            SuggestionType::ChangeTypeAnnotation {
                current_type,
                suggested_type,
                location,
            } => {
                format!(
                    "Change type annotation at {location}: {current_type:?} -> {suggested_type:?}"
                )
            }
            SuggestionType::AddFunctionOverload {
                function_name,
                suggested_signature,
                reasoning,
            } => {
                format!(
                    "Add function overload: {function_name} with signature {suggested_signature}. {reasoning}"
                )
            }
            SuggestionType::RelaxConstraint {
                original_constraint,
                relaxed_constraint,
                reasoning,
            } => {
                format!(
                    "Relax constraint: {original_constraint:?} -> {relaxed_constraint:?}. {reasoning}"
                )
            }
        }
    }

    /// Get the confidence level for this suggestion
    pub fn confidence(&self) -> f32 {
        match self {
            SuggestionType::AddTraitImplementation { .. } => 0.8,
            SuggestionType::ChangeTypeAnnotation { .. } => 0.9,
            SuggestionType::AddFunctionOverload { .. } => 0.7,
            SuggestionType::RelaxConstraint { .. } => 0.6,
        }
    }
}
