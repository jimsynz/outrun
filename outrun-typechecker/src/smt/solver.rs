//! Z3 SMT solver interface
//!
//! This module provides a high-level interface to the Z3 SMT solver,
//! handling constraint translation and result interpretation.

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::smt::constraints::{ConstraintSet, SMTConstraint};
use crate::smt::translator::SMTTranslator;
use crate::unification::StructuredType;
use std::collections::HashMap;
use z3::{
    ast::{Ast, Bool},
    Config, Context, Model, SatResult, Solver,
};

/// Error types for SMT solving
#[derive(Debug, Clone)]
pub enum SMTError {
    SolverError(String),
    TranslationError(String),
    TimeoutError,
    ResourceLimitError,
    NoConstraintsFound(String),
    SolvingFailed(String),
}

impl std::fmt::Display for SMTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SMTError::SolverError(msg) => write!(f, "SMT solver error: {msg}"),
            SMTError::TranslationError(msg) => write!(f, "SMT translation error: {msg}"),
            SMTError::TimeoutError => write!(f, "SMT solver timeout"),
            SMTError::ResourceLimitError => write!(f, "SMT solver resource limit exceeded"),
            SMTError::NoConstraintsFound(msg) => write!(f, "No constraints found: {msg}"),
            SMTError::SolvingFailed(msg) => write!(f, "SMT solving failed: {msg}"),
        }
    }
}

impl std::error::Error for SMTError {}

/// Result of SMT constraint solving
#[derive(Debug, Clone)]
pub enum SolverResult {
    Satisfiable(ConstraintModel),
    Unsatisfiable(Vec<SMTConstraint>), // Conflicting constraints
    Unknown(String),                   // Timeout or resource limits
}

/// Model representing a satisfying assignment for constraints
#[derive(Debug, Clone)]
pub struct ConstraintModel {
    pub type_assignments: HashMap<String, StructuredType>,
    pub boolean_assignments: HashMap<String, bool>,
    pub function_selections: HashMap<String, String>, // constraint_id -> selected_function
}

/// Z3 SMT solver wrapper
pub struct Z3ConstraintSolver<'ctx> {
    context: &'ctx Context,
    solver: Solver<'ctx>,
    translator: SMTTranslator,
    active_constraints: Vec<SMTConstraint>,
}

/// Z3 context wrapper for managing solver lifetime
pub struct Z3Context {
    context: Context,
}

impl Z3Context {
    pub fn new() -> Self {
        let mut config = Config::new();

        // Optimize Z3 configuration for Outrun's constraint patterns
        Self::configure_for_performance(&mut config);

        let context = Context::new(&config);
        Self { context }
    }

    /// Configure Z3 for optimal performance on Outrun type constraints
    /// Based on optimization strategies from Z3 performance literature
    fn configure_for_performance(config: &mut Config) {
        // Set timeout for individual solver calls (5 seconds)
        config.set_param_value("timeout", "5000");

        // Enable model generation for extracting type assignments
        config.set_model_generation(true);

        // Optimize for satisfiability checking (vs proof generation)
        config.set_proof_generation(false);
        config.set_param_value("unsat_core", "false");

        // Disable auto configuration to use our custom settings
        config.set_param_value("auto_config", "false");

        // Set resource limit to prevent runaway queries
        config.set_param_value("rlimit", "100000");

        // Z3 OPTIMIZATION: Only use parameters that work with binary API
        // Many Z3 parameters are shell-only and not available via binary API
        
        // The key performance benefit comes from our subproblem optimization
        // and constraint ordering, not Z3 parameter tuning

        // Note: model_validate and well_sorted_check are shell-only parameters
        // and cannot be set via the binary API
    }

    pub fn create_solver(&self) -> Z3ConstraintSolver {
        Z3ConstraintSolver::new(&self.context)
    }

    pub fn get_context(&self) -> &Context {
        &self.context
    }
}

impl<'ctx> Z3ConstraintSolver<'ctx> {
    /// Create a new Z3 constraint solver
    pub fn new(context: &'ctx Context) -> Self {
        let solver = Solver::new(context);
        let translator = SMTTranslator::new();

        Self {
            context,
            solver,
            translator,
            active_constraints: Vec::new(),
        }
    }

    /// Add constraints to the solver with a compiler environment for type resolution
    pub fn add_constraints(
        &mut self,
        constraints: &[SMTConstraint],
        compiler_env: &CompilerEnvironment,
    ) -> Result<(), SMTError> {
        for constraint in constraints {
            let _smt_formula = self
                .translator
                .translate_trait_constraint(constraint, compiler_env);

            // Parse the SMT formula and add it to the Z3 solver
            // For now, we'll create simple assertions for basic cases
            match constraint {
                SMTConstraint::TraitImplemented {
                    impl_type,
                    trait_type,
                } => {
                    // Create a boolean variable representing "impl_type implements trait_type"
                    let impl_sort = self
                        .translator
                        .translate_structured_type(impl_type, compiler_env);
                    let trait_sort = self
                        .translator
                        .translate_structured_type(trait_type, compiler_env);

                    // Create a boolean assertion
                    let var_name = format!("implements_{impl_sort}_{trait_sort}");
                    let bool_var = Bool::new_const(self.context, var_name);
                    self.solver.assert(&bool_var);
                }
                SMTConstraint::TypeUnification { type1, type2, .. } => {
                    // Create equality constraint between two types
                    let type1_sort = self
                        .translator
                        .translate_structured_type(type1, compiler_env);
                    let type2_sort = self
                        .translator
                        .translate_structured_type(type2, compiler_env);

                    // For now, create boolean variables for each type and assert equality
                    let var1 = Bool::new_const(self.context, format!("type_{type1_sort}"));
                    let var2 = Bool::new_const(self.context, format!("type_{type2_sort}"));
                    let equality = var1._eq(&var2);
                    self.solver.assert(&equality);
                }
                SMTConstraint::GenericInstantiation {
                    generic_type,
                    concrete_candidates,
                } => {
                    // Create disjunction: generic_type = candidate1 OR generic_type = candidate2 OR ...
                    let generic_sort = self
                        .translator
                        .translate_structured_type(generic_type, compiler_env);
                    let generic_var = Bool::new_const(self.context, format!("type_{generic_sort}"));

                    let mut disjuncts = Vec::new();
                    for candidate in concrete_candidates {
                        let candidate_sort = self
                            .translator
                            .translate_structured_type(candidate, compiler_env);
                        let candidate_var =
                            Bool::new_const(self.context, format!("type_{candidate_sort}"));
                        disjuncts.push(generic_var._eq(&candidate_var));
                    }

                    if !disjuncts.is_empty() {
                        let disjunction =
                            Bool::or(self.context, &disjuncts.iter().collect::<Vec<_>>());
                        self.solver.assert(&disjunction);
                    }
                }
                SMTConstraint::FunctionSignatureMatch { .. } => {
                    // For now, just assert true (placeholder)
                    let true_const = Bool::from_bool(self.context, true);
                    self.solver.assert(&true_const);
                }
                SMTConstraint::GuardCondition { .. } => {
                    // For now, just assert true (placeholder)
                    let true_const = Bool::from_bool(self.context, true);
                    self.solver.assert(&true_const);
                }
                SMTConstraint::TypeParameterUnification {
                    parameter_name,
                    concrete_type,
                    ..
                } => {
                    // Assert that the type parameter equals the concrete type
                    let concrete_sort = self
                        .translator
                        .translate_structured_type(concrete_type, compiler_env);

                    // Create variables for the parameter and concrete type
                    let param_var =
                        Bool::new_const(self.context, format!("param_{parameter_name}"));
                    let concrete_var =
                        Bool::new_const(self.context, format!("type_{concrete_sort}"));

                    // Assert they are equal
                    let equality = param_var._eq(&concrete_var);
                    self.solver.assert(&equality);
                }
                SMTConstraint::TypeVariableConstraint {
                    variable_id,
                    bound_type,
                    ..
                } => {
                    // Assert that the TypeVariable equals the bound type
                    let bound_sort = self
                        .translator
                        .translate_structured_type(bound_type, compiler_env);

                    let var_name =
                        if let Some(type_name) = compiler_env.resolve_type(variable_id.clone()) {
                            format!("TypeVar_{type_name}")
                        } else {
                            format!("TypeVar_unknown_{}", variable_id.hash)
                        };

                    // Create variables for the TypeVariable and bound type
                    let type_var = Bool::new_const(self.context, var_name);
                    let bound_var = Bool::new_const(self.context, format!("type_{bound_sort}"));

                    // Assert they are equal
                    let equality = type_var._eq(&bound_var);
                    self.solver.assert(&equality);
                }
                SMTConstraint::TraitCompatibility {
                    trait_type,
                    implementing_type,
                    ..
                } => {
                    // Create a boolean variable representing "implementing_type implements trait_type"
                    let impl_sort = self
                        .translator
                        .translate_structured_type(implementing_type, compiler_env);
                    let trait_sort = self
                        .translator
                        .translate_structured_type(trait_type, compiler_env);

                    // Create a boolean assertion
                    let var_name = format!("implements_{impl_sort}_{trait_sort}");
                    let bool_var = Bool::new_const(self.context, var_name);
                    self.solver.assert(&bool_var);
                }
                SMTConstraint::UniversalSelfConstraint {
                    self_variable_id,
                    trait_being_defined,
                    bound_traits,
                    context: _,
                } => {
                    // Universal Self constraint: ∀ Self. (implements(Self, TraitBeingDefined) ∧ implements(Self, BoundTrait))
                    let self_var_name = format!("Self_{}", self_variable_id.hash);
                    let trait_defined_sort = self
                        .translator
                        .translate_structured_type(trait_being_defined, compiler_env);

                    // For each bound trait, create an implication:
                    // implements(Self, TraitBeingDefined) → implements(Self, BoundTrait)
                    for bound_trait in bound_traits {
                        let bound_sort = self
                            .translator
                            .translate_structured_type(bound_trait, compiler_env);

                        let implements_defined = Bool::new_const(
                            self.context,
                            format!("implements_{self_var_name}_{trait_defined_sort}"),
                        );
                        let implements_bound = Bool::new_const(
                            self.context,
                            format!("implements_{self_var_name}_{bound_sort}"),
                        );

                        // Assert: implements_defined → implements_bound
                        let implication = Bool::implies(&implements_defined, &implements_bound);
                        self.solver.assert(&implication);
                    }
                }
                SMTConstraint::ConcreteSelfBinding {
                    self_variable_id,
                    concrete_type,
                    ..
                } => {
                    // Concrete Self binding: Self = ConcreteType
                    let self_var_name = format!("Self_{}", self_variable_id.hash);
                    let concrete_sort = self
                        .translator
                        .translate_structured_type(concrete_type, compiler_env);

                    let self_var = Bool::new_const(self.context, self_var_name);
                    let concrete_var =
                        Bool::new_const(self.context, format!("type_{concrete_sort}"));

                    // Assert Self = ConcreteType
                    let equality = self_var._eq(&concrete_var);
                    self.solver.assert(&equality);
                }
                SMTConstraint::SelfTypeInference {
                    self_variable_id,
                    inferred_type,
                    confidence,
                    ..
                } => {
                    // Self type inference: Self should equal inferred type
                    let self_var_name = format!("Self_{}", self_variable_id.hash);
                    let inferred_sort = self
                        .translator
                        .translate_structured_type(inferred_type, compiler_env);

                    let self_var = Bool::new_const(self.context, self_var_name);
                    let inferred_var =
                        Bool::new_const(self.context, format!("type_{inferred_sort}"));

                    // Create a weighted constraint based on confidence
                    let equality = self_var._eq(&inferred_var);
                    match confidence {
                        crate::smt::constraints::InferenceConfidence::High => {
                            // High confidence: assert directly
                            self.solver.assert(&equality);
                        }
                        crate::smt::constraints::InferenceConfidence::Medium => {
                            // Medium confidence: soft assertion (for now, treat as hard)
                            self.solver.assert(&equality);
                        }
                        crate::smt::constraints::InferenceConfidence::Low => {
                            // Low confidence: soft assertion (for now, treat as hard)
                            self.solver.assert(&equality);
                        }
                    }
                }
                SMTConstraint::ArgumentTypeMatch {
                    clause_id,
                    parameter_name,
                    expected_type,
                    actual_type,
                    ..
                } => {
                    // Argument type matching: expected_type = actual_type
                    let expected_sort = self
                        .translator
                        .translate_structured_type(expected_type, compiler_env);
                    let actual_sort = self
                        .translator
                        .translate_structured_type(actual_type, compiler_env);
                    
                    let expected_var = Bool::new_const(self.context, format!("type_{expected_sort}"));
                    let actual_var = Bool::new_const(self.context, format!("type_{actual_sort}"));
                    
                    // Create clause applicability variable
                    let clause_var = Bool::new_const(self.context, format!("clause_applicable_{clause_id}_{parameter_name}"));
                    
                    // Assert: clause is applicable if types match
                    let type_match = expected_var._eq(&actual_var);
                    let implication = Bool::implies(&type_match, &clause_var);
                    self.solver.assert(&implication);
                }
                SMTConstraint::GuardApplicable {
                    clause_id,
                    guard_expression: _,
                    guard_variables: _,
                    ..
                } => {
                    // Guard applicability: for now, just assert that guard can be evaluated
                    // TODO: Implement proper guard expression analysis
                    let guard_var = Bool::new_const(self.context, format!("guard_applicable_{clause_id}"));
                    self.solver.assert(&guard_var);
                }
                SMTConstraint::ClausePriority {
                    clause_id,
                    priority,
                    ..
                } => {
                    // Clause priority: lower number = higher priority
                    // For now, just record the priority value
                    let priority_var = Bool::new_const(self.context, format!("clause_priority_{clause_id}_{priority}"));
                    self.solver.assert(&priority_var);
                }
                SMTConstraint::GuardStaticallyEvaluated {
                    clause_id,
                    guard_expression: _,
                    evaluation_result,
                    ..
                } => {
                    // Static guard evaluation: guard result is known at compile time
                    let guard_result_var = Bool::new_const(self.context, format!("guard_static_{clause_id}"));
                    let static_bool = Bool::from_bool(self.context, *evaluation_result);
                    let equality = guard_result_var._eq(&static_bool);
                    self.solver.assert(&equality);
                }
                SMTConstraint::PreResolvedClause {
                    call_site: _,
                    trait_type: _,
                    impl_type: _,
                    function_name: _,
                    selected_clause_id,
                    guard_pre_evaluated,
                    argument_types: _,
                } => {
                    // Pre-resolved clause: specific clause was selected by SMT at compile time
                    // Create constraint that this clause is selected
                    let clause_selected_var = Bool::new_const(self.context, format!("clause_selected_{selected_clause_id}"));
                    self.solver.assert(&clause_selected_var);
                    
                    // If guard was pre-evaluated, add that constraint
                    if let Some(guard_result) = guard_pre_evaluated {
                        let guard_var = Bool::new_const(self.context, format!("guard_preresolve_{selected_clause_id}"));
                        let guard_bool = Bool::from_bool(self.context, *guard_result);
                        let guard_equality = guard_var._eq(&guard_bool);
                        self.solver.assert(&guard_equality);
                    }
                }
                
                // Exhaustiveness analysis constraints
                SMTConstraint::FunctionClauseSetExhaustive { function_name, clauses, .. } => {
                    // For now, create a boolean variable representing exhaustiveness
                    let exhaustive_var = Bool::new_const(self.context, format!("exhaustive_{function_name}"));
                    // Simplified: assume exhaustive if all clauses are present
                    if !clauses.is_empty() {
                        self.solver.assert(&exhaustive_var);
                    }
                }
                
                SMTConstraint::FunctionClauseReachable { clause_id, .. } => {
                    // Create a boolean variable for reachability
                    let reachable_var = Bool::new_const(self.context, format!("reachable_{clause_id}"));
                    self.solver.assert(&reachable_var);
                }
                
                SMTConstraint::GuardCoverageComplete { function_name, .. } => {
                    // Create a boolean variable for coverage completeness
                    let coverage_var = Bool::new_const(self.context, format!("coverage_complete_{function_name}"));
                    self.solver.assert(&coverage_var);
                }
                
                SMTConstraint::GuardConditionSatisfiable { clause_id, .. } => {
                    // Create a boolean variable for guard satisfiability
                    let satisfiable_var = Bool::new_const(self.context, format!("satisfiable_guard_{clause_id}"));
                    self.solver.assert(&satisfiable_var);
                }
                SMTConstraint::PendingClauseResolution { .. } => {
                    // PendingClauseResolution constraints are handled in post-SMT phase
                    // They should not be sent to Z3 solver
                    continue;
                }
            }
        }

        self.active_constraints.extend_from_slice(constraints);
        Ok(())
    }

    /// Solve the current set of constraints
    pub fn solve(&mut self) -> SolverResult {
        match self.solver.check() {
            SatResult::Sat => {
                // Satisfiable - extract model
                if let Some(model) = self.solver.get_model() {
                    let constraint_model = self.extract_model_from_z3(&model);
                    SolverResult::Satisfiable(constraint_model)
                } else {
                    SolverResult::Unknown("Satisfiable but no model available".to_string())
                }
            }
            SatResult::Unsat => {
                // Unsatisfiable - get unsat core if possible
                let unsat_core = self.get_unsat_core_constraints();
                SolverResult::Unsatisfiable(unsat_core)
            }
            SatResult::Unknown => {
                SolverResult::Unknown("Z3 returned unknown (timeout or resource limit)".to_string())
            }
        }
    }

    /// Extract our constraint model from Z3 model
    fn extract_model_from_z3(&mut self, z3_model: &Model) -> ConstraintModel {
        let mut constraint_model = ConstraintModel::empty();

        // Extract boolean assignments from the model
        for constraint in &self.active_constraints {
            match constraint {
                SMTConstraint::TraitImplemented {
                    impl_type,
                    trait_type,
                } => {
                    let impl_sort = self
                        .translator
                        .translate_structured_type(impl_type, &CompilerEnvironment::new());
                    let trait_sort = self
                        .translator
                        .translate_structured_type(trait_type, &CompilerEnvironment::new());
                    let var_name = format!("implements_{impl_sort}_{trait_sort}");

                    // Try to get the boolean value from the model
                    let bool_var = Bool::new_const(self.context, var_name.clone());
                    if let Some(value) = z3_model.eval(&bool_var, true) {
                        if let Some(bool_value) = value.as_bool() {
                            constraint_model.add_boolean_assignment(var_name, bool_value);
                        }
                    }
                }
                SMTConstraint::TypeUnification { type1, type2, .. } => {
                    let type1_sort = self
                        .translator
                        .translate_structured_type(type1, &CompilerEnvironment::new());
                    let type2_sort = self
                        .translator
                        .translate_structured_type(type2, &CompilerEnvironment::new());

                    // Store type assignments
                    constraint_model
                        .add_type_assignment(format!("type_{type1_sort}"), type1.clone());
                    constraint_model
                        .add_type_assignment(format!("type_{type2_sort}"), type2.clone());
                }
                SMTConstraint::TypeParameterUnification {
                    parameter_name,
                    concrete_type,
                    ..
                } => {
                    // Store the type parameter assignment in the model
                    constraint_model.add_type_assignment(
                        format!("param_{parameter_name}"),
                        concrete_type.clone(),
                    );

                    // Also store the concrete type assignment
                    let concrete_sort = self
                        .translator
                        .translate_structured_type(concrete_type, &CompilerEnvironment::new());
                    constraint_model.add_type_assignment(
                        format!("type_{concrete_sort}"),
                        concrete_type.clone(),
                    );
                }
                SMTConstraint::ConcreteSelfBinding {
                    self_variable_id,
                    concrete_type,
                    ..
                } => {
                    // Store the Self variable assignment in the model
                    let self_var_name = format!("Self_{}", self_variable_id.hash);
                    constraint_model.add_type_assignment(self_var_name, concrete_type.clone());
                }
                SMTConstraint::SelfTypeInference {
                    self_variable_id,
                    inferred_type,
                    ..
                } => {
                    // Store the Self type inference in the model
                    let self_var_name = format!("Self_{}", self_variable_id.hash);
                    constraint_model.add_type_assignment(self_var_name, inferred_type.clone());
                }
                SMTConstraint::UniversalSelfConstraint {
                    self_variable_id,
                    trait_being_defined,
                    ..
                } => {
                    // Store the universal Self constraint in the model
                    // For universal constraints, we might not have a specific assignment,
                    // but we can store the trait being defined as a fallback
                    let self_var_name = format!("Self_{}", self_variable_id.hash);
                    constraint_model
                        .add_type_assignment(self_var_name, trait_being_defined.clone());
                }
                SMTConstraint::ArgumentTypeMatch {
                    clause_id,
                    parameter_name,
                    expected_type,
                    actual_type,
                    ..
                } => {
                    // Store clause applicability based on argument type matching
                    let clause_var_name = format!("clause_applicable_{clause_id}_{parameter_name}");
                    let clause_var = Bool::new_const(self.context, clause_var_name.clone());
                    if let Some(value) = z3_model.eval(&clause_var, true) {
                        if let Some(bool_value) = value.as_bool() {
                            constraint_model.add_boolean_assignment(clause_var_name, bool_value);
                        }
                    }
                    
                    // Store the type assignments
                    let expected_sort = self
                        .translator
                        .translate_structured_type(expected_type, &CompilerEnvironment::new());
                    let actual_sort = self
                        .translator
                        .translate_structured_type(actual_type, &CompilerEnvironment::new());
                    constraint_model.add_type_assignment(format!("type_{expected_sort}"), expected_type.clone());
                    constraint_model.add_type_assignment(format!("type_{actual_sort}"), actual_type.clone());
                }
                SMTConstraint::GuardApplicable {
                    clause_id,
                    ..
                } => {
                    // Store guard applicability result
                    let guard_var_name = format!("guard_applicable_{clause_id}");
                    let guard_var = Bool::new_const(self.context, guard_var_name.clone());
                    if let Some(value) = z3_model.eval(&guard_var, true) {
                        if let Some(bool_value) = value.as_bool() {
                            constraint_model.add_boolean_assignment(guard_var_name, bool_value);
                        }
                    }
                }
                SMTConstraint::ClausePriority {
                    clause_id,
                    priority,
                    ..
                } => {
                    // Store clause priority information
                    let priority_var_name = format!("clause_priority_{clause_id}_{priority}");
                    let priority_var = Bool::new_const(self.context, priority_var_name.clone());
                    if let Some(value) = z3_model.eval(&priority_var, true) {
                        if let Some(bool_value) = value.as_bool() {
                            constraint_model.add_boolean_assignment(priority_var_name, bool_value);
                        }
                    }
                    
                    // Store the function selection based on priority
                    constraint_model.add_function_selection(
                        format!("priority_{priority}"),
                        clause_id.clone(),
                    );
                }
                SMTConstraint::GuardStaticallyEvaluated {
                    clause_id,
                    evaluation_result,
                    ..
                } => {
                    // Store static guard evaluation result
                    let guard_static_var_name = format!("guard_static_{clause_id}");
                    constraint_model.add_boolean_assignment(guard_static_var_name, *evaluation_result);
                    
                    // Store the clause as statically evaluable
                    constraint_model.add_function_selection(
                        format!("static_guard_{clause_id}"),
                        clause_id.clone(),
                    );
                }
                _ => {
                    // Handle other constraint types as needed
                }
            }
        }

        constraint_model
    }

    /// Get unsatisfiable core constraints
    fn get_unsat_core_constraints(&self) -> Vec<SMTConstraint> {
        // For now, return all active constraints
        // TODO: Implement proper unsat core extraction using Z3's unsat core functionality
        self.active_constraints.clone()
    }

    /// Reset the solver to initial state
    pub fn reset(&mut self) {
        self.active_constraints.clear();
        self.solver.reset();
    }

    /// Push a new scope for incremental solving
    pub fn push(&mut self) {
        self.solver.push();
    }

    /// Pop the most recent scope
    pub fn pop(&mut self) {
        self.solver.pop(1);
    }

    /// Set a timeout for solving (in milliseconds)
    pub fn set_timeout(&mut self, timeout_ms: u64) {
        // Z3 timeout is set via parameters
        let _timeout_param = format!("timeout={timeout_ms}");
        // Note: Z3 Rust API might handle this differently, this is a placeholder
        // TODO: Implement proper timeout setting through Z3 parameters
    }

    /// Get all constraints currently loaded in the solver
    pub fn get_active_constraints(&self) -> &[SMTConstraint] {
        &self.active_constraints
    }
}

impl Default for Z3Context {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstraintModel {
    /// Create an empty constraint model
    pub fn empty() -> Self {
        Self {
            type_assignments: HashMap::new(),
            boolean_assignments: HashMap::new(),
            function_selections: HashMap::new(),
        }
    }

    /// Get a type assignment from the model
    pub fn get_type_assignment(&self, variable: &str) -> Option<&StructuredType> {
        self.type_assignments.get(variable)
    }

    /// Get a boolean assignment from the model
    pub fn get_boolean_assignment(&self, variable: &str) -> Option<bool> {
        self.boolean_assignments.get(variable).copied()
    }

    /// Get a function selection from the model
    pub fn get_function_selection(&self, constraint_id: &str) -> Option<&str> {
        self.function_selections
            .get(constraint_id)
            .map(|s| s.as_str())
    }

    /// Add a type assignment to the model
    pub fn add_type_assignment(&mut self, variable: String, structured_type: StructuredType) {
        self.type_assignments.insert(variable, structured_type);
    }

    /// Add a boolean assignment to the model
    pub fn add_boolean_assignment(&mut self, variable: String, value: bool) {
        self.boolean_assignments.insert(variable, value);
    }

    /// Add a function selection to the model
    pub fn add_function_selection(&mut self, constraint_id: String, function_name: String) {
        self.function_selections
            .insert(constraint_id, function_name);
    }

    /// Check if the model is empty
    pub fn is_empty(&self) -> bool {
        self.type_assignments.is_empty()
            && self.boolean_assignments.is_empty()
            && self.function_selections.is_empty()
    }
}

/// Helper functions for constraint solving
impl<'ctx> Z3ConstraintSolver<'ctx> {
    /// Solve a specific constraint set
    pub fn solve_constraint_set(
        &mut self,
        constraint_set: &ConstraintSet,
        compiler_env: &CompilerEnvironment,
    ) -> SolverResult {
        self.reset();
        if self
            .add_constraints(&constraint_set.constraints, compiler_env)
            .is_err()
        {
            return SolverResult::Unknown("Failed to add constraints".to_string());
        }
        self.solve()
    }

    /// Check if a single constraint is satisfiable
    pub fn is_constraint_satisfiable(
        &mut self,
        constraint: &SMTConstraint,
        compiler_env: &CompilerEnvironment,
    ) -> bool {
        self.reset();
        if self
            .add_constraints(&[constraint.clone()], compiler_env)
            .is_err()
        {
            return false;
        }
        matches!(self.solve(), SolverResult::Satisfiable(_))
    }
}
