//! SMT-LIB translation for Outrun types and constraints
//!
//! This module handles the translation from Outrun's StructuredType system
//! and SMT constraints to Z3 SMT-LIB format for solving.

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::smt::constraints::SMTConstraint;
use crate::unification::StructuredType;
use std::collections::HashMap;

/// Translator for converting Outrun types and constraints to SMT-LIB
pub struct SMTTranslator {
    type_variables: HashMap<String, String>, // Outrun type -> SMT variable
    trait_predicates: HashMap<String, String>, // Trait name -> SMT predicate
    type_counter: usize,                     // For generating unique variable names
    predicate_counter: usize,                // For generating unique predicate names
}

impl SMTTranslator {
    /// Create a new SMT translator
    pub fn new() -> Self {
        Self {
            type_variables: HashMap::new(),
            trait_predicates: HashMap::new(),
            type_counter: 0,
            predicate_counter: 0,
        }
    }

    /// Convert StructuredType to SMT sort representation
    /// This is the CRITICAL function that preserves generic information
    pub fn translate_structured_type(
        &mut self,
        structured_type: &StructuredType,
        compiler_env: &CompilerEnvironment,
    ) -> String {
        match structured_type {
            StructuredType::Simple(type_name_id) => {
                // Convert TypeNameId to string and create SMT sort
                if let Some(type_name) = compiler_env.resolve_type(type_name_id.clone()) {
                    self.get_or_create_type_variable(&type_name)
                } else {
                    self.get_or_create_type_variable(&format!("unknown_type_{}", type_name_id.hash))
                }
            }
            StructuredType::TypeVariable(type_name_id) => {
                // TypeVariable gets converted to an SMT variable
                if let Some(type_name) = compiler_env.resolve_type(type_name_id.clone()) {
                    self.get_or_create_type_variable(&format!("TypeVar_{type_name}"))
                } else {
                    self.get_or_create_type_variable(&format!(
                        "TypeVar_unknown_{}",
                        type_name_id.hash
                    ))
                }
            }
            StructuredType::Generic { base, args } => {
                // Create parameterized SMT sort: Option_Int64, Map_String_Int64, etc.
                if let Some(base_name) = compiler_env.resolve_type(base.clone()) {
                    let arg_sorts: Vec<String> = args
                        .iter()
                        .map(|arg| self.translate_structured_type(arg, compiler_env))
                        .collect();

                    if arg_sorts.is_empty() {
                        self.get_or_create_type_variable(&base_name)
                    } else {
                        let combined_name = format!("{}_{}", base_name, arg_sorts.join("_"));
                        self.get_or_create_type_variable(&combined_name)
                    }
                } else {
                    self.get_or_create_type_variable(&format!("unknown_generic_{}", base.hash))
                }
            }
            StructuredType::Tuple(elements) => {
                let element_sorts: Vec<String> = elements
                    .iter()
                    .map(|elem| self.translate_structured_type(elem, compiler_env))
                    .collect();
                let tuple_name = format!("Tuple_{}", element_sorts.join("_"));
                self.get_or_create_type_variable(&tuple_name)
            }
            StructuredType::Function {
                params,
                return_type,
            } => {
                let param_sorts: Vec<String> = params
                    .iter()
                    .map(|param| self.translate_structured_type(&param.param_type, compiler_env))
                    .collect();
                let return_sort = self.translate_structured_type(return_type, compiler_env);
                let function_name = format!("Function_{}_{}", param_sorts.join("_"), return_sort);
                self.get_or_create_type_variable(&function_name)
            }
            // Handle concrete primitive types
            StructuredType::Integer64 => self.get_or_create_type_variable("Integer64"),
            StructuredType::Float64 => self.get_or_create_type_variable("Float64"),
            StructuredType::Boolean => self.get_or_create_type_variable("Boolean"),
            StructuredType::String => self.get_or_create_type_variable("String"),
            StructuredType::Atom => self.get_or_create_type_variable("Atom"),

            // Handle concrete collection types
            StructuredType::List { element_type } => {
                let element_sort = self.translate_structured_type(element_type, compiler_env);
                let list_name = format!("List_{element_sort}");
                self.get_or_create_type_variable(&list_name)
            }
            StructuredType::Map {
                key_type,
                value_type,
            } => {
                let key_sort = self.translate_structured_type(key_type, compiler_env);
                let value_sort = self.translate_structured_type(value_type, compiler_env);
                let map_name = format!("Map_{key_sort}_{value_sort}");
                self.get_or_create_type_variable(&map_name)
            }
            StructuredType::Option { inner_type } => {
                let inner_sort = self.translate_structured_type(inner_type, compiler_env);
                let option_name = format!("Option_{inner_sort}");
                self.get_or_create_type_variable(&option_name)
            }
            StructuredType::Result { ok_type, err_type } => {
                let ok_sort = self.translate_structured_type(ok_type, compiler_env);
                let err_sort = self.translate_structured_type(err_type, compiler_env);
                let result_name = format!("Result_{ok_sort}_{err_sort}");
                self.get_or_create_type_variable(&result_name)
            }
            StructuredType::Struct { name, .. } => {
                if let Some(struct_name) = compiler_env.resolve_type(name.clone()) {
                    self.get_or_create_type_variable(&struct_name)
                } else {
                    self.get_or_create_type_variable(&format!("unknown_struct_{}", name.hash))
                }
            }
            StructuredType::Trait { name, .. } => {
                if let Some(trait_name) = compiler_env.resolve_type(name.clone()) {
                    self.get_or_create_type_variable(&trait_name)
                } else {
                    self.get_or_create_type_variable(&format!("unknown_trait_{}", name.hash))
                }
            }
            StructuredType::TypeError { fallback_type, .. } => {
                // Handle type error case - use fallback type if available
                if let Some(fallback) = fallback_type {
                    self.translate_structured_type(fallback, compiler_env)
                } else {
                    self.get_or_create_type_variable("type_error")
                }
            }
        }
    }

    /// Generate trait implementation predicate: implements(impl_type, trait_type)
    pub fn create_trait_predicate(
        &mut self,
        impl_type: &StructuredType,
        trait_type: &StructuredType,
        compiler_env: &CompilerEnvironment,
    ) -> String {
        let impl_sort = self.translate_structured_type(impl_type, compiler_env);
        let trait_sort = self.translate_structured_type(trait_type, compiler_env);

        // Create SMT predicate: implements(impl_type, trait_type)
        let predicate_name = format!("implements_{impl_sort}_{trait_sort}");
        self.get_or_create_predicate(&predicate_name)
    }

    /// Convert trait implementation constraint to SMT formula
    pub fn translate_trait_constraint(
        &mut self,
        constraint: &SMTConstraint,
        compiler_env: &CompilerEnvironment,
    ) -> String {
        match constraint {
            SMTConstraint::TraitImplemented {
                impl_type,
                trait_type,
            } => self.create_trait_predicate(impl_type, trait_type, compiler_env),
            SMTConstraint::TypeUnification { type1, type2, .. } => {
                let type1_sort = self.translate_structured_type(type1, compiler_env);
                let type2_sort = self.translate_structured_type(type2, compiler_env);
                format!("(= {type1_sort} {type2_sort})")
            }
            SMTConstraint::GenericInstantiation {
                generic_type,
                concrete_candidates,
            } => {
                let generic_sort = self.translate_structured_type(generic_type, compiler_env);
                let candidate_sorts: Vec<String> = concrete_candidates
                    .iter()
                    .map(|candidate| self.translate_structured_type(candidate, compiler_env))
                    .collect();

                if candidate_sorts.is_empty() {
                    "false".to_string() // No valid instantiations
                } else if candidate_sorts.len() == 1 {
                    format!("(= {} {})", generic_sort, candidate_sorts[0])
                } else {
                    let or_clauses: Vec<String> = candidate_sorts
                        .iter()
                        .map(|candidate| format!("(= {generic_sort} {candidate})"))
                        .collect();
                    format!("(or {})", or_clauses.join(" "))
                }
            }
            SMTConstraint::FunctionSignatureMatch {
                expected, actual, ..
            } => {
                // For now, translate to equality of return types
                // TODO: Implement full signature matching
                let expected_return =
                    self.translate_structured_type(&expected.return_type, compiler_env);
                let actual_return =
                    self.translate_structured_type(&actual.return_type, compiler_env);
                format!("(= {expected_return} {actual_return})")
            }
            SMTConstraint::GuardCondition { .. } => {
                // TODO: Implement guard condition translation
                "true".to_string() // Placeholder
            }
            SMTConstraint::TypeParameterUnification {
                parameter_name,
                concrete_type,
                ..
            } => {
                // Create an equality constraint between the parameter and concrete type
                let concrete_sort = self.translate_structured_type(concrete_type, compiler_env);
                format!("(= {parameter_name} {concrete_sort})")
            }
            SMTConstraint::TypeVariableConstraint {
                variable_id,
                bound_type,
                ..
            } => {
                // TypeVariable = concrete type constraint
                let var_name =
                    if let Some(type_name) = compiler_env.resolve_type(variable_id.clone()) {
                        format!("TypeVar_{type_name}")
                    } else {
                        format!("TypeVar_unknown_{}", variable_id.hash)
                    };
                let bound_sort = self.translate_structured_type(bound_type, compiler_env);
                format!("(= {var_name} {bound_sort})")
            }
            SMTConstraint::TraitCompatibility {
                trait_type,
                implementing_type,
                ..
            } => {
                // Trait compatibility: implementing_type must implement trait_type
                // This is essentially the same as TraitImplemented but with different semantics
                self.create_trait_predicate(implementing_type, trait_type, compiler_env)
            }
            SMTConstraint::UniversalSelfConstraint {
                self_variable_id,
                trait_being_defined,
                bound_traits,
                ..
            } => {
                // Universal Self constraint: ∀ Self. (implements(Self, TraitBeingDefined) ∧ implements(Self, BoundTrait))
                let self_var_name = format!("Self_{}", self_variable_id.hash);
                let trait_defined_sort =
                    self.translate_structured_type(trait_being_defined, compiler_env);

                let mut implications = Vec::new();
                for bound_trait in bound_traits {
                    let bound_sort = self.translate_structured_type(bound_trait, compiler_env);
                    // implements(Self, TraitBeingDefined) → implements(Self, BoundTrait)
                    implications.push(format!(
                        "(=> (implements {self_var_name} {trait_defined_sort}) (implements {self_var_name} {bound_sort}))"
                    ));
                }

                if implications.is_empty() {
                    "true".to_string()
                } else if implications.len() == 1 {
                    implications[0].clone()
                } else {
                    format!("(and {})", implications.join(" "))
                }
            }
            SMTConstraint::ConcreteSelfBinding {
                self_variable_id,
                concrete_type,
                ..
            } => {
                // Concrete Self binding: Self = ConcreteType
                let self_var_name = format!("Self_{}", self_variable_id.hash);
                let concrete_sort = self.translate_structured_type(concrete_type, compiler_env);
                format!("(= {self_var_name} {concrete_sort})")
            }
            SMTConstraint::SelfTypeInference {
                self_variable_id,
                inferred_type,
                ..
            } => {
                // Self type inference: Self should equal inferred type
                let self_var_name = format!("Self_{}", self_variable_id.hash);
                let inferred_sort = self.translate_structured_type(inferred_type, compiler_env);
                format!("(= {self_var_name} {inferred_sort})")
            }
            SMTConstraint::ArgumentTypeMatch {
                clause_id,
                parameter_name,
                expected_type,
                actual_type,
                ..
            } => {
                // Argument type matching: clause is applicable if expected = actual
                let expected_sort = self.translate_structured_type(expected_type, compiler_env);
                let actual_sort = self.translate_structured_type(actual_type, compiler_env);
                let clause_var = format!("clause_applicable_{clause_id}_{parameter_name}");
                format!("(=> (= {expected_sort} {actual_sort}) {clause_var})")
            }
            SMTConstraint::GuardApplicable {
                clause_id,
                guard_expression: _,
                guard_variables: _,
                ..
            } => {
                // Guard applicability: for now, just assert applicability
                // TODO: Implement proper guard expression analysis
                let guard_var = format!("guard_applicable_{clause_id}");
                guard_var
            }
            SMTConstraint::ClausePriority {
                clause_id,
                priority,
                ..
            } => {
                // Clause priority: assert priority relationship
                let priority_var = format!("clause_priority_{clause_id}_{priority}");
                priority_var
            }
            SMTConstraint::GuardStaticallyEvaluated {
                clause_id,
                evaluation_result,
                ..
            } => {
                // Static guard evaluation: guard result is compile-time constant
                let guard_static_var = format!("guard_static_{clause_id}");
                format!("(= {guard_static_var} {evaluation_result})")
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
                let clause_var = format!("clause_selected_{selected_clause_id}");
                let mut constraints = vec![format!("(= {clause_var} true)")];
                
                // If guard was pre-evaluated, add that constraint
                if let Some(guard_result) = guard_pre_evaluated {
                    let guard_var = format!("guard_preresolve_{selected_clause_id}");
                    constraints.push(format!("(= {guard_var} {guard_result})"));
                }
                
                if constraints.len() == 1 {
                    constraints[0].clone()
                } else {
                    format!("(and {})", constraints.join(" "))
                }
            }
            
            // Exhaustiveness analysis constraints
            SMTConstraint::FunctionClauseSetExhaustive { function_name, clauses, .. } => {
                format!("(exhaustive_{} {})", function_name, clauses.join(" "))
            }
            SMTConstraint::FunctionClauseReachable { clause_id, earlier_clauses, .. } => {
                format!("(reachable_{} {})", clause_id, earlier_clauses.join(" "))
            }
            SMTConstraint::GuardCoverageComplete { function_name, guard_clauses, has_default_clause, .. } => {
                let guards: Vec<String> = guard_clauses.iter().map(|g| g.clause_id.clone()).collect();
                format!("(coverage_complete_{} {} {})", function_name, guards.join(" "), has_default_clause)
            }
            SMTConstraint::GuardConditionSatisfiable { clause_id, guard_expression, .. } => {
                format!("(satisfiable_guard_{} {})", clause_id, guard_expression.replace(' ', "_"))
            }
            SMTConstraint::PendingClauseResolution { .. } => {
                // PendingClauseResolution constraints are not translated to SMT
                // They are processed after SMT solving
                "(pending_clause_resolution true)".to_string()
            }
        }
    }

    /// Convert unification constraint to SMT equality
    pub fn translate_unification(
        &mut self,
        type1: &StructuredType,
        type2: &StructuredType,
        compiler_env: &CompilerEnvironment,
    ) -> String {
        let type1_sort = self.translate_structured_type(type1, compiler_env);
        let type2_sort = self.translate_structured_type(type2, compiler_env);
        format!("(= {type1_sort} {type2_sort})")
    }

    /// Generate SMT declarations for all types in scope
    pub fn generate_type_declarations(&self) -> String {
        let mut declarations = Vec::new();

        // Declare type sort
        declarations.push("(declare-sort Type 0)".to_string());

        // Declare all type variables
        for smt_var in self.type_variables.values() {
            declarations.push(format!("(declare-const {smt_var} Type)"));
        }

        // Declare implements predicate
        declarations.push("(declare-fun implements (Type Type) Bool)".to_string());

        // Declare all trait predicates
        for smt_predicate in self.trait_predicates.values() {
            declarations.push(format!("(declare-const {smt_predicate} Bool)"));
        }

        declarations.join("\n")
    }

    /// Get or create an SMT variable for a type name
    fn get_or_create_type_variable(&mut self, type_name: &str) -> String {
        if let Some(existing) = self.type_variables.get(type_name) {
            existing.clone()
        } else {
            let smt_var = format!("type_{}", self.type_counter);
            self.type_counter += 1;
            self.type_variables
                .insert(type_name.to_string(), smt_var.clone());
            smt_var
        }
    }

    /// Get or create an SMT predicate for a predicate name
    fn get_or_create_predicate(&mut self, predicate_name: &str) -> String {
        if let Some(existing) = self.trait_predicates.get(predicate_name) {
            existing.clone()
        } else {
            let smt_predicate = format!("pred_{}", self.predicate_counter);
            self.predicate_counter += 1;
            self.trait_predicates
                .insert(predicate_name.to_string(), smt_predicate.clone());
            smt_predicate
        }
    }

    /// Reset the translator state
    pub fn reset(&mut self) {
        self.type_variables.clear();
        self.trait_predicates.clear();
        self.type_counter = 0;
        self.predicate_counter = 0;
    }

    /// Get the current type variable mapping
    pub fn get_type_variables(&self) -> &HashMap<String, String> {
        &self.type_variables
    }

    /// Get the current trait predicate mapping
    pub fn get_trait_predicates(&self) -> &HashMap<String, String> {
        &self.trait_predicates
    }
}

impl Default for SMTTranslator {
    fn default() -> Self {
        Self::new()
    }
}
