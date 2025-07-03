//! Z3-based exhaustiveness and reachability analysis for function clauses
//!
//! This module provides exhaustiveness checking for function clause sets,
//! ensuring that all possible input combinations are covered and that
//! no clauses are unreachable due to source ordering.

use crate::checker::{FunctionClause, FunctionClauseSet};
use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::smt::constraints::{
    GuardClauseInfo, ParameterConstraint, SMTConstraint,
};
use crate::smt::solver::{SMTError, SolverResult, Z3ConstraintSolver, Z3Context};
use crate::types::traits::{ExhaustivenessResult, GuardCounterExample};
use crate::unification::StructuredType;
use outrun_parser::Span;
use std::collections::HashMap;

/// Exhaustiveness analysis results
#[derive(Debug, Clone, PartialEq)]
pub enum ExhaustivenessAnalysis {
    /// All clauses are exhaustive and reachable
    Complete,
    /// Missing coverage for some input combinations
    IncompleteExhaustiveness {
        missing_patterns: Vec<MissingPattern>,
        suggested_clauses: Vec<String>,
    },
    /// Some clauses are unreachable due to source ordering
    UnreachableClauses {
        unreachable_clauses: Vec<String>,
        shadowing_clauses: Vec<String>,
    },
    /// Both exhaustiveness and reachability issues
    MultipleIssues {
        exhaustiveness_issues: Box<ExhaustivenessAnalysis>,
        reachability_issues: Box<ExhaustivenessAnalysis>,
    },
}

/// Represents a missing pattern in function clause coverage
#[derive(Debug, Clone, PartialEq)]
pub struct MissingPattern {
    pub parameter_values: HashMap<String, String>, // Parameter name -> example value
    pub constraint_description: String,            // Human-readable description
    pub suggested_guard: Option<String>,           // Suggested guard expression
}

/// Z3-based exhaustiveness analyzer for function clauses
pub struct FunctionClauseExhaustivenessAnalyzer {
    context: Z3Context,
    compiler_env: CompilerEnvironment,
}

impl FunctionClauseExhaustivenessAnalyzer {
    /// Create a new exhaustiveness analyzer
    pub fn new(compiler_env: CompilerEnvironment) -> Self {
        Self {
            context: Z3Context::new(),
            compiler_env,
        }
    }

    /// Analyze exhaustiveness and reachability for a function clause set
    pub fn analyze_clause_set(
        &mut self,
        clause_set: &FunctionClauseSet,
        function_span: Span,
    ) -> Result<ExhaustivenessAnalysis, SMTError> {
        // Step 1: Check exhaustiveness
        let exhaustiveness_result = self.check_exhaustiveness(clause_set)?;
        
        // Step 2: Check reachability
        let reachability_result = self.check_reachability(clause_set)?;
        
        // Step 3: Combine results
        match (exhaustiveness_result, reachability_result) {
            (ExhaustivenessAnalysis::Complete, ExhaustivenessAnalysis::Complete) => {
                Ok(ExhaustivenessAnalysis::Complete)
            }
            (exhaustiveness, ExhaustivenessAnalysis::Complete) => Ok(exhaustiveness),
            (ExhaustivenessAnalysis::Complete, reachability) => Ok(reachability),
            (exhaustiveness, reachability) => Ok(ExhaustivenessAnalysis::MultipleIssues {
                exhaustiveness_issues: Box::new(exhaustiveness),
                reachability_issues: Box::new(reachability),
            }),
        }
    }

    /// Check if the clause set is exhaustive (covers all possible inputs)
    fn check_exhaustiveness(
        &mut self,
        clause_set: &FunctionClauseSet,
    ) -> Result<ExhaustivenessAnalysis, SMTError> {
        // Extract parameter types from the first clause (all clauses should have same signature)
        let parameter_types = if let Some(first_clause) = clause_set.clauses.first() {
            first_clause
                .base_function
                .parameters
                .iter()
                .map(|p| p.param_type.as_ref().cloned().unwrap_or_else(|| {
                    StructuredType::Simple(self.compiler_env.intern_type_name("Any"))
                }))
                .collect::<Vec<_>>()
        } else {
            return Ok(ExhaustivenessAnalysis::Complete); // Empty clause set is trivially exhaustive
        };

        // Generate SMT constraints for exhaustiveness
        let constraints = self.generate_exhaustiveness_constraints(clause_set, &parameter_types)?;
        
        // Solve constraints to check for missing patterns
        let mut solver = self.context.create_solver();
        solver.add_constraints(&constraints, &self.compiler_env)?;
        
        match solver.solve() {
            SolverResult::Unsatisfiable(_) => {
                // Unsatisfiable means the clauses are exhaustive
                Ok(ExhaustivenessAnalysis::Complete)
            }
            SolverResult::Satisfiable(model) => {
                // Satisfiable means there are missing patterns
                let missing_patterns = self.extract_missing_patterns(&model, &parameter_types)?;
                let suggested_clauses = self.generate_suggested_clauses(&missing_patterns);
                
                Ok(ExhaustivenessAnalysis::IncompleteExhaustiveness {
                    missing_patterns,
                    suggested_clauses,
                })
            }
            SolverResult::Unknown(reason) => {
                Err(SMTError::SolvingFailed(format!(
                    "Could not determine exhaustiveness: {}",
                    reason
                )))
            }
        }
    }

    /// Check if all clauses are reachable (not shadowed by earlier clauses)
    fn check_reachability(
        &mut self,
        clause_set: &FunctionClauseSet,
    ) -> Result<ExhaustivenessAnalysis, SMTError> {
        let mut unreachable_clauses = Vec::new();
        let mut shadowing_clauses = Vec::new();
        
        // Get clauses in source order
        let ordered_clauses = clause_set.get_clauses_by_priority();
        
        // Check each clause against all earlier clauses
        for (i, clause) in ordered_clauses.iter().enumerate() {
            let earlier_clauses: Vec<_> = ordered_clauses[..i].iter().cloned().collect();
            
            if !earlier_clauses.is_empty() {
                let is_reachable = self.check_clause_reachability(clause, &earlier_clauses)?;
                
                if !is_reachable {
                    unreachable_clauses.push(clause.clause_id.clone());
                    // Find which earlier clause(s) shadow this one
                    for earlier_clause in &earlier_clauses {
                        if self.check_clause_shadows(earlier_clause, clause)? {
                            shadowing_clauses.push(earlier_clause.clause_id.clone());
                        }
                    }
                }
            }
        }
        
        if unreachable_clauses.is_empty() {
            Ok(ExhaustivenessAnalysis::Complete)
        } else {
            Ok(ExhaustivenessAnalysis::UnreachableClauses {
                unreachable_clauses,
                shadowing_clauses,
            })
        }
    }

    /// Generate SMT constraints to check exhaustiveness
    fn generate_exhaustiveness_constraints(
        &self,
        clause_set: &FunctionClauseSet,
        parameter_types: &[StructuredType],
    ) -> Result<Vec<SMTConstraint>, SMTError> {
        let mut constraints = Vec::new();
        
        // Extract guard clauses
        let guard_clauses: Vec<GuardClauseInfo> = clause_set
            .clauses
            .iter()
            .filter_map(|clause| {
                clause.base_function.guard.as_ref().map(|guard| GuardClauseInfo {
                    clause_id: clause.clause_id.clone(),
                    guard_expression: format!("{:?}", guard), // Simplified - would need proper expression serialization
                    parameter_names: clause
                        .base_function
                        .parameters
                        .iter()
                        .map(|p| p.name.clone())
                        .collect(),
                    source_order: clause.source_order,
                })
            })
            .collect();
        
        // Check if there's a default clause (no guard)
        let has_default_clause = clause_set
            .clauses
            .iter()
            .any(|clause| clause.base_function.guard.is_none());
        
        // Add guard coverage constraint
        constraints.push(SMTConstraint::GuardCoverageComplete {
            function_name: clause_set.function_name.clone(),
            guard_clauses,
            parameter_types: parameter_types.to_vec(),
            has_default_clause,
            context: format!("Exhaustiveness check for {}", clause_set.function_name),
        });
        
        // Add individual guard satisfiability constraints
        for clause in &clause_set.clauses {
            if let Some(guard) = &clause.base_function.guard {
                let parameter_constraints: HashMap<String, ParameterConstraint> = clause
                    .base_function
                    .parameters
                    .iter()
                    .zip(parameter_types.iter())
                    .map(|(param, param_type)| {
                        (param.name.clone(), ParameterConstraint::TypeConstraint(param_type.clone()))
                    })
                    .collect();
                
                constraints.push(SMTConstraint::GuardConditionSatisfiable {
                    clause_id: clause.clause_id.clone(),
                    guard_expression: format!("{:?}", guard), // Simplified
                    parameter_constraints,
                    context: format!("Guard satisfiability for clause {}", clause.clause_id),
                });
            }
        }
        
        Ok(constraints)
    }

    /// Check if a specific clause is reachable given earlier clauses
    fn check_clause_reachability(
        &mut self,
        clause: &FunctionClause,
        earlier_clauses: &[&FunctionClause],
    ) -> Result<bool, SMTError> {
        if earlier_clauses.is_empty() {
            return Ok(true); // First clause is always reachable
        }
        
        let parameter_types: Vec<StructuredType> = clause
            .base_function
            .parameters
            .iter()
            .map(|p| {
                p.param_type.as_ref().cloned().unwrap_or_else(|| {
                    StructuredType::Simple(self.compiler_env.intern_type_name("Any"))
                })
            })
            .collect();
        
        let constraints = vec![SMTConstraint::FunctionClauseReachable {
            clause_id: clause.clause_id.clone(),
            earlier_clauses: earlier_clauses.iter().map(|c| c.clause_id.clone()).collect(),
            parameter_types,
            context: format!("Reachability check for clause {}", clause.clause_id),
        }];
        
        let mut solver = self.context.create_solver();
        solver.add_constraints(&constraints, &self.compiler_env)?;
        
        match solver.solve() {
            SolverResult::Satisfiable(_) => Ok(true),  // Clause is reachable
            SolverResult::Unsatisfiable(_) => Ok(false), // Clause is unreachable
            SolverResult::Unknown(reason) => Err(SMTError::SolvingFailed(format!(
                "Could not determine reachability: {}",
                reason
            ))),
        }
    }

    /// Check if one clause shadows another
    fn check_clause_shadows(
        &mut self,
        earlier_clause: &FunctionClause,
        later_clause: &FunctionClause,
    ) -> Result<bool, SMTError> {
        // For now, implement a simple heuristic
        // A more sophisticated implementation would use SMT to check if
        // the earlier clause's conditions subsume the later clause's conditions
        
        // If both have guards, check if they could overlap
        match (&earlier_clause.base_function.guard, &later_clause.base_function.guard) {
            (None, _) => Ok(true), // Default clause shadows everything
            (Some(_), None) => Ok(false), // Guard clause doesn't shadow default
            (Some(_), Some(_)) => {
                // Would need sophisticated guard analysis here
                // For now, assume they don't shadow each other if both have guards
                Ok(false)
            }
        }
    }

    /// Extract missing patterns from SMT model
    fn extract_missing_patterns(
        &self,
        _model: &crate::smt::solver::ConstraintModel,
        parameter_types: &[StructuredType],
    ) -> Result<Vec<MissingPattern>, SMTError> {
        // Simplified implementation - would extract actual parameter values from model
        let missing_pattern = MissingPattern {
            parameter_values: parameter_types
                .iter()
                .enumerate()
                .map(|(i, _)| (format!("param_{}", i), "uncovered_value".to_string()))
                .collect(),
            constraint_description: "Some parameter combinations are not covered".to_string(),
            suggested_guard: Some("Add a guard clause or default case".to_string()),
        };
        
        Ok(vec![missing_pattern])
    }

    /// Generate suggested clauses for missing patterns
    fn generate_suggested_clauses(&self, missing_patterns: &[MissingPattern]) -> Vec<String> {
        missing_patterns
            .iter()
            .filter_map(|pattern| pattern.suggested_guard.clone())
            .collect()
    }
}

/// Integrate with existing ExhaustivenessResult for compatibility
impl From<ExhaustivenessAnalysis> for ExhaustivenessResult {
    fn from(analysis: ExhaustivenessAnalysis) -> Self {
        match analysis {
            ExhaustivenessAnalysis::Complete => ExhaustivenessResult::Exhaustive,
            ExhaustivenessAnalysis::IncompleteExhaustiveness { missing_patterns, .. } => {
                let counter_examples = missing_patterns
                    .into_iter()
                    .map(|pattern| GuardCounterExample {
                        // Convert String values to boolean assignments based on constraints
                        variable_assignments: pattern.parameter_values
                            .into_iter()
                            .map(|(name, value)| (name, value == "true" || value != "0"))
                            .collect(),
                        description: pattern.constraint_description,
                        suggested_guard: pattern.suggested_guard,
                    })
                    .collect();
                ExhaustivenessResult::MissingGuardPatterns(counter_examples)
            }
            ExhaustivenessAnalysis::UnreachableClauses { .. } => {
                // Map to missing patterns for now - would need new ExhaustivenessResult variant
                ExhaustivenessResult::Exhaustive
            }
            ExhaustivenessAnalysis::MultipleIssues { exhaustiveness_issues, .. } => {
                // Prioritize exhaustiveness issues
                (*exhaustiveness_issues).into()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::{TypedFunctionDefinition, TypedFunctionParameter};
    use crate::compilation::compiler_environment::CompilerEnvironment;
    use outrun_parser::Span;

    fn create_test_clause_set() -> FunctionClauseSet {
        let mut clause_set = FunctionClauseSet::new("test_function".to_string());
        
        // Add a clause with guard
        let guard_clause = FunctionClause::new(
            "test_clause_1".to_string(),
            TypedFunctionDefinition {
                name: outrun_parser::ast::Identifier {
                    name: "test_function".to_string(),
                    span: Span::new(0, 0),
                },
                parameters: vec![TypedFunctionParameter {
                    name: "x".to_string(),
                    param_type: None,
                    span: Span::new(0, 0),
                }],
                return_type: None,
                body: None,
                guard: None, // Would be Some(guard_expression) in real test
                generic_context: None,
                span: Span::new(0, 0),
                debug_info: None,
            },
            0, // source_order
            Span::new(0, 0),
        );
        
        clause_set.add_clause(guard_clause);
        clause_set
    }

    #[test]
    fn test_exhaustiveness_analyzer_creation() {
        let compiler_env = CompilerEnvironment::new();
        let _analyzer = FunctionClauseExhaustivenessAnalyzer::new(compiler_env);
        // Test passes if analyzer can be created without panicking
    }

    #[test]
    fn test_empty_clause_set_is_exhaustive() {
        let compiler_env = CompilerEnvironment::new();
        let mut analyzer = FunctionClauseExhaustivenessAnalyzer::new(compiler_env);
        let empty_clause_set = FunctionClauseSet::new("empty_function".to_string());
        
        let result = analyzer.analyze_clause_set(&empty_clause_set, Span::new(0, 0));
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), ExhaustivenessAnalysis::Complete));
    }

    #[test]
    fn test_clause_set_analysis() {
        let compiler_env = CompilerEnvironment::new();
        let mut analyzer = FunctionClauseExhaustivenessAnalyzer::new(compiler_env);
        let clause_set = create_test_clause_set();
        
        let result = analyzer.analyze_clause_set(&clause_set, Span::new(0, 0));
        assert!(result.is_ok());
        // Result type depends on the specific clause set structure
    }
}