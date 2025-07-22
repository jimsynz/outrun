//! Constraint solving for protocol bounds
//!
//! Implements logical constraint satisfaction for Outrun's protocol system.
//! Handles protocol implementation verification, logical operators, and orphan rules.

use crate::error::ConstraintError;
use crate::types::{Constraint, ProtocolId, SelfBindingContext, Substitution, Type, TypeVarId};
use std::collections::{HashMap, HashSet};

/// Context information about available protocol implementations
#[derive(Debug, Clone)]
pub struct ImplementationContext {
    /// Map from (Protocol, Type) pairs to whether implementation exists
    implementations: HashMap<(ProtocolId, String), bool>,
    /// Set of orphan implementations that need special validation
    orphan_implementations: HashSet<(ProtocolId, String)>,
}

impl ImplementationContext {
    pub fn new() -> Self {
        Self {
            implementations: HashMap::new(),
            orphan_implementations: HashSet::new(),
        }
    }

    /// Register that a type implements a protocol
    pub fn register_implementation(&mut self, protocol: ProtocolId, type_name: String) {
        self.implementations.insert((protocol, type_name), true);
    }

    /// Mark an implementation as orphan (for orphan rule checking)
    pub fn mark_orphan(&mut self, protocol: ProtocolId, type_name: String) {
        self.orphan_implementations.insert((protocol, type_name));
    }

    /// Check if a type implements a protocol
    pub fn has_implementation(&self, protocol: &ProtocolId, type_name: &str) -> bool {
        self.implementations
            .get(&(protocol.clone(), type_name.to_string()))
            .copied()
            .unwrap_or(false)
    }
}

impl Default for ImplementationContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Constraint solver implementing logical constraint satisfaction
#[derive(Debug)]
pub struct ConstraintSolver {
    /// Context of available protocol implementations
    implementation_context: ImplementationContext,
    /// Cache of solved constraint results
    solution_cache: HashMap<String, bool>,
}

impl ConstraintSolver {
    pub fn new() -> Self {
        Self {
            implementation_context: ImplementationContext::new(),
            solution_cache: HashMap::new(),
        }
    }

    /// Create solver with existing implementation context
    pub fn with_context(context: ImplementationContext) -> Self {
        Self {
            implementation_context: context,
            solution_cache: HashMap::new(),
        }
    }

    /// Get mutable access to implementation context
    pub fn implementation_context_mut(&mut self) -> &mut ImplementationContext {
        &mut self.implementation_context
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
        }
    }

    /// Solve an implements constraint: T: Protocol
    fn solve_implements_constraint(
        &mut self,
        type_var: TypeVarId,
        protocol: &ProtocolId,
        substitution: &Substitution,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), ConstraintError> {
        // Apply substitution to resolve the type variable
        let resolved_type = self.resolve_type_var(type_var, substitution)?;

        match resolved_type {
            Type::Concrete { id, args, .. } => {
                // Check if this concrete type implements the protocol
                let type_name = if args.is_empty() {
                    id.name().to_string()
                } else {
                    // For generic types like List<String>, create a canonical representation
                    format!(
                        "{}[{}]",
                        id.name(),
                        args.iter()
                            .map(|arg| self.type_to_string(arg))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };

                if !self
                    .implementation_context
                    .has_implementation(protocol, &type_name)
                {
                    return Err(ConstraintError::MissingImplementation {
                        type_name,
                        protocol_name: protocol.0.clone(),
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

            Type::Protocol { id, .. } => {
                // Protocol types automatically implement themselves
                if id == *protocol {
                    Ok(())
                } else {
                    // Check if there's a relationship between protocols
                    Err(ConstraintError::MissingImplementation {
                        type_name: format!("protocol {}", id.0),
                        protocol_name: protocol.0.clone(),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    })
                }
            }

            Type::SelfType { binding_context, .. } => {
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
    #[allow(clippy::only_used_in_recursion)]
    fn type_to_string(&self, ty: &Type) -> String {
        match ty {
            Type::Concrete { id, args, .. } => {
                if args.is_empty() {
                    id.name().to_string()
                } else {
                    format!(
                        "{}[{}]",
                        id.name(),
                        args.iter()
                            .map(|arg| self.type_to_string(arg))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Type::Variable { var_id, .. } => format!("T{}", var_id.0),
            Type::Protocol { id, .. } => format!("protocol {}", id.0),
            Type::SelfType { .. } => "Self".to_string(),
            Type::Function { .. } => "function".to_string(),
        }
    }

    /// Solve a Self implements constraint: Self: Protocol
    fn solve_self_implements_constraint(
        &mut self,
        self_context: &SelfBindingContext,
        protocol: &ProtocolId,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), ConstraintError> {
        match self_context {
            SelfBindingContext::Implementation {
                implementing_type,
                implementing_args,
                ..
            } => {
                // In implementation context, Self is bound to the concrete implementing type
                let type_name = if implementing_args.is_empty() {
                    implementing_type.name().to_string()
                } else {
                    // For generic implementations like impl Display for List<T>
                    format!(
                        "{}[{}]",
                        implementing_type.name(),
                        implementing_args
                            .iter()
                            .map(|arg| self.type_to_string(arg))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };

                if !self
                    .implementation_context
                    .has_implementation(protocol, &type_name)
                {
                    return Err(ConstraintError::MissingImplementation {
                        type_name,
                        protocol_name: protocol.0.clone(),
                        span: span.and_then(|s| crate::error::to_source_span(Some(s))),
                    });
                }

                Ok(())
            }

            SelfBindingContext::ProtocolDefinition { protocol_id, .. } => {
                // In protocol definition context, Self: Protocol is valid if:
                // 1. The protocol is the same as the defining protocol (trivially true)
                // 2. The protocol is a super-protocol of the defining protocol
                if protocol_id == protocol {
                    // Self: SameProtocol is trivially satisfied
                    Ok(())
                } else {
                    // Check if there's a protocol hierarchy relationship
                    // For now, assume no super-protocols (this would be extended in a full implementation)
                    Err(ConstraintError::MissingImplementation {
                        type_name: "Self".to_string(),
                        protocol_name: protocol.0.clone(),
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
    #[allow(clippy::only_used_in_recursion)]
    fn solve_self_binding_constraint(
        &mut self,
        self_context: &SelfBindingContext,
        bound_type: &Type,
        substitution: &Substitution,
        span: Option<outrun_parser::Span>,
    ) -> Result<(), ConstraintError> {
        // Apply substitution to the bound type
        let resolved_bound_type = substitution.apply(bound_type);

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

                Ok(())
            }

            SelfBindingContext::ProtocolDefinition { .. } => {
                // In protocol definition, Self binding constraints establish requirements
                // for implementers. For now, accept any binding (would need more sophisticated
                // validation in a full implementation)
                Ok(())
            }

            SelfBindingContext::FunctionContext { parent_context, .. } => {
                // Delegate to parent context
                self.solve_self_binding_constraint(parent_context, bound_type, substitution, span)
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

    /// Clear the solution cache (useful when implementation context changes)
    pub fn clear_cache(&mut self) {
        self.solution_cache.clear();
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

    fn create_test_context() -> ImplementationContext {
        let mut context = ImplementationContext::new();

        // Register some basic implementations for testing
        context.register_implementation(ProtocolId::new("Display"), "Integer".to_string());
        context.register_implementation(ProtocolId::new("Display"), "String".to_string());
        context.register_implementation(ProtocolId::new("Debug"), "Integer".to_string());
        context.register_implementation(ProtocolId::new("Debug"), "String".to_string());
        context.register_implementation(ProtocolId::new("Clone"), "String".to_string());

        context
    }

    #[test]
    fn test_simple_implements_constraint_satisfied() {
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("Display"),
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
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: NonExistentProtocol
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("NonExistentProtocol"),
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
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display || T: NonExistentProtocol
            let left = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("Display"),
                span: None,
            };
            let right = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("NonExistentProtocol"),
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
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: NonExistent1 || T: NonExistent2
            let left = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("NonExistent1"),
                span: None,
            };
            let right = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("NonExistent2"),
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
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display && T: Debug
            let left = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("Display"),
                span: None,
            };
            let right = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("Debug"),
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
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display && T: NonExistent
            let left = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("Display"),
                span: None,
            };
            let right = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("NonExistent"),
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
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("Display"),
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
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("Display"),
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
    fn test_implementation_context_registration() {
        let mut context = ImplementationContext::new();
        let protocol = ProtocolId::new("TestProtocol");
        let type_name = "TestType".to_string();

        // Initially no implementation
        assert!(!context.has_implementation(&protocol, &type_name));

        // Register implementation
        context.register_implementation(protocol.clone(), type_name.clone());

        // Now has implementation
        assert!(context.has_implementation(&protocol, &type_name));
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
                protocol: ProtocolId::new("TestProtocol"),
                span: None,
            };
            let constraint2 = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("TestProtocol"),
                span: None,
            };

            let result = solver.check_conflicts(&[constraint1, constraint2]);
            // This shouldn't conflict - same constraint repeated
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_protocol_self_implementation() {
        let mut solver = ConstraintSolver::with_context(create_test_context());
        let mut gen = TypeVarGenerator::new();

        let type_var = gen.fresh(Level(0));
        if let Type::Variable { var_id, .. } = type_var {
            // Create constraint T: Display
            let constraint = Constraint::Implements {
                type_var: var_id,
                protocol: ProtocolId::new("Display"),
                span: None,
            };

            // Create substitution T -> Display protocol type
            let protocol_type = Type::Protocol {
                id: ProtocolId::new("Display"),
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
        let mut solver = ConstraintSolver::with_context(create_test_context());

        // Create Self: Display constraint in implementation context
        let self_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("String"),
            implementing_args: vec![],
            protocol_id: ProtocolId::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ProtocolId::new("Display"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // String implements Display, so Self: Display should succeed
        assert!(result.is_ok());
    }

    #[test]
    fn test_self_implements_in_implementation_context_missing() {
        let mut solver = ConstraintSolver::with_context(create_test_context());

        // Create Self: NonExistentProtocol constraint
        let self_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("String"),
            implementing_args: vec![],
            protocol_id: ProtocolId::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ProtocolId::new("NonExistentProtocol"),
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
            protocol_id: ProtocolId::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ProtocolId::new("Display"),
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
            protocol_id: ProtocolId::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ProtocolId::new("Debug"),
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
            protocol_id: ProtocolId::new("Display"),
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
            protocol_id: ProtocolId::new("Display"),
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
        let mut context = ImplementationContext::new();
        context.register_implementation(ProtocolId::new("Display"), "List[String]".to_string());
        let mut solver = ConstraintSolver::with_context(context);

        // Create Self: Display constraint in List<String> implementation context
        let self_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("List"),
            implementing_args: vec![Type::concrete("String")],
            protocol_id: ProtocolId::new("Display"),
            protocol_args: vec![],
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ProtocolId::new("Display"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // List<String> implements Display, so Self: Display should succeed
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_context_self_constraint() {
        let mut solver = ConstraintSolver::with_context(create_test_context());

        // Create Self: Display constraint in function context within String implementation
        let parent_context = SelfBindingContext::Implementation {
            implementing_type: TypeId::new("String"),
            implementing_args: vec![],
            protocol_id: ProtocolId::new("Display"),
            protocol_args: vec![],
        };
        let self_context = SelfBindingContext::FunctionContext {
            parent_context: Box::new(parent_context),
            function_name: Some("to_string".to_string()),
        };
        let constraint = Constraint::SelfImplements {
            self_context,
            protocol: ProtocolId::new("Display"),
            span: None,
        };

        let substitution = Substitution::new();
        let result = solver.solve(&[constraint], &substitution);
        // Should delegate to parent context and succeed
        assert!(result.is_ok());
    }
}
