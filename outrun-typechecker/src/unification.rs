//! Hindley-Milner Unification Algorithm
//!
//! Core unification algorithm with occurs check and clear error messages.
//! Extends existing error patterns from the parser.

use crate::error::{to_source_span, UnificationError};

use crate::types::{Level, Substitution, Type, TypeVarId};
use outrun_parser::Span;

/// A unification task for iterative processing
#[derive(Debug, Clone)]
struct UnifyTask {
    left: Type,
    right: Type,
    left_context: Option<String>,
    right_context: Option<String>,
}

/// Unification algorithm implementing Hindley-Milner type inference
#[derive(Debug)]
pub struct Unifier {
    /// Current level for let-polymorphism
    current_level: Level,
    /// Enable occurs check (prevents infinite types)
    occurs_check: bool,
}

impl Unifier {
    /// Create a new unifier
    pub fn new() -> Self {
        Self {
            current_level: Level(0),
            occurs_check: true,
        }
    }

    /// Create unifier with specific level
    pub fn with_level(level: Level) -> Self {
        Self {
            current_level: level,
            occurs_check: true,
        }
    }

    /// Unify two types, returning a substitution that makes them equal
    pub fn unify(&mut self, left: &Type, right: &Type) -> Result<Substitution, UnificationError> {
        self.unify_with_context(left, right, None, None)
    }

    /// Unify two types with source context for better error messages
    /// Uses iterative work queue to prevent stack overflow on deep type hierarchies
    pub fn unify_with_context(
        &mut self,
        left: &Type,
        right: &Type,
        left_context: Option<&str>,
        right_context: Option<&str>,
    ) -> Result<Substitution, UnificationError> {
        // Use iterative work queue approach to prevent stack overflow
        self.unify_iterative(left, right, left_context, right_context)
    }

    /// Iterative unification implementation using work queue
    fn unify_iterative(
        &mut self,
        initial_left: &Type,
        initial_right: &Type,
        initial_left_context: Option<&str>,
        initial_right_context: Option<&str>,
    ) -> Result<Substitution, UnificationError> {
        let mut work_queue = std::collections::VecDeque::new();
        let mut result_substitution = Substitution::new();

        // Add initial task
        work_queue.push_back(UnifyTask {
            left: initial_left.clone(),
            right: initial_right.clone(),
            left_context: initial_left_context.map(String::from),
            right_context: initial_right_context.map(String::from),
        });

        while let Some(task) = work_queue.pop_front() {
            let left = &task.left;
            let right = &task.right;
            let left_context = task.left_context.as_deref();
            let right_context = task.right_context.as_deref();

            let task_substitution =
                self.unify_single_step(left, right, left_context, right_context, &mut work_queue)?;
            result_substitution = result_substitution.compose(&task_substitution);
        }

        Ok(result_substitution)
    }

    /// Handle a single unification step, potentially adding sub-tasks to the work queue
    fn unify_single_step(
        &mut self,
        left: &Type,
        right: &Type,
        left_context: Option<&str>,
        right_context: Option<&str>,
        work_queue: &mut std::collections::VecDeque<UnifyTask>,
    ) -> Result<Substitution, UnificationError> {
        match (left, right) {
            // Variable unification
            (Type::Variable { var_id: var1, .. }, Type::Variable { var_id: var2, .. })
                if var1 == var2 =>
            {
                // Same variable unifies with itself
                Ok(Substitution::new())
            }

            (Type::Variable { var_id, level, .. }, other)
            | (other, Type::Variable { var_id, level, .. }) => {
                self.unify_variable(*var_id, *level, other)
            }

            // Concrete type unification
            (
                Type::Concrete {
                    name: id1,
                    args: args1,
                    ..
                },
                Type::Concrete {
                    name: id2,
                    args: args2,
                    ..
                },
            ) => {
                if id1 != id2 {
                    return Err(UnificationError::TypeMismatch {
                        expected: left.clone(),
                        found: right.clone(),
                        expected_context: left_context.map(String::from),
                        found_context: right_context.map(String::from),
                        span: to_source_span(self.get_span_for_error(left, right)),
                    });
                }

                if args1.len() != args2.len() {
                    return Err(UnificationError::ArityMismatch {
                        type_name: id1.as_str().to_string(),
                        expected_arity: args1.len(),
                        found_arity: args2.len(),
                        span: to_source_span(self.get_span_for_error(left, right)),
                    });
                }

                // Queue generic arguments for unification instead of recursing
                for (i, (arg1, arg2)) in args1.iter().zip(args2.iter()).enumerate() {
                    work_queue.push_back(UnifyTask {
                        left: arg1.clone(),
                        right: arg2.clone(),
                        left_context: Some(format!("generic argument {}", i)),
                        right_context: Some(format!("generic argument {}", i)),
                    });
                }
                Ok(Substitution::new())
            }

            // Protocol type unification
            (
                Type::Protocol {
                    name: id1,
                    args: args1,
                    ..
                },
                Type::Protocol {
                    name: id2,
                    args: args2,
                    ..
                },
            ) => {
                if id1 != id2 {
                    return Err(UnificationError::ProtocolMismatch {
                        expected: id1.as_str().to_string(),
                        found: id2.as_str().to_string(),
                        span: to_source_span(self.get_span_for_error(left, right)),
                    });
                }

                if args1.len() != args2.len() {
                    return Err(UnificationError::ArityMismatch {
                        type_name: id1.as_str().to_string(),
                        expected_arity: args1.len(),
                        found_arity: args2.len(),
                        span: to_source_span(self.get_span_for_error(left, right)),
                    });
                }

                // Queue protocol arguments for unification instead of recursing
                for (i, (arg1, arg2)) in args1.iter().zip(args2.iter()).enumerate() {
                    work_queue.push_back(UnifyTask {
                        left: arg1.clone(),
                        right: arg2.clone(),
                        left_context: Some(format!("protocol argument {}", i)),
                        right_context: Some(format!("protocol argument {}", i)),
                    });
                }
                Ok(Substitution::new())
            }

            // Function type unification
            (
                Type::Function {
                    params: params1,
                    return_type: ret1,
                    ..
                },
                Type::Function {
                    params: params2,
                    return_type: ret2,
                    ..
                },
            ) => {
                if params1.len() != params2.len() {
                    return Err(UnificationError::FunctionArityMismatch {
                        expected_params: params1.len(),
                        found_params: params2.len(),
                        span: to_source_span(self.get_span_for_error(left, right)),
                    });
                }

                // Queue parameter types for unification instead of recursing
                for ((name1, type1), (name2, type2)) in params1.iter().zip(params2.iter()) {
                    work_queue.push_back(UnifyTask {
                        left: type1.clone(),
                        right: type2.clone(),
                        left_context: Some(format!("parameter '{name1}'")),
                        right_context: Some(format!("parameter '{name2}'")),
                    });
                }

                // Queue return types for unification instead of recursing
                work_queue.push_back(UnifyTask {
                    left: (**ret1).clone(),
                    right: (**ret2).clone(),
                    left_context: Some("return type".to_string()),
                    right_context: Some("return type".to_string()),
                });

                Ok(Substitution::new())
            }

            // Self type unification (handled differently based on context)
            (Type::SelfType { .. }, Type::SelfType { .. }) => {
                // Two Self types unify if in the same binding context
                // This is a simplification - real implementation needs context checking
                Ok(Substitution::new())
            }

            // Self type with concrete type - resolve Self and unify
            (Type::SelfType { .. }, concrete_type) => {
                eprintln!(
                    "🔍 Unification: Trying to unify Self with concrete type {}",
                    concrete_type
                );
                if let Some(resolved_self) = left.resolve_self() {
                    eprintln!("🔍 Unification: Self resolved to {}", resolved_self);
                    work_queue.push_back(UnifyTask {
                        left: resolved_self,
                        right: concrete_type.clone(),
                        left_context: Some("resolved Self type".to_string()),
                        right_context: right_context.map(|s| s.to_string()),
                    });
                    Ok(Substitution::new())
                } else {
                    eprintln!(
                        "🔍 Unification: Self could not be resolved - binding context: {:?}",
                        left
                    );
                    Err(UnificationError::CategoryMismatch {
                        expected: self.type_category(left),
                        found: self.type_category(right),
                        expected_type: left.clone(),
                        found_type: right.clone(),
                        span: to_source_span(self.get_span_for_error(left, right)),
                    })
                }
            }

            // Concrete type with Self type - resolve Self and unify
            (concrete_type, Type::SelfType { .. }) => {
                eprintln!(
                    "🔍 Unification: Trying to unify concrete type {} with Self",
                    concrete_type
                );
                if let Some(resolved_self) = right.resolve_self() {
                    eprintln!("🔍 Unification: Self resolved to {}", resolved_self);
                    work_queue.push_back(UnifyTask {
                        left: concrete_type.clone(),
                        right: resolved_self,
                        left_context: left_context.map(|s| s.to_string()),
                        right_context: Some("resolved Self type".to_string()),
                    });
                    Ok(Substitution::new())
                } else {
                    eprintln!(
                        "🔍 Unification: Self could not be resolved - binding context: {:?}",
                        right
                    );
                    Err(UnificationError::CategoryMismatch {
                        expected: self.type_category(left),
                        found: self.type_category(right),
                        expected_type: left.clone(),
                        found_type: right.clone(),
                        span: to_source_span(self.get_span_for_error(left, right)),
                    })
                }
            }

            // Cross-category mismatches
            _ => Err(UnificationError::CategoryMismatch {
                expected: self.type_category(left),
                found: self.type_category(right),
                expected_type: left.clone(),
                found_type: right.clone(),
                span: to_source_span(self.get_span_for_error(left, right)),
            }),
        }
    }

    /// Unify a type variable with another type
    fn unify_variable(
        &mut self,
        var_id: TypeVarId,
        var_level: Level,
        other_type: &Type,
    ) -> Result<Substitution, UnificationError> {
        // Occurs check: prevent infinite types like T = List<T>
        if self.occurs_check && other_type.contains_var(var_id) {
            return Err(UnificationError::OccursCheckViolation {
                var_id,
                var_name: format!("T{}", var_id.0),
                containing_type: other_type.clone(),
                span: to_source_span(other_type.span().copied()),
            });
        }

        // Level check for let-polymorphism
        if let Type::Variable {
            level: other_level, ..
        } = other_type
        {
            if *other_level > var_level {
                // Update the level of the other variable to maintain invariant
                // This is a simplification - real implementation needs mutable type tracking
            }
        }

        // Create substitution
        let mut substitution = Substitution::new();
        substitution.insert(var_id, other_type.clone());
        Ok(substitution)
    }

    /// Get type category for error messages
    fn type_category(&self, ty: &Type) -> String {
        match ty {
            Type::Concrete { .. } => "concrete type".to_string(),
            Type::Protocol { .. } => "protocol".to_string(),
            Type::Variable { .. } => "type variable".to_string(),
            Type::SelfType { .. } => "Self type".to_string(),
            Type::Function { .. } => "function type".to_string(),
        }
    }

    /// Extract span for error reporting (prefer left type's span)
    fn get_span_for_error(&self, left: &Type, right: &Type) -> Option<Span> {
        left.span().copied().or_else(|| right.span().copied())
    }

    /// Enter a new level for let-polymorphism
    pub fn enter_level(&mut self) {
        self.current_level.0 += 1;
    }

    /// Exit a level for let-polymorphism
    pub fn exit_level(&mut self) {
        if self.current_level.0 > 0 {
            self.current_level.0 -= 1;
        }
    }

    /// Get current level
    pub fn current_level(&self) -> Level {
        self.current_level
    }

    /// Generalize a type by converting type variables at current level to universal variables
    pub fn generalize(&self, ty: &Type) -> Type {
        // Simplified generalization - real implementation needs proper scheme handling
        ty.clone()
    }

    /// Instantiate a polymorphic type with fresh type variables
    pub fn instantiate(&mut self, ty: &Type) -> Type {
        // Simplified instantiation - real implementation needs scheme handling
        ty.clone()
    }
}

impl Default for Unifier {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeVarGenerator;

    #[test]
    fn test_unify_same_concrete_types() {
        let mut unifier = Unifier::new();

        let int1 = Type::concrete("Integer");
        let int2 = Type::concrete("Integer");

        let result = unifier.unify(&int1, &int2);
        assert!(result.is_ok());

        let substitution = result.unwrap();
        assert_eq!(substitution.len(), 0); // No substitution needed
    }

    #[test]
    fn test_unify_variable_with_concrete() {
        let mut unifier = Unifier::new();
        let mut gen = TypeVarGenerator::new();

        let var = gen.fresh(Level(0));
        let int_type = Type::concrete("Integer");

        let result = unifier.unify(&var, &int_type);
        assert!(result.is_ok());

        let substitution = result.unwrap();
        if let Type::Variable { var_id, .. } = &var {
            assert_eq!(substitution.get(*var_id), Some(&int_type));
        }
    }

    #[test]
    fn test_occurs_check_violation() {
        let mut unifier = Unifier::new();
        let mut gen = TypeVarGenerator::new();

        let var = gen.fresh(Level(0));
        if let Type::Variable { .. } = &var {
            // Create List<T> where T is the variable
            let list_type = Type::generic_concrete("List", vec![var.clone()]);

            let result = unifier.unify(&var, &list_type);
            assert!(result.is_err());

            if let Err(UnificationError::OccursCheckViolation { .. }) = result {
                // Expected error type
            } else {
                panic!("Expected occurs check violation");
            }
        }
    }

    #[test]
    fn test_type_mismatch() {
        let mut unifier = Unifier::new();

        let int_type = Type::concrete("Integer");
        let string_type = Type::concrete("String");

        let result = unifier.unify(&int_type, &string_type);
        assert!(result.is_err());

        if let Err(UnificationError::TypeMismatch { .. }) = result {
            // Expected error type
        } else {
            panic!("Expected type mismatch error");
        }
    }

    #[test]
    fn test_generic_type_unification() {
        let mut unifier = Unifier::new();
        let mut gen = TypeVarGenerator::new();

        let var_t = gen.fresh(Level(0));
        let _var_u = gen.fresh(Level(0));

        let list1 = Type::generic_concrete("List", vec![var_t.clone()]);
        let list2 = Type::generic_concrete("List", vec![Type::concrete("Integer")]);

        let result = unifier.unify(&list1, &list2);
        assert!(result.is_ok());

        let substitution = result.unwrap();
        if let Type::Variable { var_id, .. } = &var_t {
            assert_eq!(substitution.get(*var_id), Some(&Type::concrete("Integer")));
        }
    }
}
