//! Exhaustiveness checking for patterns and guards using SAT solving
//!
//! This module implements exhaustiveness checking for case expressions and guard clauses
//! by encoding patterns as boolean formulas and using SAT solving to verify coverage.

use crate::error::{to_source_span, ExhaustivenessError, TypecheckError};
use crate::registry::TypeRegistry;
use crate::types::Type;
use outrun_parser::{CaseClause, CaseExpression, Expression, Pattern};
use rustsat::instances::SatInstance;
use rustsat::solvers::{Solve, SolverResult};
use rustsat::types::{Lit, Var};
use std::collections::HashMap;
use std::rc::Rc;

/// Pattern variable for SAT encoding
#[derive(Debug, Clone, PartialEq)]
struct PatternVariable {
    /// Unique identifier for this pattern component
    pattern_id: usize,
    /// Type being matched
    matched_type: Type,
    /// Source pattern for error reporting
    source_pattern: Pattern,
}

/// Encoding of patterns into SAT formulas
struct PatternEncoding {
    /// SAT instance for this pattern matching problem
    sat_instance: SatInstance,
    /// Mapping from pattern IDs to SAT variables
    pattern_vars: HashMap<usize, Var>,
    /// Type registry for looking up type information
    #[allow(dead_code)]
    type_registry: Rc<TypeRegistry>,
    /// Counter for generating unique pattern IDs
    next_pattern_id: usize,
}

impl PatternEncoding {
    /// Create a new pattern encoding
    fn new(type_registry: Rc<TypeRegistry>) -> Self {
        Self {
            sat_instance: SatInstance::new(),
            pattern_vars: HashMap::new(),
            type_registry,
            next_pattern_id: 0,
        }
    }

    /// Generate a unique pattern ID
    fn next_id(&mut self) -> usize {
        let id = self.next_pattern_id;
        self.next_pattern_id += 1;
        id
    }

    /// Encode a pattern into a SAT literal
    fn encode_pattern(&mut self, pattern: &Pattern, scrutinee_type: &Type) -> Lit {
        match pattern {
            Pattern::Identifier(_) => {
                // Wildcard pattern - always matches
                let var = self.sat_instance.new_var();
                let lit = Lit::positive(var.idx32());
                // Add unit clause to make it always true
                self.sat_instance.add_unit(lit);
                lit
            }
            Pattern::Literal(_lit_pattern) => {
                // Create variable for this literal value
                let pattern_var = PatternVariable {
                    pattern_id: self.next_id(),
                    matched_type: scrutinee_type.clone(),
                    source_pattern: pattern.clone(),
                };
                let var = self.sat_instance.new_var();
                self.pattern_vars.insert(pattern_var.pattern_id, var);
                Lit::positive(var.idx32())
            }
            Pattern::Struct(struct_pattern) => {
                // Encode struct pattern as conjunction of field patterns
                self.encode_struct_pattern(struct_pattern, scrutinee_type)
            }
            Pattern::Tuple(tuple_pattern) => {
                // Encode tuple pattern as conjunction of element patterns
                self.encode_tuple_pattern(tuple_pattern, scrutinee_type)
            }
            Pattern::List(list_pattern) => {
                // Encode list pattern - simplified for now
                self.encode_list_pattern(list_pattern, scrutinee_type)
            }
        }
    }

    /// Encode a struct pattern
    fn encode_struct_pattern(
        &mut self,
        struct_pattern: &outrun_parser::StructPattern,
        scrutinee_type: &Type,
    ) -> Lit {
        // Check if the struct type matches
        let type_match_var = self.sat_instance.new_var();
        let type_match_lit = Lit::positive(type_match_var.idx32());

        // For now, we'll treat struct patterns as matching if the type name matches
        // In a full implementation, we'd also encode field patterns
        let pattern_var = PatternVariable {
            pattern_id: self.next_id(),
            matched_type: scrutinee_type.clone(),
            source_pattern: Pattern::Struct(struct_pattern.clone()),
        };
        self.pattern_vars
            .insert(pattern_var.pattern_id, type_match_var);

        type_match_lit
    }

    /// Encode a tuple pattern
    fn encode_tuple_pattern(
        &mut self,
        tuple_pattern: &outrun_parser::TuplePattern,
        scrutinee_type: &Type,
    ) -> Lit {
        // For tuples, all elements must match
        let mut element_lits = Vec::new();

        // Get element types from scrutinee type if it's a tuple
        let element_types = match scrutinee_type {
            Type::Tuple { element_types, .. } => element_types.clone(),
            _ => {
                // Type mismatch - pattern won't match
                let var = self.sat_instance.new_var();
                let lit = !Lit::positive(var.idx32());
                self.sat_instance.add_unit(lit);
                return lit;
            }
        };

        // Encode each element pattern
        for (i, element_pattern) in tuple_pattern.elements.iter().enumerate() {
            let element_type = element_types.get(i).cloned().unwrap_or_else(|| {
                Type::variable(crate::types::TypeVarId(0), crate::types::Level(0))
            });
            let element_lit = self.encode_pattern(element_pattern, &element_type);
            element_lits.push(element_lit);
        }

        // Create conjunction of all element patterns
        self.create_conjunction(element_lits)
    }

    /// Encode a list pattern
    fn encode_list_pattern(
        &mut self,
        list_pattern: &outrun_parser::ListPattern,
        scrutinee_type: &Type,
    ) -> Lit {
        // Simplified list pattern encoding
        // In a full implementation, we'd handle rest patterns and length constraints
        let pattern_var = PatternVariable {
            pattern_id: self.next_id(),
            matched_type: scrutinee_type.clone(),
            source_pattern: Pattern::List(list_pattern.clone()),
        };
        let var = self.sat_instance.new_var();
        self.pattern_vars.insert(pattern_var.pattern_id, var);
        Lit::positive(var.idx32())
    }

    /// Create a conjunction (AND) of literals
    fn create_conjunction(&mut self, literals: Vec<Lit>) -> Lit {
        if literals.is_empty() {
            // Empty conjunction is true
            let var = self.sat_instance.new_var();
            let lit = Lit::positive(var.idx32());
            self.sat_instance.add_unit(lit);
            return lit;
        }

        if literals.len() == 1 {
            return literals[0];
        }

        // Create a new variable for the conjunction
        let conj_var = self.sat_instance.new_var();
        let conj_lit = Lit::positive(conj_var.idx32());

        // Add clauses: conj_lit => all literals must be true
        // This is encoded as: !conj_lit OR lit_i for each literal
        for &lit in &literals {
            self.sat_instance.add_binary(!conj_lit, lit);
        }

        // Add clause: all literals true => conj_lit
        // This is encoded as: !lit_1 OR !lit_2 OR ... OR conj_lit
        let mut clause = vec![conj_lit];
        for &lit in &literals {
            clause.push(!lit);
        }
        let clause = rustsat::types::Clause::from_iter(clause);
        self.sat_instance.add_clause(clause);

        conj_lit
    }

    /// Create a disjunction (OR) of literals
    fn create_disjunction(&mut self, literals: Vec<Lit>) -> Lit {
        if literals.is_empty() {
            // Empty disjunction is false
            let var = self.sat_instance.new_var();
            let lit = !Lit::positive(var.idx32());
            self.sat_instance.add_unit(lit);
            return lit;
        }

        if literals.len() == 1 {
            return literals[0];
        }

        // Create a new variable for the disjunction
        let disj_var = self.sat_instance.new_var();
        let disj_lit = Lit::positive(disj_var.idx32());

        // Add clause: disj_lit => at least one literal must be true
        // This is encoded as: !disj_lit OR lit_1 OR lit_2 OR ...
        let mut clause = vec![!disj_lit];
        for &lit in &literals {
            clause.push(lit);
        }
        let clause = rustsat::types::Clause::from_iter(clause);
        self.sat_instance.add_clause(clause);

        // Add clauses: each literal true => disj_lit
        // This is encoded as: !lit_i OR disj_lit for each literal
        for &lit in &literals {
            self.sat_instance.add_binary(!lit, disj_lit);
        }

        disj_lit
    }

    /// Encode a guard expression (simplified - only handles basic cases)
    fn encode_guard(
        &mut self,
        guard: &Expression,
        _pattern_bindings: &HashMap<String, Type>,
    ) -> Lit {
        // For now, we'll treat guards conservatively
        // A full implementation would analyze the guard expression
        match &guard.kind {
            outrun_parser::ExpressionKind::Boolean(b) => {
                let var = self.sat_instance.new_var();
                let lit = if b.value {
                    Lit::positive(var.idx32())
                } else {
                    !Lit::positive(var.idx32())
                };
                self.sat_instance.add_unit(lit);
                lit
            }
            _ => {
                // For complex guards, we conservatively assume they might be true or false
                let var = self.sat_instance.new_var();
                Lit::positive(var.idx32())
            }
        }
    }
}

/// Exhaustiveness checker using SAT solving
pub struct ExhaustivenessChecker {
    /// Type registry for looking up type information
    type_registry: Option<Rc<TypeRegistry>>,
}

impl ExhaustivenessChecker {
    /// Create a new exhaustiveness checker
    pub fn new() -> Self {
        Self {
            type_registry: None,
        }
    }

    /// Set the type registry
    pub fn set_type_registry(&mut self, registry: Rc<TypeRegistry>) {
        self.type_registry = Some(registry);
    }

    /// Check exhaustiveness of a case expression
    ///
    /// This method requires a proper type compatibility function from the inference engine.
    /// Use check_case_exhaustiveness_with_type_checker() instead.
    pub fn check_case_exhaustiveness(
        &mut self,
        _case_expr: &CaseExpression,
        _scrutinee_type: &Type,
    ) -> Result<(), TypecheckError> {
        panic!("check_case_exhaustiveness() requires a proper type checker. Use check_case_exhaustiveness_with_type_checker() instead.")
    }

    /// Check exhaustiveness of a case expression using the type registry
    pub fn check_case_exhaustiveness_with_registry(
        &mut self,
        case_expr: &CaseExpression,
        scrutinee_type: &Type,
    ) -> Result<(), TypecheckError> {
        // Special case for Boolean types - simple heuristic check
        if self.is_boolean_type_with_registry(scrutinee_type) {
            return self.check_boolean_exhaustiveness(case_expr);
        }

        // Special case for Integer types - infinite domain check
        if self.is_integer_type_with_registry(scrutinee_type) {
            return self.check_integer_exhaustiveness(case_expr);
        }

        // Special case for String types - infinite domain check
        if self.is_string_type_with_registry(scrutinee_type) {
            return self.check_string_exhaustiveness(case_expr);
        }

        // Special case for Atom types - infinite domain check
        if self.is_atom_type_with_registry(scrutinee_type) {
            return self.check_atom_exhaustiveness(case_expr);
        }

        // Special case for Float types - infinite domain check
        if self.is_float_type_with_registry(scrutinee_type) {
            return self.check_float_exhaustiveness(case_expr);
        }

        // Special case for Tuple types - finite domain check
        if self.is_tuple_type(scrutinee_type) {
            return self.check_tuple_exhaustiveness(case_expr, scrutinee_type);
        }

        // Special case for Struct types - finite domain check
        if self.is_struct_type(scrutinee_type) {
            return self.check_struct_exhaustiveness(case_expr, scrutinee_type);
        }

        // For other types, fall back to SAT-based checking (not implemented yet)
        Ok(())
    }

    /// Check if a type is compatible with Boolean using the type registry
    fn is_boolean_type_with_registry(&self, type_: &Type) -> bool {
        let type_registry = match &self.type_registry {
            Some(registry) => registry,
            None => return false,
        };

        match type_ {
            Type::Concrete { name, .. } => {
                // Check if this concrete type implements Boolean protocol
                type_registry.has_implementation(&crate::types::ModuleName::new("Boolean"), name)
            }
            Type::Protocol { name, .. } => {
                // Check if this is the Boolean protocol itself
                name.as_str() == "Boolean"
            }
            _ => false,
        }
    }

    /// Check if a type is compatible with Integer using the type registry
    fn is_integer_type_with_registry(&self, type_: &Type) -> bool {
        let type_registry = match &self.type_registry {
            Some(registry) => registry,
            None => return false,
        };

        match type_ {
            Type::Concrete { name, .. } => {
                // Check if this concrete type implements Integer protocol
                type_registry.has_implementation(&crate::types::ModuleName::new("Integer"), name)
            }
            Type::Protocol { name, .. } => {
                // Check if this is the Integer protocol itself
                name.as_str() == "Integer"
            }
            _ => false,
        }
    }

    /// Check if a type is compatible with String using the type registry
    fn is_string_type_with_registry(&self, type_: &Type) -> bool {
        let type_registry = match &self.type_registry {
            Some(registry) => registry,
            None => return false,
        };

        match type_ {
            Type::Concrete { name, .. } => {
                // Check if this concrete type implements String protocol
                type_registry.has_implementation(&crate::types::ModuleName::new("String"), name)
            }
            Type::Protocol { name, .. } => {
                // Check if this is the String protocol itself
                name.as_str() == "String"
            }
            _ => false,
        }
    }

    /// Check if a type is compatible with Atom using the type registry
    fn is_atom_type_with_registry(&self, type_: &Type) -> bool {
        let type_registry = match &self.type_registry {
            Some(registry) => registry,
            None => return false,
        };

        match type_ {
            Type::Concrete { name, .. } => {
                // Check if this concrete type implements Atom protocol
                type_registry.has_implementation(&crate::types::ModuleName::new("Atom"), name)
            }
            Type::Protocol { name, .. } => {
                // Check if this is the Atom protocol itself
                name.as_str() == "Atom"
            }
            _ => false,
        }
    }

    /// Check if a type is compatible with Float using the type registry
    fn is_float_type_with_registry(&self, type_: &Type) -> bool {
        let type_registry = match &self.type_registry {
            Some(registry) => registry,
            None => return false,
        };

        match type_ {
            Type::Concrete { name, .. } => {
                // Check if this concrete type implements Float protocol
                type_registry.has_implementation(&crate::types::ModuleName::new("Float"), name)
            }
            Type::Protocol { name, .. } => {
                // Check if this is the Float protocol itself
                name.as_str() == "Float"
            }
            _ => false,
        }
    }

    /// Check if a type is a tuple type
    fn is_tuple_type(&self, type_: &Type) -> bool {
        matches!(type_, Type::Tuple { .. })
    }

    /// Check if a type is a struct type
    fn is_struct_type(&self, type_: &Type) -> bool {
        matches!(type_, Type::Concrete { .. })
    }

    /// Check exhaustiveness of a case expression with a provided type compatibility function
    pub fn check_case_exhaustiveness_with_type_checker<F>(
        &mut self,
        case_expr: &CaseExpression,
        scrutinee_type: &Type,
        type_checker: F,
    ) -> Result<(), TypecheckError>
    where
        F: Fn(&Type, &Type) -> bool,
    {
        // Special case for Boolean types - simple heuristic check
        if self.is_boolean_type(scrutinee_type, &type_checker) {
            return self.check_boolean_exhaustiveness(case_expr);
        }

        // Special case for Integer types - infinite domain check
        if self.is_integer_type(scrutinee_type, &type_checker) {
            return self.check_integer_exhaustiveness(case_expr);
        }

        // Special case for String types - infinite domain check
        if self.is_string_type(scrutinee_type, &type_checker) {
            return self.check_string_exhaustiveness(case_expr);
        }
        let type_registry = self
            .type_registry
            .as_ref()
            .ok_or_else(|| TypecheckError::Generic {
                message: "Type registry not set in exhaustiveness checker".to_string(),
                span: None,
            })?
            .clone();

        let mut encoding = PatternEncoding::new(type_registry);

        // Encode each case clause
        let mut clause_literals = Vec::new();
        for clause in &case_expr.clauses {
            let pattern_lit = encoding.encode_pattern(&clause.pattern, scrutinee_type);

            let clause_lit = if let Some(guard) = &clause.guard {
                // Extract pattern bindings (simplified for now)
                let bindings = self.extract_pattern_bindings(&clause.pattern, scrutinee_type)?;
                let guard_lit = encoding.encode_guard(guard, &bindings);
                // Pattern matches AND guard is true
                encoding.create_conjunction(vec![pattern_lit, guard_lit])
            } else {
                pattern_lit
            };

            clause_literals.push(clause_lit);
        }

        // Create "at least one pattern matches" formula
        let some_pattern_matches = encoding.create_disjunction(clause_literals);

        // Check if "no pattern matches" is satisfiable
        // If it is, then the patterns are not exhaustive
        let no_pattern_matches = !some_pattern_matches;

        // Create a SAT solver and check satisfiability
        let mut solver = rustsat_minisat::core::Minisat::default();

        // Add the CNF formula to the solver
        let cnf = encoding.sat_instance.into_cnf().0;
        solver.add_cnf(cnf).map_err(|e| TypecheckError::Generic {
            message: format!("Failed to add CNF to solver: {:?}", e),
            span: None,
        })?;

        // Add the "no pattern matches" constraint
        solver
            .add_unit(no_pattern_matches)
            .map_err(|e| TypecheckError::Generic {
                message: format!("Failed to add unit clause: {:?}", e),
                span: None,
            })?;

        // Solve the SAT problem
        match solver.solve().map_err(|e| TypecheckError::Generic {
            message: format!("SAT solver failed: {:?}", e),
            span: None,
        })? {
            SolverResult::Sat => {
                // Not exhaustive - we found a case where no pattern matches
                Err(TypecheckError::ExhaustivenessError(
                    ExhaustivenessError::MissingPattern {
                        missing_pattern: "_ (wildcard)".to_string(),
                        span: to_source_span(Some(case_expr.span)),
                    },
                ))
            }
            SolverResult::Unsat => {
                // Exhaustive - all cases are covered
                Ok(())
            }
            SolverResult::Interrupted => {
                // Solver was interrupted - be conservative
                Ok(())
            }
        }
    }

    /// Check exhaustiveness of guard clauses in a function
    pub fn check_guard_exhaustiveness(
        &mut self,
        guards: &[outrun_parser::GuardClause],
        _param_types: &[(String, Type)],
    ) -> Result<(), TypecheckError> {
        // Simplified guard exhaustiveness checking
        // A full implementation would analyze guard conditions
        if guards.is_empty() {
            // No guards means the function might not handle all inputs
            return Err(TypecheckError::ExhaustivenessError(
                ExhaustivenessError::IncompleteGuards {
                    span: to_source_span(Some(outrun_parser::Span {
                        start: 0,
                        end: 0,
                        start_line_col: None,
                        end_line_col: None,
                    })),
                },
            ));
        }

        // For now, we'll accept any non-empty set of guards
        // A full implementation would use SAT solving on guard conditions
        Ok(())
    }

    /// Check if a type is compatible with Boolean using proper type compatibility
    fn is_boolean_type<F>(&self, type_: &Type, type_checker: F) -> bool
    where
        F: Fn(&Type, &Type) -> bool,
    {
        // Create a Boolean protocol type for comparison
        let boolean_protocol = Type::Protocol {
            name: crate::types::ModuleName::new("Boolean"),
            args: vec![],
            span: None,
        };

        // Use the provided type compatibility function
        type_checker(type_, &boolean_protocol)
    }

    /// Check if a type is compatible with Integer using proper type compatibility
    fn is_integer_type<F>(&self, type_: &Type, type_checker: F) -> bool
    where
        F: Fn(&Type, &Type) -> bool,
    {
        // Create an Integer protocol type for comparison
        let integer_protocol = Type::Protocol {
            name: crate::types::ModuleName::new("Integer"),
            args: vec![],
            span: None,
        };

        // Use the provided type compatibility function
        type_checker(type_, &integer_protocol)
    }

    /// Check if a type is compatible with String using proper type compatibility
    fn is_string_type<F>(&self, type_: &Type, type_checker: F) -> bool
    where
        F: Fn(&Type, &Type) -> bool,
    {
        // Create a String protocol type for comparison
        let string_protocol = Type::Protocol {
            name: crate::types::ModuleName::new("String"),
            args: vec![],
            span: None,
        };

        // Use the provided type compatibility function
        type_checker(type_, &string_protocol)
    }

    /// Simple exhaustiveness check for Boolean types
    fn check_boolean_exhaustiveness(
        &self,
        case_expr: &CaseExpression,
    ) -> Result<(), TypecheckError> {
        let mut has_true = false;
        let mut has_false = false;
        let mut has_wildcard = false;

        for clause in &case_expr.clauses {
            match &clause.pattern {
                Pattern::Literal(lit_pattern) => {
                    if let outrun_parser::Literal::Boolean(b) = &lit_pattern.literal {
                        if b.value {
                            has_true = true;
                        } else {
                            has_false = true;
                        }
                    }
                }
                Pattern::Identifier(_) => {
                    has_wildcard = true;
                }
                _ => {}
            }
        }

        // Boolean is exhaustive if we have both true and false, or a wildcard
        if (has_true && has_false) || has_wildcard {
            Ok(())
        } else {
            Err(TypecheckError::ExhaustivenessError(
                ExhaustivenessError::MissingPattern {
                    missing_pattern: if has_true { "false" } else { "true" }.to_string(),
                    span: to_source_span(Some(case_expr.span)),
                },
            ))
        }
    }

    /// Simple exhaustiveness check for Integer types (infinite domain)
    fn check_integer_exhaustiveness(
        &self,
        case_expr: &CaseExpression,
    ) -> Result<(), TypecheckError> {
        let mut literal_values = std::collections::HashSet::new();
        let mut has_wildcard = false;

        for clause in &case_expr.clauses {
            match &clause.pattern {
                Pattern::Literal(lit_pattern) => {
                    if let outrun_parser::Literal::Integer(i) = &lit_pattern.literal {
                        literal_values.insert(i.value);
                    }
                }
                Pattern::Identifier(_) => {
                    has_wildcard = true;
                }
                _ => {
                    // Other patterns (like structs) could potentially match integers
                    // For now, we'll be conservative and assume they provide coverage
                    has_wildcard = true;
                }
            }
        }

        // Integer domain is infinite, so we need a wildcard unless we're being very specific
        // For now, we'll require a wildcard for any integer matching
        if !has_wildcard {
            return Err(TypecheckError::ExhaustivenessError(
                ExhaustivenessError::MissingPattern {
                    missing_pattern: "_ (wildcard required for infinite Integer domain)"
                        .to_string(),
                    span: to_source_span(Some(case_expr.span)),
                },
            ));
        }

        Ok(())
    }

    /// Simple exhaustiveness check for String types
    fn check_string_exhaustiveness(
        &self,
        case_expr: &CaseExpression,
    ) -> Result<(), TypecheckError> {
        let mut _has_string_literals = false;
        let mut has_wildcard = false;

        for clause in &case_expr.clauses {
            match &clause.pattern {
                Pattern::Literal(lit_pattern) => {
                    if let outrun_parser::Literal::String(string_lit) = &lit_pattern.literal {
                        // Check for string interpolation in patterns - this should be an error
                        for part in &string_lit.parts {
                            if let outrun_parser::StringPart::Interpolation { .. } = part {
                                return Err(TypecheckError::Generic {
                                    message: "String interpolation is not allowed in case patterns. When would the interpolation be evaluated? Use a guard clause instead.".to_string(),
                                    span: to_source_span(Some(string_lit.span)),
                                });
                            }
                        }
                        _has_string_literals = true;
                    }
                }
                Pattern::Identifier(_) => {
                    has_wildcard = true;
                }
                _ => {
                    // Other patterns (like structs) could potentially match strings
                    // For now, we'll be conservative and assume they provide coverage
                    has_wildcard = true;
                }
            }
        }

        // String domain is infinite, so we need a wildcard unless we're being very specific
        // For now, we'll require a wildcard for any string matching
        if !has_wildcard {
            return Err(TypecheckError::ExhaustivenessError(
                ExhaustivenessError::MissingPattern {
                    missing_pattern: "_ (wildcard required for infinite String domain)".to_string(),
                    span: to_source_span(Some(case_expr.span)),
                },
            ));
        }

        Ok(())
    }

    /// Simple exhaustiveness check for Atom types
    fn check_atom_exhaustiveness(&self, case_expr: &CaseExpression) -> Result<(), TypecheckError> {
        let mut _has_atom_literals = false;
        let mut has_wildcard = false;

        for clause in &case_expr.clauses {
            match &clause.pattern {
                Pattern::Literal(lit_pattern) => {
                    if let outrun_parser::Literal::Atom(_) = &lit_pattern.literal {
                        _has_atom_literals = true;
                    }
                }
                Pattern::Identifier(_) => {
                    has_wildcard = true;
                }
                _ => {
                    // Other patterns (like structs) could potentially match atoms
                    // For now, we'll be conservative and assume they provide coverage
                    has_wildcard = true;
                }
            }
        }

        // Atom domain is infinite, so we need a wildcard unless we're being very specific
        // For now, we'll require a wildcard for any atom matching
        if !has_wildcard {
            return Err(TypecheckError::ExhaustivenessError(
                ExhaustivenessError::MissingPattern {
                    missing_pattern: "_ (wildcard required for infinite Atom domain)".to_string(),
                    span: to_source_span(Some(case_expr.span)),
                },
            ));
        }

        Ok(())
    }

    /// Simple exhaustiveness check for Float types
    fn check_float_exhaustiveness(&self, case_expr: &CaseExpression) -> Result<(), TypecheckError> {
        let mut _has_float_literals = false;
        let mut has_wildcard = false;

        for clause in &case_expr.clauses {
            match &clause.pattern {
                Pattern::Literal(lit_pattern) => {
                    if let outrun_parser::Literal::Float(_) = &lit_pattern.literal {
                        _has_float_literals = true;
                    }
                }
                Pattern::Identifier(_) => {
                    has_wildcard = true;
                }
                _ => {
                    // Other patterns (like structs) could potentially match floats
                    // For now, we'll be conservative and assume they provide coverage
                    has_wildcard = true;
                }
            }
        }

        // Float domain is infinite, so we need a wildcard unless we're being very specific
        // For now, we'll require a wildcard for any float matching
        if !has_wildcard {
            return Err(TypecheckError::ExhaustivenessError(
                ExhaustivenessError::MissingPattern {
                    missing_pattern: "_ (wildcard required for infinite Float domain)".to_string(),
                    span: to_source_span(Some(case_expr.span)),
                },
            ));
        }

        Ok(())
    }

    /// Check exhaustiveness for tuple types (finite domain)
    fn check_tuple_exhaustiveness(
        &self,
        case_expr: &CaseExpression,
        scrutinee_type: &Type,
    ) -> Result<(), TypecheckError> {
        // Get tuple element types
        let element_types = match scrutinee_type {
            Type::Tuple { element_types, .. } => element_types,
            _ => return Ok(()), // Not a tuple, shouldn't happen
        };

        // For now, implement a simple heuristic:
        // - If there's a wildcard pattern, consider it exhaustive
        // - If all patterns are tuple patterns, we'd need complex analysis
        // - For simplicity, we'll be conservative and require a wildcard for now

        let mut has_wildcard = false;
        let mut has_tuple_patterns = false;

        for clause in &case_expr.clauses {
            match &clause.pattern {
                Pattern::Tuple(_) => {
                    has_tuple_patterns = true;
                }
                Pattern::Identifier(_) => {
                    has_wildcard = true;
                }
                _ => {
                    // Other patterns could potentially match tuples
                    has_wildcard = true;
                }
            }
        }

        // Simple heuristic: require wildcard for tuple exhaustiveness
        // A full implementation would analyze all possible element combinations
        if has_tuple_patterns && !has_wildcard {
            return Err(TypecheckError::ExhaustivenessError(
                ExhaustivenessError::MissingPattern {
                    missing_pattern: format!(
                        "_ (wildcard recommended for tuple with {} elements - full analysis not implemented)",
                        element_types.len()
                    ),
                    span: to_source_span(Some(case_expr.span)),
                },
            ));
        }

        Ok(())
    }

    /// Check exhaustiveness for struct types (finite domain)
    fn check_struct_exhaustiveness(
        &self,
        case_expr: &CaseExpression,
        _scrutinee_type: &Type,
    ) -> Result<(), TypecheckError> {
        // For now, implement a simple heuristic:
        // - If there's a wildcard pattern, consider it exhaustive
        // - If all patterns are struct patterns, we'd need complex field analysis
        // - For simplicity, we'll be conservative and require a wildcard for now

        let mut has_wildcard = false;
        let mut has_struct_patterns = false;

        for clause in &case_expr.clauses {
            match &clause.pattern {
                Pattern::Struct(_) => {
                    has_struct_patterns = true;
                }
                Pattern::Identifier(_) => {
                    has_wildcard = true;
                }
                _ => {
                    // Other patterns could potentially match structs
                    has_wildcard = true;
                }
            }
        }

        // Simple heuristic: require wildcard for struct exhaustiveness
        // A full implementation would analyze all possible field combinations
        if has_struct_patterns && !has_wildcard {
            return Err(TypecheckError::ExhaustivenessError(
                ExhaustivenessError::MissingPattern {
                    missing_pattern: "_ (wildcard recommended for struct patterns - full field analysis not implemented)".to_string(),
                    span: to_source_span(Some(case_expr.span)),
                },
            ));
        }

        Ok(())
    }

    /// Extract pattern bindings and their types
    #[allow(clippy::only_used_in_recursion)]
    fn extract_pattern_bindings(
        &self,
        pattern: &Pattern,
        scrutinee_type: &Type,
    ) -> Result<HashMap<String, Type>, TypecheckError> {
        let mut bindings = HashMap::new();

        match pattern {
            Pattern::Identifier(id) => {
                bindings.insert(id.name.clone(), scrutinee_type.clone());
            }
            Pattern::Tuple(tuple_pattern) => {
                if let Type::Tuple { element_types, .. } = scrutinee_type {
                    for (i, elem_pattern) in tuple_pattern.elements.iter().enumerate() {
                        if let Some(elem_type) = element_types.get(i) {
                            let elem_bindings =
                                self.extract_pattern_bindings(elem_pattern, elem_type)?;
                            bindings.extend(elem_bindings);
                        }
                    }
                }
            }
            Pattern::Struct(struct_pattern) => {
                // Extract bindings from struct fields
                for field in &struct_pattern.fields {
                    if let Some(field_pattern) = &field.pattern {
                        // For now, use a generic type for field patterns
                        let field_type =
                            Type::variable(crate::types::TypeVarId(0), crate::types::Level(0));
                        let field_bindings =
                            self.extract_pattern_bindings(field_pattern, &field_type)?;
                        bindings.extend(field_bindings);
                    } else {
                        // Shorthand syntax - field name is the binding
                        let field_type =
                            Type::variable(crate::types::TypeVarId(0), crate::types::Level(0));
                        bindings.insert(field.name.name.clone(), field_type);
                    }
                }
            }
            Pattern::List(list_pattern) => {
                // Extract bindings from list elements
                for elem_pattern in &list_pattern.elements {
                    let elem_type =
                        Type::variable(crate::types::TypeVarId(0), crate::types::Level(0));
                    let elem_bindings = self.extract_pattern_bindings(elem_pattern, &elem_type)?;
                    bindings.extend(elem_bindings);
                }
                // Handle rest pattern
                if let Some(rest_id) = &list_pattern.rest {
                    bindings.insert(rest_id.name.clone(), scrutinee_type.clone());
                }
            }
            Pattern::Literal(_) => {
                // Literal patterns don't create bindings
            }
        }

        Ok(bindings)
    }

    /// Check for unreachable patterns (redundancy checking)
    pub fn check_pattern_redundancy(
        &mut self,
        clauses: &[CaseClause],
        scrutinee_type: &Type,
    ) -> Result<Vec<usize>, TypecheckError> {
        let mut unreachable_indices = Vec::new();

        let type_registry = self
            .type_registry
            .as_ref()
            .ok_or_else(|| TypecheckError::Generic {
                message: "Type registry not set in exhaustiveness checker".to_string(),
                span: None,
            })?
            .clone();

        // Check each clause to see if it's reachable
        for (i, clause) in clauses.iter().enumerate() {
            let mut encoding = PatternEncoding::new(type_registry.clone());

            // Encode all previous clauses
            let mut previous_clause_lits = Vec::new();
            for prev_clause in &clauses[..i] {
                let pattern_lit = encoding.encode_pattern(&prev_clause.pattern, scrutinee_type);
                let clause_lit = if let Some(guard) = &prev_clause.guard {
                    let bindings =
                        self.extract_pattern_bindings(&prev_clause.pattern, scrutinee_type)?;
                    let guard_lit = encoding.encode_guard(guard, &bindings);
                    encoding.create_conjunction(vec![pattern_lit, guard_lit])
                } else {
                    pattern_lit
                };
                previous_clause_lits.push(clause_lit);
            }

            // Encode current clause
            let current_pattern_lit = encoding.encode_pattern(&clause.pattern, scrutinee_type);
            let current_clause_lit = if let Some(guard) = &clause.guard {
                let bindings = self.extract_pattern_bindings(&clause.pattern, scrutinee_type)?;
                let guard_lit = encoding.encode_guard(guard, &bindings);
                encoding.create_conjunction(vec![current_pattern_lit, guard_lit])
            } else {
                current_pattern_lit
            };

            // Check if current clause is reachable
            // It's unreachable if: (previous clauses cover everything) AND (current clause matches)
            if !previous_clause_lits.is_empty() {
                let previous_cover_all = encoding.create_disjunction(previous_clause_lits);

                let mut solver = rustsat_minisat::core::Minisat::default();
                let cnf = encoding.sat_instance.into_cnf().0;
                solver.add_cnf(cnf).map_err(|e| TypecheckError::Generic {
                    message: format!("Failed to add CNF to solver: {:?}", e),
                    span: None,
                })?;

                // Check if current pattern can match when previous patterns don't
                solver
                    .add_unit(!previous_cover_all)
                    .map_err(|e| TypecheckError::Generic {
                        message: format!("Failed to add unit clause: {:?}", e),
                        span: None,
                    })?;
                solver
                    .add_unit(current_clause_lit)
                    .map_err(|e| TypecheckError::Generic {
                        message: format!("Failed to add unit clause: {:?}", e),
                        span: None,
                    })?;

                match solver.solve().map_err(|e| TypecheckError::Generic {
                    message: format!("SAT solver failed: {:?}", e),
                    span: None,
                })? {
                    SolverResult::Unsat => {
                        // Current clause is unreachable
                        unreachable_indices.push(i);
                    }
                    _ => {
                        // Current clause is reachable
                    }
                }
            }
        }

        Ok(unreachable_indices)
    }
}

impl Default for ExhaustivenessChecker {
    fn default() -> Self {
        Self::new()
    }
}
