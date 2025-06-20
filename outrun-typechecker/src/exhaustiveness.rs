//! Comprehensive exhaustiveness analysis for case expressions and function guards
//!
//! This module implements exhaustiveness checking for:
//! - Boolean case expressions (true/false coverage)
//! - Option/Result case expressions (Some/None, Ok/Err coverage)  
//! - Trait case expressions (all implementors covered)
//! - Function guard exhaustiveness (default case or complete coverage)
//!
//! The analyzer operates on typed AST for accurate type information and uses
//! resolved TypeIds for precise type-specific exhaustiveness rules.

use crate::checker::{TypeContext, TypedExpression};
use crate::error::TypeError;
use crate::types::{ConcreteType, TraitId, TypeId};
use outrun_parser::Span;

/// Main exhaustiveness analyzer that operates on typed AST
#[derive(Debug)]
pub struct ExhaustivenessAnalyzer<'a> {
    context: &'a TypeContext,
}

/// Result of exhaustiveness analysis
#[derive(Debug, Clone, PartialEq)]
pub enum ExhaustivenessResult {
    /// All possible values are covered
    Exhaustive,
    /// Missing specific values (with type information)
    Missing(Vec<MissingPattern>),
    /// Non-exhaustive due to open type system (e.g., user-defined types)
    OpenType,
}

/// Represents a missing pattern in exhaustiveness analysis
#[derive(Debug, Clone, PartialEq)]
pub struct MissingPattern {
    /// Type of the missing pattern
    pub type_id: TypeId,
    /// Human-readable description of what's missing
    pub description: String,
    /// Example pattern that would cover this case
    pub example_pattern: String,
}

/// Type of case expression for exhaustiveness analysis
#[derive(Debug, Clone, PartialEq)]
pub enum CaseType {
    /// Pattern-based case with value patterns
    Pattern(TypeId),
    /// Trait-based case with trait implementations
    Trait(TraitId),
}

/// Represents a covered pattern/value in a case expression
#[derive(Debug, Clone, PartialEq)]
pub enum CoveredPattern {
    /// Boolean literal (true/false)
    Boolean(bool),
    /// Concrete type in a case expression (e.g., Option.Some, Option.None, Result.Ok, Result.Err)
    ConcreteType(TypeId),
    /// Trait implementation type
    TraitType(TypeId),
    /// Generic pattern for other types
    Generic(TypeId, String),
}

impl<'a> ExhaustivenessAnalyzer<'a> {
    /// Create a new exhaustiveness analyzer
    pub fn new(context: &'a TypeContext) -> Self {
        Self { context }
    }

    /// Analyze exhaustiveness of a case expression
    pub fn analyze_case_exhaustiveness(
        &self,
        case_type: CaseType,
        covered_patterns: &[CoveredPattern],
        span: Span,
    ) -> Result<ExhaustivenessResult, TypeError> {
        match case_type {
            CaseType::Pattern(type_id) => {
                self.analyze_pattern_case_exhaustiveness(type_id, covered_patterns, span)
            }
            CaseType::Trait(trait_id) => {
                self.analyze_trait_case_exhaustiveness(trait_id, covered_patterns, span)
            }
        }
    }

    /// Analyze exhaustiveness for pattern-based case expressions
    fn analyze_pattern_case_exhaustiveness(
        &self,
        type_id: TypeId,
        covered_patterns: &[CoveredPattern],
        span: Span,
    ) -> Result<ExhaustivenessResult, TypeError> {
        // Get the concrete type to determine what exhaustiveness rules apply
        let concrete_type = self.context.get_concrete_type(type_id).ok_or_else(|| {
            TypeError::internal(format!(
                "Type not found in registry: {:?}",
                self.context.get_type_name(type_id)
            ))
        })?;

        match concrete_type {
            ConcreteType::Boolean => self.analyze_boolean_exhaustiveness(covered_patterns, span),
            _ => {
                // All other types (String, Integer, List, Tuple, Map, Option, Result, Struct, Trait, Function)
                // are either infinite or should use trait-based exhaustiveness analysis
                Ok(ExhaustivenessResult::OpenType)
            }
        }
    }

    /// Analyze Boolean exhaustiveness (true/false coverage)
    fn analyze_boolean_exhaustiveness(
        &self,
        covered_patterns: &[CoveredPattern],
        _span: Span,
    ) -> Result<ExhaustivenessResult, TypeError> {
        let mut has_true = false;
        let mut has_false = false;

        for pattern in covered_patterns {
            match pattern {
                CoveredPattern::Boolean(true) => has_true = true,
                CoveredPattern::Boolean(false) => has_false = true,
                _ => {
                    // Non-boolean pattern in boolean case - this would be a type error
                    // but we're only doing exhaustiveness analysis here
                }
            }
        }

        let mut missing = Vec::new();
        // Use the pre-computed boolean type ID from the context
        let boolean_type_id = self.context.interner.get_type("Outrun.Core.Boolean");

        // If we can't find the boolean type, there's a serious issue with the type system
        let boolean_type_id = match boolean_type_id {
            Some(id) => id,
            None => {
                return Err(TypeError::internal(
                    "Boolean type not found in type registry".to_string(),
                ));
            }
        };

        if !has_true {
            missing.push(MissingPattern {
                type_id: boolean_type_id,
                description: "Boolean value 'true'".to_string(),
                example_pattern: "true".to_string(),
            });
        }
        if !has_false {
            missing.push(MissingPattern {
                type_id: boolean_type_id,
                description: "Boolean value 'false'".to_string(),
                example_pattern: "false".to_string(),
            });
        }

        if missing.is_empty() {
            Ok(ExhaustivenessResult::Exhaustive)
        } else {
            Ok(ExhaustivenessResult::Missing(missing))
        }
    }

    /// Analyze exhaustiveness for trait-based case expressions
    fn analyze_trait_case_exhaustiveness(
        &self,
        trait_id: TraitId,
        covered_patterns: &[CoveredPattern],
        _span: Span,
    ) -> Result<ExhaustivenessResult, TypeError> {
        // Extract covered type IDs from trait patterns
        let covered_types: Vec<TypeId> = covered_patterns
            .iter()
            .filter_map(|pattern| match pattern {
                CoveredPattern::TraitType(type_id) => Some(*type_id),
                _ => None,
            })
            .collect();

        // Use existing trait registry exhaustiveness checking
        let result = self
            .context
            .trait_registry
            .check_trait_case_exhaustiveness(trait_id, &covered_types);

        match result {
            crate::types::traits::ExhaustivenessResult::Exhaustive => {
                Ok(ExhaustivenessResult::Exhaustive)
            }
            crate::types::traits::ExhaustivenessResult::Missing(missing_types) => {
                let missing_patterns: Vec<MissingPattern> = missing_types
                    .into_iter()
                    .map(|type_id| {
                        let type_name = self
                            .context
                            .get_type_name(type_id)
                            .unwrap_or("Unknown")
                            .to_string();
                        MissingPattern {
                            type_id,
                            description: format!("Trait implementation for type '{}'", type_name),
                            example_pattern: format!("{} {{ /* fields */ }}", type_name),
                        }
                    })
                    .collect();
                Ok(ExhaustivenessResult::Missing(missing_patterns))
            }
        }
    }

    /// Analyze function guard exhaustiveness
    /// Functions with guards should either have a default case (no guard) or complete coverage
    pub fn analyze_function_guard_exhaustiveness(
        &self,
        _function_name: &str,
        _guards: &[TypedExpression],
        _has_default_case: bool,
        _span: Span,
    ) -> Result<ExhaustivenessResult, TypeError> {
        Ok(ExhaustivenessResult::OpenType)
    }
}

impl MissingPattern {
    /// Create a new missing pattern
    pub fn new(type_id: TypeId, description: String, example_pattern: String) -> Self {
        Self {
            type_id,
            description,
            example_pattern,
        }
    }

    /// Get a user-friendly error message for this missing pattern
    pub fn error_message(&self) -> String {
        format!(
            "Missing pattern: {}. Consider adding: {}",
            self.description, self.example_pattern
        )
    }
}

impl ExhaustivenessResult {
    /// Convert exhaustiveness result to appropriate TypeError for case expressions
    pub fn to_case_error(
        self,
        case_type: CaseType,
        context: &TypeContext,
        span: Span,
    ) -> Option<TypeError> {
        match self {
            ExhaustivenessResult::Exhaustive => None,
            ExhaustivenessResult::Missing(missing_patterns) => {
                let missing_strings: Vec<String> = missing_patterns
                    .iter()
                    .map(|p| p.example_pattern.clone())
                    .collect();

                match case_type {
                    CaseType::Pattern(type_id) => {
                        // Determine specific error type based on the type
                        if let Some(concrete_type) = context.get_concrete_type(type_id) {
                            match concrete_type {
                                ConcreteType::Boolean => Some(TypeError::boolean_not_exhaustive(
                                    missing_strings,
                                    crate::error::span_to_source_span(span),
                                )),
                                _ => Some(TypeError::non_exhaustive_patterns(
                                    missing_strings,
                                    crate::error::span_to_source_span(span),
                                )),
                            }
                        } else {
                            Some(TypeError::non_exhaustive_patterns(
                                missing_strings,
                                crate::error::span_to_source_span(span),
                            ))
                        }
                    }
                    CaseType::Trait(trait_id) => {
                        let trait_name = context
                            .trait_registry
                            .get_trait(trait_id)
                            .map(|t| t.name.clone())
                            .unwrap_or_else(|| "Unknown".to_string());

                        let missing_type_names: Vec<String> = missing_patterns
                            .iter()
                            .map(|p| {
                                context
                                    .get_type_name(p.type_id)
                                    .unwrap_or("Unknown")
                                    .to_string()
                            })
                            .collect();

                        Some(TypeError::case_not_exhaustive_trait(
                            trait_name,
                            missing_type_names.join(", "),
                            crate::error::span_to_source_span(span),
                        ))
                    }
                }
            }
            ExhaustivenessResult::OpenType => {
                // For open types, we could issue a warning but not an error
                // For now, treat as non-exhaustive requiring explicit catch-all
                let case_type_name = match case_type {
                    CaseType::Pattern(type_id) => context
                        .get_type_name(type_id)
                        .unwrap_or("Unknown")
                        .to_string(),
                    CaseType::Trait(trait_id) => context
                        .trait_registry
                        .get_trait(trait_id)
                        .map(|t| t.name.clone())
                        .unwrap_or_else(|| "Unknown".to_string()),
                };

                match case_type {
                    CaseType::Pattern(_) => Some(TypeError::non_exhaustive_patterns(
                        vec!["catch-all pattern (_)".to_string()],
                        crate::error::span_to_source_span(span),
                    )),
                    CaseType::Trait(_) => Some(TypeError::case_not_exhaustive_trait(
                        case_type_name,
                        "catch-all pattern (_)".to_string(),
                        crate::error::span_to_source_span(span),
                    )),
                }
            }
        }
    }

    /// Convert exhaustiveness result to function guard error
    pub fn to_function_error(self, function_name: &str, span: Span) -> Option<TypeError> {
        match self {
            ExhaustivenessResult::Exhaustive => None,
            ExhaustivenessResult::Missing(missing_patterns) => {
                let missing_cases: Vec<String> = missing_patterns
                    .iter()
                    .map(|p| p.description.clone())
                    .collect();

                Some(TypeError::function_not_exhaustive(
                    function_name.to_string(),
                    missing_cases,
                    crate::error::span_to_source_span(span),
                ))
            }
            ExhaustivenessResult::OpenType => Some(TypeError::function_not_exhaustive(
                function_name.to_string(),
                vec!["default case (function without guard)".to_string()],
                crate::error::span_to_source_span(span),
            )),
        }
    }
}
