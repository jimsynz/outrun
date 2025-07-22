//! Outrun Typechecker v3
//!
//! Hindley-Milner type inference with protocol constraint solving and exhaustiveness checking.

// Allow clippy lints for development
#![allow(clippy::needless_range_loop)]
#![allow(clippy::single_match)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::useless_vec)]
//!
//! ## Architecture
//!
//! This typechecker follows Outrun's minimalist development philosophy by extending existing
//! parser infrastructure rather than creating parallel systems. Key components:
//!
//! - **Type Inference Engine**: HM algorithm with unification and constraint accumulation
//! - **Protocol System**: Implementation registry with orphan rule checking
//! - **Constraint Solver**: Logical constraint satisfaction for protocol bounds
//! - **Exhaustiveness Checker**: Pattern coverage analysis for case expressions and multi-head functions
//!
//! ## Integration
//!
//! The typechecker extends the existing parser AST by adding optional `TypeInfo` fields
//! to expressions, eliminating duplication while enabling seamless integration.

pub mod constraints;
pub mod desugaring;
pub mod dispatch;
pub mod error;
pub mod exhaustiveness;
pub mod inference;
pub mod registry;
pub mod types;
pub mod unification;

// Re-export public API
pub use constraints::ConstraintSolver;
pub use desugaring::DesugaringEngine;
pub use dispatch::{
    build_dispatch_table,
    // Legacy aliases
    DispatchResolver,
    DispatchResult,
    DispatchTable,
    DispatchTarget,
    FunctionContext,
    FunctionDispatcher,
    FunctionInfo,
    FunctionRegistry,
    FunctionVisibility,
    ResolvedFunction,
};
pub use error::{
    CompilerError, ConstraintError, DispatchError, ErrorContext, InferenceError, 
    TypecheckError, UnificationError,
};
pub use inference::{TypeInferenceEngine, InferenceContext, InferenceResult};
pub use registry::{ImplementationInfo, ImplementationKey, ProtocolRegistry};
pub use types::{
    Constraint, ModuleId, ProtocolId, Substitution, Type, TypeId, TypeInfo, TypeVarId,
};
pub use unification::Unifier;

/// A collection of parsed programs representing a complete Outrun package
pub struct Package {
    pub programs: Vec<outrun_parser::Program>,
    pub package_name: String,
}

impl Package {
    pub fn new(package_name: String) -> Self {
        Self {
            programs: Vec::new(),
            package_name,
        }
    }

    pub fn add_program(&mut self, program: outrun_parser::Program) {
        self.programs.push(program);
    }
}

/// Main entry point for type checking a complete Outrun package
#[allow(clippy::result_large_err)]
pub fn typecheck_package(package: &mut Package) -> Result<(), CompilerError> {
    let mut engine = TypeInferenceEngine::new();
    let mut desugaring_engine = DesugaringEngine::new();

    // Phase 1: Desugar all operators into protocol function calls
    for program in &mut package.programs {
        desugaring_engine.desugar_program(program)?;
    }

    // Phase 2: Collect all type and protocol definitions across all files
    for program in &package.programs {
        engine.collect_definitions(program)?;
    }

    // Phase 3: Build protocol implementation registry
    for program in &package.programs {
        engine.register_implementations(program)?;
    }

    // Phase 4: Type check all expressions with complete context
    for program in &mut package.programs {
        engine.typecheck_program(program)?;
    }

    Ok(())
}

/// Convenience function for single-file type checking (mainly for testing)
#[allow(clippy::result_large_err)]
pub fn typecheck_program(program: &mut outrun_parser::Program) -> Result<(), CompilerError> {
    let mut package = Package::new("single_file".to_string());
    package.add_program(program.clone());

    typecheck_package(&mut package)?;

    // Replace the original program with the type-checked version
    *program = package.programs.into_iter().next().unwrap();

    Ok(())
}

#[cfg(test)]
mod desugaring_tests {
    pub mod test_operator_desugaring_integration;
}

#[cfg(test)]
mod integration_tests {
    use crate::types::{Level, Type, TypeInfo, TypeVarGenerator};
    use outrun_parser::{parse_program, ExpressionKind, ParsedTypeInfo};

    #[test]
    fn test_typechecker_typeinfo_conversion() {
        // Create a typechecker TypeInfo
        let resolved_type = Type::concrete("Integer");
        let type_info = TypeInfo::new(resolved_type);

        // Convert to parser's ParsedTypeInfo
        let parsed_type_info = type_info.to_parsed_type_info();

        // Verify the conversion
        assert_eq!(parsed_type_info.resolved_type, "Integer");
        assert!(parsed_type_info.type_span.is_none());
    }

    #[test]
    fn test_typechecker_typeinfo_conversion_with_span() {
        // Create a typechecker TypeInfo
        let resolved_type = Type::concrete("String");
        let type_info = TypeInfo::new(resolved_type);

        // Create a span
        let span = outrun_parser::Span::new(10, 20);

        // Convert to parser's ParsedTypeInfo with span
        let parsed_type_info = type_info.to_parsed_type_info_with_span(span);

        // Verify the conversion
        assert_eq!(parsed_type_info.resolved_type, "String");
        assert_eq!(parsed_type_info.type_span, Some(span));
    }

    #[test]
    fn test_parser_expression_with_type_info() {
        // Parse a simple expression
        let source = "42";
        let mut program = parse_program(source).unwrap();

        // Get the expression
        if let Some(item) = program.items.first_mut() {
            if let outrun_parser::ItemKind::Expression(ref mut expr) = item.kind {
                // Initially, no type info should be present
                assert!(expr.type_info.is_none());

                // Simulate typechecker populating type info
                let parsed_type_info = ParsedTypeInfo::new("Integer");
                expr.type_info = Some(parsed_type_info);

                // Verify type info is now present
                assert!(expr.type_info.is_some());
                assert_eq!(expr.type_info.as_ref().unwrap().resolved_type, "Integer");
            } else {
                panic!("Expected expression item");
            }
        } else {
            panic!("Expected at least one item in program");
        }
    }

    #[test]
    fn test_complex_type_conversion() {
        // Create a complex generic type
        let inner_type = Type::concrete("String");
        let list_type = Type::generic_concrete("List", vec![inner_type]);
        let type_info = TypeInfo::new(list_type);

        // Convert to parser's ParsedTypeInfo
        let parsed_type_info = type_info.to_parsed_type_info();

        // Verify the conversion shows the generic type structure
        assert_eq!(parsed_type_info.resolved_type, "List<String>");
        assert!(parsed_type_info.type_span.is_none());
    }

    #[test]
    fn test_type_variable_conversion() {
        // Create a type variable
        let mut gen = TypeVarGenerator::new();
        let type_var = gen.fresh(Level(0));
        let type_info = TypeInfo::new(type_var);

        // Convert to parser's ParsedTypeInfo
        let parsed_type_info = type_info.to_parsed_type_info();

        // Verify the conversion shows type variable representation
        assert_eq!(parsed_type_info.resolved_type, "T0");
        assert!(parsed_type_info.type_span.is_none());
    }

    #[test]
    fn test_function_type_conversion() {
        // Create a function type
        let param_type = Type::concrete("Integer");
        let return_type = Type::concrete("String");
        let function_type = Type::Function {
            params: vec![("x".to_string(), param_type)],
            return_type: Box::new(return_type),
            span: None,
        };
        let type_info = TypeInfo::new(function_type);

        // Convert to parser's ParsedTypeInfo
        let parsed_type_info = type_info.to_parsed_type_info();

        // Verify the conversion shows function type structure
        assert_eq!(
            parsed_type_info.resolved_type,
            "Function<(x: Integer) -> String>"
        );
        assert!(parsed_type_info.type_span.is_none());
    }

    #[test]
    fn test_integration_workflow() {
        // This test demonstrates the complete workflow:
        // 1. Parse source code (creates AST with no type info)
        // 2. Simulate typechecker adding type information
        // 3. Verify the integration works end-to-end

        let source = r#"
            let x = 42
            let y = "hello"
        "#;

        let mut program = parse_program(source).unwrap();

        // Simulate what the typechecker would do
        for item in program.items.iter_mut() {
            if let outrun_parser::ItemKind::LetBinding(let_binding) = &mut item.kind {
                // Simulate type inference on the expression
                match &let_binding.expression.kind {
                    ExpressionKind::Integer(_) => {
                        let type_info = TypeInfo::new(Type::concrete("Integer"));
                        let_binding.expression.type_info = Some(type_info.to_parsed_type_info());
                    }
                    ExpressionKind::String(_) => {
                        let type_info = TypeInfo::new(Type::concrete("String"));
                        let_binding.expression.type_info = Some(type_info.to_parsed_type_info());
                    }
                    _ => {}
                }
            }
        }

        // Verify the type information was populated
        let mut found_integer = false;
        let mut found_string = false;

        for item in &program.items {
            if let outrun_parser::ItemKind::LetBinding(let_binding) = &item.kind {
                if let Some(ref type_info) = let_binding.expression.type_info {
                    match type_info.resolved_type.as_str() {
                        "Integer" => found_integer = true,
                        "String" => found_string = true,
                        _ => {}
                    }
                }
            }
        }

        assert!(found_integer, "Should have found Integer type");
        assert!(found_string, "Should have found String type");
    }
}

#[cfg(test)]
mod tests;
