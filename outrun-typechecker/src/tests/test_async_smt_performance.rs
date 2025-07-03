//! Test async SMT performance improvements from parallelization

#![allow(clippy::uninlined_format_args)]

use crate::compilation::compiler_environment::CompilerEnvironment;
use crate::smt::constraints::SMTConstraint;
use crate::smt::solver_pool::{
    check_constraint_batches_satisfiable_async,
    check_constraints_satisfiable_cached,
    solve_constraint_batches_async,
};
use crate::unification::StructuredType;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::Instant;
use tokio::runtime::Runtime;

#[test]
fn test_parallel_smt_performance_improvement() {
    // Create a Tokio runtime for async operations
    let rt = Runtime::new().expect("Failed to create Tokio runtime");
    
    // Create multiple independent constraint sets to solve in parallel
    let constraint_batches = create_independent_constraint_batches(8);
    
    println!("🔄 Testing parallel vs sequential SMT solving with {} constraint sets", constraint_batches.len());
    
    // Test 1: Sequential solving (baseline)
    let sequential_start = Instant::now();
    let mut sequential_results = Vec::new();
    for (constraints, compiler_env) in &constraint_batches {
        let result = check_constraints_satisfiable_cached(constraints, compiler_env);
        sequential_results.push(result);
    }
    let sequential_duration = sequential_start.elapsed();
    
    println!("⏱️  Sequential solving took: {:?}", sequential_duration);
    
    // Test 2: Parallel solving with async
    let parallel_start = Instant::now();
    let parallel_results = rt.block_on(async {
        check_constraint_batches_satisfiable_async(constraint_batches.clone()).await
    });
    let parallel_duration = parallel_start.elapsed();
    
    println!("⏱️  Parallel solving took: {:?}", parallel_duration);
    
    // Verify results are consistent
    assert_eq!(sequential_results.len(), parallel_results.len());
    for (seq_result, par_result) in sequential_results.iter().zip(parallel_results.iter()) {
        // Both should succeed or fail in the same way
        match (seq_result, par_result) {
            (Ok(seq_val), Ok(par_val)) => assert_eq!(seq_val, par_val),
            (Err(_), Err(_)) => {}, // Both failed - acceptable
            _ => panic!("Sequential and parallel results should be consistent"),
        }
    }
    
    // Calculate performance improvement
    let speedup_ratio = sequential_duration.as_secs_f64() / parallel_duration.as_secs_f64();
    println!("🚀 Speedup ratio: {:.2}x", speedup_ratio);
    
    if speedup_ratio > 1.1 {
        println!("✅ Async parallelization provides {:.1}% performance improvement", (speedup_ratio - 1.0) * 100.0);
    } else {
        println!("ℹ️  No significant speedup observed (overhead may dominate for simple constraints)");
    }
    
    // The test passes if both approaches complete without panicking
    // and produce consistent results
    assert!(sequential_results.len() > 0);
    assert!(parallel_results.len() > 0);
}

#[test]
fn test_complex_parallel_smt_workload() {
    let rt = Runtime::new().expect("Failed to create Tokio runtime");
    
    // Create more complex constraint sets that take longer to solve
    let complex_batches = create_complex_constraint_batches(4);
    
    println!("🔄 Testing complex parallel SMT workload with {} constraint sets", complex_batches.len());
    
    // Test parallel solving with more complex constraints
    let start = Instant::now();
    let results = rt.block_on(async {
        solve_constraint_batches_async(complex_batches).await
    });
    let duration = start.elapsed();
    
    println!("⏱️  Complex parallel solving took: {:?}", duration);
    println!("📊 Processed {} complex constraint sets", results.len());
    
    // Verify all results are proper (not panics)
    for (i, result) in results.iter().enumerate() {
        match result {
            Ok(_) => println!("✅ Constraint set {} solved successfully", i),
            Err(e) => println!("⚠️  Constraint set {} failed: {:?}", i, e),
        }
    }
    
    assert_eq!(results.len(), 4);
}

/// Create multiple independent constraint sets for parallel testing
fn create_independent_constraint_batches(count: usize) -> Vec<(Vec<SMTConstraint>, CompilerEnvironment)> {
    let mut batches = Vec::new();
    
    for i in 0..count {
        let storage = Arc::new(RwLock::new(HashMap::new()));
        
        // Create unique types for this batch
        let type_id = crate::compilation::compiler_environment::TypeNameId::new(1000 + i as u64, storage.clone());
        let trait_id = crate::compilation::compiler_environment::TypeNameId::new(2000 + i as u64, storage.clone());
        let impl_type_id = crate::compilation::compiler_environment::TypeNameId::new(3000 + i as u64, storage.clone());
        
        let constraints = vec![
            // Basic trait implementation constraint
            SMTConstraint::TraitImplemented {
                impl_type: StructuredType::Simple(type_id.clone()),
                trait_type: StructuredType::Simple(trait_id.clone()),
            },
            // Type unification constraint
            SMTConstraint::TypeUnification {
                type1: StructuredType::Simple(type_id.clone()),
                type2: StructuredType::Simple(impl_type_id.clone()),
                context: format!("test_batch_{}", i),
            },
            // Self type inference constraint
            SMTConstraint::SelfTypeInference {
                self_variable_id: type_id.clone(),
                inferred_type: StructuredType::Simple(impl_type_id),
                call_site_context: format!("test_call_site_{}", i),
                confidence: crate::smt::constraints::InferenceConfidence::High,
            },
        ];
        
        batches.push((constraints, CompilerEnvironment::new()));
    }
    
    batches
}

/// Create more complex constraint sets with multiple interdependent constraints
fn create_complex_constraint_batches(count: usize) -> Vec<(Vec<SMTConstraint>, CompilerEnvironment)> {
    let mut batches = Vec::new();
    
    for i in 0..count {
        let storage = Arc::new(RwLock::new(HashMap::new()));
        
        // Create a web of interconnected type constraints
        let base_offset = (i * 10) as u64;
        let mut constraints = Vec::new();
        
        for j in 0..5 {
            let type_id = crate::compilation::compiler_environment::TypeNameId::new(base_offset + j, storage.clone());
            let trait_id = crate::compilation::compiler_environment::TypeNameId::new(base_offset + j + 100, storage.clone());
            let param_id = crate::compilation::compiler_environment::TypeNameId::new(base_offset + j + 200, storage.clone());
            
            constraints.push(SMTConstraint::TraitImplemented {
                impl_type: StructuredType::Simple(type_id.clone()),
                trait_type: StructuredType::Simple(trait_id.clone()),
            });
            
            constraints.push(SMTConstraint::TypeParameterUnification {
                parameter_name: format!("T{}", j),
                concrete_type: StructuredType::Simple(param_id),
                context: format!("complex_batch_{}_{}", i, j),
            });
            
            constraints.push(SMTConstraint::TraitCompatibility {
                trait_type: StructuredType::Simple(trait_id),
                implementing_type: StructuredType::Simple(type_id),
                context: format!("compat_{}_{}", i, j),
            });
        }
        
        batches.push((constraints, CompilerEnvironment::new()));
    }
    
    batches
}

#[tokio::test]
async fn test_async_smt_solver_integration() {
    // Test the async SMT solver integration works properly with Tokio
    use crate::smt::solver_pool::{check_constraints_satisfiable_async, solve_constraints_cached_async};
    
    let storage = Arc::new(RwLock::new(HashMap::new()));
    let type_id = crate::compilation::compiler_environment::TypeNameId::new(42, storage.clone());
    let trait_id = crate::compilation::compiler_environment::TypeNameId::new(43, storage.clone());
    
    let constraints = vec![SMTConstraint::TraitImplemented {
        impl_type: StructuredType::Simple(type_id),
        trait_type: StructuredType::Simple(trait_id),
    }];
    
    let compiler_env = CompilerEnvironment::new();
    
    // Test async satisfiability check
    let satisfiable_result = check_constraints_satisfiable_async(constraints.clone(), compiler_env.clone()).await;
    assert!(satisfiable_result.is_ok(), "Async satisfiability check should complete");
    
    // Test async full solving
    let solve_result = solve_constraints_cached_async(constraints, compiler_env).await;
    assert!(solve_result.is_ok(), "Async solving should complete");
    
    println!("✅ Async SMT solver integration test passed");
}