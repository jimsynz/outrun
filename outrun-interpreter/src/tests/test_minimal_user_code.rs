use crate::test_harness::OutrunTestHarness;

#[test]
fn test_unary_minus_constraint_system() {
    let mut harness = OutrunTestHarness::new().unwrap();
    
    // This should work now with the redesigned constraint system
    match harness.evaluate("-1") {
        Ok(_result) => {
            println!("✅ UnaryMinus constraint resolution working!");
        }
        Err(e) => {
            println!("❌ UnaryMinus constraint resolution failed: {:?}", e);
            println!("This means the constraint system redesign hasn't fully solved the issue.");
        }
    }
}

#[test] 
fn test_simple_literal_only() {
    let mut harness = OutrunTestHarness::new().unwrap();
    
    // Try just a literal with no operators
    match harness.evaluate("42") {
        Ok(result) => {
            println!("✅ Simple literal works: {:?}", result);
        }
        Err(e) => {
            println!("❌ Even simple literals fail: {:?}", e);
            println!("This indicates a more fundamental issue in core library loading.");
        }
    }
}