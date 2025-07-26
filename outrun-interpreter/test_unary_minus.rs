use outrun_interpreter::test_harness::OutrunTestHarness;

fn main() {
    println!("Testing UnaryMinus with redesigned constraint system...");
    
    let mut harness = OutrunTestHarness::new().unwrap();
    
    match harness.evaluate("-1") {
        Ok(result) => {
            println!("✅ SUCCESS: UnaryMinus works! Result: {:?}", result);
            println!("🎉 Constraint system redesign successful - user code can run!");
        }
        Err(e) => {
            println!("❌ FAILED: {:?}", e);
            println!("Still debugging constraint system issues...");
        }
    }
}