use outrun_interpreter::test_harness::OutrunTestHarness;

fn main() {
    println!("Creating test harness...");
    let mut harness = match OutrunTestHarness::new() {
        Ok(h) => h,
        Err(e) => {
            println!("Failed to create harness: {:?}", e);
            return;
        }
    };

    println!("Testing 42...");
    match harness.assert_evaluates_to("42", "42") {
        Ok(_) => println!("✅ 42 test passed"),
        Err(e) => println!("❌ 42 test failed: {:?}", e),
    }

    println!("Testing true...");
    match harness.assert_evaluates_to("true", "true") {
        Ok(_) => println!("✅ true test passed"),
        Err(e) => println!("❌ true test failed: {:?}", e),
    }

    println!("Testing string...");
    match harness.assert_evaluates_to("\"test\"", "\"test\"") {
        Ok(_) => println!("✅ string test passed"),
        Err(e) => println!("❌ string test failed: {:?}", e),
    }
}