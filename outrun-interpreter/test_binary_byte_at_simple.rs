use outrun_interpreter::test_harness::OutrunTestHarness;

fn main() {
    println!("Testing Binary.byte_at without pipe operator...");
    let mut harness = match OutrunTestHarness::new() {
        Ok(h) => h,
        Err(e) => {
            println!("Failed to create harness: {:?}", e);
            return;
        }
    };

    // Test what Binary.byte_at returns directly
    println!("Testing Binary.byte_at(value: \"hello\", index: 0)...");
    match harness.evaluate("Binary.byte_at(value: \"hello\", index: 0)") {
        Ok(result) => println!("✅ Result: {:?}", result),
        Err(e) => println!("❌ Error: {:?}", e),
    }
}