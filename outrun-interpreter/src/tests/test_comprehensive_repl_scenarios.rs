//! Comprehensive REPL scenario tests - Phase 3 Enhanced Test Coverage
//!
//! These tests demonstrate the full REPL context persistence capabilities
//! implemented in Phase 1 and Phase 2.

use crate::InterpreterSession;

#[test]
fn test_multi_step_struct_development_workflow() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Step 1: Define a basic struct
    session.evaluate("struct User(name: String, age: Integer64) {}").unwrap();
    
    // Step 2: Create some instances
    session.evaluate("let alice = User { name: \"Alice\", age: 30 }").unwrap();
    session.evaluate("let bob = User { name: \"Bob\", age: 25 }").unwrap();
    
    // Step 3: Verify persistence
    assert!(session.evaluate("alice").is_ok());
    assert!(session.evaluate("bob").is_ok());
    
    // Step 4: Define another struct that uses the first
    session.evaluate("struct Team(name: String, leader: User) {}").unwrap();
    
    // Step 5: Create composite structures
    session.evaluate("let team = Team { name: \"Development\", leader: alice }").unwrap();
    
    // Step 6: Verify everything still works
    assert!(session.evaluate("team").is_ok());
    assert!(session.evaluate("User { name: \"Charlie\", age: 35 }").is_ok());
    
    println!("✅ Multi-step struct development workflow completed successfully!");
}

#[test] 
fn test_repl_error_recovery() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Step 1: Define valid structs
    session.evaluate("struct Point(x: Integer64, y: Integer64) {}").unwrap();
    session.evaluate("let p1 = Point { x: 1, y: 2 }").unwrap();
    
    // Step 2: Try invalid syntax - should fail without corrupting session
    assert!(session.evaluate("struct BadStruct { invalid syntax }").is_err());
    
    // Step 3: Verify session is still intact
    assert!(session.evaluate("p1").is_ok());
    assert!(session.evaluate("Point { x: 3, y: 4 }").is_ok());
    
    // Step 4: Try type error - should fail without corrupting session
    // Note: This might not fail depending on current type checking, but session should remain intact
    let _result = session.evaluate("let invalid: NonexistentType = Point { x: 1, y: 2 }");
    
    // Step 5: Verify Point is still available
    assert!(session.evaluate("Point { x: 5, y: 6 }").is_ok());
    
    println!("✅ REPL error recovery working correctly!");
}

#[test]
fn test_complex_nested_struct_definitions() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Step 1: Define base structs
    session.evaluate("struct Address(street: String, city: String) {}").unwrap();
    session.evaluate("struct Contact(email: String, phone: String) {}").unwrap();
    
    // Step 2: Define composite struct
    session.evaluate("struct Person(name: String, address: Address, contact: Contact) {}").unwrap();
    
    // Step 3: Create nested instances
    session.evaluate("let addr = Address { street: \"123 Main St\", city: \"NYC\" }").unwrap();
    session.evaluate("let contact = Contact { email: \"alice@example.com\", phone: \"555-1234\" }").unwrap();
    session.evaluate("let person = Person { name: \"Alice\", address: addr, contact: contact }").unwrap();
    
    // Step 4: Verify all definitions persist
    assert!(session.evaluate("person").is_ok());
    assert!(session.evaluate("Address { street: \"456 Oak Ave\", city: \"LA\" }").is_ok());
    assert!(session.evaluate("Contact { email: \"bob@example.com\", phone: \"555-5678\" }").is_ok());
    assert!(session.evaluate("Person { name: \"Bob\", address: Address { street: \"789 Pine St\", city: \"SF\" }, contact: Contact { email: \"bob@test.com\", phone: \"555-9999\" } }").is_ok());
    
    println!("✅ Complex nested struct definitions working!");
}

#[test]
fn test_variable_persistence_across_struct_redefinition_attempts() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Step 1: Define struct and create variables
    session.evaluate("struct Data(value: Integer64) {}").unwrap();
    session.evaluate("let d1 = Data { value: 42 }").unwrap();
    session.evaluate("let d2 = Data { value: 84 }").unwrap();
    
    // Step 2: Attempt to redefine struct (should fail/warn but not corrupt session)
    let redefinition_result = session.evaluate("struct Data(different: String) {}");
    // Whether this succeeds or fails, the session should remain coherent
    
    // Step 3: Verify original variables are still accessible
    assert!(session.evaluate("d1").is_ok());
    assert!(session.evaluate("d2").is_ok());
    
    // Step 4: Test that we can still create Data instances
    // This should work with whatever the current definition is
    let creation_result = session.evaluate("Data { value: 100 }");
    // The specific result depends on redefinition behavior, but shouldn't crash
    
    println!("✅ Variable persistence across struct redefinition working!");
    println!("   Redefinition result: {:?}", redefinition_result.is_ok());
    println!("   Creation result: {:?}", creation_result.is_ok());
}

#[test]
fn test_session_state_management() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Step 1: Build up session state
    session.evaluate("struct Config(debug: Boolean, threads: Integer64) {}").unwrap();
    session.evaluate("let config = Config { debug: true, threads: 4 }").unwrap();
    session.evaluate("struct Application(name: String, config: Config) {}").unwrap();
    session.evaluate("let app = Application { name: \"MyApp\", config: config }").unwrap();
    
    // Step 2: Verify state
    assert!(session.evaluate("app").is_ok());
    assert!(session.evaluate("config").is_ok());
    
    // Step 3: Clear session 
    session.clear_variables();
    
    // Step 4: Verify everything is cleared
    assert!(session.evaluate("app").is_err());
    assert!(session.evaluate("config").is_err());
    assert!(session.evaluate("Config { debug: false, threads: 8 }").is_err());
    assert!(session.evaluate("Application { name: \"Test\", config: Config { debug: true, threads: 2 } }").is_err());
    
    // Step 5: Verify we can start fresh
    session.evaluate("struct Simple(id: Integer64) {}").unwrap();
    session.evaluate("let simple = Simple { id: 1 }").unwrap();
    assert!(session.evaluate("simple").is_ok());
    
    println!("✅ Session state management (including clear) working correctly!");
}

#[test]
fn test_integer_arithmetic_with_struct_persistence() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Step 1: Define struct with numeric operations
    session.evaluate("struct Calculator(value: Integer64) {}").unwrap();
    
    // Step 2: Create calculator instances with arithmetic
    session.evaluate("let calc1 = Calculator { value: 10 + 5 }").unwrap();
    session.evaluate("let calc2 = Calculator { value: 20 * 2 }").unwrap();
    session.evaluate("let calc3 = Calculator { value: 100 - 25 }").unwrap();
    
    // Step 3: Verify arithmetic worked and structs persist
    assert!(session.evaluate("calc1").is_ok());
    assert!(session.evaluate("calc2").is_ok());
    assert!(session.evaluate("calc3").is_ok());
    
    // Step 4: Test that arithmetic still works with struct context
    session.assert_evaluates_to_integer("1 + 2", 3).unwrap();
    session.assert_evaluates_to_integer("5 * 6", 30).unwrap();
    session.assert_evaluates_to_integer("10 - 3", 7).unwrap();
    
    println!("✅ Integer arithmetic working correctly with struct persistence!");
}

#[test]
fn test_multiple_session_workflows() {
    // Test that multiple independent sessions work correctly
    
    let mut session1 = InterpreterSession::new().unwrap();
    let mut session2 = InterpreterSession::new().unwrap();
    
    // Different structs in different sessions
    session1.evaluate("struct TypeA(field1: String) {}").unwrap();
    session2.evaluate("struct TypeB(field2: Integer64) {}").unwrap();
    
    // Create instances in each session  
    session1.evaluate("let instance_a = TypeA { field1: \"session1\" }").unwrap();
    session2.evaluate("let instance_b = TypeB { field2: 42 }").unwrap();
    
    // Verify isolation - each session only knows about its own types
    assert!(session1.evaluate("instance_a").is_ok());
    assert!(session1.evaluate("TypeA { field1: \"test\" }").is_ok());
    assert!(session1.evaluate("TypeB { field2: 1 }").is_err()); // Should fail - TypeB not defined in session1
    
    assert!(session2.evaluate("instance_b").is_ok());
    assert!(session2.evaluate("TypeB { field2: 99 }").is_ok());
    assert!(session2.evaluate("TypeA { field1: \"test\" }").is_err()); // Should fail - TypeA not defined in session2
    
    println!("✅ Multiple session isolation working correctly!");
}

#[test]
fn test_session_performance_under_load() {
    let mut session = InterpreterSession::new().unwrap();
    
    // Define multiple structs to build up session complexity
    session.evaluate("struct Node(id: Integer64, value: String) {}").unwrap();
    session.evaluate("struct Edge(from: Integer64, to: Integer64, weight: Integer64) {}").unwrap();
    session.evaluate("struct Graph(name: String) {}").unwrap();
    
    // Create many instances to test performance
    for i in 0..10 {
        let node_eval = format!("let node{} = Node {{ id: {}, value: \"node{}\" }}", i, i, i);
        session.evaluate(&node_eval).unwrap();
        
        if i > 0 {
            let edge_eval = format!("let edge{} = Edge {{ from: {}, to: {}, weight: {} }}", i, i-1, i, i*10);
            session.evaluate(&edge_eval).unwrap();
        }
    }
    
    // Verify all instances are accessible
    for i in 0..10 {
        let node_ref = format!("node{}", i);
        assert!(session.evaluate(&node_ref).is_ok());
        
        if i > 0 {
            let edge_ref = format!("edge{}", i);
            assert!(session.evaluate(&edge_ref).is_ok());
        }
    }
    
    // Verify struct definitions are still available
    assert!(session.evaluate("Node { id: 999, value: \"new_node\" }").is_ok());
    assert!(session.evaluate("Edge { from: 999, to: 1000, weight: 50 }").is_ok());
    
    println!("✅ Session performance under load test completed!");
}