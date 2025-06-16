use crate::checker::TypeContext;

#[test]
fn test_generic_param_scope_system() {
    let mut context = TypeContext::new();

    // Create a scope
    context.push_scope(false);

    // Register a generic parameter
    let t_type_id = context.interner.intern_type("T");
    let result = context.register_generic_param("T".to_string(), t_type_id);

    assert!(
        result.is_ok(),
        "Should be able to register generic parameter"
    );

    // Look up the generic parameter
    let lookup_result = context.lookup_generic_param("T");
    assert!(
        lookup_result.is_some(),
        "Should be able to lookup generic parameter T"
    );
    assert_eq!(
        lookup_result.unwrap(),
        t_type_id,
        "Should return the same TypeId"
    );

    // Check if it's recognized as a generic param
    assert!(
        context.is_generic_param("T"),
        "Should recognize T as a generic parameter"
    );

    // Clean up
    context.pop_scope();

    // Should no longer be available after scope cleanup
    assert!(
        !context.is_generic_param("T"),
        "T should not be available after scope cleanup"
    );
}
