use crate::intrinsics::IntrinsicsHandler;
use outrun_typechecker::intrinsics::get_intrinsic_function_definitions;
use std::collections::HashSet;

#[test]
fn test_intrinsic_registry_completeness() {
    // Step 1: Get a list of all intrinsic function names from the type-checker registry and put them into a set
    let typechecker_intrinsics = get_intrinsic_function_definitions();
    let typechecker_names: HashSet<String> = typechecker_intrinsics
        .into_iter()
        .map(|func_def| format!("Outrun.Intrinsic.{}", func_def.name.name))
        .collect();

    // Step 2: Get a list of all intrinsic function names from the interpreter registry and put them into a set
    let interpreter_names: HashSet<String> = IntrinsicsHandler::get_all_intrinsic_names();

    // Step 3: Assert that the two sets are equal
    assert_eq!(
        typechecker_names,
        interpreter_names,
        "Typechecker and interpreter intrinsic registries must match exactly.\n\
         Missing from interpreter: {:?}\n\
         Extra in interpreter: {:?}",
        typechecker_names
            .difference(&interpreter_names)
            .collect::<Vec<_>>(),
        interpreter_names
            .difference(&typechecker_names)
            .collect::<Vec<_>>()
    );
}
