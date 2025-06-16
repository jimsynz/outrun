use crate::{parse_program, ImportClause, ImportDefinition, ItemKind};

/// Helper function to extract import definition from parse result
fn extract_import(input: &str) -> ImportDefinition {
    let result = parse_program(input).expect("Failed to parse");
    assert_eq!(result.items.len(), 1, "Expected exactly one item");

    match &result.items[0].kind {
        ItemKind::ImportDefinition(import_def) => import_def.clone(),
        other => panic!("Expected ImportDefinition, got {:?}", other),
    }
}

#[test]
fn test_import_simple_module() {
    let input = "import String";
    let import = extract_import(input);

    assert_eq!(import.path.len(), 1);
    assert_eq!(import.path[0].name, "String");
    assert_eq!(import.clauses.len(), 0);
}

#[test]
fn test_import_nested_module() {
    let input = "import Http.Client";
    let import = extract_import(input);

    assert_eq!(import.path.len(), 2);
    assert_eq!(import.path[0].name, "Http");
    assert_eq!(import.path[1].name, "Client");
    assert_eq!(import.clauses.len(), 0);
}

#[test]
fn test_import_deep_nested_module() {
    let input = "import Outrun.Core.Collections.List";
    let import = extract_import(input);

    assert_eq!(import.path.len(), 4);
    assert_eq!(import.path[0].name, "Outrun");
    assert_eq!(import.path[1].name, "Core");
    assert_eq!(import.path[2].name, "Collections");
    assert_eq!(import.path[3].name, "List");
    assert_eq!(import.clauses.len(), 0);
}

#[test]
fn test_import_with_only_clause_single_function() {
    let input = "import String, only: [length: 1]";
    let import = extract_import(input);

    assert_eq!(import.path.len(), 1);
    assert_eq!(import.path[0].name, "String");
    assert_eq!(import.clauses.len(), 1);

    match &import.clauses[0] {
        ImportClause::Only { functions, .. } => {
            assert_eq!(functions.len(), 1);
            assert_eq!(functions[0].name.name, "length");
            assert_eq!(functions[0].arity, 1);
        }
        other => panic!("Expected Only clause, got {:?}", other),
    }
}

#[test]
fn test_import_with_only_clause_multiple_functions() {
    let input = "import List, only: [length: 1, empty?: 1, append: 2]";
    let import = extract_import(input);

    assert_eq!(import.path.len(), 1);
    assert_eq!(import.path[0].name, "List");
    assert_eq!(import.clauses.len(), 1);

    match &import.clauses[0] {
        ImportClause::Only { functions, .. } => {
            assert_eq!(functions.len(), 3);

            assert_eq!(functions[0].name.name, "length");
            assert_eq!(functions[0].arity, 1);

            assert_eq!(functions[1].name.name, "empty?");
            assert_eq!(functions[1].arity, 1);

            assert_eq!(functions[2].name.name, "append");
            assert_eq!(functions[2].arity, 2);
        }
        other => panic!("Expected Only clause, got {:?}", other),
    }
}

#[test]
fn test_import_with_except_clause_single_function() {
    let input = "import String, except: [unsafe_operation: 1]";
    let import = extract_import(input);

    assert_eq!(import.path.len(), 1);
    assert_eq!(import.path[0].name, "String");
    assert_eq!(import.clauses.len(), 1);

    match &import.clauses[0] {
        ImportClause::Except { functions, .. } => {
            assert_eq!(functions.len(), 1);
            assert_eq!(functions[0].name.name, "unsafe_operation");
            assert_eq!(functions[0].arity, 1);
        }
        other => panic!("Expected Except clause, got {:?}", other),
    }
}

#[test]
fn test_import_with_except_clause_multiple_functions() {
    let input = "import Map, except: [unsafe_get: 2, deprecated_method: 1, old_api: 3]";
    let import = extract_import(input);

    assert_eq!(import.path.len(), 1);
    assert_eq!(import.path[0].name, "Map");
    assert_eq!(import.clauses.len(), 1);

    match &import.clauses[0] {
        ImportClause::Except { functions, .. } => {
            assert_eq!(functions.len(), 3);

            assert_eq!(functions[0].name.name, "unsafe_get");
            assert_eq!(functions[0].arity, 2);

            assert_eq!(functions[1].name.name, "deprecated_method");
            assert_eq!(functions[1].arity, 1);

            assert_eq!(functions[2].name.name, "old_api");
            assert_eq!(functions[2].arity, 3);
        }
        other => panic!("Expected Except clause, got {:?}", other),
    }
}

#[test]
fn test_import_with_multiple_clauses() {
    let input = "import Utilities, only: [helper: 1], except: [debug_print: 1]";
    let import = extract_import(input);

    assert_eq!(import.path.len(), 1);
    assert_eq!(import.path[0].name, "Utilities");
    assert_eq!(import.clauses.len(), 2);

    // First clause should be "only"
    match &import.clauses[0] {
        ImportClause::Only { functions, .. } => {
            assert_eq!(functions.len(), 1);
            assert_eq!(functions[0].name.name, "helper");
            assert_eq!(functions[0].arity, 1);
        }
        other => panic!("Expected Only clause, got {:?}", other),
    }

    // Second clause should be "except"
    match &import.clauses[1] {
        ImportClause::Except { functions, .. } => {
            assert_eq!(functions.len(), 1);
            assert_eq!(functions[0].name.name, "debug_print");
            assert_eq!(functions[0].arity, 1);
        }
        other => panic!("Expected Except clause, got {:?}", other),
    }
}

#[test]
fn test_import_with_trailing_comma_in_function_list() {
    let input = "import List, only: [length: 1, empty?: 1,]";
    let import = extract_import(input);

    assert_eq!(import.clauses.len(), 1);

    match &import.clauses[0] {
        ImportClause::Only { functions, .. } => {
            assert_eq!(functions.len(), 2);
            assert_eq!(functions[0].name.name, "length");
            assert_eq!(functions[1].name.name, "empty?");
        }
        other => panic!("Expected Only clause, got {:?}", other),
    }
}

#[test]
fn test_import_with_zero_arity_function() {
    let input = "import Random, only: [initialize: 0, get_seed: 0]";
    let import = extract_import(input);

    match &import.clauses[0] {
        ImportClause::Only { functions, .. } => {
            assert_eq!(functions.len(), 2);
            assert_eq!(functions[0].name.name, "initialize");
            assert_eq!(functions[0].arity, 0);
            assert_eq!(functions[1].name.name, "get_seed");
            assert_eq!(functions[1].arity, 0);
        }
        other => panic!("Expected Only clause, got {:?}", other),
    }
}

#[test]
fn test_import_with_high_arity_function() {
    let input = "import Complex, only: [complex_operation: 10]";
    let import = extract_import(input);

    match &import.clauses[0] {
        ImportClause::Only { functions, .. } => {
            assert_eq!(functions.len(), 1);
            assert_eq!(functions[0].name.name, "complex_operation");
            assert_eq!(functions[0].arity, 10);
        }
        other => panic!("Expected Only clause, got {:?}", other),
    }
}

#[test]
fn test_import_display_simple() {
    let input = "import String";
    let import = extract_import(input);
    let displayed = format!("{}", import);
    assert_eq!(displayed, "import String");
}

#[test]
fn test_import_display_nested() {
    let input = "import Http.Client";
    let import = extract_import(input);
    let displayed = format!("{}", import);
    assert_eq!(displayed, "import Http.Client");
}

#[test]
fn test_import_display_with_only_clause() {
    let input = "import List, only: [length: 1, append: 2]";
    let import = extract_import(input);
    let displayed = format!("{}", import);
    assert_eq!(displayed, "import List, only: [length: 1, append: 2]");
}

#[test]
fn test_import_display_with_except_clause() {
    let input = "import Map, except: [unsafe_get: 2]";
    let import = extract_import(input);
    let displayed = format!("{}", import);
    assert_eq!(displayed, "import Map, except: [unsafe_get: 2]");
}

#[test]
fn test_import_display_with_multiple_clauses() {
    let input = "import Utilities, only: [helper: 1], except: [debug: 1]";
    let import = extract_import(input);
    let displayed = format!("{}", import);
    assert_eq!(
        displayed,
        "import Utilities, only: [helper: 1], except: [debug: 1]"
    );
}
