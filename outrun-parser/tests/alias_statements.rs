use outrun_parser::{parse_program, AliasDefinition, AliasPath, ItemKind};

/// Helper function to extract alias definition from parse result
fn extract_alias(input: &str) -> AliasDefinition {
    let result = parse_program(input).expect("Failed to parse");
    assert_eq!(result.items.len(), 1, "Expected exactly one item");

    match &result.items[0].kind {
        ItemKind::AliasDefinition(alias_def) => alias_def.clone(),
        other => panic!("Expected AliasDefinition, got {:?}", other),
    }
}

#[test]
fn test_alias_simple_module_path() {
    let input = "alias Outrun.Option";
    let alias = extract_alias(input);

    match &alias.path {
        AliasPath::Simple { path, .. } => {
            assert_eq!(path.len(), 2);
            assert_eq!(path[0].name, "Outrun");
            assert_eq!(path[1].name, "Option");
        }
        other => panic!("Expected Simple alias path, got {:?}", other),
    }

    assert!(alias.alias_name.is_none());
}

#[test]
fn test_alias_simple_with_as_clause() {
    let input = "alias Outrun.Option as Option";
    let alias = extract_alias(input);

    match &alias.path {
        AliasPath::Simple { path, .. } => {
            assert_eq!(path.len(), 2);
            assert_eq!(path[0].name, "Outrun");
            assert_eq!(path[1].name, "Option");
        }
        other => panic!("Expected Simple alias path, got {:?}", other),
    }

    let alias_name = alias.alias_name.expect("Expected alias name");
    assert_eq!(alias_name.name, "Option");
}

#[test]
fn test_alias_single_module() {
    let input = "alias String";
    let alias = extract_alias(input);

    match &alias.path {
        AliasPath::Simple { path, .. } => {
            assert_eq!(path.len(), 1);
            assert_eq!(path[0].name, "String");
        }
        other => panic!("Expected Simple alias path, got {:?}", other),
    }

    assert!(alias.alias_name.is_none());
}

#[test]
fn test_alias_brace_expansion_simple() {
    let input = "alias Outrun.{Option, Result}";
    let alias = extract_alias(input);

    match &alias.path {
        AliasPath::BraceExpansion {
            base_path, items, ..
        } => {
            assert_eq!(base_path.len(), 1);
            assert_eq!(base_path[0].name, "Outrun");

            assert_eq!(items.len(), 2);
            assert_eq!(items[0].name.name, "Option");
            assert!(items[0].alias_name.is_none());
            assert_eq!(items[1].name.name, "Result");
            assert!(items[1].alias_name.is_none());
        }
        other => panic!("Expected BraceExpansion alias path, got {:?}", other),
    }

    assert!(alias.alias_name.is_none());
}

#[test]
fn test_alias_brace_expansion_with_as_clauses() {
    let input = "alias Outrun.{Option as Opt, Result, Iterator as Iter}";
    let alias = extract_alias(input);

    match &alias.path {
        AliasPath::BraceExpansion {
            base_path, items, ..
        } => {
            assert_eq!(base_path.len(), 1);
            assert_eq!(base_path[0].name, "Outrun");

            assert_eq!(items.len(), 3);

            // Option as Opt
            assert_eq!(items[0].name.name, "Option");
            let opt_alias = items[0].alias_name.as_ref().expect("Expected alias name");
            assert_eq!(opt_alias.name, "Opt");

            // Result (no alias)
            assert_eq!(items[1].name.name, "Result");
            assert!(items[1].alias_name.is_none());

            // Iterator as Iter
            assert_eq!(items[2].name.name, "Iterator");
            let iter_alias = items[2].alias_name.as_ref().expect("Expected alias name");
            assert_eq!(iter_alias.name, "Iter");
        }
        other => panic!("Expected BraceExpansion alias path, got {:?}", other),
    }

    assert!(alias.alias_name.is_none());
}

#[test]
fn test_alias_brace_expansion_nested_module() {
    let input = "alias Http.Client.{Connection, Request, Response}";
    let alias = extract_alias(input);

    match &alias.path {
        AliasPath::BraceExpansion {
            base_path, items, ..
        } => {
            assert_eq!(base_path.len(), 2);
            assert_eq!(base_path[0].name, "Http");
            assert_eq!(base_path[1].name, "Client");

            assert_eq!(items.len(), 3);
            assert_eq!(items[0].name.name, "Connection");
            assert_eq!(items[1].name.name, "Request");
            assert_eq!(items[2].name.name, "Response");
        }
        other => panic!("Expected BraceExpansion alias path, got {:?}", other),
    }
}

#[test]
fn test_alias_brace_expansion_trailing_comma() {
    let input = "alias Core.{String, Integer,}";
    let alias = extract_alias(input);

    match &alias.path {
        AliasPath::BraceExpansion {
            base_path, items, ..
        } => {
            assert_eq!(base_path.len(), 1);
            assert_eq!(base_path[0].name, "Core");

            assert_eq!(items.len(), 2);
            assert_eq!(items[0].name.name, "String");
            assert_eq!(items[1].name.name, "Integer");
        }
        other => panic!("Expected BraceExpansion alias path, got {:?}", other),
    }
}

#[test]
fn test_alias_display_simple() {
    let input = "alias Outrun.Option";
    let alias = extract_alias(input);
    let displayed = format!("{}", alias);
    assert_eq!(displayed, "alias Outrun.Option");
}

#[test]
fn test_alias_display_with_as() {
    let input = "alias Outrun.Option as Option";
    let alias = extract_alias(input);
    let displayed = format!("{}", alias);
    assert_eq!(displayed, "alias Outrun.Option as Option");
}

#[test]
fn test_alias_display_brace_expansion() {
    let input = "alias Outrun.{Option, Result}";
    let alias = extract_alias(input);
    let displayed = format!("{}", alias);
    assert_eq!(displayed, "alias Outrun.{Option, Result}");
}

#[test]
fn test_alias_display_brace_expansion_with_as() {
    let input = "alias Outrun.{Option as Opt, Result}";
    let alias = extract_alias(input);
    let displayed = format!("{}", alias);
    assert_eq!(displayed, "alias Outrun.{Option as Opt, Result}");
}
