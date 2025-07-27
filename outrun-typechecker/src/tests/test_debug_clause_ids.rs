//! Debug tests for universal clause ID assignment
//!
//! These tests specifically verify that desugared function calls from operators
//! receive proper universal_clause_ids for interpreter dispatch.

use crate::{typecheck_package, Package};
use outrun_parser::{parse_program, ExpressionKind, FunctionPath, ItemKind};

#[test]
fn test_binary_addition_clause_ids() {
    // Test that 1 + 2 gets desugared to BinaryAddition.add with clause IDs
    let mut package = Package::new("test_binary_addition".to_string());
    
    let source = "1 + 2";
    let program = parse_program(source).expect("Should parse successfully");
    package.add_program(program);
    
    // Type check the package
    let _compilation_result = typecheck_package(&mut package).expect("Should type check successfully");
    
    // Find our expression in the programs (it will be in the last program after core library)
    let mut found_expression = None;
    for (prog_idx, program) in package.programs.iter().enumerate() {
        for (item_idx, item) in program.items.iter().enumerate() {
            if let ItemKind::Expression(_) = &item.kind {
                found_expression = Some((prog_idx, item_idx));
                break;
            }
        }
        if found_expression.is_some() {
            break;
        }
    }
    
    let (prog_idx, item_idx) = found_expression.expect("Should find our expression");
    let program = &package.programs[prog_idx];
    let item = &program.items[item_idx];
    
    if let ItemKind::Expression(expr) = &item.kind {
        if let ExpressionKind::FunctionCall(func_call) = &expr.kind {
            if let FunctionPath::Qualified { module, name } = &func_call.path {
                assert_eq!(module.name, "BinaryAddition");
                assert_eq!(name.name, "add");
                
                // This is the critical test - clause IDs should be present
                assert!(
                    func_call.universal_clause_ids.is_some(),
                    "BinaryAddition.add should have universal_clause_ids after type checking"
                );
                
                let clause_ids = func_call.universal_clause_ids.as_ref().unwrap();
                assert!(
                    !clause_ids.is_empty(),
                    "universal_clause_ids should contain at least one clause ID"
                );
                
                println!("✅ BinaryAddition.add has {} clause IDs", clause_ids.len());
            } else {
                panic!("Expected qualified function path for desugared operator");
            }
        } else {
            panic!("Expected function call after operator desugaring");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_let_binding_with_addition_clause_ids() {
    // Test that let x = 1 + 2 gets proper clause IDs (this is the failing test case)
    let mut package = Package::new("test_let_addition".to_string());
    
    let source = "let x = 1 + 2";
    let program = parse_program(source).expect("Should parse successfully");
    package.add_program(program);
    
    // Type check the package
    let _compilation_result = typecheck_package(&mut package).expect("Should type check successfully");
    
    // Check that the desugared function call in the let binding has clause IDs
    println!("Total programs in package: {}", package.programs.len());
    
    // Find our let binding in the programs
    let mut found_let_binding = None;
    for (prog_idx, program) in package.programs.iter().enumerate() {
        for (item_idx, item) in program.items.iter().enumerate() {
            if let ItemKind::LetBinding(_) = &item.kind {
                println!("Found let binding in program {} item {}", prog_idx, item_idx);
                found_let_binding = Some((prog_idx, item_idx));
                break;
            }
        }
        if found_let_binding.is_some() {
            break;
        }
    }
    
    let (prog_idx, item_idx) = found_let_binding.expect("Should find our let binding");
    let program = &package.programs[prog_idx];
    let item = &program.items[item_idx];
    
    // Debug: Print what we actually got
    println!("Item kind: {:?}", std::mem::discriminant(&item.kind));
    match &item.kind {
        ItemKind::Keyword(_) => println!("Got Keyword item (0)"),
        ItemKind::Expression(_) => println!("Got Expression item (1)"),
        ItemKind::BooleanLiteral(_) => println!("Got BooleanLiteral item (2)"),
        ItemKind::IntegerLiteral(_) => println!("Got IntegerLiteral item (3)"),
        ItemKind::FloatLiteral(_) => println!("Got FloatLiteral item (4)"),
        ItemKind::StringLiteral(_) => println!("Got StringLiteral item (5)"),
        ItemKind::AtomLiteral(_) => println!("Got AtomLiteral item (6)"),
        ItemKind::SigilLiteral(_) => println!("Got SigilLiteral item (7)"),
        ItemKind::ListLiteral(_) => println!("Got ListLiteral item (8)"),
        ItemKind::MapLiteral(_) => println!("Got MapLiteral item (9)"),
        ItemKind::TupleLiteral(_) => println!("Got TupleLiteral item (10)"),
        ItemKind::Identifier(_) => println!("Got Identifier item (11)"),
        ItemKind::TypeIdentifier(_) => println!("Got TypeIdentifier item (12)"),
        ItemKind::FunctionDefinition(_) => println!("Got FunctionDefinition item (13)"),
        ItemKind::ConstDefinition(_) => println!("Got ConstDefinition item (14)"),
        ItemKind::LetBinding(_) => println!("Got LetBinding item (15)"),
        ItemKind::StructDefinition(_) => println!("Got StructDefinition item (16)"),
        ItemKind::ProtocolDefinition(_) => println!("Got ProtocolDefinition item (17)"),
        ItemKind::ImplBlock(_) => println!("Got ImplBlock item (18)"),
        ItemKind::AliasDefinition(_) => println!("Got AliasDefinition item (19)"),
        ItemKind::ImportDefinition(_) => println!("Got ImportDefinition item (20)"),
        ItemKind::MacroDefinition(_) => println!("Got MacroDefinition item (21)"),
        ItemKind::Comment(_) => println!("Got Comment item (22)"),
    }
    
    if let ItemKind::LetBinding(let_binding) = &item.kind {
        if let ExpressionKind::FunctionCall(func_call) = &let_binding.expression.kind {
            if let FunctionPath::Qualified { module, name } = &func_call.path {
                assert_eq!(module.name, "BinaryAddition");
                assert_eq!(name.name, "add");
                
                // Debug the function call state
                println!("Function call path: {}.{}", module.name, name.name);
                println!("universal_clause_ids: {:?}", func_call.universal_clause_ids);
                println!("resolved_function_key: {:?}", func_call.resolved_function_key);
                
                // This is the critical test - clause IDs should be present
                if func_call.universal_clause_ids.is_none() {
                    println!("❌ BinaryAddition.add is missing universal_clause_ids!");
                    panic!("BinaryAddition.add in let binding should have universal_clause_ids after type checking");
                }
                
                let clause_ids = func_call.universal_clause_ids.as_ref().unwrap();
                assert!(
                    !clause_ids.is_empty(),
                    "universal_clause_ids should contain at least one clause ID"
                );
                
                println!("✅ Let binding BinaryAddition.add has {} clause IDs", clause_ids.len());
            } else {
                panic!("Expected qualified function path for desugared operator in let binding");
            }
        } else {
            panic!("Expected function call after operator desugaring in let binding");
        }
    } else {
        panic!("Expected let binding item");
    }
}

#[test]
fn test_unary_minus_clause_ids() {
    // Test that -42 gets desugared to UnaryMinus.minus with clause IDs
    let mut package = Package::new("test_unary_minus".to_string());
    
    let source = "-42";
    let program = parse_program(source).expect("Should parse successfully");
    package.add_program(program);
    
    // Type check the package
    let _compilation_result = typecheck_package(&mut package).expect("Should type check successfully");
    
    // Find our expression in the programs (it will be in the last program after core library)
    let mut found_expression = None;
    for (prog_idx, program) in package.programs.iter().enumerate() {
        for (item_idx, item) in program.items.iter().enumerate() {
            if let ItemKind::Expression(_) = &item.kind {
                found_expression = Some((prog_idx, item_idx));
                break;
            }
        }
        if found_expression.is_some() {
            break;
        }
    }
    
    let (prog_idx, item_idx) = found_expression.expect("Should find our expression");
    let program = &package.programs[prog_idx];
    let item = &program.items[item_idx];
    
    if let ItemKind::Expression(expr) = &item.kind {
        if let ExpressionKind::FunctionCall(func_call) = &expr.kind {
            if let FunctionPath::Qualified { module, name } = &func_call.path {
                assert_eq!(module.name, "UnaryMinus");
                assert_eq!(name.name, "minus");
                
                // This is the critical test - clause IDs should be present
                assert!(
                    func_call.universal_clause_ids.is_some(),
                    "UnaryMinus.minus should have universal_clause_ids after type checking"
                );
                
                let clause_ids = func_call.universal_clause_ids.as_ref().unwrap();
                assert!(
                    !clause_ids.is_empty(),
                    "universal_clause_ids should contain at least one clause ID"
                );
                
                println!("✅ UnaryMinus.minus has {} clause IDs", clause_ids.len());
            } else {
                panic!("Expected qualified function path for desugared unary operator");
            }
        } else {
            panic!("Expected function call after unary operator desugaring");
        }
    } else {
        panic!("Expected expression item");
    }
}

#[test]
fn test_complex_expression_clause_ids() {
    // Test that 1 + 2 * 3 gets proper clause IDs for both operations
    let mut package = Package::new("test_complex_expression".to_string());
    
    let source = "1 + 2 * 3";
    let program = parse_program(source).expect("Should parse successfully");
    package.add_program(program);
    
    // Type check the package
    let _compilation_result = typecheck_package(&mut package).expect("Should type check successfully");
    
    // Check that both desugared function calls have clause IDs
    let program = &package.programs[0];
    let item = &program.items[0];
    
    if let ItemKind::Expression(expr) = &item.kind {
        if let ExpressionKind::FunctionCall(outer_call) = &expr.kind {
            // Outer should be BinaryAddition.add
            if let FunctionPath::Qualified { module, name } = &outer_call.path {
                assert_eq!(module.name, "BinaryAddition");
                assert_eq!(name.name, "add");
                assert!(outer_call.universal_clause_ids.is_some(), "Outer addition should have clause IDs");
                
                // Check the right argument (should be BinaryMultiplication.multiply)
                if let outrun_parser::Argument::Named { expression, .. } = &outer_call.arguments[1] {
                    if let ExpressionKind::FunctionCall(inner_call) = &expression.kind {
                        if let FunctionPath::Qualified { module, name } = &inner_call.path {
                            assert_eq!(module.name, "BinaryMultiplication");
                            assert_eq!(name.name, "multiply");
                            assert!(inner_call.universal_clause_ids.is_some(), "Inner multiplication should have clause IDs");
                            
                            println!("✅ Both nested operations have clause IDs");
                        }
                    }
                }
            }
        }
    }
}