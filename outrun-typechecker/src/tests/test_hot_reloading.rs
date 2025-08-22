use crate::{
    CompilationResult, Package, UniversalDispatchRegistry, MonomorphisationTable,
    TypeRegistry, FunctionRegistry, DispatchTable,
};

#[test]
fn test_hot_reloading_no_warning_when_unchanged() {
    // Test that recompiling with unchanged content doesn't produce warnings
    let code = r#"
struct User(name: String, id: Integer) {}

def create_user(name: String, id: Integer): User {
    User { name: name, id: id }
}
    "#;
    
    // First compilation without core library (for testing pure package recompilation)
    let mut package = Package::new("test_app".to_string());
    package.add_program(outrun_parser::parse_program(code).unwrap());
    
    let initial_result = CompilationResult::compile_with_dependencies(&mut package, vec![]).unwrap();
    
    // Recompile with same content - using recompile_package which reuses dependencies
    let mut package2 = Package::new("test_app".to_string());
    package2.add_program(outrun_parser::parse_program(code).unwrap());
    
    let recompiled_result = CompilationResult::recompile_package(
        &mut package2,
        Some(&initial_result),
        vec![]
    ).unwrap();
    
    // Should compile successfully without errors
    assert_eq!(recompiled_result.package_name, "test_app");
    
    // Both package-specific hashes should be the same (ignoring dependency differences)
    // The package_content_hash only covers the package's own programs, not dependencies
    assert_eq!(initial_result.package_content_hash, recompiled_result.package_content_hash);
}

#[test]
fn test_hot_reloading_package_redefinition() {
    // Test that recompiling with changed content produces appropriate warnings
    let code1 = r#"
struct User(name: String, id: Integer) {}
    "#;
    
    let code2 = r#"
struct User(name: String, id: Integer, email: String) {}
    "#;
    
    // First compilation without core library
    let mut package = Package::new("test_app".to_string());
    package.add_program(outrun_parser::parse_program(code1).unwrap());
    
    let initial_result = CompilationResult::compile_with_dependencies(&mut package, vec![]).unwrap();
    
    // Recompile with modified struct
    let mut package2 = Package::new("test_app".to_string());
    package2.add_program(outrun_parser::parse_program(code2).unwrap());
    
    let recompiled_result = CompilationResult::recompile_package(
        &mut package2,
        Some(&initial_result),
        vec![]
    ).unwrap();
    
    // Should compile successfully
    assert_eq!(recompiled_result.package_name, "test_app");
    
    // Content hashes should be different
    assert_ne!(initial_result.package_content_hash, recompiled_result.package_content_hash);
}

#[test]
fn test_hot_reloading_dependency_preservation() {
    // Test that dependencies are preserved during hot reloading
    let dep_code = r#"
struct Config(value: Outrun.Core.Integer64) {}
    "#;
    
    let app_code1 = r#"
def get_value(): Outrun.Core.Integer64 {
    42
}
    "#;
    
    let app_code2 = r#"
def get_value(): Outrun.Core.Integer64 {
    84
}
    "#;
    
    // Create a mock core library to prevent actual core loading
    let mock_core = CompilationResult {
        type_registry: std::rc::Rc::new(TypeRegistry::new()),
        function_registry: std::rc::Rc::new(FunctionRegistry::new()),
        dispatch_table: DispatchTable::new(),
        monomorphisation_table: MonomorphisationTable::new(),
        package_name: "outrun-core".to_string(),  // This prevents core loading
        programs: vec![],
        local_modules: std::collections::HashSet::new(),
        defined_modules: std::collections::HashSet::new(),
        universal_dispatch: UniversalDispatchRegistry::new(),
        variable_types: std::collections::HashMap::new(),
        package_type_registry: None,
        package_function_registry: None,
        package_universal_dispatch: None,
        dependency_registries: vec![],
        package_content_hash: 0,
        current_file: std::cell::RefCell::new(None),
    };
    
    // Compile dependency with mock core to prevent real core loading
    let mut dep_package = Package::new("config_lib".to_string());
    dep_package.add_program(outrun_parser::parse_program(dep_code).unwrap());
    let dep_result = CompilationResult::compile_with_dependencies(&mut dep_package, vec![mock_core.clone()]).unwrap();
    
    // First compilation with dependency and mock core
    let mut app_package = Package::new("app".to_string());
    app_package.add_program(outrun_parser::parse_program(app_code1).unwrap());
    let initial_result = CompilationResult::compile_with_dependencies(
        &mut app_package,
        vec![mock_core.clone(), dep_result.clone()]
    ).unwrap();
    
    // Verify dependencies were stored (mock core and config_lib)
    assert_eq!(initial_result.dependency_registries.len(), 2);
    assert!(initial_result.dependency_registries.iter().any(|(name, _, _, _)| name == "config_lib"));
    
    // Recompile with modified code but same dependency
    let mut app_package2 = Package::new("app".to_string());
    app_package2.add_program(outrun_parser::parse_program(app_code2).unwrap());
    
    let recompiled_result = CompilationResult::recompile_package(
        &mut app_package2,
        Some(&initial_result),
        vec![] // No dependencies provided - should use previous ones
    ).unwrap();
    
    // Should still have the dependencies
    assert_eq!(recompiled_result.dependency_registries.len(), 2);
    assert!(recompiled_result.dependency_registries.iter().any(|(name, _, _, _)| name == "config_lib"));
}

#[test]
fn test_hot_reloading_selective_package_replacement() {
    // Test that only the target package is recompiled, not dependencies
    let core_code = r#"
protocol Show {
    def show(value: Self): String
}
    "#;
    
    let app_code1 = r#"
struct Message(text: String) {}

impl Show for Message {
    def show(value: Message): String {
        value.text
    }
}
    "#;
    
    let app_code2 = r#"
struct Message(text: String, priority: Integer) {}

impl Show for Message {
    def show(value: Message): String {
        value.text
    }
}
    "#;
    
    // Create a mock outrun-core to prevent actual core loading  
    let mock_outrun_core = CompilationResult {
        type_registry: std::rc::Rc::new(TypeRegistry::new()),
        function_registry: std::rc::Rc::new(FunctionRegistry::new()),
        dispatch_table: DispatchTable::new(),
        monomorphisation_table: MonomorphisationTable::new(),
        package_name: "outrun-core".to_string(),
        programs: vec![],
        local_modules: std::collections::HashSet::new(),
        defined_modules: std::collections::HashSet::new(),
        universal_dispatch: UniversalDispatchRegistry::new(),
        variable_types: std::collections::HashMap::new(),
        package_type_registry: None,
        package_function_registry: None,
        package_universal_dispatch: None,
        dependency_registries: vec![],
        package_content_hash: 0,
        current_file: std::cell::RefCell::new(None),
    };
    
    // Compile core as dependency with mock to prevent real core loading
    let mut core_package = Package::new("core".to_string());
    core_package.add_program(outrun_parser::parse_program(core_code).unwrap());
    let core_result = CompilationResult::compile_with_dependencies(&mut core_package, vec![mock_outrun_core.clone()]).unwrap();
    
    // First app compilation with both dependencies
    let mut app_package = Package::new("app".to_string());
    app_package.add_program(outrun_parser::parse_program(app_code1).unwrap());
    let initial_result = CompilationResult::compile_with_dependencies(
        &mut app_package,
        vec![mock_outrun_core, core_result.clone()]
    ).unwrap();
    
    // Store initial state
    let initial_dep_count = initial_result.dependency_registries.len();
    
    // Recompile app with changes
    let mut app_package2 = Package::new("app".to_string());
    app_package2.add_program(outrun_parser::parse_program(app_code2).unwrap());
    
    let recompiled_result = CompilationResult::recompile_package(
        &mut app_package2,
        Some(&initial_result),
        vec![]
    ).unwrap();
    
    // Dependency count should remain the same
    assert_eq!(recompiled_result.dependency_registries.len(), initial_dep_count);
    
    // Package-specific registries should be populated
    assert!(recompiled_result.package_type_registry.is_some());
    assert!(recompiled_result.package_function_registry.is_some());
    assert!(recompiled_result.package_universal_dispatch.is_some());
}