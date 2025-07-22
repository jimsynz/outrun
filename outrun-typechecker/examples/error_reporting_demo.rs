//! Comprehensive error reporting demonstration
//!
//! This example showcases the enhanced error reporting capabilities of Outrun typechecker v3.
//!
//! Run with: `cargo run --example error_reporting_demo`

use outrun_typechecker::{
    error::{
        ErrorContext, InferenceError, UnificationError, DispatchError, 
        TypecheckError, CompilerError
    },
    types::Type,
    TypeInferenceEngine,
};
use outrun_parser::{parse_program, Span};
use miette::{Diagnostic, NamedSource, Report};

fn main() {
    println!("🔍 Outrun Typechecker v3 - Enhanced Error Reporting Demo\n");
    
    demo_undefined_variable_suggestions();
    demo_type_mismatch_with_suggestions();
    demo_missing_protocol_implementation();
    demo_collection_type_errors();
    demo_function_call_errors();
    demo_empty_collection_annotation();
    demo_context_aware_suggestions();
    
    println!("✨ Error reporting demo complete!");
}

fn demo_undefined_variable_suggestions() {
    println!("📍 Demo 1: Undefined Variable with Smart Suggestions");
    println!("═══════════════════════════════════════════════════");
    
    // Create error context with available variables
    let mut context = ErrorContext::new();
    context.available_variables = vec![
        "username".to_string(),
        "user_email".to_string(),
        "user_id".to_string(),
        "password".to_string(),
    ];
    
    // Simulate typo in variable name
    let error = InferenceError::undefined_variable_with_suggestions(
        "usename".to_string(),  // Typo: missing 'r'
        Some(Span::new(45, 52)),
        &context,
    );
    
    let error_source = r#"
def login(usename: String, password: String): Result<User, LoginError> {
    // Implementation...
}"#;
    
    let named_source = NamedSource::new("login.outrun", error_source);
    let report = Report::new(CompilerError::Typecheck(TypecheckError::InferenceError(error)))
        .with_source_code(named_source);
        
    println!("{:?}", report);
    println!();
}

fn demo_type_mismatch_with_suggestions() {
    println!("📍 Demo 2: Type Mismatch with Conversion Suggestions");
    println!("══════════════════════════════════════════════════");
    
    let expected = Type::concrete("String");
    let found = Type::concrete("Integer64");
    
    let error = UnificationError::type_mismatch_with_suggestions(
        expected,
        found,
        Some(Span::new(25, 30)),
        "function parameter",
    );
    
    let error_source = r#"
def format_message(text: String, count: Integer64): String {
    text + count  // Type mismatch here
}
"#;
    
    let named_source = NamedSource::new("format.outrun", error_source);
    let report = Report::new(CompilerError::Typecheck(TypecheckError::UnificationError(error)))
        .with_source_code(named_source);
        
    println!("{:?}", report);
    println!();
}

fn demo_missing_protocol_implementation() {
    println!("📍 Demo 3: Missing Protocol Implementation with Guidance");
    println!("═══════════════════════════════════════════════════════");
    
    let error = DispatchError::no_implementation_with_suggestions(
        "BinaryAddition".to_string(),
        "CustomPoint".to_string(),
        Some(Span::new(15, 16)),
        vec!["BinaryAddition for Integer64".to_string(), "BinaryAddition for Float64".to_string()],
    );
    
    let error_source = r#"
struct CustomPoint { x: Float64, y: Float64 }
let p1 = CustomPoint { x: 1.0, y: 2.0 }
let p2 = CustomPoint { x: 3.0, y: 4.0 }
let sum = p1 + p2  // Missing BinaryAddition implementation
"#;
    
    let named_source = NamedSource::new("point.outrun", error_source);
    let report = Report::new(CompilerError::Typecheck(TypecheckError::DispatchError(error)))
        .with_source_code(named_source);
        
    println!("{:?}", report);
    println!();
}

fn demo_collection_type_errors() {
    println!("📍 Demo 4: Collection Type Mismatch with Element Context");
    println!("═════════════════════════════════════════════════════");
    
    let expected_type = Type::concrete("Integer64");
    let found_type = Type::concrete("String");
    
    let error = InferenceError::collection_type_mismatch(
        "All list elements must have the same type".to_string(),
        "List".to_string(),
        Some(expected_type),
        Some(found_type),
        Some(Span::new(12, 28)),  // Whole list
        Some(Span::new(13, 14)),  // First element
        Some(Span::new(19, 26)),  // Problematic element
    );
    
    let error_source = r#"
let mixed = [1, 2, "three", 4]  // Type mismatch in list
"#;
    
    let named_source = NamedSource::new("collections.outrun", error_source);
    let report = Report::new(CompilerError::Typecheck(TypecheckError::InferenceError(error)))
        .with_source_code(named_source);
        
    println!("{:?}", report);
    println!();
}

fn demo_function_call_errors() {
    println!("📍 Demo 5: Function Call Error with Signature Information");
    println!("═════════════════════════════════════════════════════════");
    
    let error = InferenceError::function_call_error_with_suggestions(
        "Parameter type mismatch".to_string(),
        Some("calculate_area".to_string()),
        Some("calculate_area(width: Float64, height: Float64) -> Float64".to_string()),
        Some("calculate_area(width: String, height: Float64)".to_string()),
        Some(Span::new(20, 45)),
        vec![
            "Convert String to Float64: String.to_float(width)".to_string(),
            "Check function signature and parameter types".to_string(),
        ],
    );
    
    let error_source = r#"
def calculate_area(width: Float64, height: Float64): Float64 {
    width * height
}

let area = calculate_area(width: "10.5", height: 20.0)  // Wrong type
"#;
    
    let named_source = NamedSource::new("geometry.outrun", error_source);
    let report = Report::new(CompilerError::Typecheck(TypecheckError::InferenceError(error)))
        .with_source_code(named_source);
        
    println!("{:?}", report);
    println!();
}

fn demo_empty_collection_annotation() {
    println!("📍 Demo 6: Empty Collection Needs Type Annotation");
    println!("═════════════════════════════════════════════════");
    
    let error = InferenceError::empty_collection_needs_annotation(
        "List".to_string(),
        Some(Span::new(18, 20)),
    );
    
    let error_source = r#"
let empty_list = []  // Cannot infer element type
"#;
    
    let named_source = NamedSource::new("empty.outrun", error_source);
    let report = Report::new(CompilerError::Typecheck(TypecheckError::InferenceError(error)))
        .with_source_code(named_source);
        
    println!("{:?}", report);
    println!();
}

fn demo_context_aware_suggestions() {
    println!("📍 Demo 7: Context-Aware Variable Suggestions");
    println!("═════════════════════════════════════════════");
    
    // Simulate a realistic development context
    let mut context = ErrorContext::new();
    context.available_variables = vec![
        "database_connection".to_string(),
        "user_session".to_string(),
        "request_headers".to_string(),
        "response_body".to_string(),
        "error_handler".to_string(),
    ];
    context.available_types = vec![
        "DatabaseConnection".to_string(),
        "UserSession".to_string(),
        "HttpRequest".to_string(),
        "HttpResponse".to_string(),
        "ApiError".to_string(),
    ];
    context.current_function = Some("handle_api_request".to_string());
    context.current_module = Some("Api.Handlers".to_string());
    
    // Test variable suggestion
    let var_error = InferenceError::undefined_variable_with_suggestions(
        "databse_connection".to_string(),  // Missing 'a'
        Some(Span::new(35, 52)),
        &context,
    );
    
    // Test type suggestion  
    let type_error = InferenceError::undefined_type_with_suggestions(
        "HttpRespons".to_string(),  // Missing 'e'
        Some(Span::new(15, 26)),
        &context,
    );
    
    let error_source = r#"
def handle_api_request(request: HttpRequest): HttpRespons {
    let connection = databse_connection
    // Process request...
}
"#;
    
    let named_source = NamedSource::new("api_handlers.outrun", error_source);
    
    println!("Variable error:");
    let report1 = Report::new(CompilerError::Typecheck(TypecheckError::InferenceError(var_error)))
        .with_source_code(named_source.clone());
    println!("{:?}", report1);
    
    println!("\nType error:");
    let report2 = Report::new(CompilerError::Typecheck(TypecheckError::InferenceError(type_error)))
        .with_source_code(named_source);
    println!("{:?}", report2);
    println!();
}