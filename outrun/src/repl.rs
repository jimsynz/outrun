//! REPL (Read-Eval-Print Loop) implementation for the Outrun interpreter
//!
//! This module provides an interactive REPL that integrates the parser, typechecker,
//! and interpreter to provide a complete Outrun evaluation experience. Features:
//! - Interactive expression evaluation with persistent variable bindings
//! - Integration with parser → typechecker → interpreter pipeline
//! - REPL commands for debugging and inspection
//! - Error recovery and beautiful error reporting with miette
//! - History support and line editing with rustyline

use miette::Diagnostic;
use outrun_interpreter::{ExpressionEvaluator, InterpreterContext, Value};
use outrun_parser::{ParseError, parse_expression, parse_program};
use outrun_typechecker::{
    CompilerEnvironment,
    checker::{TypedExpression, TypedItemKind},
    error::TypeError,
    unification::StructuredType,
};
use rustyline::{DefaultEditor, error::ReadlineError};
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur in the REPL
#[derive(Debug, Error, Diagnostic)]
pub enum ReplError {
    #[error("Parse error: {source}")]
    Parse {
        #[from]
        source: ParseError,
    },

    #[error("Type error: {source}")]
    TypeCheck {
        #[from]
        source: TypeError,
    },

    #[error("Runtime error: {source}")]
    Runtime {
        #[from]
        source: outrun_interpreter::EvaluationError,
    },

    #[error("IO error: {source}")]
    Io {
        #[from]
        source: std::io::Error,
    },

    #[error("Readline error: {source}")]
    Readline {
        #[from]
        source: ReadlineError,
    },

    #[error("REPL command error: {message}")]
    Command { message: String },

    #[error("Internal REPL error: {message}")]
    Internal { message: String },
}

/// REPL session that maintains state across evaluations
pub struct ReplSession {
    /// Interactive line editor with history
    editor: DefaultEditor,

    /// Interpreter context for persistent variable bindings
    interpreter_context: InterpreterContext,

    /// Compiler environment for type checking and compilation
    compiler_environment: outrun_typechecker::CompilerEnvironment,

    /// Variable bindings visible to the user
    user_variables: HashMap<String, (Value, StructuredType)>, // name -> (value, structured_type)

    /// REPL configuration
    config: ReplConfig,

    /// Session statistics
    stats: ReplStats,

    /// Shared compilation context for efficient memory usage
    compilation_session: outrun_typechecker::shared_context::CompilationSessionContext,
}

/// REPL configuration options
#[derive(Debug, Clone)]
pub struct ReplConfig {
    /// Show type information with results
    pub show_types: bool,

    /// Show detailed error information
    pub verbose_errors: bool,

    /// Maximum number of items to display in collections
    pub max_display_items: usize,

    /// Prompt string for input
    pub prompt: String,

    /// Enable history persistence
    pub persist_history: bool,

    /// History file path
    pub history_file: Option<String>,
}

impl Default for ReplConfig {
    fn default() -> Self {
        Self {
            show_types: true,
            verbose_errors: false,
            max_display_items: 20,
            prompt: "outrun> ".to_string(),
            persist_history: true,
            history_file: Some(".outrun_history".to_string()),
        }
    }
}

/// REPL session statistics
#[derive(Debug, Clone, Default)]
pub struct ReplStats {
    /// Number of expressions evaluated
    pub expressions_evaluated: usize,

    /// Number of variables bound
    pub variables_bound: usize,

    /// Number of errors encountered
    pub errors_encountered: usize,

    /// Total REPL commands executed
    pub commands_executed: usize,
}

/// Result of evaluating a line in the REPL
#[derive(Debug)]
pub enum ReplResult {
    /// Successfully evaluated expression
    Value {
        value: Value,
        type_display: String,
        bound_variables: Vec<String>,
    },

    /// Executed a REPL command
    Command { message: String },

    /// Let binding result
    LetBinding { variables: String, value: Value },

    /// Empty line or comment
    Empty,

    /// Exit request
    Exit,
}

impl ReplSession {
    /// Create a new REPL session with default configuration
    pub fn new() -> Result<Self, ReplError> {
        Self::with_config(ReplConfig::default())
    }

    /// Create a new REPL session with custom configuration
    pub fn with_config(config: ReplConfig) -> Result<Self, ReplError> {
        let mut editor = DefaultEditor::new()?;

        // Load history if configured
        if config.persist_history {
            if let Some(ref history_file) = config.history_file {
                let _ = editor.load_history(history_file); // Ignore errors for new sessions
            }
        }

        // Initialize CompilerEnvironment with core library directly by compiling it
        // This matches the working typecheck command approach
        let mut compiler_environment = CompilerEnvironment::new();
        let _core_result = outrun_typechecker::core_library::compile_core_library_with_environment(
            &mut compiler_environment,
        );

        // Create shared compilation context from core library (efficient, no duplication)
        let shared_context = outrun_typechecker::shared_context::SharedCompilationContextFactory::create_from_core_library();
        let compilation_session = shared_context.create_session_context();

        // Use the UnificationContext from the core compilation to ensure TypeInterner consistency
        let unification_context = shared_context.core_compilation().type_context.clone();

        // Create interpreter context with the CompilerEnvironment
        let interpreter_context = InterpreterContext::new(
            unification_context,
            compiler_environment.clone(),
            Some(100), // Reasonable stack limit for REPL
        );

        Ok(Self {
            editor,
            interpreter_context,
            compiler_environment,
            user_variables: HashMap::new(),
            config,
            stats: ReplStats::default(),
            compilation_session,
        })
    }

    /// Start the REPL main loop
    pub fn run(&mut self) -> Result<(), ReplError> {
        self.print_welcome();

        while let Some(line) = self.read_line()? {
            match self.evaluate_line(&line) {
                Ok(ReplResult::Exit) => break,
                Ok(result) => self.display_result(result),
                Err(error) => {
                    self.stats.errors_encountered += 1;
                    self.display_error(error, Some(&line));
                }
            }
        }

        self.print_goodbye();
        self.save_history()?;
        Ok(())
    }

    /// Read a line from the user with prompt, supporting multi-line input
    fn read_line(&mut self) -> Result<Option<String>, ReplError> {
        let mut complete_input = String::new();
        let mut line_count = 0;

        loop {
            let prompt = if line_count == 0 {
                &self.config.prompt
            } else {
                "... "
            };

            match self.editor.readline(prompt) {
                Ok(line) => {
                    line_count += 1;

                    if line_count == 1 && line.trim().is_empty() {
                        // Empty first line - just return empty
                        return Ok(Some(String::new()));
                    }

                    if !complete_input.is_empty() {
                        complete_input.push('\n');
                    }
                    complete_input.push_str(&line);

                    // Check if input is complete (balanced brackets)
                    if self.is_input_complete(&complete_input) {
                        // Add the complete multi-line input to history as a single entry
                        if !complete_input.trim().is_empty() {
                            self.editor.add_history_entry(complete_input.clone())?;
                        }
                        return Ok(Some(complete_input));
                    }

                    // Continue reading for more lines
                }
                Err(ReadlineError::Interrupted) => {
                    if line_count == 0 {
                        println!("^C");
                        return Ok(Some(String::new())); // Continue REPL
                    } else {
                        println!("^C");
                        // Cancel multi-line input and start over
                        return Ok(Some(String::new()));
                    }
                }
                Err(ReadlineError::Eof) => return Ok(None), // Exit REPL
                Err(err) => return Err(ReplError::Readline { source: err }),
            }
        }
    }

    /// Check if the input has balanced brackets and is likely complete
    fn is_input_complete(&self, input: &str) -> bool {
        let mut brace_count = 0;
        let mut bracket_count = 0;
        let mut paren_count = 0;
        let mut in_string = false;
        let mut in_char = false;
        let mut escaped = false;

        let chars: Vec<char> = input.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            let ch = chars[i];

            if escaped {
                escaped = false;
                i += 1;
                continue;
            }

            match ch {
                '\\' if in_string || in_char => {
                    escaped = true;
                }
                '"' if !in_char => {
                    in_string = !in_string;
                }
                '\'' if !in_string => {
                    in_char = !in_char;
                }
                '{' if !in_string && !in_char => {
                    brace_count += 1;
                }
                '}' if !in_string && !in_char => {
                    brace_count -= 1;
                }
                '[' if !in_string && !in_char => {
                    bracket_count += 1;
                }
                ']' if !in_string && !in_char => {
                    bracket_count -= 1;
                }
                '(' if !in_string && !in_char => {
                    paren_count += 1;
                }
                ')' if !in_string && !in_char => {
                    paren_count -= 1;
                }
                '#' if !in_string && !in_char => {
                    // Skip rest of line (comment)
                    while i < chars.len() && chars[i] != '\n' {
                        i += 1;
                    }
                    continue;
                }
                _ => {}
            }

            i += 1;
        }

        // Input is complete if all brackets are balanced and we're not in a string
        brace_count == 0 && bracket_count == 0 && paren_count == 0 && !in_string && !in_char
    }

    /// Evaluate a line of input
    pub fn evaluate_line(&mut self, line: &str) -> Result<ReplResult, ReplError> {
        let trimmed = line.trim();

        // Handle empty lines and comments
        if trimmed.is_empty() || trimmed.starts_with("//") {
            return Ok(ReplResult::Empty);
        }

        // Handle REPL commands
        if trimmed.starts_with('/') {
            return self.execute_command(trimmed);
        }

        // Try to parse as expression first, fallback to parsing as a program for statements
        let parsed_expr = match parse_expression(trimmed) {
            Ok(expr) => expr,
            Err(_) => {
                // If expression parsing fails, try parsing as a program to handle let bindings
                let program = parse_program(trimmed)?;

                // Extract the first item from the program
                if let Some(first_item) = program.items.first() {
                    match &first_item.kind {
                        outrun_parser::ItemKind::Expression(expr) => expr.clone(),
                        outrun_parser::ItemKind::LetBinding(let_binding) => {
                            // Handle let bindings by type checking and evaluating them
                            return self.evaluate_let_binding(let_binding, line);
                        }
                        _ => {
                            return Err(ReplError::Internal {
                                message: format!(
                                    "Unsupported item type in REPL: {:?}",
                                    first_item.kind
                                ),
                            });
                        }
                    }
                } else {
                    return Err(ReplError::Internal {
                        message: "No items found in parsed program".to_string(),
                    });
                }
            }
        };

        // Create a simple program with just the expression
        let temp_program = outrun_parser::Program {
            items: vec![outrun_parser::Item {
                kind: outrun_parser::ItemKind::Expression(parsed_expr.clone()),
                span: parsed_expr.span,
            }],
            span: parsed_expr.span,
            debug_info: outrun_parser::DebugInfo {
                source_file: Some("<repl>".to_string()),
                comments: Vec::new(),
            },
        };

        // Collect external variables for the type checker
        let external_variables: HashMap<String, StructuredType> = self
            .user_variables
            .iter()
            .map(|(name, (_, structured_type))| (name.clone(), structured_type.clone()))
            .collect();

        // Create a program collection with just the user's expression
        let mut user_collection = outrun_typechecker::ProgramCollection::new();
        user_collection.add_program(
            "<repl>".to_string(),
            temp_program.clone(),
            trimmed.to_string(),
        );

        // Use the persistent compiler environment which already has core library loaded
        // This eliminates the problematic fresh CompilerEnvironment creation
        let user_compilation = self
            .compiler_environment
            .compile_collection_with_external_variables(user_collection, external_variables)
            .map_err(|errors| {
                // Take the first error for now - in the future we could display all errors
                if let Some(first_error) = errors.into_iter().next() {
                    ReplError::TypeCheck {
                        source: first_error,
                    }
                } else {
                    ReplError::Internal {
                        message: "Type checker returned empty error list".to_string(),
                    }
                }
            })?;

        // Update our compilation session to include the new code (efficient, no cloning)
        self.compilation_session
            .set_user_compilation(user_compilation.clone());

        // Extract the typed program from the compilation result
        let typed_program = user_compilation
            .typed_programs
            .get("<repl>")
            .ok_or_else(|| ReplError::Internal {
                message: "No typed program found for REPL input".to_string(),
            })?;

        // Extract the typed expression (should be the only item in the program)
        let typed_expr = if let Some(typed_item) = typed_program.items.first() {
            if let TypedItemKind::Expression(expr) = &typed_item.kind {
                expr.as_ref().clone()
            } else {
                return Err(ReplError::Internal {
                    message: "Expected typed expression from type checker".to_string(),
                });
            }
        } else {
            return Err(ReplError::Internal {
                message: "Type checker returned empty program".to_string(),
            });
        };

        // The interpreter context automatically uses the updated CompilerEnvironment from the type context
        // No need to manually update function registry - it's handled through CompilerEnvironment

        // Create evaluator with the persistent CompilerEnvironment
        let dispatch_context = outrun_typechecker::context::FunctionDispatchContext::new(Some(
            self.compiler_environment.clone(),
        ));
        let mut evaluator = ExpressionEvaluator::from_dispatch_context(dispatch_context);

        // Evaluate the expression with the updated main context
        let value = evaluator.evaluate(&mut self.interpreter_context, &typed_expr)?;

        // Update statistics
        self.stats.expressions_evaluated += 1;

        // Extract type information for display
        let type_display = self.format_type_for_display(&typed_expr);

        // Handle variable bindings from let expressions or patterns
        let bound_variables = self.extract_and_bind_variables(&typed_expr, &value)?;

        if !bound_variables.is_empty() {
            self.stats.variables_bound += bound_variables.len();
        }

        Ok(ReplResult::Value {
            value,
            type_display,
            bound_variables,
        })
    }

    /// Execute a REPL command
    fn execute_command(&mut self, command: &str) -> Result<ReplResult, ReplError> {
        self.stats.commands_executed += 1;

        let parts: Vec<&str> = command.split_whitespace().collect();
        if parts.is_empty() {
            return Ok(ReplResult::Empty);
        }

        match parts[0] {
            "/help" | "/h" => Ok(ReplResult::Command {
                message: self.help_message(),
            }),

            "/vars" | "/variables" => Ok(ReplResult::Command {
                message: self.format_variables(),
            }),

            "/clear" => {
                self.user_variables.clear();

                // Reset compilation session back to just the core library (efficient, no cloning)
                self.compilation_session.clear_user_compilation();

                // Recreate interpreter context with fresh UnificationContext from core library
                let core_compilation = self.compilation_session.core_context().core_compilation();
                self.interpreter_context = InterpreterContext::new(
                    core_compilation.type_context.clone(),
                    self.compiler_environment.clone(),
                    Some(100),
                );
                Ok(ReplResult::Command {
                    message: "Variables and compilation state cleared".to_string(),
                })
            }

            "/stats" => Ok(ReplResult::Command {
                message: self.format_stats(),
            }),

            "/config" => Ok(ReplResult::Command {
                message: self.format_config(),
            }),

            "/quit" | "/q" | "/exit" => Ok(ReplResult::Exit),

            "/types" => match parts.get(1) {
                Some(&"on") => {
                    self.config.show_types = true;
                    Ok(ReplResult::Command {
                        message: "Type display enabled".to_string(),
                    })
                }
                Some(&"off") => {
                    self.config.show_types = false;
                    Ok(ReplResult::Command {
                        message: "Type display disabled".to_string(),
                    })
                }
                _ => Ok(ReplResult::Command {
                    message: format!(
                        "Type display is {}",
                        if self.config.show_types { "on" } else { "off" }
                    ),
                }),
            },

            unknown => Err(ReplError::Command {
                message: format!("Unknown command: {unknown}. Type /help for available commands."),
            }),
        }
    }

    /// Extract and bind variables from expressions (for let expressions and patterns)
    fn extract_and_bind_variables(
        &mut self,
        _typed_expr: &TypedExpression,
        _value: &Value,
    ) -> Result<Vec<String>, ReplError> {
        // TODO: Implement variable binding extraction for let expressions
        // For now, return empty vector
        Ok(vec![])
    }

    /// Extract variable names from a pattern
    fn extract_pattern_variables(
        &self,
        pattern: &outrun_parser::Pattern,
        _value: &Value,
    ) -> Result<Vec<String>, ReplError> {
        // For now, only support simple identifier patterns
        // TODO: Add support for complex patterns (struct destructuring, etc.)
        match pattern {
            outrun_parser::Pattern::Identifier(identifier) => Ok(vec![identifier.name.clone()]),
            _ => Err(ReplError::Internal {
                message: format!("Complex patterns not yet supported in REPL: {pattern:?}"),
            }),
        }
    }

    /// Evaluate a let binding statement
    fn evaluate_let_binding(
        &mut self,
        let_binding: &outrun_parser::LetBinding,
        _input: &str,
    ) -> Result<ReplResult, ReplError> {
        // Create a temporary program with the let binding for type checking
        let temp_program = outrun_parser::Program {
            items: vec![outrun_parser::Item {
                kind: outrun_parser::ItemKind::LetBinding(let_binding.clone()),
                span: let_binding.span,
            }],
            span: let_binding.span,
            debug_info: outrun_parser::DebugInfo {
                source_file: Some("<repl>".to_string()),
                comments: Vec::new(),
            },
        };

        // Type check the let binding using the persistent compiler environment
        let mut collection = outrun_typechecker::ProgramCollection::new();
        collection.add_program(
            "<repl-let>".to_string(),
            temp_program.clone(),
            format!("{temp_program}"),
        );

        let external_variables: HashMap<String, StructuredType> = self
            .user_variables
            .iter()
            .map(|(name, (_, structured_type))| (name.clone(), structured_type.clone()))
            .collect();

        let compilation_result = self
            .compiler_environment
            .compile_collection_with_external_variables(collection, external_variables)
            .map_err(|errors| {
                if let Some(first_error) = errors.into_iter().next() {
                    ReplError::TypeCheck {
                        source: first_error,
                    }
                } else {
                    ReplError::Internal {
                        message: "Type checker returned empty error list".to_string(),
                    }
                }
            })?;

        // Extract the typed program from the compilation result
        let typed_program =
            if let Some(typed_program) = compilation_result.typed_programs.get("<repl-let>") {
                typed_program.clone()
            } else {
                return Err(ReplError::Internal {
                    message: "Failed to extract typed program from compilation result".to_string(),
                });
            };

        // Extract the typed let binding
        let typed_let_binding = if let Some(first_item) = typed_program.items.first() {
            if let TypedItemKind::LetBinding(let_binding) = &first_item.kind {
                let_binding.as_ref()
            } else {
                return Err(ReplError::Internal {
                    message: "Expected typed let binding from type checker".to_string(),
                });
            }
        } else {
            return Err(ReplError::Internal {
                message: "Type checker returned empty program".to_string(),
            });
        };

        // Create evaluator with the persistent CompilerEnvironment
        let dispatch_context = outrun_typechecker::context::FunctionDispatchContext::new(Some(
            self.compiler_environment.clone(),
        ));
        let mut evaluator = ExpressionEvaluator::from_dispatch_context(dispatch_context);

        // Evaluate the RHS expression
        let value =
            evaluator.evaluate(&mut self.interpreter_context, &typed_let_binding.expression)?;

        // Extract variable name(s) from the pattern
        let variable_names = self.extract_pattern_variables(&let_binding.pattern, &value)?;

        // Bind variables in the interpreter context
        for variable_name in &variable_names {
            // In REPL mode, allow rebinding variables by updating if they already exist
            match self
                .interpreter_context
                .define_variable(variable_name.clone(), value.clone())
            {
                Ok(()) => {} // Successfully defined new variable
                Err(_) => {
                    // Variable might already exist, try updating it
                    self.interpreter_context
                        .update_variable(variable_name, value.clone())
                        .map_err(|_| ReplError::Internal {
                            message: format!("Failed to bind variable '{variable_name}'"),
                        })?;
                }
            }

            // Also track in user_variables for :vars command
            // Get the structured type from the typed expression
            let structured_type = typed_let_binding
                .expression
                .structured_type
                .clone()
                .unwrap_or_else(|| {
                    // If no structured type is available, create a placeholder using the compiler environment's interner
                    // This should not happen in a properly type-checked expression but provides a fallback
                    let placeholder_type_id =
                        self.compiler_environment.intern_type_name("UnknownType");
                    StructuredType::Simple(placeholder_type_id)
                });
            self.user_variables
                .insert(variable_name.clone(), (value.clone(), structured_type));
        }

        // Update statistics
        self.stats.variables_bound += variable_names.len();

        // Display result
        let variable_list = variable_names.join(", ");
        Ok(ReplResult::LetBinding {
            variables: variable_list,
            value: value.clone(),
        })
    }

    /// Format type information for display
    fn format_type_for_display(&self, typed_expr: &TypedExpression) -> String {
        // Method 1: Check structured_type field first
        if let Some(ref structured_type) = typed_expr.structured_type {
            return structured_type.to_string_representation();
        }

        // Method 2: Check debug info for inferred types
        if let Some(ref debug_info) = typed_expr.debug_info {
            let span_key = (typed_expr.span.start, typed_expr.span.end);
            if let Some(inferred_type) = debug_info.inferred_types.get(&span_key) {
                return inferred_type.to_string_representation();
            }
        }

        // Method 3: Fallback to expression kind analysis
        match &typed_expr.kind {
            outrun_typechecker::checker::TypedExpressionKind::Integer(_) => "Integer".to_string(),
            outrun_typechecker::checker::TypedExpressionKind::Float(_) => "Float".to_string(),
            outrun_typechecker::checker::TypedExpressionKind::String(_) => "String".to_string(),
            outrun_typechecker::checker::TypedExpressionKind::Boolean(_) => "Boolean".to_string(),
            outrun_typechecker::checker::TypedExpressionKind::Atom(_) => "Atom".to_string(),
            outrun_typechecker::checker::TypedExpressionKind::List { .. } => "List".to_string(),
            outrun_typechecker::checker::TypedExpressionKind::Map { .. } => "Map".to_string(),
            outrun_typechecker::checker::TypedExpressionKind::Tuple { .. } => "Tuple".to_string(),
            _ => "Unknown".to_string(),
        }
    }

    /// Display the result of evaluation
    fn display_result(&self, result: ReplResult) {
        match result {
            ReplResult::Value {
                value,
                type_display,
                bound_variables,
            } => {
                // Display the value
                if self.config.show_types {
                    println!("{value}: {type_display}");
                } else {
                    println!("{value}");
                }

                // Display bound variables if any
                if !bound_variables.is_empty() {
                    for var in bound_variables {
                        if let Some((val, structured_type)) = self.user_variables.get(&var) {
                            if self.config.show_types {
                                let type_display = structured_type.to_string_representation();
                                println!("{var}: {type_display} = {val}");
                            } else {
                                println!("{var} = {val}");
                            }
                        }
                    }
                }
            }

            ReplResult::Command { message } => {
                println!("{message}");
            }

            ReplResult::LetBinding { variables, value } => {
                // Display the let binding result
                if self.config.show_types {
                    // Get type information for the first variable (they all have the same value)
                    let first_var = variables.split(',').next().unwrap_or("").trim();
                    if let Some((_, structured_type)) = self.user_variables.get(first_var) {
                        let type_display = structured_type.to_string_representation();
                        println!("{variables}: {type_display} = {value}");
                    } else {
                        println!("{variables} = {value}");
                    }
                } else {
                    println!("{variables} = {value}");
                }
            }

            ReplResult::Empty => {} // No output for empty lines

            ReplResult::Exit => {} // Handled by caller
        }
    }

    /// Display an error with appropriate formatting
    fn display_error(&self, error: ReplError, source_code: Option<&str>) {
        match error {
            ReplError::TypeCheck { source } => {
                // TypeError implements Clone + Diagnostic, so we can create proper miette reports
                if let Some(source_code) = source_code {
                    let named_source = miette::NamedSource::new("<repl>", source_code.to_string());
                    let report = miette::Report::new(source).with_source_code(named_source);
                    eprintln!("{report:?}");
                } else {
                    // Without source context, create a simple report
                    let report = miette::Report::new(source);
                    eprintln!("{report:?}");
                }
            }
            ReplError::Parse { source } => {
                // ParseError implements Diagnostic and contains its own source code
                // from #[source_code] annotation, so we can create a report directly
                let report = miette::Report::new(source);
                eprintln!("{report:?}");
            }
            ReplError::Internal { .. } | ReplError::Command { .. } => {
                // Create a miette report for internal/command errors
                if let Some(source_code) = source_code {
                    let named_source = miette::NamedSource::new("<repl>", source_code.to_string());
                    let report = miette::Report::new(error).with_source_code(named_source);
                    eprintln!("{report:?}");
                } else {
                    let report = miette::Report::new(error);
                    eprintln!("{report:?}");
                }
            }
            error => {
                // For other errors, use regular formatting
                if self.config.verbose_errors {
                    eprintln!("Error: {error:?}");
                } else {
                    eprintln!("Error: {error}");
                }
            }
        }
    }

    /// Print welcome message
    fn print_welcome(&self) {
        println!("🌆 Outrun REPL v0.1.0 🌃");
        println!("Type /help for commands, /quit to exit");
        println!();
    }

    /// Print goodbye message
    fn print_goodbye(&self) {
        println!("Goodbye! 👋");
    }

    /// Save history to file
    fn save_history(&mut self) -> Result<(), ReplError> {
        if self.config.persist_history {
            if let Some(ref history_file) = self.config.history_file {
                self.editor.save_history(history_file)?;
            }
        }
        Ok(())
    }

    /// Get help message
    fn help_message(&self) -> String {
        r#"Outrun REPL Commands:
  /help, /h           Show this help message
  /vars, /variables   List all variables with their values and types
  /clear              Clear all variables and reset the session
  /stats              Show session statistics
  /config             Show current configuration
  /types [on|off]     Toggle type display
  /quit, /q, /exit    Exit the REPL

Examples:
  42                  # Evaluate an integer literal
  let x = 42          # Bind a variable
  List.head([1,2,3])  # Call a function
  [1, 2, 3]           # Create a list
  Option.some(42)     # Create an Option

Multi-line input:
  let list = [         # Type [ and press Enter
      1,               # Continuation prompt (...)
      2,               # appears for incomplete input
      3                # with balanced brackets
  ]                    # Completes when brackets close

Use Ctrl+C to interrupt, Ctrl+D to exit."#
            .to_string()
    }

    /// Format current variables for display
    fn format_variables(&self) -> String {
        if self.user_variables.is_empty() {
            "No variables defined".to_string()
        } else {
            let mut lines = vec!["Variables:".to_string()];
            for (name, (value, structured_type)) in &self.user_variables {
                if self.config.show_types {
                    let type_display = structured_type.to_string_representation();
                    lines.push(format!("  {name}: {type_display} = {value}"));
                } else {
                    lines.push(format!("  {name} = {value}"));
                }
            }
            lines.join("\n")
        }
    }

    /// Format session statistics
    fn format_stats(&self) -> String {
        format!(
            r#"Session Statistics:
  Expressions evaluated: {}
  Variables bound: {}
  Errors encountered: {}
  Commands executed: {}"#,
            self.stats.expressions_evaluated,
            self.stats.variables_bound,
            self.stats.errors_encountered,
            self.stats.commands_executed
        )
    }

    /// Format current configuration
    fn format_config(&self) -> String {
        format!(
            r#"REPL Configuration:
  Show types: {}
  Verbose errors: {}
  Max display items: {}
  Prompt: "{}"
  Persist history: {}
  History file: {}"#,
            self.config.show_types,
            self.config.verbose_errors,
            self.config.max_display_items,
            self.config.prompt,
            self.config.persist_history,
            self.config.history_file.as_deref().unwrap_or("<none>")
        )
    }
}

impl Default for ReplSession {
    fn default() -> Self {
        Self::new().expect("Failed to create default REPL session")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_repl() -> ReplSession {
        let config = ReplConfig {
            persist_history: false, // Don't save history in tests
            history_file: None,
            ..Default::default()
        };
        ReplSession::with_config(config).expect("Failed to create test REPL")
    }

    #[test]
    fn test_repl_creation() {
        let repl = create_test_repl();
        assert_eq!(repl.stats.expressions_evaluated, 0);
        assert_eq!(repl.stats.variables_bound, 0);
        assert_eq!(repl.stats.errors_encountered, 0);
        assert!(repl.user_variables.is_empty());
    }

    #[test]
    fn test_simple_expression_evaluation() {
        let mut repl = create_test_repl();

        let result = repl.evaluate_line("42").unwrap();
        if let ReplResult::Value { value, .. } = result {
            assert_eq!(value, Value::integer(42));
        } else {
            panic!("Expected value result");
        }

        assert_eq!(repl.stats.expressions_evaluated, 1);
    }

    #[test]
    fn test_empty_line_handling() {
        let mut repl = create_test_repl();

        let result = repl.evaluate_line("").unwrap();
        assert!(matches!(result, ReplResult::Empty));

        let result = repl.evaluate_line("   ").unwrap();
        assert!(matches!(result, ReplResult::Empty));

        let result = repl.evaluate_line("// comment").unwrap();
        assert!(matches!(result, ReplResult::Empty));
    }

    #[test]
    fn test_help_command() {
        let mut repl = create_test_repl();

        let result = repl.evaluate_line("/help").unwrap();
        if let ReplResult::Command { message } = result {
            assert!(message.contains("Outrun REPL Commands"));
            assert!(message.contains("/help"));
            assert!(message.contains("/vars"));
            assert!(message.contains("/quit"));
        } else {
            panic!("Expected command result");
        }

        assert_eq!(repl.stats.commands_executed, 1);
    }

    #[test]
    fn test_vars_command_empty() {
        let mut repl = create_test_repl();

        let result = repl.evaluate_line("/vars").unwrap();
        if let ReplResult::Command { message } = result {
            assert!(message.contains("No variables defined"));
        } else {
            panic!("Expected command result");
        }
    }

    #[test]
    fn test_clear_command() {
        let mut repl = create_test_repl();

        // Add a mock variable - create a simple type for testing
        let temp_env =
            outrun_typechecker::compilation::compiler_environment::CompilerEnvironment::new();
        let integer_type = StructuredType::Simple(temp_env.intern_type_name("Integer"));
        repl.user_variables
            .insert("x".to_string(), (Value::integer(42), integer_type));

        let result = repl.evaluate_line("/clear").unwrap();
        if let ReplResult::Command { message } = result {
            assert!(message.contains("Variables and compilation state cleared"));
        } else {
            panic!("Expected command result");
        }

        assert!(repl.user_variables.is_empty());
    }

    #[test]
    fn test_quit_command() {
        let mut repl = create_test_repl();

        let result = repl.evaluate_line("/quit").unwrap();
        assert!(matches!(result, ReplResult::Exit));

        let result = repl.evaluate_line("/q").unwrap();
        assert!(matches!(result, ReplResult::Exit));

        let result = repl.evaluate_line("/exit").unwrap();
        assert!(matches!(result, ReplResult::Exit));
    }

    #[test]
    fn test_types_command() {
        let mut repl = create_test_repl();

        // Test toggling types on
        let result = repl.evaluate_line("/types on").unwrap();
        if let ReplResult::Command { message } = result {
            assert!(message.contains("enabled"));
        } else {
            panic!("Expected command result");
        }
        assert!(repl.config.show_types);

        // Test toggling types off
        let result = repl.evaluate_line("/types off").unwrap();
        if let ReplResult::Command { message } = result {
            assert!(message.contains("disabled"));
        } else {
            panic!("Expected command result");
        }
        assert!(!repl.config.show_types);

        // Test status query
        let result = repl.evaluate_line("/types").unwrap();
        if let ReplResult::Command { message } = result {
            assert!(message.contains("off"));
        } else {
            panic!("Expected command result");
        }
    }

    #[test]
    fn test_unknown_command() {
        let mut repl = create_test_repl();

        let result = repl.evaluate_line("/unknown");
        assert!(result.is_err());
        if let Err(ReplError::Command { message }) = result {
            assert!(message.contains("Unknown command"));
            assert!(message.contains("/unknown"));
        } else {
            panic!("Expected command error");
        }
    }

    #[test]
    fn test_stats_command() {
        let mut repl = create_test_repl();

        // Execute some operations first
        let _ = repl.evaluate_line("42");
        let _ = repl.evaluate_line("/help");

        let result = repl.evaluate_line("/stats").unwrap();
        if let ReplResult::Command { message } = result {
            assert!(message.contains("Session Statistics"));
            assert!(message.contains("Expressions evaluated: 1"));
            assert!(message.contains("Commands executed: 2")); // /help + /stats
        } else {
            panic!("Expected command result");
        }
    }

    #[test]
    fn test_config_command() {
        let mut repl = create_test_repl();

        let result = repl.evaluate_line("/config").unwrap();
        if let ReplResult::Command { message } = result {
            assert!(message.contains("REPL Configuration"));
            assert!(message.contains("Show types"));
            assert!(message.contains("outrun>"));
        } else {
            panic!("Expected command result");
        }
    }

    #[test]
    fn test_arithmetic_expressions() {
        let mut repl = create_test_repl();

        // Test simple arithmetic that should work with intrinsics
        let test_cases = vec![
            ("42", Value::integer(42)),
            ("true", Value::boolean(true)),
            ("false", Value::boolean(false)),
            (r#""hello""#, Value::string("hello".to_string())),
        ];

        for (input, expected) in test_cases {
            let result = repl.evaluate_line(input).unwrap();
            if let ReplResult::Value { value, .. } = result {
                assert_eq!(value, expected, "Failed for input: {input}");
            } else {
                panic!("Expected value result for input: {input}");
            }
        }
    }

    #[test]
    fn test_multi_line_input_detection() {
        let repl = create_test_repl();

        // Test cases for bracket balancing
        assert!(repl.is_input_complete("42")); // Simple complete input
        assert!(repl.is_input_complete("let x = 42")); // Complete let binding
        assert!(repl.is_input_complete("[1, 2, 3]")); // Complete list
        assert!(repl.is_input_complete("\"hello world\"")); // Complete string

        // Incomplete inputs that should trigger continuation
        assert!(!repl.is_input_complete("let x = [")); // Unclosed bracket
        assert!(!repl.is_input_complete("let x = {")); // Unclosed brace
        assert!(!repl.is_input_complete("func(")); // Unclosed paren
        assert!(!repl.is_input_complete("\"hello")); // Unclosed string
        assert!(!repl.is_input_complete("let x = [1,")); // Incomplete list

        // Complex nested cases
        assert!(repl.is_input_complete("[[1, 2], [3, 4]]")); // Nested complete
        assert!(!repl.is_input_complete("[[1, 2], [3,")); // Nested incomplete
        assert!(repl.is_input_complete("\"string with [brackets] inside\"")); // Brackets in string

        // Comments should be ignored
        assert!(repl.is_input_complete("42 # this is a comment"));
        assert!(!repl.is_input_complete("[ # comment\n1,"));
    }

    #[test]
    fn test_repl_config_defaults() {
        let config = ReplConfig::default();
        assert!(config.show_types);
        assert!(!config.verbose_errors);
        assert_eq!(config.max_display_items, 20);
        assert_eq!(config.prompt, "outrun> ");
        assert!(config.persist_history);
        assert_eq!(config.history_file, Some(".outrun_history".to_string()));
    }
}
