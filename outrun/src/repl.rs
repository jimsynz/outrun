//! REPL (Read-Eval-Print Loop) implementation for Outrun
//!
//! This module provides an interactive REPL that works with the
//! interpreter and typechecker v3 system. Features:
//! - Interactive expression evaluation with persistent variable bindings
//! - Pattern matching support for let bindings and case expressions
//! - REPL commands for debugging and inspection
//! - Error recovery with beautiful error reporting using miette
//! - History support and line editing with rustyline

use miette::Diagnostic;
use outrun_interpreter::{InterpreterSession, TestHarnessError, Value};
use outrun_parser::ParseError;
use rustyline::{DefaultEditor, error::ReadlineError};
// use std::collections::HashMap; // Not needed for simple REPL
use thiserror::Error;

/// Errors that can occur in the REPL
#[derive(Debug, Error, Diagnostic)]
pub enum ReplError {
    #[error("Parse error: {source}")]
    Parse {
        #[from]
        source: ParseError,
    },

    #[error("Runtime error: {source}")]
    Runtime { source: BoxedTestHarnessError },

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

impl From<TestHarnessError> for ReplError {
    fn from(err: TestHarnessError) -> Self {
        ReplError::Runtime {
            source: BoxedTestHarnessError(Box::new(err)),
        }
    }
}

#[derive(Debug)]
pub struct BoxedTestHarnessError(Box<TestHarnessError>);

impl std::fmt::Display for BoxedTestHarnessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for BoxedTestHarnessError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }
}

impl miette::Diagnostic for BoxedTestHarnessError {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.0.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        self.0.source_code()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.0.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn miette::Diagnostic> + 'a>> {
        self.0.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn miette::Diagnostic> {
        self.0.diagnostic_source()
    }
}

/// REPL session that maintains state across evaluations
pub struct ReplSession {
    /// Interactive line editor with history
    editor: DefaultEditor,

    /// Interpreter session for full pipeline integration (Parser → Typechecker → Interpreter)
    session: InterpreterSession,

    /// REPL configuration
    config: ReplConfig,

    /// Session statistics
    stats: ReplStats,
}

/// REPL configuration options
#[derive(Debug, Clone)]
pub struct ReplConfig {
    /// Show type information with results (simplified - no actual type checking)
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
            show_types: false, // Simplified - no type checking yet
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
    Value { value: Value },

    /// Executed a REPL command
    Command { message: String },

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

        // Create interpreter session for full pipeline integration
        let session = InterpreterSession::new().map_err(|e| ReplError::Internal {
            message: format!("Failed to create interpreter session: {e}"),
        })?;

        Ok(Self {
            editor,
            session,
            config,
            stats: ReplStats::default(),
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
                '\\' if in_string => {
                    escaped = true;
                }
                '"' => {
                    in_string = !in_string;
                }
                '{' if !in_string => {
                    brace_count += 1;
                }
                '}' if !in_string => {
                    brace_count -= 1;
                }
                '[' if !in_string => {
                    bracket_count += 1;
                }
                ']' if !in_string => {
                    bracket_count -= 1;
                }
                '(' if !in_string => {
                    paren_count += 1;
                }
                ')' if !in_string => {
                    paren_count -= 1;
                }
                '#' if !in_string => {
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
        brace_count == 0 && bracket_count == 0 && paren_count == 0 && !in_string
    }

    /// Evaluate a line of input
    pub fn evaluate_line(&mut self, line: &str) -> Result<ReplResult, ReplError> {
        let trimmed = line.trim();

        // Handle empty lines and comments
        if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with("#") {
            return Ok(ReplResult::Empty);
        }

        // Handle REPL commands
        if trimmed.starts_with('/') {
            return self.execute_command(trimmed);
        }

        // Use the interpreter session to evaluate with full pipeline integration
        let value = self.session.evaluate(trimmed)?;

        // Update statistics
        self.stats.expressions_evaluated += 1;

        Ok(ReplResult::Value { value })
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
                // Reset interpreter session (clears all variables)
                self.session = InterpreterSession::new().map_err(|e| ReplError::Internal {
                    message: format!("Failed to reset interpreter session: {e}"),
                })?;
                Ok(ReplResult::Command {
                    message: "Variables cleared".to_string(),
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
                        message: "Type display enabled (note: no type checking yet)".to_string(),
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
                        "Type display is {} (note: no type checking yet)",
                        if self.config.show_types { "on" } else { "off" }
                    ),
                }),
            },

            unknown => Err(ReplError::Command {
                message: format!("Unknown command: {unknown}. Type /help for available commands."),
            }),
        }
    }

    /// Display the result of evaluation
    fn display_result(&self, result: ReplResult) {
        match result {
            ReplResult::Value { value } => {
                // Display the value
                println!("{}", value.display());
            }

            ReplResult::Command { message } => {
                println!("{message}");
            }

            ReplResult::Empty => {} // No output for empty lines

            ReplResult::Exit => {} // Handled by caller
        }
    }

    /// Display an error with appropriate formatting
    fn display_error(&self, error: ReplError, source_code: Option<&str>) {
        match error {
            ReplError::Parse { source } => {
                // ParseError implements Diagnostic and contains its own source code
                let report = miette::Report::new(source);
                eprintln!("{report:?}");
            }
            ReplError::Runtime { source } => {
                // EvaluationError should be displayed with context
                if let Some(source_code) = source_code {
                    let named_source = miette::NamedSource::new("<repl>", source_code.to_string());
                    let report = miette::Report::new(source).with_source_code(named_source);
                    eprintln!("{report:?}");
                } else {
                    let report = miette::Report::new(source);
                    eprintln!("{report:?}");
                }
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
        println!("🌆 Outrun REPL v2.0 (Simplified) 🌃");
        println!("Using new interpreter system - Type /help for commands, /quit to exit");
        println!("Note: Type checking not yet integrated - expressions evaluated directly");
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
        r#"Outrun REPL Commands (Simplified):
  /help, /h           Show this help message
  /vars, /variables   List all variables with their values
  /clear              Clear all variables and reset the session
  /stats              Show session statistics
  /config             Show current configuration
  /types [on|off]     Toggle type display (note: no type checking yet)
  /quit, /q, /exit    Exit the REPL

Examples:
  42                  # Evaluate an integer literal
  let x = 42          # Bind a variable
  "hello"             # String literal
  [1, 2, 3]           # List literal
  true                # Boolean literal
  case x { 42 -> "answer"; _ -> "other" }  # Case expression

Note: This is a simplified REPL using the new interpreter.
Type checking integration is planned for future versions.

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
        // Use the interpreter session context to get variable information
        let context = self.session.context();
        if context.is_empty() {
            "No variables defined".to_string()
        } else {
            let mut lines = vec!["Variables:".to_string()];
            let variables = context.list_variables();

            for (name, value) in variables {
                lines.push(format!("  {} = {}", name, value.display()));
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
  Show types: {} (note: no type checking yet)
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
