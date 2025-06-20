// Outrun Parser Error Handling
// Beautiful error reporting with miette integration

use crate::parser::Rule;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Main parse error type with miette integration
#[derive(Error, Diagnostic, Debug)]
pub enum ParseError {
    #[error("Parse error")]
    #[diagnostic(
        code(outrun::parse::pest_error),
        help("Check the syntax near the highlighted location")
    )]
    PestError {
        #[source_code]
        src: String,
        #[label("error occurred here")]
        span: SourceSpan,
        message: String,
    },

    #[error("Unexpected token")]
    #[diagnostic(
        code(outrun::parse::unexpected_token),
        help("Expected one of: {expected}")
    )]
    UnexpectedToken {
        #[source_code]
        src: String,
        #[label("found this")]
        span: SourceSpan,
        expected: String,
    },

    #[error("Invalid keyword")]
    #[diagnostic(
        code(outrun::parse::invalid_keyword),
        help("Keywords must be one of the reserved words")
    )]
    InvalidKeyword {
        #[source_code]
        src: String,
        #[label("invalid keyword")]
        span: SourceSpan,
        found: String,
    },

    #[error("Invalid boolean literal")]
    #[diagnostic(
        code(outrun::parse::invalid_boolean),
        help("Boolean literals must be 'true' or 'false'")
    )]
    InvalidBoolean {
        #[source_code]
        src: String,
        #[label("invalid boolean")]
        span: SourceSpan,
        found: String,
    },

    #[error("Invalid integer literal")]
    #[diagnostic(
        code(outrun::parse::invalid_integer),
        help("Integer literals must be valid decimal numbers")
    )]
    InvalidInteger {
        #[source_code]
        src: String,
        #[label("invalid integer")]
        span: SourceSpan,
        found: String,
    },

    #[error("Invalid float literal")]
    #[diagnostic(
        code(outrun::parse::invalid_float),
        help("Float literals must be valid decimal numbers with decimal points (e.g., 3.14, 1.23e-4)")
    )]
    InvalidFloat {
        #[source_code]
        src: String,
        #[label("invalid float")]
        span: SourceSpan,
        found: String,
    },

    #[error("Invalid string escape sequence")]
    #[diagnostic(
        code(outrun::parse::invalid_string_escape),
        help("Valid escape sequences: \\n, \\t, \\r, \\\\, \\\", \\uXXXX")
    )]
    InvalidStringEscape {
        #[source_code]
        src: String,
        #[label("invalid escape sequence")]
        span: SourceSpan,
        found: String,
    },

    #[error("Unexpected grammar rule")]
    #[diagnostic(
        code(outrun::parse::unexpected_rule),
        help("Expected rule: {expected}")
    )]
    UnexpectedRule {
        expected: String,
        found: crate::parser::Rule,
        span: crate::ast::Span,
    },

    #[error("Invalid spread element")]
    #[diagnostic(
        code(outrun::parse::invalid_spread),
        help("Spread elements must be '..identifier'")
    )]
    InvalidSpreadElement { span: crate::ast::Span },
}

impl ParseError {
    /// Create a parse error from a Pest parsing error
    pub fn from_pest_error(error: pest::error::Error<crate::parser::Rule>, src: String) -> Self {
        let span = match error.location {
            pest::error::InputLocation::Pos(pos) => SourceSpan::new(pos.into(), 1),
            pest::error::InputLocation::Span((start, end)) => {
                SourceSpan::new(start.into(), end - start)
            }
        };

        // Extract useful information from Pest error
        let enhanced_message = match &error.variant {
            pest::error::ErrorVariant::ParsingError {
                positives,
                negatives: _,
            } => {
                let mut message_parts = Vec::new();

                // Add the original error description
                let base_message = format!("{}", error);
                message_parts.push(base_message);

                // Add helpful context about what was expected
                if !positives.is_empty() {
                    message_parts.push("\n🎯 EXPECTED:".to_string());
                    let expected_descriptions: Vec<String> = positives
                        .iter()
                        .map(rule_to_user_friendly_description)
                        .collect();

                    if expected_descriptions.len() == 1 {
                        message_parts.push(format!("   {}", expected_descriptions[0]));
                    } else {
                        message_parts.push("   One of:".to_string());
                        for desc in expected_descriptions {
                            message_parts.push(format!("   • {}", desc));
                        }
                    }
                }

                // Add context about the parsing location
                let location_context = get_parsing_context(&src, &error.location);
                if !location_context.is_empty() {
                    message_parts.push(format!("\n📍 CONTEXT: {}", location_context));
                }

                // Add helpful suggestions based on the expected rules
                let suggestions = get_suggestions_for_rules(positives);
                if !suggestions.is_empty() {
                    message_parts.push(format!("\n💡 SUGGESTIONS:\n{}", suggestions));
                }

                // Add debug info for developers
                message_parts.push(format!(
                    "\n🔧 DEBUG INFO:\n   Rules expected: {:?}\n   Position: {:?}",
                    positives, error.line_col
                ));

                message_parts.join("")
            }
            _ => {
                // For other error types, provide basic enhancement
                format!(
                    "{}\n\n🔧 DEBUG INFO:\n   Error variant: {:?}\n   Position: {:?}",
                    error, error.variant, error.line_col
                )
            }
        };

        ParseError::PestError {
            src,
            span,
            message: enhanced_message,
        }
    }

    /// Create an unexpected token error
    pub fn unexpected_token(src: String, span: SourceSpan, expected: String) -> Self {
        ParseError::UnexpectedToken {
            src,
            span,
            expected,
        }
    }

    /// Create an invalid keyword error
    pub fn invalid_keyword(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidKeyword { src, span, found }
    }

    /// Create an invalid boolean error
    pub fn invalid_boolean(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidBoolean { src, span, found }
    }

    /// Create an invalid integer error
    pub fn invalid_integer(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidInteger { src, span, found }
    }

    /// Create an invalid float error
    pub fn invalid_float(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidFloat { src, span, found }
    }

    /// Create an invalid string escape error
    pub fn invalid_string_escape(src: String, span: SourceSpan, found: String) -> Self {
        ParseError::InvalidStringEscape { src, span, found }
    }
}

/// Result type for parsing operations
pub type ParseResult<T> = Result<T, ParseError>;

/// Convert a parser rule to a user-friendly description
fn rule_to_user_friendly_description(rule: &Rule) -> String {
    match rule {
        // Literals (using actual rule names from grammar.pest)
        Rule::integer => "a number (like 42, 0xFF, 0b1010)".to_string(),
        Rule::integer_decimal => "a decimal number (like 42)".to_string(),
        Rule::integer_binary => "a binary number (like 0b1010)".to_string(),
        Rule::integer_octal => "an octal number (like 0o755)".to_string(),
        Rule::integer_hexadecimal => "a hexadecimal number (like 0xFF)".to_string(),
        Rule::float => "a decimal number (like 3.14, 1.2e-5)".to_string(),
        Rule::float_standard => "a decimal number (like 3.14)".to_string(),
        Rule::float_scientific => "a scientific notation number (like 1.2e-5)".to_string(),
        Rule::string => "a string (like \"hello\" or \"\"\"multiline\"\"\")".to_string(),
        Rule::string_basic => "a basic string (like \"hello\")".to_string(),
        Rule::string_multiline => "a multiline string (like \"\"\"text\"\"\")".to_string(),
        Rule::boolean => "a boolean (true or false)".to_string(),
        Rule::boolean_true => "the boolean value 'true'".to_string(),
        Rule::boolean_false => "the boolean value 'false'".to_string(),
        Rule::atom => "an atom (like :ok or :error)".to_string(),
        Rule::atom_simple => "a simple atom (like :ok)".to_string(),
        Rule::atom_quoted => "a quoted atom (like :\"complex atom\")".to_string(),
        Rule::sigil => "a sigil literal (like ~R\"regex\")".to_string(),

        // Identifiers and types
        Rule::identifier => "an identifier (like variable_name)".to_string(),
        Rule::type_identifier => "a type name (like String or User)".to_string(),
        Rule::type_annotation => "a type annotation (like : String)".to_string(),
        Rule::qualified_identifier => "a qualified identifier (like Module.function)".to_string(),

        // Expressions
        Rule::expression => "an expression".to_string(),
        Rule::primary_expr => {
            "a basic expression (literal, identifier, or parenthesized expression)".to_string()
        }
        Rule::postfix_expr => {
            "a postfix expression (with method calls or field access)".to_string()
        }
        Rule::unary_expr => "a unary expression (like -x or !condition)".to_string(),
        Rule::braced_expr => "a parenthesized expression (like (x + y))".to_string(),
        Rule::function_call => "a function call (like func(arg: value))".to_string(),
        Rule::field_access => "field access (like obj.field)".to_string(),
        Rule::macro_injection => "a macro injection (like ^parameter)".to_string(),

        // Collections
        Rule::list => "a list (like [1, 2, 3])".to_string(),
        Rule::map => "a map (like {key: value})".to_string(),
        Rule::tuple => "a tuple (like (a, b, c))".to_string(),
        Rule::struct_literal => "a struct literal (like Person{name: \"John\"})".to_string(),

        // Control flow
        Rule::if_expression => "an if expression (if condition { ... })".to_string(),
        Rule::case_expression => "a case expression (case value { ... })".to_string(),
        Rule::concrete_case_expression => "a concrete case expression".to_string(),
        Rule::trait_case_expression => "a trait case expression".to_string(),
        Rule::anonymous_function => "an anonymous function (fn { ... })".to_string(),
        Rule::function_capture => "a function capture (like &function)".to_string(),

        // Definitions
        Rule::function_definition => "a function definition (def name(...) { ... })".to_string(),
        Rule::static_function_definition => {
            "a static function definition (defs name(...) { ... })".to_string()
        }
        Rule::struct_definition => "a struct definition (struct Name(...))".to_string(),
        Rule::trait_definition => "a trait definition (trait Name { ... })".to_string(),
        Rule::impl_block => {
            "an implementation block (impl TraitName for TypeName { ... })".to_string()
        }
        Rule::const_definition => "a constant definition (const NAME: Type = value)".to_string(),
        Rule::let_binding => "a let binding (let pattern = expression)".to_string(),
        Rule::macro_definition => "a macro definition (macro name(...) { ... })".to_string(),

        // Module system
        Rule::alias_definition => "an alias definition (alias Module.Type as Name)".to_string(),
        Rule::import_definition => "an import definition (import Module)".to_string(),
        Rule::module_path => "a module path (like Http.Client)".to_string(),

        // Keywords
        Rule::keyword => "a keyword".to_string(),
        Rule::keyword_def => "the 'def' keyword".to_string(),
        Rule::keyword_defp => "the 'defp' keyword".to_string(),
        Rule::keyword_defs => "the 'defs' keyword".to_string(),
        Rule::keyword_struct => "the 'struct' keyword".to_string(),
        Rule::keyword_trait => "the 'trait' keyword".to_string(),
        Rule::keyword_impl => "the 'impl' keyword".to_string(),
        Rule::keyword_let => "the 'let' keyword".to_string(),
        Rule::keyword_const => "the 'const' keyword".to_string(),
        Rule::keyword_fn => "the 'fn' keyword".to_string(),
        Rule::keyword_if => "the 'if' keyword".to_string(),
        Rule::keyword_else => "the 'else' keyword".to_string(),
        Rule::keyword_case => "the 'case' keyword".to_string(),
        Rule::keyword_when => "the 'when' keyword".to_string(),
        Rule::keyword_alias => "the 'alias' keyword".to_string(),
        Rule::keyword_import => "the 'import' keyword".to_string(),
        Rule::keyword_macro => "the 'macro' keyword".to_string(),
        Rule::keyword_for => "the 'for' keyword".to_string(),
        Rule::keyword_self => "the 'Self' keyword".to_string(),
        Rule::keyword_function => "the 'Function' keyword".to_string(),
        Rule::keyword_as => "the 'as' keyword".to_string(),
        Rule::keyword_only => "the 'only' keyword".to_string(),
        Rule::keyword_except => "the 'except' keyword".to_string(),

        // Operators
        Rule::op => "an operator".to_string(),
        Rule::op_logical_and => "a logical AND operator (&&)".to_string(),
        Rule::op_logical_or => "a logical OR operator (||)".to_string(),
        Rule::op_pipe => "a pipe operator (|>)".to_string(),
        Rule::op_pipe_maybe => "a maybe pipe operator (|?)".to_string(),
        Rule::op_shift_left => "a left shift operator (<<)".to_string(),
        Rule::op_shift_right => "a right shift operator (>>)".to_string(),
        Rule::op_equal => "an equality operator (==)".to_string(),
        Rule::op_not_equal => "a not-equal operator (!=)".to_string(),
        Rule::op_less_equal => "a less-than-or-equal operator (<=)".to_string(),
        Rule::op_greater_equal => "a greater-than-or-equal operator (>=)".to_string(),
        Rule::op_exponent => "an exponent operator (**)".to_string(),
        Rule::op_bitwise_and => "a bitwise AND operator (&)".to_string(),
        Rule::op_bitwise_or => "a bitwise OR operator (|)".to_string(),
        Rule::op_bitwise_xor => "a bitwise XOR operator (^)".to_string(),
        Rule::op_bitwise_not => "a bitwise NOT operator (~)".to_string(),
        Rule::op_logical_not => "a logical NOT operator (!)".to_string(),
        Rule::op_less => "a less-than operator (<)".to_string(),
        Rule::op_greater => "a greater-than operator (>)".to_string(),
        Rule::op_add => "an addition operator (+)".to_string(),
        Rule::op_subtract => "a subtraction operator (-)".to_string(),
        Rule::op_multiply => "a multiplication operator (*)".to_string(),
        Rule::op_divide => "a division operator (/)".to_string(),
        Rule::op_modulo => "a modulo operator (%)".to_string(),
        Rule::op_unary_plus => "a unary plus operator (+)".to_string(),
        Rule::op_unary_minus => "a unary minus operator (-)".to_string(),

        // Patterns
        Rule::pattern => "a pattern (for destructuring)".to_string(),
        Rule::literal_pattern => "a literal pattern".to_string(),
        Rule::tuple_pattern => "a tuple pattern".to_string(),
        Rule::struct_pattern => "a struct pattern".to_string(),
        Rule::list_pattern => "a list pattern".to_string(),

        // Attributes
        Rule::attribute => "an attribute (like @Doc(...))".to_string(),

        // Structure
        Rule::program => "a program".to_string(),
        Rule::program_item => "a program item".to_string(),
        Rule::block => "a block ({ ... })".to_string(),
        Rule::statement => "a statement".to_string(),
        Rule::statement_sequence => "a sequence of statements".to_string(),

        // Special tokens
        Rule::COMMENT => "a comment (starting with #)".to_string(),
        Rule::comment => "a line comment (# text)".to_string(),
        Rule::block_comment => "a block comment (### text ###)".to_string(),
        Rule::WHITESPACE => "whitespace".to_string(),
        Rule::ws_comment => "whitespace or comment".to_string(),
        Rule::item_separator => "an item separator".to_string(),
        Rule::EOI => "end of input".to_string(),

        // Function related
        Rule::function_visibility => "function visibility (def or defp)".to_string(),
        Rule::parameter_list => "a parameter list".to_string(),
        Rule::parameters => "parameters".to_string(),
        Rule::parameter => "a parameter".to_string(),
        Rule::return_type => "a return type annotation".to_string(),
        Rule::guard_clause => "a guard clause (when ...)".to_string(),
        Rule::argument_list => "an argument list".to_string(),
        Rule::argument => "an argument".to_string(),

        // Type system
        Rule::function_type => "a function type (Function<...>)".to_string(),
        Rule::tuple_type => "a tuple type".to_string(),
        Rule::generic_params => "generic parameters".to_string(),
        Rule::generic_args => "generic arguments".to_string(),
        Rule::trait_constraints => "trait constraints".to_string(),
        Rule::impl_constraints => "implementation constraints".to_string(),

        // Fallback for unknown rules
        _ => format!("a {:?}", rule).replace('_', " "),
    }
}

/// Get parsing context based on the location in source
fn get_parsing_context(source: &str, location: &pest::error::InputLocation) -> String {
    let position = match location {
        pest::error::InputLocation::Pos(pos) => *pos,
        pest::error::InputLocation::Span((start, _)) => *start,
    };

    // Find the line containing the error
    let lines: Vec<&str> = source.lines().collect();
    let mut current_pos = 0;

    for (line_num, line) in lines.iter().enumerate() {
        let line_end = current_pos + line.len();

        if position >= current_pos && position <= line_end {
            let _col = position - current_pos;

            // Provide context about where in the file we are
            let file_context = if line_num == 0 {
                "beginning of file"
            } else {
                "within file"
            };
            let line_context = format!("line {}", line_num + 1);
            let content_context = if !line.trim().is_empty() {
                format!("in: {}", line.trim())
            } else {
                "on empty line".to_string()
            };

            return format!("{}, {}, {}", file_context, line_context, content_context);
        }

        current_pos = line_end + 1; // +1 for newline
    }

    "unknown location".to_string()
}

/// Generate helpful suggestions based on expected rules
fn get_suggestions_for_rules(rules: &[Rule]) -> String {
    let mut suggestions = Vec::new();

    // Check for common patterns and provide specific advice
    if rules.contains(&Rule::keyword_defs) {
        suggestions.push(
            "   • If defining a static function in a trait, use: defs function_name(...)"
                .to_string(),
        );
        suggestions.push("   • Make sure you're inside a trait definition".to_string());
    }

    if rules.contains(&Rule::function_definition) || rules.contains(&Rule::keyword_def) {
        suggestions.push(
            "   • For regular functions, use: def function_name(param: Type) { ... }".to_string(),
        );
    }

    if rules.contains(&Rule::struct_definition) || rules.contains(&Rule::keyword_struct) {
        suggestions.push("   • For structs, use: struct StructName(field: Type)".to_string());
    }

    if rules.contains(&Rule::trait_definition) || rules.contains(&Rule::keyword_trait) {
        suggestions.push("   • For traits, use: trait TraitName { def method(...) }".to_string());
    }

    if rules.contains(&Rule::COMMENT) {
        suggestions.push("   • Comments start with # (line) or ### block ###".to_string());
    }

    if rules.contains(&Rule::expression) {
        suggestions
            .push("   • Try a literal (42, \"text\", true), identifier, or expression".to_string());
    }

    if rules.contains(&Rule::type_annotation) {
        suggestions.push("   • Type annotations use colon: variable: Type".to_string());
    }

    // If no specific suggestions, provide general advice
    if suggestions.is_empty() {
        suggestions
            .push("   • Check syntax against examples in the language documentation".to_string());
        suggestions.push("   • Ensure proper spacing and punctuation".to_string());
    }

    suggestions.join("\n")
}
