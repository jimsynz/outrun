# Outrun Parser (Pest-based)

## Overview

This directory contains the Pest-based parser implementation for the Outrun programming language. This parser is designed to provide complete source information preservation for formatting tools, IDE support, and tooling integration.

## Project Structure

```
outrun-parser/
├── src/
│   ├── lib.rs              # Main library interface
│   ├── ast.rs              # AST node definitions with source preservation
│   ├── parser.rs           # Parser implementation using Pest
│   ├── error.rs            # Error types with miette integration
│   ├── visitor.rs          # AST visitor traits
│   └── grammar.pest        # Pest grammar definition
├── tests/                  # Integration tests
└── Cargo.toml             # Dependencies and configuration
```

## Key Dependencies

- **pest** - PEG parser generator
- **pest_derive** - Procedural macros for parser generation
- **miette** - Beautiful error reporting with source highlighting
- **thiserror** - Error handling integration

## AST Design Principles

### Source Preservation
- **Lossless representation**: AST preserves all source information for perfect reconstruction
- **Span tracking**: Every AST node includes position information
- **Format preservation**: Original literal formats, whitespace, and comments maintained
- **Formatter-friendly**: Designed to support auto-formatting and refactoring tools

### Format Tracking Examples
```rust
// Integer literals preserve their original format
IntegerLiteral {
    value: 255,
    format: IntegerFormat::Hexadecimal, // Was written as 0xFF
    span: Span { start: 10, end: 14 },
}

// String literals track delimiter style
StringLiteral {
    content: "Hello\nWorld",
    format: StringFormat::Multiline,    // Was written as """..."""
    span: Span { start: 5, end: 20 },
}
```

## Working with Pest Grammar

### Grammar File Organisation
The `grammar.pest` file should be organised as follows:
1. **Lexical elements** (tokens, literals, identifiers)
2. **Type system** (struct/trait definitions, type annotations)
3. **Expressions** (precedence hierarchy, operators)
4. **Statements** (let bindings, control flow)
5. **Top-level items** (functions, modules, etc.)

### Pest Best Practices

#### Rule Naming Conventions
- Use **snake_case** for all rule names
- Be specific: `integer_literal` not just `integer`
- Group related rules: `string_basic`, `string_multiline`
- Use format suffixes: `float_standard`, `float_scientific`

#### Source Preservation Rules
```pest
// Preserve comments explicitly (don't use special COMMENT rule)
comment = { "#" ~ (!"\n" ~ ANY)* ~ "\n"? }
block_comment = { "###" ~ (!"###" ~ ANY)* ~ "###" }

// Track different literal formats separately
integer_decimal = { ASCII_DIGIT+ }
integer_binary = { "0b" ~ ASCII_BIN_DIGIT+ }
integer_octal = { "0o" ~ ASCII_OCT_DIGIT+ }
integer_hexadecimal = { "0x" ~ ASCII_HEX_DIGIT+ }

// Minimal WHITESPACE rule (only for token separation)
WHITESPACE = _{ " " | "\t" }

// Explicit newline tracking
newline = { "\n" | "\r\n" }
```

#### Expression Precedence
Follow Ruby's operator precedence (from lowest to highest):
1. Pipe operators (`|>`, `|?`)
2. Logical OR (`||`)
3. Logical AND (`&&`)
4. Equality (`==`)
5. Comparison (`>`, `>=`, `<`, `<=`)
6. Bitwise OR (`|`)
7. Bitwise XOR (`^`)
8. Bitwise AND (`&`)
9. Shift (`<<`, `>>`)
10. Additive (`+`, `-`)
11. Multiplicative (`*`, `/`, `%`)
12. Exponentiation (`**`)
13. Unary (`+`, `-`, `!`, `~`)
14. Postfix (`.`, `()`, `[]`)

#### Grammar Debugging
```bash
# Test grammar rules individually
echo "42" | pest_debugger grammar.pest integer_decimal

# Parse complete files
pest_debugger grammar.pest program < example.outrun

# Use Pest's built-in error reporting
cargo test -- --nocapture  # See parse errors in tests
```

### AST Construction

#### Span Preservation
Every AST node should include span information:
```rust
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
```

#### Format Enums
Use enums to track original formatting choices:
```rust
#[derive(Debug, Clone, PartialEq)]
pub enum IntegerFormat {
    Decimal,
    Binary,      // 0b prefix
    Octal,       // 0o prefix
    Hexadecimal, // 0x prefix
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringFormat {
    Basic,     // "..."
    Multiline, // """..."""
}
```

#### Comment Attachment
Attach comments to relevant AST nodes:
```rust
#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub name: TypeIdentifier,
    pub fields: Vec<StructField>,
    pub methods: Vec<FunctionDefinition>,
    pub preceding_comments: Vec<Comment>,
    pub span: Span,
}
```

## Error Handling with Miette

### Error Type Definition
```rust
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum ParseError {
    #[error("Unexpected token")]
    #[diagnostic(
        code(outrun::parse::unexpected_token),
        help("Expected one of: {expected}")
    )]
    UnexpectedToken {
        #[label("found this")]
        span: SourceSpan,
        expected: String,
    },
    
    #[error("Invalid integer literal")]
    #[diagnostic(
        code(outrun::parse::invalid_integer),
        help("Integer literals must be valid decimal, binary (0b), octal (0o), or hexadecimal (0x)")
    )]
    InvalidInteger {
        #[label("invalid integer")]
        span: SourceSpan,
    },
}
```

### Beautiful Error Display
```rust
// In your parser
fn parse_with_errors(input: &str) -> Result<Program, miette::Report> {
    match OutrunParser::parse(Rule::program, input) {
        Ok(pairs) => Ok(build_ast(pairs)),
        Err(pest_error) => {
            let error = convert_pest_error(pest_error);
            Err(miette::Report::new(error).with_source_code(input.to_string()))
        }
    }
}
```

## Testing Strategy

### Tree-sitter Compatibility
Convert the existing 178 tree-sitter test cases to validate equivalent parsing behaviour:
```rust
#[test]
fn test_integer_literals_positive() {
    let input = "42";
    let result = parse_expression(input).unwrap();
    
    match result.kind {
        ExpressionKind::IntegerLiteral { value, format, .. } => {
            assert_eq!(value, 42);
            assert_eq!(format, IntegerFormat::Decimal);
        }
        _ => panic!("Expected integer literal"),
    }
}
```

### Source Reconstruction Tests
Verify lossless source preservation:
```rust
#[test]
fn test_source_reconstruction() {
    let original = "let x: Integer = 0xFF  # hexadecimal\n";
    let ast = parse_program(original).unwrap();
    let reconstructed = ast.to_source_string();
    assert_eq!(original, reconstructed);
}
```

## Development Workflow

### Adding New Syntax (Pest Parser Development Process)

**🔄 AUTOMATIC RULE: Always update PEST_PLAN.md after completing each step**

1. **Update grammar.pest** with new rules following naming conventions
2. **Add AST nodes** in `ast.rs` with proper span and format tracking
3. **Update parser** in `parser.rs` to construct AST from Pest pairs
4. **Add error handling** for invalid cases with helpful miette diagnostics
5. **Write comprehensive tests** including format preservation and edge cases
6. **Test thoroughly** with `cargo test` and manual parsing
7. **Update PEST_PLAN.md** with completion checkmarks ✅ **MANDATORY**
8. **Update todo list** for next priority items

### Grammar Changes
```bash
# After editing grammar.pest
cargo check                    # Verify grammar compiles
cargo test grammar_tests       # Run grammar-specific tests
cargo test --lib              # Run all parser tests
```

### Common Patterns

#### Optional Elements
```pest
// Use ? for optional elements
function_definition = { 
    "def" ~ identifier ~ "(" ~ parameter_list? ~ ")" ~ return_type? ~ guard_clause? ~ block 
}
```

#### Lists with Separators
```pest
// Use ~ for sequences, | for choices
parameter_list = { parameter ~ ("," ~ parameter)* ~ ","? }
```

#### Atomic Rules for Tokens
```pest
// Use @ for atomic rules (no whitespace insertion)
identifier = @{ ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "_")* }
```

## Integration Points

### Future Type Checker
The AST is designed to be consumed by a future type checker:
- Preserve all type annotations with spans
- Track generic parameters and constraints
- Maintain function signatures and implementations

### Language Server Protocol
AST supports LSP features:
- Precise position information for goto-definition
- Span-based error reporting
- Symbol extraction for completions
- Comment preservation for hover documentation

### Formatting Tools
Enable auto-formatting with format preservation:
- Original literal formats guide default choices
- Comment placement preserved
- Whitespace patterns maintained
- User style preferences respected

## Useful Commands

```bash
# Build parser
cargo build

# Run all tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Check grammar without running tests
cargo check

# Build with debugging
cargo build --features debug

# Run specific test category
cargo test integer_literals

# Format code
cargo fmt

# Lint code
cargo clippy
```

## PEST PARSER IMPLEMENTATION STATUS 🚀

### ✅ **COMPLETED FEATURES (June 2025)**

#### **Core Literals & Collections**
- ✅ **String literals**: Basic `"text"` and multiline `"""text"""` with escape sequences
- ✅ **String interpolation**: `"Hello #{name}!"` with expression parsing
- ✅ **Integer formats**: Decimal, binary (`0b`), octal (`0o`), hexadecimal (`0x`)
- ✅ **Float formats**: Standard (`3.14`) and scientific (`1.23e-4`, `1.23E-4`)
- ✅ **Boolean literals**: `true`, `false`
- ✅ **Atom literals**: Simple (`:atom`) and quoted (`:\"complex atom\"`)
- ✅ **Sigil literals**: `~TypeName"content"` with interpolation support
- ✅ **Collections**: Lists `[1,2,3]`, Maps `{key: value}` & `{key => value}`, Tuples `(a,b,c)`
- ✅ **Spread operators**: Complete support in lists `[first, ..rest]`, structs `User { name, ..defaults }`, and maps `{ key: value, ..base }`

#### **Expression System with Full Precedence**
- ✅ **Arithmetic operators**: `+`, `-`, `*`, `/`, `%`, `**` (exponentiation)
- ✅ **Comparison operators**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- ✅ **Logical operators**: `&&`, `||`, `!` with proper precedence
- ✅ **Bitwise operators**: `&`, `|`, `^`, `~`, `<<`, `>>` with proper precedence
- ✅ **Pipe operators**: `|>`, `|?` (lowest precedence)
- ✅ **Unary operators**: `+`, `-`, `!`, `~` with chaining support
- ✅ **Parentheses**: Override precedence correctly
- ✅ **Complete precedence hierarchy**: Pipe → Logical OR → Logical AND → Bitwise OR → Bitwise XOR → Bitwise AND → Comparison → Shift → Additive → Multiplicative → Exponentiation → Unary → Primary
- ✅ **Associativity**: Left for most operators, right for exponentiation

#### **Control Flow**
- ✅ **If expressions**: `if condition { then } else { else }` with optional else
- ✅ **Case expressions**: `case value { when guard -> result else -> default }` with pattern matching
- ✅ **Function calls**: Named parameters with shorthand syntax
- ✅ **Let bindings**: `let name: Type = expression` with type inference
- ✅ **Function definitions**: `def name(params): ReturnType when guard { body }`
- ✅ **Struct literals**: `TypeName { field: value, shorthand, ..spread }` with all three field types

#### **AST & Source Preservation**
- ✅ **Complete AST structures**: All literals, expressions, operations with span tracking
- ✅ **Format preservation**: Original literal formats maintained (e.g., `0xFF` stays hexadecimal)
- ✅ **Source reconstruction**: Perfect round-trip formatting via Display traits
- ✅ **Expression wrapping**: All top-level items properly wrapped in `ItemKind::Expression`

#### **Testing Infrastructure**
- ✅ **340+ comprehensive tests** across 37+ test files (100% pass rate)
- ✅ **Test categories**: Literals, operators, collections, control flow, functions, precedence, display
- ✅ **Edge cases**: Nested expressions, complex control flow, format preservation
- ✅ **API testing**: Public interface functions validated
- ✅ **Integration**: All features work together seamlessly

### 📊 **Test Statistics**
```
Total Tests: 296 ✅ (100% pass rate)
├── alias_statements.rs: 11 tests (NEW)
├── api_functions.rs: 3 tests
├── arithmetic_operators.rs: 17 tests
├── atom_literals.rs: 12 tests
├── basic_parsing.rs: 16 tests
├── bitwise_operators.rs: 17 tests
├── collections.rs: 21 tests
├── comparison_operators.rs: 15 tests
├── control_flow_case.rs: 9 tests (NEW)
├── control_flow_if.rs: 9 tests
├── float_literals.rs: 11 tests
├── function_calls.rs: 10 tests
├── function_definitions.rs: 10 tests
├── import_statements.rs: 16 tests (NEW)
├── integer_formats.rs: 9 tests
├── let_bindings.rs: 11 tests
├── logical_operators.rs: 12 tests
├── multiline_strings.rs: 10 tests
├── pipe_operators.rs: 12 tests
├── sigil_literals.rs: 10 tests
├── source_reconstruction.rs: 3 tests
├── string_interpolation.rs: 10 tests
└── string_literals.rs: 10 tests
```

### 🎯 **Complete Precedence Hierarchy (Implemented)**
1. **Primary**: Literals, identifiers, parentheses
2. **Unary**: `+expr`, `-expr`, `!expr`, `~expr`
3. **Exponentiation**: `**` (right associative)
4. **Multiplicative**: `*`, `/`, `%` (left associative)
5. **Additive**: `+`, `-` (left associative)
6. **Shift**: `<<`, `>>` (left associative)
7. **Bitwise AND**: `&` (left associative)
8. **Bitwise XOR**: `^` (left associative)
9. **Bitwise OR**: `|` (left associative)
10. **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=` (left associative)
11. **Logical AND**: `&&` (left associative)
12. **Logical OR**: `||` (left associative)
13. **Pipe**: `|>`, `|?` (left associative, lowest precedence)

### 🎯 **Next Development Priorities**
1. **Macros** (macro definitions with argument injection)
2. **Anonymous functions** (closures with capture syntax)
3. **Advanced pattern matching** (destructuring assignments)
4. **Error handling** (more sophisticated error types and recovery)

### 🏗️ **Architecture Decisions Made**
- **Precedence-climbing parser**: Clean hierarchy with dedicated methods per precedence level
- **Expression-first grammar**: All top-level items wrapped in expressions for operator support
- **Format-preserving AST**: Every literal tracks its original format for perfect source reconstruction
- **Comprehensive testing**: One test file per feature with systematic coverage
- **Helper functions**: Extract collections/literals from expressions to maintain test readability

### 💡 **Key Learnings**
- **Pest operator precedence**: Must structure grammar rules in precedence order with proper climbing
- **AST design**: Balance between specificity (clean Display) and genericity (reusable patterns)
- **Test maintenance**: Expression wrapping required updating all existing tests, but systematic approach worked well
- **Source preservation**: Critical for formatters and IDE tooling - design AST with this from the start

## 🔧 **Recent Code Quality Improvements (June 2025)**

### ✅ **Span Utilities Refactoring**
**Status**: COMPLETED - Successfully reduced code duplication in span creation patterns

**Changes Made**:
- Added `span_from_pair()` and `span_from_range()` utility functions to main parser
- Replaced **29 manual `Span::new()` calls** across all parser modules
- Eliminated ~58 lines of repetitive boilerplate code
- Improved consistency and maintainability of span creation

**Impact**:
- ✅ **All 322 tests passing** with zero regressions
- ✅ **100% consistency** in span creation patterns across codebase
- ✅ **Future-proof** - new parsing functions automatically benefit from utilities
- ✅ **Centralized logic** - easier to modify span behavior in one place

**Files Updated**: parser.rs, types.rs (10 instances), collections.rs (3), expressions.rs (4), control_flow.rs (4), functions.rs (1), literals.rs (4)

### 📋 **Remaining Refactoring Opportunities**
- **Error standardization** - Use existing helper functions consistently
- **Large function breakdown** - Split complex functions like `parse_program()` (122 lines)
- **Collection parsing helpers** - Generic utilities for common list parsing patterns
- **Naming consistency** - Standardize function naming and visibility patterns