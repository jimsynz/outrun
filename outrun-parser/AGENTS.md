# AGENTS.md - Parser Development Guide

## Build/Test Commands
- `cargo test --package outrun-parser` - Run 400+ comprehensive parser tests
- `cargo clippy --package outrun-parser --all-targets --all-features -- -D warnings` - Run lints
- `cargo fmt --package outrun-parser` - Format parser code

## Critical Pest Grammar Rules
- **Multi-character operators FIRST**: `"&&"` before `"&"`, `"|>"` before `"|"` to prevent tokenization conflicts
- **Atomic rules (@)** for tokens: `op_logical_and = @{ "&&" }`
- **Silent rules (_)** for structure: `list_elements = _{ expression ~ ("," ~ expression)* }`
- **snake_case naming**: `integer_decimal`, `string_basic`, `function_call`

## AST Design Requirements
- **Every node needs Span**: `pub span: Span` for source location tracking
- **Format preservation**: Use enums like `IntegerFormat::Hexadecimal` to track original representation
- **Source reconstruction**: Implement Display trait for perfect round-trip parsing
- **Utility functions**: Use `span_from_pair()` instead of manual span creation

## Test Organization Rules
- **Test files MUST start with `test_` prefix**: `test_string_literals.rs`, `test_arithmetic_operators.rs`
- **Separate test directories**: Use `tests/` or `src/tests/`, never inline `#[cfg(test)]` modules
- **Comprehensive coverage**: Test all format variants, edge cases, and error conditions
- **Source reconstruction tests**: Every parsed construct must regenerate original source

## Parser Module Patterns
- **Modular organization**: `parser/literals.rs`, `parser/expressions.rs`, `parser/collections.rs`
- **Consistent error handling**: Propagate ParseError with proper spans
- **Format-specific parsing**: Separate paths for each literal format (decimal, hex, binary, octal)
- **Collection patterns**: Generic comma-separated lists with optional trailing comma support

## Expression Precedence (CRITICAL)
- **13-level hierarchy**: Pipe (lowest) → Logical OR → Logical AND → Equality → Comparison → Bitwise → Shift → Additive → Multiplicative → Exponentiation → Unary → Primary (highest)
- **Left associative**: Most operators except exponentiation (`**`) which is right associative
- **Precedence climbing**: Start at lowest precedence and climb up

## Source Preservation Requirements
- **Lossless representation**: AST must preserve ALL source information for formatters
- **Comment attachment**: Use DebugInfo system to attach comments to nodes
- **Whitespace tracking**: Preserve original formatting choices (trailing commas, etc.)
- **Line/column tracking**: Optional but helpful for IDE integration

## Integration Points
- **Parser AST consumed by typechecker**: Ensure AST structure supports type annotation attachment
- **Error reporting with miette**: Use SourceSpan and proper error context
- **CLI tool integration**: Support both file parsing and stdin input
- **Display protocols**: Enable source reconstruction for testing and tooling

## Absolute Prohibitions
- **NEVER use positional parameters** - All Outrun functions use named parameters only
- **NEVER assume methods exist** - Outrun uses static protocol functions, not instance methods
- **NEVER create unit/void types** - All Outrun functions return meaningful values
- **NEVER hardcode operator mappings** - All operators desugar to protocol calls through proper AST

## Quality Standards
- **400+ tests passing**: Comprehensive coverage across 43 test files
- **Zero clippy warnings**: Strict Rust best practices
- **Perfect source reconstruction**: Round-trip parsing via Display protocols
- **Production-ready error messages**: Beautiful miette integration with source highlighting