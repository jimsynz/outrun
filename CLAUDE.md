# Working with the Outrun Project

## Project Overview

Outrun is a statically-typed, functional programming language built around the concept of traits. The language emphasises immutability, named parameters, and a powerful guard system for control flow.

## Project Structure

```
outrun/
├── README.md                    # Project discussion and background
├── SYNTAX_SPEC.md              # Complete language syntax specification
├── GRAMMAR.bnf                 # Formal BNF grammar
├── outrun.toml                 # Package manifest (when created)
├── outrun-parser/              # Parser implementation (Rust)
│   ├── Cargo.toml
│   └── src/
│       └── lib.rs
└── tree-sitter-outrun/         # Tree-sitter grammar implementation ✅ COMPLETE
    ├── grammar.js              # Tree-sitter grammar definition
    ├── src/
    │   ├── grammar.json
    │   ├── node-types.json
    │   └── parser.c
    └── test/
        └── corpus/             # 178 comprehensive test files (100% pass rate)
            ├── literals_*.txt  # All literal types
            ├── operators_*.txt # All operators with precedence
            ├── functions_*.txt # Function definitions and calls
            ├── control_flow_*.txt # If/else, case statements
            ├── types_*.txt     # Structs, traits, implementations
            ├── macros_*.txt    # Macro definitions with injection
            ├── modules_*.txt   # Module system (alias/import)
            └── application_*.txt # Application entry point pattern
```

## Core Design Principles

- **Everything is traits** - All types implement traits, which define behaviour
- **Named arguments only** - No positional arguments in function calls  
- **Static typing** with trait constraints and guards
- **Immutable and functional** - No mutation, rebinding allowed
- **Actor model runtime** - Built for concurrent, distributed systems
- **Tree-sitter based** - Enables embedded DSLs with full language server support

## Key Language Features

### Trait-Based Everything
All operators, control flow, and behaviour is defined through traits:
- `+` calls `BinaryAddition.add()`
- `|>` calls `Pipe.pipe_into()`
- `|?` calls `Maybe.maybe_pipe()`
- `@Attribute()` calls `Attribute.apply()`
- `~Sigil""` calls `Sigil.parse()`

### Module System
- Types (structs/traits) ARE modules
- File structure maps to module hierarchy: `src/http/client.outrun` → `Http.Client`
- Multiple types per file all belong to the same module namespace
- Constants are always private; use functions to expose values

### Guard System
Functions ending in `?` must be side-effect-free and return Boolean:
```outrun
def divide(a: Integer, b: Integer): Float 
when Integer.non_zero?(b) {
    Float.from_integer(a) / Float.from_integer(b)
}
```

### Hygienic Macros
Same syntax as the language, using `^` for argument injection:
```outrun
macro unless(condition, do_block) {
    if !^condition {
        ^do_block
    }
}
```

## Development Workflow

### Adding New Syntax

1. **Update SYNTAX_SPEC.md** with examples and explanation
2. **Update GRAMMAR.bnf** with formal grammar rules
3. **Update tree-sitter grammar** in `tree-sitter-outrun/grammar.js`
4. **Add test cases** in `tree-sitter-outrun/test/corpus/`
5. **Regenerate parser** with `npm run generate` in tree-sitter-outrun

### Working with Tree-sitter

```bash
cd tree-sitter-outrun
npm install
npm run generate              # Generate parser from grammar.js
npm test                     # Run all 178 corpus tests (100% pass rate)
tree-sitter test --update    # Update AST expectations automatically
tree-sitter parse file.outrun # Test parsing specific files
```

### Parser Development

The Rust parser is in `outrun-parser/`. This will eventually become the main compiler frontend.

```bash
cd outrun-parser
cargo build
cargo test
```

## File Extensions

- Source files: `.outrun`
- Package manifest: `outrun.toml`

## Standard Library

Core traits live in the `Outrun.` namespace to avoid conflicts:
- `Outrun.Option<T>`
- `Outrun.Result<T, E>`
- `Outrun.Iterator<T>`
- `Outrun.BinaryAddition<T>`
- etc.

Users must explicitly alias them:
```outrun
alias Outrun.Option as Option
alias Outrun.Result as Result
```

## Testing Syntax Ideas

Create test files in `tree-sitter-outrun/test/corpus/` to validate syntax:

```
================
Basic struct definition
================

struct User(name: String, email: String) {
    def greet(self: Self): String {
        "Hello, #{self.name}!"
    }
}

---

(source_file
  (struct_definition
    (type_identifier)
    (struct_fields
      (parameter (identifier) (type_annotation))
      (parameter (identifier) (type_annotation)))
    (struct_impl_block
      (function_definition
        (identifier)
        (parameter_list
          (parameter (identifier) (type_annotation)))
        (return_type (type_annotation))
        (function_body
          (string_literal))))))
```

## Current Status

- ✅ Core syntax specification complete
- ✅ BNF grammar written
- ✅ Tree-sitter grammar implemented
- ✅ **COMPLETE: Comprehensive test coverage with 178 tests (100% pass rate)**
- ✅ **All SYNTAX_SPEC.md features implemented and tested**
- ✅ **Pest parser implementation**: Complete string interpolation with expression parsing
- 🔄 Parser implementation ongoing (logical operators next)
- ⭐ Type checker needed  
- ⭐ Interpreter/compiler needed

## Tree-sitter Implementation Achievement 🎉

**COMPLETED DECEMBER 2024**: Full tree-sitter grammar implementation with comprehensive test coverage!

### Test Coverage Statistics
- **178 individual test files** covering all syntax features
- **100% pass rate** (178/178 tests passing)
- **Complete feature coverage**: All SYNTAX_SPEC.md features implemented and tested
- **Systematic testing approach**: One test per file for maintainability

### Features Implemented and Tested
- ✅ **Core Literals**: Integers (decimal/binary/hex/octal), floats, booleans, strings, atoms
- ✅ **Collections**: Lists, maps, tuples with proper syntax
- ✅ **Operators**: Arithmetic, comparison, logical, bitwise, unary, pipe operators
- ✅ **Control Flow**: If/else, case statements with guards
- ✅ **Functions**: Definitions, calls, guards, anonymous functions
- ✅ **Types**: Struct definitions, trait definitions, generics, constraints
- ✅ **Implementations**: Trait implementations with generics and constraints
- ✅ **Advanced Features**: Destructuring, string interpolation, multi-line strings
- ✅ **Meta Features**: Comments, attributes, sigils (embedded DSLs)
- ✅ **Module System**: Alias/import statements, nested module paths
- ✅ **Macros**: Macro definitions with argument injection (`^`)
- ✅ **Application Pattern**: Application trait for entry points
- ✅ **Operator Precedence**: Complete precedence hierarchy with associativity testing

### Key Learnings and Best Practices

#### Test Organisation
- **One test per file rule**: Makes editing AST expectations much easier
- **Descriptive naming**: `category_subcategory_specific.txt` pattern
- **Systematic approach**: Implement feature → test basic case → create comprehensive tests → verify all pass

#### Grammar Development Workflow
1. Check existing grammar rules with `rg` to understand current structure
2. Test parsing with `echo 'code' | tree-sitter parse` for quick validation
3. Create test files with expected AST structure
4. Use `tree-sitter test --update` to automatically fix AST expectations
5. Run full test suite to ensure no regressions

#### Critical Commands
```bash
# Essential development commands
cd tree-sitter-outrun
npm run generate              # Regenerate parser from grammar.js
npm test                     # Run all corpus tests
tree-sitter test --update    # Update AST expectations automatically
tree-sitter parse file.outrun # Test parsing specific files
```

#### Grammar Design Insights
- **Clean AST structure**: Specific node types for operators (`binary_add`, `unary_minus`) vs generic expressions
- **Named parameter enforcement**: All function calls require named parameters, matching language design
- **Proper precedence handling**: Carefully structured operator hierarchy prevents parsing conflicts
- **Hygienic macro support**: `^` injection syntax works correctly with existing expression parsing

## Pest Parser Implementation Status

### Completed Features ✅
- **Basic literals**: Keywords, booleans, integers (all formats), floats (all formats), strings, atoms
- **String features**: Basic strings, multiline strings, escape sequences, full expression interpolation
- **Sigil literals**: Embedded DSL support with `~TypeName"content"` syntax with interpolation
- **Collections**: Lists, maps, tuples with proper syntax and nesting
- **Expression parsing**: Complete operator precedence with arithmetic and comparison operators
- **Source preservation**: Complete AST with spans and format tracking
- **Comprehensive testing**: 142 tests across all implemented features (100% pass rate)

### String Interpolation Implementation Notes

**Current Implementation (v2 - Full Expression Parsing) ✅ COMPLETE:**
```pest
string_interpolation = { "#{" ~ expression ~ "}" }
```

**What Works:**
- ✅ **Simple variables**: `#{name}`
- ✅ **Arithmetic expressions**: `#{a + b * c}` with proper precedence
- ✅ **Comparison expressions**: `#{x > y}`, `#{a == b}`
- ✅ **Parentheses**: `#{(a + b) * c}` for precedence override
- ✅ **Nested collections**: `#{[1, 2, 3]}`, `#{{"key": value}}`
- ✅ **Complete operator support**: All arithmetic and comparison operators

**Current Limitations (future work):**
- **Field access**: `#{user.name}` (requires postfix expression parsing)
- **Function calls**: `#{capitalize(name)}` (requires function call parsing)
- **Logical operators**: `#{a && b || c}` (requires logical operator implementation)
- **Pipe operators**: `#{value |> transform}` (requires pipe operator parsing)

**Implementation Notes:**
- Full expression parsing using existing precedence hierarchy
- AST stores `Box<Expression>` instead of raw strings
- Perfect source reconstruction via Display traits
- All interpolated expressions validate at parse time

### Sigil Literals Implementation

**Current Implementation (v1 - Complete):**
```pest
sigil = { "~" ~ type_identifier ~ (string_multiline | string_basic) }
```

**Features:**
- **Full string interpolation support**: `~SQL"SELECT * FROM #{table_name}"`
- **Multiline support**: `~SQL"""..."""` with interpolation
- **Multiple DSL types**: SQL, JSON, HTML, Regex, and any custom type
- **Trait-based**: `~SQL"..."` desugars to `SQL.parse("...")`
- **Source preservation**: Perfect round-trip via Display trait

**Examples:**
```outrun
let query = ~SQL"SELECT * FROM users WHERE id = #{user_id}"
let template = ~HTML"""
<div class="user">
  <h1>#{user.name}</h1>
  <p>#{user.bio}</p>
</div>
"""
let pattern = ~Regex"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
```

### Implementation Progress Summary

**✅ COMPLETED (December 2024):**
- ✅ **All literal types**: Keywords, booleans, integers (4 formats), floats (2 formats)
- ✅ **String system**: Basic strings, multiline strings, escape sequences
- ✅ **String interpolation**: `#{expression}` syntax with full expression parsing
- ✅ **Atom literals**: Simple (`:symbol`) and quoted (`:"string"`) formats
- ✅ **Sigil literals**: Embedded DSL support (`~TypeName"content"`) with interpolation
- ✅ **Collections**: Lists `[1,2,3]`, Maps `{key: value}`, Tuples `(a,b,c)`
- ✅ **Expression parsing**: Complete operator precedence hierarchy
- ✅ **Source preservation**: Complete AST with spans and format tracking
- ✅ **Comprehensive testing**: 142 tests with 100% pass rate

**🎯 IMMEDIATE NEXT PRIORITY:**
- **Logical operators** (`&&`, `||`, `!`) - Essential for conditional expressions

**📋 REMAINING CORE FEATURES:**
- **Logical & bitwise operators** - `&&`, `||`, `!`, `&`, `|`, `^`, `~`, `<<`, `>>`
- **Pipe operators** - `|>`, `|?` for functional composition
- **Function calls** - Named parameters, postfix syntax
- **Function definitions** - `def`, parameter lists, return types, guards
- **Control flow** - `if/else`, `case/when` statements
- **Type definitions** - `struct`, `trait`, `impl` blocks
- **Module system** - `alias`, `import` statements

### Next Implementation Priorities
1. **Logical operators** (`&&`, `||`, `!`) - Essential for conditional expressions
2. **Bitwise operators** (`&`, `|`, `^`, `~`, `<<`, `>>`) - Complete operator set
3. **Pipe operators** (`|>`, `|?`) - Functional composition support
4. **Function calls** with named parameters - Core language constructs
5. **Function definitions** (def) - Enable actual programs
6. **Control flow** (if/else, case/when) - Essential language features
7. **Type definitions** (structs, traits, impl blocks) - Advanced type system

## Next Steps

1. ~~**Improve tree-sitter grammar**~~ ✅ **COMPLETED**
2. ~~**Write comprehensive example programs**~~ ✅ **COMPLETED via 178 test cases**
3. ~~**Build basic Rust parser**~~ ✅ **COMPLETED for all literal types and sigils**
4. **Implement collections parsing** (lists, maps, tuples) - immediate next priority
5. **Implement full expression parsing** for interpolations and general use
6. **Add functions and control flow** to the parser
7. **Implement type definitions** (structs, traits, impl blocks)
8. **Create interpreter** for rapid iteration and language validation
9. **Design standard library** with core traits (Option, Result, Iterator, etc.)
10. **Implement compiler** targeting WASM/native via Cranelift

## Contributing

When working on the language:

1. **Syntax changes** require updates to both SYNTAX_SPEC.md and GRAMMAR.bnf
2. **Test thoroughly** with tree-sitter corpus tests
3. **Keep "everything is traits"** philosophy consistent
4. **Maintain immutability** and functional approach
5. **Document design decisions** in commit messages

## Useful Commands

```bash
# Generate tree-sitter parser
cd tree-sitter-outrun && npm run generate

# Test tree-sitter grammar  
cd tree-sitter-outrun && npm test

# Build Rust parser
cd outrun-parser && cargo build

# Format files (when formatter exists)
outrun fmt

# Run tests (when test framework exists)  
outrun test
```