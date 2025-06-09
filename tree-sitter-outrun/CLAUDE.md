# Tree-sitter Outrun Grammar

## Overview

This directory contains the tree-sitter grammar implementation for the Outrun programming language. Tree-sitter is used to provide syntax highlighting, code folding, and language server support.

## Project Structure

```
tree-sitter-outrun/
├── grammar.js              # Main grammar definition
├── src/                    # Generated parser files
│   ├── parser.c
│   ├── grammar.json
│   └── node-types.json
├── test/
│   └── corpus/             # Test cases for grammar validation
├── bindings/               # Language bindings
│   ├── c/
│   ├── go/
│   ├── node/
│   ├── python/
│   ├── rust/
│   └── swift/
└── package.json
```

## Development Workflow

### Making Grammar Changes

1. Edit `grammar.js` to add or modify syntax rules
2. Run `npm run generate` to regenerate the parser
3. Add test cases in `test/corpus/` to validate the changes
4. Run `npm test` to verify all tests pass

### Test File Guidelines

**IMPORTANT**: Each test file in `test/corpus/` should contain exactly ONE test case. This makes editing and maintaining tests much easier and eliminates bracket counting issues.

Test files should follow this naming convention:
- `category_subcategory_specific.txt` (e.g., `literals_integers_positive.txt`)
- Use descriptive names that clearly indicate what is being tested

Example test file structure:
```
================
Test description
================

source code here

---

(expected_ast_structure)
```

### Running Tests

```bash
# Install dependencies
npm install

# Generate parser from grammar
npm run generate

# Run all corpus tests
npm test

# Run specific test
npm test -- --filter "test_name"
```

### Grammar Rules

The grammar follows these principles:
- Clean, minimal AST structure
- Specific node types for operators (e.g., `binary_add`, `unary_minus`)
- Named parameters for function calls
- Support for all Outrun language features as defined in SYNTAX_SPEC.md

### Test Coverage

Current test coverage includes:
- Basic literals (integers, floats, booleans, strings, atoms)
- Collection literals (lists, maps, tuples)
- Operators with correct precedence and associativity
- Function definitions and calls
- Control flow (if/else, case statements)
- Struct definitions and construction

See TREE_SITTER_PLAN.md in the parent directory for complete coverage status and planned additions.

## Useful Commands

```bash
# Generate parser
npm run generate

# Run tests
npm test

# Build all language bindings
npm run build

# Install tree-sitter CLI globally
npm install -g tree-sitter-cli

# Parse a file for debugging
tree-sitter parse example.outrun
```