# Tree-sitter Grammar for Outrun

This directory contains the Tree-sitter grammar for the Outrun programming language, providing syntax highlighting and code navigation support for editors like Helix.

## Version Compatibility

**Important**: This grammar requires Tree-sitter CLI version 0.20.8 or earlier to maintain compatibility with Helix editor (language version 13-14). Newer Tree-sitter versions (0.21+) generate incompatible language version 15+ grammars.

## Files

- `grammar.js` - Grammar definition
- `queries/` - Syntax highlighting and editor query files
- `src/` - Generated parser files (do not edit manually)
- `package.json` - Node.js package configuration
- `binding.gyp` - Native binding configuration

## Building

```bash
# Ensure correct Tree-sitter version
asdf set tree-sitter 0.20.8

# Generate parser
tree-sitter generate

# Test parsing (with older CLI)
tree-sitter parse test.outrun
```

For editor integration, see the main [editor-support README](../README.md).