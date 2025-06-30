# Outrun Editor Support

This directory contains editor support files for the Outrun programming language.

## Supported Editors

### Helix Editor

Complete Tree-sitter based syntax highlighting, code navigation, and indentation support.

**📁 [See helix/README.md for detailed installation instructions](helix/README.md)**

#### Quick Start

1. Copy `helix/languages.toml` to your Helix config directory
2. Run `hx --grammar build` to build the grammar
3. Open any `.outrun` file and enjoy syntax highlighting

#### Features

- **Syntax Highlighting**: Complete highlighting for all Outrun language constructs
- **Code Navigation**: Textobjects for functions, types, parameters, and comments  
- **Smart Indentation**: Automatic indentation based on language structure
- **Comment Support**: Line (`#`) and block (`###`) comment support

### Zed Editor

Complete Tree-sitter based syntax highlighting, code navigation, and intelligent editing features.

**📁 [See zed/README.md for detailed installation instructions](zed/README.md)**

#### Quick Start

1. Clone this repository and navigate to `editor-support/zed`
2. In Zed: `Cmd+Shift+P` → "Extensions: Install Dev Extension" → Select the zed directory
3. Open any `.outrun` file and enjoy syntax highlighting

#### Features

- **Syntax Highlighting**: Complete highlighting for all Outrun language constructs
- **Code Navigation**: Smart text objects and navigation
- **Auto-Indentation**: Automatic indentation based on language structure  
- **Bracket Matching**: Auto-completion and matching of brackets, quotes, and sigils
- **Comment Support**: Line (`#`) and block (`###`) comment support

### Sublime Text

Basic syntax highlighting support via TextMate-style grammar.

#### Installation

1. Copy the syntax file to your Sublime Text packages directory:

   ```bash
   # For Sublime Text 4 on macOS
   cp editor-support/outrun.sublime-syntax ~/Library/Application\ Support/Sublime\ Text/Packages/User/

   # For Sublime Text 4 on Linux
   cp editor-support/outrun.sublime-syntax ~/.config/sublime-text/Packages/User/

   # For Sublime Text 4 on Windows
   cp editor-support/outrun.sublime-syntax %APPDATA%/Sublime\ Text/Packages/User/
   ```

2. Restart Sublime Text or run "Reload Settings" from the command palette.

#### Features

- Syntax highlighting for keywords, types, functions, and literals
- String interpolation highlighting
- Comment highlighting
- Operator and punctuation highlighting

## Troubleshooting

### Helix

**📁 [See helix/README.md for detailed troubleshooting](helix/README.md#troubleshooting)**

### Zed

**📁 [See zed/README.md for detailed troubleshooting](zed/README.md#troubleshooting)**

### General

**File not recognised as Outrun:**
- Ensure your files have the `.outrun` extension
- Check that the editor configuration includes `outrun` in file types

## Contributing

When updating the grammar:

1. Edit `tree-sitter-outrun/grammar.js`
2. Run `tree-sitter generate` to regenerate the parser
3. Test with sample Outrun code using `tree-sitter parse example.outrun`
4. Update query files if new language constructs were added
5. Test the integration in your editor

### Continuous Integration

The Tree-sitter grammar is automatically built and tested in CI. The CI pipeline:

1. **Generates** the parser from `grammar.js`
2. **Builds** the grammar library
3. **Validates** parsing of sample files including core library modules

This ensures the grammar stays in sync with language changes and catches parsing regressions early.

## Files

- `tree-sitter-outrun/` - Complete Tree-sitter grammar and query files
- `helix/` - Helix editor configuration files
- `zed/` - Zed editor extension files
- `outrun.sublime-syntax` - Sublime Text syntax highlighting
- `README.md` - This documentation
