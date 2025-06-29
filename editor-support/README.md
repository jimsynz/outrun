# Outrun Editor Support

This directory contains editor support files for the Outrun programming language.

## Supported Editors

### Helix Editor

Complete Tree-sitter based syntax highlighting, code navigation, and indentation support.

#### Installation

**Important**: Helix requires Tree-sitter language version 13-14, but newer Tree-sitter CLI versions (0.21+) generate version 15+ grammars. You must use Tree-sitter CLI version 0.20.8 or earlier.

1. **Install Compatible Tree-sitter Version**

   ```bash
   # Install Tree-sitter 0.20.8 (compatible with Helix)
   asdf install tree-sitter 0.20.8
   asdf set tree-sitter 0.20.8
   
   # Verify version
   tree-sitter --version  # Should show 0.20.8
   ```

2. **Build the Tree-sitter Grammar**

   ```bash
   # Navigate to grammar directory
   cd editor-support/tree-sitter-outrun
   
   # Generate and build with compatible version
   tree-sitter generate
   ```

3. **Configure Helix**

   Copy the language configuration to your Helix config directory:

   ```bash
   # Copy language configuration
   cp editor-support/helix/languages.toml ~/.config/helix/languages.toml
   ```

   Or if you already have a `languages.toml`, append the contents to your existing file.

4. **Install Query Files**

   Copy the query files to the appropriate location:

   ```bash
   # Create queries directory
   mkdir -p ~/.config/helix/runtime/queries/outrun

   # Copy query files
   cp editor-support/tree-sitter-outrun/queries/* ~/.config/helix/runtime/queries/outrun/
   ```

5. **Build Grammar in Helix**

   ```bash
   # Tell Helix to build the grammar
   hx --grammar build
   ```

   This should show "1 grammars built now: ["outrun"]"

6. **Verify Installation**

   ```bash
   # Check if Outrun is recognised
   hx --health | grep outrun
   ```

   You should see ✓ for Highlight, Textobject, and Indent columns.

7. **Test Syntax Highlighting**

   Create a test file and open it in Helix:

   ```bash
   echo 'def test(): Boolean { true }' > test.outrun
   hx test.outrun
   ```

   You should now see proper syntax highlighting with keywords, types, and literals colored correctly.

#### Features

- **Syntax Highlighting**: Complete highlighting for all Outrun language constructs
- **Code Navigation**: Textobjects for functions, types, parameters, and comments
- **Smart Indentation**: Automatic indentation based on language structure
- **Comment Support**: Line (`#`) and block (`###`) comment support

#### Usage

Open any `.outrun` file with Helix and enjoy syntax highlighting and code navigation features:

- `]f` / `[f` - Navigate between functions
- `]c` / `[c` - Navigate between classes (structs/traits)
- `]t` / `[t` - Navigate between types
- `]p` / `[p` - Navigate between parameters

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

**Tree-sitter version incompatibility:**
- Error: "Incompatible language version 15. Expected minimum 13, maximum 14"
- Solution: Use Tree-sitter 0.20.8: `asdf set tree-sitter 0.20.8` then regenerate grammar

**Grammar not found:**
- Ensure the grammar was built successfully with `hx --grammar build`
- Check that `languages.toml` contains the correct path to the grammar
- Verify query files are in the correct location

**No syntax highlighting:**
- Run `hx --health` to check grammar status (should show ✓ for outrun)
- Check Helix logs with `:log-open` for error messages
- Ensure `highlights.scm` exists in the queries directory
- Try rebuilding with `hx --grammar build`
- Verify Tree-sitter version compatibility (must be 0.20.8 or earlier)

**Textobjects not working:**
- Verify `textobjects.scm` exists in the queries directory
- Check that the grammar built successfully

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
- `outrun.sublime-syntax` - Sublime Text syntax highlighting
- `README.md` - This documentation
