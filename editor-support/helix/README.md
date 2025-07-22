# Outrun Language Support for Helix Editor

Complete Tree-sitter based syntax highlighting, code navigation, and indentation support for the Outrun programming language in Helix editor.

## Installation

**Important**: Helix requires Tree-sitter language version 13-14, but newer Tree-sitter CLI versions (0.21+) generate version 15+ grammars. You must use Tree-sitter CLI version 0.20.8 or earlier.

### Method 1: Using Git Repository (Recommended)

This method uses the updated `languages.toml` configuration that references the git repository directly.

1. **Install Compatible Tree-sitter Version**

   ```bash
   # Install Tree-sitter 0.20.8 (compatible with Helix)
   asdf install tree-sitter 0.20.8
   asdf set tree-sitter 0.20.8

   # Verify version
   tree-sitter --version  # Should show 0.20.8
   ```

2. **Configure Helix**

   Copy the language configuration to your Helix config directory:

   ```bash
   # Copy language configuration
   cp editor-support/helix/languages.toml ~/.config/helix/languages.toml
   ```

   Or if you already have a `languages.toml`, append the contents to your existing file.

3. **Build Grammar in Helix**

   ```bash
   # Tell Helix to build the grammar
   hx --grammar build
   ```

   This should show "1 grammars built now: ["outrun"]"

4. **Verify Installation**

   ```bash
   # Check if Outrun is recognised
   hx --health | grep outrun
   ```

   You should see ✓ for Highlight, Textobject, and Indent columns.

### Method 2: Manual Installation

If you prefer to build the grammar manually:

1. **Install Compatible Tree-sitter Version** (same as above)

2. **Build the Tree-sitter Grammar**

   ```bash
   # Navigate to grammar directory
   cd editor-support/tree-sitter-outrun

   # Generate and build with compatible version
   tree-sitter generate
   ```

3. **Configure Helix** (same as above)

4. **Install Query Files**

   Copy the query files to the appropriate location:

   ```bash
   # Create queries directory
   mkdir -p ~/.config/helix/runtime/queries/outrun

   # Copy query files
   cp editor-support/tree-sitter-outrun/queries/* ~/.config/helix/runtime/queries/outrun/
   ```

5. **Build Grammar in Helix** (same as above)

## Verification

Create a test file and open it in Helix:

```bash
echo 'def test(): Boolean { true }' > test.outrun
hx test.outrun
```

You should now see proper syntax highlighting with keywords, types, and literals coloured correctly.

## Features

- **Syntax Highlighting**: Complete highlighting for all Outrun language constructs
- **Code Navigation**: Textobjects for functions, types, parameters, and comments
- **Smart Indentation**: Automatic indentation based on language structure
- **Comment Support**: Line (`#`) and block (`###`) comment support

## Usage

Open any `.outrun` file with Helix and enjoy syntax highlighting and code navigation features:

- `]f` / `[f` - Navigate between functions
- `]c` / `[c` - Navigate between classes (structs/protocols)
- `]t` / `[t` - Navigate between types
- `]p` / `[p` - Navigate between parameters

## Troubleshooting

**Tree-sitter version incompatibility:**

- Error: "Incompatible language version 15. Expected minimum 13, maximum 14"
- Solution: Use Tree-sitter 0.20.8: `asdf set tree-sitter 0.20.8` then regenerate grammar

**Grammar not found:**

- Ensure the grammar was built successfully with `hx --grammar build`
- Check that `languages.toml` contains the correct path to the grammar
- Verify query files are in the correct location (if using manual installation)

**No syntax highlighting:**

- Run `hx --health` to check grammar status (should show ✓ for outrun)
- Check Helix logs with `:log-open` for error messages
- Ensure `highlights.scm` exists in the queries directory (manual installation)
- Try rebuilding with `hx --grammar build`
- Verify Tree-sitter version compatibility (must be 0.20.8 or earlier)

**Textobjects not working:**

- Verify `textobjects.scm` exists in the queries directory (manual installation)
- Check that the grammar built successfully

## Configuration

The `languages.toml` file configures:

- File type detection (`.outrun` files)
- Comment syntax (`#` for line comments, `###` for block comments)
- Indentation (2 spaces)
- Grammar source (git repository with subpath)

## Development

When updating the grammar, Helix will automatically fetch changes from the git repository when you run `hx --grammar build`.
