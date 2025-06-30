# Outrun Language Extension for Zed

Complete Tree-sitter based syntax highlighting, code navigation, and intelligent editing features for the Outrun programming language in Zed editor.

## Installation

### Method 1: From Extensions Panel (Future)

Once published to Zed's extension registry:

1. Open Zed and install the Outrun extension via the Extensions panel:
   
   ```
   Cmd+Shift+P → "Extensions: Install Extension" → Search for "Outrun"
   ```

### Method 2: Manual Installation (Current)

For development and testing:

1. Clone this repository:
   ```bash
   git clone https://harton.dev/outrun/outrun.git
   cd outrun/editor-support/zed
   ```

2. In Zed, press `Cmd+Shift+P` and select "Extensions: Install Dev Extension"
3. Select the `editor-support/zed` directory

## Verification

Create a test file and open it in Zed:

```bash
echo 'def test(): Boolean { true }' > test.outrun
# Open test.outrun in Zed
```

You should see proper syntax highlighting with keywords, types, and literals coloured correctly.

## Features

- **Syntax Highlighting**: Complete highlighting for all Outrun language constructs
- **Code Navigation**: Smart text objects and navigation
- **Auto-Indentation**: Automatic indentation based on language structure  
- **Bracket Matching**: Auto-completion and matching of brackets, quotes, and sigils
- **Comment Support**: Line (`#`) and block (`###`) comment support
- **String Interpolation**: Highlighting of `#{}` expressions within strings

## Usage

Open any `.outrun` file with Zed and enjoy:

- Syntax highlighting for keywords, types, functions, and literals
- Auto-completion of brackets and quotes
- Smart indentation with 2-space formatting
- Proper comment toggling with `Cmd+/`

## Structure

- `extension.toml` - Extension metadata and configuration
- `languages/outrun/config.toml` - Language-specific settings  
- `languages/outrun/*.scm` - Tree-sitter query files (symlinked to main grammar)

## Development

The Tree-sitter queries are symlinked to the main `tree-sitter-outrun/queries/` directory to ensure consistency across all editor integrations.

## Troubleshooting

**Extension not installing:**
- Ensure you have the latest version of Zed
- Try manually installing via "Install Dev Extension" as described above
- Check Zed's extension logs for error messages

**No syntax highlighting:**
- Verify the extension is installed via Extensions panel
- Check that files have the `.outrun` extension
- Restart Zed after installing the extension

**Grammar compilation errors:**
- Ensure the tree-sitter grammar repository is accessible
- Check that the commit SHA in `extension.toml` is valid
- Try reinstalling the extension

**File not recognised as Outrun:**
- Ensure your files have the `.outrun` extension
- Check that the editor configuration includes `outrun` in file types