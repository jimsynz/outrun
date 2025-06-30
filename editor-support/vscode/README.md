# Outrun Language Support for Visual Studio Code

Complete language support for the [Outrun programming language](https://harton.dev/outrun/outrun) in Visual Studio Code.

## Features

### ✨ Syntax Highlighting
- **Complete highlighting** for all Outrun language constructs
- **String interpolation** support with `#{expression}` highlighting
- **Sigil literals** with type-aware highlighting (`~Type"content"`)
- **Comments** - both line (`#`) and block (`###`) comments
- **Keywords, operators, and literals** with accurate scoping
- **Module paths** and type annotations
- **Atoms** and **numeric literals** (decimal, hex, binary, octal)

### 🎯 Smart Editing Features
- **Auto-closing pairs** for brackets, quotes, and Outrun-specific constructs
- **Auto-indentation** based on language structure
- **Bracket matching** and highlighting
- **Comment toggling** with `Cmd+/` (line) and `Cmd+Shift+A` (block)
- **Code folding** with region markers
- **Smart word selection** respecting Outrun identifier patterns

### 📝 Code Snippets
Comprehensive snippet collection for rapid development:

#### Core Language Constructs
- `def` - Function definition
- `defwhen` - Guard function with conditions
- `struct` - Struct definition
- `trait` - Trait definition
- `impl` - Implementation block
- `case` - Pattern matching
- `let` - Variable binding
- `const` - Constants

#### Control Flow & Patterns
- `if` - If-else expressions
- `casewhen` - Case with guard conditions
- `pipe` - Pipe operator (`|>`)
- `maybepipe` - Maybe pipe (`|?`)

#### Imports & Organization
- `import` - Module imports
- `importas` - Import with alias
- `importonly` - Selective imports
- `alias` - Type aliases

#### Common Types & Literals
- `some`/`none` - Option types
- `ok`/`error` - Result types
- `list` - List literals
- `map` - Map literals
- `sigil` - Sigil literals
- `atom` - Atom literals

### 🔧 Language Configuration
- **Word patterns** optimized for Outrun identifiers
- **Indentation rules** for functions, structs, traits, and case expressions
- **Auto-closing** for interpolation (`#{...}`) and block comments
- **Bracket definitions** including generics (`<>`)

## Installation

### From VS Code Marketplace (Coming Soon)
1. Open VS Code
2. Go to Extensions (`Cmd+Shift+X` / `Ctrl+Shift+X`)
3. Search for "Outrun Language Support"
4. Click Install

### Manual Installation (Development)
1. Clone the Outrun repository:
   ```bash
   git clone https://harton.dev/outrun/outrun.git
   cd outrun/editor-support/vscode
   ```

2. Install dependencies and compile:
   ```bash
   npm install
   npm run compile
   ```

3. Install the extension:
   ```bash
   # Link the extension for development
   ln -sf $(pwd) ~/.vscode/extensions/outrun-language

   # Or package and install
   npm install -g vsce
   vsce package
   code --install-extension outrun-language-*.vsix
   ```

4. Restart VS Code and open any `.outrun` file

## Usage

### Basic Setup
Once installed, the extension automatically activates when you open any `.outrun` file. The language will be detected by the file extension.

### Example Code
Create a new file with `.outrun` extension and try this example:

```outrun
# Function with guard condition
def divide(a: Integer, b: Integer): Float
when Integer.non_zero?(b) {
    Float.from_integer(a) / Float.from_integer(b)
}

# Struct definition
struct Person(name: String, age: Integer)

# Trait implementation
impl Display<Person> for Person {
    def display(person: Person): String {
        "#{person.name} is #{person.age} years old"
    }
}

# Pattern matching with guards
def categorize_age(age: Integer): String {
    case age {
        young when Integer.less_than?(young, 18) -> "Minor"
        adult when Integer.between?(adult, 18, 65) -> "Adult"
        _ -> "Senior"
    }
}

# Using Option and Result types
let maybe_person = Option.some(value: Person { name: "Alice", age: 30 })
let result = maybe_person
    |? Person.display
    |> Result.ok
```

### Snippets Usage
Type any snippet prefix and press `Tab` to expand:

- Type `def` + `Tab` → Function definition template
- Type `struct` + `Tab` → Struct definition template
- Type `impl` + `Tab` → Implementation block template
- Type `case` + `Tab` → Pattern matching template

### Code Formatting
The extension sets recommended formatting defaults:
- **Indentation**: 4 spaces (configurable)
- **Line endings**: Auto-detect
- **Trim whitespace**: Enabled
- **Rulers**: 80 and 120 character guides

## Configuration

### Language-Specific Settings
Add these to your VS Code `settings.json` for optimal Outrun development:

```json
{
  "[outrun]": {
    "editor.insertSpaces": true,
    "editor.tabSize": 4,
    "editor.wordWrap": "bounded",
    "editor.wordWrapColumn": 100,
    "editor.rulers": [80, 120],
    "files.trimTrailingWhitespace": true
  }
}
```

### Custom Keybindings
Add custom keybindings for Outrun-specific actions:

```json
[
  {
    "key": "ctrl+shift+p",
    "command": "editor.action.insertSnippet",
    "when": "editorTextFocus && editorLangId == 'outrun'",
    "args": {
      "snippet": "${1:value} |> ${2:function}"
    }
  }
]
```

## Features Comparison

| Feature | Status | Description |
|---------|---------|-------------|
| **Syntax Highlighting** | ✅ Complete | All language constructs highlighted |
| **Auto-completion** | 🚧 Planned | Context-aware suggestions |
| **Error Checking** | 🚧 Planned | Real-time syntax/type errors |
| **Go to Definition** | 🚧 Planned | Navigate to symbol definitions |
| **Hover Information** | 🚧 Planned | Type info and documentation |
| **Refactoring** | 🚧 Planned | Rename, extract, etc. |
| **Debugging** | 🚧 Planned | Integrated debugger support |

✅ = Available now
🚧 = Planned for future releases

## Language Server (Future)

We're planning a full Language Server Protocol implementation that will provide:

- **Real-time error checking** using the Outrun type checker
- **Intelligent auto-completion** with context awareness
- **Go to definition/references** across modules
- **Hover documentation** with type information
- **Refactoring support** (rename, extract function, etc.)
- **Code formatting** with official Outrun formatter
- **Import organization** and dependency management

## Troubleshooting

### Extension Not Activating

**Problem**: Syntax highlighting not working for `.outrun` files

**Solutions**:
1. Check that the file extension is exactly `.outrun`
2. Restart VS Code after installation
3. Check the language mode in the bottom-right corner of VS Code
4. Manually set language: `Cmd+Shift+P` → "Change Language Mode" → "Outrun"

### Snippets Not Working

**Problem**: Tab completion not expanding snippets

**Solutions**:
1. Ensure `editor.tabCompletion` is set to `"on"` or `"onlySnippets"`
2. Try `Cmd+Shift+P` → "Insert Snippet" to browse available snippets
3. Check that you're in an `.outrun` file

### Incorrect Syntax Highlighting

**Problem**: Some code highlighted incorrectly

**Solutions**:
1. Try reloading the window: `Cmd+Shift+P` → "Developer: Reload Window"
2. Check for conflicting extensions that might override Outrun highlighting
3. File an issue with a code sample on our [GitHub repository](https://harton.dev/outrun/outrun/issues)

### Performance Issues

**Problem**: VS Code slow with large Outrun files

**Solutions**:
1. Disable semantic highlighting: `"editor.semanticHighlighting.enabled": false`
2. Increase tokenization timeout: `"editor.maxTokenizationLineLength": 20000`
3. Consider breaking large files into smaller modules

## Development

### Building from Source
```bash
git clone https://harton.dev/outrun/outrun.git
cd outrun/editor-support/vscode
npm install
npm run compile
```

### Testing
```bash
# Open extension in development mode
code --extensionDevelopmentPath=.

# Package for distribution
vsce package
```

### Contributing
1. Fork the repository
2. Create a feature branch
3. Make your changes to the extension
4. Test thoroughly with sample Outrun code
5. Submit a pull request

See the main [Outrun contributing guide](https://harton.dev/outrun/outrun/blob/main/CONTRIBUTING.md) for more details.

## Related Extensions

### Recommended Extensions
- **Error Lens** - Inline error/warning display
- **Bracket Pair Colorizer** - Enhanced bracket highlighting
- **GitLens** - Git integration and history
- **Todo Highlight** - Highlight TODO comments

### Theme Recommendations
The extension works with all VS Code themes, but these complement Outrun's syntax well:
- **One Dark Pro**
- **Dracula Official**
- **Material Theme**
- **Tokyo Night**

## Support

- **Documentation**: [Outrun Language Specification](https://harton.dev/outrun/outrun/blob/main/LANGUAGE_SPEC.md)
- **Issues**: [GitHub Issues](https://harton.dev/outrun/outrun/issues)
- **Discussions**: [GitHub Discussions](https://harton.dev/outrun/outrun/discussions)
- **Chat**: [Discord Server](https://discord.gg/outrun-lang) (coming soon)

## License

This extension is licensed under the MIT License. See the [LICENSE](https://harton.dev/outrun/outrun/blob/main/LICENSE) file for details.

## Changelog

### 0.1.0 (Initial Release)
- ✨ Complete syntax highlighting for all Outrun constructs
- ✨ Comprehensive snippet collection (35+ snippets)
- ✨ Smart auto-indentation and bracket matching
- ✨ Language configuration with Outrun-specific rules
- ✨ Support for string interpolation and sigil highlighting
- ✨ Comment toggling and code folding

---

**Enjoy coding in Outrun! 🚀**
