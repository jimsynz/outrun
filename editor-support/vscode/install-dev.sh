#!/bin/bash

# Outrun VS Code Extension - Development Installation Script
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 Outrun VS Code Extension - Development Installation${NC}"
echo "=================================================="

# Check if we're in the right directory
if [[ ! -f "package.json" ]] || [[ ! -f "language-configuration.json" ]]; then
    echo -e "${RED}❌ Error: Please run this script from the vscode extension directory${NC}"
    echo "Expected: outrun/editor-support/vscode/"
    exit 1
fi

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo -e "${RED}❌ Node.js is not installed. Please install Node.js 16+ first.${NC}"
    echo "Visit: https://nodejs.org/"
    exit 1
fi

# Check Node.js version
NODE_VERSION=$(node --version | cut -d'v' -f2 | cut -d'.' -f1)
if [[ $NODE_VERSION -lt 16 ]]; then
    echo -e "${YELLOW}⚠️  Warning: Node.js version 16+ is recommended. Current: $(node --version)${NC}"
fi

# Check if npm is available
if ! command -v npm &> /dev/null; then
    echo -e "${RED}❌ npm is not installed. Please install npm first.${NC}"
    exit 1
fi

# Check if VS Code is installed
if ! command -v code &> /dev/null; then
    echo -e "${YELLOW}⚠️  VS Code 'code' command not found in PATH${NC}"
    echo "You may need to install the code command or install the extension manually"
fi

echo -e "${BLUE}📦 Installing dependencies...${NC}"
npm install

echo -e "${BLUE}🔨 Compiling TypeScript...${NC}"
npm run compile

# Determine VS Code extensions directory
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    VSCODE_EXTENSIONS_DIR="$HOME/.vscode/extensions"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux
    VSCODE_EXTENSIONS_DIR="$HOME/.vscode/extensions"
elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]] || [[ "$OSTYPE" == "win32" ]]; then
    # Windows
    VSCODE_EXTENSIONS_DIR="$APPDATA/Code/User/extensions"
else
    echo -e "${YELLOW}⚠️  Unknown OS type: $OSTYPE${NC}"
    VSCODE_EXTENSIONS_DIR="$HOME/.vscode/extensions"
fi

EXTENSION_NAME="outrun-lang.outrun-language-0.1.0"
EXTENSION_DIR="$VSCODE_EXTENSIONS_DIR/$EXTENSION_NAME"

echo -e "${BLUE}📂 Installing extension to VS Code...${NC}"
echo "Extension directory: $EXTENSION_DIR"

# Create extensions directory if it doesn't exist
mkdir -p "$VSCODE_EXTENSIONS_DIR"

# Remove existing installation if present
if [[ -d "$EXTENSION_DIR" ]] || [[ -L "$EXTENSION_DIR" ]]; then
    echo -e "${YELLOW}🗑️  Removing existing installation...${NC}"
    rm -rf "$EXTENSION_DIR"
fi

# Create symlink for development
echo -e "${GREEN}🔗 Creating development symlink...${NC}"
ln -sf "$(pwd)" "$EXTENSION_DIR"

echo -e "${GREEN}✅ Installation complete!${NC}"
echo ""
echo -e "${BLUE}🎯 Next steps:${NC}"
echo "1. Restart VS Code if it's currently running"
echo "2. Open any .outrun file to test the extension"
echo "3. Try these commands in VS Code:"
echo "   - Cmd+Shift+P → 'Outrun: Show Language Info'"
echo "   - Type 'def' and press Tab to test snippets"
echo ""
echo -e "${BLUE}📝 Testing:${NC}"
echo "• Open: example.outrun (in this directory)"
echo "• Test syntax highlighting and snippets"
echo "• Check language mode in bottom-right corner"
echo ""
echo -e "${BLUE}🔧 Development:${NC}"
echo "• Run 'npm run compile' after making changes to src/"
echo "• Use 'npm run watch' for automatic compilation"
echo "• Extension will reload automatically due to symlink"
echo ""
echo -e "${BLUE}🚫 Uninstall:${NC}"
echo "• Run: rm -rf '$EXTENSION_DIR'"
echo ""

# Test VS Code command availability
if command -v code &> /dev/null; then
    echo -e "${GREEN}🎉 Ready to code in Outrun! Try: code example.outrun${NC}"
else
    echo -e "${YELLOW}💡 Tip: Install VS Code command line tools for better integration${NC}"
fi

echo ""
echo -e "${BLUE}📚 Documentation:${NC}"
echo "• README.md - Full feature documentation"
echo "• Language Spec: ../../LANGUAGE_SPEC.md"
echo "• Issues: https://github.com/outrun-lang/outrun/issues"
