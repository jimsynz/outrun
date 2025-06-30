import * as vscode from "vscode";

export function activate(context: vscode.ExtensionContext) {
  console.log("Outrun Language Support extension is now active");

  // Register commands
  const commands = [
    vscode.commands.registerCommand(
      "outrun.showLanguageInfo",
      showLanguageInfo,
    ),
    vscode.commands.registerCommand("outrun.insertSnippet", insertSnippet),
  ];

  // Add to subscriptions for proper cleanup
  context.subscriptions.push(...commands);

  // Create status bar item
  const statusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    100,
  );
  statusBarItem.command = "outrun.showLanguageInfo";
  context.subscriptions.push(statusBarItem);

  // Update status bar when active editor changes
  const updateStatusBar = () => {
    const editor = vscode.window.activeTextEditor;
    if (editor && editor.document.languageId === "outrun") {
      statusBarItem.text = "$(code) Outrun";
      statusBarItem.tooltip = "Outrun Language Support - Click for info";
      statusBarItem.show();
    } else {
      statusBarItem.hide();
    }
  };

  // Update status bar on editor change
  vscode.window.onDidChangeActiveTextEditor(
    updateStatusBar,
    null,
    context.subscriptions,
  );
  updateStatusBar(); // Initial update

  // Future: Register language server client here
  // Future: Register additional providers (hover, completion, etc.)

  console.log("Outrun Language Support extension activated successfully");
}

export function deactivate() {
  console.log("Outrun Language Support extension deactivated");
}

async function showLanguageInfo() {
  const info = `
Outrun Language Support

Features:
• Syntax highlighting for all Outrun constructs
• Smart auto-indentation and bracket matching
• 35+ code snippets for rapid development
• String interpolation and sigil support
• Comment toggling and code folding

Learn more about Outrun:
• Language Specification: LANGUAGE_SPEC.md
• Grammar Reference: GRAMMAR.bnf
• GitHub: https://harton.dev/outrun/outrun

Version: ${vscode.extensions.getExtension("outrun-lang.outrun-language")?.packageJSON.version || "Unknown"}
    `;

  const selection = await vscode.window.showInformationMessage(
    "Outrun Language Support",
    {
      detail: info.trim(),
      modal: true,
    },
    "Open Documentation",
    "View Snippets",
  );

  if (selection === "Open Documentation") {
    vscode.env.openExternal(
      vscode.Uri.parse(
        "https://harton.dev/outrun/outrun/blob/main/LANGUAGE_SPEC.md",
      ),
    );
  } else if (selection === "View Snippets") {
    vscode.commands.executeCommand("workbench.action.openSnippets", "outrun");
  }
}

async function insertSnippet() {
  const snippets = [
    {
      label: "def - Function definition",
      snippet:
        "def ${1:function_name}(${2:param}: ${3:Type}): ${4:ReturnType} {\n\t$0\n}",
    },
    {
      label: "struct - Struct definition",
      snippet: "struct ${1:StructName}(${2:field}: ${3:Type})",
    },
    {
      label: "trait - Trait definition",
      snippet:
        "trait ${1:TraitName}<${2:T}> {\n\tdef ${3:method_name}(${4:param}: ${5:Type}): ${6:ReturnType}\n}",
    },
    {
      label: "impl - Implementation block",
      snippet:
        "impl ${1:TraitName}<${2:T}> for ${3:Type} {\n\tdef ${4:method_name}(${5:param}: ${6:Type}): ${7:ReturnType} {\n\t\t$0\n\t}\n}",
    },
    {
      label: "case - Pattern matching",
      snippet:
        "case ${1:value} {\n\t${2:pattern} -> ${3:result}\n\t_ -> ${4:default}\n}",
    },
  ];

  const selected = await vscode.window.showQuickPick(snippets, {
    placeHolder: "Select a snippet to insert",
    matchOnDescription: true,
  });

  if (selected) {
    const editor = vscode.window.activeTextEditor;
    if (editor) {
      const snippet = new vscode.SnippetString(selected.snippet);
      editor.insertSnippet(snippet);
    }
  }
}
