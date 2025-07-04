// Prism.js syntax highlighting for Outrun programming language
Prism.languages.outrun = {
    'comment': [
        {
            pattern: /\/\*[\s\S]*?\*\//,
            greedy: true
        },
        {
            pattern: /\/\/.*/,
            greedy: true
        },
        {
            pattern: /#.*/,
            greedy: true
        }
    ],
    'string': [
        {
            pattern: /"(?:\\[\s\S]|[^\\"])*"/,
            greedy: true,
            inside: {
                'interpolation': {
                    pattern: /#{[^}]*}/,
                    inside: {
                        'punctuation': /^#{|}$/,
                        'expression': {
                            pattern: /[\s\S]+/,
                            inside: Prism.languages.outrun
                        }
                    }
                }
            }
        },
        {
            pattern: /'''[\s\S]*?'''/,
            greedy: true,
            alias: 'multiline-string'
        },
        {
            pattern: /~[a-zA-Z_]\w*"[^"]*"/,
            greedy: true,
            alias: 'sigil'
        }
    ],
    'keyword': /\b(?:struct|trait|impl|def|defs|defp|let|const|fn|if|else|case|when|alias|macro|import|for|only|except|as)\b/,
    'builtin': /\b(?:Self|true|false)\b/,
    'atom': /:[a-zA-Z_]\w*/,
    'type': /\b[A-Z][a-zA-Z0-9_]*\b/,
    'function': /\b[a-zA-Z_]\w*(?=\s*\()/,
    'number': /\b(?:0x[0-9a-fA-F]+|0b[01]+|0o[0-7]+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)\b/,
    'operator': [
        /[+\-*\/%]=?/,
        /[!=]=?/,
        /[<>]=?/,
        /&&|\|\||!/,
        /[&|^~]/,
        /<<|>>/,
        /\*\*/,
        /\|[>?]/,
        /=>/,
        /->/,
        /\.\./,
        /\.\.\?/
    ],
    'punctuation': /[{}[\]();,.:]|@/,
    'attribute': /@[a-zA-Z_]\w*/
};

// Add custom styling for synthwave theme
Prism.hooks.add('wrap', function(env) {
    if (env.language === 'outrun') {
        if (env.type === 'keyword') {
            env.classes.push('synthwave-keyword');
        } else if (env.type === 'function') {
            env.classes.push('synthwave-function');
        } else if (env.type === 'string' || env.type === 'sigil') {
            env.classes.push('synthwave-string');
        } else if (env.type === 'type') {
            env.classes.push('synthwave-type');
        } else if (env.type === 'atom') {
            env.classes.push('synthwave-atom');
        } else if (env.type === 'number') {
            env.classes.push('synthwave-number');
        } else if (env.type === 'operator') {
            env.classes.push('synthwave-operator');
        } else if (env.type === 'attribute') {
            env.classes.push('synthwave-attribute');
        }
    }
});