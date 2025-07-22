; Keywords
[
  "struct"
  "protocol"
  "impl"
  "def"
  "defs"
  "defp"
  "let"
  "const"
  "fn"
  "if"
  "else"
  "case"
  "when"
  "alias"
  "macro"
  "import"
  "for"
  "only"
  "except"
  "as"
] @keyword

; Special keywords
"Self" @type.builtin

; Boolean literals
[
  "true"
  "false"
] @constant.builtin.boolean

; Types
(type_identifier) @type
(module_path (type_identifier) @type)

; Function definitions - use more specific selectors
(function_definition
  (identifier) @function)

(static_function_definition
  (identifier) @function)

(function_signature
  (identifier) @function)

; Constants
(constant_definition
  (type_identifier) @constant)

; Numbers
(integer) @number
(float) @number

; Strings
(string) @string
(multiline_string) @string

; String interpolation
(string_interpolation
  "#{" @punctuation.special
  "}" @punctuation.special)

; Escape sequences
(escape_sequence) @escape

; Atoms
(atom) @constant.character

; Sigils
(sigil) @string.special

; Comments
(comment) @comment
(block_comment) @comment

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "**"
  "=="
  "!="
  ">"
  ">="
  "<"
  "<="
  "&&"
  "||"
  "!"
  "&"
  "|"
  "^"
  "~"
  "<<"
  ">>"
] @operator

; Pipe operators
[
  "|>"
  "|?"
] @operator

; Assignment
"=" @operator

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  ":"
  "."
  ".."
  "..?"
  "=>"
  "->"
] @punctuation.delimiter

; Attributes
(attribute
  "@" @punctuation.special
  (identifier) @attribute)

; Parameters
(parameter_list
  (identifier) @variable.parameter)

; Identifiers (keep this last so more specific patterns take precedence)
(identifier) @variable
