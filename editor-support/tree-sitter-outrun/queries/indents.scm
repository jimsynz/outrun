; Increase indent for block-like structures
[
  (function_body)
  (struct_definition)
  (protocol_definition)
  (impl_block)
  (if_expression)
  (case_expression)
  (concrete_case_expression)
  (protocol_case_expression)
  (anonymous_function)
  (macro_definition)
] @indent

; Opening braces increase indent
[
  "{"
  "["
  "("
] @indent

; Closing braces decrease indent
[
  "}"
  "]"
  ")"
] @outdent

; Case clauses and arrow expressions
[
  (case_clause)
  (protocolcol_case_clause)
  (anonymous_clause)
] @indent

; Function definitions with bodies
(function_definition
  (function_body) @indent)

(static_function_definition
  (function_body) @indent)

; Struct fields and implementations
(struct_definition
  (struct_fields) @indent)

; Multi-line expressions that should be indented
(pipe_expression) @indent
(logical_or_expression) @indent
(logical_and_expression) @indent

; Multi-line function calls
(postfix_expression
  (argument_list) @indent)

; Multi-line collections
[
  (list_literal)
  (map_literal)
  (tuple_literal)
] @indent

; Multi-line type annotations
[
  (function_type)
  (tuple_type)
] @indent
