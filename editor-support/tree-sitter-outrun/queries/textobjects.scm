; Functions
(function_definition) @function.around
(function_definition
  (function_body) @function.inside)

(static_function_definition) @function.around
(static_function_definition
  (function_body) @function.inside)

; Types (structs, protocols)
(struct_definition) @class.around
(struct_definition
  (struct_fields) @class.inside)

(protocol_definition) @class.around
(protocol_definition
  (_) @class.inside)

(impl_block) @class.around
(impl_block
  (_) @class.inside)

; Parameters
(parameter_list) @parameter.around
(parameter_list
  (identifier) @parameter.inside)

; Arguments in function calls
(argument_list) @parameter.around
(argument_list
  (_) @parameter.inside)

; Anonymous function parameters
(anonymous_params) @parameter.around

; Comments
(comment) @comment.around
(comment) @comment.inside

(block_comment) @comment.around
(block_comment) @comment.inside

; Conditional blocks
(if_expression) @conditional.around

; Case expressions
(case_expression) @conditional.around
(case_clause
  (_) @conditional.inside)

(protocol_case_clause
  (_) @conditional.inside)

; Loop constructs (for when they're added)
; For now, anonymous functions can serve as iteration blocks
(anonymous_function) @loop.around
(anonymous_function
  (_) @loop.inside)

; Test - not applicable to Outrun currently, but reserved for future use

; Entry - treat each top-level item as an entry
(struct_definition) @entry.around
(protocol_definition) @entry.around
(impl_block) @entry.around
(function_definition) @entry.around
(constant_definition) @entry.around
(alias_definition) @entry.around
(macro_definition) @entry.around
(import_definition) @entry.around
