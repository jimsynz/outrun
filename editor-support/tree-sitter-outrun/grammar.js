module.exports = grammar({
  name: 'outrun',

  extras: $ => [
    /\s/,
    $.comment,
    $.block_comment,
  ],

  rules: {
    program: $ => repeat($._item),

    _item: $ => choice(
      $.struct_definition,
      $.protocol_definition,
      $.impl_block,
      $.function_definition,
      $.constant_definition,
      $.alias_definition,
      $.macro_definition,
      $.import_definition,
    ),

    // Comments
    comment: $ => token(seq('#', /[^\n]*/)),
    block_comment: $ => token(seq('###', /[^#]*/, '###')),

    // Identifiers
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*\??/,
    type_identifier: $ => /[A-Z][a-zA-Z0-9_]*/,
    module_path: $ => prec.left(seq($.type_identifier, repeat(seq('.', $.type_identifier)))),

    // Keywords
    _keyword: $ => choice(
      'struct', 'protocol', 'impl', 'def', 'defs', 'defp', 'let', 'const',
      'fn', 'if', 'else', 'case', 'when', 'alias', 'macro', 'import',
      'for', 'Self', 'true', 'false', 'only', 'except'
    ),

    // Literals
    integer: $ => choice(
      /[0-9]+/,           // decimal
      /0b[01]+/,          // binary
      /0o[0-7]+/,         // octal
      /0x[0-9a-fA-F]+/    // hexadecimal
    ),

    float: $ => /[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?/,

    boolean: $ => choice('true', 'false'),

    string: $ => seq(
      '"',
      repeat(choice(
        $.string_interpolation,
        $.escape_sequence,
        /[^"\\#]/,
        '#'
      )),
      '"'
    ),

    multiline_string: $ => seq(
      '"""',
      repeat(choice(
        $.string_interpolation,
        $.escape_sequence,
        /[^"\\#]/,
        '"',
        '#'
      )),
      '"""'
    ),

    string_interpolation: $ => seq('#{', $._expression, '}'),

    escape_sequence: $ => token(seq(
      '\\',
      choice(
        /[ntr\\"]/,
        /u[0-9a-fA-F]{4}/
      )
    )),

    atom: $ => seq(':', choice($.identifier, $.string)),

    sigil: $ => seq('~', $.type_identifier, choice($.string, $.multiline_string)),

    // Struct Definition
    struct_definition: $ => seq(
      repeat($.attribute),
      'struct',
      $.module_path,
      optional($.generic_params),
      optional(seq('(', optional($.struct_fields), ')')),
      optional(seq('{', repeat($.function_definition), '}'))
    ),

    struct_fields: $ => seq(
      seq($.identifier, ':', $.type_annotation),
      repeat(seq(',', $.identifier, ':', $.type_annotation))
    ),

    // Protocol Definition
    protocol_definition: $ => seq(
      repeat($.attribute),
      'protocol',
      choice($.type_identifier, $.module_path),
      optional($.generic_params),
      optional($.protocol_constraints),
      '{',
      repeat(choice(
        $.function_signature,
        $.function_definition,
        $.static_function_definition
      )),
      '}'
    ),

    static_function_definition: $ => seq(
      repeat($.attribute),
      'defs',
      $.identifier,
      '(',
      optional($.parameter_list),
      ')',
      optional($.return_type),
      $.function_body
    ),

    // Implementation Block
    impl_block: $ => seq(
      'impl',
      optional($.generic_params),
      $.protocol_spec,
      'for',
      $.type_spec,
      optional($.protocol_constraints),
      '{',
      repeat($.function_definition),
      '}'
    ),

    protocol_spec: $ => seq($.module_path, optional($.generic_args)),
    type_spec: $ => seq($.module_path, optional($.generic_args)),

    // Generic Parameters
    generic_params: $ => seq('<', seq($.type_identifier, repeat(seq(',', $.type_identifier))), '>'),
    generic_args: $ => seq('<', seq($.type_annotation, repeat(seq(',', $.type_annotation))), '>'),

    protocol_constraints: $ => seq('when', $._protocol_constraint_expression),
    _protocol_constraint_expression: $ => prec.left(choice(
      $.protocol_constraint_term,
      seq($._protocol_constraint_expression, choice('&&', '||'), $._protocol_constraint_expression),
      seq('(', $._protocol_constraint_expression, ')')
    )),

    protocol_constraint_term: $ => prec(1, seq(
      choice($.type_identifier, 'Self'),
      ':',
      choice($.type_identifier, $.module_path)
    )),

    // Function Definition
    function_definition: $ => seq(
      repeat($.attribute),
      optional('defp'),
      'def',
      $.identifier,
      '(',
      optional($.parameter_list),
      ')',
      optional($.return_type),
      optional($.guard_clause),
      $.function_body
    ),

    function_signature: $ => seq(
      repeat($.attribute),
      'def',
      $.identifier,
      '(',
      optional($.parameter_list),
      ')',
      optional($.return_type)
    ),

    parameter_list: $ => seq(
      seq($.identifier, ':', $.type_annotation),
      repeat(seq(',', $.identifier, ':', $.type_annotation))
    ),

    return_type: $ => seq(':', $.type_annotation),

    guard_clause: $ => seq('when', $._guard_expression),
    _guard_expression: $ => prec.left(choice(
      $.guard_call,
      seq($._guard_expression, choice('&&', '||'), $._guard_expression),
      seq('(', $._guard_expression, ')')
    )),

    guard_call: $ => prec(1, $._expression),

    function_body: $ => seq('{', repeat($._statement), '}'),

    // Type Annotations
    type_annotation: $ => choice(
      seq($.module_path, optional($.generic_args)),
      $.tuple_type,
      $.function_type,
      'Self'
    ),

    tuple_type: $ => seq('(', seq($.type_annotation, repeat(seq(',', $.type_annotation))), ')'),

    function_type: $ => seq(
      'Function',
      '<',
      '(',
      optional($.function_type_params),
      ')',
      '->',
      $.type_annotation,
      '>'
    ),

    function_type_params: $ => seq(
      seq($.identifier, ':', $.type_annotation),
      repeat(seq(',', $.identifier, ':', $.type_annotation))
    ),

    // Expressions
    _expression: $ => $.pipe_expression,

    pipe_expression: $ => prec.left(1, choice(
      $.logical_or_expression,
      seq($.pipe_expression, choice('|>', '|?'), $.logical_or_expression)
    )),

    logical_or_expression: $ => prec.left(2, choice(
      $.logical_and_expression,
      seq($.logical_or_expression, '||', $.logical_and_expression)
    )),

    logical_and_expression: $ => prec.left(3, choice(
      $.equality_expression,
      seq($.logical_and_expression, '&&', $.equality_expression)
    )),

    equality_expression: $ => prec.left(4, choice(
      $.comparison_expression,
      seq($.equality_expression, choice('==', '!='), $.comparison_expression)
    )),

    comparison_expression: $ => prec.left(5, choice(
      $.bitwise_or_expression,
      seq($.comparison_expression, choice('>', '>=', '<', '<='), $.bitwise_or_expression)
    )),

    bitwise_or_expression: $ => prec.left(6, choice(
      $.bitwise_xor_expression,
      seq($.bitwise_or_expression, '|', $.bitwise_xor_expression)
    )),

    bitwise_xor_expression: $ => prec.left(7, choice(
      $.bitwise_and_expression,
      seq($.bitwise_xor_expression, '^', $.bitwise_and_expression)
    )),

    bitwise_and_expression: $ => prec.left(8, choice(
      $.shift_expression,
      seq($.bitwise_and_expression, '&', $.shift_expression)
    )),

    shift_expression: $ => prec.left(9, choice(
      $.additive_expression,
      seq($.shift_expression, choice('<<', '>>'), $.additive_expression)
    )),

    additive_expression: $ => prec.left(10, choice(
      $.multiplicative_expression,
      seq($.additive_expression, choice('+', '-'), $.multiplicative_expression)
    )),

    multiplicative_expression: $ => prec.left(11, choice(
      $.exponentiation_expression,
      seq($.multiplicative_expression, choice('*', '/', '%'), $.exponentiation_expression)
    )),

    exponentiation_expression: $ => prec.right(12, choice(
      $.unary_expression,
      seq($.unary_expression, '**', $.exponentiation_expression)
    )),

    unary_expression: $ => choice(
      $.postfix_expression,
      prec(13, seq(choice('+', '-', '!', '~'), $.unary_expression))
    ),

    postfix_expression: $ => prec.left(14, choice(
      $.primary_expression,
      seq($.postfix_expression, '.', $.identifier),
      seq($.postfix_expression, '.', $.integer),
      seq($.postfix_expression, '(', optional($.argument_list), ')'),
      seq($.postfix_expression, '[', $._expression, ']')
    )),

    primary_expression: $ => choice(
      $.integer,
      $.float,
      $.boolean,
      $.string,
      $.multiline_string,
      $.atom,
      $.sigil,
      $.identifier,
      $.module_path,
      seq('(', $._expression, ')'),
      $.if_expression,
      $.case_expression,
      $.anonymous_function,
      $.list_literal,
      $.tuple_literal,
      $.map_literal,
      $.function_capture,
      $.macro_injection
    ),

    // Function calls
    argument_list: $ => seq(
      $._argument,
      repeat(seq(',', $._argument))
    ),

    _argument: $ => choice(
      $.spread_argument,
      $.named_argument,
      $.shorthand_argument
    ),

    spread_argument: $ => seq(choice('..', '..?'), $._expression),
    named_argument: $ => seq($.identifier, ':', $._expression),
    shorthand_argument: $ => $.identifier,

    // Literals
    list_literal: $ => seq('[', optional(seq($._expression, repeat(seq(',', $._expression)))), ']'),

    tuple_literal: $ => seq('(', $._expression, seq(',', $._expression), repeat(seq(',', $._expression)), ')'),

    map_literal: $ => seq(
      '{',
      optional(seq(
        $._map_entry,
        repeat(seq(',', $._map_entry))
      )),
      '}'
    ),

    _map_entry: $ => choice(
      $.map_key_value_pair,
      $.map_shorthand_pair
    ),

    map_key_value_pair: $ => seq($._expression, '=>', $._expression),
    map_shorthand_pair: $ => prec(1, seq($.identifier, ':', $._expression)),

    struct_literal: $ => prec.dynamic(1, seq(
      $.module_path,
      '{',
      optional(seq(
        $._struct_field,
        repeat(seq(',', $._struct_field)),
        optional(','),
        optional($.struct_update)
      )),
      '}'
    )),

    _struct_field: $ => choice(
      $.struct_field_assignment,
      $.struct_field_shorthand
    ),

    struct_field_assignment: $ => seq($.identifier, ':', $._expression),
    struct_field_shorthand: $ => $.identifier,
    struct_update: $ => seq('..', $._expression),

    // Control Flow
    if_expression: $ => seq(
      'if',
      $._expression,
      '{',
      repeat($._statement),
      '}',
      optional(seq('else', '{', repeat($._statement), '}'))
    ),

    case_expression: $ => choice(
      $.concrete_case_expression,
      $.protocol_case_expression
    ),

    concrete_case_expression: $ => seq(
      'case',
      $._expression,
      '{',
      repeat1($.case_clause),
      '}'
    ),

    protocol_case_expression: $ => seq(
      'case',
      $._expression,
      'as',
      $.type_identifier,
      '{',
      repeat1($.protocol_case_clause),
      '}'
    ),

    case_clause: $ => prec(1, seq(
      $.case_pattern,
      '->',
      choice($._expression, seq('{', repeat($._statement), '}'))
    )),

    protocol_case_clause: $ => prec(1, seq(
      $.protocol_case_pattern,
      '->',
      choice($._expression, seq('{', repeat($._statement), '}'))
    )),

    case_pattern: $ => seq(
      $._destructure_pattern,
      optional($.guard_clause)
    ),

    protocol_case_pattern: $ => seq(
      $.type_identifier,
      '{',
      optional($.struct_pattern_fields),
      '}',
      optional($.guard_clause)
    ),

    _destructure_pattern: $ => choice(
      $.struct_pattern,
      $.tuple_pattern,
      $.list_pattern,
      $.value_pattern
    ),

    struct_pattern: $ => seq(
      $.type_identifier,
      '{',
      optional($.struct_pattern_fields),
      '}'
    ),

    struct_pattern_fields: $ => seq(
      $.identifier,
      repeat(seq(',', $.identifier))
    ),

    tuple_pattern: $ => seq(
      '(',
      $._pattern_element,
      repeat(seq(',', $._pattern_element)),
      ')'
    ),

    _pattern_element: $ => choice(
      $.identifier,
      $._destructure_pattern
    ),

    list_pattern: $ => seq(
      '[',
      optional(seq(
        $._pattern_element,
        repeat(seq(',', $._pattern_element)),
        optional(seq(',', '..', $.identifier))
      )),
      ']'
    ),

    value_pattern: $ => choice(
      $.integer,
      $.float,
      $.boolean,
      $.string,
      $.atom,
      $.sigil
    ),

    identifier_pattern: $ => $.identifier,

    // Anonymous Functions
    anonymous_function: $ => seq(
      'fn',
      '{',
      repeat1($.anonymous_clause),
      '}'
    ),

    anonymous_clause: $ => prec(1, seq(
      $.anonymous_params,
      optional($.guard_clause),
      '->',
      choice($._expression, seq('{', repeat($._statement), '}'))
    )),

    anonymous_params: $ => choice(
      seq($.identifier, ':', $.type_annotation),
      seq('(', optional($.parameter_list), ')')
    ),

    // Function Captures
    function_capture: $ => seq('&', $.qualified_function_ref),

    qualified_function_ref: $ => prec.left(seq(
      optional(seq($.module_path, '.')),
      $.identifier,
      optional(seq('/', $.integer))
    )),

    // Statements
    _statement: $ => choice(
      $._expression,
      $.let_binding,
      $.constant_definition
    ),

    let_binding: $ => seq(
      'let',
      choice(
        $.typed_binding,
        prec(1, $.inferred_binding),
        $.destructure_binding
      )
    ),

    typed_binding: $ => seq(
      $.identifier,
      ':',
      $.type_annotation,
      '=',
      $._expression
    ),

    inferred_binding: $ => seq(
      $.identifier,
      '=',
      $._expression
    ),

    destructure_binding: $ => seq(
      $._destructure_pattern,
      '=',
      $._expression
    ),

    // Constants
    constant_definition: $ => seq(
      repeat($.attribute),
      'const',
      $.type_identifier,
      ':',
      $.type_annotation,
      '=',
      $._expression
    ),

    // Aliases
    alias_definition: $ => seq(
      'alias',
      $.module_path,
      optional($.alias_spec)
    ),

    alias_spec: $ => choice(
      seq('as', $.type_identifier),
      seq('.', '{', $.alias_list, '}')
    ),

    alias_list: $ => seq(
      $.alias_item,
      repeat(seq(',', $.alias_item))
    ),

    alias_item: $ => seq(
      $.type_identifier,
      optional(seq('as', $.type_identifier))
    ),

    // Macro Definitions
    macro_definition: $ => seq(
      'macro',
      $.identifier,
      '(',
      optional($.macro_parameter_list),
      ')',
      '{',
      repeat($._statement),
      '}'
    ),

    macro_parameter_list: $ => seq(
      $.identifier,
      repeat(seq(',', $.identifier))
    ),

    macro_injection: $ => seq('^', $.identifier),

    // Import Definitions
    import_definition: $ => seq(
      'import',
      $.module_path,
      optional($.import_spec)
    ),

    import_spec: $ => choice(
      seq(',', 'only', ':', '[', $.import_list, ']'),
      seq(',', 'except', ':', '[', $.import_list, ']')
    ),

    import_list: $ => seq(
      $.import_item,
      repeat(seq(',', $.import_item))
    ),

    import_item: $ => seq(
      $.identifier,
      ':',
      $.integer
    ),

    // Attributes
    attribute: $ => seq(
      '@',
      $.identifier,
      optional(seq('(', optional($.attribute_args), ')'))
    ),

    attribute_args: $ => seq(
      $._attribute_arg,
      repeat(seq(',', $._attribute_arg))
    ),

    _attribute_arg: $ => choice(
      $.named_argument,
      $._expression
    ),
  }
});
