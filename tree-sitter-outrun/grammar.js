/**
 * @file Outrun programming language grammar
 * @author James Harton <james@harton.dev>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "outrun",

  extras: $ => [
    $.comment,
    $.block_comment,
    /\s/
  ],


  conflicts: $ => [
    [$.module_path],
    [$.guard_expression, $.primary_expression],
    [$.map_literal, $.block]
  ],

  rules: {
    source_file: $ => repeat($.item),

    // Top-level items
    item: $ => choice(
      $.struct_definition,
      $.trait_definition,
      $.impl_block,
      $.function_definition,
      $.constant_definition,
      $.let_binding,
      $.alias_definition,
      $.macro_definition,
      $.import_definition
    ),

    // Comments
    comment: $ => token(prec(-1, seq('#', /.*/))),
    block_comment: $ => token(seq('###', /.*?/, '###')),

    // Struct Definition
    struct_definition: $ => seq(
      repeat($.attribute),
      'struct',
      $.type_identifier,
      optional($.generic_params),
      '(',
      optional($.struct_fields),
      ')',
      optional($.trait_constraints),
      '{',
      repeat($.function_definition),
      '}'
    ),

    struct_fields: $ => seq(
      $.struct_field,
      repeat(seq(',', $.struct_field)),
      optional(',')
    ),

    struct_field: $ => seq(
      $.identifier,
      ':',
      $.type_annotation
    ),

    // Trait Definition
    trait_definition: $ => seq(
      repeat($.attribute),
      'trait',
      $.type_identifier,
      optional($.generic_params),
      optional($.trait_constraints),
      '{',
      repeat($.trait_function),
      '}'
    ),

    trait_function: $ => choice(
      // Function signature only (no guards allowed)
      seq(
        'def',
        $.identifier,
        '(',
        optional($.parameter_list),
        ')',
        optional($.return_type)
      ),
      // Full function definition (default implementation with guards allowed)
      seq(
        'def',
        $.identifier,
        '(',
        optional($.parameter_list),
        ')',
        optional($.return_type),
        optional($.guard_clause),
        $.block
      )
    ),

    // Implementation Block
    impl_block: $ => seq(
      'impl',
      optional($.generic_params),
      $.trait_spec,
      'for',
      $.type_spec,
      optional($.trait_constraints),
      '{',
      repeat($.function_definition),
      '}'
    ),

    trait_spec: $ => seq(
      $.module_path,
      optional($.generic_args)
    ),

    type_spec: $ => seq(
      $.module_path,
      optional($.generic_args)
    ),

    // Function Definition
    function_definition: $ => seq(
      repeat($.attribute),
      choice('def', 'defp'),
      $.identifier,
      '(',
      optional($.parameter_list),
      ')',
      optional($.return_type),
      optional($.guard_clause),
      $.block
    ),

    parameter_list: $ => seq(
      $.parameter,
      repeat(seq(',', $.parameter)),
      optional(',')
    ),

    parameter: $ => seq(
      $.identifier,
      ':',
      $.type_annotation
    ),

    return_type: $ => seq(':', $.type_annotation),

    // Guard System
    guard_clause: $ => seq('when', $.guard_expression),

    guard_expression: $ => choice(
      $.expression,
      $.binary_guard_expression,
      seq('(', $.guard_expression, ')')
    ),

    binary_guard_expression: $ => choice(
      prec.left(1, seq($.guard_expression, '&&', $.guard_expression)),
      prec.left(1, seq($.guard_expression, '||', $.guard_expression))
    ),

    trait_constraints: $ => seq('when', $.constraint_expression),

    constraint_expression: $ => choice(
      $.constraint_term,
      $.binary_constraint_expression
    ),

    binary_constraint_expression: $ => choice(
      prec.left(1, seq($.constraint_expression, '&&', $.constraint_expression)),
      prec.left(1, seq($.constraint_expression, '||', $.constraint_expression))
    ),

    constraint_term: $ => choice(
      $.type_constraint,
      seq('(', $.constraint_expression, ')')
    ),

    type_constraint: $ => seq(
      $.type_identifier,
      ':',
      $.type_annotation
    ),

    // Generic Parameters and Arguments
    generic_params: $ => seq(
      '<',
      $.generic_param,
      repeat(seq(',', $.generic_param)),
      optional(','),
      '>'
    ),

    generic_param: $ => $.type_identifier,

    generic_args: $ => seq(
      '<',
      $.type_annotation,
      repeat(seq(',', $.type_annotation)),
      optional(','),
      '>'
    ),

    // Types
    type_annotation: $ => choice(
      seq($.module_path, optional($.generic_args)),
      $.tuple_type,
      $.function_type,
      'Self'
    ),

    tuple_type: $ => seq(
      '(',
      optional(seq(
        $.type_annotation,
        repeat(seq(',', $.type_annotation)),
        optional(',')
      )),
      ')'
    ),

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
      $.function_type_param,
      repeat(seq(',', $.function_type_param)),
      optional(',')
    ),

    function_type_param: $ => seq(
      $.identifier,
      ':',
      $.type_annotation
    ),

    module_path: $ => seq(
      $.type_identifier,
      repeat(seq('.', $.type_identifier))
    ),

    // Macro Definition
    macro_definition: $ => seq(
      'macro',
      $.identifier,
      '(',
      optional($.macro_parameter_list),
      ')',
      $.block
    ),

    macro_parameter_list: $ => seq(
      $.identifier,
      repeat(seq(',', $.identifier)),
      optional(',')
    ),

    // Anonymous Functions
    anonymous_function: $ => seq(
      'fn',
      '{',
      repeat1($.anonymous_clause),
      '}'
    ),

    anonymous_clause: $ => seq(
      $.anonymous_params,
      optional($.guard_clause),
      '->',
      choice($.expression, $.block)
    ),

    anonymous_params: $ => choice(
      seq($.identifier, ':', $.type_annotation),
      seq('(', optional($.parameter_list), ')')
    ),

    // Function Captures
    function_capture: $ => prec(10, seq(
      '&',
      $.qualified_function_ref
    )),

    qualified_function_ref: $ => seq(
      optional(seq($.module_path, '.')),
      $.identifier,
      optional($.arity)
    ),

    arity: $ => token(seq('/', /[0-9]+/)),

    // Expressions
    expression: $ => $._pipe_expression,

    _pipe_expression: $ => prec.left(1, choice(
      $._logical_or_expression,
      $.pipe_expression
    )),

    pipe_expression: $ => choice(
      prec.left(1, seq($._pipe_expression, '|>', $._logical_or_expression)),
      prec.left(1, seq($._pipe_expression, '|?', $._logical_or_expression))
    ),

    _logical_or_expression: $ => prec.left(2, choice(
      $._logical_and_expression,
      $.binary_logical_or
    )),

    binary_logical_or: $ => prec.left(2, seq($._logical_or_expression, '||', $._logical_and_expression)),

    _logical_and_expression: $ => prec.left(3, choice(
      $._equality_expression,
      $.binary_logical_and
    )),

    binary_logical_and: $ => prec.left(3, seq($._logical_and_expression, '&&', $._equality_expression)),

    _equality_expression: $ => prec.left(4, choice(
      $._comparison_expression,
      $.binary_equal,
      $.binary_not_equal
    )),

    binary_equal: $ => prec.left(4, seq($._equality_expression, '==', $._comparison_expression)),
    binary_not_equal: $ => prec.left(4, seq($._equality_expression, token('!='), $._comparison_expression)),

    _comparison_expression: $ => prec.left(5, choice(
      $._bitwise_or_expression,
      $.binary_greater,
      $.binary_greater_equal,
      $.binary_less,
      $.binary_less_equal
    )),

    binary_greater: $ => prec.left(5, seq($._comparison_expression, '>', $._bitwise_or_expression)),
    binary_greater_equal: $ => prec.left(5, seq($._comparison_expression, '>=', $._bitwise_or_expression)),
    binary_less: $ => prec.left(5, seq($._comparison_expression, '<', $._bitwise_or_expression)),
    binary_less_equal: $ => prec.left(5, seq($._comparison_expression, '<=', $._bitwise_or_expression)),

    _bitwise_or_expression: $ => prec.left(6, choice(
      $._bitwise_xor_expression,
      $.binary_bitwise_or
    )),

    binary_bitwise_or: $ => prec.left(6, seq($._bitwise_or_expression, '|', $._bitwise_xor_expression)),

    _bitwise_xor_expression: $ => prec.left(7, choice(
      $._bitwise_and_expression,
      $.binary_bitwise_xor
    )),

    binary_bitwise_xor: $ => prec.left(7, seq($._bitwise_xor_expression, '^', $._bitwise_and_expression)),

    _bitwise_and_expression: $ => prec.left(8, choice(
      $._shift_expression,
      $.binary_bitwise_and
    )),

    binary_bitwise_and: $ => prec.left(8, seq($._bitwise_and_expression, '&', $._shift_expression)),

    _shift_expression: $ => prec.left(9, choice(
      $._additive_expression,
      $.binary_shift_left,
      $.binary_shift_right
    )),

    binary_shift_left: $ => prec.left(9, seq($._shift_expression, '<<', $._additive_expression)),
    binary_shift_right: $ => prec.left(9, seq($._shift_expression, '>>', $._additive_expression)),

    _additive_expression: $ => prec.left(10, choice(
      $._multiplicative_expression,
      $.binary_add,
      $.binary_subtract
    )),

    binary_add: $ => prec.left(10, seq($._additive_expression, '+', $._multiplicative_expression)),
    binary_subtract: $ => prec.left(10, seq($._additive_expression, '-', $._multiplicative_expression)),

    _multiplicative_expression: $ => prec.left(11, choice(
      $._exponentiation_expression,
      $.binary_multiply,
      $.binary_divide,
      $.binary_modulo
    )),

    binary_multiply: $ => prec.left(11, seq($._multiplicative_expression, '*', $._exponentiation_expression)),
    binary_divide: $ => prec.left(11, seq($._multiplicative_expression, '/', $._exponentiation_expression)),
    binary_modulo: $ => prec.left(11, seq($._multiplicative_expression, '%', $._exponentiation_expression)),

    _exponentiation_expression: $ => prec.right(12, choice(
      $._unary_expression,
      $.binary_exponent
    )),

    binary_exponent: $ => prec.right(12, seq($._unary_expression, '**', $._exponentiation_expression)),

    _unary_expression: $ => choice(
      $._postfix_expression,
      $.unary_plus,
      $.unary_minus,
      $.unary_not,
      $.unary_complement
    ),

    unary_plus: $ => prec(13, seq('+', $._unary_expression)),
    unary_minus: $ => prec(13, seq('-', $._unary_expression)),
    unary_not: $ => prec(13, seq('!', $._unary_expression)),
    unary_complement: $ => prec(13, seq('~', $._unary_expression)),

    _postfix_expression: $ => choice(
      $.primary_expression,
      prec.left(15, seq(
        $._postfix_expression,
        $.function_call_postfix
      )),
      prec.left(14, seq(
        $._postfix_expression,
        choice(
          $.field_access,
          $.tuple_access,
          $.index_access
        )
      ))
    ),

    field_access: $ => seq('.', $.identifier),
    tuple_access: $ => seq('.', $.integer),
    function_call_postfix: $ => seq(
      '.',
      $.identifier,
      token.immediate('('),
      optional($.argument_list),
      ')'
    ),
    function_call: $ => prec(1, seq(
      optional(seq($.module_path, '.')),
      $.identifier,
      token.immediate('('),
      optional($.argument_list),
      ')'
    )),
    index_access: $ => seq('[', $.expression, ']'),

    primary_expression: $ => choice(
      $.literal,
      $.identifier,
      $.module_path,
      $.function_call,
      $.function_capture,
      seq('(', $.expression, ')'),
      $.if_expression,
      $.case_expression,
      $.anonymous_function,
      $.struct_literal,
      $.list_literal,
      $.tuple_literal,
      $.map_literal,
      $.macro_injection
    ),

    macro_injection: $ => seq('^', $.identifier),

    // Function Calls
    argument_list: $ => seq(
      $.argument,
      repeat(seq(',', $.argument)),
      optional(',')
    ),

    argument: $ => choice(
      $.spread_argument,
      $.named_argument,
      $.shorthand_argument
    ),

    spread_argument: $ => choice(
      seq('..', $.expression),
      seq(token('..?'), $.expression)
    ),

    named_argument: $ => seq(
      $.identifier,
      ':',
      $.expression
    ),

    shorthand_argument: $ => $.identifier,

    // Literals
    literal: $ => choice(
      $.integer,
      $.float,
      $.string,
      $.multiline_string,
      $.atom,
      $.sigil,
      $.boolean
    ),

    integer: $ => choice(
      /[0-9]+/,                    // Decimal
      /0b[01]+/,                   // Binary
      /0o[0-7]+/,                  // Octal
      /0x[0-9a-fA-F]+/            // Hexadecimal
    ),

    float: $ => /[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?/,

    string: $ => seq(
      '"',
      repeat(choice(
        $.interpolation,
        $.escape_sequence,
        $.string_content
      )),
      '"'
    ),

    string_content: $ => token.immediate(/[^"\\#]+/),
    
    escape_sequence: $ => token.immediate(/\\./),

    interpolation: $ => seq('#{', $.expression, '}'),

    multiline_string: $ => seq(
      '"""',
      repeat(choice(
        $.multiline_string_content,
        $.interpolation
      )),
      '"""'
    ),

    multiline_string_content: $ => token.immediate(/[^#"]+|"[^"]*"|""[^"]/),

    atom: $ => seq(
      ':',
      choice(
        $.identifier,
        $.string
      )
    ),

    sigil: $ => seq(
      token(seq('~', /[A-Z][a-zA-Z0-9_]*/)),
      choice($.string, $.multiline_string)
    ),

    boolean: $ => choice('true', 'false'),

    // Collection Literals
    list_literal: $ => seq(
      '[',
      optional(seq(
        $.expression,
        repeat(seq(',', $.expression)),
        optional(',')
      )),
      ']'
    ),

    tuple_literal: $ => choice(
      // Empty tuple
      seq('(', ')'),
      // Single element tuple (requires trailing comma)
      seq('(', $.expression, ',', ')'),
      // Multi-element tuple
      seq(
        '(',
        $.expression,
        repeat1(seq(',', $.expression)),
        optional(','),
        ')'
      )
    ),

    map_literal: $ => seq(
      '{',
      optional(seq(
        $.map_entry,
        repeat(seq(',', $.map_entry)),
        optional(',')
      )),
      '}'
    ),

    map_entry: $ => choice(
      $.map_key_value_pair,
      $.map_shorthand_pair
    ),

    map_key_value_pair: $ => prec(2, seq(
      $.expression,
      '=>',
      $.expression
    )),

    map_shorthand_pair: $ => prec(2, seq(
      choice($.identifier, $.string),
      ':',
      $.expression
    )),

    // Struct Literals
    struct_literal: $ => seq(
      $.struct_literal_start,
      optional($.struct_literal_fields),
      '}'
    ),

    struct_literal_start: $ => token(seq(
      /[A-Z][a-zA-Z0-9_]*/,
      optional(seq('.', /[A-Z][a-zA-Z0-9_]*/)),
      /\s*/,
      '{'
    )),

    struct_literal_fields: $ => choice(
      seq(
        $.struct_literal_field,
        repeat(seq(',', $.struct_literal_field)),
        optional(','),
        optional(seq(',', $.struct_update))
      ),
      $.struct_update
    ),

    struct_literal_field: $ => choice(
      $.struct_field_assignment,
      $.struct_field_shorthand
    ),

    struct_field_assignment: $ => seq(
      $.identifier,
      ':',
      $.expression
    ),

    struct_field_shorthand: $ => $.identifier,

    struct_update: $ => seq('..', $.expression),

    // Control Flow
    if_expression: $ => prec.right(seq(
      'if',
      $.expression,
      $.block,
      optional(seq('else', $.block))
    )),

    case_expression: $ => seq(
      'case',
      $.expression,
      '{',
      repeat1($.case_clause),
      '}'
    ),

    case_clause: $ => seq(
      $.case_pattern,
      '->',
      choice($.expression, $.block)
    ),

    case_pattern: $ => seq(
      $.destructure_pattern,
      optional($.guard_clause)
    ),


    // Statements
    block: $ => seq('{', repeat($.statement), '}'),

    statement: $ => choice(
      $.expression,
      $.let_binding,
      $.constant_definition
    ),

    // Variable Binding
    let_binding: $ => seq(
      'let',
      choice(
        seq($.identifier, ':', $.type_annotation, '=', $.expression),
        seq($.destructure_pattern, '=', $.expression)
      )
    ),

    destructure_pattern: $ => choice(
      $.tuple_destructure,
      $.struct_destructure,
      $.list_destructure,
      $.identifier,
      $.literal
    ),

    tuple_destructure: $ => seq(
      '(',
      $.destructure_pattern,
      repeat(seq(',', $.destructure_pattern)),
      optional(','),
      ')'
    ),

    struct_destructure: $ => seq(
      $.struct_destructure_start,
      $.destructure_pattern,
      repeat(seq(',', $.destructure_pattern)),
      optional(','),
      '}'
    ),

    struct_destructure_start: $ => token(seq(
      /[A-Z][a-zA-Z0-9_]*/,
      /\s*/,
      '{'
    )),

    list_destructure: $ => seq(
      '[',
      $.destructure_pattern,
      repeat(seq(',', $.destructure_pattern)),
      optional(seq(',', '..', $.destructure_pattern)),
      optional(','),
      ']'
    ),

    // Constants
    constant_definition: $ => seq(
      repeat($.attribute),
      'const',
      $.type_identifier,
      ':',
      $.type_annotation,
      '=',
      $.expression
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
      repeat(seq(',', $.alias_item)),
      optional(',')
    ),

    alias_item: $ => seq(
      $.type_identifier,
      optional(seq('as', $.type_identifier))
    ),

    // Attributes
    attribute: $ => seq(
      '@',
      $.type_identifier,
      optional(seq('(', $.attribute_args, ')'))
    ),

    attribute_args: $ => seq(
      $.argument,
      repeat(seq(',', $.argument)),
      optional(',')
    ),

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
      repeat(seq(',', $.import_item)),
      optional(',')
    ),

    import_item: $ => seq(
      $.identifier,
      ':',
      $.integer
    ),

    // Identifiers
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*\??/,
    type_identifier: $ => /[A-Z][a-zA-Z0-9_]*/
  }
});