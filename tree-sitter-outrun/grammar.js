/**
 * @file The one language the cops never catch
 * @author James Harton <james@harton.dev>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "outrun",

  conflicts: $ => [
    [$.type_name]
  ],

  rules: {
    source_file: ($) => choice($.type_def, $.trait_def),

    type_def: ($) => seq(
      $.kw_type, 
      $.ws, 
      $.type_name, 
      optional(seq(
        optional($.ws), 
        optional($.type_def_fields)
      )),
      optional(seq(
        optional($.ws),
        optional($.type_def_impl)
      ))
    ),
    type_def_fields: ($) => seq(
      $.op_lparen, 
      optional($.ws), 
      optional(seq(
        $.type_def_field,
        repeat(seq($.sep_list, $.type_def_field)))
      ), 
     $.op_rparen
    ),
    type_def_field: ($) => seq($.ident_name, $.sep_kw, $.type_name),
    type_def_impl: ($) => seq(
      $.op_lbrace,
      optional($.ws),
      repeat($.type_def_impl_elem),
      optional($.ws),
      $.op_rbrace
    ),
    type_def_impl_elem: ($) => choice($.fn_def),

    trait_def: ($) => seq(
      $.kw_trait,
      $.ws,
      $.type_name,
      optional(seq(
        optional($.ws),
        $.trait_def_when
      ))
    ),

    trait_def_when: ($) => seq(
      $.op_comma,
      optional($.ws),
      $.kw_when,
      optional($.ws),
      $.op_colon,
      optional($.ws),
      repeat($.trait_constraint)
    ),

    trait_constraint: ($) => seq(
      $.trait_constrait_expr,
      repeat(seq(
        optional($.ws),
        choice(
          $.op_quadrolon,
          $.op_land,
          $.op_lor
        ),
        optional($.ws),
        $.trait_constrait_expr
      )),
    ),

    trait_constrait_expr: ($) => choice(
      $.type_name,
      seq(
        $.op_lbrace,
        optional($.ws),
        $.trait_constraint,
        optional($.ws),
        $.op_rbrace
      )
    ),

    type_name: ($) => seq($.constant_name, 
      optional(seq(
        optional($.ws),
        $.type_generic
      ))
    ),
    type_generic: ($) => seq(
      $.op_lt, 
      optional($.ws), 
      optional(seq(
        $.type_name,
        repeat(seq(
          $.sep_list, $.type_name
        ))
      )),
      optional($.ws), 
      $.op_gt
    ),

    fn_def: ($) => "fn",

    constant_name: ($) => /[A-Z][a-zA-Z0-9_]*/,
    ident_name: ($) => /[a-z_][a-zA-Z0-9_]*\??/,

    ws: ($) => /\s+/,
    op_colon: ($) => ":",
    op_comma: ($) => ",",
    op_gt: ($) => ">",
    op_lt: ($) => "<",
    op_lparen: ($) => "(",
    op_rparen: ($) => ")",
    op_lbrace: ($) => "{",
    op_rbrace: ($) => "}",
    op_quadrolon: ($) => "::",
    op_land: ($) => "&&",
    op_lor: ($) => "||",
    kw_type: ($) => "type",
    kw_trait: ($) => "trait",
    kw_when: ($) => "when",
    sep_list: ($) => seq(optional($.ws), $.op_comma, optional($.ws)),
    sep_kw: ($) => seq(optional($.ws), $.op_colon, optional($.ws))
  }
});
