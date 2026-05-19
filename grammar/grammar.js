/**
 * @file Lambda grammar for tree-sitter
 * @author menaruben <ruben@mena.ch>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check
module.exports = grammar({
  name: 'lambda',

  rules: {
    source_file: $ => repeat($._line),

    _line: $ => choice(
      $.let_definition,
      $._expr
    ),

    let_definition: $ => seq(
      ':let',
      $.identifier,
      $._expr
    ),

    _expr: $ => choice(
      $.application,
      $.abstraction,
      $.macro_identifier,
      $.identifier
    ),

    application: $ => seq(
      '(',
      $._expr,
      $._expr,
      ')'
    ),

    abstraction: $ => seq(
      choice('\\', 'λ'),
      $.identifier,
      '.',
      $._expr
    ),

    macro_identifier: $ => seq(
      '@',
      $.identifier
    ),

    identifier: $ => /[a-zA-Z0-9_]+/,
  }
});
