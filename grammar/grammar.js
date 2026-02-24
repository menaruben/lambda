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
      $.define,
      $._expr
    ),
    
    define: $ => seq(
      ':define',
      $.identifier,
      $._expr
    ),

    _expr: $ => choice(
      $.application,
      $.abstraction,
      $.macro,
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

    macro: $ => seq(
      '$',
      $.identifier
    ),

    identifier: $ => /[a-zA-Z0-9_]+/,
  }
});
