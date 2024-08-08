/**
 * @file FSharp grammar for tree-sitter
 * @author Nikolaj Sidorenco
 * @license MIT
 * @see {@link https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf f# grammar}
 */

/* eslint-disable arrow-parens */
/* eslint-disable camelcase */
/* eslint-disable-next-line spaced-comment */
/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  SEQ_EXPR: 1,
  APP_EXPR: 16,
  THEN_EXPR: 2,
  RARROW: 3,
  INFIX_OP: 4,
  NEW_EXPR: 5,
  LET_EXPR: 60,
  LET_DECL: 7,
  FUN_EXPR: 8,
  MATCH_EXPR: 8,
  MATCH_DECL: 9,
  ELSE_EXPR: 11,
  INTERFACE: 12,
  COMMA: 13,
  PREFIX_EXPR: 15,
  SPECIAL_INFIX: 16,
  LARROW: 16,
  TUPLE_EXPR: 16,
  CE_EXPR: 15,
  SPECIAL_PREFIX: 17,
  IF_EXPR: 18,
  DOT: 19,
  INDEX_EXPR: 20,
  PAREN_APP: 21,
  TYPED_EXPR: 22,
  PAREN_EXPR: 21,
  DOTDOT: 22,
  DOTDOT_SLICE: 23,
  NEW_OBJ: 24,
};

module.exports = grammar({
  name: "fsharp",

  extras: ($) => [
    /[ \s\f\uFEFF\u2060\u200B]|\\\r?n/,
    $.block_comment,
    $.line_comment,
    ";",
  ],

  // The external scanner (scanner.c) allows us to inject "dummy" tokens into the grammar.
  // These tokens are used to track the indentation-based scoping used in F#
  externals: ($) => [
    $._newline, // we distinguish new scoped based on newlines.
    $._indent, // starts a new indentation-based scope.
    $._dedent, // signals that the current indentation scope has ended.
    "then",
    "else",
    "elif",
    "class",
    $._struct_begin,
    $._interface_begin,
    "end",
    $._triple_quoted_content,
    $.block_comment_content,
    $._inside_string_marker,
    $._newline_not_aligned,

    $._error_sentinel, // unused token to detect parser errors in external parser.
  ],

  conflicts: ($) => [
    [$.long_identifier, $._identifier_or_op],
    [$.rules],
  ],

  word: ($) => $.identifier,


  supertypes: ($) => [
    $._module_elem,
    $._pattern,
    $._expression,
    $._type,
    $._type_defn_body,
  ],

  rules: {
    //
    // Top-level rules (BEGIN)
    //
    file: ($) =>
      choice($.named_module, repeat($._module_elem)),

    named_module: ($) =>
      seq(
        "module",
        optional($.access_modifier),
        field("name", $.long_identifier),
        repeat($._module_elem),
      ),

    _module_elem: ($) =>
      choice(
        $.value_declaration,
        $.module_defn,
        $.module_abbrev,
        $.type_definition,
        $._expression
      ),

    module_abbrev: ($) =>
      seq(
        "module",
        $.identifier,
        "=",
        scoped($.long_identifier, $._indent, $._dedent),
      ),

    module_defn: ($) =>
      prec.left(
        seq(
          "module",
          optional($.access_modifier),
          $.identifier,
          "=",
          scoped($._module_body, $._indent, $._dedent),
        ),
      ),

    _module_body: ($) =>
      seq(
        $._module_elem,
        repeat(
          prec(
            // Make sure to parse a module node before a sequential expression
            // NOTE: This removes all sequential expressions from module bodies
            PREC.SEQ_EXPR + 1,
            seq(alias($._newline, ";"), $._module_elem),
          ),
        ),
      ),

    value_declaration: ($) =>
      prec(PREC.LET_DECL, $.function_or_value_defn),

    _function_or_value_defns: ($) =>
      prec.right(
        seq(
          $._function_or_value_defn_body,
          repeat(seq("and", $._function_or_value_defn_body)),
        ),
      ),

    function_or_value_defn: ($) =>
      seq(
        choice("let", "let!"),
        choice(
          $._function_or_value_defn_body,
          seq("rec", $._function_or_value_defns),
        ),
      ),

    _function_or_value_defn_body: ($) =>
      seq(
        choice($.function_declaration_left, $.value_declaration_left),
        optional(seq(":", $._type)),
        "=",
        field("body", $._expression_block),
      ),

    function_declaration_left: ($) =>
      prec.left(
        3,
        seq(
          optional($.access_modifier),
          prec(100, $._identifier_or_op),
          optional($.type_arguments),
          $.argument_patterns,
        ),
      ),

    value_declaration_left: ($) =>
      prec.left(
        2,
        seq(
          optional("mutable"),
          optional($.access_modifier),
          $._pattern,
          optional($.type_arguments),
        ),
      ),

    access_modifier: (_) =>
      prec(100, token(prec(1000, choice("private", "internal", "public")))),
    //
    // Top-level rules (END)
    //

    class_as_reference: ($) => seq("as", $.identifier),

    primary_constr_args: ($) =>
      seq(
        optional($.access_modifier),
        "(",
        optional($._pattern),
        ")",
        optional($.class_as_reference),
      ),

    //
    // Pattern rules (BEGIN)

    repeat_pattern: ($) =>
      prec.right(seq($._pattern, repeat1(prec(1, seq(",", $._pattern))))),

    _pattern: ($) =>
      choice(
        "null",
        alias("_", $.wildcard_pattern),
        $.const,
        $.as_pattern,
        $.disjunct_pattern,
        $.conjunct_pattern,
        $.cons_pattern,
        $.repeat_pattern,
        $.paren_pattern,
        $.list_pattern,
        $.array_pattern,
        $.record_pattern,
        $.typed_pattern,
        $.optional_pattern,
        $.identifier_pattern,
      ),

    optional_pattern: ($) => prec.left(seq("?", $._pattern)),


    paren_pattern: ($) => seq("(", $._pattern, ")"),

    as_pattern: ($) => prec.left(0, seq($._pattern, "as", $.identifier)),
    cons_pattern: ($) => prec.left(0, seq($._pattern, "::", $._pattern)),
    disjunct_pattern: ($) => prec.left(0, seq($._pattern, "|", $._pattern)),
    conjunct_pattern: ($) => prec.left(0, seq($._pattern, "&", $._pattern)),
    typed_pattern: ($) =>
      prec.left(
        3,
        seq(
          $._pattern,
          ":",
          $._type
        ),
      ),

    argument_patterns: ($) =>
      // argument patterns are generally no different from normal patterns.
      // however, any time an argument pattern is a valid node, (i.e. inside a beginning fun decl)
      // it is always the correct node to construct.
      prec.left(1000, repeat1($._atomic_pattern)),

    field_pattern: ($) => prec(1, seq($.long_identifier, "=", $._pattern)),

    _atomic_pattern: ($) =>
      prec(
        1000,
        choice(
          "null",
          "_",
          $.const,
          $.long_identifier,
          $.list_pattern,
          $.record_pattern,
          $.array_pattern,
          seq("(", $._pattern, ")"),
        ),
      ),

    _list_pattern_content: ($) =>
      scoped(
        seq(
          optional($._newline),
          $._pattern,
          repeat(seq(choice(";", $._newline), $._pattern)),
        ),
        $._indent,
        $._dedent,
      ),

    list_pattern: ($) => seq("[", optional($._list_pattern_content), "]"),
    array_pattern: ($) => seq("[|", optional($._list_pattern_content), "|]"),
    record_pattern: ($) =>
      prec.left(
        seq("{", $.field_pattern, repeat(seq(";", $.field_pattern)), "}"),
      ),

    identifier_pattern: ($) =>
      prec.left(
        -1,
        seq(
          $.long_identifier_or_op,
          optional($._pattern_param),
          optional($._pattern),
        ),
      ),

    _pattern_param: ($) =>
      prec(
        2,
        choice(
          $.const,
          $.long_identifier,
          "null",
        ),
      ),
    //
    // Pattern rules (END)
    //

    //
    // Expressions (BEGIN)
    //

    _expression_block: ($) => seq($._indent, $._expression, $._dedent),

    _expression: ($) =>
      choice(
        "null",
        $.const,
        $.paren_expression,
        $.long_identifier_or_op,
        $.typed_expression,
        $.infix_expression,
        $.index_expression,
        $.list_expression,
        $.array_expression,
        $.brace_expression,
        $.declaration_expression,
        $.fun_expression,
        $.function_expression,
        $.sequential_expression,
        $.if_expression,
        $.match_expression,
        $.tuple_expression,
        $.dot_expression
      ),

    long_identifier_or_op: ($) =>
      prec.right(
        choice(
          $.long_identifier,
          seq($.long_identifier, ".", $._identifier_or_op),
          $._identifier_or_op,
        ),
      ),

    tuple_expression: ($) =>
      prec.right(PREC.TUPLE_EXPR, seq($._expression, ",", $._expression)),

    brace_expression: ($) =>
      prec(
        PREC.CE_EXPR + 1,
        seq(
          "{",
          scoped(
            choice(
              $.field_initializers,
              $.with_field_expression,
            ),
            $._indent,
            $._dedent,
          ),
          "}",
        ),
      ),

    with_field_expression: ($) =>
      seq(
        $._expression,
        "with",
        scoped($.field_initializers, $._indent, $._dedent),
      ),

    _else_expression: ($) => seq("else", field("else", $._expression_block)),

    _then_expression: ($) => seq("then", field("then", $._expression_block)),

    elif_expression: ($) =>
      seq("elif", field("guard", $._expression_block), $._then_expression),

    _if_branch: ($) => seq("if", field("guard", $._expression_block)),

    if_expression: ($) =>
      seq(
        $._if_branch,
        $._then_expression,
        repeat($.elif_expression),
        optional($._else_expression),
      ),

    fun_expression: ($) =>
      prec.right(
        PREC.FUN_EXPR,
        seq("fun", $.argument_patterns, "->", $._expression_block),
      ),

    match_expression: ($) =>
      seq(
        choice("match", "match!"),
        $._expression,
        optional($._newline),
        "with",
        choice(seq($._newline, $.rules), scoped($.rules, $._indent, $._dedent)),
      ),

    function_expression: ($) =>
      prec(
        PREC.MATCH_EXPR,
        seq("function", scoped($.rules, $._indent, $._dedent)),
      ),


    index_expression: ($) =>
      prec(
        PREC.INDEX_EXPR,
        seq(
          $._expression,
          ".[",
          field("index", $._expression),
          "]",
        ),
      ),

    typed_expression: ($) =>
      prec(
        PREC.PAREN_EXPR,
        seq(
          $._expression,
          token.immediate(prec(PREC.PAREN_EXPR, "<")),
          optional($.types),
          prec(PREC.PAREN_EXPR, ">"),
        ),
      ),

    declaration_expression: ($) =>
      seq(
        $.function_or_value_defn,
        field("in", $._expression),
      ),

    _list_elements: ($) =>
      prec.right(
        PREC.COMMA + 100,
        seq(
          optional($._newline),
          $._expression,
          repeat(
            prec.right(
              PREC.COMMA + 100,
              seq(alias($._newline, ";"), $._expression),
            ),
          ),
        ),
      ),

    _list_element: ($) =>
      seq(
        $._indent,
        $._list_elements,
        $._dedent,
      ),

    list_expression: ($) => seq("[", optional($._list_element), "]"),

    array_expression: ($) => seq("[|", optional($._list_element), "|]"),

    rule: ($) =>
      prec.right(
        seq(
          field("pattern", $._pattern),
          optional(seq("when", field("guard", $._expression))),
          "->",
          field("block", $._expression_block),
        ),
      ),

    rules: ($) =>
      seq(
        optional("|"),
        $.rule,
        repeat(seq(optional($._newline), "|", $.rule)),
      ),


    paren_expression: ($) =>
      prec(PREC.PAREN_EXPR, seq("(", $._expression_block, ")")),

    _high_prec_app: ($) =>
      prec.left(
        PREC.DOT + 1,
        seq(
          $._expression,
          choice(
            $.unit,
            seq(token.immediate(prec(10000, "(")), $._expression_block, ")"),
          ),
        ),
      ),

    _low_prec_app: ($) =>
      prec.left(PREC.APP_EXPR, seq($._expression, $._expression)),

    dot_expression: ($) =>
      prec.right(
        PREC.DOT,
        seq(
          field("base", $._expression),
          ".",
          field("field", $.long_identifier_or_op),
        ),
      ),

    infix_expression: ($) =>
      prec.left(
        PREC.SPECIAL_INFIX,
        seq($._expression, $.infix_op, $._expression),
      ),

    sequential_expression: ($) =>
      prec.right(
        PREC.SEQ_EXPR,
        seq(
          $._expression,
          repeat1(
            prec.right(
              PREC.SEQ_EXPR,
              seq(alias($._newline, ";"), $._expression),
            ),
          ),
        ),
      ),

    //
    // Expressions (END)
    //


    //
    // Type rules (BEGIN)

    _type: ($) =>
      prec(
        4,
        choice(
          $.simple_type,
          $.generic_type,
          $.paren_type,
          $.function_type,
          $.compound_type,
          $.postfix_type,
          $.list_type,
          $.static_type,
          $.type_argument,
        ),
      ),

    simple_type: ($) => choice($.long_identifier, $._static_type_identifier),
    generic_type: ($) =>
      prec.right(
        5,
        seq($.long_identifier, "<", optional($.type_attributes), ">"),
      ),
    paren_type: ($) => seq("(", $._type, ")"),
    function_type: ($) => prec.right(seq($._type, "->", $._type)),
    compound_type: ($) =>
      prec.right(seq($._type, repeat1(prec.right(seq("*", $._type))))),
    postfix_type: ($) => prec.left(4, seq($._type, $.long_identifier)),
    list_type: ($) => seq($._type, "[]"),
    static_type: ($) => prec(10, seq($._type, $.type_arguments)),

    types: ($) =>
      seq($._type, repeat(prec.left(PREC.COMMA - 1, seq(",", $._type)))),

    _static_type_identifier: ($) =>
      prec(10, $.identifier),


    type_attribute: ($) =>
      $._type,


    type_attributes: ($) =>
      seq(
        $.type_attribute,
        repeat(prec.right(PREC.COMMA, seq(",", $.type_attribute))),
      ),


    type_argument: ($) =>
      prec(
        10,
        choice(
          "_",
          seq(
            $._static_type_identifier
          ),
        ),
      ),

    type_argument_defn: ($) => $.type_argument,

    type_arguments: ($) =>
      seq(
        "<",
        $.type_argument_defn,
        repeat(prec.left(PREC.COMMA, seq(",", $.type_argument_defn))),
        ">",
      ),

    member_signature: ($) =>
      seq(
        $.identifier,
        optional($.type_arguments),
        ":",
        $.curried_spec
      ),

    curried_spec: ($) => seq(repeat(seq($.arguments_spec, "->")), $._type),

    argument_spec: ($) =>
      prec.left(
        seq(optional($.argument_name_spec), $._type),
      ),

    arguments_spec: ($) =>
      seq($.argument_spec, repeat(seq("*", $.argument_spec))),

    argument_name_spec: ($) =>
      seq(field("name", $.identifier), ":"),


    type_definition: ($) =>
      prec.left(
        seq(
          "type",
          $._type_defn_body,
          repeat(seq("and", $._type_defn_body)),
        ),
      ),

    _type_defn_body: ($) =>
      choice(
        $.record_type_defn,
        $.union_type_defn,
        $.enum_type_defn,
        $.type_abbrev_defn,
      ),

    type_name: ($) =>
      prec(
        2,
        seq(
          optional($.access_modifier),
          choice(
            seq(
              field("type_name", $.long_identifier),
              optional($.type_arguments),
            ),
            seq(optional($.type_argument), field("type_name", $.identifier)), // Covers `type 'a option = Option<'a>`
          ),
        ),
      ),

    type_abbrev_defn: ($) =>
      seq($.type_name, "=", scoped($._type, $._indent, $._dedent)),


    _record_type_defn_inner: ($) =>
      seq(
        optional($.access_modifier),
        "{",
        scoped($.record_fields, $._indent, $._dedent),
        "}"
      ),

    record_type_defn: ($) =>
      prec.left(
        seq(
          $.type_name,
          "=",
          scoped($._record_type_defn_inner, $._indent, $._dedent),
        ),
      ),

    record_fields: ($) =>
      seq(
        $.record_field,
        repeat(seq($._newline, $.record_field)),
        optional($._newline),
      ),

    record_field: ($) =>
      seq(
        optional("mutable"),
        optional($.access_modifier),
        $.identifier,
        ":",
        $._type,
      ),

    enum_type_defn: ($) =>
      seq($.type_name, "=", scoped($.enum_type_cases, $._indent, $._dedent)),

    enum_type_cases: ($) =>
      choice(
        seq(optional("|"), $.enum_type_case),
        seq(seq("|", $.enum_type_case), repeat1(seq("|", $.enum_type_case))),
      ),

    enum_type_case: ($) => seq($.identifier, "=", $.const),

    _union_type_defn_inner: ($) =>
      seq(
        optional($.access_modifier),
        $.union_type_cases
      ),

    union_type_defn: ($) =>
      prec.left(
        seq(
          $.type_name,
          "=",
          scoped($._union_type_defn_inner, $._indent, $._dedent),
        ),
      ),

    union_type_cases: ($) =>
      seq(
        optional("|"),
        $.union_type_case,
        repeat(seq("|", $.union_type_case)),
      ),

    union_type_case: ($) =>
      prec(
        8,
        seq(
          choice(
            $.identifier,
            seq($.identifier, "of", $.union_type_fields),
            seq($.identifier, ":", $._type),
          ),
        ),
      ),

    union_type_fields: ($) =>
      seq($.union_type_field, repeat(seq("*", $.union_type_field))),

    union_type_field: ($) =>
      prec.left(choice($._type, seq($.identifier, ":", $._type))),


    _type_defn_elements: ($) =>
      choice(
        $._member_defns,
      ),


    _member_defns: ($) => prec.left(seq($.member_defn, repeat($.member_defn))),


    member_defn: ($) =>
      prec(
        PREC.APP_EXPR + 100000,
        seq(
          choice(
            seq(
              "member",
              optional($.access_modifier),
              $.method_or_prop_defn,
            ),
            $.additional_constr_defn,
          ),
        ),
      ),

    property_or_ident: ($) =>
      choice(
        seq(
          field("instance", $.identifier),
          ".",
          field("method", $.identifier),
        ),
        $._identifier_or_op,
      ),

    _method_defn: ($) =>
      choice(
        seq(
          optional($.type_arguments),
          field("args", repeat1($._pattern)),
          "=",
          $._expression_block,
        ),
      ),

    method_or_prop_defn: ($) =>
      prec(
        3,
        seq(
          field("name", $.property_or_ident),
          choice(
            seq(
              "with",
              scoped($._function_or_value_defns, $._indent, $._dedent),
            ),
            $._method_defn,
          ),
        ),
      ),

    additional_constr_defn: ($) =>
      seq(
        optional($.access_modifier),
        "new",
        $._pattern,
        "=",
        $._expression_block,
      ),

    field_initializer: ($) =>
      prec(
        PREC.SPECIAL_INFIX + 1,
        seq(
          field("field", $.long_identifier),
          token(prec(10000000, "=")),
          field("value", $._expression),
        ),
      ),

    field_initializers: ($) =>
      prec(
        10000000,
        seq($.field_initializer, repeat(seq($._newline, $.field_initializer))),
      ),

    //
    // Type rules (END)
    //

    //
    // Constants (BEGIN)
    //
    _escape_char: (_) => token.immediate(prec(100, /\\["\'ntbrafv]/)),
    _non_escape_char: (_) => token.immediate(prec(100, /\\[^"\'ntbrafv]/)),
    // using \u0008 to model \b
    _simple_char_char: (_) => token.immediate(/[^\n\t\r\u0008\a\f\v'\\]/),
    _unicodegraph_short: (_) => /\\u[0-9a-fA-F]{4}/,
    _unicodegraph_long: (_) => /\\u[0-9a-fA-F]{8}/,
    _trigraph: (_) => /\\[0-9]{3}/,

    _char_char: ($) =>
      choice(
        $._simple_char_char,
        $._escape_char,
        $._trigraph,
        $._unicodegraph_short,
      ),

    // note: \n is allowed in strings
    _simple_string_char: ($) =>
      choice(
        $._inside_string_marker,
        token.immediate(prec(1, /[^\t\r\u0008\a\f\v\\"]/)),
      ),

    _string_char: ($) =>
      choice(
        $._simple_string_char,
        $._escape_char,
        $._trigraph,
        $._unicodegraph_short,
        $._non_escape_char,
        $._unicodegraph_long,
      ),

    char: (_) =>
      prec(
        -1,
        /'([^\n\t\r\u0008\a\f\v'\\]|\\["\'ntbrafv]|\\[0-9]{3}|\\u[0-9a-fA-F]{4})*'B?/,
      ),

    format_string_eval: ($) =>
      seq(token.immediate(prec(1000, "{")), $._expression, "}"),

    format_string: ($) =>
      seq(
        token(prec(100, '$"')),
        repeat(choice($.format_string_eval, $._string_char)),
        '"',
      ),

    _string_literal: ($) => seq('"', repeat($._string_char), '"'),

    string: ($) => choice($._string_literal, $.format_string),

    _verbatim_string_char: ($) =>
      choice($._simple_string_char, $._non_escape_char, "\\", /\"\"/),
    verbatim_string: ($) =>
      seq('@"', repeat($._verbatim_string_char), token.immediate('"')),
    bytearray: ($) => seq('"', repeat($._string_char), token.immediate('"B')),
    verbatim_bytearray: ($) =>
      seq('@"', repeat($._verbatim_string_char), token.immediate('"B')),

    format_triple_quoted_string: ($) =>
      seq(
        token(prec(100, '$"""')),
        // repeat(choice($.format_string_eval, $._string_char)),
        $._triple_quoted_content,
        '"""',
      ),

    triple_quoted_string: ($) =>
      choice(
        seq('"""', $._triple_quoted_content, '"""'),
        $.format_triple_quoted_string,
      ),

    bool: (_) => token(choice("true", "false")),

    unit: (_) => token(prec(100000, "()")),

    const: ($) =>
      choice(
        $.sbyte,
        $.int16,
        $.int32,
        $.int64,
        $.byte,
        $.uint16,
        $.uint32,
        $.int,
        $.xint,
        $.nativeint,
        $.unativeint,
        $.decimal,
        $.float,
        $.uint64,
        $.ieee32,
        $.ieee64,
        $.bignum,
        $.char,
        $.string,
        $.verbatim_string,
        $.triple_quoted_string,
        $.bytearray,
        $.verbatim_bytearray,
        $.bool,
        $.unit,
      ),

    // Identifiers:
    long_identifier: ($) =>
      prec.right(seq($.identifier, repeat(seq(".", $.identifier)))),

    op_identifier: (_) =>
      token(
        prec(
          1000,
          seq(
            "(",
            /\s*/,
            choice("?", /[!%&*+-./<=>@^|~][!%&*+-./<=>@^|~?]*/, ".. .."),
            /\s*/,
            ")",
          ),
        ),
      ),

    _identifier_or_op: ($) =>
      choice($.identifier, $.op_identifier),

    _infix_or_prefix_op: (_) => choice("+", "-", "+.", "-.", "%", "&", "&&"),

    prefix_op: ($) =>
      prec.left(
        choice($._infix_or_prefix_op, repeat1("~"), /[!?][!%&*+-./<=>@^|~?]*/),
      ),

    infix_op: ($) =>
      prec(
        PREC.INFIX_OP,
        choice(
          $._infix_or_prefix_op,
          token.immediate(prec(1, /[+-]/)),
          /[-+<>|&^*/'%@][!%&*+./<=>@^|~?-]*/,
          "||",
          "=",
          "!=",
          ":=",
          "::",
          "$",
          "or",
          "?",
          "?",
          "?<-",
        ),
      ),

    // Numbers
    int: (_) => token(/[+-]?([0-9]_?)+/),
    xint: (_) =>
      token(
        choice(/0[xX]([0-9a-fA-F]_?)+/, /0[oO]([0-7]_?)+/, /0[bB]([0-1]_?)+/),
      ),

    sbyte: ($) => seq(choice($.int, $.xint), token.immediate("y")),
    byte: ($) => seq(choice($.int, $.xint), token.immediate("uy")),
    int16: ($) => seq(choice($.int, $.xint), token.immediate("s")),
    uint16: ($) => seq(choice($.int, $.xint), token.immediate("us")),
    int32: ($) => seq(choice($.int, $.xint), token.immediate("l")),
    uint32: ($) =>
      seq(choice($.int, $.xint), token.immediate(choice("ul", "u"))),
    nativeint: ($) => seq(choice($.int, $.xint), token.immediate("n")),
    unativeint: ($) => seq(choice($.int, $.xint), token.immediate("un")),
    int64: ($) => seq(choice($.int, $.xint), token.immediate("L")),
    uint64: ($) =>
      seq(choice($.int, $.xint), token.immediate(choice("UL", "uL"))),

    ieee32: ($) =>
      choice(
        seq($.float, token.immediate("f")),
        seq($.xint, token.immediate("lf")),
      ),
    ieee64: ($) => seq($.xint, token.immediate("LF")),

    bignum: ($) => seq($.int, token.immediate(/[QRZING]/)),
    decimal: ($) => seq(choice($.float, $.int), token.immediate(/[Mm]/)),

    float: ($) =>
      prec.right(
        alias(
          choice(
            seq($.int, token.immediate("."), optional($.int)),
            seq(
              $.int,
              optional(seq(token.immediate("."), $.int)),
              token.immediate(/[eE][+-]?/),
              $.int,
            ),
          ),
          "float",
        ),
      ),

    //
    // Constants (END)
    //
    //
    block_comment: ($) =>
      seq("(*", $.block_comment_content, token.immediate("*)")),
    line_comment: (_) => token(seq(/\/\/+[^\n\r]*/)),

    identifier: (_) =>
      token(
        choice(/[_\p{XID_Start}][_'\p{XID_Continue}]*/, /``([^`\n\r\t])+``/),
      ),
  },
});

function scoped(rule, indent, dedent) {
  return field("block", seq(indent, rule, dedent));
}

