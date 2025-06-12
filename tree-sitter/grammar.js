module.exports = grammar({
  name: "atom",

  rules: {
    source_file: ($) => repeat($._statement),

    _statement: ($) =>
      choice(
        $.extern_declaration,
        $.function_declaration,
        $.class_declaration,
        $.variable_declaration,
        $.assignment_statement,
        $.if_statement,
        $.for_statement,
        $.return_statement,
        $.expression_statement,
      ),

    extern_declaration: ($) =>
      seq(
        "extern",
        "fn",
        field("name", $.identifier),
        field("parameters", $.parameter_list),
        ";",
      ),

    function_declaration: ($) =>
      choice(
        // Arrow function: fn name() => expr;
        seq(
          "fn",
          field("name", $.identifier),
          field("parameters", $.parameter_list),
          "=>",
          field("body", $._expression),
          ";"
        ),
        // Expression block function: fn name() { expr };
        seq(
          "fn",
          field("name", $.identifier),
          field("parameters", $.parameter_list),
          "{",
          field("body", $._expression),
          "}",
          ";"
        ),
        // Regular block function: fn name() { statements }
        seq(
          "fn",
          field("name", $.identifier),
          field("parameters", $.parameter_list),
          field("body", $.block),
        )
      ),

    class_declaration: ($) =>
      seq("class", field("name", $.identifier), field("body", $.class_body)),

    class_body: ($) =>
      seq(
        "{",
        repeat(choice($.extern_declaration, $.function_declaration)),
        "}",
      ),

    parameter_list: ($) =>
      seq(
        "(",
        optional(seq($.identifier, repeat(seq(",", $.identifier)))),
        ")",
      ),

    variable_declaration: ($) =>
      seq(
        "let",
        field("name", $.identifier),
        "=",
        field("value", $._expression),
        ";",
      ),

    assignment_statement: ($) =>
      seq(
        field("left", $._assignable),
        field("operator", choice("=", "+=", "-=", "*=", "/=", "^=")),
        field("right", $._expression),
        ";",
      ),

    _assignable: ($) => choice($.identifier, $.member_access),

    if_statement: ($) =>
      seq(
        "if",
        field("condition", $._expression),
        field("body", $.block),
        optional(seq("else", field("else_body", $.block))),
      ),

    for_statement: ($) =>
      seq(
        "for",
        field("init", choice($.variable_declaration, $.assignment_statement)),
        field("condition", $._expression),
        ";",
        field("update", $.assignment_expression),
        field("body", $.block),
      ),

    assignment_expression: ($) =>
      seq(
        field("left", $._assignable),
        field("operator", choice("=", "+=", "-=", "*=", "/=", "^=")),
        field("right", $._expression),
      ),

    return_statement: ($) =>
      seq("return", optional(field("value", $._expression)), ";"),

    expression_statement: ($) => seq($._expression, ";"),

    block: ($) => seq("{", repeat($._statement), "}"),

    _expression: ($) =>
      choice(
        $.binary_expression,
        $.unary_expression,
        $.call_expression,
        $.member_access,
        $.identifier,
        $.number,
        $.string,
        $.boolean,
        $.parenthesized_expression,
      ),

    binary_expression: ($) =>
      choice(
        prec.left(10, seq($._expression, "*", $._expression)),
        prec.left(10, seq($._expression, "/", $._expression)),
        prec.left(10, seq($._expression, "%", $._expression)),
        prec.left(9, seq($._expression, "+", $._expression)),
        prec.left(9, seq($._expression, "-", $._expression)),
        prec.left(8, seq($._expression, "^", $._expression)),
        prec.left(6, seq($._expression, "<", $._expression)),
        prec.left(6, seq($._expression, "<=", $._expression)),
        prec.left(6, seq($._expression, ">", $._expression)),
        prec.left(6, seq($._expression, ">=", $._expression)),
        prec.left(5, seq($._expression, "==", $._expression)),
        prec.left(5, seq($._expression, "!=", $._expression)),
        prec.left(3, seq($._expression, "&&", $._expression)),
        prec.left(2, seq($._expression, "||", $._expression)),
      ),

    unary_expression: ($) =>
      choice(
        prec(11, seq("-", $._expression)),
        prec(11, seq("+", $._expression)),
        prec(11, seq("!", $._expression)),
      ),

    call_expression: ($) =>
      seq(
        field("function", $._expression),
        field("arguments", $.argument_list),
      ),

    member_access: ($) =>
      seq(field("object", $._expression), ".", field("property", $.identifier)),

    argument_list: ($) =>
      seq(
        "(",
        optional(seq($._expression, repeat(seq(",", $._expression)))),
        ")",
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    number: ($) => /\d+(\.\d+)?/,

    string: ($) =>
      seq('"', repeat(choice($.string_content, $.escape_sequence)), '"'),

    string_content: ($) => /[^"\\]+/,

    escape_sequence: ($) =>
      seq("\\", choice(/[\\"/bfnrt]/, /u[0-9a-fA-F]{4}/, /x[0-9a-fA-F]{2}/)),

    boolean: ($) => choice("true", "false"),

    comment: ($) =>
      choice(seq("//", /.*/), seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/")),
  },

  extras: ($) => [/\s/, $.comment],
});