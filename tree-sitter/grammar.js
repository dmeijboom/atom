module.exports = grammar({
  name: "atom",

  rules: {
    source_file: ($) => repeat($._statement),

    _statement: ($) =>
      choice(
        $.import_statement,
        $.function_declaration,
        $.class_declaration,
        $.variable_declaration,
        $.assignment_statement,
        $.if_statement,
        $.for_statement,
        $.return_statement,
        $.expression_statement,
      ),

    import_statement: ($) =>
      seq(
        "import",
        field("path", $.import_path),
        ";",
      ),

    import_path: ($) =>
      seq(
        $.identifier,
        repeat(seq("/", $.identifier)),
      ),

    function_declaration: ($) =>
      choice(
        // Arrow function: [pub] fn[*] name() => expr;
        seq(
          optional("pub"),
          "fn",
          optional("*"),
          field("name", $.identifier),
          field("parameters", $.parameter_list),
          "=>",
          field("body", $._expression),
          ";",
        ),
        // Expression block function: [pub] fn[*] name() { expr };
        seq(
          optional("pub"),
          "fn",
          optional("*"),
          field("name", $.identifier),
          field("parameters", $.parameter_list),
          "{",
          field("body", $._expression),
          "}",
          ";",
        ),
        // Regular block function: [pub] fn[*] name() { statements }
        seq(
          optional("pub"),
          "fn",
          optional("*"),
          field("name", $.identifier),
          field("parameters", $.parameter_list),
          field("body", $.block),
        ),
      ),

    class_declaration: ($) =>
      seq(
        optional("pub"),
        "class",
        field("name", $.identifier),
        field("body", $.class_body),
      ),

    class_body: ($) =>
      seq(
        "{",
        repeat($.function_declaration),
        "}",
      ),

    parameter_list: ($) =>
      seq(
        "(",
        optional(seq($.identifier, repeat(seq(",", $.identifier)))),
        ")",
      ),

    variable_declaration: ($) =>
      choice(
        // let name = value;
        seq(
          "let",
          field("name", $.identifier),
          "=",
          field("value", $._expression),
          ";",
        ),
        // let name;
        seq(
          "let",
          field("name", $.identifier),
          ";",
        ),
      ),

    assignment_statement: ($) =>
      seq(
        field("left", $._assignable),
        field("operator", choice("=", "+=", "-=", "*=", "/=", "%=", "^=")),
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
      choice(
        // for condition { body }
        seq(
          "for",
          field("condition", $._expression),
          field("body", $.block),
        ),
        // for ident in expr { body }
        seq(
          "for",
          field("variable", $.identifier),
          "in",
          field("iterable", $._expression),
          field("body", $.block),
        ),
        // for init; condition; update { body }
        seq(
          "for",
          field("init", choice($.variable_declaration, $.assignment_statement)),
          field("condition", $._expression),
          ";",
          field("update", $.assignment_expression),
          field("body", $.block),
        ),
      ),

    assignment_expression: ($) =>
      seq(
        field("left", $._assignable),
        field("operator", choice("=", "+=", "-=", "*=", "/=", "%=", "^=")),
        field("right", $._expression),
      ),

    return_statement: ($) =>
      seq("return", optional(field("value", $._expression)), ";"),

    expression_statement: ($) => seq($._expression, ";"),

    block: ($) =>
      seq(
        "{",
        repeat($._statement),
        optional($._expression),
        "}",
      ),

    _expression: ($) =>
      choice(
        $.binary_expression,
        $.unary_expression,
        $.call_expression,
        $.builtin_call,
        $.member_access,
        $.array_expression,
        $.index_expression,
        $.identifier,
        $.number,
        $.string,
        $.atom,
        $.parenthesized_expression,
      ),

    binary_expression: ($) =>
      choice(
        prec.left(12, seq($._expression, "*", $._expression)),
        prec.left(12, seq($._expression, "/", $._expression)),
        prec.left(12, seq($._expression, "%", $._expression)),
        prec.left(11, seq($._expression, "+", $._expression)),
        prec.left(11, seq($._expression, "-", $._expression)),
        prec.left(10, seq($._expression, "<<", $._expression)),
        prec.left(10, seq($._expression, ">>", $._expression)),
        prec.left(9, seq($._expression, "<", $._expression)),
        prec.left(9, seq($._expression, "<=", $._expression)),
        prec.left(9, seq($._expression, ">", $._expression)),
        prec.left(9, seq($._expression, ">=", $._expression)),
        prec.left(8, seq($._expression, "==", $._expression)),
        prec.left(8, seq($._expression, "!=", $._expression)),
        prec.left(8, seq($._expression, "is", $._expression)),
        prec.left(7, seq($._expression, "&", $._expression)),
        prec.left(6, seq($._expression, "^", $._expression)),
        prec.left(5, seq($._expression, "|", $._expression)),
        prec.left(4, seq($._expression, "&&", $._expression)),
        prec.left(3, seq($._expression, "||", $._expression)),
      ),

    unary_expression: ($) =>
      choice(
        prec(15, seq("-", $._expression)),
        prec(15, seq("+", $._expression)),
        prec(15, seq("!", $._expression)),
      ),

    call_expression: ($) =>
      prec(
        14,
        seq(
          field("function", $._expression),
          field("arguments", $.argument_list),
        ),
      ),

    builtin_call: ($) =>
      seq(
        "@",
        field("name", $.identifier),
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

    array_expression: ($) =>
      seq(
        "[",
        optional(seq($._expression, repeat(seq(",", $._expression)))),
        "]",
      ),

    index_expression: ($) =>
      prec(
        14,
        seq(
          field("object", $._expression),
          "[",
          field(
            "index",
            choice(
              $._expression,
              seq(optional($._expression), ":", optional($._expression)),
            ),
          ),
          "]",
        ),
      ),

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    number: ($) => /\d+(\.\d+)?/,

    string: ($) =>
      seq('"', repeat(choice($.string_content, $.escape_sequence)), '"'),

    string_content: ($) => /[^"\\]+/,

    escape_sequence: ($) =>
      seq("\\", choice(/[\\"/bfnrt]/, /u[0-9a-fA-F]{4}/, /x[0-9a-fA-F]{2}/)),

    atom: ($) => /:[a-zA-Z_][a-zA-Z0-9_]*/,

    comment: ($) =>
      choice(seq("//", /.*/), seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/")),
  },

  extras: ($) => [/\s/, $.comment],
});
