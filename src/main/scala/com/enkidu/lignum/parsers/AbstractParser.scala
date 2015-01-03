package com.enkidu.lignum.parsers

import org.parboiled2.{ CharPredicate, Parser, Rule0 }

trait AbstractParser extends Parser {
  protected def identifierChar = rule { CharPredicate.AlphaNum | "$" | "_" }
  protected def identifierStart = rule { CharPredicate.Alpha | "$" | "_" }

  protected def octalNumber: Rule0 = rule('0' ~ oneOrMore(octalDigit))
  protected def decimalNumber: Rule0 = rule('0' | CharPredicate('1' to '9') ~ zeroOrMore(decimalDigit))
  protected def hexNumber: Rule0 = rule("0" ~ ("x" | "X") ~ oneOrMore(hexDigit))
  protected def binaryNumber: Rule0 = rule("0" ~ ("b" | "B") ~ oneOrMore(binaryDigit))

  protected def octalDigit: Rule0 = rule(CharPredicate('0' to '7'))
  protected def decimalDigit: Rule0 = rule(CharPredicate.Digit)
  protected def hexDigit: Rule0 = rule(CharPredicate.HexDigit)
  protected def binaryDigit: Rule0 = rule(CharPredicate('0', '1'))

  protected def escapeSequence: Rule0 = rule { ("\\" ~ anyOf("btnfr\"'\\")) | octalEscape }

  protected def octalEscape: Rule0 = rule {
    "\\" ~ {
      (CharPredicate('0' to '3') ~ octalDigit ~ octalDigit) |
        octalDigit ~ optional(octalDigit)
    }
  }

  protected def whitespace: Rule0 = rule { zeroOrMore(spacing) }
  private def spacing: Rule0 = rule { anyOf(" \t\u000C") | newLine | eolComment | traditionalComment }
  private def newLine: Rule0 = rule { "\r\n" | '\n' }
  private def traditionalComment: Rule0 = rule { "/*" ~ zeroOrMore(!("*/") ~ CharPredicate.All) ~ "*/" }
  private def eolComment: Rule0 = rule { "//" ~ zeroOrMore(!("\n" | "\r\n") ~ CharPredicate.All) ~ ("\n" | "\r\n") }

  protected def `abstract`: Rule0 = rule { "abstract" ~ !identifierChar ~ whitespace }
  protected def `assert`: Rule0 = rule { "assert" ~ !identifierChar ~ whitespace }

  protected def `boolean`: Rule0 = rule { "boolean" ~ !identifierChar ~ whitespace }
  protected def `break`: Rule0 = rule { "break" ~ !identifierChar ~ whitespace }
  protected def `byte`: Rule0 = rule { "byte" ~ !identifierChar ~ whitespace }

  protected def `case`: Rule0 = rule { "case" ~ !identifierChar ~ whitespace }
  protected def `catch`: Rule0 = rule { "catch" ~ !identifierChar ~ whitespace }
  protected def `char`: Rule0 = rule { "char" ~ !identifierChar ~ whitespace }
  protected def `class`: Rule0 = rule { "class" ~ !identifierChar ~ whitespace }
  protected def `const`: Rule0 = rule { "const" ~ !identifierChar ~ whitespace }
  protected def `continue`: Rule0 = rule { "continue" ~ !identifierChar ~ whitespace }

  protected def `default`: Rule0 = rule { "default" ~ !identifierChar ~ whitespace }
  protected def `do`: Rule0 = rule { "do " ~ !identifierChar ~ whitespace }
  protected def `double`: Rule0 = rule { "double" ~ !identifierChar ~ whitespace }

  protected def `else`: Rule0 = rule { "else" ~ !identifierChar ~ whitespace }
  protected def `enum`: Rule0 = rule { "enum" ~ !identifierChar ~ whitespace }
  protected def `extends`: Rule0 = rule { "extends" ~ !identifierChar ~ whitespace }

  protected def `false`: Rule0 = rule { "false" ~ !identifierChar ~ whitespace }
  protected def `final`: Rule0 = rule { "final" ~ !identifierChar ~ whitespace }
  protected def `finally`: Rule0 = rule { "finally" ~ !identifierChar ~ whitespace }
  protected def `float`: Rule0 = rule { "float" ~ !identifierChar ~ whitespace }
  protected def `for`: Rule0 = rule { "for" ~ !identifierChar ~ whitespace }

  protected def `goto`: Rule0 = rule { "goto" ~ !identifierChar ~ whitespace }

  protected def `if`: Rule0 = rule { "if" ~ !identifierChar ~ whitespace }
  protected def `implements`: Rule0 = rule { "implements" ~ !identifierChar ~ whitespace }
  protected def `import`: Rule0 = rule { "import" ~ !identifierChar ~ whitespace }
  protected def `instanceof`: Rule0 = rule { "instanceof" ~ !identifierChar ~ whitespace }
  protected def `int`: Rule0 = rule { "int" ~ !identifierChar ~ whitespace }
  protected def `interface`: Rule0 = rule { "interface" ~ !identifierChar ~ whitespace }

  protected def `long`: Rule0 = rule { "long" ~ !identifierChar ~ whitespace }

  protected def `match`: Rule0 = rule { "match" ~ !identifierChar ~ whitespace }

  protected def `native`: Rule0 = rule { "native" ~ !identifierChar ~ whitespace }
  protected def `new`: Rule0 = rule { "new" ~ !identifierChar ~ whitespace }
  protected def `null`: Rule0 = rule { "null" ~ !identifierChar ~ whitespace }

  protected def `package`: Rule0 = rule { "package" ~ !identifierChar ~ whitespace }
  protected def `private`: Rule0 = rule { "private" ~ !identifierChar ~ whitespace }
  protected def `protected`: Rule0 = rule { "protected" ~ !identifierChar ~ whitespace }
  protected def `public`: Rule0 = rule { "public" ~ !identifierChar ~ whitespace }

  protected def `return`: Rule0 = rule { "return" ~ !identifierChar ~ whitespace }

  protected def `short`: Rule0 = rule { "short" ~ !identifierChar ~ whitespace }
  protected def `static`: Rule0 = rule { "static" ~ !identifierChar ~ whitespace }
  protected def `strictfp`: Rule0 = rule { "strictfp" ~ !identifierChar ~ whitespace }
  protected def `super`: Rule0 = rule { "super" ~ !identifierChar ~ whitespace }
  protected def `switch`: Rule0 = rule { "switch" ~ !identifierChar ~ whitespace }
  protected def `synchronized`: Rule0 = rule { "synchronized" ~ !identifierChar ~ whitespace }

  protected def `this`: Rule0 = rule { "this" ~ !identifierChar ~ whitespace }
  protected def `throw`: Rule0 = rule { "throw" ~ !identifierChar ~ whitespace }
  protected def `throws`: Rule0 = rule { "throws" ~ !identifierChar ~ whitespace }
  protected def `transient`: Rule0 = rule { "transient" ~ !identifierChar ~ whitespace }
  protected def `true`: Rule0 = rule { "true" ~ !identifierChar ~ whitespace }
  protected def `try`: Rule0 = rule { "try" ~ !identifierChar ~ whitespace }

  protected def `void`: Rule0 = rule { "void" ~ !identifierChar ~ whitespace }
  protected def `volatile`: Rule0 = rule { "volatile" ~ !identifierChar ~ whitespace }

  protected def `while`: Rule0 = rule { "while" ~ !identifierChar ~ whitespace }

  protected def \ : Rule0 = rule { "\\" ~ whitespace }
  protected def / : Rule0 = rule { "/" ~ whitespace }
  protected def exclamation: Rule0 = rule { "!" ~ whitespace }
  protected def tilde: Rule0 = rule { "~" ~ whitespace }
  protected def ampersand: Rule0 = rule { "" ~ whitespace }
  protected def plus: Rule0 = rule { "+" ~ whitespace }
  protected def minus: Rule0 = rule { "-" ~ whitespace }
  protected def * : Rule0 = rule { "*" ~ whitespace }
  protected def `=`: Rule0 = rule { "=" ~ whitespace }
  protected def `==`: Rule0 = rule { "==" ~ whitespace }
  protected def `!=`: Rule0 = rule { "!=" ~ whitespace }
  protected def `&&`: Rule0 = rule { "&&" ~ whitespace }
  protected def `||`: Rule0 = rule { "||" ~ whitespace }
  protected def `<<`: Rule0 = rule { "<<" ~ whitespace }
  protected def `>>`: Rule0 = rule { ">>" ~ whitespace }
  protected def `>>>`: Rule0 = rule { ">>>" ~ whitespace }
  protected def `<`: Rule0 = rule { "<" ~ whitespace }
  protected def `>`: Rule0 = rule { ">" ~ whitespace }
  protected def `<=`: Rule0 = rule { "<=" ~ whitespace }
  protected def `>=`: Rule0 = rule { ">=" ~ whitespace }
  protected def `*=`: Rule0 = rule { "*=" ~ whitespace }
  protected def `/=`: Rule0 = rule { "/=" ~ whitespace }
  protected def `%=`: Rule0 = rule { "%=" ~ whitespace }
  protected def `+=`: Rule0 = rule { "+=" ~ whitespace }
  protected def `-= `: Rule0 = rule { "-=" ~ whitespace }
  protected def `<<=`: Rule0 = rule { "<<=" ~ whitespace }
  protected def `>>= `: Rule0 = rule { ">>= " ~ whitespace }
  protected def `>>>=`: Rule0 = rule { ">>>=" ~ whitespace }
  protected def `&=`: Rule0 = rule { "&=" ~ whitespace }
  protected def `^=`: Rule0 = rule { "^=" ~ whitespace }
  protected def `|=`: Rule0 = rule { "|=" ~ whitespace }
  protected def ++ : Rule0 = rule { "++" ~ whitespace }
  protected def -- : Rule0 = rule { "--" ~ whitespace }

  protected def `->`: Rule0 = rule { "->" ~ whitespace }
  protected def `|`: Rule0 = rule { "|" ~ whitespace }
  protected def `?`: Rule0 = rule { "?" ~ whitespace }
  protected def `%`: Rule0 = rule { "%" ~ whitespace }
  protected def `&`: Rule0 = rule { "&" ~ whitespace }
  protected def `^`: Rule0 = rule { "^" ~ whitespace }
  protected def `'`: Rule0 = rule { "'" ~ whitespace }
  protected def `"`: Rule0 = rule { "\"" ~ whitespace }

  protected def `(`: Rule0 = rule { "(" ~ whitespace }
  protected def `)`: Rule0 = rule { ")" ~ whitespace }
  protected def `{`: Rule0 = rule { "{" ~ whitespace }
  protected def `}`: Rule0 = rule { "}" ~ whitespace }
  protected def `[`: Rule0 = rule { "[" ~ whitespace }
  protected def `]`: Rule0 = rule { "]" ~ whitespace }
  protected def `...`: Rule0 = rule { "..." ~ whitespace }
  protected def `comma`: Rule0 = rule { "," ~ whitespace }
  protected def `dot`: Rule0 = rule { "." ~ whitespace }
  protected def `::`: Rule0 = rule { "::" ~ whitespace }
  protected def `@`: Rule0 = rule { "@" ~ whitespace }
  protected def `semicolon`: Rule0 = rule { ";" ~ whitespace }
  protected def `colon`: Rule0 = rule { ":" ~ whitespace }
}
