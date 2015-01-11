package com.enkidu.lignum.parsers.commons

import org.parboiled2
import org.parboiled2.{ CharPredicate, Rule0 }

import scala.language.implicitConversions

trait AbstractParser extends parboiled2.Parser with SpecialCharacters with Keywords{

  protected implicit def str2Rule(s: String): Rule0 =
    if (s.endsWith("  "))
      rule { str(s.dropRight(2)) ~ oneOrMore(spacing) }
    else if (s.endsWith(" "))
      rule { str(s.dropRight(1)) ~ whitespace }
    else rule { str(s) }

  protected def whitespace: Rule0 = rule { zeroOrMore(spacing) }
  private def spacing: Rule0 = rule { anyOf(" \t\u000C") | newLine | eolComment | traditionalComment }
  private def newLine: Rule0 = rule { "\r\n" | '\n' }
  private def traditionalComment: Rule0 = rule { "/*" ~ zeroOrMore(!("*/") ~ CharPredicate.All) ~ "*/" }
  private def eolComment: Rule0 = rule { "//" ~ zeroOrMore(!("\n" | "\r\n") ~ CharPredicate.All) ~ ("\n" | "\r\n") }

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


}
