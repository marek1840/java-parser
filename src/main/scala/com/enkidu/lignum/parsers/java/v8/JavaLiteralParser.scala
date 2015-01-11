package com.enkidu.lignum.parsers.java.v8

import org.parboiled2.{ CharPredicate, Rule1 }
import com.enkidu.lignum.parsers.ast.expression.discardable.literals._
import org.parboiled2.ParserInput

abstract class JavaLiteralParser extends JavaIdentifierParser {
  def literal: Rule1[Literal] = rule {
    { floatLiteral ~> FloatLiteral|
      integerLiteral ~> IntegerLiteral|
      characterLiteral ~> CharLiteral|
      stringLiteral ~> StringLiteral|
      `null` ~ push(NullLiteral) |
      booleanLiteral ~> BooleanLiteral } ~ whitespace
  }

  protected def integerLiteral: Rule1[String] = rule {
    capture((hexNumber | binaryNumber | octalNumber | decimalNumber) ~ optionalIntegerSuffix)
  }

  protected def floatLiteral: Rule1[String] = rule { hexFloat | decimalFloat }

  protected def booleanLiteral: Rule1[String] = rule { capture(`true` | `false`) }

  protected def characterLiteral: Rule1[String] = rule {
    capture("'" ~ (escapeSequence | unicodeEscape | noneOf("'\\" + 10.toChar + 13.toChar)) ~ "'")
  }

  protected def stringLiteral: Rule1[String] = rule {
    capture('"' ~ zeroOrMore(escapeSequence | noneOf("\\\"\n\r")) ~ '"')
  }


  private def unicodeEscape = rule {
    "\\" ~ oneOrMore("u") ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit
  }

  private def optionalIntegerSuffix = rule(optional("l" | "L"))
  private def optionalFloatSuffix = rule(optional("f" | "F" | "d" | "D"))
  private def exponent = rule(("e" | "E") ~ signedInteger)
  private def signedInteger = rule { optional("+" | "-") ~ zeroOrMore(decimalDigit) }

  private def decimalFloat: Rule1[String] = rule {
    capture {
      decimalNumber ~ "." ~ zeroOrMore(decimalDigit) ~ optional(exponent) ~ optionalFloatSuffix |
        decimalNumber ~ exponent ~ optionalFloatSuffix |
        decimalNumber ~ optional(exponent) ~ ("f" | "F" | "d" | "D") |
        "." ~ oneOrMore(decimalDigit) ~ optional(exponent) ~ optionalFloatSuffix
    }
  }

  private def hexFloat: Rule1[String] = rule {
    capture(("0x" | "0X") ~ {
      (zeroOrMore(hexDigit) ~ "." ~ oneOrMore(hexDigit)) | (oneOrMore(hexDigit) ~ optional("."))
    } ~ ("p" | "P") ~ signedInteger ~ optionalFloatSuffix)
  }
}
