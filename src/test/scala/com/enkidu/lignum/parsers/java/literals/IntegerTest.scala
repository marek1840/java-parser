package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.{IntegerLiteral, Literal}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class IntegerTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.literal.run())
  }

  "Integer parser should parse" - {
    "decimal" - {
      "0" in { parse("0") shouldBe IntegerLiteral("0") }
      "1" in { parse("1") shouldBe IntegerLiteral("1") }
      "1l" in { parse("1l") shouldBe IntegerLiteral("1l") }
      "1L" in { parse("1L") shouldBe IntegerLiteral("1L") }
    }
    "hex" - {
      "0xabcdefABCDEF" in { parse("0xabcdefABCDEF") shouldBe IntegerLiteral("0xabcdefABCDEF") }
      "0XabcdefABCDEF" in { parse("0XabcdefABCDEF") shouldBe IntegerLiteral("0XabcdefABCDEF") }
      "0xabcdefABCDEFl" in { parse("0xabcdefABCDEFl") shouldBe IntegerLiteral("0xabcdefABCDEFl") }
      "0xabcdefABCDEFL" in { parse("0XabcdefABCDEFL") shouldBe IntegerLiteral("0XabcdefABCDEFL") }
    }
    "octal" - {
      "0123" in { parse("0123") shouldBe IntegerLiteral("0123") }
      "0123l" in { parse("0123l") shouldBe IntegerLiteral("0123l") }
      "0123L" in { parse("0123L") shouldBe IntegerLiteral("0123L") }
    }
    "binary" - {
      "0b001010" in { parse("0b001010") shouldBe IntegerLiteral("0b001010") }
      "0B001010" in { parse("0b001010") shouldBe IntegerLiteral("0b001010") }
      "0B001010l" in { parse("0b001010l") shouldBe IntegerLiteral("0b001010l") }
      "0B001010L" in { parse("0b001010L") shouldBe IntegerLiteral("0b001010L") }
    }
  }

}
