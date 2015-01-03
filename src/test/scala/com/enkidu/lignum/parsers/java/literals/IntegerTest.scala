package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class LiteralTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaParser(string)
    get(parser.literal.run())
  }

  "Integer parser should parse" - {
    "decimal" - {
      "0" in { parse("0") shouldBe PrimitiveLiteral("0") }
      "1" in { parse("1") shouldBe PrimitiveLiteral("1") }
      "1l" in { parse("1l") shouldBe PrimitiveLiteral("1l") }
      "1L" in { parse("1L") shouldBe PrimitiveLiteral("1L") }
    }
    "hex" - {
      "0xabcdefABCDEF" in { parse("0xabcdefABCDEF") shouldBe PrimitiveLiteral("0xabcdefABCDEF") }
      "0XabcdefABCDEF" in { parse("0XabcdefABCDEF") shouldBe PrimitiveLiteral("0XabcdefABCDEF") }
      "0xabcdefABCDEFl" in { parse("0xabcdefABCDEFl") shouldBe PrimitiveLiteral("0xabcdefABCDEFl") }
      "0xabcdefABCDEFL" in { parse("0XabcdefABCDEFL") shouldBe PrimitiveLiteral("0XabcdefABCDEFL") }
    }
    "octal" - {
      "0123" in { parse("0123") shouldBe PrimitiveLiteral("0123") }
      "0123l" in { parse("0123l") shouldBe PrimitiveLiteral("0123l") }
      "0123L" in { parse("0123L") shouldBe PrimitiveLiteral("0123L") }
    }
    "binary" - {
      "0b001010" in { parse("0b001010") shouldBe PrimitiveLiteral("0b001010") }
      "0B001010" in { parse("0b001010") shouldBe PrimitiveLiteral("0b001010") }
      "0B001010l" in { parse("0b001010l") shouldBe PrimitiveLiteral("0b001010l") }
      "0B001010L" in { parse("0b001010L") shouldBe PrimitiveLiteral("0b001010L") }
    }
  }

}
