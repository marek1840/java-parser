package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class CharacterTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaParser(string)
    get(parser.literal.run())
  }

  "Character parser should" - {
    "parse valid characters" - {
      "'a'" in (parse("'a'") shouldBe PrimitiveLiteral("'a'"))
      "'%'" in (parse("'%'") shouldBe PrimitiveLiteral("'%'"))
      "'\\t'" in (parse("'\\t'") shouldBe PrimitiveLiteral("'\\t'"))
      "'\\\\'" in (parse("'\\\\'") shouldBe PrimitiveLiteral("'\\\\'"))
      "'\\''" in (parse("'\\''") shouldBe PrimitiveLiteral("'\\''"))
      "'\\u03a9'" in (parse("'\\u03a9'") shouldBe PrimitiveLiteral("'\\u03a9'"))
      "'\\uFFFF'" in (parse("'\\uFFFF'") shouldBe PrimitiveLiteral("'\\uFFFF'"))
      "'\\177'" in (parse("'\\177'") shouldBe PrimitiveLiteral("'\\177'"))
    }
    "not parse invalid characters" - {
      "'" in { an[Exception] should be thrownBy parse("'''") }
      "\\" in { an[Exception] should be thrownBy parse("'\\'") }
      "'\n'" in { an[Exception] should be thrownBy parse("'\n'") }
      "'\r'" in { an[Exception] should be thrownBy parse("'\r'") }
    }
  }
}