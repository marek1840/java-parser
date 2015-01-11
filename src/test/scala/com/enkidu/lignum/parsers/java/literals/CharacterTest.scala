package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.{CharLiteral, Literal}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class CharacterTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.literal.run())
  }

  "Character parser should" - {
    "parse valid characters" - {
      "'a'" in (parse("'a'") shouldBe CharLiteral("'a'"))
      "'%'" in (parse("'%'") shouldBe CharLiteral("'%'"))
      "'\\t'" in (parse("'\\t'") shouldBe CharLiteral("'\\t'"))
      "'\\\\'" in (parse("'\\\\'") shouldBe CharLiteral("'\\\\'"))
      "'\\''" in (parse("'\\''") shouldBe CharLiteral("'\\''"))
      "'\\u03a9'" in (parse("'\\u03a9'") shouldBe CharLiteral("'\\u03a9'"))
      "'\\uFFFF'" in (parse("'\\uFFFF'") shouldBe CharLiteral("'\\uFFFF'"))
      "'\\177'" in (parse("'\\177'") shouldBe CharLiteral("'\\177'"))
    }
    "not parse invalid characters" - {
      "'" in { an[Exception] should be thrownBy parse("'''") }
      "\\" in { an[Exception] should be thrownBy parse("'\\'") }
      "'\n'" in { an[Exception] should be thrownBy parse("'\n'") }
      "'\r'" in { an[Exception] should be thrownBy parse("'\r'") }
    }
  }
}