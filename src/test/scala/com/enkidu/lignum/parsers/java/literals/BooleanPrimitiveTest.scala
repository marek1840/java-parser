package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.{BooleanLiteral, Literal}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class BooleanPrimitiveTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.literal.run())
  }

  "Boolean parser should parse" - {
    "true" in (parse("true") shouldBe BooleanLiteral("true"))
    "false" in (parse("false") shouldBe BooleanLiteral("false"))
  }
}