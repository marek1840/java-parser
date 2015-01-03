package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class BooleanTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaParser(string)
    get(parser.literal.run())
  }

  "Boolean parser should parse" - {
    "true" in (parse("true") shouldBe PrimitiveLiteral("true"))
    "false" in (parse("false") shouldBe PrimitiveLiteral("false"))
  }
}