package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class StringTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaParser(string)
    get(parser.literal.run())
  }

  "String parser should" - {
    "parse" - {
      "\"\"" in (parse("\"\"") shouldBe PrimitiveLiteral("\"\""))
      """"\""""" in (parse(""""\""""") shouldBe PrimitiveLiteral(""""\"""""))
      """"This is a string"""" in (parse(""""This is a string"""") shouldBe PrimitiveLiteral(""""This is a string""""))
    }
    "not parse" - {
      "\"\\u000a\"" in (an[Exception] should be thrownBy parse("\"\\u000a\""))
      """"\"""" in (an[Exception] should be thrownBy parse(""""\""""))
      "fu00e9v" in { an[Exception] should be thrownBy parse("f\u00e9v") }
    }
  }

}