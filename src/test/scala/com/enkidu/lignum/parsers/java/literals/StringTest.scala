package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.{Literal, StringLiteral}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class StringTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.literal.run())
  }

  "String parser should" - {
    "parse" - {
      "\"\"" in (parse("\"\"") shouldBe StringLiteral("\"\""))
      """"\""""" in (parse(""""\""""") shouldBe StringLiteral(""""\"""""))
      """"This is a string"""" in (parse(""""This is a string"""") shouldBe StringLiteral(""""This is a string""""))
    }
    "not parse" - {
      "\"\\u000a\"" in (an[Exception] should be thrownBy parse("\"\\u000a\""))
      """"\"""" in (an[Exception] should be thrownBy parse(""""\""""))
      "fu00e9v" in { an[Exception] should be thrownBy parse("f\u00e9v") }
    }
  }

}