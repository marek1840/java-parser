package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.{FloatLiteral, Literal}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class FloatPrimitiveTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.literal.run())
  }

  "Float parser should parse" - {
    "decimal floats" - {
      ".01" in {parse(".01") shouldBe FloatLiteral(".01")}
      "1." in { parse("1.") shouldBe FloatLiteral("1.") }
      "1.1" in { parse("1.1") shouldBe FloatLiteral("1.1") }
      "1.1e1" in { parse("1.1e1") shouldBe FloatLiteral("1.1e1") }
      "1.1E+1" in { parse("1.1E+1") shouldBe FloatLiteral("1.1E+1") }
      "1.1e-1f" in { parse("1.1e-1f") shouldBe FloatLiteral("1.1e-1f") }
      "1.E1" in { parse("1.E1") shouldBe FloatLiteral("1.E1") }
      "1.e+1" in { parse("1.e+1") shouldBe FloatLiteral("1.e+1") }
      "1.E-1D" in { parse("1.E-1D") shouldBe FloatLiteral("1.E-1D") }
      ".1" in { parse(".1") shouldBe FloatLiteral(".1") }
      ".1e+1" in { parse(".1e+1") shouldBe FloatLiteral(".1e+1") }
      ".1e-1" in { parse(".1e-1") shouldBe FloatLiteral(".1e-1") }
      "1e+1" in { parse("1e+1") shouldBe FloatLiteral("1e+1") }
      "1f" in { parse("1f") shouldBe FloatLiteral("1f") }
      "1F" in { parse("1F") shouldBe FloatLiteral("1F") }
      "1d" in { parse("1d") shouldBe FloatLiteral("1d") }
      "1D" in { parse("1D") shouldBe FloatLiteral("1D") }
      "1.0812448255518705E-301d" in { parse("1.0812448255518705E-301d") shouldBe FloatLiteral("1.0812448255518705E-301d") }
      ".611609510448141581788E-08" in { parse(".611609510448141581788E-08") shouldBe FloatLiteral(".611609510448141581788E-08") }
      ".f" in {an[Exception] should be thrownBy parse(".f")}
    }
    "hexal floats" - {
      "0x1p+1f" in { parse("0x1p+1f") shouldBe FloatLiteral("0x1p+1f") }
      "0x1.p-1F" in { parse("0x1.p-1F") shouldBe FloatLiteral("0x1.p-1F") }
      "0X1.1P10d" in { parse("0X1.1P10d") shouldBe FloatLiteral("0X1.1P10d") }
      "0x.1P-91D" in { parse("0x.1P-91D") shouldBe FloatLiteral("0x.1P-91D") }
      "0x.1P-91" in { parse("0x.1P-91") shouldBe FloatLiteral("0x.1P-91") }
      "0x1.p-18" in { parse("0x1.p-18") shouldBe FloatLiteral("0x1.p-18") }
    }

  }
}