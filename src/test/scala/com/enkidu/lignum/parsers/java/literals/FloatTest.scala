package com.enkidu.lignum.parsers.java.literals

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class FloatTest extends ParserTest {
  def parse(string: String): Literal = {
    implicit val parser = new JavaParser(string)
    get(parser.literal.run())
  }

  "Float parser should parse" - {
    "decimal floats" - {
      ".01" in {parse(".01") shouldBe PrimitiveLiteral(".01")}
      "1." in { parse("1.") shouldBe PrimitiveLiteral("1.") }
      "1.1" in { parse("1.1") shouldBe PrimitiveLiteral("1.1") }
      "1.1e1" in { parse("1.1e1") shouldBe PrimitiveLiteral("1.1e1") }
      "1.1E+1" in { parse("1.1E+1") shouldBe PrimitiveLiteral("1.1E+1") }
      "1.1e-1f" in { parse("1.1e-1f") shouldBe PrimitiveLiteral("1.1e-1f") }
      "1.E1" in { parse("1.E1") shouldBe PrimitiveLiteral("1.E1") }
      "1.e+1" in { parse("1.e+1") shouldBe PrimitiveLiteral("1.e+1") }
      "1.E-1D" in { parse("1.E-1D") shouldBe PrimitiveLiteral("1.E-1D") }
      ".1" in { parse(".1") shouldBe PrimitiveLiteral(".1") }
      ".1e+1" in { parse(".1e+1") shouldBe PrimitiveLiteral(".1e+1") }
      ".1e-1" in { parse(".1e-1") shouldBe PrimitiveLiteral(".1e-1") }
      "1e+1" in { parse("1e+1") shouldBe PrimitiveLiteral("1e+1") }
      "1f" in { parse("1f") shouldBe PrimitiveLiteral("1f") }
      "1F" in { parse("1F") shouldBe PrimitiveLiteral("1F") }
      "1d" in { parse("1d") shouldBe PrimitiveLiteral("1d") }
      "1D" in { parse("1D") shouldBe PrimitiveLiteral("1D") }
      "1.0812448255518705E-301d" in { parse("1.0812448255518705E-301d") shouldBe PrimitiveLiteral("1.0812448255518705E-301d") }
      ".611609510448141581788E-08" in { parse(".611609510448141581788E-08") shouldBe PrimitiveLiteral(".611609510448141581788E-08") }
      ".f" in {an[Exception] should be thrownBy parse(".f")}
    }
    "hexal floats" - {
      "0x1p+1f" in { parse("0x1p+1f") shouldBe PrimitiveLiteral("0x1p+1f") }
      "0x1.p-1F" in { parse("0x1.p-1F") shouldBe PrimitiveLiteral("0x1.p-1F") }
      "0X1.1P10d" in { parse("0X1.1P10d") shouldBe PrimitiveLiteral("0X1.1P10d") }
      "0x.1P-91D" in { parse("0x.1P-91D") shouldBe PrimitiveLiteral("0x.1P-91D") }
      "0x.1P-91" in { parse("0x.1P-91") shouldBe PrimitiveLiteral("0x.1P-91") }
      "0x1.p-18" in { parse("0x1.p-18") shouldBe PrimitiveLiteral("0x1.p-18") }
    }

  }
}