package com.enkidu.lignum.parsers.java.types

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.JavaParser

class PrimitiveTypeTest extends ParserTest {
  import PrimitiveType._
  def parse(string: String): Type = {
    implicit val parser = new JavaParser(string)
    get(parser.`type`.run())
  }

  "Primitive type parser should parse" - {
    "types" - {
      "byte" in { parse("byte") shouldBe Byte(Nil) }
      "short" in { parse("short") shouldBe Short(Nil) }
      "int" in { parse("int") shouldBe Integer(Nil) }
      "long" in { parse("long") shouldBe Long(Nil) }
      "char" in { parse("char") shouldBe Char(Nil) }
      "float" in { parse("float") shouldBe Float(Nil) }
      "double" in { parse("double") shouldBe Double(Nil) }
      "boolean" in { parse("boolean") shouldBe Boolean(Nil) }
      "void" in { parse("void") shouldBe Void(Nil) }

      "with annotations" - {
        "@A byte" in { parse("@A byte") shouldBe Byte(MarkerAnnotation("A")) }
        "@A short" in { parse("@A short") shouldBe Short(MarkerAnnotation("A")) }
        "@A int" in { parse("@A int") shouldBe Integer(MarkerAnnotation("A")) }
        "@A long" in { parse("@A long") shouldBe Long(MarkerAnnotation("A")) }
        "@A char" in { parse("@A char") shouldBe Char(MarkerAnnotation("A")) }
        "@A float" in { parse("@A float") shouldBe Float(MarkerAnnotation("A")) }
        "@A double" in { parse("@A double") shouldBe Double(MarkerAnnotation("A")) }
        "@A boolean" in { parse("@A boolean") shouldBe Boolean(MarkerAnnotation("A")) }
        "@A void" in { parse("@A void") shouldBe Void(MarkerAnnotation("A")) }
      }
    }

  }
}