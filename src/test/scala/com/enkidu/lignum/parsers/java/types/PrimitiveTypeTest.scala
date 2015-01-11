package com.enkidu.lignum.parsers.java.types

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.MarkerAnnotation
import com.enkidu.lignum.parsers.ast.expression.types.primitives._
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class PrimitiveTypeTest extends ParserTest {
  def parse(string: String): Type = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.`type`.run())
  }

  "Primitive type parser should parse" - {
    "types" - {
      "byte" in { parse("byte") shouldBe BytePrimitive(Nil) }
      "short" in { parse("short") shouldBe ShortPrimitive(Nil) }
      "int" in { parse("int") shouldBe IntegerPrimitive(Nil) }
      "long" in { parse("long") shouldBe LongPrimitive(Nil) }
      "char" in { parse("char") shouldBe CharPrimitive(Nil) }
      "float" in { parse("float") shouldBe FloatPrimitive(Nil) }
      "double" in { parse("double") shouldBe DoublePrimitive(Nil) }
      "boolean" in { parse("boolean") shouldBe BooleanPrimitive(Nil) }
      "void" in { parse("void") shouldBe VoidPrimitive(Nil) }

      "with annotations" - {
        "@A byte" in { parse("@A byte") shouldBe BytePrimitive(MarkerAnnotation("A")) }
        "@A short" in { parse("@A short") shouldBe ShortPrimitive(MarkerAnnotation("A")) }
        "@A int" in { parse("@A int") shouldBe IntegerPrimitive(MarkerAnnotation("A")) }
        "@A long" in { parse("@A long") shouldBe LongPrimitive(MarkerAnnotation("A")) }
        "@A char" in { parse("@A char") shouldBe CharPrimitive(MarkerAnnotation("A")) }
        "@A float" in { parse("@A float") shouldBe FloatPrimitive(MarkerAnnotation("A")) }
        "@A double" in { parse("@A double") shouldBe DoublePrimitive(MarkerAnnotation("A")) }
        "@A boolean" in { parse("@A boolean") shouldBe BooleanPrimitive(MarkerAnnotation("A")) }
        "@A void" in { parse("@A void") shouldBe VoidPrimitive(MarkerAnnotation("A")) }
      }
    }

  }
}