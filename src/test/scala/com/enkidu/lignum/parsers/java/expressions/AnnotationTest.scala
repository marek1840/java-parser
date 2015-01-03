package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class NormalAnnotationTest extends ParserTest {
  def parse(string: String): Annotation = {
    implicit val parser = new JavaParser(string)
    get(parser.annotation.run())
  }

  "NormalAnnotation parser should parse" - {
    "normal Annotationss" - {
      "@A(i = i < i ? i : i)" in { parse("@A(i = i < i ? i : i)") shouldBe NormalAnnotation("A", Vector(("i", TernaryConditional(BinaryOperationChain(BinaryOperator.<, Vector(Select("i"), Select("i"))), Select("i"), Select("i"))))) }
      "@A(i = {i, i, })" in { parse("@A(i = {i, i, })") shouldBe NormalAnnotation("A", Vector(("i", ArrayInitializer(Vector(Select("i"), Select("i")))))) }
      "@A(i1=@A, i2 = {}, i3 = 3)" in { parse("@A(i1 = @A, i2 = {}, i3 = 3)") shouldBe NormalAnnotation("A", Vector(("i1", MarkerAnnotation("A")), ("i2", ArrayInitializer(Vector)), ("i3", PrimitiveLiteral("3")))) }
      "@A()" in { parse("@A()") shouldBe NormalAnnotation("A", Vector()) }
      "@A( i = 1)" in { parse("@A(i = 1)") shouldBe NormalAnnotation("A", Vector(("i", PrimitiveLiteral("1")))) }
      "@A(i = {})" in { parse("@A(i = {})") shouldBe NormalAnnotation("A", Vector(("i", ArrayInitializer(Vector)))) }
      "@A(i = @A)" in { parse("@A(i = @A)") shouldBe NormalAnnotation("A", Vector(("i", MarkerAnnotation("A")))) }
    }
    "marker Annotations" - {
      "@A" in { parse("@A") shouldBe MarkerAnnotation("A") }
    }
    "single element Annotations" - {
      "@A(5)" in { parse("@A(5)") shouldBe SingleElementAnnotation("A", PrimitiveLiteral("5")) }
      "@A({})" in { parse("@A({})") shouldBe SingleElementAnnotation("A", ArrayInitializer(Vector)) }
      "@A({i, i, i})" in { parse("@A({i, i, i})") shouldBe SingleElementAnnotation("A", ArrayInitializer(Vector(Select("i"), Select("i"), Select("i")))) }
      "@A(@A)" in { parse("@A(@A)") shouldBe SingleElementAnnotation(Vector("A"), MarkerAnnotation(Vector("A"))) }
    }
    "@A(@B(i1 = @C, i2 = @D(i)))" in { parse("@A(@B(i1 = @C, i2 = @D(i)))") shouldBe SingleElementAnnotation("A", NormalAnnotation("B", Vector(("i1", MarkerAnnotation("C")), ("i2", SingleElementAnnotation("D", Select("i")))))) }
  }

}
