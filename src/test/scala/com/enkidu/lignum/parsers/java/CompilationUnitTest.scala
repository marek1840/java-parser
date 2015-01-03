package com.enkidu.lignum.parsers.java

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class CompilationUnitTest extends ParserTest {
  def parse(string: String): Declaration.CompilationUnit = {
    implicit val parser = new JavaParser(string)
    get(parser.compilationUnit.run())
  }

  val p0 = """package a; import a;"""
  val p1 = """package a; import a.*;"""
  val p2 = """package a; import static a.b;"""
  val p3 = """package a; import static a.*;"""

  "Compilation unit parser should parse" - {
    "" in {
      parse("") shouldBe Declaration.CompilationUnit(
        Declaration.UnnamedPackage,
        Vector,
        Vector)
    }
    "package a.b;" in {
      parse("package a.b;") shouldBe Declaration.CompilationUnit(
        Declaration.Package(Vector, Vector("a", "b")),
        Vector,
        Vector)
    }
    p0 in {
      parse(p0) shouldBe Declaration.CompilationUnit(
        Declaration.Package(Vector, "a"),
        Declaration.SingleImport("a"),
        Vector)
    }
    p1 in {
      parse(p1) shouldBe Declaration.CompilationUnit(
        Declaration.Package(Vector, "a"),
        Declaration.LazyImport("a"),
        Vector)
    }
    p2 in {
      parse(p2) shouldBe Declaration.CompilationUnit(
        Declaration.Package(Vector, "a"),
        Declaration.StaticImport(Vector("a", "b")),
        Vector)
    }
    p3 in {
      parse(p3) shouldBe Declaration.CompilationUnit(
        Declaration.Package(Vector, "a"),
        Declaration.StaticLazyImport("a"),
        Vector)
    }
    "class A{}" in {
      parse("class A{}") shouldBe Declaration.CompilationUnit(
        Declaration.UnnamedPackage,
        Vector,
        Declaration.Class(
          Vector,
          Vector,
          "A",
          Vector,
          None,
          Vector,
          Vector))
    }
    "interface A{}" in {
      parse("interface A{}") shouldBe Declaration.CompilationUnit(
        Declaration.UnnamedPackage,
        Vector,
        Declaration.Interface(
          Vector,
          Vector,
          "A",
          Vector,
          Vector,
          Vector))
    }
    ";" in {
      parse(";") shouldBe Declaration.CompilationUnit(
        Declaration.UnnamedPackage,
        Vector,
        Declaration.Empty)
    }
  }

}