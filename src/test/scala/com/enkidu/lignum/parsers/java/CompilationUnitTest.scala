package com.enkidu.lignum.parsers.java

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.statement.declaration.CompilationUnitDeclaration
import com.enkidu.lignum.parsers.ast.statement.declaration.imports.{LazyImportDeclaration, SingleImportDeclaration, StaticImportDeclaration, StaticLazyImportDeclaration}
import com.enkidu.lignum.parsers.ast.statement.declaration.packages.{NamedPackageDeclaration, UnnamedPackageDeclaration}
import com.enkidu.lignum.parsers.ast.statement.declaration.types.{ClassDeclaration, EmptyDeclaration, InterfaceDeclaration}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class CompilationUnitTest extends ParserTest {
  def parse(string: String): CompilationUnitDeclaration = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.compilationUnit.run())
  }

  val p0 = """package a; imports a;"""
  val p1 = """package a; imports a.*;"""
  val p2 = """package a; imports static a.b;"""
  val p3 = """package a; imports static a.*;"""

  "Compilation unit parser should parse" - {
    "" in {
      parse("") shouldBe CompilationUnitDeclaration(
        UnnamedPackageDeclaration,
        Vector,
        Vector)
    }
    "package a.b;" in {
      parse("package a.b;") shouldBe CompilationUnitDeclaration(
        NamedPackageDeclaration(Vector, Vector("a", "b")),
        Vector,
        Vector)
    }
    p0 in {
      parse(p0) shouldBe CompilationUnitDeclaration(
        NamedPackageDeclaration(Vector, "a"),
        SingleImportDeclaration("a"),
        Vector)
    }
    p1 in {
      parse(p1) shouldBe CompilationUnitDeclaration(
        NamedPackageDeclaration(Vector, "a"),
        LazyImportDeclaration("a"),
        Vector)
    }
    p2 in {
      parse(p2) shouldBe CompilationUnitDeclaration(
        NamedPackageDeclaration(Vector, "a"),
        StaticImportDeclaration(Vector("a", "b")),
        Vector)
    }
    p3 in {
      parse(p3) shouldBe CompilationUnitDeclaration(
        NamedPackageDeclaration(Vector, "a"),
        StaticLazyImportDeclaration("a"),
        Vector)
    }
    "class A{}" in {
      parse("class A{}") shouldBe CompilationUnitDeclaration(
        UnnamedPackageDeclaration,
        Vector,
        ClassDeclaration(
          Vector,
          Vector,
          "A",
          Vector,
          None,
          Vector,
          Vector))
    }
    "interface A{}" in {
      parse("interface A{}") shouldBe CompilationUnitDeclaration(
        UnnamedPackageDeclaration,
        Vector,
        InterfaceDeclaration(
          Vector,
          Vector,
          "A",
          Vector,
          Vector,
          Vector))
    }
    ";" in {
      parse(";") shouldBe CompilationUnitDeclaration(
        UnnamedPackageDeclaration,
        Vector,
        EmptyDeclaration)
    }
  }

}