package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.AbstractDimension
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.IntegerLiteral
import com.enkidu.lignum.parsers.ast.expression.types.annotations.MarkerAnnotation
import com.enkidu.lignum.parsers.ast.expression.types.primitives.IntegerPrimitive
import com.enkidu.lignum.parsers.ast.expression.types.references.{ArrayType, ClassType}
import com.enkidu.lignum.parsers.ast.expression.types.templates.ParameterTemplate
import com.enkidu.lignum.parsers.ast.statement._
import com.enkidu.lignum.parsers.ast.statement.declaration.members._
import com.enkidu.lignum.parsers.ast.statement.declaration.types.{AnnotationDeclaration, InterfaceDeclaration, TypeDeclaration}
import com.enkidu.lignum.parsers.ast.statement.declarator.{InitializedVariableDeclarator, MethodDeclarator, VariableDeclarator}
import com.enkidu.lignum.parsers.ast.statement.modifers._
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class InterfaceDeclarationTest extends ParserTest {
  def parse(string: String): TypeDeclaration = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.typeDeclaration.run())
  }

  def Interface(body: MemberDeclaration*) = InterfaceDeclaration(Vector, Vector, "A", Vector, Vector, body.toVector)
  def Annotation(body: MemberDeclaration*) = AnnotationDeclaration(Vector, Vector, "A", body.toVector)

  "Interface parser should parse" - {
    "interfaces" - {
      "interface A extends A1, A2" in {
        parse("interface A extends A1, A2{}") shouldBe InterfaceDeclaration(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(ClassType(Vector, None, "A1", Vector), ClassType(Vector, None, "A2", Vector)),
          Vector)
      }
      "@A interface A{}" in {
        parse("@A interface A{}") shouldBe InterfaceDeclaration(
          Vector(MarkerAnnotation("A")),
          Vector,
          "A",
          Vector,
          Vector,
          Vector)
      }
      "public interface A{}" in {
        parse("public interface A{}") shouldBe InterfaceDeclaration(
          Vector,
          Vector(Public),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "protected interface A{}" in {
        parse("protected interface A{}") shouldBe InterfaceDeclaration(
          Vector,
          Vector(Protected),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "private  interface A{}" in {
        parse("private  interface A{}") shouldBe InterfaceDeclaration(
          Vector,
          Vector(Private),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "abstract interface A{}" in {
        parse("abstract interface A{}") shouldBe InterfaceDeclaration(
          Vector,
          Vector(Abstract),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "static interface A{}" in {
        parse("static interface A{}") shouldBe InterfaceDeclaration(
          Vector,
          Vector(Static),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "strictfp interface A{}" in {
        parse("strictfp interface A{}") shouldBe InterfaceDeclaration(
          Vector,
          Vector(Strictfp),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "interface A extends B{}" in {
        parse("interface A extends B{}") shouldBe InterfaceDeclaration(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(ClassType(Vector, None, "B", Vector)),
          Vector)
      }
      "interface A<T> {}" in {
        parse("interface A<T> {}") shouldBe InterfaceDeclaration(
          Vector,
          Vector,
          "A",
          Vector(ParameterTemplate(Vector, "T")),
          Vector,
          Vector)
      }
      "interface A{ public static final int a; }" in {
        parse("interface A{ public static final int a; }") shouldBe Interface(ConstantDeclaration(
          Vector,
          Vector(Public, Static, Final),
          IntegerPrimitive(Vector),
          Vector(VariableDeclarator("a"))))
      }
      "interface A{ @A public static final int a = 5; }" in {
        parse("interface A{ @A public static final int a = 5; }") shouldBe Interface(ConstantDeclaration(
          Vector(MarkerAnnotation(Vector("A"))),
          Vector(Public, Static, Final),
          IntegerPrimitive(Vector),
          Vector(InitializedVariableDeclarator("a", IntegerLiteral("5")))))
      }
      "interface A{ int f(); }" in {
        parse("interface A{ int f(); }") shouldBe Interface(InterfaceMethodDeclaration(
          Vector,
          Vector,
          Vector,
          IntegerPrimitive(Vector),
          MethodDeclarator("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ @A int f(); }" in {
        parse("interface A{ @A int f(); }") shouldBe Interface(InterfaceMethodDeclaration(
          Vector(MarkerAnnotation("A")),
          Vector,
          Vector,
          IntegerPrimitive(Vector),
          MethodDeclarator("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ public int f(); }" in {
        parse("interface A{ public int f(); }") shouldBe Interface(InterfaceMethodDeclaration(
          Vector,
          Vector(Public),
          Vector,
          IntegerPrimitive(Vector),
          MethodDeclarator("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ abstract int f(); }" in {
        parse("interface A{ abstract int f(); }") shouldBe Interface(InterfaceMethodDeclaration(
          Vector,
          Vector(Abstract),
          Vector,
          IntegerPrimitive(Vector),
          MethodDeclarator("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ default int f(); }" in {
        parse("interface A{ default int f(); }") shouldBe Interface(InterfaceMethodDeclaration(
          Vector,
          Vector(Default),
          Vector,
          IntegerPrimitive(Vector),
          MethodDeclarator("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ static int f(); }" in {
        parse("interface A{ static int f(); }") shouldBe Interface(InterfaceMethodDeclaration(
          Vector,
          Vector(Static),
          Vector,
          IntegerPrimitive(Vector),
          MethodDeclarator("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ strictfp int f(); }" in {
        parse("interface A{ strictfp int f(); }") shouldBe Interface(InterfaceMethodDeclaration(
          Vector,
          Vector(Strictfp),
          Vector,
          IntegerPrimitive(Vector),
          MethodDeclarator("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ default int f(){;} }" in {
        parse("interface A{ default int f(){;} }") shouldBe Interface(InterfaceMethodDeclaration(
          Vector,
          Vector(Default),
          Vector,
          IntegerPrimitive(Vector),
          MethodDeclarator("f", Vector),
          Vector,
          Block(Vector(EmptyStatement))))
      }
    }
    "annotations" - {
      "@A @interface A{}" in {
        parse("@A @interface A{}") shouldBe AnnotationDeclaration(
          Vector(MarkerAnnotation("A")),
          Vector,
          "A",
          Vector)
      }
      "public @interface A{}" in {
        parse("public @interface A{}") shouldBe AnnotationDeclaration(
          Vector,
          Vector(Public),
          "A",
          Vector)
      }
      "protected @interface A{}" in {
        parse("protected @interface A{}") shouldBe AnnotationDeclaration(
          Vector,
          Vector(Protected),
          "A",
          Vector)
      }
      "private @interface A{}" in {
        parse("private @interface A{}") shouldBe AnnotationDeclaration(
          Vector,
          Vector(Private),
          "A",
          Vector)
      }
      "abstract @interface A{}" in {
        parse("abstract @interface A{}") shouldBe AnnotationDeclaration(
          Vector,
          Vector(Abstract),
          "A",
          Vector)
      }
      "static @interface A{}" in {
        parse("static @interface A{}") shouldBe AnnotationDeclaration(
          Vector,
          Vector(Static),
          "A",
          Vector)
      }
      "strictfp @interface A{}" in {
        parse("strictfp @interface A{}") shouldBe AnnotationDeclaration(
          Vector,
          Vector(Strictfp),
          "A",
          Vector)
      }
      "@interface A{ int a();}" in {
        parse("@interface A{ int a();}") shouldBe Annotation(AnnotationElementDeclaration(
          Vector,
          Vector,
          IntegerPrimitive(Vector),
          "a"))
      }
      "@interface A{ @A int a();}" in {
        parse("@interface A{ @A int a();}") shouldBe Annotation(AnnotationElementDeclaration(
          Vector(MarkerAnnotation("A")),
          Vector,
          IntegerPrimitive(Vector),
          "a"))
      }
      "@interface A{ public int a();}" in {
        parse("@interface A{ public int a();}") shouldBe Annotation(AnnotationElementDeclaration(
          Vector,
          Vector(Public),
          IntegerPrimitive(Vector),
          "a"))
      }
      "@interface A{ abstract int a();}" in {
        parse("@interface A{ abstract int a();}") shouldBe Annotation(AnnotationElementDeclaration(
          Vector,
          Vector(Abstract),
          IntegerPrimitive(Vector),
          "a"))
      }
      "@interface A{ int a()[];}" in {
        parse("@interface A{ int a()[];}") shouldBe Annotation(AnnotationElementDeclaration(
          Vector,
          Vector,
          ArrayType(IntegerPrimitive(Vector), Vector(AbstractDimension(Vector))),
          "a"))
      }
      "@interface A{ int a() default 3;}" in {
        parse("@interface A{ int a() default 3;}") shouldBe Annotation(AnnotationDefaultElementDeclaration(
          Vector,
          Vector,
          IntegerPrimitive(Vector),
          "a",
          IntegerLiteral("3")))
      }
    }
  }

}