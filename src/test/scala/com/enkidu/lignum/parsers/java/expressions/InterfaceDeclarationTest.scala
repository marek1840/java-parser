package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class InterfaceDeclarationTest extends ParserTest {
  def parse(string: String): TypeDeclaration = {
    implicit val parser = new JavaParser(string)
    get(parser.typeDeclaration.run())
  }

  def Interface(body: MemberDeclaration*) = Declaration.Interface(Vector, Vector, "A", Vector, Vector, body.toVector)
  def Annotation(body: MemberDeclaration*) = Declaration.Annotation(Vector, Vector, "A", body.toVector)

  "Interface parser should parse" - {
    "interfaces" - {
      "interface A extends A1, A2" in {
        parse("interface A extends A1, A2{}") shouldBe Declaration.Interface(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(ClassType(Vector, None, "A1", Vector), ClassType(Vector, None, "A2", Vector)),
          Vector)
      }
      "@A interface A{}" in {
        parse("@A interface A{}") shouldBe Declaration.Interface(
          Vector(MarkerAnnotation("A")),
          Vector,
          "A",
          Vector,
          Vector,
          Vector)
      }
      "public interface A{}" in {
        parse("public interface A{}") shouldBe Declaration.Interface(
          Vector,
          Vector(Modifier.Public),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "protected interface A{}" in {
        parse("protected interface A{}") shouldBe Declaration.Interface(
          Vector,
          Vector(Modifier.Protected),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "private  interface A{}" in {
        parse("private  interface A{}") shouldBe Declaration.Interface(
          Vector,
          Vector(Modifier.Private),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "abstract interface A{}" in {
        parse("abstract interface A{}") shouldBe Declaration.Interface(
          Vector,
          Vector(Modifier.Abstract),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "static interface A{}" in {
        parse("static interface A{}") shouldBe Declaration.Interface(
          Vector,
          Vector(Modifier.Static),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "strictfp interface A{}" in {
        parse("strictfp interface A{}") shouldBe Declaration.Interface(
          Vector,
          Vector(Modifier.Strictfp),
          "A",
          Vector,
          Vector,
          Vector)
      }
      "interface A extends B{}" in {
        parse("interface A extends B{}") shouldBe Declaration.Interface(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(ClassType(Vector, None, "B", Vector)),
          Vector)
      }
      "interface A<T> {}" in {
        parse("interface A<T> {}") shouldBe Declaration.Interface(
          Vector,
          Vector,
          "A",
          Vector(Template.Parameter(Vector, "T")),
          Vector,
          Vector)
      }
      "interface A{ public static final int a; }" in {
        parse("interface A{ public static final int a; }") shouldBe Interface(Declaration.Constant(
          Vector,
          Vector(Modifier.Public, Modifier.Static, Modifier.Final),
          PrimitiveType.Integer(Vector),
          Vector(Declarator.Variable("a"))))
      }
      "interface A{ @A public static final int a = 5; }" in {
        parse("interface A{ @A public static final int a = 5; }") shouldBe Interface(Declaration.Constant(
          Vector(MarkerAnnotation(Vector("A"))),
          Vector(Modifier.Public, Modifier.Static, Modifier.Final),
          PrimitiveType.Integer(Vector),
          Vector(Declarator.InitializedVariable("a", PrimitiveLiteral("5")))))
      }
      "interface A{ int f(); }" in {
        parse("interface A{ int f(); }") shouldBe Interface(Declaration.InterfaceMethod(
          Vector,
          Vector,
          Vector,
          PrimitiveType.Integer(Vector),
          Declarator.Method("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ @A int f(); }" in {
        parse("interface A{ @A int f(); }") shouldBe Interface(Declaration.InterfaceMethod(
          Vector(MarkerAnnotation("A")),
          Vector,
          Vector,
          PrimitiveType.Integer(Vector),
          Declarator.Method("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ public int f(); }" in {
        parse("interface A{ public int f(); }") shouldBe Interface(Declaration.InterfaceMethod(
          Vector,
          Vector(Modifier.Public),
          Vector,
          PrimitiveType.Integer(Vector),
          Declarator.Method("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ abstract int f(); }" in {
        parse("interface A{ abstract int f(); }") shouldBe Interface(Declaration.InterfaceMethod(
          Vector,
          Vector(Modifier.Abstract),
          Vector,
          PrimitiveType.Integer(Vector),
          Declarator.Method("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ default int f(); }" in {
        parse("interface A{ default int f(); }") shouldBe Interface(Declaration.InterfaceMethod(
          Vector,
          Vector(Modifier.Default),
          Vector,
          PrimitiveType.Integer(Vector),
          Declarator.Method("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ static int f(); }" in {
        parse("interface A{ static int f(); }") shouldBe Interface(Declaration.InterfaceMethod(
          Vector,
          Vector(Modifier.Static),
          Vector,
          PrimitiveType.Integer(Vector),
          Declarator.Method("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ strictfp int f(); }" in {
        parse("interface A{ strictfp int f(); }") shouldBe Interface(Declaration.InterfaceMethod(
          Vector,
          Vector(Modifier.Strictfp),
          Vector,
          PrimitiveType.Integer(Vector),
          Declarator.Method("f", Vector),
          Vector,
          EmptyStatement))
      }
      "interface A{ default int f(){;} }" in {
        parse("interface A{ default int f(){;} }") shouldBe Interface(Declaration.InterfaceMethod(
          Vector,
          Vector(Modifier.Default),
          Vector,
          PrimitiveType.Integer(Vector),
          Declarator.Method("f", Vector),
          Vector,
          Block(Vector(EmptyStatement))))
      }
    }
    "annotations" - {
      "@A @interface A{}" in {
        parse("@A @interface A{}") shouldBe Declaration.Annotation(
          Vector(MarkerAnnotation("A")),
          Vector,
          "A",
          Vector)
      }
      "public @interface A{}" in {
        parse("public @interface A{}") shouldBe Declaration.Annotation(
          Vector,
          Vector(Modifier.Public),
          "A",
          Vector)
      }
      "protected @interface A{}" in {
        parse("protected @interface A{}") shouldBe Declaration.Annotation(
          Vector,
          Vector(Modifier.Protected),
          "A",
          Vector)
      }
      "private @interface A{}" in {
        parse("private @interface A{}") shouldBe Declaration.Annotation(
          Vector,
          Vector(Modifier.Private),
          "A",
          Vector)
      }
      "abstract @interface A{}" in {
        parse("abstract @interface A{}") shouldBe Declaration.Annotation(
          Vector,
          Vector(Modifier.Abstract),
          "A",
          Vector)
      }
      "static @interface A{}" in {
        parse("static @interface A{}") shouldBe Declaration.Annotation(
          Vector,
          Vector(Modifier.Static),
          "A",
          Vector)
      }
      "strictfp @interface A{}" in {
        parse("strictfp @interface A{}") shouldBe Declaration.Annotation(
          Vector,
          Vector(Modifier.Strictfp),
          "A",
          Vector)
      }
      "@interface A{ int a();}" in {
        parse("@interface A{ int a();}") shouldBe Annotation(Declaration.AnnotationElement(
          Vector,
          Vector,
          PrimitiveType.Integer(Vector),
          "a"))
      }
      "@interface A{ @A int a();}" in {
        parse("@interface A{ @A int a();}") shouldBe Annotation(Declaration.AnnotationElement(
          Vector(MarkerAnnotation("A")),
          Vector,
          PrimitiveType.Integer(Vector),
          "a"))
      }
      "@interface A{ public int a();}" in {
        parse("@interface A{ public int a();}") shouldBe Annotation(Declaration.AnnotationElement(
          Vector,
          Vector(Modifier.Public),
          PrimitiveType.Integer(Vector),
          "a"))
      }
      "@interface A{ abstract int a();}" in {
        parse("@interface A{ abstract int a();}") shouldBe Annotation(Declaration.AnnotationElement(
          Vector,
          Vector(Modifier.Abstract),
          PrimitiveType.Integer(Vector),
          "a"))
      }
      "@interface A{ int a()[];}" in {
        parse("@interface A{ int a()[];}") shouldBe Annotation(Declaration.AnnotationElement(
          Vector,
          Vector,
          ArrayType(PrimitiveType.Integer(Vector), Vector(AbstractDimension(Vector))),
          "a"))
      }
      "@interface A{ int a() default 3;}" in {
        parse("@interface A{ int a() default 3;}") shouldBe Annotation(Declaration.AnnotationDefaultElement(
          Vector,
          Vector,
          PrimitiveType.Integer(Vector),
          "a",
          PrimitiveLiteral("3")))
      }
    }
  }

}